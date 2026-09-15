#' Per-pair matching engines: `cor` / `fft` / `dtw`
#'
#' @description The three coexisting scoring methods, each a **pure compute on
#'   supplied spectrogram matrices** (frames x frequency-bins) returning one score
#'   per complete sliding window (the dispatcher [run_matching_i()] pads/aligns and
#'   wraps them into the frozen contract). Decisions: `plans/plan-logs/plan-log-2026-06-07-001.md`
#'   (Stage 4 gate). Read-only references: `R/original/run_matching_i.R` (`cor`,
#'   `dtw`) and `R/original/alt_run_matching/raw_fft_matching.R` (`fft`).
#'
#'   - `.score_cor` — Pearson sliding-window (reference). Cross term via a BLAS
#'     matrix loop over template frames; mean/variance via cumulative sums (C1.3).
#'   - `.score_fft` — **same Pearson model**, the cross term via FFT (Lewis 1995
#'     mean-centred NCC); the fast backend for `cor`, reproduces it within
#'     tolerance (RMF-01..05).
#'   - `.score_dtw` — Dynamic Time Warping (`dtwclust::dtw_basic`), with an
#'     absolute, warp-normalized score (RMD-02, SR-02 modo B: `symmetric2`).

#' `cor` engine — Pearson per window (reference). RMC + C1.3.
#' @param mat_soundscape frames x bins.
#' @param mat_template frames x bins.
#' @return numeric Pearson score per complete window.
.score_cor <- function(mat_soundscape, mat_template) {
  sw <- nrow(mat_template)
  starts <- .window_starts(nrow(mat_soundscape), sw)
  n_complete <- length(starts)
  # cross[k] = sum_{f,b} template[f,b] * soundscape[k+f-1, b], via BLAS:
  # for each template frame f, a (n_complete x bins) %*% (bins) matrix-vector product.
  cross <- numeric(n_complete)
  for (f in seq_len(sw)) {
    block <- mat_soundscape[f:(f + n_complete - 1L), , drop = FALSE]
    cross <- cross + as.numeric(block %*% mat_template[f, ])
  }
  .pearson_windows_from_cross(cross, mat_soundscape, mat_template)
}

#' `fft` engine — Lewis (1995) mean-centred NCC via FFT (RMF-01..05).
#'
#' @description Computes the **same** Pearson score as [.score_cor()] but obtains
#'   the cross term by FFT cross-correlation along the **time axis only** (the
#'   template band is fixed): `cross = sum_b IFFT(FFT(s_b) * Conj(FFT(hpad_b)))`,
#'   reduced to one inverse FFT by summing the per-bin spectra first. This
#'   dissolves the original's defects — no `colMeans` over frequency lags (RMF-01),
#'   rigorous normalization via the shared mean-centred combine (RMF-02/04), a
#'   single correlation convention (RMF-03) — and is band-aligned and
#'   center-aligned by construction once the dispatcher pads it with the same
#'   geometry as `cor` (RMF-05). Must reproduce `cor` within numerical tolerance.
#' @param mat_soundscape frames x bins.
#' @param mat_template frames x bins.
#' @return numeric Pearson score per complete window.
.score_fft <- function(mat_soundscape, mat_template) {
  if (!all(is.finite(mat_soundscape)) || !all(is.finite(mat_template))) {
    stop("Non-finite values in the spectrogram matrices (RMF-06).")
  }
  n <- nrow(mat_soundscape)
  sw <- nrow(mat_template)
  bins <- ncol(mat_soundscape)
  # zero-pad both to a highly-composite length so the FFT is fast for any `n`
  # (without this, an `n` with a large prime factor falls back to a slow DFT —
  # the reason the original `fast_pattern_match` called `nextn`). Padding with
  # zeros beyond the soundscape/template support does not change the valid region.
  fft_len <- stats::nextn(n, factors = c(2L, 3L, 5L))
  s_pad <- matrix(0, nrow = fft_len, ncol = bins)
  s_pad[seq_len(n), ] <- mat_soundscape
  h_pad <- matrix(0, nrow = fft_len, ncol = bins)
  h_pad[seq_len(sw), ] <- mat_template
  fft_s <- stats::mvfft(s_pad)
  fft_h <- stats::mvfft(h_pad)
  agg <- rowSums(fft_s * Conj(fft_h))
  cc <- stats::fft(agg, inverse = TRUE) / fft_len
  if (max(abs(Im(cc))) > 1e-6 * max(abs(Re(cc)), 1)) {
    warning("FFT cross-correlation has a large imaginary residue (RMF-09).")
  }
  cross <- Re(cc)[seq_len(n - sw + 1L)]
  .pearson_windows_from_cross(cross, mat_soundscape, mat_template)
}

#' `dtw` engine — Dynamic Time Warping (RMD-01/02/03).
#'
#' @description Per window, a `dtwclust::dtw_basic` L1 distance between the
#'   soundscape window and the template (both multivariate series, frames x bins).
#'   Requires BOTH `dtw` and `dtwclust` (RMD-01). The raw distance is normalized to
#'   an **absolute, cross-soundscape- and cross-template-comparable** score
#'   (RMD-02; SR-02 resolved in mode B, 2026-08-31): `dtw::symmetric2` with
#'   `normalize = TRUE` (the standard path-length normalization, `dist / (N + M)`),
#'   then divided by `bins` — the mean per-cell L1 cost along the ACTUAL
#'   alignment — mapped through `1 / (1 + cost)` to a bounded `(0, 1]` scale.
#'   Because the denominator follows the real warp path (not a fixed template
#'   area), the warp-length bias of the former `symmetric1` form is gone: a
#'   no-warp alignment scores exactly as before, and heavier warping no longer
#'   inflates the score. Scores are NOT comparable against `"cor"`/`"fft"`
#'   (Pearson `-1..1`) — thresholds are engine-specific (and per template).
#' @param mat_soundscape frames x bins.
#' @param mat_template frames x bins.
#' @param dtw_slack warping-window slack as a fraction of the template length
#'   (RMD-03, exposed tuning knob; default `0.2` = the original constant).
#' @return numeric comparable score per complete window.
.score_dtw <- function(mat_soundscape, mat_template, dtw_slack = 0.2) {
  if (!requireNamespace("dtw", quietly = TRUE) ||
      !requireNamespace("dtwclust", quietly = TRUE)) {
    stop(
      "The 'dtw' method needs both 'dtw' and 'dtwclust'. Install them with ",
      "install.packages(c('dtw', 'dtwclust'))."
    )
  }
  sw <- nrow(mat_template)
  bins <- ncol(mat_template)
  starts <- .window_starts(nrow(mat_soundscape), sw)
  window_size <- round(sw + dtw_slack * sw)
  dist <- vapply(starts, function(k) {
    dtwclust::dtw_basic(
      mat_soundscape[k:(k + sw - 1L), , drop = FALSE], mat_template,
      backtrack = FALSE, norm = "L1", step.pattern = dtw::symmetric2,
      window.size = window_size, normalize = TRUE
    )
  }, numeric(1))
  # dtw_basic(normalize = TRUE) already divides by N+M (the symmetric2
  # path normalization); / bins brings it to a mean per-cell L1 cost.
  mean_cell_cost <- dist / bins
  1 / (1 + mean_cell_cost)
}
