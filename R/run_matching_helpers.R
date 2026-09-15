#' Shared helpers for the per-pair matching engine (RMC shared items)
#'
#' @description Validation, spectrogram construction, windowing and score
#'   padding shared by the three `run_matching_i` versions (`_cor`/`_fft`/`_dtw`)
#'   and their dispatcher [run_matching_i()]. Mirrors the helper-module pattern of
#'   the soundscape/template refactors. Implements the SHARED `RMC` items:
#'   RMC-10 (the dispatcher builds spectrograms once / accepts pre-computed
#'   matrices so the batch wrapper can cache them), RMC-11 (flim/Nyquist + equal
#'   frequency-resolution validation), RMC-12 (`norm = FALSE` since F3
#'   (2026-09-10) — absolute amplitude; the earlier `norm = TRUE` was an
#'   uninspected parity carry-over, see the F3 entry in
#'   `DEBT_INVENTORY_LOG-run_matching.md`),
#'   RMC-13 (short-soundscape guard + `seq_len`), RMC-14 (`df_grid_i` validation).
#'
#'   Cycle: `plans/plan-logs/plan-log-2026-06-04-001.md` (+ `-2026-06-05-001`, `-2026-06-07-001`).
#'   Read-only reference: `R/original/run_matching_i.R`.

# Required `fetch_match_grid` columns the engine reads (RMC-14). Metadata columns
# carried verbatim into the frozen contract are validated for presence too.
.RUN_MATCHING_REQUIRED_COLS <- c(
  "soundscape_path", "template_path",
  "template_wl", "template_ovlp", "template_min_freq", "template_max_freq",
  "template_start", "template_end"
)

#' Validate one grid row (RMC-14)
#' @param df_grid_i one row of `fetch_match_grid()` output.
#' @return invisibly `TRUE`; stops with a column-named message otherwise.
.validate_df_grid_i <- function(df_grid_i) {
  if (!is.data.frame(df_grid_i) || nrow(df_grid_i) != 1L) {
    stop("`df_grid_i` must be a single-row data.frame (one fetch_match_grid row).")
  }
  missing <- setdiff(.RUN_MATCHING_REQUIRED_COLS, names(df_grid_i))
  if (length(missing) > 0L) {
    stop(
      "`df_grid_i` is missing required column(s): ",
      paste(missing, collapse = ", "),
      ". Expected the fetch_match_grid schema."
    )
  }
  invisible(TRUE)
}

#' seewave spec_params for one grid row (RMC-12: `norm = FALSE` — F3, 2026-09-10:
#' the pre-1.2.0 front-end kept `norm = TRUE` as an uninspected dev artifact,
#' making scores relative to each recording's own maximum. `norm = FALSE` keeps
#' absolute amplitude, so scores compare across recordings.)
#' @param df_grid_i one row of the match grid.
#' @param side `"soundscape"` (default) reads `soundscape_channel`
#'   from the row; `"template"` reads `template_channel`.
#' @return list passed to `seewave::spectro` (no `tlim`).
.spec_params_from_row <- function(df_grid_i, side = c("soundscape", "template")) {
  side <- match.arg(side)
  col <- if (side == "soundscape") "soundscape_channel" else "template_channel"
  ch <- if (is.null(df_grid_i[[col]])) NA_character_ else df_grid_i[[col]]
  list(
    wl = df_grid_i$template_wl, ovlp = df_grid_i$template_ovlp,
    flim = c(df_grid_i$template_min_freq, df_grid_i$template_max_freq),
    plot = FALSE, norm = FALSE,
    channel = ch
  )
}

#' Build one spectrogram matrix (frames x frequency-bins) + its time vector
#'
#' @description The reusable building block the dispatcher and the batch
#'   orchestrator share, so a soundscape spectrogram can be computed **once per
#'   group** and reused across that group's templates (RMC-10 cache realised at
#'   the batch level). Validates `flim` against the file's Nyquist (RMC-11).
#' @param wav_path WAV file.
#' @param spec_params [.spec_params_from_row()] output.
#' @param tlim optional `c(start, end)` (templates only).
#' @param which_file label for error messages.
#' @return list(`mat` = `t(amp)` frames x bins, `time` = frame times).
.build_spectro_matrix <- function(wav_path, spec_params, tlim = NULL,
                                  which_file = "file") {
  .build_spectro_from_wave(tuneR::readWave(wav_path), spec_params, tlim, which_file)
}

#' Build a spectrogram matrix from an already-read `Wave` (C1.2 preload reuse)
#'
#' @description Lets the orchestrator read a soundscape WAV **once** and build a
#'   spectrogram for several `spec_params` from the same in-memory audio (the
#'   "preload" path), avoiding repeated disk reads. Same return as
#'   [.build_spectro_matrix()].
#' @param wave a `tuneR::Wave`.
#' @param spec_params spec_params list (from [.spec_params_from_row()]).
#' @param tlim optional time limits `c(from, to)` in seconds.
#' @param which_file label used in assertion messages.
.build_spectro_from_wave <- function(wave, spec_params, tlim = NULL,
                                     which_file = "file") {
  # DEC-29 (`per_channel`): when the row names a specific soundscape channel,
  # analyze that channel. tuneR::mono() on a mono Wave returns it unchanged;
  # an absent/NA channel keeps the current behavior (channel 1/left).
  ch <- spec_params$channel
  spec_params$channel <- NULL          # engine arg; never forwarded to seewave
  if (!is.null(ch) && !is.na(ch) && ch == "right") {
    wave <- tuneR::mono(wave, "right")
  } else if (!is.null(ch) && !is.na(ch) && ch == "left") {
    wave <- tuneR::mono(wave, "left")
  }
  .assert_flim_within_nyquist(wave@samp.rate, spec_params$flim, which_file)
  args <- c(list(wave), spec_params)
  if (!is.null(tlim)) args <- c(args, list(tlim = tlim))
  spectro <- do.call(seewave::spectro, args)
  mat <- t(spectro$amp)
  # AUD-38: digital silence (a recorder dropout / zero-padded segment) yields
  # non-finite dB cells. Replace every non-finite cell
  # with a finite floor (the matrix's quietest real dB, or 0 when the whole
  # matrix is silent) so BOTH scoring engines see identical finite input: the
  # fft engine (RMF-06) hard-errors on non-finite cells while cor keeps them
  # window-local, so without this floor one silent recording aborts an fft sweep
  # and the PERF-01 cor≈fft equivalence does not hold for that input class.
  if (!all(is.finite(mat))) {
    finite_min <- suppressWarnings(min(mat[is.finite(mat)]))
    if (!is.finite(finite_min)) finite_min <- 0
    mat[!is.finite(mat)] <- finite_min
  }
  list(mat = mat, time = spectro$time)
}

#' Soundscape duration in seconds from the WAV header (cheap; for the C1.2
#' preload-vs-stream decision).
#' @param wav_path path to the WAV file.
.wav_duration_s <- function(wav_path) {
  h <- tuneR::readWave(wav_path, header = TRUE)
  h$samples / h$sample.rate
}

#' Stable signature of a row's spec_params (memoization key within a group).
#' @param df_grid_i one row of the match grid.
.spec_key <- function(df_grid_i) {
  ch <- if (is.null(df_grid_i[["soundscape_channel"]])) "left"
        else if (is.na(df_grid_i[["soundscape_channel"]])) "left"
        else df_grid_i[["soundscape_channel"]]
  paste(df_grid_i$template_wl, df_grid_i$template_ovlp,
        df_grid_i$template_min_freq, df_grid_i$template_max_freq,
        ch, sep = "|")
}

#' @noRd
#' Resolve WHERE a template's audio lives and WHICH slice to cut (TM-12)
#'
#' @description The standard template schema (`_schema_templates.R`) defines
#'   `template_start`/`template_end` as **source-relative** bounds (seconds into
#'   the origin soundscape), while the engine needs a **file-relative** `tlim`
#'   for the WAV it actually reads. Conflating the two breaks every
#'   `fetch_template_metadata`-produced grid: a standalone cut of ~2 s with
#'   source bounds 36.2–38.5 s sends `seewave::spectro` out of bounds (found by
#'   the `template_matching` smoke, TM-12). Resolution by `template_mode`:
#'   - `standalone_audio` — the cut WAV **is** the template: read
#'     `template_path` whole (`tlim = NULL`);
#'   - `reference_metadata` — cut on demand from the origin recording: read
#'     `origin_soundscape_path` with `tlim = c(template_start, template_end)`;
#'   - no/NA `template_mode` (hand-built grids, original semantics where the
#'     bounds are file-relative) — unchanged: `template_path` + `tlim`.
#' @param df_grid_i one grid row.
#' @return list(`path`, `tlim`); `tlim` may be `NULL`.
.template_read_spec <- function(df_grid_i) {
  mode <- if ("template_mode" %in% names(df_grid_i)) {
    df_grid_i$template_mode
  } else {
    NA_character_
  }
  if (!is.na(mode) && mode == "standalone_audio") {
    return(list(path = df_grid_i$template_path, tlim = NULL))
  }
  if (!is.na(mode) && mode == "reference_metadata") {
    if (!"origin_soundscape_path" %in% names(df_grid_i) ||
        is.na(df_grid_i$origin_soundscape_path)) {
      stop("reference_metadata template without `origin_soundscape_path`; ",
           "cannot cut the template from its source recording (TM-12).")
    }
    return(list(
      path = df_grid_i$origin_soundscape_path,
      tlim = c(df_grid_i$template_start, df_grid_i$template_end)
    ))
  }
  list(
    path = df_grid_i$template_path,
    tlim = c(df_grid_i$template_start, df_grid_i$template_end)
  )
}

#' @noRd
#' Stable per-template cache key (TM-12): reference templates can share one
#' origin recording, so the read path alone is not unique — include the slice.
#' FEAT-02: also include the DSP spec params (`.spec_key` = wl|ovlp|min|max). The
#' cached object is a *spectrogram*, so two rows reading the same audio slice but
#' with different wl/ovlp/flim are DIFFERENT spectrograms and must not share a
#' cache entry (the soundscape side already keys on all four via `.spec_key`).
#' On a normal grid each template carries exactly one param set, so the key is
#' unique per template as before and the cache behaviour is byte-identical; the
#' fix only matters under grid augmentation (FEAT-01), where it prevents a
#' silently-wrong spectrogram reuse (differing ovlp shares bins -> no assertion).
#' @param df_grid_i one row of the match grid.
.template_cache_key <- function(df_grid_i) {
  paste(df_grid_i$template_path, df_grid_i$template_start,
        df_grid_i$template_end,
        df_grid_i$template_wl, df_grid_i$template_ovlp,
        df_grid_i$template_min_freq, df_grid_i$template_max_freq, sep = "|")
}

#' Build the soundscape + template spectrogram matrices (RMC-10/11/12/13)
#'
#' @param df_grid_i validated single grid row.
#' @return list(`mat_soundscape`, `mat_template`, `soundscape_time`,
#'   `sliding_window`): both matrices are frames x frequency-bins (`t(amp)`),
#'   `norm = FALSE` (F3 — absolute amplitude, scores comparable across
#'   recordings; both `cor` and the Lewis `fft` mean-centre downstream).
.build_match_spectrograms <- function(df_grid_i) {
  sp <- .spec_params_from_row(df_grid_i, side = "soundscape")
  ss <- .build_spectro_matrix(df_grid_i$soundscape_path, sp,
                              which_file = "soundscape")
  tspec <- .template_read_spec(df_grid_i)
  tp <- .build_spectro_matrix(
    tspec$path, .spec_params_from_row(df_grid_i, side = "template"),
    tlim = tspec$tlim, which_file = "template"
  )
  .assert_match_matrices(ss$mat, tp$mat)
  list(
    mat_soundscape = ss$mat, mat_template = tp$mat,
    soundscape_time = ss$time, sliding_window = nrow(tp$mat)
  )
}

#' Soundscape/template matrices must share bins and the soundscape be longer
#' (RMC-11 equal frequency resolution, RMC-13 length).
#' @param mat_ss soundscape spectrogram matrix.
#' @param mat_tp template spectrogram matrix.
.assert_match_matrices <- function(mat_ss, mat_tp) {
  if (ncol(mat_ss) != ncol(mat_tp)) {
    stop(
      "Soundscape and template have different frequency resolutions (",
      ncol(mat_ss), " vs ", ncol(mat_tp), " bins). They must share `wl`/`ovlp`/",
      "`flim` and sample rate (RMC-11)."
    )
  }
  if (nrow(mat_ss) < nrow(mat_tp)) {
    stop(
      "Soundscape spectrogram (", nrow(mat_ss), " frames) is shorter than the ",
      "template (", nrow(mat_tp), " frames); cannot slide (RMC-13)."
    )
  }
  invisible(TRUE)
}

#' flim (kHz) must not exceed the file's Nyquist (RMC-11)
#' @param samp_rate sample rate in Hz.
#' @param flim frequency limits `c(min, max)` in kHz.
#' @param which_file label used in the assertion message.
.assert_flim_within_nyquist <- function(samp_rate, flim, which_file) {
  nyquist_khz <- samp_rate / 2 / 1000
  if (max(flim) > nyquist_khz + 1e-9) {
    stop(
      "`flim` upper bound (", max(flim), " kHz) exceeds the ", which_file,
      " Nyquist frequency (", round(nyquist_khz, 3), " kHz)."
    )
  }
  invisible(TRUE)
}

#' Valid sliding-window start indices (RMC-13: `seq_len`, never `1:length`)
#' @param n_frames number of score frames.
#' @param sliding_window sliding-window length in frames.
#' @return integer vector `1:(n_frames - sliding_window + 1)`, or `integer(0)`.
.window_starts <- function(n_frames, sliding_window) {
  n_complete <- n_frames - sliding_window + 1L
  if (n_complete < 1L) integer(0) else seq_len(n_complete)
}

#' Pad the per-window score vector to soundscape length (RMC-01/02/03)
#'
#' @description Corrected, **symmetric-by-geometry** padding: `pad_left =
#'   sw_start`, `pad_right = sw_end` (RMC-01 — fixes the original `sw_start - 1` /
#'   `sw_end + 1` one-frame left shift). The total length is exactly
#'   `length(soundscape_time)`, asserted as a hard invariant (RMC-02, replacing
#'   the `head(score, -1)` band-aid). Borders and residual `NA`s are filled with
#'   the principled sentinel `0` (RMC-03 — "no linear association" for Pearson;
#'   pair-independent and uniform across the three versions), not the per-pair
#'   minimum.
#' @param score numeric, one value per complete sliding window.
#' @param sliding_window integer template frame count.
#' @param n_time integer length of the soundscape time vector.
#' @param sentinel numeric border / NA fill (default `0`).
#' @return numeric of length `n_time`, frame-aligned to the soundscape.
.pad_score_to_length <- function(score, sliding_window, n_time, sentinel = 0) {
  sw_start <- sliding_window %/% 2L
  sw_end <- (sliding_window - sw_start) - 1L
  padded <- c(rep(sentinel, sw_start), score, rep(sentinel, sw_end))
  if (length(padded) != n_time) {
    stop(
      "Score-vector padding produced ", length(padded), " frames but the ",
      "soundscape has ", n_time, " (RMC-02 invariant violated)."
    )
  }
  padded[is.na(padded)] <- sentinel
  padded
}

#' Mean-centred normalized cross-correlation (Pearson) from the cross term
#'
#' @description Shared combine for `_cor` and `_fft`: given the per-window cross
#'   term `cross[k] = sum_{f,b} template[f,b] * soundscape[k+f-1, b]`, returns the
#'   Pearson coefficient of each window's flattened patch against the flattened
#'   template — identical to `cor(c(window), c(template))`. The local window mean
#'   and sum-of-squares come from cumulative sums (O(n)), so only the `cross` term
#'   differs between the BLAS (`_cor`) and FFT (`_fft`, Lewis 1995) backends.
#' @param cross numeric per-window cross term (length n_complete).
#' @param mat_soundscape frames x bins.
#' @param mat_template frames x bins.
#' @return numeric Pearson score per window; `NA` where a window has zero
#'   variance (filled with the sentinel by [.pad_score_to_length()]).
.pearson_windows_from_cross <- function(cross, mat_soundscape, mat_template) {
  sw <- nrow(mat_template)
  n_total <- sw * ncol(mat_template)
  starts <- .window_starts(nrow(mat_soundscape), sw)

  row_sum <- rowSums(mat_soundscape)
  row_sum2 <- rowSums(mat_soundscape^2)
  cum_sum <- c(0, cumsum(row_sum))
  cum_sum2 <- c(0, cumsum(row_sum2))
  win_sum <- cum_sum[starts + sw] - cum_sum[starts]
  win_sum2 <- cum_sum2[starts + sw] - cum_sum2[starts]

  sum_t <- sum(mat_template)
  sum_t2 <- sum(mat_template^2)
  mean_s <- win_sum / n_total
  mean_t <- sum_t / n_total

  numerator <- cross - n_total * mean_s * mean_t
  var_s <- win_sum2 - n_total * mean_s^2
  var_t <- sum_t2 - n_total * mean_t^2
  denom <- sqrt(var_s * var_t)
  score <- numerator / denom
  score[!is.finite(score)] <- NA_real_
  score
}
