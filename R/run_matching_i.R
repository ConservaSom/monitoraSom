#' Match one template against one soundscape (internal)
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Internal engine: [run_matching()] is the public interface of this step.
#'   The per-pair engine that [run_matching()] calls once for every row of the
#'   match grid: it slides a single template spectrogram across a single
#'   soundscape and returns either the raw per-frame score vector or its
#'   detections. Not exported; the batch [run_matching()] wrapper covers the
#'   public use.
#'
#' @details It dispatches on `score_method` to one of three interchangeable
#'   scoring methods: `"fft"` (FFT-accelerated correlation, the fast default),
#'   `"cor"` (the Pearson reference) or `"dtw"` (time-warping tolerant), all
#'   producing the same output shape. **Scores are NOT interchangeable across
#'   engines**: `"cor"`/`"fft"` are windowed Pearson correlations on a
#'   `-1..1` scale, while `"dtw"` maps a mean per-cell L1 cost (path-normalized:
#'   `symmetric2`, `dist / (N + M)`, then `/ bins`) to `(0, 1]`. Within `"dtw"`,
#'   scores are comparable across soundscapes AND templates (the warp-length
#'   bias of the former `symmetric1` form was removed, 2026-08-31). Calibrate
#'   `min_score`/`min_quant` per engine, and note that DTW thresholds
#'   calibrated before the `symmetric2` migration do not transfer. When the
#'   spectrogram matrices are not supplied it builds them from
#'   the grid row; the batch wrapper passes cached matrices so each soundscape
#'   is transformed only once. It errors clearly if the soundscape is shorter
#'   than the template (nothing to slide).
#'
#' @section Pipeline context:
#'   Inner engine of step 7 (see [run_matching()]). Reads one row of the
#'   [fetch_match_grid()] grid. Produces one pair's detections or raw scores.
#'
#' @param df_grid_i One row of [fetch_match_grid()] output (a single template x
#'   soundscape pair).
#' @param score_method Character, the matching engine: `"fft"` (default), `"cor"`
#'   or `"dtw"`. See [run_matching()] for how they differ.
#' @param output Character. `"detections"` (default, via [fetch_score_peaks_i()])
#'   or `"scores"` (the raw one-row score tibble).
#' @param buffer_size,min_score,min_quant,top_n Forwarded to
#'   [fetch_score_peaks_i()] in detections mode (peak window and filters).
#' @param dtw_slack Numeric warping-window slack fraction for `score_method =
#'   "dtw"`. Default `0.2`. Ignored by `"cor"`/`"fft"`.
#' @param mat_soundscape,mat_template,soundscape_time Optional pre-computed
#'   spectrogram matrices (frames x bins) plus the soundscape time vector. When
#'   `NULL` (standalone use) they are built here; the batch wrapper supplies cached
#'   ones so spectrograms are computed once per soundscape.
#'
#' @return A one-row tibble carrying the per-frame `score_vec` when `output =
#'   "scores"`, or a [fetch_score_peaks_i()] detections `data.frame` when `output
#'   = "detections"`.
#'
#' @seealso [run_matching()] (the batch wrapper), [fetch_match_grid()],
#'   [fetch_score_peaks_i()].
#' @keywords internal
#' @examples
#' \dontrun{
#' # Score just the first pair of a grid.
#' # Load the package
#' library(monitoraSom)
#' # (Build a tiny grid from a synthesized recording; see
#' # [fetch_match_grid()] for the full recipe. The bundled df_grid dataset
#' # predates template_id and cannot feed run_matching().)
#' rec_dir <- file.path(tempdir(), "recs"); dir.create(rec_dir, showWarnings = FALSE)
#' rec <- tuneR::normalize(tuneR::sine(4000, duration = 10 * 16000,
#'                                     samp.rate = 16000), unit = "16")
#' tuneR::writeWave(rec, file.path(rec_dir, "siteA_01.wav"))
#' df_rois <- data.frame(
#'   soundscape_path = file.path(rec_dir, "siteA_01.wav"),
#'   soundscape_file = "siteA_01.wav",
#'   roi_label = "burst", roi_start = 1, roi_end = 1.5,
#'   roi_min_freq = 2, roi_max_freq = 6, roi_wl = 512, roi_ovlp = 50,
#'   stringsAsFactors = FALSE)
#' out_dir <- file.path(tempdir(), "templates")
#' export_templates(df_rois, templates_path = out_dir, create_dir = TRUE)
#' df_grid <- fetch_match_grid(
#'   fetch_soundscape_metadata(rec_dir),
#'   fetch_template_metadata(out_dir))
#' det1 <- run_matching_i(df_grid[1, ], score_method = "fft")
#' head(det1)
#' }
run_matching_i <- function(
  df_grid_i, score_method = "fft", output = "detections",
  buffer_size = "template", min_score = NULL, min_quant = NULL, top_n = NULL,
  dtw_slack = 0.2,
  mat_soundscape = NULL, mat_template = NULL, soundscape_time = NULL
) {
  score_method <- match.arg(score_method, c("cor", "fft", "dtw"))
  output <- match.arg(output, c("detections", "scores"))
  # AUD-40: dtw_slack reaches dtwclust::dtw_basic as window.size (RMD-03); a
  # negative or non-scalar value is a malformed window. Validate up front.
  if (!is.numeric(dtw_slack) || length(dtw_slack) != 1L || is.na(dtw_slack) ||
      dtw_slack < 0) {
    stop("'dtw_slack' must be a single non-negative numeric value.")
  }
  .validate_score_filters(min_score, min_quant, top_n)
  .validate_df_grid_i(df_grid_i)

  if (is.null(mat_soundscape) || is.null(mat_template) || is.null(soundscape_time)) {
    spec <- .build_match_spectrograms(df_grid_i)
    mat_soundscape <- spec$mat_soundscape
    mat_template <- spec$mat_template
    soundscape_time <- spec$soundscape_time
  }
  sliding_window <- nrow(mat_template)
  if (nrow(mat_soundscape) < sliding_window) {
    stop(
      "Soundscape (", nrow(mat_soundscape), " frames) is shorter than the ",
      "template (", sliding_window, " frames); cannot slide (RMC-13)."
    )
  }

  score_unpadded <- switch(
    score_method,
    cor = .score_cor(mat_soundscape, mat_template),
    fft = .score_fft(mat_soundscape, mat_template),
    dtw = .score_dtw(mat_soundscape, mat_template, dtw_slack = dtw_slack)
  )
  score_vec <- .pad_score_to_length(
    score_unpadded, sliding_window, length(soundscape_time)
  )

  res_raw <- tibble::as_tibble(df_grid_i)
  res_raw$score_sliding_window <- sliding_window
  res_raw$score_method <- score_method
  res_raw$score_vec <- list(
    data.frame(time_vec = soundscape_time, score_vec = score_vec)
  )

  if (output == "detections") {
    # Pure per-pair capture, then the composable filter (FSPB-10 option C); a
    # single pair is `scope = "pair"`.
    det <- fetch_score_peaks_i(res_raw, buffer_size = buffer_size)
    filter_detections_i(det, min_score = min_score, min_quant = min_quant,
                      top_n = top_n, scope = "pair")
  } else {
    res_raw
  }
}
