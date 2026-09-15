#' Run the template matching over a template x soundscape grid
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Runs the matching engine over a whole [fetch_match_grid()] search grid:
#'   for each soundscape-template pair it computes a per-frame similarity
#'   score and (by default) reduces those scores to a table of **detections**
#'   (score peaks). This is the heart of the monitoraSom workflow: the step
#'   between building the search grid and validating what was found.
#'
#' @details
#'   The engine offers three interchangeable scoring methods, all producing
#'   outputs with the same format, so you can switch freely with
#'   `score_method`:
#'   \itemize{
#'     \item `"fft"` (default): FFT-accelerated normalized cross-correlation.
#'       Numerically equal to `"cor"` (within 1e-6) but roughly 10× faster;
#'       the default option.
#'     \item `"cor"`: the plain Pearson sliding-window reference. Slower,
#'       kept for validation and teaching.
#'     \item `"dtw"` (**experimental**): Dynamic Time Warping, tolerant to
#'       small time distortions between the template and the corresponding
#'       soundscape frame. Use it when calls of the same species keep the same
#'       spectral acoustic signature but vary in duration.
#'   }
#'   Set `output = "scores"` to keep the raw per-frame score vectors instead of
#'   peaks (for custom peak-picking or plotting with [plot_scores()]).
#'
#'   Three things to know. (1) The grid should carry a real `template_id`;
#'   without one the resulting detections cannot be cleanly joined back to
#'   their template, so this is an error by default (bypass it with
#'   `require_template_id = FALSE`). (2) Long soundscapes are memory-hungry.
#'   Recordings longer than `stream_threshold_s` are re-read per template
#'   rather than held in memory; this bounds memory use at a small speed
#'   cost. (3) The filters (`min_score`, `min_quant`, `top_n`) trim the
#'   detection set. Strong filters can drop detections that were never
#'   validated, leaving an incomplete picture of what the templates found.
#'   Use filters with caution while a template's thresholds are still being
#'   tuned. The safest path is to keep the raw scores
#'   (`output = "scores"`) and filter later with [fetch_score_peaks()].
#'
#' @section Pipeline context:
#'   Step 7 of the monitoraSom analysis flow. Reads the search grid from
#'   [fetch_match_grid()] (step 6). Produces a detections table (or raw score
#'   vectors) used by [fetch_score_peaks()] / peak review and downstream by
#'   [detecs_to_rois()] (step 10).
#'
#' @param df_grid A [fetch_match_grid()] result (one row per
#'   soundscape-template pair). Should carry a `template_id` column for a
#'   clean detections join (see `require_template_id`).
#' @param score_method Character, the matching engine. `"fft"` (default,
#'   FFT-accelerated, roughly 10× faster than and numerically equal to
#'   `"cor"` within 1e-6), `"cor"` (Pearson reference) or `"dtw"`
#'   (time-warping tolerant, experimental). Leave at `"fft"` unless you need
#'   the reference engine or tolerance to time distortion in the acoustic
#'   structure.
#' @param ncores Positive integer, default `1`. Number of CPU cores used to
#'   score the grid in parallel (one group per soundscape; multiple cores
#'   work on macOS and Linux). Raise it to speed up large grids; it is
#'   capped at the number of available cores.
#' @param output Character. `"detections"` (default) reduces scores to peaks by
#'   running [fetch_score_peaks()] internally; `"scores"` returns the raw
#'   per-frame score vectors instead.
#' @param output_db Optional path of a DuckDB database (detections, or scores
#'   when `output = "scores"`). `NULL` (default) returns the result in memory
#'   and persists nothing.
#' @param autosave_action Character, only used when the detections are written
#'   to `output_db`. `"replace"` (default) clears the detections table and
#'   inserts the new rows; `"append"` updates existing rows by `detection_id`
#'   and keeps prior rows. Under `"append"`, re-running a pair you already
#'   computed never duplicates rows: `detection_id` is deterministic, so the
#'   same result replaces itself. Rows written by an earlier run with
#'   different filters are kept by design.
#' @param scores_format Character, only used when `output = "scores"`.
#'   `"duckdb"` (default) persists one compressed array column per pair.
#' @param stream_threshold_s Numeric seconds (default `300` = 5 min).
#'   Soundscapes longer than this are streamed (re-read per template) rather
#'   than held in memory. Lower it when memory is scarce; raise it to favour
#'   speed.
#' @param buffer_size Internal: the peak-capture window, in spectrogram
#'   frames. Default `"template"` (one template length); a non-negative
#'   whole number sets it explicitly, `0` disables suppression.
#' @param min_score,min_quant,top_n,scope Detection filters. `min_score`
#'   (absolute score floor in `[0, 1]`),
#'   `min_quant` (score-quantile floor), `top_n` (keep at most N by score) and
#'   `scope` (`"pair"` default, applied within each pair, or `"grid"`, across the
#'   whole grid). `NULL` filters keep everything.
#' @param dtw_slack Numeric warping-window slack fraction for `score_method =
#'   "dtw"` (the experimental engine). Default `0.2`. Ignored by
#'   `"cor"`/`"fft"`.
#' @param require_template_id Logical. When `TRUE` (default), a `df_grid` with
#'   no usable `template_id` (column absent or all `NA`) is an error, because
#'   the resulting detections would carry `NA` and could not be joined or
#'   filtered downstream. Set `FALSE` for the fallback used before 1.2.0: a
#'   warning, then the durable id falls back to the file path.
#' @param pb Logical, default `TRUE`. Show the progress bar. Set `FALSE` for
#'   non-interactive pipelines, such as rendering rmarkdown reports.
#'
#' @return When `output_db = NULL`, a detections `data.frame` (standard
#'   detections format, one row per score peak) or, when `output = "scores"`, a
#'   scores tibble (one row per pair carrying its per-frame `score_vec`). When a
#'   database is written, the same object is returned invisibly after persisting.
#'
#' @seealso [fetch_match_grid()] (previous step), [fetch_score_peaks()]
#'   (next step), [plot_scores()].
#' @export
#' @examples
#' \dontrun{
#' # Load the package
#' library(monitoraSom)
#' # Step 7: score a small soundscape-template grid into detections.
#' # (Build a tiny grid from a synthesized recording; see
#' # [fetch_match_grid()] for the full recipe. The bundled df_grid dataset
#' # predates template_id and cannot feed run_matching().)
#' rec_dir <- file.path(tempdir(), "recs"); dir.create(rec_dir, showWarnings = FALSE)
#' # A 20 s soundscape: one clear 4 kHz burst (the template), a longer copy of
#' # it, a fainter copy, and a 5.5 kHz distractor burst.
#' sr <- 16000
#' set.seed(7)
#' t <- seq_len(20 * sr) / sr
#' x <- rnorm(length(t), 0, 0.02)                       # low noise floor
#' m4 <- function(tt) 0.6 * sin(2 * pi * 4000 * tt)
#' m55 <- function(tt) 0.5 * sin(2 * pi * 5500 * tt)
#' x[(t >= 1) & (t < 1.4)]   <- x[(t >= 1) & (t < 1.4)] + m4(t[(t >= 1) & (t < 1.4)])
#' x[(t >= 3) & (t < 4.2)]   <- x[(t >= 3) & (t < 4.2)] + m4(t[(t >= 3) & (t < 4.2)])
#' x[(t >= 7) & (t < 7.4)]   <- x[(t >= 7) & (t < 7.4)] + 0.25 * m4(t[(t >= 7) & (t < 7.4)])
#' x[(t >= 4.5) & (t < 5.1)] <- x[(t >= 4.5) & (t < 5.1)] + m55(t[(t >= 4.5) & (t < 5.1)])
#' rec <- tuneR::normalize(tuneR::Wave(x, samp.rate = sr, bit = 16), unit = "16")
#' tuneR::writeWave(rec, file.path(rec_dir, "siteA_01.wav"))
#' df_rois <- data.frame(
#'   soundscape_path = file.path(rec_dir, "siteA_01.wav"),
#'   soundscape_file = "siteA_01.wav",
#'   roi_label = "burst", roi_start = 0.8, roi_end = 1.2,
#'   roi_min_freq = 3, roi_max_freq = 5, roi_wl = 512, roi_ovlp = 50,
#'   stringsAsFactors = FALSE)
#' out_dir <- file.path(tempdir(), "templates")
#' export_templates(df_rois, templates_path = out_dir, create_dir = TRUE)
#' df_grid <- fetch_match_grid(
#'   fetch_soundscape_metadata(rec_dir),
#'   fetch_template_metadata(out_dir))
#' detections <- run_matching(df_grid, score_method = "fft")
#' head(detections)
#' nrow(detections)                       # how many detections passed
#' hist(detections$peak_score)            # where the scores sit: the strong
#'                                        # 4 kHz hits near 1, the fainter copy
#'                                        # and the distractor lower
#'
#' # Thresholds can also be applied on the fly (see [fetch_score_peaks()] for
#' # re-filtering saved scores without re-running the engine):
#' detections_top <- run_matching(df_grid, score_method = "fft", top_n = 5)
#' nrow(detections_top)
#' detections_strong <- run_matching(df_grid, score_method = "fft",
#'                                   min_score = 0.6)
#' nrow(detections_strong)                # the three 4 kHz hits (the faint
#'                                        # copy still clears 0.6); the
#'                                        # distractor does not
#'
#' # Switch the engine: time-warping-tolerant DTW instead of FFT correlation.
#' detections_dtw <- run_matching(df_grid, score_method = "dtw")
#'
#' # Scores live only in DuckDB. For CSV consumers, export the detections and
#' # re-import the same file to confirm the CSV mirrors the DuckDB contents:
#' det_db <- file.path(tempdir(), "det.duckdb")
#' det_csv <- file.path(tempdir(), "det.csv")
#' run_matching(df_grid, score_method = "fft", output_db = det_db)
#' export_detections_duckdb_to_csv(det_db, det_csv, overwrite = TRUE)
#' df_csv <- utils::read.csv(det_csv)
#' head(df_csv)
#' }
run_matching <- function(
  df_grid, score_method = "fft", ncores = 1, output = "detections",
  output_db = NULL, autosave_action = "replace",
  scores_format = "duckdb", stream_threshold_s = 300,
  buffer_size = "template", min_score = NULL, min_quant = NULL, top_n = NULL,
  scope = c("pair", "grid"), dtw_slack = 0.2,
  require_template_id = TRUE, pb = TRUE
) {
  scope <- match.arg(scope)
  output <- match.arg(output, c("detections", "scores"))
  autosave_action <- match.arg(autosave_action, c("append", "replace"))
  score_method <- match.arg(score_method, c("cor", "fft", "dtw"))
  scores_format <- match.arg(scores_format)
  ncores <- .validate_ncores(ncores)
  .validate_df_grid(df_grid)
  if (!is.numeric(stream_threshold_s) || length(stream_threshold_s) != 1L ||
      is.na(stream_threshold_s) || stream_threshold_s <= 0) {
    stop("`stream_threshold_s` must be a positive number of seconds.")
  }
  if (!is.logical(pb) || length(pb) != 1L || is.na(pb)) {
    stop("`pb` must be a single TRUE or FALSE.")
  }
  if (!is.null(output_db)) .validate_output_db(output_db, output, scores_format)
  # FEAT-07: warn once when persisting to an unmarked explicit path.
  if (!is.null(output_db)) {
    .require_explicit_workspace(output_db, label = "output_db",
                                caller = "run_matching")
  }
  # AFL-25: a grid with no usable template_id makes downstream joins/filters
  # impossible (detections would carry NA). Promote to a hard error by default;
  # `require_template_id = FALSE` keeps the legacy warn + file-path fallback.
  if (!("template_id" %in% names(df_grid)) || all(is.na(df_grid$template_id))) {
    if (isTRUE(require_template_id)) {
      stop("`df_grid` has no usable `template_id` (column absent or all NA): ",
           "detections would carry NA and downstream joins/filters would be ",
           "impossible. Propagate `template_id` from the template database, or ",
           "set `require_template_id = FALSE` to fall back to the file-path key.")
    }
    message("`df_grid` has no `template_id`; detections will carry NA for it ",
            "(propagate it from the template database for a clean join).")
  }
  estimate_matching(df_grid)                # AFL-09: print the plan, then proceed
  .warn_stft_resolution(df_grid)            # AFL-18: one-time coarse-resolution warning

  scores <- .run_matching_scores(df_grid, score_method, ncores, dtw_slack,
                                 stream_threshold_s, pb = pb)

  if (output == "scores") return(.finish_scores(scores, output_db, scores_format))
  detections <- .scores_to_detections(scores, buffer_size, min_score,
                                       min_quant, top_n, scope)
  .finish_detections(detections, output_db, autosave_action)
}

#' Estimate the size of a matching run before launching it
#'
#' @description Reports how big the next [run_matching()] call will be: the number
#'   of pairs, soundscapes and templates, and, when `soundscape_duration` is on
#'   the grid, the total soundscape audio to scan. Purely informational:
#'   [run_matching()] prints this automatically and proceeds with no confirmation
#'   gate, so call it directly only when you want a standalone dry-run estimate.
#'
#' @details Use it to sanity-check a grid before committing to a long run: a
#'   surprising pair count usually means the grid crossed more templates or
#'   soundscapes than intended. It reads only the grid, never the audio, so it is
#'   instant.
#'
#' @section Pipeline context:
#'   Companion to step 7. Reads the search grid from [fetch_match_grid()]
#'   (step 6). Produces run-size figures only (no pipeline artifact).
#'
#' @param df_grid A [fetch_match_grid()] result.
#' @param quiet Logical. When `TRUE`, return the figures without printing them.
#'   Default `FALSE`.
#' @return Invisibly, a one-row `data.frame` with `n_pairs`, `n_soundscapes`,
#'   `n_templates` and `total_scan_seconds` (`NA` when `soundscape_duration` is
#'   absent from the grid).
#' @seealso [fetch_match_grid()], [run_matching()], [summarise_matching()].
#' @export
#' @examples
#' \dontrun{
#' # Dry-run size estimate for a grid, before scoring it.
#' data(df_grid)
#' estimate_matching(df_grid)
#' }
estimate_matching <- function(df_grid, quiet = FALSE) {
  .validate_df_grid(df_grid)
  n_pairs       <- nrow(df_grid)
  n_soundscapes <- length(unique(df_grid$soundscape_path))
  # Prefer the durable template_id; fall back to the (always-present) path.
  tmpl_key <- if ("template_id" %in% names(df_grid) &&
                  !all(is.na(df_grid$template_id))) {
    df_grid$template_id
  } else {
    df_grid$template_path
  }
  n_templates <- length(unique(tmpl_key))
  total_scan_seconds <- if ("soundscape_duration" %in% names(df_grid)) {
    sum(as.numeric(df_grid$soundscape_duration), na.rm = TRUE)
  } else {
    NA_real_
  }
  if (!quiet) {
    message(sprintf(
      "Matching plan: %d pair(s) = %d soundscape(s) x %d template(s).",
      n_pairs, n_soundscapes, n_templates))
    if (!is.na(total_scan_seconds)) {
      message(sprintf(
        "  Total soundscape audio to scan: %.0f s (%.1f min) across pairs.",
        total_scan_seconds, total_scan_seconds / 60))
    }
  }
  invisible(data.frame(
    n_pairs = n_pairs, n_soundscapes = n_soundscapes,
    n_templates = n_templates, total_scan_seconds = total_scan_seconds
  ))
}

#' Summarise what a matching run found (and what it missed)
#'
#' @description Cross-tabulates a run's detections against the [fetch_match_grid()]
#'   search grid to show what the run found and, crucially, what it did **not**:
#'   the pairs, templates and soundscapes that yielded no detection (silent).
#'   Purely informational; it does not modify the detections.
#'
#' @details Run it on a `output = "detections"` result to spot templates or
#'   soundscapes that came back empty, often a sign the search band, threshold
#'   or template choice needs revisiting, not that the target is truly absent.
#'   Zeros
#'   are reported explicitly so silent pairs cannot hide.
#'
#' @section Pipeline context:
#'   Companion to step 7. Reads the detections from [run_matching()] plus the
#'   grid from [fetch_match_grid()]. Produces summary tables only (no pipeline
#'   artifact).
#'
#' @param detections A [run_matching()] / [template_matching()] detections
#'   `data.frame` (standard detections format).
#' @param df_grid The [fetch_match_grid()] grid the run was launched from.
#' @param quiet Logical. When `TRUE`, return the tables without printing them.
#'   Default `FALSE`.
#' @return Invisibly, a named list of tibbles: `per_pair` (every planned
#'   soundscape-template pair with its detection count, zeros included),
#'   `per_template` and `per_soundscape` (detection totals + silent-pair counts).
#' @seealso [fetch_match_grid()], [run_matching()], [estimate_matching()].
#' @export
#' @examples
#' \dontrun{
#' # After a run, see which pairs came back silent.
#' # (Build a tiny grid from a synthesized recording; see
#' # [fetch_match_grid()] for the full recipe. The bundled df_grid dataset
#' # predates template_id and cannot feed run_matching().)
#' rec_dir <- file.path(tempdir(), "recs"); dir.create(rec_dir, showWarnings = FALSE)
#' # Two soundscapes with different 4 kHz content: a clear burst (the template)
#' # and a fainter copy. The summary shows which pairs came back silent.
#' sr <- 16000
#' set.seed(7)
#' tt <- seq_len(10 * sr) / sr
#' x1 <- rnorm(length(tt), 0, 0.02)
#' x1[(tt >= 1) & (tt < 1.4)] <- x1[(tt >= 1) & (tt < 1.4)] +
#'   0.6 * sin(2 * pi * 4000 * tt[(tt >= 1) & (tt < 1.4)])
#' rec1 <- tuneR::normalize(tuneR::Wave(x1, samp.rate = sr, bit = 16), unit = "16")
#' tuneR::writeWave(rec1, file.path(rec_dir, "siteA_01.wav"))
#' x2 <- rnorm(length(tt), 0, 0.02)
#' rec2 <- tuneR::normalize(tuneR::Wave(x2, samp.rate = sr, bit = 16), unit = "16")
#' tuneR::writeWave(rec2, file.path(rec_dir, "siteA_02.wav"))
#' df_rois <- data.frame(
#'   soundscape_path = file.path(rec_dir, "siteA_01.wav"),
#'   soundscape_file = "siteA_01.wav",
#'   roi_label = "burst", roi_start = 0.8, roi_end = 1.2,
#'   roi_min_freq = 3, roi_max_freq = 5, roi_wl = 512, roi_ovlp = 50,
#'   stringsAsFactors = FALSE)
#' out_dir <- file.path(tempdir(), "templates")
#' export_templates(df_rois, templates_path = out_dir, create_dir = TRUE)
#' df_grid <- fetch_match_grid(
#'   fetch_soundscape_metadata(rec_dir),
#'   fetch_template_metadata(out_dir))
#' detections <- run_matching(df_grid, score_method = "fft")
#' summ <- summarise_matching(detections, df_grid)
#' head(summ$per_template)      # siteA_02 pairs come back silent
#' }
summarise_matching <- function(detections, df_grid, quiet = FALSE) {
  .validate_df_grid(df_grid)
  key <- c("soundscape_path", "template_path")
  if (nrow(detections) > 0L && !all(key %in% names(detections))) {
    stop("`detections` must carry `soundscape_path` and `template_path`.")
  }
  pairs <- dplyr::distinct(dplyr::as_tibble(df_grid[, key, drop = FALSE]))
  counts <- if (nrow(detections) > 0L) {
    dplyr::count(dplyr::as_tibble(detections[, key, drop = FALSE]),
                 soundscape_path, template_path, name = "n_detections")
  } else {
    tibble::tibble(soundscape_path = character(0),
                   template_path = character(0), n_detections = integer(0))
  }
  per_pair <- dplyr::left_join(pairs, counts, by = key)
  per_pair$n_detections[is.na(per_pair$n_detections)] <- 0L

  per_template <- dplyr::summarise(
    dplyr::group_by(per_pair, template_path),
    n_detections = sum(n_detections), n_pairs = dplyr::n(),
    n_silent_pairs = sum(n_detections == 0L), .groups = "drop")
  per_soundscape <- dplyr::summarise(
    dplyr::group_by(per_pair, soundscape_path),
    n_detections = sum(n_detections), n_pairs = dplyr::n(),
    n_silent_pairs = sum(n_detections == 0L), .groups = "drop")

  if (!quiet) {
    message(sprintf(
      "Matching summary: %d detection(s) over %d pair(s); %d silent pair(s).",
      sum(per_pair$n_detections), nrow(per_pair),
      sum(per_pair$n_detections == 0L)))
    message(sprintf(
      "  Silent: %d/%d template(s), %d/%d soundscape(s).",
      sum(per_template$n_detections == 0L), nrow(per_template),
      sum(per_soundscape$n_detections == 0L), nrow(per_soundscape)))
  }
  invisible(list(per_pair = per_pair, per_template = per_template,
                 per_soundscape = per_soundscape))
}

# AFL-18: a STFT window length `wl` sets the frequency-bin width (sample_rate / wl).
# If a template's target band spans fewer than this many bins, the spectral
# resolution is too coarse for reliable matching — warn once up front. A larger
# `wl` (finer frequency resolution) is the usual fix.
.RMB_MIN_BAND_BINS <- 2

# One-time coarse-resolution warning over the whole grid (AFL-18). Uses the
# soundscape (else template) sample rate; silently no-ops when no sample rate is
# present on the grid (hand-built grids need not carry it).
.warn_stft_resolution <- function(df_grid) {
  sr <- if ("soundscape_sample_rate" %in% names(df_grid)) {
    df_grid$soundscape_sample_rate
  } else if ("template_sample_rate" %in% names(df_grid)) {
    df_grid$template_sample_rate
  } else {
    return(invisible(FALSE))
  }
  bin_hz  <- sr / df_grid$template_wl
  band_hz <- (df_grid$template_max_freq - df_grid$template_min_freq) * 1000
  n_bins  <- band_hz / bin_hz
  bad <- !is.na(n_bins) & n_bins < .RMB_MIN_BAND_BINS
  if (any(bad)) {
    warning(sprintf(
      paste0("%d grid pair(s) have a target frequency band spanning < %g bin(s) ",
             "at the chosen window length (wl); the spectral resolution may be too ",
             "coarse for reliable matching. Consider a larger `wl`."),
      sum(bad), .RMB_MIN_BAND_BINS), call. = FALSE)
  }
  invisible(any(bad))
}

# --- validation helpers (RMB-01/07/10/12) -------------------------------------

.validate_ncores <- function(ncores) {
  if (!is.numeric(ncores) || length(ncores) != 1L || is.na(ncores) || ncores < 1) {
    stop("`ncores` must be a positive integer.")
  }
  avail <- parallel::detectCores()
  if (is.na(avail)) avail <- 1L            # RMB-10: NA on restricted cgroups
  if (ncores > avail) {
    stop("`ncores` (", ncores, ") cannot exceed the available cores (", avail, ").")
  }
  as.integer(ncores)
}

.validate_df_grid <- function(df_grid) {
  if (!is.data.frame(df_grid) || nrow(df_grid) == 0L) {
    stop("`df_grid` must be a non-empty data.frame (a fetch_match_grid result).")
  }
  missing <- setdiff(.RUN_MATCHING_REQUIRED_COLS, names(df_grid))
  if (length(missing) > 0L) {
    stop("`df_grid` is missing required column(s): ",
         paste(missing, collapse = ", "), ".")
  }
  invisible(TRUE)
}

.validate_output_db <- function(output_db, output, scores_format) {
  dir <- dirname(output_db)
  if (!dir.exists(dir)) stop("The output_db directory does not exist: ", dir)
  invisible(TRUE)
}

# --- persistence finishers ----------------------------------------------------

# Persist detections to the signals store (F3 of the signals program): the
# rows enter as signal_class = "detection" via .detections_as_signals(), so
# signal_id == detection_id (join-stable) and det_* stays filled.
.finish_detections <- function(detections, output_db, autosave_action) {
  if (is.null(output_db)) {
    message("Template matching finished. Detections returned to the R session.")
    return(detections)
  }
  con <- .signals_duckdb_connect(output_db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .signals_duckdb_upsert(con, .detections_as_signals(detections),
                         replace = autosave_action == "replace")
  message("Template matching finished. Detections upserted to ", output_db, ".")
  invisible(detections)
}

.finish_scores <- function(scores, output_db, scores_format) {
  if (is.null(output_db)) {
    message("Template matching finished. Raw scores returned to the R session.")
    return(scores)
  }
  .scores_write_duckdb(output_db, scores, replace = TRUE)
  message("Template matching finished. Raw scores saved to ", output_db, ".")
  invisible(scores)
}
