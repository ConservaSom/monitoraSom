#' Capture detections from a single match-score vector (internal)
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Internal engine: [fetch_score_peaks()] is the public interface of this
#'   step. Finds the peaks of **one** match-score vector (the per-frame similarity
#'   trace) produced for a single soundscape-template pair by
#'   [run_matching_i()] (or one row of [run_matching()]), and returns them as
#'   rows of the standard **detections** table. Each peak is one candidate
#'   occurrence of the template in the recording. This is the per-pair core
#'   that the batch entry point [fetch_score_peaks()] calls once per pair.
#'
#' @details
#'   A monitoraSom user can think of the score vector as a numeric time series:
#'   one value per spectrogram frame. Peak capture proceeds in three steps.
#'   First it finds the robust local maxima (a flat plateau counts as a
#'   single peak at its centre, and `NA`s never qualify). Then it trims peaks
#'   too close to the edges to have a valid time window. Finally it runs a
#'   greedy non-maximum suppression (NMS): no two surviving detections sit
#'   within `buffer_size` frames of each other. On a conflict the higher
#'   score wins.
#'
#'   This step is deliberately **pure capture**: it applies no score, quantile
#'   or top-n threshold. Thresholds live in the separate
#'   `filter_detections_i()`, so you can capture once and then re-filter
#'   cheaply. Use this function directly only when you already hold a single
#'   per-pair score vector and want its peaks; for a whole `scores` table use
#'   [fetch_score_peaks()], and for the end-to-end path let [run_matching()]
#'   or [template_matching()] handle it.
#'
#'   Things to know: (1) `buffer_size` is an exclusion buffer counted in
#'   spectrogram frames, not a duration in seconds. The `"template"` default
#'   equals the template's frame count, which is usually what you want. (2)
#'   The reported detection time is read straight from `time_vec`, so a time
#'   axis that is not monotone, or does not match the vector, is rejected;
#'   otherwise peaks would map to wrong times. (3) `peak_quant` is an
#'   empirical quantile computed over the valid (non-padding) region of this
#'   one vector, so it is comparable within a template-soundscape pair but
#'   not necessarily across pairs.
#'
#' @section Pipeline context:
#'   Step 8 of the monitoraSom analysis flow (per-pair core). Reads one
#'   match-score vector from [run_matching_i()] (or a single row of
#'   [run_matching()]'s `scores` output). Produces candidate detection rows
#'   in the standard detections format.
#'
#' @param df_scores_i One row of [run_matching()]'s `scores` output, or the
#'   [run_matching_i()] result: a list or one-row tibble carrying `score_vec` (a
#'   list-column whose single element is a `data.frame(time_vec, score_vec)`),
#'   `score_sliding_window`, `score_method`, and the template/soundscape metadata
#'   columns. `template_id` is used when present (else `NA`). There is no
#'   default: pass the object you want to capture peaks from.
#' @param buffer_size Exclusion buffer, in spectrogram frames, within which
#'   overlapping detections are suppressed (greedy NMS keeps the higher score).
#'   Default `"template"` uses the template's own frame count
#'   (`score_sliding_window`), a sensible non-overlap rule. Pass a
#'   non-negative whole number to set the buffer explicitly; `0` disables
#'   suppression and keeps every robust local maximum, that is, every local
#'   peak with no merging and no filtering at all. A larger value yields
#'   fewer, more spread-out detections.
#'
#' @return A `data.frame` in the standard detections format (26 columns): one
#'   row per captured peak (columns include `detection_start`/`detection_end` in
#'   seconds, `peak_index`, `peak_score`, `peak_quant`, `detection_id`), or a
#'   typed zero-row frame when no peak survives. The filter columns
#'   (`detection_min_score`, `detection_min_quant`, `detection_top_n`) are `NA`
#'   here and are filled later by `filter_detections_i()`.
#'
#' @seealso [run_matching_i()] (previous step), [fetch_score_peaks()] (the batch
#'   wrapper), `filter_detections_i()`, [detecs_to_rois()] (next step).
#' @keywords internal
#' @examples
#' \dontrun{
#' # Step 8 (per-pair core): capture peaks from a single match-score vector.
#' # Build a tiny synthetic score vector with two clear peaks (no files needed).
#' n <- 200L
#' score <- 0.1 + 0.8 * exp(-((seq_len(n) - 60)^2) / 50) +
#'                0.6 * exp(-((seq_len(n) - 140)^2) / 50)
#' df_scores_i <- data.frame(score_sliding_window = 20L)
#' df_scores_i$score_vec <- list(data.frame(
#'   time_vec = seq_len(n) / 100, score_vec = score))
#'
#' dets <- fetch_score_peaks_i(df_scores_i)
#' dets[, c("detection_start", "detection_end", "peak_score", "peak_quant")]
#'
#' # Disable the exclusion buffer to keep every robust local maximum:
#' fetch_score_peaks_i(df_scores_i, buffer_size = 0)
#' }
fetch_score_peaks_i <- function(df_scores_i, buffer_size = "template") {
  sc <- .validate_scores_contract(df_scores_i)            # FSP-13
  data <- sc$score_vec
  time <- sc$time_vec
  sliding_window <- sc$sliding_window
  min_points <- .resolve_buffer(buffer_size, sliding_window)  # FSP-07

  captured <- .capture_peaks_i(data, time, sliding_window, min_points)
  .assemble_detections_i(df_scores_i, captured, min_points)   # FSP-12/15
}

#' Validate the per-pair score contract (FSP-13)
#' @return list(score_vec, time_vec, sliding_window) on success; stops otherwise.
#' @keywords internal
#' @noRd
.validate_scores_contract <- function(df_scores_i) {
  # `[[` (not `$`) so a missing column on a tibble returns NULL silently.
  sv <- if (is.list(df_scores_i)) df_scores_i[["score_vec"]] else NULL
  inner <- if (!is.null(sv)) sv[[1]] else NULL
  if (!is.list(df_scores_i) || is.null(inner) ||
      is.null(inner[["score_vec"]]) || is.null(inner[["time_vec"]]) ||
      is.null(df_scores_i[["score_sliding_window"]])) {
    stop("Invalid `df_scores_i`: needs `score_vec[[1]]$score_vec`, ",
         "`score_vec[[1]]$time_vec` and `score_sliding_window`.")
  }
  if (length(inner[["score_vec"]]) != length(inner[["time_vec"]])) {
    stop("`score_vec` (", length(inner[["score_vec"]]), ") and `time_vec` (",
         length(inner[["time_vec"]]), ") must have equal length (FSP-13).")
  }
  .validate_time_vec(inner[["time_vec"]])                 # AFL-14
  list(score_vec = inner[["score_vec"]], time_vec = inner[["time_vec"]],
       sliding_window = df_scores_i[["score_sliding_window"]])
}

# AFL-14: the detection time of a peak is read directly as `time_vec[peak_index]`,
# so the time axis must be well-formed, not merely the same length as the scores
# (FSP-13 checked only length). A non-monotone or NA `time_vec` would map peaks to
# wrong/undefined times; an irregular step signals a `score_vec`/`time_vec` built
# from mismatched spectrogram parameters. Strict monotonicity is a hard error;
# step irregularity is a warning (real spectro axes are uniform up to float error).
.FSP_TIME_STEP_TOL <- 1e-3   # relative step deviation tolerated before warning
.validate_time_vec <- function(time_vec) {
  if (length(time_vec) < 2L) return(invisible(TRUE))      # nothing to compare
  if (anyNA(time_vec)) {
    stop("`time_vec` contains NA; detection times would be undefined (AFL-14).")
  }
  d <- diff(time_vec)
  if (any(d <= 0)) {
    stop("`time_vec` must be strictly increasing; ", sum(d <= 0),
         " non-increasing step(s) found (AFL-14) -- peaks would map to wrong times.")
  }
  step <- stats::median(d)
  if (step > 0 && max(abs(d - step)) > .FSP_TIME_STEP_TOL * step) {
    warning("`time_vec` has an irregular step (max deviation ",
            signif(max(abs(d - step)) / step * 100, 3),
            "% of the median); `score_vec`/`time_vec` may come from mismatched ",
            "spectrogram parameters (AFL-14).", call. = FALSE)
  }
  invisible(TRUE)
}

#' Resolve `buffer_size` to a non-negative integer exclusion radius (FSP-07)
#' @keywords internal
#' @noRd
.resolve_buffer <- function(buffer_size, sliding_window) {
  if (length(buffer_size) != 1L) {
    stop("`buffer_size` must be a single value ('template' or a number).")
  }
  if (is.character(buffer_size)) {
    if (!identical(buffer_size, "template")) {
      stop("`buffer_size` must be 'template' or a non-negative number.")
    }
    return(as.integer(sliding_window))
  }
  # AUD-43: require a whole number so buffer_size = 10.7 no longer silently
  # truncates to radius 10.
  if (!is.numeric(buffer_size) || is.na(buffer_size) || buffer_size < 0 ||
      buffer_size != trunc(buffer_size)) {
    stop("`buffer_size` must be 'template' or a non-negative whole number.")
  }
  as.integer(buffer_size)
}

#' Robust, plateau-aware local maxima of a numeric vector (FSP-01/02/03/06/14)
#'
#' @description A run of equal consecutive values that is strictly higher than
#'   its nearest differing neighbours on both real sides counts as **one** peak,
#'   located at the run's centre. `NA`s break runs and are never peaks. This
#'   replaces `diff(sign(diff(.)))` (which split plateaus and could index `NA`).
#' @param x numeric score vector.
#' @return integer vector of peak indices (ascending).
#' @keywords internal
#' @noRd
.robust_local_maxima <- function(x) {
  n <- length(x)
  if (n < 2L) return(integer(0))
  peaks <- integer(0)
  i <- 1L
  while (i <= n) {
    if (is.na(x[i])) {
      i <- i + 1L
      next
    }
    j <- i
    while (j < n && !is.na(x[j + 1L]) && x[j + 1L] == x[i]) j <- j + 1L
    left_smaller  <- i > 1L && !is.na(x[i - 1L]) && x[i - 1L] < x[i]
    right_smaller <- j < n  && !is.na(x[j + 1L]) && x[j + 1L] < x[i]
    left_larger   <- i > 1L && !is.na(x[i - 1L]) && x[i - 1L] > x[i]
    right_larger  <- j < n  && !is.na(x[j + 1L]) && x[j + 1L] > x[i]
    if ((left_smaller || right_smaller) && !left_larger && !right_larger) {
      peaks <- c(peaks, (i + j) %/% 2L)
    }
    i <- j + 1L
  }
  peaks
}

#' Greedy non-maximum suppression by raw score (FSP-05/06)
#'
#' @description Sort candidates by score (desc; ties by lowest index), accept a
#'   peak only when no already-accepted peak lies within `radius` frames. Yields
#'   an order-independent guarantee: no two detections sit within `radius`, and on
#'   conflict the higher score wins.
#' @param scores numeric candidate scores; `idx` their (ascending) indices.
#' @param radius integer exclusion radius (`0` keeps all).
#' @return integer vector of kept indices (ascending).
#' @keywords internal
#' @noRd
.greedy_nms <- function(scores, idx, radius) {
  if (radius <= 0L || length(idx) <= 1L) return(sort(idx))
  ord <- order(scores, decreasing = TRUE)   # ties keep ascending-idx order
  accepted <- integer(0)
  for (k in ord) {
    if (length(accepted) == 0L || all(abs(idx[k] - accepted) > radius)) {
      accepted <- c(accepted, idx[k])
    }
  }
  sort(accepted)
}

#' Capture peaks of one (padded) score vector -- the pure numeric core.
#' @return data.frame(peak_index, peak_score, peak_quant, detection_start,
#'   detection_end) -- zero rows when none survive.
#' @keywords internal
#' @noRd
.capture_peaks_i <- function(data, time, sliding_window, min_points) {
  n <- length(data)
  pad_length <- sliding_window %/% 2L

  cand <- .robust_local_maxima(data)
  # FSP-04: trim by the indexing reach so detection_start/end stay in range.
  cand <- cand[cand - pad_length >= 1L & cand + pad_length <= n]
  if (length(cand) == 0L) return(.empty_capture())

  kept <- .greedy_nms(data[cand], cand, min_points)        # FSP-05/06

  # FSP-09: ECDF over the VALID (non-padding) region, not the padded vector.
  pad_right <- sliding_window - pad_length - 1L
  valid_idx <- seq.int(pad_length + 1L, n - pad_right)
  quant <- round(stats::ecdf(data[valid_idx])(data[kept]), 3)

  data.frame(
    peak_index      = kept,
    peak_score      = data[kept],
    peak_quant      = quant,
    detection_start = time[kept - pad_length],
    detection_end   = time[kept + pad_length],
    stringsAsFactors = FALSE
  )
}

#' Zero-row capture frame (the numeric core's empty case).
#' @keywords internal
#' @noRd
.empty_capture <- function() {
  data.frame(
    peak_index = integer(0), peak_score = numeric(0), peak_quant = numeric(0),
    detection_start = numeric(0), detection_end = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Assemble captured peaks into the canonical detections schema (FSP-12/15)
#' @keywords internal
#' @noRd
.assemble_detections_i <- function(df_scores_i, captured, min_points) {
  if (nrow(captured) == 0L) return(.schema_detections(0L))   # typed empty (FSP-12)
  pick <- function(col) if (!is.null(df_scores_i[[col]])) df_scores_i[[col]] else NA
  template_id  <- df_scores_i[["template_id"]] %||% NA_character_
  score_method <- df_scores_i[["score_method"]] %||% NA_character_

  df <- data.frame(
    soundscape_path       = pick("soundscape_path"),
    soundscape_file       = pick("soundscape_file"),
    template_id           = template_id,
    template_path         = pick("template_path"),
    template_file         = pick("template_file"),
    template_name         = pick("template_name"),
    template_label        = pick("template_label"),   # AFL-04: carry the template database label
    template_min_freq     = pick("template_min_freq"),
    template_max_freq     = pick("template_max_freq"),
    template_start        = pick("template_start"),
    template_end          = pick("template_end"),
    score_method          = score_method,
    detection_start       = captured$detection_start,
    detection_end         = captured$detection_end,
    detection_wl          = pick("template_wl"),
    detection_ovlp        = pick("template_ovlp"),
    detection_sample_rate = pick("template_sample_rate"),
    detection_buffer      = min_points,
    detection_min_score   = NA_real_,      # filled by filter_detections_i()
    detection_min_quant   = NA_real_,
    detection_top_n       = NA_integer_,
    peak_index            = captured$peak_index,
    peak_score            = captured$peak_score,
    peak_quant            = captured$peak_quant,
    soundscape_sha256     = pick("soundscape_sha256"),  # AFL-23: carried for the
                                                        # store's stale-source flag
    stringsAsFactors      = FALSE
  )
  det <- .coerce_detections(df)            # canonical order/types (26 cols)
  ch <- if (is.null(df_scores_i[["soundscape_channel"]])) NULL
        else rep(df_scores_i[["soundscape_channel"]], nrow(det))
  det$detection_id <- .detection_id(
    det$soundscape_path, det$template_id, det$peak_index, det$score_method,
    channel = ch
  )
  det
}
