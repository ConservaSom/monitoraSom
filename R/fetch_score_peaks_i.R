#' Find and filter detections from one score vector
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'  This function detects peaks in the score vector resulting from one iteration
#'  of template matching, i.e. the output of 'match_i()' or one row of the
#'  output of 'match_n()' . The peaks represent potential detections of a
#'  template in a soundscape recording.
#'
#' @param match_res_i One row of the output of the function 'match_n()' or the
#'  output of 'match_i()', which contain score vector the result of a template
#'  matching operation performed with a specific template and soundscape
#'  recording.
#' @param buffer_size A numeric value specifying the number of frames of the
#'  buffer within which overlap between detections is avoided. Defaults to
#'  "template", which means that the buffer size equals the number of frames
#'  present in the template spectrogram. The buffer exclusion is oriented by
#'  score quantiles, so that the highest scoring detections are always kept.
#'  Setting the buffer size to 0 disables the exclusion buffer.
#' @param min_score A numeric value between 0 and 0.99 indicating the minimum
#'  score of the detections that will be kept. Defaults to NULL, which returns
#'  all available detections.
#' @param min_quant A numeric value between 0 and 1 indicating the minimum score
#'  quantile of the kept detections. Defaults to NULL, which returns all
#'  available detections.
#' @param top_n An integer indicating the maximum number of peaks to be returned,
#'  selected according to the highest scores available. Defaults to NULL, which
#'  return all available detections. It should be noted that because the peak
#'  quantiles are callculated within each score vector, the top_n parameter is
#'  applied to each score vector separately, and not to the whole matching grid.
#'
#' @return A data frame in which each row is a detection and has the follwing
#'  attributes:
#'
#' @import dplyr purrr
#' @export
fetch_score_peaks_i <- function(
    match_res_i, buffer_size = "template", min_score = NULL, min_quant = NULL,
    top_n = NULL) {
  if (
    !is.list(match_res_i) || is.null(match_res_i$score_vec) ||
      is.null(match_res_i$score_vec[[1]]$score_vec) ||
      is.null(match_res_i$score_sliding_window)
  ) {
    stop("Invalid match_res_i: missing required components")
  }

  if (
    !is.null(min_score) && (!is.numeric(min_score) || min_score < 0 ||
      min_score > 1
    )
  ) {
    stop("min_score must be a numeric value between 0 and 1")
  }
  if (
    !is.null(min_quant) && (!is.numeric(min_quant) || min_quant < 0 ||
      min_quant > 1
    )
  ) {
    stop("min_quant must be a numeric value between 0 and 1")
  }
  if (!is.null(top_n) && (!is.numeric(top_n) || top_n < 1)) {
    stop("top_n must be a positive numeric value")
  }

  data <- match_res_i$score_vec[[1]]$score_vec
  min_points <- if (buffer_size == "template") {
    match_res_i$score_sliding_window
  } else {
    if (!is.numeric(buffer_size) || buffer_size < 0) {
      stop("buffer_size must be 'template' or a non-negative numeric value")
    }
    buffer_size
  }

  diffs <- diff(data, na.pad = FALSE)
  slope_diffs <- diff(sign(diffs))
  potential_peaks <- which(slope_diffs < 0) + 1

  valid_peaks <- sapply(potential_peaks, function(x) {
    start_idx <- max(1, x - min_points)
    end_idx <- min(length(data), x + min_points)
    surrounding_data <- data[c(start_idx:(x - 1), (x + 1):end_idx)]
    all(surrounding_data <= data[x])
  })

  peak_locations <- potential_peaks[valid_peaks]
  valid_range <- peak_locations[
    peak_locations - match_res_i$score_sliding_window > 0 &
      peak_locations + match_res_i$score_sliding_window <
        nrow(match_res_i$score_vec[[1]])
  ]

  if (length(valid_range) == 0) {
    return(data.frame())
  }

  pad_length <- match_res_i$score_sliding_window %/% 2
  score_vec <- match_res_i$score_vec[[1]]$score_vec[valid_range]

  res <- data.frame(
    soundscape_path = match_res_i$soundscape_path,
    soundscape_file = match_res_i$soundscape_file,
    template_path = match_res_i$template_path,
    template_file = match_res_i$template_file,
    template_name = match_res_i$template_name,
    template_min_freq = match_res_i$template_min_freq,
    template_max_freq = match_res_i$template_max_freq,
    template_start = match_res_i$template_start,
    template_end = match_res_i$template_end,
    detection_start = match_res_i$score_vec[[1]]$time_vec[
      valid_range - pad_length
    ],
    detection_end = match_res_i$score_vec[[1]]$time_vec[
      valid_range + pad_length
    ],
    detection_wl = match_res_i$template_wl,
    detection_ovlp = match_res_i$template_ovlp,
    detection_sample_rate = match_res_i$template_sample_rate,
    detection_buffer = min_points,
    detection_min_score = NA_real_,
    detection_min_quant = NA_real_,
    detection_top_n = NA_integer_,
    peak_index = valid_range,
    peak_score = score_vec,
    peak_quant = round(ecdf(data)(score_vec), 3)
  )

  if (!is.null(min_score)) {
    res <- subset(res, peak_score >= min_score)
    if (nrow(res) > 0) res$detection_min_score <- min_score
  }

  if (!is.null(min_quant)) {
    res <- subset(res, peak_quant >= min_quant)
    if (nrow(res) > 0) res$detection_min_quant <- min_quant
  }

  if (!is.null(top_n) && top_n <= nrow(res)) {
    res <- res[order(-res$peak_quant)[1:top_n], ]
    res <- res[order(res$peak_index), ]
    res$detection_top_n <- top_n
  }

  return(res)
}
