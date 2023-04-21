#' Title
#'
#' @param match_res_i
#' @param buffer_size
#'
#' @return
#' @export
#'
#' @examples
fetch_score_peaks_i <- function(match_res_i, buffer_size = "template") {

  data <- match_res_i$score_vec[[1]]$score_vec

  if (buffer_size == "template") {
    min_points <- match_res_i$score_sliding_window
  } else {
    min_points <- buffer_size
  }

  # if (type == "valley") data <- -data
  slope_diffs <- diff(sign(diff(data, na.pad = FALSE)))
  peak_locations <- sapply(
    which(slope_diffs < 0),
    FUN = function(x) {
      start_idx <- x - min_points + 1
      start_idx <- ifelse(start_idx > 0, start_idx, 1)
      end_idx <- x + min_points + 1
      end_idx <- ifelse(end_idx < length(data), end_idx, length(data))
      if (all(data[c(start_idx:x, (x + 2):end_idx)] <= data[x + 1])) {
        return(x + 1)
      } else {
        return(numeric(0))
      }
    }
  ) |> unlist()

  # Controls wether peaks result in detections that are out of the recording.
  # ? The alternative would be allowing partial detections, but this would require a more complex approach to set its time limits

  peak_locations <- peak_locations[
    c(
      which(
        as.integer(peak_locations - match_res_i$score_sliding_window) > 0 &
          as.integer(peak_locations + match_res_i$score_sliding_window) <
          nrow(match_res_i$score_vec[[1]])
      )
    )
  ]
  pad_length <- match_res_i$score_sliding_window %/% 2
  res <- data.frame(
    peak_index = peak_locations,
    peak_cor = match_res_i$score_vec[[1]]$score_vec[peak_locations],
    peak_quant = round(
      ecdf(
        match_res_i$score_vec[[1]]$score_vec
      )(
        match_res_i$score_vec[[1]]$score_vec[peak_locations]), 3
    )
  ) %>%
    mutate(
      soundscape_path = match_res_i$soundscape_path,
      soundscape_file = match_res_i$soundscape_file,
      template_path = match_res_i$template_path,
      template_file = match_res_i$template_file,
      detection_start = as.vector(
        match_res_i$score_vec[[1]]$time_vec[peak_locations - as.integer(pad_length)]
      ),
      detection_end = as.vector(
        match_res_i$score_vec[[1]]$time_vec[peak_locations + as.integer(pad_length)]
      ),
      min_freq = match_res_i$template_min_freq,
      max_freq = match_res_i$template_max_freq,
      detec_wl = match_res_i$template_wl,
      detec_ovlp = match_res_i$template_ovlp,
      detec_sample_rate = match_res_i$template_sample_rate,
      detec_buffer = min_points,
      .before = peak_index
    )
  return(res)
