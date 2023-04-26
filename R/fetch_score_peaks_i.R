#' Find score peaks
#'
#' This function detects peaks in the score vector resulting from a template matching operation.
#' The peaks represent potential detections of a template in a soundscape recording.
#'
#' @param match_res_i A list containing the result of a template matching operation performed with a specific template and soundscape recording. It must contain at least the following elements:
#' \describe{
#'   \item{\code{score_vec}}{A list containing the score vector obtained from the matching operation.}
#'   \item{\code{score_sliding_window}}{The size of the sliding window used in the matching operation.}
#'   \item{\code{soundscape_path}}{The path to the soundscape recording.}
#'   \item{\code{soundscape_file}}{The name of the soundscape recording file.}
#'   \item{\code{template_path}}{The path to the template used in the matching operation.}
#'   \item{\code{template_file}}{The name of the template file.}
#'   \item{\code{template_min_freq}}{The minimum frequency of the template.}
#'   \item{\code{template_max_freq}}{The maximum frequency of the template.}
#'   \item{\code{template_wl}}{The window length used in the template matching operation.}
#'   \item{\code{template_ovlp}}{The overlap between windows used in the template matching operation.}
#'   \item{\code{template_sample_rate}}{The sample rate of the template.}
#' }
#'
#' @param buffer_size An integer indicating the minimum number of points required to define a peak. Defaults to "template", which sets it equal to the sliding window size of the matching operation.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{\code{peak_index}}{The index of the peak in the score vector.}
#'   \item{\code{peak_cor}}{The value of the peak.}
#'   \item{\code{peak_quant}}{The quantile of the peak value in the score vector.}
#'   \item{\code{soundscape_path}}{The path to the soundscape recording.}
#'   \item{\code{soundscape_file}}{The name of the soundscape recording file.}
#'   \item{\code{template_path}}{The path to the template used in the matching operation.}
#'   \item{\code{template_file}}{The name of the template file.}
#'   \item{\code{detection_start}}{The start time of the detection in the soundscape recording.}
#'   \item{\code{detection_end}}{The end time of the detection in the soundscape recording.}
#'   \item{\code{min_freq}}{The minimum frequency of the template.}
#'   \item{\code{max_freq}}{The maximum frequency of the template.}
#'   \item{\code{detec_wl}}{The window length used in the detection process.}
#'   \item{\code{detec_ovlp}}{The overlap between windows used in the detection process.}
#'   \item{\code{detec_sample_rate}}{The sample rate of the soundscape recording.}
#'   \item{\code{detec_buffer}}{The minimum number of points required to define a peak.}
#' }
#'
#' @export
#'
#' @examples
#' fetch_score_peaks_i(match_res_i, 10)
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
}
