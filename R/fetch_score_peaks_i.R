#' Find and filter detections from one score vector
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function detects peaks in the score vector resulting from one
#'   iteration of template matching, i.e. the output of 'match_i()' or one row
#'   of the output of 'match_n()' . The peaks represent potential detections of
#'   a template in a soundscape recording.
#'
#' @param match_res_i One row of the output of the function 'match_n()' or the
#'   output of 'match_i()', which contain score vector the result of a template
#'   matching operation performed with a specific template and soundscape
#'   recording.
#' @param buffer_size A numeric value specifying the number of frames of the
#'   buffer within which overlap between detections is avoided. Defaults to
#'   "template", which means that the buffer size equals the number of frames
#'   present in the template spectrogram. The buffer exclusion is oriented by
#'   score quantiles, so that the highest scoring detections are always kept.
#'   Setting the buffer size to 0 disables the exclusion buffer.
#' @param min_score A numeric value between 0 and 0.99 indicating the minimum
#'   score of the detections that will be kept. Defaults to NULL, which returns
#'   all available detections.
#' @param min_quant A numeric value between 0 and 1 indicating the minimum score
#'   quantile of the kept detections. Defaults to NULL, which returns all
#'   available detections.
#' @param top_n An integer indicating the maximum number of peaks to be
#'   returned, selected according to the highest scores available. Defaults to
#'   NULL, which return all available detections. It should be noted that
#'   because the peak quantiles are callculated within each score vector, the
#'   top_n parameter is applied to each score vector separately, and not to the
#'   whole matching grid.
#'
#' @return A data frame in which each row is a detection and has the follwing
#'   attributes:
#' \describe{
#'   \item \code{soundscape_path} {A character string specifying the full path to the soundscape WAV file.}
#'   \item \code{soundscape_file} {A character string specifying the name of the soundscape WAV file.}
#'   \item \code{template_path} {A character string specifying the complete path to the soundscape WAV files.}
#'   \item \code{template_file} {A character string specifying the name of the soundscape WAV files (without the complete path).}
#'   \item \code{template_min_freq} {The minimum frequency (kHz) of the band used to compute the template spectrogram.}
#'   \item \code{template_max_freq} {The maximum frequency (kHz) of the band used to compute the template spectrogram.}
#'   \item \code{detection_start} {The start time (s) of the detection in the soundscape recording. Based on the template duration.}
#'   \item \code{detection_end} {The end time (s) of the detection in the soundscape recording. Based on the template duration.}
#'   \item \code{detec_wl} {The FFT window length used to generate the spectrograms.}
#'   \item \code{detec_ovlp} {The overlap between windows used generate the spectrograms.}
#'   \item \code{detec_sample_rate} {The sample rate of the soundscape and template WAV files.}
#'   \item \code{detec_buffer} {The value of 'buffer_size'.}
#'   \item \code{detec_min_score} {The value of 'min_score'.}
#'   \item \code{detec_min_quant} {The value of 'min_quant'.}
#'   \item \code{detec_top_n} {The value of 'top_n'.}
#'   \item \code{peak_index} {A numeric value representing the index of the score peak in the original score vector.}
#'   \item \code{peak_cor} {A numeric value between 0 and 1 with the detection score.}
#'   \item \code{peak_quant} {The quantile of the peak value in the score vector.}
#' }
#'
#' @export
#'
#' @examples
#' fetch_score_peaks_i(match_res_i, 10)
fetch_score_peaks_i <- function(
  match_res_i, buffer_size = "template", min_score = NULL, min_quant = NULL,
  top_n = NULL
  ) {

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
    peak_score = match_res_i$score_vec[[1]]$score_vec[peak_locations],
    peak_quant = round(
      ecdf(
        match_res_i$score_vec[[1]]$score_vec
      )(
        match_res_i$score_vec[[1]]$score_vec[peak_locations]), 3
    )
  ) %>%
    fmutate(
      soundscape_path = match_res_i$soundscape_path,
      soundscape_file = match_res_i$soundscape_file,
      template_path = match_res_i$template_path,
      template_file = match_res_i$template_file,
      template_min_freq = match_res_i$template_min_freq,
      template_max_freq = match_res_i$template_max_freq,
      detection_start = as.vector(
        match_res_i$score_vec[[1]]$time_vec[peak_locations - as.integer(pad_length)]
      ),
      detection_end = as.vector(
        match_res_i$score_vec[[1]]$time_vec[peak_locations + as.integer(pad_length)]
      ),
      detection_wl = match_res_i$template_wl,
      detection_ovlp = match_res_i$template_ovlp,
      detection_sample_rate = match_res_i$template_sample_rate,
      detection_buffer = min_points,
      detec_min_score = NA,
      detec_min_quant = NA,
      detec_top_n = NA
    ) %>%
    fselect(
      soundscape_path, soundscape_file, template_path, template_file,
      template_min_freq, template_max_freq, detection_start, detection_end,
      detection_wl, detection_ovlp, detection_sample_rate, detection_buffer,
      detec_min_score, detec_min_quant, detec_top_n, peak_index, peak_score,
      peak_quant
    )

    if (!is.null(min_score)) {
      if (is.numeric(min_score) & min_score >= 0 & min_score <= 1) {
        res_temp <- fsubset(res, peak_score >= min_score) |>
          fmutate(detec_min_score = min_score)
        if (nrow(res) == 0) {
          # todo Verificar se é essa a solução adequada para o problema
          warning("No detections found with the specified min_score, returning all available detections instead")
          res <- fsubset(res, peak_score >= 0)
        } else {
          res <- res_temp
        }
      } else {
        stop("min_score must be a numeric value between 0 and 1")
      }
    }

    if (!is.null(min_quant)) {
      if (is.numeric(min_quant) & min_quant >= 0 & min_quant <= 1) {
        res_temp <- fsubset(res, peak_quant >= min_quant) |>
          fmutate(detec_min_quant = min_quant)
        if (nrow(res) == 0) {
          res <- fsubset(res, peak_quant >= 0)
          # todo Verificar se é essa a solução adequada para o problema
          warning("No detections found with the specified min_quant, returning all available detections instead")
        } else {
          res <- res_temp
        }
      } else {
        stop("min_quant must be a numeric value between 0 and 1")
      }
    }

    if (!is.null(top_n)) {
      if (is.numeric(top_n)) {
        if (top_n >= 1) {
          if (top_n <= nrow(res)) {
            res <- res %>%
              arrange(-peak_quant) %>%
              slice(1:top_n) %>%
              arrange(peak_index) %>%
              fmutate(detec_top_n = top_n)
          } else {
            res <- res %>%
              fmutate(detec_top_n = top_n)
            warning(
              "top_n must be smaller than the number of detections, returning all available detections instead"
            )
          }
        } else {
          stop("top_n must be equal or larger than 1")
        }
      } else {
        stop("top_n must be a numeric value")
      }
    }

  return(res)
}
