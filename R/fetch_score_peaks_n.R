#' Batch find and filter detections from score vectors
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function is wrapper of 'fetch_score_peaks_i' for detection of peaks in
#'   score vectors from matches between multiple templates and soundscapes, i.e.
#'   the output of 'match_n'. The also allows multiple methods for filtering
#'   suboptimal detections.
#'
#' @param tib_match A tibble containing the output of 'match_n' or a filtered
#'   subset of it with at least two rows that is already within the environement
#'   of the current R session. Alternatively, a path to a folder containing the
#'   output of 'match_n' as .rds files for importing multiple match objects from
#'   outside the current R session.
#' @param buffer_size A numeric value specifying the number of frames of the
#'   buffer within which overlap between detections is avoided. Defaults to
#'   "template", which means that the buffer size equals the number of frames
#'   present in the template spectrogram. The buffer exclusion priority is
#'   oriented by score quantiles, so that the highest scoring detections are
#'   always kept. Setting the buffer size to 0 disables the exclusion buffer.
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
#' @param save_res Character. Path to save the result as an .rds file.
#' @param recursive Set file search to recursive
#'
#' @return A Tibble containing the detections of all audio scores.
#'
#'
#' @export
fetch_score_peaks_n <- function(
    tib_match, recursive = FALSE, buffer_size = "template", min_score = NULL,
    min_quant = NULL, top_n = NULL, save_res = NULL) {

  if (is.character(tib_match) & length(tib_match) == 1) {
    if (!dir.exists(tib_match)) {
      stop("The path provided does not exist")
    } else {
      rds_list <- list.files(
        path = tib_match, pattern = ".rds",
        full.names = TRUE
      )
      if (length(rds_list) == 0) {
        stop("The path provided does not contain any .wav.rds files")
      } else {
        tib_match <- map_dfr(rds_list, readRDS)
      }
    }
  } else if (tibble::is_tibble(tib_match)) {
    if (nrow(tib_match) < 2) {
      stop("The tibble provided has less than two rows, use 'fetch_score_peaks_i()' instead")
    }
  } else {
    stop("The input is not a tibble or a path to a folder containing '.rds' files")
  }

  tib_detecs <- tib_match |>
    rowwise() |>
    group_split() |>
    map(
      ~ fetch_score_peaks_i(
        .x,
        buffer_size = buffer_size, min_score = min_score,
        min_quant = min_quant, top_n = top_n
      )
    ) |>
    list_rbind()

  if (!is.null(save_res)) saveRDS(tib_detecs, save_res)

  message("Detections extracted from scores")

  return(tib_detecs)
}
