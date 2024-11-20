#' Batch find and filter detections from score vectors
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function is wrapper of 'fetch_score_peaks_i' for detection of peaks in
#'   score vectors from matches between multiple templates and soundscapes, i.e.
#'   the `scores` output of 'match_n'.
#'
#' @param tib_match A tibble containing the `scores` output of 'match_n' or a
#'   filtered subset of it with at least two rows that is already within the
#'   environement of the current R session. Alternatively, a path to a folder
#'   containing the output of 'match_n' as .rds files for importing multiple
#'   match objects from outside the current R session.
#' @param recursive Set file search to recursive.
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
#' @param save_res Character. Path to save the result as an .rds or .csv file.
#'   Defaults to NULL, which does not save the result.
#' @param ncores An integer indicating the number of cores to be used for
#'   parallelization. Default is 1.
#'
#' @return A Tibble containing the detections of all audio scores.
#'
#' @import tibble dplyr purrr parallel pbapply
#' @export
fetch_score_peaks_n <- function(
    tib_match, recursive = FALSE, buffer_size = "template", min_score = NULL,
    min_quant = NULL, top_n = NULL, save_res = NULL, ncores = 1) {
  require(dplyr)

  # Check if input is a character path or a tibble
  if (is.character(tib_match)) {
    if (!dir.exists(tib_match)) {
      stop("The path provided does not exist")
    }
    rds_list <- list.files(path = tib_match, pattern = "\\.rds$", full.names = TRUE)
    if (length(rds_list) == 0) {
      stop("The path provided does not contain any .rds files")
    }
    tib_match <- map_dfr(rds_list, readRDS)
  } else if (!tibble::is_tibble(tib_match) || nrow(tib_match) < 2) {
    stop("The input must be a tibble with at least two rows, or a path to a folder containing '.rds' files")
  }

  # Setup parallel processing if applicable
  if (ncores > 1 && Sys.info()["sysname"] == "Windows") {
    ncores <- min(ncores, parallel::detectCores())
    ncores <- parallel::makePSOCKcluster(getOption("cl.cores", ncores))
  } else {
    ncores <- 1
  }

  # Process each row of tib_match
  split_data <- split(tib_match, seq(nrow(tib_match)))
  result_list <- pblapply(
    split_data,
    function(subset) {
      fetch_score_peaks_i(
        subset,
        buffer_size = buffer_size,
        min_score = min_score,
        min_quant = min_quant,
        top_n = top_n
      )
    },
    cl = ncores
  )
  tib_detecs <- do.call(rbind, result_list)

  # Save results if specified
  if (!is.null(save_res)) {
    file_ext <- tools::file_ext(save_res)
    if (file_ext == "rds") {
      saveRDS(tib_detecs, save_res)
    } else if (file_ext == "csv") {
      write.csv(tib_detecs, save_res, row.names = FALSE)
    } else {
      stop("The file extension is not supported")
    }
  }

  message("Detections extracted from scores")

  return(tib_detecs)
}
