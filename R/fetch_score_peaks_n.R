#' Batch find and filter detections from score vectors
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function is wrapper of 'fetch_score_peaks_i' for detection of peaks in
#'   score vectors from matches between multiple templates and soundscapes, i.e.
#'   the `scores` output of 'match_n'.
#'
#' @param df_scores A tibble containing the `scores` output of 'match_n' or a
#'   filtered subset of it with at least two rows that is already within the
#'   environement of the current R session. Alternatively, a path to a folder
#'   containing the output of 'match_n' as .rds files for importing multiple
#'   match objects from outside the current R session.
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
#' @param output_file Character. Path to save the result as an .rds or .csv
#'   file. Defaults to NULL, which does not save the result.
#' @param ncores An integer indicating the number of cores to be used for
#'   parallelization. Default is 1.
#'
#' @return A Tibble containing the detections of all audio scores.
#'
#' @import dplyr
#' @importFrom pbapply pblapply
#' @importFrom parallel makePSOCKcluster detectCores
#' @importFrom tibble is_tibble
#' @importFrom tools file_ext
#' @export
#' @examples
#' \dontrun{
#' 
#' # Load the necessary packages to run this example
#' library(monitoraSom)
#' library(dplyr)
#'
#' # Load the scores (output of `match_n()`)
#' data(df_scores)
#'
#' # Get detections from raw scores
#' df_detecs <- fetch_score_peaks_n(df_scores = df_scores)
#' glimpse(df_detecs)
#'
#' }
fetch_score_peaks_n <- function(
    df_scores, buffer_size = "template", min_score = NULL,
    min_quant = NULL, top_n = NULL, output_file = NULL, ncores = 1) {


  # Check if input is a character path or a tibble
  if (is.character(df_scores)) {
    if (!file.exists(df_scores)) {
      stop("The path to the score RDS file does not exist")
    }
    df_scores <- readRDS(df_scores)
  } else if (!tibble::is_tibble(df_scores) || nrow(df_scores) < 2) {
    stop("The input must be a tibble with at least two rows")
  }

  # Setup parallel processing if applicable
  if (ncores > 1 && Sys.info()["sysname"] == "Windows") {
    ncores <- min(ncores, parallel::detectCores())
    ncores <- parallel::makePSOCKcluster(getOption("cl.cores", ncores))
  } else {
    ncores <- 1
  }

  # Process each row of df_scores
  split_data <- split(df_scores, seq(nrow(df_scores)))
  result_list <- pbapply::pblapply(
    split_data,
    function(subset) {
      fetch_score_peaks_i(
        df_scores_i = subset,
        buffer_size = buffer_size,
        min_score = min_score,
        min_quant = min_quant,
        top_n = top_n
      )
    },
    cl = ncores
  )
  tib_detecs <- do.call(rbind, result_list)
  message("Detections extracted from scores")

  # Handle results based on output_file argument
  if (!is.null(output_file)) {
    # Validate file extension
    if (!grepl("\\.csv$", output_file)) {
      stop("Output file must have .csv extension")
    }

    # Validate directory exists
    output_dir <- dirname(output_file)
    if (!dir.exists(output_dir)) {
      stop("The directory '", output_dir, "' to save the detections does not exist")
    }

    # Save to CSV
    write.csv(tib_detecs, output_file, row.names = FALSE)
    message("Detections have been exported to ", output_file)
  }

  # Always return the results
  return(tib_detecs)
}
