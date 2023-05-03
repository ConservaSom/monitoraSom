#' Title
#'
#' description
#'
#' @param tib_match
#' @param tib_match_path
#' @param buffer_size A numeric value specifying the number of frames of the buffer within which overlap between detections is avoided. Defaults to "template", which means that the buffer size equals the number of frames present in the template spectrogram. The buffer exclusion is oriented by score quantiles, so that the highest scoring detections are always kept. Setting the buffer size to 0 disables the exclusion buffer.
#' @param min_score A numeric value between 0 and 0.99 indicating the minimum score of the detections that will be kept. Defaults to NULL, which returns all available detections.
#' @param min_quant A numeric value between 0 and 1 indicating the minimum score quantile of the kept detections. Defaults to NULL, which returns all available detections.
#' @param top_n An integer indicating the maximum number of peaks to be returned, selected according to the highest scores available. Defaults to NULL, which return all available detections. It should be noted that because the peak quantiles are callculated within each score vector, the top_n parameter is applied to each score vector separately, and not to the whole matching grid.
#' @param save_res Character. Path to save the result as an .rds file.
#'
#' @return A Tibble containing the detections of all audio scores.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load example audio scores
#' tib_match <- readRDS(system.file("extdata", "example_audio_scores.rds", package = "my_package"))
#'
#' # Extract detections from scores
#' tib_detecs <- fetch_score_peaks_n(tib_match, buffer_size = 25, save_res = "detections.rds")
#' }
#'
#' @import dplyr
#' @import purrr
#' @importFrom utils saveRDS
#' @seealso \code{\link{match_template_n}}, \code{\link{fetch_score_peaks_i}}
fetch_score_peaks_n <- function(
    tib_match, tib_match_path = NULL, buffer_size = "template", min_score = NULL,
    min_quant = NULL, top_n = NULL, save_res = NULL
    ) {

  if (is.null(tib_match) & !is.null(tib_match_path)) {
    tib_match <- map_dfr(
      list.files(
        path = tib_match_path, pattern = ".wav.rds",
        full.names = TRUE
      ),
      readRDS
    ) %>%
      group_split(rowwise())
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
