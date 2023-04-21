#' Title
#'
#' @param tib_match
#' @param tib_match_path
#' @param buffer_size
#' @param save_res
#'
#' @return
#' @export
#'
#' @examples
fetch_score_peaks_n <- function(
    tib_match, tib_match_path = NULL, buffer_size = "template",
    save_res = NULL) {

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
    map(~ fetch_score_peaks_i(.x, buffer_size = buffer_size)) |>
    list_rbind()

  if (!is.null(save_res)) saveRDS(tib_detecs, save_res)

  message("Detections extracted from scores")

  return(tib_detecs)
}
