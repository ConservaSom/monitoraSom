#'Gather ROI tables from a directory
#'
#' @param path Path to directory containing ROI tables
#' @param recursive Search recursively for roi tables, default is FALSE
#'
#' @return A data frame with ROI tables
#' @export
fetch_rois_n <- function(path, recursive = FALSE) {

  if (!dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  roi_name_pattern <- "_roi_.*\\.csv$"

  ls_roi_tables <- list.files(
    path, pattern = roi_name_pattern, recursive = recursive, full.names = TRUE
  ) %>%
    gsub("//", "/", .)

  if (length(ls_roi_tables) == 0) {
    stop("No ROI tables found in path: ", path)
  }

  df_rois <- map_dfr(
    ls_roi_tables, ~ read.csv(.x) %>%
      mutate(
        roi_path = .x, roi_file = basename(.x),
        .after = "soundscape_file"
      ),
    .progress = TRUE
  )

  return(df_rois)
}
