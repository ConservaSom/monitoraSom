#'Gather ROI tables from a directory
#'
#' @param path Path to directory containing ROI tables
#' @param roi_filter Filter ROI tables by comment, default is FALSE
#'
#' @return A data frame with ROI tables
#' @export
#'
fetch_rois_n <- function(path, recursive = FALSE, roi_filter = FALSE) {

  if (!dir.exists(path)) {
    stop("Path does not exist")
  } else {
    ls_roi_tables_raw <- list.files(
      path,
      pattern = "*.csv", recursive = recursive, full.names = TRUE
    ) %>%
      gsub("//", "/", .)
  }

  if (length(ls_roi_tables_raw) == 0) {
    stop("No CSV files found in path")
  } else {
    roi_name_pattern <- "_roi_[[:alnum:]]+_[[:digit:]]{14}\\.csv$"
    file_name_check <- grepl(
      pattern = roi_name_pattern, x = basename(ls_roi_tables_raw)
    )
  }

  if (sum(file_name_check) == 0) {
    stop("No ROI tables found in path")
  } else {
    ls_roi_tables <- ls_roi_tables_raw[file_name_check]

    df_rois <- map_dfr(
      ls_roi_tables, ~ read.csv(.x) %>%
        mutate(
          roi_path = .x, roi_file = basename(.x),
          .after = "soundscape_file"
        )
    )
  }

  if (roi_filter != FALSE & is.character(roi_filter)) {
    df_rois <- df_rois %>%
      filter(
        grepl(pattern = roi_filter, roi_comment, ignore.case = TRUE)
      )
    # test if there are any rows left
    if (nrow(df_rois) == 0) {
      stop("No ROI tables found with the specified filter")
    }
  }

  return(df_rois)
}
