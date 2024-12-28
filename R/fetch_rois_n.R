#' ather ROI tables from a directory
#'
#' @param path Path to directory containing ROI tables
#' @param recursive Search recursively for roi tables, default is FALSE
#'
#' @return A data frame with ROI tables
#'
#' @importFrom pbapply pblapply
#' @export
fetch_rois_n <- function(path, recursive = FALSE) {

  if (!dir.exists(path)) {
    stop("Path does not exist: ", path)
  }

  ls_roi_tables <- gsub(
    "//", "/", list.files(
      path, pattern = "_roi_.*\\.csv$", recursive = recursive, full.names = TRUE
    )
  )

  if (length(ls_roi_tables) == 0) {
    stop("No ROI tables found in path: ", path)
  }

  df_rois <- do.call(
    rbind, pblapply(ls_roi_tables, function(x) {
    read.csv(x) %>%
      mutate(
        roi_path = x, roi_file = basename(x),
        .after = "soundscape_file"
      )
    })
  )

  return(df_rois)
}
