#' Gather ROI tables from a directory
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function gathers ROI tables from a directory and returns a data frame
#'   with the ROI tables.
#'
#' @param rois_path Path to directory containing ROI tables in CSV format. The
#'   expected variables must be consistent with those exported by the
#'   segmentation app.
#' @param recursive Search recursively for roi tables. Defaults to FALSE.
#'
#' @return A data frame with ROI tables
#'
#' @importFrom pbapply pblapply
#' @importFrom dplyr mutate
#' @export
#' @examples
#' \dontrun{
#'
#' # Load the necessary packages to run this example
#' library(monitoraSom)
#' library(dplyr)
#'
#' # Load the roi tables to populate the example data
#' data(ls_roi_tables)
#'
#' # Create a directory and export the roi tables
#' rois_path <- "./030_roi_tables"
#' dir.create(rois_path)
#' invisible(lapply(1:length(ls_roi_tables), function(i) {
#'   write.csv(
#'     ls_roi_tables[[i]],
#'     file.path(rois_path, names(ls_roi_tables)[i]),
#'     row.names = FALSE
#'   )
#' }))
#'
#' # Import the roi tables as a unified dataframe
#' df_rois <- fetch_rois(rois_path = rois_path)
#' glimpse(df_rois)
#' 
#' }
fetch_rois <- function(rois_path = NULL, recursive = FALSE) {

  rois_path <- if (is.null(rois_path) || !dir.exists(rois_path)) {
    stop("The provided path to the roi tables does not exist")
  } else {
    rois_path
  }

  expected_cols <- list(
    soundscape_path = "character",
    soundscape_file = "character",
    roi_path = "character",
    roi_file = "character",
    roi_user = "character",
    roi_input_timestamp = "character",
    roi_label = "character",
    roi_start = "numeric",
    roi_end = "numeric",
    roi_min_freq = "numeric",
    roi_max_freq = "numeric",
    roi_type = "character",
    roi_label_confidence = "character",
    roi_is_complete = "character",
    roi_comment = "character",
    roi_wl = "integer",
    roi_ovlp = "integer",
    roi_sample_rate = "integer",
    roi_pitch_shift = "integer"
  )

  ls_roi_tables <- normalizePath(
    list.files(
      rois_path,
      pattern = "_roi_.*\\.csv$",
      recursive = recursive,
      full.names = TRUE
    ),
    mustWork = FALSE
  )

  if (length(ls_roi_tables) == 0) {
    stop("No ROI tables found in path: ", rois_path)
  }

  read_roi_fun <- function(x) {
    tryCatch({
      df <- read.csv(
        x, colClasses = "character", stringsAsFactors = FALSE,
        na.strings = c("", "NA", "NULL")
      )

      missing_cols <- setdiff(names(expected_cols), names(df))
      if (length(missing_cols) > 0) {
        df[missing_cols] <- NA_character_
      }
      numeric_cols <- c("roi_start", "roi_end", "roi_min_freq", "roi_max_freq")
      integer_cols <- c(
        "roi_wl", "roi_ovlp", "roi_sample_rate", "roi_pitch_shift"
      )

      df[numeric_cols] <- lapply(df[numeric_cols], function(x) {
        as.numeric(ifelse(x == "", NA, x))
      })
      df[integer_cols] <- lapply(df[integer_cols], function(x) {
        as.integer(ifelse(x == "", NA, x))
      })

      df$roi_path <- x
      df$roi_file <- basename(x)
      df[names(expected_cols)]
    },
    error = function(e) {
      message(sprintf("Error processing ROI file: %s\nError: %s", x, e$message))
      NULL
    })
  }

  df_rois <- do.call(
    rbind, pblapply(ls_roi_tables, read_roi_fun)
  )

  valid_entries <- !vapply(df_rois, is.null, logical(1))
  if (!any(valid_entries)) {
    warning("No valid ROI tables could be read from the files found")
    return(NULL)
  }

  df_rois <- df_rois[valid_entries, ]
  return(df_rois)
}
