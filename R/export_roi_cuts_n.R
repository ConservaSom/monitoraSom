#' Export ROI cuts
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function exports the cuts of the ROIs in the provided ROI table. The
#'   function will cut the ROIs from the soundscape files and save them in the
#'   specified path.
#'
#' @param rois_n A tibble containing the ROIs to be cut
#' @param path A character string indicating the path to save the cuts
#'
#' @import dplyr seewave pbapply stringr
#' @return A message indicating if all cuts were made successfully
#' @export
export_roi_cuts_n <- function(rois_n, path, overwrite = FALSE) {
  if (nrow(rois_n) == 0) {
    stop("No ROIs available in the provided ROI table")
  }
  if (!dir.exists(path)) {
    stop("Specified path does not exist or is not writable: ", path)
  }

  rois_list <- rois_n %>%
    mutate(
      cut_name = paste(
        str_replace(
          soundscape_file, "\\.wav$|\\.WAV$", ""
        ),
        "_",
        str_pad(sprintf("%.3f", round(roi_start, 3)), 7, pad = "0"),
        "-",
        str_pad(sprintf("%.3f", round(roi_end, 3)), 7, pad = "0"),
        "s_",
        str_pad(sprintf("%.3f", round(roi_min_freq, 3)), 6, pad = "0"),
        "-",
        str_pad(sprintf("%.3f", round(roi_max_freq, 3)), 6, pad = "0"),
        "kHz_",
        roi_wl, "wl_", roi_ovlp, "ovlp_", roi_label, ".wav",
        sep = ""
      ),
      filename = file.path(path, cut_name)
    )

  results <- pblapply(1:nrow(rois_list), function(i) {
    roi_row <- rois_list[i, ]
    if (!file.exists(roi_row$soundscape_path)) {
      warning("File does not exist: ", roi_row$soundscape_path)
      return(FALSE)
    }
    if (file.exists(roi_row$filename) && !overwrite) {
      message("Skipping overwrite of existing file: ", roi_row$filename)
      return(FALSE)
    }
    tryCatch(
      {
        sound <- readWave(
          roi_row$soundscape_path,
          from = roi_row$roi_start,
          to = roi_row$roi_end, units = "seconds"
        )
        writeWave(sound, roi_row$filename)
        TRUE
      },
      error = function(e) {
        message(
          "Failed to process: ", roi_row$filename, " Error: ", e$message
        )
        FALSE
      }
    )
  })

  if (all(results)) {
    message("All cuts were made successfully")
  } else {
    message("Some cuts were not made. Check the output for more information")
  }
}
