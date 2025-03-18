#' Export ROI cuts
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function exports the cuts of the ROIs in the provided ROI table. The
#'   function will cut the ROIs from the soundscape files and save them in the
#'   specified path.
#'
#' @param df_rois A tibble containing the ROIs to be cut
#' @param roi_cuts_path A character string indicating the path to save the cuts.
#'   We recommend to provide the "040_roi_cuts/" directory, which is the default
#'   path used by the segmentation app.
#' @param overwrite If TRUE, existing files will be overwritten
#'
#' @import dplyr
#' @importFrom tuneR readWave writeWave
#' @importFrom pbapply pblapply
#' @importFrom stringr str_replace str_pad
#'
#' @return A message indicating if all cuts were made successfully
#' @export
export_roi_cuts_n <- function(
    df_rois, roi_cuts_path = "040_roi_cuts/", overwrite = FALSE
    ) {

  if (nrow(df_rois) == 0) {
    stop("No ROIs available in the provided ROI table")
  }
  if (!dir.exists(roi_cuts_path)) {
    stop("Specified path does not exist or is not writable: ", roi_cuts_path)
  }

  rois_list <- dplyr::mutate(
    df_rois,
    cut_name = paste(
      str_replace(soundscape_file, "\\.wav$|\\.WAV$", ""),
        "_",
        stringr::str_pad(sprintf("%.3f", round(roi_start, 3)), 7, pad = "0"),
        "-",
        stringr::str_pad(sprintf("%.3f", round(roi_end, 3)), 7, pad = "0"),
        "s_",
        stringr::str_pad(sprintf("%.3f", round(roi_min_freq, 3)), 6, pad = "0"),
        "-",
        stringr::str_pad(sprintf("%.3f", round(roi_max_freq, 3)), 6, pad = "0"),
        "kHz_", roi_wl,
        "wl_", roi_ovlp,
        "ovlp_", roi_label,
        ".wav",
        sep = ""
      ),
      filename = file.path(roi_cuts_path, cut_name)
    )

  results <- pbapply::pblapply(1:nrow(rois_list), function(i) {
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
        sound <- tuneR::readWave(
          roi_row$soundscape_path,
          from = roi_row$roi_start,
          to = roi_row$roi_end, units = "seconds"
        )
        tuneR::writeWave(sound, roi_row$filename)
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
