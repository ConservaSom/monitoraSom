#' roi_razor function
#'
#' This function cuts out regions of interest (ROIs) from a given wave file and
#' saves them as separate wave files. The ROIs are specified in a data frame,
#' and the output files are named based on the start and end times, minimum and
#' maximum frequencies, window length, overlap, and label of each ROI.
#'
#' @param wav A Wave object, representing the audio data to be cut.
#' @param rois A data frame containing the ROIs.
#' @param path A string representing the directory where the output wave files
#'   should be saved.
#'
#' @return This function does not return a value. It saves the cut wave files to
#'   the specified directory.
#'
#' @examples
#' \dontrun{
#' # Load a wave file
#' wav <- tuneR::readWave("path_to_your_wave_file.wav")
#'
#' # Define the ROIs
#' rois <- data.frame(
#'   roi_start = c(0.1, 0.2),
#'   roi_end = c(0.2, 0.3),
#'   roi_min_freq = c(1, 2),
#'   roi_max_freq = c(2, 3),
#'   roi_wl = c(0.01, 0.01),
#'   roi_ovlp = c(0.5, 0.5),
#'   roi_label = c("label1", "label2")
#' )
#'
#' # Cut the ROIs from the wave file
#' roi_razor(wav, rois, "path_to_save_directory")
#' }
#' @export
roi_razor <- function(wav, rois, path) {
  if (is.null(rois)) {
    stop("No ROIs available")
  } else {
    rois_list <- rois %>%
      fmutate(
        cut_name = paste(
          str_replace(
            soundscape_file, ".wav|.WAV",
            paste0(
              "_",
              stringr::str_pad(
                sprintf("%.3f", round(roi_start, 3)), 7, pad = "0"), "-",
              stringr::str_pad(
                sprintf("%.3f", round(roi_end, 3)), 7, pad = "0"), "s_",
              stringr::str_pad(
                sprintf("%.3f", round(roi_min_freq, 3)), 6, pad = "0"), "-",
              stringr::str_pad(
                sprintf("%.3f", round(roi_max_freq, 3)), 6, pad = "0"), "kHz_",
              roi_wl, "wl_", roi_ovlp, "ovlp_",
              roi_label, ".wav"
            )
          )
        )
      ) %>%
      rowwise() %>%
      group_split()

    purrr::map(
      rois_list,
      ~ seewave::cutw(
        wav,
        f = wav@samp.rate, output = "Wave",
        from = .x$roi_start, to = .x$roi_end
      ) %>%
        seewave::savewav(filename = file.path(path, .x$cut_name))
    )
  }
}
