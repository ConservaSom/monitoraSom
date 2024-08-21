#' Export ROI cuts
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function takes in a ROI table and export its ROIs as audio files in the
#' specified path. The ROI table should be in the format produced by the
#' segmentation app.
#'
#' @param rois_n A data frame in the format produced by the segmentation app
#' @param path A character string with the path to save the cuts.
#'
#' @return A message indicating if all cuts were made successfully
export_roi_cuts_n <- function(rois_n, path) {

    if (nrow(rois_n) == 0) {
        stop("No ROIs available in the provided ROI table")
    } else {
        rois_list <- rois_n %>%
            mutate(
                cut_name = paste(
                    str_replace(
                        soundscape_file, ".wav|.WAV",
                        paste0(
                            "_",
                            stringr::str_pad(
                                sprintf("%.3f", round(roi_start, 3)), 7,
                                pad = "0"
                            ), "-",
                            stringr::str_pad(
                                sprintf("%.3f", round(roi_end, 3)), 7,
                                pad = "0"
                            ), "s_",
                            stringr::str_pad(
                                sprintf("%.3f", round(roi_min_freq, 3)), 6,
                                pad = "0"
                            ), "-",
                            stringr::str_pad(
                                sprintf("%.3f", round(roi_max_freq, 3)), 6,
                                pad = "0"
                            ), "kHz_",
                            roi_wl, "wl_", roi_ovlp, "ovlp_",
                            roi_label, ".wav"
                        )
                    )
                ),
                filename = file.path(path, cut_name)
            )
    }

    dump <- rois_list %>%
        rowwise() %>%
        group_split() %>%
        purrr::map(
            ., function(x) {
                readWave(
                    x$soundscape_path,
                    from = x$roi_start, to = x$roi_end, units = "seconds"
                ) %>%
                    writeWave(., x$filename)
            },
            .progress = TRUE
        )

    # identify if all cuts were made
    if (all(file.exists(rois_list$filename))) {
        message("All cuts were made successfully")
    } else {
        message("Some cuts were not made. Check the output for more information")
    }
}