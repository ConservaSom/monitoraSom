#' Convert detections to ROIs
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function converts detections from template matching or external sources
#'   into ROIs (Regions of Interest) in the 'monitoraSom' format.
#'
#' @param input A data frame containing detections or path to a CSV file with
#'   detections in the 'monitoraSom' format.
#' @param output Path where the ROIs will be saved. If NULL, ROIs will be saved
#'   in "./roi_from_detecs/" in the current working directory.
#' @param username A character string identifying the user creating the ROIs.
#'
#' @return A data frame containing the ROI information.
#' @import dplyr
#' @importFrom stringr str_split
#' @export
detecs_to_rois <- function(input = df_detecs, output = NULL, username = NULL) {

    if (is.null(username)) {
        stop("Please provide a username")
    }
    username <- gsub("[^a-zA-Z0-9\\. ]", "", username)

    if (is.data.frame(input)) {
        df_input <- input
    } else {
        if (!file.exists(input)) {
            stop("File not found")
        }
        if (file.info(input)$isdir) {
            stop("The provided path is a directory. Please provide a CSV file.")
        }
        df_input <- read.csv(input)
        message("File read successfully")
    }

    required_cols <- c(
        "soundscape_path", "soundscape_file", "template_path",
        "template_file", "template_name", "template_min_freq",
        "template_max_freq", "template_start", "template_end",
        "detection_start", "detection_end", "detection_wl",
        "detection_ovlp", "detection_sample_rate", "detection_buffer",
        "detec_min_score", "detec_min_quant", "detec_top_n", "peak_index",
        "peak_score", "peak_quant"
    )

    missing_cols <- required_cols[!required_cols %in% colnames(df_input)]

    if (length(missing_cols) > 0) {
        stop(
            "The input data frame is missing the following variables: ",
            paste(missing_cols, collapse = ", ")
        )
    }

    output_path <- if (is.null(output)) {
        "./roi_from_detecs/"
    } else {
        output
    }

    df_input_proc <- df_input %>%
        dplyr::mutate(
            soundscape_path = soundscape_path,
            soundscape_file = soundscape_file,
            roi_file = paste0(
                gsub("\\.wav$", "", basename(soundscape_file)),
                "_roi_", username, "_", format(Sys.time(), "%Y%m%d%H%M%S"),
                ".csv"
            ),
            roi_user = username,
            roi_input_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            roi_label = tail(
                stringr::str_split(
                    gsub("\\.wav$", "", template_name), "_"
                )[[1]], 1
            ),
            roi_start = detection_start,
            roi_end = detection_end,
            roi_min_freq = template_min_freq,
            roi_max_freq = template_max_freq,
            roi_type = "detection",
            roi_label_confidence = if ("validation" %in% names(df_input)) {
                ifelse(validation == "TP", "certain", NA)
            } else {
                NA
            },
            roi_is_complete = NA,
            roi_comment = paste(
                sprintf("peak_score=%s", peak_score),
                sprintf("peak_quant=%s", peak_quant),
                sprintf("detec_min_score=%s", detec_min_score),
                sprintf("detec_min_quant=%s", detec_min_quant),
                sprintf("detec_top_n=%s", detec_top_n),
                sprintf("peak_index=%s", peak_index),
                sprintf("detection_buffer=%s", detection_buffer),
                sep = "|"
            ),
            roi_wl = detection_wl,
            roi_ovlp = detection_ovlp,
            roi_sample_rate = detection_sample_rate,
            roi_pitch_shift = 1
        ) %>%
        dplyr::mutate(
            roi_path = file.path(output_path, roi_file)
        ) %>%
        dplyr::select(
            soundscape_path, soundscape_file, roi_path, roi_file, roi_user,
            roi_input_timestamp, roi_label, roi_start, roi_end, roi_min_freq,
            roi_max_freq, roi_type, roi_label_confidence, roi_is_complete,
            roi_comment, roi_wl, roi_ovlp, roi_sample_rate, roi_pitch_shift
        ) %>%
        dplyr::group_split(roi_path)

    if (!dir.exists(output_path)) {
        stop("The provided output path does not exist")
    } else {
        lapply(df_input_proc, function(x) {
            write.csv(x, file = unique(x$roi_path), row.names = FALSE)
        })
        message("ROIs tables successfully exported to ", output_path)
    }
    return(df_input_proc)
}
