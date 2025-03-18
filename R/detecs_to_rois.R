#' Convert detections to ROIs
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function converts detections from template matching or external
#'   sources into ROIs (Regions of Interest) in the 'monitoraSom' format.
#'
#' @param df_detecs A data frame containing detections or path to a CSV file
#'   with detections in the 'monitoraSom' format.
#' @param output_path Path where the ROIs will be saved. We avise against
#'   providing the same path as the one used to export ROI tables obtained from
#'   the segmentation app.
#' @param username A character string identifying the user creating the ROIs.
#' @param filter_tp Logical indicating whether to filter the detections to only
#'   include true positives. Defaults to FALSE.
#'
#' @return A data frame containing the ROI information.
#' @import dplyr
#' @importFrom stringr str_split
#' @export
#' @examples
#' \dontrun{
#'
#' # Load the necessary packages to run this example
#' library(monitoraSom)
#' library(dplyr)
#'
#' # Load a dataset with validated detections (output of the of validation app)
#' data(df_detecs_val_manual)
#' glimpse(df_detecs_val_manual)
#'
#' # Create a directory to store the roi tables
#' roi_cuts_path <- "./130_rois_from_detections"
#' dir.create(roi_cuts_path)
#'
#' # Convert the detections to rois to a unified roi table within the current R
#' # session and export it as multiple roi tables in the directory created earlier
#' df_detecs_to_rois <- detecs_to_rois(
#'     df_detecs = df_detecs_val_manual, username = "Rosa G.L.M.",
#'     output_path = roi_cuts_path
#' )
#'
#' # Check the object
#' glimpse(df_detecs_to_rois)
#'
#' # Check the roi tables in the directory created earlier
#' list.files(roi_cuts_path, pattern = "*.csv")
#' }
#'
detecs_to_rois <- function(
    df_detecs, username = NULL, output_path = NULL, filter_tp = FALSE
    ) {

    if (is.null(username)) {
        stop("Please provide a username")
    }
    username <- gsub("[^a-zA-Z0-9\\. ]", "", username)

    if (is.data.frame(df_detecs)) {
        df_input <- df_detecs
        if (filter_tp) {
            if ("validation" %in% colnames(df_input)) {
                df_input <- df_input %>%
                    dplyr::filter(validation == "TP")
            } else {
                warning(
                    "Data was not filtered - validation variable not found in input data"
                )
            }
        }
    } else {
        if (!file.exists(df_detecs)) {
            stop("File not found")
        }
        if (file.info(df_detecs)$isdir) {
            stop("The provided path is a directory. Please provide a CSV file.")
        }
        df_input <- read.csv(df_detecs)
        message("File read successfully")
    }

    required_cols <- c(
        "soundscape_path", "soundscape_file", "template_path",
        "template_file", "template_name", "template_min_freq",
        "template_max_freq", "template_start", "template_end",
        "detection_start", "detection_end", "detection_wl",
        "detection_ovlp", "detection_sample_rate", "detection_buffer",
        "detection_min_score", "detection_min_quant", "detection_top_n",
        "peak_index", "peak_score", "peak_quant"
    )

    missing_cols <- required_cols[!required_cols %in% colnames(df_input)]

    if (length(missing_cols) > 0) {
        stop(
            "The input data frame is missing the following variables: ",
            paste(missing_cols, collapse = ", ")
        )
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
                sprintf("detection_min_score=%s", detection_min_score),
                sprintf("detection_min_quant=%s", detection_min_quant),
                sprintf("detection_top_n=%s", detection_top_n),
                sprintf("peak_index=%s", peak_index),
                sprintf("detection_buffer=%s", detection_buffer),
                sep = "|"
            ),
            roi_wl = detection_wl,
            roi_ovlp = detection_ovlp,
            roi_sample_rate = detection_sample_rate,
            roi_pitch_shift = 1
        ) %>%
        dplyr::select(
            soundscape_path, soundscape_file, roi_file, roi_user,
            roi_input_timestamp, roi_label, roi_start, roi_end, roi_min_freq,
            roi_max_freq, roi_type, roi_label_confidence, roi_is_complete,
            roi_comment, roi_wl, roi_ovlp, roi_sample_rate, roi_pitch_shift
        )

    if (is.null(output_path)) {
        message(
            paste(
                "Detections successfully converted to ROIs. Because a path is",
                "not provided, a ROI data frame is returned to the current",
                "R session environment"
            )
        )
    } else {
        if (dir.exists(output_path)) {
            df_input_proc %>%
                dplyr::mutate(roi_path = file.path(output_path, roi_file)) %>%
                dplyr::select(-roi_file) %>%
                dplyr::group_split(roi_path) %>%
                lapply(function(x) {
                    write.csv(x, file = unique(x$roi_path), row.names = FALSE)
                })
            message("ROIs tables successfully exported to ", output_path)
        } else {
            stop("The provided output path to ROI tablesdoes not exist")
        }
    }

    return(df_input_proc)
}
