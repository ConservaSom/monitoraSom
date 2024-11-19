#' Import external detections
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function imports detections from an external CSV file and converts
#'   them to the 'monitoraSom' format. The function will rename the variables to
#'   match the 'monitoraSom' format and will add the necessary variables to
#'   complete the format.
#'
#' @param input_path A character string indicating the path to the CSV file
#' @param names_vec A named character vector indicating the names of the
#'   variables and the corresponding names in the provided table.
#' @param output_path A character string indicating the path to save the output
#'
#' @import dplyr
#' @return A tibble containing the detections in the 'monitoraSom' format
#' @export
import_external_detections <- function(
    input_path, names_vec = NA, output_path = NA
    ) {
    if (!file.exists(input_path)) {
        stop("File not found")
    }
    if (file.info(input_path)$isdir) {
        stop("The provided path is a directory. Please provide a CSV file.")
    }

    df_detec_raw <- read.csv(input_path)

    if (is.na(names_vec)) {
        names_vec <- c(
            "template_name" = "scientific_name",
            "detection_start" = "start_time",
            "detection_end" = "end_time",
            "peak_score" = "confidence",
            "soundscape_path" = "filepath",
            "detection_min_score" = "min_conf",
            "bn_lat" = "lat",
            "bn_lon" = "lon",
            "bn_sensitivity" = "sensitivity",
            "bn_model" = "model_name"
        )
    }

    if (!all(names_vec %in% names(df_detec_raw))) {
        stop(
            "There are variables in 'names_vec' that are not present in the provided table."
        )
    }
    if (all(names(names_vec) %in% names(df_detec_raw))) {
        stop("The provided table is already in the 'monitoraSom' format")
    }

    res <- df_detec_raw %>%
        rename(all_of(names_vec)) %>%
        select(names(names_vec))

    if (!all(names(names_vec) %in% names(res))) {
        stop(
            paste0(
                "Thee following variables are missing or were not properly ranamed: ",
                paste(
                    names(names_vec)[which(!(names(names_vec) %in% names(res)))],
                    collapse = ", "
                )
            )
        )
    }

    res <- res %>%
        dplyr::transmute(
            soundscape_path = soundscape_path,
            soundscape_file = basename(soundscape_path),
            template_path = NA_character_,
            template_file = NA_character_,
            template_name = template_name,
            template_min_freq = 0, # mudar para checagem real na gravação
            template_max_freq = 20, # mudar para checagem real na gravação
            template_start = NA_real_,
            template_end = NA_real_,
            detection_start = detection_start,
            detection_end = detection_end,
            detection_wl = NA_real_,
            detection_ovlp = NA_real_,
            detection_sample_rate = NA_real_,
            detection_buffer = NA_real_,
            detection_min_score = detection_min_score,
            detection_min_quant = NA_real_,
            detection_top_n = NA_integer_,
            peak_index = NA_integer_,
            peak_score = peak_score,
            peak_quant = NA_real_,
            bn_lat = bn_lat,
            bn_lon = bn_lon,
            bn_sensitivity = bn_sensitivity,
            bn_model = bn_model
        )

    if (is.na(output_path)) {
        return(res)
    } else if (file.exists(output_path)) {
        stop("The output file already exists. Please provide a new path.")
    } else if (input_path == output_path) {
        stop(
            "The input and output paths are the same. Please provide a new path."
        )
    } else {
        write.csv(res, output_path, row.names = FALSE)

    }
}
