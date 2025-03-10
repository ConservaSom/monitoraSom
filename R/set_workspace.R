#' Set the workspace for the monitoraSom package
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function sets the working directory to the project path and creates
#' the necessary directories if they do not exist. It also creates the species
#' labels file if it doesn't exist.
#'
#' @param project_path The path to the project directory. If NULL, the function
#'   will try to determine the project path automatically based on the path to
#'   the R script that is being executed.
#' @param ext_soundscapes_path Logical indicating whether the soundscapes path
#'   is external to the project directory. If TRUE, the soundscapes directory
#'   will not be created.
#' @param soundscapes_path The path to the soundscapes directory.
#' @param soundscapes_metadata_path The path to the soundscapes metadata
#'   directory.
#' @param roi_tables_path The path to the roi tables directory.
#' @param roi_cuts_path The path to the roi cuts directory.
#' @param detections_path The path to the detections directory.
#' @param validation_outputs_path The path to the validation outputs directory.
#' @param detection_spectrograms_path The path to the detection spectrograms
#'   directory.
#' @param detection_cuts_path The path to the detection cuts directory.
#' @param validation_diagnostics_path The path to the validation diagnostics
#'   directory.
#' @param app_presets_path The path to the app presets directory.
#'
#' @returns
#' @export
#'
#' @examples
set_workspace <- function(
    project_path = NULL, ext_soundscapes_path = FALSE,
    soundscapes_path = "./soundscapes/",
    soundscapes_metadata_path = "./soundscapes_metadata/",
    roi_tables_path = "./roi_tables/", roi_cuts_path = "./roi_cuts/",
    detections_path = "./detections/",
    validation_outputs_path = "./validation_outputs/",
    detection_spectrograms_path = "./detection_spectrograms/",
    detection_cuts_path = "./detection_cuts/",
    validation_diagnostics_path = "./validation_diagnostics/",
    app_presets_path = "./app_presets/"
    ) {

    requireNamespace("rstudioapi")
    requireNamespace("openxlsx")

    if (is.null(project_path)) {
        tryCatch(
            {
                project_path <- dirname(rstudioapi::getSourceEditorContext()$path)
            },
            error = function(e) {
                stop("Could not determine project path automatically. Please provide project_path parameter.")
            }
        )
    }
    setwd(project_path)

    create_directory <- function(path, name, skip_if_external = FALSE) {
        if (skip_if_external && name == "soundscapes") {
            warning("- The soundscapes path is external to the project directory. It will not be created.")
            return()
        }

        if (!dir.exists(path)) {
            tryCatch(
                {
                    dir.create(path)
                    message(sprintf("- The %s directory was created at %s", name, path))
                },
                error = function(e) {
                    stop(sprintf("Failed to create directory at %s: %s", path, e$message))
                }
            )
        } else {
            warning(sprintf("- The %s directory already exists. It will not be overwritten.", name))
        }
    }

    directories <- list(
        list(
            path = soundscapes_path, name = "soundscapes",
            skip_external = ext_soundscapes_path
        ),
        list(path = soundscapes_metadata_path, name = "soundscapes_metadata"),
        list(path = roi_tables_path, name = "roi_tables"),
        list(path = roi_cuts_path, name = "roi_cuts"),
        list(path = app_presets_path, name = "app_presets"),
        list(path = detections_path, name = "detections"),
        list(path = validation_outputs_path, name = "validation_outputs"),
        list(
            path = detection_spectrograms_path, name = "detection_spectrograms"
        ),
        list(path = detection_cuts_path, name = "detection_cuts"),
        list(
            path = validation_diagnostics_path, name = "validation_diagnostics"
        )
    )

    for (dir in directories) {
        create_directory(dir$path, dir$name, dir$skip_external %||% FALSE)
    }

    if (!file.exists(file.path(app_presets_path, "sp_labels.xlsx"))) {
        tryCatch(
            {
                data("sp_labels", package = "monitoraSom", envir = environment())
                write.xlsx(sp_labels, file.path(app_presets_path, "sp_labels.xlsx"))
            },
            error = function(e) {
                warning(sprintf("Failed to create sp_labels.xlsx: %s", e$message))
            }
        )
    } else {
        warning("The labels file already exists. It will not be overwritten.")
    }

    message(
        paste(
            "Workspace set successfully.",
            "Check the workspace and start another R session within the",
            "project to start using the monitoraSom package."
        )
    )
}
