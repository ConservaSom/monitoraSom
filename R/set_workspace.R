#' Set the workspace for the monitoraSom package
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function sets the working directory to the project path and creates
#'   the necessary directories if they do not exist. It also creates the species
#'   labels file if it doesn't exist.
#'
#' @param project_path The path to the project directory. If NULL, the function
#'   will try to determine the project path automatically based on the path to
#'   the R script that is being executed.
#' @param ext_soundscapes_path Logical indicating whether the soundscapes path
#'   is external to the project directory. If TRUE, the soundscapes directory
#'   will not be created.
#' @param app_presets_path The path to the app presets directory.
#' @param soundscapes_path The path to the soundscapes directory.
#' @param soundscapes_metadata_path The path to the soundscapes metadata
#'   directory.
#' @param roi_tables_path The path to the roi tables directory.
#' @param roi_cuts_path The path to the roi cuts directory.
#' @param templates_metadata_path The path to the templates metadata directory.
#' @param match_grid_metadata_path The path to the match grid metadata directory.
#' @param match_scores_path The path to the match scores directory.
#' @param detections_path The path to the detections directory.
#' @param detection_cuts_path The path to the detection cuts directory.
#' @param detection_spectrograms_path The path to the detection spectrograms
#'   directory.
#' @param validation_outputs_path The path to the validation outputs directory.
#' @param validation_diagnostics_path The path to the validation diagnostics
#'   directory.
#' @param example_data Logical indicating whether to populate the workspace with
#'   example data. Defaults to FALSE.
#'
#' @returns Multiple directories at the project path.
#' @importFrom rstudioapi getSourceEditorContext
#' @importFrom openxlsx write.xlsx
#' @importFrom tuneR writeWave
#' @export
set_workspace <- function(
    project_path = NULL, ext_soundscapes_path = FALSE,
    app_presets_path = "./000_app_presets/",
    soundscapes_path = "./010_soundscapes/",
    soundscapes_metadata_path = "./020_soundscapes_metadata/",
    roi_tables_path = "./030_roi_tables/",
    roi_cuts_path = "./040_roi_cuts/",
    templates_metadata_path = "./050_templates_metadata/",
    match_grid_metadata_path = "./060_match_grid_metadata/",
    match_scores_path = "./070_match_scores/",
    detections_path = "./080_detections/",
    detection_cuts_path = "./090_detection_cuts/",
    detection_spectrograms_path = "./100_detection_spectrograms/",
    validation_outputs_path = "./110_validation_outputs/",
    validation_diagnostics_path = "./120_validation_diagnostics/",
    example_data = FALSE
    ) {
    requireNamespace("rstudioapi")
    requireNamespace("openxlsx")

    if (is.null(project_path)) {
        tryCatch(
            {
                project_path <- dirname(
                    rstudioapi::getSourceEditorContext()$path
                )
                usethis::create_project(
                    path = project_path, open = FALSE, rstudio = TRUE
                )
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
        list(path = app_presets_path, name = "app_presets"),
        list(
            path = soundscapes_path, name = "soundscapes",
            skip_external = ext_soundscapes_path
        ),
        list(path = soundscapes_metadata_path, name = "soundscapes_metadata"),
        list(path = templates_metadata_path, name = "templates_metadata"),
        list(path = match_grid_metadata_path, name = "match_grid_metadata"),
        list(path = roi_tables_path, name = "roi_tables"),
        list(path = roi_cuts_path, name = "roi_cuts"),
        list(path = detections_path, name = "detections"),
        list(path = match_scores_path, name = "match_scores"),
        list(path = detection_spectrograms_path, name = "detection_spectrograms"),
        list(path = detection_cuts_path, name = "detection_cuts"),
        list(path = validation_outputs_path, name = "validation_outputs"),
        list(path = validation_diagnostics_path, name = "validation_diagnostics")
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

    if (file.exists(file.path(project_path, ".gitignore"))) {
        file.remove(file.path(project_path, ".gitignore"))
    }
    if (dir.exists(file.path(project_path, "R"))) {
        unlink(file.path(project_path, "R"), recursive = TRUE)
    }

    if (example_data) {
        # soundscapes
        data(ls_soundscapes)
        invisible(
            lapply(1:length(ls_soundscapes), function(i) {
                tuneR::writeWave(
                    ls_soundscapes[[i]],
                    file.path(soundscapes_path, names(ls_soundscapes)[i])
                )
            })
        )

        # templates
        data(ls_templates)
        invisible(
            lapply(1:length(ls_templates), function(i) {
                tuneR::writeWave(
                    ls_templates[[i]],
                    file.path(roi_cuts_path, names(ls_templates)[i])
                )
            })
        )

        # roi tables
        data(ls_roi_tables)
        invisible(
            lapply(1:length(ls_roi_tables), function(i) {
                write.csv(
                    ls_roi_tables[[i]],
                    file.path(roi_tables_path, names(ls_roi_tables)[i])
                )
            })
        )

        # soundscapes metadata
        data(df_soundscapes)
        write.csv(
            df_soundscapes,
            file.path(soundscapes_metadata_path, "df_soundscapes.csv")
        )

        # templates metadata
        data(df_templates)
        write.csv(
            df_templates,
            file.path(templates_metadata_path, "df_templates.csv")
        )

        # match grid metadata
        data(df_grid)
        write.csv(
            df_grid,
            file.path(match_grid_metadata_path, "df_grid.csv")
        )

        # detections
        data(df_detecs)
        write.csv(df_detecs, file.path(detections_path, "df_detecs.csv"))

        # match scores
        data(df_scores)
        saveRDS(df_scores, file.path(match_scores_path, "df_scores.rds"))

        # validated detections
        data(df_detecs_val_manual)
        write.csv(
            df_detecs_val_manual,
            file.path(validation_outputs_path, "df_detecs_val_manual.csv")
        )
        data(df_detecs_val_tovlp)
        write.csv(
            df_detecs_val_tovlp,
            file.path(validation_outputs_path, "df_detecs_val_tovlp.csv")
        )
        message(
            paste(
                "Workspace and example data set successfully.",
                "Check the workspace and start another R session within the",
                "project to start using the monitoraSom package."
            )
        )
    } else {
        message(
            paste(
                "Workspace set successfully.",
                "Check the workspace and start another R session within the",
                "project to start using the monitoraSom package."
            )
        )
    }
}


