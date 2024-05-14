#' Launch validation app
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function launches the validation app, which is a Shiny app to validate
#'   for segemntation of WAV recorcings of soundscapes into tables containing
#'   regions of interest (ROIs) and audio cuts of the ROIs. The app settings can
#'   be imported from presets or set manually.
#'
#' @param preset_path Path from which presets can be imported and to which new
#'   presets can be exported.
#' @param validation_user User name.
#' @param templates_path Path to the template wave files.
#' @param soundscapes_path Path to the soundscape wave files.
#' @param input_path Path to the input file containing detections.
#' @param output_path Path to the output file.
#' @param wav_cuts_path Path to the folder containing the cut wave files.
#' @param spec_path Path to the folder containing the spectrogram images.
#' @param diag_tab_path Path to the folder containing the diagnostic tables.
#' @param wav_player_path Path to the wav player executable (only for system
#'   player).
#' @param wav_player_type The type of wav player. "R session" for R
#'   session-based player, "system" for system player.
#' @param val_subset Subset of detections to be validated.
#' @param min_score Minimum score of the detections to be validated.
#' @param time_pads Time pads to be added to the start and end of the cut wave
#'   files.
#' @param ovlp Overlap between consecutive cuts.
#' @param wl Window length for the spectrogram.
#' @param dyn_range Dynamic range for the spectrogram.
#' @param color_scale Color scale for the spectrogram.
#' @param zoom_freq Frequency range to zoom in the spectrogram.
#' @param nav_shuffle If TRUE, the files will be shuffled before navigation.
#' @param seed Seed for the random shuffling.
#' @param auto_next If TRUE, the next file is automatically displayed when the
#'   user validates a cut.
#' @param nav_autosave If TRUE, the current validation is saved when the user
#'   navigates to another file.
#' @param overwrite If TRUE, the output file is overwritten.
#' @param session_notes Notes related to the validation session.
#'
#' @return todo
#'
#' @export
#' @import shiny dplyr tidyr ggplot2 lubridate seewave stringr tuneR
#'   collapse DT shinyWidgets shinydashboard shinyFiles
#'   keys shinyjs shinyBS cutpointr
#' @importFrom caret downSample upSample
#' @importFrom ROSE ROSE
#' @importFrom data.table fread fwrite
#' @importFrom farver encode_native
#' @importFrom tuneR play setWavPlayer readWave
#' @examples
#' \dontrun{
#' library(monitoraSom)
#' library(usethis)
#'
#' # set the path to the diorectory where soundscapes are located (it is not recursive)
#' soundscapes_path <- "path/to/soundscapes"
#' # set the path to the project folder
#' project_path <- "path/to/project"
#'
#' # in case the current working directory is not an active project, set it with the following command. it may be necessary to restart the R session after that.
#' create_project(path = paste0(project_path, "/MyProject", open = TRUE, rstudio = TRUE)
#'
#' # launch the segmentation app
#' launch_segmentation_app(
#'   project_path = project_path,
#'   user = "Identify the user here",
#'   soundscapes_path = soundscapes_path
#' )
#' }
launch_validation_app <- function(
    project_path = NULL, preset_path = NULL,
    validation_user, templates_path, soundscapes_path, input_path,
    output_path = NULL, spec_path = NULL, wav_cuts_path = NULL, diag_tab_path = NULL,
    wav_player_path = "play", wav_player_type = "HTML player",
    val_subset = c("NA", "TP", "FP", "UN"), min_score = 0,
    time_pads = 1, ovlp = 0, wl = 2048, dyn_range = c(0, 50),
    color_scale = "inferno", zoom_freq = c(0, 10),
    nav_shuffle = FALSE, seed = 123, auto_next = FALSE, nav_autosave = FALSE,
    overwrite = FALSE, pitch_shift = 1, visible_bp = FALSE, play_norm = FALSE
    ) {

      # todo Adicionar informações de pitch_shift

  library(dplyr, warn.conflicts = FALSE)
  # require(tidyr)
  # require(ggplot2)
  # require(lubridate)
  # require(seewave)
  # require(stringr)
  # require(tuneR)
  # require(purrr)
  # require(collapse)
  # require(DT)
  # require(data.table)
  # require(cutpointr)
  # require(caret)
  # require(ROSE)
  # require(viridis)
  # require(farver)
  # require(shiny)
  # require(shinyWidgets)
  # require(shinyjs)
  # require(kableExtra)
  # require(keys)
  # require(shinyFiles)
  # require(shinydashboard)
  # require(shinyBS)

  options(dplyr.summarise.inform = FALSE)

  session_data <- list()

  if (!is.null(project_path)) {
    if (dir.exists(project_path)) {
      session_data$project_path <- project_path
    } else {
      dir.create(project_path)
      if (dir.exists(project_path)) {
        session_data$project_path <- project_path
        warning(
          paste0("The validation app project directory was sucessfully created at '", project_path, "'")
        )
      } else {
        stop("Error! Tried to create the validation app project directory at '", project_path, "' but failed.")
      }
    }
  }

  if (!is.null(preset_path)) {
    if (dir.exists(preset_path)) {
      session_data$preset_path <- preset_path
    } else {
      dir.create(preset_path)
      if (dir.exists(preset_path)) {
        session_data$preset_path <- preset_path
        warning(
          "The segmentation preset destination directory was created automatically at '",
          preset_path, "'"
        )
      } else {
        stop("Error! The selected preset destination folder does not exist and could not be created.")
      }
    }
    # The creation of the temp directory assumes that the preset directory exists
    temp_path <- file.path(preset_path, "temp/")
    if (!dir.exists(temp_path)) {
      dir.create(temp_path)
    }
    session_data$temp_path <- temp_path
  } else if (!is.null(project_path)) {
    # If a project path is defined,
    preset_path <- file.path(project_path, "app_presets/")
    temp_path <- file.path(project_path, "app_presets/temp/")
    if (!dir.exists(preset_path)) {
      dir.create(preset_path)
      dir.create(temp_path)
    }
    session_data$preset_path <- preset_path
    session_data$temp_path <- temp_path
  }

  if (!is.null(validation_user)) {
    session_data$validation_user <- as.character(gsub(",", "", validation_user))
  } else {
    session_data$validation_user <- as.character(NA)
    warning("Warning! A value was not provided for 'validation_user' variable. Inform the correct value and confirm within the app.")
  }

  if (is.null(templates_path)) {
    if (dir.exists("roi_cuts/")) {
      session_data$templates_path <- "roi_cuts/"
      warning(
        "Warning! The path to the template wave files was not provided. Using the default path 'roi_cuts/'"
      )
    }
  } else {
    if (dir.exists(templates_path)) {
      session_data$templates_path <- templates_path
    } else {
      stop("Error! The path to the template wave files was not found locally.")
    }
  }

  session_data$soundscapes_path <- soundscapes_path
  if (!dir.exists(soundscapes_path)) {
    stop("Error! The path to the soundscape wave files was not found locally.")
  }

  session_data$input_path <- input_path
  if (!file.exists(input_path)) {
    stop("Error! The input file containing detections was not found locally.")
    # todo Add other checks for the input file
  }

  if (is.null(output_path)) {
    session_data$output_path <- input_path
    warning(
      "Warning! The output file was not provided. Using the input file as output."
    )
  } else {
    session_data$output_path <- output_path
    if (!file.exists(output_path)) {
      warning("Warning! The output file was not found locally. It will be created automatically.")
    }
  }

  if (all(val_subset %in% c("NA", "TP", "FP", "UN"))) {
    session_data$val_subset <- val_subset
  } else {
    stop(
      "Error! At least one of the values assigned to 'val_subset' are not within the accepted alternatives ('NA', 'TP', 'FP', 'UN'). Observe that 'NA' is handled as a character string in this function"
    )
  }

  # todo Adicionar os outros filtros de detecções
  if (is.numeric(min_score)) {
    if (0 <= min_score & min_score < 1) {
      session_data$min_score <- min_score
    } else {
      stop(
        "Error! The value assigned to 'min_score' is not within the expected interval. Provide a numeric value equal to or higher than zero and smaller than one"
      )
    }
  } else {
    stop(
      "Error! The value assigned to 'min_score' is not numeric. Provide a numeric value equal to or higher than zero and smaller than one"
    )
  }

  if (is.numeric(time_pads)) {
    if (0 <= time_pads & time_pads <= 16) {
      session_data$time_pads <- time_pads
    } else {
      stop(
        "Error! The value assigned to 'time_pads' is outside the expected interval. Provide an integer between 0 and 16."
      )
    }
  } else {
    stop(
      "Error! The value assigned to 'time_pads' is not numeric."
    )
  }

  if (length(dyn_range) == 2) {
    if (all(is.numeric(dyn_range))) {
      if (dyn_range[1] < dyn_range[2]) {
        if (dyn_range[1] %% 10 == 0 & dyn_range[2] %% 10 == 0) {
          session_data$dyn_range <- dyn_range
        } else {
          stop("Error! 'dyn_range' must be a multiple of 10.")
        }
      } else {
        session_data$dyn_range <- sort(dyn_range)
        warning(
          "Warning! The first value of 'dyn_range' must be smaller than the second. Sorting to match the expected order"
        )
      }
    } else {
      stop(
        "Error! At least one value of 'dyn_range' is not numeric"
      )
    }
  } else {
    stop("Error! The 'dyn_range' must be a numeric vector of length equals 2.")
  }

  if (is.numeric(wl)) {
    if (wl %in% c(128, 256, 512, 1024, 2048, 4096, 8192, 16384)) {
      session_data$wl <- wl
    } else {
      stop(
        "Error! The value assigned to 'wl' is not among the expected alternatives: 128, 256, 512, 1024, 2048, 4096, 8192, or 16384."
      )
    }
  } else {
    stop(
      "Error! The value assigned to 'wl' is not among the expected alternatives: 128, 256, 512, 1024, 2048, 4096, 8192, or 16384."
    )
  }

  if (is.numeric(ovlp)) {
    if (ovlp %in% seq(0, 80, 10)) {
      session_data$ovlp <- ovlp
    } else {
      stop(
        "Error! The value assigned to 'ovlp' is outside the expected interval. Provide a numeric value between 0 and 80 in steps of 10."
      )
    }
  } else {
    stop(
      "Error! The value assigned to 'time_pads' is not numeric. Provide a value between 0 and 80 in steps of 10."
    )
  }

  if (is.character(color_scale)) {
    if (color_scale %in% c("viridis", "magma", "inferno", "cividis", "greyscale 1", "greyscale 2")) {
      session_data$color_scale <- color_scale
    } else {
      stop(
        "Error! The value assigned to 'color_scale' is not among the expected alternatives: 'viridis', 'magma', 'inferno', 'cividis', 'greyscale 1', or 'greyscale 2'."
      )
    }
  } else {
    stop(
      "Error! The value assigned to 'color_scale' is not among the expected alternatives: 'viridis', 'magma', 'inferno', 'cividis', 'greyscale 1', or 'greyscale 2'."
    )
  }

  if (wav_player_type %in% c("HTML player", "R session", "External player")) {
    if (wav_player_type == "External player") {
      if (file.exists(wav_player_path)) {
        session_data$wav_player_path <- wav_player_path
      } else {
        stop(
          "Error! The path informed in 'wav_player_path' was not found locally."
        )
      }
    }
    session_data$wav_player_type <- wav_player_type
  } else {
    stop(
      "Error! The selected WAV player method is not valid. Select one of the following: 'HTML player', 'R session', 'External player'"
    )
  }

  if (length(zoom_freq) == 2) {
    if (all(is.numeric(zoom_freq))) {
      if (zoom_freq[1] < zoom_freq[2]) {
        if (zoom_freq[1] >= 0 & zoom_freq[2] <= 10) {
          if (zoom_freq[1] %% 1 == 0 & zoom_freq[2] %% 1 == 0) {
            session_data$zoom_freq <- zoom_freq
          } else {
            stop("Error! 'zoom_freq' must be an integer.")
          }
        } else {
          stop("Error! 'zoom_freq' must be between 0 and 10.")
        }
      } else {
        session_data$zoom_freq <- sort(zoom_freq)
        warning(
          "Warning! The first value of 'zoom_freq' must be smaller than the second. Sorting to match the expected order"
        )
      }
    } else {
      stop(
        "Error! At least one value of 'zoom_freq' is not numeric"
      )
    }
  } else {
    stop("Error! The 'zoom_freq' must be a numeric vector of length 2.")
  }

  if (isTRUE(visible_bp) | isFALSE(visible_bp)) {
    session_data$visible_bp <- visible_bp
  } else {
    stop(
      "Error! The value assigned to 'visible_bp' is not logical. Set it to TRUE or FALSE."
    )
  }
  if (isTRUE(play_norm) | isFALSE(play_norm)) {
    session_data$play_norm <- play_norm
  } else {
    stop(
      "Error! The value assigned to 'play_norm' is not logical. Set it to TRUE or FALSE."
    )
  }

  if (isTRUE(nav_shuffle) | isFALSE(nav_shuffle)) {
    session_data$nav_shuffle <- nav_shuffle
  } else {
    stop("Error! 'nav_shuffle' must be set to TRUE or FALSE.")
  }

  if (is.numeric(seed)) {
    session_data$seed <- seed
  } else {
    stop("Error! Non-numeric value input provided to 'seed'")
  }
  # todo Add another seed for the diagnostics

  if (isTRUE(auto_next) | isFALSE(auto_next)) {
    session_data$auto_next <- auto_next
  } else {
    stop("Error! 'auto_next' must be set to TRUE or FALSE.")
  }

  if (isTRUE(nav_autosave) | isFALSE(nav_autosave)) {
    session_data$nav_autosave <- nav_autosave
  } else {
    stop("Error! 'nav_autosave' must be set to TRUE or FALSE.")
  }

  if (isTRUE(overwrite) | isFALSE(overwrite)) {
    session_data$overwrite <- overwrite
  } else {
    stop("Error! 'overwrite' must be set to TRUE or FALSE.")
  }

  # session_data$session_notes <- session_notes

  if (is.null(wav_cuts_path)) {
    if (dir.exists("detection_cuts/")) {
      warning(
        "Warning! The informed 'wav_cuts_path' was not found locally. Using the default path 'detection_cuts/'"
      )
    } else {
      dir.create("detection_cuts/")
      warning(
        "Warning! The informed 'wav_cuts_path' was not found locally. Using the default path 'detection_cuts/'"
      )
    }
    session_data$wav_cuts_path <- "detection_cuts/"
  } else {
    if (dir.exists(wav_cuts_path)) {
      session_data$wav_cuts_path <- wav_cuts_path
    } else {
      stop("Error! The provided path to store detection cut files was not found locally.")
    }
  }

  #
  if (is.null(spec_path)) {
    if (dir.exists("detection_spectrograms/")) {
      warning(
        "Warning! The informed 'spec_path' was not found locally. Using the default path 'detection_spectrograms/'"
      )
    } else {
      dir.create("detection_spectrograms/")
      warning(
        "Warning! The informed 'spec_path' was not found locally. Using the default path 'detection_spectrograms/'"
      )
    }
    session_data$spec_path <- "detection_spectrograms/"
  } else {
    if (dir.exists(spec_path)) {
      session_data$spec_path <- wav_cuts_path
    } else {
      stop("Error! The provided path to store detection spectrograms was not found locally.")
    }
  }

  if (is.null(diag_tab_path)) {
    if (dir.exists("detection_validation_tables/")) {
      warning(
        "Warning! The informed 'diag_tab_path' was not found locally. Using the default path 'detection_validation_tables/'"
      )
    } else {
      dir.create("detection_validation_tables/")
      warning(
        "Warning! The informed 'diag_tab_path' was not found locally. Using the default path 'detection_validation_tables/'"
      )
    }
    session_data$diag_tab_path <- "detection_validation_tables/"
  } else {
    if (dir.exists(diag_tab_path)) {
      session_data$diag_tab_path <- wav_cuts_path
    } else {
      stop("Error! The provided path to store validation tables was not found locally.")
    }
  }

  if (is.numeric(pitch_shift)) {
    if (pitch_shift %in% c(-8, -6, -4, -2, 1)) {
      session_data$pitch_shift <- pitch_shift
    } else {
      stop(
        "Error! The value assigned to 'pitch_shift' is not among the expected alternatives: -8, -6, -4, -2, or 1."
      )
    }
  } else {
    stop(
      "Error! The value assigned to 'pitch_shift' is not among the expected alternatives: -8, -6, -4, -2, or 1."
    )
  }

# if (!is.null(preset_path) & !is.null(preset_id)) {
#   # todo Adicionar esse prefixo no arquivo de preset
#   preset_file <- file.path(
#     preset_path, paste0("validation_preset_", preset_id, ".rds")
#   )
#   preset_to_export <- list(
#     validation_user = session_data$validation_user,
#     templates_path = session_data$templates_path,
#     soundscapes_path = session_data$soundscapes_path,
#     input_path = session_data$input_path,
#     output_path = session_data$output_path,
#     wav_player_path = session_data$wav_player_path,
#     wav_player_type = session_data$wav_player_type,
#     val_subset = session_data$val_subset,
#     min_score = session_data$min_score,
#     time_pads = session_data$time_pads,
#     ovlp = session_data$ovlp,
#     wl = session_data$wl,
#     dyn_range = session_data$dyn_range,
#     color_scale = session_data$color_scale,
#     zoom_freq = session_data$zoom_freq,
#     nav_shuffle = session_data$nav_shuffle,
#     seed = session_data$seed,
#     auto_next = session_data$auto_next,
#     nav_autosave = session_data$nav_autosave,
#     overwrite = session_data$overwrite,
#     session_notes = session_data$session_notes,
#     wav_cuts_path = session_data$wav_cuts_path,
#     spec_path = session_data$spec_path,
#     diag_tab_path = session_data$diag_tab_path
#   )
#   saveRDS(object = preset_to_export, file = preset_file)
#   message("Preset sucessfully exported to the selected destination!")
# }

  auc_trap <- function(x, y) {
    res <- sum(
      (rowMeans(cbind(y[-length(y)], y[-1]))) *
        (x[-1] - x[-length(x)])
    )
    return(res)
  }

  # This block defines where embedded html wav players will look for the files
  shiny::addResourcePath("audio", temp_path)
  # resourcePaths()
  # todo Clear temp folder when closing the app

  hotkeys <- c(
    "q", #
    "w", #
    "e", #
    "r", #
    "t", #
    "y", #
    "a", #
    "s", #
    "d", #
    "1", #
    "2" #
  )

  shinyApp(
    ui = dashboardPage(
      header = dashboardHeader(title = "MonitoraSom", titleWidth = "400px"),
      sidebar = dashboardSidebar(
        tags$head(tags$style(HTML(".form-group { margin-bottom: 10px !important; }"))),
        width = "400px",
        # Pop up control. Credits to: soundgen::annotation_app()
        sidebarMenu(
          menuItem(
            "User setup",
            tabName = "user_setup_tab", startExpanded = TRUE,
            icon = icon(lib = "glyphicon", "glyphicon glyphicon-user"),
            splitLayout(
              cellWidths = c("75%", "25%"),
              # path to the soundscape directory
              textAreaInput(
                inputId = "preset_path", label = "Path to preset files (.rds)",
                value = session_data$preset_path,
                placeholder = "Paste or load path here",
                height = "40px", width = "395px", resize = "vertical"
              ),
              shinyDirButton(
                id = "preset_path_load", label = "Load", title = "Teste",
                icon = icon(lib = "glyphicon", "glyphicon glyphicon-import")
              ),
              tags$style(type = "text/css", "#preset_path_load { margin-top: 40px;}")
            ),
            # selectizeInput("available_presets", "Available presets",
            #   choices = "Export new preset file...", width = "100%",
            # ),
            # fluidRow(
            #   column(
            #     width = 5, offset = "0px",
            #     actionButton(
            #       "export_preset", "Export preset",
            #       icon = icon(lib = "glyphicon", "glyphicon glyphicon-export"),
            #       width = "170px"
            #     )
            #   ),
            #   column(
            #     width = 6, offset = "0px",
            #     actionButton(
            #       "import_preset", "Import preset",
            #       icon = icon(lib = "glyphicon", "glyphicon glyphicon-import"),
            #       width = "170px"
            #     )
            #   ),
            #   tags$style(type = "text/css", "#export_preset { margin-top: 0px;}"),
            #   tags$style(type = "text/css", "#import_preset { margin-top: 0px;}")
            # ),
            textInput("validation_user", "User name (*):",
              value = session_data$validation_user,
              placeholder = "Identify yourself here", width = "100%"
            ),
            splitLayout(
              cellWidths = c("75%", "25%"),
              icon = icon(lib = "glyphicon", "glyphicon glyphicon-import"),
              textAreaInput("templates_path", "Templates path (*)",
                value = session_data$templates_path,
                placeholder = "Paste or load path here",
                height = "40px", resize = "vertical", width = "100%"
              ),
              shinyDirButton(
                id = "templates_path_load", label = "Load", title = "Teste",
                icon = icon(lib = "glyphicon", "glyphicon glyphicon-import")
              ),
              tags$style(type = "text/css", "#templates_path_load { margin-top: 40px;}")
            ),
            splitLayout(
              cellWidths = c("75%", "25%"),
              textAreaInput("soundscapes_path", "Soundscapes path (*)",
                value = session_data$soundscapes_path,
                placeholder = "Paste or load path here",
                height = "40px", width = "395px", resize = "vertical"
              ),
              shinyDirButton(
                id = "soundscapes_path_load", label = "Load", title = "Teste",
                multiple = FALSE,
                icon = icon(lib = "glyphicon", "glyphicon glyphicon-import")
              ),
              tags$style(type = "text/css", "#soundscapes_path_load { margin-top: 40px;}")
            ),
            splitLayout(
              cellWidths = c("75%", "25%"),
              textAreaInput("input_path", "Detections table path (input) (*)",
                value = session_data$input_path,
                placeholder = "Paste or load path here",
                height = "40px", width = "395px", resize = "vertical"
              ),
              shinyFilesButton(
                id = "input_path_load", label = "Load", title = "Teste", multiple = FALSE,
                icon = icon(lib = "glyphicon", "glyphicon glyphicon-import")
              ),
              tags$style(type = "text/css", "#input_path_load { margin-top: 40px;}")
            ),
            splitLayout(
              cellWidths = c("75%", "25%"),
              textAreaInput("output_path", "Validated data path (output) (*)",
                value = session_data$output_path,
                placeholder = "Paste or load path here",
                height = "40px", width = "395px", resize = "vertical"
              ),
              shinyFilesButton(
                id = "output_path_load", label = "Load", title = "Teste", multiple = FALSE,
                icon = icon(lib = "glyphicon", "glyphicon glyphicon-export")
              ),
              tags$style(type = "text/css", "#output_path_load { margin-top: 40px;}")
            ),
                        # todo Mudar de input path para "user_setup"
            actionButton("input_path_confirm", "Confirm Paths",
              icon = icon(lib = "glyphicon", "glyphicon glyphicon-check"),
              style = "color: #000000; background-color: #33b733; border-color: #288d28; width: 360px;"
            ),
            shinyBS::bsTooltip("input_path_confirm",
              title = "<b>Part 1 of 2 required to start the session</b>. All inputs marked with (*) are required for this step",
              placement = "right", trigger = "hover",
              options = list(delay = list(show = 1000, hide = 0))
            ),
            tags$style(".tooltip {width: 300px;}")

          ),
          menuItem(
            "Session Setup",
            icon = icon(lib = "glyphicon", "glyphicon glyphicon-check"),
            tabName = "sect_setup_tab",
            # todo Template name is not among validation presets. Is is a must?
            selectInput(
              "template_name", "Template file (*)",
              choices = NULL, width = "100%"
            ),
            selectizeInput(
              "val_subset", "Filter validation inputs (*)",
              choices = c(
                "True positives - TP" = "TP", "False positives - FP" = "FP",
                "Unknown - UN" = "UN", "Not reviewed - NA" = "NA"
              ),
              selected = session_data$val_subset, multiple = TRUE, width = "100%"
            ),
            sliderInput(
              "min_score", "Minimum correlation (*)",
              width = "100%", min = 0, max = 1, step = 0.01,
              value = session_data$min_score
            ),
            actionButton(
              "confirm_session_setup", "Confirm validation setup",
              icon = icon(lib = "glyphicon", "glyphicon glyphicon-check"),
              style = "color: #000000; background-color: #33b733; border-color: #288d28; width: 360px;"
            )
          ),
          menuItem(
            "Spectrogram Parameters",
            tabName = "spec_par_tab",
            icon = icon(lib = "glyphicon", "glyphicon glyphicon-cog"),
            sliderInput(
              "time_pads", "Pad size (s):",
              min = 0, max = 16, width = "100%",
              value = session_data$time_pads
            ),
            sliderInput(
              "dyn_range", "Dynamic range (dB)",
              min = -120, max = 120, step = 10, value = session_data$dyn_range, width = "100%"
            ),
            sliderTextInput(
              "wl", "Window length:",
              choices = c(128, 256, 512, 1024, 2048, 4096, 8192, 16384),
              grid = TRUE, width = "100%",
              selected = session_data$wl
            ),
            sliderInput(
              "ovlp", "Overlap (%):",
              min = 0, max = 80, step = 10, width = "100%",
              value = session_data$ovlp
            ),
            sliderTextInput(
              "pitch_shift", "Pitch shift (octaves) and slow down (factor)",
              choices = c(-8, -6, -4, -2, 1), selected = session_data$pitch_shift,
              grid = TRUE, width = "100%"
            ),
            selectInput(
              "color_scale", "Color:",
              choices = c(
                "viridis", "magma", "inferno", "cividis", "greyscale 1", "greyscale 2"
              ),
              width = "100%", selected = session_data$color_scale
            ),
            checkboxInput(
              "visible_bp", "Play only the visible frequency band",
              value = session_data$visible_bp, width = "400px"
            ),
            checkboxInput(
              "play_norm", "Normalize detection audio",
              value = session_data$play_norm, width = "400px"
            ),
            radioButtons(
              "wav_player_type", "Sound player",
              choices = c("HTML player", "R session", "External player"), inline = TRUE,
              selected = session_data$wav_player_type
            ),
            splitLayout(
              cellWidths = c("75%", "25%"),
              textAreaInput(
                "wav_player_path",
                label = "Path to player executable (default = 'play')",
                height = "40px", resize = "vertical",
                value = session_data$wav_player_path
              ),
              shinyDirButton(
                id = "wav_player_path_load", label = "Load", title = "Teste",
                icon = icon(lib = "glyphicon", "glyphicon glyphicon-export")
              ),
              tags$style(type = "text/css", "#wav_player_path_load { margin-top: 40px;}")
            ),
            actionButton(
              inputId = "get_templ_pars",
              label = "Get current template parameters", icon = icon("gear"),
              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 360px;"
            ),
            actionButton(
              inputId = "default_pars",
              label = "Reset to default parameters", icon = icon("gear"),
              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 360px;"
            )
          )
          # menuItem(
          #   "Session Notes",
          #   icon = icon(lib = "glyphicon", "glyphicon glyphicon-pencil"),
          #   tabName = "tab_notes",
          #   textAreaInput(
          #     "session_notes", "Session notes",
          #     placeholder = "Write session notes here", width = "100%",
          #     height = "150px", resize = "vertical",
          #     value = session_data$session_notes
          #   )
          # )
        ),
        actionButton(
          "end_session", "End validation session",
          icon = icon(lib = "glyphicon", "glyphicon glyphicon-log-out"),
          style = "color: #fff; background-color: #b73333; border-color: #8d2c2c; width: 370px;"
        )
      ),
      body = dashboardBody(

        # Set up shinyjs
        useShinyjs(),

        # Make keyboard shotcuts available
        useKeys(),
        keysInput("hotkeys", hotkeys),

        # Avoid blinking figures while rendering
        tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),
        tags$head(tags$style(HTML(".content-wrapper { overflow: auto; }"))),

        # box(width = 6, verbatimTextOutput("checagem1")),
        # box(width = 6, verbatimTextOutput("checagem2")),

        box(
          width = 12, height = "550px",
          splitLayout(
            cellWidths = c("6%", "94%"),
            noUiSliderInput(
              "zoom_freq", "Frequency zoom",
              min = 0, max = 18, step = 1, direction = "rtl",
              orientation = "vertical", width = "100px", height = "425px",
              value = session_data$zoom_freq
            ),
            column(
              width = 12,
              column(
                width = 6,
                plotOutput("TemplateSpectrogram", height = "475px"),
                tags$div(id = "template_player"),
                actionButton(
                  "play_template", "Play Template (1)",
                  icon = icon("play"), width = "100%", style = "height:50px"
                )
              ),
              column(
                width = 6,
                plotOutput("DetectionSpectrogram", height = "475px"),
                column(
                  width = 8, offset = 0,
                  tags$div(id = "detection_player"),
                  actionButton(
                    "play_detec", "Play Detection (2)",
                    icon = icon("play"), width = "100%", style = "height:50px"
                  )
                ),
                column(
                  width = 4, offset = 0,
                  actionButton(
                    "prev_detec", "",
                    icon = icon("backward", lib = "font-awesome"),
                    width = "45%", style = "height:50px"
                  ),
                  actionButton(
                    "next_detec", "",
                    icon = icon("forward", lib = "font-awesome"),
                    width = "45%", style = "height:50px"
                  )
                )
              )
            )
          )
        ),
        box(
          width = 12,
          column(
            width = 3,
            actionButton(
              "button_tp",
              HTML("<b>TRUE POSITIVE (Q)</b>"),
              icon = icon("check", lib = "font-awesome"),
              width = "100%", style =
                "color: #000000; background-color: #6ae46a; height: 55px" #
            )
          ),
          column(
            width = 3,
            actionButton(
              "button_un", HTML("<b>UNKNOWN (W)</b>"),
              icon = icon("question", lib = "font-awesome"),
              width = "100%", style =
                "color: #000000; background-color: #ffba52; height: 55px" #
            )
          ),
          column(
            width = 3,
            actionButton(
              "button_fp",
              HTML("<b>FALSE POSITIVE (E)</b>"),
              icon = icon("x", lib = "font-awesome"),
              width = "100%", style =
                "color: #000000; background-color: #ff7e7e; height: 55px" #
            )
          ),
          column(
            width = 3,
            actionButton(
              "button_save", HTML("<b>Export output</b>"),
              icon = icon("save", lib = "font-awesome"),
              width = "100%", style =
                "color: #ffffff; background-color: #000000; height: 55px" #
            )
          ),
          column(
            width = 4,
            disabled( # todo: enable this
              selectizeInput(
                "soundscape_file", "Soundscape file",
                choices = NULL, width = "100%"
              )
            )
          ),
          column(
            width = 2,
            selectInput("detec", "Detection ID", choices = NULL, width = "100%")
          ),
          column(
            width = 2,
            numericInput("seed", "Seed", value = session_data$seed)
          ),
          column(
            width = 1,
            checkboxInput(
              "nav_shuffle", HTML("<b>Shuffle</b>"),
              value = session_data$nav_shuffle
            )
          ),
          column(
            width = 1,
            checkboxInput(
              "auto_next", HTML("<b>Autonavigate</b>"),
              value = session_data$auto_next
            )
          ),
          column(
            width = 1,
            checkboxInput(
              "overwrite", HTML("<b>Overwrite</b>"),
              value = session_data$overwrite
            )
          ),
          column(
            width = 1,
            checkboxInput(
              "nav_autosave", HTML("<b>Autosave</b>"),
              value = session_data$nav_autosave
            )
          )
        ),
        tabBox(
          width = 12, height = "900px",
          id = "tabset1",
          tabPanel(
            "Progress",
            fluidRow(
              column(
                width = 6,
                h5("Full dataset progress"),
                progressBar(
                  id = "prog_bar_full", value = 0, total = 1,
                  status = "info", display_pct = TRUE, striped = TRUE
                ),
                tableOutput("count_full_tab")
              ),
            column(
              width = 6,
              h5("Current subset progress"),
              progressBar(
                id = "prog_bar_subset", value = 0, total = 1,
                status = "info", display_pct = TRUE, striped = TRUE
              ),
              tableOutput("count_i_tab")
            )
            )
          ),
          tabPanel("Detection Table",
            height = "100%",
            DTOutput("res_table")
          ),
          tabPanel(
            "Soundscape Spectrogram",
            column(
              width = 2,
              switchInput(
                inputId = "show_soundscape", offLabel = "Hidden",
                onLabel = "Visible"
              )
            ),
            column(
              width = 7,
              actionButton(
                "play_soundscape", "Play Soundscape",
                icon = icon("play"), width = "49%", style = "height:50px"
              ),
              tags$div(id = "soundscape_player")
            ),
            hidden(plotOutput("SoundscapeSpectrogram", height = "350px"))
          ),
          tabPanel(
            "Export Detection",
            column(
              width = 6,
              splitLayout(
                cellWidths = c("80%", "20%"),
                textInput(
                  "wav_cuts_path", "Wave cuts path",
                  value = session_data$wav_cuts_path, width = "100%",
                  placeholder = "Paste or load here the path to export the wav file"
                ),
                shinyDirButton(
                  id = "wav_cuts_path_load", label = "Load path", title = "Teste",
                  icon = icon(lib = "glyphicon", "glyphicon glyphicon-import")
                ),
                tags$style(type = "text/css", "#wav_cuts_path_load { margin-top: 25px; width: 100%; }")
              ),
              splitLayout(
                cellWidths = c("80%", "20%"),
                textInput(
                  "wav_cut_name", "Wav file name (*.wav)",
                  value = NULL, width = "100%",
                  placeholder = "Input the file name here"
                ),
                actionButton("reset_wav_cut_name", "Reset filename", width = "100%"),
                tags$style(type = "text/css", "#reset_wav_cut_name { margin-top: 25px;}")
              ),
              actionButton(
                "confirm_wav_export", "Export wav file (r)",
                width = "100%",
                style = "color: #fff; background-color: #33b76e; border-color: #5da42e;"
              )
            ),
            column(
              width = 6,
              splitLayout(
                cellWidths = c("80%", "20%"),
                textInput(
                  "spec_path", "Spectrogram cuts path",
                  value = session_data$spec_path, width = "100%",
                  placeholder = "Paste or load here the path to export the spectrogram"
                ),
                shinyDirButton(
                  id = "spec_path_load", label = "Load path", title = "Teste",
                  icon = icon(lib = "glyphicon", "glyphicon glyphicon-import")
                ),
                tags$style(type = "text/css", "#spec_path_load { margin-top: 25px; width: 100%; }")
              ),
              splitLayout(
                cellWidths = c("80%", "20%"),
                textInput(
                  "spec_name", "Spectrogram file name (*.jpeg)",
                  value = NULL, width = "100%",
                  placeholder = "Input the file name here"
                ),
                actionButton("reset_spec_filename", "Reset filename", width = "100%"),
                tags$style(type = "text/css", "#reset_spec_filename { margin-top: 25px;}")
              ),
              actionButton(
                "confirm_spec_export", "Export spectrogram (t)",
                width = "100%",
                style = "color: #fff; background-color: #33b76e; border-color: #5da42e;"
              )
            )
          ),
          tabPanel(
            "Diagnostics",
            column(
              width = 3,
              selectInput(
                "diag_balance", "Dataset balance method",
                choices = c(
                  "None", "Downsample larger class", "Upsample smaller  class", "ROSE"
                ),
                selected = "None", width = "100%"
              ),
              selectInput(
                "diag_method", "Cutpoint detection method",
                choices = c("Manual", "Error = 0.05", "Error = 0.1"),
                width = "100%"
              ),
              sliderInput(
                "diag_cut", "Cutpoint threshold",
                min = 0, max = 1, step = 0.001, value = 0.2, width = "100%"
              )
            ),
            tableOutput("cut_i_tab"),
            column(width = 3, plotOutput("plot_binomial", height = "340px")),
            column(width = 3, plotOutput("plot_roc", height = "340px")),
            column(width = 3, plotOutput("plot_prec_rec", height = "340px")),
            column(
              width = 6,
              splitLayout(
                cellWidths = c("80%", "20%"),
                textInput("diag_tab_path", "Diagnostics table path",
                  value = session_data$diag_tab_path, width = "100%",
                  placeholder = "Paste or load here the path to export the .csv file"
                ),
                shinyDirButton(
                  id = "diag_tab_path_load", label = "Load path", title = "Teste",
                  icon = icon(lib = "glyphicon", "glyphicon glyphicon-import")
                ),
                tags$style(
                  type = "text/css", "#diag_tab_path_load { margin-top: 25px; width: 100%; }"
                )
              )
            ),
            column(
              width = 6,
              splitLayout(
                cellWidths = c("80%", "20%"),
                textInput(
                  "diag_tab_name", "Diagnostics table name (*.csv)",
                  value = NULL, width = "100%",
                  placeholder = "Input the file name here"
                ),
                actionButton("reset_diag_tab_filename", "Reset filename"),
                tags$style(
                  type = "text/css", "#reset_diag_tab_filename { margin-top: 25px; width: 100%; }"
                )
              ),
              actionButton(
                "confirm_diag_tab_export", "Export table (y)",
                width = "100%",
                style = "color: #fff; background-color: #33b76e; border-color: #5da42e;"
              )
            )
          )
        )
      ),
      skin = "black"
    ),
    server = function(input, output, session) {
      # Set a reactive object to detect paths for system volumes
      volumes <- shinyFiles::getVolumes()()

      # Open the server side for choosing the directory with
      # the templates
      shinyDirChoose(
        input, "templates_path_load",
        session = session,
        roots = volumes
      )
      # Observe the interface input to update the text input
      observeEvent(input$templates_path_load, {
        res <- parseDirPath(
          volumes, input$templates_path_load
        ) %>%
          as.character() %>%
          gsub("//", "/", .)
        updateTextInput(
          session,
          inputId = "templates_path", value = res
        )
      })

      # Open the server side for choosing the directory with
      # the soundscapes
      shinyDirChoose(
        input, "soundscapes_path_load",
        session = session,
        roots = volumes
      )
      # Observe the interface input to update the text input
      observeEvent(input$soundscapes_path_load, {
        res <- parseDirPath(
          volumes, input$soundscapes_path_load
        ) %>%
          as.character() %>%
          gsub("//", "/", .)
        updateTextInput(
          session,
          inputId = "soundscapes_path", value = res
        )
      })

      # Set the path to the player executable
      shinyFileChoose(input, "wav_player_path_load",
        session = session, roots = volumes
      )
      observeEvent(input$wav_player_path_load, {
        input_file <- parseFilePaths(
          roots = volumes, input$wav_player_path_load
        ) %>%
          as.data.frame() %>%
          .$datapath %>%
          gsub("//", "/", .)
        updateTextInput(session, inputId = "wav_player_path", value = input_file)
      })

      # Open the server side for choosing the csv input with detections
      shinyFileChoose(
        input, "input_path_load",
        session = session, roots = volumes
      )
      # observe the input and update the text accordingly
      observeEvent(input$input_path_load, {
        input_file <- parseFilePaths(
          roots = volumes, input$input_path_load
        ) %>%
          as.data.frame() %>%
          .$datapath %>%
          gsub("//", "/", .)
        if (!is.null(input$input_path_load)) {
          updateTextInput(
            session,
            inputId = "input_path", value = input_file
          )
        }
      })

      # Open the server side for choosing the csv in which validations will be stored
      shinyFileChoose(
        input, "output_path_load",
        session = session, roots = volumes
      )
      # observe and update the text accordingly
      observeEvent(input$output_path_load, {
        output_file <- parseFilePaths(
          roots = volumes, input$output_path_load
        ) %>%
          as.data.frame() %>%
          .$datapath %>%
          gsub("//", "/", .)
        if (!is.null(input$output_path_load)) {
          updateTextInput(
            session,
            inputId = "output_path", value = output_file
          )
        }
      })

      # Create a reactive object for storing the path
      templates_path <- reactiveVal(NULL)
      # Create a reactive object for storing the path
      soundscapes_path <- reactiveVal(NULL)
      # Create empty reactive object with the full detection dataset
      df_full <- reactiveValues(data = NULL)
      df_output <- reactiveVal(NULL)
      # On confirmation of the provided path...
      observeEvent(input$input_path_confirm, {
        req(
          input$input_path, input$output_path, input$soundscapes_path,
          input$templates_path, input$validation_user
        )

        if (input$input_path == input$output_path) {
          showModal(
            modalDialog(
              title = "Input and output files are the same",
              "There is risk of overwritting previsous validations. Make sure the Overwrite check is not enabled to protect your data.",
              easyClose = TRUE,
              footer = NULL
            )
          )
        }
        if (input$input_path != input$output_path) {
          showModal(
            modalDialog(
              title = "Input and output files are different",
              "Proceed if you wish to create a new output file. If the output file already exists, consider choosing another name or make a backup copy, as exporting new detections may silently overwrite it.",
              easyClose = TRUE,
              footer = NULL
            )
          )
        }
        df_soundscapes <- data.frame(
          soundscape_path = list.files(
            input$soundscapes_path,
            pattern = ".wav|.WAV", recursive = TRUE, full.names = TRUE
          )
        ) %>%
          mutate(soundscape_file = basename(soundscape_path))

        df_templates <- data.frame(
          template_path = list.files(
            input$templates_path,
            pattern = ".wav|.WAV", recursive = TRUE, full.names = TRUE
          )
        ) %>%
          mutate(template_file = basename(template_path))

        res <- fread(input$input_path, data.table = FALSE, header = TRUE) %>%
          mutate(
            soundscape_path = as.character(NA),
            template_path = as.character(NA)
          ) %>%
          rows_update(df_templates, by = "template_file", unmatched = "ignore") %>%
          rows_update(df_soundscapes, by = "soundscape_file", unmatched = "ignore")

        var_names <- c(
          "detection_id", "validation_user", "validation_time", "validation"
        )

        # Add variables for review inputs in case those are not in res
        if (!all(var_names %in% colnames(res))) {
          res <- res %>%
            mutate(
              detection_id = 1:nrow(.),
              validation_user = as.character(NA),
              validation_time = as.character(NA),
              validation = as.character(NA)
            )
        } else if ("validation_time" %in% colnames(res)) {
          res <- res %>%
            mutate(
              validation_time = as.character(validation_time)
            )
        }

        updateSelectInput(
          session, "template_name", # todo Importante!
          choices = unique(res$template_name)
        )

        # Initial upodate of the progress bar
        updateProgressBar(
          session = session, id = "prog_bar_full",
          value = length(which(res$validation %in% c("TP", "FP", "UN"))),
          total = nrow(res)
        )

        # Two diential reactive objects for modification control
        df_full$data <- res
        df_output(res)

        showNotification("Paths updated successfully")
      })

      # observe and update cut levels for avoiding returning empty detection tables
      observeEvent(input$template_name, {
        req(df_full$data)
        df_ref <- df_full$data[
          which(df_full$data$template_name == input$template_name),
        ]
        updateSliderInput(
          session, "min_score",
          max = round(head(tail(sort(df_ref$peak_score), 2), 1), 2)
        )
      })

      # Alternative version of df_detections_full containing only the
      # samples above the specified threshold to avoid showing soundscapes
      # without detections
      df_cut <- reactiveVal(NULL)
      #
      vec_soundscapes <- reactiveVal(NULL)
      #
      df_template <- reactiveVal(NULL)

      observeEvent(input$confirm_session_setup, {
        req(df_full$data)

        if (!is.null(df_output())) {
          df_full$data <- df_output()
        }

        val_subset <- input$val_subset
        val_subset[val_subset == "NA"] <- NA

        # Gather the metadata to validate the active template
        res <- df_full$data %>%
          # filter by template name
          filter(template_name == input$template_name) %>%
          # filter by correlation threshold
          filter(peak_score >= input$min_score) %>%
          # filter by validation outcome
          filter(validation %in% val_subset)

        # if the filtering process result is not null, get some more information
        if (!is.null(res)) {
          updateSelectInput(
            session, "detec",
            choices = res$detection_id, selected = res$detection_id[1]
          )
          res_soundscapes <- unique(res$soundscape_file)
          vec_soundscapes(res_soundscapes)
          updateSelectInput(session, "soundscape_file", choices = res_soundscapes)
          df_cut(res)
          updateProgressBar(
            session = session, id = "prog_bar_subset",
            value = length(which(res$validation %in% c("TP", "FP", "UN"))),
            total = nrow(res)
          )
        }
        # Reactive object to update info about the active template
        df_template({
          df_cut() %>%
            filter(template_name == input$template_name) %>%
            slice_head()
        })

        # # Clean diagnostics of validation in case another template was validated in the same session
        # mod_res_react(NULL)
        # mod_plot_react(NULL)

        #
        showNotification("Validation session updated")
      })

      # If the user sets the random order review mode, it will randomize rows of df_cut()
      observeEvent(input$nav_shuffle, {
        req(df_cut())

        if (input$nav_shuffle == TRUE) {
          req(input$confirm_session_setup)
          if (is.integer(input$seed)) {
            set.seed(input$seed)
          } else {
            showModal(
              modalDialog(
                title = "Seed not defined!",
                "A numeric seed is needed to ensure reproducibility when shuffling the sequence of detections to be validated. The input will be updated to the default value (123). Provide another value if necessary.",
                easyClose = TRUE,
                footer = NULL
              )
            )
            updateNumericInput(session, "seed", value = 123)
            set.seed(123)
          }
          random_index <- sample(1:nrow(df_cut()))
          df_cut(df_cut()[random_index, ])
          det_counter(random_index[1]) # ! corrigir erro de nevagação aqui
          updateSelectInput(session, "detec", selected = df_cut()$detection_id[1])
        } else if (input$nav_shuffle == FALSE) {
          df_cut(df_cut()[order(df_cut()$detection_id), ])
          det_counter(1)
          updateSelectInput(session, "detec", selected = df_cut()$detection_id[1])
        }
      })

      # Reactive object to store the detection index within df_cut()
      det_counter <- reactiveVal(1)
      # Upon initialization and under these conditions, det_counter is set to 1
      observeEvent(
        list(input$min_score, input$input_path_confirm), det_counter(1)
      )
      # Create a reactive object to store the data of the active detection
      det_i <- reactiveVal(NULL)
      # Update the active acoording to the counter value
      observeEvent(det_counter(), {
        req(df_cut())
        det_i(df_cut()[det_counter(), ])
      })

      # Add 1 to the counter to navigate forward
      observeEvent(input$next_detec, {
        req(df_cut(), det_counter())
        if (nrow(df_cut()) > det_counter() & det_counter() >= 1) {
          det_counter(det_counter() + 1)
          updateSelectInput(
            session, "detec",
            selected = df_cut()$detection_id[det_counter()]
          )
        }
      })

      # Subtract 1 from the counter to nvigate backwards
      observeEvent(input$prev_detec, {
        req(df_cut(), det_counter())
        if (1 < det_counter() & det_counter() <= nrow(df_cut())) {
          det_counter(det_counter() - 1)
          updateSelectInput(
            session, "detec",
            selected = df_cut()$detection_id[det_counter()]
          )
        } else if (det_counter() == 1) { # cuidado com isso com os outros updates
          det_counter(1)
          updateSelectInput(
            session, "detec",
            selected = df_cut()$detection_id[1]
          )
        }
      })

      observeEvent(input$detec, {
        req(df_cut(), det_counter())
        i <- which(df_cut()$detection_id == input$detec)
        if (!is.null(i)) det_counter(i)
        det_i(df_cut()[i, ])

        if (det_i()$soundscape_file != input$soundscape_file &
          det_i()$soundscape_file %in% vec_soundscapes()) {
          updateSelectInput(session, "soundscape_file",
            selected = det_i()$soundscape_file
          )
        }
      })

      # todo select soundscape_name
      # observeEvent(input$soundscape_name, {
      #   req(df_cut(), det_counter(), det_i())
      #   i <- which(df_cut()$soundscape_name == input$soundscape_name)[1]
      #   if (!is.null(i)) det_counter(i)
      #   updateSelectInput(
      #     session, "detec",
      #     selected = isolate(df_cut()$detection_id[det_counter()])
      #     )
      # })

      # Reactive object containing the wav of the active template
      # todo Remake this portion so that templates standalone or roi_tabs work the same way
      rec_template <- reactiveVal(NULL)
      observe({
        req(df_template())
        wav_path <- df_template()$template_path
        rec_start <- df_template()$template_start - zoom_pad()
        pre_silence <- 0
        rec_end <- df_template()$template_end + zoom_pad()
        pos_silence <- 0


        if (rec_start < 0) {
          pre_silence <- abs(rec_start)
          rec_start <- 0
        }
        if (rec_end > (df_template()$template_end - df_template()$template_start)) {
            pos_silence <- rec_end - (df_template()$template_end - df_template()$template_start)
            rec_end <- (df_template()$template_end - df_template()$template_start)
        }

        if (length(wav_path) == 1) {
          res <- readWave(
            wav_path,
            from = rec_start, to = rec_end, units = "seconds"
          ) %>%
            addsilw(., at = "start", d = pre_silence, output = "Wave") %>%
            addsilw(., at = "end", d = pos_silence, output = "Wave")

          rec_template(res)

          # Rendering the template HTML player
          if (file.exists(wav_path) & input$wav_player_type == "HTML player") {
            temp_file <- tempfile(
              pattern = "template_", tmpdir = session_data$temp_path, fileext = ".wav"
            ) %>%
              gsub("\\\\", "/", .)
            if (zoom_pad() != 0) {
              res <- cutw(
                res,
                from = zoom_pad(), to = duration(res) - zoom_pad(),
                output = "Wave"
              )
            }
            if (input$pitch_shift < 1) {
              res@samp.rate <- res@samp.rate / pitch_shift
            }
            if (isTRUE(input$visible_bp)) {
              res <- seewave::fir(
                res,
                f = res@samp.rate,
                from = (input$zoom_freq[1] / pitch_shift) * 1000,
                to = (input$zoom_freq[2] / pitch_shift) * 1000,
                wl = input$wl, output = "Wave"
              )
            }

            seewave::savewav(res, f = res@samp.rate, filename = temp_file)
            removeUI(selector = "#template_player_selector")
            insertUI(
              selector = "#template_player", where = "afterEnd",
              ui = tags$audio(
                id = "template_player_selector",
                src = paste0("audio/", basename(temp_file)),
                type = "audio/wav", autostart = FALSE, controls = TRUE
              )
            )
            unlink("template_.*.wav")
            list.files(
              session_data$temp_path,
              pattern = "template_.*.wav", full.names = TRUE
            ) %>%
              .[. != temp_file] %>%
              file.remove()
          } else {
            removeUI(selector = "#template_player_selector")
          }
        }
      })

      # Set of updates of spectrogram parameters that are obtained from
      # detection metadata
      observeEvent(input$get_templ_pars, {
        req(df_template(), df_cut())
        # update spectrogram overlap
        updateSliderInput(
          session,
          inputId = "ovlp", label = "Overlap (%):",
          value = df_template()$detection_ovlp, step = 10
        )
        # update spectroram window length
        updateSliderTextInput(
          session,
          inputId = "wl", label = "Window length:",
          selected <- df_template()$detection_wl
        )
        # update spectrogram minimum frequency
        min_freq <- (df_template()$template_min_freq - 1) %>%
          round(0) %>%
          ifelse(. < 0, 0, .)
        # update spectrogram maximum frequency
        max_freq <- (df_template()$template_max_freq + 1) %>%
          round(0) %>%
          ifelse(. > 23, 23, .)
        # update the frequency band
        updateSliderInput(
          session, "zoom_freq", "Frequency band (kHz):",
          min = 0, max = (min(df_cut()$detection_sample_rate) / 2000),
          step = 1, value = c(min_freq, max_freq)
        )
      })

      observeEvent(input$default_pars, {
        req(det_i())
        # todo Update pad slider
        updateSliderInput(session, inputId = "dyn_range", value = c(0, 50))
        updateSliderTextInput(session, inputId = "wl", selected = 2048)
        updateSliderInput(session, inputId = "ovlp", value = 0)
        updateSelectInput(session, inputId = "color_scale", selected = "inferno")
        updateSliderInput(session, inputId = "time_pads", value = 1)
        updateSliderTextInput(session, inputId = "pitch_shift", selected = 1)
        updateCheckboxInput(session, inputId = "visible_bp", value = FALSE)
        updateCheckboxInput(session, inputId = "play_norm", value = FALSE)
        updateSliderInput(
          session, "zoom_freq",
          min = 0, max = (det_i()$detection_sample_rate / 2000),
          step = 1, value = c(0, 10)
        )
      })

      zoom_pad <- reactiveVal(0)
      observe({
        zoom_pad(input$time_pads)
      })

      spectro_template <- reactive({
        req(rec_template(), df_template())
        box_color <- ifelse(
          input$color_scale %in% c("greyscale 1", "greyscale 2"), "black", "white"
        )
        temp_rec <- rec_template()
        fast_spectro(
          rec = temp_rec, f = df_template()$sample_rate, wl = input$wl,
          ovlp = input$ovlp, flim = c(input$zoom_freq[1], input$zoom_freq[2]),
          dyn_range = c(input$dyn_range[1], input$dyn_range[2]),
          color_scale = input$color_scale,
          pitch_shift = input$pitch_shift
        ) +
          labs(title = "Template spectrogram") +
          annotate(
            "label",
            label = paste0("Template: '", df_template()$template_name, "'"),
            x = -Inf, y = Inf, hjust = 0, vjust = 1,
            color = "white", fill = "black"
          ) +
          annotate(
            "rect",
            xmin = ifelse(zoom_pad() == 0, 0, zoom_pad()),
            xmax = ifelse(
              zoom_pad() == 0, duration(rec_template()),
              duration(rec_template()) - zoom_pad()
            ),
            ymin = df_template()$template_min_freq,
            ymax = df_template()$template_max_freq,
            linetype = "dashed", alpha = 0,
            color = box_color, fill = box_color
          )
      })

      # render the template spectrogram in the interface
      output$TemplateSpectrogram <- renderPlot({
        req(rec_template(), df_template())
        spectro_template()
      })

      # Reactive object to store info about the detections in the active sounscape
      df_detections <- reactive({
        req(df_cut())
        df_cut() %>%
          filter(
            template_name == input$template_name,
            soundscape_file == input$soundscape_file # todo Confirmar se está certo
          )
      })

      rec_detection <- reactiveVal(NULL)
      det_sel <- reactiveVal(NULL)
      # reactive object to store the recording of the active detection
      observe({
        req(det_i())
        # wav_path <- df_template()$template_path
        wav_path <- det_i()$soundscape_path

        pad_start <- det_i()$detection_start - zoom_pad()
        pad_end <- det_i()$detection_end + zoom_pad()
        dur <- det_i()$detection_end - det_i()$detection_start

        if (pad_start < 0) {
          det_sel(c(zoom_pad() + pad_start, zoom_pad() + pad_start + dur))
          pad_start <- 0
          pad_end <- dur + (2 * zoom_pad())
        } else {
          det_sel(c(zoom_pad(), zoom_pad() + dur))
        }

        if (length(wav_path) == 1) {
          res <- readWave(
            filename = det_i()$soundscape_path,
            from = pad_start, to = pad_end, units = "seconds"
          )
          rec_detection(res)
        }

        # Rendering the detection HTML player
        if (file.exists(det_i()$soundscape_path) & input$wav_player_type == "HTML player") {
          # file.remove("temp/detection_clip.wav")
          temp_file <- tempfile(
            pattern = "detection_", tmpdir = session_data$temp_path, fileext = ".wav"
          ) %>%
            gsub("\\\\", "/", .)
          if (input$pitch_shift < 1) {
            res@samp.rate <- res@samp.rate / pitch_shift
          }
          if (isTRUE(input$visible_bp)) {
            res <- seewave::fir(
              res,
              f = res@samp.rate,
              from = (input$zoom_freq[1] / pitch_shift) * 1000,
              to = (input$zoom_freq[2] / pitch_shift) * 1000,
              wl = input$wl, output = "Wave"
            )
          }
          if (input$play_norm == TRUE) {
            res <- normalize(object = res, unit = as.character(res@bit), pcm = TRUE)
          }
          seewave::savewav(res, f = res@samp.rate, filename = temp_file)
          removeUI(selector = "#detection_player_selector")
          insertUI(
            selector = "#detection_player", where = "afterEnd",
            ui = tags$audio(
              id = "detection_player_selector",
              src = paste0("audio/", basename(temp_file)),
              type = "audio/wav", autostart = FALSE, controls = TRUE
            )
          )
          unlink("detection_.*.wav")
          list.files(
            session_data$temp_path,
            pattern = "detection_.*.wav", full.names = TRUE
          ) %>%
            .[. != temp_file] %>%
            file.remove()
        } else {
          removeUI(selector = "#detection_player_selector")
        }
      })

      spectro_detection <- reactive({
        req(rec_detection(), det_i(), det_sel())
        box_color <- ifelse(
          input$color_scale %in% c("greyscale 1", "greyscale 2"), "black", "white"
        )
        # efficient_spectro(
        fast_spectro(
          rec = rec_detection(), f = det_i()$detection_sample_rate,
          wl = input$wl, ovlp = input$ovlp,
          flim = c(input$zoom_freq[1], input$zoom_freq[2]),
          dyn_range = c(input$dyn_range[1], input$dyn_range[2]),
          color_scale = input$color_scale, pitch_shift = input$pitch_shift
        ) +
          labs(title = "Detection spectrogram") +
          annotate(
            "label",
            label = paste0(
              det_i()$soundscape_file, "\n",
              "Detection ID: '", det_i()$detection_id, "' in '",
              basename(input$output_path), "'"
            ),
            x = -Inf, y = Inf, hjust = 0, vjust = 1,
            color = "white", fill = "black"
          ) +
          annotate(
            "rect",
            xmin = det_sel()[1], xmax = det_sel()[2],
            ymin = det_i()$template_min_freq,
            ymax = det_i()$template_max_freq,
            linetype = "dashed", color = box_color, alpha = 0
          ) +
          annotate(
            "label",
            x = Inf, y = input$zoom_freq[2], vjust = 1, hjust = 1,
            label = if (!(det_i()$validation %in% c("TP", "FP", "UN"))) {
              "Not validated"
            } else if (det_i()$validation == "TP") {
              "True Positive"
            } else if (det_i()$validation == "FP") {
              "False Positive"
            } else if (det_i()$validation == "UN") {
              "Unknown"
            },
            fill = if (!(det_i()$validation %in% c("TP", "FP", "UN"))) {
              "white"
            } else if (det_i()$validation == "TP") {
              "#6ae46a"
            } else if (det_i()$validation == "FP") {
              "#ff7e7e"
            } else if (det_i()$validation == "UN") {
              "#ffba52"
            },
            fontface = "bold"
          ) +
          annotate(
            "label",
            x = Inf, y = -Inf, vjust = 0, hjust = 1,
            fontface = "bold",
            label = paste0(
              "Score: ", round(det_i()$peak_score, 3)
            )
          )
      })

      # render the detections spectrogram in the interface
      output$DetectionSpectrogram <- renderPlot({
        req(rec_detection(), det_i(), spectro_detection())
        spectro_detection()
      })

      # Reactive object to store info about the active soundscape
      df_soundscape <- reactive({
        req(df_detections())
        slice_head(df_detections())
      })

      # Reactive object to store the active soundscape wav
      rec_soundscape <- reactiveVal(NULL)
      observe({
        req(df_soundscape())
        if (input$show_soundscape == TRUE) {
          wav_path <- df_soundscape()$soundscape_path
          if (length(wav_path) == 1) {
            res <- readWave(wav_path)
            rec_soundscape(res)
            # Rendering the template HTML player
            if (file.exists(wav_path)) {
              if (input$wav_player_type == "HTML player") {
                # file.remove("temp/soundscape_clip.wav")
                temp_file <- tempfile(
                  pattern = "soundscape_", tmpdir = session_data$temp_path, fileext = ".wav"
                ) %>%
                  gsub("\\\\", "/", .)
                seewave::savewav(res, f = res@samp.rate, filename = temp_file)
                removeUI(selector = "#soundscape_player_selector")
                insertUI(
                  selector = "#soundscape_player", where = "afterEnd",
                  ui = tags$audio(
                    id = "soundscape_player_selector",
                    src = paste0("audio/", basename(temp_file)),
                    type = "audio/wav", autostart = FALSE, controls = TRUE
                  )
                )
                unlink("soundscape_.*.wav")
                list.files(
                  session_data$temp_path,
                  pattern = "soundscape_.*.wav", full.names = TRUE
                ) %>%
                  .[. != temp_file] %>%
                  file.remove()
              } else if (input$wav_player_type != "HTML player") {
                removeUI(selector = "#soundscape_player_selector")
                showElement("play_soundscape")
              }
            }
          }
        } else {
          removeUI(selector = "#soundscape_player_selector")
          hideElement("play_soundscape")
        }
      })

      spectro_soundscape <- reactive({
        if (input$show_soundscape == TRUE) {
          fast_spectro(
            rec = rec_soundscape(), f = df_soundscape()$sample_rate,
            wl = input$wl, ovlp = input$ovlp,
            flim = c(input$zoom_freq[1], input$zoom_freq[2]),
            dyn_range = c(input$dyn_range[1], input$dyn_range[2]),
            color_scale = input$color_scale,
            pitch_shift = input$pitch_shift
          )
        } else {
          return()
        }
      })

      output$SoundscapeSpectrogram <- renderPlot({
        req(spectro_soundscape(), df_detections(), det_i())
        if (input$show_soundscape == TRUE) {
          inactive_detecs <- df_detections() %>%
            filter(detection_id != det_i()$detection_id)

          spectro_soundscape() +
            annotate(
              "rect",
              alpha = 0.2, linewidth = 1, color = "yellow",
              fill = "yellow", color = "yellow",
              xmin = det_i()$detection_start,
              xmax = det_i()$detection_end,
              ymin = det_i()$template_min_freq,
              ymax = det_i()$template_max_freq
            ) +
            annotate(
              "rect",
              alpha = 0, linewidth = 1, linetype = "dashed", color = "#000000",
              fill = "#000000", color = "#000000",
              xmin = inactive_detecs$detection_start,
              xmax = inactive_detecs$detection_end,
              ymin = inactive_detecs$template_min_freq,
              ymax = inactive_detecs$template_max_freq
            ) +
            annotate(
              "label",
              label = paste0(
                det_i()$soundscape_file, "\n",
                "Detection ID: '", det_i()$detection_id, "' in '",
                basename(input$output_path), "'"
              ),
              x = -Inf, y = Inf, hjust = 0, vjust = 1,
              color = "white", fill = "black"
            )
        } else {
          return()
        }
      })

      observeEvent(input$show_soundscape, {
        if (input$show_soundscape == TRUE) {
          showElement("SoundscapeSpectrogram")
        } else {
          hideElement("SoundscapeSpectrogram")
        }
      })

      # in case no wav player is defined, it wil'l use "play", which requires SoX to be instaled in the OS
      observeEvent(input$wav_player_type, {
        x <- input$wav_player_type
        if (x == "R session") {
          updateTextInput(session, "wav_player_path", value = "play")
          tuneR::setWavPlayer("play")
          # todo Adicionar aqui uma opção para detectar o OS e substituir o caminho default para o SoX (https://rug.mnhn.fr/seewave/HTML/MAN/sox.html)
          showElement("play_detec")
          showElement("play_template")
          # if (input$show_soundscape == TRUE) showElement("play_soundscape")
        } else if (x == "External player" & !is.null(input$wav_player_path)) {
          if (file.exists(input$wav_player_path)) {
            tuneR::setWavPlayer(input$wav_player_path)
            showElement("play_detec")
            showElement("play_template")
            # if (input$show_soundscape == TRUE) showElement("play_soundscape")
          } else {
            updateRadioButtons(session, "wav_player_type", selected = "R session")
          }
        }
        if (x == "HTML player") {
          hideElement("play_detec")
          hideElement("play_template")
          hideElement("play_soundscape")
        }
      })

      # Template player (not HTML)
      observeEvent(input$play_template, {
        req(df_template())
        res <- readWave(df_template()$template_path)
        pitch_shift <- abs(input$pitch_shift)
        if (input$pitch_shift < 1) {
          res@samp.rate <- res@samp.rate / pitch_shift
        }
        if (isTRUE(input$visible_bp)) { # ! Quebra quando filtra
          # templates are normalized by default
          res <- normalize(
            object = seewave::fir(
              res,
              f = res@samp.rate,
              from = (input$zoom_freq[1] / pitch_shift) * 1000,
              to = (input$zoom_freq[2] / pitch_shift) * 1000,
              wl = input$wl, output = "Wave"
            ),
            unit = as.character(res@bit), pcm = TRUE #
          )
        }
        tuneR::play(object = res)
      })

      observeEvent(input$hotkeys, {
        req(
          rec_template(), input$hotkeys == "1",
          input$wav_player_type %in% c("R session", "External player")
        )
        req(df_template())
        res <- readWave(df_template()$template_path)
        pitch_shift <- abs(input$pitch_shift)
        if (input$pitch_shift < 1) {
          res@samp.rate <- res@samp.rate / pitch_shift
        }
        if (isTRUE(input$visible_bp)) { # ! Quebra quando filtra
          # templates are normalized by default
          res <- normalize(
            object = seewave::fir(
              res,
              f = res@samp.rate,
              from = (input$zoom_freq[1] / pitch_shift) * 1000,
              to = (input$zoom_freq[2] / pitch_shift) * 1000,
              wl = input$wl, output = "Wave"
            ),
            unit = as.character(res@bit), pcm = TRUE #
          )
        }
        tuneR::play(object = res)
      })

      # Soundscape player (not HTML)
      observeEvent(input$play_soundscape, {
        req(
          rec_soundscape(),
          input$wav_player_type %in% c("R session", "External player")
        )
        tuneR::play(object = rec_soundscape())
      })

      # Detection player (not HTML)
      observeEvent(input$play_detec, {
        req(rec_detection())
        res <- rec_detection()
        pitch_shift <- abs(input$pitch_shift)
        if (input$pitch_shift < 1) {
          res@samp.rate <- res@samp.rate / pitch_shift
        }
        if (isTRUE(input$visible_bp)) {
          res <- seewave::fir(
            res,
            f = res@samp.rate,
            from = (input$zoom_freq[1] / pitch_shift) * 1000,
            to = (input$zoom_freq[2] / pitch_shift) * 1000,
            wl = input$wl, output = "Wave"
          )
        }
        if (isTRUE(input$play_norm)) {
          res <- normalize(
            object = res, unit = as.character(res@bit), pcm = TRUE #
          )
        }
        tuneR::play(object = res)
      })

      observeEvent(input$hotkeys, {
        req(
          rec_detection(), input$hotkeys == "2",
          input$wav_player_type %in% c("R session", "External player")
        )
        res <- rec_detection()
        pitch_shift <- abs(input$pitch_shift)
        if (input$pitch_shift < 1) {
          res@samp.rate <- res@samp.rate / pitch_shift
        }
        if (isTRUE(input$visible_bp)) {
          res <- seewave::fir(
            res,
            f = res@samp.rate,
            from = (input$zoom_freq[1] / pitch_shift) * 1000,
            to = (input$zoom_freq[2] / pitch_shift) * 1000,
            wl = input$wl, output = "Wave"
          )
        }
        if (isTRUE(input$play_norm)) {
          res <- normalize(
            object = res, unit = as.character(res@bit), pcm = TRUE #
          )
        }
        tuneR::play(object = res)
      })

      validation_input <- reactiveVal(NULL)
      observeEvent(input$button_tp, validation_input("TP"))
      observeEvent(input$button_un, validation_input("UN"))
      observeEvent(input$button_fp, validation_input("FP"))

      # Auto navigation as reaction to validation buttons
      observeEvent(list(input$button_tp, input$button_un, input$button_fp), {
        req(df_cut(), det_counter(), input$auto_next == TRUE)
        if (nrow(df_cut()) > det_counter() & det_counter() >= 1) {
          det_counter(det_counter() + 1)
          updateSelectInput(session, "detec",
            selected = df_cut()$detection_id[det_counter()]
          )
        }
      })

      # Update of reactive objects with the value of the
      observeEvent(input$hotkeys, {
        if (input$hotkeys == "q") validation_input("TP")
        if (input$hotkeys == "w") validation_input("UN")
        if (input$hotkeys == "e") validation_input("FP")
      })

      # Reaction to validation when auto_next is TRUE
      observeEvent(input$hotkeys, {
        req(
          df_cut(), det_counter(), input$template_name, input$soundscape_file,
          input$auto_next == TRUE, input$hotkeys %in% c("q", "w", "e")
        )
        if (nrow(df_cut()) > det_counter() & det_counter() >= 1) {
          det_counter(det_counter() + 1)
          updateSelectInput(session, "detec",
            selected = df_cut()$detection_id[det_counter()]
          )
        }
      })

      # reactions to the navigation hotkeys
      observeEvent(input$hotkeys, {
        req(df_cut(), det_counter())
        # Use "d" to navigate to the next detection
        if (input$hotkeys == "d") {
          if (nrow(df_cut()) > det_counter() & det_counter() >= 1) {
            det_counter(det_counter() + 1)
            updateSelectInput(session, "detec",
              selected = df_cut()$detection_id[det_counter()]
            )
          }
        }
        if (input$hotkeys == "a") {
          if (1 < det_counter() & det_counter() <= nrow(df_cut())) {
            det_counter(det_counter() - 1)
            updateSelectInput(session, "detec",
              selected = df_cut()$detection_id[det_counter()]
            )
          } else if (det_counter() == 1) { # attention here
            det_counter(1)
            updateSelectInput(session, "detec",
              selected = df_cut()$detection_id[1]
            )
          }
        }
      })

      # Reactive object with the data used for template diagnostics
      df_diag_input_raw <- reactiveVal(NULL)
      diag_input_procFUN <- function(x) {
        res <- x %>%
          select(template_name, peak_score, validation) %>%
          filter(
            template_name == input$template_name &
              validation %in% c("TP", "FP")
          ) %>%
          mutate(
            validation_bin = case_when(
              validation == "TP" ~ 1, validation == "FP" ~ 0
            )
          )
        return(res)
      }

      # Observe the object validation_input() while controlling overwrite and autosave reactions
      observeEvent(validation_input(), {
        req(input$validation_user, det_i(), df_cut(), df_output())

        res_A <- det_i() %>%
          mutate(
            validation = validation_input(),
            validation_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            validation_user = input$validation_user
          )

        if (input$overwrite == TRUE) {
          if (res_A$validation_user == det_i()$validation_user ||
            is.na(det_i()$validation_user)) {
            det_i(res_A)
            df_cut(rows_update(df_cut(), res_A, by = "detection_id", unmatched = "ignore"))
            df_output(rows_update(df_output(), res_A, by = "detection_id", unmatched = "ignore"))
            validation_input(NULL) # reset after value is passed on forward
          } else {
            showModal(
              modalDialog(
                title = "This detection was already validated by another user!",
                "Overwritting existing validations can only be performed when the user of the current session is the same specified in the input dataset",
                easyClose = TRUE, footer = NULL
              )
            )
          }
        } else if (input$overwrite == FALSE) {
          det_i(res_A)
          df_cut(rows_patch(df_cut(), res_A, by = "detection_id", unmatched = "ignore"))
          df_output(rows_patch(df_output(), res_A, by = "detection_id", unmatched = "ignore"))
          validation_input(NULL) # reset after value is passed on forward
        }

        if (input$nav_autosave == TRUE) {
          fwrite(x = df_output(), file = input$output_path, na = NA, row.names = FALSE)
          showNotification("Detections successfully exported")
          updateProgressBar(
            session = session, id = "prog_bar_full",
            value = length(which(df_output()$validation %in% c("TP", "FP", "UN"))),
            total = nrow(df_output())
          )
          updateProgressBar(
            session = session, id = "prog_bar_subset",
            value = length(which(df_cut()$validation %in% c("TP", "FP", "UN"))),
            total = nrow(df_cut())
          )

          diag_input <- diag_input_procFUN(df_output())
          df_diag_input_raw(diag_input)
        }
      })

      # Set up the reaction of the export button from the UI
      observeEvent(input$button_save, {
        req(df_output(), df_cut(), input$output_path)
        fwrite(x = df_output(), file = input$output_path, na = NA, row.names = FALSE)
        showNotification("Detections successfully exported")
        updateProgressBar(
          session = session, id = "prog_bar_full",
          value = length(which(df_output()$validation %in% c("TP", "FP", "UN"))),
          total = nrow(df_output())
        )
        updateProgressBar(
          session = session, id = "prog_bar_subset",
          value = length(which(df_cut()$validation %in% c("TP", "FP", "UN"))),
          total = nrow(df_cut())
        )
        diag_input <- diag_input_procFUN(df_output())
        df_diag_input_raw(diag_input)
      })

      # Set up the reaction of the export hotkey
      observeEvent(input$hotkeys, {
        req(df_output(), df_cut(), input$output_path, input$hotkeys == "s")
        fwrite(x = df_output(), file = input$output_path, na = NA, row.names = FALSE)
        showNotification("Detections successfully exported")
        updateProgressBar(
          session = session, id = "prog_bar_full",
          value = length(which(df_output()$validation %in% c("TP", "FP", "UN"))),
          total = nrow(df_output())
        )
        updateProgressBar(
          session = session, id = "prog_bar_subset",
          value = length(which(df_cut()$validation %in% c("TP", "FP", "UN"))),
          total = nrow(df_cut())
        )
        diag_input <- diag_input_procFUN(df_output())
        df_diag_input_raw(diag_input)
      })

      observe({
        req(df_cut())
        if (sum(is.na(df_cut()$validation)) == 0) {
          showModal(
            modalDialog(
              title = "All detections from this template are validated",
              "Review the session setup if there are detections from other templates to validate",
              easyClose = TRUE, footer = NULL
            )
          )
        }
      })

      # Render the interactive detection table
      output$res_table <- renderDT(
        {
          req(df_cut())
          df_cut() %>%
            select(
              template_name, soundscape_file, detection_id,
              detection_start, detection_end, template_min_freq, template_max_freq,
              peak_score, validation_user, validation_time, validation
            ) %>%
            datatable(
              editable = FALSE, style = "bootstrap4",
              selection = "single", filter = "none", # escape = FALSE,
              colnames = c(
                "Template", "Soundscape", "ID", "Start", "End",
                "Min. Freq.", "Max. Freq.", "Score", "User",
                "Time Stamp", "Validation"
              )
            ) %>%
            formatRound(c("detection_start", "detection_end", "peak_score"), 3) %>%
            formatRound(c("template_min_freq", "template_max_freq"), 1)
        },
        server = TRUE,
        options = list(lengthChange = FALSE)
      )

      # Catch row selection in the interactive detection table to update the active detection
      observeEvent(input$res_table_rows_selected, {
        req(input$res_table_rows_selected)
        i <- df_cut()$detection_id[input$res_table_rows_selected]
        updateSelectInput(session, "detec", selected = i)
      })

      output$count_i_tab <- renderTable(
        {
          req(df_output(), df_cut())
          df_output() %>%
            filter(template_name == input$template_name) %>%
            group_by(template_name, validation) %>%
            summarise(n = n()) %>%
            ungroup() %>%
            pivot_wider(names_from = "validation", values_from = "n") %>%
            rename(Template = template_name)
        },
        width = "100%"
      )

      output$count_full_tab <- renderTable(
        {
          req(df_output(), df_cut())
          df_output() %>%
            group_by(template_name, validation) %>%
            summarise(n = n()) %>%
            ungroup() %>%
            pivot_wider(names_from = "validation", values_from = "n") %>%
            rename(Template = template_name)
        },
        width = "100%"
      )

      df_diag_input <- reactive({
        req(df_diag_input_raw())
        if ("TP" %in% input$val_subset & "TP" %in% input$val_subset) {
          if (input$diag_balance == "None") {
            df_diag_input_raw()
          } else if (input$diag_balance == "Downsample larger class") {
            df_diag_input_raw() %>%
              mutate(validation = as.factor(validation)) %>%
              caret::downSample(x = ., y = .$validation, yname = "temp") %>%
              select(-temp)
          } else if (input$diag_balance == "Upsample smaller  class") {
            df_diag_input_raw() %>%
              mutate(validation = as.factor(validation)) %>%
              caret::upSample(x = ., y = .$validation, yname = "temp") %>%
              select(-temp)
          } else if (input$diag_balance == "ROSE") {
            df_diag_input_raw() %>%
              select(-template_name) %>%
              mutate(validation = as.factor(validation)) %>%
              {
                ROSE::ROSE(validation ~ ., data = .)$data
              } %>%
              mutate(
                template_name = unique(df_diag_input_raw()$template_name),
                .before = "peak_score"
              )
          }
        }
      })

      mod_res_react <- reactiveVal(NULL)
      mod_plot_react <- reactiveVal(NULL)
      roc_plot_react <- reactiveVal(NULL)
      precrec_plot_react <- reactiveVal(NULL)
      cut_full_tab <- reactiveVal(NULL)
      cut_i_tab <- reactiveVal(NULL)

      # JUAMPY LOOK HERE

      observe({
        req(df_diag_input())
        if (nrow(df_diag_input()) > 2) {
          if (length(unique(df_diag_input()$validation)) == 2) {
            bin_mod <- glm(
              validation_bin ~ peak_score,
              family = "binomial", data = df_diag_input()
            )
            mod_res_react(bin_mod)

            if (input$diag_method == "Manual") {
              binom_cut <- input$diag_cut
            } else if (input$diag_method == "Error = 0.05") {
              binom_cut <- data.frame(peak_score = seq(0, 1, 0.001)) %>%
                mutate(
                  prob = predict(bin_mod, newdata = ., type = "response")
                ) %>%
                {
                  .$peak_score[min(which(.$prob >= 0.95))]
                }
              updateSliderInput(session, "diag_cut", value = binom_cut)
            } else if (input$diag_method == "Error = 0.1") {
              binom_cut <- data.frame(peak_score = seq(0, 1, 0.01)) %>%
                mutate(
                  prob = predict(bin_mod, newdata = ., type = "response")
                ) %>%
                {
                  .$peak_score[min(which(.$prob >= 0.90))]
                }
              updateSliderInput(session, "diag_cut", value = binom_cut)
            } else {
              binom_cut <- NULL
            }

            mod_plot <- ggplot(
              df_diag_input(), aes(x = peak_score, y = validation_bin)
            ) +
              geom_point() +
              stat_smooth(
                formula = y ~ x, method = "glm",
                method.args = list(family = "binomial"),
                se = TRUE, fullrange = T, na.rm = TRUE
              ) +
              geom_vline(
                xintercept = binom_cut, color = "red", linetype = 2, linewidth = 1
              ) +
              ylim(0, 1) +
              xlim(0, 1) +
              labs(
                title = "Binomial regression",
                y = "Probability of validations as TP", x = "Correlation"
              ) +
              coord_equal() +
              theme_bw()
            mod_plot_react(mod_plot)

            # todo ROC curve data in 'seq(0, 1, 0.001)' and not 'seq(0, 1, 0.01)'
            cutpointr_raw <- cutpointr(
              df_diag_input(), peak_score, validation,
              cutpoint = input$diag_cut, silent = TRUE,
              pos_class = "TP", neg_class = "FP", direction = ">=",
              method = oc_manual, use_midpoints = FALSE
            )

            diag_out <- cutpointr_raw$roc_curve[[1]] %>%
              rename(peak_score = x.sorted) %>%
              mutate(
                template_name = input$template_name,
                precision = tp / (tp + fp),
                recall = tp / (tp + fn),
                sensitivity = tp / (tp + fn),
                specificity = tn / (tn + fp),
                selected = FALSE
                # selected = ifelse(peak_score >= input$diag_cut, TRUE, FALSE)
              ) %>%
              relocate(contains("template"), everything()) %>%
              as.data.frame()

            diag_out$selected[max(which(diag_out$peak_score > input$diag_cut))] <- TRUE
            sel_i <- which(diag_out$selected == TRUE)

            cut_full_tab(diag_out)

            roc_plot <- cutpointr::plot_roc(cutpointr_raw) +
              geom_segment(
                aes(x = 0, y = 0, xend = 1, yend = 1),
                linetype = "dashed", color = "grey40"
              ) +
              annotate(
                "label",
                x = 0.75, y = 0.25,
                label = paste0(
                  "AUC = ", round(cutpointr::auc(cutpointr_raw$roc_curve[[1]]), 3)
                )
              ) +
              ylim(0, 1) + xlim(0, 1) +
              coord_equal() +
              theme_bw()
            roc_plot_react(roc_plot)

            precrec_data <- diag_out %>%
              select(peak_score, precision, recall, selected) %>%
              filter(is.finite(peak_score)) %>%
              tidyr::drop_na()

            precrec_plot <- precrec_data %>%
              ggplot(aes(recall, precision)) +
              geom_line() +
              geom_point(
                data = precrec_data[sel_i, ], aes(recall, precision)
              ) +
              annotate(
                "label",
                x = 0.75, y = 0.25,
                label = paste0(
                  "prAUC = ",
                  round(
                    auc_trap(precrec_data$recall, precrec_data$precision), 3
                  )
                )
              ) +
              labs(title = "Precision and Recall") +
              ylim(0, 1) +
              xlim(0, 1) +
              coord_equal() +
              theme_bw()
            precrec_plot_react(precrec_plot)
            cut_i_tab(diag_out[sel_i, ])
          }
        }
      })

      observeEvent(input$diag_method, {
        if (input$diag_method != "Manual") {
          disable("diag_cut")
        } else if (input$diag_method == "Manual") {
          enable("diag_cut")
        }
      })

      observe({
        req(df_cut())
        if ("TP" %in% df_cut()$validation & "FP" %in% df_cut()$validation) {
          enable("diag_balance")
          enable("diag_method")
          enable("diag_cut")
          enable("plot_binomial")
          enable("plot_roc")
          enable("plot_prec_rec")
          shinyjs::show("plot_binomial")
          shinyjs::show("plot_roc")
          shinyjs::show("plot_prec_rec")
        } else {
          disable("diag_balance")
          disable("diag_method")
          disable("diag_cut")
          disable("plot_binomial")
          disable("plot_roc")
          disable("plot_prec_rec")
          shinyjs::hide("plot_binomial")
          shinyjs::hide("plot_roc")
          shinyjs::hide("plot_prec_rec")
        }
      })

      output$cut_i_tab <- renderTable(
        {
          req(cut_i_tab())
          cut_i_tab() %>%
            mutate(tp = as.integer(tp), fp = as.integer(fp)) %>%
            select(-tn, -fn, -tnr, -fnr, -selected) %>%
            setNames(
              c(
                "Template", "Threshold", "TP (n)", "FP (n)",
                "TP Rate", "FP Rate", "Precision", "Recall",
                "Sensibility", "Specificity"
              )
            )
        },
        width = "75%"
      )
      output$plot_binomial <- renderPlot({
        req(mod_plot_react())
        mod_plot_react()
      })
      output$plot_prec_rec <- renderPlot({
        req(roc_plot_react())
        roc_plot_react()
      })
      output$plot_roc <- renderPlot({
        req(precrec_plot_react())
        precrec_plot_react()
      })

      # Load path to export the diagnostics table
      shinyDirChoose(
        input, "diag_tab_path_load",
        session = session,
        roots = volumes
      )
      # Observe the interface input to update the text input
      observeEvent(input$diag_tab_path_load, {
        res <- parseDirPath(volumes, input$diag_tab_path_load) %>%
          as.character() %>%
          gsub("//", "/", .)
        updateTextInput(session, inputId = "diag_tab_path", value = res)
      })
      diag_tab_filename <- reactiveVal()
      diag_tab_default_filename <- reactive({
        req(cut_i_tab(), cut_full_tab(), det_i(), input$input_path)
        res <- paste0(
          "diagnostics_table_",
          # str_remove(basename(input$input_path), ".csv"), "_",
          str_remove(basename(det_i()$template_name), ".WAV|.wav"), "_",
          format(Sys.time(), "%Y%m%d_%H%M%S"), "_",
          "minscore", round(cut_i_tab()$peak_score, 3), ".csv"
        )
      })
      observe({
        req(diag_tab_default_filename())
        updateTextInput(session, "diag_tab_name", value = diag_tab_default_filename())
      })
      # Reset the diagnostics table file name
      observeEvent(input$reset_diag_tab_filename, {
        req(diag_tab_filename())
        updateTextInput(session, "diag_tab_name", value = diag_tab_default_filename())
        diag_tab_filename(diag_tab_default_filename())
      })
      observeEvent(input$diag_tab_name, {
        req(input$diag_tab_name)
        diag_tab_filename(input$diag_tab_name)
      })
      # Export the diagnostics table file by clicking the export button
      observeEvent(input$confirm_diag_tab_export, {
        req(input$diag_tab_path, diag_tab_filename(), cut_full_tab())
        fwrite(
          x = cut_full_tab(), na = NA, row.names = FALSE,
          file = file.path(input$diag_tab_path, diag_tab_filename())
        )
        showNotification("Diagnostics table successfully exported")
      })
      # Export the diagnostics table file by using the hotkey
      observeEvent(input$hotkeys, {
        req(input$hotkeys == "y", input$diag_tab_path, diag_tab_filename(), cut_full_tab())
        fwrite(
          x = cut_full_tab(), na = NA, row.names = FALSE,
          file = file.path(input$diag_tab_path, diag_tab_filename())
        )
        showNotification("Diagnostics table successfully exported")
      })

      # Open the server side for choosing the directory with the presets
      shinyDirChoose(input, "preset_path_load", session = session, roots = volumes)
      # Observe the interface input to update the text input
      observeEvent(input$preset_path_load, {
        res <- parseDirPath(
          volumes, input$preset_path_load
        ) %>%
          as.character() %>%
          gsub("//", "/", .)
        updateTextInput(session, inputId = "preset_path", value = res)
      })

      # Observe the presets path to update the list of available presets
      # # todo Modificar o prefixo dos presets para "validation_preset_"
      # observeEvent(input$preset_path, {
      #   res_list <- list.files(
      #     path = input$preset_path, pattern = "^validation_preset_.*\\.rds$"
      #   ) %>%
      #     str_remove("validation_preset_") %>%
      #     str_remove(".rds")
      #   if (!is.null(res_list)) {
      #     updateSelectInput(
      #       session, "available_presets",
      #       choices = c("Export new preset file...", res_list) #
      #     )
      #   }
      # })

      # # Whatch and store the settings of the active session
      # session_settings <- reactiveVal(NULL)
      # observe({
      #   res <- list(
      #     validation_user = input$validation_user,
      #     templates_path = input$templates_path,
      #     soundscapes_path = input$soundscapes_path,
      #     input_path = input$input_path,
      #     output_path = input$output_path,
      #     wav_player_path = input$wav_player_path,
      #     wav_player_type = input$wav_player_type,
      #     val_subset = input$val_subset,
      #     min_score = as.numeric(input$min_score),
      #     time_pads = as.numeric(input$time_pads),
      #     ovlp = as.numeric(input$ovlp),
      #     wl = as.numeric(input$wl),
      #     dyn_range = as.numeric(input$dyn_range),
      #     color_scale = input$color_scale,
      #     zoom_freq = as.numeric(input$zoom_freq),
      #     nav_shuffle = input$nav_shuffle,
      #     seed = as.numeric(input$seed),
      #     auto_next = input$auto_next,
      #     nav_autosave = input$nav_autosave,
      #     overwrite = input$overwrite,
      #     session_notes = input$session_notes,
      #     wav_cuts_path = input$wav_cuts_path,
      #     spec_path = input$spec_path, diag_tab_path = input$diag_tab_path #
      #   )
      #   session_settings(res)
      # })

      # # teste_val <- reactiveVal(NULL)
      # output$checagem1 <- renderPrint({
      #   req(session_settings())
      #   glimpse(session_settings())
      # })

      # output$checagem2 <- renderPrint({
      #   glimpse(readRDS("app_presets/validation_preset_Salobo_validation.rds"))
      # })

      # # Export current session settings as a rds file
      # observeEvent(input$export_preset, {
      #   req(
      #     session_settings(), input$preset_path, input$available_presets
      #   )
      #   if (
      #     all(c(
      #       nchar(input$validation_user) != 0,
      #       dir.exists(c(input$preset_path, input$templates_path, input$soundscapes_path)),
      #       file.exists(c(input$input_path))
      #     ))
      #   ) {
      #     preset_file <- file.path(
      #       input$preset_path,
      #       paste0("validation_preset_", input$available_presets, ".rds")
      #     )
      #     if (file.exists(preset_file)) {
      #       saved_preset <- readRDS(preset_file)
      #       if (identical(saved_preset, session_settings())) {
      #         shinyalert(
      #           title = "Nothing to be done",
      #           text = tagList(h3("No changes were made in the current preset")),
      #           closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = TRUE,
      #           type = "warning", animation = TRUE, showConfirmButton = FALSE,
      #           showCancelButton = FALSE
      #         )
      #       } else {
      #         what_changed <- rbind(saved_preset, session_settings()) %>%
      #           as.data.frame() %>%
      #           setNames(
      #             list(
      #               "user name", "path to templates", "path to soundscapes",
      #               "path to input table", "path to output table",
      #               "wave player type", "wave player path",
      #               "validation outcome subset", "minimum detection score",
      #               "time pad duration", "overlap", "window length",
      #               "dynamic range", "color scale", "visibile frequency band",
      #               "shuffle navigation", "random seed",
      #               "autonavigate", "autosave", "overwrite",
      #               "session notes", "path to export wav cuts",
      #               "path to export spectrograms",
      #               "path to export diagnostics table"
      #             )
      #           ) %>%
      #           select_if(function(col) length(unique(col)) > 1) %>%
      #           colnames() %>%
      #           paste(collapse = "; ")

      #         shinyalert(
      #           title = "Changes detected in preset",
      #           text = tagList(
      #             h3("There are differences between settings in the current session and in the preset file:"),
      #             h3(what_changed),
      #             h3("Exporting will overwrite the existing preset file. Provide a new name below if if you wish to create a new preset instead:"),
      #             textInput(
      #               "new_preset_name",
      #               label = NULL,
      #               value = input$available_presets, placeholder = TRUE
      #             ),
      #             actionButton("confirm_export_preset", label = "Confirm & Export")
      #           ),
      #           closeOnEsc = TRUE, closeOnClickOutside = FALSE, html = TRUE,
      #           type = "warning", animation = TRUE, showConfirmButton = FALSE,
      #           showCancelButton = TRUE
      #         )
      #       }
      #     } else if (input$available_presets == "Export new preset file...") {
      #       new_name <- paste0(
      #         stringr::str_remove(input$validation_user, ","), "_",
      #         format(Sys.time(), "%Y-%m-%d_%H:%M:%S")
      #       )
      #       shinyalert(
      #         title = "Creating a new preset file",
      #         text = tagList(
      #           h3("Provide a name in the box below:"),
      #           textInput("new_preset_name", label = NULL, value = new_name, placeholder = TRUE),
      #           h4("(*) avoid commas"),
      #           actionButton("confirm_export_preset", label = "Confirm & Export")
      #         ),
      #         closeOnEsc = TRUE, closeOnClickOutside = FALSE, html = TRUE,
      #         type = "warning", animation = TRUE, showConfirmButton = FALSE,
      #         showCancelButton = FALSE
      #       )
      #     }
      #   } else {
      #     shinyalert(
      #       title = "There are missing information in the User setup",
      #       text = tagList(
      #         h3("Complete the missing inputs before exporting a preset"),
      #       ),
      #       closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = TRUE,
      #       type = "warning", animation = TRUE, showConfirmButton = FALSE,
      #       showCancelButton = FALSE
      #     )
      #   }
      # })

      # observeEvent(input$confirm_export_preset, {
      #   req(session_settings(), input$preset_path, input$new_preset_name)
      #   saveRDS(
      #     session_settings(),
      #     file.path(
      #       input$preset_path, paste0("preset_", input$new_preset_name, ".rds")
      #     )
      #   )
      #   res_list <- list.files(
      #     path = input$preset_path, pattern = "^preset_.*\\.rds$"
      #   ) %>%
      #     str_remove("preset_") %>%
      #     str_remove(".rds$")
      #   if (!is.null(res_list)) {
      #     updateSelectInput(
      #       session, "available_presets",
      #       choices = c("Export new preset file...", res_list), #
      #       selected = input$new_preset_name
      #     )
      #   }
      #   showNotification("Preset file successfully exported")
      # })

      # # Import the settins of the preset files to the active session
      # observeEvent(input$import_preset, {
      #   req(input$available_presets)
      #   preset_file <- file.path(
      #     input$preset_path,
      #     paste0("preset_", input$available_presets, ".rds")
      #   )

      #   if (file.exists(preset_file)) {
      #     res <- readRDS(preset_file)
      #     updateTextInput(session, inputId = "validation_user", value = res$validation_user)
      #     updateTextAreaInput(session, inputId = "templates_path", value = res$templates_path)
      #     updateTextAreaInput(session, inputId = "soundscapes_path", value = res$soundscapes_path)
      #     updateTextAreaInput(session, inputId = "wav_player_path", value = res$wav_player_path)
      #     updateRadioButtons(session, inputId = "wav_player_type", selected = res$wav_player_type)
      #     updateTextAreaInput(session, inputId = "input_path", value = res$input_path)
      #     updateTextAreaInput(session, inputId = "output_path", value = res$output_path)
      #     updateSelectizeInput(session, inputId = "val_subset", selected = res$val_subset)
      #     updateSliderInput(session, inputId = "min_score", value = res$cut)
      #     updateSliderInput(session, inputId = "time_pads", value = res$time_pads)
      #     updateCheckboxInput(session, inputId = "auto_next", value = res$auto_next)
      #     updateCheckboxInput(session, inputId = "nav_autosave", value = res$nav_autosave)
      #     updateCheckboxInput(session, inputId = "overwrite", value = res$overwrite)
      #     updateCheckboxInput(session, inputId = "nav_shuffle", value = res$nav_shuffle)
      #     updateNumericInput(session, inputId = "seed", value = res$seed)
      #     updateSliderInput(session, inputId = "ovlp", value = res$ovlp)
      #     updateSliderTextInput(session, inputId = "wl", selected = res$wl)
      #     updateSliderInput(session, inputId = "dyn_range", value = res$dyn_range)
      #     updateSelectInput(session, inputId = "color_scale", selected = res$color_scale)
      #     updateNoUiSliderInput(session, inputId = "zoom_freq", value = res$zoom_freq)
      #     updateTextAreaInput(session, inputId = "session_notes", value = res$session_notes)
      #     updateTextInput(session, inputId = "wav_cuts_path", value = res$wav_cuts_path)
      #     updateTextInput(session, inputId = "spec_path", value = res$spec_path)
      #     updateTextInput(session, inputId = "diag_tab_path", value = res$diag_tab_path)
      #     session_settings(res)
      #     showNotification("Preset file successfully imported")
      #   }
      # })

      # Load path to export the wav file
      shinyDirChoose(
        input, "wav_cuts_path_load",
        session = session,
        roots = volumes
      )
      # Observe the interface input to update the text input
      observeEvent(input$wav_cuts_path_load, {
        res <- parseDirPath(volumes, input$wav_cuts_path_load) %>%
          as.character() %>%
          gsub("//", "/", .)
        updateTextInput(session, inputId = "wav_cuts_path", value = res)
      })
      wav_filename <- reactiveVal()
      wav_default_filename <- reactive({
        req(det_i())
        res <- paste0(
          str_remove(basename(det_i()$soundscape_file), ".WAV|.wav"), "_",
          str_pad(round(det_i()$detection_start, 3), 7, "left", "0"), "-",
          str_pad(round(det_i()$detection_end, 3), 7, "left", "0"), "s_",
          str_pad(round(det_i()$template_min_freq, 3), 6, "left", "0"), "-",
          str_pad(round(det_i()$template_max_freq, 3), 6, "left", "0"), "kHz.wav"
          # todo add species name in the last field of this cut
        )
      })
      observe({
        req(wav_default_filename())
        updateTextInput(session, "wav_cut_name", value = wav_default_filename())
      })
      # Reset the wav file name
      observeEvent(input$reset_wav_cut_name, {
        req(wav_filename(), det_i())
        updateTextInput(session, "wav_cut_name", value = wav_default_filename())
        wav_filename(wav_default_filename())
      })
      observeEvent(input$wav_cut_name, {
        req(input$wav_cut_name)
        wav_filename(input$wav_cut_name)
      })
      # Export the wav file by clicking the export button
      observeEvent(input$confirm_wav_export, {
        req(input$wav_cuts_path, wav_filename(), rec_detection())
        writeWave(
          object = rec_detection(),
          filename = file.path(input$wav_cuts_path, wav_filename())
        )
        showNotification("Detection wave file successfully exported")
      })
      # Export the wav file by using the hotkey
      observeEvent(input$hotkeys, {
        req(input$hotkeys == "r", input$wav_cuts_path, wav_filename(), rec_detection())
        writeWave(
          object = rec_detection(),
          filename = file.path(input$wav_cuts_path, wav_filename())
        )
        showNotification("Detection wave file successfully exported")
      })

      # Load path to export the spectrogram
      shinyDirChoose(
        input, "spec_path_load",
        session = session,
        roots = volumes
      )
      # Observe the interface input to update the text input
      observeEvent(input$spec_path_load, {
        res <- parseDirPath(volumes, input$spec_path_load) %>%
          as.character() %>%
          gsub("//", "/", .)
        updateTextInput(session, inputId = "spec_path", value = res)
      })
      spec_filename <- reactiveVal()
      spec_default_filename <- reactive({
        req(det_i())
        res <- paste0(
          str_remove(basename(det_i()$soundscape_file), ".WAV|.wav"), "_",
          str_pad(round(det_i()$detection_start, 3), 7, "left", "0"), "-",
          str_pad(round(det_i()$detection_end, 3), 7, "left", "0"), "s_",
          str_pad(round(det_i()$template_min_freq, 3), 6, "left", "0"), "-",
          str_pad(round(det_i()$template_max_freq, 3), 6, "left", "0"), "kHz.jpeg"
          # todo add species name in the last field of this cut
        )
      })
      observe({
        req(spec_default_filename())
        updateTextInput(session, "spec_name", value = spec_default_filename())
      })
      # Reset the spectrogram file name
      observeEvent(input$reset_spec_filename, {
        req(spec_filename(), det_i())
        updateTextInput(session, "spec_name", value = spec_default_filename())
        spec_filename(spec_default_filename())
      })
      observeEvent(input$spec_name, {
        req(input$spec_name)
        spec_filename(input$spec_name)
      })
      # Export the wav file by clicking the export button
      observeEvent(input$confirm_spec_export, {
        req(input$spec_path, spec_filename(), rec_detection())
        res <- cowplot::plot_grid(spectro_template(), spectro_detection())
        ggsave(
          filename = file.path(input$spec_path, spec_filename()),
          plot = res, width = 12, height = 6, units = "in", dpi = 72
        )
        showNotification("Detection spectrogram successfully exported")
      })
      # Export the wav file by using the hotkey
      observeEvent(input$hotkeys, {
        req(input$hotkeys == "t", input$spec_path, spec_filename(), rec_detection())
        res <- cowplot::plot_grid(spectro_template(), spectro_detection())
        ggsave(
          filename = file.path(input$spec_path, spec_filename()),
          plot = res, width = 12, height = 6, units = "in", dpi = 72
        )
        showNotification("Detection spectrogram successfully exported")
      })

      # Trigger checks and confirm or cancel the end of the session
      observeEvent(input$end_session, {
        req(df_cut(), df_output())

        # ! BUG

        dfa <- df_cut() %>%
          dplyr::select(tidyr::contains("validation"))
        nrow_unsaved <- 0

        if (file.exists(input$output_path)) {
          dfb <- fread(file = input$output_path) %>%
            as.data.frame() %>%
            dplyr::mutate(validation_time = as.character(validation_time)) %>%
            dplyr::filter(template_name == input$template_name) %>%
            dplyr::select(detection_id, dplyr::contains("validation"))
          nrow_unsaved <- nrow(
            anti_join(
              dfa, dfb,
              by = c("validation_user", "validation_time", "validation")
            )
          )
        }

        if (nrow_unsaved != 0) {
          message_detecs <- paste0(
            "There are ", nrow_unsaved, " rows in the current session thar have different validation inputs from the output '.csv' file. Consider saving before leaving the session."
          )
        } else if (nrow_unsaved == 0) {
          message_detecs <- paste0("There are no differences between the current session and the output '.csv' file.")
        }

        # preset_file <- file.path(
        #   input$preset_path,
        #   paste0("preset_", input$available_presets, ".rds")
        # )
        # showNotification(length(preset_file))
        # if (file.exists(preset_file)) {
        #   saved_preset <- readRDS(preset_file)
        #   what_changed <- rbind(saved_preset, session_settings()) %>%
        #     as.data.frame() %>%
        #     setNames(
        #       list(
        #         "user name", "path to templates", "path to soundscapes",
        #         "path to input table", "path to output table", "wave player path",
        #         "wave player type", "validation outcome subset", "minimum correlation value",
        #         "time pad duration", "overlap", "window length",
        #         "dynamic range", "color scale", "visibile frequency band",
        #         "shuffle navigation", "random seed", "autonavigate", "autosave",
        #         "overwrite", "session notes", "path to export wav cuts",
        #         "path to export spectrograms", "path to export diagnostics table"
        #       )
        #     ) %>%
        #     select_if(function(col) length(unique(col)) > 1) %>%
        #     colnames() %>%
        #     paste(collapse = "; ")
        # }

        # if (!identical(saved_preset, session_settings())) {
        #   message_settings <- paste0(
        #     "The following settings were updated: ", what_changed,
        #     ". Consider update the preset or create a new one before leaving the session."
        #   )
        # } else {
        #   message_settings <- paste0("No setting changes detected.")
        # }

        showModal(
          modalDialog(
            title = "End session",
            paste0(message_detecs),
            footer = tagList(
              actionButton("cancel_exit", "Cancel"),
              actionButton("confirm_exit", "End session")
            )
          )
        )
      })

      # Stop the session after confirmation
      observeEvent(input$confirm_exit, {
        stopApp()
      })

      # General popover options
      pop_up_opt <- list(delay = list(show = 1000, hide = 0))

      # Side bar menu - User setup
      shinyBS::addTooltip(session,
        id = "preset_path",
        title = "Presets available here will be shown in the drop-down menu below",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      # ! tooltips não funcionam em selectize widgets
      # shinyBS::addTooltip(session,
      #   id = "available_presets",
      #   title = "Select a preset or type a new name to create a new one (must contain the .rds extension)",
      #   placement = "right", trigger = "hover", options = pop_up_opt
      # )
      # shinyBS::addTooltip(session,
      #   id = "import_preset",
      #   title = "Apply stored settings to the current session",
      #   placement = "right", trigger = "hover", options = pop_up_opt
      # )
      # shinyBS::addTooltip(session,
      #   id = "export_preset",
      #   title = "Store the current session settings to a preset file. Existing files will be overwritten",
      #   placement = "right", trigger = "hover", options = pop_up_opt
      # )
      shinyBS::addTooltip(session,
        id = "validation_user",
        title = "Recommended format: 'Rosa G. L. M. (avoid commas)",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "templates_path",
        title = "Parent location that contains only template files or folders of these",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "soundscapes_path",
        title = "Parent location that contains only soundscape files or folders of these",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "input_path",
        title = "Complete path to the 'csv.' file that contains detection data",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      # shinyBS::addTooltip(session,
      #   id = "same_detec_file",
      #   title = paste0(
      #     "Toggle this option if you wish to store outputs in the same file used as input. ",
      #     "Choosing to do it with different files, will result in the creation of a new output file or overwriting the file, if the name is the same "
      #   ),
      #   placement = "right", trigger = "click", options = pop_up_opt
      # )
      shinyBS::addTooltip(session,
        id = "output_path",
        title = "Complete path to the 'csv.' file in which validation from this session will be stored",
        placement = "right", trigger = "hover", options = pop_up_opt
      )

      # Side bar menu - Session setup

      shinyBS::addTooltip(session,
        id = "confirm_session_setup",
        title = "<b>Part 2 of 2 required to start the session</b>.",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "template_name",
        title = "Select here one of the templates available in the input file",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "val_subset",
        title = "Select at least one options. Only those selected will be shown.",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "min_score",
        title = "Only detections above this threshold will be shown in this session",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "time_pads",
        title = "Zoom in and out in the time axis of template and detection spectrograms",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "dyn_range",
        title = "Adjust what portion of the amplitude scale is shown in the spectrograms",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "wl",
        title = "Tradeoff between time and frequency resolution",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "ovlp",
        title = "Increase if more resultion is needed. Performance may decrease for values above 80%",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      # shinyBS::addTooltip(session,
      #   id = "color_scale",
      #   title = "",
      #   placement = "right", trigger = "hover", options = pop_up_opt
      # )
      shinyBS::addTooltip(session,
        id = "wav_player_type",
        title = "Select the method to play wav files",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "wav_player_path",
        title = "Necessary when 'External plyer' is selected. If the executable is not available, 'HTML player' will be automatically selected",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "get_templ_pars",
        title = "Set spectrogram parameters to those used to run the detections",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "default_pars",
        title = "Set spectrogram parameters back to the default",
        placement = "right", trigger = "hover", options = pop_up_opt
      )

      # Body - Spectrogram parameters

      shinyBS::addTooltip(session,
        id = "zoom_freq",
        title = "Define the visible frequency band for all spectrograms",
        placement = "right", trigger = "hover", options = pop_up_opt
      )

      # Box - Template spectrogram

      shinyBS::addTooltip(session,
        id = "play_template",
        title = "Hotkey = 1",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )

      # Box - Detection spectrogram
      shinyBS::addTooltip(session,
        id = "play_detec",
        title = "Hotkey = 2",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "prev_detec",
        title = "Navigate to the previous detection. Hotkey: A",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "next_detec",
        title = "Navigate to the next detection. Hotkey: D",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )

      # Box - Validation input

      # shinyBS::addTooltip(session,
      #   id = "soundscape_name",
      #   title = "",
      #   placement = "right", trigger = "hover", options = pop_up_opt
      # )
      shinyBS::addTooltip(session,
        id = "detec",
        title = "Numeric ID of the detection in the original input data",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "seed",
        title = "Random seed for shuffling reproducibility",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "auto_next",
        title = "Automatic navigation to the next detection after a validation",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "nav_shuffle",
        title = "Shuffle the detection order according to the required seed (default = 123). It may require multiple sweeps to validate the full dataset.",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "overwrite",
        title = "Overwrite existing validations",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "nav_autosave",
        title = "Export validation to the output '.csv' file after each validation",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "button_tp",
        title = "Validate active detection as 'True Positive'. Hotkey: Q",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "button_un",
        title = "Validate active detection as 'Unknown'. Hotkey: W",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "button_fp",
        title = "Validate active detection as a 'False Positive'. Hotkey: E",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "button_save",
        title = "Export validations to the output '.csv' file. Hotkey: S",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )

      # BoxTab - Export detection

      shinyBS::addTooltip(session,
        id = "wav_cuts_path",
        title = "Path for exporting an audio sample of the detection",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "wav_cut_name",
        title = "Name of the file with the audio sample ('.wav')",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "reset_wav_cut_name",
        title = "Reset the filename back to the default",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "confirm_wav_export",
        title = "Hotkey: R",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "spec_path",
        title = "Path for exporting the spectrogram image",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "spec_name",
        title = "Name of the spectrogram image file ('.jpeg')",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "reset_spec_filename",
        title = "Reset the filename back to the default",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "confirm_spec_export",
        title = "Hotkey: T",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )

      # BoxTab - Diagnostics

      shinyBS::addTooltip(session,
        id = "diag_balance",
        title = "Consider using one of the methods available here if the number of TP and FP are not similar",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "diag_method",
        title = "Defines if the cutpoint will be automatically defined by the binomial model or manually",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "diag_cut",
        title = "Manual input of cupoint value here",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "diag_tab_path",
        title = "Path for exporting the diagnostics table",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "diag_tab_name",
        title = "Name of the diagnostics table file ('.csv')",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "reset_diag_tab_filename",
        title = "Reset the filename back to the default",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "confirm_diag_tab_export",
        title = "The selected cutpoint will be indicated as the only row in which 'selected = TRUE'",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
    }
  )
}

