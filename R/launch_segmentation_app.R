#' Function to launch the segmentation app
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function launches the segmentation app, which is a Shiny app for
#'   segemntation of WAV recorcings of soundscapes into tables containing
#'   regions of interest (ROIs) and audio cuts of the ROIs. The app settings can
#'   be imported from presets or set manually.
#'
#' @param project_path Path to the project folder, where the segmentation
#'   project will be stored. It is necessary to provide this path if the
#'   segmentation project is being created for the first time or if the user
#'   wants to use relative paths for other input fields.
#' @param preset_path Path to temporary files and other acessory files used by the app.
#' @param user Identification of the user of the segmentation app.
#' @param soundscapes_path Path to soundscapes files.
#' @param roi_tables_path Path to the folder from which the ROI tables will be
#'   imported and to which they will be exported.
#' @param cuts_path Path to the folder from which the ROI audio cuts will be
#'   imported and to which they will be exported.
#' @param labels_file Path to the file containing the list of labels to be used
#'   in the segmentation. This file must be a '.xlsx' spreadsheet in which
#'   available lists are stored as columns, identified in the app menu by their
#'   titles.
#' @param sp_list A character string with the available labels to be used in the
#'   cuirrent session. The default is "CBRO-2021 (Brazil)".
#' @param label_angle Angle between 0 and 180 to draw the ROI labels in the
#'   spectrogram plot.
#' @param show_label If TRUE, ROI labels will be displayed alongside ROI
#'   selections in the sepctrogram.
#' @param wl An integer specifying the length of the FFT window used to
#'   calculate the spectrogram.
#' @param ovlp A numeric value between 0 and 90 specifying the percentage
#'   overlap of windows for computing the spectrogram.
#' @param dyn_range A numeric vector of length 2 specifying the minimum and
#'   maximum relative amplitudes (in dBFS) to be displayed on the spectrogram.
#'   By default c(-100, 0).
#' @param dyn_range_bar A numeric vector of length 2 specifying the limits
#'   to be diplayed on the the bar that controls the dinamic range on the app.
#'   By default c(-200, 10).
#' @param color_scale A character string specifying the color scale to be used
#'   in the spectrogram. Possible values are "viridis", "magma", "inferno",
#'   "cividis", "greyscale 1", or "greyscale 2".
#' @param wav_player_type The type of wav player. "R session" for R
#'   session-based player, "HTML player" for an embedded HTML player in the
#'   interface, and "External player" for playing in an external player. The
#'   last options becomes available only when a valid path to the player
#'   executable is provided in the 'wav_player_path' argument.
#' @param wav_player_path Path to the wav player executable (only for
#'   wav_player_type = "External player")
#' @param session_notes A single character string with related to the current
#'   segmentation session.
#' @param zoom_freq Vector with two numeric values between 0 and the Nyquist
#'   Frequency of the soundscape recording, indicating the frequency values in
#'   kHz of the frequency band to be displayed in the spectrogram.
#' @param nav_autosave If TRUE, navigating between soundscapes will
#'   automatically save the ROI table of the active soundscape.
#' @param pitch_shift A numeric value indicating the pitch shift to be applied
#'  to the soundscape audio. The default is 1, which means no pitch shift.
#' @param visible_bp If TRUE, the visible frequency band will be used as a
#'  bandpass filter for the audio playback.
#'
#' @return Todo
#'
#' @export
#' @import shiny dplyr ggplot2 lubridate seewave stringr tuneR collapse DT
#'  shinyWidgets shinydashboard shinyFiles keys shinyjs shinyBS openxlsx
#'  readxl
#' @importFrom data.table fread fwrite
#' @examples
#' \dontrun{
#' library(monitoraSom)
#' library(usethis)
#'
#' # set the path to the project folder
#' # project_path <- "path/to/project"
#' # in case the current working directory is not an active project, set it with the following command. it may be necessary to restart the R session after that.
#' create_project(path = project_path, open = TRUE, rstudio = TRUE)
#' # check if the currently active working directory matches the project
#' getwd()
#'
#' # set the path to the diorectory where soundscapes are located (it is not recursive)
#' soundscapes_path <- "soundscapes/"
#'
#' # launch the segmentation app
#' launch_segmentation_app(
#'   project_path = ".",
#'   user = "Identify the user here",
#'   soundscapes_path = soundscapes_path
#' )
#' }
launch_segmentation_app <- function(
  project_path = NULL, preset_path = NULL, user = NULL,
  soundscapes_path = NULL, roi_tables_path = NULL, cuts_path = NULL,
  labels_file = NULL, sp_list = "CBRO-2021 (Birds - Brazil)", label_angle = 90,
  show_label = TRUE, dyn_range = c(-100, 0), dyn_range_bar = c(-200, 10),
  wl = 1024, ovlp = 0, color_scale = "inferno",
  wav_player_type = "HTML player", wav_player_path = "play",
  visible_bp = FALSE, play_norm = FALSE, session_notes = NULL, zoom_freq = c(0, 180),
  nav_autosave = TRUE, pitch_shift = 1
  ) {

  # require(shiny)
  # require(dplyr)
  # require(ggplot2)
  # require(lubridate)
  # require(seewave)
  # require(stringr)
  # require(tuneR)
  # require(collapse)
  # require(knitr)
  # require(rmarkdown)
  # require(DT)
  # require(data.table)
  # require(shinyWidgets)
  # require(shinydashboard)
  # require(shinyFiles)
  # require(shiny)
  # require(keys)
  # require(shinyjs)
  # require(shinyBS)

  session_data <- list()

  if (!is.null(project_path)) {
    if (dir.exists(project_path)) {
      session_data$project_path <- project_path
    } else {
      dir.create(project_path)
      if (dir.exists(project_path)) {
        session_data$project_path <- project_path
        warning(
          paste0("The segmentation project destination directory was sucessfully created at '", project_path, "'")
        )
      } else {
        stop("Error! Tried to create the segmentation project directory at '", project_path, "' but failed.")
      }
    }
  }

  if (!is.null(user)) {
    session_data$user <- as.character(gsub(",", "", user))
  } else if (is.null(preset_path)) {
    session_data$user <- as.character(NA)
    warning("Warning! A value was not provided for 'user' variable. Inform the correct value and confirm within the app.")
  }

  if (!dir.exists(soundscapes_path)) {
    session_data$soundscapes_path <- NA
    warning("Warning! The path informed in 'soundscapes_path' was not found locally. Inform the correct value and confirm within the app.")
  } else {
    session_data$soundscapes_path <- soundscapes_path
  }

  if (is.null(roi_tables_path) && is.null(project_path)) {
    session_data$roi_tables_path <- NA
    warning(
      "Warning! No values were provided for 'roi_tables_path' and 'project_path' variables. Provide and confirm it within the app."
    )
  } else if (is.null(roi_tables_path) & !is.null(project_path)) {
    roi_tables_path <- file.path(project_path, "roi_tables/")
    if (!dir.exists(roi_tables_path)) {
      dir.create(roi_tables_path)
      warning(
        "Warning! The path informed in 'roi_tables_path' was not found locally. A new 'roi_tables' directory was sucessfully created at '", roi_tables_path, "'"
      )
    }
    session_data$roi_tables_path <- roi_tables_path
  } else if (!is.null(roi_tables_path)){
    if (!dir.exists(roi_tables_path)) {
      stop(
        "Error! The path informed in 'roi_tables_path' was not found locally and no 'project_path' was informed to create it."
      )
    } else {
      session_data$roi_tables_path <- roi_tables_path
    }

  }

  if (is.null(cuts_path) && is.null(project_path)) {
    session_data$cuts_path <- NA
    warning(
      "Warning! No value was provided for 'cuts_path' and 'project_path' variables. Inform the value and confirm within the app."
    )
  } else if (is.null(cuts_path) & !is.null(project_path)) {
    cuts_path <- file.path(project_path, "roi_cuts/")
    if (!dir.exists(cuts_path)) {
      # if the path does not exist, create it
      dir.create(cuts_path)
      warning(
        "Warning! The path informed in 'cuts_path' was not found locally. A new 'cuts' directory was sucessfully created within the informed 'project_path', at '", cuts_path, "'"
      )
    }
    session_data$cuts_path <- cuts_path
  } else if (!is.null(cuts_path)) {
    if (!dir.exists(cuts_path)) {
      stop(
        "Error! The path informed in 'cuts_path' was not found locally and no 'project_path' was informed to create it."
      )
    } else {
      session_data$cuts_path <- cuts_path
    }
  }

  # check of "label_angle" variable is within 0-180 range and is a multiple of 10
  if (!is.numeric(label_angle)) {
    stop("Error! 'label_angle' must be numeric.")
  } else if (label_angle < 0 | label_angle > 180) {
    stop("Error! 'label_angle' must be between 0 and 180.")
  } else if (label_angle %% 10 != 0) {
    stop("Error! 'label_angle' must be a multiple of 10.")
  } else {
    session_data$label_angle <- label_angle
  }

  if (is.logical(show_label)) {
    session_data$show_label <- show_label
  } else {
    stop("Error! The value assigned to 'show_label' is not logical. Set it to TRUE or FALSE.")
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

  # check if the 'ovelp' is numeric, is a multiple of 10 and is between 0 and 80
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

  # check if the 'color_scale' variable is within the expected alternatives ("viridis", "magma", "inferno", "cividis", "greyscale 1", and "greyscale 2")
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

  # visible_bp
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

  # if (is.null(session_notes)) {
  #   session_data$session_notes <- NA
  # } else {
  #   session_notes <- as.character(session_notes)
  #   if (is.character(session_notes)) {
  #     session_data$session_notes <- session_notes
  #   } else {
  #     stop(
  #       "Error! The value assigned to 'session_notes' cannot be coerced to a character string."
  #     )
  #   }
  # }

  # check if the variable 'zoom_freq' is an integer vector of length equals 2, is between 0 and 180 and the first value is smaller than the second
  if (length(zoom_freq) == 2) {
    if (all(is.numeric(zoom_freq))) {
      if (zoom_freq[1] < zoom_freq[2]) {
        if (zoom_freq[1] >= 0 & zoom_freq[2] <= 180) {
          # round to .5 intervals
          session_data$zoom_freq <- round(zoom_freq * 2) / 2
          if (any(session_data$zoom_freq != zoom_freq)) {
            warning(
              "Warning! The values of 'zoom_freq' were rounded to the nearest 0.5 interval. The values are now: ",
              session_data$zoom_freq[1], " and ", session_data$zoom_freq[2]
            )
          }
        } else {
          stop("Error! 'zoom_freq' must be between 0 and 180.")
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

  # check if the variable 'nav_autosave' is a logical variable
  if (isTRUE(nav_autosave) | isFALSE(nav_autosave)) {
    session_data$nav_autosave <- nav_autosave
  } else {
    stop(
      "Error! The value assigned to 'nav_autosave' is not logical. Set it to TRUE or FALSE."
    )
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

  sp_labels_default <- sp_labels
  if (!is.null(labels_file)) {
    stopifnot(file.exists(labels_file))
    sp_labels_custom <- read_xlsx(labels_file)
  } else if (!is.null(project_path)) {
    sp_labels_custom <- file.path(project_path, "app_presets/sp_labels.xlsx")
    if (file.exists(sp_labels_custom)) {
      sp_labels_custom <- read_xlsx(sp_labels_custom)
    } else {
      write.xlsx(sp_labels_default, sp_labels_custom)
      sp_labels_custom <- sp_labels_default
    }
  }
  sp_labels <- coalesce(sp_labels_custom, sp_labels_default)

  if (!sp_list %in% colnames(sp_labels)) {
    warning("The selected species list is not among the available species lists. Using the default species list.")
  }
  session_data$sp_list <- sp_list

  # if (!is.null(preset_id)) {
  #   if (is.character(preset_id) & length(preset_id) == 1) {
  #     session_data$preset_id <- preset_id
  #     if (!is.null(preset_path)) {
  #       preset_file <- file.path(
  #         preset_path, paste0("segmentation_preset_", preset_id, ".rds")
  #       )
  #       preset_to_export <- list(
  #         user = session_data$user,
  #         soundscapes_path = session_data$soundscapes_path,
  #         roi_tables_path = session_data$roi_tables_path,
  #         cuts_path = session_data$cuts_path,
  #         # fastdisp = session_data$fastdisp,
  #         label_angle = session_data$label_angle,
  #         show_label = session_data$show_label,
  #         dyn_range = session_data$dyn_range,
  #         wl = session_data$wl,
  #         ovlp = session_data$ovlp,
  #         color_scale = session_data$color_scale,
  #         wav_player_type = session_data$wav_player_type,
  #         wav_player_path = session_data$wav_player_path,
  #         session_notes = session_data$session_notes,
  #         zoom_freq = session_data$zoom_freq,
  #         nav_autosave = session_data$nav_autosave,
  #         sp_list = session_data$sp_list,
  #         pitch_shift = session_data$pitch_shift
  #       )
  #       saveRDS(object = preset_to_export, file = preset_file)
  #       message("Preset sucessfully exported to the selected destination!")
  #     }
  #   } else {
  #     stop(
  #       "Error! The value assigned to 'preset_id' is not a character string of length 1."
  #     )
  #   }
  # }

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

  # todo Deactivate when the function is ready
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
                str_pad(sprintf("%.3f", round(roi_start, 3)), 7, pad = "0"), "-",
                str_pad(sprintf("%.3f", round(roi_end, 3)), 7, pad = "0"), "s_",
                str_pad(sprintf("%.3f", round(roi_min_freq, 3)), 6, pad = "0"), "-",
                str_pad(sprintf("%.3f", round(roi_max_freq, 3)), 6, pad = "0"), "kHz_",
                roi_wl, "wl_", roi_ovlp, "ovlp_",
                roi_label, ".wav"
              )
            )
          )
        ) %>%
        rowwise() %>%
        group_split()

      map(
        rois_list,
        ~ cutw(
          wav,
          f = wav@samp.rate, output = "Wave",
          from = .x$roi_start, to = .x$roi_end
        ) %>%
          savewav(filename = file.path(path, .x$cut_name))
      )
    }
  }

  # This function defines where embedded html wav players will look for the files
  addResourcePath("audio", temp_path)
  # resourcePaths()

    # adicionar botao para deposito direto de paths nos caminhos
  hotkeys <- c(
    "q", # delete active ROI
    "w", # zoom in time
    "alt+w", # zoom out to full soundscape duration and frequency band
    "e", # store corrent selection as a ROI
    "ctrl+e", # export current ROI table
    "r", # measurements tool
    "a", # navigate backwards in in time within a sounscape
    "z", # navigate to the previous soundscape
    "s", # zoom out in time
    "alt+s", # zoom out to full soundscape duration
    "d", # navigate forward in in time within a sounscape
    "c", # navigate to the next soudnscape
    "1" # play audio of visible soundscape spectrogram
    # "2" # todo play concatenated audio of all selected rois (default to all if none selected)
  )

  shinyApp(
    ui = dashboardPage(
      header = dashboardHeader(title = "MonitoraSom", titleWidth = "400px"),
      sidebar = dashboardSidebar(
        tags$head(tags$style(HTML(".form-group { margin-bottom: 10px !important; }"))),
        width = "400px",
        sidebarMenu(
          menuItem(
            "User setup",
            tabName = "user_setup_tab", startExpanded = TRUE,
            icon = icon(lib = "glyphicon", "glyphicon glyphicon-user"),
            splitLayout(
              cellWidths = c("75%", "25%"),
              # path to the soundscape directory
              textAreaInput(
                inputId = "preset_path", label = "Path to preset files",
                value = session_data$preset_path,
                placeholder = "Paste or load path here",
                height = "50px", width = "395px", resize = "vertical"
              ),
              shinyDirButton(
                id = "preset_path_load", label = "Load", title = "Teste",
                icon = icon(lib = "glyphicon", "glyphicon glyphicon-import")
              ),
              tags$style(type = "text/css", "#preset_path_load { margin-top: 50px;}")
            ),
            bsTooltip("preset_path",
              title = "Paste the path to app acessory files here.",
              placement = "right", trigger = "hover",
              options = list(delay = list(show = 1000, hide = 0))
            ),
            tags$style(".tooltip {width: 300px;}"),
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
            textInput(
              "user", "User name",
              value = session_data$user, placeholder = "Type your name here"
            ),
            splitLayout(
              cellWidths = c("75%", "25%"),
              icon = icon(lib = "glyphicon", "glyphicon glyphicon-import"),
              textAreaInput("soundscapes_path", "Path to the Soundscapes",
                value = session_data$soundscapes_path,
                placeholder = "Paste or load path here",
                height = "50px", resize = "vertical", width = "100%"
              ),
              shinyDirButton(
                id = "soundscapes_path_load", label = "Load", title = "Teste",
                icon = icon(lib = "glyphicon", "glyphicon glyphicon-import")
              ),
              tags$style(type = "text/css", "#soundscapes_path_load { margin-top: 50px;}")
            ),
            splitLayout(
              cellWidths = c("75%", "25%"),
              icon = icon(lib = "glyphicon", "glyphicon glyphicon-import"),
              textAreaInput("roi_tables_path", "Destination of ROI tables",
                value = session_data$roi_tables_path,
                placeholder = "Paste or load path here",
                height = "50px", resize = "vertical", width = "100%"
              ),
              shinyDirButton(
                id = "roi_tables_path_load", label = "Load", title = "Teste",
                icon = icon(lib = "glyphicon", "glyphicon glyphicon-import")
              ),
              tags$style(type = "text/css", "#roi_tables_path_load { margin-top: 50px;}")
            ),
            splitLayout(
              cellWidths = c("75%", "25%"),
              icon = icon(lib = "glyphicon", "glyphicon glyphicon-import"),
              textAreaInput("cuts_path", "Destination of ROI audio cuts",
                value = session_data$cuts_path,
                placeholder = "Paste or load path here",
                height = "50px", resize = "vertical", width = "100%"
              ),
              shinyDirButton(
                id = "cuts_path_load", label = "Load", title = "Teste",
                icon = icon(lib = "glyphicon", "glyphicon glyphicon-import")
              ),
              tags$style(type = "text/css", "#cuts_path_load { margin-top: 50px;}")
            ),
            actionButton(
              "user_setup_confirm", "Confirm Paths",
              icon = icon(lib = "glyphicon", "glyphicon glyphicon-check"),
              style = "color: #000000; background-color: #33b733; border-color: #288d28; width: 360px;"
            )
          ),

          menuItem(
            "Spectrogram Parameters",
            tabName = "spec_par_tab",
            icon = icon(lib = "glyphicon", "glyphicon glyphicon-cog"),
            splitLayout(
              cellWidths = c("75%", "25%"),
              sliderInput("label_angle", "Adjust label angle (º)",
                min = 0, max = 180, step = 10, value = session_data$label_angle
              ),
              checkboxInput("show_label", "Show label", value = session_data$show_label)
            ),
            sliderInput(
              "dyn_range", "Dynamic range (dB)",
              min = dyn_range_bar[1], max = dyn_range_bar[2], step = 10, value = session_data$dyn_range, width = "100%"
            ),
            sliderTextInput(
              "wl", "Window length",
              choices = c(128, 256, 512, 1024, 2048, 4096, 8192, 16384),
              selected = session_data$wl, grid = TRUE, width = "100%"
            ),
            sliderInput(
              "ovlp", "Overlap (%)",
              min = 0, max = 80, value = session_data$ovlp, step = 10, width = "100%"
            ),
            sliderTextInput(
              "pitch_shift", "Pitch shift (octaves) and slow down (factor)",
              choices = c(-8, -6, -4, -2, 1), selected = session_data$pitch_shift,
              grid = TRUE, width = "100%"
            ),
            selectInput("color_scale", "Color scale",
              choices = c(
                "viridis", "magma", "inferno", "cividis",
                "greyscale 1", "greyscale 2"
              ),
              selected = session_data$color_scale, width = "100%"
            ),
            radioButtons(
              "wav_player_type", "Sound player",
              choices = c("HTML player", "External player", "R session"),
              selected = session_data$wav_player_type, inline = TRUE
            ),
            checkboxInput(
              "visible_bp", "Play only the visible frequency band",
              value = session_data$visible_bp, width = "400px"
            ),
            checkboxInput(
              "play_norm", "Normalize playable audio",
              value = session_data$play_norm, width = "400px"
            ),
            splitLayout(
              cellWidths = c("75%", "25%"),
              textAreaInput("wav_player_path",
                value = session_data$wav_player_path,
                label = "Path to player executable (default = 'play')",
                height = "40px", resize = "vertical"
              ),
              shinyDirButton(
                id = "wav_player_path_load", label = "Load", title = "Teste",
                icon = icon(lib = "glyphicon", "glyphicon glyphicon-export")
              ),
              tags$style(type = "text/css", "#wav_player_path_load { margin-top: 40px;}")
            ),
            actionButton(
              inputId = "default_pars",
              label = "Reset to default parameters", icon = icon("gear"),
              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 360px;"
            )
          )
        ),
        actionButton(
          "end_session", "End Session",
          icon = icon(lib = "glyphicon", "glyphicon glyphicon-log-out"),
          style = "color: #fff; background-color: #b73333; border-color: #8d2c2c; width: 370px;"
        )
      ),
      body = dashboardBody(

        # Make keyboard shotcuts available
        useKeys(),
        keysInput("hotkeys", hotkeys),

        # Set up shinyjs
        useShinyjs(),

        tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),

        # box(width = 12, verbatimTextOutput("checagem1")),

        # Spectrogram box and time zoom slider
        box(
            width = "100%", height = "100%",
            # verbatimTextOutput("checagem1"),
            fluidRow(
              column(
                width = 4,
                selectizeInput(
                  "soundscape_file", "Soundscape (0 of 0)",
                  choices = NULL, width = "100%",
                  options = list(maxOptions = 10000)
                )
              ),
              column(
                width = 6,
                selectizeInput(
                  "roi_table_name", "Active ROI table",
                  choices = NULL, width = "100%"
                )
              ),
              column(
                width = 2,
                selectizeInput(
                  "sp_list", "Available species names",
                  choices = session_data$sp_list, selected = sp_list,
                  width = "100%"
                )
              )
            ),
            fluidRow(
              column(
                width = 1,
                strong("seconds", style = "text-align:right;")
              ),
              column(
                width = 11,
                noUiSliderInput(
                  "zoom_time",
                  label = NULL, min = 0, max = 0.1, step = 0.05,
                  value = c(0, 0.1), width = "100%", behaviour = "drag",
                  update_on = "end"
                )
              )
            ),
            fluidRow(
              column(
                width = 1,
                noUiSliderInput(
                  "zoom_freq", "kHz",
                  min = 0, max = 180, step = 0.5, value = session_data$zoom_freq,
                  direction = "rtl", orientation = "vertical", width = "100px",
                  height = "25vh", behaviour = "drag", format = wNumbFormat(decimals = 1),
                  update_on = "end"
                )
              ),
              column(
                width = 11,
                # jqui_resizable(
                  plotOutput(
                    "spectrogram_plot", brush = "roi_limits", height = "500px" #, width = "1400px"
                  )
                #   ,
                #   options = list(handles = "se")
                # )
              )
            ),
            fluidRow(
              column(
                width = 1,
                checkboxInput(
                  "nav_autosave", "Autosave",
                  value = session_data$nav_autosave
                )
              ),
              column(
                width = 2,
                actionButton(
                  "prev_soundscape_noroi", HTML("Previous<br/>unsegmented"),
                  width = "100%",
                  icon = icon(lib = "glyphicon", "glyphicon glyphicon-fast-backward"),
                  style = "height:54px;"
                )
              ),
              column(
                width = 1,
                actionButton(
                  "prev_soundscape", "Prev.",
                  width = "100%",
                  icon = icon(lib = "glyphicon", "glyphicon glyphicon-step-backward"),
                  style = "height:54px;"
                )
              ),
              column(
                width = 1,
                actionButton(
                  "next_soundscape", "Next",
                  width = "100%",
                  icon = icon(lib = "glyphicon", "glyphicon glyphicon-step-forward"),
                  style = "height:54px;"
                )
              ),
              column(
                width = 2,
                actionButton(
                  "next_soundscape_noroi", HTML("Next<br/>unsegmented"),
                  width = "100%",
                  icon = icon(lib = "glyphicon", "glyphicon glyphicon-fast-forward"),
                  style = "height:54px; width:100%"
                )
              ),
              column(
                width = 2,
                actionButton(
                  "no_soi", HTML("No signals<br/>of interest"),
                  width = "100%", icon = icon("remove"),
                  style = "height:54px;"
                )
              ),
              column( # activate when the R session player is ready
                width = 2,
                actionButton(
                  "play_soundscape", "Play visible",
                  width = "100%", icon = icon("play"),
                  style = "height:54px;"
                ),
                tags$div(id = "visible_soundscape_clip") # set the location of the html player
              )
            )
          ),
        tabBox(
          width = 12, height = "900px",
          id = "tabset1",
          tabPanel(
            "Setup and Input",
            fluidRow(
              column(
                width = 3,
                selectizeInput(
                  "label_name", "Label",
                  choices = NULL, selected = NULL, width = "100%"
                )
              ),
              column(
                width = 3,
                selectizeInput(
                  "signal_type", "Type",
                  choices = c(
                    "anuran - advertisement",
                    "anuran - advertisement - duet",
                    "anuran - advertisement - multiple",
                    "anuran - alarm",
                    "anuran - amplectant",
                    "anuran - courtship",
                    "anuran - displacement",
                    "anuran - distres",
                    "anuran - encounter",
                    "anuran - feeding",
                    "anuran - fighting",
                    "anuran - post-oviposition",
                    "anuran - rain",
                    "anuran - release",
                    "anuran - territorial",
                    "anuran - warning",
                    "bat - echolocation call",
                    "bat - echolocation call - mutiple",
                    "bat - feed buzz",
                    "bat - feed buzz - multiple",
                    "bat - social call",
                    "bat - social call - multiple",
                    "bird - song",
                    "bird - song - duet",
                    "bird - song - multiple",
                    "bird - call",
                    "bird - call - duet",
                    "bird - call - multiple",
                    "bird - mechanical",
                    "anthopophony",
                    "geophony",
                    "other"
                  ),
                  options = list(
                    placeholder = "ex.: song",
                    onInitialize = I('function() { this.setValue(""); }'),
                    "create" = TRUE, "persist" = FALSE
                  ),
                  width = "100%"
                )
              ),
              column(
                width = 2,
                selectizeInput(
                  "label_certainty", "ID certainty",
                  width = "100%",
                  choices = c("certain", "uncertain"),
                  selected = "certain"
                )
              ),
              column(
                width = 1,
                checkboxInput(
                  "lock_label_certainty",
                  label = icon(lib = "glyphicon", "glyphicon glyphicon-lock"),
                  value = FALSE
                )
              ),
              column(
                width = 2,
                selectizeInput(
                  "signal_is_complete", "Complete",
                  width = "100%",
                  choices = c("complete", "incomplete"),
                  selected = "complete"
                )
              ),
              column(
                width = 1,
                checkboxInput(
                  "lock_is_complete",
                  label = icon(lib = "glyphicon", "glyphicon glyphicon-lock"),
                  value = FALSE
                )
              )
            ),
            fluidRow(
              column(
                width = 11,
                textInput(
                  "label_comment", "Additional comments",
                  value = NULL, placeholder = "Type comments here",
                  width = "100%"
                )
              ),
              column(
                width = 1,
                checkboxInput(
                  "lock_comment",
                  label = icon(lib = "glyphicon", "glyphicon glyphicon-lock"),
                  value = FALSE
                )
              )
            ),
            progressBar(
              id = "progress_bar", value = 0, total = 1,
              status = "info", display_pct = TRUE, striped = TRUE
            )
          ),
          tabPanel(
            "ROI table",
            fluidRow(
              column(
                width = 3,
                actionButton("save_roi", "Save",
                  icon = icon(lib = "glyphicon", "glyphicon glyphicon-floppy-saved"),
                  width = "100%"
                )
              ),
              column(
                width = 3,
                actionButton(
                  "export_new_roi_table", "Export new",
                  icon = icon("save", lib = "font-awesome"),
                  style = "color: #ffffff; background-color: #000000",
                  width = "100%"
                )
              ),
              column(
                width = 3,
                actionButton(
                  "export_selected_cut", "Export audio of selected ROI",
                  icon = icon(lib = "glyphicon", "glyphicon glyphicon-music"),
                  width = "100%"
                )
              ),
              column(
                width = 3,
                actionButton(
                  "delete_selected_rois", "Delete selected ROI",
                  icon = icon(lib = "glyphicon", "glyphicon glyphicon-trash"),
                  width = "100%"
                )
              )
            ),
            DTOutput("res_table")
          ),
          tabPanel(#
            "User Manual",
            p("Q - delete the last created ROI"),
            p("W - zoom in on the spectrogram time axis"),
            p("A - navigate backwards in the time axis"),
            p("S - zoom out on the spectrogram time axis"),
            p("D - navigate forward in the time axis"),
            p("E - store current selection as a ROI"),
            p("R - activate/deactivate the measurement tool"),
            p("Z - navigate to the previous soundscape"),
            p("C - navigate to the next seoundscape"),
            p("Alt + W - reset zoom on the spectrogram time and frequency axes"),
            p("Alt + S - reset zoom on the spectrogram time axis"),
            p("Ctrl + E - export the current ROI table"),
            # p("1 - play audio of visible soundscape spectrogram"),
            p("Ctrl + - Zoom in the whole interface"),
            p("Ctrl - - Zoom out the whole interface")
          )
        )
      ),
      skin = "black"
    ),
    server = function(input, output, session) {
      volumes <- getVolumes()()

      # Path inputs
      shinyDirChoose(input, "soundscapes_path_load", roots = volumes)
      soundscapes_path_load <- reactive(input$soundscapes_path_load)
      observeEvent(input$soundscapes_path_load, {
        updateTextInput(
          session,
          inputId = "soundscapes_path",
          value = str_replace(parseDirPath(
            volumes, soundscapes_path_load()
          ), "//", "/")
        )
      })

      shinyDirChoose(input, "roi_tables_path_load", roots = volumes)
      roi_tables_path_load <- reactive(input$roi_tables_path_load)
      observeEvent(input$roi_tables_path_load, {
        updateTextInput(
          session,
          inputId = "roi_tables_path",
          value = str_replace(parseDirPath(
            volumes,
            roi_tables_path_load()
          ), "//", "/")
        )
      })

      shinyDirChoose(input, "cuts_path_load", roots = volumes)
      cuts_path_load <- reactive(input$cuts_path_load)
      observeEvent(input$cuts_path_load, {
        updateTextInput(
          session,
          inputId = "cuts_path",
          value = str_replace(
            parseDirPath(volumes, cuts_path_load()), "//", "/"
          )
        )
      })

      shinyDirChoose(input, "wav_player_path_load", roots = volumes)
      wav_player_path_load <- reactive(input$wav_player_path_load)
      observeEvent(input$wav_player_path_load, {
        updateTextInput(
          session,
          inputId = "wav_player_path",
          value = str_replace(
            parseDirPath(volumes, wav_player_path_load()), "//", "/"
          )
        )
      })

      user_val <- reactiveVal(NULL)
      soundscape_data <- reactiveVal(NULL)
      soundscape_path_val <- reactiveVal(NULL)
      roi_tables_data <- reactiveVal(NULL)
      roi_tables_path_val <- reactiveVal(NULL)
      cuts_path_val <- reactiveVal(NULL)

      observeEvent(input$user_setup_confirm, {
        req(input$user, input$soundscapes_path, input$roi_tables_path, input$cuts_path)

        user_val(input$user)
        soundscape_path_val(input$soundscapes_path)
        roi_tables_path_val(input$roi_tables_path)
        cuts_path_val(input$cuts_path)

        if (dir.exists(roi_tables_path_val()) & dir.exists(roi_tables_path_val())) {
          list_soundscapes <- list.files(
            soundscape_path_val(),
            pattern = ".wav", full.names = TRUE,
            ignore.case = TRUE
          )
          roi_tables_raw <- list.files(
            roi_tables_path_val(),
            pattern = ".csv", full.names = TRUE,
            ignore.case = TRUE
          )
          roi_tables <- data.frame(
            roi_table_name = basename(roi_tables_raw),
            roi_table_path = roi_tables_raw
          )
          if (length(list_soundscapes) > 0) {
            soundscape_data_res <- list_soundscapes %>%
              data.frame(soundscape_path = ., soundscape_file = basename(.)) |>
              fmutate(
                roi_table_prefix = str_remove(soundscape_file, ".wav|.WAV"),
                n_char = nchar(roi_table_prefix)
              ) |>
              fmutate(
                has_table = (
                  roi_table_prefix %in% substr(
                    list.files(
                      roi_tables_path_val(),
                      pattern = ".csv", full.names = FALSE,
                      ignore.case = TRUE
                    ), 1, n_char
                  )
                )
              )
            updateSelectizeInput(
              session, "soundscape_file",
              choices = soundscape_data_res$soundscape_file, server = TRUE
            )
            soundscape_data(soundscape_data_res)
            showNotification("Setup sucessfull!", type = "message")
            roi_tables_data(roi_tables)
          } else {
            showModal(
              modalDialog(
                title = "Setup error",
                "There are no readable WAV files in the provided Soundscape path",
                footer = tagList(modalButton("OK")), easyClose = TRUE
              )
            )
          }
        } else if (!dir.exists(soundscape_path_val()) & dir.exists(roi_tables_path_val())) {
          showModal(
            modalDialog(
              title = "Setup error",
              "The provided Soundscape path does not exist",
              footer = tagList(modalButton("OK")), easyClose = TRUE
            )
          )
        } else if (dir.exists(soundscape_path_val()) & !dir.exists(roi_tables_path_val())) {
          showModal(
            modalDialog(
              title = "Setup error",
              "The provided path to ROI tables does not exist",
              footer = tagList(modalButton("OK")), easyClose = TRUE
            )
          )
        } else if (!dir.exists(soundscape_path_val()) & !dir.exists(roi_tables_path_val())) {
          showModal(
            modalDialog(
              title = "Setup error",
              "The paths to Soundscapes and to ROI tables do not exist",
              footer = tagList(modalButton("OK")), easyClose = TRUE
            )
          )
        }

        if (!dir.exists(cuts_path_val())) {
          showModal(
            modalDialog(
              title = "Setup error",
              "The path to export audio and spectrograms from ROIs does not exist",
              footer = tagList(modalButton("OK")), easyClose = TRUE
            )
          )
          disable("export_selected_cut")
        } else {
          enable("export_selected_cut")
        }
      })

      observe({
        updateSelectizeInput(
          session, "sp_list",
          choices = colnames(sp_labels), selected = sp_list,
          server = TRUE
        )
      })

      observeEvent(input$sp_list, {
        req(input$sp_list)
        res <- sp_labels %>% pull(input$sp_list)
        updateSelectizeInput(
          session, "label_name",
          choices = c(NA, res),
          selected = NULL, server = TRUE,
          options = list(
            placeholder = "Input the species name here",
            onInitialize = I('function() { this.setValue(""); }'),
            "create" = TRUE, "persist" = FALSE
          )
        )
      })

      # Reactive object with the soundscape recording
      rec_soundscape <- reactiveVal(NULL)
      duration_val <- reactiveVal(NULL)
      observe({
        req(soundscape_data())
        i <- which(soundscape_data()$soundscape_file == input$soundscape_file)
        wav_path <- soundscape_data()$soundscape_path[i]

        if (!is.null(wav_path) & length(wav_path) == 1) {
          res <- readWave(wav_path)
          duration_val(duration(res))

          updateNoUiSliderInput(
            session,
            inputId = "zoom_time",
            range = c(0, duration_val()),
            value = c(0, ifelse(duration_val() >= 60, 60, duration_val()))
          )
          updateSelectizeInput(
            session,
            inputId = "soundscape_file",
            label = paste0(
              "Soundscape (", i, " of ", nrow(soundscape_data()), ")"
            )
          )
          updateNoUiSliderInput(
            session,
            inputId = "zoom_freq",
            range = c(0, (res@samp.rate / 2000) - 1)
          )
          rec_soundscape(res)
        }
      })

      wav_path_val <- reactiveVal(NULL)
      observe({
        req(soundscape_data(), rec_soundscape())
        i <- which(soundscape_data()$soundscape_file == input$soundscape_file)
        wav_path <- soundscape_data()$soundscape_path[i]
        wav_path_val(wav_path)
        if (!is.null(wav_path) & length(wav_path) == 1) {
          if (file.exists(wav_path) & input$wav_player_type == "HTML player") {
            temp_file <- tempfile(tmpdir = session_data$temp_path, fileext = ".wav") %>%
              gsub("\\\\", "/", .)
            pitch_shift <- abs(input$pitch_shift)
            res_cut <- cutw(
              rec_soundscape(),
              from = input$zoom_time[1],
              to = ifelse(input$zoom_time[2] > duration_val(), duration_val(), input$zoom_time[2]),
              output = "Wave"
            )
            if (input$pitch_shift < 1) {
              res_cut@samp.rate <- res_cut@samp.rate / pitch_shift
            }
            if (isTRUE(input$visible_bp)) {
              res_cut <- fir(
                res_cut,
                f = res_cut@samp.rate,
                from = (input$zoom_freq[1] / pitch_shift) * 1000,
                to = (input$zoom_freq[2] / pitch_shift) * 1000,
                wl = input$wl, output = "Wave"
              )
            }
            if (isTRUE(input$play_norm)) {
              res_cut <- normalize(
                object = res_cut,
                unit = as.character(res_cut@bit),
                pcm <- TRUE
              )
            }
            savewav(res_cut, f = res_cut@samp.rate, filename = temp_file)
            removeUI(selector = "#visible_soundscape_clip_selector")
            insertUI(
              selector = "#visible_soundscape_clip", where = "afterEnd",
              ui = tags$audio(
                id = "visible_soundscape_clip_selector",
                src = paste0("audio/", basename(temp_file)),
                type = "audio/wav", autostart = FALSE, controls = TRUE # , style = "display: none;"
              )
            )
            unlink("*.wav")
            to_remove <- list.files(session_data$temp_path, pattern = ".wav", full.names = TRUE)
            file.remove(to_remove[to_remove != temp_file])
          } else {
            removeUI(selector = "#visible_soundscape_clip_selector")
          }
        }
      })

      observeEvent(input$wav_player_type, {
        x <- input$wav_player_type
        if (x == "R session") {
          updateTextInput(session, "wav_player_path", value = "play")
          setWavPlayer("play")
          # todo Adicionar aqui uma opçao para detectar o OS e substituir o caminho default para o SoX (https://rug.mnhn.fr/seewave/HTML/MAN/sox.html)
          showElement("play_soundscape")
        } else if (x == "External player" & !is.null(input$wav_player_path)) {
          if (file.exists(input$wav_player_path)) {
            setWavPlayer(input$wav_player_path)
            showElement("play_soundscape")
          } else {
            updateRadioButtons(session, "wav_player_type", selected = "R session")
          }
        }
        if (x == "HTML player") {
          hideElement("play_soundscape")
        }
      })

      # Filter available ROI tables based in the prefix retrieved from the
      # soundscape file names
      alt_roitabs_meta <- reactiveVal(NULL)
      observeEvent(input$soundscape_file, {
        req(input$soundscape_file)
        roi_list <- list.files(
          roi_tables_path_val(),
          pattern = ".csv", full.names = TRUE, ignore.case = TRUE
        )
        roi_tables <- data.frame(
          roi_table_name = basename(roi_list), roi_table_path = roi_list
        )
        roi_tables_data(roi_tables)

        roi_prefix <- subset(
          soundscape_data(), soundscape_file == input$soundscape_file
        )[1, "roi_table_prefix"]

        name_pattern <- paste0(roi_prefix, ".*\\.csv$")
        if (any(grepl(name_pattern, roi_tables_data()$roi_table_name))) {
          roi_table_alts <- roi_tables_data() %>%
            fsubset(grepl(name_pattern, roi_table_name))
        } else {
          roi_table_alts <- data.frame(
            roi_table_name = gsub(
              ".wav|.WAV",
              paste0("_roi_", user_val(), "_", format(Sys.time(), "%Y%m%d%H%M%S.csv")),
              input$soundscape_file
            )
          )
          roi_table_alts$roi_table_path <- paste0(
            roi_tables_path_val(), "/", roi_table_alts$roi_table_name
          )
        }
        alt_roitabs_meta(arrange(roi_table_alts, desc(roi_table_name)))

        # Update the menu with the available ROI tables
        updateSelectizeInput(
          session, "roi_table_name",
          choices = roi_table_alts$roi_table_name,
          selected = roi_table_alts$roi_table_name[1],
          server = TRUE
        )
      })

      # # Create a reactive object with the content of the active ROI table
      roi_values <- reactiveVal(NULL)
      observeEvent(input$roi_table_name, {
        req(alt_roitabs_meta(), input$roi_table_name, user_val())
        active_roi_table_path <- alt_roitabs_meta() %>%
          fsubset(roi_table_name == input$roi_table_name) %>%
          pull(roi_table_path)

        # Unless the file exists, the reactive object remains empty
        if (file.exists(active_roi_table_path)) {
          # res <- as.data.frame(fread(file = active_roi_table_path))
          var_names <- c(
            "soundscape_path", "soundscape_file", "roi_user", "roi_input_timestamp",
            "roi_label", "roi_start", "roi_end", "roi_min_freq", "roi_max_freq",
            "roi_type", "roi_label_confidence", "roi_is_complete", "roi_comment",
            "roi_wl", "roi_ovlp", "roi_sample_rate", "roi_pitch_shift"
          )
          res_raw <- fread(file = active_roi_table_path) %>%
            as.data.frame()
          missing_vars <- var_names[!var_names %in% names(res_raw)]
          if (length(missing_vars) != 0) res_raw[missing_vars] <- NA
          res <- res_raw %>%
            transmute(
              soundscape_path = soundscape_path,
              soundscape_file = soundscape_file,
              roi_user = roi_user,
              roi_input_timestamp = format(roi_input_timestamp, "%Y-%m-%d %H:%M:%S"),
              roi_label = roi_label,
              roi_start = roi_start,
              roi_end = roi_end,
              roi_min_freq = roi_min_freq,
              roi_max_freq = roi_max_freq,
              roi_type = roi_type,
              roi_label_confidence = roi_label_confidence,
              roi_is_complete = roi_is_complete,
              roi_comment = roi_comment,
              roi_wl = roi_wl,
              roi_ovlp = roi_ovlp,
              roi_sample_rate = roi_sample_rate,
              roi_pitch_shift = roi_pitch_shift
            )
          roi_values(res)
        } else {
          res <- data.frame(
            soundscape_path = NA, # new
            soundscape_file = NA, # ex-soundscape
            roi_user = NA, # ex-user
            roi_input_timestamp = NA, # ex-time_stamp
            roi_label = NA, # ex-label
            roi_start = NA, # ex-start
            roi_end = NA, # ex-end
            roi_min_freq = NA, # fica como está
            roi_max_freq = NA, # fica como está
            roi_type = NA,
            roi_label_confidence = NA, # label_certainty
            roi_is_complete = NA,
            roi_comment = NA, # ex-label_comment
            roi_wl = NA,
            roi_ovlp = NA,
            roi_sample_rate = NA,
            roi_pitch_shift = NA
          )
          roi_values(res)
        }
      })

      ruler <- reactiveVal(NULL)

      observeEvent(input$hotkeys, {
        req(roi_values())
        current_rois <- tibble(roi_values())

        if (all(is.na(current_rois))) {
          if (input$hotkeys == "e") {
            roi_i <- tibble(
              soundscape_path = wav_path_val(),
              soundscape_file = input$soundscape_file,
              roi_user = user_val(),
              roi_input_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              roi_label = input$label_name,
              roi_start = input$roi_limits$xmin,
              roi_end = input$roi_limits$xmax,
              roi_min_freq = input$roi_limits$ymin,
              roi_max_freq = input$roi_limits$ymax,
              roi_type = input$signal_type,
              roi_label_confidence = input$label_certainty, # label_certainty
              roi_is_complete = input$signal_is_complete,
              roi_comment = input$label_comment, # ex-label_comment
              roi_wl = input$wl,
              roi_ovlp = input$ovlp,
              roi_sample_rate = rec_soundscape()@samp.rate,
              roi_pitch_shift = input$pitch_shift
            )
            roi_values(roi_i)
          }
        } else {
          if (input$hotkeys == "e") {
            if (fnrow(current_rois) >= 1) {
              roi_i <- tibble(
                soundscape_path = wav_path_val(),
                soundscape_file = input$soundscape_file,
                roi_user = user_val(),
                roi_input_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                roi_label = input$label_name,
                roi_start = input$roi_limits$xmin,
                roi_end = input$roi_limits$xmax,
                roi_min_freq = input$roi_limits$ymin,
                roi_max_freq = input$roi_limits$ymax,
                roi_type = input$signal_type,
                roi_label_confidence = input$label_certainty, # label_certainty
                roi_is_complete = input$signal_is_complete,
                roi_comment = input$label_comment, # ex-label_comment
                roi_wl = input$wl,
                roi_ovlp = input$ovlp,
                roi_sample_rate = rec_soundscape()@samp.rate,
                roi_pitch_shift = input$pitch_shift
              )
              res <- tibble(bind_rows(current_rois, roi_i))
              roi_values(res)
            }
          }
          if (input$hotkeys == "q") {
            if (fnrow(current_rois) > 1) {
              res <- head(current_rois, -1)
              roi_values(res)
            } else {
              roi_i_empty <- tibble(
                soundscape_path = NA,
                soundscape_file = NA,
                roi_user = NA,
                roi_input_timestamp = NA,
                roi_label = NA,
                roi_start = NA,
                roi_end = NA,
                roi_min_freq = NA,
                roi_max_freq = NA,
                roi_type = NA,
                roi_label_confidence = NA,
                roi_is_complete = NA,
                roi_comment = NA,
                roi_wl = NA,
                roi_ovlp = NA,
                roi_sample_rate = NA,
                roi_pitch_shift = NA
              )
              roi_values(roi_i_empty)
            }
          }
        }

        if (input$lock_label_certainty == FALSE) {
          updateSelectInput(session, "label_certainty", selected = "certain")
        }
        if (input$lock_is_complete == FALSE) {
          updateSelectInput(session, "signal_is_complete", selected = "complete")
        }
        if (input$lock_comment == FALSE) {
          updateTextInput(session, "label_comment", value = NA)
        }

        if (input$hotkeys == "r") {
          if (is.null(ruler())) {
            res <- data.frame(
              soundscape_path = wav_path_val(),
              soundscape_file = input$soundscape_file,
              start = input$roi_limits$xmin,
              end = input$roi_limits$xmax,
              duration = input$roi_limits$xmax - input$roi_limits$xmin,
              min_freq = input$roi_limits$ymin,
              max_freq = input$roi_limits$ymax,
              bandwidth = input$roi_limits$ymax - input$roi_limits$ymin
            )
            ruler(res)
          } else {
            ruler(NULL)
          }
        }

        if (input$hotkeys == "w") {
          if (input$zoom_time[1] < input$zoom_time[2] & duration_val() > 1) {
            tlim <- input$zoom_time
            timepad <- (tlim[2] - tlim[1]) / 4
            zoom_val <- c(tlim[1] + timepad, tlim[2] - timepad)
            updateNoUiSliderInput(session, inputId = "zoom_time", value = zoom_val)
          }
        }

        if (input$hotkeys == "s") {
          if (input$zoom_time[1] < input$zoom_time[2] & duration_val() > 1) {
            tlim <- input$zoom_time
            timepad <- (tlim[2] - tlim[1]) / 4
            t1 <- tlim[1] - (2 * timepad)
            if (t1 < 0) t1 <- 0
            t2 <- tlim[2] + (2 * timepad)
            if (t2 >= duration_val()) {
              if (duration_val() > 60) t2 <- 60 else t2 <- duration_val()
            }
            zoom_val <- c(t1, t2)
            updateNoUiSliderInput(session, inputId = "zoom_time", value = zoom_val)
          }
        }

        if (input$hotkeys == "alt+s") {
          req(duration_val())
          if (duration_val() > 60) {
            updateNoUiSliderInput(session, inputId = "zoom_time", value = c(0, 60))
          } else {
            updateNoUiSliderInput(session, inputId = "zoom_time", value = c(0, duration_val()))
          }
        }

        if (input$hotkeys == "alt+w") {
          req(duration_val(), rec_soundscape())
          if (duration_val() > 60) {
            updateNoUiSliderInput(session, inputId = "zoom_time", value = c(0, 60))
          } else {
            updateNoUiSliderInput(session, inputId = "zoom_time", value = c(0, duration_val()))
          }

          updateNoUiSliderInput(
            session,
            inputId = "zoom_freq", value = c(0, (rec_soundscape()@samp.rate / 2000) - 1)
          )
        }

        if (input$hotkeys == "d") {
          tlim <- input$zoom_time
          timepad <- tlim[2] - tlim[1]
          if (
            tlim[2] >= duration_val() & tlim[1] >= (duration_val() - timepad) &
              timepad == (duration_val() - (duration_val() - timepad))
          ) {
            updateNoUiSliderInput(
              session,
              inputId = "zoom_time",
              value = c(duration_val() - timepad, duration_val())
            )
          } else {
            updateNoUiSliderInput(
              session,
              inputId = "zoom_time", value = tlim + (timepad / 2)
            )
          }
        }

        if (input$hotkeys == "a") {
          tlim <- input$zoom_time
          timepad <- tlim[2] - tlim[1]
          if (tlim[1] > 0 & tlim[2] > timepad) {
            updateNoUiSliderInput(session, inputId = "zoom_time", value = tlim - timepad / 2)
          } else {
            updateNoUiSliderInput(session, inputId = "zoom_time", value = c(0, timepad))
          }
        }

        if (input$hotkeys == "1") {
          if (
            !is.null(rec_soundscape()) &
              input$wav_player_type %in% c("R session", "External player")
          ) {
            play(
              cutw(
                rec_soundscape(),
                from = input$zoom_time[1], to = input$zoom_time[2],
                output = "Wave"
              )
            )
          }
        }

        if (input$hotkeys == "ctrl+e") {
          req(roi_values(), input$roi_table_name, alt_roitabs_meta())
          if (!all(is.na(roi_values())) & fnrow(roi_values()) > 0) {
            filename <- file.path(roi_tables_path_val(), input$roi_table_name)
            fwrite(roi_values(), filename, row.names = FALSE)
            progress_tracker$df$has_table[
              which(soundscape_data()$soundscape_file == input$soundscape_file)
            ] <- TRUE
            n_done <- length(which(progress_tracker$df$has_table == TRUE))
            n_total <- nrow(progress_tracker$df)
            updateProgressBar(
              session = session, id = "progress_bar",
              value = n_done, total = n_total
            )
            showNotification("ROI table sucessfully exported", type = "message")
            if (n_done == n_total) {
              showNotification("All recordings were segmented!", type = "message")
            }
          }
        }

        if (input$hotkeys == "z") {
          vec_soundscapes <- soundscape_data()$soundscape_file
          i <- which(vec_soundscapes == input$soundscape_file)
          if (input$nav_autosave == TRUE) {
            if (!all(is.na(roi_values())) & fnrow(roi_values()) > 0) {
              filename <- file.path(roi_tables_path_val(), input$roi_table_name)
              fwrite(roi_values(), filename, row.names = FALSE)
              progress_tracker$df$has_table[i] <- TRUE
              n_done <- length(which(progress_tracker$df$has_table == TRUE))
              n_total <- nrow(progress_tracker$df)
              updateProgressBar(
                session = session, id = "progress_bar",
                value = n_done, total = n_total
              )
              showNotification("ROI table sucessfully exported", type = "message")
              if (n_done == n_total) {
                showNotification("All recordings were segmented!", type = "message")
              }
            }
          }
          if (length(vec_soundscapes) >= i & i > 1) {
            updateSelectInput(
              session, "soundscape_file",
              selected = vec_soundscapes[i - 1]
            )
          }
        }

        if (input$hotkeys == "c") {
          vec_soundscapes <- soundscape_data()$soundscape_file
          i <- which(vec_soundscapes == input$soundscape_file)
          if (input$nav_autosave == TRUE) {
            if (!all(is.na(roi_values())) & fnrow(roi_values()) > 0) {
              filename <- file.path(roi_tables_path_val(), input$roi_table_name)
              fwrite(roi_values(), filename, row.names = FALSE)
              progress_tracker$df$has_table[i] <- TRUE
              n_done <- length(which(progress_tracker$df$has_table == TRUE))
              n_total <- nrow(progress_tracker$df)
              updateProgressBar(
                session = session, id = "progress_bar",
                value = n_done, total = n_total
              )
              showNotification("ROI table sucessfully exported", type = "message")
              if (n_done == n_total) {
                showNotification("All recordings were segmented!", type = "message")
              }
            }
          }
          if (length(vec_soundscapes) > i & i >= 1) {
            updateSelectInput(
              session, "soundscape_file",
              selected = vec_soundscapes[i + 1]
            )
          }
        }
      })

      # Play the soundscape
      observeEvent(input$play_soundscape, {
        req(
          input$wav_player_path, rec_soundscape(),
          input$wav_player_type %in% c("R session", "External player")
        )
        rec_to_play <- rec_soundscape()
        pitch_shift <- abs(input$pitch_shift)
        if (input$pitch_shift < 1) {
          rec_to_play@samp.rate <- rec_to_play@samp.rate / pitch_shift
        }
        rec_to_play <- cutw(
          rec_to_play,
          from = input$zoom_time[1] * pitch_shift, to = input$zoom_time[2] * pitch_shift,
          output = "Wave"
        )
        if (isTRUE(input$visible_bp)) {
          rec_to_play <- fir(
            rec_to_play,
            f = rec_to_play@samp.rate,
            from = (input$zoom_freq[1] / pitch_shift) * 1000,
            to = (input$zoom_freq[2] / pitch_shift) * 1000,
            wl = input$wl, output = "Wave"
          )
        }
        if (isTRUE(input$play_norm)) {
          rec_to_play <- normalize(
            object = rec_to_play, unit = as.character(rec_to_play@bit), pcm = TRUE #
          )
        }
        play(rec_to_play,player = "play")
      })

      progress_tracker <- reactiveValues(df = NULL)
      observe({
        req(soundscape_data())
        progress_tracker$df <- soundscape_data()
      })

      observeEvent(input$export_new_roi_table, {
        req(roi_values())
        if (!all(is.na(roi_values())) & fnrow(roi_values()) > 0) {
          filename <- file.path(
            roi_tables_path_val(),
            str_replace(
              input$soundscape_file, ".wav|.WAV",
              paste0(
                "_roi_", user_val(), "_", format(Sys.time(), "%Y%m%d%H%M%S.csv")
              )
            )
          )
          fwrite(roi_values(), filename, row.names = FALSE)
          progress_tracker$df$has_table[
            which(soundscape_data()$soundscape_file == input$soundscape_file)
          ] <- TRUE
          n_done <- length(which(progress_tracker$df$has_table == TRUE))
          n_total <- nrow(progress_tracker$df)
          updateProgressBar(
            session = session, id = "progress_bar",
            value = n_done, total = n_total
          )
          showNotification("New ROI table sucessfully exported", type = "message")
          if (n_done == n_total) {
            showNotification("All recordings were segmented!", type = "message")
          }
        }
      })

      observeEvent(input$save_roi, {
        req(roi_values(), input$roi_table_name, alt_roitabs_meta())
        if (!all(is.na(roi_values())) & fnrow(roi_values()) > 0) {
          filename <- file.path(roi_tables_path_val(), input$roi_table_name)
          fwrite(roi_values(), filename, row.names = FALSE)
          progress_tracker$df$has_table[
            which(soundscape_data()$soundscape_file == input$soundscape_file)
          ] <- TRUE
          n_done <- length(which(progress_tracker$df$has_table == TRUE))
          n_total <- nrow(progress_tracker$df)
          updateProgressBar(
            session = session, id = "progress_bar",
            value = n_done, total = n_total
          )
          showNotification("ROI table sucessfully exported", type = "message")
          if (n_done == n_total) {
            showNotification("All recordings were segmented!", type = "message")
          }
        }
      })

      observeEvent(input$prev_soundscape, {
        vec_soundscapes <- soundscape_data()$soundscape_file
        i <- which(vec_soundscapes == input$soundscape_file)
        if (input$nav_autosave == TRUE) {
          if (!all(is.na(roi_values())) & fnrow(roi_values()) > 0) {
            filename <- file.path(roi_tables_path_val(), input$roi_table_name)
            fwrite(roi_values(), filename, row.names = FALSE)
            progress_tracker$df$has_table[i] <- TRUE
            n_done <- length(which(progress_tracker$df$has_table == TRUE))
            n_total <- nrow(progress_tracker$df)
            updateProgressBar(
              session = session, id = "progress_bar",
              value = n_done, total = n_total
            )
            showNotification("ROI table sucessfully exported", type = "message")
            if (n_done == n_total) {
              showNotification("All recordings were segmented!", type = "message")
            }
          }
        }
        if (length(vec_soundscapes) >= i & i > 1) {
          updateSelectInput(
            session, "soundscape_file",
            selected = vec_soundscapes[i - 1]
          )
        }
      })

      observeEvent(input$next_soundscape, {
        vec_soundscapes <- soundscape_data()$soundscape_file
        i <- which(vec_soundscapes == input$soundscape_file)
        if (input$nav_autosave == TRUE) {
          if (!all(is.na(roi_values())) & fnrow(roi_values()) > 0) {
            filename <- file.path(roi_tables_path_val(), input$roi_table_name)
            fwrite(roi_values(), filename, row.names = FALSE)
            progress_tracker$df$has_table[i] <- TRUE
            n_done <- length(which(progress_tracker$df$has_table == TRUE))
            n_total <- nrow(progress_tracker$df)
            updateProgressBar(
              session = session, id = "progress_bar",
              value = n_done, total = n_total
            )
            showNotification("ROI table sucessfully exported", type = "message")
            if (n_done == n_total) {
              showNotification("All recordings were segmented!", type = "message")
            }
          }
        }
        if (length(vec_soundscapes) > i & i >= 1) {
          updateSelectInput(
            session, "soundscape_file",
            selected = vec_soundscapes[i + 1]
          )
        }
      })

      observeEvent(input$no_soi, {
        req(
          roi_values(), wav_path_val(), user_val(), rec_soundscape(),
          soundscape_data()
        )
        # create it
        roi_i <- tibble(
          soundscape_path = wav_path_val(),
          soundscape_file = input$soundscape_file,
          roi_user = user_val(),
          roi_input_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          roi_label = "no signals of interest",
          roi_start = 0,
          roi_end = duration_val(),
          roi_min_freq = input$zoom_freq[1],
          roi_max_freq = input$zoom_freq[2],
          roi_type = NA_character_,
          roi_label_confidence = NA_character_,
          roi_is_complete = NA_character_,
          roi_comment = NA_character_,
          roi_wl = input$wl,
          roi_ovlp = input$ovlp,
          roi_sample_rate = rec_soundscape()@samp.rate,
          roi_pitch_shift = input$pitch_shift
        )
        roi_values(roi_i)
        # save it
        filename <- file.path(roi_tables_path_val(), input$roi_table_name)
        fwrite(roi_values(), filename, row.names = FALSE)
        progress_tracker$df$has_table[
          which(soundscape_data()$soundscape_file == input$soundscape_file)
        ] <- TRUE
        n_done <- length(which(progress_tracker$df$has_table == TRUE))
        n_total <- nrow(progress_tracker$df)
        updateProgressBar(
          session = session, id = "progress_bar",
          value = n_done, total = n_total
        )
        showNotification(
          "ROI table sucessfully exported (marked as dissmissed)",
          type = "message"
        )
        if (n_done == n_total) {
          showNotification("All recordings were segmented!", type = "message")
        }
        # next
        vec_soundscapes <- soundscape_data()$soundscape_file
        i <- which(vec_soundscapes == input$soundscape_file)
        if (length(vec_soundscapes) > i & i >= 1) {
          updateSelectInput(
            session, "soundscape_file",
            selected = vec_soundscapes[i + 1]
          )
        }
      })

      # todo Adicionar versao para hotkeys no F

      observeEvent(input$next_soundscape_noroi, {
        noroi_data <- progress_tracker$df %>%
          filter(
            has_table == FALSE | soundscape_file == input$soundscape_file
          )
        noroi_vec_soundscapes <- noroi_data$soundscape_file
        noroi_i <- which(noroi_vec_soundscapes == input$soundscape_file)
        if (input$nav_autosave == TRUE) {
          if (!all(is.na(roi_values())) & fnrow(roi_values()) > 0) {
            vec_soundscapes <- soundscape_data()$soundscape_file
            i <- which(vec_soundscapes == input$soundscape_file)
            filename <- file.path(roi_tables_path_val(), input$roi_table_name)
            fwrite(roi_values(), filename, row.names = FALSE)
            progress_tracker$df$has_table[i] <- TRUE
            n_done <- length(which(progress_tracker$df$has_table == TRUE))
            n_total <- nrow(progress_tracker$df)
            updateProgressBar(
              session = session, id = "progress_bar",
              value = n_done, total = n_total
            )
            showNotification("ROI table sucessfully exported", type = "message")
            if (n_done == n_total) {
              showNotification("All recordings were segmented!", type = "message")
            }
          }
        }
        if (length(noroi_vec_soundscapes) > noroi_i & noroi_i >= 1) {
          updateSelectInput(
            session, "soundscape_file",
            selected = noroi_vec_soundscapes[noroi_i + 1]
          )
        }
      })

      # todo Adicionar versao para hotkeys no T

      observeEvent(input$prev_soundscape_noroi, {
        noroi_data <- progress_tracker$df %>%
          filter(
            has_table == FALSE | soundscape_file == input$soundscape_file
          )
        noroi_vec_soundscapes <- noroi_data$soundscape_file
        noroi_i <- which(noroi_vec_soundscapes == input$soundscape_file)
        if (input$nav_autosave == TRUE) {
          if (!all(is.na(roi_values())) & fnrow(roi_values()) > 0) {
            vec_soundscapes <- soundscape_data()$soundscape_file
            i <- which(vec_soundscapes == input$soundscape_file)
            filename <- file.path(roi_tables_path_val(), input$roi_table_name)
            fwrite(roi_values(), filename, row.names = FALSE)
            progress_tracker$df$has_table[i] <- TRUE
            n_done <- length(which(progress_tracker$df$has_table == TRUE))
            n_total <- nrow(progress_tracker$df)
            updateProgressBar(
              session = session, id = "progress_bar",
              value = n_done, total = n_total
            )
            showNotification("ROI table sucessfully exported", type = "message")
            if (n_done == n_total) {
              showNotification("All recordings were segmented!", type = "message")
            }
          }
        }
        if (length(noroi_vec_soundscapes) >= noroi_i & noroi_i > 1) {
          updateSelectInput(
            session, "soundscape_file",
            selected = noroi_vec_soundscapes[noroi_i - 1]
          )
        }
      })

      # Spectrogram ----------------------------------------------------------------

      spectro_soundscape_raw <- reactiveVal(NULL)
      observe({
        req(
          rec_soundscape(), input$wl, input$ovlp, input$color_scale
        )
          res <- fast_spectro(
            rec_soundscape(),
            f = rec_soundscape()@samp.rate,
            ovlp = input$ovlp, wl = input$wl,
            dyn = input$dyn_range, pitch_shift = input$pitch_shift,
            color_scale = input$color_scale, ncolors = 124, norm = FALSE
          )
        spectro_soundscape_raw(res)
      })

      # # IN case necessary, process the spectrogram outside of the output object
      # rois_to_plot <- reactiveVal(NULL)
      # spectro_soundscape <- reactiveVal(NULL)
      # observe({
      # })

      output$spectrogram_plot <- renderPlot(execOnResize = TRUE, {
        req(
          input$zoom_freq, input$zoom_time, roi_values(),
          spectro_soundscape_raw(), rec_soundscape(), duration_val()
        )
        zoom_freq <- input$zoom_freq
        if (zoom_freq[1] >= zoom_freq[2]) {
          zoom_freq[2] <- zoom_freq[1] + 1
        }
        zoom_freq <- sort(zoom_freq)
        zoom_time <- input$zoom_time

        selection_color <- ifelse(
          input$color_scale %in% c("greyscale 1", "greyscale 2"),
          "black", "white"
        )
        # Sys.sleep(0.3)


        spectro_plot <- spectro_soundscape_raw() +
          coord_cartesian(
            xlim = zoom_time, ylim = zoom_freq
          ) +
          annotate(
            "label",
            label = paste0(
              input$soundscape_file, " (sr = ", rec_soundscape()@samp.rate,
              "; wl = ", input$wl, "; ovlp = ", input$ovlp,
              "; pitch_shift = ", input$pitch_shift, ")"
            ),
            x = -Inf, y = Inf, hjust = 0, vjust = 1,
            color = "white", fill = "black"
          ) +
          labs(x = "Time (s)", y = "Frequency (kHz)") +
          theme(legend.position = "none")


        rois_to_plot <- roi_values() |>
          mutate(id = row_number()) |>
          fsubset(
            roi_start < zoom_time[2] & roi_end > zoom_time[1]
          )


        if (nrow(rois_to_plot) > 0) {
          if (unique(rois_to_plot$soundscape_file) == input$soundscape_file) {
            spectro_plot <- spectro_plot +
              {
                if (input$show_label == TRUE) {
                  annotate(
                    "text",
                    alpha = 1, vjust = "inward", hjust = "inward",
                    angle = input$label_angle, color = selection_color,
                    x = rois_to_plot$roi_start, y = rois_to_plot$roi_max_freq,
                    label = paste0("(", rois_to_plot$id, ") ", rois_to_plot$roi_label),
                    na.rm = TRUE
                  )
                }
              } +
              {
                if (!is.null(input$res_table_rows_selected)) {
                  annotate(
                    "rect",
                    alpha = 0.2, linewidth = 0.5, linetype = "solid",
                    fill = selection_color, color = selection_color,
                    xmin = rois_to_plot$roi_start[input$res_table_rows_selected],
                    xmax = rois_to_plot$roi_end[input$res_table_rows_selected],
                    ymin = rois_to_plot$roi_min_freq[input$res_table_rows_selected],
                    ymax = rois_to_plot$roi_max_freq[input$res_table_rows_selected]
                  )
                }
              } +
              annotate(
                "rect",
                alpha = 0.05, linewidth = 0.3, linetype = "dashed",
                fill = selection_color, color = selection_color,
                xmin = rois_to_plot$roi_start, xmax = rois_to_plot$roi_end,
                ymin = rois_to_plot$roi_min_freq, ymax = rois_to_plot$roi_max_freq
              )
          }
        }

        if (!is.null(ruler())) {
          snd_to_measure <- cutw(
            rec_soundscape(),
            f = rec_soundscape()@samp.rate,
            from = ruler()$start, to = ruler()$end, units = "seconds",
            output = "Wave"
          )

          dom_freq <- mean(
            dfreq(
              snd_to_measure,
              f = snd_to_measure@samp.rate, wl = input$wl, ovlp = input$ovlp,
              bandpass = c(ruler()$min_freq, ruler()$max_freq) * 1000,
              plot = FALSE
            )[, 2]
          )

          ac_stats <- acoustat(
            snd_to_measure,
            f = snd_to_measure@samp.rate, wl = input$wl, ovlp = input$ovlp,
            fraction = 80, plot = FALSE,
            # tlim = c(ruler()$start, ruler()$end),
            flim = c(ruler()$min_freq, ruler()$max_freq)
          ) %>%
            .[-c(1, 2)] %>%
            as.data.frame() %>%
            transmute(
              t10 = time.P1,
              t90 = time.P2,
              f10 = freq.P1,
              f90 = freq.P2
            )

          spectro_plot <- spectro_plot +
            annotate(
              "rect",
              alpha = 0.2, linewidth = 0.5, linetype = "solid",
              color = "yellow",
              xmin = ruler()$start, xmax = ruler()$end,
              ymin = ruler()$min_freq, ymax = ruler()$max_freq
            ) +
            annotate(
              "label",
              alpha = 1, color = "black",
              hjust = c(
                "right", "right", "right", "right", "right", "left"
              ),
              vjust = c(
                "top", "bottom", "bottom", "top", "top", "center"
              ),
              size = rep(5, 6),
              angle = c(
                90, 0, 0, 90, 0, 0
              ),
              x = c(
                ruler()$end,
                ruler()$start, ruler()$start,
                ruler()$end - (ruler()$duration / 2),
                ruler()$start, ruler()$end
              ),
              y = c(
                ruler()$min_freq,
                ruler()$min_freq, ruler()$max_freq,
                ruler()$min_freq,
                ruler()$max_freq - (ruler()$bandwidth / 2),
                dom_freq
              ),
              label = c(
                paste0("t=", round(ruler()$end, 3)),
                paste0(
                  "f0=", round(ruler()$min_freq, 3), "\n",
                  "t0=", round(ruler()$start, 3)
                ),
                paste0("f=", round(ruler()$max_freq, 3)),
                paste0("d=", round(ruler()$duration, 3)),
                paste0("bw=", round(ruler()$bandwidth, 3)),
                paste0("fdom=", round(dom_freq, 3))
              )
            ) +
            annotate(
              "segment",
              color = "yellow", size = 0.5,
              linetype = "solid",
              x = ruler()$start,
              xend = ruler()$end,
              y = dom_freq
            ) +
            annotate(
              "segment",
              color = "yellow", size = 0.5, linetype = rep("dotted", 4),
              x = c(ruler()$start, ruler()$start, ruler()$start + ac_stats$t10, ruler()$start + ac_stats$t90),
              xend = c(ruler()$end, ruler()$end, ruler()$start + ac_stats$t10, ruler()$start + ac_stats$t90),
              y = c(ac_stats$f10, ac_stats$f90, ruler()$min_freq, ruler()$min_freq),
              yend = c(ac_stats$f10, ac_stats$f90, ruler()$max_freq, ruler()$max_freq)
            )
        }


        spectro_plot
      })

      output$res_table <- renderDT(
        {
          req(roi_values())
          datatable(
            roi_values(),
            editable = TRUE,
            colnames = c(
              "soundscape_path", "soundscape_file", "roi_user", "roi_input_timestamp",
              "roi_label", "roi_start", "roi_end", "roi_min_freq", "roi_max_freq",
              "roi_type", "roi_label_confidence", "roi_is_complete", "roi_comment",
              "roi_wl", "roi_ovlp", "roi_sample_rate", "roi_pitch_shift"
            ),
            options = list(
              pageLength = 50, info = FALSE, dom = "tpl",
              columnDefs = list(
                list(
                  visible = FALSE,
                  targets = c(
                    "soundscape_path", "soundscape_file", "roi_user",
                    "roi_input_timestamp", "roi_sample_rate"
                  )
                )
              )
            )
          ) %>%
            formatRound(c("roi_start", "roi_end"), 3) %>%
            formatRound(c("roi_min_freq", "roi_max_freq"), 1)
        },
        server = TRUE,
        options = list(lengthChange = FALSE)
      )

      observeEvent(input$res_table_cell_edit, {
        req(roi_values(), roi_tables_path_val(), input$roi_table_name)
        df <- roi_values()
        df[input$res_table_cell_edit$row, input$res_table_cell_edit$col] <-
          input$res_table_cell_edit$value
        roi_values(df)
        if (input$nav_autosave == TRUE) {
          fwrite(
            roi_values(), file.path(roi_tables_path_val(), input$roi_table_name),
            row.names = FALSE
          )
        }
      })

      observeEvent(input$delete_selected_rois, {
        req(roi_values())
        df <- roi_values()
        if (!is.null(input$res_table_rows_selected)) {
          df <- df[-input$res_table_rows_selected, ]
          showNotification("Selected ROIs deleted!", type = "message")
        }
        roi_values(df)
      })

      observeEvent(input$export_selected_cut, {
        req(roi_values(), rec_soundscape(), cuts_path_val())
        if (!all(is.na(roi_values())) & fnrow(roi_values()) > 0) {
          df <- roi_values()
          if (!is.null(input$res_table_rows_selected)) {
            df <- df[input$res_table_rows_selected, ]
          }
          roi_razor(wav = rec_soundscape(), rois = df, path = cuts_path_val())
          showNotification("Cuts sucessfully exported!", type = "message")
        }
      })

      # todo Adicionar aqui o server side para exportar os sonogramas dos cortes

      shinyDirChoose(input, "preset_path_load", roots = volumes)
      preset_path_load <- reactive(input$preset_path_load)
      observeEvent(input$preset_path_load, {
        updateTextInput(
          session,
          inputId = "preset_path",
          value = str_replace(
            parseDirPath(volumes, preset_path_load()), "//", "/"
          )
        )
      })

      # observeEvent(input$preset_path, {
      #   res_list <- list.files(
      #     path = input$preset_path, pattern = "^segmentation_preset_.*\\.rds$"
      #   ) %>%
      #     str_remove("segmentation_preset_") %>%
      #     str_remove(".rds")
      #   if (!is.null(res_list)) {
      #     updateSelectInput(
      #       session, "available_presets",
      #       choices = c("Export new preset file...", res_list)
      #     )
      #   }
      # })

      # todo update here
      observeEvent(input$default_pars, {
        req(rec_soundscape())
        updateSliderInput(session, inputId = "dyn_range", value = session_data$dyn_range)
        updateSliderTextInput(session, inputId = "wl", selected = session_data$wl)
        updateSliderInput(session, inputId = "ovlp", value = session_data$ovlp)
        updateSelectInput(session, inputId = "color_scale", selected = session_data$color_scale)
        updateSliderInput(session, inputId = "label_angle", value = session_data$label_angle)
        updateCheckboxInput(session, inputId = "show_label", value = session_data$show_label)
        updateRadioButtons(session, inputId = "wav_player_type", selected = session_data$wav_player_type)
        updateNoUiSliderInput(session, inputId = "zoom_freq", value = session_data$zoom_freq)
        updateCheckboxInput(session, inputId = "nav_autosave", value = session_data$nav_autosave)
        updateSliderTextInput(session, inputId = "pitch_shift", selected = session_data$pitch_shift)
      })

      session_settings <- reactiveVal(NULL)
      observe({
        res <- list(
          user = input$user,
          soundscapes_path = input$soundscapes_path,
          roi_tables_path = input$roi_tables_path,
          cuts_path = input$cuts_path, # temporario
          # fastdisp = input$fastdisp,
          label_angle = input$label_angle,
          show_label = input$show_label,
          dyn_range = input$dyn_range,
          wl = input$wl, # adicionar salvamento disso na tabela de rois
          ovlp = input$ovlp, # adicionar na tabela de rois
          color_scale = input$color_scale,
          wav_player_type = input$wav_player_type,
          wav_player_path = input$wav_player_path,
          session_notes = input$session_notes,
          zoom_freq = input$zoom_freq,
          nav_autosave = input$nav_autosave,
          sp_list = input$sp_list,
          pitch_shift = input$pitch_shift
        )
        session_settings(res)
      })

      # # Export current session settings as a rds file
      # observeEvent(input$export_preset, {
      #   req(
      #     session_settings(), input$preset_path, input$available_presets
      #   )
      #   if (
      #     all(c(
      #       nchar(input$user) != 0,
      #       dir.exists(c(input$preset_path, soundscape_path_val(), roi_tables_path_val()))
      #     ))
      #   ) {
      #     preset_file <- file.path(
      #       input$preset_path,
      #       paste0("segmentation_preset_", input$available_presets, ".rds")
      #     )
      #     if (file.exists(preset_file)) {
      #       saved_preset <- readRDS(preset_file)
      #       if (identical(saved_preset, session_settings())) {
      #         showModal(
      #           modalDialog(
      #             title = "Nothing to be done",
      #             h3("No changes were made in the current preset"),
      #             easyClose = TRUE,
      #             footer = NULL
      #           )
      #         )
      #       } else {
      #         what_changed <- rbind(saved_preset, session_settings()) %>%
      #           as.data.frame() %>%
      #           setNames(
      #             list(
      #               "user name", "path to soundscapes", "path to ROI tables",
      #               "path to audio cuts and spectrograms",
      #               # "fast display spectrogram",
      #               "roi label angle", "show roi rabels", "spectrogram dynamic range",
      #               "spectrogram window length", "spectrogram overlap",
      #               "spectrogram color scale", "wave player type", "wave player path",
      #               "session notes", "visible frequency band", "autosave while navigating",
      #               "available labels for ROIs (species lists)", "pitch shift for ultrasound recordings"
      #             )
      #           ) %>%
      #           select_if(function(col) length(unique(col)) > 1) %>%
      #           colnames() %>%
      #           paste(collapse = "; ")

      #         # showModal(
      #         #   modalDialog(
      #         #     title = "Changes detected in preset",
      #         #     paste0(
      #         #       "There are differences between settings in the current session and in the preset file:", what_changed,
      #         #       "Exporting will overwrite the existing preset file. Provide a new name below if if you wish to create a new preset instead:"
      #         #     )
      #         #     footer = tagList(
      #         #       textInput(
      #         #         "new_preset_name",
      #         #         label = NULL,
      #         #         value = input$available_presets, placeholder = TRUE
      #         #       ),
      #         #       actionButton("confirm_export_preset", label = "Confirm & Export"),
      #         #       modalButton("Cancel")
      #         #     )
      #         #   )
      #         # )

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
      #         str_remove(user_val(), ","), "_",
      #         format(Sys.time(), "%Y-%m-%d_%H:%M:%S")
      #       )
      #       shinyalert(
      #         title = "Creating a new preset file",
      #         text = tagList(
      #           h3("Provide a name in the box below:"),
      #           textInput(
      #             "new_preset_name",
      #             label = NULL,
      #             value = new_name, placeholder = TRUE
      #           ),
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
      #       input$preset_path, paste0("segmentation_preset_", input$new_preset_name, ".rds")
      #     )
      #   )
      #   res_list <- list.files(
      #     path = input$preset_path, pattern = "^segmentation_preset_.*\\.rds$"
      #   )
      #   res_list <- gsub("segmentation_preset_", "", res_list)
      #   res_list <- gsub(".rds$", "", res_list)

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
      #     paste0("segmentation_preset_", input$available_presets, ".rds")
      #   )

      #   if (file.exists(preset_file)) {
      #     res <- readRDS(preset_file)
      #     session_settings(res)
      #     user_val(res$user)
      #     updateTextInput(session, inputId = "user", value = res$user)
      #     soundscape_path_val(res$soundscapes_path)
      #     updateTextAreaInput(session, inputId = "soundscapes_path", value = res$soundscapes_path)
      #     roi_tables_path_val(res$roi_tables_path)
      #     updateTextAreaInput(session, inputId = "roi_tables_path", value = res$roi_tables_path)
      #     cuts_path_val(res$cuts_path)
      #     updateTextAreaInput(session, inputId = "cuts_path", value = cuts_path_val())
      #     # updateCheckboxInput(session, inputId = "fastdisp", value = res$fastdisp)
      #     updateSliderInput(session, inputId = "label_angle", value = res$label_angle)
      #     updateCheckboxInput(session, inputId = "show_label", value = res$show_label)
      #     updateSliderInput(session, inputId = "dyn_range", value = res$dyn_range)
      #     updateSliderTextInput(session, inputId = "wl", selected = res$wl)
      #     updateSliderInput(session, inputId = "ovlp", value = res$ovlp)
      #     updateSelectInput(session, inputId = "color_scale", selected = res$color_scale)
      #     updateTextAreaInput(session, inputId = "wav_player_path", value = res$wav_player_path)
      #     updateRadioButtons(session, inputId = "wav_player_type", selected = res$wav_player_type)
      #     updateTextAreaInput(session, inputId = "session_notes", value = res$session_notes)
      #     updateNoUiSliderInput(session, inputId = "zoom_freq", value = res$zoom_freq)
      #     updateCheckboxInput(session, inputId = "nav_autosave", value = res$nav_autosave)
      #     updateSelectizeInput(session, inputId = "sp_list", selected = res$sp_list)
      #     updateSliderInput(session, inputId = "pitch_shift", value = res$pitch_shift)
      #     showNotification("Preset file successfully imported")
      #   }
      # })

      # Trigger checks for ending the session
      observeEvent(input$end_session, {
        req(
          roi_tables_path_val(), roi_values(), session_settings(), input$roi_table_name
        )
        table_name <- file.path(roi_tables_path_val(), input$roi_table_name)

        if (file.exists(table_name)) {
          roi_tables <- list.files(
            roi_tables_path_val(),
            pattern = ".csv", full.names = TRUE, ignore.case = TRUE
          ) %>%
            data.frame(
              roi_table_name = basename(.), roi_table_path = .
            )

          active_roi_table_path <- roi_tables %>%
            fsubset(roi_table_name == input$roi_table_name) %>%
            pull(roi_table_path)

          if (file.exists(active_roi_table_path)) {
            saved_rois <- fread(file = active_roi_table_path) %>%
              as.data.frame() %>%
              mutate(roi_input_timestamp = format(roi_input_timestamp, "%Y-%m-%d %H:%M:%S"))

            nrow_unsaved <- nrow(
              anti_join(
                roi_values(), saved_rois,
                by = c("soundscape_file", "roi_user", "roi_label", "roi_input_timestamp")
              )
            )

            if (nrow_unsaved > 0) {
              message_rois <- paste0(
                "There are ", nrow_unsaved, " unsaved ROIs in the current soundscape. Consider saving before leaving the session. Press ESC to get back to the session or END it in the button below."
              )
            } else {
              message_rois <- paste0("There are no unsaved ROIs in the current soundscape. Press ESC to get back to the session or END it in the button below.")
            }
          }
        } else {
          message_rois <- paste0("ROIs in the current soundscape are not stored in a file. Consider exporting a new one before leaving the session. Press ESC to get back to the session or END it in the button below.")
        }

        # # Check if there are settings to be saved on the preset file
        # preset_file <- file.path(
        #   input$preset_path,
        #   paste0("segmentation_preset_", input$available_presets, ".rds")
        # )
        # if (file.exists(preset_file)) {
        #   saved_preset <- readRDS(preset_file)
        #   message_settings <- "teste1"
        #   what_changed <- rbind(saved_preset, session_settings()) %>%
        #     as.data.frame() %>%
        #     setNames(
        #       list(
        #         "user name", "path to soundscapes", "path to ROI tables",
        #         "path to audio cuts and spectrograms", "fast display spectrogram",
        #         "roi label angle", "show roi rabels", "spectrogram dynamic range",
        #         "spectrogram window length", "spectrogram overlap",
        #         "spectrogram color scale", "wave player type", "wave player path",
        #         "session notes", "visible frequency band", "autosave while navigating",
        #         "available labels for ROIs (species lists)", "pitch shift for ultrasound recordings"
        #       )
        #     ) %>%
        #     select_if(function(col) length(unique(col)) > 1) %>%
        #     colnames() %>%
        #     paste(collapse = "; ")
        #   if (!identical(saved_preset, session_settings())) {
        #     message_settings <- paste0(
        #       "The following settings were updated: ", what_changed,
        #       ". Consider update the preset or create a new one before leaving the session."
        #     )
        #   } else {
        #     message_settings <- paste0("No setting changes detected.")
        #   }
        # } else {
        #   message_settings <- paste0("No preset file was found. Consider creating a new one before leaving the session.")
        # }

        showModal(
          modalDialog(
            title = "Check out",
            message_rois,
            # message_settings,
            footer = tagList(
              # actionButton("cancel_exit", "Cancel"),
              actionButton("confirm_exit", "End session")
            ),
            easyClose = TRUE
          )
        )
      })

      observeEvent(input$confirm_exit, {
        # Clear temp folder before stopping the app
        unlink(paste0(getwd(), "/temp/"), recursive = TRUE, force = TRUE)
        stopApp()
      })

      # teste_val <- reactiveVal(NULL)
      # output$checagem1 <- renderPrint({
      #   req(duration_val())
      #   duration_val()
      # })

      # General popover options
      pop_up_opt <- list(delay = list(show = 1000, hide = 0))

      # Side bar menu - User setup
      # addTooltip(session,
      #   id = "available_presets",
      #   title = "Select a preset or type a new name to create a new one.",
      #   placement = "right", trigger = "hover", options = pop_up_opt
      # )
      # addTooltip(session,
      #   id = "import_preset",
      #   title = "Apply stored settings to the current session",
      #   placement = "right", trigger = "hover", options = pop_up_opt
      # )
      # addTooltip(session,
      #   id = "export_preset",
      #   title = "Store the current session settings to a preset file. Existing files will be overwritten",
      #   placement = "right", trigger = "hover", options = pop_up_opt
      # )
      addTooltip(session,
        id = "user",
        title = "Identify yourself in the recommended format: 'Rosa G. L. M. (avoid commas)",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "soundscapes_path",
        title = "Parent location that contains only template files or folders of these",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "roi_tables_path",
        title = "Path to the location where ROI tables will be exported to",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "cuts_path",
        title = "Path to the location where audio cuts and spectrograms will be exported to",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "user_setup_confirm",
        title = "Setup confirmation required to start the segmentation",
        placement = "right", trigger = "hover", options = pop_up_opt
      )

      #  # Side bar menu - Session setup
      # addTooltip(
      #   session,
      #   id = "fastdisp",
      #   title = "Requires que 'fftw' package",
      #   placement = "right", trigger = "hover", options = pop_up_opt
      # )
      addTooltip(session,
        id = "label_angle",
        title = "Adjust the angle of labels in the spectrogram. Recommended 90º for a less cluttering.",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "wl",
        title = "Tradeoff between time and frequency resolution",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "ovlp",
        title = "Increase if more resultion is needed. Performance may decrease for values above 80%",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "pitch_shift",
        title = "Adjust the pitch of ultrasound recordings to improve visualization",
        placement = "right", trigger = "hover", options = pop_up_opt
      )

      addTooltip(session,
        id = "dyn_range",
        title = "Adjust what portion of the amplitude scale is shown in the spectrograms",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "show_label",
        title = "Show the label alongside the ROI. Hide in case of ROI cluttering",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "color_scale",
        title = "Available palettes for representing spectrogram colors",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "wav_player_type",
        title = "Select the method to play wav files",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "wav_player_path",
        title = "Necessary when 'External plyer' is selected. If the executable is not available, 'HTML player' will be automatically selected",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "default_pars",
        title = "Set spectrogram parameters back to the default",
        placement = "right", trigger = "hover", options = pop_up_opt
      )

      # Body - Spectrogram parameters

      addTooltip(session,
        id = "zoom_freq",
        title = "Zoom in, zoom out and slide to navigate in the fdrequency axis (affects exported spectrograms)",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "zoom_time",
        title = "Zoom in, zoom out and slide to navigate in the time axis",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )

      # Box - Soundscape spectrogram

      addTooltip(session,
        id = "prev_soundscape_noroi",
        title = "Navigate to the previous soundscape",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "prev_soundscape",
        title = "Navigate to the previous soundscape without ROIs",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "play_soundscape",
        title = "Play visible portion of the soundscape",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "next_soundscape",
        title = "Navigate to the next soundscape",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "next_soundscape_noroi",
        title = "Navigate to the next soundscape without ROIs",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )

      # Box - Setup and Input

      addTooltip(session,
        id = "soundscape_file",
        title = "Select one of the available soundscapes",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "roi_table_name",
        title = "Alternative ROI tables available for the selected soundscape",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "sp_list",
        title = "Select available lists of labels for autocompletion",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "label_name",
        title = "Label the content of the next roi",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "signal_type",
        title = "Identify the signal type",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "label_certainty",
        title = "Inform the certainty level for the label",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "signal_is_complete",
        title = "Inform if the entire target signal is clearly within ROI limits",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "label_comment",
        title = 'Provide additional information (avoid "quotation marks" and separate different fields of content with "_" or ";") ',
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )

      # Box - ROI table

      addTooltip(session,
        id = "save_roi",
        title = "Export the changes made to the currently active ROI table",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "export_new_roi_table",
        title = "Export new ROI table",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "export_selected_cut",
        title = "Export audio cuts of the ROIs selected in the table below",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      addTooltip(session,
        id = "delete_selected_rois",
        title = "Delete the ROIs selected in the table below",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
    }
  )
}
