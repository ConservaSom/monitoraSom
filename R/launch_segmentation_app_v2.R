#' Function to launch the segmentation app
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function launches the segmentation app, which is a Shiny app for
#'   segemntation of WAV recorcings of soundscapes into tables containing
#'   regions of interest (ROIs) and audio cuts of the ROIs. The app settings can
#'   be imported from presets or set manually.
#'
#' @param project_path Todo
#' @param preset_path Path from which presets can be imported and to which new
#'   presets can be exported.
#' @param preset_id ID of the preset to be imported from the 'preset_path'
#'   folder. If export_preset = TRUE, the preset will be exported to the
#'   'preset_path' folder with the name "segmentation_preset_<preset_id>.rds".
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
#' @param fastdisp If TRUE, the spectrogram will be displayed in a lower
#'   quality, but faster.
#' @param label_angle Angle between 0 and 180 to draw the ROI labels in the
#'   spectrogram plot.
#' @param show_label If TRUE, ROI labels will be displayed alongside ROI
#'   selections in the sepctrogram.
#' @param wl An integer specifying the length of the FFT window used to
#'   calculate the spectrogram.
#' @param ovlp A numeric value between 0 and 90 specifying the percentage
#'   overlap of windows for computing the spectrogram.
#' @param dyn_range A numeric vector of length 2 between -100 and 0, specifying
#'   the minimum and maximum relative amplitudes to be displayed in the
#'   spectrogram
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
#'
#' @return Todo
#'
#' @export
launch_segmentation_app_v2 <- function(
  project_path = NULL, preset_id = NULL, user = NULL, preset_path = NULL,
  soundscapes_path = NULL, roi_tables_path = NULL, cuts_path = NULL,
  labels_file = NULL, sp_list = "CBRO-2021 (Brazil)", fastdisp = TRUE, label_angle = 90,
  show_label = TRUE, dyn_range = c(-60, 0), wl = 1024, ovlp = 0,
  color_scale = "inferno", wav_player_type = "R session", wav_player_path = "play",
  session_notes = NULL, zoom_freq = c(0, 10), nav_autosave = FALSE
  ) {
  require(shiny)
  require(dplyr)
  require(here)
  require(ggplot2)
  require(lubridate)
  require(seewave)
  require(stringr)
  require(tuneR)
  require(collapse)
  require(knitr)
  require(rmarkdown)
  require(beepr)
  require(DT)
  require(data.table)
  require(shinyWidgets)
  require(shinydashboard)
  require(shinyFiles)
  require(shinyalert)
  require(shiny)
  require(keys)
  require(shinyjs)
  require(shinyBS)

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
  } else if (is.null(preset_id) | is.null(preset_path)) {
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
    cuts_path <- file.path(project_path, "cuts/")
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

  if (is.logical(fastdisp)) {
    session_data$fastdisp <- fastdisp
  } else {
    stop("Error! The value assigned to 'fastdisp' is not logical. Set it to TRUE or FALSE.")
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
        if (dyn_range[1] >= -100 & dyn_range[2] <= 10) {
          if (dyn_range[1] %% 10 == 0 & dyn_range[2] %% 10 == 0) {
            session_data$dyn_range <- dyn_range
          } else {
            stop("Error! 'dyn_range' must be a multiple of 10.")
          }
        } else {
          stop("Error! 'dyn_range' must be between -100 and 10.")
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
    if (wl %in% c(128, 256, 512, 1024, 2048, 4096)) {
      session_data$wl <- wl
    } else {
      stop(
        "Error! The value assigned to 'wl' is not among the expected alternatives: 128, 256, 512, 1024, 2048, or 4096."
      )
    }
  } else {
    stop(
      "Error! The value assigned to 'wl' is not among the expected alternatives: 128, 256, 512, 1024, 2048, or 4096."
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

  if (is.null(session_notes)) {
    session_data$session_notes <- NA
  } else {
    session_notes <- as.character(session_notes)
    if (is.character(session_notes)) {
      session_data$session_notes <- session_notes
    } else {
      stop(
        "Error! The value assigned to 'session_notes' cannot be coerced to a character string."
      )
    }
  }

  # check if the variable 'zoom_freq' is an integer vector of length equals 2, is between 0 and 10 and the first value is smaller than the second
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
    preset_path <- file.path(project_path, "presets/")
    temp_path <- file.path(project_path, "presets/temp/")
    if (!dir.exists(preset_path)) {
      dir.create(preset_path)
      dir.create(temp_path)
    }
    session_data$preset_path <- preset_path
    session_data$temp_path <- temp_path
  }

  data(sp_labels)
  sp_labels_default <- sp_labels
  if (!is.null(labels_file)) {
    stopifnot(file.exists(labels_file))
    sp_labels_custom <- readxl::read_xlsx(labels_file)
  } else if (!is.null(project_path)) {
    sp_labels_custom <- file.path(project_path, "presets/sp_labels.xlsx")
    if (file.exists(sp_labels_custom)) {
      sp_labels_custom <- readxl::read_xlsx(sp_labels_custom)
    } else {
      openxlsx::write.xlsx(sp_labels_default, sp_labels_custom)
      sp_labels_custom <- sp_labels_default
    }
  }
  sp_labels <- dplyr::coalesce(sp_labels_custom, sp_labels_default)


  if (!sp_list %in% colnames(sp_labels)) {
    warning("The selected species list is not among the available species lists. Using the default species list.")
  }
  session_data$sp_list <- sp_list

  if (!is.null(preset_id)) {
    if (is.character(preset_id) & length(preset_id) == 1) {
      session_data$preset_id <- preset_id
      if (!is.null(preset_path)) {
        preset_file <- file.path(
          preset_path, paste0("segmentation_preset_", preset_id, ".rds")
        )
        preset_to_export <- list(
          user = session_data$user,
          soundscapes_path = session_data$soundscapes_path,
          roi_tables_path = session_data$roi_tables_path,
          cuts_path = session_data$cuts_path,
          fastdisp = session_data$fastdisp,
          label_angle = session_data$label_angle,
          show_label = session_data$show_label,
          dyn_range = session_data$dyn_range,
          wl = session_data$wl,
          ovlp = session_data$ovlp,
          color_scale = session_data$color_scale,
          wav_player_type = session_data$wav_player_type,
          wav_player_path = session_data$wav_player_path,
          session_notes = session_data$session_notes,
          zoom_freq = session_data$zoom_freq,
          nav_autosave = session_data$nav_autosave,
          sp_list = session_data$sp_list
        )
        saveRDS(object = preset_to_export, file = preset_file)
        message("Preset sucessfully exported to the selected destination!")
      }
    } else {
      stop(
        "Error! The value assigned to 'preset_id' is not a character string of length 1."
      )
    }
  }

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
                stringr::str_pad(sprintf("%.3f", round(roi_start, 3)), 7, pad = "0"), "-",
                stringr::str_pad(sprintf("%.3f", round(roi_end, 3)), 7, pad = "0"), "s_",
                stringr::str_pad(sprintf("%.3f", round(min_freq, 3)), 6, pad = "0"), "-",
                stringr::str_pad(sprintf("%.3f", round(max_freq, 3)), 6, pad = "0"), "kHz_",
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
        ~ cutw(
          wav,
          f = wav@samp.rate, output = "Wave",
          from = .x$roi_start, to = .x$roi_end
        ) %>%
          savewav(filename = file.path(path, .x$cut_name))
      )
    }
  }

  # Credits to Sergio Oller in <https://github.com/tidyverse/ggplot2/issues/4989>
  mat_to_nativeRaster <- function(x, colormap, rangex) {
    cols_to_ints <- farver::encode_native(colormap)
    breaks <- seq(from = rangex[1], to = rangex[2], length.out = length(colormap))
    xdim <- dim(x)
    rev_cols <- seq.int(ncol(x), 1L, by = -1L)
    x <- x[, rev_cols]
    x <- findInterval(x, breaks, rightmost.closed = TRUE)
    x <- cols_to_ints[x]
    x <- matrix(x, nrow = xdim[1], ncol = xdim[2], byrow = FALSE)
    structure(
      x,
      dim = c(xdim[2], xdim[1]), class = "nativeRaster", channels = 4L
    )
  }

  # This function defines where embedded html wav players will look for the files
  # shiny::addResourcePath("audio", here("temp/"))
  shiny::addResourcePath("audio", temp_path)
  # resourcePaths()

  # ! Atençao para o funcionamento dessa funçao quando o pacote estiver montado
  # todo Adicionar aqui a definiçao de settings introduzidos ao ambiente por meio da funçao que chama o app ou de um preset com caminho especificado

  # adicionar botao para deposito direto de paths nos caminhos
  hotkeys <- c(
    "q", # delete active ROI
    "w", # zoom in time
    "alt+w", # todo zoom out to full soundscape duration and frequency band
    "e", # store corrent selection as a ROI
    "a", # navigate backwards in in time within a sounscape
    "alt+a", # todo navigate to the previous soundscape
    "s", # zoom out in time
    "alt+s", # todo zoom out to full soundscape duration
    "d", # navigate forward in in time within a sounscape
    "alt+d", # todo navigate to the next soudnscape
    "1", # play audio of visible soundscape spectrogram
    "2" # todo play concatenated audio of all selected rois (default to all if none selected)
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
            shinyBS::bsTooltip("preset_path",
              title = "Paste the path to segmentation presets here. Available presets will be shown in the drop-down menu below",
              placement = "right", trigger = "hover",
              options = list(delay = list(show = 1000, hide = 0))
            ),
            tags$style(".tooltip {width: 300px;}"),
            selectizeInput("available_presets", "Available presets",
              choices = "Export new preset file...", width = "100%",
            ),
            fluidRow(
              column(
                width = 5, offset = "0px",
                actionButton(
                  "export_preset", "Export preset",
                  icon = icon(lib = "glyphicon", "glyphicon glyphicon-export"),
                  width = "170px"
                )
              ),
              column(
                width = 6, offset = "0px",
                actionButton(
                  "import_preset", "Import preset",
                  icon = icon(lib = "glyphicon", "glyphicon glyphicon-import"),
                  width = "170px"
                )
              ),
              tags$style(type = "text/css", "#export_preset { margin-top: 0px;}"),
              tags$style(type = "text/css", "#import_preset { margin-top: 0px;}")
            ),
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

          # menuItem(
          #   "Session Setup",
          #   icon = icon(lib = "glyphicon", "glyphicon glyphicon-check"),
          #   tabName = "sect_setup_tab",

          # ),
          menuItem(
            "Spectrogram Parameters",
            tabName = "spec_par_tab",
            icon = icon(lib = "glyphicon", "glyphicon glyphicon-cog"),
            checkboxInput(
              "fastdisp", "Faster spectrogram (lower quality)",
              value = session_data$fastdisp, width = "400px"
            ),
            splitLayout(
              cellWidths = c("75%", "25%"),
              sliderInput("label_angle", "Adjust label angle (º)",
                min = 0, max = 180, step = 10, value = session_data$label_angle
              ),
              checkboxInput("show_label", "Show label", value = session_data$show_label)
            ),
            sliderInput(
              "dyn_range", "Dynamic range (dB)",
              min = -100, max = 10, step = 10, value = session_data$dyn_range, width = "100%"
            ),
            sliderTextInput(
              "wl", "Window length",
              choices = c(128, 256, 512, 1024, 2048, 4096),
              selected = session_data$wl, grid = TRUE, width = "100%"
            ),
            sliderInput(
              "ovlp", "Overlap (%)",
              min = 0, max = 80, value = session_data$ovlp, step = 10, width = "100%"
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
              choices = c("HTML player", "R session", "External player"),
              selected = session_data$wav_player_type, inline = TRUE
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
          ),
          menuItem(
            "Session Notes",
            icon = icon(lib = "glyphicon", "glyphicon glyphicon-pencil"),
            tabName = "tab_notes",
            textAreaInput("session_notes", "Session notes",
              value = session_data$session_notes, placeholder = "Write session notes here", width = "100%",
              height = "150px", resize = "vertical"
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
          width = 12, height = "550px",
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
          splitLayout(
            cellWidths = c("7%", "93%"),
            noUiSliderInput(
              "zoom_freq", "kHz",
              min = 0, max = 18, step = 1, value = session_data$zoom_freq,
              direction = "rtl", orientation = "vertical", width = "100px",
              height = "350px", behaviour = "drag", format = wNumbFormat(decimals = 0),
              update_on = "end"
            ),
            plotOutput(
              "spectrogram_plot",
              brush = "roi_limits", height = "400px", width = "auto"
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
                "prev_soundscape_noroi", "Prev.Unsegmented",
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
              width = 3,
              actionButton(
                "play_soundscape", "Play visible",
                width = "100%", icon = icon("play"),
                style = "height:54px;"
              ),
              tags$div(id = "visible_soundscape_clip") # set the location of the html player
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
                "next_soundscape_noroi", "Next Unsegmented",
                width = "100%",
                icon = icon(lib = "glyphicon", "glyphicon glyphicon-fast-forward"),
                style = "height:54px; width:100%"
              )
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
                width = 5,
                selectizeInput(
                  "soundscape_file", "Soundscape (0 of 0)",
                  choices = NULL, width = "100%"
                )
              ),
              column(
                width = 7,
                selectizeInput(
                  "roi_table_name", "Active ROI table",
                  choices = NULL, width = "100%"
                )
              )
            ),
            fluidRow(
              column(
                width = 3,
                selectizeInput(
                  "sp_list", "Available species names",
                  choices = session_data$sp_list, width = "100%"
                )
              ),
              column(
                width = 3,
                selectizeInput(
                  "label_name", "Label",
                  choices = NULL, selected = NULL, width = "100%"
                )
              ),
              column(
                width = 2,
                selectizeInput(
                  "signal_type", "Type",
                  choices = c(
                    "call", "song", "mechanical", "multiple calls", "multiple songs",
                    "call duet", "song duet", "choir", "anthropophony", "geophony",
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
                  options = list(
                    placeholder = "ex.: certain",
                    onInitialize = I('function() { this.setValue(""); }'),
                    "create" = TRUE, "persist" = FALSE
                  )
                )
              ),
              column(
                width = 2,
                selectizeInput(
                  "signal_is_complete", "Complete",
                  width = "100%",
                  choices = c("complete", "incomplete"),
                  options = list(
                    placeholder = "ex.: complete",
                    onInitialize = I('function() { this.setValue(""); }'),
                    "create" = TRUE, "persist" = FALSE
                  )
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                textInput(
                  "label_comment", "Additional comments",
                  value = NULL, placeholder = "Type comments here",
                  width = "100%"
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
          tabPanel(
            "Progress Overview",
            progressBar(
              id = "progress_bar", value = 0, total = 1,
              status = "info", display_pct = TRUE, striped = TRUE
            )
          ),
          tabPanel(
            "User Manual",
            p("Q - delete active ROI"),
            p("W - zoom in time"),
            # p("Alt+W - (inactive) zoom out to full soundscape duration and frequency band"),
            p("E - store corrent selection as a ROI"),
            p("A - navigate backwards in in time within a sounscape"),
            # p("Alt+A - (inactive) navigate to the previous soundscape"),
            p("S - zoom out in time"),
            # p("Alt+S - (inactive) zoom out to full soundscape duration"),
            p("D - navigate forward in in time within a sounscape"),
            # p("Alt+D - (inactive) navigate to the next soudnscape"),
            p("1 - play audio of visible soundscape spectrogram")
            # p("2 - (inactive) play concatenated audio of all selected rois (default to all if none selected)")
          )
        )
      ),
      skin = "black"
    ),
    server = function(input, output, session) {
      volumes <- shinyFiles::getVolumes()()

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
            shinyalert(
              title = "There are no readable WAV files in the provided Soundscape path",
              text = "Review the provided path and confirm the setup again",
              size = "s", closeOnEsc = TRUE, closeOnClickOutside = TRUE,
              html = FALSE, type = "warning", showConfirmButton = FALSE,
              showCancelButton = FALSE, timer = 0, animation = TRUE
            )
          }
        } else if (!dir.exists(soundscape_path_val()) & dir.exists(roi_tables_path_val())) {
          shinyalert(
            title = "The provided Soundscape path does not exist",
            text = "Review the provided path and confirm the setup again",
            size = "s", closeOnEsc = TRUE, closeOnClickOutside = TRUE,
            html = FALSE, type = "warning", showConfirmButton = FALSE,
            showCancelButton = FALSE, timer = 0, animation = TRUE
          )
        } else if (dir.exists(soundscape_path_val()) & !dir.exists(roi_tables_path_val())) {
          shinyalert(
            title = "The provided path to ROI tables does not exist",
            text = "Review the provided path and confirm the setup again",
            size = "s", closeOnEsc = TRUE, closeOnClickOutside = TRUE,
            html = FALSE, type = "warning", showConfirmButton = FALSE,
            showCancelButton = FALSE, timer = 0, animation = TRUE
          )
        } else if (!dir.exists(soundscape_path_val()) & !dir.exists(roi_tables_path_val())) {
          shinyalert(
            title = "The paths to Soundscapes and to ROI tables do not exist",
            text = "Review the provided paths and confirm the setup again",
            size = "s", closeOnEsc = TRUE, closeOnClickOutside = TRUE,
            html = FALSE, type = "warning", showConfirmButton = FALSE,
            showCancelButton = FALSE, timer = 0, animation = TRUE
          )
        }

        if (!dir.exists(cuts_path_val())) {
          shinyalert(
            title = "The path to export audio and spectrograms from ROIs does not exist",
            text = "If you wish to export it, review the provided path and confirm the setup again",
            size = "s", closeOnEsc = TRUE, closeOnClickOutside = TRUE,
            html = FALSE, type = "warning", showConfirmButton = FALSE,
            showCancelButton = FALSE, timer = 0, animation = TRUE
          )
          disable("export_selected_cut")
        } else {
          enable("export_selected_cut")
        }
      })

      # todo - import labels dataframe from the global environment
      # sp_lists <- reactive({
      #   readxl::read_xlsx(labels_file, sheet = 1)
      # })

      observe({
        updateSelectizeInput(
          session, "sp_list",
          choices = colnames(sp_labels), server = TRUE
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
          updateSelectizeInput(
            session,
            inputId = "soundscape_file",
            label = paste0(
              "Soundscape (", i, " of ", nrow(soundscape_data()), ")"
            )
          )
          duration_val(seewave::duration(res))
          rec_soundscape(res)
          updateNoUiSliderInput(
            session,
            inputId = "zoom_time",
            range = c(0, duration_val()),
            value = c(0, ifelse(duration_val() >= 60, 60, duration_val()))
          )
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
            res_cut <- cutw(
              rec_soundscape(),
              from = input$zoom_time[1], to = input$zoom_time[2],
              output = "Wave"
            )
            seewave::savewav(res_cut, f = res_cut@samp.rate, filename = temp_file)
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
          tuneR::setWavPlayer("play")
          # todo Adicionar aqui uma opçao para detectar o OS e substituir o caminho default para o SoX (https://rug.mnhn.fr/seewave/HTML/MAN/sox.html)
          showElement("play_soundscape")
        } else if (x == "External player" & !is.null(input$wav_player_path)) {
          if (file.exists(input$wav_player_path)) {
            tuneR::setWavPlayer(input$wav_player_path)
            showElement("play_soundscape")
          } else {
            updateRadioButtons(session, "wav_player_type", select = "R session")
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
          res <- as.data.frame(fread(file = active_roi_table_path))
          res <- fread(file = active_roi_table_path) %>%
            as.data.frame() %>%
            fmutate(
              roi_input_timestamp = format(roi_input_timestamp, "%Y-%m-%d %H:%M:%S")
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
            roi_sample_rate = NA
          )
          roi_values(res)
        }
      })

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
              roi_sample_rate = rec_soundscape()@samp.rate
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
                roi_sample_rate = rec_soundscape()@samp.rate
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
                roi_sample_rate = NA
              )
              roi_values(roi_i_empty)
            }
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
            tuneR::play(
              seewave::cutw(
                rec_soundscape(),
                from = input$zoom_time[1], to = input$zoom_time[2],
                output = "Wave"
              )
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
        tuneR::play(
          seewave::cutw(
            rec_soundscape(),
            from = input$zoom_time[1], to = input$zoom_time[2],
            output = "Wave"
          )
        )
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
          data.table::fwrite(roi_values(), filename, row.names = FALSE)
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
          data.table::fwrite(roi_values(), filename, row.names = FALSE)
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
            data.table::fwrite(roi_values(), filename, row.names = FALSE)
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
            data.table::fwrite(roi_values(), filename, row.names = FALSE)
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
            data.table::fwrite(roi_values(), filename, row.names = FALSE)
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
            data.table::fwrite(roi_values(), filename, row.names = FALSE)
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

      spectro_soundscape <- reactiveVal(NULL)
      observe({
        req(input$soundscape_file, rec_soundscape(), roi_values())

        spec_raw <- spectro(
          rec_soundscape(),
          f = rec_soundscape()@samp.rate, norm = TRUE, plot = FALSE,
          ovlp = input$ovlp, wl = input$wl, fastdisp = TRUE, fftw = TRUE
        )
        spec_raw$amp <- pmax(pmin(spec_raw$amp, input$dyn_range[2]), input$dyn_range[1])
        n_colors <- 124
        amp_range <- range(spec_raw$amp)
        if (input$color_scale %in% c("viridis", "magma", "inferno", "cividis")) {
          colormap <- viridis::viridis(n_colors, option = input$color_scale)
        } else if (input$color_scale == "greyscale 1") {
          colormap <- seewave::reverse.gray.colors.1(n_colors)
        } else if (input$color_scale == "greyscale 2") {
          colormap <- seewave::reverse.gray.colors.2(n_colors)
        }

        rois_to_plot <- roi_values() |>
          mutate(id = row_number()) |>
          fsubset(
            roi_start < input$zoom_time[2] & roi_end > input$zoom_time[1]
          ) |>
          fmutate(
            roi_start = ifelse(
              roi_start < input$zoom_time[1], input$zoom_time[1], roi_start
            ),
            roi_end = ifelse(
              roi_end > input$zoom_time[2], input$zoom_time[2], roi_end
            ),
            roi_max_freq = ifelse(
              roi_max_freq > input$zoom_freq[2], input$zoom_freq[2], roi_max_freq
            ),
            roi_min_freq = ifelse(
              roi_min_freq < input$zoom_freq[1], input$zoom_freq[1], roi_min_freq
            )
          )

        selection_color <- ifelse(
          input$color_scale %in% c("greyscale 1", "greyscale 2"),
          "black", "white"
        )
        nr <- mat_to_nativeRaster(t(spec_raw$amp), colormap, amp_range)
        xmin <- min(spec_raw$time)
        xmax <- max(spec_raw$time)
        ymin <- min(spec_raw$freq)
        ymax <- max(spec_raw$freq)

        spectro_plot <- ggplot() +
          geom_rect(
            data = data.frame(x = NA_real_), mapping = aes(fill = x),
            xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, na.rm = TRUE
          ) +
          annotation_raster(
            nr,
            xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
          ) +
          scale_fill_gradient( # This has to match with the colormap above
            low = colormap[0], high = colormap[length(colormap)],
            limits = amp_range, na.value = "#00000000"
          ) +
          scale_x_continuous(
            expand = c(0, 0), limits = c(input$zoom_time[1], input$zoom_time[2])
          ) +
          scale_y_continuous(
            expand = c(0, 0), limits = c(input$zoom_freq[1], input$zoom_freq[2])
          ) +
          annotate(
            "label",
            label = paste0(
              input$soundscape_file, " (sr = ", rec_soundscape()@samp.rate,
              "; wl = ", input$wl, "; ovlp = ", input$ovlp, ")"
            ),
            x = -Inf, y = Inf, hjust = 0, vjust = 1,
            color = "white", fill = "black"
          ) +
          labs(x = "Time (s)", y = "Frequency (kHz)") +
          theme(legend.position = "none")

        if (nrow(rois_to_plot) > 0) {
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
          spectro_soundscape(spectro_plot)
        } else {
          spectro_soundscape(spectro_plot)
        }
      })

      output$spectrogram_plot <- renderPlot(execOnResize = TRUE, {
        req(spectro_soundscape())
        spectro_soundscape()
      })

      output$res_table <- renderDT(
        {
          req(roi_values())
          datatable(
            roi_values(),
            editable = TRUE,
            colnames = c(
              "soundscape_path", "soundscape_file", "user", "timestamp",
              "label", "start", "end", "min_freq", "max_freq",
              "type", "label_confidence", "is_complete", "comment",
              "wl", "ovlp", "sample_rate"
            ),
            options = list(
              pageLength = 50, info = FALSE, dom = "tpl",
              columnDefs = list(
                list(
                  visible = FALSE,
                  targets = c(
                    "soundscape_path", "soundscape_file", "roi_sample_rate"
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
        req(roi_values())
        df <- roi_values()
        df[input$res_table_cell_edit$row, input$res_table_cell_edit$col] <-
          input$res_table_cell_edit$value
        roi_values(df)
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
        req(roi_values(), rec_soundscape())
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

      observeEvent(input$preset_path, {
        res_list <- list.files(
          path = input$preset_path, pattern = "^segmentation_preset_.*\\.rds$"
        ) %>%
          str_remove("segmentation_preset_") %>%
          str_remove(".rds")
        if (!is.null(res_list)) {
          updateSelectInput(
            session, "available_presets",
            choices = c("Export new preset file...", res_list)
          )
        }
      })

      observeEvent(input$default_pars, {
        req(rec_soundscape())
        updateSliderInput(session, inputId = "dyn_range", value = c(-60, 0))
        updateSliderTextInput(session, inputId = "wl", selected = 2048)
        updateSliderInput(session, inputId = "ovlp", value = 0)
        updateSelectInput(session, inputId = "color_scale", selected = "inferno")
        updateSliderInput(session,
          "zoom_freq",
          min = 0, max = (rec_soundscape()@samp.rate / 2000),
          step = 1, value = c(0, 10)
        )
        updateCheckboxInput(session, inputId = "fastdisp", value = TRUE)
        updateSliderInput(session, inputId = "label_angle", value = 90)
        updateCheckboxInput(session, inputId = "show_label", value = TRUE)
        updateRadioButtons(session, inputId = "wav_player_type", selected = "R session")
        updateNoUiSliderInput(session, inputId = "zoom_freq", value = c(0, 10))
        updateCheckboxInput(session, inputId = "nav_autosave", value = TRUE)
      })

      session_settings <- reactiveVal(NULL)
      observe({
        res <- list(
          user = input$user,
          soundscapes_path = input$soundscapes_path,
          roi_tables_path = input$roi_tables_path,
          cuts_path = input$cuts_path, # temporario
          fastdisp = input$fastdisp,
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
          sp_list = input$sp_list
        )
        session_settings(res)
      })

      # Export current session settings as a rds file
      observeEvent(input$export_preset, {
        req(
          session_settings(), input$preset_path, input$available_presets
        )
        if (
          all(c(
            nchar(input$user) != 0,
            dir.exists(c(input$preset_path, soundscape_path_val(), roi_tables_path_val()))
          ))
        ) {
          preset_file <- file.path(
            input$preset_path,
            paste0("segmentation_preset_", input$available_presets, ".rds")
          )
          if (file.exists(preset_file)) {
            saved_preset <- readRDS(preset_file)
            if (identical(saved_preset, session_settings())) {
              shinyalert(
                title = "Nothing to be done",
                text = tagList(h3("No changes were made in the current preset")),
                closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = TRUE,
                type = "warning", animation = TRUE, showConfirmButton = FALSE,
                showCancelButton = FALSE
              )
            } else {
              what_changed <- rbind(saved_preset, session_settings()) %>%
                as.data.frame() %>%
                setNames(
                  list(
                    "user name", "path to soundscapes", "path to ROI tables",
                    "path to audio cuts and spectrograms", "fast display spectrogram",
                    "roi label angle", "show roi rabels", "spectrogram dynamic range",
                    "spectrogram window length", "spectrogram overlap",
                    "spectrogram color scale", "wave player type", "wave player path",
                    "session notes", "visible frequency band", "autosave while navigating",
                    "available labels for ROIs (species lists)"
                  )
                ) %>%
                select_if(function(col) length(unique(col)) > 1) %>%
                colnames() %>%
                paste(collapse = "; ")

              shinyalert(
                title = "Changes detected in preset",
                text = tagList(
                  h3("There are differences between settings in the current session and in the preset file:"),
                  h3(what_changed),
                  h3("Exporting will overwrite the existing preset file. Provide a new name below if if you wish to create a new preset instead:"),
                  textInput(
                    "new_preset_name",
                    label = NULL,
                    value = input$available_presets, placeholder = TRUE
                  ),
                  actionButton("confirm_export_preset", label = "Confirm & Export")
                ),
                closeOnEsc = TRUE, closeOnClickOutside = FALSE, html = TRUE,
                type = "warning", animation = TRUE, showConfirmButton = FALSE,
                showCancelButton = TRUE
              )
            }
          } else if (input$available_presets == "Export new preset file...") {
            new_name <- paste0(
              str_remove(user_val(), ","), "_",
              format(Sys.time(), "%Y-%m-%d_%H:%M:%S")
            )
            shinyalert(
              title = "Creating a new preset file",
              text = tagList(
                h3("Provide a name in the box below:"),
                textInput(
                  "new_preset_name",
                  label = NULL,
                  value = new_name, placeholder = TRUE
                ),
                h4("(*) avoid commas"),
                actionButton("confirm_export_preset", label = "Confirm & Export")
              ),
              closeOnEsc = TRUE, closeOnClickOutside = FALSE, html = TRUE,
              type = "warning", animation = TRUE, showConfirmButton = FALSE,
              showCancelButton = FALSE
            )
          }
        } else {
          shinyalert(
            title = "There are missing information in the User setup",
            text = tagList(
              h3("Complete the missing inputs before exporting a preset"),
            ),
            closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = TRUE,
            type = "warning", animation = TRUE, showConfirmButton = FALSE,
            showCancelButton = FALSE
          )
        }
      })

      observeEvent(input$confirm_export_preset, {
        req(session_settings(), input$preset_path, input$new_preset_name)
        saveRDS(
          session_settings(),
          file.path(
            input$preset_path, paste0("segmentation_preset_", input$new_preset_name, ".rds")
          )
        )
        res_list <- list.files(
          path = input$preset_path, pattern = "^segmentation_preset_.*\\.rds$"
        )
        res_list <- gsub("segmentation_preset_", "", res_list)
        res_list <- gsub(".rds$", "", res_list)

        if (!is.null(res_list)) {
          updateSelectInput(
            session, "available_presets",
            choices = c("Export new preset file...", res_list), #
            selected = input$new_preset_name
          )
        }
        showNotification("Preset file successfully exported")
      })

      # Import the settins of the preset files to the active session
      observeEvent(input$import_preset, {
        req(input$available_presets)

        preset_file <- file.path(
          input$preset_path,
          paste0("segmentation_preset_", input$available_presets, ".rds")
        )

        if (file.exists(preset_file)) {
          res <- readRDS(preset_file)
          session_settings(res)
          user_val(res$user)
          updateTextInput(session, inputId = "user", value = res$user)
          soundscape_path_val(res$soundscapes_path)
          updateTextAreaInput(session, inputId = "soundscapes_path", value = res$soundscapes_path)
          roi_tables_path_val(res$roi_tables_path)
          updateTextAreaInput(session, inputId = "roi_tables_path", value = res$roi_tables_path)
          cuts_path_val(res$cuts_path)
          updateTextAreaInput(session, inputId = "cuts_path", value = cuts_path_val())
          updateCheckboxInput(session, inputId = "fastdisp", value = res$fastdisp)
          updateSliderInput(session, inputId = "label_angle", value = res$label_angle)
          updateCheckboxInput(session, inputId = "show_label", value = res$show_label)
          updateSliderInput(session, inputId = "dyn_range", value = res$dyn_range)
          updateSliderInput(session, inputId = "wl", value = res$wl)
          updateSliderInput(session, inputId = "ovlp", value = res$ovlp)
          updateSelectInput(session, inputId = "color_scale", selected = res$color_scale)
          updateTextAreaInput(session, inputId = "wav_player_path", value = res$wav_player_path)
          updateRadioButtons(session, inputId = "wav_player_type", selected = res$wav_player_type)
          updateTextAreaInput(session, inputId = "session_notes", value = res$session_notes)
          updateNoUiSliderInput(session, inputId = "zoom_freq", value = res$zoom_freq)
          updateCheckboxInput(session, inputId = "nav_autosave", value = res$nav_autosave)
          updateSelectizeInput(session, inputId = "sp_list", selected = res$sp_list)
          showNotification("Preset file successfully imported")
        }
      })

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
                "There are ", nrow_unsaved, " unsaved ROIs in the current soundscape. Consider saving before leaving the session."
              )
            } else {
              message_rois <- paste0("There are no unsaved ROIs in the current soundscape.")
            }
          }
        } else {
          message_rois <- paste0("ROIs in the current soundscape are not stored in a file. Consider exporting a new one before leaving the session.")
        }
        # Check if there are settings to be saved on the preset file
        preset_file <- file.path(
          input$preset_path,
          paste0("segmentation_preset_", input$available_presets, ".rds")
        )
        if (file.exists(preset_file)) {
          saved_preset <- readRDS(preset_file)
          message_settings <- "teste1"
          what_changed <- rbind(saved_preset, session_settings()) %>%
            as.data.frame() %>%
            setNames(
              list(
                "user name", "path to soundscapes", "path to ROI tables",
                "path to audio cuts and spectrograms", "fast display spectrogram",
                "roi label angle", "show roi rabels", "spectrogram dynamic range",
                "spectrogram window length", "spectrogram overlap",
                "spectrogram color scale", "wave player type", "wave player path",
                "session notes", "visible frequency band", "autosave while navigating",
                "available labels for ROIs (species lists)"
              )
            ) %>%
            select_if(function(col) length(unique(col)) > 1) %>%
            colnames() %>%
            paste(collapse = "; ")
          if (!identical(saved_preset, session_settings())) {
            message_settings <- paste0(
              "The following settings were updated: ", what_changed,
              ". Consider update the preset or create a new one before leaving the session."
            )
          } else {
            message_settings <- paste0("No setting changes detected.")
          }
        } else {
          message_settings <- paste0("No preset file was found. Consider creating a new one before leaving the session.")
        }

        shinyalert(
          title = "Check out",
          text = tagList(
            h3(message_rois),
            h3(message_settings),
            actionButton("cancel_exit", "Cancel"),
            actionButton("confirm_exit", "End session"),
          ),
          closeOnEsc = TRUE, closeOnClickOutside = TRUE, html = TRUE,
          type = "warning", animation = TRUE,
          showConfirmButton = FALSE, showCancelButton = FALSE
        )
      })

      observeEvent(input$confirm_exit, {
        # Clear temp folder before stopping the app
        unlink(paste0(getwd(), "/temp/"), recursive = TRUE, force = TRUE)
        stopApp()
      })

      # teste_val <- reactiveVal(NULL)
      # output$checagem1 <- renderPrint({
      #   req(session_settings())
      #   list(session_data, session_settings()) %>%
      #     glimpse()
      # })

      # General popover options
      pop_up_opt <- list(delay = list(show = 1000, hide = 0))

      # Side bar menu - User setup
      shinyBS::addTooltip(session,
        id = "available_presets",
        title = "Select a preset or type a new name to create a new one.",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "import_preset",
        title = "Apply stored settings to the current session",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "export_preset",
        title = "Store the current session settings to a preset file. Existing files will be overwritten",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "user",
        title = "Identify yourself in the recommended format: 'Rosa G. L. M. (avoid commas)",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "soundscapes_path",
        title = "Parent location that contains only template files or folders of these",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "roi_tables_path",
        title = "Path to the location where ROI tables will be exported to",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "cuts_path",
        title = "Path to the location where audio cuts and spectrograms will be exported to",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "user_setup_confirm",
        title = "Setup confirmation required to start the segmentation",
        placement = "right", trigger = "hover", options = pop_up_opt
      )

      #  # Side bar menu - Session setup
      shinyBS::addTooltip(
        session,
        id = "fastdisp",
        title = "Requires que 'fftw' package",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "label_angle",
        title = "Adjust the angle of labels in the spectrogram. Recommended 90º for a less cluttering.",
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
      shinyBS::addTooltip(session,
        id = "dyn_range",
        title = "Adjust what portion of the amplitude scale is shown in the spectrograms",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "show_label",
        title = "Show the label alongside the ROI. Hide in case of ROI cluttering",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "color_scale",
        title = "Available palettes for representing spectrogram colors",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
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
        id = "default_pars",
        title = "Set spectrogram parameters back to the default",
        placement = "right", trigger = "hover", options = pop_up_opt
      )

      # Body - Spectrogram parameters

      shinyBS::addTooltip(session,
        id = "zoom_freq",
        title = "Zoom in, zoom out and slide to navigate in the fdrequency axis (affects exported spectrograms)",
        placement = "right", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "zoom_time",
        title = "Zoom in, zoom out and slide to navigate in the time axis",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )

      # Box - Soundscape spectrogram

      shinyBS::addTooltip(session,
        id = "prev_soundscape_noroi",
        title = "Navigate to the previous soundscape",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "prev_soundscape",
        title = "Navigate to the previous soundscape without ROIs",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "play_soundscape",
        title = "Play visible portion of the soundscape",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "next_soundscape",
        title = "Navigate to the next soundscape",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "next_soundscape_noroi",
        title = "Navigate to the next soundscape without ROIs",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )

      # Box - Setup and Input

      shinyBS::addTooltip(session,
        id = "soundscape_file",
        title = "Select one of the available soundscapes",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "roi_table_name",
        title = "Alternative ROI tables available for the selected soundscape",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "sp_list",
        title = "Select available lists of labels for autocompletion",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "label_name",
        title = "Label the content of the next roi",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "signal_type",
        title = "Identify the signal type",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "label_certainty",
        title = "Inform the certainty level for the label",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "signal_is_complete",
        title = "Inform if the entire target signal is clearly within ROI limits",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "label_comment",
        title = 'Provide additional information (avoid "quotation marks" and separate different fields of content with "_" or ";") ',
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )

      # Box - ROI table

      shinyBS::addTooltip(session,
        id = "save_roi",
        title = "Export the changes made to the currently active ROI table",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "export_new_roi_table",
        title = "Export new ROI table",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "export_selected_cut",
        title = "Export audio cuts of the ROIs selected in the table below",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
      shinyBS::addTooltip(session,
        id = "delete_selected_rois",
        title = "Delete the ROIs selected in the table below",
        placement = "bottom", trigger = "hover", options = pop_up_opt
      )
    },
  )
}
