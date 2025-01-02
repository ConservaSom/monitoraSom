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
#' @param preset_path Path to temporary files and other acessory files used by
#'   the app.
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
#'   current session. The default is "CBRO-2021 (Birds - Brazil)".
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
#' @param dyn_range_bar A numeric vector of length 2 specifying the limits to be
#'   diplayed on the the bar that controls the dinamic range on the app. By
#'   default c(-200, 10).
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
#' @param visible_bp If TRUE, the visible frequency band will be used as a
#'   bandpass filter for the audio playback.
#' @param play_norm If TRUE, the audio playback will be normalized.
#' @param session_notes A single character string with related to the current
#'   segmentation session.
#' @param zoom_freq Vector with two numeric values between 0 and the Nyquist
#'   Frequency of the soundscape recording, indicating the frequency values in
#'   kHz of the frequency band to be displayed in the spectrogram.
#' @param nav_autosave If TRUE, navigating between soundscapes will
#'   automatically save the ROI table of the active soundscape.
#' @param pitch_shift A numeric value indicating the pitch shift to be applied
#'   to the soundscape audio. The default is 1, which means no pitch shift.
#' @param visible_bp If TRUE, the visible frequency band will be used as a
#'   bandpass filter for the audio playback.
#' @param play_norm If TRUE, the audio playback will be normalized.
#' @param time_guide_interval A numeric value indicating the interval in seconds
#'   between time guides in the spectrogram.
#' @param freq_guide_interval A numeric value indicating the interval in kHz
#'   between frequency guides in the spectrogram.
#'
#' @return Todo
#' @export
#' @import shiny dplyr ggplot2 DT shinyWidgets shinydashboard keys
#'  shinyjs shinyBS
#' @importFrom data.table fread fwrite
#' @importFrom lubridate ymd_hms
#' @importFrom tuneR readWave normalize setWavPlayer play
#' @importFrom readxl read_excel
#' @importFrom openxlsx write.xlsx
#' @importFrom patchwork plot_layout
#' @importFrom seewave ffilter duration
#' @importFrom stringr str_replace str_remove str_pad
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shinyWidgets alert
#' @examples
#' \dontrun{
#' library(monitoraSom)
#'
#' # set the path to the project folder
#' project_path <- "path/to/project"
#' }
launch_segmentation_app <- function(
    project_path = NULL, preset_path = NULL, user = NULL,
    soundscapes_path = NULL, roi_tables_path = NULL, cuts_path = NULL,
    labels_file = NULL, sp_list = "CBRO-2021 (Birds - Brazil)",
    label_angle = 90, show_label = TRUE,
    time_guide_interval = 3, freq_guide_interval = 2,
    dyn_range = c(-60, 0), dyn_range_bar = c(-144, 0),
    wl = 1024, ovlp = 0, color_scale = "inferno",
    wav_player_type = "HTML player", wav_player_path = "play",
    visible_bp = FALSE, play_norm = FALSE, session_notes = NULL, zoom_freq = c(0, 180),
    nav_autosave = TRUE, pitch_shift = 1) {

  session_data <- list()

  if (!is.null(project_path)) {
    if (!dir.exists(project_path)) {
      tryCatch(
        {
          dir.create(project_path)
          warning("Created project directory at '", project_path, "'")
        },
        error = function(e) {
          stop("Failed to create project directory at '", project_path, "': ", e$message)
        }
      )
    }
    session_data$project_path <- project_path
  }

  if (!is.null(user)) {
    session_data$user <- as.character(gsub(",", "", user))
  } else if (is.null(preset_path)) {
    session_data$user <- as.character(NA)
    warning("Warning! 'user' variable was not provided. Please provide the correct value and confirm within the app.")
  }

  if (!dir.exists(soundscapes_path)) {
    session_data$soundscapes_path <- NA
    warning("Warning! The path provided in 'soundscapes_path' was not found locally. Please provide the correct value and confirm within the app.")
  } else {
    session_data$soundscapes_path <- soundscapes_path
  }

  if (is.null(roi_tables_path) && is.null(project_path)) {
    session_data$roi_tables_path <- NA
    warning(
      "Warning! No values were provided for 'roi_tables_path' and 'project_path' variables. Please provide and confirm within the app."
    )
  } else if (is.null(roi_tables_path) & !is.null(project_path)) {
    roi_tables_path <- file.path(project_path, "roi_tables/")
    if (!dir.exists(roi_tables_path)) {
      dir.create(roi_tables_path)
      warning(
        "Warning! The path provided in 'roi_tables_path' was not found locally. A new 'roi_tables' directory was successfully created at '", roi_tables_path, "'"
      )
    }
    session_data$roi_tables_path <- roi_tables_path
  } else if (!is.null(roi_tables_path)) {
    if (!dir.exists(roi_tables_path)) {
      stop(
        "Error! The path provided in 'roi_tables_path' was not found locally and no 'project_path' was provided to create it."
      )
    } else {
      session_data$roi_tables_path <- roi_tables_path
    }
  }
  if (is.null(cuts_path)) {
    if (is.null(project_path)) {
      session_data$cuts_path <- NA
      warning("Warning! No value was provided for 'cuts_path' and 'project_path' variables. Inform the value and confirm within the app.")
    } else {
      cuts_path <- file.path(project_path, "roi_cuts/")
      if (!dir.exists(cuts_path)) {
        dir.create(cuts_path)
        warning("Warning! The path informed in 'cuts_path' was not found locally. A new 'cuts' directory was successfully created within the informed 'project_path', at '", cuts_path, "'")
      }
      session_data$cuts_path <- cuts_path
    }
  } else {
    if (!dir.exists(cuts_path)) {
      stop("Error! The path informed in 'cuts_path' was not found locally and no 'project_path' was informed to create it.")
    } else {
      session_data$cuts_path <- cuts_path
    }
  }

  if (
    !is.numeric(label_angle) ||
      !all(c(label_angle %% 10 == 0, label_angle >= 0, label_angle <= 90))
  ) {
    stop("Error! 'label_angle' must be numeric, a multiple of 10 between 0 and 90.")
  }
  session_data$label_angle <- label_angle

  if (!is.logical(show_label)) {
    stop("Error! 'show_label' must be a logical value (TRUE or FALSE).")
  }
  session_data$show_label <- show_label

  # validate time_guide_interval and freq_guide_interval
  if (!is.numeric(time_guide_interval) || time_guide_interval <= 0) {
    session_data$time_guide_interval <- 3 # Default value
  } else {
    session_data$time_guide_interval <- time_guide_interval
  }
  if (!is.numeric(freq_guide_interval) || freq_guide_interval <= 0) {
    session_data$freq_guide_interval <- 2 # Default value
  } else {
    session_data$freq_guide_interval <- freq_guide_interval
  }

  if (length(dyn_range) != 2 || !all(is.numeric(dyn_range))) {
    stop("Error! 'dyn_range' must be a numeric vector of length 2.")
  }

  if (dyn_range[1] >= dyn_range[2]) {
    session_data$dyn_range <- sort(dyn_range)
    warning("Warning! The first value of 'dyn_range' must be smaller than the second. Sorting to match the expected order.")
  } else {
    session_data$dyn_range <- dyn_range
  }

  if (length(dyn_range_bar) != 2 || !all(is.numeric(dyn_range_bar))) {
    stop("Error! 'dyn_range_bar' must be a numeric vector of length 2 with all values numeric.")
  }

  if (length(dyn_range_bar) == 2 && all(is.numeric(dyn_range_bar))) {
    session_data$dyn_range_bar <- sort(dyn_range_bar)
    if (dyn_range_bar[1] >= dyn_range_bar[2]) {
      warning("Warning! The first value of 'dyn_range_bar' must be smaller than the second. Sorting to match the expected order.")
    }
  } else {
    stop("Error! 'dyn_range_bar' must be a numeric vector of length 2.")
  }

  valid_wl_values <- c(128, 256, 512, 1024, 2048, 4096, 8192, 16384)
  if (!is.numeric(wl) || !wl %in% valid_wl_values) {
    stop(sprintf(
      "Error! The value assigned to 'wl' must be numeric and among the expected alternatives: %s.",
      paste(valid_wl_values, collapse = ", ")
    ))
  }

  session_data$wl <- wl
  # Validate 'ovlp': must be numeric, between 0 and 80, and a multiple of 10
  if (!is.numeric(ovlp) || ovlp < 0 || ovlp > 80 || ovlp %% 10 != 0) {
    stop("Error! The value assigned to 'ovlp' must be a numeric value between 0 and 80, in steps of 10.")
  }
  session_data$ovlp <- ovlp

  # Validate 'color_scale' against expected values
  valid_color_scales <- c("viridis", "magma", "inferno", "cividis", "greyscale 1", "greyscale 2")
  if (!is.character(color_scale) || !(color_scale %in% valid_color_scales)) {
    stop(sprintf(
      "Error! The value assigned to 'color_scale' must be one of the following: %s.",
      paste(valid_color_scales, collapse = ", ")
    ))
  }
  session_data$color_scale <- color_scale

  valid_player_types <- c("HTML player", "R session", "External player")
  if (!wav_player_type %in% valid_player_types) {
    stop(sprintf(
      "Error! The selected WAV player method is not valid. Choose one of the following: %s.",
      paste(valid_player_types, collapse = ", ")
    ))
  }
  if (wav_player_type == "External player" && !file.exists(wav_player_path)) {
    stop("Error! The path informed in 'wav_player_path' was not found locally.")
  }
  session_data$wav_player_type <- wav_player_type
  if (wav_player_type == "External player") {
    session_data$wav_player_path <- wav_player_path
  }

  # Validate and assign 'visible_bp'
  if (!is.logical(visible_bp)) {
    stop("Error! The value assigned to 'visible_bp' must be a logical value (TRUE or FALSE).")
  }
  session_data$visible_bp <- visible_bp

  # Validate and assign 'play_norm'
  if (!is.logical(play_norm)) {
    stop("Error! The value assigned to 'play_norm' must be a logical value (TRUE or FALSE).")
  }
  session_data$play_norm <- play_norm

  # Validate 'zoom_freq': must be a numeric vector of length 2, with values between 0 and 180, and the first value smaller than the second
  if (length(zoom_freq) != 2 || !is.numeric(zoom_freq)) {
    stop("Error! 'zoom_freq' must be a numeric vector of length 2.")
  }

  if (any(zoom_freq < 0) || any(zoom_freq > 192)) {
    stop("Error! 'zoom_freq' values must be between 0 and 192.")
  }

  if (zoom_freq[1] >= zoom_freq[2]) {
    session_data$zoom_freq <- sort(zoom_freq)
    warning("Warning! The first value of 'zoom_freq' must be smaller than the second. Sorting to match the expected order.")
  } else {
    # Round to 0.1 intervals
    session_data$zoom_freq <- round(zoom_freq * 10) / 10
    if (any(session_data$zoom_freq != zoom_freq)) {
      warning(
        "Warning! The values of 'zoom_freq' were rounded to the nearest 0.1 interval. The values are now: ",
        session_data$zoom_freq[1], " and ", session_data$zoom_freq[2]
      )
    }
  }

  # Validate 'nav_autosave' to ensure it is a logical value
  if (!is.logical(nav_autosave)) {
    stop("Error! The value assigned to 'nav_autosave' must be logical (TRUE or FALSE).")
  }
  session_data$nav_autosave <- nav_autosave

  # Handle preset path
  if (!is.null(preset_path)) {
    if (!dir.exists(preset_path)) {
      dir.create(preset_path, recursive = TRUE)
      warning("The segmentation preset destination directory was created automatically at '", preset_path, "'")
    }
    session_data$preset_path <- preset_path

    # Create temp directory within preset path
    temp_path <- file.path(preset_path, "temp/")
    if (!dir.exists(temp_path)) {
      dir.create(temp_path)
    }
    session_data$temp_path <- temp_path
  } else if (!is.null(project_path)) {
    # Define preset path based on project path
    session_data$preset_path <- file.path(project_path, "app_presets/")
    session_data$temp_path <- file.path(session_data$preset_path, "temp/")

    # Create preset and temp directories if they do not exist
    dir.create(session_data$preset_path, showWarnings = FALSE, recursive = TRUE)
    dir.create(session_data$temp_path, showWarnings = FALSE)
  } else {
    # Fallback to temporary directory if neither preset_path nor project_path is provided
    session_data$preset_path <- tempdir()
    session_data$temp_path <- file.path(session_data$preset_path, "temp/")
    dir.create(session_data$temp_path, showWarnings = FALSE)
  }

  # validate the pitch shift value
  if (!is.numeric(pitch_shift) || pitch_shift < -8 || pitch_shift > 1) {
    stop("Error! The value assigned to 'pitch_shift' must be a numeric value between -8 and 1.")
  }
  session_data$pitch_shift <- pitch_shift

  # Load species labels with proper fallback
  load_species_labels <- function(labels_file, project_path) {
    # Load default labels from package
    data("sp_labels", package = "monitoraSom", envir = environment())
    default_labels <- sp_labels
    # Return if no custom path needed
    if (is.null(labels_file) && is.null(project_path)) {
      return(default_labels)
    }
    # Try custom file if provided
    if (!is.null(labels_file) && file.exists(labels_file)) {
      return(tryCatch(read_xlsx(labels_file), error = function(e) default_labels))
    }
    # Handle project path case
    if (!is.null(project_path)) {
      preset_path <- file.path(project_path, "app_presets", "sp_labels.xlsx")
      if (!dir.exists(dirname(preset_path))) {
        dir.create(dirname(preset_path), recursive = TRUE)
      }
      if (!file.exists(preset_path)) {
        write.xlsx(default_labels, preset_path)
      }
      return(tryCatch(
        read_xlsx(preset_path),
        error = function(e) default_labels
      ))
    }
    return(default_labels)
  }
  sp_labels <- load_species_labels(labels_file, project_path)

  if (!sp_list %in% colnames(sp_labels)) {
    warning("The selected species list is not among the available species lists. Using the default species list.")
  }
  session_data$sp_list <- sp_list

  # This function defines where embedded html wav players will look for the files
  addResourcePath("audio", session_data$temp_path)

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
    "c" # navigate to the next soudnscape
  )

  # helper functions ---------------------------------------------------------

  # Helper function to play audio with optional filtering and normalization
  play_within_R <- function(rec, zoom_time, pitch_shift, visible_bp, zoom_freq, wl, play_norm) {
    tryCatch(
      {
        # Debug information
        message("Debug - Original zoom_time: ", paste(zoom_time, collapse = ", "))
        message("Debug - Pitch shift: ", pitch_shift)

        # Calculate adjusted times
        from_time <- zoom_time[1] * abs(pitch_shift)
        to_time <- zoom_time[2] * abs(pitch_shift)

        message("Debug - Adjusted times: from=", from_time, " to=", to_time)

        # Adjust sample rate for pitch shift
        if (pitch_shift < 1) {
          rec@samp.rate <- rec@samp.rate / abs(pitch_shift)
        }

        # Validate and ensure correct time order
        if (from_time >= to_time) {
          message("Invalid time range detected. Swapping values.")
          temp <- from_time
          from_time <- to_time
          to_time <- temp
        }

        # Cut the audio based on zoom time
        rec <- cutw(rec, from = from_time, to = to_time, output = "Wave")

        # Rest of the function remains the same
        if (isTRUE(visible_bp)) {
          rec <- fir(
            rec,
            f = rec@samp.rate,
            from = (zoom_freq[1] / pitch_shift) * 1000,
            to = (zoom_freq[2] / pitch_shift) * 1000,
            wl = wl, output = "Wave"
          )
        }

        if (isTRUE(play_norm)) {
          rec <- tuneR::normalize(
            object = rec, unit = as.character(rec@bit), pcm = TRUE
          )
        }

        play(rec, player = "play")
      },
      error = function(e) {
        message("Error in play_within_R: ", e$message)
        message("zoom_time: ", paste(zoom_time, collapse = ", "))
        message("pitch_shift: ", pitch_shift)
      }
    )
  }

  shinyApp(
    ui = dashboardPage(
      header = dashboardHeader(title = "MonitoraSom", titleWidth = "400px"),
      sidebar = dashboardSidebar(
        tags$head(
          tags$style(HTML(".form-group { margin-bottom: 10px !important; }")),
          # Add JavaScript for spacebar audio control
          tags$script("
            $(document).on('keydown', function(e) {
                // Ignore if focus is on an input element
                if (e.target.tagName === 'INPUT') return true;
                // Handle spacebar press
                if (e.key === ' ' || e.key === 'Spacebar') {
                    e.preventDefault();  // Prevent page scroll
                    // For HTML player
                    var audioPlayer = document.getElementById('visible_soundscape_clip_selector');
                    if (audioPlayer) {
                        if (audioPlayer.paused) {
                            audioPlayer.play();
                        } else {
                            audioPlayer.pause();
                        }
                    } else {
                        // For R session/External player
                        Shiny.setInputValue('toggle_play', Math.random());
                    }
                }
            });
        ")
        ),
        width = "400px",
        sidebarMenu(
          menuItem(
            "User setup",
            tabName = "user_setup_tab", startExpanded = TRUE,
            icon = icon(lib = "glyphicon", "glyphicon glyphicon-user"),
            textInput(
              "user", "User name",
              value = session_data$user, placeholder = "Type your name here",
              width = "100%"
            ),
            textAreaInput(
              inputId = "preset_path", label = "Path to preset files",
              value = session_data$preset_path,
              placeholder = "Paste or load path here",
              height = "50px", width = "100%", resize = "vertical"
            ),
            bsTooltip("preset_path",
              title = "Paste the path to app accessory files here.",
              placement = "right", trigger = "hover",
              options = list(delay = list(show = 1000, hide = 0))
            ),
            tags$style(".tooltip {width: 300px;}"),
            textAreaInput("soundscapes_path", "Path to the Soundscapes",
              value = session_data$soundscapes_path,
              placeholder = "Paste or load path here",
              height = "50px", resize = "vertical", width = "100%"
            ),
            textAreaInput("roi_tables_path", "Destination of ROI tables",
              value = session_data$roi_tables_path,
              placeholder = "Paste or load path here",
              height = "50px", resize = "vertical", width = "100%"
            ),
            textAreaInput("cuts_path", "Destination of ROI audio cuts",
              value = session_data$cuts_path,
              placeholder = "Paste or load path here",
              height = "50px", resize = "vertical", width = "100%"
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
              sliderInput(
                "label_angle", "Adjust label angle (degrees)",
                min = 0, max = 90, step = 10, value = session_data$label_angle
              ),
              checkboxInput("show_label", "Show label", value = session_data$show_label)
            ),
            splitLayout(
              cellWidths = c("50%", "50%"),
              numericInput("time_guide_interval", "Time Guide Interval (s)",
                value = session_data$time_guide_interval, min = 0.01, max = 60, step = 0.01
              ),
              numericInput("freq_guide_interval", "Freq Guide Interval (kHz)",
                value = session_data$freq_guide_interval, min = 0.01, max = 192, step = 0.01
              )
            ),
            sliderInput(
              "dyn_range", "Dynamic range (dB)",
              min = session_data$dyn_range_bar[1],
              max = session_data$dyn_range_bar[2],
              step = 6, value = session_data$dyn_range,
              width = "100%", post = "dB"
            ),
            sliderTextInput(
              "wl", "Window length",
              choices = c(128, 256, 512, 1024, 2048, 4096, 8192, 16384),
              selected = session_data$wl, grid = TRUE, width = "100%"
            ),
            sliderInput(
              "ovlp", "Overlap (%)",
              min = 0, max = 80, value = session_data$ovlp,
              post = "%", step = 10, width = "100%"
            ),
            sliderTextInput(
              "pitch_shift", "Pitch shift (octaves) and slow down (factor)",
              choices = c(-8, -6, -4, -2, 1),
              selected = session_data$pitch_shift,
              post = " octaves", grid = TRUE, width = "100%"
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
            textAreaInput("wav_player_path",
              value = session_data$wav_player_path,
              label = "Path to player executable (default = 'play')",
              height = "40px", resize = "vertical"
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
                min = 0, max = 180, step = 0.1, value = session_data$zoom_freq,
                direction = "rtl", orientation = "vertical", width = "100px",
                height = "25vh", behaviour = "drag", format = wNumbFormat(decimals = 1),
                update_on = "end"
              )
            ),
            column(
              width = 11,
              plotOutput(
                "spectrogram_plot",
                brush = "roi_limits", height = "500px"
              )
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
                  # todo - implement custom choices based on a spreadsheet of signal types
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
          tabPanel( #
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
            p("Ctrl + - Zoom in the whole interface"),
            p("Ctrl - - Zoom out the whole interface"),
            p("Spacebar - play/pause the visible soundscape audio")
          )
        )
      ),
      skin = "black"
    ),
    server = function(input, output, session) {

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

        if (!dir.exists(soundscape_path_val()) || !dir.exists(roi_tables_path_val())) {
          error_msg <- if (!dir.exists(soundscape_path_val()) && !dir.exists(roi_tables_path_val())) {
            "The paths to Soundscapes and to ROI tables do not exist"
          } else if (!dir.exists(soundscape_path_val())) {
            "The provided Soundscape path does not exist"
          } else {
            "The provided path to ROI tables does not exist"
          }

          showModal(
            modalDialog(
              title = "Setup error", error_msg,
              footer = tagList(modalButton("OK")), easyClose = TRUE
            )
          )
          return()
        }

        list_soundscapes <- list.files(
          soundscape_path_val(),
          pattern = ".wav", full.names = TRUE, ignore.case = TRUE
        )
        roi_tables_raw <- list.files(
          roi_tables_path_val(),
          pattern = ".csv", full.names = TRUE, ignore.case = TRUE
        )
        roi_tables <- data.frame(
          roi_table_name = basename(roi_tables_raw),
          roi_table_path = roi_tables_raw
        )

        if (length(list_soundscapes) == 0) {
          showModal(modalDialog(
            title = "Setup error",
            "There are no readable WAV files in the provided Soundscape path",
            footer = tagList(modalButton("OK")),
            easyClose = TRUE
          ))
          return()
        }

        soundscape_data_res <- data.frame(
          soundscape_path = list_soundscapes,
          soundscape_file = basename(list_soundscapes)
        )
        soundscape_data_res$roi_table_prefix <- gsub(
          ".wav|.WAV", "", soundscape_data_res$soundscape_file
        )
        soundscape_data_res$n_char <- nchar(soundscape_data_res$roi_table_prefix)
        roi_table_files <- list.files(
          roi_tables_path_val(),
          pattern = ".csv", full.names = FALSE, ignore.case = TRUE
        )
        soundscape_data_res$has_table <- sapply(
          soundscape_data_res$roi_table_prefix,
          function(prefix) {
            any(prefix == substr(roi_table_files, 1, nchar(prefix)))
          }
        )
        updateSelectizeInput(
          session, "soundscape_file",
          choices = soundscape_data_res$soundscape_file, server = TRUE
        )
        soundscape_data(soundscape_data_res)
        showNotification("Setup sucessfull!", type = "message")
        roi_tables_data(roi_tables)

        if (!dir.exists(cuts_path_val())) {
          showModal(modalDialog(
            title = "Setup error",
            "The path to export audio and spectrograms from ROIs does not exist",
            footer = tagList(modalButton("OK")),
            easyClose = TRUE
          ))
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

      # Reactive values for soundscape data
      rec_soundscape <- reactiveVal(NULL)
      duration_val <- reactiveVal(NULL)
      observe({
        req(soundscape_data(), input$soundscape_file)
        wav_data <- soundscape_data()
        current_index <- which(wav_data$soundscape_file == input$soundscape_file)
        wav_path <- wav_data$soundscape_path[current_index]
        if (is.null(wav_path) || length(wav_path) != 1 || !file.exists(wav_path)) {
          return()
        }
        tryCatch(
          {
            wav_obj <- tuneR::readWave(wav_path)
            wav_duration <- seewave::duration(wav_obj)
            updateNoUiSliderInput(
              session,
              inputId = "zoom_time",
              range = c(0, wav_duration), value = c(0, min(wav_duration, 60))
            )
            updateSelectizeInput(
              session,
              inputId = "soundscape_file",
              label = sprintf("Soundscape (%d of %d)", current_index, nrow(wav_data))
            )
            updateNoUiSliderInput(
              session,
              inputId = "zoom_freq",
              range = c(0, (wav_obj@samp.rate / 2000) - 1)
            )
            duration_val(wav_duration)
            rec_soundscape(wav_obj)
          },
          error = function(e) {
            showNotification(
              sprintf("Error reading wav file: %s", e$message),
              type = "error"
            )
          }
        )
      })

      wav_path_val <- reactiveVal(NULL)
      observe({
        req(soundscape_data(), rec_soundscape())
        i <- which(soundscape_data()$soundscape_file == input$soundscape_file)
        wav_path <- soundscape_data()$soundscape_path[i]
        wav_path_val(wav_path)
        if (!is.null(wav_path) & length(wav_path) == 1) {
          if (file.exists(wav_path) & input$wav_player_type == "HTML player") {
            temp_file <- gsub(
              "\\\\", "/",
              tempfile(tmpdir = session_data$temp_path, fileext = ".wav")
            )
            pitch_shift <- abs(input$pitch_shift)
            if (input$zoom_time[1] < input$zoom_time[2]) {
              tryCatch(
                {
                  start_time <- max(0, input$zoom_time[1])
                  end_time <- min(duration_val(), input$zoom_time[2])
                  if (start_time >= end_time) {
                    showNotification("Invalid time range", type = "error")
                    return()
                  }
                  res_cut <- cutw(
                    rec_soundscape(), from = start_time, to = end_time, output = "Wave"
                  )
                  if (input$pitch_shift < 1) {
                    res_cut@samp.rate <- res_cut@samp.rate / pitch_shift
                  }
                  if (isTRUE(input$visible_bp)) {
                    tryCatch(
                      {
                        res_cut <- seewave::ffilter(
                          res_cut, f = res_cut@samp.rate,
                          from = (input$zoom_freq[1] / pitch_shift) * 1000,
                          to = (input$zoom_freq[2] / pitch_shift) * 1000,
                          wl = input$wl, output = "Wave", bandpass = TRUE
                        )
                      },
                      error = function(e) {
                        showNotification("Error applying frequency filter", type = "error")
                        return()
                      }
                    )
                  }
                  if (isTRUE(input$play_norm)) {
                    tryCatch(
                      {
                        res_cut <- tuneR::normalize(
                          object = res_cut, unit = as.character(res_cut@bit),
                          pcm = TRUE
                        )
                      },
                      error = function(e) {
                        showNotification("Error normalizing audio", type = "error")
                        return()
                      }
                    )
                  }
                  seewave::savewav(res_cut, f = res_cut@samp.rate, filename = temp_file)
                  removeUI(selector = "#visible_soundscape_clip_selector")
                  insertUI(
                    selector = "#visible_soundscape_clip", where = "afterEnd",
                    ui = tags$audio(
                      id = "visible_soundscape_clip_selector",
                      src = paste0("audio/", basename(temp_file)),
                      type = "audio/wav", autostart = FALSE, controls = TRUE
                    )
                  )
                  unlink("*.wav")
                  to_remove <- list.files(session_data$temp_path, pattern = ".wav", full.names = TRUE)
                  file.remove(to_remove[to_remove != temp_file])
                },
                error = function(e) {
                  showNotification(
                    "Error processing audio segment. Please try again.",
                    type = "error"
                  )
                  return()
                }
              )
            }
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
          # todo Adicionar aqui uma opcao para detectar o OS e substituir o caminho default para o SoX (https://rug.mnhn.fr/seewave/HTML/MAN/sox.html)
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

      # Add this at the beginning of the server function (around line 921)
      audio_playing <- reactiveVal(FALSE)

      # Add this in the server section, near other observeEvent handlers (around
      # line 1525)
      observeEvent(input$toggle_play, {
        req(
          rec_soundscape(),
          input$wav_player_type %in% c("R session", "External player")
        )

        if (!audio_playing()) {
          # Start playback
          validate(need(
            input$zoom_time[1] < input$zoom_time[2],
            "Invalid time range selected"
          ))

          audio_playing(TRUE)
          play_within_R(
            rec_soundscape(),
            zoom_time = input$zoom_time,
            pitch_shift = input$pitch_shift,
            visible_bp = input$visible_bp,
            zoom_freq = input$zoom_freq,
            wl = input$wl,
            play_norm = input$play_norm
          )
        } else {
          # Stop playback
          audio_playing(FALSE)
          if (input$wav_player_type != "R session") {
            system("killall play", ignore.stderr = TRUE) # For Linux/Mac
            system("taskkill /IM play.exe /F", ignore.stderr = TRUE) # For Windows
          }
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
            dplyr::filter(grepl(name_pattern, roi_table_name))
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

      # Create a reactive object with the content of the active ROI table
      roi_values <- reactiveVal(NULL)
      observeEvent(input$roi_table_name, {
        req(alt_roitabs_meta(), input$roi_table_name, user_val())
        active_roi_table_path <- alt_roitabs_meta() %>%
          dplyr::filter(roi_table_name == input$roi_table_name) %>%
          dplyr::pull(roi_table_path)
        var_names <- c(
          "soundscape_path", "soundscape_file", "roi_user", "roi_input_timestamp",
          "roi_label", "roi_start", "roi_end", "roi_min_freq", "roi_max_freq",
          "roi_type", "roi_label_confidence", "roi_is_complete", "roi_comment",
          "roi_wl", "roi_ovlp", "roi_sample_rate", "roi_pitch_shift"
        )
        create_empty_roi_df <- function() {
          data.frame(matrix(NA, nrow = 0, ncol = length(var_names), dimnames = list(NULL, var_names)))
        }
        if (file.exists(active_roi_table_path)) {
          res_raw <- as.data.frame(
            data.table::fread(file = active_roi_table_path)
          )
          res_raw <- lapply(var_names, function(var) {
            if (!var %in% names(res_raw)) {
              res_raw[[var]] <- NA
            }
            return(res_raw)
          })[[1]]
          res <- dplyr::transmute(
            res_raw,
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
        } else {
          res <- create_empty_roi_df()
        }
        roi_values(res)
      })

      ruler <- reactiveVal(NULL)

      # Define shared save function for buttons and hotkeys
      save_roi_table <- function() {
        req(roi_values(), input$roi_table_name, alt_roitabs_meta())
        if (is.null(roi_values()) || !is.data.frame(roi_values()) || nrow(roi_values()) == 0) {
          showNotification("No ROIs to save", type = "warning")
          return()
        }
        current_rois <- roi_values()[roi_values()$soundscape_file == input$soundscape_file, ]
        if (nrow(current_rois) == 0) {
          showNotification("Error: No valid ROIs for current soundscape", type = "error")
          return()
        }
        complete_rows <- complete.cases(current_rois[, c("roi_label", "roi_start", "roi_end", "roi_min_freq", "roi_max_freq")])
        if (!any(complete_rows)) {
          showNotification("Error: No complete ROIs found", type = "error")
          return()
        }
        current_rois <- current_rois[complete_rows, ]
        if (nrow(current_rois) < nrow(roi_values())) {
          roi_values(current_rois)
          showNotification(sprintf(
            "Removed %d mismatched or incomplete ROI(s)",
            nrow(roi_values()) - nrow(current_rois)
          ), type = "warning")
        }
        fwrite(current_rois, file.path(roi_tables_path_val(), input$roi_table_name))
        current_index <- which(soundscape_data()$soundscape_file == input$soundscape_file)
        progress_tracker$df$has_table[current_index] <- TRUE
        n_done <- sum(progress_tracker$df$has_table)
        updateProgressBar(session, "progress_bar", value = n_done, total = nrow(progress_tracker$df))
        showNotification("ROI table successfully exported", type = "message")
        if (n_done == nrow(progress_tracker$df)) {
          showNotification("All recordings were segmented!", type = "message")
        }
      }

      # Define shared navigation function
      navigate_soundscape <- function(direction) {
        req(soundscape_data())
        vec_soundscapes <- soundscape_data()$soundscape_file
        current_index <- which(vec_soundscapes == input$soundscape_file)
        if (input$nav_autosave && !all(is.na(roi_values())) && nrow(roi_values()) > 0) {
          save_roi_table()
        }
        new_index <- switch(direction,
          "prev" = if (current_index > 1) current_index - 1 else current_index,
          "next" = if (current_index < length(vec_soundscapes)) current_index + 1 else current_index
        )
        if (new_index != current_index) {
          updateSelectInput(
            session, "soundscape_file",
            selected = vec_soundscapes[new_index]
          )
        }
      }

      # Define shared navigation function for unsegmented soundscapes
      navigate_unsegmented <- function(direction) {
        req(progress_tracker$df)

        # Get unsegmented soundscapes and current position
        unsegmented_data <- progress_tracker$df %>%
          dplyr::filter(has_table == FALSE | soundscape_file == input$soundscape_file)

        unsegmented_files <- unsegmented_data$soundscape_file
        current_index <- which(unsegmented_files == input$soundscape_file)

        # Handle autosave if enabled
        if (input$nav_autosave && !all(is.na(roi_values())) && nrow(roi_values()) > 0) {
          save_roi_table()
        }

        # Calculate new index based on direction
        new_index <- switch(direction,
          "prev" = if (current_index > 1) current_index - 1 else current_index,
          "next" = if (current_index < length(unsegmented_files)) current_index + 1 else current_index
        )

        # Update selection if index changed
        if (new_index != current_index) {
          updateSelectInput(
            session, "soundscape_file",
            selected = unsegmented_files[new_index]
          )
        }
      }

      # Create a single play function to handle all playback requests
      play_audio <- function() {
        req(rec_soundscape())

        # Validate time range
        validate(need(
          input$zoom_time[1] < input$zoom_time[2],
          "Invalid time range selected"
        ))

        if (input$wav_player_type == "HTML player") {
          runjs("
            var player = document.getElementById('visible_soundscape_clip_selector');
            if (player) {
              if (player.paused) {
                player.play();
              } else {
                player.pause();
              }
            }
          ")
        } else if (input$wav_player_type %in% c("R session", "External player")) {
          req(input$wav_player_path)
          play_within_R(
            rec_soundscape(),
            zoom_time = input$zoom_time,
            pitch_shift = input$pitch_shift,
            visible_bp = input$visible_bp,
            zoom_freq = input$zoom_freq,
            wl = input$wl,
            play_norm = input$play_norm
          )
        }
      }

      observeEvent(input$hotkeys, {
        req(roi_values())
        current_rois <- tibble(roi_values())

        if (input$hotkeys == "e") {
          # Create a new ROI entry
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
            roi_label_confidence = input$label_certainty,
            roi_is_complete = input$signal_is_complete,
            roi_comment = input$label_comment,
            roi_wl = input$wl,
            roi_ovlp = input$ovlp,
            roi_sample_rate = rec_soundscape()@samp.rate,
            roi_pitch_shift = input$pitch_shift
          )

          # Update roi_values based on current_rois
          if (all(is.na(current_rois))) {
            roi_values(roi_i)
          } else {
            if (nrow(current_rois) >= 1) {
              res <- bind_rows(current_rois, roi_i)
              roi_values(res)
            }
          }
        } else if (input$hotkeys == "q") {
          # Remove the last ROI entry if more than one exists
          if (nrow(current_rois) > 1) {
            res <- head(current_rois, -1)
            roi_values(res)
          } else {
            # Create an empty ROI entry if no entries remain
            roi_i_empty <- tibble::tibble(
              soundscape_path = NA, soundscape_file = NA, roi_user = NA,
              roi_input_timestamp = NA, roi_label = NA, roi_start = NA,
              roi_end = NA, roi_min_freq = NA, roi_max_freq = NA, roi_type = NA,
              roi_label_confidence = NA, roi_is_complete = NA, roi_comment = NA,
              roi_wl = NA, roi_ovlp = NA, roi_sample_rate = NA,
              roi_pitch_shift = NA
            )
            roi_values(roi_i_empty)
          }
        }

        # these should be kept dependent on hotkey pressing
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
              roi_input_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              roi_start = input$roi_limits$xmin,
              roi_end = input$roi_limits$xmax,
              roi_duration = input$roi_limits$xmax - input$roi_limits$xmin,
              roi_min_freq = input$roi_limits$ymin,
              roi_max_freq = input$roi_limits$ymax,
              roi_bandwidth = input$roi_limits$ymax - input$roi_limits$ymin
            )
            ruler(res)
          } else {
            ruler(NULL)
          }
        }

        if (input$hotkeys %in% c("w", "s")) {
          zoom_range <- input$zoom_time
          if (zoom_range[1] >= zoom_range[2] || duration_val() <= 1) {
            return()
          }
          if (input$hotkeys == "w") {
            current_width <- diff(zoom_range)
            padding <- current_width * 0.25 # Zoom in: 25% padding
            new_zoom <- c(
              min(zoom_range[1] + padding, duration_val()),
              max(zoom_range[2] - padding, 0)
            )
          } else {
            current_width <- diff(zoom_range)
            padding <- current_width * 0.5 # Zoom out: 50% padding
            new_zoom <- c(
              max(zoom_range[1] - padding, 0),
              min(zoom_range[2] + padding, duration_val())
            )
            if (diff(new_zoom) > 60) {
              new_zoom[2] <- min(new_zoom[1] + 60, duration_val())
            }
          }
          if (new_zoom[1] < new_zoom[2]) {
            updateNoUiSliderInput(session, inputId = "zoom_time", value = new_zoom)
          }
        }

        if (input$hotkeys == "alt+s") {
          req(duration_val())
          updateNoUiSliderInput(
            session,
            inputId = "zoom_time", value = c(0, min(60, duration_val()))
          )
        }

        if (input$hotkeys == "alt+w") {
          req(duration_val(), rec_soundscape())
          updateNoUiSliderInput(
            session,
            inputId = "zoom_time", value = c(0, min(60, duration_val()))
          )
          updateNoUiSliderInput(
            session,
            inputId = "zoom_freq",
            value = c(0, (rec_soundscape()@samp.rate / 2000) - 1)
          )
        }

        if (input$hotkeys == "d") {
          zoom_range <- input$zoom_time
          window_width <- diff(zoom_range)
          step_size <- window_width / 2
          is_at_end <- zoom_range[2] >= duration_val() &&
            zoom_range[1] >= (duration_val() - window_width)
          new_zoom <- if (is_at_end) {
            c(duration_val() - window_width, duration_val())
          } else {
            pmin(zoom_range + step_size, duration_val())
          }
          updateNoUiSliderInput(session, inputId = "zoom_time", value = new_zoom)
        }

        if (input$hotkeys == "a") {
          zoom_range <- input$zoom_time
          window_width <- diff(zoom_range)
          step_size <- window_width / 2
          is_at_start <- zoom_range[1] <= 0
          new_zoom <- if (is_at_start) {
            c(0, window_width)
          } else {
            pmax(zoom_range - step_size, 0)
          }
          updateNoUiSliderInput(session, inputId = "zoom_time", value = new_zoom)
        }

        # todo - add delay to avoid saving rois and rendering spectrograms mid navigation
        if (input$hotkeys == "z") {
          navigate_soundscape("prev")
        } else if (input$hotkeys == "c") {
          navigate_soundscape("next")
        }

        if (input$hotkeys == "ctrl+e") {
          save_roi_table()
        }

        # Handle hotkeys (1 and space)
        if (input$hotkeys == "space") {
          play_audio()
        }
      })

      # Handle play button
      observeEvent(input$play_soundscape, {
        play_audio()
      })

      progress_tracker <- reactiveValues(df = NULL)
      observe({
        req(soundscape_data())
        progress_tracker$df <- soundscape_data()
      })

      observeEvent(input$export_new_roi_table, {
        req(roi_values())
        if (!all(is.na(roi_values())) & nrow(roi_values()) > 0) {
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

      observeEvent(input$save_roi, save_roi_table())
      observeEvent(input$prev_soundscape, navigate_soundscape("prev"))
      observeEvent(input$next_soundscape, navigate_soundscape("next"))
      observeEvent(input$next_soundscape_noroi, navigate_unsegmented("next"))
      observeEvent(input$prev_soundscape_noroi, navigate_unsegmented("prev"))

      observeEvent(input$no_soi, {
        showModal(modalDialog(
          title = "Confirm Action",
          "Are you sure you want to mark this recording as having no signals of interest? This action will erase all ROIs in the active soundscape.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_no_soi", "Proceed",
              style = "color: #fff; background-color: #b73333; border-color: #2e6da4"
            )
          )
        ))
      })

      observeEvent(input$confirm_no_soi, {
        req(
          wav_path_val(), user_val(), rec_soundscape(),
          soundscape_data()
        )
        roi_i <- tibble::tibble(
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
        save_roi_table()
        showNotification(
          "Recording marked as having no signals of interest",
          type = "message"
        )
        navigate_soundscape("next")
        removeModal()
      })

      # Spectrogram ----------------------------------------------------------------

      # Helper function to extract acoustic measurements from a selection
      extract_acoustic_measurements <- function(rec, ruler_data, wl, ovlp) {
        if (is.null(rec) || is.null(ruler_data)) {
          return(NULL)
        }
        snd_selection <- seewave::cutw(
          rec,
          f = rec@samp.rate, from = ruler_data$roi_start, to = ruler_data$roi_end,
          units = "seconds", output = "Wave"
        )
        dom_freq <- mean(
          seewave::dfreq(
            snd_selection,
            f = snd_selection@samp.rate, wl = wl, ovlp = ovlp,
            bandpass = c(ruler_data$roi_min_freq, ruler_data$roi_max_freq) * 1000,
            plot = FALSE
          )[, 2]
        )
        ac_stats <- seewave::acoustat(
          snd_selection,
          f = snd_selection@samp.rate, wl = wl, ovlp = ovlp, fraction = 80,
          flim = c(ruler_data$roi_min_freq, ruler_data$roi_max_freq),
          plot = FALSE
        )
        list(
          dom_freq = dom_freq,
          ac_stats = ac_stats[-c(1, 2)] %>%
            as.data.frame() %>%
            transmute(t10 = time.P1, t90 = time.P2, f10 = freq.P1, f90 = freq.P2)
        )
      }

      # # Consolidated reactive object for spectrogram parameters
      # spectro_params <- reactive({
      #   list(
      #     rec = rec_soundscape(),
      #     wl = input$wl[1],
      #     ovlp = input$ovlp[1],
      #     color_scale = input$color_scale[1],
      #     dyn_range = input$dyn_range,
      #     pitch_shift = input$pitch_shift[1],
      #     time_guide_interval = input$time_guide_interval[1],
      #     freq_guide_interval = input$freq_guide_interval[1],
      #     zoom_freq = sort(input$zoom_freq),
      #     zoom_time = input$zoom_time,
      #     show_label = input$show_label,
      #     label_angle = input$label_angle,
      #     soundscape_file = input$soundscape_file,
      #     sample_rate = if (!is.null(rec_soundscape())) rec_soundscape()@samp.rate else NULL,
      #     selection_color = ifelse(
      #       input$color_scale %in% c("greyscale 1", "greyscale 2"),
      #       "black", "white"
      #     )
      #   )
      # })

      # Generate spectrogram data only when core parameters change
      spectro_soundscape_raw <- reactive({
        req(
          rec_soundscape(), input$wl, input$ovlp, input$color_scale,
          length(input$dyn_range) == 2, length(input$pitch_shift) == 1,
          length(input$time_guide_interval) == 1,
          length(input$freq_guide_interval) == 1
        )
        fast_spectro(
          rec_soundscape(),
          f = rec_soundscape()@samp.rate,
          ovlp = input$ovlp, wl = input$wl, dyn_range = input$dyn_range,
          pitch_shift = input$pitch_shift, color_scale = input$color_scale,
          ncolors = 124, norm = FALSE,
          time_guide_interval = input$time_guide_interval,
          freq_guide_interval = input$freq_guide_interval
        )
      })

      # Render the plot with all parameters
      output$spectrogram_plot <- renderPlot(execOnResize = TRUE, {
        req(
          input$zoom_freq, input$zoom_time, roi_values(),
          spectro_soundscape_raw(), rec_soundscape(), duration_val()
        )
        zoom_freq <- input$zoom_freq
        if (zoom_freq[1] >= zoom_freq[2]) {
          zoom_freq[2] <- zoom_freq[1] + 1
        }

        # Build base plot
        spectro_plot <- spectro_soundscape_raw() +
          coord_cartesian(xlim = input$zoom_time, ylim = zoom_freq) +
          annotate(
            "label",
            label = paste0(
              input$soundscape_file, " (sr = ", input$sample_rate,
              "; wl = ", input$wl, "; ovlp = ", input$ovlp,
              "; pitch_shift = ", input$pitch_shift, ")"
            ),
            x = -Inf, y = Inf, hjust = 0, vjust = 1, color = "white",
            fill = "black"
          ) +
          labs(x = "Time (s)", y = "Frequency (kHz)") +
          theme(legend.position = "none")

        # Add ROIs if available
        rois_to_plot <- roi_values() |>
          dplyr::mutate(id = row_number()) |>
          dplyr::filter(
            roi_start < input$zoom_time[2] & roi_end > input$zoom_time[1]
          )

        # Prepare ROI elements if available
        if (nrow(rois_to_plot) > 0 &&
          identical(unique(rois_to_plot$soundscape_file), input$soundscape_file)) {

          selection_color <- ifelse(
            input$color_scale %in% c("greyscale 1", "greyscale 2"),
            "black", "white"
          )
          roi_elements <- list(
            # Base ROI rectangles (always added first)
            annotate(
              "rect",
              alpha = 0.05,
              linewidth = 0.3,
              linetype = "dashed",
              fill = rep(selection_color, nrow(rois_to_plot)),
              color = rep(selection_color, nrow(rois_to_plot)),
              xmin = rois_to_plot$roi_start,
              xmax = rois_to_plot$roi_end,
              ymin = rois_to_plot$roi_min_freq,
              ymax = rois_to_plot$roi_max_freq
            )
          )

          # Add highlighted selections if any
          selected_rows <- input$res_table_rows_selected
          if (!is.null(selected_rows) && length(selected_rows) > 0) {
            roi_elements <- c(roi_elements, list(
              annotate(
                "rect",
                alpha = 0.2,
                linewidth = 0.5,
                linetype = "solid",
                fill = selection_color,
                color = selection_color,
                xmin = rois_to_plot$roi_start[selected_rows],
                xmax = rois_to_plot$roi_end[selected_rows],
                ymin = rois_to_plot$roi_min_freq[selected_rows],
                ymax = rois_to_plot$roi_max_freq[selected_rows]
              )
            ))
          }

          # Add labels if enabled
          if (isTRUE(input$show_label)) {
            roi_elements <- c(roi_elements, list(
              annotate(
                "text",
                alpha = 1,
                vjust = "inward",
                hjust = "inward",
                angle = input$label_angle,
                color = selection_color,
                x = rois_to_plot$roi_start,
                y = rois_to_plot$roi_max_freq,
                label = paste0("(", rois_to_plot$id, ") ", rois_to_plot$roi_label),
                na.rm = TRUE
              )
            ))
          }

          # Add all ROI elements in a single operation
          spectro_plot <- spectro_plot + roi_elements
        }

        # Add ruler and measurements if available
        if (!is.null(ruler())) {
          spectro_plot <- spectro_plot +
            geom_rect(
              data = ruler(),
              aes(
                xmin = roi_start, xmax = roi_end, ymin = roi_min_freq,
                ymax = roi_max_freq
              ),
              fill = NA, color = "yellow", linetype = "dashed"
            )

          tryCatch(
            {
              measurements <- extract_acoustic_measurements(
                rec = rec_soundscape(), ruler_data = ruler(), wl = input$wl,
                ovlp = input$ovlp
              )

              if (!is.null(measurements)) {
                measurement_text <- paste0(
                  "ROI Measurements\n",
                  "Time Parameters\n",
                  sprintf("%-12s %8.3f s\n", "Start:", ruler()$roi_start),
                  sprintf("%-12s %8.3f s\n", "End:", ruler()$roi_end),
                  sprintf("%-12s %8.3f s\n", "Duration:", ruler()$roi_duration),
                  sprintf("%-12s %8.3f s\n", "T10-T90:", with(measurements$ac_stats, t90 - t10)),
                  "\nFrequency Parameters\n",
                  sprintf("%-10s %8.3f kHz\n", "Min Freq:", ruler()$roi_min_freq),
                  sprintf("%-10s %8.3f kHz\n", "Max Freq:", ruler()$roi_max_freq),
                  sprintf("%-10s %8.3f kHz\n", "Bandwidth:", ruler()$roi_bandwidth),
                  sprintf("%-10s %8.3f kHz\n", "F10-F90:", with(measurements$ac_stats, f90 - f10)),
                  sprintf("%-10s %8.3f kHz\n", "Dom. Freq:", measurements$dom_freq)
                )

                spectro_plot <- spectro_plot +
                  annotate(
                    "label",
                    label = measurement_text, x = Inf, y = -Inf,
                    hjust = 1, vjust = 0, color = "yellow", fill = "black",
                    alpha = 0.8, label.padding = unit(0.8, "lines"),
                    label.size = 0.6, size = 5, family = "mono"
                  )
              }
            },
            error = function(e) {
              warning("Error calculating measurements: ", e$message)
            }
          )
        }

        return(spectro_plot)
      })

      output$res_table <- renderDT(
        {
          req(roi_values())
          roi_df <- roi_values()
          if (nrow(roi_df) == 0) {
            return(data.frame())
          }
          roi_df[, c(
            "roi_label", "roi_start", "roi_end", "roi_min_freq", "roi_max_freq",
            "roi_type", "roi_label_confidence", "roi_is_complete", "roi_comment"
          )]
        },
        server = TRUE,
        editable = TRUE,  # Enable cell editing
        options = list(
          lengthChange = FALSE,
          pageLength = 20,
          scrollX = TRUE
        )
      )

      observeEvent(input$res_table_cell_edit, {
        req(roi_values(), roi_tables_path_val(), input$roi_table_name)
        info <- input$res_table_cell_edit
        df <- isolate(roi_values())  # Isolate to prevent reactive chain

        # Ensure we have valid edit information
        if (!is.null(info$row) && !is.null(info$col) && !is.null(info$value)) {
          # Get column name instead of index
          cols <- c("roi_label", "roi_start", "roi_end", "roi_min_freq", "roi_max_freq",
                   "roi_type", "roi_label_confidence", "roi_is_complete", "roi_comment")
          col_name <- cols[info$col]

          # Update the specific cell with proper type conversion
          df[[col_name]][info$row] <- info$value

          # Update the reactive value
          roi_values(df)

          # Handle autosave if enabled
          if (isTRUE(input$nav_autosave)) {
            tryCatch({
              data.table::fwrite(
                df,
                file.path(roi_tables_path_val(), input$roi_table_name),
                row.names = FALSE
              )
            }, error = function(e) {
              showNotification(
                paste("Error saving table:", e$message),
                type = "error"
              )
            })
          }
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
        req(roi_values(), cuts_path_val())
        if (!all(is.na(roi_values())) & nrow(roi_values()) > 0) {
          df <- roi_values()
          if (!is.null(input$res_table_rows_selected)) {
            df <- df[input$res_table_rows_selected, ]
          }
          export_roi_cuts_n(rois_n = df, path = cuts_path_val())
          showNotification("Cuts sucessfully exported!", type = "message")
        }
      })

      preset_path_load <- reactive(input$preset_path_load)

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
        updateSliderInput(session, inputId = "time_guide_interval", value = session_data$time_guide_interval)
        updateSliderInput(session, inputId = "freq_guide_interval", value = session_data$freq_guide_interval)
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
            dplyr::filter(roi_table_name == input$roi_table_name) %>%
            dplyr::pull(roi_table_path)
          if (file.exists(active_roi_table_path)) {
            saved_rois <- data.table::fread(file = active_roi_table_path) %>%
              as.data.frame() %>%
              dplyr::mutate(
                roi_input_timestamp = format(roi_input_timestamp, "%Y-%m-%d %H:%M:%S")
              )
            nrow_unsaved <- nrow(
              dplyr::anti_join(
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
        showModal(
          modalDialog(
            title = "Check out", message_rois,
            footer = tagList(actionButton("confirm_exit", "End session")),
            easyClose = TRUE
          )
        )
      })

      # Add safe cleanup function
      safe_cleanup_temp_files <- function(temp_path, current_file = NULL) {
        if (is.null(temp_path) || !dir.exists(temp_path)) {
          return(invisible(NULL))
        }
        temp_files <- list.files(
          path = temp_path, pattern = "\\.wav$", full.names = TRUE
        )
        for (file in temp_files) {
          if (!identical(file, current_file) && file.exists(file)) {
            try(file.remove(file), silent = TRUE)
          }
        }
        invisible(NULL)
      }

      # Handle manual exit
      observeEvent(input$confirm_exit, {
        tryCatch(
          {
            safe_cleanup_temp_files(session_data$temp_path)
          },
          error = function(e) {
            warning("Error cleaning temporary files: ", e$message)
          }
        )
        stopApp()
      })

      # Handle session end
      session$onSessionEnded(function() {
        safe_cleanup_temp_files(session_data$temp_path)
      })

      # teste_val <- reactiveVal(NULL)
      # output$checagem1 <- renderPrint({
      #   req(duration_val())
      #   duration_val()
      # })

      # General popover options
      pop_up_opt <- list(delay = list(show = 1000, hide = 0))

      # Define tooltips configuration
      tooltips <- list(
        # User Setup
        user = "Format: 'FirstName I. S.' (no commas)",
        soundscapes_path = "Root folder containing soundscape files",
        roi_tables_path = "Output folder for ROI tables",
        cuts_path = "Output folder for audio cuts and spectrograms",
        user_setup_confirm = "Confirm settings to begin",

        # Session Setup
        label_angle = "Label rotation angle (90 degrees recommended)",
        wl = "FFT window length (affects time/frequency resolution)",
        ovlp = "Window overlap % (higher = better resolution)",
        pitch_shift = "Adjust playback pitch for ultrasound",
        dyn_range = "Amplitude range shown in spectrogram",
        show_label = "Toggle ROI labels visibility",
        time_guide_interval = "Time guide interval (s)",
        freq_guide_interval = "Frequency guide interval (kHz)",
        color_scale = "Spectrogram color scheme",
        wav_player_type = "Audio playback method",
        wav_player_path = "External player executable path",
        default_pars = "Reset to default settings",

        # Spectrogram Controls
        zoom_freq = "Adjust visible frequency range",
        zoom_time = "Adjust visible time range",

        # Navigation
        prev_soundscape_noroi = "Previous file",
        prev_soundscape = "Previous unprocessed file",
        play_soundscape = "Play visible section",
        next_soundscape = "Next file",
        next_soundscape_noroi = "Next unprocessed file",
        no_soi = "Mark the whole spectrogram as No signals of interest (erase all ROIs)",

        # ROI Input
        soundscape_file = "Select current soundscape",
        roi_table_name = "Select ROI table",
        sp_list = "Choose label list",
        label_name = "Name for next ROI",
        signal_type = "Type of sound",
        label_certainty = "Confidence in identification",
        lock_label_certainty = "Lock label certainty value",
        signal_is_complete = "Signal fully captured in ROI",
        lock_is_complete = "Lock signal completeness value",
        label_comment = "Notes (use _ or ; as separators)",
        lock_comment = "Lock label comment",

        # ROI Actions
        save_roi = "Save current ROI table",
        export_new_roi_table = "Create new ROI table",
        export_selected_cut = "Export selected ROI audio",
        delete_selected_rois = "Remove selected ROIs"
      )

      # Apply tooltips
      for (id in names(tooltips)) {
        addTooltip(
          session,
          id = id,
          title = tooltips[[id]],
          placement = if (id %in% c(
            "zoom_time", "soundscape_file", "roi_table_name",
            "sp_list", "label_name", "signal_type",
            "label_certainty", "signal_is_complete",
            "label_comment", "save_roi", "export_new_roi_table",
            "export_selected_cut", "delete_selected_rois",
            "prev_soundscape_noroi", "prev_soundscape",
            "play_soundscape", "next_soundscape",
            "next_soundscape_noroi"
          )) {
            "bottom"
          } else {
            "right"
          },
          trigger = "hover",
          options = pop_up_opt
        )
      }
    }
  )
}
