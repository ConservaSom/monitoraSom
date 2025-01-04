#' Launch validation app
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function launches the validation app, which is a Shiny app to validate
#'   for segemntation of WAV recorcings of soundscapes into tables containing
#'   regions of interest (ROIs) and audio cuts of the ROIs.
#'
#' @param project_path Path to the project folder.
#' @param preset_path Path from which presets can be imported and to which new
#'   presets can be exported.
#' @param validation_user User name.
#' @param templates_path Path to the template wave files.
#' @param soundscapes_path Path to the soundscape wave files.
#' @param input_path Path to the input file containing detections.
#' @param output_path Path to the output file.
#' @param wav_cuts_path Path to the folder containing the cut wave files.
#' @param spec_path Path to the folder containing the spectrogram images.
#' @param wav_player_path Path to the wav player executable (only for system
#'   player).
#' @param wav_player_type The type of wav player. "R session" for R
#'   session-based player, "system" for system player.
#' @param val_subset Subset of detections to be validated.
#' @param time_pads Time pads to be added to the start and end of the cut wave
#'   files.
#' @param ovlp Overlap between consecutive cuts.
#' @param wl Window length for the spectrogram.
#' @param dyn_range_templ Dynamic range for the template spectrogram.
#' @param dyn_range_detec Dynamic range for the detection spectrogram.
#' @param dyn_range_bar Adjustment of the maximum range of the dynamic range
#'   slider.
#' @param color_scale Color scale for the spectrogram.
#' @param zoom_freq Frequency range to zoom in the spectrogram.
#' @param nav_shuffle If TRUE, the files will be shuffled before navigation.
#' @param subset_seed Seed for the random shuffling.
#' @param auto_next If TRUE, the next file is automatically displayed when the
#'   user validates a cut.
#' @param nav_autosave If TRUE, the current validation is saved when the user
#'   navigates to another file.
#' @param overwrite If TRUE, the output file is overwritten.
#' @param pitch_shift Pitch shift for the audio cuts.
#' @param visible_bp If TRUE, the bandpass filter is visible in the spectrogram.
#' @param play_norm If TRUE, the played audio is normalized.
#' @param time_guide_interval A numeric value indicating the interval in seconds
#'   between time guides in the spectrogram.
#' @param freq_guide_interval A numeric value indicating the interval in kHz
#'   between frequency guides in the spectrogram.
#'
#' @return todo
#'
#' @export
#' @import shiny dplyr tidyr ggplot2 stringr DT shinyWidgets shinydashboard keys
#'    shinyjs shinyBS cutpointr
#' @importFrom caret downSample upSample
#' @importFrom ROSE ROSE
#' @importFrom data.table fread fwrite
#' @importFrom cowplot plot_grid
#' @importFrom tuneR readWave normalize setWavPlayer play
#' @importFrom seewave ffilter cutw duration addsilw ffilter fir
#' @importFrom DT dataTableOutput renderDataTable
#' @importFrom shinyjs show
#' @importFrom shinyWidgets alert
#' @examples
#' \dontrun{
#' library(monitoraSom)
#' library(usethis)
#'
#' # set the path to the diorectory where soundscapes are located (it is not
#' # recursive)
#' soundscapes_path <- "path/to/soundscapes"
#' # set the path to the project folder
#' project_path <- "path/to/project"
#'
#' # in case the current working directory is not an active project, set it with
#' # the following command. it may be necessary to restart the R session after that.
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
    output_path = NULL, spec_path = NULL, wav_cuts_path = NULL,
    wav_player_path = "play", wav_player_type = "HTML player",
    val_subset = c("NV", "TP", "FP", "UN"),
    time_pads = 1, ovlp = 0, wl = 2048, dyn_range_bar = c(-144, 0),
    dyn_range_templ = c(-84, 0), dyn_range_detec = c(-84, 0),
    color_scale = "inferno", zoom_freq = c(0, 23),
    time_guide_interval = 1, freq_guide_interval = 1,
    nav_shuffle = FALSE, subset_seed = 123, auto_next = TRUE, nav_autosave = TRUE,
    overwrite = FALSE, pitch_shift = 1, visible_bp = FALSE, play_norm = FALSE) {
  library(dplyr, warn.conflicts = FALSE)
  options(dplyr.summarise.inform = FALSE)

  # requireNamespace("tidyr")
  # requireNamespace("dplyr")
  # requireNamespace("ggplot2")
  # requireNamespace("lubridate")
  # requireNamespace("seewave")
  # requireNamespace("stringr")
  # requireNamespace("tuneR")
  # requireNamespace("purrr")
  # requireNamespace("DT")
  # requireNamespace("data.table")
  # requireNamespace("cutpointr")
  # requireNamespace("caret")
  # requireNamespace("ROSE")
  # requireNamespace("viridis")
  # requireNamespace("farver")
  # requireNamespace("shiny")
  # requireNamespace("shinyWidgets")
  # requireNamespace("shinyjs")
  # requireNamespace("keys")
  # requireNamespace("shinydashboard")
  # requireNamespace("shinyBS")



  # input validation -----------------------------------------------------------

  session_data <- list()

  if (!is.null(project_path)) {
    tryCatch(
      {
        if (!dir.exists(project_path)) {
          dir.create(project_path)
          warning("The validation app project directory was successfully created at '", project_path, "'")
        }
        session_data$project_path <- project_path
      },
      error = function(e) {
        stop("Failed to create validation app project directory at '", project_path, "': ", e$message)
      }
    )
  }

  if (!is.null(preset_path)) {
    if (!dir.exists(preset_path)) {
      dir.create(preset_path)
      if (!dir.exists(preset_path)) {
        stop("Error! The selected preset destination folder does not exist and could not be created.")
      }
      warning("The segmentation preset destination directory was created automatically at '", preset_path, "'")
    }
    session_data$preset_path <- preset_path

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

  # Validate and set the validation user
  if (!is.null(validation_user)) {
    session_data$validation_user <- as.character(gsub(",", "", validation_user))
  } else {
    session_data$validation_user <- NA_character_
    warning("Warning! A value was not provided for 'validation_user' variable. Inform the correct value and confirm within the app.")
  }

  # Validate and set the templates path
  if (is.null(templates_path)) {
    templates_path <- "roi_cuts/" # Default path
    if (dir.exists(templates_path)) {
      session_data$templates_path <- templates_path
      warning("Warning! The path to the template wave files was not provided. Using the default path 'roi_cuts/'.")
    } else {
      stop("Error! The default path 'roi_cuts/' does not exist.")
    }
  } else if (dir.exists(templates_path)) {
    session_data$templates_path <- templates_path
  } else {
    stop("Error! The provided path to the template wave files was not found locally.")
  }

  # Validate and set the soundscapes path
  session_data$soundscapes_path <- soundscapes_path
  if (!dir.exists(soundscapes_path)) {
    stop("Error! The path to the soundscape wave files was not found locally.")
  }

  # Validate and set the input path
  session_data$input_path <- input_path
  if (!file.exists(input_path) || tools::file_ext(input_path) != "csv" || file.size(input_path) == 0) {
    stop("Error! The input file must exist, be a CSV file, and not be empty.")
  }

  session_data$output_path <- if (is.null(output_path)) {
    warning("Warning! The output file was not provided. Using the input file as output.")
    input_path
  } else {
    if (!file.exists(output_path)) {
      warning("Warning! The output file was not found locally. It will be created automatically.")
    }
    output_path
  }

  if (all(val_subset %in% c("NV", "TP", "FP", "UN"))) {
    session_data$val_subset <- val_subset
  } else {
    stop(
      "Error! At least one of the values assigned to 'val_subset' are not within the accepted alternatives ('NV', 'TP', 'FP', 'UN')."
    )
  }

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

  if (!is.numeric(time_pads) || time_pads < 0 || time_pads > 16) {
    stop(
      "Error! The value assigned to 'time_pads' must be a numeric value between 0 and 16."
    )
  }
  session_data$time_pads <- time_pads

  # Function to validate dynamic range vectors
  validate_dyn_range <- function(dyn_range, name) {
    if (length(dyn_range) != 2 || !all(is.numeric(dyn_range))) {
      stop(
        paste(
          "Error! '", name,
          "' must be a numeric vector of length 2 with all values numeric.",
          sep = ""
        )
      )
    }
    if (dyn_range[1] >= dyn_range[2]) {
      warning(
        paste("Warning! The first value of '", name,
          "' must be smaller than the second. Sorting to match the expected order.",
          sep = ""
        )
      )
      return(sort(dyn_range))
    }
    return(dyn_range)
  }
  # Validate and set dynamic ranges
  session_data$dyn_range_templ <- validate_dyn_range(
    dyn_range_templ, "dyn_range_templ"
  )
  session_data$dyn_range_detec <- validate_dyn_range(
    dyn_range_detec, "dyn_range_detec"
  )
  session_data$dyn_range_bar <- validate_dyn_range(
    dyn_range_bar, "dyn_range_bar"
  )

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
    stop(
      "Error! The value assigned to 'ovlp' must be a numeric value between 0 and 80, in steps of 10."
    )
  }
  session_data$ovlp <- ovlp

  # Validate 'color_scale' against expected values
  valid_color_scales <- c(
    "viridis", "magma", "inferno", "cividis", "greyscale 1", "greyscale 2"
  )
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

  if (any(zoom_freq < 0) || any(zoom_freq > 192)) {
    stop("Error! 'zoom_freq' values must be between 0 and 192.")
  }

  if (zoom_freq[1] >= zoom_freq[2]) {
    session_data$zoom_freq <- sort(zoom_freq)
    warning(
      "Warning! The first value of 'zoom_freq' must be smaller than the second. Sorting to match the expected order."
    )
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

  if (!is.numeric(subset_seed)) {
    stop("Error! Non-numeric value input provided to 'seed'")
  }
  session_data$subset_seed <- subset_seed

  # Function to validate logical values and assign them to session_data
  validate_logical_and_assign <- function(value, name) {
    if (!is.logical(value)) {
      stop(
        paste(
          "Error! The value assigned to '", name,
          "' is not logical. Set it to TRUE or FALSE.",
          sep = ""
      ))
    }
    session_data[[name]] <- value
  }
  validate_logical_and_assign(visible_bp, "visible_bp")
  validate_logical_and_assign(play_norm, "play_norm")
  validate_logical_and_assign(nav_shuffle, "nav_shuffle")
  validate_logical_and_assign(auto_next, "auto_next")
  validate_logical_and_assign(nav_autosave, "nav_autosave")
  validate_logical_and_assign(overwrite, "overwrite")

  # Function to validate and set paths
  validate_and_set_path <- function(path, default_path, session_key) {
    if (is.null(path)) {
      if (dir.exists(default_path)) {
        warning(paste("Warning! The informed '", session_key,
          "' was not found locally. Using the default path '", default_path, "'.",
          sep = ""
        ))
      } else {
        dir.create(default_path, recursive = TRUE)
        warning(paste("Warning! The informed '", session_key,
          "' was not found locally. Using the default path '", default_path, "'.",
          sep = ""
        ))
      }
      return(default_path)
    } else {
      if (dir.exists(path)) {
        return(path)
      } else {
        stop(paste("Error! The provided path to store ", session_key,
          " was not found locally.",
          sep = ""
        ))
      }
    }
  }
  session_data$wav_cuts_path <- validate_and_set_path(
    wav_cuts_path, "detection_cuts/", "wav_cuts_path"
  )
  session_data$spec_path <- validate_and_set_path(
    spec_path, "detection_spectrograms/", "spec_path"
  )

  if (!is.numeric(pitch_shift) || !(pitch_shift %in% c(-8, -6, -4, -2, 1))) {
    stop(
      "Error! The value assigned to 'pitch_shift' is not numeric or not among the expected alternatives: -8, -6, -4, -2, or 1."
    )
  } else {
    session_data$pitch_shift <- pitch_shift
  }

  auc_trap <- function(x, y) {
    res <- sum(
      (
        rowMeans(cbind(y[-length(y)], y[-1]))) * (x[-1] - x[-length(x)]
      )
    )
    return(res)
  }

  # hotkeys --------------------------------------------------------------------

  hotkeys <- c(
    "q", #
    "w", #
    "e", #
    "r", #
    "t", #
    # "y", #
    "a", #
    "s", #
    "d", #
    "1", #
    "2" #
  )

  # resource paths -------------------------------------------------------------

  # This block defines where embedded html wav players will look for the files
  shiny::addResourcePath("audio", temp_path)
  # resourcePaths()
  # todo Clear temp folder when closing the app

  # app ------------------------------------------------------------------------

  shinyApp(

    # UI -----------------------------------------------------------------------
    ui = dashboardPage(
      header = dashboardHeader(title = "MonitoraSom", titleWidth = "400px"),

      # Sidebar ----------------------------------------------------------------
      sidebar = dashboardSidebar(
        tags$head(tags$style(HTML(".form-group { margin-bottom: 10px !important; }"))),
        width = "400px",
        # Pop up control. Credits to: soundgen::annotation_app()
        sidebarMenu(

          # User setup ---------------------------------------------------------
          menuItem(
            "User setup",
            tabName = "user_setup_tab", startExpanded = TRUE,
            icon = icon(lib = "glyphicon", "glyphicon glyphicon-user"),
            textAreaInput(
              inputId = "preset_path", label = "Path to preset files (.rds)",
              value = session_data$preset_path,
              placeholder = "Paste or load path here",
              height = "40px", width = "395px", resize = "vertical"
            ),
            textInput("validation_user", "User name (*):",
              value = session_data$validation_user,
              placeholder = "Identify yourself here", width = "100%"
            ),
            textAreaInput("templates_path", "Templates path (*)",
              value = session_data$templates_path,
              placeholder = "Paste or load path here",
              height = "40px", resize = "vertical", width = "100%"
            ),
            textAreaInput("soundscapes_path", "Soundscapes path (*)",
              value = session_data$soundscapes_path,
              placeholder = "Paste or load path here",
              height = "40px", width = "395px", resize = "vertical"
            ),
            textAreaInput("input_path", "Detections table path (input) (*)",
              value = session_data$input_path,
              placeholder = "Paste or load path here",
              height = "40px", width = "395px", resize = "vertical"
            ),
            textAreaInput("output_path", "Validated data path (output) (*)",
              value = session_data$output_path,
              placeholder = "Paste or load path here",
              height = "40px", width = "395px", resize = "vertical"
            ),
            actionButton("user_setup_confirm", "Confirm Paths",
              icon = icon(lib = "glyphicon", "glyphicon glyphicon-check"),
              style = "color: #000000; background-color: #33b733; border-color: #288d28; width: 360px;"
            ),
            shinyBS::bsTooltip("user_setup_confirm",
              title = "<b>Part 1 of 2 required to start the session</b>. All inputs marked with (*) are required for this step",
              placement = "right", trigger = "hover",
              options = list(delay = list(show = 1000, hide = 0))
            ),
            tags$style(".tooltip {width: 300px;}")
          ),

          # Session setup ------------------------------------------------------

          menuItem(
            "Session Setup",
            icon = icon(lib = "glyphicon", "glyphicon glyphicon-check"),
            tabName = "sect_setup_tab",
            selectInput(
              "template_name", "Template file (*)",
              choices = NULL, width = "100%"
            ),
            selectizeInput(
              "val_subset", "Filter validation inputs (*)",
              choices = c(
                "True positives - TP" = "TP", "False positives - FP" = "FP",
                "Unknown - UN" = "UN", "Not validated - NV" = "NV"
              ),
              selected = session_data$val_subset, multiple = TRUE, width = "100%"
            ),
            # convert in an interval
            sliderInput(
              "score_interval", "Score interval (*)",
              width = "100%", min = -1, max = 1, step = 0.01,
              value = c(0, 1)
            ),
            # top n detections
            splitLayout(
              cellWidths = c("50%", "50%"),
              textInput("top_n_detecs", "Top detections", value = 0, width = "100%"),
              checkboxInput(
                "top_by_file", "Search top scores within soundscape files",
                value = FALSE, width = "100%"
              )
            ),
            # arrange the order of validation per score
            selectInput(
              "order_by", "Order by",
              choices = c(
                "Original file order",
                "Soundscape file name (ASC)",
                "Soundscape file name (DESC)",
                "Score (ASC)",
                "Score (DESC)",
                "Soundscape file name (ASC) and Score (ASC)",
                "Soundscape file name (ASC) and Score (DESC)",
                "Soundscape file name (DESC) and Score (ASC)",
                "Soundscape file name (DESC) and Score (DESC)",
                "Score (ASC) and Soundscape file name (ASC)",
                "Score (ASC) and Soundscape file name (DESC)",
                "Score (DESC) and Soundscape file name (ASC)",
                "Score (DESC) and Soundscape file name (DESC)",
                "Random"
              ),
              selected = "Score", width = "100%"
            ),
            numericInput(
              "subset_seed", "Seed for random subsetting",
              value = session_data$subset_seed
            ),
            actionButton(
              "confirm_session_setup", "Confirm validation setup",
              icon = icon(lib = "glyphicon", "glyphicon glyphicon-check"),
              style = "color: #000000; background-color: #33b733; border-color: #288d28; width: 360px;"
            )
          ),

          # Spectrogram parameters -----------------------------------------------

          menuItem(
            "Spectrogram Parameters",
            tabName = "spec_par_tab",
            icon = icon(lib = "glyphicon", "glyphicon glyphicon-cog"),
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
              "time_pads", "Pad size (s):",
              min = 0, max = 16, width = "100%", step = 0.5,
              value = session_data$time_pads, post = "s"
            ),
            sliderInput(
              "dyn_range_templ", "Template dynamic range (dB)",
              min = session_data$dyn_range_bar[1],
              max = session_data$dyn_range_bar[2],
              value = session_data$dyn_range_templ, step = 6,
              width = "100%", post = "dB"
            ),
            sliderInput(
              "dyn_range_detec", "Detection dynamic range (dB)",
              min = session_data$dyn_range_bar[1],
              max = session_data$dyn_range_bar[2],
              value = session_data$dyn_range_detec, step = 6,
              width = "100%", post = "dB"
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
              value = session_data$ovlp, post = "%"
            ),
            sliderTextInput(
              "pitch_shift", "Pitch shift (octaves) and slow down (factor)",
              choices = c(-8, -6, -4, -2, 1), selected = session_data$pitch_shift,
              grid = TRUE, width = "100%", post = " octaves"
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
            textAreaInput(
              "wav_player_path",
              label = "Path to player executable (default = 'play')",
              height = "40px", resize = "vertical",
              value = session_data$wav_player_path
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
        ),
        actionButton(
          "end_session", "End validation session",
          icon = icon(lib = "glyphicon", "glyphicon glyphicon-log-out"),
          style = "color: #fff; background-color: #b73333; border-color: #8d2c2c; width: 370px;"
        )
      ),

      # Body -------------------------------------------------------------------

      body = dashboardBody(

        # Set up shinyjs
        useShinyjs(),

        # Make keyboard shotcuts available
        useKeys(),
        keysInput("hotkeys", hotkeys),

        # Avoid blinking figures while rendering
        tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),
        tags$head(tags$style(HTML(".content-wrapper { overflow: auto; }"))),

        # box(width = 12, verbatimTextOutput("checagem1")),
        # box(width = 6, verbatimTextOutput("checagem2")),

        # Detection spectrogram --------------------------------------------------

        box(
          width = 6, height = "550px",
          column(
            width = 1,
            noUiSliderInput(
              "zoom_freq", "Frequency zoom",
              min = 0, max = 192, # todo Increase to work with bats
              step = 0.1, direction = "rtl",
              orientation = "vertical", width = "100px", height = "425px",
              value = session_data$zoom_freq
            )
          ),
          column(
            width = 11, offset = 0,
            plotOutput("DetectionSpectrogram", height = "475px"),
            column(
              width = 8,
              tags$div(id = "detection_player"),
              actionButton(
                "play_detec", "Play Detection (1)",
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
        ),

        # Template spectrogram -------------------------------------------------

        box(
          id = "template_box",
          width = 6, height = "550px",
          plotOutput("TemplateSpectrogram", height = "475px"),
          tags$div(id = "template_player"),
          actionButton(
            "play_template", "Play Template (2)",
            icon = icon("play"), width = "50%", style = "height:50px"
          )
        ),

        # Input box ------------------------------------------------------------

        # todo - reorganize the ui within this box
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
            width = 3,
            # disabled( # todo: enable this
            selectizeInput(
              "soundscape_file", "Soundscape file",
              choices = NULL, width = "100%"
            )
            # )
          ),
          column(
            width = 1,
            selectInput("detec", "Detection ID", choices = NULL, width = "100%")
          ),
          column(
            width = 1,
            column(
              width = 12,
              checkboxInput(
                "auto_next", HTML("<b>Autonavigate</b>"),
                value = session_data$auto_next
              ),
              checkboxInput(
                "overwrite", HTML("<b>Overwrite</b>"),
                value = session_data$overwrite
              ),
            )
          ),
          column(
            width = 1,
            column(
              width = 12,
              checkboxInput(
                "nav_autosave", HTML("<b>Autosave</b>"),
                value = session_data$nav_autosave
              )
            )
          ),
          column(
            width = 5,
            textInput(
              "detec_note", "Detection notes",
              value = NA_character_,
              placeholder = "Write detection notes here", width = "100%"
            )
          ),
          column(
            width = 1,
            checkboxInput("lock_detec_note", icon("lock", lib = "font-awesome"), value = FALSE)
          ),
          column(
            width = 11,
            disabled(
              selectInput("custom_reference", "Reference spectrogram", choices = NULL)
            )
          ),
          column(
            width = 1,
            checkboxInput("lock_template", icon("lock", lib = "font-awesome"), value = TRUE)
          )
        ),

        # Outputs --------------------------------------------------------------

        tabBox(
          width = 12, height = "900px",
          id = "tabset1",

          # Progress -----------------------------------------------------------
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

          # Detection Table -----------------------------------------------------

          tabPanel("Detection Table",
            height = "100%",
            DTOutput("res_table")
          ),

          # Soundscape spectrogram -----------------------------------------------

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

          # Export Detection -----------------------------------------------------

          tabPanel(
            "Export Detection",
            column(
              width = 6,
              textInput(
                "wav_cuts_path", "Wave cuts path",
                value = session_data$wav_cuts_path, width = "100%",
                placeholder = "Paste or load here the path to export the wav file"
              ),
              textInput(
                "wav_cut_name", "Wav file name (*.wav)",
                value = NULL, width = "100%",
                placeholder = "Input the file name here"
              ),
              actionButton("reset_wav_cut_name", "Reset filename", width = "100%"),
              actionButton(
                "confirm_wav_export", "Export wav file (r)",
                width = "100%",
                style = "color: #fff; background-color: #33b76e; border-color: #5da42e;"
              )
            ),

            # Export Spectrogram -------------------------------------------------

            column(
              width = 6,
              textInput(
                "spec_path", "Spectrogram cuts path",
                value = session_data$spec_path, width = "100%",
                placeholder = "Paste or load here the path to export the spectrogram"
              ),
              textInput(
                "spec_name", "Spectrogram file name (*.jpeg)",
                value = NULL, width = "100%",
                placeholder = "Input the file name here"
              ),
              actionButton("reset_spec_filename", "Reset filename", width = "100%"),
              actionButton(
                "confirm_spec_export", "Export spectrogram (t)",
                width = "100%",
                style = "color: #fff; background-color: #33b76e; border-color: #5da42e;"
              )
            )
          ),

          # Diagnostics ----------------------------------------------------------

          tabPanel(
            "Diagnostics",
            column(
              width = 4,
              selectInput(
                "diag_balance", "Dataset balance method",
                choices = c(
                  "None", "Downsample larger class", "Upsample smaller  class", "ROSE"
                ),
                selected = "None", width = "100%"
              ),
            ),
            column(width = 4, selectInput(
              "diag_method", "Cutpoint detection method",
              choices = c("Manual", "Error = 0.05", "Error = 0.1"),
              width = "100%"
            )),
            column(width = 4, sliderInput(
              "diag_cut", "Cutpoint threshold",
              min = 0, max = 1, step = 0.001, value = 0.2, width = "100%"
            )),
            column(width = 4, plotOutput("plot_dens", height = "340px")),
            column(width = 4, plotOutput("plot_binomial", height = "340px")),
            # column(width = 3, plotOutput("plot_roc", height = "340px")),
            column(width = 4, plotOutput("plot_prec_rec", height = "340px")),
            tableOutput("cut_i_tab")
          )
        )
      ),
      skin = "black"
    ),

    # Server -------------------------------------------------------------------
    server = function(input, output, session) {
      # Create a reactive object for storing the path
      templates_path <- reactiveVal(NULL)
      # Create a reactive object for storing the path
      soundscapes_path <- reactiveVal(NULL)
      # Create empty reactive object with the full detection dataset
      df_full <- reactiveValues(data = NULL)
      df_output <- reactiveVal(NULL)
      df_ref_templates <- reactiveVal(NULL)


      observeEvent(input$user_setup_confirm, {
        # Initial input validation
        req(
          input$input_path, input$output_path, input$soundscapes_path,
          input$templates_path, input$validation_user
        )

        # Validate paths and files
        validation_errors <- character()

        # Check input file
        if (!file.exists(input$input_path)) {
          validation_errors <- c(validation_errors, "Input file does not exist")
        } else {
          tryCatch(
            {
              test_read <- fread(input$input_path, nrows = 1)
            },
            error = function(e) {
              validation_errors <<- c(
                validation_errors,
                sprintf("Cannot read input file: %s", e$message)
              )
            }
          )
        }

        # Check directories
        paths_to_check <- list(
          "Soundscapes directory" = input$soundscapes_path,
          "Templates directory" = input$templates_path
        )

        for (path_name in names(paths_to_check)) {
          path <- paths_to_check[[path_name]]
          if (!dir.exists(path)) {
            validation_errors <- c(
              validation_errors,
              sprintf("%s does not exist", path_name)
            )
          } else if (length(list.files(path,
            pattern = ".wav$",
            recursive = TRUE, ignore.case = TRUE
          )) == 0) {
            validation_errors <- c(
              validation_errors,
              sprintf("No WAV files found in %s", path_name)
            )
          }
        }

        # Check output path
        output_dir <- dirname(input$output_path)
        if (!dir.exists(output_dir)) {
          validation_errors <- c(
            validation_errors,
            "Output directory does not exist"
          )
        }

        # If there are validation errors, show them and stop
        if (length(validation_errors) > 0) {
          showModal(modalDialog(
            title = "Setup Validation Errors",
            tags$div(
              tags$p("Please correct the following errors:"),
              tags$ul(
                lapply(validation_errors, function(error) tags$li(error))
              )
            ),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return()
        }

        # Show warning about input/output paths
        if (input$input_path == input$output_path) {
          showModal(modalDialog(
            title = "Warning: Input and Output Files Are the Same",
            tags$div(
              tags$p(
                icon("warning"),
                "Risk of overwriting previous validations!"
              ),
              tags$p("Make sure the Overwrite check is not enabled to protect your data.")
            ),
            easyClose = TRUE,
            footer = modalButton("I Understand")
          ))
        } else {
          showModal(modalDialog(
            title = "Different Input and Output Files",
            tags$div(
              tags$p(
                icon("info-circle"),
                "You are creating a new output file."
              ),
              tags$p("If the output file already exists, it may be overwritten. Consider backing up existing data.")
            ),
            easyClose = TRUE,
            footer = modalButton("Proceed")
          ))
        }

        # Safe data loading
        tryCatch(
          {
            df_soundscapes <- data.frame(
              soundscape_path = list.files(
                input$soundscapes_path,
                pattern = ".wav$", recursive = TRUE, full.names = TRUE,
                ignore.case = TRUE
              )
            ) %>%
              dplyr::mutate(soundscape_file = basename(soundscape_path))

            df_templates <- data.frame(
              template_path = list.files(
                input$templates_path,
                pattern = ".wav$", recursive = TRUE, full.names = TRUE,
                ignore.case = TRUE
              )
            ) %>%
              dplyr::mutate(template_file = basename(template_path))
            df_ref_templates(df_templates)

            res <- data.table::fread(input$input_path, data.table = FALSE, header = TRUE) %>%
              dplyr::mutate(
                soundscape_path = as.character(NA),
                template_path = as.character(NA)
              ) %>%
              dplyr::rows_update(df_templates, by = "template_file", unmatched = "ignore") %>%
              dplyr::rows_update(df_soundscapes, by = "soundscape_file", unmatched = "ignore")

            var_names <- c(
              "detection_id", "validation_user", "validation_time", "validation",
              "validation_note"
            )

            # Add variables for review inputs if needed
            if (!all(var_names %in% colnames(res))) {
              res <- res %>%
                dplyr::mutate(
                  detection_id = 1:nrow(.),
                  validation_user = NA_character_,
                  validation_time = NA_character_,
                  validation = "NV",
                  validation_note = NA_character_
                )
            } else if ("validation_time" %in% colnames(res)) {
              res <- res %>%
                dplyr::mutate(
                  validation_time = as.character(validation_time),
                  validation_note = as.character(validation_note)
                )
            }

            updateSelectInput(
              session, "template_name",
              choices = unique(res$template_name)
            )

            # Update progress bar
            updateProgressBar(
              session = session, id = "prog_bar_full",
              value = length(which(res$validation %in% c("TP", "FP", "UN"))),
              total = nrow(res)
            )

            # Update reactive values
            df_full$data <- res
            df_output(res)

            showModal(modalDialog(
              title = "Setup Successful",
              tags$div(
                tags$p(icon("check"), "Paths updated successfully."),
                tags$p("You can now proceed with the validation.")
              ),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          },
          error = function(e) {
            showModal(modalDialog(
              title = "Error Processing Data",
              tags$div(
                tags$p(
                  icon("exclamation-triangle"),
                  "An error occurred while processing the data:"
                ),
                tags$pre(e$message)
              ),
              easyClose = TRUE,
              footer = modalButton("OK")
            ))
          }
        )
      })


      # Alternative version of df_detections_full containing only the
      # samples above the specified threshold to avoid showing soundscapes
      # without detections
      df_cut <- reactiveVal(NULL)
      vec_soundscapes <- reactiveVal(NULL)
      df_template <- reactiveVal(NULL)

      observeEvent(input$confirm_session_setup, {
        req(df_full$data)
        if (!is.null(df_output())) {
          df_full$data <- df_output()
        }
        val_subset <- input$val_subset
        val_subset[val_subset == "NA" & is.na(val_subset)] <- "NV"
        order_options <- list(
          "Original file order" = function(res) res,
          "Random" = function(res, seed = input$subset_seed) {
            set.seed(input$subset_seed)
            res[sample(nrow(res)), ]
          },
          "Soundscape file name (ASC)" =
            function(res) res[order(res$soundscape_file), ],
          "Soundscape file name (DESC)" =
            function(res) res[order(res$soundscape_file, decreasing = TRUE), ],
          "Score (ASC)" =
            function(res) res[order(res$peak_score), ],
          "Score (DESC)" =
            function(res) res[order(res$peak_score, decreasing = TRUE), ],
          "Soundscape file name (ASC) and Score (ASC)" =
            function(res) res[order(res$soundscape_file, res$peak_score), ],
          "Soundscape file name (ASC) and Score (DESC)" =
            function(res) res[order(res$soundscape_file, -res$peak_score), ],
          "Soundscape file name (DESC) and Score (ASC)" =
            function(res) res[order(-res$soundscape_file, res$peak_score), ],
          "Soundscape file name (DESC) and Score (DESC)" =
            function(res) res[order(-res$soundscape_file, -res$peak_score), ],
          "Score (ASC) and Soundscape file name (ASC)" =
            function(res) res[order(res$peak_score, res$soundscape_file), ],
          "Score (ASC) and Soundscape file name (DESC)" =
            function(res) res[order(res$peak_score, -res$soundscape_file), ],
          "Score (DESC) and Soundscape file name (ASC)" =
            function(res) res[order(-res$peak_score, res$soundscape_file), ],
          "Score (DESC) and Soundscape file name (DESC)" =
            function(res) res[order(-res$peak_score, -res$soundscape_file), ]
        )
        top_n_detecs_val <- as.numeric(input$top_n_detecs)

        # Gather the metadata to validate the active template
        res <- df_full$data %>%
          dplyr::filter(template_name == input$template_name) %>%
          dplyr::filter(
            peak_score >= input$score_interval[1] &
              peak_score <= input$score_interval[2]
          ) %>%
          dplyr::filter(validation %in% val_subset) %>%
          {
            if (input$top_by_file == TRUE) {
              dplyr::group_by(., soundscape_file)
            } else {
              .
            }
          } %>%
          {
            if (top_n_detecs_val > 0) {
              dplyr::top_n(., top_n_detecs_val, wt = peak_score)
            } else {
              .
            }
          } %>%
          dplyr::ungroup()

        res <- order_options[[input$order_by]](res)

        # if the filtering process result is not null, get some more information
        if (!is.null(res)) {
          if (nrow(res) == 0) {
            showModal(
              modalDialog(
                title = "No detections found",
                "No detections were found with the provided parameters. Please, review the validation setup.",
                easyClose = TRUE,
                footer = NULL
              )
            )
          } else {
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
        }
        # Reactive object to update info about the active template
        df_template({
          df_cut() %>%
            dplyr::filter(template_name == input$template_name) %>%
            dplyr::slice_head()
        })

        showNotification("Validation session updated")
      })

      # Reactive object to store the detection index within df_cut()
      det_counter <- reactiveVal(1)
      # Upon initialization and under these conditions, det_counter is set to 1
      observeEvent(
        list(input$score_interval, input$user_setup_confirm), det_counter(1)
        # list(input$user_setup_confirm), det_counter(1)
      )
      # Create a reactive object to store the data of the active detection
      det_i <- reactiveVal(NULL)
      # Update the active acoording to the counter value
      observeEvent(det_counter(), {
        req(df_cut())
        det_i(df_cut()[det_counter(), ])
        updateNoUiSliderInput(
          session, "zoom_freq",
          range = c(0, (min(df_cut()$detection_sample_rate) / 2000) - 1)
        )
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
        if (
          det_i()$soundscape_file != input$soundscape_file &
            det_i()$soundscape_file %in% vec_soundscapes()
        ) {
          updateSelectInput(session, "soundscape_file",
            selected = det_i()$soundscape_file
          )
        }
      })

      # todo - activate this to navigate between soundscapes directly
      # observeEvent(input$soundscape_file, {
      #   req(df_cut(), det_counter(), det_i())
      #   i <- which(df_cut()$detection_id == input$detec)
      #   j <- which(df_cut()$soundscape_file == input$soundscape_file)
      #   if (!is.null(j) & length(j) > 0) {
      #     det_counter(j[1])
      #     updateSelectInput(session, "detec", selected = df_cut()$detection_id[j[1]])
      #   }
      # })


      custom_references <- reactiveVal(NULL)

      # Reactive object containing the wav of the active template
      # todo - change the rendered template spectrogram according to the selected template
      observeEvent(input$lock_template, {
        req(input$templates_path)
        if (input$lock_template != TRUE) {
          enable("custom_reference")
          custom_refs <- monitoraSom::fetch_template_metadata(
            path = input$templates_path, recursive = TRUE
          )
          # Add validation to ensure custom_refs is not empty
          if (nrow(custom_refs) > 0) {
            custom_references(custom_refs)
            updateSelectInput(
              session, "custom_reference",
              choices = custom_refs$template_path,
              selected = input$custom_reference
            )
          } else {
            showNotification("No template files found in the specified path", type = "warning")
          }
        } else {
          updateSelectInput(
            session, "custom_reference",
            choices = NULL
          )
          disable("custom_reference")
          custom_references(NULL)
        }
      })

      rec_template <- reactiveVal(NULL)
      observe({
        req(df_template())

        template_data <- if (input$lock_template == TRUE) {
          df_template()
        } else if (!is.null(custom_references())) {
          custom_refs <- custom_references() %>%
            dplyr::filter(template_path == input$custom_reference)
          if (nrow(custom_refs) > 0) {
            custom_refs %>% head(1)
          } else {
            df_template() # Fallback to default if no custom template found
          }
        } else {
          df_template() # Fallback to default if no custom references available
        }

        wav_path <- template_data$template_path
        if (nrow(template_data) == 0 | is.na(wav_path) | !file.exists(wav_path)) {
          rec_template(NULL)
          showNotification("Template file not found", type = "warning")
        } else {
          template_duration <- template_data$template_end - template_data$template_start
          rec_start <- max(0, template_data$template_start - zoom_pad())
          pre_silence <- max(0, -(template_data$template_start - zoom_pad()))
          rec_end <- min(template_duration, template_data$template_end + zoom_pad())
          pos_silence <- max(0, (template_data$template_end + zoom_pad()) - template_duration)

          if (length(wav_path) == 1) {
            res <- tuneR::readWave(
              wav_path,
              from = rec_start, to = rec_end, units = "seconds"
            ) %>%
              seewave::addsilw(., at = "start", d = pre_silence, output = "Wave") %>%
              seewave::addsilw(., at = "end", d = pos_silence, output = "Wave")

            rec_template(res)

            # Rendering the template HTML player
            if (file.exists(wav_path) & input$wav_player_type == "HTML player") {
              temp_file <- gsub("\\\\", "/", tempfile(
                pattern = "template_", tmpdir = session_data$temp_path,
                fileext = ".wav"
              ))
              if (zoom_pad() != 0) {
                res <- seewave::cutw(
                  res,
                  from = zoom_pad(), to = seewave::duration(res) - zoom_pad(),
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
              template_list <- list.files(
                session_data$temp_path,
                pattern = "template_.*.wav", full.names = TRUE
              )
              file.remove(template_list[template_list != temp_file])
            } else {
              removeUI(selector = "#template_player_selector")
            }
          }
        }
      })

      spectro_template <- reactive({
        req(df_template())
        if (is.null(rec_template())) {
          ggplot() +
            annotate("label", x = 1, y = 1, label = "Template not available") +
            theme_void()
        } else {
          box_color <- ifelse(
            input$color_scale %in% c("greyscale 1", "greyscale 2"), "black", "white"
          )
          temp_rec <- rec_template()
          fast_spectro(
            rec = temp_rec, f = temp_rec@samp.rate, wl = input$wl,
            ovlp = input$ovlp, flim = c(input$zoom_freq[1], input$zoom_freq[2]),
            dyn_range = c(input$dyn_range_templ[1], input$dyn_range_templ[2]),
            time_guide_interval = input$time_guide_interval,
            freq_guide_interval = input$freq_guide_interval,
            color_scale = input$color_scale, pitch_shift = input$pitch_shift
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
                zoom_pad() == 0, seewave::duration(rec_template()),
                seewave::duration(rec_template()) - zoom_pad()
              ),
              ymin = df_template()$template_min_freq,
              ymax = df_template()$template_max_freq,
              linetype = "dashed", alpha = 0,
              color = box_color, fill = box_color
            ) +
            theme(legend.position = "none")
        }
      })

      # render the template spectrogram in the interface
      output$TemplateSpectrogram <- renderPlot({
        req(df_template())
        spectro_template()
      })


      # Set of updates of spectrogram parameters that are obtained from
      # detection metadata
      observeEvent(input$get_templ_pars, {
        req(df_template(), df_cut())
        updateSliderInput(
          session,
          inputId = "ovlp", label = "Overlap (%):",
          value = df_template()$detection_ovlp, step = 10
        )
        updateSliderTextInput(
          session,
          inputId = "wl", label = "Window length:",
          selected <- df_template()$detection_wl
        )
        min_freq <- (df_template()$template_min_freq - 1) %>%
          round(0) %>%
          ifelse(. < 0, 0, .)
        max_freq <- (df_template()$template_max_freq + 1) %>%
          round(0) %>%
          ifelse(. > 23, 23, .)
        updateNoUiSliderInput(
          session, "zoom_freq", "Frequency band (kHz):",
          range = c(0, (min(df_cut()$detection_sample_rate) / 2000) - 1),
          value = c(min_freq, max_freq)
        )
      })

      observeEvent(input$default_pars, {
        req(det_i())
        updateSliderInput(session, inputId = "dyn_range_templ", value = session_data$dyn_range_templ)
        updateSliderInput(session, inputId = "dyn_range_detec", value = session_data$dyn_range_detec)
        updateSliderTextInput(session, inputId = "wl", selected = session_data$wl)
        updateSliderInput(session, inputId = "ovlp", value = session_data$ovlp)
        updateSelectInput(session, inputId = "color_scale", selected = session_data$color_scale)
        updateSliderInput(session, inputId = "time_pads", value = session_data$time_pads)
        updateSliderTextInput(session, inputId = "pitch_shift", selected = session_data$pitch_shift)
        updateCheckboxInput(session, inputId = "visible_bp", value = session_data$visible_bp)
        updateCheckboxInput(session, inputId = "play_norm", value = session_data$play_norm)
        updateNoUiSliderInput(
          session, "zoom_freq", "Frequency band (kHz):",
          range = c(0, (min(df_cut()$detection_sample_rate) / 2000) - 1),
          value = c(0, (min(df_cut()$detection_sample_rate) / 2000) - 1)
        )
        updateSliderInput(session, inputId = "time_guide_interval", value = session_data$time_guide_interval)
        updateSliderInput(session, inputId = "freq_guide_interval", value = session_data$freq_guide_interval)
      })

      zoom_pad <- reactiveVal(0)
      observe({
        zoom_pad(input$time_pads)
      })


      # Reactive object to store info about the detections in the active sounscape
      df_detections <- reactive({
        req(df_cut())
        df_cut() %>%
          dplyr::filter(
            template_name == input$template_name
            # ,
            # soundscape_file == # input$soundscape_file # todo Confirmar se está certo
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
          res <- tuneR::readWave(
            filename = det_i()$soundscape_path,
            from = pad_start, to = pad_end, units = "seconds"
          )
          rec_detection(res)
        }

        if (is.na(det_i()$detection_sample_rate)) {
          updateNoUiSliderInput(
            session, "zoom_freq",
            range = c(0, (res@samp.rate / 2000) - 1)
          )
        }

        # Rendering the detection HTML player
        if (file.exists(det_i()$soundscape_path) & input$wav_player_type == "HTML player") {
          # file.remove("temp/detection_clip.wav")
          temp_file <- gsub("\\\\", "/", tempfile(
            pattern = "detection_", tmpdir = session_data$temp_path,
            fileext = ".wav"
          ))
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
            res <- tuneR::normalize(
              object = res, unit = as.character(res@bit), pcm = TRUE
            )
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
          dyn_range = c(input$dyn_range_detec[1], input$dyn_range_detec[2]),
          time_guide_interval = input$time_guide_interval,
          freq_guide_interval = input$freq_guide_interval,
          color_scale = input$color_scale, pitch_shift = input$pitch_shift,
          norm = FALSE
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
          ) +
          theme(legend.position = "none")
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
            res <- tuneR::readWave(wav_path)
            rec_soundscape(res)
            # Rendering the template HTML player
            if (file.exists(wav_path)) {
              if (input$wav_player_type == "HTML player") {
                # file.remove("temp/soundscape_clip.wav")
                temp_file <- gsub(
                  "\\\\", "/", tempfile(
                    pattern = "soundscape_", tmpdir = session_data$temp_path,
                    fileext = ".wav"
                  )
                )
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
            dyn_range = c(input$dyn_range_detec[1], input$dyn_range_detec[2]),
            time_guide_interval = input$time_guide_interval,
            freq_guide_interval = input$freq_guide_interval,
            color_scale = input$color_scale, pitch_shift = input$pitch_shift,
            norm = FALSE
          )
        } else {
          return()
        }
      })

      output$SoundscapeSpectrogram <- renderPlot({
        req(spectro_soundscape(), df_detections(), det_i())
        if (input$show_soundscape == TRUE) {
          inactive_detecs <- df_detections() %>%
            dplyr::filter(detection_id != det_i()$detection_id)

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
          if (!is.na(df_template()$template_path) | file.exists(df_template()$template_path)) {
            showElement("play_template")
          }
          # if (input$show_soundscape == TRUE) showElement("play_soundscape")
        } else if (x == "External player" & !is.null(input$wav_player_path)) {
          if (file.exists(input$wav_player_path)) {
            tuneR::setWavPlayer(input$wav_player_path)
            showElement("play_detec")
            if (!is.na(df_template()$template_path) | file.exists(df_template()$template_path)) {
              showElement("play_template")
            }
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
          res <- tuneR::normalize(
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

      # Audio control state management
      audio_state <- reactiveValues(
        template_player = NULL,
        detection_player = NULL
      )

      # Handle hotkey audio control
      observeEvent(input$hotkeys, {
        req(input$hotkeys %in% c("1", "2"))

        if (input$wav_player_type == "HTML player") {
          # Determine which player to control (1 = detection, 2 = template)
          player_id <- if (input$hotkeys == "1") {
            "#detection_player_selector"
          } else {
            "#template_player_selector"
          }

          # Control player via JavaScript
          runjs(sprintf("
            var player = document.querySelector('%s');
            if (player) {
              if (player.paused) {
                player.play();
              } else {
                player.pause();
                player.currentTime = 0;
              }
            }
          ", player_id))
        } else {
          # Handle existing R session / External player logic
          if (input$hotkeys == "1") {
            # Play detection (was template)
            req(
              rec_detection(),
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
              res <- tuneR::normalize(
                object = res, unit = as.character(res@bit), pcm = TRUE
              )
            }
            tuneR::play(object = res)
          } else if (input$hotkeys == "2") {
            # Play template (was detection)
            req(
              rec_template(),
              input$wav_player_type %in% c("R session", "External player")
            )
            req(df_template())
            res <- readWave(df_template()$template_path)
            pitch_shift <- abs(input$pitch_shift)
            if (input$pitch_shift < 1) {
              res@samp.rate <- res@samp.rate / pitch_shift
            }
            if (isTRUE(input$visible_bp)) {
              res <- seewave::ffilter(
                res,
                f = res@samp.rate,
                from = (input$zoom_freq[1] / pitch_shift) * 1000,
                to = (input$zoom_freq[2] / pitch_shift) * 1000,
                wl = input$wl, output = "Wave", bandpass = TRUE
              )
            }
            tuneR::play(object = res)
          }
        }
      })

      # Update tooltips to reflect HTML player hotkeys
      observe({
        req(input$wav_player_type)
        if (input$wav_player_type == "HTML player") {
          shinyBS::addTooltip(session,
            id = "template_player_selector",
            title = "Hotkey = 1",
            placement = "bottom", trigger = "hover", options = pop_up_opt
          )
          shinyBS::addTooltip(session,
            id = "detection_player_selector",
            title = "Hotkey = 2",
            placement = "bottom", trigger = "hover", options = pop_up_opt
          )
        }
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
          df_cut(), det_counter(), input$template_name, # input$soundscape_file,
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
          dplyr::select(template_name, peak_score, validation) %>%
          dplyr::filter(
            template_name == input$template_name &
              validation %in% c("TP", "FP")
          ) %>%
          dplyr::mutate(
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
          dplyr::mutate(
            validation = validation_input(),
            validation_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            validation_user = input$validation_user,
            validation_note = ifelse(
              is.na(input$detec_note),
              NA_character_, as.character(input$detec_note)
            )
          )

        if (input$overwrite == TRUE) {
          if (res_A$validation_user == det_i()$validation_user || is.na(det_i()$validation_user)) {
            det_i(res_A)
            df_cut(rows_update(df_cut(), res_A, by = "detection_id", unmatched = "ignore"))
            df_output(rows_update(df_output(), res_A, by = "detection_id", unmatched = "ignore"))
            validation_input(NULL) # reset after value is passed on forward
            if (input$lock_detec_note == FALSE) {
              updateTextInput(session, "detec_note", value = NA)
            }
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
          df_cut(
            dplyr::rows_patch(
              df_cut(), res_A,
              by = "detection_id", unmatched = "ignore"
            )
          )
          df_output(
            dplyr::rows_patch(
              df_output(), res_A,
              by = "detection_id", unmatched = "ignore"
            )
          )
          validation_input(NULL) # reset after value is passed on forward
          if (input$lock_detec_note == FALSE) {
            updateTextInput(session, "detec_note", value = NA)
          }
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
        if (sum(df_cut()$validation == "NV") == 0) {
          showModal(
            modalDialog(
              title = "All detections from this template are validated",
              "Review the session setup if there are more detections to be validated",
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
            dplyr::select(
              template_name, soundscape_file, detection_id,
              detection_start, detection_end, template_min_freq, template_max_freq,
              peak_score, validation_user, validation_time, validation,
              validation_note
            ) %>%
            DT::datatable(
              editable = FALSE, style = "bootstrap4",
              selection = "single", filter = "none", # escape = FALSE,
              colnames = c(
                "Template", "Soundscape", "ID", "Start", "End",
                "Min. Freq.", "Max. Freq.", "Score", "User",
                "Time Stamp", "Validation", "Note"
              )
            ) %>%
            DT::formatRound(c("detection_start", "detection_end", "peak_score"), 3) %>%
            DT::formatRound(c("template_min_freq", "template_max_freq"), 1)
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


      output$count_full_tab <- renderTable(
        {
          req(df_output(), df_cut())
          df_output() %>%
            dplyr::group_by(template_name, validation) %>%
            dplyr::summarise(n = n()) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_wider(names_from = "validation", values_from = "n") %>%
            dplyr::rename(Template = template_name)
        },
        width = "100%"
      )
      output$count_i_tab <- renderTable(
        {
          req(df_output(), df_cut())
          df_cut() %>%
            # filter(template_name == input$template_name) %>%
            dplyr::group_by(template_name, validation) %>%
            dplyr::summarise(n = n()) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_wider(names_from = "validation", values_from = "n") %>%
            dplyr::rename(Template = template_name)
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
              dplyr::mutate(validation = as.factor(validation)) %>%
              caret::downSample(x = ., y = .$validation, yname = "temp") %>%
              select(-temp)
          } else if (input$diag_balance == "Upsample smaller  class") {
            df_diag_input_raw() %>%
              dplyr::mutate(validation = as.factor(validation)) %>%
              caret::upSample(x = ., y = .$validation, yname = "temp") %>%
              dplyr::select(-temp)
          } else if (input$diag_balance == "ROSE") {
            df_diag_input_raw() %>%
              dplyr::select(-template_name) %>%
              dplyr::mutate(validation = as.factor(validation)) %>%
              {
                ROSE::ROSE(validation ~ ., data = .)$data
              } %>%
              dplyr::mutate(
                template_name = unique(df_diag_input_raw()$template_name),
                .before = "peak_score"
              )
          }
        }
      })

      mod_plot_react <- reactiveVal(NULL)
      # roc_plot_react <- reactiveVal(NULL)
      plot_dens_react <- reactiveVal(NULL)
      precrec_plot_react <- reactiveVal(NULL)
      cut_full_tab <- reactiveVal(NULL)
      cut_i_tab <- reactiveVal(NULL)

      observe({
        req(df_diag_input())
        if (nrow(df_diag_input()) > 2) {
          if (length(unique(df_diag_input()$validation)) == 2) {
            if (input$diag_method == "Manual") {
              diag_method <- "Manual"
              custom_cut <- input$diag_cut
              pos_prob <- NULL
            } else if (input$diag_method == "Error = 0.05") {
              diag_method <- "Auto"
              custom_cut <- NULL
              pos_prob <- 0.95
            } else if (input$diag_method == "Error = 0.1") {
              diag_method <- "Auto"
              custom_cut <- NULL
              pos_prob <- 0.90
            }
            val_res <- monitoraSom::diagnostic_validations_i(
              val_i = df_diag_input(),
              diag_method = diag_method, pos_prob = pos_prob, diag_cut = custom_cut
            )
            if (
              val_res$score_cut < 1 & val_res$score_cut > 0 &
                input$diag_method != "Manual"
            ) {
              updateSliderInput(session, "diag_cut", value = val_res$score_cut)
            }
            mod_plot_react(val_res$mod_plot)
            cut_full_tab(val_res$diag_out)
            precrec_plot_react(val_res$precrec_plot)
            cut_i_tab(val_res$diag_out[which(val_res$diag_out$peak_score == val_res$score_cut), ])
            plot_dens_react(val_res$plot_dens)
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
          enable("plot_dens")
          enable("plot_binomial")
          enable("plot_prec_rec")
          shinyjs::show("plot_binomial")
          shinyjs::show("plot_prec_rec")
        } else {
          disable("diag_balance")
          disable("diag_method")
          disable("plot_dens")
          disable("diag_cut")
          disable("plot_binomial")
          disable("plot_prec_rec")
          shinyjs::hide("plot_binomial")
          shinyjs::hide("plot_prec_rec")
        }
      })

      output$cut_i_tab <- renderTable(
        {
          req(cut_i_tab())
          cut_i_tab() %>%
            dplyr::mutate(tp = as.integer(tp), fp = as.integer(fp)) %>%
            dplyr::select(-tn, -fn, -tnr, -fnr, -selected) %>%
            setNames(
              c(
                "Template", "Threshold", "TP (n)", "FP (n)",
                "TP Rate", "FP Rate", "Precision", "Relative Recall",
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
        req(precrec_plot_react())
        precrec_plot_react()
      })
      # output$plot_roc <- renderPlot({
      #   req(plot_roc())
      #   roc_plot_react()
      # })
      output$plot_dens <- renderPlot({
        req(plot_dens_react())
        plot_dens_react()
      })

      # teste_val <- reactiveVal(NULL)
      # output$checagem1 <- renderPrint({
      #   req(det_i())
      #   det_i() %>%
      #     glimpse()
      #   # glimpse()
      # })

      # output$checagem2 <- renderPrint({
      #   glimpse(readRDS("app_presets/validation_preset_Salobo_validation.rds"))
      # })

      wav_filename <- reactiveVal()
      wav_default_filename <- reactive({
        req(det_i())
        res <- paste0(
          stringr::str_replace(det_i()$soundscape_file, "\\.wav$|\\.WAV$", ""),
          "_",
          stringr::str_pad(
            sprintf("%.3f", round(det_i()$detection_start, 3)), 7, pad = "0"
          ),
          "-",
          stringr::str_pad(
            sprintf("%.3f", round(det_i()$detection_end, 3)), 7, pad = "0"
          ),
          "s_",
          stringr::str_pad(
            sprintf("%.3f", round(det_i()$template_min_freq, 3)), 6, pad = "0"
          ),
          "-",
          stringr::str_pad(
            sprintf("%.3f", round(det_i()$template_max_freq, 3)), 6, pad = "0"
          ),
          "kHz_", input$wl, "wl_", input$ovlp, "ovlp_",
          tail(
            stringr::str_split(
              gsub("\\.wav$", "", det_i()$template_name), "_"
            )[[1]], 1
          ),
          ".wav"
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
        # todo - validate the path and return a message if it's not valid
        tuneR::writeWave(
          object = rec_detection(),
          filename = file.path(input$wav_cuts_path, wav_filename())
        )
        showNotification("Detection wave file successfully exported")
      })
      # Export the wav file by using the hotkey
      observeEvent(input$hotkeys, {
        req(input$hotkeys == "r", input$wav_cuts_path, wav_filename(), rec_detection())
        tuneR::writeWave(
          object = rec_detection(),
          filename = file.path(input$wav_cuts_path, wav_filename())
        )
        showNotification("Detection wave file successfully exported")
      })

      spec_filename <- reactiveVal()
      spec_default_filename <- reactive({
        req(det_i())
        res <- paste0(
          stringr::str_replace(det_i()$soundscape_file, "\\.wav$|\\.WAV$", ""),
          "_",
          stringr::str_pad(
            sprintf("%.3f", round(det_i()$detection_start, 3)), 7, pad = "0"
          ),
          "-",
          stringr::str_pad(
            sprintf("%.3f", round(det_i()$detection_end, 3)), 7, pad = "0"
          ),
          "s_",
          stringr::str_pad(
            sprintf("%.3f", round(det_i()$template_min_freq, 3)), 6, pad = "0"
          ),
          "-",
          stringr::str_pad(
            sprintf("%.3f", round(det_i()$template_max_freq, 3)), 6, pad = "0"
          ),
          "kHz_", input$wl, "wl_", input$ovlp, "ovlp_",
          tail(
            stringr::str_split(
              gsub("\\.wav$", "", det_i()$template_name), "_"
            )[[1]], 1
          ),
          ".jpeg"
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
        # todo - validate the path and return a message if it's not valid
        res <- cowplot::plot_grid(spectro_template(), spectro_detection())
        ggplot2::ggsave(
          filename = file.path(input$spec_path, spec_filename()),
          plot = res, width = 12, height = 6, units = "in", dpi = 72
        )
        showNotification("Detection spectrogram successfully exported")
      })
      # Export the wav file by using the hotkey
      observeEvent(input$hotkeys, {
        req(input$hotkeys == "t", input$spec_path, spec_filename(), rec_detection())
        res <- cowplot::plot_grid(spectro_template(), spectro_detection())
        ggplot2::ggsave(
          filename = file.path(input$spec_path, spec_filename()),
          plot = res, width = 12, height = 6, units = "in", dpi = 72
        )
        showNotification("Detection spectrogram successfully exported")
      })

      # Trigger checks and confirm or cancel the end of the session
      observeEvent(input$end_session, {
        req(df_cut(), df_output())

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
            dplyr::anti_join(
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
      # todo - complete the tooltips of the remaining UI
      # Define all tooltips in a structured list
      tooltips_config <- list(
        # User Setup Section
        user_setup = list(
          list(
            id = "preset_path", title = "Presets available here will be shown in the drop-down menu below", placement = "right"
          ),
          list(
            id = "validation_user", title = "Recommended format: 'Rosa G. L. M. (avoid commas)", placement = "right"
          ),
          list(
            id = "templates_path", title = "Parent location that contains only template files or folders of these", placement = "right"
          ),
          list(
            id = "soundscapes_path", title = "Parent location that contains only soundscape files or folders of these", placement = "right"
          ),
          list(
            id = "input_path", title = "Complete path to the 'csv.' file that contains detection data", placement = "right"
          ),
          list(
            id = "output_path", title = "Complete path to the 'csv.' file in which validation from this session will be stored", placement = "right"
          )
        ),

        # Session Setup Section
        session_setup = list(
          list(
            id = "confirm_session_setup", title = "<b>Part 2 of 2 required to start the session</b>.", placement = "right"
          ),
          list(
            id = "template_name", title = "Select here one of the templates available in the input file", placement = "right"
          ),
          list(
            id = "val_subset", title = "Select at least one options. Only those selected will be shown.", placement = "right"
          ),
          list(
            id = "score_interval", title = "Only detections within this interval will be presented to the user", placement = "right"
          ),
          list(
            id = "time_pads", title = "Zoom in and out in the time axis of template and detection spectrograms", placement = "right"
          ),
          list(
            id = "dyn_range_templ", title = "Adjust what portion of the amplitude scale is shown in the template spectrogram", placement = "right"
          ),
          list(
            id = "dyn_range_detec", title = "Adjust what portion of the amplitude scale is shown in the detection spectrogram", placement = "right"
          ),
          list(
            id = "wl", title = "Tradeoff between time and frequency resolution", placement = "right"
          ),
          list(
            id = "ovlp", title = "Increase if more resultion is needed. Performance may decrease for values above 80%", placement = "right"
          ),
          list(
            id = "wav_player_type", title = "Select the method to play wav files", placement = "right"
          ),
          list(
            id = "wav_player_path", title = "Necessary when 'External player' is selected. If the executable is not available, 'HTML player' will be automatically selected", placement = "right"
          ),
          list(
            id = "get_templ_pars", title = "Set spectrogram parameters to those used to run the detections", placement = "right"
          ),
          list(
            id = "default_pars", title = "Set spectrogram parameters back to the default", placement = "right"
          )
        ),

        # Controls Section
        controls = list(
          list(
            id = "play_detec", title = "Play detection. Hotkey = 1", placement = "bottom"
          ),
          list(
            id = "play_template", title = "Play template. Hotkey = 2", placement = "bottom"
          ),
          list(
            id = "prev_detec", title = "Navigate to the previous detection. Hotkey: A", placement = "bottom"
          ),
          list(
            id = "next_detec", title = "Navigate to the next detection. Hotkey: D", placement = "bottom"
          ),
          list(
            id = "button_tp", title = "Validate active detection as 'True Positive'. Hotkey: Q", placement = "bottom"
          ),
          list(
            id = "button_un", title = "Validate active detection as 'Unknown'. Hotkey: W", placement = "bottom"
          ),
          list(
            id = "button_fp", title = "Validate active detection as a 'False Positive'. Hotkey: E", placement = "bottom"
          ),
          list(
            id = "button_save", title = "Export validations to the output '.csv' file. Hotkey: S", placement = "bottom"
          )
        ),

        # Export Section
        export = list(
          list(
            id = "wav_cuts_path", title = "Path for exporting an audio sample of the detection", placement = "bottom"
          ),
          list(
            id = "wav_cut_name", title = "Name of the file with the audio sample ('.wav')", placement = "bottom"
          ),
          list(
            id = "reset_wav_cut_name", title = "Reset the filename back to the default", placement = "bottom"
          ),
          list(
            id = "confirm_wav_export", title = "Hotkey: R", placement = "bottom"
          ),
          list(
            id = "spec_path", title = "Path for exporting the spectrogram image", placement = "bottom"
          ),
          list(
            id = "spec_name", title = "Name of the spectrogram image file ('.jpeg')", placement = "bottom"
          ),
          list(
            id = "reset_spec_filename", title = "Reset the filename back to the default", placement = "bottom"
          ),
          list(
            id = "confirm_spec_export", title = "Hotkey: T", placement = "bottom"
          )
        )
      )

      # General popover options
      pop_up_opt <- list(delay = list(show = 1000, hide = 0))

      # Apply all tooltips efficiently
      lapply(unlist(tooltips_config, recursive = FALSE), function(tip) {
        shinyBS::addTooltip(
          session,
          id = tip$id,
          title = tip$title,
          placement = tip$placement,
          trigger = "hover",
          options = pop_up_opt
        )
      })
    }
  )
}

