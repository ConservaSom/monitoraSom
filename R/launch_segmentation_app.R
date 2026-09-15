#' Launch the segmentation app
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Launches an interactive Shiny app for segmenting soundscape WAV recordings
#'   into regions of interest (ROIs). You draw boxes on the spectrogram, label
#'   them, and the app stores the ROIs in a DuckDB database. Optionally exporting
#'   each ROI as an audio cut. It is the manual starting point of the analysis
#'   flow, where ground-truth ROIs and templates are created.
#'
#' @details
#'   The app opens a \pkg{shinydashboard} two-pane layout (a settings sidebar and
#'   a tabbed spectrogram/table pane); display and playback settings can be set in
#'   the call, adjusted live, or imported from a saved preset. Two navigation modes
#'   are available: the classic mode scans `soundscapes_path` for WAV files, while
#'   passing `df_soundscapes` drives a table-guided review of a pre-filtered,
#'   pre-ordered set of recordings. ROIs accumulate in `roi_db` across sessions.
#'   Being interactive, the app must be run in an R session and will be
#'   accessible as a pop-up on RStudio, or in the user browser of choice.
#'
#' @section Detection overlay and promotion:
#'   The "Detections" sidebar tab draws the `signal_class = "detection"` rows
#'   of the current soundscape read from `roi_db` (the signals store) as
#'   a read-only overlay, off by default. A peak-score slider and a template
#'   dropdown filter which detections are drawn and selectable; left-click
#'   selects one and `Alt+P` promotes it: the detection box is copied into a
#'   new editable ROI (`signal_class = "detection_to_roi"`, `origin =
#'   "template_matching"`, `roi_type` NA) that is persisted immediately,
#'   while the source detection row stays untouched (additive promotion). A
#'   one-per-session orientation popup asks you to confirm before the first
#'   promotion. Promote a detection only when it satisfies the assumptions of
#'   a manual ROI: the box is correct and complete, and the label is the one a
#'   person would assign. Promoted ROIs are eligible as validation ground
#'   truth (`validate_by_overlap()`). Detections are read from `roi_db`; when
#'   they were persisted to a separate store, migrate them first with
#'   `migrate_detections_store_to_signals()`.
#'
#'   **Exhaustiveness contract (validation data).** A recording is ready to
#'   feed `validate_by_overlap()` only when its review state is known: it
#'   either carries ROIs, or it carries the "no signals of interest" sentinel
#'   (mark a fully reviewed recording with it). A recording with neither was
#'   never reviewed — `validate_by_overlap()` EXCLUDES its detections and
#'   warns. An unreviewed recording never counts as absence of the species.
#'
#' @section Pipeline context:
#'   Step 1 of the monitoraSom analysis flow. Reads soundscape WAV recordings
#'   (a directory, or a `df_soundscapes` table). Produces ROI data in a DuckDB
#'   store (and optional audio cuts) that [fetch_rois()] (step 2) reads.
#'
#' @param project_path Path to the project folder where the segmentation
#'   project will be stored. Required for first-time setup or relative paths.
#' @param roi_user Identification of the segmentation app user. **Required**.
#' @param soundscapes_path Path to soundscape WAV files.
#' @param df_soundscapes Optional data.frame selecting the soundscapes to
#'   segment and their navigation order (table-guided mode). Rows are used
#'   as the navigation universe, in the order they appear; only a
#'   character `soundscape_path` column is required. When supplied, path fields start
#'   locked and directory scanning is skipped. Pre-filter and order the table
#'   with ordinary `dplyr` before launching, e.g.
#'   `df <- fetch_soundscape_metadata("soundscapes/") |>
#'   dplyr::arrange(soundscape_timestamp)`. Leave `NULL` for the classic
#'   directory-scan mode.
#' @param roi_db Path to the DuckDB database for storing ROIs; detections are
#'   stored in this same database. Defaults to
#'   \code{file.path(project_path, "rois.duckdb")} when \code{project_path} is
#'   provided.
#' @param templates_path Directory for exporting template audio cuts and the
#'   template database (default: `<project_path>/templates/`).
#' @param labels_file Path to a label-list `.txt` file (one label per line;
#'   the list name is the file name without the `label_list_` prefix), a
#'   directory of such files, or, for back-compatibility, the old `.xlsx` file
#'   with one list per column.
#' @param roi_label_list Name of the default ROI label list column.
#' @param label_angle Angle (0-90, multiples of 10) for ROI label display.
#' @param show_label If TRUE, ROI labels are shown on the spectrogram.
#' @param time_guide_interval Interval in seconds between time guides.
#'   0 disables the guides (default).
#' @param freq_guide_interval Interval in kHz between frequency guides.
#'   0 disables the guides (default).
#' @param dyn_range Numeric vector of length 2: min and max amplitude (dBFS).
#' @param dyn_range_bar Numeric vector of length 2: limits for the dynamic
#'   range slider.
#' @param wl FFT window length (power of 2, 128-16384).
#' @param ovlp Overlap percentage (0-80, step 10).
#' @param color_scale Color scale: "viridis", "magma", "inferno", "cividis",
#'   "greyscale 1", or "greyscale 2".
#' @param visible_bp If TRUE, applies bandpass filter matching visible
#'   frequency band to audio playback.
#' @param play_norm If TRUE, normalizes audio playback.
#' @param zoom_freq Numeric vector of length 2: initial frequency range (kHz).
#' @param zoom_time Numeric vector of length 2: initial time range (seconds)
#'   for the `zoom_time` slider. This argument only sets the slider's initial
#'   state on each recording load; it is clamped to the recording duration.
#'   `NULL` (default) keeps the current behaviour of showing the first 60
#'   seconds.
#' @param nav_autosave If TRUE, navigation saves rebuild the dropdown on the
#'   destination file and auto-export template ROIs. ROI edits are always
#'   persisted on navigation.
#' @param pitch_shift Pitch shift value (-8, -6, -4, -2, or 1) for playback.
#' @param skip_path_confirmation If TRUE, automatically confirms path setup.
#'
#' @return Called for its side effect: it starts an interactive Shiny app in the
#'   browser and returns when the app is closed. ROIs are persisted to `roi_db`.
#' @seealso [fetch_rois()], [export_templates()], [launch_validation_app()]
#' @export
#' @import ggplot2
#' @importFrom dplyr %>%
#' @examples
#' # Step 1 of the analysis flow: segment recordings into ROIs. Interactive app;
#' # run it in a session with a browser. See the segmentation-app vignette for a
#' # full walkthrough.
#' if (interactive()) {
#'   # Load the package
#'   library(monitoraSom)
#'
#'   # Synthesize a small recording with three tone bursts: a clear,
#'   # segmentable signal for your first ROI boxes.
#'   sr <- 44100
#'   gap <- tuneR::silence(duration = 1.5 * sr, samp.rate = sr)
#'   tone <- tuneR::sine(4000, duration = 0.5 * sr, samp.rate = sr)
#'   rec <- tuneR::bind(gap, tone, gap, tone, gap, tone, gap)
#'   rec <- tuneR::normalize(rec, unit = "16")
#'   soundscapes_dir <- file.path(tempdir(), "soundscapes")
#'   dir.create(soundscapes_dir, showWarnings = FALSE)
#'   tuneR::writeWave(rec, file.path(soundscapes_dir, "synth_tone_bursts.wav"))
#'
#'   # Launch the app on the synthesized recording: draw boxes around the
#'   # bursts, label them, and save. The ROIs persist to rois.duckdb.
#'   launch_segmentation_app(
#'     project_path = tempdir(), roi_user = "User",
#'     soundscapes_path = soundscapes_dir
#'   )
#' }
launch_segmentation_app <- function(
    project_path = NULL,
    roi_user = NULL,
    soundscapes_path = NULL,
    df_soundscapes = NULL,
    roi_db = NULL,
    templates_path = NULL,
    labels_file = NULL,
    roi_label_list = "Brazilian birds (Pacheco et al. 2021)",
    label_angle = 90,
    show_label = TRUE,
    time_guide_interval = 0,
    freq_guide_interval = 0,
    dyn_range = c(-60, 0),
    dyn_range_bar = c(-144, 0),
    wl = 1024,
    ovlp = 0,
    color_scale = "inferno",
    visible_bp = FALSE,
    play_norm = FALSE,
    zoom_freq = c(0, 180),
    zoom_time = NULL,
    nav_autosave = TRUE,
    pitch_shift = 1,
    skip_path_confirmation = TRUE) {

  # --- FEAT-07: default-path write guard ------------------------------------
  # The app anchors every output (rois.duckdb, roi_cuts/, app_presets/) under
  # project_path; warn (and confirm, interactively) when that root resolves to
  # the session working directory or another unmarked location.
  .require_explicit_workspace(
    if (is.null(project_path)) "." else project_path,
    default_target = is.null(project_path),
    label = "project_path", caller = "launch_segmentation_app")

  # --- Soundscape default resolution (CRAN item 3, §5) ----------------------

  # Only when neither a soundscape source was provided: resolve the canonical
  # default — the exported cache (soundscapes/soundscapes_metadata.duckdb)
  # first, then WAVs dropped in soundscapes/, else a clear error. A provided
  # soundscapes_path or df_soundscapes always takes precedence (no regression
  # for custom paths).
  if (is.null(soundscapes_path) && is.null(df_soundscapes)) {
    base <- if (is.null(project_path)) "." else project_path
    ss_dir <- file.path(base, .monitora_db_homes()[["soundscapes_metadata"]])
    ss_cache <- file.path(ss_dir, .monitora_db_names()[["soundscapes_metadata"]])
    if (file.exists(ss_cache)) {
      df_soundscapes <- .read_soundscape_table(ss_cache)  # table mode (LSA-203)
    } else if (dir.exists(ss_dir) &&
               length(list.files(ss_dir, pattern = "\\.wav$",
                                 ignore.case = TRUE)) > 0) {
      soundscapes_path <- ss_dir                         # directory mode
    } else {
      stop(
        "No default soundscape source was found: no cache at '", ss_cache,
        "' and no WAV files in '", ss_dir, "'. ",
        "Provide 'soundscapes_path' or 'df_soundscapes'."
      )
    }
  }

  # --- Session configuration (LSA-05) --------------------------------------

  session_data <- .validate_session_config(
    project_path = project_path,
    roi_user = roi_user,
    soundscapes_path = soundscapes_path,
    roi_db = roi_db,
    templates_path = templates_path,
    label_angle = label_angle,
    show_label = show_label,
    time_guide_interval = time_guide_interval,
    freq_guide_interval = freq_guide_interval,
    dyn_range = dyn_range,
    dyn_range_bar = dyn_range_bar,
    wl = wl,
    ovlp = ovlp,
    color_scale = color_scale,
    visible_bp = visible_bp,
    play_norm = play_norm,
    zoom_freq = zoom_freq,
    zoom_time = zoom_time,
    nav_autosave = nav_autosave,
    pitch_shift = pitch_shift
  )

  # --- Table-guided mode (LSA-203) ------------------------------------------

  # When a df_soundscapes table is supplied at the call, validate it eagerly so
  # a bad table (missing files, wrong shape) stops at launch (Q1) rather than
  # inside the running app. The validated frame (rows verbatim, order preserved)
  # is the pre-confirmed navigation universe handed to the server closure.
  validated_launch_table <- NULL
  if (!is.null(df_soundscapes)) {
    validated_launch_table <- .validate_soundscape_table(df_soundscapes)
  }

  # --- Load ROI label lists --------------------------------------------------

  # A6: seed app_presets/ with editable default roi_types.xlsx +
  # roi_label_lists.xlsx when missing, so the user edits ready-made files
  # instead of authoring them. Seed exactly where the loaders read
  # (project_path/app_presets); no-op when project_path is NULL; never
  # overwrites an existing preset.
  if (!is.null(project_path)) {
    .seed_app_presets(file.path(project_path, "app_presets"))
  }
  roi_label_lists <- .load_roi_label_lists(labels_file, project_path)
  roi_type_choices <- .load_roi_type_list(project_path)
  if (!roi_label_list %in% colnames(roi_label_lists)) {
    warning(
      "Selected ROI label list not found. Using first available list: '",
      colnames(roi_label_lists)[1], "'.", .msg_ref("001")
    )
    roi_label_list <- colnames(roi_label_lists)[1]
  }
  session_data$roi_label_list <- roi_label_list

  shiny::addResourcePath("audio", session_data$temp_path)

  # --- Hotkey definitions ----------------------------------------------------

  hotkeys <- c(
    "q", "w", "alt+w", "e", "ctrl+e", "r",
    "a", "z", "s", "alt+s", "d", "c",
    "g", "t", "f", "v",
    "alt+k", "alt+n",
    # LSA-106 active-ROI resize (Shift = step, Alt = snap to full)
    "shift+w", "shift+s", "shift+g", "shift+t",
    "shift+e", "shift+q", "alt+t", "alt+g", "alt+b",
    # LSA-112 metadata popup, LSA-115 boundary re-selection toggle
    "p", "b",
    # LSA-119 brush-guided zoom toggle
    "x",
    # LSA-208 detection promotion (overlay selection -> editable ROI)
    "alt+p",
    # A4 alternative store/delete keys (mirror e/q). Mousetrap (the keys
    # package backend) names the Delete key "del", not "delete".
    "enter", "del",
    # A5 undo the last active-ROI bounds change (snap / resize / reselect)
    "u"
  )

  # --- Extracted helpers ----------------------------------------------------
  # DB layer, ROI/audio helpers, config validation and setup helpers now live
  # in sibling package files (_roi_duckdb.R, _schema_rois.R, _roi_helpers.R,
  # _audio_helpers.R, _validate_session_config.R, _segmentation_setup.R).


  # --- UI (shinydashboard) ---------------------------------------------------

  ui <- shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(
      title = "monitoraSom", titleWidth = "400px"
    ),
    sidebar = shinydashboard::dashboardSidebar(
      width = "400px",
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          "User setup",
          tabName = "user_setup_tab", startExpanded = TRUE,
          icon = shiny::icon("user"),
          shiny::textInput(
            "roi_user", "ROI user",
            value = session_data$roi_user, placeholder = "Type your name here",
            width = "100%"
          ),
          # LSA-203: table-guided mode. Checking the lock activates table mode
          # (source below) and disables both path fields; unchecking re-enables
          # them (mode is re-derived on the next "Confirm Paths"). It starts
          # checked when a df_soundscapes table was supplied at the call.
          shiny::checkboxInput(
            "table_lock", "Table-guided mode (lock paths)",
            value = !is.null(df_soundscapes)
          ),
          shiny::textAreaInput(
            "df_soundscapes_path", "Path to a df_soundscapes table",
            value = "",
            placeholder = if (is.null(df_soundscapes)) {
              "Path to a saved .csv / .duckdb metadata table"
            } else {
              "supplied by function call"
            },
            height = "50px", resize = "vertical", width = "100%"
          ),
          shiny::textAreaInput(
            "soundscapes_path", "Path to the Soundscapes",
            value = session_data$soundscapes_path,
            placeholder = "Paste or load path here",
            height = "50px", resize = "vertical", width = "100%"
          ),
          shiny::textAreaInput(
            "roi_db", "ROI database path",
            value = session_data$roi_db,
            placeholder = "Path to .duckdb file",
            height = "50px", resize = "vertical", width = "100%"
          ),
          shiny::textAreaInput(
            "templates_path", "Destination of template cuts",
            value = session_data$templates_path,
            placeholder = "Paste or load path here",
            height = "50px", resize = "vertical", width = "100%"
          ),
          .btn_confirm("user_setup_confirm", "Confirm Paths")
        ),
        # Spectrogram Parameters as a sidebar tab -- layout restored to the
        # original app's placement (gate review 2026-08-18; the bottom-panel
        # placement was a regression). Inputs and grouping adapted to the
        # current implementation; ids unchanged so the server is unaffected.
        shinydashboard::menuItem(
          "Spectrogram Parameters",
          tabName = "spectro_params_tab", startExpanded = FALSE,
          icon = shiny::icon("sliders"),
          shinyWidgets::sliderTextInput(
            "wl", "Window length",
            choices = c(128, 256, 512, 1024, 2048, 4096, 8192, 16384),
            selected = session_data$wl, grid = TRUE, width = "100%"
          ),
          shiny::sliderInput(
            "ovlp", "Overlap (%)", min = 0, max = 80,
            value = session_data$ovlp, post = "%", step = 10, width = "100%"
          ),
          shiny::sliderInput(
            "dyn_range", "Dynamic range (dB)",
            min = session_data$dyn_range_bar[1],
            max = session_data$dyn_range_bar[2],
            step = 6, value = session_data$dyn_range, post = "dB",
            width = "100%"
          ),
          shinyWidgets::sliderTextInput(
            "pitch_shift", "Pitch shift (octaves)",
            choices = c(-8, -6, -4, -2, 1),
            selected = session_data$pitch_shift, post = " oct.",
            grid = TRUE, width = "100%"
          ),
          shiny::selectInput(
            "color_scale", "Color scale",
            choices = c(
              "Inferno" = "inferno", "Viridis" = "viridis",
              "Magma" = "magma", "Cividis" = "cividis",
              "Greyscale 1" = "greyscale 1", "Greyscale 2" = "greyscale 2"
            ),
            selected = session_data$color_scale, width = "100%"
          ),
          # Item-4: invert colors with dark mode directly below it.
          shiny::splitLayout(
            cellWidths = c("50%", "50%"),
            shiny::checkboxInput(
              "invert_colormap", "Invert colors", value = FALSE
            ),
            # UIX-05 (alternative A, user decision 2026-08-05): this toggle
            # only themes the ggplot -- the dashboard shell stays light.
            htmltools::tagAppendAttributes(
              shiny::checkboxInput(
                "dark_mode", "Dark spectrogram", value = FALSE
              ),
              title = paste(
                "Themes the spectrogram plot only.",
                "The surrounding dashboard keeps its light theme."
              )
            )
          ),
          # Freq guide first (item-5); time guide sits directly below.
          shiny::splitLayout(
            cellWidths = c("50%", "50%"),
            shiny::numericInput(
              "freq_guide_interval", "Freq Guide (kHz)",
              value = session_data$freq_guide_interval,
              min = 0, max = 192, step = 0.5, width = "100%"
            ),
            shiny::numericInput(
              "time_guide_interval", "Time Guide (s)",
              value = session_data$time_guide_interval,
              min = 0, max = 60, step = 0.5, width = "100%"
            )
          ),
          shiny::sliderInput(
            "label_angle", "Adjust label angle (degrees)",
            min = 0, max = 90, step = 10, value = session_data$label_angle,
            width = "100%"
          ),
          shiny::sliderInput(
            "font_scale", "Plot text size",
            min = 0.5, max = 3, step = 0.1, value = 1, width = "100%"
          ),
          # Item-5: show_label on the same line as the bandpass toggle.
          shiny::splitLayout(
            cellWidths = c("50%", "50%"),
            shiny::checkboxInput(
              "show_label", "Show label", value = session_data$show_label
            ),
            shiny::checkboxInput(
              "visible_bp", "Bandpass filter audio",
              value = session_data$visible_bp
            )
          ),
          shiny::checkboxInput(
            "play_norm", "Normalize audio", value = session_data$play_norm
          ),
          shiny::actionButton(
            "default_pars", "Reset to default parameters",
            icon = shiny::icon("gear"),
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 360px;"
          )
        )
      ),
      .btn_danger_wide("end_session", "End Session")
    ),
    body = shinydashboard::dashboardBody(

      shiny::tags$style(shiny::HTML(".form-group { margin-bottom: 10px !important; }")),

      keys::useKeys(),
      keys::keysInput("hotkeys", hotkeys),
      shinyjs::useShinyjs(),
      shiny::tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),
      shiny::tags$style(type = "text/css", "
        .path-ok { border: 2px solid #33b733 !important; }
        .path-bad { border: 2px solid #b73333 !important; }
        #spectro_panel {
          display: flex;
          flex-direction: column;
        }
        #spectro_panel .shiny-plot-output {
          flex: 1 1 0;
          min-height: 0;
          height: auto !important;
        }
        #spectro_panel .audio-container {
          flex-shrink: 0;
        }
        #spectro_panel .ui-resizable-s {
          height: 9px; cursor: row-resize;
          background: #adb5bd55; border-radius: 0 0 4px 4px;
        }
        #spectro_panel .ui-resizable-s:hover {
          background: #2c7fb8aa;
        }
      "),

      # Spectrogram box
      shinydashboard::box(
        width = "100%", height = "100%",

        # Title row: soundscape_file + roi_label_list + channel
        shiny::fluidRow(
          shiny::column(
            width = 5,
            shiny::selectizeInput(
              "soundscape_file", "Soundscape (0 of 0)",
              choices = NULL, width = "100%",
              options = list(
                maxOptions = 10000,
                render = I('{
                  option: function(item, escape) {
                    // UIX-05a: item.label is the HTML status <span> built by
                    // .build_soundscape_choices(); escape() would show the raw
                    // markup (the color code) instead of rendering it.
                    return "<div>" + item.label + "</div>";
                  },
                  item: function(item, escape) {
                    return "<div>" + item.label + "</div>";
                  }
                }')
              )
            )
          ),
          shiny::column(
            width = 4,
            shiny::selectizeInput(
              "roi_label_list", "Available ROI label lists",
              choices = colnames(roi_label_lists),
              selected = roi_label_list, width = "100%"
            )
          ),
          shiny::column(
            width = 3,
            # Channel dropdown (L/R for <=2 ch, 1..N for multichannel -- choices
            # refreshed per loaded recording) + the HTML-player enable toggle to
            # its right.
            shiny::div(
              style = "display:flex; gap:10px; align-items:flex-end;",
              shiny::div(
                style = "flex:1; min-width:0;",
                shiny::selectInput(
                  "display_channel", "Channel",
                  choices = c(L = "left", R = "right"),
                  selected = "left", width = "100%"
                )
              ),
              shiny::div(
                style = "flex:0 0 auto; margin-bottom:8px;",
                shiny::checkboxInput(
                  "enable_player", "Show audio player", value = TRUE
                )
              )
            )
          )
        ),

        # Time + Frequency sliders (same row)
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::div(
              style = "display: flex; align-items: center; gap: 8px; padding: 0 5px;",
              shiny::strong("Time (s)", style = "flex-shrink: 0; white-space: nowrap;"),
              shiny::div(
                style = "flex: 1;",
                shiny::sliderInput(
                  "zoom_time", label = NULL,
                  min = 0, max = 0.1, step = 0.05,
                  value = c(0, 0.1), width = "100%",
                  ticks = FALSE
                )
              )
            )
          ),
          shiny::column(
            width = 6,
            shiny::div(
              style = "display: flex; align-items: center; gap: 8px; padding: 0 5px;",
              shiny::strong("Freq (kHz)", style = "flex-shrink: 0; white-space: nowrap;"),
              shiny::div(
                style = "flex: 1;",
                shiny::sliderInput(
                  "zoom_freq_slider", label = NULL,
                  min = 0, max = .MAX_ZOOM_FREQ_KHZ, step = 0.1,
                  value = session_data$zoom_freq,
                  width = "100%", ticks = FALSE
                )
              )
            )
          )
        ),

        # Spectrogram + audio (resizable)
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shinyjqui::jqui_resizable(
              shiny::div(
                id = "spectro_panel",
                style = "height: 500px;",
                shiny::plotOutput("spectrogram_plot",
                  brush = "roi_limits", click = "roi_select", height = "100%"
                ),
                shiny::div(
                  class = "audio-container",
                  shiny::uiOutput("audio_player")
                )
              ),
              options = list(
                handles = "s",
                minHeight = 300,
                maxHeight = 1200
              )
            )
          )
        ),

        # Navigation buttons
        shiny::fluidRow(
          style = "margin-top: 5px; margin-bottom: 5px;",
          shiny::column(1,
            shiny::div(
              style = "display: flex; align-items: center; height: 54px;",
              shiny::checkboxInput(
                "nav_autosave", "Autosave",
                value = session_data$nav_autosave
              )
            )
          ),
          # UIX-06: hotkey hints on the buttons themselves. The LSA has ~30
          # bindings that were only discoverable in the "User Manual" tab; the
          # LVA already appends the key to the label ("TP (Q)"), so adopt that
          # convention here. Buttons with no binding get a plain `title` tooltip
          # rather than a misleading key hint.
          shiny::column(2,
            shiny::actionButton("prev_soundscape_noroi", "Prev unsegmented",
              width = "100%", icon = shiny::icon("fast-backward"),
              class = "btn-info",
              title = "Jump back to the previous soundscape with no ROIs yet"
            )
          ),
          shiny::column(2,
            shiny::actionButton("prev_soundscape", "Prev (Z)",
              width = "100%", icon = shiny::icon("step-backward"),
              class = "btn-primary",
              title = "Previous soundscape - hotkey: Z"
            )
          ),
          shiny::column(2,
            shiny::actionButton("next_soundscape", "Next (C)",
              width = "100%", icon = shiny::icon("step-forward"),
              class = "btn-primary",
              title = "Next soundscape - hotkey: C"
            )
          ),
          shiny::column(2,
            shiny::actionButton("next_soundscape_noroi", "Next unsegmented",
              width = "100%", icon = shiny::icon("fast-forward"),
              class = "btn-info",
              title = "Jump forward to the next soundscape with no ROIs yet"
            )
          ),
          shiny::column(2,
            shiny::actionButton("no_soi", "No signals of interest (Alt+N)",
              width = "100%", icon = shiny::icon("times"),
              class = "btn-warning",
              title = "Mark this soundscape as having no signal of interest - hotkey: Alt+N"
            )
          ),
          shiny::column(1,
            shiny::actionButton("play_soundscape", "Play",
              width = "100%", icon = shiny::icon("play"),
              class = "btn-success",
              title = "Play the visible selection"
            )
          )
        )
      ),

      # Tab box: Setup, ROI table, User Manual, Metadata
      shinydashboard::tabBox(
        width = 12, id = "tabset1",

        # Tab 1: Setup and Input
        shiny::tabPanel(
          "Setup and Input",
          icon = shiny::icon("pen-to-square"),
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::selectizeInput(
                "label_name", "Label",
                choices = NULL, selected = NULL, width = "100%"
              )
            ),
            shiny::column(
              width = 3,
              shiny::selectizeInput(
                "signal_type", "Type",
                choices = roi_type_choices,
                options = list(
                  placeholder = "ex.: song",
                  onInitialize = I('function() { this.setValue(""); }'),
                  "create" = TRUE, "persist" = FALSE
                ),
                width = "100%"
              )
            ),
            shiny::column(
              width = 2,
              shiny::fluidRow(
                shiny::column(9,
                  shiny::selectizeInput("label_certainty", "Certainty",
                    choices = c("certain", "uncertain"), selected = "certain",
                    width = "100%"
                  )
                ),
                shiny::column(3,
                  # UIX-13: the label is only a padlock icon, so what locking
                  # does was learnable by experiment alone. Native tooltip.
                  title = "Lock: keep this Certainty value when moving to the next ROI",
                  shiny::checkboxInput("lock_label_certainty",
                    label = shiny::icon("lock"), value = FALSE
                  )
                )
              )
            ),
            shiny::column(
              width = 2,
              shiny::fluidRow(
                shiny::column(9,
                  shiny::selectizeInput("signal_is_complete", "Complete",
                    choices = c("complete", "incomplete"), selected = "complete",
                    width = "100%"
                  )
                ),
                shiny::column(3,
                  title = "Lock: keep this Complete value when moving to the next ROI",  # UIX-13
                  shiny::checkboxInput("lock_is_complete",
                    label = shiny::icon("lock"), value = FALSE
                  )
                )
              )
            )
          ),
          # UIX-26: the comment field sits on its own full-width row below the
          # annotation fields. Its previous home (a 10/12 slice inside a
          # width-2 column) rendered as a thin sliver that read as a filename
          # note; the hotkey-driven flow (draw box -> Alt+S) never passed by
          # it. Full width makes the field visible and invites free-text notes.
          shiny::fluidRow(
            shiny::column(10,
              shiny::textInput("label_comment", "Comment",
                value = "",
                placeholder = "e.g. doubts about the identification",
                width = "100%"
              )
            ),
            shiny::column(2,
              title = "Lock: keep this Comment when moving to the next ROI",  # UIX-13
              shiny::checkboxInput("lock_comment",
                label = shiny::icon("lock"), value = FALSE
              )
            )
          )
        ),

        # Tab 2: Detections overlay (LSA-208; moved to the bottom panel at
        # the gate review 2026-08-18). "(all)" is the sentinel value for the
        # unfiltered template dropdown -- an empty-string option value gets
        # eaten by select/updateSelectInput round trips, making "All" vanish
        # after a selection (gate feedback).
        shiny::tabPanel(
          "Detections",
          icon = shiny::icon("bullseye"),
          shiny::fluidRow(
            style = "margin-top: 10px;",
            shiny::column(
              4,
              shiny::checkboxInput(
                "show_detections", "Show detections (read-only overlay)",
                value = FALSE, width = "100%"
              )
            ),
            shiny::column(
              4,
              shiny::sliderInput(
                "det_score_min", "Min peak score",
                min = 0, max = 1, value = 0, step = 0.01, width = "100%"
              )
            ),
            shiny::column(
              4,
              shiny::selectInput(
                "det_template", "Template",
                choices = c("All templates" = "(all)"),
                selected = "(all)", width = "100%"
              )
            )
          )
        ),

        # Tab 3: ROI table
        shiny::tabPanel(
          "ROI table",
          icon = shiny::icon("table"),
          shiny::fluidRow(
            style = "margin-top: 10px; margin-bottom: 10px;",
            # UIX-06: Ctrl+E saves the ROI table; the other two have no binding
            # (the Q/Del hotkeys delete the LAST ROI, not the table selection),
            # so they carry a descriptive tooltip instead of a key hint.
            shiny::column(4,
              shiny::actionButton("save_roi", "Save (Ctrl+E)",
                icon = shiny::icon("save"), width = "100%", class = "btn-primary",
                title = "Save the ROI table to the store - hotkey: Ctrl+E"
              )
            ),
            shiny::column(4,
              shiny::actionButton("export_selected_cut", "Export audio cut",
                icon = shiny::icon("music"), width = "100%",
                title = "Export the selected ROI as a WAV cut"
              )
            ),
            shiny::column(4,
              shiny::actionButton("delete_selected_rois", "Delete selected",
                icon = shiny::icon("trash"), width = "100%", class = "btn-danger",
                title = paste0("Delete the rows selected in the table ",
                  "(the Q / Del hotkeys remove the last ROI instead)")
              )
            )
          ),
          DT::DTOutput("res_table")
        ),

        # Tab 4: User Manual
        shiny::tabPanel(
          "User Manual",
          icon = shiny::icon("book"),
          shiny::tags$div(
            style = "padding: 15px;",
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shiny::tags$h5("Keyboard Shortcuts"),
                shiny::tags$ul(
                  shiny::tags$li("Q / Delete - Delete the last created ROI"),
                  shiny::tags$li("W - Zoom in on time axis"),
                  shiny::tags$li("S - Zoom out on time axis"),
                  shiny::tags$li("A - Navigate backwards in time"),
                  shiny::tags$li("D - Navigate forward in time"),
                  shiny::tags$li("E / Enter - Store current selection as ROI"),
                  shiny::tags$li("R - Activate/deactivate measurement tool"),
                  shiny::tags$li("Z - Previous soundscape"),
                  shiny::tags$li("C - Next soundscape"),
                  shiny::tags$li("G - Zoom in on frequency axis"),
                  shiny::tags$li("T - Zoom out on frequency axis"),
                  shiny::tags$li("V - Navigate up in frequency"),
                  shiny::tags$li("F - Navigate down in frequency"),
                  shiny::tags$li("Alt+W - Reset zoom (time + frequency)"),
                  shiny::tags$li("Alt+S - Reset time zoom"),
                  shiny::tags$li("Ctrl+E - Save ROI table to database"),
                  shiny::tags$li("Alt+K - Confirm/refresh paths setup"),
                  shiny::tags$li("Alt+N - Mark as no signals of interest"),
                  shiny::tags$li("Spacebar - Play/pause audio"),
                  shiny::tags$li(
                    paste(
                      "X - Toggle brush-guided zoom: draw a box and press X to zoom",
                      "in; X again restores the previous view (no box = no-op)"
                    )
                  ),
                  shiny::tags$li(
                    paste(
                      "Alt+P - Promote the selected detection (Detections tab",
                      "overlay) to an editable ROI; a one-per-session popup",
                      "asks for confirmation the first time"
                    )
                  )
                )
              ),
              shiny::column(
                width = 4,
                shiny::tags$h5("Mouse"),
                shiny::tags$ul(
                  shiny::tags$li(
                    "Left-drag on the spectrogram - Draw a new ROI selection"
                  ),
                  shiny::tags$li(
                    paste(
                      "Left-click on a ROI - Select it as the active ROI and copy",
                      "its label/type/certainty/completeness/comment into the input",
                      "fields for reuse on the next ROI; click empty space to clear"
                    )
                  ),
                  shiny::tags$li(
                    paste(
                      "Left-click on a detection (overlay ON, Detections tab) -",
                      "Select it as the detection to promote with Alt+P"
                    )
                  )
                )
              ),
              shiny::column(
                width = 4,
                shiny::tags$h5("Active ROI (left-click to select)"),
                shiny::tags$ul(
                  shiny::tags$li("Shift+W / Shift+S - Expand / contract in time"),
                  shiny::tags$li("Shift+G / Shift+T - Expand / contract in frequency"),
                  shiny::tags$li("Shift+E / Shift+Q - Expand / contract in both axes"),
                  shiny::tags$li("Alt+T - Expand to the full recording duration"),
                  shiny::tags$li("Alt+G - Expand to the full frequency band"),
                  shiny::tags$li("Alt+B - Expand to full time and frequency"),
                  shiny::tags$li(
                    "U - Undo the last bounds change (snap / resize / re-selection)"
                  ),
                  shiny::tags$li(
                    paste(
                      "P - Open the metadata popup: writes to the active ROI if one",
                      "is selected, otherwise fills the input fields for the next ROI"
                    )
                  ),
                  shiny::tags$li(
                    paste(
                      "B - Toggle boundary re-selection: with a ROI active, draw the",
                      "new bounds and press E to commit them (border turns red/dashed;",
                      "B again to cancel)"
                    )
                  ),
                  shiny::tags$li(
                    "Resizes edit the active ROI live; autosave / Ctrl+E persists them"
                  )
                )
              )
            )
          )
        ),

        # Tab 4: Soundscape metadata (NEW)
        shiny::tabPanel(
          "Metadata",
          icon = shiny::icon("circle-info"),
          shiny::tags$div(
            style = "padding: 15px;",
            shiny::tableOutput("soundscape_metadata")
          )
        )
      ),

      # Spacebar audio control via JavaScript
      shiny::tags$script("
        $(document).on('keydown', function(e) {
          if (e.target.tagName === 'INPUT' || e.target.tagName === 'SELECT') return true;
          if (e.key === ' ' || e.key === 'Spacebar') {
            e.preventDefault();
            var audioPlayer = document.querySelector('audio');
            if (audioPlayer) {
              if (audioPlayer.paused) { audioPlayer.play(); }
              else { audioPlayer.pause(); }
            }
          }

        });
      "),

      # Progress bar in the footer (moved from the header; UIX-14a position)
      shiny::tags$div(
        id = "progress_bar_wrap",
        style = "margin: 8px 15px;",
        shinyWidgets::progressBar(
          id = "progress_bar", value = 0, total = 1,
          status = "info", display_pct = TRUE, striped = TRUE
        )
      )
    ),
    skin = "black"
  )

  # --- Server (identical to launch_segmentation_app) -------------------------

  server <- function(input, output, session) {

    # -- Reactive values ------------------------------------------------------

    user_val <- shiny::reactiveVal(NULL)
    soundscape_data <- shiny::reactiveVal(NULL)
    soundscape_path_val <- shiny::reactiveVal(NULL)
    # LSA-203: the pre-validated launch-time df_soundscapes (NULL in directory
    # mode); the table-mode source when df_soundscapes_path is left empty.
    launch_table_store <- shiny::reactiveVal(validated_launch_table)
    templates_path_val <- shiny::reactiveVal(NULL)
    roi_db_val <- shiny::reactiveVal(NULL)
    roi_con <- shiny::reactiveVal(NULL)
    rec_soundscape <- shiny::reactiveVal(NULL)
    duration_val <- shiny::reactiveVal(NULL)
    wav_path_val <- shiny::reactiveVal(NULL)
    base_spectrogram <- shiny::reactiveVal(NULL)
    roi_values <- shiny::reactiveVal(NULL)
    active_roi_id <- shiny::reactiveVal(NULL)
    reselect_mode <- shiny::reactiveVal(FALSE)
    zoom_home <- shiny::reactiveVal(NULL)
    # A2: holds the readWave error message for the current soundscape (NULL
    # when the recording loaded fine); drives the in-plot failure message.
    load_error <- shiny::reactiveVal(NULL)
    # A5: single-level undo snapshot of the active ROI's bounds (NULL = none)
    # captured before each bounds mutation (snap / resize / reselect).
    roi_bounds_undo <- shiny::reactiveVal(NULL)
    rois_changed <- shiny::reactiveVal(FALSE)
    # UIX-23(a): per-session label usage counter, so the label_name
    # autocomplete ranks by recency/frequency instead of the static xlsx order.
    label_usage <- shiny::reactiveValues()
    .rank_labels_by_usage <- function(labels) {
      labs <- unique(labels[!is.na(labels)])
      cnt <- vapply(labs, function(l) {
        v <- label_usage[[l]]
        if (is.null(v)) 0L else v
      }, integer(1))
      labs[order(-cnt, labs)]
    }
    ruler <- shiny::reactiveVal(NULL)
    progress_tracker <- shiny::reactiveValues(df = NULL)
    # CRAN-103: workspace root for canonical (workspace-relative)
    # soundscape_path keys at save time. Mirrors the app's output anchoring:
    # project_path when set, else the working directory.
    ws_root <- function() {
      p <- session_data$project_path
      if (is.null(p) || length(p) == 0L || is.na(p)) "." else p
    }
    # LSA-208: detection overlay state -- all detection-class rows of the
    # current soundscape (signals-shaped), the selected detection's signal_id
    # (NA = none), and the one-per-session orientation-popup flag.
    detection_values <- shiny::reactiveVal(NULL)
    selected_detection <- shiny::reactiveVal(NA_character_)
    promotion_oriented <- shiny::reactiveVal(FALSE)

    # -- Spectrogram cache (PERF: B) -------------------------------------------
    # In-memory cache of SpectroResult + pre-rendered ggplot objects, keyed by
    # (wav_path, wl, ovlp, pitch_shift, channel). Populated on first visit and
    # by background prefetch of adjacent files. Cuts STFT recomputation on
    # revisit and on prev/next navigation when params are unchanged.
    .spec_cache <- new.env(parent = emptyenv())
    # UIX-22(a): the prefetch DISPATCH is idle-gated -- it only starts after
    # .prefetch_idle_secs without any input event, retrying every
    # .prefetch_retry_secs, so prefetch never starts while the user draws ROIs
    # on the freshly loaded file.
    # UIX-22(b)/B8: the prefetch COMPUTE runs on a mirai daemon (off the main
    # R thread), mirroring the validation app's proven pattern. Fallback when
    # daemons are unavailable (no mirai / no trusted sources / cores = 0):
    # the previous idle-gated main-thread compute.
    .prefetch_idle_secs <- 3
    .prefetch_retry_secs <- 0.5
    .input_clock <- new.env(parent = emptyenv())
    .input_clock$t <- Sys.time()
    shiny::observe({
      shiny::reactiveValuesToList(input)
      .input_clock$t <- Sys.time()
    })
    .prefetch_state <- new.env(parent = emptyenv())
    .prefetch_state$ready <- FALSE
    .prefetch_cores <- function() {
      as.integer(getOption("monitoraSom.lsa_prefetch_cores", default = 2L))
    }
    .prefetch_src_dir <- function() {
      d <- getOption(
        "monitoraSom.lsa_src_dir",
        default = getOption("monitoraSom.src_dir", default = NULL)
      )
      if (!is.null(d) && dir.exists(d)) d else NULL
    }
    # AS-02 (audit triage): single source of truth for the files pre-sourced
    # into mirai workers. Add new files here, never at the sys.source site.
    .prefetch_sources <- "fast_spectro.R"
    # Set up + prime the daemon once per session (lazy). Workers source
    # fast_spectro.R so they resolve extract_spectro(). Returns TRUE when the
    # off-thread path is usable; FALSE = caller falls back to main-thread.
    .prefetch_ensure_daemons <- function() {
      if (isTRUE(.prefetch_state$ready)) return(TRUE)
      if (.prefetch_cores() < 1L || !requireNamespace("mirai", quietly = TRUE)) {
        return(FALSE)
      }
      src <- .prefetch_src_dir()
      if (is.null(src)) return(FALSE)  # no trusted sources -> main-thread only
      ok <- tryCatch(
        {
          mirai::daemons(.prefetch_cores())
          fls <- file.path(src, .prefetch_sources)
          mirai::everywhere(
            {
              for (.f in .fls) sys.source(.f, envir = globalenv())
            },
            .fls = fls
          )
          TRUE
        },
        error = function(e) FALSE
      )
      if (!ok) {
        .prefetch_teardown()
        return(FALSE)
      }
      .prefetch_state$ready <- TRUE
      TRUE
    }
    # AS-02 (audit triage): shutdown failures were swallowed silently; a
    # leaked daemon pool is invisible without this console warning.
    .prefetch_teardown <- function() {
      tryCatch(
        mirai::daemons(0L),
        error = function(e) {
          warning(
            "prefetch daemon shutdown failed: ", conditionMessage(e),
            call. = FALSE
          )
        }
      )
    }
    # Compute one spectrogram off-thread and store it in the cache on arrival.
    .prefetch_mirai <- function(wav, wl, ovlp, ps, ch) {
      if (!.prefetch_ensure_daemons()) return(FALSE)
      m <- mirai::mirai(
        {
          w <- tuneR::readWave(.wav)
          extract_spectro(
            w, wl = .wl, ovlp = .ovl, pitch_shift = .ps,
            channel = .ch, use_fftw = TRUE
          )
        },
        .wav = wav, .wl = wl, .ovl = ovlp, .ps = ps, .ch = ch
      )
      kk <- .spec_cache_key(wav, wl, ovlp, ps, ch)
      promises::then(
        promises::as.promise(m),
        onFulfilled = function(spec) {
          if (!is.null(spec) && !mirai::is_error_value(spec)) {
            assign(kk, spec, envir = .spec_cache)
          }
        },
        onRejected = function(e) NULL
      )
      TRUE
    }
    .spec_cache_key <- function(wav, wl, ovlp, ps, ch) {
      paste(wav, wl, ovlp, ps, ch, sep = "|")
    }
    .spec_cache_get <- function(wav, wl, ovlp, ps, ch) {
      key <- .spec_cache_key(wav, wl, ovlp, ps, ch)
      if (exists(key, envir = .spec_cache, inherits = FALSE)) {
        return(get(key, envir = .spec_cache, inherits = FALSE))
      }
      NULL
    }
    .spec_prefetch_later <- function(wav, wl, ovlp, ps, ch) {
      if (is.null(wav) || !is.character(wav) || !file.exists(wav)) return()
      if (!is.null(.spec_cache_get(wav, wl, ovlp, ps, ch))) return()
      .dispatch_prefetch <- function() {
        later::later(function() {
          idle_for <- as.numeric(
            Sys.time() - .input_clock$t, units = "secs"
          )
          if (idle_for < .prefetch_idle_secs) {
            .dispatch_prefetch()
            return()
          }
          # UIX-22(b)/B8: off-thread when daemons are ready; the idle-gated
          # main-thread compute (A5) stays as the fallback.
          if (.prefetch_mirai(wav, wl, ovlp, ps, ch)) return()
          tryCatch({
            w <- tuneR::readWave(wav)
            spec <- extract_spectro(w, wl = wl, ovlp = ovlp,
                                    pitch_shift = ps, channel = ch,
                                    use_fftw = TRUE)
            assign(.spec_cache_key(wav, wl, ovlp, ps, ch),
                   spec, envir = .spec_cache)
          }, error = function(e) NULL)
        }, delay = .prefetch_retry_secs)
      }
      .dispatch_prefetch()
    }

    # Coerce sliderTextInput values to their declared types. shinyWidgets
    # returns values with the same type as the choices vector (numeric here),
    # but an explicit coercion guards against any future API changes.
    wl_input <- shiny::reactive(as.integer(input$wl))
    pitch_shift_input <- shiny::reactive(as.numeric(input$pitch_shift))

    # UIX-17: debounced views of the drag-heavy sliders.
    #
    # Shiny sliders emit on every drag tick, and each tick re-runs the ggplot
    # build + raster draw (and, for dyn_range/font_scale, the whole
    # plot_spectro() call). On a large spectrogram that makes dragging feel
    # sticky and burns CPU on frames the user never sees.
    #
    # These debounced reactives are consumed ONLY by the render paths (the
    # spectrogram observe, renderPlot and the audio-player render). The
    # read-modify-write hotkey handlers (handle_zoom_in_time and friends) must
    # keep reading `input$...` directly: they read the current value and
    # immediately updateSliderInput() from it, so feeding them a delayed value
    # would make rapid key presses compound off a stale viewport.
    #
    # 250 ms for view-only params; STFT-changing params are not debounced here
    # because they are already gated by the spectrogram cache.
    zoom_time_d  <- shiny::debounce(shiny::reactive(input$zoom_time), 250)
    zoom_freq_d  <- shiny::debounce(shiny::reactive(input$zoom_freq_slider), 250)
    dyn_range_d  <- shiny::debounce(shiny::reactive(input$dyn_range), 250)
    font_scale_d <- shiny::debounce(shiny::reactive(input$font_scale), 250)

    # LSA-208: immediate feedback on the overlay toggle -- the plot redraw
    # alone was not obviously attributable to the checkbox (boxes are thin
    # dashed outlines), so confirm the state (and the filtered count) in a
    # toast.
    shiny::observeEvent(input$show_detections, {
      if (isTRUE(input$show_detections)) {
        n <- NROW(overlay_detections())
        shiny::showNotification(
          if (n > 0) sprintf("Detection overlay ON: %d shown (after filters)", n)
          else paste("Detection overlay ON: no detections for this soundscape",
                     "(or all filtered out)"),
          type = "message", duration = 4
        )
      }
    })

    # LSA-208: the overlay frame -- detections of the current soundscape that
    # pass the tab's filters (toggle, min score, template). The overlay
    # drawing and the click/promotion hit-tests consume this single view, so
    # what is drawn is exactly what is selectable.
    overlay_detections <- shiny::reactive({
      if (!isTRUE(input$show_detections)) return(NULL)
      dets <- detection_values()
      if (is.null(dets) || nrow(dets) == 0) return(NULL)
      keep <- rep(TRUE, nrow(dets))
      thr <- input$det_score_min
      if (!is.null(thr) && !is.na(thr)) {
        keep <- dets$det_peak_score >= thr
      }
      tmpl <- input$det_template
      if (!is.null(tmpl) && nzchar(tmpl) && !identical(tmpl, "(all)")) {
        keep <- keep & dets$det_template_name == tmpl
      }
      dets[which(keep), , drop = FALSE]
    })

    # -- Path confirmation / Setup --------------------------------------------

    # UIX-09(a): per-field existence feedback (green check / red border) on
    # blur, so path errors surface before "Confirm Paths" instead of only in
    # the setup modal. shinyjs classes; no new dependency.
    .path_exists <- function(x) {
      is.character(x) && length(x) == 1L && !is.na(x) &&
        nzchar(trimws(x)) && (file.exists(x) || dir.exists(x))
    }
    shiny::observe({
      for (id in c("df_soundscapes_path", "soundscapes_path", "roi_db",
                   "templates_path")) {
        val <- input[[id]]
        if (is.null(val) || !nzchar(trimws(val))) {
          shinyjs::removeClass(id, "path-ok")
          shinyjs::removeClass(id, "path-bad")
          next
        }
        if (.path_exists(val)) {
          shinyjs::addClass(id, "path-ok")
          shinyjs::removeClass(id, "path-bad")
        } else {
          shinyjs::addClass(id, "path-bad")
          shinyjs::removeClass(id, "path-ok")
        }
      }
    })

    shiny::observeEvent(input$user_setup_confirm, {
      # LSA-203: the lock checkbox selects the mode. Table mode needs a table
      # source, not a soundscapes directory; only the directory branch requires
      # (and existence-checks) soundscapes_path.
      table_mode <- isTRUE(input$table_lock)
      # A blank ROI user used to silently halt the setup observer via req(),
      # which reads as the whole app "freezing" with no data loaded. Warn the
      # user explicitly instead (the name is stamped on every saved ROI).
      if (is.null(input$roi_user) || is.na(input$roi_user) ||
          !nzchar(trimws(input$roi_user))) {
        shiny::showModal(shiny::modalDialog(
          title = "User name required",
          paste(
            "Please enter a user name before confirming the setup -- it is",
            "stamped on every ROI you save."
          ),
          footer = shiny::tagList(shiny::modalButton("OK")), easyClose = TRUE
        ))
        return()
      }
      if (table_mode) {
        shiny::req(input$roi_db)
      } else {
        shiny::req(input$soundscapes_path, input$roi_db)
        if (!dir.exists(input$soundscapes_path)) {
          shiny::showModal(shiny::modalDialog(
            title = "Setup error", "The soundscapes path does not exist.",
            footer = shiny::tagList(shiny::modalButton("OK")), easyClose = TRUE
          ))
          return()
        }
      }

      user_val(input$roi_user)
      if (!table_mode) soundscape_path_val(input$soundscapes_path)
      roi_db_val(input$roi_db)
      if (!is.null(input$templates_path) && input$templates_path != "" &&
          !is.na(input$templates_path)) {
        templates_path_val(input$templates_path)
      }

      # LSA-209 (F5): connect is create-if-missing, so a typo'd or wrong-cwd
      # roi_db silently starts a NEW empty store -- every recording then shows
      # unsegmented with no error, which reads as data loss. Warn instead.
      if (!is.null(input$roi_db) && nzchar(input$roi_db) &&
          !file.exists(input$roi_db)) {
        shiny::showNotification(
          sprintf(paste0("roi_db '%s' does not exist yet; ",
                         "a new empty store will be created there."),
                  input$roi_db),
          type = "warning", duration = 15
        )
      }

      # Open the DuckDB ROI store (create-if-missing happens in connect;
      # LSA-116/12). LSA-01: capture the outcome into a sentinel and branch in
      # the observer body. A bare return() inside the error handler only
      # returns from the anonymous error function, so setup would otherwise
      # continue (dir_ls, status classification, ...) on an unopened DB and
      # raise a second, confusing error.
      con <- tryCatch(
        {
          old <- roi_con()
          if (!is.null(old)) {
            try(DBI::dbDisconnect(old, shutdown = TRUE), silent = TRUE)
          }
          .signals_duckdb_connect(input$roi_db)
        },
        error = function(e) {
          shiny::showModal(shiny::modalDialog(
            title = "Database error",
            paste("Failed to initialize database:", e$message),
            footer = shiny::tagList(shiny::modalButton("OK")), easyClose = TRUE
          ))
          NULL
        }
      )
      if (is.null(con)) return()
      roi_con(con)

      # Build the navigation universe. LSA-203 table mode: use the supplied /
      # loaded df_soundscapes rows verbatim (order preserved, no sort). Directory
      # mode: discover + deterministic sort (LSA-25, matching
      # .discover_soundscapes() in fetch_soundscape_metadata) so the navigation
      # order is the metadata-table order regardless of traversal order.
      if (table_mode) {
        # Source: an in-app path (takes precedence, loads a store) or the
        # pre-validated launch table when the path field is empty.
        path_in <- input$df_soundscapes_path
        if (!is.null(path_in) && nzchar(trimws(path_in))) {
          raw_table <- tryCatch(
            .read_soundscape_table(trimws(path_in)),
            error = function(e) {
              shiny::showModal(shiny::modalDialog(
                title = "Setup error",
                paste("Could not load the metadata table:",
                      conditionMessage(e)),
                footer = shiny::tagList(shiny::modalButton("OK")),
                easyClose = TRUE
              ))
              NULL
            }
          )
          if (is.null(raw_table)) return()
        } else if (!is.null(launch_table_store())) {
          raw_table <- launch_table_store()
        } else {
          shiny::showModal(shiny::modalDialog(
            title = "Setup error",
            paste(
              "Table-guided mode is active but no table was supplied.",
              "Provide a path to a saved metadata table, or uncheck the lock",
              "to use a soundscapes directory."
            ),
            footer = shiny::tagList(shiny::modalButton("OK")), easyClose = TRUE
          ))
          return()
        }
        # Validate: hard error -> modal; dedup warning -> notification (Q1/Q3).
        soundscape_df <- tryCatch(
          withCallingHandlers(
            .validate_soundscape_table(raw_table),
            warning = function(w) {
              shiny::showNotification(conditionMessage(w), type = "warning")
              invokeRestart("muffleWarning")
            }
          ),
          error = function(e) {
            shiny::showModal(shiny::modalDialog(
              title = "Setup error",
              paste("Invalid metadata table:", conditionMessage(e)),
              footer = shiny::tagList(shiny::modalButton("OK")),
              easyClose = TRUE
            ))
            NULL
          }
        )
        if (is.null(soundscape_df)) return()
        # Q4: table mode locks the path fields.
        shinyjs::disable("soundscapes_path")
        shinyjs::disable("df_soundscapes_path")
      } else {
        shinyjs::enable("soundscapes_path")
        shinyjs::enable("df_soundscapes_path")
        # LSA-209: discovery + canonical keys via the pure helper (normalized
        # like every other soundscape_path producer; a raw trailing-slash or
        # "./" key would miss the store on every status/load/save join).
        soundscape_df <- tryCatch(
          .discover_soundscapes_df(input$soundscapes_path),
          error = function(e) {
            shiny::showModal(shiny::modalDialog(
              title = "Setup error",
              paste("No WAV files found in the soundscapes path:",
                    conditionMessage(e)),
              footer = shiny::tagList(shiny::modalButton("OK")), easyClose = TRUE
            ))
            NULL
          }
        )
        if (is.null(soundscape_df)) return()
      }

      # Classify by durable path (LSA-19), not basename.
      soundscape_df$status <- .signals_duckdb_statuses(
        roi_con(), soundscape_df$soundscape_path
      )
      soundscape_df$has_table <- soundscape_df$status != "unsegmented"

      soundscape_data(soundscape_df)
      progress_tracker$df <- soundscape_df

      shiny::updateSelectizeInput(
        session, "soundscape_file",
        choices = .build_soundscape_choices(soundscape_df),
        server = TRUE
      )

      n_done <- sum(soundscape_df$has_table)
      shinyWidgets::updateProgressBar(
        session, "progress_bar", value = n_done, total = nrow(soundscape_df)
      )
      shiny::showNotification("Setup successful!", type = "message")

      if (!is.null(templates_path_val()) && dir.exists(templates_path_val())) {
        shinyjs::enable("export_selected_cut")
      } else {
        shinyjs::disable("export_selected_cut")
      }
    })

    if (skip_path_confirmation) {
      shinyjs::click("user_setup_confirm")
    }

    # LSA-203/Q4: the lock disables the path fields while table mode is active
    # and re-enables them when unchecked (mode is re-derived on the next
    # "Confirm Paths"). Runs at init so an argument-supplied table starts locked.
    shiny::observeEvent(input$table_lock, {
      if (isTRUE(input$table_lock)) {
        shinyjs::disable("soundscapes_path")
        shinyjs::disable("df_soundscapes_path")
      } else {
        shinyjs::enable("soundscapes_path")
        shinyjs::enable("df_soundscapes_path")
      }
    }, ignoreInit = FALSE)

    # -- Label list management ------------------------------------------------

    shiny::observe({
      shiny::updateSelectizeInput(
        session,
        "roi_label_list",
        choices = colnames(roi_label_lists),
        selected = session_data$roi_label_list,
        server = TRUE
      )
    })

    shiny::observeEvent(input$roi_label_list, {
      shiny::req(input$roi_label_list)
      label_choices <- dplyr::pull(roi_label_lists, input$roi_label_list)
      # UIX-23(a): rank by recency/frequency of use this session.
      label_choices <- .rank_labels_by_usage(label_choices)
      shiny::updateSelectizeInput(
        session,
        "label_name",
        choices = c(NA, label_choices),
        selected = NULL,
        server = TRUE,
        options = list(
          placeholder = "Input the ROI label here",
          onInitialize = I('function() { this.setValue(""); }'),
          "create" = TRUE,
          "persist" = FALSE
        )
      )
    })

    # -- Soundscape loading ---------------------------------------------------

    shiny::observe({
      shiny::req(soundscape_data(), input$soundscape_file)
      wav_data <- soundscape_data()
      current_index <- which(wav_data$soundscape_path == input$soundscape_file)
      if (length(current_index) == 0) {
        return()
      }

      wav_path <- wav_data$soundscape_path[current_index]
      if (
        is.null(wav_path) || length(wav_path) != 1 || !file.exists(wav_path)
      ) {
        return()
      }

      tryCatch(
        {
          wav_obj <- tuneR::readWave(wav_path)
          wav_duration <- seewave::duration(wav_obj)
          # A2: some intentionally malformed WAVs read without error but carry
          # zero audio samples (duration 0); they cannot produce a spectrogram.
          # Treat them as a load failure with a clear message rather than
          # letting fast_spectro throw a cryptic error downstream.
          if (length(wav_obj@left) == 0 || !is.finite(wav_duration) ||
              wav_duration <= 0) {
            rec_soundscape(NULL)
            base_spectrogram(NULL)
            load_error("the recording contains no audio samples")
            return()
          }

          # Initial time window: a configured zoom_time (clamped to the
          # recording) takes precedence; otherwise the first 60 s.
          zt <- session_data$zoom_time
          zoom_window <- if (is.null(zt)) {
            c(0, min(wav_duration, 60))
          } else {
            w <- c(max(0, min(zt[1], wav_duration)),
                   min(wav_duration, max(zt[2], 0)))
            if (w[1] >= w[2]) c(0, min(wav_duration, 60)) else w
          }
          shiny::updateSliderInput(
            session, "zoom_time", min = 0, max = wav_duration,
            value = zoom_window
          )
          shiny::updateSelectizeInput(
            session, "soundscape_file",
            label = sprintf(
              "Soundscape (%d of %d)", current_index, nrow(wav_data)
            )
          )
          nyquist_khz <- wav_obj@samp.rate / 2000
          # Preserve the user's current frequency window, clamped to the new
          # Nyquist. isolate() prevents adding zoom_freq_slider as a reactive
          # dependency of this observer, which would cause a reload loop.
          prev_freq <- shiny::isolate(input$zoom_freq_slider)
          clamped_min <- max(0, min(prev_freq[1], nyquist_khz - 0.01))
          clamped_max <- min(nyquist_khz, max(prev_freq[2], 0.01))
          if (clamped_min >= clamped_max) {
            clamped_min <- 0
            clamped_max <- nyquist_khz
          }
          shiny::updateSliderInput(
            session, "zoom_freq_slider", min = 0, max = nyquist_khz,
            value = c(clamped_min, clamped_max)
          )

          duration_val(wav_duration)
          rec_soundscape(wav_obj)
          # Item-6: refresh the channel dropdown for this recording's channel
          # count, keeping the current selection when it is still valid.
          ch_choices <- .channel_choices(wav_obj)
          ch_sel <- if (!is.null(input$display_channel) &&
                        input$display_channel %in% ch_choices) {
            input$display_channel
          } else {
            ch_choices[[1]]
          }
          shiny::updateSelectInput(
            session, "display_channel", choices = ch_choices, selected = ch_sel
          )
          wav_path_val(wav_path)
          load_error(NULL)
        },
        error = function(e) {
          # A2: surface the failure both as a toast and as an in-plot message.
          # Clear the recording so a stale spectrogram from the previous file
          # is never shown for an unreadable one (e.g. intentionally malformed
          # WAVs in the test corpora).
          rec_soundscape(NULL)
          load_error(e$message)
          shiny::showNotification(
            sprintf("Error reading WAV file: %s", e$message), type = "error"
          )
        }
      )
    })

    # Load ROIs when soundscape changes
    shiny::observeEvent(
      input$soundscape_file,
      {
        shiny::req(input$soundscape_file, roi_con())

        # Save previous file's ROIs if changed (keyed by path, LSA-19).
        prev_rois <- roi_values()
        if (rois_changed() && !is.null(prev_rois) && nrow(prev_rois) > 0) {
          prev_path <- prev_rois$soundscape_path[1]
          if (!is.null(prev_path) && !is.na(prev_path)) {
            tryCatch(
              .signals_duckdb_save_rois(roi_con(),
                                        .rois_as_signals(prev_rois,
                                                          ws_root()),
                                        prev_path,
                                        workspace_root = ws_root()),
              error = function(e) {
                shiny::showNotification(
                  paste("Error saving previous ROIs:", e$message),
                  type = "warning"
                )
              }
            )
          }
        }

        # Load ROIs for the new selection (path key). LSA-208: promoted rows
        # (detection_to_roi) load as editable ROI-family rows too, projected
        # with roi_source = "detection" so the next save re-persists them as
        # detection_to_roi (never as plain roi).
        tryCatch(
          {
            sig_all <- rbind(
              .signals_duckdb_read(
                roi_con(), input$soundscape_file, .SIGNAL_CLASS_ROI),
              .signals_duckdb_read(
                roi_con(), input$soundscape_file,
                .SIGNAL_CLASS_DETECTION_TO_ROI)
            )
            new_rois <- .signals_as_rois(sig_all, include_detections = TRUE)
            # keep the legacy read order (by entry timestamp, .roi_duckdb_read)
            new_rois <- new_rois[order(new_rois$roi_input_timestamp), ,
                                 drop = FALSE]
            if (nrow(new_rois) > 0) {
              roi_values(new_rois)
            } else {
              roi_values(.schema_rois())
            }
            rois_changed(FALSE)
          },
          error = function(e) {
            shiny::showNotification(
              paste("Error loading ROIs:", e$message), type = "warning"
            )
            roi_values(.schema_rois())
          }
        )

        # LSA-208: load the detection-class rows of the new soundscape (same
        # signals store the app already has open) and reset the overlay
        # state. Detections persisted to a separate store are brought in
        # first with migrate_detections_store_to_signals() (see the docs).
        new_dets <- tryCatch(
          .signals_duckdb_read(
            roi_con(), input$soundscape_file, .SIGNAL_CLASS_DETECTION
          ),
          error = function(e) {
            shiny::showNotification(
              paste("Error loading detections:", e$message), type = "warning"
            )
            .schema_signals(0L)
          }
        )
        detection_values(new_dets)
        selected_detection(NA_character_)
        .update_detection_filters(new_dets)
      },
      priority = 1
    )

    # -- Spectrogram cache + render -------------------------------------------

    # Precompute spectrogram when core parameters change.
    # PERF: checks an in-memory cache before running the STFT (strategy B);
    # dispatches prefetch of adjacent files via later::later().
    shiny::observe({
      shiny::req(
        rec_soundscape(),
        wl_input(),
        input$ovlp,
        input$color_scale,
        length(dyn_range_d()) == 2,   # UIX-17 (debounced)
        length(pitch_shift_input()) == 1,
        length(input$time_guide_interval) == 1,
        length(input$freq_guide_interval) == 1
      )

      theme_mode <- if (
        identical(input$dark_mode, "dark") || isTRUE(input$dark_mode)
      ) {
        "dark"
      } else {
        "light"
      }
      ch <- if (is.null(input$display_channel)) "left" else input$display_channel

      tryCatch(
        {
          # PERF: check cache for pre-computed SpectroResult (strategy B).
          # The cache key covers the parameters that affect the STFT output.
          # Always runs extract_spectro (from cache or fresh), then renders
          # with plot_spectro -- avoids the double-STFT of calling fast_spectro
          # (which computes STFT internally) AND extract_spectro separately.
          wav <- wav_path_val()
          wl  <- wl_input()
          ov  <- input$ovlp
          ps  <- pitch_shift_input()
          cached <- .spec_cache_get(wav, wl, ov, ps, ch)

          if (!is.null(cached)) {
            spec <- cached
          } else {
            spec <- extract_spectro(
              rec = rec_soundscape(),
              ovlp = ov, wl = wl,
              pitch_shift = ps, channel = ch,
              use_fftw = TRUE
            )
            assign(.spec_cache_key(wav, wl, ov, ps, ch),
                   spec, envir = .spec_cache)
          }

          # Render with current display params (these change independently
          # of the STFT and must always re-render).
          p <- plot_spectro(
            spec,
            dyn_range = dyn_range_d(),          # UIX-17 (debounced)
            color_scale = input$color_scale,
            n_colors = 124,
            invert_colormap = input$invert_colormap,
            theme_mode = theme_mode,
            time_guide_interval = input$time_guide_interval,
            freq_guide_interval = input$freq_guide_interval,
            font_scale = font_scale_d()         # UIX-17 (debounced)
          )
          base_spectrogram(p)
          load_error(NULL)

          # PERF: prefetch adjacent soundscapes in the background.
          # Runs after the current render completes; the compute is
          # idle-gated inside .spec_prefetch_later (UIX-22a) so the browser
          # gets the plot first and the UI never freezes mid-interaction.
          if (!is.null(wav) && requireNamespace("later", quietly = TRUE)) {
            sd <- shiny::isolate(soundscape_data())
            if (!is.null(sd)) {
              paths <- sd$soundscape_path
              idx <- which(paths == wav)
              if (length(idx) == 1) {
                for (offset in c(1, -1)) {
                  ni <- idx + offset
                  if (ni >= 1 && ni <= length(paths)) {
                    .spec_prefetch_later(paths[ni], wl, ov, ps, ch)
                  }
                }
              }
            }
          }
        },
        error = function(e) {
          base_spectrogram(NULL)
          load_error(
            sprintf("this recording could not be displayed (%s)", e$message)
          )
          shiny::showNotification(
            paste("Error creating spectrogram:", e$message),
            type = "error"
          )
        }
      )
    })

    # Render spectrogram with current view window, ROIs, and ruler
    output$spectrogram_plot <- shiny::renderPlot(execOnResize = TRUE, {
      if (!is.null(load_error())) {
        return(.blank_plot_message(
          sprintf("This recording could not be displayed:\n%s", load_error())
        ))
      }
      # UIX-17: the whole render reads the DEBOUNCED view window. Every use of
      # the zoom sliders below must go through `zoom_time`/`zoom_freq` (never
      # `input$...`) so the viewport, the ROI-overlap filter and the ruler all
      # agree on one snapshot — mixing raw and debounced values here would draw
      # ROIs clipped to a window the axes do not show.
      shiny::req(
        base_spectrogram(), zoom_time_d(),
        zoom_freq_d(), rec_soundscape(), duration_val()
      )

      zoom_time <- zoom_time_d()
      zoom_freq <- zoom_freq_d()
      if (zoom_freq[1] >= zoom_freq[2]) zoom_freq[2] <- zoom_freq[1] + 1

      # Mark the base coord as default so replacing it doesn't warn
      spectro_plot <- base_spectrogram()
      spectro_plot$coordinates$default <- TRUE

      spectro_plot <- spectro_plot +
        ggplot2::coord_cartesian(
          xlim = zoom_time,
          ylim = zoom_freq,
          expand = FALSE
        ) +
        ggplot2::annotate(
          "label",
          label = paste0(
            input$soundscape_file,
            " (sr = ",
            rec_soundscape()@samp.rate,
            "; wl = ",
            wl_input(),
            "; ovlp = ",
            input$ovlp,
            "; pitch = ",
            pitch_shift_input(),
            ")"
          ),
          x = -Inf,
          y = Inf,
          hjust = 0,
          vjust = 1,
          color = "white",
          fill = "black",
          size = 3.88 * font_scale_d()
        ) +
        ggplot2::labs(x = "Time (s)", y = "Frequency (kHz)") +
        ggplot2::theme(legend.position = "none")

      # Add ROIs
      current_rois <- roi_values()
      if (!is.null(current_rois) && nrow(current_rois) > 0 &&
          !all(is.na(current_rois$roi_start))) {

        rois_to_plot <- current_rois %>%
          dplyr::mutate(id = dplyr::row_number()) %>%
          dplyr::filter(
            as.numeric(.data$roi_start) < zoom_time[2] &
              as.numeric(.data$roi_end) > zoom_time[1]
          )

        if (nrow(rois_to_plot) > 0 &&
            identical(unique(rois_to_plot$soundscape_path), input$soundscape_file)) {

          selection_color <- ifelse(
            input$color_scale %in% c("greyscale 1", "greyscale 2"), "black", "white"
          )

          spectro_plot <- spectro_plot +
            ggplot2::annotate("rect",
              alpha = 0.05, linewidth = 0.3, linetype = "dashed",
              fill = rep(selection_color, nrow(rois_to_plot)),
              color = rep(selection_color, nrow(rois_to_plot)),
              xmin = rois_to_plot$roi_start, xmax = rois_to_plot$roi_end,
              ymin = rois_to_plot$roi_min_freq, ymax = rois_to_plot$roi_max_freq
            )

          # Highlight selected rows from DT (LSA-22): match by stable row id.
          # `res_table` shows the full roi_values() table, so selected indices
          # are `id` (row_number on the unfiltered table). `rois_to_plot` is a
          # zoom-filtered subset, so positional indexing would point at the
          # wrong ROI; intersect on `id` and use the resulting subset rows.
          selected_rows <- input$res_table_rows_selected
          if (!is.null(selected_rows) && length(selected_rows) > 0) {
            sel_in_view <- which(rois_to_plot$id %in% selected_rows)
            if (length(sel_in_view) > 0) {
              spectro_plot <- spectro_plot +
                ggplot2::annotate("rect",
                  alpha = 0.2, linewidth = 0.5, linetype = "solid",
                  fill = selection_color, color = selection_color,
                  xmin = rois_to_plot$roi_start[sel_in_view],
                  xmax = rois_to_plot$roi_end[sel_in_view],
                  ymin = rois_to_plot$roi_min_freq[sel_in_view],
                  ymax = rois_to_plot$roi_max_freq[sel_in_view]
                )
            }
          }

          # LSA-104: highlight the active (left-click selected) ROI distinctly
          # from the DT selection (thicker orange border, theme-agnostic).
          # LSA-115: in re-selection mode the border turns red + dashed as a
          # persistent cue that the next brush will redraw this ROI's bounds.
          active_id <- active_roi_id()
          if (!is.null(active_id)) {
            act_in_view <- which(rois_to_plot$id == active_id)
            if (length(act_in_view) > 0) {
              active_color <- if (isTRUE(reselect_mode())) "#e53935" else "#ff9800"
              active_lty   <- if (isTRUE(reselect_mode())) "dashed" else "solid"
              spectro_plot <- spectro_plot +
                ggplot2::annotate("rect",
                  alpha = 0.1, linewidth = 1, linetype = active_lty,
                  fill = active_color, color = active_color,
                  xmin = rois_to_plot$roi_start[act_in_view],
                  xmax = rois_to_plot$roi_end[act_in_view],
                  ymin = rois_to_plot$roi_min_freq[act_in_view],
                  ymax = rois_to_plot$roi_max_freq[act_in_view]
                )
            }
          }

          if (isTRUE(input$show_label)) {
            # LSA-123: the effective label (correction from the LVA round trip).
            plot_label <- if ("roi_label_updated" %in% names(rois_to_plot)) {
              ifelse(!is.na(rois_to_plot$roi_label_updated),
                     rois_to_plot$roi_label_updated, rois_to_plot$roi_label)
            } else {
              rois_to_plot$roi_label
            }
            spectro_plot <- spectro_plot +
              ggplot2::annotate("text",
                alpha = 1, vjust = "inward", hjust = "inward",
                angle = input$label_angle, color = selection_color,
                x = rois_to_plot$roi_start, y = rois_to_plot$roi_max_freq,
                label = paste0("(", rois_to_plot$id, ") ", plot_label),
                na.rm = TRUE,
                size = 3.88 * font_scale_d()
              )
          }
        }
      }

      # LSA-208: read-only detection overlay (toggle + filters gate the frame)
      spectro_plot <- .annotate_detection_overlay(
        spectro_plot, overlay_detections(), selected_detection(), zoom_time
      )

      # Add ruler overlay
      if (!is.null(ruler())) {
        spectro_plot <- spectro_plot +
          ggplot2::geom_rect(
            data = ruler(),
            ggplot2::aes(
              xmin = roi_start, xmax = roi_end,
              ymin = roi_min_freq, ymax = roi_max_freq
            ),
            fill = NA, color = "yellow", linetype = "dashed",
            inherit.aes = FALSE
          )

        tryCatch({
          measurements <- .extract_acoustic_measurements(
            rec = rec_soundscape(),
            ruler_data = ruler(),
            wl = wl_input(),
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
              ggplot2::annotate("label",
                label = measurement_text, x = Inf, y = -Inf,
                hjust = 1, vjust = 0, color = "yellow", fill = "black",
                alpha = 0.8, label.padding = ggplot2::unit(0.8, "lines"),
                label.size = 0.6, size = 5 * font_scale_d(), family = "mono"
              )
          }
        },
        error = function(e) {
          shiny::showNotification(
            paste("Error calculating measurements:", e$message),
            type = "warning"
          )
        })
      }

      spectro_plot
    })

    # -- Soundscape metadata panel (LSA-111) --------------------------------
    output$soundscape_metadata <- shiny::renderTable({
      .format_soundscape_metadata(rec_soundscape(), input$soundscape_file)
    }, colnames = FALSE, striped = TRUE, width = "100%", na = "-")

    # -- Audio player ---------------------------------------------------------

    output$audio_player <- shiny::renderUI({
      # HTML-player toggle: when off, render nothing (and skip the segment
      # extraction + savewav cost entirely).
      if (!isTRUE(input$enable_player)) return(NULL)
      # UIX-17: the player follows the DEBOUNCED window, so it matches what the
      # spectrogram is showing and a drag no longer re-cuts + re-encodes a WAV
      # on every tick (the most expensive reaction of the two).
      shiny::req(rec_soundscape(), zoom_time_d(), duration_val())

      tryCatch({
        start_time <- max(0, zoom_time_d()[1])
        end_time <- min(duration_val(), zoom_time_d()[2])
        if (start_time >= end_time) return(NULL)

        segment <- .create_audio_segment(rec_soundscape(), start_time, end_time)

        # Apply pitch shift
        if (pitch_shift_input() < 1) {
          segment@samp.rate <- as.integer(
            segment@samp.rate / abs(pitch_shift_input())
          )
        }

        # Apply bandpass filter if enabled
        if (isTRUE(input$visible_bp)) {
          tryCatch({
            ps <- abs(pitch_shift_input())
            segment <- seewave::ffilter(
              segment,
              f = segment@samp.rate,
              from = (zoom_freq_d()[1] / ps) * 1000,
              to = (zoom_freq_d()[2] / ps) * 1000,
              wl = wl_input(),
              output = "Wave",
              bandpass = TRUE
            )
          }, error = function(e) NULL)
        }

        # Normalize if enabled
        if (isTRUE(input$play_norm)) {
          tryCatch({
            segment <- tuneR::normalize(segment,
              unit = as.character(segment@bit), pcm = TRUE
            )
          }, error = function(e) NULL)
        }

        # Save segment to temp file
        segment_filename <- paste0(
          "seg_", gsub("[^a-zA-Z0-9]", "", input$soundscape_file), "_",
          gsub("[^a-zA-Z0-9]", "",
            paste0(format(Sys.time(), "%H%M%S"), basename(tempfile("")))),
          ".wav"
        )
        temp_file <- file.path(session_data$temp_path, segment_filename)
        seewave::savewav(segment, f = segment@samp.rate, filename = temp_file)

        # Clean up old temp files
        .safe_cleanup_temp_files(session_data$temp_path, temp_file)

        shiny::tags$audio(
          id = "audio_player_element",
          controls = TRUE, style = "width: 100%;",
          shiny::tags$source(
            src = paste0("audio/", segment_filename), type = "audio/wav"
          )
        )
      },
      error = function(e) {
        shiny::showNotification(
          paste("Error creating audio segment:", e$message), type = "error"
        )
        NULL
      })
    })

    shiny::observeEvent(input$play_soundscape, {
      shinyjs::runjs("
        var player = document.getElementById('audio_player_element');
        if (player) {
          if (player.paused) { player.play(); }
          else { player.pause(); }
        }
      ")
    })

    # -- ROI store / delete / clear -------------------------------------------

    handle_store_roi <- function() {
      shiny::req(
        input$roi_limits$xmin, input$roi_limits$xmax,
        input$roi_limits$ymin, input$roi_limits$ymax,
        wav_path_val(), user_val(), rec_soundscape()
      )

      # ERC-107(a): refuse a zero-area rectangle AT DRAW TIME. A degenerate
      # ROI (roi_start == roi_end, or equal frequency bounds) only exists as
      # an accidental click; storing it guarantees a named batch error at
      # export time. The notification below is the same soft rejection the
      # duplicate guard uses -- the app session stays stable.
      if (!isTRUE(input$roi_limits$xmax > input$roi_limits$xmin) ||
          !isTRUE(input$roi_limits$ymax > input$roi_limits$ymin)) {
        shiny::showNotification(
          paste0("Zero-area ROI refused [MSG-007]: a ROI must have positive ",
                 "width and height. Draw the rectangle again."),
          type = "warning"
        )
        return()
      }

      new_roi <- tibble::tibble(
        soundscape_path = wav_path_val(),
        soundscape_file = basename(wav_path_val()),
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
        roi_wl = wl_input(),
        roi_ovlp = input$ovlp,
        roi_sample_rate = rec_soundscape()@samp.rate,
        roi_pitch_shift = pitch_shift_input(),
        # LSA-109: persist the segmented channel. mono files -> "mono";
        # stereo -> the displayed channel (default "left").
        roi_channel = if (isTRUE(rec_soundscape()@stereo)) {
          if (is.null(input$display_channel)) "left" else input$display_channel
        } else {
          "mono"
        },
        # LSA-205: stamp the ACTIVE label list at annotation time (the
        # runtime selector wins; session_data holds the launch default).
        roi_label_list = if (is.null(input$roi_label_list)) {
          session_data$roi_label_list
        } else {
          input$roi_label_list
        }
      )

      current <- roi_values()
      if (is.null(current) || all(is.na(current)) || nrow(current) == 0) {
        roi_values(new_roi)
      } else {
        if (.is_duplicate_roi(new_roi, current)) {
          shiny::showNotification("Duplicate ROI detected [MSG-006]", type = "warning")
          return()
        }
        roi_values(dplyr::bind_rows(current, new_roi))
      }
      rois_changed(TRUE)

      # UIX-23(a): bump the stored label's usage so the autocomplete re-ranks.
      lab <- input$label_name
      if (is.character(lab) && length(lab) == 1L && !is.na(lab) &&
          nzchar(trimws(lab))) {
        cur <- label_usage[[lab]]
        label_usage[[lab]] <- (if (is.null(cur)) 0L else cur) + 1L
      }

      # Reset unlocked input fields
      if (!input$lock_label_certainty) {
        shiny::updateSelectInput(session, "label_certainty", selected = "certain")
      }
      if (!input$lock_is_complete) {
        shiny::updateSelectInput(session, "signal_is_complete", selected = "complete")
      }
      if (!input$lock_comment) {
        shiny::updateTextInput(session, "label_comment", value = "")
      }
    }

    handle_delete_last_roi <- function() {
      current <- roi_values()
      if (is.null(current) || nrow(current) == 0) return()
      if (nrow(current) > 1) {
        roi_values(utils::head(current, -1))
      } else {
        roi_values(.schema_rois())
      }
      rois_changed(TRUE)
    }

    # -- Save ROI table -------------------------------------------------------

    # select_path (A-nav fix): the path the soundscape selector should end up
    # on after the choices are rebuilt. Defaults to the current file (the
    # standalone Save button keeps the current selection). Navigation passes
    # the destination so the single server-side rebuild already selects it,
    # avoiding a race where a later updateSelectInput(selected=new) was clobbered
    # by this async rebuild (selected=current) -- the cause of the 'first advance
    # does nothing, second works' bug.
    handle_save_roi_table <- function(select_path = NULL) {
      shiny::req(roi_con(), input$soundscape_file)
      current <- roi_values()
      if (is.null(current)) current <- .schema_rois()
      if (is.null(select_path)) select_path <- input$soundscape_file

      tryCatch({
        .signals_duckdb_save_rois(roi_con(),
                                  .rois_as_signals(current,
                                                    ws_root()),
                                  input$soundscape_file,
                                  workspace_root = ws_root())
        rois_changed(FALSE)

        # Update progress tracker and refresh dropdown icons
        if (!is.null(progress_tracker$df)) {
          idx <- which(progress_tracker$df$soundscape_path == input$soundscape_file)
          if (length(idx) > 0) {
            new_status <- .derive_status_from_rois(current)
            progress_tracker$df$status[idx] <- new_status
            progress_tracker$df$has_table[idx] <- new_status != "unsegmented"
            n_done <- sum(progress_tracker$df$has_table)
            shinyWidgets::updateProgressBar(
              session, "progress_bar",
              value = n_done, total = nrow(progress_tracker$df)
            )
            shiny::updateSelectizeInput(
              session, "soundscape_file",
              choices = .build_soundscape_choices(progress_tracker$df),
              selected = select_path, server = TRUE
            )
            if (n_done == nrow(progress_tracker$df)) {
              shiny::showNotification(
                "All recordings segmented!", type = "message", duration = 15
              )
            }
          }
        }
        shiny::showNotification("ROI table saved to database", type = "message")
      },
      error = function(e) {
        shiny::showNotification(
          paste("Error saving ROIs:", e$message), type = "error"
        )
      })
    }

    shiny::observeEvent(input$save_roi, handle_save_roi_table())

    # -- Navigation -----------------------------------------------------------

    # LSA-110: when advancing soundscapes, auto-export ROIs the user flagged as
    # templates (the word "template" in roi_comment) as audio cuts to the cuts
    # path. Template metadata stays encoded in the cut filename (user decision
    # 2026-05-30; revisit when fetch_template_metadata (#6) is refactored). Runs
    # only after an autosave persisted edits, and only if a cuts path is set, so
    # browsing without changes never triggers an export.
    autosave_templates <- function() {
      if (is.null(templates_path_val()) || !dir.exists(templates_path_val())) {
        return(invisible(NULL))
      }
      templates <- .select_template_rois(roi_values())
      if (is.null(templates) || nrow(templates) == 0) return(invisible(NULL))
      tryCatch({
        export_templates(df_rois = templates, templates_path = templates_path_val())
        shiny::showNotification(
          sprintf("Auto-exported %d template cut(s)", nrow(templates)),
          type = "message", duration = 2
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Template export failed:", e$message), type = "warning"
        )
      })
    }

    navigate_soundscape_fn <- function(direction) {
      shiny::req(soundscape_data())
      vec_soundscapes <- soundscape_data()$soundscape_path
      current_index <- which(vec_soundscapes == input$soundscape_file)
      if (length(current_index) == 0) return()

      # Compute the destination FIRST so an autosave can rebuild the dropdown
      # choices already selecting it (single, authoritative selection update).
      new_index <- switch(direction,
        "prev" = if (current_index > 1) current_index - 1 else current_index,
        "next" = if (current_index < length(vec_soundscapes)) current_index + 1 else current_index
      )
      target <- vec_soundscapes[new_index]

      if (input$nav_autosave && rois_changed()) {
        # The save's choices rebuild carries selected = target, so no separate
        # (racing) selection update is issued here.
        handle_save_roi_table(select_path = target)
        autosave_templates()
      } else if (new_index != current_index) {
        # No rebuild happens in this branch, so a plain selection update on the
        # existing choices cannot be clobbered.
        shiny::updateSelectizeInput(session, "soundscape_file", selected = target)
      }
    }

    navigate_unsegmented_fn <- function(direction) {
      shiny::req(progress_tracker$df)

      # LSA-18: navigate by GLOBAL index to the nearest has_table == FALSE
      # neighbour. .next_unsegmented_index uses strict < / > of current_index,
      # so it never returns the current file; the autosave's flip of the current
      # file's has_table cannot change the target, letting us compute the target
      # BEFORE the save and route it through the single rebuild (nav-race fix).
      df <- progress_tracker$df
      all_paths <- df$soundscape_path
      current_index <- which(all_paths == input$soundscape_file)
      if (length(current_index) == 0) return()
      current_index <- current_index[1]

      new_index <- .next_unsegmented_index(
        df$has_table, current_index, direction
      )
      if (is.na(new_index)) {
        shiny::showNotification(
          sprintf("No %s unsegmented soundscape", direction), type = "message"
        )
        return()
      }
      target <- all_paths[new_index]

      if (input$nav_autosave && rois_changed()) {
        handle_save_roi_table(select_path = target)
        autosave_templates()
      } else {
        shiny::updateSelectizeInput(session, "soundscape_file", selected = target)
      }
    }

    shiny::observeEvent(input$prev_soundscape, navigate_soundscape_fn("prev"))
    shiny::observeEvent(input$next_soundscape, navigate_soundscape_fn("next"))
    shiny::observeEvent(input$prev_soundscape_noroi, navigate_unsegmented_fn("prev"))
    shiny::observeEvent(input$next_soundscape_noroi, navigate_unsegmented_fn("next"))

    # -- No signals of interest -----------------------------------------------

    commit_no_soi <- function() {
      roi_values(.make_no_soi_roi(
        soundscape_path = wav_path_val(),
        user            = user_val(),
        duration        = duration_val(),
        freq_min        = input$zoom_freq_slider[1],
        freq_max        = input$zoom_freq_slider[2],
        wl              = wl_input(),
        ovlp            = input$ovlp,
        sample_rate     = rec_soundscape()@samp.rate,
        pitch_shift     = pitch_shift_input(),
        channel         = if (isTRUE(rec_soundscape()@stereo)) {
          if (is.null(input$display_channel)) "left" else input$display_channel
        } else {
          "mono"
        }
      ))
      rois_changed(TRUE)
      # Save the no-SOI sentinel AND advance to the next soundscape in a single
      # choices rebuild (nav-race fix): a separate handle_save_roi_table() +
      # navigate_soundscape_fn() pair would race the async server-side rebuild
      # against the navigation's selection update (first advance does nothing).
      vec_soundscapes <- soundscape_data()$soundscape_path
      ci <- which(vec_soundscapes == input$soundscape_file)
      target <- if (length(ci) == 1 && ci < length(vec_soundscapes)) {
        vec_soundscapes[ci + 1]
      } else {
        input$soundscape_file
      }
      handle_save_roi_table(select_path = target)
      shiny::showNotification(
        "Marked as no signals of interest", type = "message"
      )
    }

    handle_no_soi <- function() {
      shiny::req(
        wav_path_val(), user_val(), rec_soundscape(),
        soundscape_data(), duration_val()
      )

      current <- roi_values()
      if (is.null(current) || all(is.na(current)) || nrow(current) == 0) {
        commit_no_soi()
      } else {
        shiny::showModal(shiny::modalDialog(
          title = "Confirm Action",
          "This will erase all ROIs for this soundscape. Continue?",
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton("confirm_no_soi", "Proceed",
              style = "color: #fff; background-color: #b73333;"
            )
          )
        ))
      }
    }

    shiny::observeEvent(input$no_soi, handle_no_soi())

    shiny::observeEvent(input$confirm_no_soi, {
      shiny::req(
        wav_path_val(), user_val(), rec_soundscape(), soundscape_data(),
        duration_val()
      )
      commit_no_soi()
      shiny::removeModal()
    })

    # -- Hotkey dispatch ------------------------------------------------------

    handle_zoom_in_time <- function() {
      shiny::req(input$zoom_time, duration_val())
      zoom_range <- input$zoom_time
      if (zoom_range[1] >= zoom_range[2] || duration_val() <= 1) return()
      current_width <- diff(zoom_range)
      padding <- current_width * 0.25
      new_zoom <- c(
        min(zoom_range[1] + padding, duration_val()),
        max(zoom_range[2] - padding, 0)
      )
      if (new_zoom[1] < new_zoom[2]) {
        shiny::updateSliderInput(session, "zoom_time", value = new_zoom)
      }
    }

    handle_zoom_out_time <- function() {
      shiny::req(input$zoom_time, duration_val())
      zoom_range <- input$zoom_time
      if (zoom_range[1] >= zoom_range[2] || duration_val() <= 1) return()
      current_width <- diff(zoom_range)
      padding <- current_width * 0.5
      new_zoom <- c(
        max(zoom_range[1] - padding, 0),
        min(zoom_range[2] + padding, duration_val())
      )
      if (diff(new_zoom) > 60) new_zoom[2] <- min(new_zoom[1] + 60, duration_val())
      if (new_zoom[1] < new_zoom[2]) {
        shiny::updateSliderInput(session, "zoom_time", value = new_zoom)
      }
    }

    handle_nav_time <- function(direction) {
      shiny::req(input$zoom_time, duration_val())
      zoom_range <- input$zoom_time
      window_width <- diff(zoom_range)
      step_size <- window_width / 2
      new_zoom <- if (direction == "forward") {
        if (zoom_range[2] >= duration_val()) {
          c(duration_val() - window_width, duration_val())
        } else {
          pmin(zoom_range + step_size, duration_val())
        }
      } else {
        if (zoom_range[1] <= 0) {
          c(0, window_width)
        } else {
          pmax(zoom_range - step_size, 0)
        }
      }
      shiny::updateSliderInput(session, "zoom_time", value = new_zoom)
    }

    handle_reset_time_zoom <- function() {
      shiny::req(duration_val())
      shiny::updateSliderInput(session, "zoom_time",
        value = c(0, min(60, duration_val()))
      )
    }

    handle_reset_all_zoom <- function() {
      shiny::req(duration_val(), rec_soundscape())
      nyquist_khz <- rec_soundscape()@samp.rate / 2000
      shiny::updateSliderInput(session, "zoom_time",
        value = c(0, min(60, duration_val()))
      )
      shiny::updateSliderInput(session, "zoom_freq_slider",
        value = c(0, nyquist_khz)
      )
    }

    handle_freq_zoom <- function(direction) {
      shiny::req(input$zoom_freq_slider, rec_soundscape())
      nyquist_khz <- rec_soundscape()@samp.rate / 2000
      current_min <- input$zoom_freq_slider[1]
      current_max <- input$zoom_freq_slider[2]
      if (direction == "in") {
        window_size <- diff(input$zoom_freq_slider) * 0.5
        new_max <- current_min + window_size
      } else {
        window_size <- diff(input$zoom_freq_slider) * 2
        new_max <- min(nyquist_khz, current_min + window_size)
      }
      shiny::updateSliderInput(session, "zoom_freq_slider",
        value = c(current_min, new_max)
      )
    }

    handle_freq_nav <- function(direction) {
      shiny::req(input$zoom_freq_slider, rec_soundscape())
      nyquist_khz <- rec_soundscape()@samp.rate / 2000
      current_min <- input$zoom_freq_slider[1]
      current_max <- input$zoom_freq_slider[2]
      window_size <- current_max - current_min
      if (direction == "up") {
        new_max <- min(nyquist_khz, current_max + 1)
        new_min <- new_max - window_size
        if (new_min >= 0) {
          shiny::updateSliderInput(session, "zoom_freq_slider",
            value = c(new_min, new_max)
          )
        }
      } else {
        new_min <- max(0, current_min - 1)
        new_max <- new_min + window_size
        if (new_max <= nyquist_khz) {
          shiny::updateSliderInput(session, "zoom_freq_slider",
            value = c(new_min, new_max)
          )
        }
      }
    }

    handle_ruler <- function() {
      shiny::req(
        input$roi_limits$xmin, input$roi_limits$xmax,
        input$roi_limits$ymin, input$roi_limits$ymax
      )
      if (is.null(ruler())) {
        ruler(data.frame(
          soundscape_file = input$soundscape_file,
          roi_start = input$roi_limits$xmin,
          roi_end = input$roi_limits$xmax,
          roi_duration = input$roi_limits$xmax - input$roi_limits$xmin,
          roi_min_freq = input$roi_limits$ymin,
          roi_max_freq = input$roi_limits$ymax,
          roi_bandwidth = input$roi_limits$ymax - input$roi_limits$ymin,
          stringsAsFactors = FALSE
        ))
      } else {
        ruler(NULL)
      }
    }

    shiny::observeEvent(input$roi_select, {
      # LSA-115: while the boundary re-selection mode is ON, ignore click
      # activation entirely -- swapping active_roi_id here would re-render
      # the plot and interrupt the brush drag that redraws the bounds.
      # Accepted side effect: clicking empty space no longer exits the
      # reselection mode; exit is via the B toggle only.
      if (isTRUE(reselect_mode())) return()
      rois <- roi_values()
      sel_id <- .roi_at_point(rois, input$roi_select$x, input$roi_select$y)
      if (is.na(sel_id)) {
        active_roi_id(NULL)
        reselect_mode(FALSE)
        # LSA-208: no ROI under the click -- resolve a detection when the
        # overlay is on. ROIs keep priority: a click that hits a ROI never
        # falls through to a detection underneath it.
        det_id <- .signal_at_point(
          overlay_detections(), input$roi_select$x, input$roi_select$y
        )
        selected_detection(det_id)
        if (!is.na(det_id)) {
          dets <- overlay_detections()
          hit <- dets[dets$signal_id == det_id, ][1, ]
          shiny::showNotification(
            sprintf(
              "Detection selected: %s (score %s) - Alt+P promotes it to a ROI",
              hit$det_template_name, format(hit$det_peak_score, digits = 3)
            ),
            type = "message", duration = 5
          )
        }
        return()
      }
      # A ROI activation clears any pending detection selection.
      selected_detection(NA_character_)
      active_roi_id(sel_id)
      row <- rois[sel_id, ]
      # LSA-123 (plan §8.8): show the EFFECTIVE label (roi_label_updated
      # when present) so the LVA round-trip correction is reflected. The
      # label may be off-list (created), so add it to the choices.
      label_choices <- dplyr::pull(roi_label_lists, input$roi_label_list)
      # UIX-23(a): rank by recency/frequency of use this session.
      label_choices <- .rank_labels_by_usage(label_choices)
      eff_label <- if ("roi_label_updated" %in% names(row) &&
                       length(row$roi_label_updated) == 1L &&
                       !is.na(row$roi_label_updated)) {
        row$roi_label_updated
      } else {
        row$roi_label
      }
      shiny::updateSelectizeInput(
        session, "label_name",
        choices = c(NA, label_choices, eff_label),
        selected = eff_label, server = TRUE,
        options = list(create = TRUE, persist = FALSE)
      )
      shiny::updateSelectizeInput(session, "signal_type", selected = row$roi_type)
      shiny::updateSelectizeInput(
        session, "label_certainty", selected = row$roi_label_confidence
      )
      shiny::updateSelectizeInput(
        session, "signal_is_complete", selected = row$roi_is_complete
      )
      shiny::updateTextInput(
        session, "label_comment",
        value = if (is.na(row$roi_comment)) "" else row$roi_comment
      )
      shiny::showNotification(
        sprintf("Active ROI: (%d) %s", sel_id, eff_label),
        type = "message", duration = 2
      )
    })

    # LSA-104/115: reset the active ROI + re-selection mode on soundscape change.
    shiny::observeEvent(input$soundscape_file, {
      active_roi_id(NULL)
      reselect_mode(FALSE)
    })

    # -- LSA-208: detection promotion (detection -> editable ROI) ------------

    # Refresh the Detections tab filters from the loaded detections.
    .update_detection_filters <- function(dets) {
      if (is.null(dets) || nrow(dets) == 0) {
        shiny::updateSelectInput(
          session, "det_template",
          choices = c("All templates" = "(all)"), selected = "(all)"
        )
        return(invisible(NULL))
      }
      scores <- dets$det_peak_score[!is.na(dets$det_peak_score)]
      if (length(scores) == 0) scores <- c(0, 1)
      lo <- min(scores)
      hi <- max(scores)
      if (!is.finite(lo) || !is.finite(hi)) {
        lo <- 0
        hi <- 1
      }
      if (hi <= lo) hi <- lo + 1
      step <- max((hi - lo) / 100, 0.001)
      shiny::updateSliderInput(
        session, "det_score_min", min = lo, max = hi, value = lo, step = step
      )
      tmpls <- sort(unique(
        dets$det_template_name[!is.na(dets$det_template_name)]
      ))
      shiny::updateSelectInput(
        session, "det_template",
        choices = c("All templates" = "(all)", tmpls), selected = "(all)"
      )
    }

    .commit_detection_promotion <- function() {
      shiny::req(selected_detection(), roi_con(), user_val())
      if (is.na(selected_detection())) return(invisible(NULL))
      dets <- detection_values()
      det <- dets[dets$signal_id == selected_detection(), , drop = FALSE]
      if (nrow(det) == 0) {
        selected_detection(NA_character_)
        shiny::showNotification(
          "The selected detection is no longer loaded [MSG-011]", type = "warning"
        )
        return(invisible(NULL))
      }
      prom <- .promote_detection_row(
        det, user = user_val(),
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      new_row <- .signals_as_rois(prom, include_detections = TRUE)
      if (.is_duplicate_roi(new_row, roi_values())) {
        shiny::showNotification(
          "This detection is already an editable ROI [MSG-012]", type = "warning"
        )
        return(invisible(NULL))
      }
      tryCatch({
        # Additive + immediate: the detection row is never touched; the
        # promoted row persists now, independent of the next ROI table save
        # (which also re-persists it, class-scoped, like any ROI edit).
        .signals_duckdb_upsert(roi_con(), prom, replace = FALSE)
        current <- roi_values()
        roi_values(if (is.null(current) || nrow(current) == 0) {
          new_row
        } else {
          dplyr::bind_rows(current, new_row)
        })
        rois_changed(TRUE)
        shiny::showNotification(
          sprintf("Detection promoted to ROI: %s", prom$roi_label),
          type = "message", duration = 3
        )
      },
      error = function(e) {
        shiny::showNotification(
          paste("Promotion failed:", e$message), type = "error"
        )
      })
    }

    # One-per-session orientation gate: the first Alt+P opens the orientation
    # popup; only its confirm button performs the promotion. Later promotions
    # run directly (maintainer decision 2026-08-18: no per-detection modal).
    handle_promote_detection <- function() {
      det_id <- selected_detection()
      if (is.null(det_id) || is.na(det_id)) {
        shiny::showNotification(
          "No detection selected -- turn the overlay on and left-click one",
          type = "warning"
        )
        return(invisible(NULL))
      }
      if (!isTRUE(promotion_oriented())) {
        dets <- detection_values()
        hit <- dets[dets$signal_id == det_id, ][1, ]
        shiny::showModal(shiny::modalDialog(
          title = "Promote detection to ROI",
          shiny::tags$p("Before promoting, check the assumptions of a manual ROI:"),
          shiny::tags$ul(
            shiny::tags$li("the box is correct and complete for the signal;"),
            shiny::tags$li("the label is the one a person would assign;"),
            shiny::tags$li(
              "edit the promoted ROI's metadata afterwards if needed."
            )
          ),
          shiny::tags$p(
            paste(
              "Promoting copies the detection box into a new editable ROI",
              "(detection_to_roi); the original detection stays unchanged.",
              "Promoted ROIs become eligible as validation ground truth, so",
              "promote only detections that satisfy the assumptions above.",
              "This popup is shown once per session; next time Alt+P promotes",
              "directly. See ?launch_segmentation_app."
            )
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              "confirm_first_promotion", paste0(
                "Promote (", hit$det_template_name, ", score ",
                format(hit$det_peak_score, digits = 3), ")"
              ),
              style = "color: #fff; background-color: #2e7d32;"
            )
          )
        ))
        return(invisible(NULL))
      }
      .commit_detection_promotion()
    }

    shiny::observeEvent(input$confirm_first_promotion, {
      promotion_oriented(TRUE)
      shiny::removeModal()
      .commit_detection_promotion()
    })

    # A5: single-level undo of the active ROI's bounds. capture_roi_bounds()
    # snapshots the current bounds before any mutation (snap / resize /
    # reselect); handle_undo_bounds() restores that snapshot once via the
    # canonical .set_roi_bounds() and clears it (one level, like the original).
    capture_roi_bounds <- function(rois, id) {
      roi_bounds_undo(list(
        id = id,
        start = as.numeric(rois$roi_start[id]),
        end = as.numeric(rois$roi_end[id]),
        min_freq = as.numeric(rois$roi_min_freq[id]),
        max_freq = as.numeric(rois$roi_max_freq[id])
      ))
    }
    handle_undo_bounds <- function() {
      snap <- roi_bounds_undo()
      if (is.null(snap)) {
        shiny::showNotification("Nothing to undo [MSG-018]", type = "warning")
        return()
      }
      rois <- roi_values()
      if (is.null(rois) || snap$id > nrow(rois)) {
        roi_bounds_undo(NULL)
        return()
      }
      rois <- .set_roi_bounds(
        rois, snap$id, snap$start, snap$end, snap$min_freq, snap$max_freq
      )
      roi_values(rois)
      rois_changed(TRUE)
      roi_bounds_undo(NULL)
      shiny::showNotification(
        sprintf("Reverted ROI %d bounds", snap$id), type = "message",
        duration = 2
      )
    }

    # LSA-106: live incremental resize of the active ROI. Edits the stored row
    # in place (rois_changed -> autosave/Ctrl+E persists it); the E key is
    # unchanged. axis is "time" | "freq" | "both"; direction "expand"|"contract".
    handle_resize_active_roi <- function(axis, direction) {
      active_id <- active_roi_id()
      if (is.null(active_id)) {
        shiny::showNotification(
          "No active ROI -- left-click a ROI first [MSG-013]", type = "warning"
        )
        return()
      }
      rois <- roi_values()
      if (is.null(rois) || active_id > nrow(rois)) return()
      capture_roi_bounds(rois, active_id)
      if (axis %in% c("time", "both")) {
        tv <- .resize_interval(
          as.numeric(rois$roi_start[active_id]),
          as.numeric(rois$roi_end[active_id]),
          .ROI_TIME_STEP_S, direction,
          limit_lo = 0, limit_hi = duration_val(), min_extent = .ROI_MIN_TIME_S
        )
        rois$roi_start[active_id] <- tv[1]
        rois$roi_end[active_id]   <- tv[2]
      }
      if (axis %in% c("freq", "both")) {
        nyquist_khz <- rec_soundscape()@samp.rate / 2000
        fv <- .resize_interval(
          as.numeric(rois$roi_min_freq[active_id]),
          as.numeric(rois$roi_max_freq[active_id]),
          .ROI_FREQ_STEP_KHZ, direction,
          limit_lo = 0, limit_hi = nyquist_khz, min_extent = .ROI_MIN_FREQ_KHZ
        )
        rois$roi_min_freq[active_id] <- fv[1]
        rois$roi_max_freq[active_id] <- fv[2]
      }
      roi_values(rois)
      rois_changed(TRUE)
    }

    # LSA-106: snap the active ROI to the full available range on an axis.
    handle_expand_roi_full <- function(axis) {
      active_id <- active_roi_id()
      if (is.null(active_id)) {
        shiny::showNotification(
          "No active ROI -- left-click a ROI first [MSG-013]", type = "warning"
        )
        return()
      }
      rois <- roi_values()
      if (is.null(rois) || active_id > nrow(rois)) return()
      capture_roi_bounds(rois, active_id)
      if (axis %in% c("time", "both")) {
        rois$roi_start[active_id] <- 0
        rois$roi_end[active_id]   <- duration_val()
      }
      if (axis %in% c("freq", "both")) {
        rois$roi_min_freq[active_id] <- 0
        rois$roi_max_freq[active_id] <- rec_soundscape()@samp.rate / 2000
      }
      roi_values(rois)
      rois_changed(TRUE)
    }

    # LSA-112: metadata popup. Opens a modal pre-filled from the current input
    # fields with all ROI metadata controls. On Apply: always mirror the values
    # back into the input fields (reuse on the next ROI); additionally, if a ROI
    # is active, write them into that ROI's row. The E key is unchanged.
    handle_open_roi_popup <- function() {
      label_choices <- dplyr::pull(roi_label_lists, input$roi_label_list)
      shiny::showModal(shiny::modalDialog(
        title = if (is.null(active_roi_id())) {
          "ROI metadata (fills the input fields)"
        } else {
          sprintf("ROI metadata (active ROI %d)", active_roi_id())
        },
        shiny::selectizeInput(
          "popup_label", "Label",
          choices = c(input$label_name, label_choices),
          selected = input$label_name,
          options = list(create = TRUE, persist = FALSE), width = "100%"
        ),
        shiny::selectizeInput(
          "popup_type", "Type",
          choices = unique(c(input$signal_type, roi_type_choices)),
          selected = input$signal_type,
          options = list(create = TRUE, persist = FALSE), width = "100%"
        ),
        shiny::selectInput(
          "popup_certainty", "Certainty",
          choices = c("certain", "uncertain"),
          selected = input$label_certainty, width = "100%"
        ),
        shiny::selectInput(
          "popup_complete", "Complete",
          choices = c("complete", "incomplete"),
          selected = input$signal_is_complete, width = "100%"
        ),
        shiny::textInput(
          "popup_comment", "Comment", value = input$label_comment,
          width = "100%"
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton(
            "popup_confirm", "Apply",
            style = "color: #fff; background-color: #2e7d32;"
          )
        ),
        easyClose = TRUE
      ))
    }

    shiny::observeEvent(input$popup_confirm, {
      label_choices <- dplyr::pull(roi_label_lists, input$roi_label_list)
      # Always mirror the popup values into the input fields.
      shiny::updateSelectizeInput(
        session, "label_name",
        choices = c(NA, label_choices, input$popup_label),
        selected = input$popup_label, server = TRUE,
        options = list(create = TRUE, persist = FALSE)
      )
      shiny::updateSelectizeInput(
        session, "signal_type", selected = input$popup_type
      )
      shiny::updateSelectizeInput(
        session, "label_certainty", selected = input$popup_certainty
      )
      shiny::updateSelectizeInput(
        session, "signal_is_complete", selected = input$popup_complete
      )
      shiny::updateTextInput(
        session, "label_comment", value = input$popup_comment
      )
      # With an active ROI, also write the values into that ROI's row.
      active_id <- active_roi_id()
      if (!is.null(active_id)) {
        rois <- roi_values()
        if (!is.null(rois) && active_id <= nrow(rois)) {
          rois <- .apply_roi_metadata(
            rois, active_id, input$popup_label, input$popup_type,
            input$popup_certainty, input$popup_complete, input$popup_comment
          )
          roi_values(rois)
          rois_changed(TRUE)
          shiny::showNotification(
            sprintf("Updated active ROI (%d)", active_id),
            type = "message", duration = 2
          )
        }
      } else {
        shiny::showNotification(
          "Metadata copied to the input fields", type = "message", duration = 2
        )
      }
      shiny::removeModal()
    })

    # LSA-115: boundary re-selection mode. Toggle (B) enters/exits the mode for
    # the active ROI; while ON, the user redraws the bounds with the mouse and
    # presses E to commit them onto the active ROI (the E key routes here only
    # in this mode -- see the hotkeys switch -- so the default draw-new-ROI E is
    # otherwise unchanged).
    handle_toggle_reselect <- function() {
      if (isTRUE(reselect_mode())) {
        reselect_mode(FALSE)
        shiny::showNotification("Re-selection mode OFF", type = "message",
          duration = 2)
        return()
      }
      if (is.null(active_roi_id())) {
        shiny::showNotification(
          "No active ROI -- left-click a ROI first [MSG-013]", type = "warning"
        )
        return()
      }
      reselect_mode(TRUE)
      shiny::showNotification(
        paste("Re-selection mode ON -- draw the new bounds, press E to commit",
              "(B to cancel)"),
        type = "message", duration = 5
      )
    }

    handle_commit_reselect <- function() {
      active_id <- active_roi_id()
      if (is.null(active_id)) {
        reselect_mode(FALSE)
        return()
      }
      br <- input$roi_limits
      if (is.null(br) || is.null(br$xmin)) {
        shiny::showNotification(
          "Draw the new bounds first (no selection) [MSG-014]", type = "warning"
        )
        return()
      }
      rois <- roi_values()
      if (is.null(rois) || active_id > nrow(rois)) {
        reselect_mode(FALSE)
        return()
      }
      capture_roi_bounds(rois, active_id)
      rois <- .set_roi_bounds(
        rois, active_id, br$xmin, br$xmax, br$ymin, br$ymax
      )
      roi_values(rois)
      rois_changed(TRUE)
      reselect_mode(FALSE)
      shiny::showNotification(
        sprintf("Re-selected ROI %d bounds", active_id),
        type = "message", duration = 2
      )
    }

    # LSA-119: brush-guided zoom, two-state toggle. Brush coordinates arrive
    # already in data units (s, kHz).
    # A1 fix: after every zoom action we resetBrush("roi_limits"), so a brush is
    # only ever present right after the user draws one. This avoids the earlier
    # bug where Shiny re-projected the lingering brush onto the zoomed axes,
    # changing its data coordinates and making a plain toggle-out look like a
    # freshly drawn brush (so X re-zoomed instead of restoring). State is just
    # zoom_home = NULL (normal) or list(x, y) (the viewport to restore).
    #   normal  + brush -> save home, zoom to bbox, reset brush.
    #   normal  + none  -> no-op.
    #   zoomed  + brush -> new brush: re-zoom, keep the original home, reset.
    #   zoomed  + none  -> restore home, clear state.
    handle_brush_zoom <- function() {
      br <- input$roi_limits
      cur_brush <- if (!is.null(br) && !is.null(br$xmin)) {
        list(x = sort(c(br$xmin, br$xmax)), y = sort(c(br$ymin, br$ymax)))
      } else {
        NULL
      }
      home <- zoom_home()
      if (is.null(home)) {
        if (is.null(cur_brush)) return(invisible(NULL))
        zoom_home(list(x = input$zoom_time, y = input$zoom_freq_slider))
        shiny::updateSliderInput(session, "zoom_time", value = cur_brush$x)
        shiny::updateSliderInput(
          session, "zoom_freq_slider", value = cur_brush$y
        )
        session$resetBrush("roi_limits")
        return(invisible(NULL))
      }
      if (!is.null(cur_brush)) {
        # Re-zoom to the new brush, keeping the original home as the toggle-out
        # target (alternative b).
        shiny::updateSliderInput(session, "zoom_time", value = cur_brush$x)
        shiny::updateSliderInput(
          session, "zoom_freq_slider", value = cur_brush$y
        )
        session$resetBrush("roi_limits")
      } else {
        shiny::updateSliderInput(session, "zoom_time", value = home$x)
        shiny::updateSliderInput(session, "zoom_freq_slider", value = home$y)
        zoom_home(NULL)
      }
    }

    shiny::observeEvent(input$hotkeys, {
      key <- input$hotkeys
      # LSA-119: any manual zoom/navigation discards the brush-zoom "home" so
      # the toggle never restores a stale viewport (slider drags are a minor
      # documented exception).
      if (key %in% c("w", "s", "a", "d", "g", "t", "v", "f", "alt+w", "alt+s")) {
        zoom_home(NULL)
      }
      switch(key,
        # LSA-119 brush-guided zoom toggle (no-op if no brush in normal state)
        "x"       = handle_brush_zoom(),
        # LSA-106 active-ROI resize: Shift = grow/shrink by a step, Alt = snap
        # to the full available range (no-op + warning when no ROI is active).
        "shift+w" = handle_resize_active_roi("time", "expand"),
        "shift+s" = handle_resize_active_roi("time", "contract"),
        "shift+g" = handle_resize_active_roi("freq", "expand"),
        "shift+t" = handle_resize_active_roi("freq", "contract"),
        "shift+e" = handle_resize_active_roi("both", "expand"),
        "shift+q" = handle_resize_active_roi("both", "contract"),
        "alt+t"   = handle_expand_roi_full("time"),
        "alt+g"   = handle_expand_roi_full("freq"),
        "alt+b"   = handle_expand_roi_full("both"),
        # LSA-112 metadata popup (writes the active ROI, else the input fields)
        "p"       = handle_open_roi_popup(),
        # LSA-115 boundary re-selection mode toggle (active ROI)
        "b"       = handle_toggle_reselect(),
        # LSA-208 promote the selected detection overlay box to a ROI
        "alt+p"   = handle_promote_detection(),
        # A4 alternative store/delete keys mirroring e/q. Enter follows the
        # same reselect-aware dispatch as E; Del removes the last ROI like Q.
        # The hotkey lib already suppresses keys while typing in text fields.
        "enter"   = if (isTRUE(reselect_mode())) handle_commit_reselect() else handle_store_roi(),
        "del"     = handle_delete_last_roi(),
        # A5 undo the last active-ROI bounds change (single level)
        "u"       = handle_undo_bounds(),
        "e"     = if (isTRUE(reselect_mode())) handle_commit_reselect() else handle_store_roi(),
        "q"     = handle_delete_last_roi(),
        "w"     = handle_zoom_in_time(),
        "s"     = handle_zoom_out_time(),
        "a"     = handle_nav_time("back"),
        "d"     = handle_nav_time("forward"),
        "z"     = navigate_soundscape_fn("prev"),
        "c"     = navigate_soundscape_fn("next"),
        "r"     = handle_ruler(),
        "g"     = handle_freq_zoom("in"),
        "t"     = handle_freq_zoom("out"),
        "v"     = handle_freq_nav("up"),
        "f"     = handle_freq_nav("down"),
        "alt+w" = handle_reset_all_zoom(),
        "alt+s" = handle_reset_time_zoom(),
        "ctrl+e" = handle_save_roi_table(),
        "alt+n" = handle_no_soi(),
        "alt+k" = {
          if (!is.null(input$roi_user) && input$roi_user != "" &&
              !is.null(input$soundscapes_path) && input$soundscapes_path != "" &&
              !is.null(input$roi_db) && input$roi_db != "") {
            shinyjs::click("user_setup_confirm")
          } else {
            shiny::showNotification(
              "Please fill in all required fields before confirming", type = "warning"
            )
          }
        }
      )
    })

    # -- DT table -------------------------------------------------------------

    # LSA-123: when the store carries roi_label_updated (SIG-08), display it
    # as the effective roi_label so the LVA round-trip correction shows.
    display_rois <- function(roi_df) {
      if ("roi_label_updated" %in% names(roi_df) && nrow(roi_df) > 0L) {
        roi_df$roi_label <- ifelse(!is.na(roi_df$roi_label_updated),
                                   roi_df$roi_label_updated, roi_df$roi_label)
      }
      roi_df
    }

    display_columns <- c(
      "roi_label", "roi_start", "roi_end", "roi_min_freq", "roi_max_freq",
      "roi_type", "roi_label_confidence", "roi_is_complete", "roi_comment"
    )

    output$res_table <- DT::renderDT({
      roi_df <- roi_values()
      if (is.null(roi_df) || nrow(roi_df) == 0) {
        return(.schema_rois()[, display_columns, drop = FALSE])
      }
      roi_df <- display_rois(roi_df)
      roi_df[, display_columns, drop = FALSE]
    },
    server = TRUE, editable = TRUE,
    options = list(
      lengthChange = FALSE, pageLength = 20, scrollX = TRUE
    ))

    shiny::observeEvent(input$res_table_cell_edit, {
      shiny::req(roi_values())
      info <- input$res_table_cell_edit
      df <- shiny::isolate(roi_values())
      if (!is.null(info$row) && !is.null(info$col) && !is.null(info$value)) {
        cols <- c(
          "roi_label", "roi_start", "roi_end", "roi_min_freq", "roi_max_freq",
          "roi_type", "roi_label_confidence", "roi_is_complete", "roi_comment"
        )
        col_name <- cols[info$col]
        # LSA-123 / SIG-08: the visible label is the EFFECTIVE one; when the
        # frame carries roi_label_updated, a cell edit of the label lands
        # there (the creation-time roi_label stays immutable).
        if (identical(col_name, "roi_label") &&
            "roi_label_updated" %in% names(df)) {
          col_name <- "roi_label_updated"
        }
        # LSA-02: coerce to the column's schema type instead of storing the
        # raw DT character string. Reject unparseable numeric input.
        col_type <- .roi_schema_spec()[[col_name]]
        raw <- info$value
        is_blank <- is.na(raw) || trimws(as.character(raw)) == ""
        if (col_type %in% c("numeric", "integer") && !is_blank) {
          coerced <- suppressWarnings(.coerce_type(raw, col_type))
          if (is.na(coerced)) {
            shiny::showNotification(
              sprintf("Invalid %s value: '%s' is not numeric", col_name, raw),
              type = "warning"
            )
            return()
          }
          df[[col_name]][info$row] <- coerced
        } else {
          df[[col_name]][info$row] <- .coerce_type(raw, col_type)
        }
        roi_values(df)
        rois_changed(TRUE)
      }
    })

    shiny::observeEvent(input$delete_selected_rois, {
      shiny::req(roi_values())
      n_sel <- length(input$res_table_rows_selected)
      if (is.null(n_sel) || n_sel == 0) {
        shiny::showNotification(
          "No rows selected in the ROI table [MSG-015]", type = "warning"
        )
        return()
      }
      # UIX-08: confirm before removing; nav_autosave persists the deletion on
      # the next navigation, so the confirmation mirrors the no_soi modal.
      shiny::showModal(shiny::modalDialog(
        title = "Confirm Action",
        sprintf(
          "This will delete %d selected ROI%s. Continue?",
          n_sel, if (n_sel == 1L) "" else "s"
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_delete_selected_rois", "Delete",
            style = "color: #fff; background-color: #b73333;"
          )
        )
      ))
    })

    shiny::observeEvent(input$confirm_delete_selected_rois, {
      shiny::req(roi_values())
      df <- roi_values()
      sel <- input$res_table_rows_selected
      if (!is.null(sel) && length(sel) > 0) {
        df <- df[-sel, ]
        roi_values(df)
        rois_changed(TRUE)
        shiny::showNotification("Selected ROIs deleted", type = "message")
      }
      shiny::removeModal()
    })


    # -- Export audio cuts ----------------------------------------------------

    shiny::observeEvent(input$export_selected_cut, {
      shiny::req(roi_values(), templates_path_val())
      current <- roi_values()
      if (is.null(current) || nrow(current) == 0 || all(is.na(current))) return()

      df <- if (!is.null(input$res_table_rows_selected)) {
        current[input$res_table_rows_selected, ]
      } else {
        current
      }
      export_templates(df_rois = df, templates_path = templates_path_val())
      shiny::showNotification("Audio cuts exported!", type = "message")
    })

    # -- Default parameters reset ---------------------------------------------

    shiny::observeEvent(input$default_pars, {
      shiny::updateSliderInput(session, "dyn_range", value = session_data$dyn_range)
      shinyWidgets::updateSliderTextInput(session, "wl", selected = session_data$wl)
      shiny::updateSliderInput(session, "ovlp", value = session_data$ovlp)
      shiny::updateSelectInput(session, "color_scale", selected = session_data$color_scale)
      shiny::updateSliderInput(session, "label_angle", value = session_data$label_angle)
      shiny::updateCheckboxInput(session, "show_label", value = session_data$show_label)
      shinyWidgets::updateSliderTextInput(
        session, "pitch_shift", selected = session_data$pitch_shift
      )
      shiny::updateNumericInput(
        session, "time_guide_interval", value = session_data$time_guide_interval
      )
      shiny::updateNumericInput(
        session, "freq_guide_interval", value = session_data$freq_guide_interval
      )
      shiny::updateCheckboxInput(session, "visible_bp", value = session_data$visible_bp)
      shiny::updateCheckboxInput(session, "play_norm", value = session_data$play_norm)
      shiny::updateCheckboxInput(session, "nav_autosave", value = session_data$nav_autosave)
      if (!is.null(rec_soundscape())) {
        shiny::updateSliderInput(session, "zoom_freq_slider",
          value = session_data$zoom_freq
        )
        # Restore the initial time window: a configured zoom_time takes
        # precedence; otherwise the first 60 s (current behaviour).
        zt <- session_data$zoom_time
        zoom_window <- if (is.null(zt)) {
          c(0, min(60, duration_val()))
        } else {
          w <- c(max(0, min(zt[1], duration_val())),
                 min(duration_val(), max(zt[2], 0)))
          if (w[1] >= w[2]) c(0, min(60, duration_val())) else w
        }
        shiny::updateSliderInput(session, "zoom_time", value = zoom_window)
      }
    })

    # -- Session end ----------------------------------------------------------

    shiny::observeEvent(input$end_session, {
      if (rois_changed()) {
        message_text <- "There are unsaved ROIs. Save before ending?"
      } else {
        message_text <- "No unsaved changes. End session?"
      }
      shiny::showModal(shiny::modalDialog(
        title = "End Session", message_text,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          if (rois_changed()) {
            shiny::tagList(
              shiny::actionButton("save_and_exit", "Save & Exit",
                style = "color: #fff; background-color: #337ab7;"
              ),
              shiny::actionButton("confirm_exit", "Exit without saving",
                style = "color: #fff; background-color: #b73333;"
              )
            )
          } else {
            shiny::actionButton("confirm_exit", "End session",
              style = "color: #fff; background-color: #b73333;"
            )
          }
        ),
        easyClose = TRUE
      ))
    })

    shiny::observeEvent(input$save_and_exit, {
      handle_save_roi_table()
      .safe_cleanup_temp_files(session_data$temp_path)
      shiny::stopApp()
    })

    shiny::observeEvent(input$confirm_exit, {
      .safe_cleanup_temp_files(session_data$temp_path)
      shiny::stopApp()
    })

    session$onSessionEnded(function() {
      con <- shiny::isolate(roi_con())
      if (!is.null(con)) {
        try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
      }
      .safe_cleanup_temp_files(session_data$temp_path)
      # UIX-22(b)/B8: tear down the prefetch daemon pool we started.
      if (isTRUE(.prefetch_state$ready)) {
        .prefetch_teardown()  # AS-02: log shutdown failure, never swallow
      }
    })

    # -- Tooltips -------------------------------------------------------------

    pop_up_opt <- list(delay = list(show = 1000, hide = 0))

    tooltips <- list(
      roi_user = "Format: 'FirstName I. S.' (no commas)",
      soundscapes_path = "Root folder containing soundscape files",
      roi_db = "Path to the DuckDB database for ROIs",
      templates_path = "Output folder for audio cuts and spectrograms",
      user_setup_confirm = "Confirm settings to begin",
      label_angle = "Label rotation angle (90 degrees recommended)",
      wl = "FFT window length (affects time/frequency resolution)",
      ovlp = "Window overlap % (higher = better resolution)",
      pitch_shift = "Adjust playback pitch for ultrasound",
      dyn_range = "Amplitude range shown in spectrogram",
      show_label = "Toggle ROI labels visibility",
      time_guide_interval = "Time guide interval (s)",
      freq_guide_interval = "Frequency guide interval (kHz)",
      color_scale = "Spectrogram color scheme",
      default_pars = "Reset to default settings",
      zoom_freq_slider = "Adjust visible frequency range",
      zoom_time = "Adjust visible time range",
      prev_soundscape_noroi = "Previous unprocessed file",
      prev_soundscape = "Previous file",
      play_soundscape = "Play visible section",
      next_soundscape = "Next file",
      next_soundscape_noroi = "Next unprocessed file",
      no_soi = "Mark as no signals of interest (erases ROIs)",
      soundscape_file = "Select current soundscape",
      roi_label_list = "Choose label list",
      label_name = "Name for next ROI",
      signal_type = "Type of sound",
      label_certainty = "Confidence in identification",
      lock_label_certainty = "Lock label certainty value",
      signal_is_complete = "Signal fully captured in ROI",
      lock_is_complete = "Lock signal completeness value",
      label_comment = "Free-text notes about this ROI (optional)",
      lock_comment = "Lock label comment",
      save_roi = "Save current ROI table to database",
      export_selected_cut = "Export selected ROI audio",
      delete_selected_rois = "Remove selected ROIs",
      show_detections = "Draw the detections of this soundscape as a read-only overlay",
      det_score_min = "Show/promote only detections at or above this peak score",
      det_template = "Show/promote only detections from this template"
    )

    for (id in names(tooltips)) {
      shinyBS::addTooltip(session, id = id, title = tooltips[[id]],
        placement = if (id %in% c(
          "zoom_time", "soundscape_file", "roi_label_list",
          "label_name", "signal_type", "label_certainty",
          "signal_is_complete", "label_comment",
          "save_roi", "export_selected_cut",
          "delete_selected_rois", "prev_soundscape_noroi",
          "prev_soundscape", "play_soundscape",
          "next_soundscape", "next_soundscape_noroi"
        )) "bottom" else "right",
        trigger = "hover", options = pop_up_opt
      )
    }
  }

  shiny::shinyApp(ui = ui, server = server)
}
