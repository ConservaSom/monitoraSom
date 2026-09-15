#' Launch the validation app
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Launches an interactive Shiny app for reviewing the detections produced by
#'   the template-matching pipeline. For each detection you see its spectrogram
#'   (and, when templates are provided, the template beside it), listen to the
#'   audio, and mark it True Positive (TP), False Positive (FP), Unknown (UN) or
#'   leave it Not Validated (NV). The validated labels are written to a
#'   database you can carry into the diagnostics step.
#'
#' @details
#'   The app reads the rows under review from `input_path` and writes your
#'   validations back to the SAME database: the
#'   raw rows are never destroyed - only their `val_*` columns (and, in ROI
#'   mode, `roi_label`) are updated by `signal_id`. The path accepts a DuckDB
#'   database (the routine format) or a CSV from earlier versions (deprecated;
#'   the app warns on use), autodetected by file extension. In a DuckDB
#'   database the app works on the `signals` table: `mode = "detections"`
#'   reads `signal_class = "detection"` rows, `mode = "rois"` reads `"roi"` +
#'   `"detection_to_roi"` rows; the old `detections`/`validations` layouts are
#'   tolerated on read. `templates_path` is optional: without it the
#'   interactive, it must run in an R session with a browser and is not scriptable.
#'   A step-by-step walkthrough is planned for the validation-app vignette. The UI
#'   is built on a \pkg{shinydashboard} shell. Since monitoraSom 1.2.0,
#'   every relative path is anchored under `project_path` and every standard
#'   default is resolved from `.monitora_db_names()`/`.monitora_db_homes()`, so
#'   a workspace laid out by [set_workspace()] works with all path arguments
#'   omitted (except `validation_user`).
#'
#'   Bringing data from versions before 1.2.0: a CSV detections table first
#'   moves into a DuckDB detections database with
#'   [migrate_detections_csv_to_duckdb()], and a detections database from a
#'   separate store moves into the unified `signals.duckdb` database with
#'   `migrate_detections_store_to_signals()`. The signals database it writes
#'   is the `input_path` this app reads. Old `detections`/`validations`
#'   database layouts are also tolerated on read (see `input_path`).
#'
#' @section Pipeline context:
#'   Step 14 of the monitoraSom analysis flow. Reads detections from
#'   [template_matching()] / [run_matching()] (step 7), as a DuckDB database.
#'   Produces a validations database (TP/FP/UN/NV labels) for the diagnostics
#'   step ([diagnostic_validations()]).
#'
#' @param project_path Character path to the project folder. Defaults to `"."`.
#'   Created if it does not exist.
#' @param preset_path Character path from which presets can be imported and to
#'   which new presets can be exported. `NULL` (default) auto-derives it from
#'   `project_path` as `<project_path>/app_presets/`.
#' @param validation_user Character tag identifying who is validating; stamped on
#'   every validation row (`validation_user`). Required.
#' @param templates_path Character path to the template wave files. This parameter
#'   is OPTIONAL; the app works without templates (the template comparison panel
#'   is disabled). When `NULL` (default), looks for `"templates/"` under
#'   `project_path` and simply stays without templates when the folder is
#'   absent.
#' @param soundscapes_path Character path to the soundscape wave files. OPTIONAL
#'   relocation root. Each row under review is read at the `soundscape_path`
#'   recorded in the store; this folder is scanned (once) only for rows whose
#'   recorded path no longer resolves, and only as a fallback. A basename that
#'   matches more than one file there is left unresolved (the cell is shown
#'   without audio) rather than risk reviewing the wrong recording. When `NULL`
#'   (default), uses `"soundscapes/"` under `project_path` when it exists; if it
#'   does not, the app runs with no fallback at all. A supplied path that does
#'   not exist is an error.
#' @param input_path Character path to the `signals` store to validate:
#'   a `.duckdb` database whose `signals` table holds the rows under review
#'   (ROIs and detections plus their `val_*` verdicts). In `mode = "detections"` the
#'   `signal_class = "detection"` rows are read; in `mode = "rois"` the
#'   `"roi"` + `"detection_to_roi"` rows. A `detections`-only database and a
#'   `.csv` from earlier versions (deprecated) are tolerated. Format is
#'   autodetected by file extension. When `NULL` (default), uses the standard
#'   `"detections/detections.duckdb"` under `project_path` (errors when the
#'   file does not exist). Validations are persisted BACK to this same
#'   database (there is no separate output database).
#' @param mode Character, the review mode: `"detections"` (default) or
#'   `"rois"`. It only sets the starting mode; inside the app you can switch
#'   between the two at any time. In `"rois"` mode the detections-only
#'   arguments (`detec_spec_path`, `detec_cuts_path`) are ignored.
#' @param detec_cuts_path Character path to the folder for exported detection
#'   cut WAV files. Defaults to `"detection_cuts/"` (auto-created if missing).
#' @param detec_spec_path Character path to the folder for exported detection
#'   spectrogram images. Defaults to `"detection_spectrograms/"` (auto-created).
#' @param wav_player_path `r lifecycle::badge("deprecated")` Ignored; audio
#'   playback uses the built-in HTML player.
#' @param wav_player_type `r lifecycle::badge("deprecated")` Ignored; audio
#'   playback uses the built-in HTML player.
#' @param val_subset Character vector selecting which validation statuses are
#'   included in the review set. Accepted values: `"NV"` (unvalidated, the
#'   default inclusion), `"TP"`, `"FP"`, `"UN"`. Multiple values can be combined
#'   (e.g. `c("NV", "TP")` shows unvalidated plus previously marked TP
#'   detections). Default `c("NV", "TP", "FP", "UN")` includes everything.
#'   The filter only changes what is displayed for review: no detection or
#'   ROI row is excluded from, or added to, the input data.
#' @param time_pads Numeric seconds of context added before and after each
#'   detection when cutting the audio for review. Default `1`. Useful for
#'   playback and spectrogram visualization of the sound before and after the
#'   detected event.
#' @param ovlp Overlap percentage (0-80, step 10). Default `0`. Higher values
#'   give smoother time resolution; same meaning as everywhere else in the
#'   package.
#' @param wl FFT window length (power of 2, 128-16384). Default `2048`. Larger
#'   values give finer frequency resolution at the cost of coarser time
#'   resolution.
#' @param dyn_range_templ Numeric length-2, display dynamic range in dB for the
#'   template spectrogram panel (e.g. `c(-84, 0)`). Values are sorted
#'   automatically; defaults to the range of the `dyn_range_bar` slider.
#' @param dyn_range_detec Numeric length-2, display dynamic range in dB for the
#'   detection spectrogram panel (e.g. `c(-84, 0)`).
#' @param dyn_range_bar Numeric length-2 in dB (`c(-144, 0)` default). Sets the
#'   maximum range of the dynamic-range sliders in the UI; the per-panel
#'   `dyn_range_templ`/`dyn_range_detec` must fall within this interval.
#' @param color_scale Color scale: "viridis", "magma", "inferno" (default),
#'   "cividis", "greyscale 1", "greyscale 2".
#' @param zoom_freq Numeric vector of length 2: initial frequency range (kHz).
#'   Default `c(0, 23)`. Values are sorted and rounded to 0.1 kHz.
#' @param subset_seed Integer seed for the random shuffling of the detection
#'   review order when `"Random"` is the selected ordering. A fixed seed makes
#'   the review order reproducible across sessions. Default `123`.
#' @param auto_next Logical, default `TRUE`. When `TRUE`, the next detection is
#'   automatically displayed after you validate the current one.
#' @param nav_autosave Logical, default `TRUE`. When `TRUE`, validations are
#'   saved automatically when you advance to a new page (hotkeys `Z`/`C`, the
#'   on-screen page arrows, or the `NV` button).
#' @param overwrite Logical, default `TRUE`. When `TRUE`, an existing `val_*`
#'   verdict on a row under review is replaced on save; when `FALSE`, existing
#'   validated rows are kept unchanged, but new verdicts are added (e.g. when
#'   resuming an earlier session).
#' @param pitch_shift Pitch shift value in octaves for playback: `-8`, `-6`,
#'   `-4`, `-2` (slow down / lower pitch) or `1` (no shift, default).
#' @param visible_bp Logical, default `FALSE`. When `TRUE`, applies a bandpass
#'   filter matching the visible frequency band (`zoom_freq`) to the audio
#'   playback: you hear only the sound within that band.
#' @param play_norm Logical, default `FALSE`. When `TRUE`, the played audio is
#'   normalized before playback.
#' @param time_guide_interval Interval in seconds between time guides.
#'   Default `0` (disables the guides).
#' @param freq_guide_interval Interval in kHz between frequency guides.
#'   Default `0` (disables the guides).
#' @param grid_dim Integer grid dimension for the detection panel:
#'   `1L`-`4L`, default `3L` (3x3 = 9 cells per page). `1L` shows
#'   one large detection cell beside the reference (template) column - the
#'   old single-detection view; hide the reference column with the
#'   \dQuote{Template} toggle to see the detection even wider. The page size is
#'   `grid_dim^2`; navigation, hotkeys and prefetch adapt automatically. A
#'   UI selector can change it during the session.
#' @param skip_path_confirmation Logical, default `TRUE`. When `TRUE`, the paths
#'   setup is confirmed automatically at startup without user interaction.
#' @param labels_file Path to a label-list `.txt` file (one label per line;
#'   the list name is the file name without the `label_list_` prefix), a
#'   directory of such files, or, for back-compatibility, the old `.xlsx` file
#'   with one list per column. When `NULL` (the default), the loader falls back
#'   to the project preset (`app_presets/label_list/`, then the legacy
#'   `app_presets/roi_label_lists.xlsx`) and then to the bundled dataset; if
#'   none is available the ROI-label field degrades to an empty choice list
#'   (typed labels still work).
#' @param roi_label_list Name of the default ROI label list column.
#'
#' @return Called for its side effect: it starts an interactive Shiny app in the
#'   browser and returns when the app is closed. Validations are persisted back
#'   to the `signals` store at `input_path`.
#' @seealso [template_matching()], [diagnostic_validations()],
#'   [launch_segmentation_app()]
#' @export
#' @import shiny
#' @importFrom dplyr %>%
#' @importFrom shinyjqui jqui_resizable
#' @examples
#' # Step 14 of the analysis flow: review and label the pipeline's detections.
#' # Interactive app; run it in a session with a browser. See the validation-app
#' # vignette for a full walkthrough.
#' # The example is self-contained: it synthesizes a short recording with two
#' # tone bursts, runs the detection steps on it, and opens the app on the
#' # resulting detections. Each launch needs an interactive session; the
#' # example is written to be run twice (first launch, then a second one that
#' # resumes with `overwrite = FALSE`).
#' if (interactive()) {
#'   # Load the package
#'   library(monitoraSom)
#'
#'   project_path <- tempfile("lva_example")
#'   dir.create(project_path)
#'
#'   # 1. Synthesize one 8 s soundscape with two 4 kHz bursts.
#'   sr <- 16000
#'   tt <- seq_len(8 * sr) / sr
#'   rec_x <- 0.6 * sin(2 * pi * 4000 * tt)
#'   rec_x[tt < 0.5 | (tt >= 1.5 & tt < 3) | tt >= 3.5] <- 0
#'   rec <- tuneR::normalize(tuneR::Wave(rec_x, samp.rate = sr, bit = 16),
#'                           unit = "16")
#'   soundscapes_dir <- file.path(project_path, "soundscapes")
#'   dir.create(soundscapes_dir, showWarnings = FALSE)
#'   tuneR::writeWave(rec, file.path(soundscapes_dir, "siteA_01.wav"))
#'
#'   # 2. Cut one 4 kHz burst as the template.
#'   df_rois <- data.frame(
#'     soundscape_path = file.path(soundscapes_dir, "siteA_01.wav"),
#'     soundscape_file = "siteA_01.wav",
#'     roi_label = "burst", roi_start = 0.5, roi_end = 1,
#'     roi_min_freq = 3, roi_max_freq = 5, roi_wl = 512, roi_ovlp = 50,
#'     stringsAsFactors = FALSE)
#'   templates_dir <- file.path(project_path, "templates")
#'   export_templates(df_rois, templates_path = templates_dir,
#'                   create_dir = TRUE)
#'
#'   # 3. Run the matching step and save the detections into the signals store
#'   #    the app reads (min_score keeps the two real bursts).
#'   df_grid <- fetch_match_grid(
#'     fetch_soundscape_metadata(soundscapes_dir),
#'     fetch_template_metadata(templates_dir))
#'   input_path <- file.path(project_path, "signals.duckdb")
#'   run_matching(df_grid, score_method = "fft", min_score = 0.5,
#'               output_db = input_path)
#'
#'   # 4. Launch the app on the detections and validate them.
#'   launch_validation_app(
#'     project_path = project_path, validation_user = "User",
#'     soundscapes_path = soundscapes_dir,
#'     input_path = input_path,
#'     mode = "detections"
#'   )
#'
#'   # A standardized, reproducible validation session: fix the seed so the
#'   # "Random" review order is the same in every session (two people, or two
#'   # runs, review the detections in the same order), and keep your previous
#'   # verdicts while adding new ones (resume after the first launch above).
#'   launch_validation_app(
#'     project_path = project_path, validation_user = "User",
#'     soundscapes_path = soundscapes_dir,
#'     input_path = input_path,
#'     mode = "detections",
#'     subset_seed = 2026, overwrite = FALSE
#'   )
#' }
launch_validation_app <- function(
    project_path = ".", preset_path = NULL, validation_user,
    templates_path = NULL, soundscapes_path = NULL, input_path = NULL,
    mode = c("detections", "rois"), detec_spec_path = NULL, detec_cuts_path = NULL,
    wav_player_path = "play", wav_player_type = "HTML player",
    val_subset = c("NV", "TP", "FP", "UN"), time_pads = 1, ovlp = 0, wl = 2048,
    dyn_range_bar = c(-144, 0), dyn_range_templ = c(-84, 0),
    dyn_range_detec = c(-84, 0), color_scale = "inferno", zoom_freq = c(0, 23),
    time_guide_interval = 0, freq_guide_interval = 0, subset_seed = 123,
    auto_next = TRUE, nav_autosave = TRUE, overwrite = TRUE, pitch_shift = 1,
    visible_bp = FALSE, play_norm = FALSE, skip_path_confirmation = TRUE,
    grid_dim = 3L, labels_file = NULL,
    roi_label_list = "Brazilian birds (Pacheco et al. 2021)"
  ) {

  # FEAT-07: default-path write guard. project_path defaults to ".", so a
  # user who passes nothing would write under the session working directory;
  # warn (and confirm, interactively) unless the target is a marked workspace.
  .require_explicit_workspace(
    project_path,
    default_target = !"project_path" %in% names(match.call())[-1L],
    label = "project_path", caller = "launch_validation_app")

  old_opts <- options(dplyr.summarise.inform = FALSE)
  on.exit(options(old_opts), add = TRUE)
# Validate dynamic range vectors
#
# Validates that a dynamic range vector is a numeric vector of length 2 with the
# first value less than the second. Sorts if necessary.
#
# @param dyn_range A numeric vector of length 2.
# @param name The name of the parameter being validated (for error messages).
# @return A sorted numeric vector of length 2.
# @keywords internal
validate_dyn_range <- function(dyn_range, name) {
  if (length(dyn_range) != 2 || !all(is.numeric(dyn_range))) {
    stop(
      paste(
        "Error! '",
        name,
        "' must be a numeric vector of length 2 with all values numeric.",
        sep = ""
      )
    )
  }
  if (dyn_range[1] >= dyn_range[2]) {
    warning(
      paste(
        "Warning! The first value of '",
        name,
        "' must be smaller than the second. Sorting to match the expected order.",
        sep = ""
      )
    )
    return(sort(dyn_range))
  }
  return(dyn_range)
}

# Validate and set a path, with auto-creation
#
# Validates a path for storing files. If the path is NULL, the default path is
# used. If the default path does not exist, it is created automatically. If a
# custom path is provided and does not exist, an error is raised.
#
# @param path The path to validate. May be NULL.
# @param default_path The default path to use when path is NULL.
# @param session_key A human-readable label for the path (used in messages).
# @return The validated (and possibly default) path.
# @keywords internal
validate_and_set_path <- function(path, default_path, session_key) {
  if (is.null(path)) {
    if (dir.exists(default_path)) {
      warning(paste(
        "Warning! The informed '",
        session_key,
        "' was not found locally; using the existing default path '",
        default_path,
        "'.",
        sep = ""
      ))
    } else {
      dir.create(default_path, recursive = TRUE)
      warning(paste(
        "Warning! The informed '",
        session_key,
        "' was not found locally and was created automatically as '",
        default_path,
        "'.",
        sep = ""
      ))
    }
    return(default_path)
  } else {
    if (dir.exists(path)) {
      return(path)
    } else {
      stop(paste(
        "Error! The provided path to store ",
        session_key,
        " was not found locally.",
        sep = ""
      ))
    }
  }
}

# Validate a logical parameter
#
# Checks that a value is logical (TRUE or FALSE). If not, raises an error with
# the parameter name in the message.
#
# @param value The value to check.
# @param name The name of the parameter (for error messages).
# @return The logical value, unchanged.
# @keywords internal
validate_logical_param <- function(value, name) {
  if (is.logical(value)) {
    return(value)
  } else {
    stop(
      "Error! The value assigned to '", name,
      "' is not logical. Set it to TRUE or FALSE."
    )
  }
}

# Validate zoom_freq parameter
#
# Validates the zoom_freq parameter: sorts the values first, then checks that
# all values are within `[0, 192]`, and rounds to 0.1 intervals unconditionally.
#
# @param zoom_freq A numeric vector of length 2 representing the frequency
#   range in kHz.
# @return A sorted numeric vector of length 2, rounded to 0.1 kHz, with a
#   warning if rounding changed the values.
# @keywords internal
validate_zoom_freq <- function(zoom_freq) {
  # Sort first
  zoom_freq <- sort(zoom_freq)

  # Range-check on sorted values
  if (any(zoom_freq < 0) || any(zoom_freq > 192)) {
    stop("Error! 'zoom_freq' values must be between 0 and 192.")
  }

  # Round unconditionally
  rounded <- round(zoom_freq * 10) / 10
  if (any(rounded != zoom_freq)) {
    warning(
      "Warning! The values of 'zoom_freq' were rounded to the nearest 0.1 ",
      "interval. The values are now: ",
      rounded[1], " and ", rounded[2]
    )
  }
  return(rounded)
}

# Validate the input path
#
# Validates that the input path is not NULL, the file exists, and carries a
# supported extension. LVA-142: the routine input is a detections `.duckdb`
# store (aligning with [detecs_to_rois()] / [validate_by_overlap()]); a legacy
# `.csv` file is still accepted but is deprecated (the caller warns on use).
# A `.csv` input must additionally be non-empty. Guarded against NULL
# dereference.
#
# @param input_path The path to the input file (`.duckdb` or, deprecated,
#   `.csv`).
# @return The validated path, unchanged.
# @keywords internal
validate_input_path <- function(input_path) {
  if (is.null(input_path)) {
    stop(
      "Error! The input file path was not provided (NULL). ",
      "Provide a valid path to a detections `.duckdb` store (or a legacy CSV)."
    )
  }
  if (!file.exists(input_path)) {
    stop(
      "Error! The input file '", input_path, "' does not exist."
    )
  }
  ext <- tolower(tools::file_ext(input_path))
  if (!ext %in% c("duckdb", "csv")) {
    stop(
      "Error! The input file '", input_path, "' must be a detections `.duckdb` ",
      "store or a legacy `.csv` file."
    )
  }
  if (ext == "csv" && file.size(input_path) == 0) {
    stop(
      "Error! The input file '", input_path, "' is empty."
    )
  }
  input_path
}

# LVA-155 + item 8: the validation I/O helpers (.lva_read_detections_input /
# .lva_read_rois_input / .lva_write_validations_output /
# .lva_read_validations_output / .lva_frame_to_signals, plus the signals
# probe/projection) live in _lva_validation_helpers.R -- the single canonical
# copy, always loaded before this file (underscore-prefixed files
# canonical copy, always loaded before this file (underscore-prefixed files
# collate first in the package build; tests and daemon workers source it
# explicitly). The in-app duplicates that used to live here were removed.

# LVA-109 (same decision as LVA-155, extended 2026-08-18): the render helpers
# (.lva_stabilization_series / .lva_cut_window / .lva_build_cell_plot /
# .lva_img_key / .lva_render_cell_png) -- previously inlined here
# "byte-identical" to _lva_validation_helpers.R for the one-shot assembler
# (R/sandbox/_assemble_lva_app.R, Round-1-only and stale since Round 2) -- were
# also removed from this file. The canonical copies live in
# _lva_validation_helpers.R (identical code bodies, verified 2026-08-18);
# editing them in only one place was already a live bug trap.



  # input validation -----------------------------------------------------------

  session_data <- list()

  # CRAN item 7 (F9): the validation user is required and is validated FIRST,
  # before any directory is created. The no-argument audit found app_presets/
  # was created before this check, leaving artifacts behind on a failed call.
  if (missing(validation_user) || is.null(validation_user) ||
      !is.character(validation_user) || length(validation_user) != 1L ||
      is.na(validation_user) || !nzchar(trimws(validation_user))) {
    stop("'validation_user' is required: identify yourself with a single, ",
         "non-empty name (e.g. launch_validation_app(validation_user = \"Ana\")).")
  }

  if (!is.null(project_path)) {
    tryCatch(
      {
        if (!dir.exists(project_path)) {
          dir.create(project_path)
          warning(
            "The validation app project directory was successfully created at '", project_path, "'"
          )
        }
        session_data$project_path <- project_path
      },
      error = function(e) {
        stop(
          "Failed to create validation app project directory at '",
          project_path, "': ", e$message
        )
      }
    )
  }

  if (!is.null(preset_path)) {
    if (!dir.exists(preset_path)) {
      dir.create(preset_path)
      if (!dir.exists(preset_path)) {
        stop(
          "Error! The selected preset destination folder does not exist and could not be created."
        )
      }
      warning(
        "The segmentation preset destination directory was created automatically at '",
        preset_path,
        "'"
      )
    }
    session_data$preset_path <- preset_path

    # The creation of the temp directory assumes that the preset directory
    # exists
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

  # Validate and set the validation user (gate already ran at the top, F9);
  # the comma is stripped for CSV column-safety.
  session_data$validation_user <- as.character(gsub(",", "", validation_user))

  # CRAN item 3 (sect. 2c): every relative path below is anchored under
  # project_path ('.' default = legacy cwd behaviour). Canonical defaults come
  # from .monitora_db_names()/.monitora_db_homes() (sect. 2e), never literals.
  anchor <- function(path) {
    .resolve_under_project(path, project_path)
  }
  templates_path <- if (!is.null(templates_path)) anchor(templates_path)
  soundscapes_path <- if (!is.null(soundscapes_path)) anchor(soundscapes_path)
  input_path <- if (!is.null(input_path)) anchor(input_path)
  detec_cuts_path <- if (!is.null(detec_cuts_path)) anchor(detec_cuts_path)
  detec_spec_path <- if (!is.null(detec_spec_path)) anchor(detec_spec_path)
  detec_cuts_path <- if (!is.null(detec_cuts_path)) anchor(detec_cuts_path)
  detec_spec_path <- if (!is.null(detec_spec_path)) anchor(detec_spec_path)

  # Validate and set the templates path (LVA-100: templates are OPTIONAL).
  # The template panel is auxiliary; the core validation workflow reads only the
  # soundscape WAV + the detection CSV. When no usable templates directory is
  # present we set `session_data$templates_available <- FALSE` and warn, instead
  # of stopping. Behaviour is unchanged when a valid templates path is provided.
  session_data$templates_available <- FALSE
  .lva_templates_has_wav <- function(p) {
    isTRUE(dir.exists(p)) &&
      length(list.files(p, pattern = "\\.wav$", recursive = TRUE,
                        ignore.case = TRUE)) > 0
  }
  if (is.null(templates_path)) {
    templates_path <- anchor(.monitora_db_homes()[["templates"]])
    if (.lva_templates_has_wav(templates_path)) {
      session_data$templates_path <- templates_path
      session_data$templates_available <- TRUE
    } else {
      session_data$templates_path <- NULL
      warning(
        "Warning! No template wave files were found. The template comparison ",
        "panel will be disabled; detection validation continues normally."
      )
    }
  } else if (.lva_templates_has_wav(templates_path)) {
    session_data$templates_path <- templates_path
    session_data$templates_available <- TRUE
  } else {
    session_data$templates_path <- if (dir.exists(templates_path)) {
      templates_path
    } else {
      NULL
    }
    warning(
      "Warning! The provided templates path is missing or has no WAV files. ",
      "The template comparison panel will be disabled; detection validation ",
      "continues normally."
    )
  }

# Validate and set the soundscapes path (STEP-3 / DEC-6, plan 2026-09-14_01).
  # OPTIONAL relocation root: the review frame resolves each recording from the
  # store first, so the tree is only scanned for rows whose recorded path no
  # longer resolves. When nothing is supplied (and no `soundscapes/` default
  # exists) the session runs without a fallback -- rows that fail the
  # stored-first step show the "audio not found" placeholder.
  if (is.null(soundscapes_path)) {
    default_ss <- anchor(.monitora_db_homes()[["soundscapes_metadata"]])
    session_data$soundscapes_path <- if (dir.exists(default_ss)) {
      default_ss
    } else {
      NULL
    }
  } else if (!dir.exists(soundscapes_path)) {
    stop(
      "Error! The provided path to the soundscape wave files was not found locally."
    )
  } else {
    session_data$soundscapes_path <- soundscapes_path
  }

  # Validate and set the input path (canonical default:
  # detections/detections.duckdb; errors when absent - detections are a
  # required input, not auto-created).
  if (is.null(input_path)) {
    input_path <- anchor(.monitora_db_default_path("detections"))
  }
  # DEC-20 (STEP-19): a legacy CSV input triggers an offer to convert it to
  # the signals store (portability with no friction). The converters already
  # exist and are exported: migrate_detections_csv_to_duckdb() (CSV ->
  # detections duckdb) and migrate_detections_store_to_signals() (detections
  # duckdb -> signals). The conversion is NOT forced: the session keeps the
  # CSV path (the app reads it, deprecated, and warns), and the message below
  # gives the exact two calls to run for the durable store.
  if (grepl("(?i)\\.csv$", input_path)) {
    store <- sub("\\.csv$", ".duckdb", input_path, ignore.case = TRUE)
    message("The input is a legacy CSV. Convert it once to the signals store ",
            "for full persistence:\n  migrate_detections_csv_to_duckdb('",
            input_path, "', '", store, "')\n  migrate_detections_store_to_signals('",
            store, "', '", store, "')\nThen launch with input_path = '",
            store, "'.")
  }
  session_data$input_path <- validate_input_path(input_path)

  # LVA item 8 / plan 2026-08-18_02 sect. 10.5: single-store model - validations
  # are persisted back to the SAME signals store at `input_path` (the separate
  # `output_path` argument is removed).

  if (all(val_subset %in% c("NV", "TP", "FP", "UN"))) {
    session_data$val_subset <- val_subset
  } else {
    stop(
      "Error! At least one of the values assigned to 'val_subset' are not within ",
      "the accepted alternatives ('NV', 'TP', 'FP', 'UN')."
    )
  }

  # validate time_guide_interval and freq_guide_interval
  # LVA item 8 / plan sect. 8.1: 0 disables the guides (parity with the
  # segmentation app); invalid values fall back to 0 (disabled), not 3/2.
  if (!is.numeric(time_guide_interval) || is.na(time_guide_interval) ||
      time_guide_interval < 0) {
    session_data$time_guide_interval <- 0
  } else {
    session_data$time_guide_interval <- time_guide_interval
  }
  if (!is.numeric(freq_guide_interval) || is.na(freq_guide_interval) ||
      freq_guide_interval < 0) {
    session_data$freq_guide_interval <- 0
  } else {
    session_data$freq_guide_interval <- freq_guide_interval
  }

  if (!is.numeric(time_pads) || time_pads < 0 || time_pads > 16) {
    stop(
      "Error! The value assigned to 'time_pads' must be a numeric value between 0 and 16."
    )
  }
  session_data$time_pads <- time_pads

  # LVA-109 / CRAN item 4: adjustable square grid 1x1..4x4 (page = grid_dim^2).
  if (!is.numeric(grid_dim) || length(grid_dim) != 1L || is.na(grid_dim) ||
      !grid_dim %in% 1:4) {
    stop("Error! The value assigned to 'grid_dim' must be a single integer from 1 to 4.")
  }
  session_data$grid_dim <- as.integer(grid_dim)

  # LVA item 8 / plan 2026-08-18_02 sect. 5 passo 1: review mode - detections or ROIs.
  mode <- match.arg(mode)
  session_data$mode <- mode

  # Function to validate dynamic range vectors
  # Validate and set dynamic ranges
  session_data$dyn_range_templ <- validate_dyn_range(
    dyn_range_templ,
    "dyn_range_templ"
  )
  session_data$dyn_range_detec <- validate_dyn_range(
    dyn_range_detec,
    "dyn_range_detec"
  )
  session_data$dyn_range_bar <- validate_dyn_range(
    dyn_range_bar,
    "dyn_range_bar"
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
      "Error! The value assigned to 'ovlp' must be a numeric value between 0 and ",
      "80, in steps of 10."
    )
  }
  session_data$ovlp <- ovlp

  # Validate 'color_scale' against expected values
  valid_color_scales <- c(
    "viridis",
    "magma",
    "inferno",
    "cividis",
    "greyscale 1",
    "greyscale 2"
  )
  if (!is.character(color_scale) || !(color_scale %in% valid_color_scales)) {
    stop(sprintf(
      "Error! The value assigned to 'color_scale' must be one of the following: %s.",
      paste(valid_color_scales, collapse = ", ")
    ))
  }
  session_data$color_scale <- color_scale

  session_data$wav_player_type <- "HTML player"

  session_data$zoom_freq <- validate_zoom_freq(zoom_freq)

  if (!is.numeric(subset_seed)) {
    stop("Error! Non-numeric value input provided to 'seed'")
  }
  session_data$subset_seed <- subset_seed

  # LVA-17: validate logical params via extracted helper
  for (nm in c("visible_bp", "play_norm", "auto_next", "nav_autosave", "overwrite")) {
    session_data[[nm]] <- validate_logical_param(get(nm), nm)
  }

  session_data$detec_cuts_path <- validate_and_set_path(
    detec_cuts_path,
    anchor("detection_cuts/"),
    "detec_cuts_path"
  )
  session_data$detec_spec_path <- validate_and_set_path(
    detec_spec_path,
    anchor("detection_spectrograms/"),
    "detec_spec_path"
  )

  if (!is.numeric(pitch_shift) || !(pitch_shift %in% c(-8, -6, -4, -2, 1))) {
    stop(
      "Error! The value assigned to 'pitch_shift' is not numeric or not among ",
      "the expected alternatives: -8, -6, -4, -2, or 1."
    )
  } else {
    session_data$pitch_shift <- pitch_shift
  }

  # LVA item 8.4 (plan 2026-08-18_03 sect. 8.4): species label lists for the
  # ROI-label selectize, reusing the segmentation helper
  # (.load_roi_label_lists: explicit file > project preset > bundled dataset).
  # The load degrades to an empty choice list when nothing is available (the
  # app stays usable; the selectize still accepts typed labels). The default
  # column mirrors the segmentation app.
  roi_label_lists <- tryCatch(
    .load_roi_label_lists(labels_file, project_path),
    error = function(e) NULL
  )
  if (!is.null(roi_label_lists)) {
    if (!roi_label_list %in% colnames(roi_label_lists)) {
      warning(
        "Selected ROI label list not found. Using first available list: '",
        colnames(roi_label_lists)[1], "'.", .msg_ref("001")
      )
      roi_label_list <- colnames(roi_label_lists)[1]
    }
    label_choices <- dplyr::pull(roi_label_lists, roi_label_list)
  } else {
    label_choices <- character(0)
  }

  # hotkeys --------------------------------------------------------------------

  hotkeys <- c(
    "q", # label selected cell = TP
    "w", # navigate selection up (R4.1 grid)
    "e", # label selected cell = FP
    "f", # label selected cell = UN (R4.1 grid)
    "r", # export wav
    "t", # export spectrogram
    # "y", #
    "a", # navigate selection left (R4.1 grid)
    "s", # navigate selection down (R4.1 grid)
    "d", # navigate selection right (R4.1 grid)
    "m", # apply the Mark-switch value to the selected cell (R4.1 grid;
    # was Space -- browser-scroll conflict)
    "shift+q", # validate the page's NV cells as TP (R4.1 grid)
    "shift+e", # validate the page's NV cells as FP (R4.1 grid)
    "z", # previous page, sequential (R4.2; replaces the removed "g" refresh)
    "c", # next page, sequential (R4.2)
    "ctrl+s", # save output (moved off "s", which now navigates)
    "1", # play detection
    "2", # play template
    "alt+k", # confirm/refresh session setup
    "alt+l" # apply label + detection note to the active ROI (2026-08-19)
  )

  # resource paths -------------------------------------------------------------

  # This block defines where embedded html wav players will look for the files
  shiny::addResourcePath("audio", temp_path)

  # Spectrogram-parameters accordion panel -- defined once here so it can live in
  # the bottom accordion (moved out of the sidebar per the R4.1 inc.4 redesign).
  spectro_params_panel <- shinydashboard::menuItem(
    "Spectrogram Parameters",
    icon = shiny::icon("sliders"),
    # Sidebar panel (moved from the body tabBox, 2026-08-18). Widgets are
    # DIRECT children of the menuItem (matching the User setup / Session
    # settings tabs) with width = "100%" - the previous fluidRow(column(...))
    # nesting double-counted the bootstrap padding and overflowed the 400px
    # sidebar (plan 2026-08-18_02 sect. 10.3 D2). The two audio checkboxes stay
    # side by side via splitLayout (the Session settings pattern).
    shiny::sliderInput(
      "zoom_freq", "Frequency zoom (kHz)", min = 0, max = 192, step = 0.1,
      value = session_data$zoom_freq, width = "100%"
    ),
    shiny::sliderInput(
      "time_pads", "Pad size (s):", min = 0, max = 16, step = 0.5,
      value = session_data$time_pads, post = "s", width = "100%"
    ),
    shiny::sliderInput(
      "dyn_range_templ", "Template dyn range (dB)",
      min = session_data$dyn_range_bar[1], max = session_data$dyn_range_bar[2],
      value = session_data$dyn_range_templ, step = 6, post = "dB", width = "100%"
    ),
    shiny::sliderInput(
      "dyn_range_detec", "Detection dyn range (dB)",
      min = session_data$dyn_range_bar[1], max = session_data$dyn_range_bar[2],
      value = session_data$dyn_range_detec, step = 6, post = "dB", width = "100%"
    ),
    shinyWidgets::sliderTextInput(
      "wl", "Window length:",
      choices = c(128, 256, 512, 1024, 2048, 4096, 8192, 16384),
      grid = TRUE, selected = session_data$wl, width = "100%"
    ),
    shiny::sliderInput(
      "ovlp", "Overlap (%):", min = 0, max = 80, step = 10,
      value = session_data$ovlp, post = "%", width = "100%"
    ),
    shinyWidgets::sliderTextInput(
      "pitch_shift", "Pitch / slow down",
      choices = c(-8, -6, -4, -2, 1), selected = session_data$pitch_shift,
      grid = TRUE, post = " oct", width = "100%"
    ),
    shiny::numericInput(
      "time_guide_interval", "Time guide (s)",
      value = session_data$time_guide_interval, min = 0, max = 60, step = 0.01
    ),
    shiny::numericInput(
      "freq_guide_interval", "Freq guide (kHz)",
      value = session_data$freq_guide_interval, min = 0, max = 192, step = 0.01
    ),
    shiny::selectInput(
      "color_scale", "Color",
      choices = c(
        "viridis", "magma", "inferno", "cividis",
        "greyscale 1", "greyscale 2"
      ),
      selected = session_data$color_scale, width = "100%"
    ),
    # The two audio checkboxes stay side by side in one row.
    shiny::splitLayout(
      cellWidths = c("50%", "50%"),
      shiny::checkboxInput(
        "visible_bp", "Visible band only", value = session_data$visible_bp
      ),
      shiny::checkboxInput(
        "play_norm", "Normalize audio", value = session_data$play_norm
      )
    ),
    shiny::actionButton(
      inputId = "get_templ_pars", label = "Get current template parameters",
      icon = shiny::icon("gear"), width = "360px",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4;"
    ),
    shiny::actionButton(
      inputId = "default_pars", label = "Reset to default parameters",
      icon = shiny::icon("gear"), width = "360px",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4;"
    )
  )

  # app ------------------------------------------------------------------------

  shiny::shinyApp(
    # UI -----------------------------------------------------------------------
    ui = shinydashboard::dashboardPage(
      skin = "black",
      header = shinydashboard::dashboardHeader(
        title = "monitoraSom", titleWidth = "400px"
      ),

      # Sidebar ----------------------------------------------------------------
      sidebar = shinydashboard::dashboardSidebar(
        width = "400px",
        shinydashboard::sidebarMenu(
          # User setup ---------------------------------------------------------
          shinydashboard::menuItem(
            "1 - User setup",
            icon = shiny::icon("user"),
            shiny::textAreaInput(
              inputId = "preset_path",
              label = "Path to preset files (.rds)",
              value = session_data$preset_path,
              placeholder = "Paste or load path here",
              height = "40px",
              width = "395px",
              resize = "vertical"
            ),
            shiny::textInput(
              "validation_user",
              "User name (*):",
              value = session_data$validation_user,
              placeholder = "Identify yourself here",
              width = "100%"
            ),
            shiny::textAreaInput(
              "templates_path",
              "Templates path (*)",
              value = session_data$templates_path,
              placeholder = "Paste or load path here",
              height = "40px",
              resize = "vertical",
              width = "100%"
            ),
            shiny::textAreaInput(
              "soundscapes_path",
              "Soundscapes path (*)",
              value = session_data$soundscapes_path,
              placeholder = "Paste or load path here",
              height = "40px",
              width = "395px",
              resize = "vertical"
            ),
            shiny::textAreaInput(
              "input_path",
              "Signals store path (*)",
              value = session_data$input_path,
              placeholder = "Paste or load path here",
              height = "40px",
              width = "395px",
              resize = "vertical"
            ),
            # LVA item 8 / plan 2026-08-18_02 sect. 5 passo 5: mode switch (shiny
            # base radioButtons - no shinyWidgets) setting the initial state of
            # the in-session mode toggle.
            shiny::radioButtons(
              "validation_mode",
              "Review mode",
              choices = c("Detections" = "detections", "ROIs" = "rois"),
              selected = session_data$mode,
              inline = TRUE
            ),
            .btn_confirm("user_setup_confirm", "Confirm Setup"),
            shinyBS::bsTooltip(
              "user_setup_confirm",
              title = paste0("<b>Part 1 of 2 required to start the session</b>. ",
                "All inputs marked with (*) are required for this step"),
              placement = "right",
              trigger = "hover",
              options = list(delay = list(show = 1000, hide = 0))
            ),
            tags$style(".tooltip {width: 300px;}")
          ),

          # Session setup ------------------------------------------------------

          shinydashboard::menuItem(
            "2 - Session settings",
            icon = shiny::icon("check"),
            # LVA item 8.2 follow-up (2026-08-19): open by default so the
            # review setup (Signal filter, order, val_subset) is visible on
            # launch (user decision).
            startExpanded = TRUE,
            # LVA item 8.2: the field label is mode-dependent ("Signal" in
            # ROI mode, "Template file (*)" in detections mode). The label
            # stays inside the widget (updateSelectInput changes it).
            selectInput(
              "template_name",
              "Template file (*)",
              choices = NULL,
              width = "100%"
            ),
            shiny::selectizeInput(
              "val_subset",
              "Filter validation inputs (*)",
              choices = c(
                "True positives - TP" = "TP",
                "False positives - FP" = "FP",
                "Unknown - UN" = "UN",
                "Not validated - NV" = "NV"
              ),
              selected = session_data$val_subset,
              multiple = TRUE,
              width = "100%"
            ),
            # convert in an interval
            shiny::sliderInput(
              "score_interval",
              "Score interval (*)",
              width = "100%",
              min = -1,
              max = 1,
              step = 0.01,
              value = c(0, 1)
            ),
            # top n detections
            shiny::splitLayout(
              cellWidths = c("50%", "50%"),
              # UIX-12: was a textInput holding a number, so any string reached
              # the server and `as.numeric()` turned it into NA. numericInput
              # gives the browser's spinner/step affordance and rejects
              # non-numeric entry up front. 0 keeps its "no limit" meaning.
              shiny::numericInput(
                "top_n_detecs",
                "Top detections",
                value = 0,
                min = 0,
                step = 1,
                width = "100%"
              ),
              shiny::checkboxInput(
                "top_by_file",
                "Search top scores within soundscape files",
                value = FALSE,
                width = "100%"
              )
            ),
            # arrange the order of validation per score
            shiny::selectInput(
              "order_by",
              "Order by",
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
              # UIX-15: "Score" is not one of the choices above; selectInput then
              # falls back silently to the first option, so the app opened in
              # "Original file order" while the UI claimed a score ordering.
              # Re-applied 2026-08-05: the fix landed in `8691527` against the
              # navbar build, and was lost when `da07438` promoted this
              # shinydashboard build to canonical.
              selected = "Score (DESC)",
              width = "100%"
            ),
            shiny::numericInput(
              "subset_seed",
              "Seed for random subsetting",
              value = session_data$subset_seed,
              step = 1  # UIX-12: give the field its spinner affordance
            ),
            .btn_confirm("confirm_session_setup", "Confirm validation setup")
          ),

          # Display & audio settings (panel behaviour toggles) ---------------

          shinydashboard::menuItem(
            "Display",
            icon = shiny::icon("table-cells"),
            # LVA-109 / CRAN item 4: grid dimension selector - 1x1..4x4, launch
            # value preselected. Page size = dim^2; navigation/hotkeys/prefetch
            # adapt automatically.
            shiny::selectInput(
              "grid_dim",
              "Grid size",
              choices = c("1x1" = 1, "2x2" = 2, "3x3" = 3, "4x4" = 4),
              selected = as.character(session_data$grid_dim),
              width = "100%"
            ),
            # Reference spectrogram dropdown - direct child of the menuItem
            # (matching the Session settings pattern). Disabled is applied in
            # the server (shinyjs::disable), not in the UI wrapper.
            shiny::selectInput(
              "custom_reference", "Reference spectrogram",
              choices = NULL, width = "100%"
            ),
            tags$div(
              title = "Lock: keep this reference spectrogram when moving to the next detection",
              shiny::checkboxInput(
                "lock_template", "Lock reference",
                value = TRUE
              )
            ),
            # Behaviour checkboxes - splitLayout pairs (Session settings
            # pattern), then the 5th as a direct child.
            shiny::splitLayout(
              cellWidths = c("50%", "50%"),
              shiny::checkboxInput(
                "show_reference", "Show template", value = TRUE
              ),
              shiny::checkboxInput(
                "auto_next", "Autonav", value = session_data$auto_next
              )
            ),
            shiny::splitLayout(
              cellWidths = c("50%", "50%"),
              shiny::checkboxInput(
                "nav_autosave", "Autosave", value = session_data$nav_autosave
              ),
              shiny::checkboxInput(
                "overwrite", "Overwrite", value = session_data$overwrite
              )
            ),
            shiny::checkboxInput(
              "enable_player", "Show audio player", value = TRUE
            )
          ),

          # Spectrogram Parameters (moved from the body tab) -----------------

          spectro_params_panel,

          .btn_danger_wide("end_session", "End validation session")
        )
      ),

      # Body (kept as one nav panel for R4.0) --------------------------------

      body = shinydashboard::dashboardBody(
        # Normal flow keeps the fixed-height detection grid's height; the page
        # scrolls instead (the .lva-accordion-slot CSS caps the lower panels so
        # they shrink rather than starve the grid).
        # Set up shinyjs
        shinyjs::useShinyjs(),

        # Make keyboard shotcuts available
        keys::useKeys(),
        keys::keysInput("hotkeys", hotkeys),

        # Avoid blinking / white figures while rendering: keep recalculating plots
        # (incl. the grid's cached cells) fully opaque so a cell never flashes white
        # while its new spectrogram is computed (R4.3 -- supports the no-NV-flash fix).
        tags$style(type = "text/css", HTML(
          ".path-ok{border:2px solid #33b733 !important;}
           .path-bad{border:2px solid #b73333 !important;}
           .recalculating{opacity:1 !important;}
           .lva-cell .recalculating, .lva-cell img.recalculating,
           .lva-cell .shiny-plot-output.recalculating{opacity:1 !important;}"
        )),
        tags$head(tags$style(HTML(".content-wrapper { overflow: auto; }"))),

        # R4.3-fix (round 3): decoration<->plot synchronisation. Each cell's status
        # border must appear in lockstep with ITS spectrogram, never before it. The
        # server publishes the target classNames in `window.lvaTargets`; this
        # listener applies a cell's target the instant its <img> finishes loading
        # (capture phase -- `load` does not bubble). So on a page change the previous
        # (consistent) border is held on the still-showing old image until the new
        # image loads, then flips together -- no "new border on the old plot" and no
        # blanking. Same-page label changes are applied directly by the server (no
        # image reload).
        tags$head(tags$script(HTML(
          "window.lvaTargets = window.lvaTargets || [];
           document.addEventListener('load', function(e){
             var img = e.target;
             if (!img || img.tagName !== 'IMG') return;
             var w = img.closest ? img.closest('.lva-cell') : null;
             if (!w || !w.id) return;
             var k = parseInt(w.id.replace('cellwrap_',''), 10);
             if (!isNaN(k) && window.lvaTargets[k-1]) {
               w.className = window.lvaTargets[k-1];
             }
           }, true);"
        ))),


        # R4.1 grid styles -------------------------------------------------------
        tags$head(tags$style(HTML(
          # R4.3: the grid card is full width. A LEFT column (1/4 of the width)
          # holds the relocated controls in three rows that line up with the three
          # grid rows: row 1 = validation/page-nav controls, row 2 = the reference
          # cell, row 3 = behaviour toggles + HTML players. The reactive 3x3 grid
          # takes the remaining 3/4 (flex 3:1), so each of its columns equals the
          # left column width -> all four columns are identical boxes.
          #
          # R4.3-fix: the WHOLE Validation tab is one viewport-tall flex column
          # (.lva-mainwrap). The grid slot GROWS to fill whatever is left after the
          # fixed-height control panel and the accordion, so the grid is always
          # fully visible and never overflows -- no hardcoded per-panel offset. The
          # accordion slot is capped and scrolls internally when a panel is opened,
          # so opening it shrinks (never starves past min) the grid instead of
          # pushing it off-screen. Only the navbar height is a constant (it is the
          # one element outside the flex column).
          ".lva-mainwrap{display:flex;flex-direction:column;gap:8px;
             height:calc(100vh - 70px);min-height:420px;}
           /* UIX-04: single semantic verdict palette (saturated = borders/
              badges, soft = button/label fills), referenced by both the
              .lva-* classes and the verdict buttons below. */
           :root{--lva-tp:#2ca25f;--lva-fp:#de2d26;--lva-un:#f39c12;
             --lva-tp-soft:#6ae46a;--lva-fp-soft:#ff7e7e;--lva-un-soft:#ffba52;}
           /* LVA-140: the grid panel has a USER-SET height (resizable, south
              handle); the accordion below absorbs the remainder so the tab body
              stays one viewport tall (no page scroll). Was: grid flex:1 1 auto +
              accordion capped 45vh. */
           .lva-grid-slot{flex:0 0 auto;height:58vh;min-height:180px;
             min-width:0;display:flex;flex-direction:column;position:relative;}
           /* shinydashboard box replaces the bslib card: propagate the
              resizable grid-slot height down the col-sm-12 > box > box-body
              chain so the detection grid fills (and follows) the drag handle
              instead of keeping its natural content height. */
           .lva-grid-slot > .col-sm-12{flex:1 1 auto;min-height:0;padding:0;
             display:flex;flex-direction:column;}
           .lva-grid-slot > .col-sm-12 > .box{flex:1 1 auto;min-height:0;
             margin-bottom:0;display:flex;flex-direction:column;}
           .lva-grid-slot .box-body{flex:1 1 auto;min-height:0;overflow:hidden;
             display:flex;flex-direction:column;}
           /* visible drag grip on the south resize handle (jQuery UI) */
           .lva-grid-slot > .ui-resizable-s{height:9px;cursor:row-resize;
             background:#adb5bd55;border-radius:0 0 4px 4px;}
           .lva-grid-slot > .ui-resizable-s:hover{background:#2c7fb8aa;}
           .lva-accordion-slot{flex:1 1 auto;min-height:80px;overflow-y:auto;}
           .lva-gridwrap{display:flex;gap:6px;align-items:stretch;
             flex:1 1 auto;min-height:0;}
           .lva-leftcol{display:flex;flex-direction:column;gap:6px;
             flex:1 1 0;min-width:0;}
           /* LVA-109: rows no longer align to grid rows (the grid may be 1x1..4x4).
              Rows 1/3 keep their natural height; row 2 (the reference cell)
              grows to fill - the column is independent of the grid dim. */
           .lva-leftrow{display:flex;flex-direction:column;min-height:0;}
           .lva-leftrow-fixed{flex:0 0 auto;}
           .lva-leftrow-grow{flex:1 1 auto;}
           .lva-hide{display:none !important;}
           /* LVA-109 parity (2026-08-18): the template cell must match a
              detection cell EXACTLY (same pixel dimensions), so the column
              width tracks the grid dim: grow = dim, template = 1 - 3x3 gives
              the template a fourth column of the grid, 1x1 gives half the
              width, 2x2 a third, 4x4 a fifth. The widths are set from JS by
              .lva_paint_cells (flex-grow swap: grid grows 2xdim in 1x1). */
           .lva-refcell{position:relative;border:5px solid #4d4d4d;
             border-radius:4px;overflow:hidden;flex:1 1 0;min-height:0;}
           /* LVA-109 / plan 2026-08-18_02 sect. 10.4 P3: lock the reference
              spectrogram to the same 4.2:3 aspect as the cell PNGs (1.4), so
              template and detection cells show their content at the same
              aspect ratio. The wrapper fills the refcell; the plot is
              letterboxed inside it. */
           .lva-refcell .lva-refcell-fit{position:absolute;inset:0;
             aspect-ratio:4.2 / 3;max-width:100%;max-height:100%;margin:auto;}
           .lva-refcell .lva-badge{background:#4d4d4d;}
           .lva-refcell .lva-badge::after{content:'REF';}
           /* LVA-109: the column/row count is set from JS (repeat(dim,1fr)) by
              .lva_paint_cells per active dim; 3x3 is the launch default. flex:1
              lets the grid fill the remaining width (with or without the
              reference column). */
           .lva-grid{display:grid;grid-template-columns:repeat(3,1fr);
             grid-template-rows:repeat(3,1fr);gap:6px;flex:3 1 0;min-width:0;}
           .lva-cell{position:relative;border:5px solid #adb5bd;
             border-radius:4px;cursor:pointer;overflow:hidden;min-height:0;}
           /* LVA-107 (extended): the cell is now an imageOutput; the served PNG
              fills the cell box (was a renderCachedPlot sized to the container). */
           .lva-cell .shiny-image-output, .lva-cell img{
             width:100%!important;height:100%!important;object-fit:contain;
             display:block;}
           .lva-tp{border-color:var(--lva-tp);}
           .lva-fp{border-color:var(--lva-fp);}
           .lva-un{border-color:var(--lva-un);}
           .lva-nv{border-color:#adb5bd;}
           /* LVA-109 / plan 2026-08-18_02 sect. 10.2 passo C: empty trailing cells
              (no status class - e.g. the last page has fewer rows than dim^2)
              stay VISIBLE for stable page geometry but are inert: no pointer
              events, so clicks/right-clicks on them cannot re-target the
              selection or advance the page. Statused cells keep .lva-tp/
              .lva-fp/.lva-un/.lva-nv and stay interactive. */
           .lva-grid .lva-cell:not(.lva-tp):not(.lva-fp):not(.lva-un):not(.lva-nv){
             pointer-events:none;}
           .lva-cell-selected{outline:3px solid #2c7fb8;outline-offset:1px;
             box-shadow:0 0 8px 3px rgba(44,127,184,.9);}
           .lva-badge{position:absolute;top:5px;right:5px;width:30px;height:30px;
             border-radius:50%;display:flex;align-items:center;
             justify-content:center;font-weight:bold;color:#fff;font-size:12px;
             z-index:5;background:#adb5bd;box-shadow:0 0 3px rgba(0,0,0,.6);}
           .lva-tp .lva-badge{background:var(--lva-tp);}
           .lva-tp .lva-badge::after{content:'TP';}
           .lva-fp .lva-badge{background:var(--lva-fp);}
           .lva-fp .lva-badge::after{content:'FP';}
           .lva-un .lva-badge{background:var(--lva-un);}
           .lva-un .lva-badge::after{content:'UN';}
           .lva-nv .lva-badge::after{content:'-';}
           /* compact the left-column widgets so row 1 (buttons) and row 3
              (toggles) fit their thirds and the checkboxes sit side by side */
           .lva-leftcol .form-group{margin-bottom:0;}
           .lva-leftcol .shiny-input-container{margin-bottom:0;width:100% !important;}
           .lva-toggles .checkbox{margin:0;}
           .lva-toggles .checkbox label{font-size:12px;white-space:nowrap;}
           .lva-toggles audio{width:100%;height:30px;}
           /* relocated fields (reference/detection): label one line, input below */
           .lva-ctl-field{min-width:0;}
           .lva-ctl-field > .control-label{display:block;margin-bottom:2px;
             white-space:nowrap;overflow:hidden;text-overflow:ellipsis;
             font-weight:600;}
           .lva-ctl-field .form-group{margin-bottom:0;}
           .lva-ctl-field .shiny-input-container{width:100% !important;}
           .lva-ctl-inputrow{display:flex;gap:6px;align-items:center;}
           .lva-ctl-input{flex:1 1 auto;min-width:0;}
           /* BS5 does not force width:100% on shiny's .form-control <select>; do it
              here (covers both the native fallback and the selectize control) so the
              reference/detection selects fill their field and never overflow the lock */
           .lva-ctl-field select, .lva-ctl-input select{width:100% !important;
             max-width:100%;}
           .lva-ctl-field .selectize-control,
           .lva-ctl-input .selectize-control{width:100% !important;}
           /* Gate review (2026-08-19): the Reference spectrogram dropdown must
              contain its rendered text inside the available box. The select is
              vertically resizable (resize:vertical) so a long template path
              can be read in full. Height doubled (gate review 2026-08-19). */
           #custom_reference{width:100% !important;max-width:100% !important;
             text-overflow:ellipsis;white-space:nowrap;overflow:hidden;
             resize:vertical;height:2em !important;}
           /* compact inline lock toggle (just the icon checkbox, fixed width) */
           .lva-lockfield{flex:0 0 auto;}
           .lva-lockfield .form-group{margin:0;}
           .lva-lockfield .checkbox{margin:0;white-space:nowrap;}
           .lva-lockfield .checkbox label{padding-left:0;}"
        ))),

        # R4.3-fix: the Validation tab body is one viewport-tall flex column so the
        # grid, the compact control panel and the accordion share the height
        # sensibly (grid grows, the other two keep their natural height).
        tags$div(
          class = "lva-mainwrap",

        # Detection grid -- FULL WIDTH. The left column holds the relocated
        # controls; the reactive 3x3 grid is unchanged. LVA-140: the whole panel is
        # vertically resizable (shinyjqui, south handle only -- width stays 100%); the
        # accordion below absorbs the change so the tab body stays one viewport tall.
        # The cells reflow automatically (CSS grid + imageOutput object-fit), so no
        # resizestop re-render is needed.
        shinyjqui::jqui_resizable(
        tags$div(
          class = "lva-grid-slot",
        shinydashboard::box(
          title = "Detection grid", width = 12,
          tags$div(
            class = "lva-gridwrap",
            # LEFT COLUMN (R4.3) -- three rows aligned with the three grid rows. It
            # holds the relocated controls (row 1), the reference cell (row 2) and
            # the behaviour toggles + HTML players (row 3). The reference cell is
            # NOT part of the reactive grid (no click handler, no cellwrap id, not
            # in the .LVA_CELLS_MAX loop) -- purely a visual reference whose box now
            # matches a detection cell exactly (same width 1/4 + same row height).
            tags$div(
              id = "lva_leftcol",
              class = "lva-leftcol",
              # --- row 1 (fixed): validation + page-validation + mark + page nav --
              tags$div(
                class = "lva-leftrow lva-leftrow-fixed",
                style = "justify-content:flex-start;gap:4px;",
                # TP / UN / FP
                tags$div(
                  style = "display:flex;gap:4px;align-items:center;",
                  shiny::actionButton(
                    "button_tp", "TP (Q)",
                    style = paste0(
                      "flex:1;padding:5px 0;color:#000;",
                      "background:var(--lva-tp-soft);font-weight:bold;"
                    )
                  ),
                  shiny::actionButton(
                    "button_un", "UN (F)",
                    style = paste0(
                      "flex:1;padding:5px 0;color:#000;",
                      "background:var(--lva-un-soft);font-weight:bold;"
                    )
                  ),
                  shiny::actionButton(
                    "button_fp", "FP (E)",
                    style = paste0(
                      "flex:1;padding:5px 0;color:#000;",
                      "background:var(--lva-fp-soft);font-weight:bold;"
                    )
                  )
                ),
                # Panel TP / Panel FP (NV-only batch on the current page)
                tags$div(
                  style = "display:flex;gap:4px;align-items:center;",
                  shiny::actionButton(
                    "panel_tp", "Panel TP (Shift+Q)",
                    style = "flex:1;padding:4px 0;color:#000;background:#a7e8a7;font-weight:bold;"
                  ),
                  shiny::actionButton(
                    "panel_fp", "Panel FP (Shift+E)",
                    style = "flex:1;padding:4px 0;color:#000;background:#ffb3b3;font-weight:bold;"
                  )
                ),
                # Mark TP / Mark FP (right-click paint mode) -- justified fills width
                shinyWidgets::radioGroupButtons(
                  "mark_mode", label = NULL,
                  choices = c("Mark TP" = "TP", "Mark FP" = "FP"),
                  selected = "TP", status = "primary", size = "sm",
                  justified = TRUE, width = "100%"
                ),
                # Page counter (moved here from the card header) + page navigation.
                tags$div(
                  style = "text-align:center;font-weight:bold;font-size:13px;",
                  shiny::textOutput("page_counter", inline = TRUE)
                ),
                # Page navigation: <NV < | > >NV -- each button stretches to fill
                tags$div(
                  style = "display:flex;gap:4px;align-items:stretch;",
                  shiny::actionButton(
                    "prev_page_nv", "NV", icon = shiny::icon("backward-step"),
                    class = "btn-sm", style = "flex:1;",
                    title = "Previous page with unvalidated (NV) detections"
                  ),
                  shiny::actionButton(
                    "prev_page", "", icon = shiny::icon("backward"),
                    class = "btn-sm", style = "flex:1;", title = "Previous page (Z)"
                  ),
                  shiny::actionButton(
                    "next_page", "", icon = shiny::icon("forward"),
                    class = "btn-sm", style = "flex:1;", title = "Next page (C)"
                  ),
                  shiny::actionButton(
                    "next_page_nv", "NV", icon = shiny::icon("forward-step"),
                    class = "btn-sm", style = "flex:1;",
                    title = "Next page with unvalidated (NV) detections"
                  )
                ),
                # Save (full width)
                shiny::actionButton(
                  "button_save", "Save",
                  width = "100%",
                  style = "padding:5px 0;color:#fff;background:#000;font-weight:bold;"
                ),
                # LVA-109: the reference dropdown moved to the Display sidebar
                # menu (2026-08-18); the lock stays here with it.
              ),
              # --- row 2 (grows): reference cell -----------------------------------
              tags$div(
                id = "lva_refcell_row",
                class = "lva-leftrow lva-leftrow-grow",
                tags$div(
                  id = "lva_refcell",
                  class = "lva-refcell",
                  tags$div(class = "lva-badge"),
                  tags$div(
                    class = "lva-refcell-fit",
                    shiny::plotOutput("ref_cell", height = "100%")
                  )
                )
              ),
              # --- row 3 (fixed): HTML players + detection fields -----------------
              tags$div(
                class = "lva-leftrow lva-leftrow-fixed lva-toggles",
                style = "justify-content:flex-start;gap:2px;",
                # LVA-109: the Autonav/Autosave/Overwrite/HTML player toggles and
                # the "Show template" checkbox moved to the Display sidebar menu
                # (Session settings); this row keeps only the HTML player targets
                # (insertUI injects the <audio> here; hotkeys 1/2 drive them) and
                # the detection fields.
                tags$div(
                  style = "display:flex;gap:6px;flex-wrap:nowrap;",
                  tags$div(id = "template_player", style = "flex:1;min-width:0;"),
                  tags$div(id = "detection_player", style = "flex:1;min-width:0;")
                ),
                # Detection ID -- relocated here (below the HTML players).
                tags$div(
                  class = "lva-ctl-field",
                  tags$label("Detection ID", class = "control-label"),
                  shiny::selectInput(
                    "detec", label = NULL, choices = NULL, width = "100%"
                  )
                ),
                # Detection notes [lock] -- relocated here (below the HTML players).
                tags$div(
                  class = "lva-ctl-field",
                  tags$label("Detection notes", class = "control-label"),
                  tags$div(
                    class = "lva-ctl-inputrow",
                    tags$div(
                      class = "lva-ctl-input",
                      shiny::textInput(
                        "detec_note", label = NULL, value = NA_character_,
                        placeholder = "Write detection notes here", width = "100%"
                      )
                    ),
                    tags$div(
                      class = "lva-lockfield",
                      title = "Lock: keep this note when moving to the next detection",  # UIX-13
                      shiny::checkboxInput(
                        "lock_detec_note", shiny::icon("lock", lib = "font-awesome"),
                        value = FALSE
                      )
                    )
                  )
                ),
                # ROI label [re-labelling] -- visible in ROI mode only (item 8 /
                # plan 2026-08-18_02 sect. 5 passo 9): edits roi_label_updated, leaving
                # roi_label (creation label) immutable. Hidden for detections.
                tags$div(
                  id = "roi_label_field",
                  class = "lva-ctl-field",
                  tags$label("ROI label", class = "control-label"),
                  tags$div(
                    class = "lva-ctl-inputrow",
                    tags$div(
                      class = "lva-ctl-input",
                      # LVA item 8.4: selectize over the species sheet choices
                      # (segmentation-app pattern; choices are pushed
                      # server-side from the active ROI's label).
                      shiny::selectizeInput(
                        "roi_label", label = NULL, choices = NULL,
                        selected = NULL, multiple = FALSE, width = "100%",
                        # LVA item 8.2 follow-up (2026-08-19): the dropdown
                        # is attached to <body> so the option list is never
                        # clipped by the panel's overflow.
                        options = list(create = TRUE, persist = FALSE,
                                       dropdownParent = "body")
                      )
                    )
                  ),
                  shiny::actionButton(
                    "roi_label_confirm", "Apply label",
                    style = paste0(
                      "margin-top:4px;color:#000;background:var(--lva-tp-soft);",
                      "font-weight:bold;width:100%;"
                    )
                  )
                )
              )
            ),
            # CENTRE/RIGHT -- the reactive detection grid (LVA-109: 16 static
            # slots cover the 4x4 max; the active dim hides the cells beyond
            # dim^2 and sets the CSS template - the renderImage slots are
            # registered ONCE, so the cells must NOT be renderUI). Cells are
            # height:100% inside the height-bounded, n-equal-row grid (R4.3)
            # so the whole grid is always visible and resizes cleanly.
            tags$div(
              class = "lva-grid",
              lapply(seq_len(16), function(k) {
                tags$div(
                  id = paste0("cellwrap_", k),
                  class = "lva-cell",
                  # LVA-109 / plan 2026-08-18_02 sect. 10.2 passo B: the 16 static slots
                  # cover the 4x4 max; the launch default is 3x3 (9 cells), so
                  # cells 10-16 are hidden STATICALLY to avoid the collapsed
                  # clickable slivers that showed before the first paint.
                  style = if (k > 9L) "display:none;" else NULL,
                  onclick = sprintf(
                    "Shiny.setInputValue('cell_click', %d, {priority:'event'})",
                    k
                  ),
                  oncontextmenu = sprintf(
                    paste0(
                      "Shiny.setInputValue('cell_rclick', %d, ",
                      "{priority:'event'}); return false;"
                    ),
                    k
                  ),
                  tags$div(class = "lva-badge"),
                  # LVA-107 (extended): cells are now imageOutput -- the spectrogram
                  # PNG is rendered off-thread by a mirai daemon and served as a
                  # static image (bypassing the main-thread renderCachedPlot), so a
                  # page turn to a prefetched page is instant.
                  shiny::imageOutput(paste0("cell_", k), height = "100%")
                )
              })
            )
          )
        )
        ), # /lva-grid-slot
        options = list(handles = "s") # LVA-140: vertical resize only
        ), # /jqui_resizable

        # Outputs --------------------------------------------------------------

        # Accordion -- fixed natural height; capped + internally scrollable (CSS
        # .lva-accordion-slot) so opening a panel shrinks (never starves) the grid
        # rather than pushing it off-screen. Hosts the Spectrogram Parameters that
        # used to sit in the sidebar.
        tags$div(
          class = "lva-accordion-slot",
        shinydashboard::tabBox(
          width = 12,
          id = "tabset1",
          # tabBox always shows one open tab (unlike the bslib accordion);
          # Progress is first so the detection grid stays unobstructed on launch.

          # Progress -----------------------------------------------------------
          shiny::tabPanel(
            "Progress",
            icon = shiny::icon("chart-simple"),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                h5("Full dataset progress"),
                shinyWidgets::progressBar(
                  id = "prog_bar_full",
                  value = 0,
                  total = 1,
                  status = "info",
                  display_pct = TRUE,
                  striped = TRUE
                ),
                shiny::tableOutput("count_full_tab")
              ),
              shiny::column(
                width = 6,
                h5("Current subset progress"),
                shinyWidgets::progressBar(
                  id = "prog_bar_subset",
                  value = 0,
                  total = 1,
                  status = "info",
                  display_pct = TRUE,
                  striped = TRUE
                ),
                shiny::tableOutput("count_i_tab")
              )
            )
          ),

          # Hotkeys -------------------------------------------------------------

          shiny::tabPanel(
            "Hotkeys",
            icon = shiny::icon("keyboard"),
            tags$div(
              style = "padding:8px 4px;",
              tags$h5("Keyboard & mouse reference"),
              # R4.2: split into two side-by-side columns (cell-level on the left,
              # page-level + global on the right) for better horizontal use.
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  tags$table(
                    class = "table table-sm",
                    tags$thead(tags$tr(
                      tags$th("Cell action"), tags$th("Effect")
                    )),
                    tags$tbody(
                      tags$tr(
                        tags$td(
                          tags$kbd("W"), " ", tags$kbd("A"), " ",
                          tags$kbd("S"), " ", tags$kbd("D")
                        ),
                        tags$td("Move the selected cell (up / left / down / right)")
                      ),
                      tags$tr(tags$td(tags$kbd("Q")),
                              tags$td("Mark selected cell True Positive")),
                      tags$tr(tags$td(tags$kbd("E")),
                              tags$td("Mark selected cell False Positive")),
                      tags$tr(tags$td(tags$kbd("F")),
                              tags$td("Mark selected cell Unknown")),
                      tags$tr(
                        tags$td(tags$kbd("M")),
                        tags$td("Apply the current Mark-mode label (TP/FP) to the selected cell")
                      ),
                      tags$tr(tags$td("Left-click cell"),
                              tags$td("Select the cell")),
                      tags$tr(tags$td("Right-click cell"),
                              tags$td("Cycle the cell's label TP -> FP -> UN"))
                    )
                  )
                ),
                shiny::column(
                  width = 6,
                  tags$table(
                    class = "table table-sm",
                    tags$thead(tags$tr(
                      tags$th("Page / global"), tags$th("Effect")
                    )),
                    tags$tbody(
                      tags$tr(
                        tags$td(tags$kbd("Z"), " / ", tags$kbd("C")),
                        tags$td("Previous / next page (sequential)")
                      ),
                      tags$tr(
                        tags$td("NV page buttons"),
                        tags$td("Jump to the previous / next page with unvalidated (NV) detections")
                      ),
                      tags$tr(
                        tags$td(tags$kbd("Shift"), " + ", tags$kbd("Q")),
                        tags$td("Validate the page's NV cells as TP (also the Panel TP button)")
                      ),
                      tags$tr(
                        tags$td(tags$kbd("Shift"), " + ", tags$kbd("E")),
                        tags$td("Validate the page's NV cells as FP (also the Panel FP button)")
                      ),
                      tags$tr(
                        tags$td(tags$kbd("1"), " / ", tags$kbd("2")),
                        tags$td("Play active detection / play template")
                      ),
                      tags$tr(
                        tags$td(tags$kbd("R"), " / ", tags$kbd("T")),
                        tags$td("Export wav cut / export spectrogram")
                      ),
                      tags$tr(
                        tags$td(tags$kbd("Ctrl"), " + ", tags$kbd("S")),
                        tags$td("Save the validation output")
                      ),
                      tags$tr(
                        tags$td(
                          tags$kbd("Ctrl"), " + ", tags$kbd("Shift"), " + ",
                          tags$kbd("K")
                        ),
                        tags$td("Confirm / refresh the paths setup")
                      ),
                      tags$tr(
                        tags$td(tags$kbd("Alt"), " + ", tags$kbd("K")),
                        tags$td("Confirm / refresh the session setup")
                      )
                    )
                  )
                )
              )
            )
          ),

          # Detection Table -----------------------------------------------------

          shiny::tabPanel(
            "Detection Table",
            icon = shiny::icon("table"),
            DT::DTOutput("res_table")
          ),

          # Export Detection -----------------------------------------------------

          shiny::tabPanel(
            "Export Detection",
            icon = shiny::icon("file-export"),
            # R4.0 fix: bare grid columns need a fluidRow inside the tab card.
            # AV-01 pilot: the widgets live in lva_export_ui (shiny module).
            lva_export_ui(
              LVA_EXPORT_ID,
              cuts_path_default = session_data$detec_cuts_path,
              spec_path_default = session_data$detec_spec_path
            )
          ),

          # Diagnostics ----------------------------------------------------------

          shiny::tabPanel(
            "Diagnostics",
            icon = shiny::icon("chart-line"),
            # R4.0 fix: bare grid columns need a fluidRow inside the bslib tab card
            # (3 controls wrap to one row, 3 plots to the next at 4+4+4 widths).
            shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::selectInput(
                "diag_balance",
                "Dataset balance method",
                choices = c(
                  "None",
                  "Downsample larger class",
                  "Upsample smaller class",
                  "ROSE"
                ),
                selected = "None",
                width = "100%"
              ),
            ),
            shiny::column(
              width = 4,
              shiny::selectInput(
                "diag_method",
                "Cutpoint detection method",
                choices = c("Manual", "Error = 0.05", "Error = 0.1"),
                width = "100%"
              )
            ),
            shiny::column(
              width = 4,
              shiny::sliderInput(
                "diag_cut",
                "Cutpoint threshold",
                min = 0,
                max = 1,
                step = 0.001,
                value = 0.2,
                width = "100%"
              )
            ),
            shiny::column(
              width = 4,
              shiny::plotOutput("plot_dens", height = "340px")
            ),
            shiny::column(
              width = 4,
              shiny::plotOutput("plot_binomial", height = "340px")
            ),
            # column(width = 3, plotOutput("plot_roc", height = "340px")),
            shiny::column(
              width = 4,
              shiny::plotOutput("plot_prec_rec", height = "340px")
            ),
            # LVA-110: TP/FP score-stabilization plot (wraps onto a new row).
            shiny::column(
              width = 4,
              shiny::plotOutput("plot_stabilization", height = "340px")
            )
            ),
            shiny::tableOutput("cut_i_tab")
          )   # close Diagnostics tabPanel
        )     # close tabBox
        )     # /lva-accordion-slot
        )     # /lva-mainwrap
      )       # close dashboardBody
    ),        # close dashboardPage

    # Server -------------------------------------------------------------------
    server = function(input, output, session) {
      # Create a reactive object for storing the path
      templates_path <- shiny::reactiveVal(NULL)
      # Create a reactive object for storing the path
      soundscapes_path <- shiny::reactiveVal(NULL)
      # Create empty reactive object with the full detection dataset
      df_full <- reactiveValues(data = NULL)
      df_output <- shiny::reactiveVal(NULL)
      # (FACT-17 flag: the former `df_ref_templates` reactive was written but
      # never read; removed in the STEP-1 rewrite, which was its only setter.)

      # LVA-142b: persistent output-store connection (signals table, LVA-155)
      # + dirty-row tracking, so
      # the page-change autosave upserts only the rows that changed (cheap) rather
      # than rewriting the whole DuckDB table each turn (`replace = TRUE` measured
      # ~155 ms/page vs ~8 ms for CSV). The connection is opened lazily on the first
      # DuckDB save and closed in session$onSessionEnded. CSV output keeps the full
      # overwrite. Dirty tracking is by detection_id (pagination-independent -- robust
      # to WASD page crossings that do not autosave), fed by the two validation
      # mutators apply_validation() / validate_panel().
      val_con <- shiny::reactiveVal(NULL)           # open DuckDB connection (or NULL)
      val_store_ready <- shiny::reactiveVal(FALSE)  # store initialised via a full write
      val_dirty <- shiny::reactiveVal(character())  # detection_ids changed since last save
      csv_dep_warned <- shiny::reactiveVal(FALSE)   # LVA-145: CSV deprecation shown once
      validation_order_counter <- shiny::reactiveVal(0L)  # LVA-113: monotonic order
      mark_dirty <- function(ids) {
        ids <- as.character(ids)
        ids <- ids[!is.na(ids)]
        if (length(ids)) {
          val_dirty(unique(c(shiny::isolate(val_dirty()), ids)))
        }
      }
      # LVA-113: compact fingerprint of the validation-subset settings in effect
      # (D23: the `val_subset` filter that defines which detections are in the
      # working set). Recorded on each detection as it is validated.
      .lva_subset_fingerprint <- function() {
        s <- shiny::isolate(input$val_subset)
        if (is.null(s)) return(NA_character_)
        paste(sort(unique(as.character(s))), collapse = ",")
      }
      # LVA-100: templates are optional. TRUE iff the templates dir holds >=1 WAV.
      templates_available <- shiny::reactiveVal(
        isTRUE(session_data$templates_available)
      )

      # The mode radio is passive (plan sect. 8.6: no re-read), but the UI
      # follows it immediately -- pure UI feedback, no data reload:
      #  * the order_by choices swap to the mode's set;
      #  * the template_name dropdown swaps label AND choices (item 8.2).
      #    A mode switch leaves the previous mode's choices until the next
      #    Confirm Setup; repopulate from the loaded frame so the label
      #    filter never points at a value that no longer exists.
      # (.lva_order_choices lives in _lva_validation_helpers.R.)
      shiny::observeEvent(input$validation_mode, {
        shiny::updateSelectInput(
          session, "order_by",
          choices = .lva_order_choices(input$validation_mode)
        )
        # The dropdown mirrors the LOADED frame (what the grid shows), not
        # the radio: after a radio flip without Confirm Setup the frame is
        # still the previous mode's, and the dropdown must stay consistent
        # with it. The ROI frame is identified by its roi_label column;
        # before any read, fall back to the radio's mode.
        df <- shiny::isolate(df_full$data)
        if (!is.null(df) && nrow(df) > 0L && "roi_label" %in% names(df)) {
          lbl_choices <- unique(df$template_name)
          lbl_choices <- lbl_choices[!is.na(lbl_choices)]
          cur <- shiny::isolate(input$template_name)
          if (is.null(cur) || !cur %in% lbl_choices) {
            cur <- .lva_all_labels()
          }
          shiny::updateSelectInput(
            session, "template_name",
            label = "Signal",
            choices = c("All labels" = .lva_all_labels(), lbl_choices),
            selected = cur
          )
        } else if (!is.null(df) && nrow(df) > 0L) {
          shiny::updateSelectInput(
            session, "template_name",
            label = "Template file (*)",
            choices = unique(df$template_name)
          )
        } else {
          lbl <- if (identical(input$validation_mode, "rois")) {
            "Signal"
          } else {
            "Template file (*)"
          }
          shiny::updateSelectInput(session, "template_name", label = lbl)
        }
      })

      # LVA item 8 / plan 2026-08-18_02: single read path. The setup read is
      # a local function so the setup confirm and the mode switch share one read
      # path (testable headless; shinyjs::click does not run in testServer).
      .lva_do_setup_read <- function(mode) {
        # Initial input validation. LVA-148: templates are OPTIONAL (LVA-100) -- the
        # app must load and validate from the detections alone. `templates_path` is
        # therefore NOT req()'d (an empty/NULL path renders the UI input as "" and
        # would otherwise halt this observer silently); availability is soft-checked
        # below (`:1872`) and the template comparison panel is disabled when absent.
        # STEP-3 (plan 2026-09-14_01, DEC-6): `soundscapes_path` is likewise NOT
        # req()'d any more -- it is an OPTIONAL relocation root, so an empty field
        # means "no fallback scan", not "halt this observer silently". Each row
        # resolves its recording from the store first (see the read block below).
        shiny::req(
          input$input_path,
          input$validation_user
        )

        # Validate paths and files
        validation_errors <- character()

        # Check input file
        if (!file.exists(input$input_path)) {
          validation_errors <- c(validation_errors, "Input file does not exist")
        } else {
          # LVA-142: readability smoke test, format-aware. A `.duckdb` store is
          # probed by opening it (schema is ensured on connect); a legacy `.csv`
          # is probed with a one-row fread.
          tryCatch(
            {
              if (grepl("(?i)\\.duckdb$", input$input_path)) {
                con_test <- .detections_duckdb_connect(input$input_path)
                DBI::dbDisconnect(con_test, shutdown = TRUE)
              } else {
                test_read <- data.table::fread(input$input_path, nrows = 1)
              }
            },
            error = function(e) {
              validation_errors <<- c(
                validation_errors,
                sprintf("Cannot read input file: %s", e$message)
              )
            }
          )
        }

        # Check directories. STEP-3 (plan 2026-09-14_01, DEC-6): the soundscapes
        # root is now an OPTIONAL relocation fallback -- each row resolves its
        # recording from the store first -- so only a NON-EMPTY value is checked
        # (an empty field means "no fallback scan", not a setup error).
        # Templates stay optional (LVA-100): a missing/empty dir is a soft
        # warning, not a blocking error.
        ss_input <- input$soundscapes_path
        if (length(ss_input) == 1L && !is.na(ss_input) &&
            nzchar(trimws(ss_input))) {
          if (!dir.exists(ss_input)) {
            validation_errors <- c(
              validation_errors, "Soundscapes directory does not exist"
            )
          } else if (
            length(
              fs::dir_ls(
                ss_input, type = "file", glob = "*.wav", recurse = TRUE,
                ignore.case = TRUE
              )
            ) == 0
          ) {
            validation_errors <- c(
              validation_errors, "No WAV files found in Soundscapes directory"
            )
          }
        }

        # LVA-100: soft templates check -- set availability flag + warn.
        tpl_ok <- !is.null(input$templates_path) &&
          dir.exists(input$templates_path) &&
          length(
            fs::dir_ls(
              input$templates_path, type = "file", glob = "*.wav",
              recurse = TRUE, ignore.case = TRUE
            )
          ) > 0
        templates_available(isTRUE(tpl_ok))
        if (!isTRUE(tpl_ok)) {
          shiny::showNotification(
            paste0(
              "No templates available -- the template comparison panel is ",
              "disabled. Detection validation continues normally."
            ),
            duration = 15,
            type = "warning"
          )
        }

        # (single-store model: validations go back to input_path - no output dir
        # to check)

        # If there are validation errors, show them and stop
        if (length(validation_errors) > 0) {
          shiny::showModal(shiny::modalDialog(
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

        # (single-store model: no separate input/output warning)

        # Safe data loading. STEP-1 (plan 2026-09-14_01): the recording path
        # comes from the STORE first; the `soundscapes_path` tree is only a
        # fallback for rows whose recorded path no longer resolves. The old
        # design discarded the stored path and re-resolved every row by
        # basename against a full recursive scan -- a 244k-file walk to resolve
        # 4.5k references that already resolved, plus a hard crash when the
        # tree held any duplicate basename (`rows_update` needs unique keys).
        tryCatch(
          {
            # A supplied-but-invalid relocation root is dropped here; the setup
            # gate for the field itself lives with the other path checks.
            ss_root <- if (length(input$soundscapes_path) == 1L &&
                           !is.na(input$soundscapes_path) &&
                           dir.exists(input$soundscapes_path)) {
              input$soundscapes_path
            } else {
              NULL
            }

            # LVA-142 + item 8 / plan 2026-08-18_02 sect. 5 passo 6: mode-aware
            # DuckDB-first input. In ROI mode the rows are projected with
            # detection_id := signal_id (opaque key, A2) and detection_* bounds
            # mapped from roi_*. The live soundscape/template path resolution
            # and the validation-column bootstrap below are format-agnostic.
            if (identical(mode, "rois")) {
              res <- .lva_read_rois_input(input$input_path)
            } else {
              res <- .lva_read_detections_input(input$input_path)
            }

            # Soundscape path: stored-first (DEC-6). DEC-9 anchors a relative
            # stored path under project_path. The basename fallback scans the
            # relocation root ONCE, and only when some row still failed.
            ss_first <- .lva_resolve_one_path_column(
              res, "soundscape_path", "soundscape_file",
              base_dir = project_path
            )
            res <- ss_first$res
            ss_scan <- if (ss_first$stats[["missing"]] > 0L) {
              .lva_build_basename_index(ss_root, "wav")
            } else {
              NULL
            }
            ss_stored <- ss_first$stats[["stored"]]
            ss_fallback <- 0L
            if (!is.null(ss_scan)) {
              ss_second <- .lva_resolve_one_path_column(
                res, "soundscape_path", "soundscape_file",
                base_dir = project_path, scan = ss_scan
              )
              res <- ss_second$res
              ss_stored <- ss_stored + ss_second$stats[["stored"]]
              ss_fallback <- ss_second$stats[["fallback"]]
            }
            # Parity with the old join: the file column mirrors the resolved
            # path, so exports and ordering name the recording actually read.
            # (Guarded by nrow: an empty review frame skips this entirely.)
            if (nrow(res) > 0L) {
              ss_ok <- !is.na(res$soundscape_path)
              if (any(ss_ok)) {
                res$soundscape_file[ss_ok] <- basename(res$soundscape_path[ss_ok])
              }
            }

            # Template path: same cascade, and only when templates are on
            # (LVA-100 keeps the panel optional).
            res$template_path <- rep(as.character(NA), nrow(res))
            tp_stored <- 0L
            tp_fallback <- 0L
            if (isTRUE(templates_available())) {
              tp_first <- .lva_resolve_one_path_column(
                res, "template_path", "template_file",
                base_dir = project_path
              )
              res <- tp_first$res
              tp_stored <- tp_first$stats[["stored"]]
              if (tp_first$stats[["missing"]] > 0L) {
                tpl_root <- if (length(input$templates_path) == 1L &&
                                !is.na(input$templates_path) &&
                                dir.exists(input$templates_path)) {
                  input$templates_path
                } else {
                  NULL
                }
                tp_scan <- .lva_build_basename_index(tpl_root, "wav")
                if (!is.null(tp_scan)) {
                  tp_second <- .lva_resolve_one_path_column(
                    res, "template_path", "template_file",
                    base_dir = project_path, scan = tp_scan
                  )
                  res <- tp_second$res
                  tp_stored <- tp_stored + tp_second$stats[["stored"]]
                  tp_fallback <- tp_second$stats[["fallback"]]
                }
              }
            }

            # Setup report: how many rows resolved from the store, from the
            # fallback scan, and how many have no readable recording at all
            # (those cells show the STEP-2 placeholder).
            n_missing_ss <- sum(is.na(res$soundscape_path))
            if (ss_stored + ss_fallback + tp_stored + tp_fallback > 0L ||
                n_missing_ss > 0L) {
              msg <- paste0(
                "Recording paths resolved from the store: ", ss_stored,
                " soundscape(s), ", tp_stored, " template(s)."
              )
              if (ss_fallback + tp_fallback > 0L) {
                msg <- paste0(
                  msg, " Recovered by directory scan: ", ss_fallback,
                  " soundscape(s), ", tp_fallback, " template(s)."
                )
              }
              if (n_missing_ss > 0L) {
                msg <- paste0(
                  msg, " Without a readable recording: ", n_missing_ss,
                  " row(s) (",
                  .lva_shorten_list(res$soundscape_file[is.na(res$soundscape_path)]),
                  "). Those cells are shown without audio."
                )
              }
              shiny::showNotification(
                msg, duration = 20,
                type = if (n_missing_ss > 0L) "warning" else "message"
              )
            }


            var_names <- c(
              "detection_id",
              "validation_user",
              "validation_time",
              "validation",
              "validation_note"
            )

            # Per-column bootstrap (LVA-32): add each validation column only when
            # absent, never overwriting existing values. detection_id is kept
            # verbatim when present; when absent, a sequential index is generated.

            # detection_id -- keep existing column (coerced to character), or
            # generate a sequential character index when absent. LVA-141: the id
            # is character EVERYWHERE in-session. When the output CSV is written
            # and later re-read by data.table::fread, a column of "1","2",... is
            # inferred back as integer; pinning the in-session column to character
            # (and coercing the re-read side at end_session) keeps the
            # end_session anti_join type-compatible. Generating it as character
            # was already the absent-column behaviour; this also normalises a
            # detection_id loaded as integer from a prior session's saved CSV.
            if (!"detection_id" %in% colnames(res)) {
              res <- res %>%
                dplyr::mutate(detection_id = as.character(seq_len(nrow(.))))
            } else {
              res <- res %>%
                dplyr::mutate(detection_id = as.character(detection_id))
            }

            # validation -- keep a-priori TP/FP from validate_by_overlap; default NA
            # (unvalidated = missing, consistent with validation_user / validation_time)
            if (!"validation" %in% colnames(res)) {
              res <- res %>% dplyr::mutate(validation = NA_character_)
            }

            # validation_user / validation_time / validation_note -- add only when absent
            if (!"validation_user" %in% colnames(res)) {
              res <- res %>% dplyr::mutate(validation_user = NA_character_)
            }
            if (!"validation_time" %in% colnames(res)) {
              res <- res %>% dplyr::mutate(validation_time = NA_character_)
            }
            if (!"validation_note" %in% colnames(res)) {
              res <- res %>% dplyr::mutate(validation_note = NA_character_)
            }
            if ("validation_time" %in% colnames(res)) {
              res <- res %>%
                dplyr::mutate(
                  validation_time = as.character(validation_time),
                  validation_note = as.character(validation_note)
                )
            }

            # LVA-113: validation-order index + subset fingerprint (per-column
            # bootstrap, same pattern). `validation_order` is a monotonic integer
            # assigned when the user validates a detection (kept across sessions so
            # the LVA-110 stabilization series resumes); `validation_subset` records
            # the val_subset filter in effect at that time. Added only when absent.
            if (!"validation_order" %in% colnames(res)) {
              res <- res %>% dplyr::mutate(validation_order = NA_integer_)
            } else {
              res <- res %>%
                dplyr::mutate(validation_order = suppressWarnings(
                  as.integer(validation_order)
                ))
            }
            if (!"validation_subset" %in% colnames(res)) {
              res <- res %>% dplyr::mutate(validation_subset = NA_character_)
            } else {
              res <- res %>%
                dplyr::mutate(validation_subset = as.character(validation_subset))
            }

            # LVA-123: migrate legacy "NV" strings to NA_character_.
            # CSVs written by the original app (and the refactored app before
            # 2026-06-22) use the literal string "NV" for unvalidated detections.
            # The canonical representation is now NA (missing), consistent with
            # validation_user / validation_time.
            if ("validation" %in% colnames(res)) {
              n_nv <- sum(!is.na(res$validation) & res$validation == "NV")
              if (n_nv > 0) {
                res$validation[!is.na(res$validation) & res$validation == "NV"] <- NA_character_
                shiny::showNotification(
                  sprintf("Migrated %d legacy 'NV' labels to unvalidated (NA).", n_nv),
                  type = "message"
                )
              }
            }

            # LVA-147 / single-store (plan 2026-08-18_02 sect. 5 passo 3): the input
            # now carries `val_*` (projected by the read helper), so a reopened
            # session resumes directly from `res` - no separate output merge.

            # LVA item 8.2: in ROI mode the dropdown lists the ROI labels
            # (projected into template_name) with an "All labels" sentinel
            # ("", the default); in detections mode it lists the template
            # files. A previous selection is preserved when still valid.
            # The label follows the mode (updateSelectInput changes it).
            if (identical(mode, "rois")) {
              label_choices <- unique(res$template_name)
              label_choices <- label_choices[!is.na(label_choices)]
              cur <- shiny::isolate(input$template_name)
              if (is.null(cur) || !cur %in% label_choices) {
                cur <- .lva_all_labels()
              }
              shiny::updateSelectInput(
                session,
                "template_name",
                label = "Signal",
                choices = c("All labels" = .lva_all_labels(), label_choices),
                selected = cur
              )
            } else {
              shiny::updateSelectInput(
                session,
                "template_name",
                label = "Template file (*)",
                choices = unique(res$template_name)
              )
            }

            # Update progress bar
            shinyWidgets::updateProgressBar(
              session = session,
              id = "prog_bar_full",
              value = length(which(res$validation %in% c("TP", "FP", "UN"))),
              total = nrow(res)
            )

            # Update reactive values. A (re-)load invalidates the whole in-session
            # review state: reset the cut frame, template, selection and the
            # session-held store connection, so a mode switch (ROIs <-> detections)
            # never mixes the two frame shapes (detection frames have no
            # roi_label/roi_label_updated columns; ROI frames carry them).
            df_cut(NULL)
            df_template(NULL)
            det_i(NULL)
            det_counter(1L)
            val_store_ready(FALSE)
            val_dirty(character())
            # Gate review (2026-08-19): a (re-)load is a new review set, so the
            # "all validated" modal is eligible to fire once again.
            all_validated_modal_shown(FALSE)
            if (!is.null(val_con())) {
              tryCatch(DBI::dbDisconnect(val_con(), shutdown = TRUE),
                       error = function(e) NULL)
              val_con(NULL)
            }
            df_full$data <- res
            df_output(res)

            # LVA-113: resume the validation-order counter from the highest order
            # already recorded (so a reopened project continues the series).
            validation_order_counter(
              max(c(0L, suppressWarnings(as.integer(res$validation_order))),
                  na.rm = TRUE)
            )

            shiny::showNotification(
              "Paths updated successfully. You can now proceed with the validation.",
              duration = 15,
              type = "message"
            )
          },
          error = function(e) {
            shiny::showModal(shiny::modalDialog(
              title = "Error Processing Data",
              tags$div(
                tags$p(
                  shiny::icon("exclamation-triangle"),
                  "An error occurred while processing the data:"
                ),
                tags$pre(e$message)
              ),
              easyClose = TRUE,
              footer = shiny::modalButton("OK")
            ))
          }
        )
      }

      # UIX-09(a): per-field existence feedback on the setup path inputs,
      # mirroring the seg-app. shinyjs classes; no new dependency.
      .lva_path_exists <- function(x) {
        is.character(x) && length(x) == 1L && !is.na(x) &&
          nzchar(trimws(x)) && (file.exists(x) || dir.exists(x))
      }
      shiny::observe({
        for (id in c("preset_path", "templates_path", "soundscapes_path",
                     "input_path")) {
          val <- input[[id]]
          if (is.null(val) || !nzchar(trimws(val))) {
            shinyjs::removeClass(id, "path-ok")
            shinyjs::removeClass(id, "path-bad")
            next
          }
          if (.lva_path_exists(val)) {
            shinyjs::addClass(id, "path-ok")
            shinyjs::removeClass(id, "path-bad")
          } else {
            shinyjs::addClass(id, "path-bad")
            shinyjs::removeClass(id, "path-ok")
          }
        }
      })

      shiny::observeEvent(input$user_setup_confirm, {
        .lva_do_setup_read(input$validation_mode)
      })

      # UIX-11: step 2 stays disabled until step 1 (Confirm Setup) succeeds,
      # so the two-step flow is explicit instead of a silent req() no-op.
      shinyjs::disable("confirm_session_setup")
      shiny::observeEvent(input$user_setup_confirm, {
        shinyjs::enable("confirm_session_setup")
      }, ignoreInit = TRUE)

      if (skip_path_confirmation) {
        shinyjs::click("user_setup_confirm")
      }

      # LVA item 8 / plan sect. 8.6 (user decision 2026-08-19): the review-mode
      # radio is a PASSIVE input - the re-read happens only when the user
      # confirms the setup (user_setup_confirm, above) or at boot via
      # skip_path_confirmation. The ctrl+shift+k re-read hotkey was removed;
      # the `mode` argument only sets the initial radio value and never
      # blocks a UI change.

      # Alternative version of df_detections_full containing only the
      # samples above the specified threshold to avoid showing soundscapes
      # without detections
      df_cut <- shiny::reactiveVal(NULL)
      df_template <- shiny::reactiveVal(NULL)

      shiny::observeEvent(input$confirm_session_setup, {
        shiny::req(df_full$data)
        if (!is.null(df_output())) {
          df_full$data <- df_output()
        }
        # Guard: an empty review frame (e.g. a mode switch to a store that has
        # no rows of that class) must not crash the downstream template/score
        # filtering. Surface a modal and stop instead.
        if (nrow(df_full$data) == 0L) {
          shiny::showModal(shiny::modalDialog(
            title = "No rows to review",
            "The selected review mode found no rows of that class in the store.",
            easyClose = TRUE, footer = NULL
          ))
          return()
        }
        val_subset <- input$val_subset
        val_subset[is.na(val_subset)] <- "NV"
        order_options <- list(
          "Original file order" = function(res) res,
          "Random" = function(res, seed = input$subset_seed) {
            set.seed(input$subset_seed)
            res[sample(nrow(res)), ]
          },
          "Soundscape file name (ASC)" = function(res) {
            res[order(res$soundscape_file), ]
          },
          "Soundscape file name (DESC)" = function(res) {
            res[order(res$soundscape_file, decreasing = TRUE), ]
          },
          # LVA item 8.3: ROI-mode orderings (Label = template_name slot in
          # ROI frames; Segmentation order = created_at with detection_id as
          # tiebreaker -- the opaque signal_id key).
          "Label (ASC)" = function(res) res[order(res$template_name), ],
          "Label (DESC)" = function(res) {
            res[order(res$template_name, decreasing = TRUE), ]
          },
          "Segmentation order (ASC)" = function(res) {
            res[order(res$created_at, res$detection_id), ]
          },
          "Segmentation order (DESC)" = function(res) {
            res[order(res$created_at, res$detection_id, decreasing = TRUE), ]
          },
          "Soundscape file name (ASC) and Label (ASC)" = function(res) {
            res[order(res$soundscape_file, res$template_name), ]
          },
          "Score (ASC)" = function(res) res[order(res$peak_score), ],
          "Score (DESC)" = function(res) {
            res[order(res$peak_score, decreasing = TRUE), ]
          },
          "Soundscape file name (ASC) and Score (ASC)" = function(res) {
            res[order(res$soundscape_file, res$peak_score), ]
          },
          "Soundscape file name (ASC) and Score (DESC)" = function(res) {
            res[order(res$soundscape_file, -res$peak_score), ]
          },
          "Soundscape file name (DESC) and Score (ASC)" = function(res) {
            res[order(res$soundscape_file, res$peak_score, decreasing = c(TRUE, FALSE)), ]
          },
          "Soundscape file name (DESC) and Score (DESC)" = function(res) {
            res[order(res$soundscape_file, res$peak_score, decreasing = c(TRUE, TRUE)), ]
          },
          "Score (ASC) and Soundscape file name (ASC)" = function(res) {
            res[order(res$peak_score, res$soundscape_file), ]
          },
          "Score (ASC) and Soundscape file name (DESC)" = function(res) {
            res[order(res$peak_score, res$soundscape_file, decreasing = c(FALSE, TRUE)), ]
          },
          "Score (DESC) and Soundscape file name (ASC)" = function(res) {
            res[order(-res$peak_score, res$soundscape_file), ]
          },
          "Score (DESC) and Soundscape file name (DESC)" = function(res) {
            res[order(res$peak_score, res$soundscape_file, decreasing = c(TRUE, TRUE)), ]
          }
        )
        # UIX-12: a numericInput reports NA while the field is empty (a textInput
        # reported ""), so guard explicitly and fall back to the "no limit"
        # convention rather than letting NA reach the top-n slice.
        top_n_detecs_val <- suppressWarnings(as.numeric(input$top_n_detecs))
        if (length(top_n_detecs_val) != 1L || is.na(top_n_detecs_val) ||
            top_n_detecs_val < 0) {
          top_n_detecs_val <- 0
        }

        # Gather the metadata to validate the active template. In ROI mode the
        # template/score filters do not apply (ROIs carry no peak_score); the
        # review set is the ROI rows matching the Signal label filter (when a
        # label is selected), ordered only.
        if (identical(input$validation_mode, "rois")) {
          res <- df_full$data %>%
            dplyr::filter(
              validation %in% setdiff(val_subset, "NV") |
                (is.na(validation) & "NV" %in% val_subset)
            )
          # LVA item 8.2: the "Signal" dropdown filters by ROI label. The
          # "All labels" value is the sentinel (the default) -- no filter
          # applied. The sentinel is a REAL choice value (kept in the vector
          # of possibilities), so it stays selectable after a specific label
          # was picked (gate review 2026-08-19).
          if (length(input$template_name) == 1L &&
              !identical(input$template_name, .lva_all_labels())) {
            res <- res %>% dplyr::filter(template_name == input$template_name)
          }
        } else {
          res <- df_full$data %>%
            dplyr::filter(template_name == input$template_name) %>%
            dplyr::filter(
              peak_score >= input$score_interval[1] &
                peak_score <= input$score_interval[2]
            ) %>%
            # LVA-123: NA rows (unvalidated) are included iff "NV" is selected
            # in val_subset. %in% drops NA silently, so we handle it explicitly.
            dplyr::filter(
              validation %in% setdiff(val_subset, "NV") |
                (is.na(validation) & "NV" %in% val_subset)
            ) %>%
            {
              if (input$top_by_file == TRUE) {
                dplyr::group_by(., soundscape_file)
              } else {
                .
              }
            } %>%
            {
              if (top_n_detecs_val > 0) {
                dplyr::slice_max(., order_by = peak_score, n = top_n_detecs_val)
              } else {
                .
              }
            } %>%
            dplyr::ungroup()
        }

        res <- order_options[[input$order_by]](res)

        # if the filtering process result is not null, get some more information
        if (!is.null(res)) {
          if (nrow(res) == 0) {
            shiny::showModal(
              shiny::modalDialog(
                title = "No detections found",
                "No detections were found with the provided parameters. ",
                "Please, review the validation setup.",
                easyClose = TRUE,
                footer = NULL
              )
            )
          } else {
            shiny::updateSelectInput(
              session,
              "detec",
              choices = res$detection_id,
              selected = res$detection_id[1]
            )
            df_cut(res)
            shinyWidgets::updateProgressBar(
              session = session,
              id = "prog_bar_subset",
              value = length(which(res$validation %in% c("TP", "FP", "UN"))),
              total = nrow(res)
            )
            # LVA-146: update the active-template info ONLY when df_cut() holds a
            # non-empty frame. Previously this ran unconditionally right after the
            # if-block, so an empty validation subset (the showModal branch leaves
            # df_cut() at its NULL initial value) crashed the observer with
            # "no applicable method for 'filter' applied to NULL".
            df_template({
              if (identical(input$validation_mode, "rois")) {
                df_cut() %>% dplyr::slice_head()
              } else {
                df_cut() %>%
                  dplyr::filter(template_name == input$template_name) %>%
                  dplyr::slice_head()
              }
            })
          }
        }

        shiny::showNotification("Validation session updated")
      })

      # AV-02: hotkey handler (was an observeEvent block; see the router at
      # the end of the hotkey logic).
      .hk_confirm_setup <- function() {
        if (input$hotkeys == "alt+k") {
          shinyjs::click("confirm_session_setup")
        }
      }

      # Reactive object to store the detection index within df_cut()
      det_counter <- shiny::reactiveVal(1)
      # Upon initialization and under these conditions, det_counter is set to 1
      shiny::observeEvent(
        list(input$score_interval, input$user_setup_confirm),
        det_counter(1)
        # list(input$user_setup_confirm), det_counter(1)
      )
      # Create a reactive object to store the data of the active detection
      det_i <- shiny::reactiveVal(NULL)
      # Update the active according to the counter value. Depend on df_cut() too:
      # at launch det_counter is already 1, so without df_cut() as a trigger det_i
      # would stay NULL until the counter first changed -- which is why Q/E only
      # worked after the first click/right-click.
      shiny::observeEvent(list(det_counter(), df_cut()), {
        shiny::req(df_cut())
        # LVA item 8.2 follow-up (2026-08-19): a label/template filter can
        # shrink the frame below the current counter; clamp to a valid row so
        # the active cell, the big panel and page_start stay renderable.
        if (det_counter() > nrow(df_cut())) det_counter(1L)
        det_i(df_cut()[det_counter(), ])
        shiny::updateSliderInput(
          session,
          "zoom_freq",
          max = (min(df_cut()$detection_sample_rate) / 2000) - 1
        )
        # ROI-mode re-labelling field: show only in ROI mode; sync its value to
        # the active row's EFFECTIVE label (SIG-08: roi_label_updated when
        # non-NA, else the creation-time roi_label).
        is_roi <- identical(input$validation_mode, "rois")
        if (is_roi && "roi_label" %in% names(df_cut())) {
          shinyjs::show(id = "roi_label_field")
          cur <- df_cut()[det_counter(), ]
          eff <- .signal_effective_label(
            cur$roi_label,
            if ("roi_label_updated" %in% names(cur)) cur$roi_label_updated else NULL
          )
          lbl <- if (length(eff) == 0L || is.na(eff)) "" else as.character(eff)
          # LVA item 8.4: selectize over the species sheet (segmentation-app
          # pattern). The active label may be off-list (created or legacy), so
          # it is appended to the choices before being selected.
          shiny::updateSelectizeInput(
            session, "roi_label",
            choices = c(NA, label_choices, lbl),
            selected = lbl, server = TRUE,
            options = list(create = TRUE, persist = FALSE,
                           dropdownParent = "body")
          )
        } else {
          shinyjs::hide(id = "roi_label_field")
        }
      })

      # Add 1 to the counter to navigate forward
      shiny::observeEvent(input$next_detec, {
        shiny::req(df_cut(), det_counter())
        if (nrow(df_cut()) > det_counter() & det_counter() >= 1) {
          det_counter(det_counter() + 1)
          shiny::updateSelectInput(
            session,
            "detec",
            selected = df_cut()$detection_id[det_counter()]
          )
        }
      })

      # Subtract 1 from the counter to nvigate backwards
      shiny::observeEvent(input$prev_detec, {
        shiny::req(df_cut(), det_counter())
        if (1 < det_counter() & det_counter() <= nrow(df_cut())) {
          det_counter(det_counter() - 1)
          shiny::updateSelectInput(
            session,
            "detec",
            selected = df_cut()$detection_id[det_counter()]
          )
        } else if (det_counter() == 1) {
          det_counter(1)
          shiny::updateSelectInput(
            session,
            "detec",
            selected = df_cut()$detection_id[1]
          )
        }
      })

      shiny::observeEvent(input$detec, {
        shiny::req(df_cut(), det_counter())
        i <- which(df_cut()$detection_id == input$detec)
        # Idempotency guard: this select is also driven *by* navigation via
        # updateSelectInput, so only write det_counter when it actually changes --
        # otherwise the select <-> det_counter round-trip oscillates the active
        # marker on every validation/advance.
        shiny::req(length(i) == 1L)
        if (i != det_counter()) {
          det_counter(i)
        }
      })

      # ======================================================================
      # R4.1 -- detection grid (LVA-102/103/105). The NxN grid renders a "page"
      # of df_cut() rows as independent spectrograms; the SELECTED cell is the
      # active detection (det_counter), reusing the single-detection engine.
      # ======================================================================
      # LVA-109 / CRAN item 4: grid dimension 1x1..4x4 (the LVA-109 rework
      # capped the max at 4x4, not 5x5), driven by the launch argument
      # (session_data$grid_dim) and adjustable in-session via the UI selector
      # (input$grid_dim). Page math, hotkeys, prefetch and paint all read
      # grid_dim() -- kept as a reactiveVal for that reason.
      grid_dim <- shiny::reactiveVal(session_data$grid_dim)
      .LVA_CELLS_MAX <- 16L

      # First df_cut() row index shown in the current page (block of grid_dim^2).
      page_start <- shiny::reactive({
        shiny::req(df_cut(), det_counter())
        n <- grid_dim()^2
        ((det_counter() - 1L) %/% n) * n + 1L
      })

      # LVA-107 (extended) -- off-thread PNG render buffer.
      # `img_buf` maps a full image key (`.lva_img_key`) -> a rendered PNG path;
      # `img_inflight` guards against launching duplicate daemon renders. The PNGs
      # live in a per-session temp dir, cleaned up on exit. Background daemons
      # render the WHOLE cell (readWave + fast_spectro + raster->PNG) so a page turn
      # to a prefetched page just serves a finished image -- the dominant
      # rasterization cost is off the main thread.
      img_buf <- new.env(parent = emptyenv())
      img_inflight <- new.env(parent = emptyenv())
      mirai_state <- shiny::reactiveValues(ready = FALSE)
      # TD-06: with a relative TMPDIR, tempdir() is cwd-relative and breaks
      # after any setwd - anchor the cell cache absolutely.
      png_dir <- file.path(normalizePath(tempdir(), winslash = "/",
                                        mustWork = FALSE),
                           paste0("lva_cells_", session$token))
      dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)

      # Gather the spectral parameters that define a cell render into a plain
      # list (no reactive values), so `.lva_build_cell_plot()` is pure.
      .lva_gather_params <- function() {
        list(
          wl = input$wl, ovlp = input$ovlp, zoom_freq = input$zoom_freq,
          dyn_range_detec = input$dyn_range_detec,
          color_scale = input$color_scale, pitch_shift = input$pitch_shift,
          time_guide_interval = input$time_guide_interval,
          freq_guide_interval = input$freq_guide_interval,
          time_pads = zoom_pad(),
          # LVA-109: the rendered PNG size is a pure function of the grid dim;
          # carried in the params so .lva_img_key/.lva_render_cell_png see it.
          grid_dim = grid_dim()
        )
      }

      # LVA gate review (2026-08-19): slider/option drags fire one reactive
      # flush per change; each flush would re-sign the page and re-render all
      # cells (a ~300 ms per-cell cost). Debounce the RENDER PARAMETERS by 1 s
      # after the LAST change, so a low-end machine never queues a burst of
      # full-grid renders. Navigation, labels and verdicts stay immediate
      # (they do not flow through this reactive).
      #
      # `render_params` is the debounced, STABLE list: it changes only after
      # 1 s of quiet, so both `page_sig` and the prefetch observer key off it
      # and never re-render per tick.
      render_params <- shiny::reactive({
        .lva_gather_params()
      }) %>%
        shiny::debounce(millis = 1000)

      # A scalar fingerprint of the debounced params (for `identical`-style
      # change detection without comparing the list itself).
      render_params_key <- shiny::reactive({
        p <- render_params()
        paste(
          p$wl, p$ovlp, paste(p$zoom_freq, collapse = ","),
          paste(p$dyn_range_detec, collapse = ","), p$color_scale,
          p$pitch_shift, p$time_guide_interval, p$freq_guide_interval,
          p$time_pads, p$grid_dim, sep = "|"
        )
      })

      # Synchronous render of one cell to a PNG on the MAIN thread (the fallback
      # for a buffer miss -- first page, NV jumps, a param change before the
      # prefetch warms). Stores the path in `img_buf` and returns it.
      .lva_png_sync <- function(key, row, params) {
        out <- tempfile(tmpdir = png_dir, fileext = ".png")
        .lva_render_cell_png(
          row, params, out, grid_dim = params$grid_dim %||% 3L
        )
        assign(key, out, envir = img_buf)
        out
      }

      # Page render signature: the visible page's detection ids + every drawing
      # parameter + the per-cell ROI labels (plan sect. 8.7: a re-label must
      # re-render the cell, so the label IS part of the signature). Validation
      # verdicts stay out, so a verdict edit does not flicker the cells
      # (preserving the LVA-131/LVA-139 no-flicker behaviour).
      page_sig <- shiny::reactive({
        shiny::req(df_cut(), page_start())
        # LVA item 8.2 follow-up (2026-08-19): clamp the page start to the
        # frame size. With ps > nrow, `ps:min(...)` becomes a DESCENDING
        # sequence of out-of-range indices; the NA rows crash the cell render.
        ps <- min(page_start(), nrow(df_cut()))
        n <- grid_dim()^2
        idx <- ps:min(ps + n - 1L, nrow(df_cut()))
        labels <- if ("roi_label" %in% names(df_cut())) {
          # SIG-08: the EFFECTIVE label (the badge sect. 8.7 draws), so a re-label
          # changes the signature and invalidates the cached cell images.
          .signal_effective_label(
            df_cut()$roi_label[idx],
            if ("roi_label_updated" %in% names(df_cut())) {
              df_cut()$roi_label_updated[idx]
            } else {
              NULL
            }
          )
        } else {
          NULL
        }
        list(
          ps = ps, ids = df_cut()$detection_id[idx], labels = labels,
          # Gate review: the params come from the DEBOUNCED reactive, so the
          # signature (and the cell render) only changes after 1 s of slider
          # quiet - never per tick. `params_key` is the cheap identity check.
          params = render_params(),
          params_key = render_params_key()
        )
      })
      page_sig_stable <- shiny::reactiveVal(NULL)
      shiny::observe({
        s <- page_sig()
        if (!identical(s, shiny::isolate(page_sig_stable()))) page_sig_stable(s)
      })

      # Register all cell image slots once. renderImage is SYNCHRONOUS and reads
      # `img_buf`; on a miss it renders on the main thread (no async render-fn
      # dependency). It depends only on `page_sig_stable()`, which now includes
      # the per-cell ROI labels (plan sect. 8.7): a re-label changes the signature
      # and the image key, so the cell re-renders with the new label.
      for (.k in seq_len(.LVA_CELLS_MAX)) {
        local({
          kk <- .k
          output[[paste0("cell_", kk)]] <- shiny::renderImage(
            {
              sig <- page_sig_stable()
              shiny::req(sig, kk <= grid_dim()^2, kk <= length(sig$ids))
              det_id <- sig$ids[kk]
              p <- sig$params
              # Row identity is fixed within a signature -> isolate the df_cut read.
              row <- shiny::isolate(df_cut())[sig$ps + kk - 1L, ]
              key <- .lva_img_key(det_id, p, .signal_effective_label(
                row$roi_label, row$roi_label_updated))
              path <- if (exists(key, envir = img_buf, inherits = FALSE)) {
                get(key, envir = img_buf)
              } else {
                .lva_png_sync(key, row, p)
              }
              list(
                src = path, contentType = "image/png", class = "lva-cell-img"
              )
            },
            deleteFile = FALSE
          )
        })
      }

      # ----------------------------------------------------------------------
      # LVA-107 (extended) -- off-thread cell render + next-page prefetch (mirai)
      # ----------------------------------------------------------------------
      # Background mirai daemons render the WHOLE cell PNG (readWave +
      # fast_spectro + raster->PNG), which is the dominant per-cell cost (~300 ms;
      # the read alone is ~1%). While the user reviews the current page, the NEXT
      # page's PNGs are rendered into `img_buf`, so advancing is instant. The
      # current page's misses (first page / NV jumps / a param change) fall back to
      # a synchronous main-thread render in `.lva_png_sync()`, so the app always
      # works even with no daemons.
      #
      # Decisions (DEBT LVA-107): D10 buffer = next 1 panel; D12 backend = mirai;
      # D11 "implement as you see fit" -> user chose (2026-06-22) to extend the
      # read-only prefetch to a full PNG render. Stale-package safety: the daemon
      # runs `fast_spectro`, so it MUST be primed from the refactored sources
      # (`monitoraSom.src_dir`); when that option is absent the daemon path is
      # disabled (synchronous fallback only) rather than risk priming the OLD
      # installed package.

      # Number of render daemons (default 4 -- diminishing returns past that here).
      # Set the option to 0 to disable the daemon path entirely.
      .lva_render_cores <- function() {
        as.integer(getOption("monitoraSom.lva_prefetch_cores", default = 4L))
      }

      # Where the daemons load the refactored sources from. Prefer an explicit
      # LVA option, else the project-wide `monitoraSom.src_dir` (set by the dev
      # harness / tests). NULL => no trusted sources => daemon path stays off.
      .lva_src_dir <- function() {
        d <- getOption(
          "monitoraSom.lva_src_dir",
          default = getOption("monitoraSom.src_dir", default = NULL)
        )
        if (!is.null(d) && dir.exists(d)) d else NULL
      }

      # Set up + prime the daemons once per session (lazy, on first need). Workers
      # source the files in `.LVA_PREFETCH_SOURCES` so they resolve
      # `.lva_render_cell_png()` and `fast_spectro()`. Returns TRUE when usable.
      .lva_ensure_daemons <- function() {
        if (isTRUE(mirai_state$ready)) return(TRUE)
        cores <- .lva_render_cores()
        if (cores < 1L || !requireNamespace("mirai", quietly = TRUE)) {
          return(FALSE)
        }
        src <- .lva_src_dir()
        if (is.null(src)) return(FALSE) # no trusted sources -> synchronous only
        ok <- tryCatch(
          {
            mirai::daemons(cores)
            files <- file.path(src, .LVA_PREFETCH_SOURCES)
            mirai::everywhere(
              {
                for (f in .files) sys.source(f, envir = globalenv())
              },
              .files = files
            )
            TRUE
          },
          error = function(e) FALSE
        )
        if (!ok) {
          tryCatch(mirai::daemons(0L), error = function(e) NULL)
          return(FALSE)
        }
        mirai_state$ready <- TRUE
        TRUE
      }

      # Tear the daemons down + remove the PNG temp dir when the session ends
      # (global mirai state; this app is single-session). Guarded so a missing or
      # already-torn pool never errors on close.
      session$onSessionEnded(function() {
        if (isTRUE(shiny::isolate(mirai_state$ready))) {
          tryCatch(mirai::daemons(0L), error = function(e) NULL)
        }
        tryCatch(unlink(png_dir, recursive = TRUE), error = function(e) NULL)
        # LVA-142b: close the persistent output-store connection (signals,
        # LVA-155), if open.
        con <- shiny::isolate(val_con())
        if (!is.null(con)) {
          tryCatch(DBI::dbDisconnect(con, shutdown = TRUE), error = function(e) NULL)
        }
      })

      # Schedule a background daemon render of one cell into `img_buf[[key]]`.
      .lva_png_prefetch <- function(key, row, params) {
        if (exists(key, envir = img_buf, inherits = FALSE)) return(invisible())
        if (exists(key, envir = img_inflight, inherits = FALSE)) {
          return(invisible())
        }
        if (!.lva_ensure_daemons()) return(invisible())
        out <- tempfile(tmpdir = png_dir, fileext = ".png")
        assign(key, TRUE, envir = img_inflight)
        m <- mirai::mirai(
          .lva_render_cell_png(
            row, params, out, grid_dim = params$grid_dim %||% 3L
          ),
          row = row, params = params, out = out
        )
        local({
          kk <- key
          promises::then(
            promises::as.promise(m),
            onFulfilled = function(path) {
              if (is.character(path) && length(path) == 1L && file.exists(path)) {
                assign(kk, path, envir = img_buf)
              }
              if (exists(kk, envir = img_inflight, inherits = FALSE)) {
                rm(list = kk, envir = img_inflight)
              }
            },
            onRejected = function(e) {
              if (exists(kk, envir = img_inflight, inherits = FALSE)) {
                rm(list = kk, envir = img_inflight)
              }
            }
          )
        })
        invisible()
      }

      # On every page / parameter change: evict stale PNGs (keeping only the
      # current + next page, D10 cap), then prefetch the next page's renders.
      shiny::observeEvent(
        list(
          page_start(), df_cut(), grid_dim(),
          # Gate review: key off the DEBOUNCED param value, so a slider burst
          # triggers ONE evict+prefetch pass after 1 s of quiet, not one per tick.
          render_params_key()
        ),
        {
          shiny::req(df_cut(), page_start())
          if (!.lva_ensure_daemons()) return()

          n <- grid_dim()^2
          total <- nrow(df_cut())
          ps <- page_start()
          nxt_start <- ps + n
          p <- .lva_gather_params()

          # Keys to KEEP warm: current page + next page.
          keep_idx <- intersect(ps:(nxt_start + n - 1L), seq_len(total))
          keep_keys <- vapply(
            keep_idx,
            function(i) {
              .lva_img_key(
                df_cut()$detection_id[i], p,
                .signal_effective_label(df_cut()$roi_label[i],
                                        df_cut()$roi_label_updated[i]))
            },
            character(1)
          )

          # Eviction: drop + unlink any buffered PNG not in the keep set, and any
          # stale inflight marker. Bounds the buffer to ~2 pages.
          for (k in ls(img_buf)) {
            if (!(k %in% keep_keys)) {
              f <- get(k, envir = img_buf)
              if (is.character(f) && file.exists(f)) unlink(f)
              rm(list = k, envir = img_buf)
            }
          }
          for (k in ls(img_inflight)) {
            if (!(k %in% keep_keys)) rm(list = k, envir = img_inflight)
          }

          # Prefetch the next page's renders (D10: next 1 panel only).
          if (nxt_start <= total) {
            nxt_idx <- nxt_start:min(nxt_start + n - 1L, total)
            for (i in nxt_idx) {
              key <- .lva_img_key(
                df_cut()$detection_id[i], p,
                .signal_effective_label(df_cut()$roi_label[i],
                                        df_cut()$roi_label_updated[i]))
              .lva_png_prefetch(key, df_cut()[i, ], p)
            }
          }
        },
        ignoreInit = FALSE
      )

      # Left-click a cell -> select that detection (drives the active column).
      shiny::observeEvent(input$cell_click, {
        shiny::req(df_cut(), page_start())
        idx <- page_start() + as.integer(input$cell_click) - 1L
        if (idx >= 1L && idx <= nrow(df_cut())) {
          det_counter(idx)
          shiny::updateSelectInput(
            session, "detec",
            selected = df_cut()$detection_id[idx]
          )
        }
      })

      # Right-click a cell -> select it and cycle its label TP->FP->UN.
      shiny::observeEvent(input$cell_rclick, {
        shiny::req(df_cut(), df_output(), page_start(), input$validation_user)
        idx <- page_start() + as.integer(input$cell_rclick) - 1L
        shiny::req(idx >= 1L, idx <= nrow(df_cut()))
        det_counter(idx)
        shiny::updateSelectInput(
          session, "detec",
          selected = df_cut()$detection_id[idx]
        )
        cur <- as.character(df_cut()$validation[idx])
        nxt <- switch(cur, "TP" = "FP", "FP" = "UN", "UN" = "TP", "TP")
        apply_validation(df_cut()[idx, ], nxt)
      })

      # R4.3 (round 3): decoration<->plot sync. Compute each cell's full target
      # className (status border + selection), publish them in `window.lvaTargets`,
      # then decide WHEN to apply:
      #  * page_changed = FALSE (same page -- a label or selection change, the cell
      #    image does NOT reload) -> apply immediately, so feedback is instant.
      #  * page_changed = TRUE (the page's detections change -> every cell reloads its
      #    spectrogram) -> DO NOT touch the valid cells here; the client `load`
      #    listener flips each cell's border the moment its new image appears, so
      #    border and plot are always consistent. Only the empty trailing cells (no
      #    image to load) are cleared here.
      .lva_paint_cells <- function(page_changed = FALSE) {
        shiny::req(df_cut(), page_start())
        ps <- page_start()
        n <- grid_dim()^2
        d <- grid_dim()
        sel <- det_counter() - ps + 1L
        classes <- character(.LVA_CELLS_MAX)
        is_empty <- logical(.LVA_CELLS_MAX)
        for (k in seq_len(.LVA_CELLS_MAX)) {
          idx <- ps + k - 1L
          if (k <= n && idx <= nrow(df_cut())) {
            status <- switch(
              as.character(df_cut()$validation[idx]),
              "TP" = "lva-tp",
              "FP" = "lva-fp",
              "UN" = "lva-un",
              "lva-nv"
            )
            cls <- paste("lva-cell", status)
            if (k == sel) cls <- paste(cls, "lva-cell-selected")
            classes[k] <- cls
          } else {
            classes[k] <- "lva-cell"
            is_empty[k] <- TRUE
          }
        }
        arr <- paste0("'", classes, "'", collapse = ",")
        # cells to apply NOW: all (same page) or only the empty ones (page change).
        apply_k <- if (page_changed) which(is_empty) else seq_len(.LVA_CELLS_MAX)
        apply_js <- if (length(apply_k)) sprintf(
          paste0(
            "var a=[%s];for(var j=0;j<a.length;j++){",
            "var el=document.getElementById('cellwrap_'+a[j]);",
            "if(el&&window.lvaTargets[a[j]-1]){el.className=window.lvaTargets[a[j]-1];}}"
          ),
          paste(apply_k, collapse = ",")
        ) else ""
        # LVA-109 / CRAN item 4: dim-aware grid. The CSS template is set from JS
        # (repeat(dim,1fr) columns+rows) and cells beyond dim^2 are hidden - the
        # 16 static slots cover 4x4, the active dim defines the visible dimxdim.
        # Parity sizes (template cell == a detection cell, both axes): the left
        # column gets 1/(dim+1) of the width; the reference panel's height is
        # pinned to the FIRST grid cell's height (ResizeObserver re-syncs on
        # window/dim changes). In 1x1 the grid uses 2:2 so its single cell stays
        # the same size as the template's.
        dim_js <- sprintf(
          paste0(
            "var g=document.querySelector('.lva-grid');",
            "if(g){g.style.gridTemplateColumns='repeat(%d,1fr)';",
            "g.style.gridTemplateRows='repeat(%d,1fr)';}",
            "var lc=document.getElementById('lva_leftcol');",
            "if(lc){lc.style.flex='0 0 auto';lc.style.width=(1/(%d+1)*100)+'%%';}",
            "if(g){g.style.flex='%d 1 0';}",
            "var rc=document.getElementById('lva_refcell');",
            "if(rc&&window.lvaSyncRef===undefined){",
            "window.lvaSyncRef=function(){",
            "var c=document.getElementById('cellwrap_1');",
            "if(rc&&c){rc.style.height=c.offsetHeight+'px';}};",
            "window.lvaSyncRef();",
            "new ResizeObserver(function(){window.lvaSyncRef();}).observe(g);}",
            "for(var k=1;k<=%d;k++){var c=document.getElementById('cellwrap_'+k);",
            "if(c){c.style.display=(k<=%d)?'block':'none';}}",
            "if(window.lvaSyncRef){window.lvaSyncRef();}"
          ),
          d, d, d, if (d == 1L) 2L else d, .LVA_CELLS_MAX, n
        )
        shinyjs::runjs(paste0(
          sprintf("window.lvaTargets=[%s];", arr), apply_js, dim_js
        ))
      }
      # Repaint on any data/page/dimension change. Detect a page change (vs an
      # in-page label/selection change) so the paint can defer to the per-cell
      # image-load listener on navigation (perfect decoration<->plot sync) while
      # staying instant in-page. No artificial delay -- navigation stays fluid.
      prev_page_start <- shiny::reactiveVal(NULL)
      shiny::observeEvent(
        list(df_cut(), det_counter(), page_start(), grid_dim()),
        {
          shiny::req(df_cut(), page_start())
          pc <- is.null(prev_page_start()) || prev_page_start() != page_start()
          prev_page_start(page_start())
          .lva_paint_cells(page_changed = pc)
        }
      )

      # LVA-109 / CRAN item 4: in-session dim change (UI selector). The
      # reactiveVal drives all page math; the paint observer (grid_dim() is in
      # its event list) re-applies the CSS template and hides/shows the cells.
      shiny::observeEvent(input$grid_dim, {
        d <- suppressWarnings(as.integer(input$grid_dim))
        shiny::req(d %in% 1:4)
        if (d != grid_dim()) grid_dim(d)
      }, ignoreInit = TRUE)

      # LVA-109 / CRAN item 4 + gate review (2026-08-19): "Show template"
      # unchecked OMITS the reference box entirely (the whole row wrapper,
      # not just the plot) - the left column collapses to rows 1+3.
      shiny::observeEvent(input$show_reference, {
        if (isTRUE(input$show_reference)) {
          shinyjs::show(id = "lva_refcell_row")
        } else {
          shinyjs::hide(id = "lva_refcell_row")
        }
      }, ignoreInit = TRUE)

      # Page navigation ------------------------------------------------------
      # Move the active detection to row `i` (drives page_start + the selector).
      nav_to_row <- function(i) {
        i <- max(1L, min(nrow(df_cut()), as.integer(i)))
        det_counter(i)
        shiny::updateSelectInput(
          session, "detec", selected = df_cut()$detection_id[i]
        )
      }

      # Sequential page jump (a full page of grid_dim2 detections). Hotkeys
      # Z (previous) / C (next).
      go_next_page <- function() {
        shiny::req(df_cut(), det_counter())
        if (isTRUE(input$nav_autosave)) save_output()
        nav_to_row(page_start() + grid_dim()^2)
      }
      go_prev_page <- function() {
        shiny::req(df_cut(), det_counter())
        if (isTRUE(input$nav_autosave)) save_output()
        nav_to_row(page_start() - grid_dim()^2)
      }
      shiny::observeEvent(input$next_page, go_next_page())
      shiny::observeEvent(input$prev_page, go_prev_page())
      # AV-02: hotkey handler (was an observeEvent block; see the router at
      # the end of the hotkey logic).
      .hk_page_nav <- function() {
        if (input$hotkeys == "c") go_next_page()
        if (input$hotkeys == "z") go_prev_page()
      }

      # NV-aware page jump (seg-app style): first row of the next / previous page
      # that still holds at least one NV detection. Returns NA when none exist.
      first_row_of_nv_page <- function(direction) {
        shiny::req(df_cut(), det_counter())
        n <- grid_dim()^2
        total_pages <- ceiling(nrow(df_cut()) / n)
        cur_page <- (page_start() - 1L) %/% n + 1L
        all_pages <- seq_len(total_pages)
        pages <- if (direction > 0) {
          all_pages[all_pages > cur_page]
        } else {
          rev(all_pages[all_pages < cur_page])
        }
        for (p in pages) {
          lo <- (p - 1L) * n + 1L
          hi <- min(nrow(df_cut()), p * n)
          if (any(is.na(df_cut()$validation[lo:hi]))) {
            return(lo)
          }
        }
        NA_integer_
      }
      shiny::observeEvent(input$next_page_nv, {
        if (isTRUE(input$nav_autosave)) save_output()
        i <- first_row_of_nv_page(1)
        if (is.na(i)) {
          shiny::showNotification(
            "No later page with unvalidated detections.", type = "message"
          )
        } else {
          nav_to_row(i)
        }
      })
      shiny::observeEvent(input$prev_page_nv, {
        if (isTRUE(input$nav_autosave)) save_output()
        i <- first_row_of_nv_page(-1)
        if (is.na(i)) {
          shiny::showNotification(
            "No earlier page with unvalidated detections.", type = "message"
          )
        } else {
          nav_to_row(i)
        }
      })

      # "Page X of Y" counter shown in the grid header.
      output$page_counter <- shiny::renderText({
        shiny::req(df_cut(), page_start())
        n <- grid_dim()^2
        total_pages <- max(1L, ceiling(nrow(df_cut()) / n))
        cur_page <- (page_start() - 1L) %/% n + 1L
        paste0("Page ", cur_page, " / ", total_pages)
      })

      # Panel-wide validation (LVA-106, reworked R4.2): label only the
      # NOT-validated (NV) cells of the current page. NV cells are unvalidated by
      # definition, so this can never overwrite an existing label -- it respects
      # the Overwrite flag structurally (the old version's rows_update over every
      # cell was the Overwrite bypass). When Autonav is on it then advances one
      # sequential page.
      validate_panel <- function(label) {
        shiny::req(
          df_cut(), df_output(), input$validation_user, page_start()
        )
        ps <- page_start()
        idxs <- ps:min(nrow(df_cut()), ps + grid_dim()^2 - 1L)
        rows <- df_cut()[idxs, , drop = FALSE]
        nv <- is.na(rows$validation)
        if (any(nv)) {
          # LVA-113: the NV cells are validated now -> give each a sequential
          # validation_order (in page order) + the subset fingerprint.
          n_new <- sum(nv)
          o0 <- shiny::isolate(validation_order_counter())
          patch <- rows[nv, , drop = FALSE] %>%
            dplyr::mutate(
              validation = label,
              validation_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              validation_user = input$validation_user,
              validation_order = o0 + seq_len(n_new),
              validation_subset = .lva_subset_fingerprint()
            )
          validation_order_counter(o0 + n_new)
          df_cut(dplyr::rows_update(
            df_cut(), patch, by = "detection_id", unmatched = "ignore"
          ))
          df_output(dplyr::rows_update(
            df_output(), patch, by = "detection_id", unmatched = "ignore"
          ))
          mark_dirty(patch$detection_id)  # LVA-142b
          det_i(df_cut()[det_counter(), ])
        } else {
          shiny::showNotification(
            "No unvalidated detections on this page.", type = "message"
          )
        }
        if (isTRUE(input$auto_next)) {
          # LVA-145: event-driven page flip (hold the page until the next one is
          # prefetched) instead of go_next_page()'s immediate turn, which rendered
          # all next-page cells synchronously (panel validation leaves no dwell time
          # for the prefetch). autosave = TRUE preserves go_next_page()'s autosave.
          request_page_flip(page_start() + grid_dim()^2, autosave = TRUE)
        }
      }
      shiny::observeEvent(input$panel_tp, validate_panel("TP"))
      shiny::observeEvent(input$panel_fp, validate_panel("FP"))
      # AV-02: hotkey handler (was an observeEvent block; see the router at
      # the end of the hotkey logic).
      .hk_panel_validate <- function() {
        if (input$hotkeys == "shift+q") validate_panel("TP")
        if (input$hotkeys == "shift+e") validate_panel("FP")
      }

      # R4.3: output$active_meta (the active-detection info panel: file - score -
      # status) was removed -- it duplicated the Soundscape file / Detection ID
      # widgets and the per-cell coloured borders/badges in the grid.

      custom_references <- shiny::reactiveVal(NULL)

      # Reactive object containing the wav of the active template
      shiny::observeEvent(input$lock_template, {
        # LVA-100: the custom-reference feature needs templates on disk. With no
        # templates available, keep it disabled and skip fetch_template_metadata.
        if (!isTRUE(templates_available()) || is.null(input$templates_path)) {
          shinyjs::disable("custom_reference")
          custom_references(NULL)
          return()
        }
        shiny::req(input$templates_path)
        if (input$lock_template != TRUE) {
          shinyjs::enable("custom_reference")
          custom_refs <- fetch_template_metadata(
            templates_path = input$templates_path,
            recursive = TRUE,
            source = "legacy_filename"
          )
          # Add validation to ensure custom_refs is not empty
          if (nrow(custom_refs) > 0) {
            custom_references(custom_refs)
            shiny::updateSelectInput(
              session,
              "custom_reference",
              choices = custom_refs$template_path,
              selected = input$custom_reference
            )
          } else {
            shiny::showNotification(
              "No template files found in the specified path",
              type = "warning"
            )
          }
        } else {
          shiny::updateSelectInput(
            session,
            "custom_reference",
            choices = NULL
          )
          shinyjs::disable("custom_reference")
          custom_references(NULL)
        }
      })

      rec_template <- shiny::reactiveVal(NULL)
      shiny::observe({
        shiny::req(df_template())

        # LVA item 8 / plan 2026-08-18_02 sect. 3.4: isTRUE() instead of '== TRUE'
        # so a NULL lock_template (unset input, e.g. headless testServer) does
        # not emit "if: argument is of length zero" (falls back to df_template).
        template_data <- if (isTRUE(input$lock_template)) {
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
        if (
          length(wav_path) == 0L || is.na(wav_path) || !file.exists(wav_path)
        ) {
          rec_template(NULL)
          shiny::showNotification("Template file not found [MSG-016]", type = "warning")
        } else {
          template_duration <- template_data$template_end -
            template_data$template_start
          rec_start <- max(0, template_data$template_start - zoom_pad())
          pre_silence <- max(0, -(template_data$template_start - zoom_pad()))
          rec_end <- min(
            template_duration,
            template_data$template_end + zoom_pad()
          )
          pos_silence <- max(
            0,
            (template_data$template_end + zoom_pad()) - template_duration
          )

          # LVA-121: a user-picked custom reference may have no parseable
          # start/end metadata (NA), so the windowed read + addsilw crashes with
          # "readBin: invalid 'n'". When the window is invalid, read the whole
          # file with no silence padding instead of crashing the app.
          valid_window <- all(is.finite(c(
            rec_start, rec_end, pre_silence, pos_silence
          ))) && rec_end > rec_start

          if (length(wav_path) == 1) {
            res <- if (valid_window) {
              tuneR::readWave(
                wav_path,
                from = rec_start,
                to = rec_end,
                units = "seconds"
              ) %>%
                seewave::addsilw(
                  .,
                  at = "start",
                  d = pre_silence,
                  output = "Wave"
                ) %>%
                seewave::addsilw(., at = "end", d = pos_silence, output = "Wave")
            } else {
              tuneR::readWave(wav_path)
            }

            rec_template(res)

            # Rendering the template HTML player
            if (
              isTRUE(input$enable_player) &&
                file.exists(wav_path) &&
                TRUE # Sound Player removed (LVA-122): HTML player is the only mode
            ) {
              temp_file <- gsub(
                "\\\\",
                "/",
                tempfile(
                  pattern = "template_",
                  tmpdir = session_data$temp_path,
                  fileext = ".wav"
                )
              )
              # LVA-156: guard the pad cut like LVA-121 guards the windowed
              # read. When the whole-file fallback ran (custom reference whose
              # legacy-filename times lie outside the short cut WAV) the clip
              # may be shorter than 2 * pad -- cutw(from >= to) would stop the
              # observer. Play the full clip instead (same policy as LVA-121).
              if (zoom_pad() != 0 &&
                  zoom_pad() < seewave::duration(res) - zoom_pad()) {
                res <- seewave::cutw(
                  res,
                  from = zoom_pad(),
                  to = seewave::duration(res) - zoom_pad(),
                  output = "Wave"
                )
              }
              if (input$pitch_shift < 1) {
                res@samp.rate <- res@samp.rate / abs(input$pitch_shift)
              }
              if (isTRUE(input$visible_bp)) {
                res <- seewave::fir(
                  res,
                  f = res@samp.rate,
                  from = (input$zoom_freq[1] / abs(input$pitch_shift)) * 1000,
                  to = (input$zoom_freq[2] / abs(input$pitch_shift)) * 1000,
                  wl = input$wl,
                  output = "Wave"
                )
              }
              seewave::savewav(res, f = res@samp.rate, filename = temp_file)
              shiny::removeUI(selector = "#template_player_selector")
              shiny::insertUI(
                selector = "#template_player",
                where = "afterEnd",
                ui = tags$audio(
                  id = "template_player_selector",
                  src = paste0("audio/", basename(temp_file)),
                  type = "audio/wav",
                  autostart = FALSE,
                  controls = TRUE
                )
              )
              template_list <- list.files(
                session_data$temp_path,
                pattern = "template_.*.wav",
                full.names = TRUE,
                recursive = TRUE
              )
              file.remove(template_list[template_list != temp_file])
            } else {
              shiny::removeUI(selector = "#template_player_selector")
            }
          }
        }
      })

      spectro_template <- shiny::reactive({
        shiny::req(df_template())
        if (is.null(rec_template())) {
          ggplot2::ggplot() +
            ggplot2::annotate(
              "label",
              x = 1,
              y = 1,
              label = "Template not available"
            ) +
            ggplot2::theme_void()
        } else {
          box_color <- ifelse(
            input$color_scale %in%
              c("greyscale 1", "greyscale 2"),
            "black",
            "white"
          )
          temp_rec <- rec_template()
          fast_spectro(
            rec = temp_rec,
            wl = input$wl,
            ovlp = input$ovlp,
            flim = c(input$zoom_freq[1], input$zoom_freq[2]),
            dyn_range = c(input$dyn_range_templ[1], input$dyn_range_templ[2]),
            time_guide_interval = input$time_guide_interval,
            freq_guide_interval = input$freq_guide_interval,
            color_scale = input$color_scale,
            pitch_shift = input$pitch_shift
          ) +
            ggplot2::labs(title = "Template spectrogram") +
            ggplot2::annotate(
              "label",
              # LVA item 8.2 follow-up (2026-08-19): "Signal" is the term that
              # fits both modes -- the ref cell shows the template file in
              # detections mode and the ROI label in ROI mode, matching the
              # "Signal" dropdown label approved in item 8.2(a).
              label = paste0("Signal: '", df_template()$template_name, "'"),
              x = -Inf,
              y = Inf,
              hjust = 0,
              vjust = 1,
              color = "white",
              fill = "black"
            ) +
            ggplot2::annotate(
              "rect",
              xmin = ifelse(zoom_pad() == 0, 0, zoom_pad()),
              xmax = ifelse(
                zoom_pad() == 0,
                seewave::duration(rec_template()),
                seewave::duration(rec_template()) - zoom_pad()
              ),
              ymin = df_template()$template_min_freq,
              ymax = df_template()$template_max_freq,
              linetype = "dashed",
              alpha = 0,
              color = box_color,
              fill = box_color
            ) +
            ggplot2::theme(legend.position = "none")
        }
      })

      # R4.3: render the reference template AS A GRID CELL (output$ref_cell) in the
      # grid's left column. To match a detection cell exactly, it is rendered with
      # the SAME treatment as .lva_cell_plot() -- fast_spectro on rec_template()
      # directly, stripped axes, short title -- WITHOUT spectro_template()'s baked-in
      # rect box / template-name label. The box (CSS) and the render are therefore
      # uniform with the detection cells. The export path still uses
      # spectro_template() directly, so it keeps its full axes + annotations.
      # res is FIXED at 96 (the cells' dpi) - renderPlot's `res` cannot be a
      # reactive expression (only width/height/alt are wrapped), and the on-screen
      # pixel size already comes from the container, so a per-dim res buys nothing.
      output$ref_cell <- renderPlot(res = 96, {
        shiny::req(df_template())
        if (is.null(rec_template())) {
          return(
            ggplot2::ggplot() +
              ggplot2::annotate("label", x = 1, y = 1, label = "No reference") +
              ggplot2::theme_void()
          )
        }
        fast_spectro(
          rec = rec_template(),
          wl = input$wl,
          ovlp = input$ovlp,
          flim = c(input$zoom_freq[1], input$zoom_freq[2]),
          dyn_range = c(input$dyn_range_templ[1], input$dyn_range_templ[2]),
          time_guide_interval = input$time_guide_interval,
          freq_guide_interval = input$freq_guide_interval,
          color_scale = input$color_scale,
          pitch_shift = input$pitch_shift,
          norm = FALSE
        ) +
          # In-plot top-left label (matches the detection cells, which dropped
          # their titles) so the reference box has no title bar either.
          ggplot2::annotate(
            "label",
            x = -Inf, y = Inf, hjust = -0.05, vjust = 1.05,
            label = "reference",
            size = 4.5, color = "white", fill = "black", alpha = 0.55
          ) +
          ggplot2::theme(
            legend.position = "none",
            plot.title = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank()
          )
      })

      # Set of updates of spectrogram parameters that are obtained from
      # detection metadata
      shiny::observeEvent(input$get_templ_pars, {
        shiny::req(df_template(), df_cut())
        shiny::updateSliderInput(
          session,
          inputId = "ovlp",
          label = "Overlap (%):",
          value = df_template()$detection_ovlp,
          step = 10
        )
        shinyWidgets::updateSliderTextInput(
          session,
          inputId = "wl",
          label = "Window length:",
          # UIX-18: `selected <- ...` was an assignment, not a named argument, so
          # the value landed in `...` positionally and rewrote the slider's
          # CHOICES instead of selecting one - collapsing the wl slider to a
          # single value. Re-applied 2026-08-05 (lost in the `da07438` promotion,
          # originally fixed in `8691527`).
          selected = df_template()$detection_wl
        )
        min_freq <- (df_template()$template_min_freq - 1) %>%
          round(0) %>%
          ifelse(. < 0, 0, .)
        # UIX-18: the clamp was hardcoded to 23 kHz, which silently truncated the
        # band for any recording sampled above ~46 kHz. Use the data's own
        # Nyquist instead.
        nyquist_khz <- min(df_cut()$detection_sample_rate) / 2000
        max_freq <- (df_template()$template_max_freq + 1) %>%
          round(0) %>%
          ifelse(. > nyquist_khz, nyquist_khz, .)
        shiny::updateSliderInput(
          session,
          "zoom_freq",
          max = (min(df_cut()$detection_sample_rate) / 2000) - 1,
          value = c(min_freq, max_freq)
        )
      })

      shiny::observeEvent(input$default_pars, {
        shiny::req(det_i())
        shiny::updateSliderInput(
          session,
          inputId = "dyn_range_templ",
          value = session_data$dyn_range_templ
        )
        shiny::updateSliderInput(
          session,
          inputId = "dyn_range_detec",
          value = session_data$dyn_range_detec
        )
        shinyWidgets::updateSliderTextInput(
          session,
          inputId = "wl",
          selected = session_data$wl
        )
        shiny::updateSliderInput(
          session,
          inputId = "ovlp",
          value = session_data$ovlp
        )
        shiny::updateSelectInput(
          session,
          inputId = "color_scale",
          selected = session_data$color_scale
        )
        shiny::updateSliderInput(
          session,
          inputId = "time_pads",
          value = session_data$time_pads
        )
        shinyWidgets::updateSliderTextInput(
          session,
          inputId = "pitch_shift",
          selected = session_data$pitch_shift
        )
        shiny::updateCheckboxInput(
          session,
          inputId = "visible_bp",
          value = session_data$visible_bp
        )
        shiny::updateCheckboxInput(
          session,
          inputId = "play_norm",
          value = session_data$play_norm
        )
        shiny::updateSliderInput(
          session,
          "zoom_freq",
          max = (min(df_cut()$detection_sample_rate) / 2000) - 1,
          value = c(0, (min(df_cut()$detection_sample_rate) / 2000) - 1)
        )
        shiny::updateSliderInput(
          session,
          inputId = "time_guide_interval",
          value = session_data$time_guide_interval
        )
        shiny::updateSliderInput(
          session,
          inputId = "freq_guide_interval",
          value = session_data$freq_guide_interval
        )
      })

      # R4.3: zoom_pad is now a plain reactive over input$time_pads (was a
      # reactiveVal fed by an observer). The old indirection let a renderCachedPlot
      # cell recompute (its cacheKey already keys on input$time_pads) BEFORE the
      # observer had pushed the new value into the reactiveVal, so detection cells
      # read a stale pad and Pad size appeared to resize only the template. As a
      # direct reactive, every consumer always sees the current pad.
      zoom_pad <- shiny::reactive(input$time_pads)

      # Reactive object to store info about the detections in the active sounscape
      df_detections <- shiny::reactive({
        shiny::req(df_cut())
        if (identical(input$validation_mode, "rois")) {
          return(df_cut())   # ROI mode: no template axis
        }
        df_cut() %>%
          dplyr::filter(
            template_name == input$template_name
          )
      })

      rec_detection <- shiny::reactiveVal(NULL)
      det_sel <- shiny::reactiveVal(NULL)
      # reactive object to store the recording of the active detection
      shiny::observe({
        shiny::req(det_i())
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

        # STEP-2 (plan 2026-09-14_01): the old guard `length(wav_path) == 1`
        # is TRUE for NA, so an unresolved row crashed on readWave. Use the
        # shared predicate and tell the user which recording is missing.
        if (!.lva_path_readable(wav_path)) {
          rec_detection(NULL)
          shiny::showNotification(
            paste0(
              "Soundscape file not found: ",
              if (length(wav_path) == 1L && !is.na(wav_path)) {
                basename(wav_path)
              } else {
                as.character(det_i()$soundscape_file)
              }
            ),
            type = "warning"
          )
          return()
        }

        res <- tuneR::readWave(
          filename = wav_path,
          from = pad_start,
          to = pad_end,
          units = "seconds"
        )
        rec_detection(res)


        if (is.na(det_i()$detection_sample_rate)) {
          shiny::updateSliderInput(
            session,
            "zoom_freq",
            max = (res@samp.rate / 2000) - 1
          )
        }

        # Rendering the detection HTML player
        if (
          isTRUE(input$enable_player) &&
            file.exists(det_i()$soundscape_path) &&
            TRUE # Sound Player removed (LVA-122): HTML player is the only mode
        ) {
          # file.remove("temp/detection_clip.wav")
          temp_file <- gsub(
            "\\\\",
            "/",
            tempfile(
              pattern = "detection_",
              tmpdir = session_data$temp_path,
              fileext = ".wav"
            )
          )
          if (input$pitch_shift < 1) {
            res@samp.rate <- res@samp.rate / abs(input$pitch_shift)
          }
          if (isTRUE(input$visible_bp)) {
            res <- seewave::fir(
              res,
              f = res@samp.rate,
              from = (input$zoom_freq[1] / abs(input$pitch_shift)) * 1000,
              to = (input$zoom_freq[2] / abs(input$pitch_shift)) * 1000,
              wl = input$wl,
              output = "Wave"
            )
          }
          if (input$play_norm == TRUE) {
            res <- tuneR::normalize(
              object = res,
              unit = as.character(res@bit),
              pcm = TRUE
            )
          }
          seewave::savewav(res, f = res@samp.rate, filename = temp_file)
          removeUI(selector = "#detection_player_selector")
          insertUI(
            selector = "#detection_player",
            where = "afterEnd",
            ui = tags$audio(
              id = "detection_player_selector",
              src = paste0("audio/", basename(temp_file)),
              type = "audio/wav",
              autostart = FALSE,
              controls = TRUE
            )
          )
          list.files(
            session_data$temp_path,
            pattern = "detection_.*.wav",
            full.names = TRUE
          ) %>%
            .[. != temp_file] %>%
            file.remove()
        } else {
          removeUI(selector = "#detection_player_selector")
        }
      })

      spectro_detection <- shiny::reactive({
        shiny::req(rec_detection(), det_i(), det_sel())
        box_color <- ifelse(
          input$color_scale %in% c("greyscale 1", "greyscale 2"),
          "black",
          "white"
        )
        # efficient_spectro(
        # Gate review (2026-08-19): a label correction renders on the
        # spectrogram, next to the verdict, so the user sees the fix.
        corr <- if ("roi_label_updated" %in% names(det_i()) &&
                     length(det_i()$roi_label_updated) == 1L &&
                     !is.na(det_i()$roi_label_updated) &&
                     nzchar(det_i()$roi_label_updated)) {
          as.character(det_i()$roi_label_updated)
        } else {
          NA_character_
        }
        corr_annotate <- if (!is.na(corr) &&
                             !identical(corr, as.character(det_i()$roi_label))) {
          ggplot2::annotate(
            "label",
            x = Inf, y = input$zoom_freq[2], vjust = 2.2, hjust = 1,
            label = paste0("Label: '", det_i()$roi_label,
                           "' \u2192 '", corr, "'"),
            fill = "#ffba52", fontface = "bold"
          )
        } else {
          NULL
        }
        plot <- fast_spectro(
          rec = rec_detection(),
          wl = input$wl,
          ovlp = input$ovlp,
          flim = c(input$zoom_freq[1], input$zoom_freq[2]),
          dyn_range = c(input$dyn_range_detec[1], input$dyn_range_detec[2]),
          time_guide_interval = input$time_guide_interval,
          freq_guide_interval = input$freq_guide_interval,
          color_scale = input$color_scale,
          pitch_shift = input$pitch_shift,
          norm = FALSE
        ) +
          ggplot2::labs(title = "Detection spectrogram") +
          ggplot2::annotate(
            "label",
            label = paste0(
              det_i()$soundscape_file,
              "\n",
              "Detection ID: '",
              det_i()$detection_id,
              "' in '",
              basename(input$input_path),
              "'"
            ),
            x = -Inf,
            y = Inf,
            hjust = 0,
            vjust = 1,
            color = "white",
            fill = "black"
          ) +
          ggplot2::annotate(
            "rect",
            xmin = det_sel()[1],
            xmax = det_sel()[2],
            ymin = det_i()$template_min_freq,
            ymax = det_i()$template_max_freq,
            linetype = "dashed",
            color = box_color,
            alpha = 0
          ) +
          ggplot2::annotate(
            "label",
            x = Inf,
            y = input$zoom_freq[2],
            vjust = 1,
            hjust = 1,
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
          )

        if (!is.null(corr_annotate)) {
          plot <- plot + corr_annotate
        }

        plot +
          ggplot2::annotate(
            "label",
            x = Inf,
            y = -Inf,
            vjust = 0,
            hjust = 1,
            fontface = "bold",
            label = paste0(
              "Score: ",
              round(det_i()$peak_score, 3)
            )
          ) +
          ggplot2::theme(legend.position = "none")
      })

      # render the detections spectrogram in the interface
      output$DetectionSpectrogram <- renderPlot({
        shiny::req(rec_detection(), det_i(), spectro_detection())
        spectro_detection()
      })


      # in case no wav player is defined, it wil'l use "play", which requires
      # SoX to be instaled in the OS
      # LVA-02: only HTML player is supported
      # Template player (not HTML)

      # Audio control state management
      audio_state <- shiny::reactiveValues(
        template_player = NULL,
        detection_player = NULL
      )

      # Handle hotkey audio control
      # Handle hotkey audio control (HTML player only, R session/External player removed)
      # AV-02: hotkey handler (was an observeEvent block; see the router at
      # the end of the hotkey logic).
      .hk_player_toggle <- function() {
        shiny::req(input$hotkeys %in% c("1", "2"))

        # Determine which player to control (1 = detection, 2 = template)
        player_id <- if (input$hotkeys == "1") {
          "#detection_player_selector"
        } else {
          "#template_player_selector"
        }

        # Control player via JavaScript
        shinyjs::runjs(sprintf(
          "
          var player = document.querySelector('%s');
          if (player) {
            if (player.paused) {
              player.play();
            } else {
              player.pause();
              player.currentTime = 0;
            }
          }
        ",
          player_id
        ))
      }


      validation_input <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$button_tp, validation_input("TP"))
      shiny::observeEvent(input$button_un, validation_input("UN"))
      shiny::observeEvent(input$button_fp, validation_input("FP"))

      # LVA-143 / LVA-144: auto-advance the selection one cell after a single-cell
      # validation (Autonav). WITHIN a page it advances immediately (fluid). On the
      # LAST cell of a page the page flip is DEFERRED so the just-assigned
      # label/border paints on that cell first (the user's requirement).
      #
      # LVA-144: the deferral is EVENT-DRIVEN, not a fixed wait. Validating the last
      # cell parks a `pending_advance` request and keeps the current page visible; a
      # light poll advances as soon as the NEXT page's PNGs are ready in img_buf (the
      # prefetch the daemons started on page arrival) -- so the unavoidable next-page
      # render time IS the confirmation window instead of being ADDED to a fixed wait
      # (the previous fixed `delay` summed the two). A short floor guarantees the
      # confirmation paints even when the next page is already prefetched; a safety
      # cap advances anyway should a daemon never deliver. With no daemons (purely
      # synchronous render) there is nothing to overlap, so a plain short delay is
      # used and the next page renders on the flip.
      .LVA_CONFIRM_FLOOR_MS <- 20L    # min time the confirmation stays on screen
      .LVA_ADVANCE_POLL_MS  <- 25L    # next-page readiness poll cadence
      .LVA_ADVANCE_CAP_MS   <- 4000L  # safety: advance even if never "ready"

      # Are all cells of the page CONTAINING df_cut row `row` rendered into img_buf?
      .lva_page_ready <- function(row) {
        df <- df_cut()
        if (is.null(df) || row < 1L || row > nrow(df)) return(FALSE)
        n <- grid_dim()^2
        ps <- ((row - 1L) %/% n) * n + 1L
        idx <- ps:min(ps + n - 1L, nrow(df))
        p <- .lva_gather_params()
        keys <- vapply(
          idx, function(i) {
            .lva_img_key(
              df$detection_id[i], p,
              .signal_effective_label(
                df$roi_label[i],
                if ("roi_label_updated" %in% names(df)) {
                  df$roi_label_updated[i]
                } else {
                  NA_character_
                }
              ))
          }, character(1)
        )
        all(vapply(
          keys, function(k) exists(k, envir = img_buf, inherits = FALSE), logical(1)
        ))
      }

      # Parked last-cell page-advance request: list(row, t0) while the flip waits for
      # the next page to render, or NULL. The poll observer below only runs while a
      # request is parked (returns early -- no invalidateLater -- when NULL).
      pending_advance <- shiny::reactiveVal(NULL)
      shiny::observe({
        pa <- pending_advance()
        if (is.null(pa)) return()
        shiny::invalidateLater(.LVA_ADVANCE_POLL_MS)
        df <- shiny::isolate(df_cut())
        if (is.null(df) || pa$row > nrow(df)) {       # context changed -> abandon
          pending_advance(NULL)
          return()
        }
        elapsed_ms <- (as.numeric(Sys.time()) - pa$t0) * 1000
        ready <- shiny::isolate(.lva_page_ready(pa$row))
        if ((ready && elapsed_ms >= .LVA_CONFIRM_FLOOR_MS) ||
            elapsed_ms >= .LVA_ADVANCE_CAP_MS) {
          pending_advance(NULL)
          det_counter(pa$row)
          shiny::updateSelectInput(
            session, "detec", selected = df$detection_id[pa$row]
          )
        }
      })

      # LVA-145: shared event-driven page flip, used by both the last-cell single
      # advance (LVA-144) and panel validation. With daemons it parks the flip and
      # advances when the TARGET page's PNGs are buffered (the render overlaps the
      # held current page instead of running synchronously on the flip -- which is
      # what made panel validation slow: no dwell time for the prefetch, so all
      # cells fell back to `.lva_png_sync` on the main thread). Without daemons it
      # flips after a brief floor (nothing to overlap). `autosave = TRUE` mirrors the
      # page-change autosave that go_next_page() performs (the panel path).
      request_page_flip <- function(target_row, autosave = FALSE) {
        df <- df_cut()
        if (is.null(df)) return(invisible())
        target <- max(1L, min(nrow(df), as.integer(target_row)))
        if (autosave && isTRUE(input$nav_autosave)) save_output()
        if (.lva_ensure_daemons()) {
          pending_advance(list(row = target, t0 = as.numeric(Sys.time())))
        } else {
          shinyjs::delay(.LVA_CONFIRM_FLOOR_MS, {
            det_counter(target)
            shiny::updateSelectInput(
              session, "detec", selected = df_cut()$detection_id[target]
            )
          })
        }
      }

      advance_after_validation <- function() {
        shiny::req(df_cut(), det_counter())
        if (det_counter() >= nrow(df_cut())) return(invisible()) # last detection
        n <- grid_dim()^2
        pos_in_page <- ((det_counter() - 1L) %% n) + 1L
        target <- det_counter() + 1L
        if (pos_in_page < n) {
          # within the page -> advance immediately (fluid)
          det_counter(target)
          shiny::updateSelectInput(
            session, "detec", selected = df_cut()$detection_id[target]
          )
        } else {
          # last cell of the page -> event-driven flip (single-cell path: no autosave)
          request_page_flip(target, autosave = FALSE)
        }
      }

      # Auto navigation as reaction to validation buttons
      shiny::observeEvent(
        list(input$button_tp, input$button_un, input$button_fp),
        {
          shiny::req(df_cut(), det_counter(), input$auto_next == TRUE)
          advance_after_validation()
        }
      )

      # R4.1 keymap (D8): label the SELECTED cell -- q=TP, e=FP, f=UN, and
      # space applies the current Mark-switch value (TP/FP).
      # AV-02: hotkey handler (was an observeEvent block; see the router at
      # the end of the hotkey logic).
      .hk_validation <- function() {
        if (input$hotkeys == "q") validation_input("TP")
        if (input$hotkeys == "e") validation_input("FP")
        if (input$hotkeys == "f") validation_input("UN")
        if (input$hotkeys == "m") {
          shiny::req(input$mark_mode)
          validation_input(input$mark_mode)
        }
      }

      # Reaction to validation when auto_next is TRUE (advance one cell). LVA-143:
      # delegates to advance_after_validation() so the last-cell-of-page case defers
      # the page flip until its label renders.
      # AV-02: hotkey handler (was an observeEvent block; see the router at
      # the end of the hotkey logic).
      .hk_advance <- function() {
        shiny::req(
          df_cut(),
          det_counter(),
          # LVA item 8.2: in ROI mode the template_name dropdown holds the
          # "Signal" value ("All labels" sentinel or a label); the
          # template requirement applies to detections mode only.
          identical(input$validation_mode, "rois") ||
            shiny::isTruthy(input$template_name),
          input$auto_next == TRUE,
          input$hotkeys %in% c("q", "e", "f", "m")
        )
        advance_after_validation()
      }

      # R4.1 keymap (D8): WASD spatial navigation of the selected cell --
      # a/d move +/-1 (left/right), w/s move -/+/+/-grid_dim (up/down). The page
      # follows the selection (see page_start()).
      # AV-02: hotkey handler (was an observeEvent block; see the router at
      # the end of the hotkey logic).
      .hk_nav_cell <- function() {
        shiny::req(df_cut(), det_counter())
        n <- nrow(df_cut())
        step <- switch(
          input$hotkeys,
          "d" = 1L,
          "a" = -1L,
          "s" = as.integer(grid_dim()),
          "w" = -as.integer(grid_dim()),
          0L
        )
        if (step == 0L) {
          return()
        }
        new_i <- max(1L, min(n, det_counter() + step))
        det_counter(new_i)
        shiny::updateSelectInput(
          session,
          "detec",
          selected = df_cut()$detection_id[new_i]
        )
      }

      # Reactive object with the data used for template diagnostics
      df_diag_input_raw <- shiny::reactiveVal(NULL)
      save_output <- function(full = FALSE) {
        shiny::req(df_output(), df_cut(), input$input_path)
        # LVA item 8 / plan 2026-08-18_02 sect. 10.5 A5 (decisao D2): single-store
        # model - validations go back to the SAME store as the input, and the
        # write is ALWAYS an idempotent upsert by signal_id (replace = FALSE).
        # No flow deletes rows; the old replace = TRUE (full) path deleted every
        # detection row of the store and is removed.
        path <- input$input_path
        wrote <- FALSE
        if (grepl("(?i)\\.duckdb$", path)) {
          # LVA-155: the store is the unified signals table. For detections,
          # signal_id == detection_id. For ROIs (mode = "rois"), the rows carry
          # a preserved signal_id and roi_label/roi_label_updated (A2/A3).
          if (is.null(val_con())) val_con(.signals_duckdb_connect(path))
          con <- val_con()
          ids <- if (isTRUE(full) || !isTRUE(val_store_ready())) {
            df_output()$detection_id
          } else {
            val_dirty()
          }
          if (length(ids) > 0L) {
            sub <- df_output()[df_output()$detection_id %in% ids, , drop = FALSE]
            .signals_duckdb_upsert(con, .lva_frame_to_signals(sub), replace = FALSE)
            val_dirty(character())
            val_store_ready(TRUE)
            wrote <- TRUE
          }
        } else {
          # LVA-145: the writer warns on every CSV write, so the page-change
          # autosave used to spam the deprecation line. Show it ONCE per session as
          # a notification, then suppress the repeated warning to keep page turns
          # quiet (the helper is left unchanged for its unit tests / standalone use).
          if (!isTRUE(csv_dep_warned())) {
            shiny::showNotification(
              paste0("Writing validations to a CSV is deprecated; pass a ",
                     "`.duckdb` output store instead."),
              duration = 15, type = "warning"
            )
            csv_dep_warned(TRUE)
          }
          suppressWarnings(.lva_write_validations_output(df_output(), path))
          wrote <- TRUE
        }
        # LVA item 8.2 follow-up (2026-08-19): notify only when the save
        # actually wrote rows. Page-turn autosave with nothing new to write
        # (no validation since the last save) stays silent.
        if (wrote) shiny::showNotification("Detections successfully exported")
        shinyWidgets::updateProgressBar(
          session = session,
          id = "prog_bar_full",
          value = length(
            which(df_output()$validation %in% c("TP", "FP", "UN"))
          ),
          total = nrow(df_output())
        )
        shinyWidgets::updateProgressBar(
          session = session,
          id = "prog_bar_subset",
          value = length(which(df_cut()$validation %in% c("TP", "FP", "UN"))),
          total = nrow(df_cut())
        )
        # Diagnostics are detections-only; in ROI mode there is no template/score
        # axis, so skip them (they would fail on NA template_name/peak_score).
        if (!identical(input$validation_mode, "rois")) {
          diag_input <- diag_input_procFUN(df_output())
          df_diag_input_raw(diag_input)
          # LVA-110: refresh the score-stabilization series on save (page-change
          # autosave / manual save) -- never live on df_output, so it adds no
          # per-validation overhead.
          stabilization_react(
            .lva_stabilization_series(df_output(), input$template_name)
          )
        }
      }
      diag_input_procFUN <- function(x) {
        res <- x %>%
          dplyr::select(template_name, peak_score, validation) %>%
          dplyr::filter(
            template_name == input$template_name &
              validation %in% c("TP", "FP")
          ) %>%
          dplyr::mutate(
            validation_bin = dplyr::case_when(
              validation == "TP" ~ 1,
              validation == "FP" ~ 0
            )
          )
        return(res)
      }

      # Align `y` to the schema of `x` before rows_update/rows_patch: a mode
      # switch changes the frame shape (ROI frames carry roi_label/
      # roi_label_updated; detection frames do not), and rows_* refuses to
      # update when `y` has columns missing from `x`. Dropping the extras makes
      # the write target the columns both frames share (the validation cols).
      .lva_align_schema <- function(x, y) {
        y[, intersect(names(y), names(x)), drop = FALSE]
      }

      apply_validation <- function(target, label) {
        # LVA-113: assign a validation_order on FIRST validation of this detection
        # (keep the original order on re-labels), plus the active subset fingerprint.
        cur_order <- suppressWarnings(as.integer(target$validation_order))
        if (length(cur_order) != 1L || is.na(cur_order)) {
          cur_order <- shiny::isolate(validation_order_counter()) + 1L
          validation_order_counter(cur_order)
        }
        res_A <- target %>%
          dplyr::mutate(
            validation = label,
            validation_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            validation_user = input$validation_user,
            validation_note = ifelse(
              is.na(input$detec_note),
              NA_character_,
              as.character(input$detec_note)
            ),
            validation_order = cur_order,
            validation_subset = .lva_subset_fingerprint()
          )
        is_active <- !is.null(det_i()) &&
          identical(target$detection_id, det_i()$detection_id)

        if (input$overwrite == TRUE) {
          if (
            res_A$validation_user == target$validation_user ||
              is.na(target$validation_user)
          ) {
            if (is_active) det_i(res_A)
            df_cut(
              dplyr::rows_update(
                df_cut(),
                .lva_align_schema(df_cut(), res_A),
                by = "detection_id",
                unmatched = "ignore"
              )
            )
            df_output(
              dplyr::rows_update(
                df_output(),
                .lva_align_schema(df_output(), res_A),
                by = "detection_id",
                unmatched = "ignore"
              )
            )
            df_output(
              dplyr::rows_update(
                df_output(),
                res_A,
                by = "detection_id",
                unmatched = "ignore"
              )
            )
            mark_dirty(res_A$detection_id)  # LVA-142b
            if (input$lock_detec_note == FALSE) {
              shiny::updateTextInput(session, "detec_note", value = NA)
            }
          } else {
            shiny::showModal(
              shiny::modalDialog(
                title = "This detection was already validated by another user!",
                "Overwritting existing validations can only be performed when ",
                "the user of the current session is the same specified in the ",
                "input dataset",
                easyClose = TRUE,
                footer = NULL
              )
            )
            return(invisible(FALSE))
          }
        } else if (input$overwrite == FALSE) {
          if (is_active) det_i(res_A)
          df_cut(
            dplyr::rows_patch(
              df_cut(),
              .lva_align_schema(df_cut(), res_A),
              by = "detection_id",
              unmatched = "ignore"
            )
          )
          df_output(
            dplyr::rows_patch(
              df_output(),
              .lva_align_schema(df_output(), res_A),
              by = "detection_id",
              unmatched = "ignore"
            )
          )
          df_output(
            dplyr::rows_patch(
              df_output(),
              res_A,
              by = "detection_id",
              unmatched = "ignore"
            )
          )
          mark_dirty(res_A$detection_id)  # LVA-142b
          if (input$lock_detec_note == FALSE) {
            shiny::updateTextInput(session, "detec_note", value = NA)
          }
        }

        invisible(TRUE)
      }

      # LVA item 8 / plan 2026-08-18_02 sect. 5 passo 9: re-labelling. Writes the
      # user's label into `roi_label_updated`; `roi_label` (creation label) is
      # NEVER overwritten (SIG-08 / plan sect. 8.8). Stamps val_user/val_time/
      # val_order. Uses the same dirty-tracking + rows_update/rows_patch
      # pattern as apply_validation, so the single-store upsert persists it by
      # signal_id (A1/D1: the id does NOT change on re-label).
      apply_relabel <- function(target, new_label) {
        shiny::req(input$validation_user, target)
        if (is.null(new_label) || !nzchar(trimws(new_label))) {
          return(invisible(FALSE))
        }
        cur_order <- suppressWarnings(as.integer(target$validation_order))
        if (length(cur_order) != 1L || is.na(cur_order)) {
          cur_order <- shiny::isolate(validation_order_counter()) + 1L
          validation_order_counter(cur_order)
        }
        # SIG-08 (plan sect. 8.8): the creation label never changes; the user's
        # re-label lands in roi_label_updated. The detection note (when set)
        # is stamped into validation_note too (gate review 2026-08-19: the
        # Apply-label action also applies Detection notes).
        note_val <- if (length(input$detec_note) == 1L &&
                        !is.na(input$detec_note)) {
          as.character(input$detec_note)
        } else {
          NA_character_
        }
        res <- target %>%
          dplyr::mutate(
            roi_label_updated = as.character(new_label),
            validation_note = note_val,
            validation_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            validation_user = input$validation_user,
            validation_order = cur_order,
            validation_subset = .lva_subset_fingerprint()
          )
        is_active <- !is.null(det_i()) &&
          identical(target$detection_id, det_i()$detection_id)
        if (is_active) det_i(res)
        if (input$overwrite == TRUE) {
          df_cut(
            dplyr::rows_update(df_cut(), .lva_align_schema(df_cut(), res),
                               by = "detection_id", unmatched = "ignore")
          )
          df_output(
            dplyr::rows_update(df_output(), .lva_align_schema(df_output(), res),
                               by = "detection_id", unmatched = "ignore")
          )
        } else {
          df_cut(
            dplyr::rows_patch(df_cut(), .lva_align_schema(df_cut(), res),
                              by = "detection_id", unmatched = "ignore")
          )
          df_output(
            dplyr::rows_patch(df_output(), .lva_align_schema(df_output(), res),
                              by = "detection_id", unmatched = "ignore")
          )
        }
        mark_dirty(res$detection_id)
        # LVA item 8.2 follow-up (2026-08-19): the lock checkbox was removed
        # (user decision) -- the field always clears after Apply; the next
        # active-row change re-fills it with that row's label. Same for the
        # note field (unlocked -> clear after Apply).
        shiny::updateSelectizeInput(session, "roi_label", selected = "")
        if (!isTRUE(input$lock_detec_note)) {
          shiny::updateTextInput(session, "detec_note", value = NA)
        }
        invisible(TRUE)
      }

      shiny::observeEvent(input$roi_label_confirm, {
        shiny::req(det_i(), df_cut(), df_output())
        apply_relabel(det_i(), input$roi_label)
      })

      # Gate review (2026-08-19): hotkey alt+l applies the label (and the
      # detection note) to the active ROI -- same path as the button.
      # AV-02: hotkey handler (was an observeEvent block; see the router at
      # the end of the hotkey logic).
      .hk_relabel <- function() {
        shiny::req(det_i(), df_cut(), df_output())
        if (input$hotkeys == "alt+l") {
          apply_relabel(det_i(), input$roi_label)
        }
      }


      # Observe the object validation_input() while controlling overwrite and
      # autosave reactions. Autosave now fires on page change (go_next_page /
      # go_prev_page / NV page jumps), not on every single-cell validation.
      # active cell.
      shiny::observeEvent(validation_input(), {
        shiny::req(input$validation_user, det_i(), df_cut(), df_output())
        apply_validation(det_i(), validation_input())
        validation_input(NULL) # reset after value is passed on forward
      })

      # Set up the reaction of the export button from the UI. LVA-142b: a manual
      # Save does a FULL write (safety net) so the store matches df_output in full,
      # regardless of incremental dirty tracking.
      shiny::observeEvent(input$button_save, {
        shiny::req(df_output(), df_cut(), input$input_path)
        save_output(full = TRUE)
      })

      # Set up the reaction of the export hotkey (Ctrl+S) -- full write, as above.
      # AV-02: hotkey handler (was an observeEvent block; see the router at
      # the end of the hotkey logic).
      .hk_save <- function() {
        shiny::req(
          df_output(),
          df_cut(),
          input$input_path,
          input$hotkeys == "ctrl+s"
        )
        save_output(full = TRUE)
      }

      # LVA gate review (2026-08-19): the "All detections ... validated"
      # modal must appear only the FIRST time the condition is detected in a
      # session. Once shown, the flag stays raised until a (re-)read resets
      # the review set (.lva_do_setup_read sets all_validated_modal_shown to
      # FALSE after a load), so a new template/filter set can trigger it once
      # again.
      all_validated_modal_shown <- shiny::reactiveVal(FALSE)

      shiny::observe({
        shiny::req(df_cut())
        if (sum(is.na(df_cut()$validation)) == 0 &&
            !isTRUE(all_validated_modal_shown())) {
          all_validated_modal_shown(TRUE)
          shiny::showModal(
            shiny::modalDialog(
              title = "All detections from this template are validated",
              "Review the session setup if there are more detections to be validated",
              easyClose = TRUE,
              footer = NULL
            )
          )
        }
      })

      # Render the interactive detection table
      output$res_table <- DT::renderDT(
        {
          shiny::req(df_cut())
          cols <- if (identical(input$validation_mode, "rois")) {
            c("soundscape_file", "detection_id", "detection_start",
              "detection_end", "template_min_freq", "template_max_freq",
              "roi_label", "validation_user", "validation_time",
              "validation", "validation_note")
          } else {
            c("template_name", "soundscape_file", "detection_id",
              "detection_start", "detection_end", "template_min_freq",
              "template_max_freq", "peak_score", "validation_user",
              "validation_time", "validation", "validation_note")
          }
          df_cut() %>%
            dplyr::select(dplyr::any_of(cols)) %>%
            DT::datatable(
              editable = FALSE,
              style = "bootstrap4",
              selection = "single",
              filter = "none", # escape = FALSE,
              # The length-control option lives here, not in renderDT:
              # renderDT warns "ignores ... arguments" on every evaluation
              # when the expr yields a datatable object AND extra arguments
              # are passed (pre-existing noise, surfaced by the ROI fix).
              options = list(lengthChange = FALSE),
              colnames = if (identical(input$validation_mode, "rois")) {
                # LVA item 8.2 follow-up (2026-08-19): ROI rows have a label
                # column, no template/score columns -- the header must match
                # (the detections header referenced peak_score and crashed
                # the DT render in ROI mode).
                c("Soundscape", "ID", "Start", "End", "Min. Freq.",
                  "Max. Freq.", "Label", "User", "Time Stamp",
                  "Validation", "Note")
              } else {
                c("Template", "Soundscape", "ID", "Start", "End",
                  "Min. Freq.", "Max. Freq.", "Score", "User",
                  "Time Stamp", "Validation", "Note")
              }
            ) %>%
            DT::formatRound(
              c("detection_start", "detection_end",
                if (identical(input$validation_mode, "rois")) NULL else "peak_score"),
              3
            ) %>%
            DT::formatRound(c("template_min_freq", "template_max_freq"), 1)
        },
        server = TRUE
      )

      # Catch row selection in the interactive detection table to update the
      # active detection
      shiny::observeEvent(input$res_table_rows_selected, {
        shiny::req(input$res_table_rows_selected)
        i <- df_cut()$detection_id[input$res_table_rows_selected]
        shiny::updateSelectInput(session, "detec", selected = i)
      })

      output$count_full_tab <- renderTable(
        {
          shiny::req(df_output(), df_cut())
          # LVA-170: explicit final grouping (.groups = "drop_last" silences the
          # dplyr 1.1 regroup message that spammed the console on every page
          # turn; with two grouping vars the default drops the LAST group only,
          # leaving the frame still grouped by template_name for the
          # pivot_wider).
          df_output() %>%
            dplyr::group_by(template_name, validation) %>%
            dplyr::summarise(n = dplyr::n(), .groups = "drop_last") %>%
            dplyr::ungroup() %>%
            tidyr::pivot_wider(names_from = "validation", values_from = "n") %>%
            dplyr::rename(Template = template_name)
        },
        width = "100%"
      )
      output$count_i_tab <- renderTable(
        {
          shiny::req(df_output(), df_cut())
          # LVA-170: same explicit final grouping as count_full_tab (see above).
          df_cut() %>%
            # filter(template_name == input$template_name) %>%
            dplyr::group_by(template_name, validation) %>%
            dplyr::summarise(n = dplyr::n(), .groups = "drop_last") %>%
            dplyr::ungroup() %>%
            tidyr::pivot_wider(names_from = "validation", values_from = "n") %>%
            dplyr::rename(Template = template_name)
        },
        width = "100%"
      )

      df_diag_input <- shiny::reactive({
        shiny::req(df_diag_input_raw())
        if ("TP" %in% input$val_subset & "FP" %in% input$val_subset) {
          x <- df_diag_input_raw()
          if (input$diag_balance == "None") {
            return(x)
          }

          # LVA-22: base-R class balancing replacing caret/ROSE.
          # RNG seeded from subset_seed for reproducibility.
          set.seed(input$subset_seed)
          tp_rows <- which(x$validation == "TP")
          fp_rows <- which(x$validation == "FP")
          n_tp <- length(tp_rows)
          n_fp <- length(fp_rows)

          if (n_tp == 0 || n_fp == 0) return(x)

          if (input$diag_balance == "Downsample larger class") {
            n_target <- min(n_tp, n_fp)
            x <- dplyr::bind_rows(
              x[tp_rows, ][sample(n_tp, n_target), ],
              x[fp_rows, ][sample(n_fp, n_target), ]
            )
          } else if (input$diag_balance == "Upsample smaller  class") {
            n_target <- max(n_tp, n_fp)
            x <- dplyr::bind_rows(
              x[tp_rows, ][sample(n_tp, n_target, replace = TRUE), ],
              x[fp_rows, ][sample(n_fp, n_target, replace = TRUE), ]
            )
          } else if (input$diag_balance == "ROSE") {
            if (n_tp < 2 || n_fp < 2) {
              shiny::updateSelectInput(
                session, "diag_balance", selected = "None"
              )
              shiny::showModal(shiny::modalDialog(
                title = "Not enough detections to perform ROSE",
                "There should be at least two TP and two FP to perform ROSE",
                easyClose = TRUE, footer = NULL
              ))
              return()
            }
            # Synthetic over/under-sampling: both classes brought to ~avg size
            n_target <- round((n_tp + n_fp) / 2)
            x <- dplyr::bind_rows(
              x[tp_rows, ][sample(n_tp, n_target, replace = TRUE), ],
              x[fp_rows, ][sample(n_fp, n_target, replace = TRUE), ]
            )
          }
          x
        }
      })

      mod_plot_react <- shiny::reactiveVal(NULL)
      # roc_plot_react <- shiny::reactiveVal(NULL)
      plot_dens_react <- shiny::reactiveVal(NULL)
      precrec_plot_react <- shiny::reactiveVal(NULL)
      stabilization_react <- shiny::reactiveVal(NULL)  # LVA-110: stabilization series
      cut_full_tab <- shiny::reactiveVal(NULL)
      cut_i_tab <- shiny::reactiveVal(NULL)

      shiny::observe({
        shiny::req(df_diag_input())
        if (nrow(df_diag_input()) > 2) {
          if (length(unique(df_diag_input()$validation)) == 2) {
            if (input$diag_method == "Manual") {
              diag_method <- "manual"
              custom_cut <- input$diag_cut
              pos_prob <- NULL
            } else if (input$diag_method == "Error = 0.05") {
              diag_method <- "auto"
              custom_cut <- NULL
              pos_prob <- 0.95
            } else if (input$diag_method == "Error = 0.1") {
              diag_method <- "auto"
              custom_cut <- NULL
              pos_prob <- 0.90
            }
            val_res <- diagnostic_validations_i(
              val_i = df_diag_input(),
              diag_method = diag_method,
              pos_prob = pos_prob,
              diag_cut = custom_cut
            )
            if (
              val_res$score_cut < 1 &
                val_res$score_cut > 0 &
                input$diag_method != "Manual"
            ) {
              shiny::updateSliderInput(
                session,
                "diag_cut",
                value = val_res$score_cut
              )
            }
            mod_plot_react(val_res$mod_plot)
            cut_full_tab(val_res$diagnostics)
            precrec_plot_react(val_res$precrec_plot)
            cut_i_tab(
              val_res$diagnostics[val_res$diagnostics$selected, ]
            )
            plot_dens_react(val_res$plot_dens)
          }
        }
      })

      shiny::observeEvent(input$diag_method, {
        if (input$diag_method != "Manual") {
          shinyjs::disable("diag_cut")
        } else if (input$diag_method == "Manual") {
          shinyjs::enable("diag_cut")
        }
      })

      shiny::observe({
        shiny::req(df_cut())
        if ("TP" %in% df_cut()$validation & "FP" %in% df_cut()$validation) {
          shinyjs::enable("diag_balance")
          shinyjs::enable("diag_method")
          shinyjs::enable("diag_cut")
          shinyjs::enable("plot_dens")
          shinyjs::enable("plot_binomial")
          shinyjs::enable("plot_prec_rec")
          shinyjs::show("plot_binomial")
          shinyjs::show("plot_prec_rec")
        } else {
          shinyjs::disable("diag_balance")
          shinyjs::disable("diag_method")
          shinyjs::disable("plot_dens")
          shinyjs::disable("diag_cut")
          shinyjs::disable("plot_binomial")
          shinyjs::disable("plot_prec_rec")
          shinyjs::hide("plot_binomial")
          shinyjs::hide("plot_prec_rec")
        }
      })

      output$cut_i_tab <- shiny::renderTable(
        {
          shiny::req(cut_i_tab())
          # DVI-06 rewire: the redesigned diagnostic_validations_i table columns
          # are template_name, peak_score, tp, fp, tn, fn, precision, recall,
          # sensitivity, specificity, F1_score, selected -- there are no longer
          # tpr/fpr/tnr/fnr. Select explicitly (order-robust) and rename.
          cut_i_tab() %>%
            dplyr::mutate(tp = as.integer(tp), fp = as.integer(fp)) %>%
            dplyr::select(
              template_name, peak_score, tp, fp,
              precision, recall, sensitivity, specificity, F1_score
            ) %>%
            setNames(
              c(
                "Template",
                "Threshold",
                "TP (n)",
                "FP (n)",
                "Precision",
                "Recall",
                "Sensitivity",
                "Specificity",
                "F1"
              )
            )
        },
        width = "75%"
      )
      output$plot_binomial <- renderPlot({
        shiny::req(mod_plot_react())
        mod_plot_react()
      })
      output$plot_prec_rec <- renderPlot({
        shiny::req(precrec_plot_react())
        precrec_plot_react()
      })
      # output$plot_roc <- renderPlot({
      #   shiny::req(plot_roc())
      #   roc_plot_react()
      # })
      output$plot_dens <- renderPlot({
        shiny::req(plot_dens_react())
        plot_dens_react()
      })

      # LVA-110: TP/FP score-stabilization plot for the CURRENT template. As the
      # user validates detections (in validation_order, LVA-113), the running mean
      # of each class's peak_score fluctuates then stabilizes. Driven by a
      # reactiveVal refreshed in save_output() (like the other diagnostics) -- NOT
      # live on df_output -- so it adds no per-validation overhead. The series is
      # computed by the pure helper .lva_stabilization_series().
      output$plot_stabilization <- renderPlot({
        s <- stabilization_react()
        shiny::validate(shiny::need(
          !is.null(s),
          "Validate detections (TP/FP) to build the score-stabilization series."
        ))
        ggplot2::ggplot(
          s,
          ggplot2::aes(
            x = n_validated, y = cum_mean_score, color = validation
          )
        ) +
          ggplot2::geom_line(linewidth = 1) +
          ggplot2::geom_point(size = 1.2, alpha = 0.6) +
          ggplot2::scale_color_manual(
            values = c(TP = "#2ca25f", FP = "#de2d26"),
            breaks = c("TP", "FP")
          ) +
          ggplot2::labs(
            x = "Validated detections (in validation order)",
            y = "Cumulative mean score",
            color = NULL,
            title = "TP/FP score stabilization"
          ) +
          ggplot2::theme_minimal(base_size = 13) +
          ggplot2::theme(legend.position = "top")
      })

      # teste_val <- shiny::reactiveVal(NULL)
      # output$checagem1 <- renderPrint({
      #   shiny::req(det_i())
      #   det_i() %>%
      #     glimpse()
      #   # glimpse()
      # })

      # output$checagem2 <- renderPrint({
      #   glimpse(readRDS("app_presets/validation_preset_Salobo_validation.rds"))
      # })

      # AV-01 pilot: the detection-export feature (wav cut + spectrogram)
      # lives in lva_export_server (shiny module, defined at the end of this
      # file, after the app function — repo helper convention). The hotkey
      # handlers come back through its return value and run through the
      # router below ("r" / "t").
      export_detec <- lva_export_server(
        LVA_EXPORT_ID,
        det_i = det_i,
        rec_detection = rec_detection,
        spectro_template = spectro_template,
        spectro_detection = spectro_detection,
        wl = shiny::reactive(input$wl),
        ovlp = shiny::reactive(input$ovlp)
      )

      # AV-02 - single hotkey router. The blocks above were 11 separate
      # observeEvent(input$hotkeys) observers; their bodies are now the named
      # .hk_* handlers, unchanged. Dispatch order inside a key preserves the
      # old observer registration order (validate, then advance). .hk_run
      # isolates handlers the way separate observers were isolated: a failing
      # shiny::req() stops only its own handler, not the remaining ones.
      .hk_run <- function(handler) {
        tryCatch(handler(), shiny.silent.error = function(e) NULL)
      }
      shiny::observeEvent(input$hotkeys, {
        switch(
          input$hotkeys,
          "alt+k" = .hk_run(.hk_confirm_setup),
          "c" = , "z" = .hk_run(.hk_page_nav),
          "shift+q" = , "shift+e" = .hk_run(.hk_panel_validate),
          "1" = , "2" = .hk_run(.hk_player_toggle),
          "q" = , "e" = , "f" = , "m" = {
            .hk_run(.hk_validation)
            .hk_run(.hk_advance)
          },
          "w" = , "a" = , "s" = , "d" = .hk_run(.hk_nav_cell),
          "alt+l" = .hk_run(.hk_relabel),
          "ctrl+s" = .hk_run(.hk_save),
          "r" = .hk_run(export_detec$hk_export_wav),
          "t" = .hk_run(export_detec$hk_export_spectro)
        )
      })

      # Trigger checks and confirm or cancel the end of the session
      shiny::observeEvent(input$end_session, {
        shiny::req(df_cut(), df_output())

        dfa <- df_output() %>%
          dplyr::select(detection_id, tidyr::contains("validation")) %>%
          dplyr::mutate(detection_id = as.character(detection_id))
        nrow_unsaved <- 0

        if (file.exists(input$input_path)) {
          # LVA-142 + single-store: read back the persisted validations from the
          # SAME store as the input (DuckDB routine, or deprecated CSV). The
          # DuckDB read returns detection_id as character per schema, so the
          # anti_join below stays type-safe (cf. LVA-141). LVA-142b: when the
          # session holds an open store connection, READ THROUGH IT.
          # LVA item 8.2 follow-up (2026-08-19): in ROI mode the persisted
          # comparison reads the store's ROI rows -- the Signal dropdown
          # narrows the review set, not the persisted frame, so no template
          # filter applies (the template filter is detections-mode only).
          is_roi <- identical(input$validation_mode, "rois")
          is_duck <- grepl("(?i)\\.duckdb$", input$input_path)
          con_r <- if (!is.null(val_con()) && is_duck) val_con() else NULL
          if (is_roi && is_duck) {
            if (is.null(con_r)) {
              con_r <- .signals_duckdb_connect(input$input_path)
              on.exit(DBI::dbDisconnect(con_r, shutdown = TRUE), add = TRUE)
            }
            dfb <- as.data.frame(.lva_signals_to_rois_with_vals(
              .signals_duckdb_read(
                con_r,
                signal_class = c(.SIGNAL_CLASS_ROI, .SIGNAL_CLASS_DETECTION_TO_ROI)
              )
            ))
          } else {
            dfb <- if (!is.null(con_r)) {
              as.data.frame(.lva_signals_as_validations_all(con_r))
            } else {
              as.data.frame(.lva_read_validations_output(input$input_path))
            }
          }
          # A freshly-created output file (== the raw detections input on the
          # first run) lacks the validation_* columns this observer compares on;
          # add them when absent, mirroring the input-load path, before the
          # mutate/anti_join so end_session does not crash on an unvalidated CSV.
          if (!"validation" %in% colnames(dfb)) {
            dfb$validation <- NA_character_
          }
          if (!"validation_user" %in% colnames(dfb)) {
            dfb$validation_user <- NA_character_
          }
          if (!"validation_time" %in% colnames(dfb)) {
            dfb$validation_time <- NA_character_
          }
          dfb <- dfb %>%
            dplyr::mutate(
              validation_time = as.character(validation_time),
              # LVA-141: fread infers a "1","2",... detection_id as integer; the
              # in-session df_output keeps it character, so coerce before the join.
              detection_id = as.character(detection_id)
            )
          if (!is_roi) {
            dfb <- dfb %>% dplyr::filter(template_name == input$template_name)
          }
          dfb <- dfb %>% dplyr::select(detection_id, dplyr::contains("validation"))
          nrow_unsaved <- nrow(
            dplyr::anti_join(
              dfa,
              dfb,
              by = c("detection_id", "validation_user", "validation_time", "validation")
            )
          )
        }

        if (nrow_unsaved != 0) {
          message_detecs <- paste0(
            "There are ",
            nrow_unsaved,
            " rows in the current session thar have different validation ",
            "inputs from the output file. Consider saving before leaving ",
            "the session."
          )
        } else if (nrow_unsaved == 0) {
          message_detecs <- paste0(
            "There are no differences between the current session and the output file."
          )
        }

        shiny::showModal(
          shiny::modalDialog(
            title = "End session",
            paste0(message_detecs),
            footer = tagList(
              shiny::actionButton("cancel_exit", "Cancel"),
              shiny::actionButton("confirm_exit", "End session")
            )
          )
        )
      })

      # Stop the session after confirmation
      shiny::observeEvent(input$confirm_exit, {
        shiny::stopApp()
      })

      # Cancel closes the modal and returns to the session (LVA-154)
      shiny::observeEvent(input$cancel_exit, {
        shiny::removeModal()
      })

      # Definitions of the tooltips
      tooltips_config <- list(
        # User Setup Section
        user_setup = list(
          list(
            id = "preset_path",
            title = "Presets available here will be shown in the drop-down menu below",
            placement = "right"
          ),
          list(
            id = "validation_user",
            title = "Recommended format: 'Rosa G. L. M. (avoid commas)",
            placement = "right"
          ),
          list(
            id = "templates_path",
            title = "Parent location that contains only template files or folders of these",
            placement = "right"
          ),
          list(
            id = "soundscapes_path",
            title = paste(
              "Optional relocation root. Each detection is read at the path",
              "recorded in the store; this folder is scanned only for rows",
              "whose recorded path no longer resolves."
            ),
            placement = "right"
          ),
          list(
            id = "input_path",
            title = "Complete path to the unified signals '.duckdb' store under review",
            placement = "right"
          )
        ),

        # Session Setup Section
        session_setup = list(
          list(
            id = "confirm_session_setup",
            title = "<b>Part 2 of 2 required to start the session</b>.",
            placement = "right"
          ),
          list(
            id = "template_name",
            title = "Select here one of the templates available in the input file",
            placement = "right"
          ),
          list(
            id = "val_subset",
            title = "Select at least one options. Only those selected will be shown.",
            placement = "right"
          ),
          list(
            id = "score_interval",
            title = "Only detections within this interval will be presented to the user",
            placement = "right"
          ),
          list(
            id = "time_pads",
            title = "Zoom in and out in the time axis of template and detection spectrograms",
            placement = "right"
          ),
          list(
            id = "dyn_range_templ",
            title = paste0("Adjust what portion of the amplitude scale is ",
              "shown in the template spectrogram"),
            placement = "right"
          ),
          list(
            id = "dyn_range_detec",
            title = paste0("Adjust what portion of the amplitude scale is ",
              "shown in the detection spectrogram"),
            placement = "right"
          ),
          list(
            id = "wl",
            title = "Tradeoff between time and frequency resolution",
            placement = "right"
          ),
          list(
            id = "ovlp",
            title = paste0("Increase if more resultion is needed. ",
              "Performance may decrease for values above 80%"),
            placement = "right"
          ),
          list(
            id = "wav_player_type",
            title = "Select the method to play wav files",
            placement = "right"
          ),
          list(
            id = "wav_player_path",
            title = paste0("Necessary when 'External player' is selected. ",
              "If the executable is not available, 'HTML player' will be ",
              "automatically selected"),
            placement = "right"
          ),
          list(
            id = "get_templ_pars",
            title = "Set spectrogram parameters to those used to run the detections",
            placement = "right"
          ),
          list(
            id = "default_pars",
            title = "Set spectrogram parameters back to the default",
            placement = "right"
          )
        ),

        # Controls Section
        # R4.3: the physical play_detec / play_template buttons were removed
        # (redundant with the HTML players' own play controls; hotkeys 1/2 still
        # drive the players), so their tooltip entries are gone too.
        controls = list(
          list(
            id = "prev_detec",
            title = "Navigate to the previous detection. Hotkey: A",
            placement = "bottom"
          ),
          list(
            id = "next_detec",
            title = "Navigate to the next detection. Hotkey: D",
            placement = "bottom"
          ),
          list(
            id = "button_tp",
            title = "Validate active detection as 'True Positive'. Hotkey: Q",
            placement = "bottom"
          ),
          list(
            id = "button_un",
            title = "Validate active detection as 'Unknown'. Hotkey: W",
            placement = "bottom"
          ),
          list(
            id = "button_fp",
            title = "Validate active detection as a 'False Positive'. Hotkey: E",
            placement = "bottom"
          ),
          list(
            id = "button_save",
            title = "Export validations to the output '.csv' file. Hotkey: S",
            placement = "bottom"
          )
        ),

        # Export Section
        export = list(
          list(
            id = "detec_cuts_path",
            title = "Path for exporting an audio sample of the detection",
            placement = "bottom"
          ),
          list(
            id = "wav_cut_name",
            title = "Name of the file with the audio sample ('.wav')",
            placement = "bottom"
          ),
          list(
            id = "reset_wav_cut_name",
            title = "Reset the filename back to the default",
            placement = "bottom"
          ),
          list(
            id = "confirm_wav_export",
            title = "Hotkey: R",
            placement = "bottom"
          ),
          list(
            id = "detec_spec_path",
            title = "Path for exporting the spectrogram image",
            placement = "bottom"
          ),
          list(
            id = "spec_name",
            title = "Name of the spectrogram image file ('.jpeg')",
            placement = "bottom"
          ),
          list(
            id = "reset_spec_filename",
            title = "Reset the filename back to the default",
            placement = "bottom"
          ),
          list(
            id = "confirm_spec_export",
            title = "Hotkey: T",
            placement = "bottom"
          )
        )
      )

      # General popover options
      pop_up_opt <- list(delay = list(show = 1000, hide = 0))

      # Apply all tooltips efficiently
      # AV-01 pilot: the export widgets moved into the lva_export_ui module,
      # so their DOM ids now carry the module prefix. Translate the legacy
      # tooltip-table ids once, here.
      .export_tooltip_ns <- function(x) {
        export_ids <- c(
          "detec_cuts_path", "detec_spec_path", "wav_cut_name", "spec_name",
          "reset_wav_cut_name", "reset_spec_filename",
          "confirm_wav_export", "confirm_spec_export"
        )
        ifelse(x %in% export_ids, paste0(LVA_EXPORT_ID, "-", x), x)
      }
      lapply(unlist(tooltips_config, recursive = FALSE), function(tip) {
        shinyBS::addTooltip(
          session,
          id = .export_tooltip_ns(tip$id),
          title = tip$title,
          placement = tip$placement,
          trigger = "hover",
          options = pop_up_opt
        )
      })
    }
  )
}

# AV-01 pilot module - detection export (wav cut + spectrogram) ----------------
# The first shinyModule extracted from the LVA server (audit AV-01, pilot
# scope = exactly one module). Interface: det_i (active detection row),
# rec_detection (the loaded Wave), the two spectrogram plots, and the wl/ovlp
# spectrogram parameters as reactives. Returns the hotkey handlers so the
# AV-02 router can dispatch "r" / "t" into the module.
LVA_EXPORT_ID <- "export_detec"

# AS-02 loose end (closed 2026-09-01): single source of truth for the files
# `sys.source`d into the mirai workers by `.lva_ensure_daemons()` — the LSA
# `.prefetch_sources` pattern (audit AS-02; reviewer note §12.2 item 18:
# "decide explicitly whether the source constant covers the LVA loop too").
# Adding a worker dependency means adding it here.
.LVA_PREFETCH_SOURCES <- c("fast_spectro.R", "_lva_validation_helpers.R")

lva_export_ui <- function(id, cuts_path_default, spec_path_default) {
  ns <- shiny::NS(id)
  shiny::fluidRow(
    shiny::column(
      width = 6,
      shiny::textInput(
        ns("detec_cuts_path"),
        "Wave cuts path",
        value = cuts_path_default,
        width = "100%",
        placeholder = "Paste or load here the path to export the wav file"
      ),
      shiny::textInput(
        ns("wav_cut_name"),
        "Wav file name (*.wav)",
        value = NULL,
        width = "100%",
        placeholder = "Input the file name here"
      ),
      shiny::actionButton(
        ns("reset_wav_cut_name"),
        "Reset filename",
        width = "100%"
      ),
      shiny::actionButton(
        ns("confirm_wav_export"),
        "Export wav file (r)",
        width = "100%",
        style = "color: #fff; background-color: #33b76e; border-color: #5da42e;"
      )
    ),
    shiny::column(
      width = 6,
      shiny::textInput(
        ns("detec_spec_path"),
        "Spectrogram cuts path",
        value = spec_path_default,
        width = "100%",
        placeholder = "Paste or load here the path to export the spectrogram"
      ),
      shiny::textInput(
        ns("spec_name"),
        "Spectrogram file name (*.jpeg)",
        value = NULL,
        width = "100%",
        placeholder = "Input the file name here"
      ),
      shiny::actionButton(
        ns("reset_spec_filename"),
        "Reset filename",
        width = "100%"
      ),
      shiny::actionButton(
        ns("confirm_spec_export"),
        "Export spectrogram (t)",
        width = "100%",
        style = "color: #fff; background-color: #33b76e; border-color: #5da42e;"
      )
    )
  )
}

#' Wav-cut half of the detection-export module (private).
#'
#' @param input Module Shiny input object.
#' @param session Module Shiny session object.
#' @param det_i Reactive returning the active detection row.
#' @param rec_detection Reactive returning the loaded detection `Wave` object.
#' @param wl Reactive returning the window length.
#' @param ovlp Reactive returning the overlap percentage.
.lva_export_wav <- function(input, session, det_i, rec_detection, wl, ovlp) {
  wav_filename <- shiny::reactiveVal(character(0))
  wav_default_filename <- shiny::reactive({
    shiny::req(det_i())
    res <- paste0(
      stringr::str_replace(det_i()$soundscape_file, "\\.wav$|\\.WAV$", ""),
      "_",
      stringr::str_pad(sprintf("%.3f", round(det_i()$detection_start, 3)),
                       7, pad = "0"),
      "-",
      stringr::str_pad(sprintf("%.3f", round(det_i()$detection_end, 3)),
                       7, pad = "0"),
      "s_",
      stringr::str_pad(sprintf("%.3f", round(det_i()$template_min_freq, 3)),
                       6, pad = "0"),
      "-",
      stringr::str_pad(sprintf("%.3f", round(det_i()$template_max_freq, 3)),
                       6, pad = "0"),
      "kHz_",
      wl(),
      "wl_",
      ovlp(),
      "ovlp_",
      tail(stringr::str_split(gsub("\\.wav$", "", det_i()$template_name),
                              "_")[[1]], 1),
      ".wav"
    )
    res
  })
  shiny::observe({
    shiny::req(wav_default_filename())
    shiny::updateTextInput(session, session$ns("wav_cut_name"),
                           value = wav_default_filename())
  })
  # Reset the wav file name
  shiny::observeEvent(input$reset_wav_cut_name, {
    shiny::req(wav_filename(), det_i())
    shiny::updateTextInput(session, session$ns("wav_cut_name"),
                           value = wav_default_filename())
    wav_filename(wav_default_filename())
  })
  shiny::observeEvent(input$wav_cut_name, {
    shiny::req(input$wav_cut_name)
    wav_filename(input$wav_cut_name)
  })
  list(
    export = function() {
      shiny::req(input$detec_cuts_path, wav_filename(), rec_detection())
      tuneR::writeWave(
        object = rec_detection(),
        filename = file.path(input$detec_cuts_path, wav_filename())
      )
      shiny::showNotification("Detection wave file successfully exported")
    }
  )
}

#' Spectrogram half of the detection-export module (private).
#'
#' @param input Module Shiny input object.
#' @param session Module Shiny session object.
#' @param det_i Reactive returning the active detection row.
#' @param rec_detection Reactive returning the loaded detection `Wave` object.
#' @param spectro_template Reactive returning the template spectrogram plot.
#' @param spectro_detection Reactive returning the detection spectrogram plot.
#' @param wl Reactive returning the window length.
#' @param ovlp Reactive returning the overlap percentage.
.lva_export_spec <- function(input, session, det_i, rec_detection,
                             spectro_template, spectro_detection, wl, ovlp) {
  spec_filename <- shiny::reactiveVal(character(0))
  spec_default_filename <- shiny::reactive({
    shiny::req(det_i())
    res <- paste0(
      stringr::str_replace(det_i()$soundscape_file, "\\.wav$|\\.WAV$", ""),
      "_",
      stringr::str_pad(sprintf("%.3f", round(det_i()$detection_start, 3)),
                       7, pad = "0"),
      "-",
      stringr::str_pad(sprintf("%.3f", round(det_i()$detection_end, 3)),
                       7, pad = "0"),
      "s_",
      stringr::str_pad(sprintf("%.3f", round(det_i()$template_min_freq, 3)),
                       6, pad = "0"),
      "-",
      stringr::str_pad(sprintf("%.3f", round(det_i()$template_max_freq, 3)),
                       6, pad = "0"),
      "kHz_",
      wl(),
      "wl_",
      ovlp(),
      "ovlp_",
      tail(stringr::str_split(gsub("\\.wav$", "", det_i()$template_name),
                              "_")[[1]], 1),
      ".jpeg"
    )
    res
  })
  shiny::observe({
    shiny::req(spec_default_filename())
    shiny::updateTextInput(session, session$ns("spec_name"),
                           value = spec_default_filename())
  })
  # Reset the spectrogram file name
  shiny::observeEvent(input$reset_spec_filename, {
    shiny::req(spec_filename(), det_i())
    shiny::updateTextInput(session, session$ns("spec_name"),
                           value = spec_default_filename())
    spec_filename(spec_default_filename())
  })
  shiny::observeEvent(input$spec_name, {
    shiny::req(input$spec_name)
    spec_filename(input$spec_name)
  })
  # R4.3: build the spectrogram output path. ggsave infers the image device
  # from the filename extension, so a name without one (e.g. "teste") aborted
  # with "Can't save ... supply `filename` with a file extension". Append the
  # default .jpeg when the extension is missing, and strip any trailing slash
  # from the directory so the join has no "//".
  spec_outfile <- function() {
    fn <- spec_filename()
    if (!nzchar(tools::file_ext(fn))) fn <- paste0(fn, ".jpeg")
    dir <- sub("[\\\\/]+$", "", input$detec_spec_path)
    file.path(dir, fn)
  }
  list(
    export = function() {
      shiny::req(input$detec_spec_path, spec_filename(), rec_detection())
      res <- cowplot::plot_grid(spectro_template(), spectro_detection())
      ggplot2::ggsave(
        filename = spec_outfile(),
        plot = res,
        width = 12,
        height = 6,
        units = "in",
        dpi = 72
      )
      shiny::showNotification("Detection spectrogram successfully exported")
    }
  )
}

#' The detection-export shiny module. See LVA_EXPORT_ID above.
#'
#' @param id Module namespace id (the LVA_EXPORT_ID constant).
#' @param det_i Reactive returning the active detection row.
#' @param rec_detection Reactive returning the loaded detection `Wave` object.
#' @param spectro_template Reactive returning the template spectrogram plot.
#' @param spectro_detection Reactive returning the detection spectrogram plot.
#' @param wl Reactive returning the window length.
#' @param ovlp Reactive returning the overlap percentage.
#' @return Named list with the `hk_export_wav` and `hk_export_spectro` hotkey
#'   handlers.
lva_export_server <- function(id, det_i, rec_detection, spectro_template,
                              spectro_detection, wl, ovlp) {
  shiny::moduleServer(id, function(input, output, session) {
    wav <- .lva_export_wav(input, session, det_i, rec_detection, wl, ovlp)
    spec <- .lva_export_spec(input, session, det_i, rec_detection,
                             spectro_template, spectro_detection, wl, ovlp)
    shiny::observeEvent(input$confirm_wav_export, wav$export())
    shiny::observeEvent(input$confirm_spec_export, spec$export())
    list(
      hk_export_wav = wav$export,
      hk_export_spectro = spec$export
    )
  })
}
