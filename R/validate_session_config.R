#' Validate and assemble the segmentation session configuration (LSA-05)
#'
#' @description Extracted from the `launch_segmentation_app()` pre-validation
#'   block so it is reachable by testthat and reusable by the Julia port. It
#'   validates the session parameters, creates the project directories it needs,
#'   and returns the assembled `session_data` list consumed by the Shiny server.
#'
#'   Structural decisions already applied here (vs. the alt app block):
#'   - **LSA-116/117**: `roi_db` is a DuckDB file (default `rois.duckdb`); the
#'     CSV-export parameter `roi_tables_path` is removed (CSV export is dropped,
#'     delegated to dedicated converter functions).
#'   - **LSA-03**: the dead `session_notes` parameter is removed.
#'
#'   Phase-4 validation normalisations applied here:
#'   - **LSA-08**: uniform policy for `time_guide_interval`/`freq_guide_interval`
#'     — hard-error on type/shape violations, fall back to the default (with a
#'     warning) only on a benign out-of-range value (a non-positive interval).
#'   - **LSA-09**: the `zoom_freq` ceiling and the UI slider ceiling share one
#'     constant, `.MAX_ZOOM_FREQ_KHZ`.
#'   - **LSA-15**: directory joins drop trailing slashes (let `file.path` insert
#'     the separator).
#'
#'   Directory structure created under `project_path` (LSA-15 — documented so
#'   downstream tooling and the Julia port can rely on it):
#'   \preformatted{
#'   <project_path>/
#'     rois.duckdb            # ROI store (default roi_db)
#'     templates/             # exported template cuts (default templates_path)
#'     app_presets/           # saved UI presets (derived from project_path)
#'       temp/                # transient audio segments for the player
#'   }
#'   When `project_path` is absent these fall back to the working directory
#'   (`roi_db`) and `tempdir()` (`temp_path`); `templates_path` is
#'   `NA`. Relative paths are anchored under `project_path` (CRAN item 3, §2c:
#'   [.resolve_under_project()]).
#'
#' @return a named list (`session_data`) with the validated configuration.
#' @keywords internal
#' @noRd

# Shared frequency ceiling (kHz) for both the zoom_freq validation and the UI
# zoom_freq_slider max (LSA-09): a single source of truth so a configured
# zoom_freq is always reachable from the slider.
.MAX_ZOOM_FREQ_KHZ <- 192

# AUD-21: shared scalar boundary check. LSA-08's policy is to hard-error on
# type/shape violations at the public boundary with an actionable per-parameter
# message. `is.logical()` alone accepts NA and length>1 vectors (which then break
# later in the server or collapse silently through `all()`); this closes that gap
# once for every logical parameter.
.chk_scalar_logical <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("'", name, "' must be a single TRUE or FALSE.")
  }
  invisible(x)
}
.validate_session_config <- function(project_path = NULL,
                                      roi_user = NULL,
                                      soundscapes_path = NULL,
                                      roi_db = NULL,
                                      templates_path = NULL,
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
                                      pitch_shift = 1) {
  session_data <- list()

  # CRAN item 7 (F8/D3-a): the ROI user is REQUIRED — it is stamped on every
  # saved ROI. Missing/blank stops before any directory creation; the old
  # "warn + NA + wait in-app" flow is replaced by an actionable error.
  if (is.null(roi_user) || !is.character(roi_user) ||
      length(roi_user) != 1L || is.na(roi_user) ||
      !nzchar(trimws(roi_user))) {
    stop("'roi_user' is required: identify yourself with a single, non-empty ",
         "name (e.g. launch_segmentation_app(roi_user = \"Ana\")).")
  }
  if (!is.null(project_path)) {
    if (!dir.exists(project_path)) {
      tryCatch(
        {
          dir.create(project_path)
          warning("Created project directory at '", project_path, "'")
        },
        error = function(e) {
          stop("Failed to create project directory at '", project_path, "': ",
               e$message)
        }
      )
    }
    session_data$project_path <- project_path
  }

  # AUD-22: the comma is stripped from `roi_user` for CSV column-safety. This
  # predates the DuckDB store, but ROI CSVs still circulate via the converters
  # (export_templates_duckdb_to_csv / fetch_rois), so the strip is kept. Note
  # the side effect: a name like "Silva, J." is stored as "Silva J.".
  session_data$roi_user <- as.character(gsub(",", "", roi_user))

  if (!is.null(soundscapes_path)) {
    # CRAN item 3 (§2c): a relative soundscape path is anchored under
    # project_path ("."-base keeps the legacy cwd behaviour when no project).
    soundscapes_path <- .resolve_under_project(
      soundscapes_path, if (is.null(project_path)) "." else project_path)
    if (!dir.exists(soundscapes_path)) {
      session_data$soundscapes_path <- NA
      warning("The path in 'soundscapes_path' was not found. Please correct within the app.")
    } else {
      session_data$soundscapes_path <- soundscapes_path
    }
  } else {
    session_data$soundscapes_path <- soundscapes_path
  }

  # DuckDB database path (LSA-116/117; CRAN item 3 §2c anchors a provided
  # relative path under project_path).
  if (is.null(roi_db)) {
    if (!is.null(project_path)) {
      roi_db <- file.path(project_path, "rois.duckdb")
    } else {
      roi_db <- "rois.duckdb"
      warning("No 'roi_db' or 'project_path' provided. Using 'rois.duckdb' in working directory.")
    }
  } else if (!is.null(project_path)) {
    roi_db <- .resolve_under_project(roi_db, project_path)
  }
  session_data$roi_db <- roi_db

  # Templates path (CRAN item 3, §2b: former cuts_path, home roi_cuts/)
  if (!is.null(templates_path)) {
    if (!is.null(project_path)) {
      templates_path <- .resolve_under_project(templates_path, project_path)
    }
    if (!dir.exists(templates_path)) {
      if (!is.null(project_path)) {
        dir.create(templates_path, recursive = TRUE, showWarnings = FALSE)
        warning("Created templates directory at '", templates_path, "'")
      } else {
        stop("'templates_path' does not exist and no 'project_path' provided to create it.")
      }
    }
    session_data$templates_path <- templates_path
  } else if (!is.null(project_path)) {
    templates_path <- .resolve_under_project("templates", project_path)  # LSA-15
    dir.create(templates_path, recursive = TRUE, showWarnings = FALSE)
    session_data$templates_path <- templates_path
  } else {
    session_data$templates_path <- NA
  }

  # AUD-21: a length check first, so a vector label_angle no longer collapses
  # silently through all().
  if (
    !is.numeric(label_angle) || length(label_angle) != 1L || is.na(label_angle) ||
      label_angle %% 10 != 0 || label_angle < 0 || label_angle > 90
  ) {
    stop("'label_angle' must be a single numeric multiple of 10 between 0 and 90.")
  }
  session_data$label_angle <- label_angle

  .chk_scalar_logical(show_label, "show_label")
  session_data$show_label <- show_label

  # LSA-08: hard-error on type/shape violations. 0 is the valid "guides off"
  # value; only a negative interval falls back to the default (0) with a warning.
  if (!is.numeric(time_guide_interval) || length(time_guide_interval) != 1) {
    stop("'time_guide_interval' must be a single numeric value.")
  }
  if (time_guide_interval < 0) {
    warning("'time_guide_interval' must be non-negative; using default 0 (guides off). [MSG-002]")
    session_data$time_guide_interval <- 0
  } else {
    session_data$time_guide_interval <- time_guide_interval
  }

  if (!is.numeric(freq_guide_interval) || length(freq_guide_interval) != 1) {
    stop("'freq_guide_interval' must be a single numeric value.")
  }
  if (freq_guide_interval < 0) {
    warning("'freq_guide_interval' must be non-negative; using default 0 (guides off). [MSG-002]")
    session_data$freq_guide_interval <- 0
  } else {
    session_data$freq_guide_interval <- freq_guide_interval
  }

  if (length(dyn_range) != 2 || !is.numeric(dyn_range) || anyNA(dyn_range)) {
    stop("'dyn_range' must be a numeric vector of length 2 (no NA).")
  }
  # AUD-21: equal endpoints are a zero-width range; reject them (strict).
  if (dyn_range[1] == dyn_range[2]) {
    stop("'dyn_range' endpoints must differ (got a zero-width range).")
  }
  if (dyn_range[1] > dyn_range[2]) {
    session_data$dyn_range <- sort(dyn_range)
    warning("Sorted 'dyn_range' to match expected order.")
  } else {
    session_data$dyn_range <- dyn_range
  }

  if (length(dyn_range_bar) != 2 || !is.numeric(dyn_range_bar) ||
      anyNA(dyn_range_bar)) {
    stop("'dyn_range_bar' must be a numeric vector of length 2 (no NA).")
  }
  if (dyn_range_bar[1] == dyn_range_bar[2]) {
    stop("'dyn_range_bar' endpoints must differ (got a zero-width range).")
  }
  session_data$dyn_range_bar <- sort(dyn_range_bar)
  if (dyn_range_bar[1] > dyn_range_bar[2]) {
    warning("Sorted 'dyn_range_bar' to match expected order.")
  }

  valid_wl_values <- c(128, 256, 512, 1024, 2048, 4096, 8192, 16384)
  if (!is.numeric(wl) || length(wl) != 1L || is.na(wl) ||
      !wl %in% valid_wl_values) {
    stop(sprintf("'wl' must be one of: %s.", paste(valid_wl_values, collapse = ", ")))
  }
  session_data$wl <- wl

  if (!is.numeric(ovlp) || length(ovlp) != 1L || is.na(ovlp) ||
      ovlp < 0 || ovlp > 80 || ovlp %% 10 != 0) {
    stop("'ovlp' must be a numeric value between 0 and 80, in steps of 10.")
  }
  session_data$ovlp <- ovlp

  valid_color_scales <- c("viridis", "magma", "inferno", "cividis",
                          "greyscale 1", "greyscale 2")
  if (!is.character(color_scale) || !(color_scale %in% valid_color_scales)) {
    stop(sprintf("'color_scale' must be one of: %s.", paste(valid_color_scales, collapse = ", ")))
  }
  session_data$color_scale <- color_scale

  .chk_scalar_logical(visible_bp, "visible_bp")
  session_data$visible_bp <- visible_bp

  .chk_scalar_logical(play_norm, "play_norm")
  session_data$play_norm <- play_norm

  if (length(zoom_freq) != 2 || !is.numeric(zoom_freq) || anyNA(zoom_freq)) {
    stop("'zoom_freq' must be a numeric vector of length 2 (no NA).")
  }
  if (any(zoom_freq < 0) || any(zoom_freq > .MAX_ZOOM_FREQ_KHZ)) {
    stop(sprintf("'zoom_freq' values must be between 0 and %g.",
                 .MAX_ZOOM_FREQ_KHZ))
  }
  # AUD-21: equal endpoints are a zero-width range; reject them (strict).
  if (zoom_freq[1] == zoom_freq[2]) {
    stop("'zoom_freq' endpoints must differ (got a zero-width range).")
  }
  # AUD-20: round to 0.1 kHz in BOTH branches (the out-of-order branch used to
  # store sort(zoom_freq) unrounded, so the same logical input yielded different
  # stored precision depending on argument order).
  if (zoom_freq[1] > zoom_freq[2]) {
    session_data$zoom_freq <- round(sort(zoom_freq) * 10) / 10
    warning("Sorted 'zoom_freq' to match expected order.")
  } else {
    session_data$zoom_freq <- round(zoom_freq * 10) / 10
  }

  # zoom_time: initial time window of the zoom_time slider. Shape is validated
  # here (length-2 numeric, no NA, non-negative, endpoints differ); the clamp
  # to the recording duration is runtime (only known when the WAV loads). No
  # rounding — continuous seconds.
  if (!is.null(zoom_time)) {
    if (length(zoom_time) != 2 || !is.numeric(zoom_time) || anyNA(zoom_time)) {
      stop("'zoom_time' must be a numeric vector of length 2 (no NA).")
    }
    if (any(zoom_time < 0)) {
      stop("'zoom_time' values must be non-negative (seconds).")
    }
    # AUD-21: equal endpoints are a zero-width range; reject them (strict).
    if (zoom_time[1] == zoom_time[2]) {
      stop("'zoom_time' endpoints must differ (got a zero-width range).")
    }
    if (zoom_time[1] > zoom_time[2]) {
      session_data$zoom_time <- sort(zoom_time)
      warning("Sorted 'zoom_time' to match expected order.")
    } else {
      session_data$zoom_time <- zoom_time
    }
  } else {
    session_data$zoom_time <- NULL
  }

  .chk_scalar_logical(nav_autosave, "nav_autosave")
  session_data$nav_autosave <- nav_autosave

  if (!is.numeric(pitch_shift) || length(pitch_shift) != 1L ||
      is.na(pitch_shift) || !pitch_shift %in% c(-8, -6, -4, -2, 1)) {
    stop("'pitch_shift' must be one of: -8, -6, -4, -2, 1.")
  }
  session_data$pitch_shift <- pitch_shift

  # Preset and temp paths: the preset directory is derived from the project
  # (DEC-6/STEP-7): under a project it is <project_path>/app_presets; without a
  # project it is the session tempdir (anchored absolutely, TD-06).
  if (!is.null(project_path)) {
    session_data$preset_path <- file.path(project_path, "app_presets")  # LSA-15
    dir.create(session_data$preset_path, recursive = TRUE, showWarnings = FALSE)
  } else {
    # TD-06: with a relative TMPDIR, tempdir() returns a cwd-relative string
    # that breaks after any setwd. Anchor the app scratch absolutely once.
    session_data$preset_path <- normalizePath(tempdir(), winslash = "/",
                                              mustWork = FALSE)
  }
  session_data$temp_path <- file.path(session_data$preset_path, "temp")  # LSA-15
  dir.create(session_data$temp_path, showWarnings = FALSE, recursive = TRUE)

  session_data
}
