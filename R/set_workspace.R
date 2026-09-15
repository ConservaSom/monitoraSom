#' Set up a monitoraSom project workspace
#'
#' @description
#' Creates a monitoraSom project on disk. It lays out the standard folder
#' skeleton under `project_path`, writes a plain-text project marker
#' (`monitoraSom.proj`) and creates the editable `app_presets/` default files.
#' It is the first function a new user runs
#' and produces the folder layout every later step reads from and writes into.
#'
#' @details
#' Run this once, in an **existing, writable** directory that will hold the
#' project. The function is deliberately simple and stable: it only lays out
#' directories and a couple of small text/preset files. It does **not** create
#' the pipeline databases: each `.duckdb` database is created when the function
#' that owns it writes to it for the first time (the segmentation app writes
#' `rois.duckdb`, `export_templates()` writes `templates/templates.duckdb`, and
#' so on), so a fresh workspace holds folders, not databases.
#'
#' The default skeleton creates only the essential folders: `templates/` (ROI
#' cuts + template database) and `app_presets/` (customisation), plus the
#' project marker. `match_scores/` is created on demand by [run_matching()]
#' when scores are persisted (DEC-3).
#' Every other folder is created **on demand** by the function that needs it,
#' and is skipped here unless you pass its path explicitly (`NA`, the default
#' for all of them). `rois.duckdb` stays at the project root.
#
#' Each `*_path` argument takes a **bare directory name** placed under
#' `project_path`. Pass an absolute path to put that directory elsewhere, or
#' `NA` to skip it entirely (useful when, for example, your soundscapes already
#' live in an external folder). For a ready-made runnable example project, see
#' [fetch_example_data()]; `example_data` no longer populates the workspace.
#'
#' Two things to know: `project_path` is **required** (there is no
#' auto-detection), and the directory must already exist and be writable, or
#' the call stops right away. Nothing is ever silently overwritten: the marker
#' and presets are written only when missing. The remaining
#' folders (scores, detections, validations, diagnostics) are created later, by
#' the functions that fill them, not by this function.
#'
#' @section Pipeline context:
#' Step 0 of the monitoraSom analysis flow. Reads nothing (entry point).
#' Produces the workspace directory layout and project marker
#' used by every later step. See also: \code{\link{launch_segmentation_app}}.
#'
#' @param project_path Path to an existing, writable project directory; the whole
#'   workspace is created beneath it. **Required**, as there is no default and no
#'   auto-detection. A missing or non-writable directory errors immediately.
#' @param example_data Discontinued. The runnable example ships as a standalone
#'   project: call [fetch_example_data()] and use the directory it returns as
#'   `project_path`. When `TRUE`, a message points to that flow and nothing is
#'   populated. Kept only so existing scripts get a readable message.
#' @param app_presets_path,templates_path,match_scores_path,soundscapes_path
#'   Each is a single character string: the **bare directory name** (relative to
#'   `project_path`) for that workspace subdirectory. Pass an absolute path to
#'   place the directory elsewhere, or a single `NA` to skip its creation. Only
#'   the essential pair (`templates`, `app_presets`) defaults to a name;
#'   `match_scores` defaults to `NA` (created on demand when scores are
#'   persisted); every other directory defaults to `NA` (created on demand by the
#'   function that uses it). `grids`, `validations` and `diagnostics` are the
#'   names used since monitoraSom 1.2.0 (before 1.2.0 they were
#'   `match_grid_metadata`, `validation_outputs` and
#'   `validation_diagnostics`).
#' @param recordings_path,roi_tables_path,roi_cuts_path,templates_metadata_path
#'   Workspace subdirectory name; same rules as `app_presets_path`: a bare
#'   directory name, an absolute path, or `NA` (default) to skip creation.
#' @param grids_path,detections_path,detection_cuts_path,detection_spectrograms_path
#'   Workspace subdirectory name; same rules as `app_presets_path`: a bare
#'   directory name, an absolute path, or `NA` (default) to skip creation.
#' @param validations_path,diagnostics_path Workspace subdirectory name; same
#'   rules as `app_presets_path`: a bare directory name, an absolute path, or
#'   `NA` (default) to skip creation.
#'
#' @return Invisibly, a named logical vector with **one element per non-skipped
#'   directory** (an `NA`-skipped directory is absent from the vector, not
#'   `FALSE`): `TRUE` if the directory was created this call, `FALSE` if it
#'   already existed. Directories, the marker and presets are written as side
#'   effects.
#'
#' @seealso \code{\link{launch_segmentation_app}} (the next step: segment
#'   soundscapes into ROIs).
#'
#' @export
#' @examples
#' \dontrun{
#' # Load the package
#' library(monitoraSom)
#'
#' # Step 0: create a monitoraSom project workspace in a temporary directory.
#' proj <- file.path(tempdir(), "my_monitoring_project")
#' dir.create(proj)
#'
#' # Check the content
#' list.dirs(proj)
#' list.files(proj)
#'
#' # Skeleton (directories + marker):
#' set_workspace(project_path = proj)
#'
#' # Check the content again
#' list.dirs(proj)
#' list.files(proj)
#' }
set_workspace <- function(
    project_path = NULL, example_data = FALSE,
    app_presets_path = "app_presets",
    templates_path = "templates",
    match_scores_path = NA_character_,
    soundscapes_path = NA_character_,
    recordings_path = NA_character_,
    roi_tables_path = NA_character_,
    roi_cuts_path = NA_character_,
    templates_metadata_path = NA_character_,
    grids_path = NA_character_,
    detections_path = NA_character_,
    detection_cuts_path = NA_character_,
    detection_spectrograms_path = NA_character_,
    validations_path = NA_character_,
    diagnostics_path = NA_character_) {

  # SW-103: project_path is required (no rstudioapi auto-detection).
  if (is.null(project_path)) {
    stop("'project_path' is required. Please provide the path to an existing ",
         "project directory.")
  }
  if (!dir.exists(project_path)) {
    stop("The provided project path does not exist: ", project_path)
  }
  # SW-08: fail early with a clear message on a non-writable project_path.
  if (file.access(project_path, mode = 2L) != 0L) {
    stop("The project path is not writable: ", project_path)
  }

  # Bare names (SW-13) in the canonical order; NA entries are skipped.
  # CRAN item 3 (§5): only the essential tier defaults to a name; tier-3/4
  # directories default to NA (on-demand creation by their producers).
  directories <- list(
    app_presets = app_presets_path,
    templates = templates_path,
    match_scores = match_scores_path,
    soundscapes = soundscapes_path,
    recordings = recordings_path,
    grids = grids_path,
    roi_tables = roi_tables_path,
    roi_cuts = roi_cuts_path,
    templates_metadata = templates_metadata_path,
    detections = detections_path,
    detection_spectrograms = detection_spectrograms_path,
    detection_cuts = detection_cuts_path,
    validations = validations_path,
    diagnostics = diagnostics_path
  )
  # AUD-16: validate every directory argument up front (single character, or a
  # single NA to skip) BEFORE any side effect, so an invalid argument never
  # leaves the marker / a partial directory tree behind.
  .validate_workspace_dirs(directories)

  # SW-103: a plain-text marker replaces the .Rproj / usethis project.
  .write_project_marker(project_path)

  # SW-101/SW-01/SW-07: create the directory skeleton anchored to project_path.
  created <- .create_workspace_dirs(project_path, directories)

  # SW-14: seed app_presets/ with both editable default files (when not skipped).
  if (!is.na(app_presets_path)) {
    .seed_app_presets(.ws_anchor(project_path, app_presets_path))
  }

  # DEC-5: the bundled example is discontinued; the runnable example now ships
  # as a standalone project fetched by fetch_example_data().
  if (isTRUE(example_data)) {
    message("- 'example_data = TRUE' is discontinued and populates nothing. ",
            "The runnable example ships as a standalone project: run ",
            "fetch_example_data() and use the directory it returns as ",
            "'project_path'.")
  }
  message("Workspace set successfully. Check the workspace and start a new R ",
          "session within the project to use monitoraSom.")

  invisible(created)
}

# --- path helpers ------------------------------------------------------------

# TRUE for an absolute path on POSIX ("/...") or Windows ("C:\..." / "C:/...").
.ws_is_absolute <- function(path) {
  grepl("^(/|\\\\|[A-Za-z]:[/\\\\])", path)
}

# Anchor a bare directory name to project_path; honour an absolute path as-is.
.ws_anchor <- function(project_path, path) {
  # AUD-15: expansion of a leading "~" and the absolute/relative split live in
  # the single CRAN-item-3 anchoring helper (also used by both apps).
  .resolve_under_project(path, project_path)
}

#' Validate the workspace directory arguments up front (AUD-16).
#'
#' Each entry must be a single character string (a bare name or an absolute
#' path) or a single `NA` (skip that directory). Errors name the offending
#' `<name>_path` argument, and — because this runs before any side effect — an
#' invalid argument leaves no marker or partial directory tree
#' behind. A vector argument (the AUD-03 silent-skip archetype) now errors
#' instead of being silently dropped.
#' @noRd
.validate_workspace_dirs <- function(directories) {
  for (name in names(directories)) {
    path <- directories[[name]]
    if (is.null(path) || length(path) != 1L) {
      stop("set_workspace: the '", name, "_path' argument must be a single ",
           "directory name, a single absolute path, or NA (got length ",
           length(path), ").")
    }
    if (!is.character(path) && !is.na(path)) {
      stop("set_workspace: the '", name, "_path' argument must be a character ",
           "string or NA (got ", class(path)[1L], ").")
    }
  }
  invisible(directories)
}

# --- project marker (SW-103) -------------------------------------------------

#' Write the plain-text project marker, base-R replacement for the `.Rproj`.
#'
#' Writes `monitoraSom.proj` (key=value, UTF-8) at the project root so the
#' pipeline can detect the workspace (in the spirit of the test suites that walk
#' up to `CLAUDE.md`). Never overwrites an existing marker.
#' @noRd
.write_project_marker <- function(project_path) {
  marker <- file.path(project_path, "monitoraSom.proj")
  if (file.exists(marker)) {
    message("- The project marker ALREADY EXISTS at '", marker,
            "'. It will not be overwritten.")
    return(invisible(FALSE))
  }
  lines <- c(
    "# monitoraSom project marker",
    "marker_version: 1",
    "package: monitoraSom",
    paste0("created_at: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
  )
  writeLines(lines, marker, useBytes = TRUE)
  message("- The project marker WAS CREATED at '", marker, "'")
  invisible(TRUE)
}

# --- directory skeleton (SW-01/SW-02/SW-07/SW-13) ----------------------------

#' Create the workspace directory skeleton anchored to `project_path`.
#'
#' Iterates the named `directories` list, skipping `NA` entries. Each path is
#' anchored to `project_path` ([.ws_anchor()]), created recursively (SW-07) and
#' its creation verified (an unwritable/failed target errors instead of warning
#' silently). Names come from the list, not `deparse(substitute())` (SW-02).
#' @return named logical: TRUE = created this call, FALSE = already existed.
#' @noRd
.create_workspace_dirs <- function(project_path, directories) {
  created <- logical(0)
  for (name in names(directories)) {
    path <- directories[[name]]
    # Arguments were validated up front (AUD-16, .validate_workspace_dirs);
    # here we only honour the NA-skip contract.
    if (is.na(path)) next                              # NA skips this directory
    target <- .ws_anchor(project_path, path)
    if (dir.exists(target)) {
      message("- The '", name, "' directory ALREADY EXISTS at '", target, "'")
      created[name] <- FALSE
    } else {
      dir.create(target, recursive = TRUE, showWarnings = FALSE)  # SW-07
      if (!dir.exists(target)) {
        stop("Failed to create the '", name, "' directory at '", target, "'")
      }
      message("- The '", name, "' directory WAS CREATED at '", target, "'")
      created[name] <- TRUE
    }
  }
  created
}
