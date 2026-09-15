#' Standard monitoraSom DuckDB store names (AFL-17) and homes (CRAN item 3)
#'
#' @description Single source of truth for the **filenames** of the DuckDB stores
#'   the pipeline reads and writes. Before this, the names were scattered as
#'   string literals (`"rois.duckdb"` in [set_workspace()], `"templates.duckdb"`
#'   in [export_templates()]) while the soundscape-metadata, detections and
#'   validations stores had no standard name at all — so a user could not tell
#'   from the polymorphic path arguments whether the workspace holds one DuckDB
#'   file or several (AFL-17, source ABS-02).
#'
#'   [.monitora_db_homes()] carries the **standard home directory** of each
#'   store (CRAN item 3, plan `plans/2026-08-14_01_cran-app-features.md` §2e/§5).
#'   The apps resolve their default paths from name + home, so a rename lands in
#'   one place: `file.path(.monitora_db_homes()[[s]], .monitora_db_names()[[s]])`.
#'   Homes are the **defaults the apps resolve to**; they are not enforced — a
#'   user may always pass an explicit path.
#'
#'   **Standard workspace DuckDB layout** (up to five logical stores; a user may
#'   split or unify them, the defaults below are the standard choice):
#'
#'   | Logical store | Standard filename | Standard home | Producer |
#'   |---|---|---|---|
#'   | ROIs | `rois.duckdb` | project root | [set_workspace()] / segmentation app |
#'   | Soundscape metadata | `soundscapes_metadata.duckdb` | `soundscapes/` |
#'     [fetch_soundscape_metadata()] |
#'   | Template template database | `templates.duckdb` | `templates/` | [export_templates()] |
#'   | Detections | `detections.duckdb` | `detections/` | [run_matching()] / [template_matching()] |
#'   | Validations | `validations.duckdb` | `validations/` | [launch_validation_app()] |
#'
#'   **Signals-centric note (unified schema, F0-F6 + LVA-155):** the pipeline's
#'   data now lives in one `signals` table carried **inside these same `.duckdb`
#'   filenames** -- the logical-store names above stay the on-disk identity, and
#'   a store file holds the `signals` table whenever its producer writes the
#'   unified schema (producers and both apps, including the validation app's
#'   `val_*` upserts, since LVA-155). Legacy `rois`/`detections`/`validations`
#'   tables are tolerated on read by the store-aware readers. The filenames
#'   themselves are deliberately unchanged.
#'
#' @return A named character vector of the standard store filenames, keyed by
#'   logical store (`rois`, `soundscapes_metadata`, `templates`, `detections`,
#'   `validations`).
#' @keywords internal
#' @noRd
.monitora_db_names <- function() {
  c(
    rois                 = "rois.duckdb",
    soundscapes_metadata = "soundscapes_metadata.duckdb",
    templates            = "templates.duckdb",
    detections           = "detections.duckdb",
    validations          = "validations.duckdb"
  )
}

#' Standard home directory of each DuckDB store (CRAN item 3, §5).
#'
#' Home is relative to the project root; `""` (empty string) means the project
#' root itself. The apps resolve default paths as
#' `file.path(home, name)` anchored under `project_path` — never taken as an
#' absolute path. Tier-3 homes (`detections/`, `validations/`,
#' `soundscapes/`) are created **on demand** by the function that writes the
#' store, never by [set_workspace()].
#'
#' @return A named character vector of home directories, keyed by the same
#'   logical stores as [.monitora_db_names()].
#' @keywords internal
#' @noRd
.monitora_db_homes <- function() {
  c(
    rois                 = "",
    soundscapes_metadata = "soundscapes",
    templates            = "templates",
    detections           = "detections",
    validations          = "validations"
  )
}

#' Resolve the standard default path of a store (CRAN item 3, §2e).
#'
#' Single helper that joins name + home for a logical store key, so no app
#' hardcodes a default DuckDB literal. Callers anchor the result under their
#' `project_path` (e.g. via [.resolve_under_project()]).
#'
#' @param store Single character key of a logical store (one of the names of
#'   [.monitora_db_names()]).
#' @return Single character path, e.g. `"detections/detections.duckdb"`.
#' @keywords internal
#' @noRd
.monitora_db_default_path <- function(store) {
  home <- .monitora_db_homes()[[store]]
  name <- .monitora_db_names()[[store]]
  # An empty home (rois) means the project root: return the bare name.
  # file.path("", name) would wrongly produce an absolute "/name" path.
  if (nzchar(home)) file.path(home, name) else name
}

#' Anchor a relative path under `project_path` (CRAN item 3, §2c).
#'
#' The single anchoring rule for **every** path argument of both apps: a leading
#' `~` is expanded, an absolute path (POSIX `/`, Windows drive, UNC) is honoured
#' as-is, and a relative path lands under `project_path`. With the default
#' `project_path = "."` this is identical to the old cwd-relative behaviour, so
#' nobody loses; a caller that passes an explicit `project_path` gets every
#' relative path anchored there (LSA-116/117 semantics, generalized).
#'
#' @param path Single character path.
#' @param project_path Single character base directory (default `"."`).
#' @return Single character path.
#' @keywords internal
#' @noRd
.resolve_under_project <- function(path, project_path = ".") {
  path <- path.expand(path)
  if (grepl("^(/|\\\\|[A-Za-z]:[/\\\\])", path)) return(path)
  anchored <- file.path(project_path, path)
  # LSA-209 (root cause, found 2026-08-28): a relative base with a trailing
  # slash ("./") makes file.path() produce ".//soundscapes", which leaked into
  # the soundscape_path keys and, after the canonical-key normalization,
  # turned into a nonexistent root-absolute "/soundscapes". Clean relative
  # joins with collapse-BEFORE-strip (the join-key rule [.normalize_path_key()]
  # strips first and would map ".//x" to "/x"). Absolute/UNC/drive bases and
  # targets pass through untouched.
  if (!grepl("^(/|\\\\|[A-Za-z]:[/\\\\])", anchored)) {
    anchored <- gsub("\\\\", "/", anchored)
    anchored <- gsub("/{2,}", "/", anchored)
    anchored <- sub("^\\./", "", anchored)
  }
  anchored
}
