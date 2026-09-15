#' Standard ROI schema for the segmentation app (LSA-05 / LSA-21 / LSA-23)
#'
#' @description Single source of truth for the columns, order and R types of the
#'   `rois` table produced by the segmentation app and persisted to DuckDB
#'   ([_roi_duckdb.R]). Mirrors the soundscape-metadata schema pattern in
#'   [_schema_soundscapes.R] and reuses its generic helpers (`.typed_na`,
#'   `.coerce_type`). Add or rename a column **here** and the DuckDB layer
#'   follows.
#'
#'   Differences from the legacy SQLite schema in the alt app:
#'   - The surrogate `id INTEGER PRIMARY KEY AUTOINCREMENT` column is dropped:
#'     rows are grouped/deleted by `soundscape_path` (LSA-19), and the old `id`
#'     was already stripped before every write.
#'   - `roi_pitch_shift` is **integer** (LSA-21): the allowed values are a
#'     discrete code set (`-8, -6, -4, -2, 1`), not a fractional factor. DuckDB
#'     stores it as `INTEGER`.
#'   - `roi_channel` (LSA-109): the audio channel the ROI was segmented on.
#'     `"mono"` for mono recordings; `"left"`/`"right"` for the displayed channel
#'     of stereo recordings (default `"left"`). Legacy rows written before this
#'     column existed read back as `NA` (the DuckDB layer adds the column to an
#'     existing table via `ALTER TABLE ... ADD COLUMN IF NOT EXISTS`). Downstream
#'     consumers (`fetch_rois`, `export_templates`, validation) must preserve and,
#'     where relevant, cut/read this channel.
#'
#'   Sentinel for absent values: R uses native typed `NA`; DuckDB stores `NULL`.
#'
#' @keywords internal
#' @noRd

# Column -> R storage type. Order defines the canonical column order.
# Units (AFL-03): `roi_min_freq`/`roi_max_freq` are kHz; `roi_start`/`roi_end`
# are seconds; `roi_sample_rate` is Hz (so Nyquist in kHz is roi_sample_rate/2000).
.roi_schema_spec <- function() {
  c(
    soundscape_path      = "character",
    soundscape_file      = "character",
    roi_user             = "character",
    roi_input_timestamp  = "character",
    roi_label            = "character",
    roi_start            = "numeric",
    roi_end              = "numeric",
    roi_min_freq         = "numeric",
    roi_max_freq         = "numeric",
    roi_type             = "character",
    roi_label_confidence = "character",
    roi_is_complete      = "character",
    roi_comment          = "character",
    roi_wl               = "integer",
    roi_ovlp             = "integer",
    roi_sample_rate      = "integer",
    roi_pitch_shift      = "integer",
    roi_channel          = "character",
    roi_source           = "character",   # AFL-02: provenance (see below)
    roi_label_list       = "character"   # LSA-205: active label-list name
                                         # at annotation time (NA for legacy
                                         # rows / detection-derived ROIs)
  )
}

#' Empty (or NA-filled) ROI skeleton data.frame in standard schema order.
#'
#' Replaces the alt app's `empty_roi_df()` closure. Reuses `.typed_na()` from
#' [_schema_soundscapes.R].
#'
#' @param n number of rows (all columns NA). Default 0 -> empty typed frame.
#' @noRd
.schema_rois <- function(n = 0L) {
  spec <- .roi_schema_spec()
  cols <- lapply(spec, function(type) rep(.typed_na(type), n))
  df <- as.data.frame(cols, stringsAsFactors = FALSE)
  names(df) <- names(spec)
  df
}

#' Coerce a data.frame to the standard ROI schema (column subset + types).
#'
#' Keeps only standard columns (dropping a stray legacy `id`, etc.), reorders
#' them, and coerces each to its schema storage type via `.coerce_type()`.
#' Missing standard columns are added as typed `NA`.
#'
#' CRAN-103: `soundscape_path` is canonicalized to the workspace-relative
#' form via [.normalize_soundscape_paths()] — `"./"`/`"//"` cosmetics are
#' always stripped; when `workspace_root` is supplied, absolute paths under
#' the root are rewritten relative to it.
#'
#' @param df data.frame with ROI columns (any subset).
#' @param workspace_root optional workspace root (project path). Absolute
#'   `soundscape_path` values under it become workspace-relative; absolute
#'   paths outside it are kept unchanged.
#' @noRd
.coerce_rois <- function(df, workspace_root = NULL) {
  spec <- .roi_schema_spec()
  if (is.null(df) || nrow(df) == 0L) return(.schema_rois(0L))
  out <- .schema_rois(nrow(df))
  for (col in names(spec)) {
    if (col %in% names(df)) out[[col]] <- .coerce_type(df[[col]], spec[[col]])
  }
  out$soundscape_path <- .normalize_soundscape_paths(out$soundscape_path,
                                                     workspace_root)
  out
}

#' Canonical normalization for a `soundscape_path` **join key** (AFL-06).
#'
#' The single shared rule for every producer/consumer of `soundscape_path`
#' (set_workspace, fetch_rois, validate_by_overlap, detecs_to_rois). Forward-slash
#' the separators, drop a leading `"./"` and collapse repeated slashes, so keys
#' that differ only by `"./"`/`"//"` cosmetics still match across stages (a
#' divergence here silently dropped the provenance LEFT JOIN). Not for physical
#' paths — collapsing `//` would corrupt a Windows UNC root.
#'
#' Lives in [_schema_rois.R] since CRAN-103 (the path-key contract belongs with
#' the soundscape_path schema; `.normalize_soundscape_paths()` below builds on
#' it).
#' @noRd
.normalize_path_key <- function(x) {
  x <- gsub("\\\\", "/", x)
  x <- sub("^\\./", "", x)
  gsub("/{2,}", "/", x)
}

# --- soundscape_path canonical form (CRAN-103) --------------------------------
#
# Contract: `soundscape_path` (the durable recording key, LSA-19) is stored
# RELATIVE to the workspace root whenever the recording lives under it.
# Absolute paths bind ROIs to the machine that produced them: the same
# recording re-segmented elsewhere gets a second key, splitting the store.
#
# Rules:
#   1. cosmetic normalization always (delegates to `.normalize_path_key()`:
#      forward slashes, no leading "./", no duplicate slashes);
#   2. with a non-NULL `workspace_root`, absolute paths under the root are
#      rewritten relative to it (a relative `workspace_root` resolves against
#      the current working directory);
#   3. absolute paths NOT under the root are kept unchanged — they point
#      outside the workspace; refusing them belongs to callers with user
#      context (the app shows its own explicit-path handling; the migrator
#      keeps them and the rewrite cycle reports them).
.normalize_soundscape_paths <- function(x, workspace_root = NULL) {
  x <- .normalize_path_key(x)
  if (is.null(workspace_root)) return(x)
  root <- .normalize_path_key(workspace_root)
  if (!grepl("^(/|[A-Za-z]:[\\\\/])", root)) {
    root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  }
  root <- sub("/+$", "", root)
  prefix <- paste0(root, "/")
  under <- !is.na(x) & startsWith(x, prefix)
  x[under] <- substring(x[under], nchar(prefix) + 1L)
  x
}

# --- no_soi sentinel contract (LSA-23) ---------------------------------------
#
# A "no signals of interest" sentinel is a *single* ROI row recording that a
# soundscape was reviewed and found to contain no signal of interest. It is
# deliberately distinct from an unsegmented (never-reviewed) soundscape.
#
# Contract:
#   - `roi_label` is exactly `.ROI_NO_SOI_LABEL` (the only reliable marker).
#   - `roi_start = 0`, `roi_end = <full duration>`; `roi_min_freq`/`roi_max_freq`
#     span the visible frequency window at marking time.
#   - `roi_type`, `roi_label_confidence`, `roi_is_complete`, `roi_comment` are
#     `NA`; `roi_wl`, `roi_ovlp`, `roi_sample_rate`, `roi_pitch_shift` carry the
#     session values.
#   - Status rule: a soundscape is `no_soi` iff **every** row is a sentinel
#     (n_soi == n_total); see [.roi_duckdb_statuses()].
#
# Downstream readers MUST filter rows where `roi_label == .ROI_NO_SOI_LABEL`
# before treating rows as biological ROIs (validation, cut export). The
# sentinel-row builder lives with the app wiring (LSA-06), reusing this constant.
.ROI_NO_SOI_LABEL <- "no signals of interest"

# TRUE for sentinel labels (NA-safe).
.is_no_soi_label <- function(x) {
  !is.na(x) & x == .ROI_NO_SOI_LABEL
}

# --- ROI provenance (AFL-02) -------------------------------------------------
#
# `roi_source` separates ground-truth ROIs (segmented by a person — the manual
# store the validation compares against) from detection-derived ROIs (produced
# by [detecs_to_rois()]). Mixing them in one store risks circular validation and
# inflated metrics. Values:
#   - "detection": derived from detections (stamped by detecs_to_rois).
#   - "manual":    ground-truth segmentation.
#   - NA:          legacy / unstamped (e.g. the segmentation app, which writes
#                  ground-truth) -> treated as manual/ground-truth, NEVER as a
#                  detection unless explicitly stamped.
.ROI_SOURCE_DETECTION <- "detection"
.ROI_SOURCE_MANUAL    <- "manual"

# TRUE for detection-derived rows (NA-safe). The dedicated `roi_source` is the
# primary signal; `roi_type == "detection"` is the legacy fallback for stores
# written before `roi_source` existed. Everything else is ground-truth.
.is_detection_roi <- function(df) {
  n   <- if (is.data.frame(df)) nrow(df) else length(df[["roi_source"]])
  src <- if ("roi_source" %in% names(df)) df$roi_source else rep(NA_character_, n)
  typ <- if ("roi_type"  %in% names(df)) df$roi_type  else rep(NA_character_, n)
  (!is.na(src) & src == .ROI_SOURCE_DETECTION) |
    (!is.na(typ) & typ == .ROI_SOURCE_DETECTION)
}
