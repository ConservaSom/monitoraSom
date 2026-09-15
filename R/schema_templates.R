#' Standard template-metadata schema (ERC-101 / FTM-102)
#'
#' @description Single source of truth for the columns, order and R types of the
#'   `templates` template database produced by [export_templates()] and read back by
#'   [fetch_template_metadata()]. Mirrors the soundscape-metadata schema pattern
#'   in [_schema_soundscapes.R] and the ROI schema in [_schema_rois.R], and
#'   reuses their generic helpers (`.typed_na`, `.coerce_type`). Add or rename a
#'   column **here** and the DuckDB template database layer ([_template_duckdb.R])
#'   follows.
#'
#'   This schema replaces the legacy filename-encoding scheme
#'   (`{soundscape}_{start}-{end}s_{min}-{max}kHz_{wl}wl_{ovlp}ovlp_{label}.wav`)
#'   that the original `export_templates`/`fetch_template_metadata` pair used to
#'   round-trip metadata through the file name. Metadata now lives in the
#'   template database; the file name is kept short, stable and human-readable only
#'   (see `.template_cut_name()`).
#'
#'   Design notes:
#'   - **`origin_` provenance prefix (FR-103).** Columns describing the *source*
#'     recording a template was cut from take the `origin_` prefix
#'     (`origin_soundscape_path/file/sha256`). When `df_templates` is later
#'     joined to the **search** soundscapes (which keep the bare `soundscape_*`
#'     names), the two never collide.
#'   - **`template_mode` (ERC-103).** `"standalone_audio"` (a real WAV cut on
#'     disk) or `"reference_metadata"` (template database-only; cut on demand from the
#'     source, verified by `origin_soundscape_sha256`). A third value,
#'     `"spectrogram_cache"`, is **experimental and not yet implemented** (ERC-106,
#'     adiado): it is not producible through any public function and is dropped by
#'     [fetch_match_grid()] if hand-crafted. A template database may mix the supported modes.
#'   - **Hashes (ERC-104).** `origin_soundscape_sha256` ties the template to the
#'     exact source recording (detects later edits of the source);
#'     `template_sha256` is the integrity hash of the cut itself (NA for
#'     `reference_metadata` rows). `template_id` is a short, stable hash over the
#'     identity tuple so the same ROI always yields the same id (idempotent
#'     re-export).
#'   - **`template_channel` (ERC-105).** The channel the ROI was segmented on
#'     (`roi_channel`), carried end-to-end so the cut always comes from the
#'     channel the user chose.
#'
#'   Sentinel for absent values: R uses native typed `NA`; DuckDB stores `NULL`.
#'
#' @keywords internal
#' @noRd

# Column -> R storage type. Order defines the canonical column order.
.template_schema_spec <- function() {
  c(
    template_id              = "character",  # short stable hash (ERC-104)
    template_path            = "character",  # output WAV (NA in reference_metadata)
    template_file            = "character",  # basename of template_path
    template_label           = "character",  # from roi_label, verbatim (FTM-02)
    template_mode            = "character",  # standalone_audio | reference_metadata
    template_start           = "numeric",    # source-relative start (s)
    template_end             = "numeric",    # source-relative end (s)
    template_min_freq        = "numeric",    # kHz
    template_max_freq        = "numeric",    # kHz
    template_wl              = "integer",    # roi_wl
    template_ovlp            = "integer",    # roi_ovlp
    template_sample_rate     = "integer",    # actual cut sample rate
    template_pitch_shift     = "integer",    # roi_pitch_shift
    template_channel         = "character",  # roi_channel (ERC-105)
    template_sha256          = "character",  # integrity hash of the cut (ERC-104)
    origin_soundscape_path   = "character",  # provenance: source recording (FR-103)
    origin_soundscape_file   = "character",  # provenance
    origin_soundscape_sha256 = "character",  # provenance/version (ERC-104)
    roi_type                 = "character",  # carried from the ROI (FTM-02 note)
    roi_user                 = "character",  # who segmented
    roi_input_timestamp      = "character",  # when segmented
    roi_comment              = "character"   # free text
  )
}

#' Empty (or NA-filled) template skeleton data.frame in standard schema order.
#'
#' Mirrors `.schema_rois()`/`.schema_soundscapes()`. Reuses `.typed_na()` from
#' [_schema_soundscapes.R].
#'
#' @param n number of rows (all columns NA). Default 0 -> empty typed frame.
#' @noRd
.schema_templates <- function(n = 0L) {
  spec <- .template_schema_spec()
  cols <- lapply(spec, function(type) rep(.typed_na(type), n))
  df <- as.data.frame(cols, stringsAsFactors = FALSE)
  names(df) <- names(spec)
  df
}

#' Coerce a data.frame to the standard template schema (column subset + types).
#'
#' Keeps only standard columns, reorders them, and coerces each to its schema
#' storage type via `.coerce_type()`. Missing standard columns are added as
#' typed `NA`. Mirrors `.coerce_rois()`.
#' @noRd
.coerce_templates <- function(df) {
  spec <- .template_schema_spec()
  if (is.null(df) || nrow(df) == 0L) return(.schema_templates(0L))
  out <- .schema_templates(nrow(df))
  for (col in names(spec)) {
    if (col %in% names(df)) out[[col]] <- .coerce_type(df[[col]], spec[[col]])
  }
  out
}

# --- naming + identity --------------------------------------------------------

#' Short, stable template id derived from the ROI identity tuple (ERC-104).
#'
#' Idempotent: the same ROI (same source, bounds, channel, wl, ovlp, label)
#' always yields the same id, so re-exporting is a no-op. The id is a short
#' prefix of a sha256 over the identity tuple. `origin_soundscape_sha256` is the
#' strongest identity component when available; when it is `NA` (no source hash
#' supplied), `origin_soundscape_path` stands in so the id is still
#' deterministic.
#'
#' **AFL-24 standard hash algorithm** (mirrors [.detection_id()]): sha256 over
#' the raw UTF-8 bytes of the pipe-joined `identity_key` (`serialize = FALSE`),
#' so a Julia port can reproduce the identical id with
#' `bytes2hex(SHA.sha256(Vector{UInt8}(identity_key)))[1:n_hex]`.
#'
#' @param origin_sha256 source-recording sha256 (may be `NA`).
#' @param origin_path source-recording path (fallback identity).
#' @param start,end source-relative bounds (s).
#' @param channel ROI channel.
#' @param wl,ovlp spectrogram window length / overlap.
#' @param label ROI label.
#' @param n_hex number of leading hex chars to keep (default 12).
#' @return a length-1 character id.
#' @noRd
.template_id <- function(origin_sha256, origin_path, start, end, channel,
                         wl, ovlp, label, n_hex = 12L) {
  identity_key <- paste(
    if (is.na(origin_sha256)) paste0("path:", origin_path) else origin_sha256,
    sprintf("%.6f", start), sprintf("%.6f", end),
    if (is.na(channel)) "" else channel,
    wl, ovlp, if (is.na(label)) "" else label,
    sep = "|"
  )
  substr(digest::digest(identity_key, algo = "sha256", serialize = FALSE),
        1L, n_hex)
}

#' Sanitize a label for use inside a file name (filesystem-safe, readable).
#'
#' Replaces runs of any character outside `[A-Za-z0-9-]` with a single `_`, and
#' trims leading/trailing `_`. Spaces in species names (e.g.
#' `"Myiothlypis flaveola"`) become `_` -> `Myiothlypis_flaveola`.
#' @noRd
.sanitize_for_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9-]+", "_", as.character(x))
  x <- gsub("^_+|_+$", "", x)
  ifelse(x == "" | is.na(x), "template", x)
}

#' Build the short, stable cut file name (ERC-101 / FTM-01 / FTM-02).
#'
#' Reconciles the two design directions for the name: ERC-101 asked for a short,
#' collision-free `<label>_<id>` name; the user (FTM-01) asked the first field to
#' be the **origin soundscape**, the second the **ROI label**, the third a stable
#' **serial**, and (FTM-02 note, **approved 2026-06-02**) to also carry the
#' **ROI type**, which is useful information to surface in the folder. We honour
#' that field order and use `template_id` (the short identity hash) as the
#' collision-free serial:
#'
#'   `<origin_soundscape>_<sanitized_label>_<sanitized_roi_type>_<template_id>.wav`
#'
#' `roi_type` is omitted from the name when it is `NA`/empty (so untyped ROIs get
#' `<soundscape>_<label>_<id>.wav`). Metadata is no longer parsed from this name
#' (it lives in the template database); the name exists only to keep the cut folder
#' human-scannable and organized. The `template_id` guarantees uniqueness even
#' when two ROIs share a soundscape, label and type (the ERC-02 collision the
#' legacy scheme suffered).
#' @noRd
.template_cut_name <- function(origin_file, label, template_id, roi_type = NA) {
  base_origin <- tools::file_path_sans_ext(basename(as.character(origin_file)))
  parts <- c(
    .sanitize_for_filename(base_origin),
    .sanitize_for_filename(label)
  )
  if (!is.na(roi_type) && nzchar(as.character(roi_type))) {
    parts <- c(parts, .sanitize_for_filename(roi_type))
  }
  parts <- c(parts, template_id)
  paste0(paste(parts, collapse = "_"), ".wav")
}
