#' Standard detections schema (RMB-05; Stage 4 gate output-format decision)
#'
#' @description Single source of truth for the columns, order and R types of the
#'   `detections` table produced by [run_matching()] (peak detection via
#'   [fetch_score_peaks_i()]). Replaces the original's **hand-maintained 21-column
#'   CSV header** (RMB-05): add or rename a column **here** and the DuckDB backend
#'   ([_detections_duckdb.R]) follows. Mirrors [_schema_templates.R] /
#'   [_schema_rois.R] / [_schema_soundscapes.R] and reuses their generic
#'   `.typed_na()` / `.coerce_type()` helpers.
#'
#'   Two columns are added vs the original `fetch_score_peaks_i` output:
#'   - **`detection_id`** — short stable hash over the durable key
#'     `(soundscape_path, template_id, peak_index, score_method)`; the DuckDB
#'     primary key, so re-running a match **upserts** its detections rather than
#'     duplicating/append-corrupting them (dissolves RMB-03/04/06).
#'   - **`template_id`** — propagated from the template database through `df_grid`
#'     so detections join cleanly to the template database (gate schema gap). `NA` when the
#'     grid predates the propagation.
#'   - **`score_method`** — which engine produced the detection (`cor`/`fft`/`dtw`).
#'
#' @keywords internal
#' @noRd

.detection_schema_spec <- function() {
  c(
    detection_id          = "character",  # PK: hash of the durable key
    soundscape_path       = "character",
    soundscape_file       = "character",
    template_id           = "character",  # template database join key (NA if absent)
    template_path         = "character",
    template_file         = "character",
    template_name         = "character",
    template_label        = "character",  # AFL-04: species/label, verbatim from
                                          # the template database (NA for legacy grids ->
                                          # parsed from template_name downstream)
    template_min_freq     = "numeric",    # kHz
    template_max_freq     = "numeric",    # kHz
    template_start        = "numeric",    # s
    template_end          = "numeric",    # s
    score_method          = "character",  # cor | fft | dtw
    detection_start       = "numeric",    # s (window centre - pad)
    detection_end         = "numeric",    # s (window centre + pad)
    detection_wl          = "integer",
    detection_ovlp        = "integer",
    detection_sample_rate = "integer",
    detection_buffer      = "integer",
    detection_min_score   = "numeric",
    detection_min_quant   = "numeric",  # AFL-21: the filter() threshold applied
                                        # (the requested quantile), not the
                                        # achieved one -- see peak_quant below
    detection_top_n       = "integer",
    peak_index            = "integer",
    peak_score            = "numeric",
    peak_quant            = "numeric",  # AFL-21: ALWAYS pair-scope (the per-pair
                                        # valid-region ECDF set at capture time,
                                        # FSP-09). filter_detections_i(scope="grid")
                                        # recomputes a grid-wide quantile to decide
                                        # min_quant/top_n, but that grid quantile is
                                        # never written back here -- this column
                                        # does not reflect which scope a filter ran
                                        # under.
    soundscape_sha256      = "character", # AFL-23: source-recording content hash
                                          # at capture time, propagated from
                                          # df_grid (NA if compute_sha256=FALSE
                                          # upstream, e.g. PERF-02)
    detection_source_stale = "character" # AFL-23: "TRUE"/"FALSE" (schema has no
                                          # native logical type -- character
                                          # mirrors the roi_is_complete
                                          # convention). TRUE when the store
                                          # holds detections for this
                                          # soundscape_path under more than one
                                          # distinct soundscape_sha256 -- the
                                          # source file was overwritten with
                                          # different content between two
                                          # pipeline runs at the same path (set
                                          # on upsert, both old and new rows).
                                          # NA when not verifiable (no sha256
                                          # recorded on this row).
  )
}

#' Empty (or n-row NA) detections frame in standard schema/order.
#' @param n number of NA-filled rows to create (default 0).
.schema_detections <- function(n = 0L) {
  spec <- .detection_schema_spec()
  cols <- lapply(spec, function(type) rep(.typed_na(type), n))
  df <- as.data.frame(cols, stringsAsFactors = FALSE)
  names(df) <- names(spec)
  df
}

#' Coerce an arbitrary detections frame to the standard schema/types.
#' Missing standard columns are added as typed NA; extras are dropped.
#' @param df a data.frame to coerce.
.coerce_detections <- function(df) {
  spec <- .detection_schema_spec()
  if (is.null(df) || nrow(df) == 0L) return(.schema_detections(0L))
  out <- .schema_detections(nrow(df))
  for (col in names(spec)) {
    if (col %in% names(df)) out[[col]] <- .coerce_type(df[[col]], spec[[col]])
  }
  out
}

#' @noRd
#' Durable detection id — short sha256 over the identity tuple (mirror of
#' `.template_id()`). Vectorised over the input columns.
#'
#' @description **AFL-24 standard hash algorithm:** sha256 over the **raw UTF-8
#'   bytes** of the pipe-joined identity-tuple string (`digest::digest(key, algo
#'   = "sha256", serialize = FALSE)`), truncated to `n_hex` leading hex chars.
#'   `serialize = FALSE` is required: `digest()`'s default (`serialize = TRUE`)
#'   hashes R's *serialized representation* of the string (a version-dependent
#'   binary wrapper), not its bytes, so it could never match a Julia-side
#'   `bytes2hex(SHA.sha256(Vector{UInt8}(key)))[1:n_hex]` for the same logical
#'   key. With `serialize = FALSE`, both sides hash the same bytes.
#' @param soundscape_path,template_id,peak_index,score_method equal-length vectors.
#' @param channel Optional channel component for the key (default `NULL` = no
#'   component): when matching runs per channel, include it so left/right
#'   detections of the same pair never collide.
#' @param n_hex hash prefix length (default 16).
#' @return character vector of ids.
.detection_id <- function(soundscape_path, template_id, peak_index,
                          score_method, channel = NULL, n_hex = 16L) {
  tid <- ifelse(is.na(template_id), paste0("tpath_NA"), template_id)
  # forward-slash the path so the durable key is OS-independent (FR-06 convention)
  ss <- gsub("\\\\", "/", soundscape_path)
  ch <- if (is.null(channel)) rep("", length(ss)) else
        ifelse(is.na(channel), "", paste0("|", channel))
  key <- paste0(ss, "|", tid, "|", peak_index, "|", score_method, ch)
  vapply(key, function(k) {
    substr(digest::digest(k, algo = "sha256", serialize = FALSE), 1L, n_hex)
  }, character(1), USE.NAMES = FALSE)
}
