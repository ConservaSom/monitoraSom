#' Turn detections back into ROIs
#'
#' @description `r lifecycle::badge("deprecated")`
#'
#'   Converts template-matching **detections** (or externally validated ones) into
#'   regions of interest (**ROIs**), written as signals data like any other row
#'   of the `signals.duckdb` store. Detection events can then re-enter the ROI
#'   workflow: visual review in the segmentation app, promotion to new templates
#'   via [export_templates()], or adding to a ground-truth set.
#'
#'   **Deprecated:** the `signals` database already holds every detection
#'   as a `roi_*` box; this conversion only re-packages the same boxes, it does
#'   not compute anything new. Prefer reading the `signals` database directly
#'   (the segmentation and validation apps do that) to get the same table in
#'   memory.
#'
#' @details Each detection becomes one ROI, carrying its time/frequency box, the
#'   species label read from the detection, `roi_type = "detection"`, and an
#'   origin column marking it as derived from a detection. Set `filter_tp =
#'   TRUE` to keep only detections you have already validated as true positives
#'   (needs a `validation` column).
#'
#'   Store separation is a rule of the analysis flow before monitoraSom 1.2.0:
#'   ROIs had to live in a store **distinct** from your hand-segmented ground
#'   truth, or a later validation could compare detections against themselves
#'   (circular validation, inflated metrics). The current flow supersedes it:
#'   detections live in the same `signals` store as ROIs, and promotion to ROI
#'   happens upon validation, manually or automatically. This deprecated
#'   function keeps the old guard: it refuses to write into a store that
#'   already holds ground-truth ROIs.
#'
#'   Promotion only makes sense for detections you validated as true positives
#'   (`filter_tp = TRUE`): a promoted set built this way does not inflate the
#'   ground truth.
#'
#'   The detection->ROI cycle: a detection becomes a ROI only after it is
#'   validated as a true positive. In the routine flow this happens
#'   automatically: [validate_by_overlap()] validates each detection against
#'   ground-truth ROIs and promotes every TP to a
#'   `signal_class = "detection_to_roi"` row in the same signals store
#'   (opt-out with `promote_to_roi = FALSE`). The segmentation app offers the
#'   same promotion by hand (tab Detections, `Alt+P`). After promoted ROIs
#'   join a reference set, update the diagnostics: re-run
#'   [validate_by_overlap()] and [diagnostic_validations()] so the metrics
#'   reflect the enlarged reference.
#'
#' @section Pipeline context:
#'   Step 10 of the monitoraSom analysis flow. Reads a detections table from
#'   [run_matching()] / [template_matching()] (steps 7/9). Produces standard
#'   ROIs for [export_templates()] (new templates) or the segmentation app; the
#'   ground-truth ROIs used by [validate_by_overlap()] (step 11) come from
#'   manual segmentation, not from here.
#'
#' @param df_detecs Detections: a `data.frame` in the standard detections
#'   format (tables from older versions are coerced), a path to a detections
#'   `.duckdb` database, or `r lifecycle::badge("deprecated")` a path to a
#'   detections CSV from versions before 1.2.0.
#' @param username Character tag identifying who created the ROIs. Letters,
#'   numbers, dots and spaces only; must not be empty after cleaning. Required.
#' @param output_db Optional path to a ROI DuckDB database; the ROIs are written
#'   there per soundscape (updated when they already exist). **Must be distinct
#'   from any hand-segmented ground-truth store** (see Details). `NULL` (default)
#'   persists nothing.
#' @param output_path `r lifecycle::badge("deprecated")` Old directory target;
#'   per-soundscape CSV ROI tables are written there (with a deprecation warning).
#'   Prefer `output_db`.
#' @param filter_tp Logical. Keep only true-positive detections (`validation ==
#'   "TP"`). Requires a `validation` column; otherwise warns and keeps all.
#'   Default `FALSE`.
#' @return A tibble of ROIs (signals data, `signal_class = "detection_to_roi"`);
#'   returned invisibly when the ROIs are written to a database.
#' @seealso [run_matching()] / [template_matching()] (produce the detections),
#'   [export_templates()] (promote ROIs to templates), [validate_by_overlap()].
#' @export
#' @examples
#' \donttest{
#' # Load the package
#' library(monitoraSom)
#'
#' # Load the bundled validated detections
#' data(df_detecs_val_manual)
#'
#' # Convert the detections to ROIs, keeping only the validated true positives.
#' # Promotion only makes sense for TPs: a promoted set built this way does not
#' # inflate the ground truth.
#' df_rois <- detecs_to_rois(df_detecs_val_manual, username = "User",
#'                           filter_tp = TRUE)
#' str(df_rois)          # standard ROI format, one row per TP detection
#'
#' # Persist the promoted ROIs into a DuckDB signals store (temporary file)
#' exported_ddb <- file.path(tempdir(), "rois_from_detections.duckdb")
#' detecs_to_rois(df_detecs_val_manual, username = "User",
#'                filter_tp = TRUE, output_db = exported_ddb)
#'
#' # Promoted rows are stored with signal_class = "detection_to_roi".
#' # fetch_rois() returns only the ROI rows of a store (the ground-truth and
#' # template branch); it leaves detection-derived rows out by design, so a
#' # store that holds only promoted rows reads back as empty:
#' fetch_rois(exported_ddb)
#' }
detecs_to_rois <- function(df_detecs, username = NULL, output_db = NULL,
                           output_path = NULL, filter_tp = FALSE) {

  lifecycle::deprecate_warn(
    "1.2.0", "detecs_to_rois()", "read_signals()",
    details = paste("The unified signals store already carries every detection as",
                    "a roi_* box. Read the signals store directly (apps), or call",
                    ".signals_as_rois(include_detections = TRUE) for the",
                    "in-memory projection."))

  # --- front door (DTR-06): validate before any processing -------------------
  if (is.null(username) || !is.character(username) || length(username) != 1L) {
    stop("Please provide a single `username` string.")
  }
  username <- gsub("[^a-zA-Z0-9\\. ]", "", username)
  if (!nzchar(trimws(username))) {
    stop("`username` is empty after sanitation; use letters, digits, '.' or ' '.")
  }
  if (!is.logical(filter_tp) || length(filter_tp) != 1L || is.na(filter_tp)) {
    stop("`filter_tp` must be a single TRUE/FALSE.")
  }
  if (!is.null(output_db)) {
    if (!is.character(output_db) || length(output_db) != 1L) {
      stop("`output_db` must be a single path to a ROI `.duckdb` store.")
    }
    if (!dir.exists(dirname(output_db))) {
      stop("The `output_db` directory does not exist: ", dirname(output_db))
    }
    # FEAT-07: warn once when persisting to an unmarked explicit path.
    .require_explicit_workspace(output_db, label = "output_db",
                                caller = "detecs_to_rois")
  }
  if (!is.null(output_path)) {
    .validate_legacy_output_path(output_path)
    .require_explicit_workspace(output_path, label = "output_path",
                                caller = "detecs_to_rois")
  }

  df_input <- .read_detections_input(df_detecs)            # DTR-01

  # --- optional true-positive filter (faithful semantics) --------------------
  if (filter_tp) {
    if ("validation" %in% names(df_input)) {
      df_input <- df_input[!is.na(df_input$validation) &
                             df_input$validation == "TP", , drop = FALSE]
    } else {
      warning("Data was not filtered - validation variable not found in input ",
              "data", call. = FALSE)
    }
  }

  .assert_detection_cols(df_input)                          # DTR-01 (schema-driven)

  rois <- .map_detections_to_rois(df_input, username)      # DTR-02/04/05/07

  # --- persistence -----------------------------------------------------------
  if (!is.null(output_db)) .save_rois_duckdb(rois, output_db)         # DTR-03 (F5: signals)
  if (!is.null(output_path)) .write_legacy_roi_csvs(rois, output_path) # DTR-03 compat

  if (!is.null(output_db) || !is.null(output_path)) invisible(rois) else rois
}

# --- input intake (DTR-01) ----------------------------------------------------

# Accept a data.frame, a detections .duckdb store, or a deprecated legacy CSV.
.read_detections_input <- function(df_detecs) {
  if (is.data.frame(df_detecs)) return(df_detecs)
  if (!is.character(df_detecs) || length(df_detecs) != 1L) {
    stop("`df_detecs` must be a data.frame, a detections `.duckdb` path, or a ",
         "legacy CSV path.")
  }
  if (!file.exists(df_detecs)) stop("File not found: ", df_detecs)
  if (file.info(df_detecs)$isdir) {
    stop("`df_detecs` is a directory; provide a detections `.duckdb` or CSV file.")
  }
  if (grepl("(?i)\\.duckdb$", df_detecs)) {
    # F6 (signals program): the detections store is the signals store — read the
    # detection-class rows through the compat layer (both layouts tolerated).
    con <- .signals_duckdb_connect(df_detecs)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    return(.signals_as_detections(.signals_duckdb_read(
      con, signal_class = .SIGNAL_CLASS_DETECTION)))
  }
  warning("Reading detections from a CSV is deprecated; persist detections with ",
          "`output_db` (DuckDB) and pass the store path instead.", call. = FALSE)
  df <- utils::read.csv(df_detecs, stringsAsFactors = FALSE)
  message("File read successfully")
  df
}

# The detection columns the mapping consumes (subset of the canonical schema,
# also present in the legacy 21-col layout) — schema-driven, no hand drift (C8).
.DTR_REQUIRED_COLS <- c(
  "soundscape_path", "soundscape_file", "template_name",
  "template_min_freq", "template_max_freq",
  "detection_start", "detection_end", "detection_wl", "detection_ovlp",
  "detection_sample_rate", "detection_buffer", "detection_min_score",
  "detection_min_quant", "detection_top_n", "peak_index", "peak_score",
  "peak_quant"
)

.assert_detection_cols <- function(df) {
  missing <- setdiff(.DTR_REQUIRED_COLS, names(df))
  if (length(missing) > 0) {
    stop("The detections are missing required column(s): ",
         paste(missing, collapse = ", "),
         ". Expected the canonical detections schema (.detection_schema_spec()).")
  }
  invisible(TRUE)
}

# --- mapping (DTR-02/04/05/07) ------------------------------------------------

# Parse one template_name into its ROI label: the last `_`-token after a
# case-insensitive extension strip (DTR-05). Faithful to the original rule but
# applied PER ROW (DTR-02 fix).
.roi_label_from_template_name <- function(template_name) {
  base <- sub("(?i)\\.wav$", "", template_name)
  vapply(strsplit(base, "_", fixed = TRUE),
         function(parts) if (length(parts)) parts[[length(parts)]] else NA_character_,
         character(1))
}

# AFL-04: derive the ROI label preferring the dedicated `template_label` (the
# template database's verbatim species/label, the single source of truth) and falling
# back to the fragile last-`_`-token parse of `template_name` only for legacy
# inputs that carry no label. The parse mislabels composite names and, on the
# refactored cut naming (`<soundscape>_<label>_<roi_type>_<template_id>.wav`),
# returns the template_id hash instead of the species.
.roi_label_from_detections <- function(df) {
  parsed <- .roi_label_from_template_name(df$template_name)
  if ("template_label" %in% names(df)) {
    tl <- df$template_label
    return(ifelse(!is.na(tl) & nzchar(tl), tl, parsed))
  }
  parsed
}

.map_detections_to_rois <- function(df, username) {
  if (nrow(df) == 0L) return(.schema_rois(0L))
  stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")          # DTR-07: capture once
  has_val <- "validation" %in% names(df)

  out <- .schema_rois(nrow(df))
  out$soundscape_path      <- .normalize_path_key(df$soundscape_path)   # AFL-06
  out$soundscape_file      <- df$soundscape_file
  out$roi_user             <- username
  out$roi_input_timestamp  <- stamp
  out$roi_label            <- .roi_label_from_detections(df)           # AFL-04
  # DTR-101 (plan §8.8(c)): carry a species correction from the LVA
  # detections-mode review (out-of-band `label_updated`); the derived
  # label above stays the creation-time roi_label.
  if ("label_updated" %in% names(df)) {
    out$roi_label_updated <- df$label_updated
  }
  out$roi_start            <- df$detection_start
  out$roi_end              <- df$detection_end
  out$roi_min_freq         <- df$template_min_freq
  out$roi_max_freq         <- df$template_max_freq
  out$roi_type             <- "detection"
  out$roi_source           <- .ROI_SOURCE_DETECTION         # AFL-02: provenance
  out$roi_label_confidence <- if (has_val) {
    ifelse(!is.na(df$validation) & df$validation == "TP", "certain",
           NA_character_)
  } else {
    NA_character_
  }
  # F6: the AFL-22 key=value payload is dead (signals program §4.4/SIG-03). The
  # structured detection metadata now lives in the det_* columns of the unified
  # signals store; the legacy ROI shape carries NA here — no fabricated payload.
  out$roi_comment <- NA_character_
  out$roi_wl          <- df$detection_wl
  out$roi_ovlp        <- df$detection_ovlp
  out$roi_sample_rate <- df$detection_sample_rate
  out$roi_pitch_shift <- 1L                                 # DTR-04: canonical integer
  # roi_is_complete / roi_channel stay typed NA (canonical defaults, DTR-04).
  out <- .coerce_rois(out)
  # DTR-101 (plan §8.8(c)): the correction rides out-of-band (the frozen
  # legacy shape has no such column; the coercion above drops extras).
  if ("label_updated" %in% names(df)) {
    out$roi_label_updated <- df$label_updated
  }
  out
}

# --- persistence helpers (DTR-03) ---------------------------------------------

# Write detection-derived ROIs through the signals store (F5: the conversion
# is a pure projection — `.signals_as_rois(include_detections = TRUE)`). Each
# row enters as signal_class = "detection" (its detection identity survives as
# signal_id); the roi-class rows of other producers are never touched (the
# save is class-scoped, R1). The C7 separation guard dies with the physical
# store split — the unified store makes circular validation a logical check,
# now enforced in `validate_by_overlap()` (R2).
.save_rois_duckdb <- function(rois, output_db) {
  con <- .signals_duckdb_connect(output_db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .signals_duckdb_upsert(con, .rois_as_signals(rois))
  message("Detection ROIs upserted to the signals store: ", output_db)
  invisible(rois)
}

# Deprecated per-soundscape CSV writer (DTR-03 compat). One file per soundscape,
# named with the legacy `<soundscape>_roi_<user>_<timestamp>.csv` convention.
.write_legacy_roi_csvs <- function(rois, output_path) {
  stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  for (sp in unique(rois$soundscape_path)) {
    part <- rois[rois$soundscape_path == sp, , drop = FALSE]
    base <- sub("(?i)\\.wav$", "", basename(part$soundscape_file[1]))
    fname <- sprintf("%s_roi_%s_%s.csv", base, part$roi_user[1], stamp)
    utils::write.csv(part, file.path(output_path, fname), row.names = FALSE,
                     fileEncoding = "UTF-8")
  }
  message("ROI tables exported to the deprecated CSV directory: ", output_path)
  invisible(rois)
}

# --- validation helpers (DTR-06) ----------------------------------------------

.validate_legacy_output_path <- function(output_path) {
  warning("`output_path` (per-soundscape CSV) is deprecated: persist with ",
          "`output_db` (DuckDB). Writing legacy CSV ROI tables this time.",
          call. = FALSE)
  if (!is.character(output_path) || length(output_path) != 1L ||
      !dir.exists(output_path)) {
    stop("The `output_path` directory does not exist: ",
         if (is.character(output_path)) output_path else "(not a path)")
  }
  invisible(TRUE)
}
