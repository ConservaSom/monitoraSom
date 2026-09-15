#' Standard signals schema — unified annotation store (signals plan F1)
#'
#' @description Single source of truth for the columns, order and R types of
#'   the `signals` table: one row = one annotated time×frequency box on a
#'   recording, whatever its origin (`signal_class`: "roi" / "detection" /
#'   "detection_to_roi"). Replaces the column lists of [_schema_rois.R],
#'   [_schema_detections.R] and [_schema_validations.R] as the source of
#'   truth; those stay as frozen legacy shape definitions for the compat
#'   converters below (D7). Spec: `specs/R/signals.md`. Program:
#'   `plans/2026-08-17_02_unified-signals-schema.md` (F0–F6).
#'
#'   Organization (D3): shared box geometry and capture keep the `roi_*`
#'   vocabulary — the prefix names the **shared** columns, not only the "roi"
#'   class; consumers branch on `signal_class`, never on column presence.
#'   Class-exclusive metadata lives in namespaced groups: `det_*`
#'   (detections) and `val_*` (validation verdict, any class).
#'
#'   Reuses `.typed_na()` / `.coerce_type()` ([_schema_soundscapes.R]),
#'   `.detection_id()` ([_schema_detections.R], AFL-24 hash convention),
#'   `.normalize_path_key()` ([fetch_rois.R], AFL-06) and
#'   `.is_detection_roi()` ([_schema_rois.R], AFL-02).
#'
#' @keywords internal
#' @noRd

# --- constants ---------------------------------------------------------------

.SIGNAL_CLASS_ROI             <- "roi"
.SIGNAL_CLASS_DETECTION       <- "detection"
.SIGNAL_CLASS_DETECTION_TO_ROI <- "detection_to_roi"

.SIGNAL_ORIGIN_MANUAL          <- "manual"
.SIGNAL_ORIGIN_IMPORT_RAVEN    <- "import_raven"
.SIGNAL_ORIGIN_IMPORT_AUDACITY <- "import_audacity"
.SIGNAL_ORIGIN_TEMPLATE_MATCH  <- "template_matching"
.SIGNAL_ORIGIN_BIRDNET         <- "birdnet"  # importer: read_birdnet_table.R

# Origins that make a "roi"-class row eligible as ground truth (spec §ground
# truth). "template_matching" never qualifies a "roi"-class row.
.SIGNAL_MANUAL_ORIGINS <- c(.SIGNAL_ORIGIN_MANUAL,
                             .SIGNAL_ORIGIN_IMPORT_RAVEN,
                             .SIGNAL_ORIGIN_IMPORT_AUDACITY)

#' Column -> R storage type. Order defines the standard column order (spec:
#' 44 columns — core 24, det_* 14, val_* 6).
.signal_schema_spec <- function() {
  c(
    signal_id           = "character",  # PK: hash rule below
    signal_class        = "character",  # roi | detection | detection_to_roi
    soundscape_path     = "character",  # durable key (LSA-19, normalized)
    soundscape_file     = "character",
    soundscape_sha256   = "character",  # AFL-23; NA for legacy ROIs
    roi_start           = "numeric",    # s
    roi_end             = "numeric",    # s
    roi_min_freq        = "numeric",    # kHz
    roi_max_freq        = "numeric",    # kHz
    roi_label           = "character",  # species/label (creation-time,
                                        # SIG-08: IMMUTABLE)
    roi_type            = "character",  # behavioural type (roi_types.xlsx)
    roi_label_updated   = "character",  # SIG-08: user re-label (NA until
                                        # corrected; effective label source)
    roi_channel         = "character",  # mono | left | right
    roi_wl              = "integer",
    roi_ovlp            = "integer",
    roi_sample_rate     = "integer",    # Hz
    roi_pitch_shift     = "integer",    # code set -8,-6,-4,-2,1
    roi_label_confidence = "character",
    roi_is_complete     = "character",  # "TRUE"/"FALSE"
    roi_comment         = "character",  # human free text only
    origin              = "character",  # manual | import_raven |
                                        # import_audacity | template_matching
    created_by          = "character",
    created_at          = "character",  # "%Y-%m-%d %H:%M:%S"
    source_stale        = "character",  # "TRUE"/"FALSE"/NA (AFL-23)
    det_template_id     = "character",  # --- det_* group: detection rows only
    det_template_path   = "character",
    det_template_file   = "character",
    det_template_name   = "character",
    det_template_start  = "numeric",    # s, inside the template clip
    det_template_end    = "numeric",    # s
    det_score_method    = "character",  # cor | fft | dtw
    det_buffer          = "integer",
    det_min_score       = "numeric",
    det_min_quant       = "numeric",
    det_top_n           = "integer",
    det_peak_index      = "integer",
    det_peak_score      = "numeric",
    det_peak_quant      = "numeric",    # pair-scope (FSP-09)
    val_user            = "character",  # --- val_* group: any class
    val_time            = "character",
    val_verdict         = "character",  # TP | FP (FN is a query, never stored)
    val_note            = "character",
    val_order           = "integer",    # LVA-113
    val_subset          = "character",   # LVA-113
    roi_label_list      = "character"   # LSA-205: active label-list name at
                                        # annotation time (roi-class rows)
  )
}

#' Empty (or n-row NA) signals frame in standard schema/order.
#' @param n number of NA-filled rows to create (default 0).
.schema_signals <- function(n = 0L) {
  spec <- .signal_schema_spec()
  cols <- lapply(spec, function(type) rep(.typed_na(type), n))
  df <- as.data.frame(cols, stringsAsFactors = FALSE)
  names(df) <- names(spec)
  df
}

#' Coerce an arbitrary frame to the standard signals schema/types.
#' Missing standard columns are added as typed NA; extras are dropped.
#' @param df a data.frame to coerce.
.coerce_signals <- function(df) {
  spec <- .signal_schema_spec()
  if (is.null(df) || nrow(df) == 0L) return(.schema_signals(0L))
  out <- .schema_signals(nrow(df))
  for (col in names(spec)) {
    if (col %in% names(df)) out[[col]] <- .coerce_type(df[[col]], spec[[col]])
  }
  out
}

# --- signal_id (D4a: deterministic hash identity) ----------------------------
#
# sha256 over the raw UTF-8 bytes of a pipe-joined tuple, first 16 hex chars
# (AFL-24 convention — `digest::digest(key, algo = "sha256", serialize =
# FALSE)`). Detection rows reuse the frozen `.detection_id()` tuple unchanged,
# so signal_id == detection_id and existing joins survive. ROI rows hash the
# box tuple; identical tuples in one batch get an occurrence suffix (|2, |3,
# …) after sorting by (roi_start, roi_end), so the assignment is
# deterministic. Editing a box changes its id — safe because ROI saves are
# class-scoped replaces (store contract, F2).
#
# LVA-109 / plan 2026-08-18_02 §10.5 A1 (decisão D1): `roi_label` is NOT part
# of the tuple. A re-labelling in the validation app must not change the
# signal_id, otherwise the segmentation app's next class-scoped save would
# re-hash the row (losing the id) and the user's correction would be wiped.
# Existing persisted ids stay valid — they simply stop changing on re-label.

#' ROI-class signal ids for one save batch (vectorised, deterministic).
#' @param df signals-shaped frame (uses roi_* core columns + created_by).
#' @return character vector of ids, aligned with df's row order.
.signal_id_roi <- function(df) {
  n <- nrow(df)
  if (n == 0L) return(character(0))
  ord  <- order(df$roi_start, df$roi_end, method = "radix")  # stable sort
  inv  <- integer(n)
  inv[ord] <- seq_len(n)
  path <- .normalize_path_key(df$soundscape_path[ord])
  num  <- function(x) sprintf("%.15g", x)
  key0 <- paste(path, num(df$roi_start[ord]), num(df$roi_end[ord]),
                num(df$roi_min_freq[ord]), num(df$roi_max_freq[ord]),
                df$roi_channel[ord],
                df$created_by[ord], sep = "|")
  occ  <- ave(seq_len(n), key0, FUN = seq_along)             # 1,2,3… per key
  key  <- ifelse(occ == 1, key0, paste0(key0, "|", occ))
  ids  <- vapply(key, function(k) {
    substr(digest::digest(k, algo = "sha256", serialize = FALSE), 1L, 16L)
  }, character(1), USE.NAMES = FALSE)
  ids[inv]
}

# --- class predicates (single source of the ground-truth rule) ---------------

#' TRUE for detection-class rows (NA-safe).
#'
#' @param df Standard signals frame (needs a `signal_class` column).
.is_detection_signal <- function(df) {
  !is.na(df$signal_class) & df$signal_class == .SIGNAL_CLASS_DETECTION
}

#' TRUE for rows eligible as validation ground truth (spec; R2 guard):
#' "roi"-class with a manual-family origin, or promoted "detection_to_roi".
#' Never a raw "detection" row.
#'
#' @param df Standard signals frame (needs `signal_class` and `origin`).
.is_ground_truth_signal <- function(df) {
  manual_family <- !is.na(df$origin) & df$origin %in% .SIGNAL_MANUAL_ORIGINS
  (!is.na(df$signal_class) & df$signal_class == .SIGNAL_CLASS_ROI &
     manual_family) |
    (!is.na(df$signal_class) &
       df$signal_class == .SIGNAL_CLASS_DETECTION_TO_ROI)
}

#' @noRd
#' Effective ROI label (SIG-08): `roi_label_updated` when non-NA, else the
#' creation-time `roi_label`. Single source for every consumer that must
#' read the CORRECTED label (VBO-103, ERC-107, FR-106, LSA-123).
#'
#' @param roi_label Character vector: creation-time ROI labels.
#' @param roi_label_updated Character vector: corrected labels (or NA).
.signal_effective_label <- function(roi_label, roi_label_updated) {
  if (is.null(roi_label)) return(NULL)
  upd <- if (is.null(roi_label_updated)) rep(NA_character_, length(roi_label))
         else roi_label_updated
  ifelse(!is.na(upd) & nzchar(upd), upd, roi_label)
}

# --- legacy -> signals converters (D7) ---------------------------------------

#' @noRd
#' ROI label from a detections-shaped frame: prefer the dedicated
#' `template_label` (taken unchanged from the template database, AFL-04); fall back to
#' the last-`_`
#'-token parse of `template_name` only for legacy inputs without a label.
#'
#' @param df Detections-shaped frame (legacy or standard).
.signal_label_from_detections <- function(df) {
  base <- sub("(?i)\\.wav$", "", df$template_name)
  parsed <- vapply(strsplit(base, "_", fixed = TRUE),
                   function(parts) if (length(parts)) parts[[length(parts)]] else NA_character_,
                   character(1), USE.NAMES = FALSE)
  tl <- if ("template_label" %in% names(df)) df$template_label
        else rep(NA_character_, nrow(df))
  ifelse(!is.na(tl) & nzchar(tl), tl, parsed)
}

#' @noRd
#' Legacy `rois` frame (19 cols) -> signals. Unstamped `roi_source = NA` is
#' ground truth by the schema's own rule -> origin "manual" (SIG-01).
#' Richer legacy stamps pass through unchanged ("import_raven" /
#' "import_audacity", F3: the importer stamps them). Detection-derived rows
#' (`roi_source`/`roi_type` "detection", AFL-02 -> `.is_detection_roi()`)
#' become `signal_class = "detection_to_roi"` with
#' `origin = "template_matching"` and `roi_type = NA` (the provenance
#' overload moves to the class).
#'
#' @param df Legacy ROIs frame (19-column standard ROI schema).
#' @param workspace_root optional workspace root (CRAN-103): absolute
#'   `soundscape_path` values under it become workspace-relative.
.rois_as_signals <- function(df, workspace_root = NULL) {
  if (is.null(df) || nrow(df) == 0L) return(.schema_signals(0L))
  promoted <- .is_detection_roi(df)
  out <- .schema_signals(nrow(df))
  out$signal_class <- ifelse(promoted, .SIGNAL_CLASS_DETECTION_TO_ROI,
                             .SIGNAL_CLASS_ROI)
  out$soundscape_path <- .normalize_soundscape_paths(df$soundscape_path,
                                                     workspace_root)
  out$soundscape_file <- df$soundscape_file
  out$roi_start <- df$roi_start
  out$roi_end <- df$roi_end
  out$roi_min_freq <- df$roi_min_freq
  out$roi_max_freq <- df$roi_max_freq
  out$roi_label <- df$roi_label
  # SIG-08 (plan 2026-08-18_03 §8.8): transport the user's re-label; the
  # creation-time label stays immutable in `roi_label`.
  out$roi_label_updated <- if ("roi_label_updated" %in% names(df)) {
    df$roi_label_updated
  } else {
    rep(NA_character_, nrow(df))
  }
  out$roi_type <- ifelse(promoted, NA_character_, df$roi_type)
  out$roi_channel <- df$roi_channel
  out$roi_wl <- df$roi_wl
  out$roi_ovlp <- df$roi_ovlp
  out$roi_sample_rate <- df$roi_sample_rate
  out$roi_pitch_shift <- df$roi_pitch_shift
  out$roi_label_confidence <- df$roi_label_confidence
  out$roi_is_complete <- df$roi_is_complete
  out$roi_comment <- df$roi_comment
  manual_origin <- ifelse(is.na(df$roi_source), .SIGNAL_ORIGIN_MANUAL,
                          df$roi_source)
  out$origin <- ifelse(promoted, .SIGNAL_ORIGIN_TEMPLATE_MATCH, manual_origin)
  out$created_by <- df$roi_user
  out$created_at <- df$roi_input_timestamp
  # LSA-205: transport the annotation-time label-list name when the caller's
  # frame carries it (legacy 19-col frames and detection-derived ROIs do not).
  out$roi_label_list <- if ("roi_label_list" %in% names(df)) {
    df$roi_label_list
  } else {
    rep(NA_character_, nrow(df))
  }
  out$signal_id <- .signal_id_roi(out)
  .coerce_signals(out)
}

#' Legacy `detections` frame (27 cols) -> signals ("detection" class).
#' `signal_id == detection_id` (frozen tuple reused), so validation-app joins
#' and goldens survive untouched. An existing non-NA `detection_id` is
#' authoritative (kept unchanged — e.g. CRAN-98 legacy-CSV ids synthesized with
#' the template_file fallback); the tuple is recomputed only when NA.
#'
#' @param df Legacy detections frame (27-column standard detections schema).
.detections_as_signals <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(.schema_signals(0L))
  out <- .schema_signals(nrow(df))
  out$signal_id <- if (!is.na(df$detection_id[1]) && all(!is.na(df$detection_id))) {
    df$detection_id
  } else {
    .detection_id(df$soundscape_path, df$template_id,
                  df$peak_index, df$score_method)
  }
  out$signal_class <- .SIGNAL_CLASS_DETECTION
  out$soundscape_path <- .normalize_path_key(df$soundscape_path)
  out$soundscape_file <- df$soundscape_file
  out$soundscape_sha256 <- df$soundscape_sha256
  out$roi_start <- df$detection_start
  out$roi_end <- df$detection_end
  out$roi_min_freq <- df$template_min_freq
  out$roi_max_freq <- df$template_max_freq
  out$roi_label <- .signal_label_from_detections(df)
  out$roi_wl <- df$detection_wl
  out$roi_ovlp <- df$detection_ovlp
  out$roi_sample_rate <- df$detection_sample_rate
  out$origin <- .SIGNAL_ORIGIN_TEMPLATE_MATCH
  out$source_stale <- df$detection_source_stale
  out$det_template_id <- df$template_id
  out$det_template_path <- df$template_path
  out$det_template_file <- df$template_file
  out$det_template_name <- df$template_name
  out$det_template_start <- df$template_start
  out$det_template_end <- df$template_end
  out$det_score_method <- df$score_method
  out$det_buffer <- df$detection_buffer
  out$det_min_score <- df$detection_min_score
  out$det_min_quant <- df$detection_min_quant
  out$det_top_n <- df$detection_top_n
  out$det_peak_index <- df$peak_index
  out$det_peak_score <- df$peak_score
  out$det_peak_quant <- df$peak_quant
  .coerce_signals(out)
}

#' Legacy `validations` frame (33 cols) -> signals. Migration helper only:
#' detection rows carrying their validation verdict (`val_*` filled).
#'
#' A3 (plan 2026-08-18_02 §10.5): also carries `roi_label` /
#' `roi_label_updated` when present, and PRESERVES an existing `signal_id`
#' (the re-labelling round-trip must not re-hash the id).
#'
#' @param df Legacy validations frame (33-column standard validations schema).
.validations_as_signals <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(.schema_signals(0L))
  out <- .detections_as_signals(df)
  # Preserve the caller's signal_id when supplied (re-labelling keeps identity).
  if ("signal_id" %in% names(df)) {
    out$signal_id <- df$signal_id
  }
  # Re-labelling payload: carry the label + user correction through.
  if ("roi_label" %in% names(df)) out$roi_label <- df$roi_label
  if ("roi_label_updated" %in% names(df)) {
    out$roi_label_updated <- df$roi_label_updated
  }
  out$val_user <- df$validation_user
  out$val_time <- df$validation_time
  out$val_verdict <- df$validation
  out$val_note <- df$validation_note
  out$val_order <- df$validation_order
  out$val_subset <- df$validation_subset
  .coerce_signals(out)
}

# --- detection -> detection_to_roi promotion (LSA-208) -------------------------

#' Promote one detection row to an editable ROI (signals plan §8.3; LSA-208).
#'
#' Pure builder: copies the box geometry and capture columns of a
#' `signal_class = "detection"` row into a new `signal_class =
#' "detection_to_roi"` row with `origin = "template_matching"`, `roi_type`
#' NA (the provenance overload moves to the class, spec §class rules) and all
#' `det_*`/`val_*` columns NA (spec: promoted rows carry no detection
#' metadata). The `signal_id` is a FRESH `.signal_id_roi()` hash over the ROI
#' tuple -- never the detection's id, so upserting the promoted row can never
#' overwrite the source detection. `soundscape_sha256` is carried over so the
#' AFL-23 staleness flag keeps applying to the promoted row. The source
#' detection row is NOT modified (additive/non-destructive promotion).
#'
#' @param det one-row signals-shaped frame with `signal_class = "detection"`.
#' @param user `created_by` stamp (the segmentation-app user).
#' @param timestamp `created_at` stamp ("%Y-%m-%d %H:%M:%S"); parameterised so
#'   tests are deterministic.
#' @return one-row signals-shaped frame, class "detection_to_roi".
#' @noRd
.promote_detection_row <- function(det, user, timestamp) {
  if (nrow(det) != 1L) stop("`.promote_detection_row()` expects exactly one detection row.")
  out <- .schema_signals(1L)
  out$signal_class <- .SIGNAL_CLASS_DETECTION_TO_ROI
  out$soundscape_path <- det$soundscape_path
  out$soundscape_file <- det$soundscape_file
  out$soundscape_sha256 <- det$soundscape_sha256
  # Validation frames carry the standard detections vocabulary (detection_*);
  # projected signals rows carry roi_*. Read either so the automatic promotion
  # (validate_by_overlap, DEC-4) and the app promotion share this one helper.
  out$roi_start <- if ("roi_start" %in% names(det)) det$roi_start else
    det$detection_start
  out$roi_end <- if ("roi_end" %in% names(det)) det$roi_end else
    det$detection_end
  out$roi_min_freq <- if ("roi_min_freq" %in% names(det)) det$roi_min_freq else
    det$template_min_freq
  out$roi_max_freq <- if ("roi_max_freq" %in% names(det)) det$roi_max_freq else
    det$template_max_freq
  out$roi_label <- if ("roi_label" %in% names(det)) det$roi_label else
    .signal_label_from_detections(det)
  # DTR-101 (plan §8.8(c)): a detection-carried species correction flows to
  # the promoted ROI's roi_label_updated; the derived label stays in roi_label.
  if ("roi_label_updated" %in% names(det)) {
    out$roi_label_updated <- det$roi_label_updated
  }
  out$roi_channel <- if ("roi_channel" %in% names(det)) det$roi_channel else
    rep(NA_character_, 1L)
  out$roi_wl <- if ("roi_wl" %in% names(det)) det$roi_wl else
    det$detection_wl
  out$roi_ovlp <- if ("roi_ovlp" %in% names(det)) det$roi_ovlp else
    det$detection_ovlp
  out$roi_sample_rate <- if ("roi_sample_rate" %in% names(det)) det$roi_sample_rate else
    det$detection_sample_rate
  out$roi_pitch_shift <- if ("roi_pitch_shift" %in% names(det)) det$roi_pitch_shift else
    1L
  out$origin <- .SIGNAL_ORIGIN_TEMPLATE_MATCH
  out$created_by <- user
  out$created_at <- timestamp
  out$signal_id <- .signal_id_roi(out)
  out
}

# --- signals -> legacy converters (multi-output by design, D7) ---------------

#' @noRd
#' Signals -> legacy `rois` shape (19 cols). Default keeps `signal_class ==
#' "roi"` only. `include_detections = TRUE` also projects "detection" and
#' "detection_to_roi" rows (`roi_source`/`roi_type` "detection") — the
#' in-memory replacement for `detecs_to_rois()`. Known one-way losses: origin
#' "import_*" -> roi_source "manual" (SIG-02); pure-detection projections get
#' `roi_comment = NA` (SIG-03, the AFL-22 payload is dead).
#'
#' @param signals Standard signals frame (data frame).
#' @param include_detections Logical: also project "detection" rows as
#'   `detection_to_roi` ROIs (default FALSE).
.signals_as_rois <- function(signals, include_detections = FALSE) {
  keep <- which(!is.na(signals$signal_class) & (
    signals$signal_class == .SIGNAL_CLASS_ROI |
      (include_detections & signals$signal_class != .SIGNAL_CLASS_ROI)))
  df <- signals[keep, , drop = FALSE]
  if (nrow(df) == 0L) return(.schema_rois(0L))
  proj <- df$signal_class != .SIGNAL_CLASS_ROI      # projected detection-family
  out <- .schema_rois(nrow(df))
  out$soundscape_path <- df$soundscape_path
  out$soundscape_file <- df$soundscape_file
  out$roi_user <- df$created_by
  out$roi_input_timestamp <- df$created_at
  out$roi_label <- df$roi_label
  # SIG-08 (plan 2026-08-18_03 §8.8): carry the user's re-label through the
  # legacy 19-col projection when the caller supplies it (the frozen legacy
  # shape has no such column; the LVA ROI-review frame carries it out-of-band).
  if ("roi_label_updated" %in% names(df)) {
    out$roi_label_updated <- df$roi_label_updated
  }
  out$roi_start <- df$roi_start
  out$roi_end <- df$roi_end
  out$roi_min_freq <- df$roi_min_freq
  out$roi_max_freq <- df$roi_max_freq
  out$roi_type <- ifelse(proj, .ROI_SOURCE_DETECTION, df$roi_type)
  out$roi_label_confidence <- df$roi_label_confidence
  out$roi_is_complete <- df$roi_is_complete
  out$roi_comment <- df$roi_comment
  out$roi_wl <- df$roi_wl
  out$roi_ovlp <- df$roi_ovlp
  out$roi_sample_rate <- df$roi_sample_rate
  out$roi_pitch_shift <- df$roi_pitch_shift
  out$roi_channel <- df$roi_channel
  # LSA-205: carry the annotation-time label-list name back to the ROI shape
  # (NA when the caller's frame predates the column).
  out$roi_label_list <- if ("roi_label_list" %in% names(df)) {
    df$roi_label_list
  } else {
    rep(NA_character_, nrow(df))
  }
  out$roi_source <- ifelse(proj | df$origin == .SIGNAL_ORIGIN_TEMPLATE_MATCH |
                           df$origin == .SIGNAL_ORIGIN_BIRDNET,
                           .ROI_SOURCE_DETECTION, .ROI_SOURCE_MANUAL)
  out <- .coerce_rois(out)
  # SIG-08 (plan 2026-08-18_03 §8.8): carry the user's re-label out-of-band
  # (the frozen legacy 19-col shape has no such column; in-memory frames may
  # carry it, as the correction does today -- FR-106). The coercion above
  # drops extras, so the column is attached afterwards.
  if ("roi_label_updated" %in% names(df)) {
    out$roi_label_updated <- df$roi_label_updated
  }
  out
}

#' Signals -> legacy `detections` shape (27 cols). Keeps `signal_class ==
#' "detection"` only; other classes are dropped with an informative message
#' (they carry no det_* data — keeping them would fabricate values).
#'
#' @param signals Standard signals frame (data frame).
.signals_as_detections <- function(signals) {
  keep <- .is_detection_signal(signals)
  dropped <- sum(!keep)
  if (dropped > 0L) {
    message(sprintf(
      "Dropped %d non-detection row(s): signal_class != \"%s\" carries no det_* data.",
      dropped, .SIGNAL_CLASS_DETECTION))
  }
  df <- signals[keep, , drop = FALSE]
  if (nrow(df) == 0L) return(.schema_detections(0L))
  out <- .schema_detections(nrow(df))
  out$detection_id <- df$signal_id
  out$soundscape_path <- df$soundscape_path
  out$soundscape_file <- df$soundscape_file
  out$template_id <- df$det_template_id
  out$template_path <- df$det_template_path
  out$template_file <- df$det_template_file
  out$template_name <- df$det_template_name
  out$template_label <- df$roi_label
  out$template_min_freq <- df$roi_min_freq
  out$template_max_freq <- df$roi_max_freq
  out$template_start <- df$det_template_start
  out$template_end <- df$det_template_end
  out$score_method <- df$det_score_method
  out$detection_start <- df$roi_start
  out$detection_end <- df$roi_end
  out$detection_wl <- df$roi_wl
  out$detection_ovlp <- df$roi_ovlp
  out$detection_sample_rate <- df$roi_sample_rate
  out$detection_buffer <- df$det_buffer
  out$detection_min_score <- df$det_min_score
  out$detection_min_quant <- df$det_min_quant
  out$detection_top_n <- df$det_top_n
  out$peak_index <- df$det_peak_index
  out$peak_score <- df$det_peak_score
  out$peak_quant <- df$det_peak_quant
  out$soundscape_sha256 <- df$soundscape_sha256
  out$detection_source_stale <- df$source_stale
  .coerce_detections(out)
}

#' Signals -> legacy `validations` shape (33 cols): detection rows with a
#' non-NA `val_verdict`. FN rows are not synthesizable here (never persisted);
#' callers use the FN query (plan §8.2).
#'
#' @param signals Standard signals frame (data frame).
.signals_as_validations <- function(signals) {
  keep <- .is_detection_signal(signals) & !is.na(signals$val_verdict)
  df <- signals[keep, , drop = FALSE]
  if (nrow(df) == 0L) return(.schema_validations(0L))
  out <- .signals_as_detections(df)
  out$validation_user <- df$val_user
  out$validation_time <- df$val_time
  out$validation <- df$val_verdict
  out$validation_note <- df$val_note
  out$validation_order <- df$val_order
  out$validation_subset <- df$val_subset
  .coerce_validations(out)
}
