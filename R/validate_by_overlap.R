#' Validate detections against ground-truth ROIs by temporal overlap
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Scores a detection run against hand-segmented ground truth. Each detection
#'   is labelled a true positive (TP) or false positive (FP) by whether it
#'   overlaps a ground-truth ROI of the same species in the same soundscape,
#'   and every ground-truth ROI that no detection overlapped is reconstructed
#'   as a false negative (FN), the raw material for precision/recall
#'   diagnostics.
#'
#' @details FN is counted **per template**: an ROI is a false negative for
#'   template *t* when no detection of *t* overlapped it (matching the downstream
#'   per-template diagnostics). Detections of a species that has no ground-truth
#'   ROIs at all are labelled FP rather than silently dropped, so nothing is lost.
#'
#'   The result is deliberately split into two tidy tables rather than one wide
#'   frame: `$detections_validated` (one row per detection, TP/FP) and
#'   `$false_negatives` (one row per missed ROI, tagged with its template).
#'   The single most important thing to know is that `df_rois` must be genuine
#'   **hand-segmented** ground truth, because validating against ROIs that were
#'   themselves derived from the same detections ([detecs_to_rois()]) is
#'   circular and inflates every metric.
#'
#'   Exhaustiveness contract: only recordings with a KNOWN review state are
#'   validated — those that carry ROIs, or the "no signals of interest"
#'   sentinel in `df_rois`. Detections on recordings with neither state were
#'   never reviewed; they are excluded with a warning and never count as
#'   species absence (see [launch_segmentation_app()] to review recordings).
#'
#' @section Pipeline context:
#'   Step 11 of the monitoraSom analysis flow. Reads detections from
#'   [run_matching()] / [template_matching()] (steps 7/9) and ground-truth ROIs
#'   from [fetch_rois()]. Produces TP/FP + FN validation tables for
#'   [diagnostic_validations()] (step 12).
#'
#' @param df_detecs Detections: a `data.frame` in the standard layout (the
#'   layout from earlier versions is coerced), a
#'   detections `.duckdb` path, or `r lifecycle::badge("deprecated")` a CSV path.
#'   `NULL` (default) falls back to the standard database
#'   `detections/detections.duckdb` and stops if it is absent.
#' @param df_rois Ground-truth ROIs: a `data.frame` as from [fetch_rois()], or a
#'   path forwarded to [fetch_rois()]. `NULL` (default) falls back to the
#'   standard database `rois.duckdb` at the project root and stops if it is
#'   absent. Must be hand-segmented ground truth (see Details).
#' @param include_detection_to_roi Logical. Whether `signal_class =
#'   "detection_to_roi"` rows in `df_rois` are admitted as ground truth. Default
#'   `FALSE`: promoted rows are excluded from the reference set, so metrics
#'   stay comparable to a manual-only run. When `TRUE`, they are admitted
#'   and reported in the composition note. Only meaningful when `df_rois` is a
#'   signals-layout frame carrying `signal_class`; ignored otherwise.
#' @param roi_label_source Character. Which ROI label the overlap engine uses as
#'   the ground-truth species: `"effective"` (default) uses `roi_label_updated`
#'   when present, else the creation-time `roi_label`; `"original"` uses the
#'   creation-time `roi_label` only. The correction becomes the new ground
#'   truth with `"effective"`.
#' @param validation_user Character tag identifying the validator. Required, must
#'   be non-empty.
#' @param recursive Logical. Search ROI subdirectories when `df_rois` is a path.
#'   Default `FALSE`. Ignored when `df_rois` is a `data.frame`.
#' @param output_db Optional validations DuckDB database; the TP/FP rows are
#'   written there by `detection_id` (updated when they already exist), and,
#'   when `promote_to_roi = TRUE` (default), every TP is also promoted to a
#'   `signal_class = "detection_to_roi"` ROI row in the same store.
#'   `NULL` (default) persists nothing structured (FN rows always stay in
#'   `$false_negatives`).
#' @param output_path `r lifecycle::badge("deprecated")` Old CSV target. Writes
#'   the union of TP/FP + FN rows (with a deprecation warning), approximating the
#'   old wide frame. Prefer `output_db`.
#' @param source Character, ROI source when `df_rois` is a path: `"duckdb"`
#'   (default) or `"csv"`. Ignored when `df_rois` is a `data.frame`.
#' @param promote_to_roi Logical, default `TRUE`. Promote every TP detection to
#'   a ROI row (`signal_class = "detection_to_roi"`) in `output_db`, so the
#'   detection->ROI cycle closes automatically. The promoted rows are excluded
#'   from ground truth by design. Requires `output_db`.
#'
#' @return A named list of two tibbles: `detections_validated` (TP/FP, one row per
#'   detection) and `false_negatives` (one row per (template, missed ROI), tagged
#'   with `template_name` / `template_file`). Returned invisibly when persisted.
#' @seealso [run_matching()] / [template_matching()] (produce detections),
#'   [fetch_rois()] (ground truth), [diagnostic_validations()] (next step),
#'   [detecs_to_rois()].
#' @export
#' @examples
#' \donttest{
#' # Step 11: label bundled detections TP/FP against bundled ground-truth ROIs.
#' # Load the package
#' library(monitoraSom)
#' data(df_detecs)
#' data(df_rois)
#' val <- validate_by_overlap(df_detecs, df_rois, validation_user = "User")
#' class(val)    # the result is a list...
#' names(val)    # ...of two data frames:
#' head(val$detections_validated)    # $detections_validated: TP/FP per detection
#' head(val$false_negatives)         # $false_negatives: missed ROIs per template
#' }
validate_by_overlap <- function(df_detecs = NULL, df_rois = NULL,
                                validation_user = NULL,
                                recursive = FALSE, output_db = NULL,
                                output_path = NULL,
                                source = c("duckdb", "csv"),
                                include_detection_to_roi = FALSE,
                                roi_label_source = c("effective", "original"),
                                promote_to_roi = TRUE) {
  source <- match.arg(source)
  roi_label_source <- match.arg(roi_label_source)

  # CRAN item 7 (F7): NULL inputs fall back to the canonical stores
  # (detections/detections.duckdb, rois.duckdb at the project root); the path
  # branches below stop with an actionable message when a store is absent.
  # No fallback auto-creates anything.
  if (is.null(df_detecs)) df_detecs <- .monitora_db_default_path("detections")
  if (is.null(df_rois))   df_rois   <- .monitora_db_default_path("rois")

  # --- front door (VBO-07): validate before any read -------------------------
  if (is.null(validation_user) || !is.character(validation_user) ||
      length(validation_user) != 1L || !nzchar(trimws(validation_user))) {
    stop("Please identify yourself by setting a non-empty `validation_user`.")
  }
  if (!is.logical(recursive) || length(recursive) != 1L || is.na(recursive)) {
    stop("`recursive` must be a single TRUE/FALSE.")
  }
  if (!is.logical(promote_to_roi) || length(promote_to_roi) != 1L ||
      is.na(promote_to_roi)) {
    stop("`promote_to_roi` must be a single TRUE/FALSE.")
  }
  if (isTRUE(promote_to_roi) && is.null(output_db)) {
    message("`promote_to_roi = TRUE` promotes TPs only when `output_db` is ",
            "set; nothing was promoted.")
  }
  if (!is.logical(include_detection_to_roi) ||
      length(include_detection_to_roi) != 1L ||
      is.na(include_detection_to_roi)) {
    stop("`include_detection_to_roi` must be a single TRUE/FALSE.")
  }
  if (!is.null(output_db)) {
    if (!is.character(output_db) || length(output_db) != 1L ||
        !dir.exists(dirname(output_db))) {
      stop("The `output_db` directory does not exist: ",
           if (is.character(output_db)) dirname(output_db) else "(not a path)")
    }
    # FEAT-07: warn once when persisting to an unmarked explicit path.
    .require_explicit_workspace(output_db, label = "output_db",
                                caller = "validate_by_overlap")
  }
  if (!is.null(output_path)) {
    warning("`output_path` (CSV) is deprecated: persist with `output_db` ",
            "(DuckDB). Writing a legacy CSV this time.", call. = FALSE)
    if (!dir.exists(dirname(output_path))) {
      stop("The `output_path` directory does not exist: ", dirname(output_path))
    }
    .require_explicit_workspace(output_path, label = "output_path",
                                caller = "validate_by_overlap")
  }

  df_rois   <- .vbo_read_rois(df_rois, recursive, source)        # VBO-02
  df_detecs <- .vbo_read_detecs(df_detecs)                       # VBO-02
  .vbo_guard_ground_truth(df_rois)                             # R2 (F4)
  # LSA-207 runs BEFORE the promoted-rows exclusion: a recording whose only
  # ground truth is promoted rows was still reviewed (its detections stay
  # in; VBO-102 governs what counts as reference, not what was reviewed).
  df_detecs <- .vbo_guard_exhaustiveness(df_detecs, df_rois)     # LSA-207
  df_rois <- .vbo_maybe_exclude_promoted(df_rois,
                                         include_detection_to_roi) # VBO-102
  .vbo_validate_bounds(df_detecs, df_rois)                       # AFL-12

  validation_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # VBO-103 (plan §8.8): the ground-truth species source is selectable.
  if (identical(roi_label_source, "effective") &&
      "roi_label_updated" %in% names(df_rois)) {
    df_rois$roi_label <- .signal_effective_label(
      df_rois$roi_label, df_rois$roi_label_updated)
  }

  res <- .vbo_overlap_engine(df_detecs, df_rois, validation_user,
                             validation_time)                    # VBO-06/100/101

  # --- persistence -----------------------------------------------------------
  # F4 cutover (signals program, D5): validations persist into the unified
  # signals store as val_* on the detection rows (signal_id == detection_id),
  # replacing the deprecated validations table (store frozen in F5).
  if (!is.null(output_db)) {                                     # VBO-05 (TP/FP)
    con <- .signals_duckdb_connect(output_db)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    .signals_duckdb_upsert(con, .validations_as_signals(res$detections_validated),
                           replace = TRUE)
    # DEC-4 (STEP-5): automatic promotion of validated TPs to ROIs, reusing the
    # segmentation app's promotion path (.promote_detection_row). The promoted
    # rows carry signal_class = "detection_to_roi" and never qualify as ground
    # truth (origin = template_matching), so metrics stay clean.
    if (isTRUE(promote_to_roi)) {
      tps <- res$detections_validated[
        !is.na(res$detections_validated$validation) &
          res$detections_validated$validation == "TP", , drop = FALSE]
      if (nrow(tps) > 0L) {
        stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        promoted <- lapply(seq_len(nrow(tps)), function(i) {
          .promote_detection_row(tps[i, , drop = FALSE],
                                 user = validation_user, timestamp = stamp)
        })
        .signals_duckdb_upsert(con, dplyr::bind_rows(promoted),
                               replace = FALSE)
        message(nrow(tps), " TP detection(s) promoted to ROIs in the signals ",
                "store: ", output_db)
      }
    }
    message("TP/FP validations upserted to the signals store: ", output_db,
            " (FN rows kept in $false_negatives only)")
  }
  if (!is.null(output_path)) {
    legacy <- dplyr::bind_rows(res$detections_validated, res$false_negatives)
    utils::write.csv(legacy, output_path, row.names = FALSE, fileEncoding = "UTF-8")
    message("Validation results exported to the deprecated CSV: ", output_path)
  }
  if (is.null(output_db) && is.null(output_path)) {
    message("Validation results have been returned to the R session")
  }

  if (!is.null(output_db) || !is.null(output_path)) invisible(res) else res
}

# --- input intake (VBO-02) ----------------------------------------------------

# AFL-12: the overlap predicate uses dplyr::between(x, lo, hi), which is undefined
# when lo > hi. Reject inverted time/frequency intervals up front with a clear
# error rather than producing silently wrong TP/FP/FN. NA bounds are left to the
# downstream logic (they are not inverted).
.vbo_validate_bounds <- function(df_detecs, df_rois) {
  inverted <- function(df, lo, hi) {
    if (!all(c(lo, hi) %in% names(df))) return(0L)
    sum(!is.na(df[[lo]]) & !is.na(df[[hi]]) & df[[lo]] > df[[hi]])
  }
  checks <- list(
    c("df_detecs", "detection_start", "detection_end"),
    c("df_rois",   "roi_start",       "roi_end"),
    c("df_rois",   "roi_min_freq",    "roi_max_freq"))
  for (ch in checks) {
    df <- if (ch[[1]] == "df_detecs") df_detecs else df_rois
    n <- inverted(df, ch[[2]], ch[[3]])
    if (n > 0) {
      stop(sprintf("validate_by_overlap: %d row(s) with an inverted interval (%s > %s).",
                   n, ch[[2]], ch[[3]]))
    }
  }
  invisible(TRUE)
}

.vbo_read_rois <- function(df_rois, recursive, source) {
  if (is.data.frame(df_rois)) return(df_rois)
  if (is.character(df_rois) && length(df_rois) == 1L) {
    if (!file.exists(df_rois)) {
      stop("There is no file at the provided path to `df_rois`: ", df_rois)
    }
    return(fetch_rois(df_rois, recursive = recursive, source = source))
  }
  stop("`df_rois` must be a data.frame or a path to a ROI store.")
}

# R2 ground-truth guard (F4 of the signals program, spec §ground truth): the
# reference frame must not contain the thing being validated. Unified frames:
# signal_class == "detection" rows are a hard error (raw matching output is
# never ground truth); "detection_to_roi" rows are accepted (maintainer
# decision: promote only detections satisfying the manual-ROI assumptions) and
# reported so a metric shift stays explicable (VBO-102). Legacy 19-col frames
# keep the provenance-based check (.is_detection_roi, AFL-02).
.vbo_guard_ground_truth <- function(df_rois) {
  if ("signal_class" %in% names(df_rois)) {
    n_det <- sum(df_rois$signal_class == "detection", na.rm = TRUE)
    if (n_det > 0L) {
      stop("Circular validation: `df_rois` contains ", n_det,
           " signal_class = \"detection\" row(s). Raw matching output is what ",
           "is being validated, never the reference. Pass manual ROIs ",
           "(fetch_rois) or promoted rows (signal_class = \"detection_to_roi\") ",
           "as ground truth.")
    }
    n_prom <- sum(df_rois$signal_class == "detection_to_roi", na.rm = TRUE)
    if (n_prom > 0L) {
      message(sprintf(
        "Ground truth includes %d promoted detection_to_roi row(s) (of %d): ",
        n_prom, nrow(df_rois)),
        "metrics reflect the promoted reference set (VBO-102).")
    }
    return(invisible(FALSE))
  }
  if (any(.is_detection_roi(df_rois))) {
    stop("Circular validation: `df_rois` contains detection-derived rows ",
         "(roi_source/roi_type = \"detection\"). Pass manual ROIs (fetch_rois) ",
         "as ground truth, never detecs_to_rois() output.")
  }
  invisible(FALSE)
}

# LSA-207 exhaustiveness contract: a recording may join validation only when
# its review state is KNOWN -- `segmented` (carries ROIs) or `no_soi`
# (reviewed, nothing found; the sentinel row reaches `df_rois` because
# fetch_rois keeps it). Detections on recordings with neither state were
# NEVER reviewed: counting them as false positives would read "not
# validated" as "validated as absent". They are excluded with a warning
# naming the remedy.
.vbo_guard_exhaustiveness <- function(df_detecs, df_rois) {
  reviewed  <- unique(.normalize_path_key(df_rois$soundscape_path))
  det_paths <- unique(.normalize_path_key(df_detecs$soundscape_path))
  unreviewed <- setdiff(det_paths[!is.na(det_paths)], reviewed)
  if (length(unreviewed) > 0L) {
    warning(sprintf(
      paste0("LSA-207: %d of %d detection recording(s) were never reviewed ",
             "(no ROI and no 'no signals of interest' sentinel in the ground ",
             "truth). Their detections are EXCLUDED from validation ",
             "(first: '%s'). Review them in the segmentation app, or mark ",
             "them 'no signals of interest', to include them."),
      length(unreviewed), length(det_paths), unreviewed[1]),
      call. = FALSE)
    keep <- .normalize_path_key(df_detecs$soundscape_path) %in% reviewed
    return(df_detecs[keep, , drop = FALSE])
  }
  df_detecs
}

# VBO-102: drop promoted ground-truth rows unless they are explicitly admitted.
# The guard has already run, so a `signal_class` frame is a signals-layout one
# and any remaining promoted rows are candidates. Excluding them keeps the
# metrics comparable to a manual-only run; an all-promoted reference set that
# would leave no ROIs errors instead of silently validating against nothing.
.vbo_maybe_exclude_promoted <- function(df_rois, include) {
  if (include || !("signal_class" %in% names(df_rois))) return(df_rois)
  is_prom <- !is.na(df_rois$signal_class) &
    df_rois$signal_class == "detection_to_roi"
  n_prom <- sum(is_prom)
  if (n_prom == 0L) return(df_rois)
  keep <- df_rois[!is_prom, , drop = FALSE]
  if (nrow(keep) == 0L) {
    stop("No ground-truth ROIs remain after excluding detection_to_roi rows ",
         "(include_detection_to_roi = FALSE). Set it to TRUE to admit them.")
  }
  message("Excluding ", n_prom, " detection_to_roi row(s) from the reference ",
          "set (include_detection_to_roi = FALSE, VBO-102).")
  keep
}

.vbo_read_detecs <- function(df_detecs) {
  if (is.data.frame(df_detecs)) return(df_detecs)
  if (!is.character(df_detecs) || length(df_detecs) != 1L) {
    stop("`df_detecs` must be a data.frame, a detections `.duckdb`, or a CSV path.")
  }
  if (!file.exists(df_detecs)) {
    stop("There is no file at the provided path to `df_detecs`: ", df_detecs)
  }
  if (grepl("(?i)\\.duckdb$", df_detecs)) {
    # F6 (signals program): detections live in the signals store now.
    con <- .signals_duckdb_connect(df_detecs)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    return(.signals_as_detections(.signals_duckdb_read(
      con, signal_class = .SIGNAL_CLASS_DETECTION)))
  }
  warning("Reading detections from a CSV is deprecated; prefer a detections ",
          "`.duckdb` store.", call. = FALSE)
  utils::read.csv(df_detecs, stringsAsFactors = FALSE)
}

# AFL-32: the TP note value carries a deliberate misspelling ("instersection").
# It is a FROZEN data value — asserted in the goldens and reproduced verbatim by
# the Julia port (VBO-08) — so it must NOT be "corrected" here. Centralised in a
# named constant so a reader sees the typo is intentional, not an accident.
.VBO_NOTE_TP <- "instersection with a ROI"   # sic (VBO-08)

# Detection species: last `_`-token of `template_name` after a case-insensitive
# `.wav` strip (VBO-03; behaviourally identical to the original's
# `gsub(".*_", "", gsub("\\.wav|.WAV", "", .))` on the fixtures).
.vbo_species <- function(template_name) {
  gsub(".*_", "", sub("(?i)\\.wav$", "", template_name))
}

# AFL-04: derive the detection species preferring the dedicated `template_label`
# (template database source of truth) and falling back to the fragile last-`_`-token
# parse of `template_name` only for legacy inputs without a label. A bad species
# silently breaks the (soundscape, species) join -> wrong TP/FP/FN.
.vbo_species_from <- function(df) {
  parsed <- .vbo_species(df$template_name)
  if ("template_label" %in% names(df)) {
    tl <- df$template_label
    return(ifelse(!is.na(tl) & nzchar(tl), tl, parsed))
  }
  parsed
}

# Durable detection id for the validations store (VBO-04). Uses the canonical
# `detection_id` when the input carries it; otherwise builds one via
# [.detection_id()], substituting `template_file` for the missing `template_id`
# so the six legacy templates of one species do not collide on `(soundscape,
# peak_index)`.
.vbo_detection_id <- function(df) {
  if ("detection_id" %in% names(df) && !all(is.na(df$detection_id))) {
    return(as.character(df$detection_id))
  }
  tid <- if ("template_id" %in% names(df) && !all(is.na(df$template_id))) {
    df$template_id
  } else if ("template_file" %in% names(df)) {
    df$template_file
  } else {
    rep(NA_character_, nrow(df))
  }
  sm <- if ("score_method" %in% names(df)) df$score_method else rep(NA_character_, nrow(df))
  pk <- if ("peak_index" %in% names(df)) df$peak_index else seq_len(nrow(df))
  .detection_id(df$soundscape_path, tid, pk, sm)
}

# --- overlap engine (VBO-06 predicate faithful; per-template FN; split shape) --
#
# Detection-centric TP/FP + per-template FN, returned as two tidy tibbles.
#   - Overlap predicate unchanged (VBO-06, verified correct).
#   - FN counted PER TEMPLATE (VBO-100 reverted 2026-06-13): an ROI is FN for
#     template t when no detection of t overlaps it (the diagnostics consume FN
#     per template). Counts match the original per template.
#   - Unmatched-species detections -> FP, not dropped (VBO-100 #2, kept).
#   - Split return (VBO-101): $detections_validated + $false_negatives.
.vbo_overlap_engine <- function(df_detecs, df_rois, validation_user,
                                validation_time) {
  rois <- df_rois
  rois$soundscape_path <- .normalize_path_key(rois$soundscape_path)   # AFL-06
  rois$species  <- rois$roi_label
  rois$roi_uid  <- seq_len(nrow(rois))         # within-run ROI key

  detecs <- df_detecs
  detecs$soundscape_path <- .normalize_path_key(detecs$soundscape_path)  # AFL-06
  detecs$species <- .vbo_species_from(detecs)                       # AFL-04
  detecs$det_uid <- seq_len(nrow(detecs))      # within-run detection key

  roi_species <- unique(rois$species)
  det_species <- unique(detecs$species)

  # Species-availability messaging. Unmatched-species detections are NO LONGER
  # dropped (VBO-100 #2) — they fall through to an FP label below.
  if (all(det_species %in% roi_species)) {
    message("All detected species have ROIs for validation")
  } else if (any(det_species %in% roi_species)) {
    warning("The following detected species have no ROIs for validation: ",
            paste(setdiff(det_species, roi_species), collapse = ", "),
            "; their detections are labelled FP (VBO-100 #2).")
  } else {
    stop("There are no ROIs of any detected species")
  }

  # Crossing relation: detection x roi sharing (soundscape_file, species),
  # many-to-many (a detection may cross several ROIs and vice-versa). Carries
  # `template_file` so FN can be attributed PER TEMPLATE.
  cross <- dplyr::inner_join(
    detecs[, c("det_uid", "template_file", "soundscape_file", "species",
               "detection_start", "detection_end")],
    rois[, c("roi_uid", "soundscape_file", "species", "roi_start", "roi_end")],
    by = c("soundscape_file", "species"), relationship = "many-to-many"
  )
  cross$overlap <-
    dplyr::between(cross$detection_start, cross$roi_start, cross$roi_end) |
    dplyr::between(cross$detection_end,   cross$roi_start, cross$roi_end) |
    (cross$detection_start <= cross$roi_start &
       cross$detection_end >= cross$roi_end)

  crossed_uids    <- unique(cross$det_uid)
  overlapped_uids <- unique(cross$det_uid[cross$overlap])

  # Per-detection classification (one verdict per detection, no dedup needed).
  has_sp  <- detecs$species %in% roi_species
  is_tp   <- detecs$det_uid %in% overlapped_uids
  crossed <- detecs$det_uid %in% crossed_uids

  detecs$validation <- ifelse(is_tp, "TP", "FP")
  note <- character(nrow(detecs))
  note[is_tp]                          <- .VBO_NOTE_TP                  # VBO-08/AFL-32
  note[!is_tp & !has_sp]               <- "no ROIs of this species"    # VBO-100 #2
  note[!is_tp & has_sp & !crossed]     <- "no ROIs to intersect with"
  note[!is_tp & has_sp &  crossed]     <- "no intersection with a ROI"
  detecs$validation_note <- note
  detecs$validation_user <- validation_user
  detecs$validation_time <- validation_time
  detecs$detection_id    <- .vbo_detection_id(detecs)

  detections_validated <- .coerce_validations(detecs)
  detections_validated <- detections_validated[
    order(detections_validated$soundscape_file,
          detections_validated$detection_start), , drop = FALSE]

  false_negatives <- .vbo_per_template_fn(detecs, rois, cross,
                                          validation_user, validation_time)

  list(detections_validated = tibble::as_tibble(detections_validated),
       false_negatives       = false_negatives)
}

# Per-template FN (VBO-100 reverted): for each template present in the
# detections, the ROIs of its species that NO detection of THAT template
# overlapped. Each FN row is the canonical ROI schema + the template tag
# (`template_name`/`template_file`) + the 4 validation columns, so the
# per-template diagnostics wrapper can split FN by `template_name`. Templates of
# a species with no ground-truth ROIs contribute nothing (no FN without truth).
.vbo_per_template_fn <- function(detecs, rois, cross, validation_user,
                                 validation_time) {
  roi_species <- unique(rois$species)
  tmpl <- detecs[!duplicated(detecs$template_file),
                 c("template_file", "template_name", "species"), drop = FALSE]
  parts <- lapply(seq_len(nrow(tmpl)), function(i) {
    tf <- tmpl$template_file[i]
    sp <- tmpl$species[i]
    if (!(sp %in% roi_species)) return(NULL)            # orphan species: no FN
    matched <- unique(cross$roi_uid[cross$overlap & cross$template_file == tf])
    miss <- rois[rois$species == sp & !(rois$roi_uid %in% matched), ,
                 drop = FALSE]
    if (nrow(miss) == 0L) return(NULL)
    out <- .coerce_rois(miss)
    out$template_name <- rep(tmpl$template_name[i], nrow(out))
    out$template_file <- rep(tf, nrow(out))
    out
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0L) {
    fn <- .coerce_rois(rois[0, , drop = FALSE])
    fn$template_name <- character(0)
    fn$template_file <- character(0)
  } else {
    fn <- do.call(rbind, parts)
    fn <- fn[order(fn$template_name, fn$soundscape_file, fn$roi_start), ,
             drop = FALSE]
  }
  n <- nrow(fn)
  fn$validation_user <- rep(validation_user, n)
  fn$validation_time <- rep(validation_time, n)
  fn$validation      <- rep("FN", n)
  fn$validation_note <- rep("no detections to intersect with", n)
  tibble::as_tibble(fn)
}
