# Tests for the refactored validate_by_overlap (detections vs ground-truth ROIs,
# flow step #13; VBO cycle, autonomous push plan-log-2026-06-11-002 + the
# 2026-06-13 user gate and its clarification). The golden
# (R/sandbox/validate_by_overlap.R) froze the ORIGINAL wide-frame behaviour. The
# refactor keeps the original TP/FP/FN counts (FN per template — the species-level
# attempt was reverted as inconsistent with the per-template diagnostics) but
# changes the return SHAPE and the orphan-species label:
#   VBO-101  split return: $detections_validated (TP/FP, detection-keyed) +
#            $false_negatives (per (template, missed ROI), tagged with the
#            template_name so the diagnostics wrapper can split it).
#   VBO-100  the one kept change: detections of a species with NO ground-truth
#            ROIs -> FP "no ROIs of this species" (was silently dropped). FN
#            stays per template (counts == original).
#   VBO-01/02/03/04/05/06/07/08 supporting items.
#
# Run: Rscript -e "testthat::test_file('R/refactored/tests/testthat/test-validate_by_overlap.R')"

suppressPackageStartupMessages({
  library(tibble); library(DBI); library(duckdb)
})
gold <- test_path("golden/validate_by_overlap")
det  <- readRDS(file.path(gold, "input_detecs.rds"))   # 1090 legacy detections
roi  <- readRDS(file.path(gold, "input_rois.rds"))     # 30 ROIs, 6 templates / 1 species

# ORIGINAL baseline frozen in the golden (wide frame): TP=157 FP=933 FN=108
# (FN per template: 6 templates x 18 missed ROIs).
ORIG_TP <- 157L; ORIG_FP <- 933L; ORIG_FN <- 108L; ORIG_N_TEMPLATES <- 6L

run_quiet <- function(...) suppressMessages(suppressWarnings(validate_by_overlap(...)))

# --- VBO-101: split return shape ---------------------------------------------

test_that("returns a named list of two tibbles (VBO-101)", {
  res <- run_quiet(det, roi, validation_user = "Tester")
  expect_type(res, "list")
  expect_named(res, c("detections_validated", "false_negatives"))
  expect_s3_class(res$detections_validated, "tbl_df")
  expect_s3_class(res$false_negatives, "tbl_df")
  # detection-keyed validations frame == canonical validations schema (24 + 4).
  expect_identical(names(res$detections_validated), names(.validation_schema_spec()))
  # roi-keyed FN frame == canonical ROI schema + the template tag + 4 validation.
  expect_identical(
    names(res$false_negatives),
    c(names(.roi_schema_spec()), "template_name", "template_file",
      "validation_user", "validation_time", "validation", "validation_note"))
})

# --- counts match the original per template (FN per template, VBO-100 reverted) -

test_that("detections_validated is one TP/FP row per detection (VBO-04 id, VBO-08 note)", {
  res <- run_quiet(det, roi, validation_user = "Tester")
  dv <- res$detections_validated
  expect_equal(nrow(dv), nrow(det))                       # 1090, no dedup loss
  expect_setequal(unique(dv$validation), c("TP", "FP"))
  expect_false(any(dv$validation == "FN"))                # FN never here
  expect_equal(length(unique(dv$detection_id)), nrow(dv)) # VBO-04: durable, no collision
  # VBO-08: the TP note keeps the "instersection" misspelling verbatim (frozen
  # data value, downstream string-matched).
  expect_true(all(dv$validation_note[dv$validation == "TP"] ==
                    "instersection with a ROI"))
  expect_equal(sum(dv$validation == "TP"), ORIG_TP)
  expect_equal(sum(dv$validation == "FP"), ORIG_FP)
})

test_that("FN is counted per template and matches the original total", {
  res <- run_quiet(det, roi, validation_user = "Tester")
  fn <- res$false_negatives
  expect_true(all(fn$validation == "FN"))
  expect_true(all(fn$validation_note == "no detections to intersect with"))
  # Per-template FN: each template tags its own missed ROIs. The total equals the
  # original golden FN (6 templates x 18 missed = 108) — the diagnostics consume
  # FN per template, so the per-template count is the correct unit.
  expect_equal(nrow(fn), ORIG_FN)
  expect_equal(length(unique(fn$template_name)), ORIG_N_TEMPLATES)
  per_tpl <- table(fn$template_name)
  expect_true(all(per_tpl == 18L))
  # Every FN row carries its template tag (so the wrapper can split by template).
  expect_false(any(is.na(fn$template_name)))
})

# --- VBO-100 #2: unmatched-species detections become FP, not dropped ----------

test_that("detections of a species with no ROIs are labelled FP, not dropped", {
  det_orphan <- det[1:5, , drop = FALSE]
  det_orphan$template_name <- "X_Nonexistent species"
  det2 <- rbind(det, det_orphan)
  expect_warning(
    res <- suppressMessages(validate_by_overlap(det2, roi, validation_user = "T")),
    "no ROIs for validation")
  dv <- res$detections_validated
  expect_equal(nrow(dv), nrow(det2))                      # nothing dropped
  orphans <- dv[dv$validation_note == "no ROIs of this species", ]
  expect_equal(nrow(orphans), 5L)
  expect_true(all(orphans$validation == "FP"))
})

# --- AFL-04: species join uses template_label, not the fragile name parse -----

test_that("AFL-04: template_label restores the species join (VBO-03)", {
  # Break the last-`_`-token parse (e.g. the refactored cut naming, where the
  # last token is the template_id hash, not the species).
  det_broken <- det
  det_broken$template_name <- sub("_[^_]*\\.wav$", "_deadbeef00.wav",
                                  det$template_name, ignore.case = TRUE)
  # Without a label the parsed species matches no ROI at all -> hard stop today.
  expect_error(
    suppressMessages(validate_by_overlap(det_broken, roi, validation_user = "T")),
    "no ROIs of any detected species")
  # The dedicated label is the source of truth -> the join (and the original
  # TP/FP counts) are restored despite the broken name.
  det_lbl <- det_broken
  det_lbl$template_label <- "Basileuterus culicivorus"
  res <- run_quiet(det_lbl, roi, validation_user = "T")
  expect_equal(sum(res$detections_validated$validation == "TP"), ORIG_TP)
  expect_equal(sum(res$detections_validated$validation == "FP"), ORIG_FP)
})

# --- VBO-06: the overlap predicate is correct (hand-built boundaries) ---------

test_that("the overlap predicate classifies TP/FP/FN on hand-built intervals", {
  mk_d <- function(ds, de) {
    x <- det[1, , drop = FALSE]
    x$detection_start <- ds; x$detection_end <- de
    x$template_name <- "t_SpZ"
    x$soundscape_file <- "S1.wav"; x$soundscape_path <- "a/S1.wav"; x
  }
  mk_r <- function(rs, re) {
    y <- roi[1, , drop = FALSE]
    y$roi_start <- rs; y$roi_end <- re; y$roi_label <- "SpZ"
    y$soundscape_file <- "S1.wav"; y$soundscape_path <- "a/S1.wav"; y
  }
  dd <- rbind(mk_d(10, 12),   # inside roi [5,15]      -> TP
              mk_d(4, 6),     # left-partial of [5,15] -> TP
              mk_d(50, 52))   # disjoint from any roi  -> FP
  rr <- rbind(mk_r(5, 15),    # overlapped             -> not FN
              mk_r(80, 90))   # never overlapped       -> FN
  res <- run_quiet(dd, rr, validation_user = "T")
  expect_equal(sum(res$detections_validated$validation == "TP"), 2L)
  expect_equal(sum(res$detections_validated$validation == "FP"), 1L)
  expect_equal(nrow(res$false_negatives), 1L)
  expect_equal(res$false_negatives$roi_start, 80)
})

test_that("a detection containing the ROI counts as TP (containment term)", {
  # The predicate has three terms: detection-start inside, detection-end
  # inside, OR the ROI fully contained in the detection. Exercise the third.
  d <- det[1, , drop = FALSE]
  d$detection_start <- 0; d$detection_end <- 100   # contains roi [5,15]
  d$template_name <- "Bcu_1_000.000-100.000s_Basileuterus culicivorus.wav"
  d$template_file <- "t1"
  d$soundscape_file <- "S1.wav"; d$soundscape_path <- "a/S1.wav"; d
  r <- roi[1, , drop = FALSE]
  r$roi_start <- 5; r$roi_end <- 15; r$roi_label <- "Basileuterus culicivorus"
  r$soundscape_file <- "S1.wav"; r$soundscape_path <- "a/S1.wav"; r
  res <- run_quiet(d, r, validation_user = "T")
  expect_equal(res$detections_validated$validation, "TP")
  expect_equal(nrow(res$false_negatives), 0L)
})

test_that("FN is per-template: two templates sharing ROIs attribute correctly", {
  # A detection of t1 overlaps ROI X; the same ROI X is also a potential FN
  # for t2 (same species). Per-template FN must NOT count X for t2.
  roi_a <- roi[1, , drop = FALSE]
  roi_a$roi_start <- 10; roi_a$roi_end <- 20; roi_a$roi_label <- "Basileuterus culicivorus"
  roi_a$soundscape_file <- "S1.wav"; roi_a$soundscape_path <- "a/S1.wav"; roi_a
  det1 <- det[1, , drop = FALSE]
  det1$detection_start <- 12; det1$detection_end <- 14
  det1$template_name <- "Bcu_1_012.000-014.000s_Basileuterus culicivorus.wav"
  det1$template_file <- "t1"
  det1$soundscape_file <- "S1.wav"; det1$soundscape_path <- "a/S1.wav"; det1
  det2 <- det[1, , drop = FALSE]
  det2$detection_start <- 12; det2$detection_end <- 14
  det2$template_name <- "Bcu_1_012.000-014.000s_Basileuterus culicivorus.wav"
  det2$template_file <- "t2"
  det2$soundscape_file <- "S1.wav"; det2$soundscape_path <- "a/S1.wav"; det2
  # Add a second ROI Y that only t1 overlaps: Y must be FN for t2 only.
  roi_b <- roi[1, , drop = FALSE]
  roi_b$roi_start <- 30; roi_b$roi_end <- 40; roi_b$roi_label <- "Basileuterus culicivorus"
  roi_b$soundscape_file <- "S1.wav"; roi_b$soundscape_path <- "a/S1.wav"; roi_b
  det1b <- det1; det1b$detection_start <- 32; det1b$detection_end <- 34
  res <- run_quiet(rbind(det1, det1b, det2), rbind(roi_a, roi_b),
                   validation_user = "T")
  fn <- res$false_negatives
  # Y (30-40) is missed by t2 only -> one FN row for t2.
  expect_equal(nrow(fn), 1L)
  expect_true(all(fn$template_file == "t2"))
  expect_equal(fn$roi_start, 30)
})

# --- VBO-05: DuckDB persistence (TP/FP only, FN excluded) ---------------------

test_that("output_db upserts TP/FP rows only and returns invisibly (VBO-05)", {
  store <- file.path(tempdir(), sprintf("vbo_%d.duckdb", as.integer(runif(1, 1, 1e7))))
  on.exit(unlink(store), add = TRUE)
  res <- run_quiet(det, roi, validation_user = "Tester", output_db = store)
  expect_type(res, "list")                                # still the split list
  con <- .signals_duckdb_connect(store)                   # F4: signals store
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_equal(.signals_duckdb_count(con, signal_class = "detection"),
               nrow(det))                                  # 1090 TP/FP
  stored <- .signals_as_validations(.signals_duckdb_read(con))
  expect_false(any(stored$validation == "FN", na.rm = TRUE))
  # Re-validating replaces rather than duplicating (upsert by signal_id).
  run_quiet(det, roi, validation_user = "Tester", output_db = store)
  expect_equal(.signals_duckdb_count(con, signal_class = "detection"), nrow(det))
})

# --- VBO-05: deprecated CSV writes the TP/FP + FN union -----------------------

test_that("deprecated output_path warns and writes the TP/FP + FN union", {
  csv <- file.path(tempdir(), "vbo_legacy.csv")
  on.exit(unlink(csv), add = TRUE)
  expect_warning(
    suppressMessages(validate_by_overlap(det, roi, validation_user = "T",
                                         output_path = csv)),
    "deprecated")
  expect_true(file.exists(csv))
  expect_equal(nrow(utils::read.csv(csv)), nrow(det) + ORIG_FN)  # 1090 + 108 FN
})

test_that("VBO-02: a legacy detections CSV input still works (deprecated)", {
  csv <- file.path(tempdir(), "vbo_legacy_input.csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(det, csv, row.names = FALSE)
  expect_warning(
    res <- suppressMessages(validate_by_overlap(csv, roi,
                                                validation_user = "Tester")),
    "CSV is deprecated")
  dv <- res$detections_validated
  expect_equal(sum(dv$validation == "TP"), ORIG_TP)
  expect_equal(sum(dv$validation == "FP"), ORIG_FP)
})

# --- VBO-07: front door -------------------------------------------------------

test_that("front door rejects a missing/empty validation_user", {
  expect_error(validate_by_overlap(det, roi), "identify yourself")
  expect_error(validate_by_overlap(det, roi, validation_user = "  "),
               "identify yourself")
})

# --- AFL-12: inverted intervals are rejected before the overlap predicate ----
test_that("inverted time/frequency intervals are rejected (AFL-12)", {
  det_inv <- det[1, , drop = FALSE]
  det_inv$detection_start <- 50; det_inv$detection_end <- 40
  expect_error(
    suppressWarnings(validate_by_overlap(det_inv, roi, validation_user = "T")),
    "inverted interval \\(detection_start > detection_end\\)")
  roi_inv <- roi[1, , drop = FALSE]
  roi_inv$roi_start <- 90; roi_inv$roi_end <- 80
  expect_error(
    suppressWarnings(validate_by_overlap(det, roi_inv, validation_user = "T")),
    "inverted interval \\(roi_start > roi_end\\)")
  roi_f <- roi[1, , drop = FALSE]
  roi_f$roi_min_freq <- 8; roi_f$roi_max_freq <- 2
  expect_error(
    suppressWarnings(validate_by_overlap(det, roi_f, validation_user = "T")),
    "inverted interval \\(roi_min_freq > roi_max_freq\\)")
})

test_that("an all-unmatched-species input still errors (faithful guard)", {
  det_bad <- det[1:3, , drop = FALSE]
  det_bad$template_name <- "x_Totally unknown"
  expect_error(suppressWarnings(
    validate_by_overlap(det_bad, roi, validation_user = "T")),
    "no ROIs of any detected species")
})

# --- R2 ground-truth guard (F4 of the signals program, spec §ground truth) ----

test_that("R2: a reference frame containing signal_class = 'detection' rows errors", {
  bad <- roi
  bad$signal_class <- "detection"
  expect_error(run_quiet(det, bad, validation_user = "T"),
               "Circular validation")
  expect_error(run_quiet(det, bad, validation_user = "T"),
               "signal_class = \"detection\"")
})

test_that("R2: detection_to_roi ground truth is accepted with a composition note", {
  gtr <- roi
  gtr$signal_class <- "roi"
  gtr$signal_class[1] <- "detection_to_roi"
  res <- expect_message(
    suppressWarnings(validate_by_overlap(det, gtr, validation_user = "T")),
    "detection_to_roi")
  expect_type(res, "list")                    # engine ran to completion
})

test_that("R2: legacy detection-derived ROI frames are rejected (circular)", {
  bad <- roi
  bad$roi_source <- "detection"
  expect_error(run_quiet(det, bad, validation_user = "T"),
               "Circular validation")
  expect_error(run_quiet(det, bad, validation_user = "T"),
               "detecs_to_rois")
})

# --- VBO-102: include/exclude promoted ground truth ---------------------------

test_that("VBO-102: include_detection_to_roi must be a single TRUE/FALSE", {
  expect_error(validate_by_overlap(det, roi, validation_user = "T",
                                   include_detection_to_roi = NA),
               "single TRUE/FALSE")
  expect_error(validate_by_overlap(det, roi, validation_user = "T",
                                   include_detection_to_roi = "yes"),
               "single TRUE/FALSE")
})

test_that("VBO-102: default excludes detection_to_roi rows from ground truth", {
  gtr <- roi
  gtr$signal_class <- "roi"
  gtr$signal_class[1] <- "detection_to_roi"
  # The first ROI is promoted: excluding it removes that ROI from the reference
  # set, so its species no longer yields an FN for it.
  expect_message(
    suppressWarnings(validate_by_overlap(det, gtr, validation_user = "T")),
    "Excluding 1 detection_to_roi row")
})

test_that("VBO-102: include_detection_to_roi = TRUE admits promoted rows", {
  gtr <- roi
  gtr$signal_class <- "roi"
  gtr$signal_class[1] <- "detection_to_roi"
  res <- expect_message(
    suppressWarnings(validate_by_overlap(
      det, gtr, validation_user = "T", include_detection_to_roi = TRUE)),
    "Ground truth includes 1 promoted")
  expect_type(res, "list")                       # engine ran to completion
})

test_that("VBO-102: all-promoted reference errors when excluded", {
  gtr <- roi
  gtr$signal_class <- "detection_to_roi"
  expect_error(
    suppressWarnings(validate_by_overlap(det, gtr, validation_user = "T")),
    "No ground-truth ROIs remain after excluding")
})
