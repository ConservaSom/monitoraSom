# Tests for the refactored detecs_to_rois (detections -> ROIs, flow step #11;
# DTR cycle, autonomous push plan-log-2026-06-11-002). The goldens
# (R/sandbox/detecs_to_rois.R) froze the ORIGINAL behaviour; the refactor is
# asserted against them as golden-equality on the unchanged mapping plus DERIVED
# divergences per tests/golden/detecs_to_rois/NOTES.md: DTR-01 (schema-driven
# input + DuckDB intake), DTR-02 (per-row roi_label), DTR-03 (canonical ROI
# DuckDB store + C7 separation), DTR-04 (canonical 18-col schema), DTR-05
# (case-insensitive .wav strip), DTR-06 (front door), DTR-07 (single timestamp).
#
# Run: Rscript -e "testthat::test_file('R/refactored/tests/testthat/test-detecs_to_rois.R')"

suppressPackageStartupMessages({
  library(tibble); library(DBI); library(duckdb)
})
# F5: detecs_to_rois is deprecated — silence its warning everywhere except the
# dedicated deprecation test (which opts back in via a local option).
options(lifecycle_verbosity = "quiet")
gold <- test_path("golden/detecs_to_rois")
g1 <- readRDS(file.path(gold, "G1_rois_inmem.rds"))             # frozen, 16-col
g2 <- readRDS(file.path(gold, "G2_rois_filter_tp.rds"))
g3 <- readRDS(file.path(gold, "G3_rois_no_validation.rds"))
in_val    <- readRDS(file.path(gold, "input_subset_validated.rds"))
in_legacy <- readRDS(file.path(gold, "input_subset_legacy.rds"))
in_2lab   <- readRDS(file.path(gold, "input_two_labels.rds"))

# Mapped value columns shared by ORIGINAL and refactored output (everything
# except the canonical-schema changes roi_channel/roi_file and the C9 time
# fields). roi_label is excluded here — it is the DTR-02 divergence, asserted
# separately.
shared_cols <- c("soundscape_file", "roi_start", "roi_end", "roi_min_freq",
                 "roi_max_freq", "roi_type", "roi_label_confidence",
                 "roi_wl", "roi_ovlp", "roi_sample_rate")

cmp <- function(ref, gold_df, cols = shared_cols) {
  for (c in cols) {
    expect_equal(unname(ref[[c]]), unname(gold_df[[c]]),
                 info = paste("column", c))
  }
}

test_that("in-memory mapping reproduces the golden value columns (G1)", {
  r <- detecs_to_rois(in_val, username = "Tester")
  expect_s3_class(r, "data.frame")
  expect_equal(nrow(r), nrow(g1))
  cmp(r, g1)
  # F6 (signals program, SIG-03): roi_comment is no longer the AFL-22 key=value
  # payload — structured metadata lives in det_*; the legacy shape carries NA.
  expect_true(all(is.na(r$roi_comment)))
  expect_true(all(nzchar(g1$roi_comment)))        # the frozen original HAD the payload
  expect_true(all(r$roi_type == "detection"))
  expect_equal(r$roi_pitch_shift, rep(1L, nrow(r)))
})

test_that("output is the canonical 18-col ROI schema (DTR-04)", {
  r <- detecs_to_rois(in_val, username = "Tester")
  expect_identical(names(r), names(.roi_schema_spec()))
  expect_true("roi_channel" %in% names(r))     # added by the canonical schema
  expect_false("roi_file" %in% names(r))        # dropped from the in-memory frame
  expect_type(r$roi_pitch_shift, "integer")
})

test_that("roi_label is parsed PER ROW (DTR-02 fix)", {
  # The ORIGINAL gave every row the first row's label (golden G6 = 1 unique).
  r <- detecs_to_rois(in_2lab, username = "Tester")
  expect_setequal(unique(r$roi_label),
                  c("Basileuterus culicivorus", "Myiothlypis flaveola"))
  # Row-aligned to the input's per-row template_name.
  expected <- sub("(?i)\\.wav$", "", in_2lab$template_name)
  expected <- vapply(strsplit(expected, "_"), function(p) p[[length(p)]],
                     character(1))
  expect_equal(r$roi_label, expected)
})

test_that("case-insensitive .wav strip in roi_label (DTR-05 fix)", {
  d <- in_legacy[1, , drop = FALSE]
  d$template_name <- "Foo_bar_Some species.WAV"
  r <- detecs_to_rois(d, username = "Tester")
  expect_equal(r$roi_label, "Some species")    # not "Some species.WAV"
})

test_that("AFL-04: roi_label prefers template_label over the name parse", {
  d <- in_legacy[1:2, , drop = FALSE]
  # Refactored cut naming puts the template_id hash last -> the parse would
  # return the hash; the dedicated label is the source of truth.
  d$template_name  <- c("Bcu_1_refactored_cut_ab12cd34ef56.wav",
                        "Bcu_1_refactored_cut_99aa88bb77cc.wav")
  d$template_label <- c("Basileuterus culicivorus", "Myiothlypis flaveola")
  r <- detecs_to_rois(d, username = "Tester")
  expect_equal(r$roi_label,
               c("Basileuterus culicivorus", "Myiothlypis flaveola"))
})

test_that("AFL-04: an NA/empty template_label falls back to the name parse", {
  d <- in_legacy[1:2, , drop = FALSE]
  d$template_label <- c(NA_character_, "")   # both fall back to template_name
  r <- detecs_to_rois(d, username = "Tester")
  parsed <- .roi_label_from_template_name(d$template_name)
  expect_equal(r$roi_label, parsed)
})

# --- AFL-02: roi_source provenance + contamination guard ---------------------

test_that("AFL-02: detection-derived ROIs are stamped roi_source = 'detection'", {
  # F5: emits the deprecation warning; mapping unchanged. Force verbosity so
  # the warning fires deterministically (lifecycle throttles by default).
  withr::local_options(lifecycle_verbosity = "warning")
  expect_warning(
    r <- detecs_to_rois(in_val, username = "Tester"),
    "deprecated")
  expect_true(all(r$roi_source == "detection"))
})

test_that("AFL-02: writing into a store holding manual ROIs is refused", {
  testthat::skip_if_not_installed("duckdb")
  db <- tempfile(fileext = ".duckdb")
  manual <- .schema_rois(1L)
  manual$soundscape_path     <- "ss/a.wav"
  manual$soundscape_file     <- "a.wav"
  manual$roi_label           <- "Some species"
  manual$roi_start <- 0; manual$roi_end <- 1
  manual$roi_type            <- "song"          # hand-segmented, not a detection
  manual$roi_source          <- "manual"
  manual$roi_input_timestamp <- "2026-01-01 00:00:00"
  con <- .signals_duckdb_connect(db)            # F5: unified signals store
  .signals_duckdb_upsert(con, .rois_as_signals(manual))
  DBI::dbDisconnect(con, shutdown = TRUE)

  # F5: the write guard is gone — the unified store persists both classes;
  # circular validation is now a logical check in validate_by_overlap() (R2).
  suppressWarnings(suppressMessages(
    detecs_to_rois(in_val, username = "T", output_db = db)))
  con <- .signals_duckdb_connect(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  back <- .signals_as_rois(.signals_duckdb_read(con, signal_class = "roi"))
  expect_equal(nrow(back), 1L)                  # the manual row is intact
  # detecs_to_rois output maps to detection_to_roi (spec §6.1), not detection.
  expect_equal(.signals_duckdb_count(con, signal_class = "detection_to_roi"),
               nrow(in_val))                    # detections co-exist
})

test_that("AFL-02: a fresh / detection-only store accepts (re-)writes", {
  testthat::skip_if_not_installed("duckdb")
  db <- tempfile(fileext = ".duckdb")
  m1 <- suppressMessages(suppressWarnings(
    detecs_to_rois(in_val, username = "T", output_db = db)))
  m2 <- suppressMessages(suppressWarnings(
    detecs_to_rois(in_val, username = "T", output_db = db)))
  expect_true(all(m2$roi_source == "detection"))
})

test_that("filter_tp keeps only TP rows when validation exists (G2)", {
  r <- detecs_to_rois(in_val, username = "Tester", filter_tp = TRUE)
  expect_equal(nrow(r), nrow(g2))
  expect_true(all(r$roi_label_confidence == "certain"))
  cmp(r, g2)
})

test_that("filter_tp warns and keeps all when validation is absent (G3)", {
  expect_warning(
    r <- detecs_to_rois(in_legacy, username = "Tester", filter_tp = TRUE),
    "not filtered"
  )
  expect_equal(nrow(suppressWarnings(
    detecs_to_rois(in_legacy, username = "Tester", filter_tp = TRUE))),
    nrow(g3))
  expect_true(all(is.na(suppressWarnings(
    detecs_to_rois(in_legacy, username = "Tester"))$roi_label_confidence)))
})

test_that("a single Sys.time() is captured for both time fields (DTR-07)", {
  r <- detecs_to_rois(in_val, username = "Tester")
  expect_equal(length(unique(r$roi_input_timestamp)), 1L)
  expect_false(any(is.na(r$roi_input_timestamp)))
})

test_that("output_db persists to the canonical ROI store + round-trips (DTR-03)", {
  tmp <- tempfile("dtr_db_"); dir.create(tmp)
  db <- file.path(tmp, "rois_from_detections.duckdb")
  vis <- withVisible(suppressWarnings(suppressMessages(
    detecs_to_rois(in_val, username = "Tester", output_db = db))))
  expect_false(vis$visible)                     # invisible when persisted
  expect_true(file.exists(db))
  # F5: round-trip through the signals store via the compat layer.
  con <- .signals_duckdb_connect(db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  total <- .signals_duckdb_count(con, signal_class = "detection_to_roi")
  expect_equal(total, nrow(in_val))
  back <- .signals_as_rois(.signals_duckdb_read(con,
    soundscape_path = vis$value$soundscape_path[1],
    signal_class = "detection_to_roi"), include_detections = TRUE)
  expect_true(all(back$roi_type == "detection"))
})

test_that("C7: detection ROIs do not clobber a ground-truth store", {
  tmp <- tempfile("dtr_c7_"); dir.create(tmp)
  truth <- file.path(tmp, "ground_truth.duckdb")
  # Seed a hand-segmented ground-truth ROW for a soundscape (path key
  # normalized the same way the converters do — the fixture carries a dirty
  # "./soundscapes//..." key).
  con <- .signals_duckdb_connect(truth)         # F5: unified signals store
  gt <- .schema_rois(1L)
  sp <- .normalize_path_key(in_val$soundscape_path[1])
  gt$soundscape_path <- sp; gt$soundscape_file <- in_val$soundscape_file[1]
  gt$roi_user <- "Human"; gt$roi_input_timestamp <- "2026-01-01 00:00:00"
  gt$roi_label <- "Ground truth"; gt$roi_type <- "manual"
  gt$roi_start <- 0; gt$roi_end <- 1
  .signals_duckdb_upsert(con, .rois_as_signals(gt))
  DBI::dbDisconnect(con, shutdown = TRUE)
  # F5: same unified store, class-scoped — detection rows and the manual
  # ground-truth row co-exist; the save never deletes the roi-class row (R1).
  suppressWarnings(suppressMessages(
    detecs_to_rois(in_val, username = "Tester", output_db = truth)))
  con <- .signals_duckdb_connect(truth)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  back <- .signals_as_rois(.signals_duckdb_read(con,
    soundscape_path = sp, signal_class = "roi"))
  expect_equal(nrow(back), 1L)
  expect_equal(back$roi_label, "Ground truth")
})

test_that("deprecated output_path writes per-soundscape CSVs (DTR-03 compat)", {
  tmp <- tempfile("dtr_csv_"); dir.create(tmp)
  expect_warning(
    detecs_to_rois(in_val, username = "Tester", output_path = tmp),
    "deprecated"
  )
  csvs <- list.files(tmp, pattern = "\\.csv$", full.names = TRUE)
  expect_equal(length(csvs), length(unique(in_val$soundscape_file)))
  expect_equal(sum(vapply(csvs, function(f) nrow(read.csv(f)), integer(1))),
               nrow(in_val))
})

test_that("a detections .duckdb store is a valid input (DTR-01)", {
  tmp <- tempfile("dtr_in_"); dir.create(tmp)
  db <- file.path(tmp, "detections.duckdb")
  # F6: the detections store is the signals store — seed detection-class rows.
  con <- .signals_duckdb_connect(db)
  det <- .coerce_detections(in_val)             # canonical detections frame
  .signals_duckdb_upsert(con, .detections_as_signals(det))
  DBI::dbDisconnect(con, shutdown = TRUE)
  r <- detecs_to_rois(db, username = "Tester")
  expect_s3_class(r, "data.frame")
  expect_equal(nrow(r), nrow(in_val))
})

test_that("front door rejects bad inputs (DTR-06)", {
  expect_error(detecs_to_rois(in_val), "username")
  expect_error(detecs_to_rois(in_val, username = "!!!"), "empty after sanitation")
  expect_error(detecs_to_rois(in_val, username = "U", filter_tp = "yes"),
               "filter_tp")
  expect_error(detecs_to_rois("/no/such/file.duckdb", username = "U"),
               "File not found")
  bad <- in_legacy; bad$peak_quant <- NULL
  expect_error(detecs_to_rois(bad, username = "U"), "missing required column")
})
