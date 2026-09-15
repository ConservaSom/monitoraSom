# Tests for the refactored fetch_score_peaks (batch wrapper) + the composable
# filter_detections_i() (FSPB / FSP-10/11). Goldens
# (R/sandbox/fetch_score_peaks.R) record the ORIGINAL 21-col do.call(rbind) +
# CSV behaviour; the refactor emits the canonical 24-col schema, decouples
# filtering, accepts 1-row input, and persists to DuckDB — asserted as DERIVED
# divergences (golden/fetch_score_peaks/NOTES.md).
#
# Run: Rscript -e "testthat::test_file('R/refactored/tests/testthat/test-fetch_score_peaks.R')"

suppressPackageStartupMessages({
  library(tibble); library(purrr); library(dplyr); library(DBI); library(duckdb)
})
real  <- readRDS(test_path("golden/run_matching/G4_scores_inmem.rds"))
CANON <- names(.schema_detections(0L))

# one synthetic scores row, with a controllable pair identity
mk <- function(score, sw = 4L, ss = "s.wav", tpl = "t.wav") tibble::tibble(
  soundscape_path = ss, soundscape_file = ss, template_path = tpl,
  template_file = tpl, template_name = "syn", template_wl = 512L,
  template_ovlp = 50, template_min_freq = 1, template_max_freq = 8,
  template_sample_rate = 48000L, template_start = 0, template_end = 1,
  score_sliding_window = sw, score_method = "cor",
  score_vec = list(data.frame(time_vec = seq_along(score), score_vec = score)))

# --- batch contract (FSPB-02/05/06/08/09) ------------------------------------

test_that("batch returns a tibble in the canonical schema (FSPB-06/05)", {
  b <- suppressMessages(fetch_score_peaks(real))
  expect_s3_class(b, "tbl_df")
  expect_equal(names(b), CANON)
  expect_gt(nrow(b), 0L)
})

test_that("FSPB-02: a single-row scores tibble is accepted (no error)", {
  b1 <- suppressMessages(fetch_score_peaks(real[1, ]))
  expect_s3_class(b1, "tbl_df")
  expect_gt(nrow(b1), 0L)
})

test_that("FSPB-08: batch == per-pair capture bound (one source of truth)", {
  b <- suppressMessages(fetch_score_peaks(real))
  manual <- purrr::list_rbind(lapply(seq_len(nrow(real)), function(i)
    fetch_score_peaks_i(real[i, ])))
  expect_equal(nrow(b), nrow(manual))
  expect_setequal(b$detection_id, manual$detection_id)
})

test_that("FSPB-09: the message reports the detection count", {
  expect_message(fetch_score_peaks(real), "Detections extracted from scores: [0-9]+")
})

test_that("FSPB-05: an all-empty batch returns the typed zero-row canonical schema", {
  # both pairs' only peak is within pad_length of the edge -> empty captures
  edge2 <- c(0.1, 0.9, 0.1, rep(0.05, 17))
  allnone <- dplyr::bind_rows(mk(edge2, ss = "a.wav"), mk(edge2, ss = "b.wav"))
  b <- suppressMessages(fetch_score_peaks(allnone))
  expect_equal(names(b), CANON)
  expect_equal(nrow(b), 0L)
  expect_equal(ncol(b), length(CANON))
})

test_that("FSPB-05: a mixed empty + non-empty batch keeps the schema", {
  normal <- c(0.1,0.2,0.3,0.4,0.5,0.9,0.4,0.3,0.2,0.3,0.4,0.5,0.4,0.3,0.2,0.1,0.2,0.3,0.2,0.1)
  edge2  <- c(0.1, 0.9, 0.1, rep(0.05, 17))
  mixed <- dplyr::bind_rows(mk(normal, ss = "a.wav"), mk(edge2, ss = "b.wav"))
  b <- suppressMessages(fetch_score_peaks(mixed))
  expect_equal(names(b), CANON)
  expect_gt(nrow(b), 0L)
  expect_true(all(b$soundscape_path == "a.wav"))   # only the non-empty pair
})

# --- filter_detections_i: scope, provenance, ordering (FSP-10/11, FSPB-10) ------

# a controlled 2-pair table: peaks 6 frames apart (> buffer 4 so all survive NMS).
# pair A scores {0.9,0.7,0.5} at idx 3,9,15; pair B {0.8,0.6,0.4}.
two_pairs <- suppressMessages(fetch_score_peaks(dplyr::bind_rows(
  mk(c(0.1,0.2,0.9,0.2,0.1,0.1,0.1,0.2,0.7,0.2,0.1,0.1,0.1,0.2,0.5,0.2,0.1,0.1,0.1,0.1), ss = "A.wav"),
  mk(c(0.1,0.2,0.8,0.2,0.1,0.1,0.1,0.2,0.6,0.2,0.1,0.1,0.1,0.2,0.4,0.2,0.1,0.1,0.1,0.1), ss = "B.wav")
)))

test_that("min_score filters on raw peak_score and records provenance", {
  f <- filter_detections_i(two_pairs, min_score = 0.6)
  expect_true(all(f$peak_score >= 0.6))
  expect_true(all(f$detection_min_score == 0.6))
  expect_s3_class(f, "tbl_df")
})

test_that("FSP-11: top_n selects by raw score; scope='pair' keeps n per pair", {
  fp <- filter_detections_i(two_pairs, top_n = 2, scope = "pair")
  expect_equal(nrow(fp), 4L)                       # 2 per pair x 2 pairs
  expect_true(all(fp$detection_top_n == 2L))
  # per pair, the two highest raw scores survive
  a <- sort(fp$peak_score[fp$soundscape_path == "A.wav"], decreasing = TRUE)
  expect_equal(a, c(0.9, 0.7))
})

test_that("scope='grid' top_n keeps the n highest across the whole table", {
  fg <- filter_detections_i(two_pairs, top_n = 2, scope = "grid")
  expect_equal(nrow(fg), 2L)
  expect_setequal(round(fg$peak_score, 1), c(0.9, 0.8))   # global top 2
})

test_that("FSP-10: top_n above available warns and keeps all, recording top_n", {
  expect_warning(
    fw <- filter_detections_i(two_pairs, top_n = 99, scope = "grid"),
    "exceeds the .* available"
  )
  expect_equal(nrow(fw), nrow(two_pairs))
  expect_true(all(fw$detection_top_n == 99L))
})

test_that("scope changes which detections min_quant keeps", {
  # grid quantile ranks across both pairs; pair quantile is per-pair (FSP-09)
  fp <- filter_detections_i(two_pairs, min_quant = 0.9, scope = "pair")
  fg <- filter_detections_i(two_pairs, min_quant = 0.9, scope = "grid")
  expect_false(identical(nrow(fp), nrow(fg)) && setequal(fp$detection_id, fg$detection_id))
})

test_that("filter_detections_i on an empty table returns the typed empty schema", {
  e <- filter_detections_i(.schema_detections(0L), min_score = 0.5)
  expect_equal(names(e), CANON)
  expect_equal(nrow(e), 0L)
})

# --- AFL-16: per-template batch filtering (filter_detections) -----------

mk_det <- function(tn, score, ss = "ss.wav") {
  d <- .schema_detections(length(score))
  d$template_name   <- tn
  d$peak_score      <- score
  d$peak_quant      <- seq_along(score) / length(score)
  d$soundscape_path <- ss
  d$peak_index      <- seq_along(score)
  d
}

test_that("AFL-16: applies a per-template min_score and pools listed templates", {
  det <- dplyr::bind_rows(mk_det("A", c(0.9, 0.5, 0.2)), mk_det("B", c(0.8, 0.4)))
  thr <- data.frame(template_name = c("A", "B"), min_score = c(0.5, 0.9),
                    stringsAsFactors = FALSE)
  res <- filter_detections(det, thr, scope = "grid")
  expect_equal(sum(res$template_name == "A"), 2L)   # >= 0.5 -> 0.9, 0.5
  expect_equal(sum(res$template_name == "B"), 0L)   # >= 0.9 -> none
  expect_true(all(res$detection_min_score == 0.5 | res$template_name != "A"))
})

test_that("AFL-16: unlisted templates are excluded; NA cell = no filter", {
  det <- dplyr::bind_rows(mk_det("A", c(0.9, 0.1)), mk_det("C", 0.3))
  thr <- data.frame(template_name = "A", min_score = NA_real_,
                    stringsAsFactors = FALSE)
  res <- filter_detections(det, thr)
  expect_setequal(unique(res$template_name), "A")   # C not listed -> dropped
  expect_equal(nrow(res), 2L)                        # NA min_score -> no filter
})

test_that("AFL-16: a fully-filtered batch keeps the canonical empty schema", {
  det <- mk_det("A", c(0.2, 0.1))
  thr <- data.frame(template_name = "A", min_score = 0.9, stringsAsFactors = FALSE)
  res <- filter_detections(det, thr)
  expect_equal(names(res), CANON)
  expect_equal(nrow(res), 0L)
})

test_that("AFL-16: validation errors (empty / no key / no filter column)", {
  det <- mk_det("A", 0.5)
  expect_error(filter_detections(det, data.frame()), "non-empty")
  expect_error(filter_detections(det, data.frame(min_score = 0.5)),
               "key column")
  expect_error(filter_detections(det, data.frame(template_name = "A")),
               "min_score / min_quant / top_n")
})

test_that("AUD-41: duplicate thresholds keys error (no silent duplication)", {
  det <- mk_det("A", c(0.9, 0.5))
  thr <- data.frame(template_name = c("A", "A"), min_score = c(0.5, 0.6),
                    stringsAsFactors = FALSE)
  expect_error(filter_detections(det, thr), "duplicate")
})

test_that("AUD-43: a fractional top_n is rejected (no silent truncation)", {
  det <- mk_det("A", c(0.9, 0.5, 0.2))
  expect_error(filter_detections_i(det, top_n = 2.7), "whole number")
  # A whole number still works.
  expect_equal(nrow(filter_detections_i(det, top_n = 2)), 2L)
})

# --- persistence (FSPB-04: DuckDB, CSV deprecated) ---------------------------

test_that("FSPB-04: detections persist to a DuckDB table (upsert, idempotent)", {
  dbp <- tempfile(fileext = ".duckdb")
  b1 <- suppressMessages(fetch_score_peaks(real, output_db = dbp))
  con <- .signals_duckdb_connect(dbp)                       # F3: signals store
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  expect_equal(.signals_duckdb_count(con, signal_class = "detection"), nrow(b1))
  # re-running upserts (no duplication)
  suppressMessages(fetch_score_peaks(real, output_db = dbp, autosave_action = "replace"))
  expect_equal(.signals_duckdb_count(con, signal_class = "detection"), nrow(b1))
})

test_that("FSPB-04: a missing output_db directory is a clear up-front error", {
  expect_error(
    suppressMessages(fetch_score_peaks(real, output_db = "/no/such/dir/x.duckdb")),
    "directory does not exist"
  )
})

# --- input validation (FSPB-02/03) -------------------------------------------

test_that("FSPB-03: an .rds path is accepted (legacy) and validated", {
  p <- tempfile(fileext = ".rds"); saveRDS(real, p)
  b <- suppressMessages(fetch_score_peaks(p))
  expect_gt(nrow(b), 0L)
  expect_error(suppressMessages(fetch_score_peaks(tempfile(fileext = ".rds"))),
               "does not exist")
})

test_that("FSPB-02/03: a non-scores object is rejected with a clear message", {
  expect_error(fetch_score_peaks(data.frame(x = 1)), "score contract columns|non-empty")
  expect_error(fetch_score_peaks(real[0, ]), "non-empty")
})
