# Tests for the refactored fetch_score_peaks_i (per-pair PURE peak capture, flow
# step 8). Goldens (R/sandbox/fetch_score_peaks_i.R) record the ORIGINAL 21-col
# behaviour; the peaks refactor (FSP) changes capture + emits the canonical 24-col
# detections schema, so the divergences are asserted as DERIVED expectations
# (golden/fetch_score_peaks_i/NOTES.md), not byte-equality.
#
# Run: Rscript -e "testthat::test_file('R/refactored/tests/testthat/test-fetch_score_peaks_i.R')"

suppressPackageStartupMessages({ library(tibble) })

gold     <- test_path("golden/fetch_score_peaks_i")
real     <- readRDS(test_path("golden/run_matching/G4_scores_inmem.rds"))
CANON    <- names(.schema_detections(0L))

# Build a minimal one-row scores contract row (mirrors the generator).
mk <- function(score, sw = 4L, time = seq_along(score)) tibble::tibble(
  soundscape_path = "s.wav", soundscape_file = "s.wav", template_path = "t.wav",
  template_file = "t.wav", template_name = "syn", template_wl = 512L,
  template_ovlp = 50, template_min_freq = 1, template_max_freq = 8,
  template_sample_rate = 48000L, template_start = 0, template_end = 1,
  score_sliding_window = sw, score_method = "cor",
  score_vec = list(data.frame(time_vec = time, score_vec = score)))

# --- schema / contract (FSP-12/15) -------------------------------------------

test_that("capture emits the canonical detections schema (FSP-15)", {
  d <- fetch_score_peaks_i(real[1, ])
  expect_s3_class(d, "data.frame")
  expect_equal(names(d), CANON)
  expect_equal(ncol(d), length(CANON))
  expect_gt(nrow(d), 0L)
  # detection_id present + unique; template_id NA (G4 grid predates propagation)
  expect_true(all(!is.na(d$detection_id)) && !anyDuplicated(d$detection_id))
  expect_true(all(is.na(d$template_id)))
  expect_true(all(d$score_method == "cor"))
})

test_that("capture is PURE: the three filter columns stay NA (FSPB-10)", {
  d <- fetch_score_peaks_i(real[1, ])
  expect_true(all(is.na(d$detection_min_score)))
  expect_true(all(is.na(d$detection_min_quant)))
  expect_true(all(is.na(d$detection_top_n)))
})

# --- FSP-04: edge trimming by pad_length retains near-edge peaks --------------

test_that("FSP-04: edge margin = pad_length retains MORE peaks than the golden", {
  d <- fetch_score_peaks_i(real[1, ])
  g <- readRDS(file.path(gold, "real_t075_default.rds"))   # 14 rows (full window)
  expect_gte(nrow(d), nrow(g))                             # 21 >= 14
  # the strong, well-interior peaks the golden found are still detected
  expect_true(max(d$peak_score) >= max(g$peak_score) - 1e-9)
})

test_that("FSP-04: a dominant peak ~1 window from the edge is now RETAINED", {
  edge <- c(rep(0.1, 14), 0.2, 0.5, 0.9, 0.5, 0.2, 0.1)   # golden was 0x0
  d <- fetch_score_peaks_i(mk(edge))
  expect_equal(names(d), CANON)
  expect_equal(nrow(d), 1L)
  expect_equal(d$peak_index, 17L)
})

# --- FSP-09: ECDF over the valid (non-padding) region ------------------------

test_that("FSP-09: peak_quant differs from the full-vector golden ECDF", {
  d <- fetch_score_peaks_i(real[1, ])
  g <- readRDS(file.path(gold, "real_t075_default.rds"))
  shared <- intersect(d$peak_index, g$peak_index)
  expect_gt(length(shared), 0L)
  rq <- d$peak_quant[match(shared, d$peak_index)]
  gq <- g$peak_quant[match(shared, g$peak_index)]
  expect_true(any(abs(rq - gq) > 1e-9))   # valid-region vs padded population
})

# --- FSP-01 / FSP-05 / FSP-06: plateau + greedy NMS --------------------------

test_that("FSP-01: a plateau is ONE peak at its centre (golden had two edges)", {
  plateau <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.9, 0.9, 0.9, 0.5, 0.4, 0.3, 0.2, 0.1,
               0.2, 0.3, 0.4, 0.5, 0.4, 0.3, 0.2)
  d <- fetch_score_peaks_i(mk(plateau))
  expect_true(7L %in% d$peak_index)                 # centre of the 6-8 plateau
  expect_false(any(c(6L, 8L) %in% d$peak_index))    # not the plateau edges
})

test_that("FSP-05/06: exact-tie twins within the buffer collapse to one", {
  tie <- c(0.10, 0.15, 0.20, 0.25, 0.30, 0.40, 0.50, 0.80, 0.30, 0.80, 0.30,
           0.25, 0.20, 0.15, 0.10, 0.08, 0.06, 0.04, 0.02, 0.01)
  g <- readRDS(file.path(gold, "synth_tie.rds"))    # original kept BOTH (idx 8,10)
  expect_equal(nrow(g), 2L)
  d <- fetch_score_peaks_i(mk(tie))
  expect_equal(nrow(d), 1L)
  expect_equal(d$peak_index, 8L)                    # lowest index wins the tie
})

# --- FSP-12: empty / degenerate -> typed zero-row schema, never error ---------

test_that("FSP-12: a monotone vector returns the typed empty schema (golden ERRORED)", {
  d <- fetch_score_peaks_i(mk(seq(0.1, 0.9, length.out = 20)))
  expect_equal(names(d), CANON)
  expect_equal(nrow(d), 0L)
  expect_equal(ncol(d), length(CANON))
})

test_that("FSP-12: a peak within pad_length of the edge is trimmed -> empty schema", {
  # only local maximum is at idx 2; pad_length = 2 (sw 4) trims it (2 - 2 < 1).
  edge2 <- c(0.1, 0.9, 0.1, rep(0.05, 17))
  d <- fetch_score_peaks_i(mk(edge2))
  expect_equal(names(d), CANON)
  expect_equal(nrow(d), 0L)
})

test_that("FSP-04: the formerly all-trimmed pair is now NON-empty (pad_length margin)", {
  # golden synth_alltrimmed was 0x0 under the full-window margin; under FSP-04 the
  # single peak at idx 3 (3 - pad_length = 1) is retained.
  trimmed <- c(0.1, 0.5, 0.9, seq(0.85, 0.05, length.out = 17))
  g <- readRDS(file.path(gold, "synth_alltrimmed.rds"))
  expect_equal(nrow(g), 0L)                       # original: empty (bare 0x0)
  d <- fetch_score_peaks_i(mk(trimmed))
  expect_equal(nrow(d), 1L)
  expect_equal(d$peak_index, 3L)
})

# --- buffer_size semantics + validation (FSP-07) -----------------------------

test_that("buffer_size = 0 disables suppression (more peaks than 'template')", {
  d0 <- fetch_score_peaks_i(real[1, ], buffer_size = 0)
  dt <- fetch_score_peaks_i(real[1, ], buffer_size = "template")
  expect_gt(nrow(d0), nrow(dt))
  expect_equal(unique(d0$detection_buffer), 0L)
})

test_that("FSP-07: buffer_size is validated as a scalar 'template' or >= 0 number", {
  expect_error(fetch_score_peaks_i(real[1, ], buffer_size = c(1, 2)), "single value")
  expect_error(fetch_score_peaks_i(real[1, ], buffer_size = "nope"), "non-negative")
  expect_error(fetch_score_peaks_i(real[1, ], buffer_size = -3), "non-negative")
})

# --- FSP-13: input contract validation ---------------------------------------

test_that("FSP-13: missing time_vec / score components raise a clear error", {
  bad <- real[1, ]
  bad$score_vec <- list(data.frame(score_vec = bad$score_vec[[1]]$score_vec))  # no time_vec
  expect_error(fetch_score_peaks_i(bad), "time_vec|equal length|needs")
  expect_error(fetch_score_peaks_i(list(foo = 1)), "Invalid `df_scores_i`")
})

test_that("FSP-13: score_vec and time_vec must be equal length", {
  bad <- mk(c(0.1, 0.5, 0.9, 0.2))
  # a plain list (not a data.frame) lets the two vectors differ in length
  bad$score_vec <- list(list(time_vec = 1:3, score_vec = c(0.1, 0.5, 0.9, 0.2)))
  expect_error(fetch_score_peaks_i(bad), "equal length")
})

test_that("AFL-14: a non-increasing time_vec errors", {
  bad <- mk(c(0.1, 0.5, 0.9, 0.2), time = c(0, 1, 1, 2))   # 1->1 non-increasing
  expect_error(fetch_score_peaks_i(bad), "strictly increasing")
})

test_that("AFL-14: an NA in time_vec errors", {
  bad <- mk(c(0.1, 0.5, 0.9, 0.2), time = c(0, 1, NA, 3))
  expect_error(fetch_score_peaks_i(bad), "time_vec` contains NA")
})

test_that("AFL-14: an irregular time step warns (mismatched spectro params)", {
  bad <- mk(c(0.1, 0.5, 0.9, 0.2), time = c(0, 1, 2, 5))   # last step 3x the rest
  expect_warning(fetch_score_peaks_i(bad), "irregular step")
})

test_that("AFL-14: a regular monotone time_vec is silent (golden unaffected)", {
  expect_silent(fetch_score_peaks_i(mk(c(0.1, 0.5, 0.9, 0.2))))   # time = 1:4
})
