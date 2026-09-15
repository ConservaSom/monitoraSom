# Contract tests for the pure helpers extracted from launch_segmentation_app
# (LSA-05): _roi_helpers.R and _audio_helpers.R. Behaviour is pinned to the
# alt app so the extraction is provably mechanical. Known defects are asserted
# as-is here and fixed in later phases (LSA-04 status divergence, LSA-14
# positional acoustat).
#
# Run from the project root:
#   Rscript -e "testthat::test_dir('R/refactored/tests/testthat')"

library(testthat)

roi_row <- function(label = "bird", start = 1, end = 2, lo = 1, hi = 5) {
  data.frame(
    roi_label = label, roi_start = start, roi_end = end,
    roi_min_freq = lo, roi_max_freq = hi, stringsAsFactors = FALSE
  )
}

# ── .is_duplicate_roi ──────────────────────────────────────────────────────────
test_that("no duplicate against NULL/empty existing set", {
  expect_false(.is_duplicate_roi(roi_row(), NULL))
  expect_false(.is_duplicate_roi(roi_row(), roi_row()[0, ]))
})

test_that("exact and within-tolerance matches are duplicates", {
  existing <- roi_row()
  expect_true(.is_duplicate_roi(roi_row(), existing))
  expect_true(.is_duplicate_roi(roi_row(start = 1.0005), existing))  # < 0.001
})

test_that("beyond tolerance or different label is not a duplicate", {
  existing <- roi_row()
  expect_false(.is_duplicate_roi(roi_row(start = 1.5), existing))
  expect_false(.is_duplicate_roi(roi_row(label = "frog"), existing))
})

test_that("matches only one of several existing rows", {
  existing <- rbind(roi_row(label = "frog", start = 10),
                    roi_row(label = "bird", start = 1))
  expect_true(.is_duplicate_roi(roi_row(label = "bird", start = 1), existing))
})

# ── .status_from_labels / .derive_status_from_rois (LSA-04 consolidated) ───────
test_that("status derivation: empty, segmented, single sentinel", {
  expect_equal(.derive_status_from_rois(NULL), "unsegmented")
  expect_equal(.derive_status_from_rois(roi_row()[0, ]), "unsegmented")
  expect_equal(.derive_status_from_rois(roi_row()), "segmented")
  expect_equal(.derive_status_from_rois(roi_row(label = .ROI_NO_SOI_LABEL)),
               "no_soi")
})

test_that("all-sentinel rows are 'no_soi' (LSA-04 consolidated rule)", {
  two <- rbind(roi_row(label = .ROI_NO_SOI_LABEL),
               roi_row(label = .ROI_NO_SOI_LABEL, start = 3))
  # Consolidated rule (LSA-04): no_soi iff EVERY row is a sentinel, matching the
  # DB classifier .roi_duckdb_statuses(). (Was 'segmented' under the old
  # in-memory "exactly one sentinel" rule.)
  expect_equal(.derive_status_from_rois(two), "no_soi")
  # A mix of a real ROI and a sentinel is 'segmented'.
  mixed <- rbind(roi_row(label = "bird"),
                 roi_row(label = .ROI_NO_SOI_LABEL, start = 3))
  expect_equal(.derive_status_from_rois(mixed), "segmented")
})

test_that(".status_from_labels is the shared pure rule", {
  expect_equal(.status_from_labels(character(0)), "unsegmented")
  expect_equal(.status_from_labels("bird"), "segmented")
  expect_equal(.status_from_labels(rep(.ROI_NO_SOI_LABEL, 3)), "no_soi")
  expect_equal(.status_from_labels(c(.ROI_NO_SOI_LABEL, "bird")), "segmented")
  # NA labels are not sentinels.
  expect_equal(.status_from_labels(NA_character_), "segmented")
  expect_equal(.status_from_labels(c(.ROI_NO_SOI_LABEL, NA_character_)),
               "segmented")
})

# ── .next_unsegmented_index (LSA-18 navigation) ────────────────────────────────
test_that("next/prev unsegmented finds the nearest neighbour by global index", {
  # positions:        1     2      3     4      5
  has_table <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
  # from index 3: next unsegmented is 4, prev is 2
  expect_equal(.next_unsegmented_index(has_table, 3, "next"), 4L)
  expect_equal(.next_unsegmented_index(has_table, 3, "prev"), 2L)
  # from index 1 there is no prev unsegmented
  expect_true(is.na(.next_unsegmented_index(has_table, 1, "prev")))
  # from index 5 there is no next unsegmented
  expect_true(is.na(.next_unsegmented_index(has_table, 5, "next")))
})

test_that("unsegmented navigation skips the just-segmented current file (LSA-18)", {
  # current file (index 2) was unsegmented but is now segmented after autosave;
  # next should skip to index 4, NOT return to index 2.
  has_table <- c(TRUE, TRUE, TRUE, FALSE)
  expect_equal(.next_unsegmented_index(has_table, 2, "next"), 4L)
  # strictly-after semantics: current index is never its own neighbour
  expect_true(is.na(.next_unsegmented_index(c(FALSE, TRUE, TRUE), 1, "next")))
})

# ── .make_no_soi_roi (LSA-06 / LSA-23 sentinel builder) ────────────────────────
test_that("no_soi sentinel honours the schema contract", {
  skip_if_not_installed("tibble")
  roi <- .make_no_soi_roi(
    soundscape_path = "/data/site_a/rec001.wav", user = "alice",
    duration = 60, freq_min = 1, freq_max = 12,
    wl = 1024L, ovlp = 0L, sample_rate = 48000L, pitch_shift = 1L,
    timestamp = "2026-05-29 10:00:00"
  )
  expect_equal(nrow(roi), 1L)
  expect_equal(roi$roi_label, .ROI_NO_SOI_LABEL)
  expect_true(.is_no_soi_label(roi$roi_label))
  expect_equal(roi$soundscape_path, "/data/site_a/rec001.wav")
  expect_equal(roi$soundscape_file, "rec001.wav")          # basename derived
  expect_equal(roi$roi_start, 0)
  expect_equal(roi$roi_end, 60)
  expect_equal(c(roi$roi_min_freq, roi$roi_max_freq), c(1, 12))
  # metadata fields are NA per the sentinel contract
  expect_true(all(is.na(c(roi$roi_type, roi$roi_label_confidence,
                          roi$roi_is_complete, roi$roi_comment))))
  # a single sentinel row classifies as no_soi via the shared rule
  expect_equal(.derive_status_from_rois(roi), "no_soi")
})

# ── .roi_at_point (LSA-104 active-ROI hit-test) ────────────────────────────────
test_that("point inside a single ROI returns its row id", {
  rois <- rbind(
    roi_row("a", start = 0, end = 2, lo = 1, hi = 5),
    roi_row("b", start = 5, end = 8, lo = 2, hi = 9)
  )
  expect_equal(.roi_at_point(rois, x = 1, y = 3), 1L)
  expect_equal(.roi_at_point(rois, x = 6, y = 4), 2L)
})

test_that("point on empty space (or NULL/empty/NA input) returns NA", {
  rois <- roi_row("a", start = 0, end = 2, lo = 1, hi = 5)
  expect_true(is.na(.roi_at_point(rois, x = 3, y = 3)))   # outside in time
  expect_true(is.na(.roi_at_point(rois, x = 1, y = 9)))   # outside in freq
  expect_true(is.na(.roi_at_point(NULL, 1, 1)))
  expect_true(is.na(.roi_at_point(rois[0, ], 1, 1)))
  expect_true(is.na(.roi_at_point(rois, x = NA, y = 1)))
})

test_that("overlapping ROIs: the smallest-area ROI wins", {
  rois <- rbind(
    roi_row("broad",  start = 0, end = 10, lo = 0, hi = 10),  # area 100
    roi_row("nested", start = 4, end = 6,  lo = 4, hi = 6)    # area 4
  )
  # a point inside both -> the nested (smaller) ROI
  expect_equal(.roi_at_point(rois, x = 5, y = 5), 2L)
  # a point only inside the broad ROI -> the broad one
  expect_equal(.roi_at_point(rois, x = 1, y = 1), 1L)
})

test_that("bounds are inclusive and NA-bounded rows never match", {
  rois <- rbind(
    roi_row("edge", start = 0, end = 2, lo = 1, hi = 5),
    data.frame(roi_label = "partial", roi_start = NA, roi_end = 2,
               roi_min_freq = 1, roi_max_freq = 5, stringsAsFactors = FALSE)
  )
  expect_equal(.roi_at_point(rois, x = 0, y = 1), 1L)   # on the corner
  expect_equal(.roi_at_point(rois, x = 2, y = 5), 1L)   # on the far corner
  # the NA-bounded row is never selected even at a point that would match
  expect_equal(.roi_at_point(rois, x = 1, y = 3), 1L)
})

# ── .resize_interval (LSA-106 active-ROI resize) ───────────────────────────────
test_that("expand grows both edges and clamps to the limits", {
  # interior: grows by step on each side
  expect_equal(
    .resize_interval(2, 4, step = 0.5, "expand", limit_lo = 0, limit_hi = 10),
    c(1.5, 4.5)
  )
  # clamps at both ends, never crossing the limits
  expect_equal(
    .resize_interval(0.2, 9.8, step = 0.5, "expand", limit_lo = 0, limit_hi = 10),
    c(0, 10)
  )
})

test_that("contract shrinks both edges but never inverts (min_extent)", {
  expect_equal(
    .resize_interval(2, 6, step = 0.5, "contract", 0, 10, min_extent = 0.1),
    c(2.5, 5.5)
  )
  # a contract that would invert collapses to a min_extent window at the midpoint
  res <- .resize_interval(4, 5, step = 1, "contract", 0, 10, min_extent = 0.2)
  expect_equal(res, c(4.5 - 0.1, 4.5 + 0.1))
  expect_true(res[2] - res[1] >= 0.2 - 1e-9)
})

test_that("unknown direction errors", {
  expect_error(.resize_interval(1, 2, 0.1, "grow", 0, 10), "Unknown direction")
})

# ── .apply_roi_metadata (LSA-112 popup field mapping) ──────────────────────────
test_that("metadata is written to the target row, geometry untouched", {
  rois <- rbind(
    cbind(roi_row("a", start = 0, end = 1, lo = 1, hi = 2),
          roi_type = "song", roi_label_confidence = "certain",
          roi_is_complete = "complete", roi_comment = NA_character_,
          stringsAsFactors = FALSE),
    cbind(roi_row("b", start = 2, end = 3, lo = 3, hi = 4),
          roi_type = "call", roi_label_confidence = "uncertain",
          roi_is_complete = "incomplete", roi_comment = "x",
          stringsAsFactors = FALSE)
  )
  out <- .apply_roi_metadata(
    rois, id = 2, label = "owl", type = "territorial",
    confidence = "certain", complete = "complete", comment = "night call"
  )
  # row 2 updated
  expect_equal(
    unlist(out[2, c("roi_label", "roi_type", "roi_label_confidence",
                    "roi_is_complete", "roi_comment")], use.names = FALSE),
    c("owl", "territorial", "certain", "complete", "night call")
  )
  # geometry of row 2 untouched
  expect_equal(c(out$roi_start[2], out$roi_end[2],
                 out$roi_min_freq[2], out$roi_max_freq[2]), c(2, 3, 3, 4))
  # row 1 untouched entirely
  expect_equal(out[1, ], rois[1, ])
})

# ── .set_roi_bounds (LSA-115 re-selection commit) ──────────────────────────────
test_that("geometry is overwritten on the target row, metadata untouched", {
  rois <- rbind(
    cbind(roi_row("a", start = 0, end = 1, lo = 1, hi = 2),
          roi_type = "song", stringsAsFactors = FALSE),
    cbind(roi_row("b", start = 2, end = 3, lo = 3, hi = 4),
          roi_type = "call", stringsAsFactors = FALSE)
  )
  out <- .set_roi_bounds(rois, id = 1, start = 5, end = 7,
                         min_freq = 8, max_freq = 12)
  expect_equal(c(out$roi_start[1], out$roi_end[1],
                 out$roi_min_freq[1], out$roi_max_freq[1]), c(5, 7, 8, 12))
  expect_equal(out$roi_label[1], "a")    # metadata untouched
  expect_equal(out$roi_type[1], "song")
  expect_equal(out[2, ], rois[2, ])      # other row untouched
})

# ── .create_audio_segment ──────────────────────────────────────────────────────
test_that("audio segment respects bounds, rate and layout", {
  skip_if_not_installed("tuneR")
  sr <- 1000L
  w <- tuneR::Wave(left = sin(seq_len(sr * 2)), samp.rate = sr, bit = 16)
  seg <- .create_audio_segment(w, 0, 1)
  expect_s4_class(seg, "Wave")
  expect_equal(seg@samp.rate, sr)
  expect_equal(length(seg@left), sr)            # ~1 s window
  expect_false(seg@stereo)
})

test_that("stereo input yields a stereo segment", {
  skip_if_not_installed("tuneR")
  sr <- 1000L
  w <- tuneR::Wave(left = sin(seq_len(sr * 2)), right = cos(seq_len(sr * 2)),
                   samp.rate = sr, bit = 16)
  seg <- .create_audio_segment(w, 0.5, 1.5)
  expect_true(seg@stereo)
  expect_equal(length(seg@right), length(seg@left))
})

# ── .safe_cleanup_temp_files ────────────────────────────────────────────────────
test_that("cleanup removes stale wavs but keeps the current one", {
  d <- file.path(tempdir(), paste0("seg_clean_", as.integer(runif(1, 1, 1e6))))
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  keep <- file.path(d, "current.wav")
  drop <- file.path(d, "old.wav")
  file.create(keep, drop)
  .safe_cleanup_temp_files(d, current_file = keep)
  expect_true(file.exists(keep))
  expect_false(file.exists(drop))
})

test_that("cleanup is a no-op on a missing directory", {
  expect_silent(.safe_cleanup_temp_files(NULL))
  expect_silent(.safe_cleanup_temp_files(file.path(tempdir(), "does_not_exist_xyz")))
})

# ── .extract_acoustic_measurements (LSA-14 by-name acoustat selection) ─────────
test_that("acoustic measurements select acoustat percentiles by name", {
  skip_if_not_installed("tuneR")
  skip_if_not_installed("seewave")
  set.seed(1)
  sr <- 22050L
  tt <- seq_len(sr * 2L) / sr
  # 3 kHz tone + light noise, in a band the ruler will cover (1-8 kHz).
  w <- tuneR::Wave(
    left = round(2000 * (sin(2 * pi * 3000 * tt) + 0.2 * rnorm(length(tt)))),
    samp.rate = sr, bit = 16
  )
  ruler <- list(roi_start = 0.2, roi_end = 1.5,
                roi_min_freq = 1, roi_max_freq = 8)
  res <- .extract_acoustic_measurements(w, ruler, wl = 512, ovlp = 50)
  # The four percentiles are present, named and numeric (by-name selection).
  expect_named(res$ac_stats, c("t10", "t90", "f10", "f90"))
  expect_true(all(vapply(res$ac_stats, is.numeric, logical(1))))
  expect_true(res$ac_stats$t90 > res$ac_stats$t10)
  expect_true(res$ac_stats$f90 > res$ac_stats$f10)
  # Dominant frequency recovers the 3 kHz tone (kHz).
  expect_equal(res$dom_freq, 3, tolerance = 0.2)
})

# ── .format_soundscape_metadata (LSA-111 metadata panel) ───────────────────────
test_that("soundscape metadata formatter returns a placeholder when nothing is loaded", {
  out <- .format_soundscape_metadata(NULL, NULL)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("Field", "Value"))
  expect_equal(nrow(out), 1L)
  expect_equal(out$Value, "No soundscape loaded")
})

test_that("soundscape metadata formatter derives WAV fields and the file/path", {
  skip_if_not_installed("tuneR")
  sr <- 8000L
  w <- tuneR::Wave(left = rep(0, sr), samp.rate = sr, bit = 16) # mono, 1 s
  out <- .format_soundscape_metadata(w, "/data/site_A/rec001.wav")
  expect_named(out, c("Field", "Value"))
  val <- stats::setNames(out$Value, out$Field)
  expect_equal(val[["File"]], "rec001.wav")
  expect_equal(val[["Path"]], "/data/site_A/rec001.wav")
  expect_equal(val[["Duration (s)"]], "1.000")
  expect_equal(val[["Sample rate (Hz)"]], "8000")
  expect_equal(val[["Bit depth"]], "16")
  expect_equal(val[["Channels"]], "Mono (1)")
  expect_equal(val[["Samples"]], as.character(sr))
})

test_that("soundscape metadata formatter reports stereo and a NULL path", {
  skip_if_not_installed("tuneR")
  sr <- 4000L
  w <- tuneR::Wave(
    left = rep(0, sr), right = rep(0, sr), samp.rate = sr, bit = 24
  )
  out <- .format_soundscape_metadata(w, NULL)
  val <- stats::setNames(out$Value, out$Field)
  expect_equal(val[["Channels"]], "Stereo (2)")
  expect_equal(val[["Bit depth"]], "24")
  expect_true(is.na(val[["File"]]))
  expect_true(is.na(val[["Path"]]))
})

# ── .select_template_rois (LSA-110 template auto-export) ───────────────────────
tmpl_rois <- function() {
  data.frame(
    roi_label = c("song", "call", .ROI_NO_SOI_LABEL, "buzz"),
    roi_start = c(0, 1, 0, NA),                       # last row has NA bound
    roi_end = c(1, 2, 5, 3),
    roi_min_freq = c(1, 1, 0, 2), roi_max_freq = c(5, 6, 10, 8),
    roi_comment = c("template", "Song TEMPLATE", "template", "template"),
    stringsAsFactors = FALSE
  )
}

test_that("template selection matches the comment marker case-insensitively", {
  out <- .select_template_rois(tmpl_rois())
  # rows 1 & 2 qualify; row 3 is a no_soi sentinel; row 4 has an NA time bound
  expect_equal(nrow(out), 2L)
  expect_equal(out$roi_label, c("song", "call"))
})

test_that("template selection returns an empty frame when none match", {
  rois <- tmpl_rois()
  rois$roi_comment <- c("notes", NA, "x", "y")
  expect_equal(nrow(.select_template_rois(rois)), 0L)
  expect_equal(nrow(.select_template_rois(NULL)), 0L)
})
