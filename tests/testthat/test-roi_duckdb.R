# Contract tests for the refactored DuckDB ROI persistence layer
# (_schema_rois.R + _roi_duckdb.R). There is no golden from an original
# function here: the alt app used SQLite, and this round replaces it with a
# separate DuckDB store (LSA-116). These tests assert the LSA decisions
# directly:
#   - LSA-19: soundscape_path is the durable key (basename collisions safe).
#   - LSA-11: save() is a transactional delete+append (per-path upsert).
#   - LSA-21: roi_pitch_shift round-trips as integer.
#   - LSA-23: no_soi sentinel + status classification.
#   - LSA-12: create-if-missing, no legacy migration.
#
# Run from the project root:
#   Rscript -e "testthat::test_dir('R/refactored/tests/testthat')"

skip_if_db_missing <- function() {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("duckdb")
}

# Build a single canonical ROI row. roi_pitch_shift defaults to a numeric value
# so the integer coercion (LSA-21) is exercised.
mk_roi <- function(path, file = basename(path), label = "bird",
                   start = 0, pitch = -2, channel = "left") {
  data.frame(
    soundscape_path = path, soundscape_file = file, roi_user = "tester",
    roi_input_timestamp = sprintf("2026-05-28 10:%02d:00", start %% 60),
    roi_label = label, roi_start = start, roi_end = start + 1,
    roi_min_freq = 0, roi_max_freq = 10,
    roi_type = NA_character_, roi_label_confidence = NA_character_,
    roi_is_complete = NA_character_, roi_comment = NA_character_,
    roi_wl = 512L, roi_ovlp = 50L, roi_sample_rate = 48000L,
    roi_pitch_shift = pitch, roi_channel = channel, stringsAsFactors = FALSE
  )
}

with_con <- function(code) {
  dbp <- tempfile(fileext = ".duckdb")
  con <- .roi_duckdb_connect(dbp)
  on.exit({
    DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(dbp)
  })
  code(con)
}

# ── Schema ───────────────────────────────────────────────────────────────────
test_that("empty skeleton has the canonical columns and types", {
  e <- .schema_rois(0L)
  expect_equal(names(e), names(.roi_schema_spec()))
  expect_equal(nrow(e), 0L)
  expect_false("id" %in% names(e))                       # LSA-19: no surrogate id
  expect_type(.schema_rois(1L)$roi_pitch_shift, "integer")  # LSA-21
})

test_that(".coerce_rois drops stray columns and fixes types", {
  raw <- mk_roi("/x/REC.wav", pitch = -4)
  raw$id <- 99L                       # legacy column must be dropped
  out <- .coerce_rois(raw)
  expect_equal(names(out), names(.roi_schema_spec()))
  expect_type(out$roi_pitch_shift, "integer")
  expect_identical(out$roi_pitch_shift, -4L)
})

test_that("no_soi sentinel helpers follow the LSA-23 contract", {
  expect_identical(.ROI_NO_SOI_LABEL, "no signals of interest")
  expect_equal(.is_no_soi_label(c("bird", .ROI_NO_SOI_LABEL, NA)),
               c(FALSE, TRUE, FALSE))
})

# ── Persistence round-trip ─────────────────────────────────────────────────────
test_that("save/read round-trip preserves rows and integer pitch (LSA-21)", {
  skip_if_db_missing()
  with_con(function(con) {
    p <- "/data/siteA/REC001.wav"
    .roi_duckdb_save(con, rbind(mk_roi(p, label = "bird", start = 0),
                                mk_roi(p, label = "frog", start = 1)), p)
    back <- .roi_duckdb_read(con, p)
    expect_equal(nrow(back), 2L)
    expect_equal(names(back), names(.roi_schema_spec()))
    expect_type(back$roi_pitch_shift, "integer")
    expect_equal(.roi_duckdb_count(con, p), 2L)
  })
})

test_that("roi_channel round-trips through DuckDB (LSA-109)", {
  skip_if_db_missing()
  expect_true("roi_channel" %in% names(.roi_schema_spec()))
  with_con(function(con) {
    p <- "/data/siteA/STEREO.wav"
    .roi_duckdb_save(con, rbind(mk_roi(p, start = 0, channel = "left"),
                                mk_roi(p, start = 1, channel = "right")), p)
    back <- .roi_duckdb_read(con, p)
    expect_equal(back$roi_channel, c("left", "right"))
  })
})

test_that("ensure-columns adds roi_channel to a legacy table (LSA-109)", {
  skip_if_db_missing()
  dbp <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = dbp)
  on.exit({ DBI::dbDisconnect(con, shutdown = TRUE); unlink(dbp) })
  # Simulate a pre-LSA-109 table: every canonical column EXCEPT roi_channel.
  spec <- .roi_schema_spec()
  spec_old <- spec[names(spec) != "roi_channel"]
  cols <- vapply(names(spec_old), function(n) {
    sprintf("%s %s", n, .duckdb_type(spec_old[[n]]))
  }, character(1))
  DBI::dbExecute(con, sprintf("CREATE TABLE rois (%s);", paste(cols, collapse = ", ")))
  expect_false("roi_channel" %in% DBI::dbListFields(con, "rois"))
  # Re-running ensure-schema must ALTER the table to add the missing column.
  .roi_duckdb_ensure_schema(con)
  expect_true("roi_channel" %in% DBI::dbListFields(con, "rois"))
})

test_that("create-if-missing yields an empty readable table (LSA-12)", {
  skip_if_db_missing()
  with_con(function(con) {
    expect_equal(.roi_duckdb_count(con, "/nope.wav"), 0L)
    expect_equal(nrow(.roi_duckdb_read(con, "/nope.wav")), 0L)
  })
})

# ── LSA-19: soundscape_path is the durable key ─────────────────────────────────
test_that("same basename in different dirs stays distinct, re-save is isolated", {
  skip_if_db_missing()
  with_con(function(con) {
    pA <- "/data/siteA/REC001.wav"
    pB <- "/data/siteB/REC001.wav"   # same basename, different recording
    .roi_duckdb_save(con, rbind(mk_roi(pA, start = 0), mk_roi(pA, start = 1)), pA)
    .roi_duckdb_save(con, mk_roi(pB, start = 0), pB)
    expect_equal(.roi_duckdb_count(con, pA), 2L)
    expect_equal(.roi_duckdb_count(con, pB), 1L)

    # Re-saving A (per-path upsert, LSA-11) must not touch B's identical basename.
    .roi_duckdb_save(con, mk_roi(pA, start = 0), pA)
    expect_equal(.roi_duckdb_count(con, pA), 1L)
    expect_equal(.roi_duckdb_count(con, pB), 1L)
  })
})

# ── LSA-23: status classification ──────────────────────────────────────────────
test_that("statuses classify unsegmented / segmented / no_soi by path", {
  skip_if_db_missing()
  with_con(function(con) {
    pSeg <- "/data/seg.wav"
    pNo  <- "/data/no.wav"
    pUn  <- "/data/unsegmented.wav"
    .roi_duckdb_save(con, rbind(mk_roi(pSeg, label = "bird", start = 0),
                                mk_roi(pSeg, label = .ROI_NO_SOI_LABEL, start = 1)),
                     pSeg)                               # mixed -> segmented
    .roi_duckdb_save(con, mk_roi(pNo, label = .ROI_NO_SOI_LABEL), pNo)  # all sentinel
    st <- .roi_duckdb_statuses(con, c(pSeg, pNo, pUn))
    expect_equal(st, c("segmented", "no_soi", "unsegmented"))
  })
})

test_that("two sentinel rows still classify as no_soi (LSA-04 definition)", {
  skip_if_db_missing()
  with_con(function(con) {
    p <- "/data/double_sentinel.wav"
    .roi_duckdb_save(con, rbind(mk_roi(p, label = .ROI_NO_SOI_LABEL, start = 0),
                                mk_roi(p, label = .ROI_NO_SOI_LABEL, start = 1)), p)
    expect_equal(.roi_duckdb_statuses(con, p), "no_soi")
  })
})
