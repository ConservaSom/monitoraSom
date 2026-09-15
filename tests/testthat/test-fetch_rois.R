# Golden + divergence tests for the refactored fetch_rois (FR).
# Goldens (R/sandbox/fetch_rois.R) record the ORIGINAL's behaviour faithfully,
# bugs included (FR-02 phantom rows, FR-03 silent NA, legacy 19-col schema with
# roi_path/roi_file and no roi_channel). The intentional divergences (see
# tests/golden/fetch_rois/NOTES.md) are DERIVED/asserted here, not compared
# verbatim.
#
# Run from the project root:
#   Rscript -e "testthat::test_dir('R/refactored/tests/testthat')"

skip_if_deps_missing <- function() {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("fs")
}

fixture_db <- system.file("extdata",
                        "template_trio", "rois.duckdb", package = "monitoraSom")
malformed_dir <- system.file("extdata",
                           "fetch_rois", "malformed", package = "monitoraSom")
legacy_dir <- system.file("extdata",
                        "fetch_rois", "fixture_legacy", package = "monitoraSom")

# Always read from a temp copy: never mutate the committed fixture (the DuckDB
# reader additively ensures the schema, which opens the file read-write).
fixture_copy <- function() {
  dst <- tempfile(fileext = ".duckdb")
  file.copy(fixture_db, dst)
  dst
}

# --- FR-101/102: DuckDB store + canonical schema -------------------------

test_that("FR-101: reads the DuckDB ROI store (single file)", {
  skip_if_deps_missing()
  db <- fixture_copy(); on.exit(unlink(db))
  df <- fetch_rois(db, source = "duckdb")
  # Golden FR-G2 fixed the fixture at 161 ROIs.
  expect_equal(nrow(df), 161L)
})

test_that("FR-101: unifies several .duckdb files in a directory", {
  skip_if_deps_missing()
  d <- tempfile(); dir.create(d)
  file.copy(fixture_db, file.path(d, "a.duckdb"))
  file.copy(fixture_db, file.path(d, "b.duckdb"))
  on.exit(unlink(d, recursive = TRUE))
  df <- fetch_rois(d, source = "duckdb")
  expect_equal(nrow(df), 322L)            # 2 x 161
})

test_that("FR-102: output is exactly the canonical 18-column schema", {
  skip_if_deps_missing()
  db <- fixture_copy(); on.exit(unlink(db))
  df <- fetch_rois(db, source = "duckdb")
  # Diverges from the golden 19-col legacy frame: drops roi_path/roi_file,
  # adds roi_channel.
  expect_identical(names(df), names(.roi_schema_spec()))
  expect_true("roi_channel" %in% names(df))
  expect_false("roi_path" %in% names(df))
  expect_false("roi_file" %in% names(df))
})

# --- AFL-02: opt-in roi_source provenance filter -------------------------

test_that("AFL-02: roi_source filters by provenance (NA = manual)", {
  skip_if_deps_missing()
  db <- tempfile(fileext = ".duckdb"); on.exit(unlink(db))
  mk <- function(sp, label, source, type) {
    r <- .schema_rois(1L)
    r$soundscape_path <- sp; r$soundscape_file <- basename(sp)
    r$roi_label <- label; r$roi_start <- 0; r$roi_end <- 1
    r$roi_type <- type; r$roi_source <- source
    r$roi_input_timestamp <- "2026-01-01 00:00:00"
    r
  }
  con <- .roi_duckdb_connect(db)
  .roi_duckdb_save(con, mk("ss/m.wav", "SpM", "manual", "song"), "ss/m.wav")
  .roi_duckdb_save(con, mk("ss/d.wav", "SpD", "detection", "detection"), "ss/d.wav")
  .roi_duckdb_save(con, mk("ss/n.wav", "SpN", NA_character_, "song"), "ss/n.wav")
  DBI::dbDisconnect(con, shutdown = TRUE)

  expect_equal(nrow(fetch_rois(db, source = "duckdb")), 3L)            # NULL = all
  det <- fetch_rois(db, source = "duckdb", roi_source = "detection")
  expect_equal(det$roi_label, "SpD")
  man <- fetch_rois(db, source = "duckdb", roi_source = "manual")      # manual + NA
  expect_setequal(man$roi_label, c("SpM", "SpN"))
  expect_error(fetch_rois(db, source = "duckdb", roi_source = "bogus"),
               "should be one of")
})

# --- FR-101 (csv legacy/migration) ---------------------------------------

test_that("FR-101: legacy CSV migration coerces to the canonical schema", {
  skip_if_deps_missing()
  df <- fetch_rois(legacy_dir, source = "csv")
  # Same 161 real rows as the golden FR-G2, but in the canonical schema.
  expect_equal(nrow(df), 161L)
  expect_identical(names(df), names(.roi_schema_spec()))
  expect_true("roi_channel" %in% names(df))      # added, NA for legacy rows
  expect_true(all(is.na(df$roi_channel)))
})

# --- FR-02: no phantom NA rows -------------------------------------------

test_that("FR-02: malformed input yields the real rows, no phantom NA rows", {
  skip_if_deps_missing()
  # Golden malformed_edge returned 19 rows (2 real + 17 phantom from the dead
  # valid_entries block). The refactor removes that block -> 2 real rows.
  df <- suppressWarnings(fetch_rois(malformed_dir, source = "csv"))
  expect_equal(nrow(df), 2L)
})

# --- FR-03: parse failures are reported, not silent ----------------------

test_that("FR-03: values that fail to parse are reported via warning", {
  skip_if_deps_missing()
  # The malformed CSV has roi_start = "not_a_number".
  expect_warning(fetch_rois(malformed_dir, source = "csv"),
                 "failed to parse")
})

# --- FR-05: recursive honoured -------------------------------------------

test_that("FR-05: recursive is honoured (was hardcoded TRUE)", {
  skip_if_deps_missing()
  root <- tempfile(); sub <- file.path(root, "nested")
  dir.create(sub, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE))
  roi <- .schema_rois(1L)
  roi$soundscape_path <- "data/x/A.wav"; roi$soundscape_file <- "A.wav"
  roi$roi_label <- "sp"; roi$roi_start <- 0; roi$roi_end <- 1
  utils::write.csv(roi, file.path(sub, "A_roi_x.csv"), row.names = FALSE)

  # Shallow scan: the nested table is invisible -> the original "not found" stop.
  expect_error(fetch_rois(root, source = "csv", recursive = FALSE),
               "No ROI tables found")
  # Recursive scan: finds it.
  df <- fetch_rois(root, source = "csv", recursive = TRUE)
  expect_equal(nrow(df), 1L)
})

# --- FR-06: forward-slash key --------------------------------------------

test_that("FR-06: stored soundscape_path uses forward slashes", {
  skip_if_deps_missing()
  db <- fixture_copy(); on.exit(unlink(db))
  df <- fetch_rois(db, source = "duckdb")
  expect_false(any(grepl("\\\\", df$soundscape_path)))
})

# --- FR-103: optional origin_ provenance join ----------------------------

test_that("FR-103: origin_ provenance columns attach when metadata supplied", {
  skip_if_deps_missing()
  db <- fixture_copy(); on.exit(unlink(db))
  base <- fetch_rois(db, source = "duckdb")

  # Build a minimal soundscape-metadata frame for one of the real paths.
  some_path <- base$soundscape_path[1]
  meta <- .schema_soundscapes(1L)
  meta$soundscape_path <- some_path
  meta$soundscape_file <- basename(some_path)
  meta$soundscape_sha256 <- "deadbeef"

  # Only one path has metadata, so the rest are unmatched (FR-104 warns).
  df <- suppressWarnings(
    fetch_rois(db, source = "duckdb", soundscape_metadata = meta))
  expect_true(all(c("origin_soundscape_path", "origin_soundscape_file",
                    "origin_soundscape_sha256") %in% names(df)))
  hit <- df$soundscape_path == some_path
  expect_true(all(df$origin_soundscape_sha256[hit] == "deadbeef"))
  expect_true(all(is.na(df$origin_soundscape_sha256[!hit])))
})

# --- AUD-26: coverage — corrupt store, metadata-as-path, malformed metadata --

test_that("AUD-26: an unreadable .duckdb among good stores warns and keeps the good rows", {
  skip_if_deps_missing()
  d <- tempfile("rois_dir_"); dir.create(d); on.exit(unlink(d, recursive = TRUE))
  file.copy(fixture_db, file.path(d, "good.duckdb"))
  writeLines("not a database", file.path(d, "broken.duckdb"))
  expect_warning(
    df <- suppressMessages(fetch_rois(d, source = "duckdb", recursive = FALSE)),
    "could not be read")
  expect_gt(nrow(df), 0L)
})

test_that("AUD-26: soundscape_metadata accepts a .duckdb path (connect-and-read branch)", {
  skip_if_deps_missing()
  db <- fixture_copy(); on.exit(unlink(db))
  base <- fetch_rois(db, source = "duckdb")
  some_path <- base$soundscape_path[1]
  meta <- .schema_soundscapes(1L)
  meta$soundscape_path   <- some_path
  meta$soundscape_file   <- basename(some_path)
  meta$soundscape_sha256 <- "deadbeef"
  mdb <- tempfile(fileext = ".duckdb"); on.exit(unlink(mdb), add = TRUE)
  con <- .duckdb_connect(mdb)
  .duckdb_upsert_metadata(con, meta)
  DBI::dbDisconnect(con, shutdown = TRUE)
  df <- suppressWarnings(
    fetch_rois(db, source = "duckdb", soundscape_metadata = mdb))
  hit <- df$soundscape_path == some_path
  expect_true(all(df$origin_soundscape_sha256[hit] == "deadbeef"))
})

test_that("AUD-23: soundscape_metadata missing a consumed column errors", {
  skip_if_deps_missing()
  db <- fixture_copy(); on.exit(unlink(db))
  bad_meta <- data.frame(soundscape_path = "x", soundscape_file = "x.wav",
                         stringsAsFactors = FALSE)  # missing soundscape_sha256
  expect_error(
    fetch_rois(db, source = "duckdb", soundscape_metadata = bad_meta),
    "soundscape_sha256")
})

# --- AFL-06: canonical soundscape_path key normalization -----------------

test_that("AFL-06: .normalize_path_key canonicalizes keys (idempotent)", {
  expect_identical(.normalize_path_key("./recordings/Bcu_1.wav"),
                   "recordings/Bcu_1.wav")        # leading ./ dropped
  expect_identical(.normalize_path_key("soundscapes//W.wav"),
                   "soundscapes/W.wav")            # repeated / collapsed
  expect_identical(.normalize_path_key("recordings\\Bcu_1.wav"),
                   "recordings/Bcu_1.wav")         # backslash -> forward
  # Idempotent: normalizing an already-canonical key is a no-op.
  k <- .normalize_path_key("soundscapes//.//x.wav")
  expect_identical(.normalize_path_key(k), k)
  # Deliberate split from the physical-path helper: .to_forward_slashes keeps a
  # Windows UNC root (\\server -> //server) that .normalize_path_key would
  # collapse, which is why keys and physical paths use different rules.
  expect_identical(.to_forward_slashes("\\\\server\\share\\x.wav"),
                   "//server/share/x.wav")
})

test_that("AFL-06: provenance join matches across cosmetic key differences", {
  skip_if_deps_missing()
  db <- fixture_copy(); on.exit(unlink(db))
  base <- fetch_rois(db, source = "duckdb")
  some_path <- base$soundscape_path[1]              # canonical stored key

  # Metadata carries the SAME recording but with a legacy "./" prefix and "//".
  # Before AFL-06 the metadata side was only backslash-normalized, so this would
  # miss the LEFT JOIN and leave origin_* NA; now both sides share the rule.
  meta <- .schema_soundscapes(1L)
  # F4 note: the fixture key is now workspace-relative, so the cosmetic
  # variants build ON TOP of the relative key (doubling the first character of
  # a relative key would fabricate a leading slash normalization cannot remove).
  meta$soundscape_path   <- paste0("./", sub("/", "//", some_path))
  meta$soundscape_file   <- basename(some_path)
  meta$soundscape_sha256 <- "deadbeef"

  df <- suppressWarnings(
    fetch_rois(db, source = "duckdb", soundscape_metadata = meta))
  hit <- df$soundscape_path == some_path
  expect_true(any(hit))
  expect_true(all(df$origin_soundscape_sha256[hit] == "deadbeef"))
})

# --- FR-104: unmatched-provenance diagnostic -----------------------------

test_that("FR-104: ROIs missing from soundscape_metadata warn", {
  skip_if_deps_missing()
  db <- fixture_copy(); on.exit(unlink(db))

  # Metadata for a path that matches NO ROI → every ROI is unmatched.
  meta <- .schema_soundscapes(1L)
  meta$soundscape_path <- "recordings/does_not_exist.wav"
  meta$soundscape_file <- "does_not_exist.wav"
  meta$soundscape_sha256 <- "deadbeef"

  expect_warning(
    fetch_rois(db, source = "duckdb", soundscape_metadata = meta),
    "no matching recording in soundscape_metadata")
  # Provenance is still attached, all NA (the warning does not abort the join).
  df <- suppressWarnings(
    fetch_rois(db, source = "duckdb", soundscape_metadata = meta))
  expect_true(all(is.na(df$origin_soundscape_sha256)))
})

# --- guards --------------------------------------------------------------

test_that("missing path errors clearly", {
  # CRAN item 7 (F2): NULL now resolves to the canonical rois.duckdb at the
  # project root; an absent default (or non-scalar input) still fails with an
  # actionable message instead of a raw error.
  expect_error(fetch_rois(NULL), "does not exist: rois.duckdb")
  expect_error(fetch_rois(c("a", "b")), "single non-NA character path")
  expect_error(fetch_rois(tempfile(), source = "duckdb"), "does not exist")
})
