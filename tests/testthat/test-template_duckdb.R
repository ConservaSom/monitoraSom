# Contract tests for the refactored template manifest layer
# (_schema_templates.R + _template_duckdb.R), the shared artifacts of the
# template trio (ERC-101/102 / FTM-102). No golden from an original function:
# this round introduces the manifest to replace filename-encoding, so these
# tests assert the design decisions directly:
#   - ERC-101/FTM-102: canonical template_* schema is the single source of truth.
#   - ERC-104: .template_id is a short, stable hash over the identity tuple.
#   - ERC-102: upsert is idempotent by template_id; CSV only via the converter.
#   - FTM-01/02: .template_cut_name = <soundscape>_<label>_<roi_type>_<id>.wav.
#
# Run from the project root:
#   Rscript -e "testthat::test_dir('R/refactored/tests/testthat')"

skip_if_db_missing <- function() {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("digest")
}

mk_template <- function(id, label = "Sp aa", mode = "standalone_audio") {
  df <- .schema_templates(1L)
  df$template_id <- id
  df$template_label <- label
  df$template_mode <- mode
  df$template_start <- 1.0
  df$template_end <- 2.0
  df$template_wl <- 1024L
  df$template_ovlp <- 50L
  df$template_channel <- "left"
  df
}

# --- schema --------------------------------------------------------------

test_that("ERC-101/FTM-102: schema is the canonical single source of truth", {
  spec <- .template_schema_spec()
  # The columns the trio agreed on (ERC-101 table + ERC-103 template_mode).
  expect_true(all(c(
    "template_id", "template_path", "template_file", "template_label",
    "template_mode", "template_start", "template_end", "template_min_freq",
    "template_max_freq", "template_wl", "template_ovlp", "template_sample_rate",
    "template_pitch_shift", "template_channel", "template_sha256",
    "origin_soundscape_path", "origin_soundscape_file",
    "origin_soundscape_sha256", "roi_type", "roi_user",
    "roi_input_timestamp", "roi_comment"
  ) %in% names(spec)))
  # Provenance columns carry the origin_ prefix (FR-103, no clash downstream).
  expect_true(all(grepl("^origin_soundscape_",
                        grep("soundscape", names(spec), value = TRUE))))
})

test_that(".schema_templates / .coerce_templates respect order and types", {
  e <- .schema_templates(0L)
  expect_identical(names(e), names(.template_schema_spec()))
  expect_equal(nrow(e), 0L)

  # Coercion keeps only canonical columns, in canonical order, typed.
  raw <- data.frame(template_id = "x", template_wl = "1024",
                    junk = "drop me", stringsAsFactors = FALSE)
  out <- .coerce_templates(raw)
  expect_identical(names(out), names(.template_schema_spec()))
  expect_false("junk" %in% names(out))
  expect_type(out$template_wl, "integer")
})

# --- identity (ERC-104) --------------------------------------------------

test_that("ERC-104: template_id is short, stable and identity-driven", {
  skip_if_db_missing()
  a <- .template_id("sha", "/p.wav", 1, 2, "left", 1024L, 50L, "Sp aa")
  b <- .template_id("sha", "/p.wav", 1, 2, "left", 1024L, 50L, "Sp aa")
  expect_identical(a, b)                       # idempotent
  expect_equal(nchar(a), 12L)                  # short
  # Any identity component changes the id.
  expect_false(identical(a,
    .template_id("sha", "/p.wav", 1, 2, "right", 1024L, 50L, "Sp aa")))
  expect_false(identical(a,
    .template_id("sha", "/p.wav", 1, 2.5, "left", 1024L, 50L, "Sp aa")))
  # Falls back to path when the source hash is NA (still deterministic).
  na1 <- .template_id(NA_character_, "/p.wav", 1, 2, "left", 1024L, 50L, "Sp")
  na2 <- .template_id(NA_character_, "/p.wav", 1, 2, "left", 1024L, 50L, "Sp")
  expect_identical(na1, na2)
})

test_that("FTM-01/02: cut name carries soundscape, label, roi_type, id", {
  nm <- .template_cut_name("W04_20231215_080000.WAV", "Myiothlypis flaveola",
                           "abc123def456", roi_type = "bird - song")
  expect_match(nm, "^W04_20231215_080000_Myiothlypis_flaveola_bird_-_song_abc123def456\\.wav$")
  # roi_type omitted when NA/empty.
  nm2 <- .template_cut_name("W04.WAV", "Sp", "id1", roi_type = NA)
  expect_identical(nm2, "W04_Sp_id1.wav")
  nm3 <- .template_cut_name("W04.WAV", "Sp", "id1", roi_type = "")
  expect_identical(nm3, "W04_Sp_id1.wav")
})

# --- manifest backend (ERC-102) ------------------------------------------

test_that("ERC-102: manifest round-trips through DuckDB in canonical schema", {
  skip_if_db_missing()
  dbp <- tempfile(fileext = ".duckdb")
  con <- .template_duckdb_connect(dbp)
  on.exit({ DBI::dbDisconnect(con, shutdown = TRUE); unlink(dbp) })

  .template_duckdb_upsert(con, mk_template("id_a"))
  back <- .template_duckdb_read(con)
  expect_identical(names(back), names(.template_schema_spec()))
  expect_equal(.template_duckdb_count(con), 1L)
  expect_identical(back$template_id, "id_a")
  expect_type(back$template_wl, "integer")
})

test_that("ERC-102: upsert is idempotent by template_id (no duplicates)", {
  skip_if_db_missing()
  dbp <- tempfile(fileext = ".duckdb")
  con <- .template_duckdb_connect(dbp)
  on.exit({ DBI::dbDisconnect(con, shutdown = TRUE); unlink(dbp) })

  .template_duckdb_upsert(con, mk_template("id_a", label = "first"))
  .template_duckdb_upsert(con, mk_template("id_a", label = "second"))
  expect_equal(.template_duckdb_count(con), 1L)         # replaced, not duplicated
  expect_identical(.template_duckdb_read(con)$template_label, "second")

  .template_duckdb_upsert(con, mk_template("id_b"))
  expect_equal(.template_duckdb_count(con), 2L)
})

test_that("ERC-102: CSV is produced only via the converter", {
  skip_if_db_missing()
  dbp <- tempfile(fileext = ".duckdb")
  con <- .template_duckdb_connect(dbp)
  .template_duckdb_upsert(con, mk_template("id_a"))
  DBI::dbDisconnect(con, shutdown = TRUE)

  csv <- tempfile(fileext = ".csv")
  export_templates_duckdb_to_csv(dbp, csv)
  expect_true(file.exists(csv))
  back <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_identical(back$template_id, "id_a")
  # Refuses to clobber an existing CSV without overwrite.
  expect_error(export_templates_duckdb_to_csv(dbp, csv), "overwrite")
  unlink(c(dbp, csv))
})
