# Tests for the DuckDB schema-version mechanism (_schema_version.R, AFL-19).
# The version lives in a side table `_schema_meta` (not a data column), stamped on
# write and checked on read: absent = legacy/silent, equal = silent, different =
# warn. Wired into the detections store as the reference.

mem_con <- function() DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

test_that("a legacy store with no _schema_meta reads silently (NA)", {
  con <- mem_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_silent(v <- .check_schema_version(con, "detections"))
  expect_true(is.na(v))
})

test_that("stamp then check round-trips the current version silently", {
  con <- mem_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  .stamp_schema_version(con)
  expect_silent(v <- .check_schema_version(con, "detections"))
  expect_identical(v, .SCHEMA_VERSION)
})

test_that("stamp is idempotent: a single version row", {
  con <- mem_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  .stamp_schema_version(con)
  .stamp_schema_version(con)
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM _schema_meta;")$n
  expect_equal(n, 1L)
})

test_that("a stored version different from the current one warns", {
  con <- mem_con(); on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  .stamp_schema_version(con, version = .SCHEMA_VERSION + 1L)
  expect_warning(.check_schema_version(con, "detections"), "schema version")
})
