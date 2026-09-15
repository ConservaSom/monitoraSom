#' Schema version stamping for the DuckDB stores (AFL-19)
#'
#' @description Single source of truth for the **schema version** of the
#'   monitoraSom DuckDB stores, plus the write-time stamp and read-time check.
#'   Before this, no store recorded which schema layout it was written with, so a
#'   reader could not tell a current store from a stale one (AFL-19, source BS-16,
#'   premise corrected: the gap was greenfield — *no* table carried a version).
#'
#'   **Mechanism (store-agnostic).** The version lives in a tiny side table,
#'   `_schema_meta(schema_version INTEGER)`, one row per store — **not** a column
#'   on the data tables, so the standard data schemas
#'   ([.detection_schema_spec()] etc.) and their goldens are unchanged. A store is
#'   stamped at **write** time and checked at **read** time:
#'   - absent `_schema_meta` → a legacy/unversioned store: stay **silent**
#'     (forward-compatible, no churn on existing stores/fixtures);
#'   - present and equal to [.SCHEMA_VERSION] → silent;
#'   - present and different → **warn** (migration is a future hook).
#'
#'   **Rollout (complete).** Wired into all five DuckDB stores — detections
#'   ([_detections_duckdb.R], the original reference), rois ([_roi_duckdb.R]),
#'   soundscapes metadata ([_metadata_duckdb.R]), templates
#'   ([_template_duckdb.R]) and validations ([_validations_duckdb.R]) — each with
#'   the same two calls (`.stamp_schema_version` on write, `.check_schema_version`
#'   on read). `_metadata_duckdb.R` previously carried an unrelated, unread
#'   `_schema_version` *column* on the `soundscapes` table (`DEFAULT 2`, dead
#'   code); it was removed in the same change to avoid two different
#'   "schema_version" concepts coexisting in one store.
#'
#' @keywords internal
#' @noRd

# Greenfield: every store starts at version 1. Bump on a breaking schema change.
.SCHEMA_VERSION <- 1L

# Write-time: ensure the side table and stamp the current version (idempotent,
# single row). Call inside the store's write/upsert path.
.stamp_schema_version <- function(con, version = .SCHEMA_VERSION) {
  DBI::dbExecute(con,
    "CREATE TABLE IF NOT EXISTS _schema_meta (schema_version INTEGER);")
  DBI::dbExecute(con, "DELETE FROM _schema_meta;")
  DBI::dbExecute(con, "INSERT INTO _schema_meta VALUES (?);",
                 params = list(as.integer(version)))
  invisible(con)
}

# Read-time: return the stored version (NA for a legacy store with no
# `_schema_meta`) and warn on a mismatch with the current `.SCHEMA_VERSION`.
.check_schema_version <- function(con, store = "store") {
  if (!("_schema_meta" %in% DBI::dbListTables(con))) return(NA_integer_)
  v <- DBI::dbGetQuery(con,
    "SELECT schema_version FROM _schema_meta LIMIT 1;")$schema_version
  v <- if (length(v)) as.integer(v[[1]]) else NA_integer_
  if (!is.na(v) && v != .SCHEMA_VERSION) {
    warning(sprintf(
      "The %s store was written with schema version %d, but this monitoraSom ",
      store, v),
      sprintf("expects version %d. Re-create the store from the current ",
              .SCHEMA_VERSION),
      "pipeline if you hit unexpected column errors.", call. = FALSE)
  }
  v
}
