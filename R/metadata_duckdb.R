#' DuckDB metadata backend and CSV <-> DuckDB converters (FSM-209 / FSM-210)
#'
#' @description DuckDB is the intended primary cache backend; CSV remains a
#'   supported import/export format (FSM-209, alternative (b): DuckDB with CSV
#'   backward compatibility). This file holds the connection/schema helpers, an
#'   upsert-by-path writer, the metadata reader, the inline `metadata_errors`
#'   table writer (FSM-214), and the public bidirectional converters
#'   (FSM-210). The DuckDB table carries the standard FSM-211 columns; dynamic
#'   user filename fields (FSM-218) are CSV-only this round.
#'
#' @keywords internal
#' @noRd

# DuckDB column type for each canonical schema storage type.
.duckdb_type <- function(rtype) {
  switch(rtype,
    character = "VARCHAR",
    integer   = "INTEGER",
    numeric   = "DOUBLE",
    POSIXct   = "TIMESTAMP",
    "VARCHAR"
  )
}

# Open a connection and ensure the schema exists (FSM-209). Caller closes it.
.duckdb_connect <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  .duckdb_ensure_schema(con)
  con
}

.duckdb_ensure_schema <- function(con) {
  spec <- .soundscape_schema_spec()
  cols <- vapply(names(spec), function(n) {
    pk <- if (n == "soundscape_path") " PRIMARY KEY" else ""
    sprintf("%s %s%s", n, .duckdb_type(spec[[n]]), pk)
  }, character(1))
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE IF NOT EXISTS soundscapes (%s, _created_at TIMESTAMP DEFAULT now());",
    paste(cols, collapse = ", ")
  ))
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS metadata_errors (",
    "soundscape_path VARCHAR, error_class VARCHAR,",
    "error_message VARCHAR, timestamp VARCHAR);"
  ))
  invisible(con)
}

# Read the canonical metadata table back as a data.frame.
.duckdb_read_metadata <- function(con) {
  .check_schema_version(con, "soundscapes")          # AFL-19: read-time check
  cols <- paste(names(.soundscape_schema_spec()), collapse = ", ")
  DBI::dbGetQuery(con, sprintf("SELECT %s FROM soundscapes;", cols))
}

# Upsert by path: delete matching paths, then append (transactional).
.duckdb_upsert_metadata <- function(con, df) {
  keep <- intersect(names(.soundscape_schema_spec()), names(df))
  df <- df[, keep, drop = FALSE]
  DBI::dbWithTransaction(con, {
    if (nrow(df) > 0) {
      paths <- df$soundscape_path
      DBI::dbExecute(con,
        "DELETE FROM soundscapes WHERE soundscape_path IN (?);",
        params = list(paths))
      DBI::dbAppendTable(con, "soundscapes", df)
    }
  })
  .stamp_schema_version(con)                         # AFL-19: write-time stamp
  invisible(df)
}

# Upsert structured error rows into metadata_errors (FSM-214): delete existing
# rows for the affected paths, then append — so re-runs replace a file's error
# row instead of accumulating duplicates (matches the CSV backend's overwrite).
.duckdb_write_errors <- function(con, err_df) {
  if (nrow(err_df) == 0) return(invisible(err_df))
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con,
      "DELETE FROM metadata_errors WHERE soundscape_path IN (?);",
      params = list(err_df$soundscape_path))
    DBI::dbAppendTable(con, "metadata_errors", err_df)
  })
  invisible(err_df)
}

#' Migrate a legacy CSV metadata cache into a DuckDB database (FSM-210)
#'
#' @description Store-level utility for the metadata store, exported for
#'   advanced use (not part of the everyday pipeline).
#'
#' Reads the old CSV, renames `soundscape_bitrate` -> `soundscape_bitdepth`
#' (FSM-13), fills missing standard columns with `NA`, and upserts into
#' DuckDB. Newly introduced columns left `NA` are reported with a warning
#' instructing the user to re-run [fetch_soundscape_metadata()] to populate
#' them from the recordings (FSM-210 user decision).
#'
#' **Known limitation (G6 in plan-log-2026-06-22-001):** This migration does
#' not re-read the original WAV files, so `soundscape_sha256` stays `NA` for
#' all migrated rows. Re-run [fetch_soundscape_metadata()] after migration to
#' populate the integrity hashes from the recordings. A "deep migration" that
#' re-reads WAV headers is deferred — it would require the original corpus to
#' be present and is rarely needed for legacy CSV caches.
#'
#' @param csv_path path to the legacy CSV cache.
#' @param duckdb_path destination `.duckdb` file.
#' @param overwrite overwrite an existing destination. Default `FALSE`.
#' @return invisibly, the migrated data.frame.
#' @keywords internal
#' @name migrate_metadata_csv_to_duckdb
#' @export
migrate_metadata_csv_to_duckdb <- function(csv_path, duckdb_path,
                                           overwrite = FALSE) {
  if (file.exists(duckdb_path) && !overwrite) {
    stop("Destination exists; pass overwrite = TRUE to replace it.")
  }
  if (file.exists(duckdb_path) && overwrite) unlink(duckdb_path)
  df <- utils::read.csv(csv_path, stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8")
  if ("soundscape_bitrate" %in% names(df) &&
      !"soundscape_bitdepth" %in% names(df)) {
    names(df)[names(df) == "soundscape_bitrate"] <- "soundscape_bitdepth"
  }
  missing_cols <- .validate_cache_schema(df)
  for (col in missing_cols) df[[col]] <- NA
  if (length(missing_cols) > 0) {
    warning(sprintf(
      paste("Columns filled with NA during migration: %s.",
            "Re-run fetch_soundscape_metadata() to populate them from the",
            "recordings."),
      paste(missing_cols, collapse = ", ")
    ))
  }
  con <- .duckdb_connect(duckdb_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .duckdb_upsert_metadata(con, df)
  invisible(df)
}

#' Export a DuckDB metadata table to CSV (FSM-210)
#'
#' @description Store-level utility for the metadata store, exported for
#'   advanced use (not part of the everyday pipeline).
#'
#' @param duckdb_path source `.duckdb` file.
#' @param csv_path destination CSV path.
#' @param overwrite overwrite an existing CSV. Default `FALSE`.
#' @param columns optional character vector subset of columns (default all).
#' @return invisibly, the exported data.frame.
#' @keywords internal
#' @name export_metadata_duckdb_to_csv
#' @export
export_metadata_duckdb_to_csv <- function(duckdb_path, csv_path,
                                          overwrite = FALSE, columns = NULL) {
  if (file.exists(csv_path) && !overwrite) {
    stop("Destination CSV exists; pass overwrite = TRUE to replace it.")
  }
  con <- .duckdb_connect(duckdb_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  df <- .duckdb_read_metadata(con)
  if (!is.null(columns)) df <- df[, columns, drop = FALSE]
  utils::write.csv(df, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(df)
}
