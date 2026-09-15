#' DuckDB validations backend (VBO-05; push plan §4)
#'
#' @description Transactional persistence for the `validations` table written by
#'   [validate_by_overlap()]. Mirrors [_detections_duckdb.R]:
#'   connect / ensure-schema / **upsert-by-detection_id** / read / count. Only
#'   **TP/FP** rows (which carry a `detection_id`) are stored; FN rows stay in
#'   the returned frame + the deprecated CSV (VBO-05). Re-validating a detection
#'   replaces its row rather than duplicating it.
#'
#' @keywords internal
#' @noRd

.validations_duckdb_connect <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  .validations_duckdb_ensure_schema(con)
  con
}

.validations_duckdb_ensure_schema <- function(con) {
  spec <- .validation_schema_spec()
  cols <- vapply(names(spec), function(n) {
    pk <- if (n == "detection_id") " PRIMARY KEY" else ""
    sprintf("%s %s%s", n, .duckdb_type(spec[[n]]), pk)
  }, character(1))
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE IF NOT EXISTS validations (%s, _created_at TIMESTAMP DEFAULT now());",
    paste(cols, collapse = ", ")
  ))
  for (n in names(spec)) {
    DBI::dbExecute(con, sprintf(
      "ALTER TABLE validations ADD COLUMN IF NOT EXISTS %s %s;",
      n, .duckdb_type(spec[[n]])
    ))
  }
  invisible(con)
}

.validations_duckdb_read <- function(con) {
  .check_schema_version(con, "validations")          # AFL-19: read-time check
  cols <- paste(names(.validation_schema_spec()), collapse = ", ")
  df <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s FROM validations ORDER BY detection_id;", cols
  ))
  .coerce_validations(df)
}

# Upsert TP/FP validations by detection_id in one transaction (idempotent). Rows
# without a detection_id (e.g. FN) are dropped here by contract — the caller
# keeps them in the returned frame. When `replace = TRUE` the table is truncated
# first.
.validations_duckdb_upsert <- function(con, validations_df, replace = FALSE) {
  df <- .coerce_validations(validations_df)
  df <- df[!is.na(df$detection_id), , drop = FALSE]
  DBI::dbWithTransaction(con, {
    if (replace) DBI::dbExecute(con, "DELETE FROM validations;")
    if (nrow(df) > 0) {
      if (!replace) {
        DBI::dbExecute(con, "DELETE FROM validations WHERE detection_id IN (?);",
                       params = list(df$detection_id))
      }
      DBI::dbAppendTable(con, "validations", df)
    }
  })
  .stamp_schema_version(con)                         # AFL-19: write-time stamp
  invisible(df)
}

.validations_duckdb_count <- function(con) {
  as.integer(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM validations;")$n[1])
}

#' Export a DuckDB validations table to CSV (portability converter)
#'
#' @description Store-level utility for the validations store, exported for
#'   advanced use (not part of the everyday pipeline). The sanctioned way to get
#'   a CSV out of the validations store.
#'   Mirrors [export_detections_duckdb_to_csv()] and completes the DuckDB-backend
#'   export family (G2 in plan-log-2026-06-22-001). Only TP/FP rows are stored
#'   (FN rows have no `detection_id` and are excluded from DuckDB persistence per
#'   VBO-05); the exported CSV reflects this.
#'
#' @param duckdb_path source validations `.duckdb` file.
#' @param csv_path destination CSV path.
#' @param overwrite overwrite an existing CSV. Default `FALSE`.
#' @param columns optional character vector subset of columns (default all).
#' @return invisibly, the exported data.frame.
#' @keywords internal
#' @name export_validations_duckdb_to_csv
#' @export
export_validations_duckdb_to_csv <- function(duckdb_path, csv_path,
                                              overwrite = FALSE, columns = NULL) {
  if (file.exists(csv_path) && !overwrite) {
    stop("Destination CSV exists; pass overwrite = TRUE to replace it.")
  }
  con <- .validations_duckdb_connect(duckdb_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  df <- .validations_duckdb_read(con)
  if (!is.null(columns)) df <- df[, columns, drop = FALSE]
  utils::write.csv(df, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(df)
}
