#' DuckDB template-template database backend (ERC-102)
#'
#' @description DuckDB is the exclusive persistence/export target for the
#'   template database produced by [export_templates()] (ERC-102: no JSON; CSV
#'   only via the dedicated `.duckdb`->CSV converter below). The template
#'   database is a **separate** `.duckdb` file from both the soundscape-metadata
#'   database and the ROI database (LSA-116, separate-DBs topology), holding a
#'   single `templates` table.
#'
#'   This file mirrors [_roi_duckdb.R] and [_metadata_duckdb.R]:
#'   connection/schema helpers, an upsert-by-id writer, a reader, and a counter.
#'   It reuses the standard schema in [_schema_templates.R] and the generic
#'   `.duckdb_type()` mapper from [_metadata_duckdb.R].
#'
#'   Key contract: `template_id` (ERC-104) is the durable template database key — a short
#'   stable hash over the ROI identity tuple. Upsert/read/count filter by it, so
#'   re-exporting the same ROI replaces its row rather than duplicating it.
#'
#' @keywords internal
#' @noRd

# Open a connection and ensure the schema exists. Caller closes it with
# DBI::dbDisconnect(con, shutdown = TRUE).
.template_duckdb_connect <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  .template_duckdb_ensure_schema(con)
  con
}

# Create-if-missing, then ensure-columns so a DB created with an older schema
# keeps working (mirrors .roi_duckdb_ensure_schema). `template_id` is the
# primary key; `_created_at` is a DuckDB DEFAULT (not part of the schema spec).
.template_duckdb_ensure_schema <- function(con) {
  spec <- .template_schema_spec()
  cols <- vapply(names(spec), function(n) {
    pk <- if (n == "template_id") " PRIMARY KEY" else ""
    sprintf("%s %s%s", n, .duckdb_type(spec[[n]]), pk)
  }, character(1))
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE IF NOT EXISTS templates (%s, _created_at TIMESTAMP DEFAULT now());",
    paste(cols, collapse = ", ")
  ))
  for (n in names(spec)) {
    DBI::dbExecute(con, sprintf(
      "ALTER TABLE templates ADD COLUMN IF NOT EXISTS %s %s;",
      n, .duckdb_type(spec[[n]])
    ))
  }
  invisible(con)
}

# Read the whole template database back as a data.frame, in canonical schema/types.
.template_duckdb_read <- function(con) {
  .check_schema_version(con, "templates")            # AFL-19: read-time check
  cols <- paste(names(.template_schema_spec()), collapse = ", ")
  df <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s FROM templates ORDER BY template_id;", cols
  ))
  .coerce_templates(df)
}

# Upsert template rows by `template_id`: delete the matching ids, then append
# the new set, in a single transaction (mirrors .roi_duckdb_save). Idempotent
# with the stable id, so re-exporting the same ROIs replaces their rows.
.template_duckdb_upsert <- function(con, templates_df) {
  df <- .coerce_templates(templates_df)
  DBI::dbWithTransaction(con, {
    if (nrow(df) > 0) {
      DBI::dbExecute(con,
        "DELETE FROM templates WHERE template_id IN (?);",
        params = list(df$template_id))
      DBI::dbAppendTable(con, "templates", df)
    }
  })
  .stamp_schema_version(con)                         # AFL-19: write-time stamp
  invisible(df)
}

# Count stored templates.
.template_duckdb_count <- function(con) {
  as.integer(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM templates;")$n[1])
}

#' Export a DuckDB template database to CSV (ERC-102 portability converter)
#'
#' @description Store-level utility for the template database, exported for
#'   advanced use (not part of the everyday pipeline).
#'
#' The only sanctioned way to get a CSV out of the template database: the new
#' functions never write CSV directly (ERC-102/FTM-101). Mirrors
#' [export_metadata_duckdb_to_csv()].
#'
#' @param duckdb_path source template `.duckdb` file.
#' @param csv_path destination CSV path.
#' @param overwrite overwrite an existing CSV. Default `FALSE`.
#' @param columns optional character vector subset of columns (default all).
#' @return invisibly, the exported data.frame.
#' @keywords internal
#' @name export_templates_duckdb_to_csv
#' @export
export_templates_duckdb_to_csv <- function(duckdb_path, csv_path,
                                           overwrite = FALSE, columns = NULL) {
  if (file.exists(csv_path) && !overwrite) {
    stop("Destination CSV exists; pass overwrite = TRUE to replace it.")
  }
  con <- .template_duckdb_connect(duckdb_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  df <- .template_duckdb_read(con)
  if (!is.null(columns)) df <- df[, columns, drop = FALSE]
  utils::write.csv(df, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(df)
}
