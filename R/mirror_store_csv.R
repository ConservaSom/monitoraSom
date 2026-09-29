#' CSV mirror of a DuckDB store table (plan 2026-09-21_02, STEP-2)
#'
#' @description Writes a full snapshot of one store table to CSV, so the data
#'   can be opened without DuckDB. One helper serves both stores in scope: the
#'   `signals` table (spec [.signal_schema_spec()]) and the soundscape-metadata
#'   `soundscapes` table (spec [.soundscape_schema_spec()]).
#'
#'   Policy (plan DEC-1..8):
#'   - the CSV carries the spec columns only, never `_created_at` (DEC-5);
#'   - every call rewrites the whole file, never appends (DEC-3);
#'   - a failure is a `warning()`, never an error, and never silent (DEC-6);
#'   - `csv_mirror = TRUE` resolves to `<store name>_mirror.csv` next to the
#'     store (DEC-8).
#'
#'   The CSV is a one-way, read-only copy: edits to it never reach the store.
#'   `NA` is written as an empty field (DuckDB default); read it back with
#'   `utils::read.csv(..., na.strings = "")`.
#'
#' @keywords internal
#' @noRd

# Resolve the `csv_mirror` argument to a CSV path (DEC-8). `NULL` -> `NULL`
# (off); `TRUE` -> `<store basename without extension>_mirror.csv` in the
# store's directory; a string -> that path. Anything else is an error.
.resolve_csv_mirror_path <- function(csv_mirror, store_path) {
  if (is.null(csv_mirror) || isFALSE(csv_mirror)) return(NULL)
  if (isTRUE(csv_mirror)) {
    stem <- tools::file_path_sans_ext(basename(store_path))
    return(file.path(dirname(store_path), paste0(stem, "_mirror.csv")))
  }
  if (is.character(csv_mirror) && length(csv_mirror) == 1L &&
      !is.na(csv_mirror) && nzchar(csv_mirror)) {
    return(csv_mirror)
  }
  stop("'csv_mirror' must be NULL, TRUE, FALSE or a single file path.",
       call. = FALSE)
}

# Write `table` of `store_path` to `csv_path`, spec columns only, ordered by
# `order_by`. Returns the CSV path invisibly on success, `NULL` on failure.
#
# Known limit: the no-mutation guarantee comes from the SQL (only SELECT and
# COPY, never DDL), not from `read_only = TRUE`. When another connection of the
# same process already holds the store, DuckDB treats the flag as a request
# only (plan FACT-18). Never add DDL here.
.mirror_store_csv <- function(store_path, table, spec, csv_path, order_by) {
  tmp <- tempfile(pattern = paste0(basename(csv_path), "."),
                  tmpdir = dirname(csv_path), fileext = ".tmp")
  con <- NULL
  on.exit({
    if (!is.null(con)) DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(tmp)
  }, add = TRUE)
  res <- tryCatch({
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = store_path,
                          read_only = TRUE)
    if (!(table %in% DBI::dbListTables(con))) {
      stop(sprintf("the store has no `%s` table", table), call. = FALSE)
    }
    cols <- .legacy_select_columns(con, table, spec)
    n_store <- DBI::dbGetQuery(con, sprintf(
      "SELECT count(*) AS n FROM %s;", table))$n
    n_copy <- DBI::dbExecute(con, sprintf(
      "COPY (SELECT %s FROM %s ORDER BY %s) TO '%s' (HEADER, DELIMITER ',');",
      paste(cols, collapse = ", "), table, order_by,
      gsub("'", "''", tmp, fixed = TRUE)))
    if (!isTRUE(n_copy == n_store)) {
      stop(sprintf("wrote %s rows, but the store holds %s", n_copy, n_store),
           call. = FALSE)
    }
    if (!file.rename(tmp, csv_path)) {
      stop("could not move the temporary file into place", call. = FALSE)
    }
    n_store
  }, error = function(e) {
    warning(sprintf("CSV mirror of `%s` not written (%s): %s",
                    table, csv_path, conditionMessage(e)), call. = FALSE)
    NULL
  })
  if (is.null(res)) return(invisible(NULL))
  message(sprintf("CSV mirror written: %s (%d rows of `%s`).",
                  csv_path, as.integer(res), table))
  invisible(csv_path)
}

# Front-door check for `csv_mirror` (plan DEC-12): runs before any processing.
# A mirror asked for without a DuckDB store, or towards a missing directory, is
# an error here, never a late warning after a long run. `store_arg` names the
# caller's store argument in the message, with its backticks.
.validate_csv_mirror <- function(csv_mirror, store_path, store_arg) {
  csv_path <- .resolve_csv_mirror_path(
    csv_mirror, if (is.null(store_path)) "store.duckdb" else store_path)
  if (is.null(csv_path)) return(invisible(NULL))
  if (is.null(store_path)) {
    stop(sprintf(paste0("`csv_mirror` needs %s: without a DuckDB store ",
                        "there is nothing to mirror."), store_arg),
         call. = FALSE)
  }
  if (!dir.exists(dirname(csv_path))) {
    stop("The `csv_mirror` directory does not exist: ", dirname(csv_path),
         call. = FALSE)
  }
  invisible(csv_path)
}

# Mirror the `signals` table of `store_path` when `csv_mirror` asks for it.
# Call it once, after the caller's last write to the store (plan DEC-2).
.mirror_signals_csv <- function(store_path, csv_mirror) {
  csv_path <- .resolve_csv_mirror_path(csv_mirror, store_path)
  if (is.null(csv_path)) return(invisible(NULL))
  .mirror_store_csv(store_path, "signals", .signal_schema_spec(), csv_path,
                    "signal_id")
}

# Mirror the `soundscapes` table of the metadata store (plan DEC-9, DEC-11:
# `metadata_errors` stays out). Same call contract as `.mirror_signals_csv`.
.mirror_soundscapes_csv <- function(store_path, csv_mirror) {
  csv_path <- .resolve_csv_mirror_path(csv_mirror, store_path)
  if (is.null(csv_path)) return(invisible(NULL))
  .mirror_store_csv(store_path, "soundscapes", .soundscape_schema_spec(),
                    csv_path, "soundscape_path")
}
