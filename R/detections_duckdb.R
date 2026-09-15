#' DuckDB detections backend (Stage 4 gate §2.2)
#'
#' @description Transactional persistence for the `detections` table produced by
#'   [run_matching()]. Mirrors [_template_duckdb.R] / [_roi_duckdb.R]:
#'   connect / ensure-schema / **upsert-by-id** / read / count, plus a legacy-CSV
#'   converter. Replaces the original CSV `writeLines`-header + per-row
#'   `sink`/`write.table` machinery, which dissolves its bugs at once:
#'   - **RMB-03/RMB-06** (Windows full-overwrite / Unix `sink` concurrent
#'     corruption) — a single transactional writer, no shared file handle;
#'   - **RMB-04** (dedup keyed only on `soundscape_file`) — the upsert keys on
#'     `detection_id` = hash(`soundscape_path`, `template_id`, `peak_index`,
#'     `score_method`), so a different template against the same soundscape is
#'     kept;
#'   - **RMB-05** (hand-maintained 21-col header) — the schema is
#'     [.detection_schema_spec()], a single source of truth.
#'
#'   `autosave_action`: `"append"` upserts (keeps prior rows, replaces matching
#'   ids); `"replace"` truncates the table first, then inserts.
#'
#' @keywords internal
#' @noRd

# Open a connection and ensure the schema. Caller closes with
# DBI::dbDisconnect(con, shutdown = TRUE).
.detections_duckdb_connect <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  .detections_duckdb_ensure_schema(con)
  con
}

# Create-if-missing + ensure-columns (forward-compatible with older DBs).
# detection_id is the primary key; _created_at is a DuckDB DEFAULT.
.detections_duckdb_ensure_schema <- function(con) {
  spec <- .detection_schema_spec()
  cols <- vapply(names(spec), function(n) {
    pk <- if (n == "detection_id") " PRIMARY KEY" else ""
    sprintf("%s %s%s", n, .duckdb_type(spec[[n]]), pk)
  }, character(1))
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE IF NOT EXISTS detections (%s, _created_at TIMESTAMP DEFAULT now());",
    paste(cols, collapse = ", ")
  ))
  for (n in names(spec)) {
    DBI::dbExecute(con, sprintf(
      "ALTER TABLE detections ADD COLUMN IF NOT EXISTS %s %s;",
      n, .duckdb_type(spec[[n]])
    ))
  }
  invisible(con)
}

# Read the whole table back in canonical schema/types.
.detections_duckdb_read <- function(con) {
  .check_schema_version(con, "detections")          # AFL-19: read-time check
  cols <- paste(names(.detection_schema_spec()), collapse = ", ")
  df <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s FROM detections ORDER BY detection_id;", cols
  ))
  .coerce_detections(df)
}

# AFL-23: for each soundscape_path in the incoming batch with a known
# soundscape_sha256, compare it against the distinct non-NA hashes already
# stored for that path. A mismatch means the file at that path was overwritten
# with different content between two runs -- flag EVERY row for that path
# (existing rows in-place, plus the incoming batch before insert) with
# detection_source_stale = TRUE, since none of them can be trusted to refer to
# the same audio without re-verifying the file. Rows whose soundscape_sha256 is
# NA are not verifiable and are left untouched (detection_source_stale stays
# NA on the incoming row, existing rows keep whatever they had).
.flag_stale_detection_sources <- function(con, df) {
  hashed <- df[!is.na(df$soundscape_sha256), , drop = FALSE]
  if (nrow(hashed) == 0L) return(df)
  by_path <- split(hashed$soundscape_sha256, hashed$soundscape_path)
  for (path in names(by_path)) {
    batch_hash <- unique(by_path[[path]])[1]           # one file -> one hash
    existing <- DBI::dbGetQuery(con,
      "SELECT DISTINCT soundscape_sha256 FROM detections
       WHERE soundscape_path = ? AND soundscape_sha256 IS NOT NULL;",
      params = list(path))$soundscape_sha256
    divergent <- any(existing != batch_hash)
    if (divergent) {
      DBI::dbExecute(con,
        "UPDATE detections SET detection_source_stale = 'TRUE'
         WHERE soundscape_path = ?;", params = list(path))
    }
    df$detection_source_stale[df$soundscape_path == path &
                              !is.na(df$soundscape_sha256)] <- as.character(divergent)
  }
  df
}

# Upsert detections by detection_id in one transaction (idempotent). When
# `replace = TRUE`, the whole table is truncated first (autosave_action="replace").
.detections_duckdb_upsert <- function(con, detections_df, replace = FALSE) {
  df <- .coerce_detections(detections_df)
  DBI::dbWithTransaction(con, {
    if (replace) DBI::dbExecute(con, "DELETE FROM detections;")
    if (nrow(df) > 0) {
      df <- .flag_stale_detection_sources(con, df)      # AFL-23: before insert
      if (!replace) {
        DBI::dbExecute(con, "DELETE FROM detections WHERE detection_id IN (?);",
                       params = list(df$detection_id))
      }
      DBI::dbAppendTable(con, "detections", df)
    }
  })
  .stamp_schema_version(con)                         # AFL-19: write-time stamp
  invisible(df)
}

.detections_duckdb_count <- function(con) {
  as.integer(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM detections;")$n[1])
}

#' Export a DuckDB detections table to CSV (legacy back-compat converter)
#'
#' @description Store-level utility for the detections store, exported for
#'   advanced use (not part of the everyday pipeline). The sanctioned way to
#'   get a CSV out of the detections store (the
#'   refactored pipeline no longer writes CSV directly — Stage 4 gate). By default
#'   it emits the **legacy 21-column layout** the original `run_matching` wrote
#'   (dropping `detection_id`/`template_id`/`score_method`), so existing
#'   downstream code keeps working; set `legacy = FALSE` for the full standard
#'   schema. Mirrors [export_templates_duckdb_to_csv()].
#'
#'   **Data-loss boundary (G5 in plan-log-2026-06-22-001):** When `legacy = TRUE`
#'   (the default), the three durable-key columns `detection_id`, `template_id`,
#'   and `score_method` are dropped. Consumers of this CSV **cannot** reconstruct
#'   the durable detection identity (idempotent upsert key) or join back to the
#'   template database. Use `legacy = FALSE` when full provenance is needed.
#' @param duckdb_path source detections `.duckdb`.
#' @param csv_path destination CSV.
#' @param overwrite overwrite an existing CSV (default `FALSE`).
#' @param legacy emit the original 21-column layout (default `TRUE`).
#' @return invisibly, the exported data.frame.
#' @keywords internal
#' @name export_detections_duckdb_to_csv
#' @export
export_detections_duckdb_to_csv <- function(duckdb_path, csv_path,
                                            overwrite = FALSE, legacy = TRUE) {
  if (file.exists(csv_path) && !overwrite) {
    stop("Destination CSV exists; pass overwrite = TRUE to replace it.")
  }
  con <- .detections_duckdb_connect(duckdb_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  df <- .detections_duckdb_read(con)
  if (legacy) df <- df[, .detections_legacy_cols(), drop = FALSE]
  utils::write.csv(df, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(df)
}

# The original 21-column detections CSV layout (drops detection_id/template_id/
# score_method). Single source of truth shared by the converter above and the
# deprecated `output_file` compat path in template_matching() (TM-11).
.detections_legacy_cols <- function() {
  c(
    "soundscape_path", "soundscape_file", "template_path", "template_file",
    "template_name", "template_min_freq", "template_max_freq",
    "template_start", "template_end", "detection_start", "detection_end",
    "detection_wl", "detection_ovlp", "detection_sample_rate",
    "detection_buffer", "detection_min_score", "detection_min_quant",
    "detection_top_n", "peak_index", "peak_score", "peak_quant"
  )
}

#' Migrate a legacy detections CSV into a DuckDB detections store (one-shot
#' converter)
#'
#' @description Store-level utility for the detections store, exported for
#'   advanced use (not part of the everyday pipeline). Reads a legacy detections
#'   CSV (21-column original layout or
#'   full 26-column standard layout) and persists it into a DuckDB detections
#'   store with upsert semantics. Complements [migrate_metadata_csv_to_duckdb()]
#'   and [migrate_templates_to_db()] (G3 in plan-log-2026-06-22-001).
#'
#'   The CSV is coerced to the standard 26-column schema via
#'   `.coerce_detections()`: missing standard columns become typed `NA`, extra
#'   columns are dropped. Rows are upserted by `detection_id` so re-running this
#'   on the same CSV is idempotent. When the CSV carries no `detection_id`
#'   column (legacy 21-col layout), a stable id is synthesized per row from the
#'   durable key via \code{.detection_id()} (substituting `template_file` for the
#'   absent `template_id`), so legacy rows insert cleanly under the NOT NULL
#'   primary key and re-running upserts the same ids (CRAN-98).
#'
#' @param csv_path path to a legacy detections CSV.
#' @param duckdb_path destination `.duckdb` file.
#' @param overwrite overwrite an existing destination. Default `FALSE`.
#' @return invisibly, the migrated data.frame.
#' @keywords internal
#' @name migrate_detections_csv_to_duckdb
#' @export
migrate_detections_csv_to_duckdb <- function(csv_path, duckdb_path,
                                              overwrite = FALSE) {
  if (file.exists(duckdb_path) && !overwrite) {
    stop("Destination exists; pass overwrite = TRUE to replace it.")
  }
  if (file.exists(duckdb_path) && overwrite) unlink(duckdb_path)
  if (!file.exists(csv_path)) stop("File not found: ", csv_path)
  df <- utils::read.csv(csv_path, stringsAsFactors = FALSE,
                        fileEncoding = "UTF-8")
  df <- .coerce_detections(df)
  # CRAN-98: the legacy 21-col CSV carries no detection_id, but detection_id is
  # the store's PRIMARY KEY (NOT NULL) -- inserting NA rows aborts the upsert.
  # Synthesize a stable id per NA row from the durable key, reusing the canonical
  # .detection_id() hash and substituting template_file for the absent
  # template_id (as .vbo_detection_id() does) so same-species templates do not
  # collide on (soundscape, peak_index). This makes the legacy path insertable
  # AND idempotent (re-running upserts the same ids instead of duplicating).
  na_id <- is.na(df$detection_id)
  if (any(na_id)) {
    tid <- ifelse(!is.na(df$template_id), df$template_id, df$template_file)
    df$detection_id[na_id] <- .detection_id(
      df$soundscape_path[na_id], tid[na_id],
      df$peak_index[na_id], df$score_method[na_id]
    )
  }
  con <- .signals_duckdb_connect(duckdb_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .signals_duckdb_upsert(con, .detections_as_signals(df))
  invisible(df)
}
