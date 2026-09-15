#' DuckDB backend for the unified `signals` store (signals plan F2)
#'
#' @description Connection / ensure-schema / class-scoped save / idempotent
#'   upsert / read / count / statuses for the `signals` table
#'   ([_schema_signals.R]). Mirrors the established backends
#'   ([_roi_duckdb.R], [_detections_duckdb.R]) and reuses their helpers
#'   (`.duckdb_type()`, `.stamp_schema_version()`, `.check_schema_version()`
#'   — AFL-19).
#'
#'   Key contracts (spec `specs/R/signals.md`, plan §7):
#'   - **Class-scoped save (R1):** a segmentation-app save deletes only the
#'     app-owned classes -- `signal_class IN ('roi', 'detection_to_roi')` -- of
#'     one `soundscape_path` (LSA-208 widened the delete to cover promoted
#'     rows, so editing/deleting a promoted ROI persists like a manual one).
#'     The writer has no code path that deletes `detection` rows -- the guard
#'     is structural.
#'   - **Upsert by `signal_id`** for detections and `val_*` updates.
#'   - **Staleness (AFL-23):** generalized to any row class with a known
#'     `soundscape_sha256`; ROI rows stay `NA` (SIG-06, capture deferred).
#'   - **no_soi statuses** are computed over roi-class rows only (LSA-23).
#'   - **Migrations** from the three legacy stores are idempotent upserts.
#'
#' @keywords internal
#' @noRd

# --- connection / schema ------------------------------------------------------

.signals_duckdb_connect <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  .signals_duckdb_ensure_schema(con)
  con
}

# CREATE IF NOT EXISTS + per-column ADD COLUMN IF NOT EXISTS (LSA-109 pattern;
# pre-existing tables keep working, new columns read back as NULL/NA).
.signals_duckdb_ensure_schema <- function(con) {
  spec <- .signal_schema_spec()
  cols <- vapply(names(spec), function(n) {
    sprintf("%s %s", n, .duckdb_type(spec[[n]]))
  }, character(1))
  pk <- ", PRIMARY KEY (signal_id)"
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE IF NOT EXISTS signals (%s%s, _created_at TIMESTAMP DEFAULT now());",
    paste(cols, collapse = ", "), pk
  ))
  for (n in names(spec)) {
    DBI::dbExecute(con, sprintf(
      "ALTER TABLE signals ADD COLUMN IF NOT EXISTS %s %s;",
      n, .duckdb_type(spec[[n]])
    ))
  }
  invisible(con)
}

# --- read / count -------------------------------------------------------------

# Read signals, optionally filtered by soundscape_path and/or signal_class,
# in canonical schema/types and signal_id order.
.signals_duckdb_read <- function(con, soundscape_path = NULL,
                                 signal_class = NULL) {
  .check_schema_version(con, "signals")              # AFL-19: read-time check
  cols <- paste(names(.signal_schema_spec()), collapse = ", ")
  where <- character(0)
  params <- list()
  if (!is.null(soundscape_path)) {
    where <- c(where, "soundscape_path = ?")
    params <- c(params, list(soundscape_path))
  }
  if (!is.null(signal_class)) {
    if (length(signal_class) == 1L) {
      where <- c(where, "signal_class = ?")
      params <- c(params, list(signal_class))
    } else {
      where <- c(where, sprintf("signal_class IN (%s)",
        paste(rep("?", length(signal_class)), collapse = ", ")))
      params <- c(params, as.list(signal_class))
    }
  }
  sql <- sprintf("SELECT %s FROM signals", cols)
  if (length(where)) sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
  sql <- paste(sql, "ORDER BY signal_id;")
  df <- do.call(DBI::dbGetQuery, list(con, sql, params = params))
  .coerce_signals(df)
}

# Row count, optionally scoped to a soundscape_path and/or signal_class.
.signals_duckdb_count <- function(con, soundscape_path = NULL,
                                  signal_class = NULL) {
  where <- character(0)
  params <- list()
  if (!is.null(soundscape_path)) {
    where <- c(where, "soundscape_path = ?")
    params <- c(params, list(soundscape_path))
  }
  if (!is.null(signal_class)) {
    where <- c(where, "signal_class = ?")
    params <- c(params, list(signal_class))
  }
  sql <- "SELECT COUNT(*) AS n FROM signals"
  if (length(where)) sql <- paste(sql, "WHERE", paste(where, collapse = " AND "))
  sql <- paste0(sql, ";")
  as.integer(do.call(DBI::dbGetQuery, list(con, sql, params = params))$n[1])
}

# --- AFL-23 staleness (generalized) -------------------------------------------

# For each soundscape_path in the incoming batch with a known sha256, compare
# against the distinct non-NA hashes already stored (any signal_class). A
# mismatch flags every verifiable row of that path: existing rows in-place
# (UPDATE), batch rows before insert. NA-sha rows are left untouched (NA).
.flag_stale_signal_sources <- function(con, df) {
  hashed <- df[!is.na(df$soundscape_sha256), , drop = FALSE]
  if (nrow(hashed) == 0L) return(df)
  by_path <- split(hashed$soundscape_sha256, hashed$soundscape_path)
  for (path in names(by_path)) {
    batch_hash <- unique(by_path[[path]])[1]           # one file -> one hash
    existing <- DBI::dbGetQuery(con,
      "SELECT DISTINCT soundscape_sha256 FROM signals
       WHERE soundscape_path = ? AND soundscape_sha256 IS NOT NULL;",
      params = list(path))$soundscape_sha256
    divergent <- any(existing != batch_hash)
    if (divergent) {
      DBI::dbExecute(con,
        "UPDATE signals SET source_stale = 'TRUE'
         WHERE soundscape_path = ?;", params = list(path))
    }
    df$source_stale[df$soundscape_path == path &
                    !is.na(df$soundscape_sha256)] <- as.character(divergent)
  }
  df
}

# --- save / upsert ------------------------------------------------------------

# Segmentation-app save: replace the app-owned rows of ONE soundscape_path in a
# single transaction (LSA-11 semantics narrowed to the class, R1; widened to
# `detection_to_roi` by LSA-208 so promoted ROIs save/edit/delete like manual
# ones). The structural guard: this function has no code path that deletes
# `detection` rows -- only the app-owned classes are ever removed.
# LSA-209: the delete key is canonicalized to the store contract. Inserted
# rows are canonically keyed by construction (`.rois_as_signals()` normalizes)
# PROVIDED the caller passes the same `workspace_root` to both (CRAN-103: the
# app save sites do); a caller key carrying `"./"`/`"//"` cosmetics would turn
# the DELETE into a no-op while the INSERT still lands canonical rows --
# duplicates on every save, and a signal_id primary-key conflict rolls back
# the whole re-save.
.signals_duckdb_save_rois <- function(con, signals_df, soundscape_path,
                                    workspace_root = NULL) {
  df <- .coerce_signals(signals_df)
  soundscape_path <- .normalize_soundscape_paths(soundscape_path,
                                                 workspace_root)[1L]
  .reject_duplicate_roi_batch(df)                    # LSA-204: write-time guard
  app_classes <- c(.SIGNAL_CLASS_ROI, .SIGNAL_CLASS_DETECTION_TO_ROI)
  if (nrow(df) > 0L && any(!df$signal_class %in% app_classes)) {
    stop("`_signals_duckdb_save_rois()` accepts signal_class = 'roi' and ",
         "'detection_to_roi' rows only ",
         "(use `_signals_duckdb_upsert()` for other classes).")
  }
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, paste0(
      "DELETE FROM signals WHERE soundscape_path = ? AND ",
      "signal_class IN ('roi', 'detection_to_roi');"),
      params = list(soundscape_path))
    if (nrow(df) > 0L) DBI::dbAppendTable(con, "signals", df)
  })
  .stamp_schema_version(con)                          # AFL-19: write-time stamp
  invisible(df)
}

# Idempotent upsert by signal_id in one transaction (detections, val_* updates,
# migrations). Staleness is flagged before insert (AFL-23, generalized).
# `replace = TRUE` (autosave_action="replace") deletes only the batch's own
# class rows — detection-class rows — never roi-class rows (R1: the guard is
# structural; this writer has no code path that deletes roi rows).
.signals_duckdb_upsert <- function(con, signals_df, replace = FALSE) {
  df <- .coerce_signals(signals_df)
  DBI::dbWithTransaction(con, {
    if (replace) {
      DBI::dbExecute(con,
        "DELETE FROM signals WHERE signal_class = 'detection';")
    }
    if (nrow(df) > 0L) {
      df <- .flag_stale_signal_sources(con, df)
      if (!replace) {
        DBI::dbExecute(con, "DELETE FROM signals WHERE signal_id IN (?);",
                       params = list(df$signal_id))
      }
      DBI::dbAppendTable(con, "signals", df)
    }
  })
  .stamp_schema_version(con)                          # AFL-19: write-time stamp
  invisible(df)
}

# --- statuses -----------------------------------------------------------------

# Classify each requested soundscape_path as "unsegmented", "segmented" or
# "no_soi" over roi-class rows only (LSA-23 class-aware: a detection row with
# the sentinel label never flips a soundscape). Single grouped query, mirrors
# `.roi_duckdb_statuses()`.
.signals_duckdb_statuses <- function(con, soundscape_paths) {
  counts <- DBI::dbGetQuery(con, sprintf(
    "SELECT soundscape_path,
            COUNT(*) AS n_total,
            SUM(CASE WHEN COALESCE(roi_label_updated, roi_label) = '%s'
                     THEN 1 ELSE 0 END) AS n_soi
     FROM signals WHERE signal_class = 'roi' GROUP BY soundscape_path;",
    .ROI_NO_SOI_LABEL
  ))
  status <- stats::setNames(
    rep("unsegmented", length(soundscape_paths)), soundscape_paths
  )
  for (i in seq_len(nrow(counts))) {
    sp <- counts$soundscape_path[i]
    if (sp %in% soundscape_paths) {
      status[[sp]] <- if (counts$n_soi[i] == counts$n_total[i]) "no_soi"
                      else "segmented"
    }
  }
  unname(status[soundscape_paths])
}

# --- legacy-store migrations (idempotent upserts by signal_id) ----------------

# Migrate a legacy `rois` DuckDB store into the signals store. Re-running is
# idempotent (same signal_ids upserted, no duplicates).
migrate_rois_store_to_signals <- function(rois_db, signals_db) {
  src <- DBI::dbConnect(duckdb::duckdb(), dbdir = rois_db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(src, shutdown = TRUE))
  cols <- paste(names(.roi_schema_spec()), collapse = ", ")
  df <- .coerce_rois(DBI::dbGetQuery(src, sprintf(
    "SELECT %s FROM rois ORDER BY soundscape_path, roi_input_timestamp;", cols)))
  con <- .signals_duckdb_connect(signals_db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .signals_duckdb_upsert(con, .rois_as_signals(df))
  invisible(nrow(df))
}

# Migrate a legacy `detections` DuckDB store into the signals store.
# signal_id == detection_id, so re-running is idempotent.
migrate_detections_store_to_signals <- function(detections_db, signals_db) {
  src <- DBI::dbConnect(duckdb::duckdb(), dbdir = detections_db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(src, shutdown = TRUE))
  cols <- paste(names(.detection_schema_spec()), collapse = ", ")
  df <- .coerce_detections(DBI::dbGetQuery(src, sprintf(
    "SELECT %s FROM detections ORDER BY detection_id;", cols)))
  con <- .signals_duckdb_connect(signals_db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .signals_duckdb_upsert(con, .detections_as_signals(df))
  invisible(nrow(df))
}

# Migrate a legacy `validations` DuckDB store into the signals store: the
# val_* payload joins onto detection rows by detection_id (== signal_id).
migrate_validations_store_to_signals <- function(validations_db, signals_db) {
  src <- DBI::dbConnect(duckdb::duckdb(), dbdir = validations_db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(src, shutdown = TRUE))
  cols <- paste(names(.validation_schema_spec()), collapse = ", ")
  df <- .coerce_validations(DBI::dbGetQuery(src, sprintf(
    "SELECT %s FROM validations ORDER BY detection_id;", cols)))
  val <- df[!is.na(df$validation), , drop = FALSE]    # TP/FP only (VBO-05)
  con <- .signals_duckdb_connect(signals_db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  # The validations schema carries all 27 detection columns, so each row
  # rebuilds its own detection row (signal_id == detection_id) — the join is
  # structural, no orphan filter needed. If the detections store was migrated
  # too, the shared id upserts merge them into one row.
  .signals_duckdb_upsert(con, .validations_as_signals(val))
  invisible(nrow(val))
}
