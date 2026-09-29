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

# Open a connection and ensure the schema. Caller closes with
# DBI::dbDisconnect(con, shutdown = TRUE).
#
# `read_only = TRUE` (LVA-162) opens the store WITHOUT the ensure-schema step,
# for callers that only read: opening read-write would emit the DDL (CREATE +
# one ALTER per spec column) and mutate the user's file on a pure read. See the
# matching note in [_detections_duckdb.R]. Writes use the default.
.signals_duckdb_connect <- function(path, read_only = FALSE) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = read_only)
  if (!read_only) .signals_duckdb_ensure_schema(con)
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
#
# FR-107/SIG-09: the `SELECT` names only the columns this store declares, so a
# signals table older than the current spec reads cleanly instead of failing
# with a `Binder Error`. The read must not depend on the ensure-schema write to
# have healed the store first -- once the read path is read-only, nothing else
# does it. Absent columns come back as the typed `NA` from `.coerce_signals()`.
.signals_duckdb_read <- function(con, soundscape_path = NULL,
                                 signal_class = NULL) {
  # A read-only caller cannot create the table the way ensure-schema did, so a
  # store with no `signals` table reads as empty rather than erroring (LVA-162).
  if (!.duckdb_has_table(con, "signals")) return(.schema_signals(0L))
  .check_schema_version(con, "signals")              # AFL-19: read-time check
  cols <- paste(.legacy_select_columns(con, "signals", .signal_schema_spec()),
                collapse = ", ")
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

# Columns actually present in `table` on an open connection, in declared order
# (empty when the table does not exist).
#
# The three migrators below need this because they read their source
# READ-ONLY: a source store written before a column was added to the schema
# spec does not have that column, and a `SELECT` naming it fails with a DuckDB
# `Binder Error` instead of migrating the rows. Sibling readers used to avoid
# this by running the ensure-schema step first; since LVA-162 the read paths
# also open read-only and use `.legacy_select_columns()`, so they no longer heal
# the store either. `table` is always a literal at the call sites, never user
# input.
.duckdb_table_columns <- function(con, table) {
  DBI::dbGetQuery(con, sprintf(
    "SELECT name FROM pragma_table_info('%s');", table))$name
}

# TRUE when `table` exists on `con`. A read-only caller guards with this before
# its `SELECT`: it cannot create the table the way the ensure-schema path did.
.duckdb_has_table <- function(con, table) {
  table %in% DBI::dbListTables(con)
}

# The spec columns the source store can actually supply, so old stores migrate.
# Columns added to the spec after the store was written fall back to the typed
# `NA` the `.coerce_*()` converters already supply.
.legacy_select_columns <- function(con, table, spec) {
  intersect(names(spec), .duckdb_table_columns(con, table))
}

# Migrate a legacy `rois` DuckDB store into the signals store. Re-running is
# idempotent (same signal_ids upserted, no duplicates).
migrate_rois_store_to_signals <- function(rois_db, signals_db) {
  src <- DBI::dbConnect(duckdb::duckdb(), dbdir = rois_db, read_only = TRUE)
  on.exit(DBI::dbDisconnect(src, shutdown = TRUE))
  cols <- paste(.legacy_select_columns(src, "rois", .roi_schema_spec()),
                collapse = ", ")
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
  cols <- paste(.legacy_select_columns(src, "detections",
                                      .detection_schema_spec()),
                collapse = ", ")
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
  cols <- paste(.legacy_select_columns(src, "validations",
                                      .validation_schema_spec()),
                collapse = ", ")
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

# --- legacy-store readers for the CSV exporters (ENG-13) ----------------------

# Read a legacy store table, tolerating its absence. The `SELECT` names only
# the columns the store declares (SIG-09's rule), so a store older than the
# current spec still reads. A store written by the current version has no such
# table at all, which is why the empty return is the common case, not an error.
#
# # R equivalent
#   if (!table %in% dbListTables(con)) empty else dbGetQuery(con, select)
# # Arguments
#   con       DBI connection (read-only).
#   table     character — legacy table name (`detections`, `rois`, ...).
#   spec      named list — the legacy schema spec.
#   empty     data.frame — the typed zero-row schema to return when absent.
#   order_by  character — ORDER BY expression for stable row order.
# # Returns
#   data.frame — the table rows, or `empty`.
.read_legacy_store_table <- function(con, table, spec, empty, order_by) {
  if (!.duckdb_has_table(con, table)) return(empty)
  .check_schema_version(con, table)                  # AFL-19: read-time check
  cols <- paste(.legacy_select_columns(con, table, spec), collapse = ", ")
  DBI::dbGetQuery(con, sprintf("SELECT %s FROM %s ORDER BY %s;",
                               cols, table, order_by))
}

# Signal classes a store actually holds, cheaply. Used to decide whether the
# `signals` table carries the rows an exporter was asked for.
.signal_classes_present <- function(con) {
  if (!.duckdb_has_table(con, "signals")) return(character(0))
  DBI::dbGetQuery(con, "SELECT DISTINCT signal_class FROM signals;")$signal_class
}

# Resolve which table an exporter must read (ENG-13). The current pipeline
# writes `signals`; stores from before the unified schema have the legacy table
# instead, and the migrators can leave BOTH populated in one file. Prefer
# `signals` whenever it holds rows of the class being exported, so the exporters
# never return an empty CSV on a store written by the current version.
#
# `classes` are the signal_class values that map onto the exported legacy shape
# (`roi` for the ROI shape, `detection` for detections/validations).
# # Arguments
#   con      DBI connection (read-only).
#   classes  character — signal classes the requested shape is built from.
# # Returns
#   character — `"signals"` or `"legacy"`.
.prefer_signals_store <- function(con, classes) {
  if (!.duckdb_has_table(con, "signals")) return("legacy")
  present <- .signal_classes_present(con)
  if (any(classes %in% present)) return("signals")
  "legacy"
}

# Warn when an export produced no rows. A caller who asked for a CSV and got
# only a header must be told, because the file looks valid on disk (ENG-13).
.warn_if_empty_export <- function(df, source) {
  if (nrow(df) != 0L) return(invisible(FALSE))
  warning(sprintf(
    "The export wrote 0 rows: the %s store holds no rows of the requested kind. %s",
    source,
    "The CSV has a header only."), call. = FALSE)
  invisible(TRUE)
}
