#' DuckDB ROI persistence backend for the segmentation app
#' (LSA-116 / LSA-19 / LSA-21 / LSA-23 / LSA-12 / LSA-11 / LSA-117)
#'
#' @description DuckDB is the exclusive persistence target for ROIs produced
#'   during interactive segmentation. CSV export is available through
#'   [export_rois_duckdb_to_csv()] and legacy CSV migration through
#'   [migrate_rois_csv_to_duckdb()] (G1/G4 in plan-log-2026-06-22-001). The ROI
#'   database is a **separate** `.duckdb` file from the soundscape-metadata
#'   database (LSA-116, separate-DBs topology), holding a single `rois` table.
#'
#'   This file mirrors [_metadata_duckdb.R]: connection/schema helpers, a
#'   per-path upsert writer, a reader, a counter and a status classifier. It
#'   reuses the standard schema in [_schema_rois.R] and the generic
#'   `.duckdb_type()` mapper.
#'
#'   Key contract (LSA-19): `soundscape_path` is the durable recording key.
#'   Read/save/count/statuses all filter/group by `soundscape_path`;
#'   `soundscape_file` is kept only as a readable display field. This removes
#'   the basename-collision hazard of the legacy SQLite layer.
#'
#' @keywords internal
#' @noRd

# Open a connection and ensure the schema exists. Caller closes it with
# DBI::dbDisconnect(con, shutdown = TRUE).
.roi_duckdb_connect <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  .roi_duckdb_ensure_schema(con)
  con
}

# Create-if-missing only (LSA-12): the Julia/DuckDB store has no legacy
# `start_time` schema to migrate, so the one-way rename branch is dropped.
.roi_duckdb_ensure_schema <- function(con) {
  spec <- .roi_schema_spec()
  cols <- vapply(names(spec), function(n) {
    sprintf("%s %s", n, .duckdb_type(spec[[n]]))
  }, character(1))
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE IF NOT EXISTS rois (%s, _created_at TIMESTAMP DEFAULT now());",
    paste(cols, collapse = ", ")
  ))
  # Ensure-columns (LSA-109 and any later schema growth): add canonical columns
  # missing from a pre-existing table so a DB created with an older schema keeps
  # working. Legacy rows get NULL for the new column.
  for (n in names(spec)) {
    DBI::dbExecute(con, sprintf(
      "ALTER TABLE rois ADD COLUMN IF NOT EXISTS %s %s;",
      n, .duckdb_type(spec[[n]])
    ))
  }
  invisible(con)
}

# Read one soundscape's ROIs back as a data.frame, in canonical schema/types.
.roi_duckdb_read <- function(con, soundscape_path) {
  .check_schema_version(con, "rois")                 # AFL-19: read-time check
  cols <- paste(names(.roi_schema_spec()), collapse = ", ")
  df <- DBI::dbGetQuery(con, sprintf(
    "SELECT %s FROM rois WHERE soundscape_path = ? ORDER BY roi_input_timestamp;",
    cols
  ), params = list(soundscape_path))
  .coerce_rois(df)
}

# Save one soundscape's ROIs: delete the path's existing rows, then append the
# new set, in a single transaction (LSA-11). Replaces the alt app's
# DELETE + dbWriteTable(append=TRUE) without a transaction.
.roi_duckdb_save <- function(con, rois_df, soundscape_path) {
  .reject_duplicate_roi_batch(.coerce_rois(rois_df)) # LSA-204: write-time guard
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con,
      "DELETE FROM rois WHERE soundscape_path = ?;",
      params = list(soundscape_path))
    if (!is.null(rois_df) && is.data.frame(rois_df) && nrow(rois_df) > 0) {
      DBI::dbAppendTable(con, "rois", .coerce_rois(rois_df))
    }
  })
  .stamp_schema_version(con)                         # AFL-19: write-time stamp
  invisible(rois_df)
}

# Count a soundscape's stored ROIs.
.roi_duckdb_count <- function(con, soundscape_path) {
  res <- DBI::dbGetQuery(con,
    "SELECT COUNT(*) AS n FROM rois WHERE soundscape_path = ?;",
    params = list(soundscape_path))
  as.integer(res$n[1])
}

# Classify each requested soundscape_path as "unsegmented", "segmented" or
# "no_soi" with a single grouped query. This is the SQL mirror of the canonical
# rule in .status_from_labels() (LSA-04): a path is "no_soi" iff every stored row
# is a sentinel (n_soi == n_total); paths with no rows are "unsegmented";
# otherwise "segmented". Kept as a grouped query (not a per-path label fetch +
# .status_from_labels call) to avoid N round-trips; the rule is identical.
# Returns a character vector aligned to `soundscape_paths` (unnamed).
.roi_duckdb_statuses <- function(con, soundscape_paths) {
  counts <- DBI::dbGetQuery(con, sprintf(
    "SELECT soundscape_path,
            COUNT(*) AS n_total,
            SUM(CASE WHEN roi_label = '%s' THEN 1 ELSE 0 END) AS n_soi
     FROM rois GROUP BY soundscape_path;",
    .ROI_NO_SOI_LABEL
  ))
  status <- stats::setNames(
    rep("unsegmented", length(soundscape_paths)), soundscape_paths
  )
  for (i in seq_len(nrow(counts))) {
    sp <- counts$soundscape_path[i]
    if (sp %in% soundscape_paths) {
      status[sp] <- if (counts$n_soi[i] == counts$n_total[i]) {
        "no_soi"
      } else {
        "segmented"
      }
    }
  }
  unname(status)
}

#' Export a DuckDB ROI table to CSV (portability converter)
#'
#' @description Store-level utility for the ROI store, exported for
#'   advanced use (not part of the everyday pipeline). The sanctioned way to get
#'   a CSV out of the ROI store. Mirrors
#'   [export_detections_duckdb_to_csv()] / [export_templates_duckdb_to_csv()]
#'   and completes the DuckDB-backend export family (G1 in plan-log-2026-06-22-001).
#'   ROIs are read per-soundscape and combined before writing.
#'
#' @param duckdb_path source ROI `.duckdb` file.
#' @param csv_path destination CSV path.
#' @param overwrite overwrite an existing CSV. Default `FALSE`.
#' @param columns optional character vector subset of columns (default all).
#' @return invisibly, the exported data.frame.
#' @keywords internal
#' @name export_rois_duckdb_to_csv
#' @export
export_rois_duckdb_to_csv <- function(duckdb_path, csv_path, overwrite = FALSE,
                                       columns = NULL) {
  if (file.exists(csv_path) && !overwrite) {
    stop("Destination CSV exists; pass overwrite = TRUE to replace it.")
  }
  con <- .roi_duckdb_connect(duckdb_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  # Collect ROIs across all soundscapes
  paths <- DBI::dbGetQuery(con,
    "SELECT DISTINCT soundscape_path FROM rois ORDER BY soundscape_path;"
  )$soundscape_path
  dfs <- lapply(paths, function(sp) .roi_duckdb_read(con, sp))
  df <- if (length(dfs) > 0) do.call(rbind, dfs) else .schema_rois(0L)
  if (!is.null(columns)) df <- df[, columns, drop = FALSE]
  utils::write.csv(df, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  invisible(df)
}

# Tolerant reader for legacy ROI CSVs (CRAN-102, LOBIO quoting defect).
# Private helper of the migrator below.
#
# The 2026-08-13 LOBIO analysis found real archives whose data rows are
# wrapped in an outer quote layer plus trailing padding commas (30 of 111
# files; 697 rows corrupted under plain read.csv). DIAG-01 (2026-09-09)
# showed no writer in this package produces that shape, but real user files
# carry it, so the migrator repairs it instead of refusing.
#
# Transform per data line, in order:
#   1. strip a trailing \r (CRLF files);
#   2. strip trailing padding commas (the 16-NA-cell signature);
#   3. when the whole line is wrapped, strip the outer quote layer;
#   4. unfold doubled quotes ("" -> ").
# Then parse with quote = "": after the transforms no field needs quoting,
# so the flag is the honest contract, not a workaround.
#
# Validation: after reading, the identity columns (soundscape_file,
# roi_label, roi_start, roi_end) must be populated on at least one row;
# otherwise the function stops naming the file. Silent corruption becomes a
# loud failure; the transforms above are the repair for legacy archives.
.read_legacy_roi_csv <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines) < 2L) {
    # header-only legacy export: a soundscape exported with zero ROIs. Valid
    # input for the migrator; return a 0-row frame with the header columns.
    hdr <- strsplit(sub("\r$", "", lines[1]), ",", fixed = TRUE)[[1]]
    out <- as.data.frame(matrix(logical(0L), 0L, length(hdr)))
    names(out) <- hdr
    return(out)
  }
  hdr <- sub("\r$", "", lines[1])
  data_lines <- lines[-1]
  clean <- vapply(data_lines, function(l) {
    l <- sub("\r$", "", l)
    trailing <- grepl(",+$", l)
    if (trailing) l <- sub(",+$", "", l)
    if (startsWith(l, "\"") && (trailing || endsWith(l, "\""))) {
      l <- substr(l, 2L, nchar(l) - 1L)
    }
    # unfold doubled quotes to a fixed point: the defect adds up to two
    # doubling layers (fwrite auto-quote + the outer line wrap), so a single
    # pass under-recovers fields whose true value is a quote character.
    while (grepl("\"\"", l, fixed = TRUE)) {
      l <- gsub("\"\"", "\"", l, fixed = TRUE)
    }
    l
  }, character(1L), USE.NAMES = FALSE)
  txt <- paste0(paste(c(hdr, clean), collapse = "\n"), "\n")
  df <- tryCatch(
    utils::read.csv(text = txt, stringsAsFactors = FALSE, quote = "",
                    fileEncoding = "UTF-8"),
    error = function(e) {
      stop("Legacy ROI CSV could not be read: ", path, " (",
           conditionMessage(e), ")")
    }
  )
  id_cols <- intersect(c("soundscape_file", "roi_label", "roi_start", "roi_end"),
                       names(df))
  if (length(id_cols) == 0L ||
      all(vapply(df[id_cols], function(x) all(is.na(x)), logical(1L)))) {
    stop(
      "Legacy ROI CSV has no readable data rows (identity columns empty): ",
      path, ". The file may use a quoting layout this migrator cannot repair;",
      " inspect it and, if it is a plain export, re-export it from the source."
    )
  }
  df
}

#' Migrate legacy CSV ROI files into a DuckDB ROI store (one-shot converter)
#'
#' @description Store-level utility for the ROI store, exported for
#'   advanced use (not part of the everyday pipeline). Reads a directory of
#'   legacy per-soundscape `*_roi_*.csv` files
#'   produced by the original pipeline (or a single legacy CSV) and persists
#'   them into a DuckDB ROI store. Complements [migrate_templates_to_db()]
#'   and [migrate_metadata_csv_to_duckdb()] (G4 in plan-log-2026-06-22-001).
#'
#'   When `csv_path` is a directory, it is scanned recursively for CSV files
#'   matching the `*_roi_*.csv` pattern (the legacy naming convention). When it
#'   is a single file, that file is read as a combined ROI table. Rows are
#'   coerced to the standard ROI schema via `.coerce_rois()` and upserted by
#'   soundscape_path.
#'
#' @param csv_path path to a legacy ROI CSV file or a directory of per-soundscape
#'   ROI CSV files.
#' @param duckdb_path destination `.duckdb` file.
#' @param overwrite overwrite an existing destination. Default `FALSE`.
#' @param recursive scan subdirectories when `csv_path` is a directory.
#'   Default `FALSE`.
#' @param workspace_root optional workspace root (CRAN-103): legacy absolute
#'   `soundscape_path` values under it are rewritten workspace-relative on
#'   migration; absolute paths outside it are kept. Absolute paths elsewhere
#'   in a store can be re-canonicalized by re-running this migrator over an
#'   export produced by [export_rois_duckdb_to_csv()].
#' @return invisibly, the migrated data.frame.
#' @keywords internal
#' @name migrate_rois_csv_to_duckdb
#' @export
migrate_rois_csv_to_duckdb <- function(csv_path, duckdb_path, overwrite = FALSE,
                                        recursive = FALSE,
                                        workspace_root = NULL) {
  if (file.exists(duckdb_path) && !overwrite) {
    stop("Destination exists; pass overwrite = TRUE to replace it.")
  }
  if (file.exists(duckdb_path) && overwrite) unlink(duckdb_path)

  if (dir.exists(csv_path)) {
    csvs <- list.files(csv_path, pattern = "_roi_.*\\.csv$", ignore.case = TRUE,
                       full.names = TRUE, recursive = recursive)
    if (length(csvs) == 0) stop("No legacy ROI CSV files found in: ", csv_path)
    dfs <- lapply(csvs, function(f) {
      df <- .read_legacy_roi_csv(f)
      .coerce_rois(df, workspace_root)
    })
    df <- do.call(rbind, dfs)
  } else {
    if (!file.exists(csv_path)) stop("File not found: ", csv_path)
    df <- .read_legacy_roi_csv(csv_path)
    df <- .coerce_rois(df, workspace_root)
  }

  con <- .signals_duckdb_connect(duckdb_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  sig <- .rois_as_signals(df, workspace_root)
  .reject_duplicate_roi_batch(sig)                   # LSA-204: write-time guard
  .signals_duckdb_upsert(con, sig)
  invisible(df)
}
