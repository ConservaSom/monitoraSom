#' Gather ROI tables into a `df_rois` data.frame
#'
#' @description
#' Reads the regions of interest (ROIs) produced by the segmentation app and
#' returns them as one data.frame (`df_rois`) in the standard ROI format: this
#' is pure ROI data, with no detections in it yet. It
#' reads one ROI `.duckdb` database, or many, into one table. The usual next
#' step is to split `df_rois` into a template branch (for [export_templates()])
#' and a soundscape branch (for validation).
#'
#' @details
#' The routine path is `source = "duckdb"`. This is the format the
#' segmentation app has written since monitoraSom 1.2.0. Point `rois_path` at a
#' single `.duckdb` file, or at a directory to read every ROI database found in
#' it (set `recursive = TRUE` to also search subfolders). Use `source = "csv"`
#' only to migrate old `*_roi_*.csv` exports; the output format is identical
#' either way.
#'
#' `fetch_rois()` does **not** decide which ROIs are templates and which are
#' search soundscapes. That split is yours, usually a `dplyr::filter()` on
#' `soundscape_path`. Two optional refinements: `roi_source` keeps only
#' ground-truth ROIs or only detection-derived ROIs, and `soundscape_metadata`
#' adds `origin_*` columns that record where each ROI's source recording came
#' from, so later steps can trace it back.
#'
#' Two things to know: the returned frame is always the standard 18-column
#' format (`roi_channel` present; the `roi_path`/`roi_file` columns from
#' versions before 1.2.0 are gone), so code reading those columns must
#'   adapt; and when `soundscape_metadata` is supplied, ROIs whose recording
#'   is absent from it get `NA` provenance and a warning rather than a hard
#'   error.
#'
#' @section Pipeline context:
#' Step 2 of the monitoraSom analysis flow. Reads the ROI `.duckdb` database
#' written by \code{\link{launch_segmentation_app}} (step 1). Returns a
#' `df_rois` data.frame used by \code{\link{export_templates}} (step 3, the
#' template branch) and by \code{\link{validate_by_overlap}} (the validation
#' branch).
#'
#' @param rois_path Where to read ROIs from. `NULL` (default) reads the
#'   standard ROI database `rois.duckdb` in the current working directory (the
#'   project root). For `source = "duckdb"`: a single ROI `.duckdb` file **or**
#'   a directory holding one or more of them, all read into one table. For
#'   `source = "csv"`: a directory of `*_roi_*.csv` tables from versions
#'   before 1.2.0. Must be a
#'   single, existing, non-`NA` character path.
#' @param recursive Logical. When `TRUE`, search subdirectories of `rois_path`
#'   too. Default `FALSE` (top level only). Useful to read ROIs spread across
#'   per-site/per-session subfolders.
#' @param source Which reader to use: `"duckdb"` (the routine app output,
#'   default) or `"csv"` (migration of pre-1.2.0 exports). There is no
#'   autodetection.
#' @param soundscape_metadata Optional source of origin information: a
#'   `df_soundscapes` data.frame **or** a path to a soundscape-metadata
#'   `.duckdb`. When supplied,
#'   `origin_`-prefixed columns (`origin_soundscape_path`/`_file`/`_sha256`)
#'   are joined onto `df_rois` by `soundscape_path`. `NULL` (default) skips
#'   enrichment. Requires the metadata to carry `soundscape_path`,
#'   `soundscape_file` and `soundscape_sha256`.
#' @param roi_source Optional provenance filter. `NULL` (default) returns every
#'   ROI; `"manual"` keeps only ground-truth ROIs (manual/unstamped, never
#'   detection-derived); `"detection"` keeps only detection-derived ROIs: the
#'   output of [detecs_to_rois()], the `detection_to_roi` rows promoted by
#'   [validate_by_overlap()], and, when `include_detections = TRUE`, the
#'   detection rows themselves.
#' @param include_detections Logical, default `FALSE`. When `TRUE`, projection
#'   also covers the `"detection"` and `"detection_to_roi"` rows of a unified
#'   signals store, so promoted rows and raw detections read back in ROI form
#'   (DEC-26/STEP-20d). Legacy `rois`-table stores are unaffected.
#'
#' @return A data.frame of ROIs in the standard format, one row per ROI,
#'   optionally with the three `origin_*` provenance columns added when
#'   `soundscape_metadata` is supplied. Attribute `spectro_summary` carries
#'   the distinct `roi_wl`/`roi_ovlp`/`roi_sample_rate`/`roi_pitch_shift`
#'   tuples with row counts (LSA-206): ROI bounds are only comparable across
#'   ROIs segmented under the same parameters; a `message()` flags
#'   heterogeneous inputs.
#'
#' @seealso \code{\link{launch_segmentation_app}} (the previous step that writes
#'   the ROI database); \code{\link{export_templates}} (the next step that
#'   turns the template branch into templates).
#'
#' @export
#' @examples
#' \donttest{
#' # Step 2: read the segmentation app's DuckDB ROI database into one table.
#' # Load the package
#' library(monitoraSom)
#' rois_db <- system.file("extdata", "template_trio", "rois.duckdb",
#'                        package = "monitoraSom")
#' df_rois <- fetch_rois(rois_path = rois_db)
#' str(df_rois)          # standard 18-column ROI format, one row per ROI
#'
#' # Variation: read every ROI database found under a directory (e.g. per-site):
#' rois_dir <- system.file("extdata", "template_trio", package = "monitoraSom")
#' df_all <- fetch_rois(rois_path = rois_dir, recursive = TRUE)
#' str(df_all)
#' }
fetch_rois <- function(rois_path = NULL,
                       recursive = FALSE,
                       source = c("duckdb", "csv"),
                       soundscape_metadata = NULL,
                       roi_source = NULL,
                       include_detections = FALSE) {
  source <- match.arg(source)
  if (!is.null(roi_source)) {
    roi_source <- match.arg(roi_source, c("manual", "detection"))  # AFL-02
  }
  if (!is.logical(include_detections) || length(include_detections) != 1L ||
      is.na(include_detections)) {
    stop("`include_detections` must be a single TRUE/FALSE.")
  }

  # CRAN item 7 (F2): NULL resolves to the canonical ROI store rois.duckdb
  # at the project root home (cwd-relative). No fallback creates anything: an
  # absent store stops below with an actionable message.
  if (is.null(rois_path)) {
    rois_path <- .monitora_db_default_path("rois")
  }

  # AUD-25: fail with an actionable message on a non-scalar/non-character
  # rois_path instead of a raw "length > 1" / "invalid 'file' argument" error.
  if (length(rois_path) != 1L ||
      !is.character(rois_path) || is.na(rois_path)) {
    stop("fetch_rois: 'rois_path' must be a single non-NA character path.")
  }
  if (!file.exists(rois_path)) {
    stop("The provided path to the ROI store does not exist: ", rois_path)
  }

  df_rois <- switch(
    source,
    duckdb = .fetch_rois_duckdb(rois_path, recursive, include_detections),
    csv    = .fetch_rois_csv(rois_path, recursive)
  )

  # FR-06 / AFL-06: the stored key is canonicalized (forward slashes, no leading
  # "./", collapsed "//") on every OS so downstream string matching on
  # `soundscape_path` (and the caller's grepl split) is portable and joins do not
  # silently miss on cosmetic differences.
  df_rois$soundscape_path <- .normalize_path_key(df_rois$soundscape_path)
  df_rois <- .attach_spectro_summary(df_rois)             # LSA-206(c)

  # AFL-02: optional opt-in provenance filter. "detection" keeps detection-derived
  # ROIs; "manual" keeps ground-truth (manual/NA, never detection-derived).
  if (!is.null(roi_source)) {
    is_det <- .is_detection_roi(df_rois)
    df_rois <- df_rois[if (roi_source == "detection") is_det else !is_det, ,
                       drop = FALSE]
  }

  # FR-103: optional provenance enrichment (origin_-prefixed, off by default).
  if (!is.null(soundscape_metadata)) {
    df_rois <- .attach_origin_provenance(df_rois, soundscape_metadata)
  }

  df_rois
}

# --- source readers ----------------------------------------------------------

#' Read and union ROIs from one or many ROI `.duckdb` stores (FR-101).
#'
#' `rois_path` is either a single `.duckdb` file or a directory of them. Each DB
#' is opened through [.roi_duckdb_connect()] (which additively ensures the
#' canonical schema, so an old store missing `roi_channel` reads back cleanly)
#' and its `rois` table is read in full. Unreadable DBs are reported, not
#' silently dropped (FR-02 spirit, FSM-204 pattern). The `.duckdb` extension is
#' matched **case-insensitively** (AUD-13; friendlier cross-platform).
#' @noRd
.fetch_rois_duckdb <- function(rois_path, recursive, include_detections = FALSE) {
  db_files <- if (dir.exists(rois_path)) {
    list.files(
      rois_path, pattern = "\\.duckdb$", ignore.case = TRUE,
      full.names = TRUE, recursive = recursive
    )
  } else {
    rois_path
  }
  if (length(db_files) == 0) {
    stop("No ROI .duckdb files found in path: ", rois_path)
  }

  # F4 (signals program): read the unified signals store when present; a
  # legacy `rois` table passes through unchanged (frozen compat shape). The
  # probe opens READ-ONLY — connecting through the signals layer would CREATE
  # the empty `signals` table on a legacy store and mask the layout.
  read_one <- function(db) {
    has_signals <- local({
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db, read_only = TRUE)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
      "signals" %in% DBI::dbListTables(con)
    })
    con <- if (has_signals) .signals_duckdb_connect(db) else
      .roi_duckdb_connect(db)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    if (has_signals) {
      .signals_as_rois(.signals_duckdb_read(con),
                       include_detections = include_detections)
    } else {
      cols <- paste(names(.roi_schema_spec()), collapse = ", ")
      .coerce_rois(DBI::dbGetQuery(con, sprintf("SELECT %s FROM rois;", cols)))
    }
  }
  ok <- list()
  failed <- character(0)
  for (db in db_files) {
    res <- tryCatch(read_one(db), error = function(e) {
      message(sprintf("Error reading ROI store: %s\nError: %s", db, e$message))
      NULL
    })
    if (is.null(res)) failed <- c(failed, db) else ok[[length(ok) + 1L]] <- res
  }

  if (length(ok) == 0) {
    stop("No readable ROI stores among the .duckdb files found in: ", rois_path)
  }
  if (length(failed) > 0) {
    warning(sprintf("%d ROI store(s) could not be read (first: %s)",
                    length(failed), failed[1]))
  }
  .attach_spectro_summary(.coerce_rois(dplyr::bind_rows(ok)))
}

#' Read legacy `*_roi_*.csv` tables and coerce to the canonical schema (FR-101
#' migration path).
#'
#' Reuses the original's discovery pattern (`_roi_*.csv`) but: honours
#' `recursive` (FR-05), coerces through `.coerce_rois()` so the output is the
#' canonical schema (FR-102; drops the extinct `roi_path`/`roi_file`, adds
#' `roi_channel` as `NA`), binds with `dplyr::bind_rows` against schema drift
#' (FR-04), reports unreadable files (FR-02) and reports values that failed to
#' parse to numeric/integer (FR-03) instead of coercing them to `NA` silently.
#' The `_roi_*.csv` pattern is matched **case-insensitively** (AUD-13).
#' @noRd
.fetch_rois_csv <- function(rois_path, recursive) {
  if (!dir.exists(rois_path)) {
    stop("For source = 'csv', rois_path must be a directory: ", rois_path)
  }
  csv_files <- list.files(
    rois_path, pattern = "_roi_.*\\.csv$", ignore.case = TRUE,
    full.names = TRUE, recursive = recursive
  )
  if (length(csv_files) == 0) {
    stop("No ROI tables found in path: ", rois_path)
  }

  numeric_cols <- c("roi_start", "roi_end", "roi_min_freq", "roi_max_freq")
  integer_cols <- c("roi_wl", "roi_ovlp", "roi_sample_rate", "roi_pitch_shift")

  ok <- list()
  failed <- character(0)
  parse_fail_total <- 0L
  first_parse_fail_file <- NA_character_

  for (x in csv_files) {
    df <- tryCatch(
      utils::read.csv(
        x, colClasses = "character", stringsAsFactors = FALSE,
        na.strings = c("", "NA", "NULL"), fileEncoding = "UTF-8"
      ),
      error = function(e) {
        message(sprintf("Error processing ROI file: %s\nError: %s", x, e$message))
        NULL
      }
    )
    if (is.null(df)) {
      failed <- c(failed, x)
      next
    }

    # FR-03: count strings that were present but failed to parse (NA in output
    # while the raw string was non-NA), per file.
    file_parse_fail <- 0L
    for (col in c(numeric_cols, integer_cols)) {
      if (!col %in% names(df)) next
      raw <- df[[col]]
      parsed <- suppressWarnings(as.numeric(raw))
      # A string present but unparseable to numeric became NA silently.
      file_parse_fail <- file_parse_fail + sum(!is.na(raw) & is.na(parsed))
      # AUD-24: an integer column with a non-whole value (e.g. "3.7") parses to
      # numeric but as.integer() then truncates it silently (3.7 -> 3) — a real
      # loss FR-03 intends to report, so count it too.
      if (col %in% integer_cols) {
        file_parse_fail <- file_parse_fail +
          sum(!is.na(parsed) & parsed != trunc(parsed))
      }
    }
    if (file_parse_fail > 0L) {
      parse_fail_total <- parse_fail_total + file_parse_fail
      if (is.na(first_parse_fail_file)) first_parse_fail_file <- x
    }

    ok[[length(ok) + 1L]] <- .coerce_rois(df)
  }

  if (length(ok) == 0) {
    warning("No valid ROI tables could be read from the files found")
    return(.schema_rois(0L))
  }
  if (length(failed) > 0) {
    warning(sprintf("%d ROI CSV file(s) could not be read (first: %s)",
                    length(failed), failed[1]))
  }
  if (parse_fail_total > 0L) {
    warning(sprintf(
      paste("%d value(s) failed to parse to numeric/integer and became NA",
            "(first offending file: %s). Check the source ROI tables."),
      parse_fail_total, first_parse_fail_file
    ))
  }

  # FR-02: bind only the successfully-read frames. The original's broken
  # `valid_entries` block (which indexed rows with a column-length logical and
  # padded phantom NA rows) is removed entirely.
  .attach_spectro_summary(.coerce_rois(dplyr::bind_rows(ok)))
}

# --- provenance + path helpers -----------------------------------------------

# LSA-206(c): spectrogram-parameter heterogeneity summary. The schema stores
# roi_wl/roi_ovlp/roi_sample_rate/roi_pitch_shift per ROI but nothing surfaced
# them; bounds are only comparable across ROIs segmented under the SAME
# parameters. `fetch_rois()` attaches the distinct-tuple table as attribute
# `spectro_summary` (always present, deterministic) and messages when more
# than one tuple is present -- part of the between-observer difference is
# configuration, not skill.
.spectro_param_summary <- function(df) {
  cols <- c("roi_wl", "roi_ovlp", "roi_sample_rate", "roi_pitch_shift")
  if (is.null(df) || nrow(df) == 0L) {
    empty <- data.frame(roi_wl = integer(0), roi_ovlp = integer(0),
                        roi_sample_rate = integer(0),
                        roi_pitch_shift = integer(0), n = integer(0))
    return(empty)
  }
  # string-key tabulation: stats::aggregate() drops all-NA groups, and NA
  # parameter values are legitimate (legacy rows) -- they must still count.
  key <- do.call(paste, c(lapply(df[cols], as.character), list(sep = "\r")))
  tab <- sort(table(key), decreasing = TRUE)
  parts <- strsplit(names(tab), "\r", fixed = TRUE)
  as_int <- function(x) {
    x[!is.na(x) & x == "NA"] <- NA_character_
    as.integer(x)
  }
  out <- data.frame(
    roi_wl          = as_int(vapply(parts, function(p) p[1L], character(1))),
    roi_ovlp        = as_int(vapply(parts, function(p) p[2L], character(1))),
    roi_sample_rate = as_int(vapply(parts, function(p) p[3L], character(1))),
    roi_pitch_shift = as_int(vapply(parts, function(p) p[4L], character(1))),
    n               = as.integer(unname(tab)))
  out
}

.attach_spectro_summary <- function(df) {
  s <- .spectro_param_summary(df)
  attr(df, "spectro_summary") <- s
  if (nrow(s) > 1L) {
    message(sprintf(
      paste0("LSA-206: heterogeneous spectrogram parameters across these ROIs ",
             "(%d distinct wl/ovlp/rate/pitch tuples, %d rows total). Bounds ",
             "are only comparable within one tuple. See ",
             "attr(..., 'spectro_summary')."),
      nrow(s), sum(s$n))
    )
  }
  df
}

#' Normalize a path vector to forward slashes for the durable key (FR-06).
#'
#' Use this for **physical file paths** (`template_path`, `out_path`,
#' `src_path`): it only swaps separators, so a Windows UNC root (`\\\\server`)
#' survives as `//server`. For logical `soundscape_path` **join keys** use
#' [.normalize_path_key()] instead.
#' @noRd
.to_forward_slashes <- function(x) {
  gsub("\\\\", "/", x)
}

#' Attach `origin_`-prefixed provenance columns by joining soundscape metadata
#' (FR-103).
#'
#' `soundscape_metadata` is a `df_soundscapes` data.frame or a path to a
#' soundscape-metadata `.duckdb`. The join key is `soundscape_path`, canonically
#' normalized on both sides via [.normalize_path_key()] (AFL-06). Attaches
#' `origin_soundscape_path/file/sha256`; the `origin_` prefix keeps these from
#' colliding with the search-soundscape `soundscape_*` columns downstream.
#' @noRd
.attach_origin_provenance <- function(df_rois, soundscape_metadata) {
  meta <- if (is.data.frame(soundscape_metadata)) {
    soundscape_metadata
  } else if (is.character(soundscape_metadata) &&
             length(soundscape_metadata) == 1L &&
             file.exists(soundscape_metadata)) {
    con <- .duckdb_connect(soundscape_metadata)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    .duckdb_read_metadata(con)
  } else {
    stop("soundscape_metadata must be a data.frame or a path to a .duckdb file")
  }

  # AUD-23: validate the columns this join consumes up front. A metadata frame
  # missing any of them would otherwise yield meta$col -> NULL -> NULL[idx], so
  # the origin_* column would be silently absent (or all-NA), and the gap would
  # propagate through fetch_match_grid's integrity gate unnoticed.
  required_meta <- c("soundscape_path", "soundscape_file", "soundscape_sha256")
  missing_meta <- setdiff(required_meta, names(meta))
  if (length(missing_meta) > 0L) {
    stop("soundscape_metadata is missing required column(s): ",
         paste(missing_meta, collapse = ", "),
         ". Provenance enrichment needs soundscape_path, soundscape_file and ",
         "soundscape_sha256.")
  }

  key <- .normalize_path_key(meta$soundscape_path)        # AFL-06: same rule both sides
  idx <- match(df_rois$soundscape_path, key)
  # FR-104: report ROIs whose recording is absent from soundscape_metadata
  # (their origin_* provenance is NA) instead of leaving the gap silent.
  n_unmatched <- sum(is.na(idx))
  if (n_unmatched > 0L) {
    warning(sprintf(
      paste("%d ROI(s) have no matching recording in soundscape_metadata;",
            "their origin_* provenance is NA (first unmatched: %s)."),
      n_unmatched, df_rois$soundscape_path[which(is.na(idx))[1L]]
    ))
  }
  df_rois$origin_soundscape_path   <- key[idx]
  df_rois$origin_soundscape_file   <- meta$soundscape_file[idx]
  df_rois$origin_soundscape_sha256 <- meta$soundscape_sha256[idx]
  df_rois
}
