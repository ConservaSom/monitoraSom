#' Structured error capture and dedicated error log (FSM-204 / FSM-214)
#'
#' @description Replaces the original's scattered
#'   `message("Error reading file: ...")` (which silently dropped failures)
#'   with: (1) a per-file safe read that captures the failure as a structured
#'   record, and (2) a dedicated, separately-inspectable error log written
#'   **inline with** the run, following the metadata backend (FSM-214: a
#'   `metadata_errors` table when the backend is DuckDB, otherwise a sibling
#'   `<base>_errors.csv`). An aggregated `warning()` summarises the failures
#'   (FSM-02/18).
#'
#' @keywords internal
#' @noRd

# Safe per-file read. Returns list(ok, record|NULL, error|NULL).
# `reader` is the dispatched recorder reader; `path` the WAV path.
.safe_read_metadata <- function(path, reader) {
  tryCatch(
    list(ok = TRUE, record = reader(path), error = NULL),
    error = function(e) {
      list(ok = FALSE, record = NULL, error = list(
        soundscape_path = path,
        error_class     = paste(class(e), collapse = ";"),
        error_message   = conditionMessage(e),
        timestamp       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
      ))
    }
  )
}

# Build a data.frame from a list of structured error records.
.errors_to_df <- function(errors) {
  if (length(errors) == 0L) {
    return(data.frame(
      soundscape_path = character(0), error_class = character(0),
      error_message = character(0), timestamp = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, lapply(errors, function(e) {
    data.frame(e, stringsAsFactors = FALSE)
  }))
}

# Derive the error-log sibling path from a cache/output path.
# `<base>.csv` -> `<base>_errors.csv`; explicit `errors_log_file` wins.
.errors_log_path <- function(output_file, errors_log_file) {
  if (!is.null(errors_log_file)) return(errors_log_file)
  if (is.null(output_file)) return(NULL)
  sub("(\\.[^.]+)?$", "_errors\\1", output_file)
}

# Persist the error log following the chosen backend, then warn (FSM-02/214).
# `backend` is "csv" or "duckdb"; `con` is an open DuckDB connection or NULL.
.handle_read_errors <- function(errors, output_file, errors_log_file,
                                backend = "csv", con = NULL,
                                on_error = "warn") {
  if (length(errors) == 0L) return(invisible(NULL))
  err_df <- .errors_to_df(errors)

  log_path <- .errors_log_path(output_file, errors_log_file)
  if (backend == "duckdb" && !is.null(con)) {
    .duckdb_write_errors(con, err_df)               # FSM-214 DuckDB variant
  } else if (!is.null(log_path)) {
    utils::write.csv(err_df, log_path, row.names = FALSE, fileEncoding = "UTF-8")
  }

  msg <- sprintf(
    "%d file(s) failed to read. See %s",
    nrow(err_df),
    if (backend == "duckdb") "the 'metadata_errors' table"
    else if (!is.null(log_path)) log_path else "the warning output"
  )
  if (on_error == "stop") stop(msg) else if (on_error != "skip") warning(msg)
  invisible(err_df)
}

#' Read the metadata error log back (DEC-25/STEP-20c)
#'
#' @description Returns the per-file error log produced by
#'   [fetch_soundscape_metadata()] in the standard four-column form
#'   (`soundscape_path`, `error_class`, `error_message`, `timestamp`). The
#'   reader follows the backend: a `.duckdb` cache reads the `metadata_errors`
#'   table; a `.csv` cache reads the sibling `<base>_errors.csv`; an explicit
#'   error-log path is read directly.
#'
#' @param errors_source Where the log lives. A path to a metadata cache
#'   (`.duckdb` or `.csv`) whose sibling/embedded log is read, or the path of
#'   an explicit error-log file (`.csv`). `NULL` (default) uses the standard
#'   cache `soundscapes/soundscapes_metadata.duckdb` when it exists and stops
#'   with an actionable message otherwise.
#' @return A data.frame with the four standard columns; zero rows when the log
#'   is absent or empty.
#' @seealso [fetch_soundscape_metadata()]
#' @export
#' @examples
#' \donttest{
#' # Scan a folder that holds one broken file, then read the log back
#' # without SQL:
#' demo <- file.path(tempdir(), "err_demo"); dir.create(demo, showWarnings = FALSE)
#' writeLines("not a wav", file.path(demo, "broken.wav"))
#' synth <- function(dur_s, name) {
#'   sr <- 16000
#'   w <- tuneR::normalize(tuneR::sine(4000, duration = dur_s * sr,
#'                                     samp.rate = sr), unit = "16")
#'   tuneR::writeWave(w, file.path(demo, name))
#' }
#' synth(2, "good_01.wav"); synth(2, "good_02.wav")
#' cache <- file.path(tempdir(), "err_demo.duckdb")
#' fetch_soundscape_metadata(demo, output_file = cache, on_error = "warn")
#' err <- fetch_metadata_errors(cache)
#' head(err)
#' }
fetch_metadata_errors <- function(errors_source = NULL) {
  src <- errors_source
  if (is.null(src)) {
    src <- .monitora_db_default_path("soundscapes_metadata")
  }
  if (!is.character(src) || length(src) != 1L || is.na(src) ||
      !file.exists(src)) {
    stop("fetch_metadata_errors: 'errors_source' must be the path of an ",
         "existing metadata cache or error-log file.")
  }
  empty <- data.frame(
    soundscape_path = character(0), error_class = character(0),
    error_message = character(0), timestamp = character(0),
    stringsAsFactors = FALSE
  )
  if (grepl("(?i)\\.duckdb$", src)) {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = src, read_only = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    if (!"metadata_errors" %in% DBI::dbListTables(con)) return(empty)
    out <- DBI::dbGetQuery(con, "SELECT * FROM metadata_errors;")
    return(if (nrow(out) == 0L) empty else out)
  }
  csv <- if (grepl("(?i)\\.csv$", src)) src else
    sub("(\\.[^.]+)?$", "_errors\\1", src)
  if (!file.exists(csv)) return(empty)
  utils::read.csv(csv, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
}
