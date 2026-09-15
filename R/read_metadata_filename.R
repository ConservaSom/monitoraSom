#' Generic / configurable filename-field reader (FSM-218)
#'
#' @description The fallback reader when no embedded recorder metadata exists,
#'   and a standalone tool for custom filename schemes. Reads the WAV header
#'   core (always) plus file stats, derives the fallback timestamp from the
#'   stable filename tail, and optionally splits the filename into
#'   **user-named** columns via a `filename_metadata` spec.
#'
#'   **Project rule:** the first field of the filename has **no fixed meaning**
#'   (coordinates in the user's group; collection point / sampling area /
#'   serial number elsewhere). This parser never assumes "coordinates".
#'
#'   `filename_metadata = list(sep = "_", fields = c("site", "date", "time"),
#'   parse = list(date = function(x) ...))`. `sep` may be a single string or a
#'   vector / regex of delimiters (FSM-218: N fields, multiple separators).
#'
#' @keywords internal
#' @noRd

# Reserved names every user field must avoid (whole-schema collision, FSM-218).
.reserved_metadata_names <- function() {
  c(
    names(.soundscape_schema_spec()),
    "template_path", "template_file", "template_id",
    "score_path", "roi_id", "detec_id", "diagnostic_id"
  )
}

# Validate a filename_metadata spec once, before processing (collision guard).
.validate_filename_spec <- function(spec) {
  if (is.null(spec)) return(invisible(NULL))
  if (is.null(spec$fields) || !length(spec$fields)) {
    stop("filename_metadata must provide a non-empty 'fields' vector")
  }
  clash <- intersect(spec$fields, .reserved_metadata_names())
  if (length(clash) > 0) {
    stop(sprintf(
      "filename_metadata fields collide with reserved columns: %s",
      paste(clash, collapse = ", ")
    ))
  }
  invisible(NULL)
}

# Split a basename (sans extension) into user-named fields per the spec.
.parse_filename_fields <- function(file, spec) {
  if (is.null(spec)) return(list())
  base <- tools::file_path_sans_ext(file)
  sep <- spec$sep %||% "_"
  pattern <- if (length(sep) > 1) paste(sep, collapse = "|") else sep
  parts <- strsplit(base, pattern, perl = TRUE)[[1]]
  if (length(parts) != length(spec$fields)) {
    # Field-count mismatch: emit NA for all fields (warning raised upstream).
    return(stats::setNames(
      as.list(rep(NA_character_, length(spec$fields))), spec$fields
    ))
  }
  vals <- stats::setNames(as.list(parts), spec$fields)
  for (f in names(spec$parse %||% list())) {
    if (f %in% names(vals)) vals[[f]] <- spec$parse[[f]](vals[[f]])
  }
  vals
}

# NULL-coalescing helper (R has no native `%||%` before 4.4 in base scripts).
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Generic reader: one standard record from a WAV path.
#' @noRd
.read_metadata_filename <- function(path, spec = NULL, sha256 = TRUE) {
  file <- basename(path)
  core <- .read_wav_header(path)            # errors propagate to _safe_read
  stats <- .wav_file_stats(path, sha256 = sha256)
  rec <- c(
    list(
      soundscape_path      = path,
      soundscape_file      = file,
      soundscape_timestamp = .timestamp_from_filename(file),
      device_type          = "generic"
    ),
    core, stats,
    .parse_filename_fields(file, spec)
  )
  rec
}
