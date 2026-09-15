#' Core WAV-header read and file-stat helpers (shared by all recorder readers)
#'
#' @description The core columns (`soundscape_duration`,
#'   `soundscape_sample_rate`, `soundscape_bitdepth`, `soundscape_layout`) come
#'   from the WAV header for **every** recorder (FSM-219 precedence: always
#'   WAV header). File-level stats (`soundscape_sha256`, `soundscape_mtime`,
#'   `soundscape_size_bytes`) come from the filesystem (FSM-11/17/211). The
#'   stable `..._YYYYMMDD_HHMMSS` filename tail yields a fallback timestamp
#'   (FSM-219: timestamp precedence header > filename).
#'
#' @keywords internal
#' @noRd

# Map channel count to the layout label (FSM-211 vocabulary).
.layout_from_channels <- function(channels) {
  if (is.na(channels)) return(NA_character_)
  if (channels == 1L) "mono" else if (channels == 2L) "stereo" else "other"
}

# Validate a WAV header against physical bounds (FSM-08). Returns the validated
# header list or signals an error (caught upstream by the safe-read wrapper).
.validate_wav_header <- function(h, path) {
  sr <- h$sample.rate
  ch <- h$channels
  bits <- h$bits
  if (is.null(sr) || is.na(sr) || sr < 1000 || sr > 384000) {
    stop(sprintf("Invalid sample rate (%s Hz) in '%s'", sr, path))
  }
  if (is.null(ch) || is.na(ch) || ch < 1) {
    stop(sprintf("Invalid channel count (%s) in '%s'", ch, path))
  }
  if (is.null(bits) || is.na(bits) || !(bits %in% c(8L, 16L, 24L, 32L))) {
    stop(sprintf("Invalid bit depth (%s) in '%s'", bits, path))
  }
  h
}

# Read and validate the WAV header. Errors propagate (caught by _safe_read).
.read_wav_header <- function(path) {
  h <- tuneR::readWave(path, header = TRUE)
  h <- .validate_wav_header(h, path)
  list(
    soundscape_sample_rate = as.integer(h$sample.rate),
    soundscape_bitdepth    = as.integer(h$bits),
    soundscape_duration    = as.numeric(h$samples) / as.numeric(h$sample.rate),
    soundscape_layout      = .layout_from_channels(h$channels)
  )
}

# Filesystem-level stats. `sha256 = FALSE` skips the (streaming) hash for speed.
.wav_file_stats <- function(path, sha256 = TRUE) {
  list(
    soundscape_size_bytes = as.numeric(file.size(path)),
    soundscape_mtime      = as.POSIXct(file.mtime(path), tz = "UTC"),
    soundscape_sha256     = if (sha256) {
      digest::digest(path, algo = "sha256", file = TRUE)
    } else {
      NA_character_
    }
  )
}

# Fallback timestamp from the stable `..._YYYYMMDD_HHMMSS.<ext>` filename tail.
# Returns NA when the tail is absent (FIELD1 itself is never interpreted).
.timestamp_from_filename <- function(file) {
  m <- regmatches(
    file,
    regexec("(\\d{8})_(\\d{6})\\.[^.]+$", file)
  )[[1]]
  if (length(m) < 3) return(as.POSIXct(NA, tz = "UTC"))
  as.POSIXct(paste(m[2], m[3]), format = "%Y%m%d %H%M%S", tz = "UTC")
}

# AFL-29: provenance check on the timestamp -- warn when the value that won the
# FSM-219 precedence (header > filename) disagrees with the filename tail.
#
# Deliberately WARN-ONLY and computed in the PARENT, after assembly:
#
#  * No schema change (user decision, 2026-06-28): the rejected alternative
#    added a `soundscape_timestamp_source` column, which would have propagated
#    to the DuckDB store, the Julia port and the FSM goldens. Simplicity and
#    stability were the stated priority.
#  * Not computed inside the per-file reader, because that runs on `.par_map`
#    workers where `warning()` does not propagate reliably and closure
#    environments do not travel back to the parent.
#
# Only recorders that take the timestamp from the WAV header (AudioMoth) can
# ever diverge; for filename-derived recorders the two values are identical by
# construction, so this is a silent no-op rather than a false-positive source.
# Rows with either side NA are skipped -- a missing timestamp is not a
# divergence, and is already reported through the metadata_errors path.
.check_timestamp_divergence <- function(df, tolerance_s = 1) {
  if (is.null(df) || nrow(df) == 0L ||
      !all(c("soundscape_path", "soundscape_timestamp") %in% names(df))) {
    return(invisible(character()))
  }
  from_name <- as.POSIXct(
    vapply(basename(df$soundscape_path),
           function(f) as.numeric(.timestamp_from_filename(f)),
           numeric(1), USE.NAMES = FALSE),
    origin = "1970-01-01", tz = "UTC"
  )
  delta <- abs(as.numeric(
    difftime(df$soundscape_timestamp, from_name, units = "secs")
  ))
  hit <- which(!is.na(delta) & delta > tolerance_s)
  if (length(hit) == 0L) return(invisible(character()))

  detail <- sprintf(
    "%s (header %s vs filename %s; %.0fs apart)",
    basename(df$soundscape_path[hit]),
    format(df$soundscape_timestamp[hit], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    format(from_name[hit], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    delta[hit]
  )
  shown <- utils::head(detail, 5L)
  warning(sprintf(
    paste0("%d recording(s) have a WAV-header timestamp that disagrees with ",
           "the filename by more than %gs. The header value was kept ",
           "(FSM-219 precedence).\n  %s%s"),
    length(hit), tolerance_s, paste(shown, collapse = "\n  "),
    if (length(detail) > length(shown)) {
      sprintf("\n  ... and %d more", length(detail) - length(shown))
    } else {
      ""
    }
  ), call. = FALSE)
  invisible(detail)
}
