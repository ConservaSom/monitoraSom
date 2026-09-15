#' AudioMoth reader — WAV comment chunk (FSM-216)
#'
#' @description Parses the AudioMoth metadata embedded in the WAV
#'   `LIST/INFO/ICMT` comment chunk. Real format (firmware 1.8.x):
#'   `Recorded at 07:40:00 15/12/2023 (UTC-3) by AudioMoth 2474750763FAC60A at
#'   medium gain while battery was 4.2V and temperature was 28.2C.` Extracts
#'   the timestamp **with its timezone offset** (converted to UTC for
#'   `soundscape_timestamp`), `device_id`, `device_gain`, `device_battery_v`
#'   and `soundscape_temp_c`. GPS-disabled deployments carry no coordinates.
#'
#'   Optional deployment-level `CONFIG.TXT` congruence is **flag-only**
#'   (FSM-216 decision): on divergence keep the per-file comment value and
#'   record the discrepancy in the error log; never auto-override.
#'
#' @keywords internal
#' @noRd

# Read a named RIFF INFO sub-chunk (e.g. "ICMT", "IART") as a string.
.read_riff_chunk_string <- function(path, key) {
  n <- min(as.numeric(file.size(path)), 65536)
  if (is.na(n) || n < 12) return(NA_character_)
  raw <- readBin(path, "raw", n = n)
  pos <- grepRaw(charToRaw(key), raw, fixed = TRUE)
  if (length(pos) == 0) return(NA_character_)
  pos <- pos[1]
  if (pos + 7 > length(raw)) return(NA_character_)
  len <- sum(as.integer(raw[(pos + 4):(pos + 7)]) * 256^(0:3))
  start <- pos + 8L
  end <- min(start + len - 1L, length(raw))
  chunk <- raw[start:end]
  nul <- which(chunk == as.raw(0L))
  if (length(nul)) chunk <- chunk[seq_len(nul[1] - 1L)]
  trimws(rawToChar(chunk))
}

.read_audiomoth_comment <- function(path) .read_riff_chunk_string(path, "ICMT")

# Parse the ICMT string into a partial canonical record.
.parse_audiomoth_comment <- function(s) {
  out <- list(device_type = "audiomoth")
  if (is.na(s) || !nzchar(s)) return(out)
  ts <- regmatches(s, regexec(
    "Recorded at (\\d{2}:\\d{2}:\\d{2}) (\\d{2}/\\d{2}/\\d{4}) \\(UTC([+-]?\\d+)?\\)",
    s))[[1]]
  if (length(ts) >= 3) {
    off <- if (length(ts) >= 4 && nzchar(ts[4])) as.integer(ts[4]) else 0L
    local <- as.POSIXct(paste(ts[3], ts[2]),
                        format = "%d/%m/%Y %H:%M:%S", tz = "UTC")
    out$soundscape_timestamp <- local - off * 3600  # local(UTC+off) -> UTC
  }
  dev <- regmatches(s, regexec("by AudioMoth ([0-9A-Fa-f]+)", s))[[1]]
  if (length(dev) >= 2) out$device_id <- dev[2]
  gain <- regmatches(s, regexec("at (\\w+) gain", s))[[1]]
  if (length(gain) >= 2) out$device_gain <- tolower(gain[2])
  batt <- regmatches(s, regexec("battery was ([0-9.]+)V", s))[[1]]
  if (length(batt) >= 2) out$device_battery_v <- as.numeric(batt[2])
  temp <- regmatches(s, regexec("temperature was (-?[0-9.]+)C", s))[[1]]
  if (length(temp) >= 2) out$soundscape_temp_c <- as.numeric(temp[2])
  out
}

# Parse a deployment-level CONFIG.TXT (key : value lines) into a list.
.parse_audiomoth_config <- function(dir) {
  cfg_path <- file.path(dir, "CONFIG.TXT")
  if (!file.exists(cfg_path)) return(NULL)
  lines <- readLines(cfg_path, warn = FALSE, encoding = "UTF-8")
  kv <- regmatches(lines, regexec("^([^:]+?)\\s*:\\s*(.+?)\\s*$", lines))
  out <- list()
  for (m in kv) if (length(m) == 3) out[[trimws(m[2])]] <- trimws(m[3])
  list(
    device_id   = out[["Device ID"]],
    device_gain = if (!is.null(out[["Gain"]])) tolower(out[["Gain"]]) else NULL,
    sample_rate = suppressWarnings(as.integer(out[["Sample rate (Hz)"]])),
    time_zone   = out[["Time zone"]]
  )
}

# Flag-only congruence between the per-file comment record and CONFIG.TXT.
# Returns character() of human-readable divergences (empty when congruent).
.audiomoth_congruence <- function(rec, config) {
  if (is.null(config)) return(character(0))
  d <- character(0)
  if (!is.null(config$device_id) && !is.null(rec$device_id) &&
      !identical(config$device_id, rec$device_id)) {
    d <- c(d, sprintf("device_id CONFIG=%s comment=%s",
                      config$device_id, rec$device_id))
  }
  if (!is.null(config$device_gain) && !is.null(rec$device_gain) &&
      !identical(config$device_gain, rec$device_gain)) {
    d <- c(d, sprintf("gain CONFIG=%s comment=%s",
                      config$device_gain, rec$device_gain))
  }
  d
}

#' AudioMoth reader: core header + comment-chunk enrichment.
#' @noRd
.read_metadata_audiomoth <- function(path, sha256 = TRUE) {
  base <- .read_metadata_filename(path, spec = NULL, sha256 = sha256)
  am <- .parse_audiomoth_comment(.read_audiomoth_comment(path))
  base$device_type <- "audiomoth"
  # Comment chunk wins for timestamp (FSM-219: H > F).
  for (k in names(am)) base[[k]] <- am[[k]]
  base
}
