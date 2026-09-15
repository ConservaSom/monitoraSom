#' SongMeter SM4 reader — `*_Summary.txt` metadata table (FSM-217)
#'
#' @description Joins each WAV to its row in the SM4 recorder summary table.
#'   Real header: `DATE,TIME,LAT,,LON,,POWER(V),TEMP(C),#FILES,MIC0 TYPE,MIC1
#'   TYPE` (the unnamed columns after LAT/LON are hemisphere letters `s`/`w`).
#'   DATE is `%Y-%b-%d` (English month), TIME `%H:%M:%S`. Authoritative
#'   coordinates, temperature and power live here.
#'
#'   FSM-217 decisions: multiple `*_Summary.txt` may coexist (data merged from
#'   several memory cards) — import all and **merge** before joining; non-WAV
#'   files in the directory are ignored; auto-imported when the folder is SM4;
#'   no summary present -> fall back to the filename reader (FSM-218). The
#'   standard timestamp comes from the filename (FSM-219: header > filename);
#'   the summary TIME is the **join key** (DATE + nearest TIME within a
#'   tolerance window), not the standard timestamp.
#'
#' @keywords internal
#' @noRd

# Locate `*_Summary.txt` files near the soundscapes root and the WAV dirs.
.find_sm4_summaries <- function(soundscapes_path, paths = character(0)) {
  dirs <- unique(c(soundscapes_path, dirname(paths),
                   dirname(dirname(paths))))
  dirs <- dirs[dir.exists(dirs)]
  unique(unlist(lapply(dirs, function(d) {
    list.files(d, pattern = "_Summary\\.txt$", full.names = TRUE)
  })))
}

# Parse the SM4 `%Y-%b-%d %H:%M:%S` datetime locale-independently. The device
# always writes English month abbreviations, so map them explicitly instead of
# `%b`, which follows the session's LC_TIME locale (e.g. fails under pt_BR).
.parse_sm4_datetime <- function(date_str, time_str) {
  mon <- c(Jan = "01", Feb = "02", Mar = "03", Apr = "04", May = "05",
           Jun = "06", Jul = "07", Aug = "08", Sep = "09", Oct = "10",
           Nov = "11", Dec = "12")
  parts <- strsplit(as.character(date_str), "-", fixed = TRUE)
  y <- vapply(parts, function(p) p[1], character(1))
  m <- unname(mon[vapply(parts, function(p) p[2], character(1))])
  d <- vapply(parts, function(p) p[3], character(1))
  iso <- ifelse(is.na(m), NA_character_,
                sprintf("%s-%s-%s %s", y, m, d, time_str))
  as.POSIXct(iso, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

# Read and merge one or more SM4 summary tables into a single data.frame.
.read_sm4_summaries <- function(summary_paths) {
  cols <- c("DATE", "TIME", "LAT", "LAT_HEM", "LON", "LON_HEM",
            "POWER_V", "TEMP_C", "NFILES", "MIC0", "MIC1")
  frames <- lapply(summary_paths, function(p) {
    df <- utils::read.csv(p, header = TRUE, skip = 0, strip.white = TRUE,
                          stringsAsFactors = FALSE, check.names = FALSE,
                          fileEncoding = "UTF-8")
    names(df)[seq_len(min(length(cols), ncol(df)))] <-
      cols[seq_len(min(length(cols), ncol(df)))]
    df
  })
  merged <- do.call(rbind, frames)
  ts <- .parse_sm4_datetime(merged$DATE, merged$TIME)
  data.frame(
    summary_time = ts,
    lat = .apply_hemisphere(as.numeric(merged$LAT), merged$LAT_HEM, "s"),
    lon = .apply_hemisphere(as.numeric(merged$LON), merged$LON_HEM, "w"),
    temp_c = as.numeric(merged$TEMP_C),
    power_v = as.numeric(merged$POWER_V),
    stringsAsFactors = FALSE
  )
}

# Sign a coordinate by hemisphere letter (s/w -> negative).
.apply_hemisphere <- function(value, hemisphere, negative_letter) {
  neg <- tolower(trimws(as.character(hemisphere))) == negative_letter
  ifelse(neg & !is.na(neg), -abs(value), abs(value))
}

#' Build an SM4 reader closure bound to a merged summary table.
#' `summary_df` may be NULL -> the reader degrades to the filename reader.
#' @param tolerance_s join window around the filename timestamp (default 300s).
#' @noRd
.make_sm4_reader <- function(summary_df, spec = NULL, sha256 = TRUE,
                             tolerance_s = 300) {
  function(path) {
    base <- .read_metadata_filename(path, spec = spec, sha256 = sha256)
    base$device_type <- "sm4"
    base$device_id <- sub("_\\d{8}_\\d{6}\\.[^.]+$", "", basename(path))
    if (is.null(summary_df) || nrow(summary_df) == 0L ||
        is.na(base$soundscape_timestamp)) {
      return(base)
    }
    dt <- abs(as.numeric(difftime(summary_df$summary_time,
                                  base$soundscape_timestamp, units = "secs")))
    i <- which.min(dt)
    if (length(i) == 1L && dt[i] <= tolerance_s) {
      base$soundscape_lat <- summary_df$lat[i]
      base$soundscape_lon <- summary_df$lon[i]
      base$soundscape_temp_c <- summary_df$temp_c[i]
      base$device_battery_v <- summary_df$power_v[i]
    }
    base
  }
}
