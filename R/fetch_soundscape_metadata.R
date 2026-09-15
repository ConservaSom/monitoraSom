#' Read recording metadata for soundscape recordings
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Scans a directory of field recordings and returns them as one data.frame
#'   in the standard `soundscape_*` column layout: one row per recording with
#'   its duration, sample rate, timestamp, GPS data and content hash. It finds
#'   the
#'   WAV files, detects the recorder type (or you set it), reads each file's
#'   metadata with the matching reader, and stores the result in a cache
#'   database so later scans are fast. This is the soundscape-side counterpart
#'   of [fetch_template_metadata()]; the two frames are later crossed by
#'   the
#'   [fetch_match_grid()] to produce the search grid.
#'
#' @details
#'   A "soundscape" here means one continuous field recording, usually made
#'   with an autonomous recording unit (ARU). Recordings from other devices
#'   work too, as long as they are WAV files. Metadata comes from different
#'   places depending on the recorder: AudioMoth embeds it in a WAV comment
#'   chunk, Wildlife Acoustics SM4 writes a `*_Summary.txt` next to the WAV,
#'   and a generic recorder can have fields read out of its file name. Leave
#'   `recorder = "auto"`; set it manually only when the automatic detection
#'   guesses wrong.
#'
#'   The scan is **incremental** by default: only new or changed files are
#'   re-read, and the rest are served from the cache database at
#'   `output_file`. So scanning a growing folder again is fast. Use
#'   `cache_policy = "force_refresh"` to reprocess everything, or
#'   `"verify_only"` to report which files are new or changed without
#'   writing. Things to know: (1) the first file-name field has **no fixed
#'   meaning** and is never assumed to be coordinates. Supply coordinates via
#'   `external_geo` if the recorder did not embed GPS. (2) `compute_sha256 =
#'   TRUE` (default) takes most of the scan time. Set it `FALSE` for a large
#'   speedup when you do not need the content hash in later steps. (3)
#'   recordings shorter than `min_duration_s` (default 1 second) are routed
#'   to the error log (`error_class = "duration_below_minimum"`) and stay out
#'   of the flow; a scan that reads zero usable recordings stops with an
#'   error that says where
#'   the per-file reasons were logged.
#'
#' @section Pipeline context:
#'   Step 5 of the monitoraSom analysis flow. Reads a directory of field
#'   recordings (WAVs). Produces the `df_soundscapes` frame used, together
#'   with `df_templates`, by [fetch_match_grid()] (step 6). See also:
#'   \code{\link{fetch_template_metadata}}, \code{\link{fetch_match_grid}}.
#'
#' @param soundscapes_path Directory holding the recordings. When `NULL`
#'   (default), uses `"soundscapes/"` if it exists, otherwise the function
#'   stops with an error.
#' @param recursive Logical, default `TRUE`. Also search for recordings inside
#'   sub-directories.
#' @param output_file Path of the cache database, or `NULL` for no cache. The
#'   storage follows the extension (see `output_format`): `.duckdb`/`.db`
#'   selects DuckDB, any other extension selects CSV. Without `output_file`
#'   the incremental cache cannot work.
#' @param output_format `"auto"` (default; follows the extension of
#'   `output_file`), `"duckdb"` or `"csv"`.
#' @param cache_policy `"incremental"` (default; process only new or changed
#'   files), `"force_refresh"` (reprocess all) or `"verify_only"` (report
#'   which files are new, changed or missing, without writing). Needs
#'   `output_file` to have any effect.
#' @param recorder `"auto"` (default, detect), `"audiomoth"`, `"sm4"` or
#'   `"generic"`. Set explicitly only when auto-detection guesses wrong.
#' @param filename_metadata Optional `list(sep, fields, parse)` to read extra
#'   columns out of the recording file names (useful for generic recorders).
#'   `sep` splits the file name into fields, `fields` names them, and `parse`
#'   converts named fields with a function. Example:
#'   `list(sep = "_", fields = c("site", "temp_c"), parse = list(temp_c =
#'   as.numeric))` turns `siteA_23.5.wav` into `site = "siteA"` and
#'   `temp_c = 23.5`. The first field has no fixed meaning and is never
#'   treated as coordinates. See the last example below.
#' @param external_geo Optional data.frame of decimal coordinates for recordings
#'   whose GPS is not embedded, with columns `soundscape_file`, `latitude`,
#'   `longitude` (decimal degrees, range-checked). Lowest-priority source: it
#'   only fills `soundscape_lat`/`soundscape_lon` left `NA` by the reader; a
#'   GPS found in an SM4 summary or in the WAV header always wins.
#' @param errors_log_file Optional explicit path for the per-file error log.
#'   When `NULL` (the default), the log is derived from `output_file`: a
#'   sibling CSV file for the CSV backend, or the `metadata_errors` table
#'   inside the cache database for the DuckDB backend.
#' @param min_duration_s Numeric, default `1`. Minimum recording duration (in
#'   seconds) a readable file must have to stay in the flow; shorter recordings
#'   are routed to the error log and excluded from the result. Set `0` to
#'   disable the floor. (DEC-9/STEP-10: a too-short recording is one of the
#'   problem-recording classes; see Things to know.)
#' @param on_error `"warn"` (default), `"stop"` or `"skip"` for individual read
#'   failures.
#' @param ncores Positive integer, default `1`. Number of CPU cores used to
#'   scan the recordings in parallel (multiple cores work on macOS and Linux).
#'   Raise it to speed up large scans.
#' @param compute_sha256 Logical, default `TRUE`. Compute the sha256 hash of
#'   each file's full content into `soundscape_sha256`. Later steps use this
#'   hash to check that recordings did not change ([fetch_match_grid()] can
#'   verify it). Set `FALSE` to skip the hashing for a large speedup, at the
#'   cost of an `NA` hash column; the cache still notices new or changed files
#'   by their modification time.
#' @param pb Logical, default `TRUE`. Show the progress bar. Set `FALSE` for
#'   non-interactive pipelines, such as rendering rmarkdown reports.
#'
#' @return A data.frame in the standard `soundscape_*` column layout, one row
#'   per recording, carrying attributes `"new"`, `"changed"` and `"missing"` that
#'   list the diff against the cache.
#'
#' @seealso [fetch_template_metadata()] (step 4), [fetch_match_grid()]
#'   (downstream, step 6).
#' @export
#' @examples
#' \dontrun{
#' # Load the package
#' library(monitoraSom)
#' # Step 5: scan a folder of recordings into df_soundscapes.
#' # (The package ships no field recordings, so this example synthesizes two;
#' # for the same steps on real data see [fetch_example_data()].)
#' rec_dir <- file.path(tempdir(), "recs"); dir.create(rec_dir, showWarnings = FALSE)
#' rec <- tuneR::normalize(tuneR::sine(4000, duration = 10 * 16000,
#'                                     samp.rate = 16000), unit = "16")
#' for (f in c("siteA_01.wav", "siteA_02.wav"))
#'   tuneR::writeWave(rec, file.path(rec_dir, f))
#' df_soundscapes <- fetch_soundscape_metadata(rec_dir)
#' head(df_soundscapes[, c("soundscape_file", "soundscape_duration",
#'                         "soundscape_sample_rate")])
#'
#' # Faster scan when you do not need the content-integrity hash:
#' df_soundscapes <- fetch_soundscape_metadata(rec_dir, compute_sha256 = FALSE)
#'
#' # Read extra columns out of the file names (generic recorder):
#' # "siteA_01.wav" -> a site column ("siteA") and a numeric id column (1).
#' df <- fetch_soundscape_metadata(
#'   rec_dir,
#'   filename_metadata = list(sep = "_",
#'                            fields = c("site", "id"),
#'                            parse = list(id = as.numeric)))
#' df[, c("soundscape_file", "site", "id")]
#'
#' # Demo: the incremental cache. Scan 4 readable 60 s recordings twice; the
#' # second scan re-reads only new or changed files (none here), so it is much
#' # faster.
#' demo_dir <- file.path(tempdir(), "recs"); dir.create(demo_dir, showWarnings = FALSE)
#' synth <- function(sec, file) {
#'   r <- tuneR::normalize(tuneR::sine(4000, duration = sec * 16000,
#'                                     samp.rate = 16000), unit = "16")
#'   tuneR::writeWave(r, file.path(demo_dir, file))
#' }
#' for (i in 1:4) synth(60, sprintf("rec_%02d.wav", i))   # good 60 s recordings
#'
#' cache_db <- file.path(tempdir(), "soundscapes_metadata.duckdb")
#' t_first  <- system.time(df <- fetch_soundscape_metadata(
#'   demo_dir, output_file = cache_db))
#' t_second <- system.time(df <- fetch_soundscape_metadata(
#'   demo_dir, output_file = cache_db))   # re-reads only new/changed files
#' t_first; t_second    # the gain grows with the folder size
#'
#' # The scan diff (vs the cache) travels as attributes:
#' attr(df, "new"); attr(df, "changed"); attr(df, "missing")
#'
#' # Demo: the error log. Add one broken file and rescan with on_error = "warn":
#' # the scan keeps the good recordings and reports the bad file. The per-file
#' # reasons live in the cache database's metadata_errors table, read back with
#' # [fetch_metadata_errors()] (no SQL needed).
#' writeLines("not a wav", file.path(demo_dir, "rec_05_broken.wav"))
#' df <- fetch_soundscape_metadata(demo_dir, output_file = cache_db,
#'                                on_error = "warn")
#' attr(df, "new")             # the broken file is retried on every scan
#' fetch_metadata_errors(cache_db)
#' }
fetch_soundscape_metadata <- function(
    soundscapes_path = NULL, recursive = TRUE, output_file = NULL,
    output_format = "auto", cache_policy = "incremental",
    recorder = "auto", filename_metadata = NULL,
    external_geo = NULL, errors_log_file = NULL, on_error = "warn",
    min_duration_s = 1, ncores = 1, compute_sha256 = TRUE, pb = TRUE) {

  recorder <- match.arg(recorder, c("auto", "audiomoth", "sm4", "generic"))
  on_error <- match.arg(on_error, c("warn", "stop", "skip"))

  # AUD-37: validate the scalar flags up front so an NA/vector value fails with
  # an actionable message here, not with a raw error deep in the per-file reader
  # (which may run across parallel workers, mid-scan).
  if (!is.numeric(min_duration_s) || length(min_duration_s) != 1L ||
      is.na(min_duration_s) || min_duration_s < 0) {
    stop("'min_duration_s' must be a single non-negative number of seconds.")
  }
  if (!is.logical(compute_sha256) || length(compute_sha256) != 1L ||
      is.na(compute_sha256)) {
    stop("'compute_sha256' must be a single TRUE or FALSE.")
  }
  if (!is.logical(pb) || length(pb) != 1L || is.na(pb)) {
    stop("'pb' must be a single TRUE or FALSE.")
  }
  if (!is.logical(recursive) || length(recursive) != 1L || is.na(recursive)) {
    stop("'recursive' must be a single TRUE or FALSE.")
  }
  if (!is.numeric(ncores) || length(ncores) != 1L || is.na(ncores) ||
      ncores < 1) {
    stop("'ncores' must be a single positive integer.")
  }

  policy <- .resolve_cache_policy(cache_policy,
                                  policy_supplied = !missing(cache_policy))
  .validate_filename_spec(filename_metadata)
  external_geo <- .validate_external_geo(external_geo)

  soundscapes_path <- .resolve_soundscapes_path(soundscapes_path)
  paths <- .discover_soundscapes(soundscapes_path, recursive)

  # FEAT-07: warn once when persisting the cache to an unmarked path.
  if (!is.null(output_file)) {
    .require_explicit_workspace(output_file, label = "output_file",
                                caller = "fetch_soundscape_metadata")
  }

  backend <- .resolve_backend(output_file, output_format)
  con <- NULL
  if (backend == "duckdb" && !is.null(output_file)) {
    con <- .duckdb_connect(output_file)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  }

  df_cache <- .align_cache_schema(.read_cache(output_file, backend, con))
  diff <- .diff_files_vs_cache(paths, df_cache)
  to_process <- .select_to_process(paths, diff, policy)

  rd <- .make_reader(if (recorder == "auto")
    .detect_recorder(soundscapes_path, paths) else recorder,
    soundscapes_path, paths, filename_metadata, sha256 = compute_sha256)

  # Read-side sources used to prime mirai daemons (FSM-212); harmless on the
  # mclapply/serial fallbacks, which ignore `worker_setup`.
  reads <- .par_map(to_process, function(p) .safe_read_metadata(p, rd$reader),
                    ncores, worker_setup = .METADATA_WORKER_SOURCES, pb = pb)
  ok <- Filter(function(r) isTRUE(r$ok), reads)
  errs <- lapply(Filter(function(r) !isTRUE(r$ok), reads), `[[`, "error")
  errs <- c(errs, if (!is.null(rd$divergences)) rd$divergences$rows)

  processed <- .records_to_schema(lapply(ok, `[[`, "record"))
  # DEC-9 (STEP-10): readable-but-too-short recordings are excluded from the
  # flow and reported through the same structured error path as unreadable
  # files. The floor runs on the assembled result so cached rows are covered
  # too; error rows are re-derived from the cache on every scan (like broken
  # files, they are retried and re-logged, FSM-204 shape).
  if (min_duration_s > 0 && !is.null(processed) && nrow(processed) > 0L) {
    too_short <- !is.na(processed$soundscape_duration) &
      processed$soundscape_duration < min_duration_s
    if (any(too_short)) {
      short_errs <- lapply(processed$soundscape_path[too_short], function(p) {
        list(soundscape_path = p,
             error_class     = "duration_below_minimum",
             error_message   = sprintf(
               "recording duration %.3f s is below min_duration_s = %g",
               processed$soundscape_duration[
                 processed$soundscape_path == p][1], min_duration_s),
             timestamp       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
      })
      errs <- c(errs, short_errs)
      processed <- processed[!too_short, , drop = FALSE]
    }
  }
  res <- .assemble_result(processed, df_cache, diff, policy)
  res <- .apply_external_geo(res, external_geo)   # FSM-219 'E' source (fills NA)

  .handle_read_errors(errs, output_file, errors_log_file, backend, con,
                      on_error)
  .maybe_write_missing_log(diff$missing, output_file, backend)
  # B.5: a verify_only run is a diagnostic that may legitimately be empty; any
  # other call that would return zero usable records is an error, with a message
  # precise enough to localise the cause (before writing an empty cache).
  if (policy != "verify_only" && nrow(res) == 0L) {
    .stop_empty_result(paths, to_process, ok, output_file, errors_log_file,
                       backend)
  }
  if (policy != "verify_only" && !is.null(output_file)) {
    # CRAN item 3 (§5, rev. 6): the cache's parent directory (conventionally
    # soundscapes/) is created on demand by this export — set_workspace no
    # longer lays it out.
    out_dir <- dirname(output_file)
    if (out_dir != "." && !dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }
    .write_cache(res, output_file, backend, con)
  }
  if (min_duration_s == 0 && any(res$soundscape_duration < 1, na.rm = TRUE)) {
    warning(sprintf(
      "%d recording(s) have duration < 1 second; check for possible errors.",
      sum(res$soundscape_duration < 1, na.rm = TRUE)
    ))
  }
  # AFL-29: provenance check only (warn-only, no schema column). Runs here, in
  # the parent, after assembly -- see .check_timestamp_divergence for why it
  # cannot live inside the parallel per-file reader.
  .check_timestamp_divergence(res)
  attr(res, "new") <- diff$new
  attr(res, "changed") <- diff$changed
  attr(res, "missing") <- diff$missing
  res
}

# --- Private helpers ---------------------------------------------------------

# Read-side source files that define the (unexported) functions the per-file
# reader closure resolves by name on each parallel worker (FSM-212). Used as
# `worker_setup` for `.par_map`; resolved against getOption("monitoraSom.src_dir")
# by `.worker_bootstrap`. Order matters (dependency order).
.METADATA_WORKER_SOURCES <- c(
  "_schema_soundscapes.R", "_read_metadata_core.R", "_read_metadata_filename.R",
  "_read_metadata_audiomoth.R", "_read_metadata_sm4.R", "_detect_recorder.R",
  "_metadata_errors.R"
)

.resolve_soundscapes_path <- function(soundscapes_path) {
  if (is.null(soundscapes_path)) {
    # AUD-35: do NOT create a directory here (workspace creation belongs to
    # set_workspace, the SW-01 anti-pattern). Default to ./soundscapes/ only when
    # it already exists; otherwise fail with an actionable message.
    if (!dir.exists("soundscapes")) {
      stop("No 'soundscapes_path' provided and the default './soundscapes/' ",
           "directory does not exist. Provide soundscapes_path, or create the ",
           "workspace with set_workspace() first.")
    }
    return("soundscapes/")
  }
  if (!dir.exists(soundscapes_path)) {
    stop("The provided path to the soundscapes does not exist")
  }
  soundscapes_path
}

# FSM-219 ('E' coordinate source). external_geo is an optional in-R data.frame
# of decimal coordinates the user supplies for recordings whose GPS is not
# embedded in the file or summary. Required columns: soundscape_file, latitude,
# longitude (decimal degrees). Validated once, up front; returns it unchanged
# (or NULL). It is a LOW-priority source applied by `.apply_external_geo`: it
# only fills lat/lon left NA by the reader (SM4 summary / header GPS win).
.validate_external_geo <- function(external_geo) {
  if (is.null(external_geo)) return(NULL)
  if (!is.data.frame(external_geo)) {
    stop("external_geo must be a data.frame with columns soundscape_file, ",
         "latitude, longitude")
  }
  req <- c("soundscape_file", "latitude", "longitude")
  miss <- setdiff(req, names(external_geo))
  if (length(miss) > 0) {
    stop("external_geo is missing required column(s): ",
         paste(miss, collapse = ", "))
  }
  if (!is.numeric(external_geo$latitude) ||
      !is.numeric(external_geo$longitude)) {
    stop("external_geo latitude/longitude must be numeric decimal degrees ",
         "(e.g. -23.55, not a degrees-minutes-seconds string)")
  }
  lat <- external_geo$latitude
  lon <- external_geo$longitude
  bad_lat <- !is.na(lat) & (lat < -90 | lat > 90)
  bad_lon <- !is.na(lon) & (lon < -180 | lon > 180)
  if (any(bad_lat)) {
    stop(sprintf("external_geo latitude out of range [-90, 90]: %s",
                 paste(unique(lat[bad_lat]), collapse = ", ")))
  }
  if (any(bad_lon)) {
    stop(sprintf("external_geo longitude out of range [-180, 180]: %s",
                 paste(unique(lon[bad_lon]), collapse = ", ")))
  }
  dup <- unique(external_geo$soundscape_file[
    duplicated(external_geo$soundscape_file)])
  if (length(dup) > 0) {
    stop("external_geo has duplicate soundscape_file row(s): ",
         paste(dup, collapse = ", "))
  }
  external_geo
}

# Fill soundscape_lat/lon from external_geo where the reader left them NA,
# matching by soundscape_file (FSM-219 'E', below the embedded GPS sources).
.apply_external_geo <- function(res, external_geo) {
  if (is.null(external_geo) || nrow(res) == 0L) return(res)
  idx <- match(res$soundscape_file, external_geo$soundscape_file)
  # AUD-36: coordinates are a unit — fill only rows where BOTH lat and lon are
  # NA, so a reader-supplied half-pair is never completed from a second source
  # (a mixed-source point could be geographically incoherent).
  fill <- is.na(res$soundscape_lat) & is.na(res$soundscape_lon) & !is.na(idx)
  res$soundscape_lat[fill] <- external_geo$latitude[idx[fill]]
  res$soundscape_lon[fill] <- external_geo$longitude[idx[fill]]
  res
}

# B.5: error (not a silent empty frame) when a non-diagnostic run yields zero
# usable records, with a message precise enough to localise the cause.
.stop_empty_result <- function(paths, to_process, ok, output_file,
                               errors_log_file, backend) {
  n_failed <- length(to_process) - length(ok)
  where <- if (!is.null(errors_log_file)) {
    sprintf(" See the error log '%s' for per-file reasons.", errors_log_file)
  } else if (!is.null(output_file) && backend == "duckdb") {
    " See the 'metadata_errors' table for per-file reasons."
  } else if (!is.null(output_file)) {
    " An '*_errors' log was written next to the output file."
  } else {
    " Pass `errors_log_file=` or `on_error=\"stop\"` to surface per-file reasons."
  }
  stop(sprintf(
    paste0("fetch_soundscape_metadata produced no usable records: %d WAV ",
           "file(s) discovered, %d processed this run, %d failed to read, ",
           "0 records returned.%s"),
    length(paths), length(to_process), n_failed, where))
}

# Discover WAVs: anchored regex (FSM-07), .txt/non-audio guard (FSM-201),
# honoured `recursive` (FSM-01), deterministic sort (FSM-15).
.discover_soundscapes <- function(soundscapes_path, recursive) {
  raw <- list.files(soundscapes_path, pattern = "\\.wav$", ignore.case = TRUE,
                    full.names = TRUE, recursive = recursive)
  raw <- raw[tolower(tools::file_ext(raw)) == "wav"]
  if (length(raw) == 0) stop("There are no WAV files in the provided path")
  sort(unname(raw))
}

# Files to read this run, per cache policy (FSM-203).
.select_to_process <- function(paths, diff, policy) {
  switch(policy,
    force_refresh = paths,
    verify_only   = character(0),
    incremental   = union(diff$new, diff$changed)
  )
}

# Rename the legacy bit-depth column (FSM-13/25) and coerce cache columns to
# the canonical types so they bind cleanly with freshly processed rows.
.align_cache_schema <- function(df_cache) {
  if (is.null(df_cache)) return(NULL)
  if ("soundscape_bitrate" %in% names(df_cache) &&
      !"soundscape_bitdepth" %in% names(df_cache)) {
    names(df_cache)[names(df_cache) == "soundscape_bitrate"] <-
      "soundscape_bitdepth"
  }
  .coerce_cache_to_schema(df_cache)
}

# Merge processed rows with retained cache rows; drop missing; sort (FSM-03/06).
.assemble_result <- function(processed, df_cache, diff, policy) {
  if (is.null(df_cache)) {
    res <- processed
  } else if (policy == "force_refresh") {
    res <- processed
  } else {
    retained_paths <- setdiff(df_cache$soundscape_path,
                              c(diff$new, diff$changed, diff$missing))
    retained <- df_cache[df_cache$soundscape_path %in% retained_paths, ,
                         drop = FALSE]
    res <- dplyr::bind_rows(retained, processed)
  }
  res <- res[!res$soundscape_path %in% diff$missing, , drop = FALSE]
  res[order(res$soundscape_path), , drop = FALSE]
}

# Preserve the original's timestamped missing-recordings log (CSV backend).
.maybe_write_missing_log <- function(missing, output_file, backend) {
  if (length(missing) == 0 || is.null(output_file) || backend == "duckdb") {
    return(invisible(NULL))
  }
  log_file <- sub("(\\.[^.]+)?$",
                  paste0("_missing_recordings_log_",
                         format(Sys.time(), "%Y%m%d_%H%M%S"), "\\1"),
                  output_file)
  utils::write.csv(data.frame(missing_files = missing), log_file,
                   row.names = FALSE, fileEncoding = "UTF-8")
  message("A list of cached-but-missing recordings was saved in ", log_file)
  invisible(log_file)
}
