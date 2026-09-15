#' Metadata cache: read, write, diff and schema validation
#'
#' @description Cache layer for [fetch_soundscape_metadata()]. Backend is
#'   chosen by `output_format` (FSM-209): `"auto"` deduces from the file
#'   extension (`.duckdb`/`.db` -> DuckDB, else CSV). DuckDB is the intended
#'   primary backend; CSV stays as an import/export format with backward
#'   compatibility. Incremental processing and missing/changed detection
#'   (FSM-203/208) live here; the schema-compatibility check (FSM-25,
#'   alternative 3) is a reusable helper shared with the converters (FSM-210).
#'
#' @keywords internal
#' @noRd

# Resolve the effective backend from output_format + the cache path.
.resolve_backend <- function(output_file, output_format = "auto") {
  output_format <- match.arg(output_format, c("auto", "csv", "duckdb"))
  if (output_format != "auto") return(output_format)
  if (is.null(output_file)) return("csv")
  ext <- tolower(tools::file_ext(output_file))
  if (ext %in% c("duckdb", "db")) "duckdb" else "csv"
}

# Validate `cache_policy` (FSM-203). `policy_supplied` flags whether the
# caller passed it explicitly (the orchestrator computes it with `missing()`).
# Returns the effective policy.
.resolve_cache_policy <- function(cache_policy, policy_supplied = TRUE) {
  choices <- c("incremental", "force_refresh", "verify_only")
  match.arg(cache_policy, choices)
}

# Coerce every canonical column present in a cache data.frame to its schema
# storage type (CSV reads everything as character/NA; empty strings -> NA).
.coerce_cache_to_schema <- function(df) {
  if (is.null(df)) return(NULL)
  spec <- .soundscape_schema_spec()
  for (col in intersect(names(spec), names(df))) {
    x <- df[[col]]
    if (is.character(x)) x[x == ""] <- NA
    df[[col]] <- .coerce_type(x, spec[[col]])
  }
  df
}

# Validate that a cache data.frame carries the canonical columns (FSM-25).
# Returns the missing canonical columns (character(0) when compatible).
.validate_cache_schema <- function(df_cache) {
  expected <- names(.soundscape_schema_spec())
  setdiff(expected, names(df_cache))
}

# Read the cache, returning a data.frame or NULL when absent.
.read_cache <- function(output_file, backend, con = NULL) {
  if (backend == "duckdb") {
    if (is.null(con)) return(NULL)
    return(.duckdb_read_metadata(con))
  }
  if (is.null(output_file) || !file.exists(output_file)) return(NULL)
  utils::read.csv(output_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
}

# Write the cache following the chosen backend.
.write_cache <- function(df, output_file, backend, con = NULL) {
  if (backend == "duckdb" && !is.null(con)) {
    .duckdb_upsert_metadata(con, df)
  } else if (!is.null(output_file)) {
    utils::write.csv(df, output_file, row.names = FALSE, fileEncoding = "UTF-8")
  }
  invisible(df)
}

# Classify disk paths against the cache (FSM-203/208).
# Returns list(new, changed, missing). `check_method`: "mtime" | "sha256" |
# "mtime+sha256". `changed` is empty when the cache lacks the needed column.
.diff_files_vs_cache <- function(paths_disk, df_cache, check_method = "mtime") {
  cached <- if (is.null(df_cache)) character(0) else df_cache$soundscape_path
  new <- setdiff(paths_disk, cached)
  missing <- setdiff(cached, paths_disk)
  both <- intersect(paths_disk, cached)
  changed <- character(0)
  if (length(both) > 0 && !is.null(df_cache)) {
    idx <- match(both, df_cache$soundscape_path)
    if (grepl("mtime", check_method) &&
        "soundscape_mtime" %in% names(df_cache)) {
      disk_mtime <- as.POSIXct(file.mtime(both),
                               tz = "UTC")
      cache_mtime <- as.POSIXct(df_cache$soundscape_mtime[idx], tz = "UTC")
      changed <- union(changed, both[!is.na(disk_mtime) &
        (is.na(cache_mtime) | disk_mtime > cache_mtime)])
    }
    if (grepl("sha256", check_method) &&
        "soundscape_sha256" %in% names(df_cache)) {
      disk_sha <- vapply(both, function(p) {
        digest::digest(p, algo = "sha256", file = TRUE)
      }, character(1))
      changed <- union(changed, both[disk_sha != df_cache$soundscape_sha256[idx]])
    }
  }
  list(new = new, changed = changed, missing = missing)
}
