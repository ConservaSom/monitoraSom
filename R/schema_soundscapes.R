#' Standard soundscape-metadata schema (FSM-211)
#'
#' @description Single source of truth for the columns, order and R types of
#'   the `df_soundscapes` table produced by [fetch_soundscape_metadata()].
#'   Every recorder reader (FSM-216/217/218) maps its partial record onto this
#'   schema; the dispatcher (FSM-219) fills any column the source cannot supply
#'   with `NA`. Add or rename a column **here** and the whole pipeline follows.
#'
#'   Sentinel for absent values: R uses native `NA` (typed). Julia uses
#'   `missing`; DuckDB stores `NULL`; CSV writes an empty string.
#'
#' @keywords internal
#' @noRd

# Column -> R storage type. Order defines the canonical column order.
.soundscape_schema_spec <- function() {
  c(
    soundscape_path        = "character",
    soundscape_file        = "character",
    soundscape_duration    = "numeric",
    soundscape_sample_rate = "integer",
    soundscape_bitdepth    = "integer",
    soundscape_layout      = "character",
    soundscape_channel     = "character",
    soundscape_timestamp   = "POSIXct",
    soundscape_sha256      = "character",
    soundscape_mtime       = "POSIXct",
    soundscape_size_bytes  = "numeric",
    soundscape_temp_c      = "numeric",
    soundscape_lat         = "numeric",
    soundscape_lon         = "numeric",
    soundscape_elev_m      = "numeric",
    device_type            = "character",
    device_id              = "character",
    device_battery_v       = "numeric",
    device_gain            = "character"
  )
}

# A typed NA scalar for a given storage type.
.typed_na <- function(type) {
  switch(type,
    character = NA_character_,
    integer   = NA_integer_,
    numeric   = NA_real_,
    POSIXct   = as.POSIXct(NA, tz = "UTC"),
    stop(sprintf("Unknown schema type: %s", type))
  )
}

# Coerce a vector to a schema storage type (length preserved).
.coerce_type <- function(x, type) {
  if (all(is.na(x))) return(rep(.typed_na(type), length(x)))
  switch(type,
    character = as.character(x),
    integer   = as.integer(x),
    numeric   = as.numeric(x),
    POSIXct   = {
      if (inherits(x, "POSIXct")) x else as.POSIXct(x, tz = "UTC")
    },
    stop(sprintf("Unknown schema type: %s", type))
  )
}

#' Empty (or NA-filled) skeleton data.frame in standard schema order.
#' @param n number of rows (all columns NA). Default 0 -> empty typed frame.
#' @noRd
.schema_soundscapes <- function(n = 0L) {
  spec <- .soundscape_schema_spec()
  cols <- lapply(spec, function(type) rep(.typed_na(type), n))
  df <- as.data.frame(cols, stringsAsFactors = FALSE)
  names(df) <- names(spec)
  df
}

#' Map a list of partial records onto the standard schema.
#'
#' Each element of `records` is a named list holding any subset of the schema
#' columns (plus optional extra user columns from FSM-218). Missing standard
#' columns are filled with typed `NA`; types are coerced. Extra columns are
#' appended after the standard block (collision is the caller's concern,
#' FSM-218 guards it upstream).
#' @noRd
.records_to_schema <- function(records) {
  spec <- .soundscape_schema_spec()
  n <- length(records)
  out <- .schema_soundscapes(n)
  if (n == 0L) return(out)

  for (col in names(spec)) {
    vals <- lapply(records, function(r) {
      v <- r[[col]]
      if (is.null(v) || length(v) == 0L) .typed_na(spec[[col]]) else v[[1]]
    })
    out[[col]] <- .coerce_type(unlist(vals, use.names = FALSE), spec[[col]])
  }

  # Extra (non-canonical) columns: union across records, appended in order.
  extra <- setdiff(unique(unlist(lapply(records, names))), names(spec))
  for (col in extra) {
    vals <- vapply(records, function(r) {
      v <- r[[col]]
      if (is.null(v) || length(v) == 0L) NA_character_ else as.character(v[[1]])
    }, character(1))
    out[[col]] <- vals
  }
  out
}
