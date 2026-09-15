#' Recorder auto-detection and reader dispatch (FSM-219)
#'
#' @description Detects the recorder type for a directory and builds the
#'   matching reader closure (FSM-215 dispatcher + FSM-216/217/218 readers).
#'   Detection heuristic (`recorder = "auto"`): a non-empty AudioMoth ICMT
#'   comment chunk -> `"audiomoth"`; else a sibling `*_Summary.txt` ->
#'   `"sm4"`; else `"generic"`. An explicit `recorder` value overrides
#'   detection.
#'
#'   The per-field metadata-source precedence (FSM-219, confirmed): exact
#'   timestamp = header > filename (SM4 summary TIME is the join key);
#'   lat/lon/elev = SM4 summary > header GPS > external > filename decoder;
#'   temperature = summary > header; battery/gain = header > CONFIG; device_id
#'   = header > CONFIG > filename; sample_rate/bitdepth/duration/layout = WAV
#'   header always.
#'
#' @keywords internal
#' @noRd

.detect_recorder <- function(soundscapes_path, paths) {
  for (p in utils::head(paths, 5L)) {
    cm <- tryCatch(.read_audiomoth_comment(p), error = function(e) NA_character_)
    if (!is.na(cm) && grepl("AudioMoth", cm)) return("audiomoth")
  }
  if (length(.find_sm4_summaries(soundscapes_path, paths)) > 0) return("sm4")
  "generic"
}

# Build the reader closure for a recorder type. Returns
# list(reader = function(path), divergences = env|NULL) where `divergences`
# accumulates flag-only congruence rows (AudioMoth, FSM-216).
.make_reader <- function(recorder, soundscapes_path, paths,
                         filename_metadata = NULL, sha256 = TRUE) {
  if (recorder == "sm4") {
    sums <- .find_sm4_summaries(soundscapes_path, paths)
    summary_df <- if (length(sums) > 0) .read_sm4_summaries(sums) else NULL
    return(list(
      reader = .make_sm4_reader(summary_df, spec = filename_metadata,
                                sha256 = sha256),
      divergences = NULL
    ))
  }
  if (recorder == "audiomoth") {
    config <- .parse_audiomoth_config(soundscapes_path)
    div <- new.env(parent = emptyenv())
    div$rows <- list()
    reader <- function(path) {
      rec <- .read_metadata_audiomoth(path, sha256 = sha256)
      d <- .audiomoth_congruence(rec, config)
      if (length(d) > 0) {
        div$rows[[length(div$rows) + 1L]] <- list(
          soundscape_path = path,
          error_class     = "config_congruence",
          error_message   = paste(d, collapse = "; "),
          timestamp       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
        )
      }
      rec
    }
    return(list(reader = reader, divergences = div))
  }
  list(
    reader = function(path) {
      .read_metadata_filename(path, spec = filename_metadata, sha256 = sha256)
    },
    divergences = NULL
  )
}

# NOTE: `.par_map` (the cross-platform parallel map) moved to `_parallel.R`
# (FSM-212) so it can be reused by `run_matching` later.
