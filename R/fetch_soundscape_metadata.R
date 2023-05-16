#' Extract Soundscape Metadata
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Extracts metadata from all WAV files found in a directory and its
#' subdirectories.
#'
#' @param path A character string specifying the directory path where all WAV
#'   files are located.
#' @param recursive A logical value indicating whether the search for WAV files
#'   should be recursive or not. Default is TRUE.
#' @param ncores An integer indicating the number of cores to be used for
#'   parallelization. Default is 1.
#'
#' @return A data frame with the following columns:
#'
#' @export
fetch_soundscape_metadata <- function(path, recursive = TRUE, ncores = 1) {

  soundscape_list <- list.files(
    path_soundscapes,
    pattern = ".wav", recursive = recursive, ignore.case = TRUE,
    full.names = TRUE
  )

  handlers(handler_pbcol(
    adjust = 1.0,
    complete = function(s) cli::bg_cyan(cli::col_black(s)),
    incomplete = function(s) cli::bg_red(cli::col_black(s))
  ))

  get_metadata <- function(soundscape_list) {
    p <- progressor(along = 1:length(soundscape_list), auto_finish = FALSE)
    res <- future_map_dfr(
      soundscape_list,
      function(x) {
        res <- unlist(av_media_info(x), recursive = FALSE)
        p(message = "Extracting soundscape metadata")
        return(res)
      }
    )
  }

  if (ncores > 1) {
    plan(multicore, workers = ncores)
  } else {
    plan(sequential)
  }

  with_progress({
    res_raw <- get_metadata(soundscape_list)
  })
  plan(sequential)

  names(res_raw) <- gsub("audio\\.", "", names(res_raw))
  res_raw$soundscape_path <- soundscape_list
  res_raw$soundscape_file <- basename(soundscape_list)
  res_raw$soundscape_duration <- res_raw$duration
  res_raw$soundscape_sample_rate <- res_raw$sample_rate
  res_raw$soundscape_codec <- res_raw$codec
  res_raw$soundscape_layout <- res_raw$layout
  res <- res_raw[, c(
    "soundscape_path", "soundscape_file", "soundscape_duration",
    "soundscape_sample_rate", "soundscape_codec", "soundscape_layout"
  )]
  message("Soundscape metadata successfully extracted")
  return(res)

}
