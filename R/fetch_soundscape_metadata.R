#' Fetch Soundscape Metadata
#'
#' Extracts metadata from all WAV files found in a directory and its subdirectories.
#'
#' @param path A character string specifying the directory path where the WAV files are located.
#' @param ncores An integer specifying the number of CPU cores to use for parallel processing.
#'
#' @return A data frame with the following columns:
#' \describe{
#' \item{\code{soundscape_path}}{A character string specifying the full path to the WAV file.}
#' \item{\code{soundscape_file}}{A character string specifying the name of the WAV file.}
#' \item{\code{soundscape_duration}}{A numeric value specifying the duration of the WAV file in seconds.}
#' \item{\code{soundscape_sample_rate}}{An integer specifying the sample rate of the WAV file in Hz.}
#' \item{\code{soundscape_codec}}{A character string specifying the audio codec used in the WAV file.}
#' \item{\code{soundscape_layout}}{A character string specifying the audio channel layout of the WAV file# .}
#' }
#'
#' @export
#'
#' @examples
#' fetch_soundscape_metadata(path = "/path/to/soundscapes", ncores = 4)
#'

fetch_soundscape_metadata <- function(path, ncores = 1) {
  # The process begin by creating a dataframe with the complete paths to all
  # files as a variable in a dataframe
  soundscape_list <- list.files(
    path_soundscapes,
    pattern = ".wav", recursive = TRUE, ignore.case = TRUE,
    full.names = TRUE
  )

  handlers(handler_pbcol(
    adjust = 1.0,
    complete = function(s) cli::bg_cyan(cli::col_black(s)),
    incomplete = function(s) cli::bg_red(cli::col_black(s))
  ))

  # todo Implementar as outras estraégias de paralelização

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
