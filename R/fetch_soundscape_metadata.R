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
#' @import progressr furrr purrr
#' @export
fetch_soundscape_metadata <- function(path, recursive = TRUE, ncores = 1) {
  soundscape_list <- list.files(
    path,
    pattern = ".wav", recursive = recursive, ignore.case = TRUE,
    full.names = TRUE
  )

  handlers(
    handler_pbcol(
      adjust = 1.0,
      complete = function(s) cli::bg_cyan(cli::col_black(s)),
      incomplete = function(s) cli::bg_red(cli::col_black(s))
    )
  )

  get_metadata_safely <- safely(
    function(soundscape_list) {
      p <- progressor(along = 1:length(soundscape_list), auto_finish = FALSE)
      res <- future_map_dfr(
        soundscape_list,
        function(x) {
          res_raw <- as.data.frame(readWave(x, header = TRUE))
          res <- data.frame(
            soundscape_path = x,
            soundscape_file = basename(x),
            soundscape_duration = length(res_raw$samples) / res_raw$sample.rate,
            soundscape_sample_rate = res_raw$sample.rate,
            soundscape_bitrate = res_raw$bits,
            soundscape_layout = case_when(
              res_raw$channels == 1 ~ "mono",
              res_raw$channels == 2 ~ "stereo",
              TRUE ~ "other"
            )
          )
          p(message = "Extracting soundscape metadata")
          return(res)
        }
      )
    }
  )

  if (ncores > 1) {
    future::plan(multicore, workers = ncores)
  } else {
    future::plan(sequential)
  }

  with_progress({
    res <- get_metadata_safely(soundscape_list)$result
  })
  future::plan(sequential)

  message("Soundscape metadata successfully extracted")
  return(res)
}
