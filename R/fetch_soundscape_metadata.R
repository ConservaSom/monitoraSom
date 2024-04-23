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
#' @import progressr furrr purrr tuneR
#' @export
fetch_soundscape_metadata <- function(path, recursive = TRUE, ncores = 1) {

  soundscape_list <- list.files(
    path, pattern = ".wav$", recursive = recursive, ignore.case = TRUE,
    full.names = TRUE
  )

  get_metadata_safely <- safely(
    function(x) {
      res <- as.data.frame(readWave(x, header = TRUE))
      res$path <- x
      return(res)
    }
  )

  if (ncores > 1) {
    future::plan(multicore, workers = ncores)
  } else {
    future::plan(sequential)
  }

  res <- future_map_dfr(
    soundscape_list, ~ get_metadata_safely(.x)$result,
    .progress = TRUE
  ) %>%
    transmute(
      soundscape_path = path,
      soundscape_file = basename(path),
      soundscape_duration = samples / sample.rate,
      soundscape_sample_rate = sample.rate,
      soundscape_bitrate = bits,
      soundscape_layout = case_when(
        channels == 1 ~ "mono",
        channels == 2 ~ "stereo",
        TRUE ~ "other"
      )
    )

  future::plan(sequential)

  message("Soundscape metadata successfully extracted")
  return(res)
}
