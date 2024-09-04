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
fetch_soundscape_metadata_v3 <- function(path, recursive = TRUE, ncores = 1) {
  require(pbapply)

  soundscape_list <- list.files(
    path,
    pattern = ".wav$", recursive = recursive, ignore.case = TRUE,
    full.names = TRUE
  )

  if (ncores > 1 & Sys.info()["sysname"] == "Windows") {
    if (ncores <= parallel::detectCores()) {
      ncores <- parallel::makePSOCKcluster(getOption("cl.cores", ncores))
    } else {
      stop(
        "The number of cores requested cannot be higher than the number of available cores"
      )
    }
  } else {
    ncores <- 1
  }

  read_wav_fun <- function(x) {
    res <- tryCatch(
      {
        data <- readWave(x, header = TRUE)
        data$path <- x
        data
      },
      error = function(e) {
        message(paste("Error reading file:", x))
        NULL
      }
    )
    return(res)
  }

  res <- pblapply(soundscape_list, read_wav_fun, cl = ncores) %>%
    bind_rows() %>%
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

  # give a warning if there are soundscapes with duration less than 1 second
  if (any(res$soundscape_duration < 1)) {
    message(
      paste(
        "Warning: There are", sum(res$soundscape_duration < 1),
        "soundscapes with duration less than 1 second. ",
        "Please check the soundscapes for possible errors."
      )
    )
  }

  message("Soundscape metadata successfully extracted")
  return(res)
}
