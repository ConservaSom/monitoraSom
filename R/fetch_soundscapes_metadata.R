#' Extract Soundscape Metadata
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Extracts metadata from all WAV files found in a directory and its
#'   subdirectories.
#'
#' @param soundscapes_path A character string specifying the directory path
#'   where all WAV files are located. If no path is provided, the function will
#'   look for the soundscapes in the "soundscapes/" directory, or create it if
#'   it does not exist.
#' @param recursive A logical value indicating whether the search for WAV files
#'   should be recursive or not. Defaults to TRUE.
#' @param output_file A character string specifying the path to the output file.
#'   Defaults to NULL. If there is a CSV table with metadata, a check of missing
#'   recordings will be performed. If there are missing recordings, a log file
#'   with a timestamp will be created with the names of the missing recordings.
#'   We recommend importing the metadata from the CSV file directly to the R
#'   session when no changes are expected in the metadata.
#' @param skip_processed A logical value indicating whether to check the
#'   recordings present in the provided CSV file were already processed, and
#'   avoid processing them again. Defaults to TRUE.
#' @param ncores An integer indicating the number of cores to be used for
#'   parallelization. Defaults to 1.
#'
#' @return A data frame in the R session with the soundscapes metadata. If an
#'   output path is provided, the function will save the metadata in a CSV file
#'   in the provided path.
#' @import dplyr
#' @importFrom pbapply pblapply
#' @importFrom parallel makePSOCKcluster detectCores
#' @importFrom tuneR readWave
#' @export
fetch_soundscapes_metadata <- function(
    soundscapes_path = NULL, recursive = TRUE, output_file = NULL,
    skip_processed = TRUE, ncores = 1) {

  soundscapes_path <- if (is.null(soundscapes_path)) {
    "soundscapes/"
  } else if (!dir.exists(soundscapes_path)) {
    stop("The provided path to the soundscapes does not exist")
  } else {
    soundscapes_path
  }

  ls_soundscapes_raw <- list.files(
    path = soundscapes_path,
    pattern = ".wav$", recursive = recursive, ignore.case = TRUE,
    full.names = TRUE
  )

  if (length(ls_soundscapes_raw) == 0) {
    stop("There are no WAV files in the provided path")
  }

  if (!is.null(output_file)) {
    if (file.exists(output_file)) {
      df_imported <- read.csv(output_file)
      if (skip_processed) {
        ls_to_process <- ls_soundscapes_raw[
          !ls_soundscapes_raw %in% df_imported$soundscape_path
        ]
      }
    } else {
      ls_to_process <- ls_soundscapes_raw
    }
  } else {
    ls_to_process <- ls_soundscapes_raw
  }

  if (ncores > 1 & Sys.info()["sysname"] == "Windows") {
    if (ncores <= parallel::detectCores()) {
      ncores <- parallel::makePSOCKcluster(getOption("cl.cores", ncores))
    } else {
      stop(
        paste(
          "The number of cores requested cannot be higher than the",
          "number of available cores"
        )
      )
    }
  } else {
    ncores <- 1
  }

  read_wav_fun <- function(x) {
    res <- tryCatch(
      {
        data <- tuneR::readWave(x, header = TRUE)
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

  if (length(ls_to_process) > 0) {
    res <- pbapply::pblapply(ls_to_process, read_wav_fun, cl = ncores) %>%
      dplyr::bind_rows() %>%
      dplyr::transmute(
        soundscape_path = path,
        soundscape_file = basename(path),
        soundscape_duration = samples / sample.rate,
        soundscape_sample_rate = sample.rate,
        soundscape_bitrate = bits,
        soundscape_layout = dplyr::case_when(
          channels == 1 ~ "mono",
          channels == 2 ~ "stereo",
          TRUE ~ "other"
        )
      )
  } else {
    res <- df_imported
  }

  if (exists("df_imported")) {
    missing_soundscapes <- df_imported$soundscape_path[
      !df_imported$soundscape_path %in% ls_soundscapes_raw
    ]
    if (length(missing_soundscapes) > 0 & !is.null(output_file)) {
      log_file <- gsub(
        ".csv$",
        paste0(
          "_missing_recordings_log_",
          format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"
        ),
        output_file
      )
      data.frame(missing_files = missing_soundscapes) %>%
        write.csv(log_file, row.names = FALSE)
      message(
        paste(
          "A list of the soundscapes in the metadata, but missing files",
          "was saved in", log_file
        )
      )
    }
    res <- res %>%
      dplyr::filter(!soundscape_path %in% missing_soundscapes)
    if (skip_processed & length(ls_to_process) > 0) {
      res <- rbind(df_imported, res) %>%
        dplyr::arrange(soundscape_path)
    }
  }

  if (!is.null(output_file)) {
    write.csv(res, output_file, row.names = FALSE)
  }

  # give a warning if there are soundscapes with duration less than 1 second
  if (any(res$soundscape_duration < 1)) {
    message(
      paste(
        "Warning: There are", sum(res$soundscape_duration < 1),
        " recordings with duration smaller than 1 second.",
        "Please, check for possible errors."
      )
    )
  }

  return(res)
}
