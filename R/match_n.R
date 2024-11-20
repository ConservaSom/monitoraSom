#' Batch template matching
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function is wrapper of 'match_i()' to perform a batch computation of
#'   the matching vectors of the template and soundscape matches defined in the
#'   metadata grid obtained as output of the function 'fetch_match_grid()'. The
#'   available algorithms to compare spectrograms and compute matching scores
#'   are the Pearson correlation coefficient ("cor") or dynamic time warping
#'   ("dtw").
#'
#' @param df_grid The output of the function 'fetch_match_grid()
#' @param score_method A character string indicating the method to use for
#'   matching. The two methods available are: "cor" (Pearson correlation
#'   coefficient) or "dtw" (dynamic time warping). Defaults to "cor".
#' @param ncores An integer indicating the number of cores to be used for
#'   parallelization. Default is 1.
#' @param output A character string indicating the output of the function. The
#'   two options are: "detections" (default) or "scores". If "detections" is
#'   selected, the function will return the output of the function
#'   'fetch_score_peaks_i()'. If "scores" is selected, the function will return
#'   the raw output of the matching algorithm.
#' @param output_file A character string indicating the path to save the results
#'   in the format of a CSV file. Default is NULL.
#' @param autosave_action A character string indicating the action to be taken
#'   if the output file already exists. The available options are: "append"
#'   (default) and "replace".
#' @param buffer_size A character string indicating the size of the buffer to be
#'   used in the function 'fetch_score_peaks_i()'. The two options are:
#'   "template" (default) or the number of frames of the template spectrogram to
#'   be used as buffer.
#' @param min_score A numeric value indicating the minimum score to be used in
#'   the function 'fetch_score_peaks_i()'.
#' @param min_quant A numeric value indicating the minimum quantile to be used
#'   in the function 'fetch_score_peaks_i()'.
#' @param top_n A numeric value indicating the number of top detections to be
#'   used in the function 'fetch_score_peaks_i()'.
#'
#' @return A tibble containing input data frame with an additional column
#'   "score_vec", which is a list of dataframes with the columns "time_vec" (the
#'   time value of each spectrogram frame) and "score_vec" (the matching score
#'   obtained when the template and the soundscape spectrogram of samew
#'   dimensions are alligned at that frame) for each match. The length of the
#'   "score_vec" is equal to the number of frames of the soundscape spectrogram
#'   minus the number of frames of the template spectrogram (i.e. the number of
#'   possible allignments between the two spectrograms. The score is not
#'   available for the first and last frames of the soundscape spectrogram
#'   because score cannot be calculated between spectrograms of different
#'   dimensions. To produce a score vector with the same number of frames of the
#'   soundscape spectrogram, pads with length quals half the number of frames
#'   from the template are added to the beginning and end of the
#'
#' @import dplyr future pbapply furrr purrr
#' @importFrom furrr future_map
#' @importFrom purrr list_rbind
#' @export
match_n <- function(
    df_grid, score_method = "cor", ncores = 1, output = "detections",
    output_file = NULL, autosave_action = "append", buffer_size = "template",
    min_score = NULL, min_quant = NULL, top_n = NULL) {
  output <- match.arg(output, c("detections", "scores"))
  autosave_action <- match.arg(autosave_action, c("append", "replace"))

  if (ncores > 1 && Sys.info()["sysname"] == "Windows") {
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

  if (output == "detections") {
    if (is.null(output_file)) {
      grid_list <- group_split(rowwise(df_grid))
      res <- pbapply::pblapply(
        grid_list,
        function(x) {
          match_i(
            df_grid_i = x, score_method = score_method, output = "detections",
            buffer_size = buffer_size, min_score = min_score,
            min_quant = min_quant, top_n = top_n
          )
        },
        cl = ncores
      ) %>% list_rbind()
      message(
        "Template matching finished. Detections have been returned to the R session"
      )
      return(res)
    } else {
      if (!dir.exists(dirname(output_file)) || !grepl("\\.csv$", output_file)) {
        stop("The path for the CSV output file is not valid")
      }
      if (file.exists(output_file) && autosave_action == "replace") {
        file.remove(output_file)
      }
      if (!file.exists(output_file) || autosave_action == "replace") {
        csv_headers <- c(
          "soundscape_path", "soundscape_file", "template_path",
          "template_file", "template_name", "template_min_freq",
          "template_max_freq", "template_start", "template_end",
          "detection_start", "detection_end", "detection_wl",
          "detection_ovlp", "detection_sample_rate", "detection_buffer",
          "detection_min_score", "detection_min_quant", "detection_top_n",
          "peak_index", "peak_score", "peak_quant"
        )
        writeLines(paste(csv_headers, collapse = ","), output_file)
      }
      if (file.exists(output_file) && autosave_action == "append") {
        df_check <- data.table::fread(output_file)
        if (nrow(df_check) > 0) {
          df_check <- df_check %>%
            transmute(
              soundscape_file = soundscape_file,
              template_file = template_file,
              template_wl = detection_wl,
              template_ovlp = detection_ovlp,
              template_sample_rate = detection_sample_rate,
              buffer_size = detection_buffer,
              min_score = detection_min_score,
              min_quant = detection_min_quant,
              top_n = detection_top_n
            )
          df_grid_check <- df_grid %>%
            mutate(
              buffer_size = buffer_size, min_score = min_score,
              min_quant = min_quant, top_n = top_n
            ) %>%
            select(
              soundscape_file, template_file, template_wl, template_ovlp,
              template_sample_rate, buffer_size, min_score, min_quant, top_n
            )
          idx <- which(!df_grid_check$soundscape_file %in% df_check$soundscape_file)
          df_grid <- df_grid[idx, ]
          if (nrow(df_grid) == 0) {
            message(
              "All matches have already been processed. No new detections were computed."
            )
            return(NULL)
          }
        }
      }
      grid_list <- group_split(rowwise(df_grid))
      pbapply::pblapply(
        grid_list,
        function(x) {
          res <- match_i(
            df_grid_i = x, score_method = score_method, output = "detections",
            buffer_size = buffer_size, min_score = min_score,
            min_quant = min_quant, top_n = top_n
          )
          sink(output_file, append = TRUE)
          write.table(res, sep = ",", row.names = FALSE, col.names = FALSE)
          sink()
        },
        cl = ncores
      )
      message(
        "Template matching finished. Detections have been saved to ",
        output_file
      )
    }
  } else {
    grid_list <- group_split(rowwise(df_grid))
    res <- pbapply::pblapply(
      grid_list,
      function(x) {
        match_i(
          df_grid_i = x, score_method = score_method, output = "scores",
          buffer_size = buffer_size, min_score = min_score,
          min_quant = min_quant, top_n = top_n
        )
      },
      cl = ncores
    ) %>% list_rbind()
    if (!is.null(output_file)) {
      if (!dir.exists(dirname(output_file)) || !grepl("\\.rds$", output_file)) {
        stop("The path for the RDS output file is not valid")
      }
      saveRDS(res, output_file)
      message(
        "Template matching finished. Raw scores have been saved to ",
        output_file
      )
    } else {
      message(
        "Template matching finished. Raw scores have been returned to the R session"
      )
      return(res)
    }
  }
}
