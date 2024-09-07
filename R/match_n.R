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
#'  two options are: "detections" (default) or "scores". If "detections" is
#'  selected, the function will return the output of the function
#'  'fetch_score_peaks_i()'. If "scores" is selected, the function will return
#'  the raw output of the matching algorithm.
#' @param output_file A character string indicating the path to save the results
#'  in the format of a CSV file. Default is NULL.
#' @param autosave_action A character string indicating the action to be taken if
#'  the output file already exists. The available options are: "append" (default)
#'  and "replace".
#' @param buffer_size A character string indicating the size of the buffer to be
#'  used in the function 'fetch_score_peaks_i()'. The two options are: "template"
#'  (default) or the number of frames of the template spectrogram to be used as
#'  buffer.
#' @param min_score A numeric value indicating the minimum score to be used in
#'  the function 'fetch_score_peaks_i()'.
#' @param min_quant A numeric value indicating the minimum quantile to be used in
#'  the function 'fetch_score_peaks_i()'.
#' @param top_n A numeric value indicating the number of top detections to be
#'  used in the function 'fetch_score_peaks_i()'.
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
    df_grid,
    score_method = "cor", ncores = 1, output = "detections", output_file = NULL,
    autosave_action = "append", buffer_size = "template", min_score = NULL,
    min_quant = NULL, top_n = NULL) {

  grid_list <- group_split(rowwise(df_grid))

  validate_output_path <- function(output_file, extension) {
    if (
      dir.exists(dirname(output_file)) &&
        grepl(paste0("\\.", extension, "$"), output_file)) {
      return(TRUE)
    } else {
      stop(
        paste("The path for the", toupper(extension), "output file is not valid")
      )
    }
  }

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

  if (output == "detections") {
    if (is.null(output_file)) {
      # run the no sink version returning the result object
      res <- pbapply::pblapply(
        grid_list,
        function(x) {
          match_i(
            df_grid_i = x,
            score_method = score_method, output = "detections",
            buffer_size = buffer_size, min_score = min_score,
            min_quant = min_quant, top_n = top_n
          )
        },
        cl = ncores
      ) %>%
        list_rbind(.)
      message("Template matching finished. Detections have been returned to the R session")
      return(res)
    } else {
      if (validate_output_path(output_file, "csv")) {
        if (!(autosave_action %in% c("append", "replace"))) {
          stop("Invalid autosave action. Choose 'append' or 'replace'")
        }
        if (file.exists(output_file) & autosave_action == "replace") {
          file.remove(output_file)
        }
        if (file.exists(output_file) & autosave_action == "replace" |
          !file.exists(output_file)
        ) {
          writeLines(
            paste(
              "\"soundscape_path\"", "\"soundscape_file\"", "\"template_path\"",
              "\"template_file\"", "\"template_name\"", "\"template_min_freq\"",
              "\"template_max_freq\"", "\"template_start\"", "\"template_end\"",
              "\"detection_start\"", "\"detection_end\"", "\"detection_wl\"",
              "\"detection_ovlp\"", "\"detection_sample_rate\"",
              "\"detection_buffer\"", "\"detec_min_score\"",
              "\"detec_min_quant\"", "\"detec_top_n\"", "\"peak_index\"",
              "\"peak_score\"", "\"peak_quant\"",
              sep = ","
            ),
            output_file
          )
        }
        match_i_sink <- function(
          df_grid_i, score_method, output, output_file, buffer_size, min_score,
          min_quant, top_n
          ) {
          res <- match_i(
            df_grid_i = df_grid_i, score_method = score_method,
            output = "detections", buffer_size = buffer_size,
            min_score = min_score, min_quant = min_quant, top_n = top_n
          )
          sink(output_file, append = TRUE)
          write.table(res, sep = ",", row.names = F, col.names = F)
          sink()
        }
        dump <- pbapply::pblapply(
          grid_list,
          function(x) {
            match_i_sink(
              df_grid_i = x, score_method = score_method,
              output = "detections", output_file = output_file,
              buffer_size = buffer_size, min_score = min_score,
              min_quant = min_quant, top_n = top_n
            )
          },
          cl = ncores
        )
        message(
          "Template matching finished. Detections have been saved to ",
          output_file
        )
      }
    }
  } else if (output == "scores") {
    res <- pbapply::pblapply(
      grid_list,
      function(x) {
        match_i(
          df_grid_i = x,
          score_method = score_method, output = "scores",
          buffer_size = buffer_size, min_score = min_score,
          min_quant = min_quant, top_n = top_n
        )
      },
      cl = ncores
    ) %>%
      list_rbind(.)
    if (!is.null(output_file)) {
      if (validate_output_path(output_file, "rds")) {
        saveRDS(res, output_file)
        message(
          "Template matching finished. Raw scores have been saved to ",
          output_file
        )
      }
    } else {
      return(res)
      message("Template matching finished. Raw scores have been returned to the R session")
    }
  } else {
    stop("Invalid output type")
  }
}