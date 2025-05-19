#' Batch template matching
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function is wrapper of 'run_matching_i()' to perform a batch computation of
#'   the matching vectors of the template and soundscape matches defined in the
#'   metadata grid obtained as output of the function 'fetch_match_grid()'. The
#'   available algorithms to compare spectrograms and compute matching scores
#'   are the Pearson correlation coefficient ("cor") or dynamic time warping
#'   ("dtw").
#'
#' @param df_grid The output of the function 'fetch_match_grid()
#' @param score_method A character string indicating the method to use for
#'   matching. The two methods available are: "cor" (Pearson correlation
#'   coefficient) or "dtw" (dynamic time warping). It defaults to "cor".
#' @param ncores An integer indicating the number of cores to be used for
#'   parallelization. It defaults to 1.
#' @param output A character string indicating the output of the function. The
#'   two options are: "detections" or "scores". If "detections" is selected, the
#'   function will return the output of the function 'fetch_score_peaks()'. If
#'   "scores" is selected, the function will return the raw output of the
#'   matching algorithm. It defaults to "detections".
#' @param output_file A character string indicating the path to save the results
#'   in the format of a CSV file for detections or RDS file for scores. Default
#'   is NULL. We recommend to export detection or raw score files to the
#'   "detections/" subdirectory.
#' @param autosave_action A character string indicating the action to be taken
#'   if the output file already exists. Possible values are "append" and
#'   "replace". To avoid overwriting existing files, set to "append", but be
#'   aware that it can result in duplicated entries in the output file if the
#'   function is run again. It defaults to "replace".
#' @param buffer_size A character string indicating the size of the buffer to be
#'   used in the function 'fetch_score_peaks()'. The two options are:
#'   "template" or the number of frames of the template spectrogram to be used
#'   as buffer. It defaults to "template".
#' @param min_score A numeric value indicating the minimum score to be used in
#'   the function 'fetch_score_peaks()'. It defaults to NULL.
#' @param min_quant A numeric value indicating the minimum quantile to be used
#'   in the function 'fetch_score_peaks()'. It defaults to NULL.
#' @param top_n A numeric value indicating the number of top detections to be
#'   used in the function 'fetch_score_peaks()'. It defaults to NULL.
#'
#' @return If the format is set to scores, a tibble containing the input data
#'   frame with an additional column "score_vec". This column contains
#'   dataframes with "time_vec" (time of each frame) and "score_vec" (matching
#'   score at that frame) columns. The scores are padded at the start and end to
#'   match the soundscape spectrogram length. If the format is set to
#'   detections, the function returns a data frame with the detections. If no
#'   paths are provided, the output are returned to the R session.
#'
#' @import future

#' @export
#' @examples
#' \dontrun{
#' library(monitoraSom)
#' library(dplyr)
#' library(tuneR)
#'
#' library(monitoraSom)
#' library(dplyr)
#' library(tuneR)
#'
#' # Load the soundscape list to populate the example data
#' data(ls_soundscapes)
#'
#' # Create a directory to store the soundscapes
#' soundscapes_path <- "./soundscapes"
#' dir.create(soundscapes_path)
#' invisible(lapply(1:length(ls_soundscapes), function(i) {
#'   writeWave(
#'     ls_soundscapes[[i]], file.path(soundscapes_path, names(ls_soundscapes)[i])
#'   )
#' }))
#'
#' # Load the templates to populate the example data
#' data(ls_templates)
#'
#' # Create a directory to store the templates
#' templates_path <- "./templates"
#' dir.create(templates_path)
#' invisible(lapply(1:length(ls_templates), function(i) {
#'   writeWave(
#'     ls_templates[[i]], file.path(templates_path, names(ls_templates)[i])
#'   )
#' }))
#'
#' # Import the soundscapes and templates as dataframes
#' df_soundscapes <- fetch_soundscape_metadata(
#'   soundscapes_path = soundscapes_path
#' )
#'
#' # Import the templates as a dataframe
#' df_templates <- fetch_template_metadata(templates_path = templates_path)
#'
#' # Create a match grid
#' df_grid <- fetch_match_grid(
#'   soundscape_data = df_soundscapes, template_data = df_templates
#' )
#'
#' # Run the template matching to store raw scores of the first ten matches
#' df_scores <- run_matching(
#'   df_grid = df_grid[1:10, ], score_method = "cor", output = "scores"
#' )
#' # Look into the whole results
#' glimpse(df_scores)
#' # Look into the raw score vector of the first match
#' glimpse(df_scores$score_vec[1])
#'
#' # Run the template matching to get the raw detections
#' df_detecs <- run_matching(
#'   df_grid = df_grid[1:10, ], score_method = "cor", output = "detections"
#' )
#' # Look into the whole results
#' glimpse(df_detecs)
#' # make a histogram of the scores of the detections
#' hist(df_detecs$peak_score, breaks = 100)
#'
#' }
run_matching <- function(
  df_grid, score_method = "cor", ncores = 1, output = "detections",
  output_file = NULL, autosave_action = "replace", buffer_size = "template",
  min_score = NULL, min_quant = NULL, top_n = NULL
) {
  output <- match.arg(output, c("detections", "scores"))
  autosave_action <- match.arg(autosave_action, c("append", "replace"))

  if (ncores > 1 && Sys.info()["sysname"] == "Windows") {
    if (ncores <= parallel::detectCores()) {
      ncores <- parallel::makePSOCKcluster(getOption("cl.cores", ncores))
    } else {
      stop(
        paste(
          "The number of cores requested cannot be higher than the number of ",
          "available cores"
        )
      )
    }
  } else {
    ncores <- 1
  }

  if (output == "detections") {
    if (is.null(output_file)) {
      grid_list <- dplyr::group_split(
        dplyr::rowwise(df_grid)
      )
      res <- pbapply::pblapply(
        grid_list,
        function(x) {
          run_matching_i(
            df_grid_i = x, score_method = score_method, output = "detections",
            buffer_size = buffer_size, min_score = min_score,
            min_quant = min_quant, top_n = top_n
          )
        },
        cl = ncores
      )
      res <- purrr::list_rbind(res)
      message(
        paste(
          "Template matching finished. Detections have been returned to the",
          "R session"
        )
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
          "soundscape_path",
          "soundscape_file",
          "template_path",
          "template_file",
          "template_name",
          "template_min_freq",
          "template_max_freq",
          "template_start",
          "template_end",
          "detection_start",
          "detection_end",
          "detection_wl",
          "detection_ovlp",
          "detection_sample_rate",
          "detection_buffer",
          "detection_min_score",
          "detection_min_quant",
          "detection_top_n",
          "peak_index",
          "peak_score",
          "peak_quant"
        )
        writeLines(paste(csv_headers, collapse = ","), output_file)
      }
      if (file.exists(output_file) && autosave_action == "append") {
        df_check <- data.table::fread(output_file)
        if (nrow(df_check) > 0) {
          df_check <- df_check |>
            dplyr::transmute(
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
          df_grid_check <- df_grid |>
            dplyr::mutate(
              buffer_size = buffer_size,
              min_score = min_score,
              min_quant = min_quant,
              top_n = top_n
            ) |>
            dplyr::select(
              soundscape_file, template_file, template_wl, template_ovlp,
              template_sample_rate, buffer_size, min_score, min_quant, top_n
            )
          idx <- which(
            !df_grid_check$soundscape_file %in% df_check$soundscape_file
          )
          df_grid <- df_grid[idx, ]
          if (nrow(df_grid) == 0) {
            message(
              paste(
                "All matches have already been processed. No new detections",
                "were computed."
              )
            )
            return(NULL)
          }
        }
      }
      grid_list <- dplyr::group_split(
        dplyr::rowwise(df_grid)
      )
      pbapply::pblapply(
        grid_list,
        function(x) {
          res <- run_matching_i(
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
    grid_list <- dplyr::group_split(
      dplyr::rowwise(df_grid)
    )
    res <- pbapply::pblapply(
      grid_list,
      function(x) {
        run_matching_i(
          df_grid_i = x, score_method = score_method, output = "scores",
          buffer_size = buffer_size, min_score = min_score,
          min_quant = min_quant, top_n = top_n
        )
      },
      cl = ncores
    ) |>
      purrr::list_rbind()
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
        paste(
          "Template matching finished. Raw scores have been returned to the",
          "R session"
        )
      )
      return(res)
    }
  }
}
