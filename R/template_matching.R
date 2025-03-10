#' Perform template matching on a set of soundscapes using a set of templates
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function performs template matching on a set of soundscapes using a
#'   set of templates, and returns a dataframe with the detected events.
#'
#' @param path_soundscapes Path to a directory containing the soundscapes to be
#'   analyzed. It defaults to "soundscapes/".
#' @param path_templates Path to a directory containing the templates to be used
#'   for matching. It defaults to "roi_cuts/".
#' @param score_method The method used for template matching, either "cor" or
#'   "dtw". It defaults to "cor".
#' @param buffer_size The size of the buffer used to calculate the scores for
#'   the template matches, either "template" or "event". It defaults to "template".
#' @param min_score A numeric value between 0 and 0.99 indicating the minimum
#'   score of the detections that will be kept. It defaults to NULL, which returns
#'   all available detections.
#' @param min_quant A numeric value between 0 and 1 indicating the minimum score
#'   quantile of the kept detections. It defaults to NULL, which returns all
#'   available detections.
#' @param top_n An integer indicating the maximum number of peaks to be
#'   returned, selected according to the highest scores available. It defaults to
#'   NULL, which return all available detections. It should be noted that
#'   because the peak quantiles are callculated within each score vector, the
#'   top_n parameter is applied to each score vector separately, and not to the
#'   whole matching grid.
#' @param ncores An integer indicating the number of cores to be used for
#'   parallelization. It defaults to 1.
#' @param recursive_soundscapes A logical value indicating whether to search
#'   soundscapes recursively. It defaults to FALSE.
#' @param recursive_templates A logical value indicating whether to search
#'   templates recursively. It defaults to FALSE.
#' @param output_file Path to the file where the results will be saved. It
#'   defaults to NULL. We recommend to export detection or raw score files to the
#'   "detections/" subdirectory.
#' @param autosave_action A character string indicating the action to be taken
#'   if the output file already exists. Possible values are "append" and
#'   "overwrite". To avoid overwriting existing files, set to "append", but be
#'   aware that it can result in duplicated entries in the output file if the
#'   function is run again. It defaults to "append".
#'
#' @return A dataframe with the detected events.
#'
#' @export
template_matching <- function(
    path_soundscapes = "soundscapes/", recursive_soundscapes = FALSE,
    path_templates = "roi_cuts/", recursive_templates = FALSE,
    score_method = "cor", output_file = NULL, autosave_action = "append",
    buffer_size = "template", min_score = NULL, min_quant = NULL, top_n = NULL,
    ncores = 1
    ) {

  df_templates <- fetch_template_metadata(
    path = path_templates, recursive = recursive_templates
  )
  df_soundscapes <- fetch_soundscape_metadata(
    path = path_soundscapes, recursive = recursive_soundscapes, ncores = 1
  )
  df_grid <- fetch_match_grid(
    template_data = df_templates, soundscape_data = df_soundscapes
  )
  df_detections <- match_n(
    df_grid = df_grid, score_method = score_method, output = "detections",
    output_file = output_file, autosave_action = autosave_action,
    ncores = ncores, buffer_size = buffer_size, min_score = min_score,
    min_quant = min_quant, top_n = top_n
  )
  message("Template matching finished")
  if (!is.null(output_file)) {
    message("Detections have been saved to ", output_file)
  } else {
    return(df_detections)
  }
}
