#' Perform template matching on a set of soundscapes using a set of templates
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function performs template matching on a set of soundscapes using a
#'   set of templates, and returns a dataframe with the detected events.
#'
#' @param path_soundscapes Path to a directory containing the soundscapes to be
#'   analyzed.
#' @param path_templates Path to a directory containing the templates to be used
#'   for matching.
#' @param score_method The method used for template matching, either "cor" or
#'   "dtw".
#' @param buffer_size The size of the buffer used to calculate the scores for
#'   the template matches, either "template" or "event".
#' @param min_score A numeric value between 0 and 0.99 indicating the minimum
#'   score of the detections that will be kept. Defaults to NULL, which returns
#'   all available detections.
#' @param min_quant A numeric value between 0 and 1 indicating the minimum score
#'   quantile of the kept detections. Defaults to NULL, which returns all
#'   available detections.
#' @param top_n An integer indicating the maximum number of peaks to be
#'   returned, selected according to the highest scores available. Defaults to
#'   NULL, which return all available detections. It should be noted that
#'   because the peak quantiles are callculated within each score vector, the
#'   top_n parameter is applied to each score vector separately, and not to the
#'   whole matching grid.
#' @param ncores An integer indicating the number of cores to be used for
#'   parallelization. Default is 1.
#' @param recursive_soundscapes Todo
#' @param recursive_templates Todo
#' @param output_file Path to the file where the results will be saved.
#' @param autosave_action Action to be taken when the user navigates to another
#'   file. Possible values are "append" (default) and "overwrite".
#'
#' @return A dataframe with the detected events.
#'
#' @export
template_matching <- function(
    path_soundscapes, recursive_soundscapes = TRUE,
    path_templates, recursive_templates = TRUE, score_method = "cor",
    output_file = NULL, autosave_action = "append", buffer_size = "template",
    min_score = NULL, min_quant = NULL, top_n = NULL, ncores = 1) {

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
