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
#' @param template_type The type of templates, either "standalone" or "library".
#' @param score_method The method used for template matching, either "cor" or
#'   "dtw".
#' @param buffer_size The size of the buffer used to calculate the scores for
#'   the template matches, either "template" or "event".
#' @param recursive_soundscape Search for soundscape recordings recursively in
#'   'path_soundscapes'
#' @param recursive_template Search for template files (ROI tables or standalone
#'   templates) in 'path_templates'
#' @param save_res Default to FALSE or provide a path to an rds file where the
#'   raw template matching results will be stored.
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
#' @param par_strat A character string indicating the parallelization strategy
#'   to be used. The available options are: "foreach" (default), "future" and
#'   "pbapply". The 'future' and 'pbapply' strategies do not work on Windows,
#'   but are more efficient in linux (especially when running and R session
#'   outside of Rstudio). The 'foreach' strategy works on all platforms, but is
#'   less efficient than the other two. See the documentation of the 'future'
#'   and 'pbapply' packages for more details.
#' @param ncores An integer indicating the number of cores to be used for
#'   parallelization. Default is 1.
#' @param backend_type For usage when par_strat = "parabar"
#' @param cluster_type For usage when par_strat = "parabar"
#'
#' @return A dataframe with the detected events.
#'
#' @export
template_matching <- function(
    path_soundscapes, recursive_soundscapes = TRUE,
    path_templates, recursive_templates = TRUE, template_type = "standalone",
    score_method = "cor", save_res = FALSE,
    buffer_size = "template", min_score = NULL, min_quant = NULL, top_n = NULL,
    ncores = 1, par_strat = "future", backend_type = "async", cluster_type = "psock"
) {
  df_templates <- fetch_template_metadata(
    path = path_templates, method = "standalone",
    recursive = recursive_templates
  )
  df_soundscapes <- fetch_soundscape_metadata(
    path = path_soundscapes, recursive = recursive_soundscapes, ncores = 6
  )
  df_grid <- fetch_match_grid(
    template_data = df_templates, soundscape_data = df_soundscapes
  )
  tib_match <- match_n(
    df_grid = df_grid, score_method = score_method,
    save_res = save_res, par_strat = par_strat, ncores = ncores,
    backend_type = backend_type, cluster_type = cluster_type
  )
  df_detections <- fetch_score_peaks_n(
    tib_match = tib_match, recursive = FALSE, buffer_size = buffer_size,
    min_score = min_score, min_quant = min_quant, top_n = top_n
  )
  message("Done")
  return(df_detections)
}
