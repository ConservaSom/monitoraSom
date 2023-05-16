#' Perform template matching on a set of soundscapes using a set of templates
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function performs template matching on a set of soundscapes using a set
#' of templates, and returns a dataframe with the detected events.
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
#' @param ncores The number of cores to be used for parallel processing.
#'
#' @return A dataframe with the detected events.
#'
#' @export
template_matching <- function(
    path_soundscapes, path_templates, template_type = "standalone",
    score_method = "cor", buffer_size = "template", ncores = 1
) {

  df_templates <- fetch_template_metadata(
    path = path_templates, method = "standalone"
  )
  df_soundscapes <- fetch_soundscape_metadata(
    path = path_soundscapes, ncores = 6
  )
  df_grid <- fetch_match_grid(
    template_data = df_templates, soundscape_data = df_soundscapes
  )
  tib_match <- match_n(
    df_grid = df_grid, score_method = score_method, ncores = ncores
  )
  df_detections <- fetch_score_peaks_n(
    tib_match = tib_match, buffer_size = buffer_size
  )
  message("Done")
  return(df_detections)
}
