
#' Title
#'
#' @param path_soundscapes
#' @param path_templates
#' @param template_type
#' @param score_method
#' @param buffer_size
#' @param ncores
#'
#' @return
#' @export
#'
#' @examples
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
