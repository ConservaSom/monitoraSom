
#' Creates a grid with all combinations of templates and soundscapes for template matching
#'
#' This function takes in two data frames: soundscape_data (the output of the function 'fetch_soundscape_metadata()') and template_data (the output of the function 'fetch_template_metadata()'), checks their compatibilities, and returns a new data frame with a grid of all possible matching combinations between the two data sets.
#'
#' @param soundscape_data A data frame containing metadata about the soundscapes to be matched.
#' @param template_data A data frame containing metadata about the templates to be matched.
#'
#' @return A data frame with a grid of all possible matching combinations between the two input data sets. In other words, each row of the output data frame represents the match between a template and a soundscape. All columns with metadata from soundscapes and templates are kept (see the documentation of the functions 'fetch_soundscape_metadata()' and 'fetch_template_metadata()' for more details).
#' @export
#'
#' @examples
#' fetch_match_grid(soundscape_data, template_data)
fetch_match_grid <- function(soundscape_data, template_data) {
  res <- cross_join(soundscape_data, template_data)

  # check if files in soundscape_path exist
  check_1 <- map_vec(res$soundscape_path, ~ file.exists(.x))
  if (any(check_1 == FALSE)) {
    n <- sum(check_1 == FALSE)
    stop(paste0("There are ", n, " files from soundscape_path that do not exist"))
  }

  # check if files in template_path exist
  check_2 <- map_vec(res$template_path, ~ file.exists(.x))
  if (any(check_2 == FALSE)) {
    n <- sum(check_2 == FALSE)
    stop(paste0("There are ", n, " files from template_path that do not exist"))
  }

  # check if sample rates are compatible
  check_3 <- res$soundscape_sample_rate == res$template_sample_rate
  if (any(check_3 == FALSE)) {
    n <- sum(check_3 == FALSE)
    stop(paste0("There are ", n, " files with incompatible sample rates"))
  }

  # check if template min_freq max_freq are compatible with the shared sample rates
  check_4 <- res$template_min_freq * 1000 < res$soundscape_sample_rate / 2
  if (any(check_4 == FALSE)) {
    n <- sum(check_4 == FALSE)
    stop(paste0("There are ", n, " templates with minimum frequencies higher than the Nyquist frequency (sample_rate / 2)"))
  }

  # check if template max_freq are compatible with the shared sample rates
  check5 <- res$template_max_freq * 1000 < res$soundscape_sample_rate / 2
  if (any(check5 == FALSE)) {
    n <- sum(check5 == FALSE)
    stop(paste0("There are ", n, " templates with maximum frequencies higher than the Nyquist frequency (sample_rate / 2)"))
  }

  # check if template duration is smaller than soundscape duration
  check_6 <- res$template_end - res$template_start < res$soundscape_duration
  if (any(check_6 == FALSE)) {
    n <- sum(check_6 == FALSE)
    stop(paste0("There are ", n, " templates with duration higher than the soundscape duration"))
  }
  n <- nrow(res)
  message("All files locally available, are compatible and resulted in a grid of ", n, " matchings")

  return(res)
}


