#' Creates a grid with all combinations of templates and soundscapes for
#' template matching
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function takes in two data frames: soundscape_data (the output of the
#'   function 'fetch_soundscape_metadata()') and template_data (the output of
#'   the function 'fetch_template_metadata()'), checks their compatibilities,
#'   and returns a new data frame with a grid of all possible matching
#'   combinations between the two data sets.
#'
#' @param soundscape_data A data frame containing metadata about the soundscapes
#'   to be matched.
#' @param template_data A data frame containing metadata about the templates to
#'   be matched.
#'
#' @return A data frame with a grid of all possible matching combinations
#'   between the two input data sets. In other words, each row of the output
#'   data frame represents the match between a template and a soundscape. All
#'   columns with metadata from soundscapes and templates are kept (see the
#'   documentation of the functions 'fetch_soundscape_metadata()' and
#'   'fetch_template_metadata()' for more details).
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # Load the necessary packages to run this example
#' library(monitoraSom)
#' library(dplyr)
#' library(tuneR)
#'
#' # Load the soundscape list and the template list to populate the example data
#' data(ls_soundscapes)
#' data(ls_templates)
#'
#' # Create a directory and export the soundscapes
#' soundscapes_path <- "./soundscapes"
#' dir.create(soundscapes_path)
#' invisible(lapply(1:length(ls_soundscapes), function(i) {
#'   writeWave(
#'     ls_soundscapes[[i]], file.path(soundscapes_path, names(ls_soundscapes)[i])
#'   )
#' }))
#'
#' # Create a directory and export the templates
#' templates_path <- "./templates"
#' dir.create(templates_path)
#' invisible(lapply(1:length(ls_templates), function(i) {
#'   writeWave(
#'     ls_templates[[i]], file.path(templates_path, names(ls_templates)[i])
#'   )
#' }))
#'
#' # Import the soundscapes metadata
#' df_soundscapes <- fetch_soundscapes_metadata(
#'   soundscapes_path = soundscapes_path
#' )
#'
#' # Import the templates metadata
#' df_templates <- fetch_template_metadata(templates_path = templates_path)
#'
#' # Create a match grid
#' df_grid <- fetch_match_grid(
#'   soundscape_data = df_soundscapes, template_data = df_templates
#' )
#' glimpse(df_grid)
#'
#' # Check if there is one match per soundscape-template pair in the grid. The test
#' # should return TRUE
#' nrow(df_grid) == length(ls_soundscapes) * length(ls_templates)
#'
#' }
fetch_match_grid <- function(soundscape_data, template_data) {

  res <- dplyr::cross_join(soundscape_data, template_data)
  warnings <- character()
  missing_soundscapes <- !file.exists(res$soundscape_path)

  if (any(missing_soundscapes)) {
    n <- sum(missing_soundscapes)
    warnings <- c(warnings, paste0(n, " files from soundscape_path do not exist. These will not be included in the matching grid."))
    res <- res[!missing_soundscapes, ]
  }
  missing_templates <- !file.exists(res$template_path)
  if (any(missing_templates)) {
    n <- sum(missing_templates)
    warnings <- c(warnings, paste0("There are ", n, " crossings in which the template files do not exist. These will not be included in the matching grid."))
    res <- res[!missing_templates, ]
  }
  incompatible_sample_rates <- res$soundscape_sample_rate != res$template_sample_rate
  if (any(incompatible_sample_rates)) {
    n <- sum(incompatible_sample_rates)
    warnings <- c(warnings, paste0("There are ", n, " crossings in which the soundscape and template files have incompatible sample rates. These will not be included in the matching grid."))
    res <- res[!incompatible_sample_rates, ]
  }
  nyquist_frequency <- res$soundscape_sample_rate / 2
  incompatible_min_freq <- res$template_min_freq * 1000 >= nyquist_frequency
  if (any(incompatible_min_freq)) {
    n <- sum(incompatible_min_freq)
    warnings <- c(warnings, paste0(n, " templates have minimum frequencies higher than the Nyquist frequency (sample_rate / 2). These will not be included in the matching grid."))
    res <- res[!incompatible_min_freq, ]
  }
  incompatible_max_freq <- res$template_max_freq * 1000 >= nyquist_frequency
  if (any(incompatible_max_freq)) {
    n <- sum(incompatible_max_freq)
    warnings <- c(warnings, paste0(n, " templates have maximum frequencies higher than the Nyquist frequency (sample_rate / 2). These will not be included in the matching grid."))
    res <- res[!incompatible_max_freq, ]
  }
  template_duration_exceeds <- (res$template_end - res$template_start) >= res$soundscape_duration
  if (any(template_duration_exceeds)) {
    n <- sum(template_duration_exceeds)
    warnings <- c(warnings, paste0(n, " templates have durations higher than the soundscape duration. These will not be included in the matching grid."))
    res <- res[!template_duration_exceeds, ]
  }
  if (length(warnings) > 0) {
    warning(paste(warnings, collapse = "\n"))
  } else {
    message("All files are compatible and included in the matching grid.")
  }
  n <- nrow(res)
  return(res)
}
