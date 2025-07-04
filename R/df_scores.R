#' Raw results of the template matching
#'
#' A tibble (see the tidyr package for more details) containing the raw results
#' of the template matching run with the `run_matching()` function with the default
#' parameters.
#'
#' @format ## `df_scores` A data frame with 72 rows and 20 columns containing
#'   raw template matching results, including soundscape and template file paths,
#'   audio parameters, template frequency ranges, and score vectors containing
#'   correlation values between templates and soundscape segments. The raw score
#'   vectors are stored as a list-column, which cannot be exported as a CSV file,
#'   but it can be exported as an RData or Rds file.
"df_scores"
