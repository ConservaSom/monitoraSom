#' Extract template metadata
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function extracts metadata from a directory of template wave files,
#'   which can be used for soundscape analysis.
#'
#' @param templates_path The directory path containing the template wave files.
#'   If no path is provided, the function will look for the templates in the
#'   "040_roi_cuts/" directory.
#' @param recursive A logical value indicating whether the search for template
#'   files should be recursive or not. Defaults to FALSE.
#'
#' @return A tibble containing the following metadata for each template file:
#' @import dplyr
#' @importFrom purrr map_df safely map_chr map_dbl
#' @importFrom tuneR readWave
#' @importFrom stringr str_split
#' @export
#' @examples
#' \dontrun{
#'
#' # Load the necessary packages to run this example
#' library(monitoraSom)
#' library(dplyr)
#' library(tuneR)
#' 
#' # Load the templates to populate the example data
#' data(ls_templates)
#' 
#' # Create a directory and export the templates
#' templates_path <- "./040_roi_cuts"
#' dir.create(templates_path)
#' invisible(lapply(1:length(ls_templates), function(i) {
#'   writeWave(
#'     ls_templates[[i]], file.path(templates_path, names(ls_templates)[i])
#'   )
#' }))
#' 
#' # Import the templates metadata
#' df_templates <- fetch_template_metadata(templates_path = templates_path)
#' glimpse(df_templates)
#' 
#' }
fetch_template_metadata <- function(templates_path = NULL, recursive = FALSE) {

  templates_path <- if (is.null(templates_path)) {
    "040_roi_cuts/"
  } else if (!dir.exists(templates_path)) {
    stop("The provided path to the templates does not exist")
  } else {
    templates_path
  }

  template_list <- list.files(
    templates_path, pattern = ".wav", full.names = TRUE, ignore.case = TRUE,
    recursive = recursive
  )

  if (length(template_list) == 0) {
    stop("There are no WAV files in the provided path")
  }

  temp_names <- basename(template_list)
  check_1 <- grepl(
    ".+\\d{3}\\.\\d{3}-\\d{3}\\.\\d{3}s_\\d{2}\\.\\d{3}-\\d{2}\\.\\d{3}kHz_\\d+wl_\\d+ovlp_.+\\.(wav|WAV)$",
    temp_names
  )

  if (!all(check_1)) {
    template_list <- template_list[check_1]
    non_matching_files <- temp_names[!check_1]
    if (length(non_matching_files) > 0) {
      warning(
        paste(
          "The following files do not match the template naming convention",
          "and were not included in the result:\n",
          paste0(non_matching_files, collapse = "\n")
        )
      )
    }
  } else if (all(check_1 == FALSE)) {
    stop(
      paste(
        "None of the wav files found in the specified directory match the",
        "template naming convention"
      )
    )
  }

  get_metadata_safely <- purrr::safely(
    function(x) {
      res <- as.data.frame(tuneR::readWave(x, header = TRUE))
      res$template_path <- x
      return(res)
    }
  )

  res <- purrr::map_df(template_list, ~ get_metadata_safely(.x)$result) %>%
    dplyr::mutate(
      template_path = template_path,
      template_file = basename(template_path),
      template_name = basename(template_path),
      template_label = gsub(
        ".WAV|.wav", "", purrr::map_chr(
          stringr::str_split(template_path, "_"),
          ~ tail(., 1)
        )
      ),
      template_start = 0,
      template_end = samples / sample.rate,
      template_sample_rate = sample.rate
    ) |>
    dplyr::select(
      template_path, template_file, template_name, template_label,
      template_start, template_end, template_sample_rate
    ) |>
    dplyr::mutate(
      template_min_freq = purrr::map_dbl(
        base::strsplit(template_file, "_"),
        ~ .x[which(grepl("kHz$", .x))] %>%
          {
            strsplit(., "-")[[1]][1]
          } %>%
          as.numeric()
      ),
      template_max_freq = purrr::map_dbl(
        base::strsplit(template_file, "_"),
        ~ .x[which(grepl("kHz$", .x))] %>%
          {
            base::strsplit(., "-")[[1]][2]
          } %>%
          gsub("kHz", "", .) %>%
          as.numeric()
      ),
      template_wl = purrr::map_dbl(
        base::strsplit(template_file, "_"),
        ~ .x[which(grepl("wl$", .x))] %>%
          gsub("wl", "", .) %>%
          as.numeric()
      ),
      template_ovlp = purrr::map_dbl(
        base::strsplit(template_file, "_"),
        ~ .x[which(grepl("ovlp$", .x))] %>%
          gsub("ovlp", "", .) %>%
          as.numeric()
      )
    )

  message("Template metadata successfully extracted")
  return(res)
}
