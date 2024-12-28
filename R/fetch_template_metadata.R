#' Extract template metadata
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function extracts metadata from a directory of template wave files,
#' which can be used for soundscape analysis.
#'
#' @param path The directory path containing the template wave files (for method
#'   = "standalone").
#' @param recursive A logical value indicating whether the search for template
#'   files should be recursive or not. Default is TRUE.
#'
#' @return A tibble containing the following metadata for each template file:
#' @import dplyr
#' @importFrom purrr map_df safely map_chr map_dbl
#' @importFrom tuneR readWave
#' @importFrom stringr str_split
#' @export
fetch_template_metadata <- function(path, recursive = TRUE) {

  template_list <- list.files(
    path,
    pattern = ".wav", full.names = TRUE, ignore.case = TRUE,
    recursive = recursive
  )

  if (length(template_list) == 0) {
    stop("No template files found in the specified directory")
  }

  temp_names <- basename(template_list)
  check_1 <- grepl(
    ".+\\d{3}\\.\\d{3}-\\d{3}\\.\\d{3}s_\\d{2}\\.\\d{3}-\\d{2}\\.\\d{3}kHz_\\d+wl_\\d+ovlp_.+\\.(wav|WAV)$", temp_names
  )

  if (!all(check_1)) {
    template_list <- template_list[check_1]
    non_matching_files <- temp_names[!check_1]
    if (length(non_matching_files) > 0) {
      warning(
        "The following files do not match the template naming convention and were not included in the result:\n",
        paste0(non_matching_files, collapse = "\n")
      )
    }
  } else if (all(check_1 == FALSE)) {
    stop("None of the wav files found in the specified directory match the template naming convention")
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
