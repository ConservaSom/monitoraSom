#' Extract template metadata
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function extracts metadata from a directory of template waveform files, which can be used for soundscape analysis.
#'
#' @param path The directory path containing the template waveform files (for method = "standalone") or CSV files (for method = "roi_table").
#' @param recursive A logical value indicating whether the search for template files should be recursive or not. Default is TRUE.
#' @param method The method of metadata extraction. The available Options include "standalone" and "roi_table". See details for more information on the format of the input files for each method.
#'
#' @return A tibble containing the following metadata for each template file:
#'
#' @export
fetch_template_metadata <- function(path, recursive = TRUE, method = c("standalone")) {
  if (method == "standalone") {
    template_list <- list.files(
      path, pattern = ".wav", full.names = TRUE, ignore.case = TRUE,
      recursive = recursive
    )
    if (length(template_list) == 0) {
      stop("No template files found in the specified directory")
    }

    temp_names <- basename(template_list)
    check_1 <- grepl(
      pattern = "^W\\d+S\\d+_\\d{8}_\\d{6}_\\d{3}\\.\\d{3}-\\d{3}\\.\\d{3}s_\\d{2}\\.\\d{3}-\\d{2}\\.\\d{3}kHz_\\d+wl_\\d+ovlp_.+\\.(wav|WAV)$",
      temp_names
    )
    if (any(check_1 == FALSE)) {
      template_list <- template_list[check_1 == TRUE]
      n <- length(check_1 == FALSE)
      warning(
        "The following files do not match the template naming convention and were not included in the result:\n",
        paste0(temp_names[check_1 == FALSE], collapse = "\n")
      )
    } else if (all(check_1 == FALSE)) {
      stop("None of the wav files found in the specified directory match the template naming convention")
    }

    res <- map_df(
      template_list, ~ unlist(av_media_info(.x), recursive = FALSE)
    ) %>%
      rename_with(~ gsub("audio.", "", .x)) |>
      fmutate(
        template_path = template_list,
        template_file = basename(template_list),
        template_label =
          tail(
            strsplit(
              gsub(".WAV|.wav", "", template_file),
              split = "_"
            )[[1]], 1
          ),
        template_start = 0,
        template_end = duration,
        template_sample_rate = sample_rate
      ) |>
      fselect(
        template_path, template_file, template_label,
        template_start, template_end, template_sample_rate
      ) |>
      fmutate(
        template_min_freq = strsplit(
          strsplit(template_file, "_")[[1]][5], "-"
        )[[1]][1] %>%
          gsub("kHz", "", .) %>% # str_remove("kHz") %>%
          as.numeric(),
        template_max_freq = strsplit(
          strsplit(template_file, "_")[[1]][5], "-"
        )[[1]][2] %>%
          gsub("kHz", "", .) %>% # str_remove("kHz") %>%
          as.numeric(),
        template_wl = strsplit(template_file, "_")[[1]][6] %>%
          gsub("wl", "", .) %>% as.numeric(),
        template_ovlp = strsplit(template_file, "_")[[1]][7] %>%
          gsub("ovlp", "", .) %>% as.numeric()
      )
  } else if (method == "roi_table") {
    table_list <- list.files(
      path,
      pattern = ".csv", full.names = TRUE,
      ignore.case = TRUE, recursive = TRUE
    )
    if (length(table_list) == 0) {
      stop("No ROI tables found in the specified directory")
    }
    res <- map(table_list, ~ fread(.x)) |>
      list_rbind() |>
      fmutate(
        template_path = soundscape_path,
        template_file = soundscape_file,
        template_label = roi_label,
        template_start = roi_start,
        template_end = roi_end,
        template_sample_rate = roi_sample_rate,
        template_min_freq = roi_min_freq,
        template_max_freq = roi_max_freq,
        template_wl = roi_wl,
        template_ovlp = roi_ovlp,
        template_comment = roi_comment
      ) %>%
      fselect(
        template_path, template_file, template_label,
        template_start, template_end, template_sample_rate,
        template_min_freq, template_max_freq, template_wl,
        template_ovlp, template_comment
      ) %>%
      fsubset(grepl("template", template_comment)) %>%
      fselect(-template_comment)
  }
  message("Template metadata successfully extracted")
  return(res)
}
