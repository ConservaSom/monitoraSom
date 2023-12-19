#' Extract template metadata
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function extracts metadata from a directory of template wave files, which can be used for soundscape analysis.
#'
#' @param path The directory path containing the template wave files (for method = "standalone") or CSV files (for method = "roi_table").
#' @param recursive A logical value indicating whether the search for template files should be recursive or not. Default is TRUE.
#' @param method The method of metadata extraction. The available Options include "standalone" and "roi_table". See details for more information on the format of the input files for each method.
#'
#' @return A tibble containing the following metadata for each template file:
#' @import dplyr purrr
#' @importFrom data.table fread
#' @importFrom stringr str_replace str_pad
#' @importFrom collapse fmutate fselect fsubset
#' @export
fetch_template_metadata <- function(path, recursive = TRUE, method = "standalone") {

  # todo Adicionar informação de pitch_shift

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
      pattern =
      ".+\\d{3}\\.\\d{3}-\\d{3}\\.\\d{3}s_\\d{2}\\.\\d{3}-\\d{2}\\.\\d{3}kHz_\\d+wl_\\d+ovlp_.+\\.(wav|WAV)$",
      # "^W\\d+S\\d+_\\d{8}_\\d{6}_\\d{3}\\.\\d{3}-\\d{3}\\.\\d{3}s_\\d{2}\\.\\d{3}-\\d{2}\\.\\d{3}kHz_\\d+wl_\\d+ovlp_.+\\.(wav|WAV)$",
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

    get_metadata_safely <- safely(
      function(x) {
        res <- as.data.frame(readWave(x, header = TRUE))
        res$path <- x
        return(res)
      }
    )

    res <- map_df(
      template_list,
      ~ get_metadata_safely(.x)$result
    ) %>%
      collapse::fmutate(
        template_path = path,
        template_file = basename(path),
        template_name = basename(path),
        template_label = tail(
          unlist(strsplit(gsub(".WAV|.wav", "", basename(path)), split = "_")), 1
        ),
        template_start = 0,
        template_end = samples / sample.rate,
        template_sample_rate = sample.rate
      ) |>
      collapse::fselect(
        template_path, template_file, template_name, template_label,
        template_start, template_end, template_sample_rate
      ) |>
      collapse::fmutate(
        template_min_freq = map_dbl(
          base::strsplit(template_file, "_"),
          ~ .x[which(grepl("kHz$", .x))] %>%
            {
              strsplit(., "-")[[1]][1]
            } %>%
            as.numeric()
        ),
        template_max_freq = map_dbl(
          base::strsplit(template_file, "_"),
          ~ .x[which(grepl("kHz$", .x))] %>%
            {
              base::strsplit(., "-")[[1]][2]
            } %>%
            gsub("kHz", "", .) %>%
            as.numeric()
        ),
        template_wl = map_dbl(
          base::strsplit(template_file, "_"),
          ~ .x[which(grepl("wl$", .x))] %>%
            gsub("wl", "", .) %>%
            as.numeric()
        ),
        template_ovlp = map_dbl(
          base::strsplit(template_file, "_"),
          ~ .x[which(grepl("ovlp$", .x))] %>%
            gsub("ovlp", "", .) %>%
            as.numeric()
        )
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
      collapse::fmutate(
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
      collapse::fmutate(
        template_name = paste(
          stringr::str_replace(
            soundscape_file, ".wav|.WAV",
            paste0(
              "_",
              stringr::str_pad(sprintf("%.3f", round(roi_start, 3)), 7, pad = "0"), "-",
              stringr::str_pad(sprintf("%.3f", round(roi_end, 3)), 7, pad = "0"), "s_",
              stringr::str_pad(sprintf("%.3f", round(roi_min_freq, 3)), 6, pad = "0"), "-",
              stringr::str_pad(sprintf("%.3f", round(roi_max_freq, 3)), 6, pad = "0"), "kHz_",
              roi_wl, "wl_", roi_ovlp, "ovlp_", roi_label, ".wav"
            )
          )
        )
      ) %>%
      collapse::fselect(
        template_path, template_file, template_name, template_label,
        template_start, template_end, template_sample_rate,
        template_min_freq, template_max_freq, template_wl,
        template_ovlp, template_comment
      ) %>%
      collapse::fsubset(grepl("template", template_comment)) %>%
      collapse::fselect(-template_comment)
  }
  message("Template metadata successfully extracted")
  return(res)
}
