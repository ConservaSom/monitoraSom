
#' Extract template metadata
#'
#' This function extracts metadata from a directory of template waveform files, which can be used for soundscape analysis.
#'
#' @param path The directory path containing the template waveform files.
#' @param method The method to use for metadata extraction. Options include "standalone" to extract metadata from standalone waveform files, or "roi_table" to extract metadata from a table of regions of interest (ROIs).
#'
#' @return A tibble containing the following metadata for each template file:
#' \itemize{
#' \item \code{template_path}: The full path to the template file.
#' \item \code{template_file}: The filename of the template file.
#' \item \code{template_label}: A label for the template file, extracted from the filename.
#' \item \code{template_start}: The start time of the template file, set to 0.
#' \item \code{template_end}: The end time of the template file, calculated from the duration of the file.
#' \item \code{template_sample_rate}: The sample rate of the template file.
#' \item \code{template_min_freq}: The minimum frequency of the template file, extracted from the filename.
#' \item \code{template_max_freq}: The maximum frequency of the template file, extracted from the filename.
#' \item \code{template_wl}: The waveform length (WL) of the template file, extracted from the filename.
#' \item \code{template_ovlp}: The waveform overlap (OVLP) of the template file, extracted from the filename.
#' }
#'
#' @export
#'
#' @examples
#' # Extract metadata from standalone waveform files
#' fetch_template_metadata("/path/to/templates", method = "standalone")
#'
#' # Extract metadata from a table of ROIs
#' fetch_template_metadata("/path/to/rois.csv", method = "roi_table")
fetch_template_metadata <- function(path, method = c("standalone")) {
  if (method == "standalone") {
    template_list <- list.files(
      path,
      pattern = ".wav",
      full.names = TRUE, ignore.case = TRUE, recursive = TRUE
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
        template_min_freq = min_freq,
        template_max_freq = max_freq,
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
      filter(grepl("template", template_comment)) %>%
      select(-template_comment)
  }
  message("Template metadata successfully extracted")
  return(res)
}
