#' Validate detections by overlap with Regions of Interest (ROIs)
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function takes in two data frames, one containing Regions of Interest
#' (ROIs) and another containing detections. It then validates the detections by
#' checking for overlaps with the ROIs. The function returns a data frame with
#' the validated detections.
#'
#' @param df_rois A data frame containing Regions of Interest (ROIs) as in the
#'   output of `fetch_rois_n()`.
#' @param df_detecs A data frame containing detections as in the output of
#'   `fetch_score_peaks_n()`.
#' @param det_species A character string specifying the species to validate.
#'
#' @return A data frame with the validated detections.
#' @export
#'
validate_by_overlap_i <- function(df_rois, df_detecs, det_species) {
  df_rois <- df_rois %>%
    mutate(
      soundscape_file = paste0(substr(roi_file, 1, 33), ".wav"),
      soundscape_path = paste0("soundscapes//", soundscape_file),
      roi_id = paste0(
        paste0("roi", sprintf("%06d", 1:nrow(df_rois)))
      ),
      species = roi_label
    ) %>%
    filter(species == det_species)

  df_detections <- df_detecs %>%
    mutate(
      soundscape_file = gsub(".WAV", ".wav", soundscape_file),
      soundscape_path = paste0("soundscapes//", soundscape_file),
      species = ifelse(
        template_file == "BirdNet",
        sub(".*_([^_]+)$", "\\1", template_name),
        gsub(
          ".wav|.WAV", "",
          regmatches(
            template_file,
            regexpr("(?<=_)[^_]+$", template_file, perl = TRUE)
          )
        )
      ),
      detection_id = paste0(
        "det", sprintf("%06d", 1:nrow(df_detecs))
      )
    ) %>%
    filter(species == det_species) %>%
    group_by(template_name) %>%
    group_split()

  df_all_overlaps <- map_dfr(
    df_detections,
    function(x) {
      # find macthes between detections and rois. This process is done grouping and
      # splitting the dataframe by soundscape_file. This is necessary because only
      # overlaps that happen within the same soundscape are relevant. Non
      # overlapping elements of both dataframes are discarded here and will be added
      # later.
      df_detec_inner <- x %>%
        inner_join(
          df_rois,
          by = c("soundscape_file", "species"),
          suffix = c("", "_y"), relationship = "many-to-many"
        ) %>%
        select(-soundscape_path_y) %>%
        group_by(soundscape_file) %>%
        group_split() %>%
        map_dfr(
          ~ .x %>%
            mutate(
              validation = ifelse(
                (detection_start <= roi_end) & (detection_end >= roi_start),
                "TP", "FP"
              )
            ) %>%
            as.data.frame()
        )

      if (nrow(df_detec_inner) == 0) {
        # When the inner_join returns no matches, it means that all detections
        # can be cosnidered false positives (FP).
        df_detec_proc <- x %>%
          full_join(
            df_rois,
            by = c("soundscape_file", "species"),
            suffix = c("", "_y"), relationship = "many-to-many"
          ) %>%
          mutate(validation = NA) %>%
          select(-ends_with("_y"))

        df_detec_val <- full_join(
          df_detec_proc,
          filter(df_rois, !roi_id %in% df_detec_proc$roi_id),
          by = colnames(df_rois)
        ) %>%
          mutate_at(vars(starts_with("template")), ~ na.omit(unique(.))) %>%
          mutate(validation = ifelse(is.na(roi_id), "FP", "FN")) %>%
          arrange(soundscape_file, detection_id)
      } else {
        # The next step is to recompose the metadata of detections and rois, as well
        # as detections from soundscapes with no rois. Grouping the process by
        # template_name is an essential step for a correct recomposition. Those
        # detections are considered false positives (FP) and receive "no_match" as
        # roi_id.
        df_detec_proc <- suppressMessages(
          full_join(
            df_detec_inner, x,
            by = colnames(df_detections),
            suffix = c("", "_y")
          )
        ) %>%
          mutate(
            validation = ifelse(is.na(validation), "FP", validation),
            roi_id = ifelse(validation == "FP", NA, roi_id)
          ) %>%
          select(-ends_with("_y"))

        # Adding back rois from soundscapes with no detections from the target
        # species. This process must be done for each template separately. The added
        # rows will be validated as FN.
        df_detec_val <- full_join(
          df_detec_proc,
          filter(df_rois, !roi_id %in% df_detec_proc$roi_id),
          by = colnames(df_rois)
        ) %>%
          mutate_at(vars(starts_with("template")), ~ na.omit(unique(.))) %>%
          mutate(validation = ifelse(is.na(validation), "FN", validation)) %>%
          arrange(soundscape_file, detection_id) # %>% glimpse()
      }

      return(df_detec_val)
    }
  )

  df_val <- df_all_overlaps %>%
    group_by(template_name, detection_id, validation) %>%
    summarise(peak_score = max(peak_score)) %>%
    summarise(
      validation = case_when(
        any(validation == "TP") ~ "TP",
        all(validation == "FN") ~ "FN",
        all(validation == "FP") ~ "FP"
      ),
      peak_score = max(peak_score)
    ) %>%
    ungroup() %>%
    arrange(peak_score)

  res <- map_dfr(
    seq(0, 1, 0.01),
    ~ {
      df_val$validation[which(df_val$validation == "TP" & df_val$peak_score < .x)] <- "FN"
      df_val$validation[which(df_val$validation == "FN" & df_val$peak_score >= .x)] <- "TP"
      df_val$validation[which(df_val$validation == "FP" & df_val$peak_score < .x)] <- "TN"
      df_val$validation[which(df_val$validation == "TN" & df_val$peak_score >= .x)] <- "FP"
      res <- df_val %>%
        summarise(
          template_name = unique(template_name),
          min_score = .x,
          n_TP = sum(validation == "TP"),
          n_FP = sum(validation == "FP"),
          n_FN = sum(validation == "FN"),
          n_TN = sum(validation == "TN")
        )
      return(res)
    }
  )
  return(res)
}
