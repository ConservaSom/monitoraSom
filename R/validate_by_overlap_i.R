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
    group_split() %>%
      as.list()

  res <- map_dfr(
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

      # TP prontos, checar se não tem duplicação
      dfTP <- df_detec_inner %>%
        filter(validation == "TP") %>%
          mutate(
            validation_obs = "instersection with a ROI"
          )

      # false positives from all detections that have not overlapped a roi
      # within soundscapes
      dfFP_a <- df_detec_inner %>%
        filter(validation == "FP", !(detection_id %in% dfTP$detection_id)) %>%
        select(-starts_with("roi_")) %>%
        mutate(validation_obs = "no intersection with a ROI") %>%
        distinct()

      # additional false positives
      dfFP_b <- x %>%
        filter(!(detection_id %in% unique(c(dfTP$detection_id, dfFP_a$detection_id)))) %>%
        dplyr::distinct() %>%
          mutate(
            validation = "FP", validation_obs = "no ROIs to intersect with"
          )

      res_raw <- dfTP %>%
        full_join(dfFP_a, by = colnames(dfFP_a)) %>%
        full_join(dfFP_b, by = colnames(dfFP_b))

      dfFN <- df_rois[which(!(df_rois$roi_id %in% res_raw$roi_id)), ] %>%
        mutate(validation = "FN", validation_obs = "no detections to intersect with")

      res_i <- res_raw %>%
        # remove for retrieval of multiple overlaps
        arrange(-detection_id, peak_score) %>% 
        filter(!duplicated(detection_id)) %>%
        full_join(dfFN, by = colnames(dfFN))

      return(res_i)
    }
  )
  return(res)
}
