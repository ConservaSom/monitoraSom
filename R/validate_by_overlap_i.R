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

  # todo Acicionar controle de caixa alta nas extensões dos arquivos

  # todo Preencher nome e caminhos de templates automaticamente para admitir
  # birdnet

  # todo Adicionar uma rodada de template matching entre as rois FN e os
  # templates para completar os scores

  df_rois <- df_rois %>%
    mutate(
      soundscape_file = paste0(substr(roi_file, 1, 33), ".wav"), # todo solve extension cases
      roi_id = paste0(
        paste0("roi", sprintf("%06d", 1:nrow(df_rois)))
      ),
      species = roi_label
    ) %>%
    filter(species == det_species)

  df_detections <- df_detecs %>%
    mutate(
      species = gsub(".wav|.WAV", "",
        regmatches(template_file,
          regexpr("(?<=_)[^_]+$", template_file, perl = TRUE)
        )
      ),
      detection_id = paste0(
        "det", sprintf("%06d", 1:nrow(df_detections))
      )
    ) %>%
    filter(species == det_species) %>%
    group_by(template_name) %>%
    group_split()

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
        arrange(soundscape_file, detection_id)

      return(df_detec_val)
    }
  )
  return(res)
}
