#' Batch validation of detections by overlap with Regions of Interest (ROIs)
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
#' @param validation_user A character string specifying the user name.
#'
#' @return A data frame with the validated detections.
#' @export
validate_by_overlap_n <- function(df_rois, df_detecs, validation_user) {
  validation_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  tib_rois <- df_rois %>%
    mutate(
      soundscape_path = paste0("soundscapes//", soundscape_file),
      roi_id = 1:nrow(df_rois),
      species = roi_label
    ) %>%
    group_by(species) %>%
    group_split() %>%
    tibble(
      species = map_chr(., ~ unique(.x$species)),
      rois = .
    )

  tib_detecs <- df_detecs %>%
    mutate(
      soundscape_file = gsub(".WAV", ".wav", soundscape_file),
      soundscape_path = paste0("soundscapes//", soundscape_file),
      species = gsub(".*_", "", gsub("\\.wav", "", template_name)),
      detection_id = 1:nrow(df_detecs)
    ) %>%
    group_by(species, template_file) %>%
    group_split() %>%
    tibble(
      species = map_chr(., ~ unique(.x$species)),
      template_file = map_chr(., ~ unique(.x$template_file)),
      detections = .
    )

  if (all(tib_detecs$species %in% tib_rois$species)) {
    message("All detected species have ROIs for validation")
  } else if (any(tib_detecs$species %in% tib_rois$species)) {
    warning(
      paste0(
        "The following detected species have no ROIs for validation: ",
        paste(
          tib_detecs$species[
            which(!(tib_detecs$species %in% tib_rois$species))
          ],
          collapse = ", "
        )
      )
    )
  } else {
    stop("There are no ROIs of any detected species")
  }

  ls_validation <- left_join(tib_detecs, tib_rois, by = "species") %>%
    filter(!is.na(rois))

  val_res_raw <- map2(
    ls_validation$detections, ls_validation$rois,
    function(x, y) {
      if (is.null(y)) {
        return("No ROIs to validate with")
      } else {
        # find macthes between detections and rois. This process is done grouping and
        # splitting the dataframe by soundscape_file. This is necessary because only
        # overlaps that happen within the same soundscape are relevant. Non
        # overlapping elements of both dataframes are discarded here and will be added
        # later.
            df_detec_inner_raw <- inner_join(
                x, y,
                by = c("soundscape_file", "species"),
                suffix = c("", "_y"), relationship = "many-to-many"
            )
            if (nrow(df_detec_inner_raw) == 0) {
              res_i <- x %>%
                mutate(
                  validation_user = validation_user,
                  validation_time = validation_time,
                  validation = "FP",
                  validation_note = "no intersection with a ROI"
                )
            } else {
              df_detec_inner <- df_detec_inner_raw %>%
                select(-soundscape_path_y) %>%
                group_by(soundscape_file) %>%
                group_split() %>%
                map_dfr(
                  ~ .x %>%
                    mutate(
                      validation = ifelse(
                        (detection_start <= roi_end) &
                          (detection_end >= roi_start),
                        "TP", "FP"
                      )
                    ) %>%
                    as.data.frame()
                )

              # TP prontos, checar se não tem duplicação
              dfTP <- df_detec_inner %>%
                filter(validation == "TP") %>%
                mutate(
                  validation_note = "instersection with a ROI"
                )

              # false positives from all detections that have not overlapped a roi
              # within soundscapes
              dfFP_a <- df_detec_inner %>%
                filter(validation == "FP", !(detection_id %in% dfTP$detection_id)) %>%
                select(-starts_with("roi_")) %>%
                mutate(validation_note = "no intersection with a ROI") %>%
                distinct()

              # additional false positives
              dfFP_b <- x %>%
                filter(
                  !(
                    detection_id %in%
                      unique(c(dfTP$detection_id, dfFP_a$detection_id)))
                ) %>%
                dplyr::distinct() %>%
                mutate(
                  validation = "FP", validation_note = "no ROIs to intersect with"
                )

              res_raw <- dfTP %>%
                full_join(dfFP_a, by = colnames(dfFP_a)) %>%
                full_join(dfFP_b, by = colnames(dfFP_b))

              dfFN <- y[which(!(y$roi_id %in% res_raw$roi_id)), ] %>%
                mutate(
                  template_path = unique(x$template_path),
                  template_file = unique(x$template_file),
                  template_name = unique(x$template_name),
                  template_min_freq = unique(x$template_min_freq),
                  template_max_freq = unique(x$template_max_freq),
                  template_start = unique(x$template_start),
                  template_end = unique(x$template_end),
                  validation = "FN",
                  validation_note = "no detections to intersect with"
                )

              res_i <- res_raw %>%
                # remove for retrieval of multiple overlaps
                arrange(desc(detection_id), peak_score) %>%
                filter(!duplicated(detection_id)) %>%
                full_join(dfFN, by = colnames(dfFN)) %>%
                arrange(soundscape_file, detection_start) %>%
                mutate(
                  validation_user = validation_user,
                  validation_time = validation_time,
                  .before = "validation"
                ) %>%
                tibble()
            }
        return(res_i)
      }
    }
  )
  res <- bind_rows(val_res_raw)
  return(res)
}