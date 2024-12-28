#' Batch validation of detections by overlap with Regions of Interest (ROIs)
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function takes in two data frames, one containing Regions of Interest
#'   (ROIs) and another containing detections. It then validates the detections
#'   by checking for overlaps with the ROIs. The function returns a data frame
#'   with the validated detections.
#'
#' @param df_rois A data frame containing Regions of Interest (ROIs) as in the
#'   output of `fetch_rois_n()`.
#' @param df_detecs A data frame containing detections as in the output of
#'   `fetch_score_peaks_n()`.
#' @param validation_user A character string specifying the user name.
#'
#' @return A data frame with the validated detections.
#' @import dplyr tidyr
#' @importFrom purrr map_chr map2
#' @export
validate_by_overlap_n <- function(df_rois, df_detecs, validation_user) {

  requireNamespace("dplyr")
  requireNamespace("purrr")

  validation_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Prepare ROIs for validation
  tib_rois <- df_rois %>%
    dplyr::mutate(
      soundscape_path = paste0("soundscapes//", soundscape_file),
      roi_id = 1:nrow(df_rois), species = roi_label
    ) %>%
    dplyr::group_split(species) %>%
    tibble::tibble(
      species = purrr::map_chr(., ~ unique(.x$species)), rois = .
    )

  # Prepare detections for validation
  tib_detecs <- df_detecs %>%
    dplyr::mutate(
      soundscape_path = paste0("soundscapes//", soundscape_file),
      species = gsub(".*_", "", gsub("\\.wav|.WAV", "", template_name)),
      detection_id = 1:nrow(df_detecs)
    ) %>%
    dplyr::group_split(species, template_file) %>%
    tibble::tibble(
      species = purrr::map_chr(., ~ unique(.x$species)),
      template_file = purrr::map_chr(., ~ unique(.x$template_file)),
      detections = .
    )

  # Check for availability of ROIs for each detected species
  if (all(tib_detecs$species %in% tib_rois$species)) {
    message("All detected species have ROIs for validation")
  } else if (any(tib_detecs$species %in% tib_rois$species)) {
    warning(
      paste0(
        "The following detected species have no ROIs for validation: ",
        paste(
          tib_detecs$species[which(!(tib_detecs$species %in% tib_rois$species))],
          collapse = ", "
        )
      )
    )
  } else {
    stop("There are no ROIs of any detected species")
  }

  # Join detections with ROIs based on species and soundscape file
  ls_validation <- dplyr::left_join(tib_detecs, tib_rois, by = "species") %>%
    dplyr::filter(!is.na(rois))

  # Validate detections by checking for overlaps with ROIs
  val_res_raw <- map2(
    # the process is done for each pair of detection-ROI
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
        df_detec_inner_raw <- dplyr::inner_join(
          x, y, by = c("soundscape_file", "species"), suffix = c("", "_y"),
          relationship = "many-to-many"
        )
        if (nrow(df_detec_inner_raw) == 0) {
          # if there are no detections, all detections are considered false positives
          res_i <- x %>%
            dplyr::mutate(
              validation_user = validation_user,
              validation_time = validation_time,
              validation = "FP",
              validation_note = "no intersection with a ROI"
            )
        } else {
          # if there are detections, check for overlaps
          df_detec_inner <- df_detec_inner_raw %>%
            dplyr::select(-soundscape_path_y) %>%
            dplyr::group_by(soundscape_file) %>%
            dplyr::group_split() %>%
            purrr::map_dfr(
              ~ .x %>%
                dplyr::mutate(
                  validation = ifelse(
                    between(detection_start, roi_start, roi_end) |
                      between(detection_end, roi_start, roi_end) |
                      (detection_start <= roi_start & detection_end >= roi_end),
                    "TP", "FP"
                  )
                )
            )

          # Get the TP
          dfTP <- df_detec_inner %>%
            dplyr::filter(validation == "TP") %>%
            dplyr::mutate(validation_note = "instersection with a ROI")

          # FP are all detections that have not overlapped a roi within soundscapes
          dfFP_a <- df_detec_inner %>%
            dplyr::filter(validation == "FP", !(detection_id %in% dfTP$detection_id)) %>%
            dplyr::select(-starts_with("roi_")) %>%
            dplyr::mutate(validation_note = "no intersection with a ROI") %>%
            dplyr::distinct()

          # ... or when there are no ROIs to intersect with
          dfFP_b <- x %>%
            dplyr::filter(
              !(
                detection_id %in%
                  unique(c(dfTP$detection_id, dfFP_a$detection_id)))
            ) %>%
            dplyr::distinct() %>%
            dplyr::mutate(
              validation = "FP", validation_note = "no ROIs to intersect with"
            )

          # Combine TP, FP_a and FP_b
          res_raw <- dfTP %>%
            dplyr::full_join(dfFP_a, by = colnames(dfFP_a)) %>%
            dplyr::full_join(dfFP_b, by = colnames(dfFP_b))

          # False negatives are all ROIs that have no detections within them
          dfFN <- y[which(!(y$roi_id %in% res_raw$roi_id)), ] %>%
            dplyr::mutate(
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

          # Combine TP, FP and FN
          res_i <- res_raw %>%
            # remove for retrieval of multiple overlaps
            dplyr::arrange(desc(detection_id), peak_score) %>%
            dplyr::filter(!duplicated(detection_id)) %>%
            dplyr::full_join(dfFN, by = colnames(dfFN)) %>%
            dplyr::arrange(soundscape_file, detection_start) %>%
            dplyr::mutate(
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
  res <- dplyr::bind_rows(val_res_raw)
  return(res)
}
