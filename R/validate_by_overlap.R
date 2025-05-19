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
#'   output of `fetch_rois()`. Alternatively, a path to a directory containing
#'   the ROI tables as CSV files can be provided.
#' @param df_detecs A data frame containing detections as in the output of
#'   `fetch_score_peaks()`. Alternatively, a path to a CSV file containing the
#'   detections (it accepts only one CSV file at a time).
#' @param validation_user A character string specifying the user name.
#' @param recursive A logical value indicating whether to search for ROIs
#'   recursively when a path is provided instead of a data frame. Defaults to
#'   FALSE.
#' @param output_path A character string specifying the path to the output file.
#'   Defaults to NULL. it is recommended to export it as a CSV file to the
#'   "validation_output/" directory to avoid replacement of existing detection
#'   files.
#'
#' @return A data frame with the validated detections.
#' @export
#' @examples
#' \dontrun{
#'
#' # Load the necessary packages to run this example
#' library(monitoraSom)
#' library(dplyr)
#'
#' # Load the detections to populate the example data
#' data(df_detecs)
#' glimpse(df_detecs)
#'
#' # Load the rois to populate the example data
#' data(df_rois)
#' glimpse(df_rois)
#'
#' # Run the validation by overlap.
#' df_detecs_val_tovlp <- validate_by_overlap(
#'   df_detecs = df_detecs, df_rois, validation_user = "User"
#' )
#' glimpse(df_detecs_val_tovlp)
#'
#' }
validate_by_overlap <- function(
  df_detecs, df_rois, validation_user = NULL, recursive = FALSE, 
  output_path = NULL
  ) {

  requireNamespace("dplyr")
  requireNamespace("purrr")

  if (is.character(df_rois)) {
    if (file.exists(df_rois)) {
      df_rois <- fetch_rois(df_rois, recursive = recursive)
    } else {
      stop("There is no file at the provided path to df_rois")
    }
  } else if (!is.data.frame(df_rois)) {
    stop(
      "The input of the df_rois argument must be a data frame or a path to a CSV file"
    )
  }

  if (is.character(df_detecs)) {
    if (file.exists(df_detecs)) {
      df_detecs <- read.csv(df_detecs)
    } else {
      stop("There is no file at the provided path to df_detecs")
    }
  } else if (!is.data.frame(df_detecs)) {
    stop(
      "The input of the df_detecs argument must be a data frame or a path to a CSV file"
    )
  }

  if (is.null(validation_user)) {
    stop("Please, identify yourself by setting the validation_user argument")
  }

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
  val_res_raw <- purrr::map2(
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

  if (!is.null(output_path)) {
    write.csv(res, output_path, row.names = FALSE)
    message("Validation results have been saved to ", output_path)
  } else {
    message("Validation results have been returned to the R session")
  }
  return(res)
}
