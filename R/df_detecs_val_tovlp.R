#' Detections with validation labels based on temporal overlap
#'
#' A data frame containing detections with validation labels assigned based on
#' temporal overlap with manually validated detections. The detections were
#' extracted from soundscape recordings with the `run_matching()` function and
#' validated by comparing their temporal overlap with manually validated
#' detections in `df_detecs_val_manual`. Detections with sufficient temporal
#' overlap with true positives were labeled as true positives (TP), while those
#' without sufficient overlap were labeled as false positives (FP).
#'
#' @format ## `df_detecs_val_tovlp` A data frame containing detections with
#'   validation labels and all the standard detection variables from the
#'   `run_matching()` function output.
"df_detecs_val_tovlp"
