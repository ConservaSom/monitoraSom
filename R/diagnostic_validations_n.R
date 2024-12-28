#' Compute detection diagnostics of validated detections from multiple templates
#'
#' #' @description `r lifecycle::badge("experimental")`
#'
#' This function performs diagnostic validations on the detections from
#' `diagnostic_validations_i()` in cases where there are detections generated
#' for multiple templates or classes. The diagnostic validations are used to
#' determine the optimal cutpoint to be used in the detections. The function
#' will automatically determine the cutpoint to be used in the diagnostic
#' validations if the "Auto" method is selected. If the "Manual" method is
#' selected, the user must provide the cutpoint to be used in the diagnostic
#' validations.
#'
#' @param val_n A tibble containing the validation results of the detections
#'   `validate_by_overlap()` function or within the validation app.
#' @param diag_method A character string indicating the method to use for
#'   diagnostic validations. The two methods available are: "Auto" (default) or
#'   "Manual". If "Auto" is selected, the function will automatically determine
#'   the cutpoint to be used in the diagnostic validations. If "Manual" is
#'   selected, the user must provide the cutpoint to be used in the diagnostic
#'   validations.
#' @param pos_prob A numeric value indicating the probability of a positive test
#' @return A list of results from the diagnostic validations, or NULL if all validations failed.
#' @export
#'
#' @import dplyr
diagnostic_validations_n <- function(
    val_n, diag_method = "Auto", pos_prob = 0.95) {

  split_validations <- val_n %>%
    group_by(template_name) %>%
    group_split()

  res <- lapply(
    split_validations,
    function(x) {
      tryCatch(
        {
          suppressWarnings(
            monitoraSom::diagnostic_validations_i(
              val_i = x, diag_method = diag_method, pos_prob = pos_prob
            )
          )
        },
        error = function(e) {
          message(
            "Error extracting detection diagnostics of template:",
            x$template_name[1]
          )
          NULL
        }
      )
    }
  )

  if (all(sapply(res, is.null))) {
    message("All diagnostic validations failed.")
  } else {
    message("Validation diagnostics completed successfully.")
  }

  return(res)
}
