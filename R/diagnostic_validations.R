#' Compute detection diagnostics of validated detections from multiple templates
#'
#' #' @description `r lifecycle::badge("experimental")`
#'
#' This function performs diagnostic validations on the detections from
#' `diagnostic_validations_i()` in cases where there are detections generated
#' for multiple templates or classes. The diagnostic validations are used to
#' determine the optimal cutpoint to be used in the detections. This function is
#' a wrapper around `diagnostic_validations_i()`, and is works stricctly in the
#' "auto" mode, thus cutpoints are automatically determined by the logistic
#' regression model, if the data allows for it.
#'
#' @param df_validated A tibble containing the validation results of the
#'   detections `validate_by_overlap()` function or within the validation app.
#' @param pos_prob A numeric value indicating the probability of a positive test
#' @param val_a_priori A logical value indicating whether the validation was
#'   performed with a priori method (with the `validate_by_overlap()` function),
#'   which defaults to TRUE. If FALSE, it will be assumed that the validation
#'   was performed with the a posteriori method (with the validation app).
#' @return A list of results from the diagnostic validations, or NULL if all
#'   validations failed.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Load the necessary packages to run this example
#' library(monitoraSom)
#' library(dplyr)
#' library(ggplot2)
#' library(patchwork)
#'
#' # Load a dataset with detections validated with the time overlap method (output
#' # of `validate_by_overlap()`)
#' data(df_detecs_val_tovlp)
#' glimpse(df_detecs_val_tovlp)
#'
#' # In this type of validation it is recommended to mitigate the large number of
#' # false positives. Here we filter the detections with a peak score greater than
#' # 0.2, but other methods can be used for more informed decisions at this stage.
#' df_detecs_val_tovlp_subset <- df_detecs_val_tovlp %>%
#'   filter(peak_score > 0.2) %>%
#'   glimpse()
#'
#' # Run the diagnostics on the validated detections
#' ls_diag_tovlp <- diagnostic_validations(
#'   df_validated = df_detecs_val_tovlp, pos_prob = 0.90, val_a_priori = TRUE
#' )
#'
#' # Check the number of templates in the diagnostics object
#' length(ls_diag_tovlp)
#'
#' # check the diagnostics plots of the first template
#' template_1 <- ls_diag_tovlp[[1]]
#' template_1$mod_plot /
#'   (template_1$roc_plot + template_1$precrec_plot + template_1$f1_plot) /
#'   (template_1$plot_dens)
#'
#' # Comparing diagnostics of all templates
#' template_2 <- ls_diag_tovlp[[2]]
#' template_3 <- ls_diag_tovlp[[3]]
#' template_4 <- ls_diag_tovlp[[4]]
#' template_5 <- ls_diag_tovlp[[5]]
#' template_6 <- ls_diag_tovlp[[6]]
#'
#' (template_1$precrec_plot + template_2$precrec_plot + template_3$precrec_plot +
#'   template_4$precrec_plot + template_5$precrec_plot + template_6$precrec_plot)
#'
#' data(df_detecs_val_manual)
#' glimpse(df_detecs_val_manual)
#'
#' # Run the diagnostics on the validated detections
#' ls_diag_manual <- diagnostic_validations(
#'   df_validated = df_detecs_val_manual, pos_prob = 0.90, val_a_priori = FALSE
#' )
#'
#' # Check the number of templates in the diagnostics object
#' length(ls_diag_manual)
#'
#' # check the diagnostics plots of the first template
#' template_1 <- ls_diag_manual[[1]]
#' template_1$mod_plot /
#'   (template_1$roc_plot + template_1$precrec_plot + template_1$f1_plot) /
#'   (template_1$plot_dens)
#'
#' # Comparing the diagnostics of the same template under different validation
#' # methods
#' template_A <- ls_diag_tovlp[[1]]
#' template_B <- ls_diag_manual[[1]]
#'
#' (
#'   template_A$mod_plot + labs(title = "Time overlap validation") +
#'     template_B$mod_plot + labs(title = "Manual validation")
#' ) /
#'   (template_A$precrec_plot + template_B$precrec_plot)
#'
#' }
diagnostic_validations <- function(
  df_validated,
  pos_prob = 0.95,
  val_a_priori = TRUE
) {
  split_validations <- df_validated |>
    dplyr::group_by(template_name) |>
    dplyr::group_split()

  res <- lapply(
    split_validations,
    function(x) {
      tryCatch(
        {
          suppressWarnings(
            diagnostic_validations_i(
              val_i = x,
              diag_method = "auto",
              pos_prob = pos_prob,
              val_a_priori = val_a_priori
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
