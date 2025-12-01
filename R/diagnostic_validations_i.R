#' Compute detection diagnostics of validated detections from one template
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function performs diagnostic validations on the detections. The
#'   diagnostic validations are used to determine the optimal cutpoint to be
#'   used in the detections.
#'
#' @param val_i A tibble containing the validation results of the detections.
#'   This function takes data from a single template only, otherwise it will
#'   return an error.
#' @param diag_method A character string indicating the method to use for
#'   diagnostic validations. The two methods available are: "auto" (default) or
#'   "manual". If "auto" is selected, the function will automatically determine
#'   the cutpoint to be used in the diagnostic validations. If "manual" is
#'   selected, the user must explicitly pass the cutpoint to be used in the
#'   diagnostic validations to the `diag_cut` argument.
#' @param pos_prob A numeric value indicating the probability of a positive
#'   test, if `diag_method` is set to "auto", otherwise it is ignored. It
#'   defaults to 0.95.
#' @param diag_cut A numeric value indicating the cutpoint to be used in the
#'   diagnostic, if `diag_method` is set to "manual", otherwise it is ignored.
#'   It defaults to NULL.
#' @param val_a_priori A logical value indicating whether the validation was
#'   performed with a priori method (with the `validate_by_overlap()` function),
#'   which defaults to TRUE. If FALSE, it will be assumed that the validation
#'   was performed with the a posteriori method (with the validation app).
#'
#' @import dplyr ggplot2
#' @return A list containing the diagnostic results
#' @export
diagnostic_validations_i <- function(
  val_i,
  diag_method = "auto",
  pos_prob = 0.95,
  diag_cut = NULL,
  val_a_priori = TRUE
) {
  # check if there is only one template name in the data
  if (length(unique(val_i$template_name)) > 1) {
    stop(
      paste0(
        "The data contains detections from more than one template or class. ",
        " Use the 'diagnostic_validations()' function to perform diagnostic ",
        "validations on multiple templates."
      )
    )
  }

  if (diag_method == "auto" & is.null(pos_prob)) {
    stop(
      "Error: An explicit value for 'pos_prob' must be set when the",
      "diagnostics method is set to 'auto'."
    )
  } else if (diag_method == "auto" & !is.null(pos_prob)) {
    if (pos_prob < 0 | pos_prob > 1) {
      stop("'pos_prob' must be a numeric value between 0 and 1")
    }
  }

  if (diag_method == "manual" & is.null(diag_cut)) {
    stop(
      "Error: An explicit value for 'diag_cut' must be set when the",
      "diagnostics method is set to 'manual'."
    )
  } else if (diag_method == "manual" & !is.null(diag_cut)) {
    if (diag_cut < 0 | diag_cut > 1) {
      stop("'diag_cut' must be a numeric value between 0 and 1")
    }
    # The cutpoint is already defined
    score_cut <- diag_cut
  }

  fun_auc <- function(x, y) {
    res <- -sum(
      (rowMeans(cbind(y[-length(y)], y[-1]))) * (x[-1] - x[-length(x)])
    )
    return(res)
  }

  val_i$validation_bin <- ifelse(val_i$validation == "TP", 1, 0)
  val_i$is_fp <- val_i$validation == "FP"
  val_i$is_tp <- val_i$validation == "TP"
  n_fn <- length(which(val_i$validation == "FN"))
  n_tp <- length(which(val_i$validation == "TP"))
  n_fp <- length(which(val_i$validation == "FP"))
  df_diag_input <- val_i[which(val_i$validation %in% c("TP", "FP")), ]

  bin_mod <- glm(
    validation_bin ~ peak_score,
    family = "binomial",
    data = df_diag_input
  )

  # For the "auto" method, the cutpoint is based is bin_mod predictions
  if (diag_method == "auto") {
    df_pred <- data.frame(peak_score = seq(0.01, 0.99, 0.01))
    df_pred$prob <- predict(bin_mod, newdata = df_pred, type = "response")
    score_cut <- df_pred$peak_score[min(which(df_pred$prob >= pos_prob))]
  }

  if (n_fp >= 4 & n_tp >= 4) {
    cutpointr_raw <- cutpointr::cutpointr(
      df_diag_input,
      peak_score,
      validation,
      cutpoint = score_cut,
      silent = TRUE,
      pos_class = "TP",
      neg_class = "FP",
      direction = ">=",
      method = cutpointr::oc_manual,
      use_midpoints = FALSE
    )

    diag_out <- cutpointr_raw$roc_curve[[1]][-1, ] |>
      dplyr::rename(peak_score = x.sorted) |>
      dplyr::mutate(fn = fn + n_fn) |>
      dplyr::select(peak_score, tp, fp, tn, fn) |>
      dplyr::mutate(
        template_name = unique(df_diag_input$template_name),
        precision = tp / (tp + fp),
        recall = tp / (tp + fn),
        sensitivity = tp / (tp + fn),
        specificity = tn / (tn + fp),
        F1_score = 2 * (precision * recall) / (precision + recall),
        selected = FALSE
      ) |>
      dplyr::relocate(contains("template"), everything()) |>
      as.data.frame()
  } else {
    thresholds <- seq(0, 1, 0.01)
    thresholds <- thresholds[thresholds < max(val_i$peak_score)]
    diag_list <- lapply(thresholds, function(threshold) {
      val_i |>
        dplyr::mutate(
          new_validation = dplyr::case_when(
            is_tp & peak_score >= threshold ~ "TP",
            is_tp & peak_score < threshold ~ "FN",
            is_fp & peak_score >= threshold ~ "FP",
            is_fp & peak_score < threshold ~ "TN"
          )
        ) |>
        dplyr::summarise(
          template_name = unique(df_diag_input$template_name),
          peak_score = threshold,
          tp = sum(new_validation == "TP", na.rm = TRUE),
          fp = sum(new_validation == "FP", na.rm = TRUE),
          tn = sum(new_validation == "TN", na.rm = TRUE),
          fn = sum(new_validation == "FN", na.rm = TRUE) + n_fn
        )
    })

    diag_out_raw <- dplyr::bind_rows(diag_list) |>
      dplyr::mutate(
        precision = tp / (tp + fp),
        recall = tp / (tp + fn),
        sensitivity = recall,
        specificity = tn / (tn + fp),
        F1_score = 2 * (precision * recall) / (precision + recall),
        selected = FALSE
      )

    diag_out <- diag_out_raw |>
      dplyr::mutate(
        dplyr::across(
          c(precision, recall, sensitivity, specificity),
          ~ ifelse(is.nan(.), max(., na.rm = TRUE), .)
        )
      ) |>
      dplyr::distinct()
  }

  diag_out$selected[max(which(diag_out$peak_score >= score_cut))] <- TRUE

  if (length(which(diag_out$peak_score >= score_cut)) >= 1) {
    sel_i <- diag_out |>
      dplyr::mutate(ID = 1:nrow(diag_out)) |>
      dplyr::filter(
        precision >= diag_out$precision[which(diag_out$selected == TRUE)]
      ) |>
      dplyr::slice_max(recall) |>
      dplyr::pull(ID)
    diag_out$selected <- FALSE
    diag_out$selected[sel_i] <- TRUE
  } else {
    sel_i <- which(diag_out$selected == TRUE)
  }

  mod_plot <- ggplot(df_diag_input, aes(x = peak_score, y = validation_bin)) +
    geom_point(pch = 1) +
    stat_smooth(
      formula = y ~ x,
      method = "glm",
      method.args = list(family = "binomial"),
      se = TRUE,
      fullrange = T,
      na.rm = TRUE
    ) +
    geom_vline(xintercept = score_cut, color = "red", linetype = 2) +
    labs(
      title = "Binomial regression",
      y = "Probability of validations as TP",
      x = "Peak score"
    ) +
    theme_bw()

  plot_dens <- df_diag_input |>
    rbind(mutate(df_diag_input, validation = "All")) |>
    ggplot() +
    geom_density(
      aes(x = peak_score, fill = validation, linetype = validation),
      alpha = 0.5
    ) +
    scale_fill_manual(
      values = c(
        "All" = "black",
        "TP" = "green",
        "FP" = "red"
      )
    ) +
    scale_linetype_manual(values = c("All" = 2, "TP" = 1, "FP" = 1)) +
    geom_vline(xintercept = score_cut, color = "red", linetype = 2) +
    labs(
      title = "Peak score density",
      y = "Density",
      x = "Peak score",
      fill = "",
      linetype = ""
    ) +
    theme_bw() +
    theme(
      legend.position.inside = c(0.85, 0.85),
      legend.background = element_blank(),
      legend.box.background = element_blank()
    )

  precrec_plot <- diag_out |>
    ggplot(aes(recall, precision)) +
    geom_line() +
    geom_point(data = diag_out[sel_i, ], aes(recall, precision)) +
    geom_vline(
      xintercept = diag_out[sel_i, ]$recall,
      color = "red",
      linetype = 2
    ) +
    annotate(
      "label",
      x = 0.75,
      y = 0.25,
      label = paste0(
        "prAUC = ",
        round(
          -fun_auc(diag_out$recall, diag_out$precision),
          3
        )
      )
    ) +
    labs(title = "Precision and Recall", x = "Recall", y = "Precision") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_bw()

  f1_plot <- diag_out |>
    ggplot(aes(peak_score, F1_score)) +
    geom_line() +
    geom_point(
      data = diag_out[sel_i, ],
      aes(peak_score, F1_score)
    ) +
    geom_vline(
      xintercept = diag_out[sel_i, ]$peak_score,
      color = "red",
      linetype = 2
    ) +
    labs(
      title = "F1 score",
      x = "Peak score",
      y = "F1 score"
    ) +
    theme_bw()

  if (val_a_priori) {
    roc_plot <- diag_out |>
      ggplot(aes(x = c(1 - specificity), y = sensitivity)) +
      geom_line() +
      geom_segment(
        aes(x = 0, y = 0, xend = 1, yend = 1),
        linetype = "dashed",
        color = "grey40"
      ) +
      geom_point(
        data = diag_out[sel_i, ],
        aes(x = c(1 - specificity), y = sensitivity)
      ) +
      labs(
        title = "ROC Curve",
        x = "False Positive Rate (1 - Specificity)",
        y = "True Positive Rate (Sensitivity)"
      ) +
      annotate(
        "label",
        x = 0.75,
        y = 0.25,
        label = paste0(
          "AUC = ",
          round(-fun_auc(1 - diag_out$specificity, diag_out$sensitivity), 3)
        )
      ) +
      theme_bw()
  } else {
    roc_plot <- ggplot2::ggplot() +
      ggplot2::annotate(
        "label",
        x = 1,
        y = 1,
        label = "ROC plot not available"
      ) +
      ggplot2::theme_void()
  }

  res <- list(
    diagnostics = diag_out,
    score_cut = score_cut,
    bin_mod = bin_mod,
    mod_plot = mod_plot,
    roc_plot = roc_plot,
    precrec_plot = precrec_plot,
    f1_plot = f1_plot,
    plot_dens = plot_dens
  )
}
