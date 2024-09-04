#' @title Diagnostic validation
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function takes in a data frame containing detections and validates them
#' using a binomial regression model. The function returns a data frame with the
#' validated detections.
#'
#' @param val_i A data frame containing detections as in the output of
#'  the validation functions.
#' @param diag_method A character string indicating the method to select the
#' peak score cut-off. The default is "Auto". The other option is "Manual".
#' @param pos_prob A numeric value between 0 and 1 indicating the probability
#' of a positive detection. The default is 0.95.
#' @param diag_cut A numeric value between 0 and 1 indicating the peak score
#' cut-off. The default is NULL.
#'
#' @return Validation results.
old_diagnostic_validations_i <- function(
    val_i, diag_method = "Auto", pos_prob = 0.95, diag_cut = NULL) {
    auc_trap <- function(x, y) {
        res <- sum(
            (rowMeans(cbind(y[-length(y)], y[-1]))) * (x[-1] - x[-length(x)])
        )
        return(res)
    }

    df_diag_input_raw <- val_i %>%
        mutate(validation_bin = ifelse(validation == "TP", 1, 0))

    n_fn <- length(which(df_diag_input_raw$validation == "FN"))
    n_tp <- length(which(df_diag_input_raw$validation == "TP"))
    n_fp <- length(which(df_diag_input_raw$validation == "FP"))
    df_diag_input <- filter(df_diag_input_raw, validation %in% c("TP", "FP"))

    bin_mod <- NULL
    cutpointr_raw <- NULL
    score_cut <- NULL

    bin_mod <- glm(
        validation_bin ~ peak_score,
        family = "binomial",
        data = df_diag_input
    )

    if (diag_method == "Manual") {
        if (diag_cut < 0 | diag_cut > 1) {
            stop("'diag_cut' must be a numeric value between 0 and 1")
        } else {
            (
                score_cut <- diag_cut
            )
        }
    } else if (diag_method == "Auto") {
        score_cut <- data.frame(peak_score = seq(0, 1, 0.001)) %>%
            mutate(prob = predict(bin_mod, newdata = ., type = "response")) %>%
            {
                .$peak_score[min(which(.$prob >= pos_prob))]
            }
    }

    cutpointr_raw <- cutpointr(
        df_diag_input, peak_score, validation,
        cutpoint = score_cut,
        silent = TRUE, pos_class = "TP", neg_class = "FP", direction = ">=",
        method = oc_manual, use_midpoints = FALSE
    )

    diag_out <- cutpointr_raw$roc_curve[[1]] %>%
        rename(peak_score = x.sorted) %>%
        mutate(fn = fn + n_fn) %>%
        select(peak_score, tp, fp, tn, fn) %>%
        mutate(
            template_name = unique(df_diag_input$template_name),
            precision = tp / (tp + fp),
            recall = tp / (tp + fn),
            sensitivity = tp / (tp + fn),
            specificity = tn / (tn + fp),
            F1_score = 2 * (precision * recall) / (precision + recall),
            selected = FALSE
        ) %>%
        relocate(contains("template"), everything()) %>%
        as.data.frame()

    diag_out$selected[max(which(diag_out$peak_score > score_cut))] <- TRUE
    sel_i <- which(diag_out$selected == TRUE)

    mod_plot <- ggplot(df_diag_input, aes(x = peak_score, y = validation_bin)) +
        geom_point(pch = 1) +
        stat_smooth(
            formula = y ~ x, method = "glm",
            method.args = list(family = "binomial"), se = TRUE,
            fullrange = T, na.rm = TRUE
        ) +
        geom_vline(xintercept = score_cut, color = "red", linetype = 2) +
        labs(
            title = "Binomial regression",
            y = "Probability of validations as TP", x = "Correlation"
        ) +
        theme_bw()

    roc_plot <- cutpointr::plot_roc(cutpointr_raw) +
        geom_segment(
            aes(x = 0, y = 0, xend = 1, yend = 1),
            linetype = "dashed", color = "grey40"
        ) +
        annotate(
            "label", x = 0.75, y = 0.25,
            label = paste0(
                "AUC = ",
                round(cutpointr::auc(cutpointr_raw$roc_curve[[1]]), 3)
            )
        ) +
        ylim(0, 1) + xlim(0, 1) +
        theme_bw()

    precrec_data <- diag_out %>%
        select(peak_score, precision, recall, selected) %>%
        filter(is.finite(peak_score)) %>%
        tidyr::drop_na()

    precrec_plot <- precrec_data %>%
        ggplot(aes(recall, precision)) +
        geom_line() +
        geom_point(
            data = precrec_data[sel_i, ], aes(recall, precision)
        ) +
        annotate(
            "label",
            x = 0.75, y = 0.25,
            label = paste0(
                "prAUC = ",
                round(
                    auc_trap(precrec_data$recall, precrec_data$precision), 3
                )
            )
        ) +
        labs(
            title = "Precision and Relative Recall", x = "Relative Recall",
            y = "Precision"
        ) +
        ylim(0, 1) +
        xlim(0, 1) +
        # coord_equal() +
        theme_bw()

    f1_plot <- diag_out %>%
        ggplot(aes(peak_score, F1_score)) +
        geom_line() +
        geom_point(
            data = diag_out[sel_i, ], aes(peak_score, F1_score)
        ) +
        geom_vline(xintercept = score_cut, color = "red", linetype = 2) +
        labs(
            title = "F1 score", x = "Peak score", y = "F1 score"
        ) +
        theme_bw()

    plot_dens <- df_diag_input %>%
        rbind(mutate(df_diag_input, validation = "All detections")) %>%
        filter(!is.na(validation)) %>%
        ggplot() +
        geom_density(
            aes(x = peak_score, fill = validation, linetype = validation),
            alpha = 0.5
        ) +
        scale_fill_manual(
            values = c(
                "All detections" = "black", "TP" = "green", "FP" = "red"
            )
        ) +
        scale_linetype_manual(values = c("All detections" = 2, "TP" = 1, "FP" = 1)) +
        {
            if (!is.null(score_cut)) {
                geom_vline(xintercept = score_cut, color = "red", linetype = 2)
            }
        } +
        labs(
            title = "Peak score density", y = "Density", x = "Peak score",
            fill = "", linetype = ""
        ) +
        theme_bw() +
        theme(
            legend.position.inside = c(0.9, 0.9),
            legend.background = element_blank(),
            legend.box.background = element_blank()
        )

    res <- list(
        diagnostics = diag_out,
        bin_mod = bin_mod,
        mod_plot = mod_plot,
        roc_plot = roc_plot,
        precrec_plot = precrec_plot,
        f1_plot = f1_plot,
        plot_dens = plot_dens
    )
    return(res)
}