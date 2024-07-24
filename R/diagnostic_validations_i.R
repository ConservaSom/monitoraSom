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
#'
#' @return
#' @export
#'
diagnostic_validations_i <- function(val_i) {
    auc_trap <- function(x, y) {
        res <- sum(
            (rowMeans(cbind(y[-length(y)], y[-1]))) *
                (x[-1] - x[-length(x)])
        )
        return(res)
    }

    df_diag_input_raw <- val_i %>%
        mutate(validation_bin = ifelse(validation == "TP", 1, 0))

    n_fn <- length(which(df_diag_input_raw$validation == "FN"))

    df_diag_input <- df_diag_input_raw %>%
        filter(validation %in% c("TP", "FP"))

    bin_mod <- glm(
        validation_bin ~ peak_score,
        family = "binomial", data = df_diag_input
    )

    # diag_method <- "Manual"
    # diag_cut <- 0.5
    diag_method <- "Auto"
    pos_prob <- 0.95

    if (diag_method == "Manual") {
        binom_cut <- diag_cut
    } else if (diag_method == "Auto") {
        binom_cut <- data.frame(peak_score = seq(0, 1, 0.001)) %>%
            mutate(prob = predict(bin_mod, newdata = ., type = "response")) %>%
            {
                .$peak_score[min(which(.$prob >= pos_prob))]
            }
    }

    mod_plot <- ggplot(
        df_diag_input, aes(x = peak_score, y = validation_bin)
    ) +
        geom_point() +
        stat_smooth(
            formula = y ~ x, method = "glm",
            method.args = list(family = "binomial"),
            se = TRUE, fullrange = T, na.rm = TRUE
        ) +
        geom_vline(
            xintercept = binom_cut, color = "red", linetype = 2, linewidth = 1
        ) +
        ylim(0, 1) +
        xlim(0, 1) +
        labs(
            title = "Binomial regression",
            y = "Probability of validations as TP", x = "Correlation"
        ) +
        coord_equal() +
        theme_bw()

    cutpointr_raw <- cutpointr(
        df_diag_input, peak_score, validation,
        cutpoint = binom_cut, silent = TRUE,
        pos_class = "TP", neg_class = "FP", direction = ">=",
        method = oc_manual, use_midpoints = FALSE
    )

    diag_out <- cutpointr_raw$roc_curve[[1]] %>%
        rename(peak_score = x.sorted) %>%
        mutate(
            fn = fn + n_fn
        ) %>%
        select(peak_score, tp, fp, tn, fn) %>%
        mutate(
            template_name = unique(df_diag_input$template_name),
            precision = tp / (tp + fp),
            relative_recall = tp / (tp + fn),
            sensitivity = tp / (tp + fn),
            specificity = tn / (tn + fp),
            F1_score = 2 * (precision * relative_recall) / (precision + relative_recall),
            selected = FALSE
            # selected = ifelse(peak_score >= diag_cut, TRUE, FALSE)
        ) %>%
        relocate(contains("template"), everything()) %>%
        as.data.frame()

    diag_out$selected[max(which(diag_out$peak_score > binom_cut))] <- TRUE
    sel_i <- which(diag_out$selected == TRUE)

    roc_plot <- cutpointr::plot_roc(cutpointr_raw) +
        geom_segment(
            aes(x = 0, y = 0, xend = 1, yend = 1),
            linetype = "dashed", color = "grey40"
        ) +
        annotate(
            "label",
            x = 0.75, y = 0.25,
            label = paste0(
                "AUC = ", round(cutpointr::auc(cutpointr_raw$roc_curve[[1]]), 3)
            )
        ) +
        ylim(0, 1) + xlim(0, 1) +
        coord_equal() +
        theme_bw()

    precrec_data <- diag_out %>%
        select(peak_score, precision, relative_recall, selected) %>%
        filter(is.finite(peak_score)) %>%
        tidyr::drop_na()

    precrec_plot <- precrec_data %>%
        ggplot(aes(relative_recall, precision)) +
        geom_line() +
        geom_point(
            data = precrec_data[sel_i, ], aes(relative_recall, precision)
        ) +
        annotate(
            "label",
            x = 0.75, y = 0.25,
            label = paste0(
                "prAUC = ",
                round(
                    auc_trap(precrec_data$relative_recall, precrec_data$precision), 3
                )
            )
        ) +
        labs(
            title = "Precision and Relative Recall", x = "Relative Recall",
            y = "Precision"
        ) +
        ylim(0, 1) +
        xlim(0, 1) +
        coord_equal() +
        theme_bw()

    f1_plot <- diag_out %>%
        ggplot(aes(peak_score, F1_score)) +
        geom_line() +
        geom_point(
            data = diag_out[sel_i, ], aes(peak_score, F1_score)
        ) +
        geom_vline(
            xintercept = binom_cut, color = "red", linetype = 2,
            linewidth = 1
        ) +
        labs(
            title = "F1 score", x = "Peak score", y = "F1 score"
        ) +
        theme_bw()

    plot_dens <- df_diag_input %>%
        filter(!is.na(validation)) %>%
        ggplot(aes(x = peak_score, fill = validation)) +
        geom_density(alpha = 0.5) +
        geom_vline(
            xintercept = binom_cut, color = "red", linetype = 2,
            linewidth = 1
        ) +
        labs(
            title = "Peak score density",
            y = "Density", x = "Peak score", fill = ""
        ) +
        theme_bw() +
        theme(
            legend.position = c(0.9, 0.9),
            legend.background = element_blank(),
            legend.box.background = element_blank()
        )

    res <- list(
        diagnostics = diag_out,
        mod_plot = mod_plot,
        roc_plot = roc_plot,
        precrec_plot = precrec_plot,
        f1_plot = f1_plot,
        plot_dens = plot_dens
    )
    return(res)
}