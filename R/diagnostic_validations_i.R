#' Diagnose detection quality for a single template (internal)
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Internal engine: [diagnostic_validations()] is the public interface of
#'   this step. Computes the performance diagnostics for the validated detections
#'   obtained from a single template. It relates each detection's matching
#'   score to whether it was a true or false positive, derives a score
#'   threshold, and returns a precision / recall / F1 table together with five
#'   diagnostic plots. This is the single-template engine behind
#'   [diagnostic_validations()], and reads the output of
#'   [validate_by_overlap()].
#'
#' @details
#'   Each validated detection carries a matching score. The function fits a
#'   binomial model (a logistic GLM) that estimates, for every score value, the
#'   probability that a detection with that score is a true positive. From the
#'   model a score threshold is chosen: in `"auto"` mode the threshold is the
#'   lowest score whose modelled probability of being a true positive reaches
#'   `pos_prob`; in `"manual"` mode you supply `diag_cut` directly. The
#'   diagnostics table is then built by testing every observed score as a
#'   threshold and counting the resulting true/false positives and negatives,
#'   giving the exact data-driven precision / recall / specificity / F1 curve.
#'
#'   Two metrics carry most of the meaning. **Precision** answers: of the
#'   detections kept, how many were real? **Recall** answers: of the real
#'   signals present, how many were found? They are the main metrics to
#'   maximize in template matching over long passive-acoustic recordings: low
#'   precision means hours spent discarding false detections by hand, and low
#'   recall means target signals pass unnoticed. The two usually trade off
#'   through the score threshold: raising it favors precision, lowering it
#'   favors recall.
#'
#'   Use this to inspect one template in detail: where its score threshold
#'   should sit, and how precision trades off against recall. For a whole run
#'   (many templates at once) use [diagnostic_validations()], which calls this
#'   for each template. Multiclass imports such as BirdNET (via
#'   [import_birdnet_detections()]) are read the same way: each class works as
#'   one "template", evaluated on its own.
#'
#'   Things to know: the automatic threshold is only as reliable as the model
#'   fit behind it. Sometimes the model cannot learn from the data: all
#'   detections fall in one class (only TPs or only FPs), there are too few
#'   detections, the estimation does not converge, or some score separates TP
#'   from FP perfectly. In these cases a warning is raised and you should
#'   switch to `diag_method = "manual"` with an explicit `diag_cut`. False
#'   negatives are counted per template (they enter the table as
#'   recall-affecting rows with no score), so pass this function a single
#'   template's data only. More than one `template_name` is an error.
#'
#' @section Pipeline context:
#'   Step 12 of the monitoraSom analysis flow (metrics), single-template engine.
#'   Reads one template's validated detections from [validate_by_overlap()]
#'   (step 11). Produces a metrics table, a score threshold, and diagnostic
#'   plots. See also \code{\link{validate_by_overlap}},
#'   \code{\link{diagnostic_validations}}, \code{\link{plot_scores}}.
#'
#' @param val_i The validated detections of one template (required). Accepts
#'   the [validate_by_overlap()] split list
#'   (`list(detections_validated =, false_negatives =)`, the routine input) or a
#'   single-template wide validation data.frame with a `validation` column
#'   (back-compatible). Must contain exactly one `template_name`.
#' @param diag_method `"auto"` (default) picks the threshold from the model
#'   where the true-positive probability crosses `pos_prob`; `"manual"` uses
#'   the explicit `diag_cut` you supply. Switch to `"manual"` when the
#'   automatic fit is flagged as unreliable.
#' @param pos_prob Numeric in `[0, 1]` (default `0.95`). Target true-positive
#'   probability for the `"auto"` threshold; higher is stricter. Used only
#'   when `diag_method = "auto"`.
#' @param diag_cut Numeric in `[0, 1]` (default `NULL`). Explicit score
#'   threshold; required when `diag_method = "manual"`, ignored otherwise.
#' @param build_plots Logical (default `TRUE`). `TRUE` builds the five plots;
#'   set `FALSE` to skip the plot construction, which takes most of the run
#'   time, and leave the plot slots `NULL` when you only need `diagnostics` /
#'   `score_cut` / `bin_mod`.
#' @param theme_mode `"light"` (default) or `"dark"`, the shared plot theme,
#'   matching [plot_scores()]. Ignored when `build_plots = FALSE`.
#'
#' @return A named list: `diagnostics` (the precision/recall/specificity/F1
#'   table, one row per distinct score threshold, with the selected row flagged),
#'   `score_cut` (the chosen threshold), `bin_mod` (the fitted binomial GLM), and
#'   the five `ggplot` objects `mod_plot`, `roc_plot`, `precrec_plot`, `f1_plot`,
#'   `plot_dens` (each `NULL` when `build_plots = FALSE`).
#'
#' @seealso [diagnostic_validations()] (the multi-template wrapper),
#'   [validate_by_overlap()] (upstream, step 11), [plot_scores()] (step 13).
#' @import dplyr ggplot2
#' @keywords internal
#' @examples
#' \dontrun{
#' # Step 12: diagnose ONE template's validated detections (from step 11).
#' # Load the package
#' library(monitoraSom)
#' data(df_detecs_val_tovlp)
#' one <- df_detecs_val_tovlp[df_detecs_val_tovlp$template_name ==
#'                              df_detecs_val_tovlp$template_name[1], ]
#' res <- diagnostic_validations_i(one)
#' res$score_cut                      # the automatic score threshold
#' head(res$diagnostics)              # precision / recall / F1 table
#'
#' # Set the threshold by hand instead of letting the model choose it:
#' res_manual <- diagnostic_validations_i(one, diag_method = "manual",
#'                                        diag_cut = 0.5)
#' }
diagnostic_validations_i <- function(val_i,
                                     diag_method = c("auto", "manual"),
                                     pos_prob = 0.95,
                                     diag_cut = NULL,
                                     build_plots = TRUE,
                                     theme_mode = "light") {
  diag_method <- match.arg(diag_method)                                 # DVI-05
  val_i <- .dvi_normalize_input(val_i)                                  # DVI-01

  # single-template guard (kept) ------------------------------------------------
  if (length(unique(val_i$template_name)) > 1) {
    stop("The data contains detections from more than one template or class. ",
         "Use 'diagnostic_validations()' for multiple templates.")
  }

  # argument range checks (kept) ------------------------------------------------
  if (diag_method == "auto") {
    if (is.null(pos_prob)) {
      stop("An explicit 'pos_prob' must be set when diag_method = 'auto'.")
    }
    if (pos_prob < 0 || pos_prob > 1) {
      stop("'pos_prob' must be a numeric value between 0 and 1")
    }
  } else { # manual
    if (is.null(diag_cut)) {
      stop("An explicit 'diag_cut' must be set when diag_method = 'manual'.")
    }
    if (diag_cut < 0 || diag_cut > 1) {
      stop("'diag_cut' must be a numeric value between 0 and 1")
    }
    score_cut <- diag_cut
  }

  val_i$validation_bin <- ifelse(val_i$validation == "TP", 1, 0)
  n_fn <- sum(val_i$validation == "FN")
  # DEC-15 (STEP-15): the validation origin is read from the data itself. When
  # false negatives are registered for this template/class, recall and the ROC
  # curve are computable (a-priori validation); without them, recall is
  # degenerate (no FN to catch) and the ROC is not drawn.
  has_fn <- n_fn > 0L
  if (!has_fn) {
    warning("diagnostic_validations_i: no false negatives registered for this ",
            "template or class. Recall and the ROC curve are not computable ",
            "from the current validation data; run an a-priori validation ",
            "(validate_by_overlap against ground-truth ROIs) to obtain them. ",
            "The remaining diagnostics (precision, F1, threshold) still apply.",
            call. = FALSE)
  }
  df_diag_input <- val_i[val_i$validation %in% c("TP", "FP"), ]

  bin_mod <- stats::glm(validation_bin ~ peak_score, family = "binomial",
                        data = df_diag_input)

  # "auto" cutpoint from the GLM (DVI-04: clamp instead of NA when nothing
  # crosses pos_prob -- unreachable on the goldens, where a crossing exists).
  if (diag_method == "auto") {
    # AFL-11: the GLM drives the auto score_cut, so a degenerate fit (single
    # outcome class, tiny n, non-convergence, perfect separation) silently
    # yields an unreliable cutpoint. Surface it with a clear, actionable warning
    # (the native glm.fit message is cryptic) instead of "looking" successful.
    degenerate <- .dvi_glm_degeneracy(df_diag_input, bin_mod)
    if (!is.na(degenerate)) {
      warning("diagnostic_validations_i: degenerate GLM fit -- ", degenerate,
              ". The 'auto' score_cut is unreliable; consider ",
              "diag_method = \"manual\" with an explicit diag_cut.",
              call. = FALSE)
    }
    df_pred <- data.frame(peak_score = seq(.DVI_PRED_LO, .DVI_PRED_HI,
                                           .DVI_PRED_BY))
    df_pred$prob <- stats::predict(bin_mod, newdata = df_pred,
                                   type = "response")
    hit <- which(df_pred$prob >= pos_prob)
    score_cut <- if (length(hit) >= 1L) {
      df_pred$peak_score[min(hit)]
    } else {
      warning("No detection reaches P(TP) >= pos_prob = ", pos_prob,
              "; clamping score_cut to the grid maximum.", call. = FALSE)
      max(df_pred$peak_score)
    }
  }

  diag_out <- .dvi_diagnostics_table(df_diag_input, n_fn)              # DVI-06

  # selected cutpoint row. Faithful to the original: pick the highest-precision-
  # then-max-recall row at/above score_cut; if score_cut exceeds every score the
  # original selected nothing (its `max(which())` returned -Inf and no-oped) -- we
  # preserve that outcome but drop the spurious -Inf warning (DVI-02).
  diag_out$selected <- FALSE
  ge <- which(diag_out$peak_score >= score_cut)
  if (length(ge) >= 1L) {
    diag_out$selected[max(ge)] <- TRUE
    sel_i <- diag_out |>
      dplyr::mutate(ID = seq_len(nrow(diag_out))) |>
      dplyr::filter(precision >= diag_out$precision[which(diag_out$selected)]) |>
      dplyr::slice_max(recall) |>
      dplyr::pull(ID)
    diag_out$selected <- FALSE
    diag_out$selected[sel_i] <- TRUE
  } else {
    sel_i <- integer(0)                          # original: nothing selected
  }

  # PERF-04: the five ggplots are the dominant cost (ggplot2-4.0 S7 object
  # construction). Skip them when the caller only needs the metrics table; the
  # plot slots then stay NULL (`NULL$x` is NULL). Default builds them (unchanged).
  plots <- if (build_plots)
    .dvi_plots(df_diag_input, diag_out, sel_i, score_cut, has_fn,
               theme_mode = theme_mode) else NULL

  list(
    diagnostics  = diag_out,
    score_cut    = score_cut,
    bin_mod      = bin_mod,
    mod_plot     = plots$mod_plot,
    roc_plot     = plots$roc_plot,
    precrec_plot = plots$precrec_plot,
    f1_plot      = plots$f1_plot,
    plot_dens    = plots$plot_dens
  )
}

# Constants (DVI-09: name the magic numbers) -----------------------------------
.DVI_PRED_LO <- 0.01
.DVI_PRED_HI <- 0.99
.DVI_PRED_BY <- 0.01
# AFL-11: below this many TP/FP detections the binomial fit is too small to trust
# for an automatic cutpoint (a rule-of-thumb floor, not a precise power bound).
.DVI_MIN_FIT_N <- 10L

# AFL-11: diagnose a degenerate binomial GLM, returning a human-readable reason
# string or NA_character_ when the fit is healthy. Covers the four ways the auto
# score_cut becomes meaningless: a single outcome class, too few detections,
# non-convergence, and perfect separation (fitted P(TP) collapsed to 0/1).
.dvi_glm_degeneracy <- function(df_diag_input, bin_mod) {
  n <- nrow(df_diag_input)
  n_classes <- length(unique(df_diag_input$validation_bin))
  if (n_classes < 2L) {
    return("only one outcome class (all TP or all FP)")
  }
  if (n < .DVI_MIN_FIT_N) {
    return(sprintf("too few detections (n = %d < %d)", n, .DVI_MIN_FIT_N))
  }
  if (!isTRUE(bin_mod$converged)) {
    return("the binomial GLM did not converge")
  }
  fitted <- bin_mod$fitted.values
  tol <- sqrt(.Machine$double.eps)
  if (length(fitted) > 0L &&
      all(fitted < tol | fitted > 1 - tol)) {
    return("perfect separation (fitted P(TP) is 0/1 for every detection)")
  }
  NA_character_
}

# Trapezoidal AUC (DVI-09: lifted from the inline `fun_auc`). Identical formula.
.dvi_auc <- function(x, y) {
  -sum((rowMeans(cbind(y[-length(y)], y[-1]))) * (x[-1] - x[-length(x)]))
}

# Input normalization (DVI-01): accept the VBO split list (recombined into one
# per-template frame, FN rows carrying peak_score = NA exactly as the legacy
# wide frame did) or a legacy single-template wide frame. Returns the full frame
# the engine expects (TP/FP/FN), so all downstream behaviour -- including the
# manual-sweep branch that keys on `max(val_i$peak_score)` -- is identical.
.dvi_normalize_input <- function(val_i) {
  if (is.list(val_i) && !is.data.frame(val_i) &&
      all(c("detections_validated", "false_negatives") %in% names(val_i))) {
    dv <- as.data.frame(val_i$detections_validated)
    fn <- val_i$false_negatives
    .dvi_require_cols(dv, "detections_validated")
    if (is.null(fn) || nrow(fn) == 0L) return(dv)
    return(dplyr::bind_rows(dv, as.data.frame(fn)))   # FN get peak_score = NA
  }
  if (is.data.frame(val_i)) {
    .dvi_require_cols(val_i, "val_i")
    return(val_i)
  }
  stop("`val_i` must be a `validate_by_overlap()` split list or a single-template ",
       "validation data.frame.")
}

# Required-column front door (DVI-10).
.dvi_require_cols <- function(df, what) {
  need <- c("validation", "peak_score", "template_name")
  miss <- setdiff(need, names(df))
  if (length(miss) > 0) {
    stop("`", what, "` is missing required column(s): ",
         paste(miss, collapse = ", "), ".")
  }
}

# Unified diagnostics table (DVI-06) -------------------------------------------
# One empirical sweep over the observed TP/FP scores replaces BOTH the original
# `cutpointr` branch and the `seq(0, 1, 0.01)` grid branch (and the `>= 4`-per-
# class gate that chose between them). For each observed score `t` a detection is
# positive iff `peak_score >= t`; `tp/fp/tn/fn` are read off cumulative counts
# over the descending-sorted scores, so each row is "all detections with score
# >= this threshold". This is exactly the table `cutpointr::roc(direction=">=")`
# built internally (minus its dropped `Inf` boundary row), with ties collapsed to
# one row per unique observed score -- the data-driven (exact) PR/ROC thresholds.
#
# `fn` adds the external per-template `n_fn` (ground-truth ROIs the template never
# detected). Undefined metrics use standard conventions (DVI-03): precision = 1
# when tp + fp = 0 (PR-curve terminal point), recall = 0 when tp + fn = 0,
# specificity = 1 when tn + fp = 0, F1 = 0 when precision + recall = 0. In this
# `>=` sweep every row has tp + fp >= 1, so only the F1 convention can fire (the
# top rows where the highest scores are all FP); the others are kept as safe
# defaults. Rows are returned in descending `peak_score` order (as the original
# `cutpointr` branch was), so the inline plots, prAUC and AUC are unchanged.
.dvi_diagnostics_table <- function(df_diag_input, n_fn) {
  scores   <- df_diag_input$peak_score
  is_tp    <- df_diag_input$validation == "TP"
  is_fp    <- df_diag_input$validation == "FP"
  n_tp_all <- sum(is_tp)
  n_fp_all <- sum(is_fp)

  ord    <- order(scores, decreasing = TRUE)
  s      <- scores[ord]
  cum_tp <- cumsum(is_tp[ord])
  cum_fp <- cumsum(is_fp[ord])
  keep   <- !duplicated(s, fromLast = TRUE)   # last of each tied group = full ">="

  tp <- cum_tp[keep]
  fp <- cum_fp[keep]
  data.frame(
    template_name = unique(df_diag_input$template_name),
    peak_score    = s[keep],
    tp = tp,
    fp = fp,
    tn = n_fp_all - fp,
    fn = (n_tp_all - tp) + n_fn
  ) |>
    dplyr::mutate(
      precision   = ifelse(tp + fp == 0, 1, tp / (tp + fp)),
      recall      = ifelse(tp + fn == 0, 0, tp / (tp + fn)),
      sensitivity = recall,
      specificity = ifelse(tn + fp == 0, 1, tn / (tn + fp)),
      F1_score    = ifelse(precision + recall == 0, 0,
                           2 * (precision * recall) / (precision + recall)),
      selected    = FALSE
    )
}

# the five diagnostic plots. FEAT-08: styled through the shared theme module
# (_plot_theme.R) -- one light/dark `.monitora_theme()`, the named TP/FP/All
# `.monitora_palette()`, and the common `.monitora_cutline()` -- replacing the
# ad-hoc `theme_bw()` + literal "green"/"red" of the original. Cosmetic only.
.dvi_plots <- function(df_diag_input, diag_out, sel_i, score_cut, has_fn,
                       theme_mode = "light") {
  pal <- .monitora_palette()

  mod_plot <- ggplot2::ggplot(df_diag_input,
                              ggplot2::aes(x = peak_score, y = validation_bin)) +
    ggplot2::geom_point(pch = 1) +
    ggplot2::stat_smooth(formula = y ~ x, method = "glm",
                         method.args = list(family = "binomial"),
                         se = TRUE, fullrange = TRUE, na.rm = TRUE) +
    .monitora_cutline(score_cut) +
    ggplot2::labs(title = "Binomial regression",
                  y = "Probability of validations as TP", x = "Peak score") +
    .monitora_theme(theme_mode)

  plot_dens <- df_diag_input |>
    rbind(dplyr::mutate(df_diag_input, validation = "All")) |>
    ggplot2::ggplot() +
    ggplot2::geom_density(
      ggplot2::aes(x = peak_score, fill = validation, linetype = validation),
      alpha = 0.5) +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::scale_linetype_manual(values = c("All" = 2, "TP" = 1, "FP" = 1)) +
    .monitora_cutline(score_cut) +
    ggplot2::labs(title = "Peak score density", y = "Density", x = "Peak score",
                  fill = "", linetype = "") +
    .monitora_theme(theme_mode) +
    ggplot2::theme(legend.position.inside = c(0.85, 0.85),
                   legend.box.background = ggplot2::element_blank())

  precrec_plot <- diag_out |>
    ggplot2::ggplot(ggplot2::aes(recall, precision)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = diag_out[sel_i, ],
                        ggplot2::aes(recall, precision)) +
    .monitora_cutline(diag_out[sel_i, ]$recall) +
    ggplot2::annotate("label", x = 0.75, y = 0.25,
                      label = paste0("prAUC = ",
                                     round(-.dvi_auc(diag_out$recall,
                                                     diag_out$precision), 3))) +
    ggplot2::labs(title = "Precision and Recall", x = "Recall", y = "Precision") +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    .monitora_theme(theme_mode)

  f1_plot <- diag_out |>
    ggplot2::ggplot(ggplot2::aes(peak_score, F1_score)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(data = diag_out[sel_i, ],
                        ggplot2::aes(peak_score, F1_score)) +
    .monitora_cutline(diag_out[sel_i, ]$peak_score) +
    ggplot2::labs(title = "F1 score", x = "Peak score", y = "F1 score") +
    .monitora_theme(theme_mode)

  if (has_fn) {
    roc_plot <- diag_out |>
      ggplot2::ggplot(ggplot2::aes(x = c(1 - specificity), y = sensitivity)) +
      ggplot2::geom_line() +
      ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = 1, yend = 1),
                            linetype = "dashed", color = "grey40") +
      ggplot2::geom_point(data = diag_out[sel_i, ],
                          ggplot2::aes(x = c(1 - specificity), y = sensitivity)) +
      ggplot2::labs(title = "ROC Curve",
                    x = "False Positive Rate (1 - Specificity)",
                    y = "True Positive Rate (Sensitivity)") +
      ggplot2::annotate("label", x = 0.75, y = 0.25,
                        label = paste0("AUC = ",
                                       round(-.dvi_auc(1 - diag_out$specificity,
                                                       diag_out$sensitivity),
                                             3))) +
      .monitora_theme(theme_mode)
  } else {
    roc_plot <- ggplot2::ggplot() +
      ggplot2::annotate("label", x = 1, y = 1,
                        label = "ROC not computable: no false negatives") +
      ggplot2::theme_void()
  }

  list(mod_plot = mod_plot, roc_plot = roc_plot, precrec_plot = precrec_plot,
       f1_plot = f1_plot, plot_dens = plot_dens)
}
