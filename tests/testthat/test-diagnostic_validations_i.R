# Tests for the refactored diagnostic_validations_i (per-template detection
# diagnostics, flow #14; DVI cycle, push plan-log-2026-06-11-002 §9 +
# plan-log-2026-06-15-001 for DVI-06).
#
# DVI-06 is a deliberate STATISTICAL redesign: the diagnostics table is one
# empirical sweep over the observed scores (a detection is positive iff
# `peak_score >= t`), `cutpointr` and the `>= 4`-per-class gate are dropped, and
# undefined metrics use standard 0/0 conventions instead of the old NaN->max
# imputation. So the refactored numbers DIVERGE from the original-behaviour
# witnesses (G1..G5, still frozen under tests/golden/.../). This suite therefore
# asserts (a) a regression lock against the REDESIGNED expectations
# (tests/golden/.../redesigned/, from R/sandbox/diagnostic_validations_i_redesigned.R),
# (b) sanity properties of an exact PR/ROC sweep, (c) hand-verified points incl.
# the 0/0 conventions, and (d) the documented divergence from / agreement with
# the original witness. `cutpointr` is no longer a dependency.
#
# Run: Rscript -e "testthat::test_file('R/refactored/tests/testthat/test-diagnostic_validations_i.R')"

suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })

gold  <- test_path("golden/diagnostic_validations_i")
redes <- file.path(gold, "redesigned")
g_in_tovlp  <- readRDS(file.path(gold, "input_val_tovlp_1.rds"))
g_in_sweep  <- readRDS(file.path(gold, "input_val_sweep.rds"))
g_in_manual <- readRDS(file.path(gold, "input_val_manual_1.rds"))

.metric_cols <- c("precision", "recall", "sensitivity", "specificity", "F1_score")

# (a) regression lock against the redesigned expectations -----------------------

fun_auc <- function(x, y) {
  -sum((rowMeans(cbind(y[-length(y)], y[-1]))) * (x[-1] - x[-length(x)]))
}
expect_matches_redesigned <- function(res, tag) {
  g_diag <- readRDS(file.path(redes, paste0(tag, "_diagnostics.rds")))
  g_sum  <- readRDS(file.path(redes, paste0(tag, "_summary.rds")))
  expect_equal(res$diagnostics, g_diag, ignore_attr = TRUE, info = tag)
  expect_equal(res$score_cut, g_sum$score_cut, info = tag)
  d <- res$diagnostics
  expect_equal(-fun_auc(d$recall, d$precision), g_sum$prauc, info = tag)
  expect_equal(-fun_auc(1 - d$specificity, d$sensitivity), g_sum$auc, info = tag)
}

test_that("auto reproduces the redesigned expectations (R1)", {
  expect_matches_redesigned(
    suppressWarnings(diagnostic_validations_i(g_in_tovlp, "auto", pos_prob = 0.95)),
    "R1_auto")
})
test_that("manual cut reproduces the redesigned expectations (R2)", {
  expect_matches_redesigned(
    diagnostic_validations_i(g_in_tovlp, "manual", diag_cut = 0.30),
    "R2_manual_cut")
})
test_that("small-n input now sweeps the observed scores (R3)", {
  expect_matches_redesigned(
    suppressWarnings(diagnostic_validations_i(g_in_sweep, "auto", pos_prob = 0.95)),
    "R3_small_n")
})
test_that("no-FN input reproduces the redesigned expectations (R4, DEC-15)", {
  res <- suppressWarnings(
    diagnostic_validations_i(g_in_manual, "auto", pos_prob = 0.95))
  expect_matches_redesigned(res, "R4_aposteriori")
  expect_equal(length(res$roc_plot$layers), 1L)   # ROC placeholder (no FN)
})

# (a') FEAT-08: the shared theme threads through and the palette is applied ------

test_that("FEAT-08: theme_mode builds the five plots with the shared theme + palette", {
  res_light <- suppressWarnings(diagnostic_validations_i(g_in_tovlp, "auto"))
  res_dark  <- suppressWarnings(
    diagnostic_validations_i(g_in_tovlp, "auto", theme_mode = "dark"))
  for (p in c("mod_plot", "roc_plot", "precrec_plot", "f1_plot", "plot_dens")) {
    expect_s3_class(res_light[[p]], "ggplot")
    expect_s3_class(res_dark[[p]], "ggplot")
  }
  # Dark mode paints the panel background black (light keeps it white).
  expect_equal(res_dark$mod_plot$theme$panel.background$fill, "black")
  expect_equal(res_light$mod_plot$theme$panel.background$fill, "white")
  # The density fill scale uses the shared named TP/FP/All palette.
  pal <- .monitora_palette()
  built <- ggplot2::ggplot_build(res_light$plot_dens)
  expect_true(all(unique(built$data[[1]]$fill) %in% unname(pal)))
})

# (b) sanity properties of an exact PR/ROC sweep --------------------------------

test_that("the diagnostics table is a well-formed exact PR/ROC sweep", {
  for (case in list(g_in_tovlp, g_in_sweep, g_in_manual)) {
    v <- case
    res <- suppressWarnings(diagnostic_validations_i(
      v, "auto", pos_prob = 0.95))
    d <- res$diagnostics
    di <- v[v$validation %in% c("TP", "FP"), ]
    # one row per unique observed TP/FP score, descending
    expect_equal(nrow(d), length(unique(di$peak_score)))
    expect_true(all(diff(d$peak_score) <= 0))
    # all metrics finite and in [0, 1] (no NaN from the 0/0 conventions)
    for (m in .metric_cols) {
      expect_false(any(is.nan(d[[m]])), info = m)
      expect_true(all(d[[m]] >= -1e-9 & d[[m]] <= 1 + 1e-9), info = m)
    }
    # recall is monotone non-decreasing as the threshold falls (rows descend)
    expect_true(all(diff(d$recall) >= -1e-9))
    # sensitivity is an exact alias of recall
    expect_equal(d$sensitivity, d$recall)
    # counts add up: tp+tn+fp+fn constant = total detections + external FN
    n_fn <- sum(v$validation == "FN")
    expect_true(all((d$tp + d$tn + d$fp + d$fn) == nrow(di) + n_fn))
  }
})

# (c) hand-verified points incl. the 0/0 conventions ----------------------------

test_that("a hand-computed top-FP case triggers the F1 = 0 convention (public fn)", {
  # FP at 0.9 (highest), TP at 0.5; no external FN. Both classes -> GLM fits.
  v <- data.frame(
    template_name = "t",
    peak_score    = c(0.9, 0.5),
    validation    = c("FP", "TP"),
    stringsAsFactors = FALSE)
  d <- suppressWarnings(diagnostic_validations_i(v, "auto", pos_prob = 0.95))$diagnostics
  expect_equal(d$peak_score, c(0.9, 0.5))
  expect_equal(d$tp, c(0, 1)); expect_equal(d$fp, c(1, 1))
  expect_equal(d$tn, c(0, 0)); expect_equal(d$fn, c(1, 0))
  expect_equal(d$precision, c(0, 0.5))
  expect_equal(d$recall,    c(0, 1))
  expect_equal(d$specificity, c(0, 0))
  expect_equal(d$F1_score,  c(0, 2 * 0.5 * 1 / 1.5))   # top row: 0/0 -> 0
})

test_that("specificity = 1 convention fires when there are no FP (helper)", {
  df <- data.frame(template_name = "t", peak_score = c(0.5, 0.3),
                   validation = c("TP", "TP"), stringsAsFactors = FALSE)
  d <- .dvi_diagnostics_table(df, n_fn = 0)
  expect_equal(d$specificity, c(1, 1))            # tn + fp = 0 -> 1
  expect_equal(d$precision, c(1, 1))
  expect_equal(d$recall, c(0.5, 1))
  expect_equal(d$F1_score, c(2 * 1 * 0.5 / 1.5, 1))
})

test_that("recall = 0 and F1 = 0 conventions fire when there are no TP (helper)", {
  df <- data.frame(template_name = "t", peak_score = c(0.5, 0.3),
                   validation = c("FP", "FP"), stringsAsFactors = FALSE)
  d <- .dvi_diagnostics_table(df, n_fn = 0)
  expect_equal(d$recall, c(0, 0))                 # tp + fn = 0 -> 0
  expect_equal(d$F1_score, c(0, 0))               # precision + recall = 0 -> 0
  expect_equal(d$precision, c(0, 0))
  expect_equal(d$specificity, c(0.5, 0))
})

# (d) divergence from / agreement with the original witness (DVI-06 re-baseline)-

test_that("where no 0/0 occurs DVI-06 reproduces the original cutpointr table (G1==R1)", {
  g1 <- readRDS(file.path(gold, "G1_auto_cutpointr_diagnostics.rds"))
  r1 <- suppressWarnings(
    diagnostic_validations_i(g_in_tovlp, "auto", pos_prob = 0.95))$diagnostics
  # the a-priori template's top score is a TP, so tp >= 1 on every row and no
  # convention can fire -> the redesigned table equals the original verbatim.
  expect_equal(r1, g1, ignore_attr = TRUE)
})

test_that("the small-n input no longer degenerates to a single row (G3 divergence)", {
  g3 <- readRDS(file.path(gold, "G3_manual_sweep_diagnostics.rds"))
  expect_equal(nrow(g3), 1L)                       # original: degenerate
  r3 <- suppressWarnings(
    diagnostic_validations_i(g_in_sweep, "auto", pos_prob = 0.95))$diagnostics
  expect_gt(nrow(r3), 1L)                          # redesigned: full sweep
  expect_equal(nrow(r3), length(unique(
    g_in_sweep$peak_score[g_in_sweep$validation %in% c("TP", "FP")])))
})

# --- DVI-01: the VBO split list yields the SAME result as the legacy frame ----

test_that("the VBO split-list input matches the legacy wide-frame input", {
  legacy <- suppressWarnings(
    diagnostic_validations_i(g_in_tovlp, "auto", pos_prob = 0.95))
  dv <- g_in_tovlp[g_in_tovlp$validation %in% c("TP", "FP"), ]
  fn <- g_in_tovlp[g_in_tovlp$validation == "FN", ]
  fn$template_name <- g_in_tovlp$template_name[1]
  split <- suppressWarnings(diagnostic_validations_i(
    list(detections_validated = dv, false_negatives = fn),
    "auto", pos_prob = 0.95))
  expect_equal(split$diagnostics, legacy$diagnostics, ignore_attr = TRUE)
  expect_equal(split$score_cut, legacy$score_cut)
})

# --- guards (DVI-05 / DVI-10 / single-template) -------------------------------

test_that("diag_method is match.arg'd and out-of-range args error (DVI-05)", {
  expect_error(diagnostic_validations_i(g_in_tovlp, "Auto"), "should be one of")
  expect_error(diagnostic_validations_i(g_in_tovlp, "auto", pos_prob = 1.5),
               "between 0 and 1")
  expect_error(diagnostic_validations_i(g_in_tovlp, "manual"),
               "explicit 'diag_cut'")
})

test_that("missing required columns error at the front door (DVI-10)", {
  bad <- g_in_tovlp[, setdiff(names(g_in_tovlp), "peak_score")]
  expect_error(diagnostic_validations_i(bad, "auto"), "peak_score")
})

test_that("more than one template errors", {
  two <- rbind(g_in_tovlp, transform(g_in_tovlp, template_name = "other"))
  expect_error(diagnostic_validations_i(two, "auto"),
               "more than one template")
})

# (e) AFL-11: degenerate-GLM guard on the auto cutpoint -------------------------

test_that("AFL-11: a tiny-n auto fit warns about an unreliable score_cut", {
  tiny <- data.frame(
    template_name = "t",
    peak_score    = c(0.9, 0.8, 0.6, 0.4, 0.3, 0.1),
    validation    = c("TP", "TP", "FP", "TP", "FP", "FP"),
    stringsAsFactors = FALSE)
  expect_warning(diagnostic_validations_i(tiny, "auto", pos_prob = 0.95),
                 "degenerate GLM fit")
})

test_that("AFL-11: a single-outcome-class auto fit warns", {
  one_class <- data.frame(
    template_name = "t",
    peak_score    = seq(0.95, 0.40, length.out = 12),
    validation    = rep("TP", 12),
    stringsAsFactors = FALSE)
  expect_warning(diagnostic_validations_i(one_class, "auto", pos_prob = 0.95),
                 "only one outcome class")
})

test_that("AFL-11: a healthy auto fit emits no degeneracy warning", {
  w <- capture_warnings(
    diagnostic_validations_i(g_in_tovlp, "auto", pos_prob = 0.95))
  expect_false(any(grepl("degenerate GLM fit", w)))
})

test_that("AFL-11: manual mode does not trigger the auto degeneracy warning", {
  tiny <- data.frame(
    template_name = "t",
    peak_score    = c(0.9, 0.5, 0.3),
    validation    = c("TP", "FP", "TP"),
    stringsAsFactors = FALSE)
  w <- capture_warnings(diagnostic_validations_i(tiny, "manual", diag_cut = 0.5))
  expect_false(any(grepl("degenerate GLM fit", w)))
})
