# Tests for plot_scores_overview (AFL-28: batch template x soundscape heatmap).
# CRAN-lean subset: only the fully-synthetic plot_scores_overview blocks are
# ported to the mirror. The plot_scores() blocks (upstream test-plot_scores.R,
# lines 92-196) read data/ls_soundscapes.rda (25 MB, evicted to Zenodo per
# CRAN-30/31) and stay monorepo-only.

suppressPackageStartupMessages({
  library(ggplot2)
})

# Minimal detections fixture: template A fires twice on s1 and once on s2;
# template B fires once on s1 and never on s2 (the silent-pair zero cell).
overview_detecs <- function(label_col = TRUE) {
  d <- data.frame(
    soundscape_file = c("s1.wav", "s1.wav", "s2.wav", "s1.wav"),
    template_name   = c("tplA", "tplA", "tplA", "tplB"),
    peak_score      = c(0.9, 0.8, 0.7, 0.6),
    stringsAsFactors = FALSE)
  if (label_col) d$template_label <- c("ave_A", "ave_A", "ave_A", "ave_B")
  d
}

test_that("plot_scores_overview returns a ggplot heatmap with a completed grid", {
  p <- plot_scores_overview(overview_detecs())
  expect_s3_class(p, "ggplot")
  # 2 templates x 2 soundscapes = 4 cells, the absent (B, s2) pair completed to 0
  expect_setequal(names(p$data), c("template", "soundscape", "n"))
  expect_equal(nrow(p$data), 4L)
  cell <- function(t, s) p$data$n[p$data$template == t & p$data$soundscape == s]
  expect_equal(cell("ave_A", "s1.wav"), 2L)
  expect_equal(cell("ave_A", "s2.wav"), 1L)
  expect_equal(cell("ave_B", "s1.wav"), 1L)
  expect_equal(cell("ave_B", "s2.wav"), 0L)   # silent pair is a visible zero
})

test_that("plot_scores_overview defaults to template_label and falls back to template_name", {
  # label present -> y axis uses the species label
  p_lab <- plot_scores_overview(overview_detecs(label_col = TRUE))
  expect_true(all(c("ave_A", "ave_B") %in% p_lab$data$template))
  # label column absent -> falls back to template_name
  p_nam <- plot_scores_overview(overview_detecs(label_col = FALSE))
  expect_true(all(c("tplA", "tplB") %in% p_nam$data$template))
  # explicit label = "template_name" overrides the default
  p_for <- plot_scores_overview(overview_detecs(label_col = TRUE),
                                label = "template_name")
  expect_true(all(c("tplA", "tplB") %in% p_for$data$template))
})

test_that("plot_scores_overview show_counts toggles the text layer", {
  has_text <- function(p) any(vapply(p$layers,
    function(L) inherits(L$geom, "GeomText"), logical(1)))
  expect_true(has_text(plot_scores_overview(overview_detecs())))
  expect_false(has_text(plot_scores_overview(overview_detecs(), show_counts = FALSE)))
})

test_that("plot_scores_overview guards invalid input", {
  expect_error(plot_scores_overview(list(a = 1)), "must be a data.frame")
  expect_error(plot_scores_overview(overview_detecs()[0, ]), "is empty")
  no_ss <- overview_detecs(); no_ss$soundscape_file <- NULL
  expect_error(plot_scores_overview(no_ss), "soundscape_file")
  no_lab <- overview_detecs(label_col = FALSE); no_lab$template_name <- NULL
  expect_error(plot_scores_overview(no_lab), "template_label.*template_name")
})
