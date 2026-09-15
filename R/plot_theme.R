#' Shared plot theme for monitoraSom diagnostic + score plots (FEAT-08)
#'
#' @description Generalizes the `plot_scores` theme system (PS-12) into one place
#'   so the diagnostic plots (`diagnostic_validations_i`) and the score plots
#'   share a single light/dark theme, a named TP/FP/All palette, and a common
#'   decision cut-line style. Cosmetic only — no data goldens depend on plot
#'   aesthetics (diagnostics goldens are data-contract, no vdiffr).
#' @name plot_theme
#' @keywords internal
NULL

# Named validation-class palette: true positive / false positive / pooled "All".
# Colour-blind-safe hues; used by every plot that colours by validation class.
.monitora_palette <- function() {
  c(TP = "#1b9e77", FP = "#d95f02", All = "grey30")
}

# The decision cut line colour (score_cut / selected operating point).
.MONITORA_CUT_COLOUR <- "#d73027"

# Light/dark text + background colours (a superset of plot_scores' PS-12
# `.ps_theme_colors`; both panels and the diagnostics share one source now).
.monitora_theme_colors <- function(theme_mode = "light") {
  if (identical(theme_mode, "dark")) {
    list(text = "white", background = "black")
  } else {
    list(text = "black", background = "white")
  }
}

#' A `theme_bw()`-based theme parameterized by `theme_mode` (FEAT-08).
#'
#' Themed text/ticks/border over a solid `theme_mode` background, matching the
#' `plot_scores` score panel's look. `grid = TRUE` keeps the `theme_bw()` grid
#' (diagnostic plots have no explicit guides, so they need it); `grid = FALSE`
#' blanks it (the score panel draws its own time guides).
#' @noRd
.monitora_theme <- function(theme_mode = "light", font_scale = 1, grid = TRUE) {
  tc <- .monitora_theme_colors(theme_mode)
  base <- ggplot2::theme_bw(base_size = 11 * font_scale) +
    ggplot2::theme(
      axis.text    = ggplot2::element_text(color = tc$text, size = 10 * font_scale),
      axis.title   = ggplot2::element_text(color = tc$text, size = 12 * font_scale),
      axis.ticks   = ggplot2::element_line(color = tc$text),
      plot.title   = ggplot2::element_text(color = tc$text),
      plot.caption = ggplot2::element_text(color = tc$text),
      legend.text  = ggplot2::element_text(color = tc$text),
      legend.title = ggplot2::element_text(color = tc$text),
      panel.background  = ggplot2::element_rect(fill = tc$background),
      plot.background   = ggplot2::element_rect(fill = tc$background, color = NA),
      legend.background = ggplot2::element_blank(),
      legend.key        = ggplot2::element_rect(fill = tc$background, color = NA),
      panel.border = ggplot2::element_rect(color = tc$text, fill = NA)
    )
  if (!grid) base <- base + ggplot2::theme(panel.grid = ggplot2::element_blank())
  base
}

# Standard decision cut line (vertical, dashed, in the palette cut colour).
.monitora_cutline <- function(xintercept) {
  ggplot2::geom_vline(xintercept = xintercept, colour = .MONITORA_CUT_COLOUR,
                      linetype = 2)
}
