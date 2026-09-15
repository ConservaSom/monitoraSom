#' Plot the scores and detections of one template-match pair
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Renders a stacked two-panel figure for a single soundscape-template match
#'   row: the top panel is the soundscape spectrogram overlaid with detection
#'   rectangles and a soundscape label; the bottom panel is the matching-score
#'   line with peak points, detection bands, an optional `min_score` threshold
#'   rule and a template label. It is the visual-inspection tool for the pipeline
#'   output, the point where you look at *why* a detection fired.
#'
#' @details
#'   The function takes exactly one row of [run_matching()] output in the
#'   `output = "scores"` mode (subset it yourself, e.g. `df_scores[i, ]`; more
#'   than one row is an error) and runs the detection step itself, so the
#'   plotted detections match what the pipeline would keep under the same
#'   thresholds. The
#'   `output = "detections"` result cannot be plotted: captured detections
#'   carry no complete score vector. The two panels share a single light/dark
#'   theme and the same vertical time guides, so they read as one figure.
#'   Things to know: the soundscape WAV named in the row is re-read from disk,
#'   so the recording must still be at `soundscape_path`; `ovlp`/`wl` default
#'   to the values recorded in the match row (`template_ovlp`/`template_wl`) so
#'   the spectrogram matches the one that was scored. Override them only to
#'   re-inspect, not to reproduce the run. For an overview of a whole run
#'   across many pairs, use [plot_scores_overview()].
#'
#' @section Pipeline context:
#'   Step 13 of the monitoraSom analysis flow (visualization). Reads one row
#'   of the scored matches from [run_matching()] / [template_matching()] (step 7).
#'   Produces a `patchwork` figure for human inspection, a leaf of the flow,
#'   feeding nothing downstream.
#'
#' @param df_scores_i One row of [run_matching()] output in the
#'   `output = "scores"` mode: the raw `score_vec` list-column plus
#'   template/soundscape metadata. Already-captured detections (as from
#'   [fetch_score_peaks()]) are not accepted: they carry no complete score
#'   vector.
#' @param buffer_size Exclusion buffer in spectrogram frames. Default
#'   `"template"` uses the template's frame
#'   count (`score_sliding_window`); a non-negative whole number sets it
#'   explicitly, `0` disables suppression. Larger values give fewer, more
#'   spread-out detections.
#' @param min_score,min_quant,top_n Optional thresholds with `scope =
#'   "pair"`: mark peaks with score at
#'   or above `min_score`, score quantile at or above `min_quant`, and at most
#'   `top_n` peaks by raw score. Each defaults to `NULL` (no threshold); they
#'   compose.
#' @param zoom_freq,zoom_time Length-2 numeric panel limits (kHz / seconds).
#'   `NULL` uses the full recording extent.
#' @param zoom_score Length-2 numeric score-axis limits. `NULL` uses the score
#'   range.
#' @param ovlp,wl Spectrogram FFT overlap (\%) and window length. `NULL` uses the
#'   values from `df_scores_i` (`template_ovlp` / `template_wl`).
#' @param dyn_range Length-2 numeric relative-amplitude display range (dB).
#'   `NULL` uses `c(-60, 0)`.
#' @param color_scale One of `"viridis"`, `"magma"`, `"inferno"`, `"cividis"`,
#'   `"greyscale 1"`, `"greyscale 2"`.
#' @param n_colors Integer number of colours in the colour scale.
#' @param interpolate Logical; raster interpolation.
#' @param theme_mode,time_guide_interval,freq_guide_interval,font_scale,channel,invert_colormap
#'   Presentation controls forwarded to [fast_spectro()]. `theme_mode`
#'   (`"light"` default or `"dark"`) is the shared plot theme of the two
#'   panels, and `time_guide_interval` additionally themes the score panel so
#'   both panels match.
#' @param ... Other arguments forwarded to [fast_spectro()] (hence
#'   [extract_spectro()]).
#'
#' @return A `patchwork` composite of a spectrogram panel (top) and a
#'   score-vector panel (bottom).
#' @seealso [run_matching()], [template_matching()], [fetch_score_peaks()],
#'   [fast_spectro()], [plot_scores_overview()]
#' @import ggplot2 patchwork
#' @export
#' @examples
#' # Step 13 of the analysis flow: inspect one scored soundscape-template pair
#' # from run_matching (step 7), saved with output = "scores". The soundscape
#' # WAV named in the row must be readable at its soundscape_path.
#' # (Build a tiny grid from a synthesized recording; see [fetch_match_grid()]
#' # for the full recipe. The bundled df_grid dataset predates template_id and
#' # cannot feed the pipeline.)
#' \donttest{
#' # Load the package
#' library(monitoraSom)
#' rec_dir <- file.path(tempdir(), "recs"); dir.create(rec_dir, showWarnings = FALSE)
#' # A 20 s soundscape with a varied 4 kHz content: one clear burst (the
#' # template), a longer copy, a fainter copy, plus a 5.5 kHz distractor and a
#' # low noise floor. This gives a score vector with peaks of different
#' # heights, so the filter choices below are visible.
#' sr <- 16000
#' set.seed(7)
#' tt <- seq_len(20 * sr) / sr
#' x <- rnorm(length(tt), 0, 0.02)
#' m4 <- function(tt) 0.6 * sin(2 * pi * 4000 * tt)
#' m55 <- function(tt) 0.5 * sin(2 * pi * 5500 * tt)
#' x[(tt >= 1) & (tt < 1.4)]   <- x[(tt >= 1) & (tt < 1.4)] + m4(tt[(tt >= 1) & (tt < 1.4)])
#' x[(tt >= 3) & (tt < 4.2)]   <- x[(tt >= 3) & (tt < 4.2)] + m4(tt[(tt >= 3) & (tt < 4.2)])
#' x[(tt >= 7) & (tt < 7.4)]   <- x[(tt >= 7) & (tt < 7.4)] + 0.25 * m4(tt[(tt >= 7) & (tt < 7.4)])
#' x[(tt >= 4.5) & (tt < 5.1)] <- x[(tt >= 4.5) & (tt < 5.1)] + m55(tt[(tt >= 4.5) & (tt < 5.1)])
#' rec <- tuneR::normalize(tuneR::Wave(x, samp.rate = sr, bit = 16), unit = "16")
#' tuneR::writeWave(rec, file.path(rec_dir, "siteA_01.wav"))
#' df_rois <- data.frame(
#'   soundscape_path = file.path(rec_dir, "siteA_01.wav"),
#'   soundscape_file = "siteA_01.wav",
#'   roi_label = "burst", roi_start = 0.8, roi_end = 1.2,
#'   roi_min_freq = 3, roi_max_freq = 5, roi_wl = 512, roi_ovlp = 50,
#'   stringsAsFactors = FALSE)
#' out_dir <- file.path(tempdir(), "templates")
#' export_templates(df_rois, templates_path = out_dir, create_dir = TRUE)
#' df_scores <- run_matching(
#'   fetch_match_grid(fetch_soundscape_metadata(rec_dir),
#'                    fetch_template_metadata(out_dir)),
#'   output = "scores", score_method = "fft")
#'
#' # The two-panel figure of one match row: spectrogram on top, score line
#' # with detection bands below. The score panel marks every captured peak
#' # (exclusion buffer applied) with a red point.
#' p <- plot_scores(df_scores[1, ])   # one match row -> two-panel figure
#' class(p)                           # "patchwork" / "gg"
#'
#' # Apply a score threshold so only strong peaks are kept as detections:
#' # the two clear 4 kHz hits pass, the fainter copy and the distractor do not.
#' plot_scores(df_scores[1, ], min_score = 0.6)
#'
#' # Keep only the 3 best peaks, whatever their score:
#' plot_scores(df_scores[1, ], top_n = 3)
#'
#' # Zoom into the first 4 seconds and the 0-8 kHz band, dark theme:
#' plot_scores(df_scores[1, ], zoom_time = c(0, 4), zoom_freq = c(0, 8),
#'             theme_mode = "dark")
#' }
plot_scores <- function(
  df_scores_i, buffer_size = "template", min_score = NULL, min_quant = NULL,
  top_n = NULL, zoom_freq = NULL, zoom_time = NULL, zoom_score = NULL,
  ovlp = NULL, wl = NULL, dyn_range = NULL, color_scale = "inferno",
  n_colors = .PS_DEFAULT_N_COLORS, interpolate = FALSE, theme_mode = "light",
  time_guide_interval = 3, freq_guide_interval = 1, font_scale = 1,
  channel = c("left", "right", "mix"), invert_colormap = FALSE, ...
) {
  # PS-08: one row only.
  if (nrow(df_scores_i) != 1L) {
    stop("`df_scores_i` must be exactly one row of `run_matching()` output. ",
         "Subset a single match row (e.g. `df_scores[i, ]`).")
  }
  # PS-03 / channel validation (errors name plot_scores).
  color_scale <- match.arg(color_scale, .PS_COLOR_SCALES)
  channel <- match.arg(channel)
  # PS-04: plot-only range checks (filter args are validated by filter_detections_i).
  .ps_check_len2(zoom_freq, "zoom_freq")
  .ps_check_len2(zoom_time, "zoom_time")
  .ps_check_len2(zoom_score, "zoom_score")
  .ps_check_len2(dyn_range, "dyn_range")

  # PS-13: decoupled detection (detect) + filter (filter_detections_i, pair scope).
  detecs <- fetch_score_peaks_i(df_scores_i, buffer_size = buffer_size)
  detecs <- filter_detections_i(detecs, min_score = min_score,
                              min_quant = min_quant, top_n = top_n,
                              scope = "pair")

  rec <- tuneR::readWave(filename = df_scores_i$soundscape_path)
  # The refactored fast_spectro crops via flim/tlim (PS-05). Pass the user's zoom
  # straight through, or NULL for the full range — an explicit full-range flim up
  # to the exact Nyquist is out of bounds in extract_spectro, so NULL is the
  # correct "whole recording" signal. zoom_time is still resolved for the score
  # panel's shared x-axis.
  flim_arg <- zoom_freq
  tlim_arg <- zoom_time
  if (is.null(zoom_time)) zoom_time <- c(0, length(rec@left) / rec@samp.rate)
  if (is.null(ovlp)) ovlp <- df_scores_i$template_ovlp
  if (is.null(wl)) wl <- df_scores_i$template_wl
  if (is.null(dyn_range)) dyn_range <- c(-60, 0)
  if (is.null(zoom_score)) zoom_score <- range(df_scores_i$score_vec[[1]]$score_vec)

  # PS-02: only the outline colour survives (greyscale -> black, else white).
  selection_color <- if (color_scale %in% c("greyscale 1", "greyscale 2")) {
    "black"
  } else {
    "white"
  }
  # PS-12 (revised): both panels share one theme (dark/light) and the same
  # vertical time guides, so the score panel is themed to match the spectrogram.
  theme_colors <- .ps_theme_colors(theme_mode)

  # buffer resolved for the caption (the engine resolves "template" itself).
  buffer_label <- if (identical(buffer_size, "template")) {
    df_scores_i$score_sliding_window
  } else if (is.numeric(buffer_size)) {
    buffer_size
  } else {
    stop("buffer_size must be either 'template' or a numeric value")
  }
  filter_caption <- paste0(
    "buffer_size = ", buffer_label, "; ",
    "min_score = ", ifelse(is.null(min_score), "NULL", min_score), "; ",
    "min_quant = ", ifelse(is.null(min_quant), "NULL", min_quant), "; ",
    "top_n = ", ifelse(is.null(top_n), "NULL", top_n)
  )

  spectro_panel <- .ps_spectro_panel(
    rec, detecs, df_scores_i, flim_arg, tlim_arg, ovlp, wl, dyn_range,
    color_scale, n_colors, interpolate, selection_color, theme_mode,
    time_guide_interval, freq_guide_interval, font_scale, channel,
    invert_colormap, ...
  )
  score_panel <- .ps_score_panel(
    detecs, df_scores_i, zoom_time, zoom_score, min_score, filter_caption,
    theme_colors, time_guide_interval, font_scale
  )

  spectro_panel + score_panel + patchwork::plot_layout(nrow = 2, byrow = FALSE)
}

#' Plot a detection-count heatmap for a whole matching run
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Summarises an entire matching run at a glance. Where [plot_scores()] shows
#'   one soundscape-template pair, this draws a `ggplot2` heatmap of the whole
#'   grid: one tile per (template, soundscape) pair, filled and labelled by the
#'   number of detections in that pair. It answers "which templates fired, on
#'   which recordings, and how much" in a single figure.
#'
#' @details
#'   Counts come from `table()` over the detections, which completes the full
#'   soundscape-template grid: a pair that never produced a detection renders as
#'   a `0` tile, so silent gaps are as visible as hits. This complements the
#'   tabular [summarise_matching()]. Use it right after a run to spot templates
#'   that never fire or recordings that no template matched. Note: the y-axis
#'   uses `template_label` by default and silently falls back to `template_name`
#'   when the label column is absent or all-`NA`, so a run without labels still
#'   plots (keyed by name).
#'
#' @section Pipeline context:
#'   Step 13 of the monitoraSom analysis flow (visualization). Reads the
#'   detections table from [run_matching()] / [template_matching()] (step 7).
#'   Produces a `ggplot` heatmap for human inspection, a leaf of the flow.
#'
#' @param detections A detections data.frame as returned by [run_matching()] /
#'   [template_matching()] (standard detections format). Must carry
#'   `soundscape_file` and at least one of `template_label` / `template_name`.
#' @param label Which template column to show on the y-axis: `"template_label"`
#'   (default, the species/label) or `"template_name"`. Falls back to the other
#'   column when the chosen one is absent or all-`NA`.
#' @param low_color,high_color End colours of the count fill gradient. Defaults
#'   are the viridis dark/bright endpoints; change to recolour the scale.
#' @param show_counts Logical, default `TRUE`. Prints the per-cell count on each
#'   tile; set `FALSE` for a colour-only heatmap on large grids.
#'
#' @return A `ggplot` heatmap with one tile per soundscape-template pair, the
#'   fill and (optionally) the printed number giving the detection count.
#' @seealso [plot_scores()], [run_matching()], [template_matching()],
#'   [summarise_matching()]
#' @import ggplot2
#' @export
#' @examples
#' # Step 13 of the analysis flow: overview a whole matching run (step 7) as a
#' # per-pair detection-count heatmap.
#' \dontrun{
#' # Build a tiny run from synthesized recordings (see [fetch_match_grid()] for
#' # the full recipe), then overview the whole run as a per-pair count heatmap.
#' rec_dir <- file.path(tempdir(), "recs"); dir.create(rec_dir, showWarnings = FALSE)
#' # Two soundscapes: one clear 4 kHz burst, one with a fainter copy, so the
#' # two tiles show different detection counts.
#' sr <- 16000
#' set.seed(7)
#' tt <- seq_len(10 * sr) / sr
#' x1 <- rnorm(length(tt), 0, 0.02)
#' x1[(tt >= 1) & (tt < 1.4)] <- x1[(tt >= 1) & (tt < 1.4)] +
#'   0.6 * sin(2 * pi * 4000 * tt[(tt >= 1) & (tt < 1.4)])
#' rec1 <- tuneR::normalize(tuneR::Wave(x1, samp.rate = sr, bit = 16), unit = "16")
#' tuneR::writeWave(rec1, file.path(rec_dir, "siteA_01.wav"))
#' x2 <- rnorm(length(tt), 0, 0.02)
#' x2[(tt >= 2) & (tt < 2.4)] <- x2[(tt >= 2) & (tt < 2.4)] +
#'   0.25 * sin(2 * pi * 4000 * tt[(tt >= 2) & (tt < 2.4)])
#' rec2 <- tuneR::normalize(tuneR::Wave(x2, samp.rate = sr, bit = 16), unit = "16")
#' tuneR::writeWave(rec2, file.path(rec_dir, "siteA_02.wav"))
#' df_rois <- data.frame(
#'   soundscape_path = file.path(rec_dir, "siteA_01.wav"),
#'   soundscape_file = "siteA_01.wav",
#'   roi_label = "burst", roi_start = 0.8, roi_end = 1.2,
#'   roi_min_freq = 3, roi_max_freq = 5, roi_wl = 512, roi_ovlp = 50,
#'   stringsAsFactors = FALSE)
#' out_dir <- file.path(tempdir(), "templates")
#' export_templates(df_rois, templates_path = out_dir, create_dir = TRUE)
#' det <- template_matching(
#'   fetch_match_grid(fetch_soundscape_metadata(rec_dir),
#'                    fetch_template_metadata(out_dir)),
#'   output = "detections")
#'
#' plot_scores_overview(det)                        # counts printed on the tiles
#' plot_scores_overview(det, show_counts = FALSE)   # colour-only heatmap
#' }
plot_scores_overview <- function(detections,
                                 label = c("template_label", "template_name"),
                                 low_color = "#3B0F70", high_color = "#FDE725",
                                 show_counts = TRUE) {
  if (!is.data.frame(detections)) {
    stop("`detections` must be a data.frame of detections.")
  }
  if (nrow(detections) == 0L) {
    stop("`detections` is empty: there is nothing to overview.")
  }
  if (!"soundscape_file" %in% names(detections)) {
    stop("`detections` must contain a `soundscape_file` column.")
  }
  label <- match.arg(label)
  tmpl_col <- .ps_overview_template_col(detections, label)

  # table() completes the full template x soundscape grid, filling absent pairs
  # with 0 (a template that never fired on a soundscape is a meaningful zero).
  tab <- table(template = as.character(detections[[tmpl_col]]),
               soundscape = as.character(detections$soundscape_file))
  df <- as.data.frame(tab, responseName = "n", stringsAsFactors = FALSE)

  p <- ggplot2::ggplot(
        df, ggplot2::aes(x = soundscape, y = template, fill = n)) +
    ggplot2::geom_tile(color = "grey92") +
    ggplot2::scale_fill_gradient(low = low_color, high = high_color,
                                 name = "detections") +
    ggplot2::labs(x = "soundscape", y = "template",
                  title = "Detections per template x soundscape") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank())
  if (isTRUE(show_counts)) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = n), size = 3)
  }
  p
}

# Pick the template label column for plot_scores_overview (AFL-04 fallback):
# the requested column when present and not all-NA, else the other one.
.ps_overview_template_col <- function(detections, label) {
  other <- setdiff(c("template_label", "template_name"), label)
  usable <- function(col) {
    col %in% names(detections) && any(!is.na(detections[[col]]))
  }
  if (usable(label)) return(label)
  if (usable(other)) return(other)
  stop("`detections` must contain a non-empty `template_label` or ",
       "`template_name` column.")
}

# Named constants (PS-10). ------------------------------------------------------
.PS_DEFAULT_N_COLORS <- 124L
.PS_COLOR_SCALES <- c("viridis", "magma", "inferno", "cividis",
                      "greyscale 1", "greyscale 2")

# Length-2 numeric guard for the zoom/dyn_range args (PS-04). NULL is allowed.
.ps_check_len2 <- function(x, name) {
  if (!is.null(x) && (!is.numeric(x) || length(x) != 2L)) {
    stop("`", name, "` must be NULL or a length-2 numeric vector.")
  }
}

# Shared panel theme colours (PS-12), mirroring fast_spectro's plot_spectro: a
# single light/dark choice drives both panels' text/background/guide colours.
# FEAT-08: delegates to the shared `.monitora_theme_colors` (_plot_theme.R) so
# the score plots and the diagnostic plots draw from one colour source.
.ps_theme_colors <- function(theme_mode) {
  .monitora_theme_colors(theme_mode)
}

# The same vertical time guides fast_spectro draws on the spectrogram, at
# multiples of `time_guide_interval` over the panel's time window (PS-12). Drawn
# in the theme text colour so they read on either background.
.ps_time_guides <- function(zoom_time, time_guide_interval, colour) {
  if (is.null(time_guide_interval) || time_guide_interval <= 0) return(NULL)
  at <- seq(
    floor(zoom_time[1] / time_guide_interval) * time_guide_interval,
    ceiling(zoom_time[2] / time_guide_interval) * time_guide_interval,
    by = time_guide_interval
  )
  at <- at[at >= zoom_time[1] & at <= zoom_time[2]]
  if (length(at) == 0L) return(NULL)
  ggplot2::geom_vline(xintercept = at, colour = colour, alpha = 0.4,
                      linewidth = 0.3)
}

# Spectrogram panel: the refactored fast_spectro (flim/tlim), with detection
# rectangles + a soundscape label on top, and the top x-axis hidden last (PS-05/
# PS-06/PS-07; no dropped `f`, no redundant coord_cartesian, blanking not reset).
.ps_spectro_panel <- function(rec, detecs, df_scores_i, flim, tlim,
                              ovlp, wl, dyn_range, color_scale, n_colors,
                              interpolate, selection_color, theme_mode,
                              time_guide_interval, freq_guide_interval,
                              font_scale, channel, invert_colormap, ...) {
  fast_spectro(
    rec, flim = flim, tlim = tlim, ovlp = ovlp, wl = wl,
    dyn_range = dyn_range, color_scale = color_scale, n_colors = n_colors,
    interpolate = interpolate, theme_mode = theme_mode,
    time_guide_interval = time_guide_interval,
    freq_guide_interval = freq_guide_interval, font_scale = font_scale,
    channel = channel, invert_colormap = invert_colormap, ...
  ) +
    ggplot2::annotate(
      "rect", alpha = 0, linewidth = 0.2, linetype = "solid",
      color = selection_color,
      xmin = detecs$detection_start, xmax = detecs$detection_end,
      ymin = detecs$template_min_freq, ymax = detecs$template_max_freq
    ) +
    ggplot2::annotate(
      "label",
      label = paste0(df_scores_i$soundscape_file, " (sr = ",
                     df_scores_i$soundscape_sample_rate, ")"),
      x = -Inf, y = Inf, hjust = 0, vjust = 1, color = "white",
      fill = "black", size = 3
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
}

# Score panel: the raw score line with peak points, detection bands, an optional
# min_score rule and a template label. Themed to match the spectrogram panel
# (PS-12): same light/dark colours and the same vertical time guides, so the two
# stacked panels read as one figure. Statistics are unchanged.
.ps_score_panel <- function(detecs, df_scores_i, zoom_time, zoom_score,
                            min_score, filter_caption, theme_colors,
                            time_guide_interval, font_scale) {
  sv <- df_scores_i$score_vec[[1]]
  p <- ggplot2::ggplot(sv, ggplot2::aes(x = time_vec, y = score_vec)) +
    .ps_time_guides(zoom_time, time_guide_interval, theme_colors$text) +
    ggplot2::annotate(
      "rect", alpha = 0.2, linewidth = 0.5, linetype = "solid", fill = "red",
      xmin = detecs$detection_start, xmax = detecs$detection_end,
      ymin = -Inf, ymax = Inf
    ) +
    ggplot2::geom_line(colour = theme_colors$text) +
    ggplot2::annotate(
      "point",
      x = sv$time_vec[detecs$peak_index], y = sv$score_vec[detecs$peak_index],
      pch = 21, color = "black", fill = "#ff6262", size = 4
    ) +
    ggplot2::annotate(
      "label",
      label = paste0(df_scores_i$template_file, " (sr = ",
                     df_scores_i$template_sample_rate, ")"),
      x = -Inf, y = Inf, hjust = 0, vjust = 1, color = "#000000",
      fill = "#ffffff", size = 3
    )
  if (!is.null(min_score)) {
    p <- p + ggplot2::geom_hline(yintercept = min_score, linetype = "dashed",
                                 colour = theme_colors$text)
  }
  p +
    ggplot2::labs(x = "seconds", y = "matching score", caption = filter_caption) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::coord_cartesian(
      xlim = zoom_time, ylim = c(zoom_score[1], zoom_score[2] + 0.1)
    ) +
    .ps_score_theme(theme_colors, font_scale)
}

# Score-panel theme mirroring fast_spectro's .spectro_theme: solid light/dark
# background, themed text/ticks, and NO default grid (the explicit time guides
# are the grid, matching the spectrogram).
.ps_score_theme <- function(theme_colors, font_scale) {
  ggplot2::theme_bw(base_size = 11 * font_scale) +
    ggplot2::theme(
      axis.text   = ggplot2::element_text(color = theme_colors$text,
                                          size = 10 * font_scale),
      axis.title  = ggplot2::element_text(color = theme_colors$text,
                                          size = 12 * font_scale),
      axis.ticks  = ggplot2::element_line(color = theme_colors$text),
      plot.caption = ggplot2::element_text(color = theme_colors$text),
      panel.background = ggplot2::element_rect(fill = theme_colors$background),
      plot.background  = ggplot2::element_rect(fill = theme_colors$background,
                                               color = NA),
      panel.border = ggplot2::element_rect(color = theme_colors$text, fill = NA),
      panel.grid = ggplot2::element_blank()
    )
}
