#' Plot scores and detections of one template match run
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function plots a spectrogram of the soundscape and the results of the
#'   run_matching_i() and fetch_score_peaks() functions.
#'
#' @param df_scores_i One row of the output of `run_matching` containing the raw
#'   scores of a template match run.
#' @param buffer_size A numeric value specifying the number of frames of the
#'   buffer within which overlap between detections is avoided. Defaults to
#'   "template", which means that the buffer size equals the number of frames
#'   present in the template spectrogram. The buffer exclusion priority is
#'   oriented by score quantiles, so that the highest scoring detections are
#'   always kept. Setting the buffer size to 0 disables the exclusion buffer.
#' @param min_score A numeric value between 0 and 0.99 indicating the minimum
#'   score of the detections that will be kept. Defaults to NULL, which returns
#'   all available detections.
#' @param min_quant A numeric value between 0 and 1 indicating the minimum score
#'   quantile of the kept detections. Defaults to NULL, which returns all
#'   available detections.
#' @param top_n An integer indicating the maximum number of peaks to be
#'   returned, selected according to the highest scores available. Defaults to
#'   NULL, which return all available detections. It should be noted that
#'   because the peak quantiles are callculated within each score vector, the
#'   top_n parameter is applied to each score vector separately, and not to the
#'   whole matching grid.
#' @param ovlp A numeric value specifying the percentage overlap of windows in
#'   the spectrogram calculation. Defaults to NULL, which uses the value
#'   obtained from df_scores_i, but overrides it if a value is provided.
#' @param wl An integer specifying the length of the FFT window used to
#'   calculate the spectrogram. Defaults to NULL, which uses the value obtained
#'   from df_scores_i, but overrides it if a value is provided.
#' @param dyn_range A numeric vector of length 2 giving the minimum and maximum
#'   values of relative amplitude to be displayed in the spectrogram.
#' @param color_scale A character string specifying the color scale to be used
#'   in the spectrogram. Possible values are "viridis", "magma", "inferno",
#'   "cividis", "greyscale 1", or "greyscale 2".
#' @param n_colors An integer specifying the number of colors to be used in the
#'   color scale. Smaller values will result in lower resolution color scales,
#'   but will also result in faster rendering times.
#' @param interpolate A logical value indicating whether the raster should be
#'   interpolated or not.
#' @param zoom_score A numeric vector of length 2 specifying the score range to
#'   be displayed.
#' @param zoom_freq Set frequency limits for the spectrogram panel
#' @param zoom_time Set time limits for the spectrogram panel.
#' @param ... Other arguments passed to the seewave::spectro() function
#'
#' @return A ggplot object containing the spectrogram and the score vector.
#'
#' @export
#' @import ggplot2 patchwork seewave viridis
#' @examples
#' \dontrun{
#'
#' # Load the necessary packages to run this example
#' library(monitoraSom)
#' library(dplyr)
#' library(tuneR)
#'
#' # Load the soundscape list to populate the example data
#' data(ls_soundscapes)
#'
#' # Create a directory to store the soundscapes
#' soundscapes_path <- "./soundscapes"
#' dir.create(soundscapes_path)
#' invisible(lapply(1:length(ls_soundscapes), function(i) {
#'   writeWave(
#'     ls_soundscapes[[i]],
#'     file.path(soundscapes_path, names(ls_soundscapes)[i])
#'   )
#' }))
#'
#' data(df_scores)
#'
#' # Plot the scores of the 20th match with the default parameters. It gets most
#' # peaks, but several false positives and the spectrogram is not very
#' # informative.
#' plot_scores(df_scores_i = df_scores[20, ])
#'
#' # Now with filtered scores and adjusted spectrogram parameters. Now we can see
#' # the target signals more clearly, but we still have some false positives.
#' plot_scores(
#'   df_scores_i = df_scores[20, ], ovlp = 50, wl = 512, dyn_range = c(-90, -30)
#' )
#'
#' # Now lets turn of the default detection filters to have a look at the
#' # detections obtained without any filtering. We can barely see the target
#' # signals among so many false positives.
#' plot_scores(
#'   df_scores_i = df_scores[20, ], ovlp = 50, wl = 512, dyn_range = c(-90, -30),
#'   buffer_size = 0, min_score = NULL, min_quant = NULL, top_n = NULL
#' )
#'
#' # Now lets improve the filters so that only true positives are shown as
#' # detections. We set a minimum score of 0.3 and a minimum quantile of 0.9.
#' plot_scores(
#'   df_scores_i = df_scores[20, ], ovlp = 50, wl = 512, dyn_range = c(-90, -30),
#'   buffer_size = 0, min_score = 0.3, min_quant = 0.9, top_n = NULL
#' )
#'
#' # Now lets zoom in one detection to see the details. We can notice that the
#' # template has a target siognal that matches the final two notes of the target
#' # species' song
#' plot_scores(
#'   df_scores_i = df_scores[20, ],
#'   buffer_size = "template", min_score = 0.3, min_quant = NULL, top_n = NULL,
#'   ovlp = 50, wl = 512, dyn_range = c(-90, -30),
#'   zoom_freq = c(2, 10), zoom_time = c(20, 25)
#' )
#'
#' }
plot_scores <- function(
  df_scores_i, buffer_size = "template", min_score = NULL, min_quant = NULL,
  top_n = NULL, zoom_freq = NULL, zoom_time = NULL, zoom_score = NULL,
  ovlp = NULL, wl = NULL, dyn_range = NULL, color_scale = "inferno",
  n_colors = 124, interpolate = FALSE, ...
) {
  # requireNamespace("patchwork")
  # requireNamespace("viridis")
  # requireNamespace("seewave")
  # requireNamespace("tuneR")
  # requireNamespace("ggplot2")
  # requireNamespace("dplyr")

  detecs <- fetch_score_peaks_i(
    df_scores_i,
    buffer_size = buffer_size,
    min_score = min_score,
    min_quant = min_quant,
    top_n = top_n
  )

  rec <- tuneR::readWave(filename = df_scores_i$soundscape_path)
  if (is.null(zoom_time)) {
    zoom_time <- c(0, length(rec@left) / rec@samp.rate)
  }
  if (is.null(zoom_freq)) {
    zoom_freq <- c(0, (rec@samp.rate / 2) / 1000)
  }

  if (color_scale %in% c("viridis", "magma", "inferno", "cividis")) {
    colormap <- viridis::viridis(n_colors, option = color_scale)
  } else if (color_scale == "greyscale 1") {
    colormap <- seewave::reverse.gray.colors.1(n_colors)
  } else if (color_scale == "greyscale 2") {
    colormap <- seewave::reverse.gray.colors.2(n_colors)
  }
  selection_color <- ifelse(
    color_scale %in% c("greyscale 1", "greyscale 2"),
    "black",
    "white"
  )

  if (is.null(ovlp)) {
    ovlp <- df_scores_i$template_ovlp
  }
  if (is.null(wl)) {
    wl <- df_scores_i$template_wl
  }
  if (is.null(dyn_range)) {
    dyn_range <- c(-60, 0)
  }
  if (is.null(zoom_score)) {
    zoom_score <- range(df_scores_i$score_vec[[1]]$score_vec)
  }
  if (buffer_size == "template") {
    buffer_size <- df_scores_i$score_sliding_window
  } else if (!is.numeric(buffer_size)) {
    stop("buffer_size must be either 'template' or a numeric value")
  }

  filter_caption <- paste0(
    "buffer_size = ", buffer_size, "; ",
    "min_score = ", ifelse(is.null(min_score), "NULL", min_score), "; ",
    "min_quant = ", ifelse(is.null(min_quant), "NULL", min_quant), "; ",
    "top_n = ", ifelse(is.null(top_n), "NULL", top_n)
  )

  soundscape_spectro <- fast_spectro(
    rec, f = rec@samp.rate, ovlp = ovlp, wl = wl, dyn_range = dyn_range,
    color_scale = color_scale, n_colors = 124, interpolate = interpolate
  ) +
    annotate(
      "rect", alpha = 0, linewidth = 0.2, linetype = "solid",
      color = selection_color,
      xmin = detecs$detection_start,
      xmax = detecs$detection_end,
      ymin = detecs$template_min_freq,
      ymax = detecs$template_max_freq
    ) +
    annotate(
      "label",
      label = paste0(
        df_scores_i$soundscape_file, " (sr = ",
        df_scores_i$soundscape_sample_rate, ")"
      ),
      x = -Inf, y = Inf, hjust = 0, vjust = 1, color = "white",
      fill = "black", size = 3
    ) +
    coord_cartesian(ylim = zoom_freq, xlim = zoom_time) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  plot_score <- df_scores_i$score_vec[[1]] |>
    ggplot(aes(x = time_vec, y = score_vec)) +
    annotate(
      "point",
      x = df_scores_i$score_vec[[1]]$time_vec[detecs$peak_index],
      y = df_scores_i$score_vec[[1]]$score_vec[detecs$peak_index],
      pch = 21, color = "black", fill = "#ff6262", size = 4
    ) +
    annotate(
      "rect",
      alpha = 0.2, linewidth = 0.5, linetype = "solid", fill = "red",
      xmin = detecs$detection_start, xmax = detecs$detection_end,
      ymin = -Inf, ymax = Inf
    ) +
    geom_line() +
    annotate(
      "label",
      label = paste0(
        df_scores_i$template_file, " (sr = ", df_scores_i$template_sample_rate,
        ")"
      ),
      x = -Inf, y = Inf, hjust = 0, vjust = 1, color = "#000000",
      fill = "#ffffff", size = 3
    ) +
    {
      if (!is.null(min_score)) {
        geom_hline(yintercept = min_score, linetype = "dashed")
      }
    } +
    labs(
      x = "seconds", y = "matching score", caption = filter_caption
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    coord_cartesian(
      xlim = zoom_time, ylim = c(zoom_score[1], zoom_score[2] + 0.1)
    ) +
    theme_bw()

  res <- soundscape_spectro +
    plot_score +
    patchwork::plot_layout(nrow = 2, byrow = FALSE)

  return(res)
}
