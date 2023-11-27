#' plot_detecs_i - Plots the results of the match_i function
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function plots a spectrogram of the soundscape and the results of the
#' match_i() and fetch_score_peaks_i() functions.
#'
#' @param match_res_i The output of the function tpmatch::match_i
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
#'   obtained from match_res_i, but overrides it if a value is provided.
#' @param wl An integer specifying the length of the FFT window used to
#'   calculate the spectrogram. Defaults to NULL, which uses the value obtained
#'   from match_res_i, but overrides it if a value is provided.
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
#' @param score_lims A numeric vector of length 2 specifying the score range to
#'   be displayed.
#' @param ... Other arguments passed to the seewave::spectro() function
#' @param flim Set frequency limits for the spectrogram panel
#'
#' @return Todo
#'
#' @export
#' @import ggplot2 patchwork viridis
#' @importFrom tuneR readWave
#' @importFrom seewave spectro reverse.gray.colors.1 reverse.gray.colors.2
plot_detecs_i <- function(
    match_res_i,
    buffer_size = "template", min_score = NULL, min_quant = NULL, top_n = NULL,
    flim = c(0, 10), ovlp = NULL, wl = NULL, dyn_range = c(-60, 0),
    color_scale = "inferno", n_colors = 124, interpolate = FALSE,
    score_lims = NULL, ...
    ) {

    # todo Adicionar informação de pitch_shift
    
    require(patchwork)

    detecs <- fetch_score_peaks_i(
      match_res_i, buffer_size = buffer_size, min_score = min_score,
      min_quant = min_quant, top_n = top_n
    )

    rec <- readWave(filename = match_res_i$soundscape_path)

    if (color_scale %in% c("viridis", "magma", "inferno", "cividis")) {
      colormap <- viridis::viridis(n_colors, option = color_scale)
    } else if (color_scale == "greyscale 1") {
      colormap <- seewave::reverse.gray.colors.1(n_colors)
    } else if (color_scale == "greyscale 2") {
      colormap <- seewave::reverse.gray.colors.2(n_colors)
    }
    selection_color <- ifelse(
      color_scale %in% c("greyscale 1", "greyscale 2"),
      "black", "white"
    )

    if (is.null(ovlp)) {
      ovlp <- match_res_i$template_ovlp
    }
    if (is.null(wl)) {
      wl <- match_res_i$template_wl
    }
    if (is.null(flim)) {
      flim <- c(0, 10)
    }
    if (is.null(dyn_range)) {
      dyn_range <- c(-60, 0)
    }
    if (is.null(score_lims)) {
      score_lims <- range(match_res_i$score_vec[[1]]$score_vec)
    }
    if (buffer_size == "template") {
      buffer_size <- match_res_i$score_sliding_window
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
      rec,
      f = rec@samp.rate, flim = flim, ovlp = ovlp, wl = wl,
      dyn_range = dyn_range, color_scale = color_scale, n_colors = 124,
      interpolate = interpolate, ...
    ) +
      annotate(
        "rect",
        alpha = 0, linewidth = 0.2, linetype = "solid",
        color = selection_color,
        xmin = detecs$detection_start,
        xmax = detecs$detection_end,
        ymin = detecs$template_min_freq,
        ymax = detecs$template_max_freq
      ) +
      annotate(
        "label",
        label = paste0(
          match_res_i$soundscape_file,
          " (sr = ", match_res_i$soundscape_sample_rate, ")"
        ),
        x = -Inf, y = Inf, hjust = 0, vjust = 1,
        color = "white", fill = "black", size = 3
      ) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )

    plot_score <- match_res_i$score_vec[[1]] %>%
      ggplot(aes(x = time_vec, y = score_vec)) +
      annotate(
        "point",
        x = match_res_i$score_vec[[1]]$time_vec[detecs$peak_index],
        y = match_res_i$score_vec[[1]]$score_vec[detecs$peak_index],
        pch = 21, color = "black", fill = "#ff6262", size = 4
      ) +
      annotate(
        "rect",
        alpha = 0.2, linewidth = 0.5, linetype = "solid",
        fill = "red",
        xmin = detecs$detection_start,
        xmax = detecs$detection_end,
        ymin = -Inf, ymax = Inf
      ) +
      geom_line() +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(limits = c(score_lims[1], score_lims[2] + 0.1)) +
      annotate(
        "label",
        label = paste0(
          match_res_i$template_file,
          " (sr = ", match_res_i$template_sample_rate, ")"
        ),
        x = -Inf, y = Inf, hjust = 0, vjust = 1,
        color = "#000000", fill = "#ffffff", size = 3
      ) + {
        if (!is.null(min_score)) {
          geom_hline(yintercept = min_score, linetype = "dashed")
        }
      } +
      labs(
        x = "seconds", y = "matching score", caption = filter_caption
        ) +
      theme_bw()

    res <- soundscape_spectro +
      plot_score +
      plot_layout(nrow = 2, byrow = FALSE)

    return(res)
}
