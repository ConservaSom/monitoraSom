#' plot_match_i - Plots the results of the match_i function
#'
#' @param tpmatch_detecs_i The output of the function tpmatch::match_i
#' @param corr_cut The correlation threshold above which to plot detected templates
#' @param wl The length of the window (in samples) used for the STFT calculation
#' @param flim A vector of length 2 specifying the frequency limits (in kHz) of the plot
#' @param dyn.range A vector of length 2 specifying the dynamic range (in dB) of the plot
#' @param ovlp The overlap (in %) between adjacent windows used for the STFT calculation
#' @param ... Other arguments passed to the sonograma function from the seewave package
#'
#' @return A plot of the spectrogram with detected templates highlighted
#' @export
#'
#' @examples
#' plot_match_i(tpmatch_detecs_i, corr_cut = 0.4, wl = 2048, flim = c(0, 20),
#'               dyn.range = c(-100, -20), ovlp = 75)
plot_match_i <- function(
    match_i_res,
    buffer_size = "template", min_score = NULL, min_quant = NULL, top_n = NULL,
    flim = c(0, 10), ovlp = NULL, wl = NULL, dyn_range = c(-60, 0),
    color_scale = "inferno", n_colors = 124, interpolate = FALSE,
    score_lims = NULL, ...
    ) {

    require(patchwork)

    detecs <- fetch_score_peaks_i(
      match_i_res, buffer_size = buffer_size, min_score = min_score,
      min_quant = min_quant, top_n = top_n
    )



    rec <- readWave(filename = match_i_res$soundscape_path)

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
      ovlp <- match_i_res$template_ovlp
    }
    if (is.null(wl)) {
      wl <- match_i_res$template_wl
    }
    if (is.null(flim)) {
      flim <- c(0, 10)
    }
    if (is.null(dyn_range)) {
      dyn_range <- c(-60, 0)
    }
    if (is.null(score_lims)) {
      score_lims <- range(match_i_res$score_vec[[1]]$score_vec)
    }

    soundscape_spectro <- fast_spectro(
      rec,
      f = rec@samp.rate,
      flim = flim, ovlp = ovlp, wl = wl, dyn_range = dyn_range,
      color_scale = color_scale, n_colors = 124, interpolate = interpolate
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
          match_i_res$soundscape_file,
          " (sr = ", match_i_res$soundscape_sample_rate, ")"
        ),
        x = -Inf, y = Inf, hjust = 0, vjust = 1,
        color = "white", fill = "black", size = 3
      ) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )

    plot_score <- match_i_res$score_vec[[1]] %>%
      ggplot(aes(x = time_vec, y = score_vec)) +
      annotate(
        "point",
        x = match_i_res$score_vec[[1]]$time_vec[detecs$peak_index],
        y = match_i_res$score_vec[[1]]$score_vec[detecs$peak_index],
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
          match_i_res$template_file,
          " (sr = ", match_i_res$template_sample_rate, ")"
        ),
        x = -Inf, y = Inf, hjust = 0, vjust = 1,
        color = "#000000", fill = "#ffffff", size = 3
      ) +
      labs(x = "seconds", y = "matching score") +
      theme_bw()

    # todo Mostrar os critérios aqui
      # todo Adicionar uma trave para min_score
      # todo Adicionar mostradores para top_n e min_quant

    res <- soundscape_spectro +
      plot_score +
      plot_layout(nrow = 2, byrow = FALSE)
    return(res)
}