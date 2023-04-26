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
    tpmatch_detecs_i, corr_cut = 0.3, wl = 1024, flim = c(0, 12),
    dyn.range = c(-80, -10), ovlp = 50, ...) {

  detecs <- tpmatch_detecs_i$detecs[[1]]
  wav_file <- readWave(tpmatch_detecs_i$query_file)

  res_plot <- sonograma(
    wav_file, qdet = qdet, wl = wl, flim = flim, dyn.range = dyn.range,
    ovlp = ovlp, ...) +
    annotate(
      "rect",
      xmin = detecs$start[which(detecs$peak_cor > corr_cut)],
      xmax = detecs$end[which(detecs$peak_cor > corr_cut)],
      ymin = detecs$min_freq[which(detecs$peak_cor > corr_cut)],
      ymax = detecs$max_freq[which(detecs$peak_cor > corr_cut)],
      color = "white", linetype = "dashed", alpha = 0) +
    labs(x = "Tempo (s)", y = "Frequência (kHz)")

  return(res_plot)
}
