#' Title
#'
#' @param tpmatch_detecs_i
#' @param corr_cut
#' @param wl
#' @param flim
#' @param dyn.range
#' @param ovlp
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
