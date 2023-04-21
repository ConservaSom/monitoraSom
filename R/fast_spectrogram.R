#' Title
#'
#' @param rec
#' @param wl
#' @param flim
#' @param ovlp
#' @param zp
#' @param dyn.range
#' @param legend
#' @param cores
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
fast_spectrogram <- function(rec, wl = 512, flim = c(0, 15), ovlp = 70, zp = 2,
                         dyn.range = NULL, legend = "none", cores = "B", ...) {
  res <- suppressMessages(
    seewave::ggspectro(rec,
      wl = wl, flim = flim, ovlp = ovlp, zp = zp,
      norm = T, ...
    ) +
      geom_raster(aes(fill = amplitude),
        hjust = 0, vjust = 0,
        interpolate = F
      ) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_viridis_c(
        na.value = "black", name = "Amplitude \n (dB)",
        option = cores, limits = dyn.range
      ) +
      theme(legend.position = legend)
  )

  return(res)
}
