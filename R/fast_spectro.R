#' Fast spectrogram rendering with ggplot2
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function creates a spectrogram plot using ggplot2 nearly ten times
#' faster by introducing the spectrogram layer with 'annotation_raster' instead
#' of 'geom_raster' or 'geom_tile'. Credits to Sergio Oller in
#' <https://github.com/tidyverse/ggplot2/issues/4989>
#'
#' @param rec An object of class "Wave" as implemented in the tuneR package
#' @param f The sampling frequency of the recording, in Hz
#' @param flim A numeric vector of length 2 giving the minimum and maximum
#'   frequency limits to be displayed in the spectrogram, in kHz.
#' @param ovlp A numeric value specifying the percentage overlap of windows for computing the spectrogram.
#' @param wl An integer specifying the length of the FFT window used to
#'   calculate the spectrogram.
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
#' @param ... Additional arguments to be passed internally to the 'spectro'
#'   function.
#'
#' @import dplyr viridis farver ggplot2
#' @importFrom seewave spectro
#' @return This function returns a ggplot2 object.
#'
#' @export
fast_spectro <- function(
    rec, f, flim = c(0, 10), tlim = NULL,
    ovlp = 50, wl = 1024, dyn_range = c(-60, 0),
    color_scale = "inferno", n_colors = 124, interpolate = FALSE,
    pitch_shift = 1, ...
    ) {

    if (is.numeric(pitch_shift)) {
      if (pitch_shift %in% c(-8, -6, -4, -2, 1)) {
        rec@samp.rate <- rec@samp.rate / pitch_shift
        if (!is.null(tlim)) {
          tlim <- tlim * pitch_shift
        }
        if (!is.null(flim)) {
          flim <- flim / pitch_shift
        }
      } else {
        stop(
          "Error! The value assigned to 'pitch_shift' is not among the expected alternatives: -8, -6, -4, -2, or 1."
        )
      }
    } else {
      stop(
        "Error! The value assigned to 'pitch_shift' is not among the expected alternatives: -8, -6, -4, -2, or 1."
      )
    }

  spec_raw <- seewave::spectro(
    rec, f = rec@samp.rate,
    ovlp = ovlp, wl = wl, flim = flim, tlim = tlim,
    norm = TRUE, fftw = FALSE, plot = FALSE, interpolate = FALSE, ...
  )
  spec_raw$time <- spec_raw$time / pitch_shift
  spec_raw$freq <- spec_raw$freq * pitch_shift

  # mat <- pmax(pmin(spec_raw$amp, dyn_range[2]), dyn_range[1]) # ou...
  mat <- spec_raw$amp
  mat[mat < dyn_range[1]] <- dyn_range[1]
  mat[mat > dyn_range[2]] <- dyn_range[2]

  mat <- t(mat)

  amp_range <- range(mat)
  if (color_scale %in% c("viridis", "magma", "inferno", "cividis")) {
    colormap <- viridis::viridis(n_colors, option = color_scale)
  } else if (color_scale == "greyscale 1") {
    colormap <- seewave::reverse.gray.colors.1(n_colors)
  } else if (color_scale == "greyscale 2") {
    colormap <- seewave::reverse.gray.colors.2(n_colors)
  }

  cols_to_ints <- farver::encode_native(colormap)
  breaks <- seq(
    from = amp_range[1], to = amp_range[2], length.out = length(colormap)
  )
  xdim <- dim(mat)
  rev_cols <- seq.int(ncol(mat), 1L, by = -1L)
  mat <- mat[, rev_cols]
  mat <- findInterval(mat, breaks, rightmost.closed = TRUE)
  mat <- cols_to_ints[mat]
  nr <- matrix(mat, nrow = xdim[1], ncol = xdim[2], byrow = FALSE)
  nr <- structure(
    nr,
    dim = c(xdim[2], xdim[1]), class = "nativeRaster", channels = 4L
  )

  xmin <- min(spec_raw$time)
  xmax <- max(spec_raw$time)
  ymin <- min(spec_raw$freq)
  ymax <- max(spec_raw$freq)
  x <- NULL

  suppressMessages(
    ggplot() +
      geom_rect(
        data = data.frame(x = NA_real_),
        mapping = aes(fill = x),
        xmin = xmin, xmax = xmax,
        ymin = ymin, ymax = ymax
      ) +
      annotation_raster(
        nr,
        interpolate = interpolate,
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ) +
      scale_fill_gradientn(
        colors = colormap, limits = amp_range, na.value = "#00000000"
      ) +
      scale_x_continuous(limits = c(xmin, xmax), expand = c(0, 0)) +
      scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +
      labs(x = "seconds", y = "kHz", fill = "dB")
  )
}
