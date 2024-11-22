#' Fast spectrogram rendering with ggplot2
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function creates a spectrogram plot using ggplot2 nearly ten times
#'   faster by introducing the spectrogram layer with 'annotation_raster'
#'   instead of 'geom_raster' or 'geom_tile'. Credits to Sergio Oller in
#'   <https://github.com/tidyverse/ggplot2/issues/4989>
#'
#' @param rec An object of class "Wave" as implemented in the tuneR package
#' @param f The sampling frequency of the recording, in Hz
#' @param flim A numeric vector of length 2 giving the minimum and maximum
#'   frequency limits to be displayed in the spectrogram, in kHz.
#' @param tlim A numeric vector of length 2 giving the minimum and maximum.
#' @param ovlp A numeric value specifying the percentage overlap of windows for
#'   computing the spectrogram.
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
#' @param pitch_shift A numeric value indicating the pitch shift to be applied
#'   to the recording. The value must be among the expected alternatives: -8,
#' -6, -4, -2, or 1.
#' @param theme_mode A character string indicating the theme mode to be used in
#'   the spectrogram. Possible values are "dark" or "light".
#' @param time_guide_interval A numeric value indicating the interval between
#'   time guides in seconds.
#' @param freq_guide_interval A numeric value indicating the interval between
#'   frequency guides in kHz.
#' @param norm A logical value indicating whether the amplitude values should be
#'   normalized or not.
#' @param ... Additional arguments to be passed internally to the
#'   'seeave::spectro' function.
#'
#' @import dplyr viridis farver ggplot2
#' @importFrom seewave spectro
#' @return This function returns a ggplot2 object.
#'
#' @export
fast_spectro <- function(
    rec, f, flim = NULL, tlim = NULL,
    ovlp = 50, wl = 1024, norm = FALSE, dyn_range = c(-80, 0),
    color_scale = "inferno", n_colors = 124, interpolate = FALSE,
    pitch_shift = 1, theme_mode = "light",
    time_guide_interval = 3, freq_guide_interval = 1, ...) {
  validate_inputs <- function() {
    if (!inherits(rec, "Wave")) {
      stop("rec must be a 'Wave' object")
    }
    if (!is.numeric(f) || f <= 0) {
      stop("f must be a positive numeric value")
    }
    if (!is.null(flim) && (!is.numeric(flim) || length(flim) != 2)) {
      stop("flim must be a numeric vector of length 2")
    }
    if (!is.null(tlim) && (!is.numeric(tlim) || length(tlim) != 2)) {
      stop("tlim must be a numeric vector of length 2")
    }
    if (!is.numeric(ovlp) || ovlp < 0 || ovlp > 100) {
      stop("ovlp must be a numeric value between 0 and 100")
    }
    if (!is.numeric(wl) || wl <= 0) {
      stop("wl must be a positive integer")
    }
    if (!is.numeric(dyn_range) || length(dyn_range) != 2) {
      stop("dyn_range must be a numeric vector of length 2")
    }
    if (
      !is.character(color_scale) ||
        !color_scale %in% c(
          "viridis", "magma", "inferno", "cividis", "greyscale 1", "greyscale 2"
        )
    ) {
      stop(
        "color_scale must be one of: 'viridis', 'magma', 'inferno', 'cividis', 'greyscale 1', 'greyscale 2'"
      )
    }
    if (!is.numeric(n_colors) || n_colors <= 0) {
      stop("n_colors must be a positive integer")
    }
    if (!is.logical(interpolate)) {
      stop("interpolate must be a logical value")
    }
    if (!is.numeric(pitch_shift) || !pitch_shift %in% c(-8, -6, -4, -2, 1)) {
      stop("pitch_shift must be one of: -8, -6, -4, -2, 1")
    }
    if (!is.character(theme_mode) || !theme_mode %in% c("dark", "light")) {
      stop("theme_mode must be 'dark' or 'light'")
    }
    if (!is.numeric(time_guide_interval) || time_guide_interval < 0) {
      stop("time_guide_interval must be a non-negative numeric value")
    }
    if (!is.numeric(freq_guide_interval) || freq_guide_interval < 0) {
      stop("freq_guide_interval must be a non-negative numeric value")
    }
  }

  validate_inputs()

  apply_pitch_shift <- function(rec, tlim, flim, pitch_shift) {
    rec@samp.rate <- rec@samp.rate / pitch_shift
    if (!is.null(tlim)) tlim <- tlim * pitch_shift
    if (!is.null(flim)) flim <- flim / pitch_shift
    list(rec = rec, tlim = tlim, flim = flim)
  }

  adjusted <- apply_pitch_shift(rec, tlim, flim, pitch_shift)
  rec <- adjusted$rec
  tlim <- adjusted$tlim
  flim <- adjusted$flim

  spec_raw <- seewave::spectro(
    rec,
    f = rec@samp.rate, ovlp = ovlp, wl = wl, flim = flim, tlim = tlim,
    norm = norm, fftw = TRUE, plot = FALSE, interpolate = FALSE, dB = NULL, ...
  )

  spec_raw$time <- spec_raw$time / pitch_shift
  spec_raw$freq <- spec_raw$freq * pitch_shift

  if (!norm) {
    spec_raw$amp <- spec_raw$amp / 2^(rec@bit - 1)
  }
  spec_raw$amp <- 20 * log10(spec_raw$amp)

  mat <- matrix(
    pmax(pmin(spec_raw$amp, dyn_range[2]), dyn_range[1]),
    nrow = nrow(spec_raw$amp)
  )
  mat <- t(mat)

  colormap <- switch(color_scale,
    "viridis" = viridis::viridis(n_colors),
    "magma" = viridis::magma(n_colors),
    "inferno" = viridis::inferno(n_colors),
    "cividis" = viridis::cividis(n_colors),
    "greyscale 1" = seewave::reverse.gray.colors.1(n_colors),
    "greyscale 2" = seewave::reverse.gray.colors.2(n_colors),
    viridis::viridis(n_colors)
  )
  cols_to_ints <- farver::encode_native(colormap)
  breaks <- seq(dyn_range[1], dyn_range[2], length.out = length(colormap))
  xdim <- dim(mat)
  rev_cols <- rev(seq_len(ncol(mat)))
  mat <- mat[, rev_cols]
  mat <- findInterval(mat, breaks, rightmost.closed = TRUE)
  mat <- cols_to_ints[mat]
  nr <- matrix(mat, nrow = xdim[1], ncol = xdim[2], byrow = FALSE)
  nr <- structure(
    nr,
    dim = c(xdim[2], xdim[1]), class = "nativeRaster", channels = 4L
  )

  plot_limits <- list(
    xmin = min(spec_raw$time), xmax = max(spec_raw$time),
    ymin = min(spec_raw$freq), ymax = max(spec_raw$freq)
  )
  guide_color <- if (grepl("greyscale", color_scale)) {
    "black"
  } else {
    "white"
  }
  theme_colors <- if (theme_mode == "dark") {
    list(text = "white", background = "black", guides = guide_color)
  } else {
    list(text = "black", background = "white", guides = guide_color)
  }
  generate_guide_data <- function(limit_min, limit_max, guide_interval) {
    if (guide_interval == 0) {
        return(numeric(0))  # Return an empty numeric vector if interval is 0
    }
    major <- seq(
      floor(limit_min / guide_interval) * guide_interval,
      ceiling(limit_max / guide_interval) * guide_interval,
      by = guide_interval
    )
    major[major >= limit_min & major <= limit_max]
  }

  # Set default intervals for when guides are disabled
  default_time_interval <- 3
  default_freq_interval <- 1

  # Generate guide data with default intervals when user sets them to 0
  freq_major <- generate_guide_data(
    plot_limits$ymin,
    plot_limits$ymax,
    if (freq_guide_interval == 0) default_freq_interval else freq_guide_interval
  )

  time_major <- generate_guide_data(
    plot_limits$xmin,
    plot_limits$xmax,
    if (time_guide_interval == 0) default_time_interval else time_guide_interval
  )

  calculate_nice_breaks <- function(min_val, max_val) {
    if (is.null(min_val) || is.null(max_val) ||
      is.na(min_val) || is.na(max_val)) {
      return(NULL)
    }
    range <- abs(max_val - min_val)
    if (range <= .Machine$double.eps) {
      return(min_val)
    }
    interval <- if (range <= 0.1) {
      0.01
    } else if (range <= 0.5) {
      0.05
    } else if (range <= 1) {
      0.1
    } else if (range <= 5) {
      0.5
    } else if (range <= 10) {
      1
    } else {
      ceiling(range / 10)
    }
    breaks <- seq(
      floor(min_val / interval) * interval,
      ceiling(max_val / interval) * interval,
      by = interval
    )
    breaks[breaks >= min_val & breaks <= max_val]
  }

  suppressMessages(
    ggplot() +
      geom_rect(
        data = data.frame(x = NA_real_),
        aes(fill = x),
        xmin = plot_limits$xmin, xmax = plot_limits$xmax,
        ymin = plot_limits$ymin, ymax = plot_limits$ymax
      ) +
      annotation_raster(
        nr,
        interpolate = interpolate,
        xmin = plot_limits$xmin, xmax = plot_limits$xmax,
        ymin = plot_limits$ymin, ymax = plot_limits$ymax
      ) +
      # Major frequency guides (always present, alpha controlled)
      geom_segment(
        data = data.frame(y = freq_major, xmin = plot_limits$xmin, xmax = plot_limits$xmax),
        aes(x = xmin, xend = xmax, y = y, yend = y),
        color = theme_colors$guides,
        alpha = if (freq_guide_interval == 0) 0 else 0.4,
        size = 0.3
      ) +
      # Major time guides
      geom_segment(
        data = data.frame(x = time_major, ymin = plot_limits$ymin, ymax = plot_limits$ymax),
        aes(x = x, xend = x, y = ymin, yend = ymax),
        color = theme_colors$guides,
        alpha = if (time_guide_interval == 0) 0 else 0.4,
        size = 0.3
      ) +
      scale_fill_gradientn(
        colors = colormap, limits = dyn_range, na.value = "#00000000"
      ) +
      coord_cartesian(
        xlim = c(plot_limits$xmin, plot_limits$xmax),
        ylim = c(plot_limits$ymin, plot_limits$ymax)
      ) +
      scale_x_continuous(
        expand = c(0, 0),
        breaks = function(limits) {
          calculate_nice_breaks(limits[1], limits[2])
        },
        labels = function(x) {
          if (length(x) == 0) {
            return(NULL)
          }
          range <- diff(range(x))
          decimals <- if (range <= 0.1) {
            3
          } else if (range <= 5) {
            2
          } else if (range <= 10) {
            1
          } else {
            0
          }
          format(round(x, decimals), nsmall = decimals)
        }
      ) +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = function(limits) {
          calculate_nice_breaks(limits[1], limits[2])
        },
        labels = function(x) {
          if (length(x) == 0) {
            return(NULL)
          }
          range <- diff(range(x))
          decimals <- if (range <= 0.1) {
            3
          } else if (range <= 5) {
            2
          } else if (range <= 10) {
            1
          } else {
            0
          }
          format(round(x, decimals), nsmall = decimals)
        }
      ) +
      labs(x = "seconds", y = "kHz", fill = "dB") +
      theme(
        axis.text = element_text(color = theme_colors$text, size = 10),
        axis.title = element_text(color = theme_colors$text, size = 12),
        legend.text = element_text(color = theme_colors$text),
        legend.title = element_text(color = theme_colors$text),
        panel.background = element_rect(fill = theme_colors$background),
        plot.background = element_rect(fill = theme_colors$background),
        panel.grid = element_blank(),
        legend.background = element_rect(fill = theme_colors$background)
      )
  )
}
