# =============================================================================
# fast_spectro.R  (refactored)
# Baseline: R/original/fast_spectro.R  (canonical) + colormap clamp from
#           R/alt_segmentation_app/fast_spectro.R (FS-15).
# Phase 2 refactor applying debt items FS-01..FS-21, FS-201, FS-202.
#
# Structure (FS-19, mirrors Julia/src/visualization/fast_spectro.jl):
#   extract_spectro()  -> computes the spectrogram data (a "SpectroResult")
#   plot_spectro()     -> renders a SpectroResult into a ggplot
#   fast_spectro()     -> backward-compatible wrapper: plot_spectro(extract_*())
# Private helpers are prefixed with a dot.
# =============================================================================

#' Compute spectrogram data from a Wave object
#'
#' @description The computation half of [fast_spectro()]: it runs
#'   `seewave::spectro()` on a `Wave` recording and converts amplitude to
#'   decibels, returning the raw spectrogram data *without* drawing anything.
#'   Pass the result to [plot_spectro()] to render it. Splitting compute from
#'   render lets you cache the `SpectroResult` (e.g. with `saveRDS()`) and
#'   redraw it cheaply under different display settings.
#'
#' @details
#'   Use this half directly only when you want the numeric spectrogram (a dB
#'   matrix) or want to render the same data several ways without recomputing the
#'   FFT; for a one-shot plot, [fast_spectro()] is simpler. Restrict `flim`/`tlim`
#'   to compute only the band and time window you care about, since it is
#'   faster than cropping afterwards. Things to know: the input must be integer-PCM; float-encoded
#'   (non-PCM) WAVs are rejected with an error, because `seewave`'s per-bit-depth
#'   scaling assumes integer PCM and a float input would silently produce an
#'   all-black spectrogram. `wl` is coerced to integer and warns when it is not a
#'   power of 2 (FFT is fastest with powers of 2). The returned `power_db`
#'   matrix keeps the full measured range; the display range is applied
#'   later, in [plot_spectro()].
#'
#' @section Pipeline context:
#'   A utility used at multiple steps of the workflow, including the
#'   segmentation and validation apps and [plot_scores()]. Reads a `Wave`
#'   recording (from `tuneR::readWave()`). Produces a `SpectroResult` for
#'   [plot_spectro()].
#'
#' @param rec An object of class `"Wave"` (tuneR). Must be PCM-encoded; float
#'   WAVs are rejected (see Details).
#' @param flim A numeric vector of length 2 giving the minimum and maximum
#'   frequency limits to compute, in kHz. Must satisfy `flim[1] < flim[2]`.
#' @param tlim A numeric vector of length 2 giving the minimum and maximum
#'   time limits to compute, in seconds. Must satisfy `tlim[1] < tlim[2]`.
#' @param ovlp A numeric value specifying the percentage overlap of windows
#'   (0-100).
#' @param wl An integer FFT window length. Coerced to integer; a warning is
#'   issued when it is not a power of 2 (FFTW is fastest with powers of 2).
#' @param norm A logical; if `TRUE`, amplitude is normalised by `seewave` so
#'   the loudest bin is 0 dB and the per-bit-depth scaling is skipped.
#' @param pitch_shift A numeric display pitch factor; one of -8, -6, -4, -2, 1.
#'   The size of the number (`abs()`) sets how much the time and frequency
#'   axes are stretched. By convention, a negative sign also flips the
#'   playback direction.
#' @param channel One of `"left"`, `"right"`, `"mix"`. Selects the channel of a
#'   stereo recording (`"mix"` averages both). Ignored for mono input, except
#'   `"right"` on mono input which emits a warning.
#' @param use_fftw A logical; if `TRUE`, use the `fftw` library (via `seewave`)
#'   for the FFT. Requires the `fftw` package. Defaults to `FALSE`.
#' @param ... Additional arguments passed to `seewave::spectro()`.
#'
#' @return An object of class `"SpectroResult"`: a list with `power_db`
#'   (numeric matrix, freq x time, in dB, NOT clamped to any range),
#'   `time_vec` (seconds), `freq_vec` (kHz) and `sample_rate` (Hz, after the
#'   pitch-shift rescaling).
#'
#' @seealso [plot_spectro()], [fast_spectro()]
#' @examples
#' # Off-flow utility: compute (but do not draw) the spectrogram of a recording.
#' \dontrun{
#' # Load the package
#' library(monitoraSom)
#' # (The package ships no field recordings; synthesize one to draw.)
#' wav <- file.path(tempdir(), "rec_01.wav")
#' rec <- tuneR::normalize(tuneR::sine(4000, duration = 3 * 16000,
#'                                     samp.rate = 16000), unit = "16")
#' tuneR::writeWave(rec, wav)
#' rec <- tuneR::readWave(wav)
#'
#' sp <- extract_spectro(rec, flim = c(0, 10), ovlp = 50, wl = 1024)
#' dim(sp$power_db)   # frequency bins x time frames, in dB
#'
#' # Restrict the computed band/window to speed up and focus:
#' extract_spectro(rec, flim = c(2, 8), tlim = c(0, 2))
#' }
#' @export
extract_spectro <- function(rec, flim = NULL, tlim = NULL, ovlp = 50,
                            wl = 1024, norm = FALSE, pitch_shift = 1,
                            channel = c("left", "right", "mix"),
                            use_fftw = FALSE, ...) {
  channel <- match.arg(channel)
  .validate_extract_inputs(rec, flim, tlim, ovlp, wl, pitch_shift)

  wl <- as.integer(wl)
  if (bitwAnd(wl, wl - 1L) != 0L) {
    warning("wl is not a power of 2; FFTW performs best with powers of 2.")
  }

  rec <- .select_channel(rec, channel)

  ps <- abs(pitch_shift) # FS-03: magnitude drives the rescaling
  shifted <- .apply_pitch_shift(rec, tlim, flim, ps)
  rec <- shifted$rec
  tlim <- shifted$tlim
  flim <- .clamp_flim(shifted$flim, abs(rec@samp.rate) / 2000) # FS-04

  spec <- seewave::spectro(
    rec, f = rec@samp.rate, ovlp = ovlp, wl = wl, flim = flim, tlim = tlim,
    norm = norm, fftw = use_fftw, plot = FALSE, interpolate = FALSE, dB = NULL, ...
  )

  structure(
    list(
      power_db = .amp_to_db(spec$amp, rec@bit, norm), # FS-02
      time_vec = spec$time / ps,
      freq_vec = spec$freq * ps,
      sample_rate = rec@samp.rate
    ),
    class = "SpectroResult"
  )
}

#' Render a SpectroResult into a ggplot spectrogram
#'
#' @description The rendering half of [fast_spectro()]: it turns a
#'   `SpectroResult` (from [extract_spectro()]) into a `ggplot2` spectrogram.
#'   Rendering is roughly ten times faster than `geom_raster()`/`geom_tile()`
#'   because the dB matrix is drawn as a pre-encoded native raster via
#'   `annotation_raster()` (technique credited to Sergio Oller,
#'   <https://github.com/tidyverse/ggplot2/issues/4989>).
#'
#' @details
#'   Call this when you have already computed the data with [extract_spectro()]
#'   and want to draw it, possibly several times with different display
#'   settings
#'   (colour scale, dynamic range, theme) without recomputing the FFT. `dyn_range`
#'   sets the dB window mapped onto the colour scale: values below `dyn_range[1]`
#'   render as the "low" colour, so raise its floor to suppress background noise
#'   or lower it to reveal faint structure. Time and frequency guide lines are
#'   drawn at fixed intervals; set either interval to `0` to omit that set of
#'   guides. Note: the input must be a `SpectroResult`; passing a raw matrix or
#'   a `Wave` errors; use [fast_spectro()] to go straight from a recording.
#'
#' @section Pipeline context:
#'   A utility used across the flow, outside the numbered steps. Reads a
#'   `SpectroResult` from [extract_spectro()]. Produces a `ggplot2` spectrogram
#'   for display or further annotation.
#'
#' @param spectro A `"SpectroResult"` from [extract_spectro()].
#' @param dyn_range A numeric vector of length 2 giving the minimum and maximum
#'   relative amplitude (dB) to display.
#' @param color_scale A character colour scale: `"viridis"`, `"magma"`,
#'   `"inferno"`, `"cividis"`, `"greyscale 1"` or `"greyscale 2"`.
#' @param n_colors An integer number of colours in the scale. Smaller values
#'   render faster at lower colour resolution.
#' @param interpolate A logical passed to `annotation_raster()`.
#' @param invert_colormap A logical; if `TRUE`, reverse the colour scale so
#'   quiet regions take the "high" colour.
#' @param theme_mode A character theme: `"dark"` or `"light"`.
#' @param time_guide_interval A numeric interval between time guides (seconds).
#'   `0` disables time guides (no layer is drawn).
#' @param freq_guide_interval A numeric interval between frequency guides (kHz).
#'   `0` disables frequency guides (no layer is drawn).
#' @param font_scale A single positive numeric multiplier applied to every text
#'   element in the plot (axis text/titles and legend text/title). `1` (default)
#'   reproduces the default sizes; values `> 1` enlarge and `< 1` shrink all
#'   spectrogram text together.
#'
#' @return A `ggplot2` object.
#'
#' @seealso [extract_spectro()], [fast_spectro()]
#' @examples
#' # Off-flow utility: render a precomputed SpectroResult, re-themed cheaply.
#' \dontrun{
#' # Load the package
#' library(monitoraSom)
#' # (The package ships no field recordings; synthesize one to draw.)
#' wav <- file.path(tempdir(), "rec_01.wav")
#' rec <- tuneR::normalize(tuneR::sine(4000, duration = 3 * 16000,
#'                                     samp.rate = 16000), unit = "16")
#' tuneR::writeWave(rec, wav)
#' sp  <- extract_spectro(tuneR::readWave(wav), flim = c(0, 10))
#'
#' p <- plot_spectro(sp, color_scale = "inferno")
#' class(p)   # "gg" / "ggplot"
#'
#' # Same data, dark theme + a tighter dynamic range:
#' plot_spectro(sp, theme_mode = "dark", dyn_range = c(-80, -20))
#' }
#' @export
plot_spectro <- function(spectro, dyn_range = c(-100, -20),
                         color_scale = "inferno", n_colors = 124,
                         interpolate = FALSE, invert_colormap = FALSE,
                         theme_mode = "light", time_guide_interval = 3,
                         freq_guide_interval = 1, font_scale = 1) {
  if (!inherits(spectro, "SpectroResult")) {
    stop("spectro must be a 'SpectroResult' produced by extract_spectro()")
  }
  .validate_plot_inputs(dyn_range, color_scale, n_colors, interpolate,
                        theme_mode, time_guide_interval, freq_guide_interval,
                        invert_colormap, font_scale)

  colormap <- .build_colormap(color_scale, n_colors, invert_colormap)
  nr <- .spectro_to_raster(spectro$power_db, dyn_range, colormap)

  lims <- list(
    xmin = min(spectro$time_vec), xmax = max(spectro$time_vec),
    ymin = min(spectro$freq_vec), ymax = max(spectro$freq_vec)
  )
  guide_color <- if (grepl("greyscale", color_scale)) "black" else "white"
  theme_colors <- if (theme_mode == "dark") {
    list(text = "white", background = "black", guides = guide_color)
  } else {
    list(text = "black", background = "white", guides = guide_color)
  }
  freq_major <- .generate_guide_data(lims$ymin, lims$ymax, freq_guide_interval)
  time_major <- .generate_guide_data(lims$xmin, lims$xmax, time_guide_interval)

  .assemble_spectro_plot(nr, colormap, dyn_range, lims, theme_colors,
                         freq_major, time_major, interpolate, font_scale)
}

#' Draw a fast spectrogram of a recording
#'
#' @description Draws a `ggplot2` spectrogram directly from a `Wave` recording:
#'   the one-shot convenience wrapper that runs [extract_spectro()] (compute) and
#'   [plot_spectro()] (render) in a single call. Use it when you just want the
#'   picture; use the two halves separately when you need the numeric data or want
#'   to re-render the same computation several ways.
#'
#' @details
#'   This is the workhorse spectrogram function used throughout the package (the
#'   apps, [plot_scores()]) and the natural entry point for a quick look at a
#'   recording. Every argument is forwarded to one of the two halves, so their
#'   Details apply here too: the input must be integer-PCM (float WAVs are
#'   rejected), `flim`/`tlim` restrict the computed band/window, `dyn_range` sets
#'   the dB-to-colour window, and a guide interval of `0` omits that set of guide
#'   lines. If you plan to redraw the same recording under different colour or
#'   theme settings, call [extract_spectro()] once and feed the result to
#'   [plot_spectro()] repeatedly instead of paying for the FFT each time.
#'
#' @section Pipeline context:
#'   A utility used across the flow, outside the numbered steps. Reads a
#'   `Wave` recording (from `tuneR::readWave()`). Produces a `ggplot2`
#'   spectrogram.
#'
#' @inheritParams extract_spectro
#' @inheritParams plot_spectro
#'
#' @return A `ggplot2` object.
#'
#' @seealso [extract_spectro()], [plot_spectro()], [plot_scores()]
#' @examples
#' # Off-flow utility: the one-call spectrogram of a bundled recording.
#' \dontrun{
#' # Load the package
#' library(monitoraSom)
#' # (The package ships no field recordings; synthesize one to draw.)
#' wav <- file.path(tempdir(), "rec_01.wav")
#' rec <- tuneR::normalize(tuneR::sine(4000, duration = 3 * 16000,
#'                                     samp.rate = 16000), unit = "16")
#' tuneR::writeWave(rec, wav)
#' rec <- tuneR::readWave(wav)
#'
#' # Defaults: inferno colours, light theme, 3 s / 1 kHz guides.
#' fast_spectro(rec, flim = c(0, 10), ovlp = 50, wl = 1024)
#'
#' # Dark theme with guide lines every second and every kHz:
#' fast_spectro(rec, flim = c(0, 10), theme_mode = "dark",
#'              time_guide_interval = 1, freq_guide_interval = 1)
#'
#' # Quieter picture: raise the dyn_range floor to suppress background noise,
#' # or lower it to reveal faint sounds.
#' fast_spectro(rec, flim = c(0, 10), dyn_range = c(-70, -20))
#'
#' # Colour scales: magma, viridis, cividis, or greyscale (reversed here).
#' fast_spectro(rec, flim = c(0, 10), color_scale = "magma")
#' fast_spectro(rec, flim = c(0, 10), color_scale = "greyscale 1",
#'              invert_colormap = TRUE)
#'
#' # Zoom: compute only a 2 s window of the 2-8 kHz band (faster, sharper),
#' # with a higher window overlap for more time detail.
#' fast_spectro(rec, flim = c(2, 8), tlim = c(0, 2), ovlp = 75, wl = 512)
#'
#' # Text size: shrink all spectrogram text together (e.g. for reports).
#' fast_spectro(rec, flim = c(0, 10), font_scale = 0.8)
#' }
#' @export
fast_spectro <- function(rec, flim = NULL, tlim = NULL, ovlp = 50, wl = 1024,
                         norm = FALSE, dyn_range = c(-100, -20),
                         color_scale = "inferno", n_colors = 124,
                         interpolate = FALSE, pitch_shift = 1,
                         theme_mode = "light", time_guide_interval = 3,
                         freq_guide_interval = 1,
                         channel = c("left", "right", "mix"),
                         invert_colormap = FALSE, font_scale = 1,
                         use_fftw = FALSE, ...) {
  channel <- match.arg(channel)
  spectro <- extract_spectro(
    rec, flim = flim, tlim = tlim, ovlp = ovlp, wl = wl, norm = norm,
    pitch_shift = pitch_shift, channel = channel, use_fftw = use_fftw, ...
  )
  plot_spectro(
    spectro, dyn_range = dyn_range, color_scale = color_scale,
    n_colors = n_colors, interpolate = interpolate,
    invert_colormap = invert_colormap, theme_mode = theme_mode,
    time_guide_interval = time_guide_interval,
    freq_guide_interval = freq_guide_interval, font_scale = font_scale
  )
}

# -----------------------------------------------------------------------------
# Private helpers
# -----------------------------------------------------------------------------

#' Validate extract_spectro() inputs
#' @noRd
.validate_extract_inputs <- function(rec, flim, tlim, ovlp, wl, pitch_shift) {
  if (!inherits(rec, "Wave")) stop("rec must be a 'Wave' object")
  if (!isTRUE(rec@pcm)) {
    stop("Float-encoded (non-PCM) WAVs are not supported. ",
         "Convert to integer PCM first.")
  }
  if (!is.null(flim)) {
    if (!is.numeric(flim) || length(flim) != 2) {
      stop("flim must be a numeric vector of length 2")
    }
    if (flim[1] >= flim[2]) stop("flim must satisfy flim[1] < flim[2]")
  }
  if (!is.null(tlim)) {
    if (!is.numeric(tlim) || length(tlim) != 2) {
      stop("tlim must be a numeric vector of length 2")
    }
    if (tlim[1] >= tlim[2]) stop("tlim must satisfy tlim[1] < tlim[2]")
  }
  if (!is.numeric(ovlp) || ovlp < 0 || ovlp > 100) {
    stop("ovlp must be a numeric value between 0 and 100")
  }
  if (!is.numeric(wl) || wl <= 0) stop("wl must be a positive integer")
  if (!is.numeric(pitch_shift) || !pitch_shift %in% c(-8, -6, -4, -2, 1)) {
    stop("pitch_shift must be one of: -8, -6, -4, -2, 1")
  }
  invisible(TRUE)
}

#' Validate plot_spectro() inputs
#' @noRd
.validate_plot_inputs <- function(dyn_range, color_scale, n_colors, interpolate,
                                  theme_mode, time_guide_interval,
                                  freq_guide_interval, invert_colormap,
                                  font_scale = 1) {
  valid_scales <- c("viridis", "magma", "inferno", "cividis",
                    "greyscale 1", "greyscale 2")
  if (!is.numeric(dyn_range) || length(dyn_range) != 2) {
    stop("dyn_range must be a numeric vector of length 2")
  }
  if (!is.character(color_scale) || !color_scale %in% valid_scales) {
    stop("color_scale must be one of: ",
         paste(sprintf("'%s'", valid_scales), collapse = ", "))
  }
  if (!is.numeric(n_colors) || n_colors <= 0) {
    stop("n_colors must be a positive integer")
  }
  if (!is.logical(interpolate)) stop("interpolate must be a logical value")
  if (!is.logical(invert_colormap)) {
    stop("invert_colormap must be a logical value")
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
  if (!is.numeric(font_scale) || length(font_scale) != 1 || font_scale <= 0) {
    stop("font_scale must be a single positive numeric value")
  }
  invisible(TRUE)
}

#' Select a single channel from a (possibly stereo) Wave (FS-202)
#' @noRd
.select_channel <- function(rec, channel) {
  if (rec@stereo) {
    rec <- switch(channel,
      left = tuneR::mono(rec, "left"),
      right = tuneR::mono(rec, "right"),
      mix = tuneR::mono(rec, "both")
    )
  } else if (channel == "right") {
    warning("Recording is mono; ignoring channel = 'right'.")
  }
  rec
}

#' Apply the pitch-shift rescaling to rate, tlim and flim (FS-03)
#' @noRd
.apply_pitch_shift <- function(rec, tlim, flim, ps) {
  rec@samp.rate <- as.integer(round(rec@samp.rate / ps))
  if (!is.null(tlim)) tlim <- tlim * ps
  if (!is.null(flim)) flim <- flim / ps
  list(rec = rec, tlim = tlim, flim = flim)
}

#' Clamp `flim` to `[0, Nyquist]` keeping `flim[1] < flim[2]` (FS-04)
#' @noRd
.clamp_flim <- function(flim, nyquist_khz) {
  if (is.null(flim)) return(NULL)
  eps <- 1e-3 # 1 Hz expressed in kHz
  fmin <- max(0, flim[1])
  # seewave::spectro errors when flim[2] == nyquist_khz exactly;
  # subtract eps from the ceiling to stay strictly below Nyquist.
  fmax <- min(nyquist_khz - eps, flim[2])
  fmax <- max(fmin + eps, fmax)
  c(fmin, fmax)
}

#' Convert seewave amplitude to dB with a floor against log10(0) (FS-02)
#' @noRd
.amp_to_db <- function(amp, bit, norm) {
  if (!norm) amp <- amp / 2^(bit - 1)
  20 * log10(pmax(amp, 1e-30))
}

#' Build the colour ramp, optionally reversed (FS-201)
#' @noRd
.build_colormap <- function(color_scale, n_colors, invert_colormap) {
  colormap <- switch(color_scale,
    "viridis" = viridisLite::viridis(n_colors),
    "magma" = viridisLite::magma(n_colors),
    "inferno" = viridisLite::inferno(n_colors),
    "cividis" = viridisLite::cividis(n_colors),
    "greyscale 1" = seewave::reverse.gray.colors.1(n_colors),
    "greyscale 2" = seewave::reverse.gray.colors.2(n_colors),
    viridisLite::viridis(n_colors)
  )
  if (invert_colormap) colormap <- rev(colormap)
  colormap
}

#' Encode the dB matrix into a nativeRaster (FS-02/FS-15 index clamp)
#' @noRd
.spectro_to_raster <- function(power_db, dyn_range, colormap) {
  mat <- matrix(
    pmax(pmin(power_db, dyn_range[2]), dyn_range[1]), nrow = nrow(power_db)
  )
  mat <- t(mat)
  cols_to_ints <- farver::encode_native(colormap)
  breaks <- seq(dyn_range[1], dyn_range[2], length.out = length(colormap))
  xdim <- dim(mat)
  mat <- mat[, rev(seq_len(ncol(mat)))]
  idx <- findInterval(mat, breaks, rightmost.closed = TRUE)
  idx <- pmax(1, pmin(length(colormap), idx)) # guard against index 0 / overflow
  nr <- matrix(cols_to_ints[idx], nrow = xdim[1], ncol = xdim[2], byrow = FALSE)
  structure(
    nr, dim = c(xdim[2], xdim[1]), class = "nativeRaster", channels = 4L
  )
}

#' Major guide positions for one axis; empty when disabled (FS-11)
#' @noRd
.generate_guide_data <- function(limit_min, limit_max, guide_interval) {
  if (guide_interval == 0) return(numeric(0))
  major <- seq(
    floor(limit_min / guide_interval) * guide_interval,
    ceiling(limit_max / guide_interval) * guide_interval,
    by = guide_interval
  )
  major[major >= limit_min & major <= limit_max]
}

#' Pretty axis breaks adapted to the displayed range
#' @noRd
.calculate_breaks <- function(min_val, max_val) {
  if (is.null(min_val) || is.null(max_val) ||
    is.na(min_val) || is.na(max_val)) {
    return(NULL)
  }
  rng <- abs(max_val - min_val)
  if (rng <= .Machine$double.eps) return(min_val)
  interval <- if (rng <= 0.1) 0.01 else if (rng <= 0.5) 0.05 else
    if (rng <= 1) 0.1 else if (rng <= 5) 0.5 else if (rng <= 10) 1 else
      ceiling(rng / 10)
  breaks <- seq(
    floor(min_val / interval) * interval,
    ceiling(max_val / interval) * interval, by = interval
  )
  breaks[breaks >= min_val & breaks <= max_val]
}

#' Format axis tick labels with range-dependent precision
#' @noRd
.format_axis_labels <- function(x) {
  if (length(x) == 0) return(NULL)
  rng <- diff(range(x))
  decimals <- if (rng <= 0.1) 3 else if (rng <= 5) 2 else
    if (rng <= 10) 1 else 0
  format(round(x, decimals), nsmall = decimals)
}

#' ggplot2 theme block for the spectrogram
#'
#' @param font_scale multiplier applied to every text element's size (FS-203 /
#'   LSA-100 Phase B). The base sizes are the previous fixed/default values, so
#'   `font_scale = 1` reproduces the original rendering exactly: axis text 10,
#'   axis title 12, legend text 8.8 (the theme_grey rel(0.8) default), legend
#'   title 11 (the theme_grey default).
#' @noRd
.spectro_theme <- function(theme_colors, font_scale = 1) {
  ggplot2::theme(
    axis.text = ggplot2::element_text(
      color = theme_colors$text, size = 10 * font_scale
    ),
    axis.title = ggplot2::element_text(
      color = theme_colors$text, size = 12 * font_scale
    ),
    legend.text = ggplot2::element_text(
      color = theme_colors$text, size = 8.8 * font_scale
    ),
    legend.title = ggplot2::element_text(
      color = theme_colors$text, size = 11 * font_scale
    ),
    panel.background = ggplot2::element_rect(fill = theme_colors$background),
    plot.background = ggplot2::element_rect(fill = theme_colors$background),
    panel.grid = ggplot2::element_blank(),
    legend.background = ggplot2::element_rect(fill = theme_colors$background)
  )
}

#' Assemble the ggplot from the raster and guide layers
#' @noRd
.assemble_spectro_plot <- function(nr, colormap, dyn_range, lims, theme_colors,
                                   freq_major, time_major, interpolate,
                                   font_scale = 1) {
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      data = data.frame(x = NA_real_), ggplot2::aes(fill = x),
      xmin = lims$xmin, xmax = lims$xmax, ymin = lims$ymin, ymax = lims$ymax
    ) +
    ggplot2::annotation_raster(
      nr, interpolate = interpolate,
      xmin = lims$xmin, xmax = lims$xmax, ymin = lims$ymin, ymax = lims$ymax
    )
  if (length(freq_major) > 0) {
    p <- p + ggplot2::geom_segment(
      data = data.frame(y = freq_major, xmin = lims$xmin, xmax = lims$xmax),
      ggplot2::aes(x = xmin, xend = xmax, y = y, yend = y),
      color = theme_colors$guides, alpha = 0.4, linewidth = 0.3
    )
  }
  if (length(time_major) > 0) {
    p <- p + ggplot2::geom_segment(
      data = data.frame(x = time_major, ymin = lims$ymin, ymax = lims$ymax),
      ggplot2::aes(x = x, xend = x, y = ymin, yend = ymax),
      color = theme_colors$guides, alpha = 0.4, linewidth = 0.3
    )
  }
  p <- p +
    ggplot2::scale_fill_gradientn(
      colors = colormap, limits = dyn_range, na.value = "#00000000"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(lims$xmin, lims$xmax), ylim = c(lims$ymin, lims$ymax)
    ) +
    ggplot2::scale_x_continuous(
      expand = c(0, 0),
      breaks = function(l) .calculate_breaks(l[1], l[2]),
      labels = .format_axis_labels
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      breaks = function(l) .calculate_breaks(l[1], l[2]),
      labels = .format_axis_labels
    ) +
    ggplot2::labs(x = "seconds", y = "kHz", fill = "dB") +
    .spectro_theme(theme_colors, font_scale = font_scale)
  suppressMessages(p)
}
