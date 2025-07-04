% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fast_spectro.R
\name{fast_spectro}
\alias{fast_spectro}
\title{Fast spectrogram rendering with ggplot2}
\usage{
fast_spectro(
  rec,
  f,
  flim = NULL,
  tlim = NULL,
  ovlp = 50,
  wl = 1024,
  norm = FALSE,
  dyn_range = c(-80, 0),
  color_scale = "inferno",
  n_colors = 124,
  interpolate = FALSE,
  pitch_shift = 1,
  theme_mode = "light",
  time_guide_interval = 3,
  freq_guide_interval = 1,
  ...
)
}
\arguments{
\item{rec}{An object of class "Wave" as implemented in the tuneR package}

\item{f}{The sampling frequency of the recording, in Hz}

\item{flim}{A numeric vector of length 2 giving the minimum and maximum
frequency limits to be displayed in the spectrogram, in kHz.}

\item{tlim}{A numeric vector of length 2 giving the minimum and maximum.}

\item{ovlp}{A numeric value specifying the percentage overlap of windows for
computing the spectrogram.}

\item{wl}{An integer specifying the length of the FFT window used to
calculate the spectrogram.}

\item{norm}{A logical value indicating whether the amplitude values should be
normalized or not.}

\item{dyn_range}{A numeric vector of length 2 giving the minimum and maximum
values of relative amplitude to be displayed in the spectrogram.}

\item{color_scale}{A character string specifying the color scale to be used
in the spectrogram. Possible values are "viridis", "magma", "inferno",
"cividis", "greyscale 1", or "greyscale 2".}

\item{n_colors}{An integer specifying the number of colors to be used in the
color scale. Smaller values will result in lower resolution color scales,
but will also result in faster rendering times.}

\item{interpolate}{A logical value indicating whether the raster should be
interpolated or not.}

\item{pitch_shift}{A numeric value indicating the pitch shift to be applied
  to the recording. The value must be among the expected alternatives: -8,
-6, -4, -2, or 1.}

\item{theme_mode}{A character string indicating the theme mode to be used in
the spectrogram. Possible values are "dark" or "light".}

\item{time_guide_interval}{A numeric value indicating the interval between
time guides in seconds.}

\item{freq_guide_interval}{A numeric value indicating the interval between
frequency guides in kHz.}

\item{...}{Additional arguments to be passed internally to the
'seeave::spectro' function.}
}
\value{
This function returns a ggplot2 object.
}
\description{
`r lifecycle::badge("experimental")`

  This function creates a spectrogram plot using ggplot2 nearly ten times
  faster by introducing the spectrogram layer with 'annotation_raster'
  instead of 'geom_raster' or 'geom_tile'. Credits to Sergio Oller in
  <https://github.com/tidyverse/ggplot2/issues/4989>
}
\examples{
\dontrun{
# Load the necessary packages to run this example

library(monitoraSom)
library(dplyr)

# Load the soundscape list stored in the package to populate the example data
data(ls_soundscapes)

# Get the second soundscape in the list
soundscape_2 <- ls_soundscapes[[2]]
glimpse(soundscape_2)

# Render the spectrogram
fast_spectro(
  rec = soundscape_2, f = soundscape_2@samp.rate,
  flim = c(0, 10), tlim = c(10, 30), ovlp = 90, wl = 2048,
  dyn_range = c(-80, -40), color_scale = "inferno", theme_mode = "dark",
  time_guide_interval = 1, freq_guide_interval = 1
)

}
}
