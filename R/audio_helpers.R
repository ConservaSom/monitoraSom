#' Audio helpers for the segmentation app (LSA-05)
#'
#' @description Audio-side helpers extracted from the
#'   `launch_segmentation_app()` closures (LSA-05). Behaviour is preserved
#'   unchanged from the alt app so the extraction is reviewable as mechanical,
#'   except for the LSA-14 hardening of `extract_acoustic_measurements` (select
#'   the `acoustat` percentiles by name with a guard; see below).
#'
#' @keywords internal
#' @noRd

#' Cut a `[start_time, end_time]` segment (seconds) from a tuneR Wave.
#'
#' Preserves the alt app `create_audio_segment`: clamps the sample window to
#' valid bounds and keeps the stereo/mono layout, sample rate and bit depth.
#'
#' @param wav_data a `tuneR::Wave`.
#' @param start_time,end_time segment bounds in seconds.
#' @return a `tuneR::Wave` of the requested segment.
#' @noRd
.create_audio_segment <- function(wav_data, start_time, end_time) {
  start_sample <- max(1, round(start_time * wav_data@samp.rate) + 1)
  end_sample <- min(length(wav_data@left), round(end_time * wav_data@samp.rate))
  if (wav_data@stereo) {
    tuneR::Wave(
      left = wav_data@left[start_sample:end_sample],
      right = wav_data@right[start_sample:end_sample],
      samp.rate = wav_data@samp.rate, bit = wav_data@bit
    )
  } else {
    tuneR::Wave(
      left = wav_data@left[start_sample:end_sample],
      samp.rate = wav_data@samp.rate, bit = wav_data@bit
    )
  }
}

#' Ruler acoustic measurements over a ROI window.
#'
#' Preserves the alt app `extract_acoustic_measurements`: mean dominant
#' frequency plus the 10/90% time and frequency percentiles from
#' `seewave::acoustat`. LSA-14: the percentiles are selected **by name**
#' (`time.P1/P2`, `freq.P1/P2`) directly off the `acoustat` result, dropping the
#' fragile positional `[-c(1, 2)]` element-drop, and guarded so a `seewave` API
#' change surfaces an informative error (caught by the caller into a user-visible
#' notice) instead of producing `NULL` percentiles silently.
#'
#' @param rec a `tuneR::Wave`.
#' @param ruler_data list/one-row df with `roi_start`, `roi_end`,
#'   `roi_min_freq`, `roi_max_freq` (kHz for the freq fields).
#' @param wl,ovlp spectrogram window length and overlap.
#' @return list with `dom_freq` and `ac_stats` (`t10`, `t90`, `f10`, `f90`).
#' @noRd
.extract_acoustic_measurements <- function(rec, ruler_data, wl, ovlp) {
  snd_selection <- seewave::cutw(
    rec,
    f = rec@samp.rate,
    from = ruler_data$roi_start, to = ruler_data$roi_end,
    units = "seconds", output = "Wave"
  )
  dom_freq <- mean(
    seewave::dfreq(
      snd_selection,
      f = snd_selection@samp.rate, wl = wl, ovlp = ovlp,
      bandpass = c(ruler_data$roi_min_freq, ruler_data$roi_max_freq) * 1000,
      plot = FALSE
    )[, 2]
  )
  ac_stats <- seewave::acoustat(
    snd_selection,
    f = snd_selection@samp.rate, wl = wl, ovlp = ovlp, fraction = 80,
    flim = c(ruler_data$roi_min_freq, ruler_data$roi_max_freq),
    plot = FALSE
  )
  # Select the percentiles by name, not position (LSA-14). Guard against a
  # seewave API change that would drop/rename these fields.
  needed <- c("time.P1", "time.P2", "freq.P1", "freq.P2")
  missing <- needed[!needed %in% names(ac_stats)]
  if (length(missing) > 0) {
    stop(sprintf(
      "seewave::acoustat did not return expected field(s): %s",
      paste(missing, collapse = ", ")
    ))
  }
  list(
    dom_freq = dom_freq,
    ac_stats = data.frame(
      t10 = ac_stats$time.P1, t90 = ac_stats$time.P2,
      f10 = ac_stats$freq.P1, f90 = ac_stats$freq.P2
    )
  )
}

#' Remove stale `.wav` temp files, keeping the current one.
#'
#' Preserves the alt app `safe_cleanup_temp_files`.
#'
#' @param temp_path directory holding temp segment files (may be NULL/missing).
#' @param current_file full path of the file to keep. Default NULL.
#' @return invisibly NULL.
#' @noRd
.safe_cleanup_temp_files <- function(temp_path, current_file = NULL) {
  if (is.null(temp_path) || !dir.exists(temp_path)) return(invisible(NULL))
  temp_files <- list.files(
    temp_path, pattern = "\\.wav$", full.names = TRUE, ignore.case = TRUE
  )
  for (f in temp_files) {
    if (!identical(f, current_file) && file.exists(f)) {
      try(file.remove(f), silent = TRUE)
    }
  }
  invisible(NULL)
}

#' Summarise a loaded soundscape's WAV metadata for the read-only panel (LSA-111).
#'
#' Pure formatter: derives display fields from the in-memory `tuneR::Wave`
#' object and the file path, with no I/O. Used by the "Soundscape metadata"
#' accordion panel. When no recording is loaded yet (`rec` is `NULL`), returns a
#' single placeholder row so the table renders cleanly.
#'
#' @param rec a `tuneR::Wave` (or `NULL` when nothing is loaded).
#' @param path the soundscape file path (character, may be `NULL`).
#' @return a two-column `data.frame` (`Field`, `Value`) of character values.
#' @noRd
.format_soundscape_metadata <- function(rec, path = NULL) {
  if (is.null(rec)) {
    return(data.frame(
      Field = "Status", Value = "No soundscape loaded",
      stringsAsFactors = FALSE
    ))
  }

  samp_rate <- rec@samp.rate
  n_samples <- length(rec@left)
  duration <- if (is.finite(samp_rate) && samp_rate > 0) {
    n_samples / samp_rate
  } else {
    NA_real_
  }
  channels <- if (isTRUE(rec@stereo)) "Stereo (2)" else "Mono (1)"

  data.frame(
    Field = c(
      "File", "Path", "Duration (s)", "Sample rate (Hz)",
      "Bit depth", "Channels", "Samples"
    ),
    Value = c(
      if (is.null(path)) NA_character_ else basename(path),
      if (is.null(path)) NA_character_ else path,
      if (is.na(duration)) NA_character_ else sprintf("%.3f", duration),
      as.character(samp_rate),
      as.character(rec@bit),
      channels,
      as.character(n_samples)
    ),
    stringsAsFactors = FALSE
  )
}
