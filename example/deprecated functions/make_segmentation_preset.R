#' Function to create segmentation presets for soundscape analysis
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function creates a list with the segmentation preset parameters for
#' soundscape analysis, such as soundscapes path, region of interest tables
#' path, cuts path, fast display, label angle, and dynamic range, among others.
#' The validation user parameter is mandatory. If the 'soundscapes_path'
#' parameter is not found locally, a warning message will be shown, and if the
#' roi_tables_path or cuts_path do not exist, the function will create new
#' folders.
#'
#' @param preset_path Path to save the preset file (optional)
#' @param preset_id Identification for the preset (optional)
#' @param validation_user User responsible for the validation of the
#'   segmentation
#' @param soundscapes_path Path to soundscapes files
#' @param roi_tables_path Path to region of interest tables files
#' @param cuts_path Path to cuts files
#' @param fastdisp If TRUE, the next cut is automatically displayed when the
#'   user validates a cut
#' @param label_angle Angle to draw the cut labels. Must be between 0 and 180,
#'   and multiple of 10
#' @param show_label If TRUE, the cut labels will be displayed
#' @param dyn_range Dynamic range. A vector with two numeric values. Must be
#'   between -100 and 10, and multiple of 10
#' @param wl Window length. Must be one of the alternatives: 128, 256, 512,
#'   1024, 2048, or 4096
#' @param ovlp Overlap. Must be a numeric value, between 0 and 80, and multiple
#'   of 10
#' @param color_scale The color scale for the spectrogram
#' @param wav_player_type The type of wav player. "R session" for R
#'   session-based player, "system" for system player
#' @param wav_player_path Path to the wav player executable (only for system
#'   player)
#' @param session_notes Notes related to the analysis session
#' @param zoom_freq Vector with two numeric values, indicating the frequency
#'   range to zoom in the spectrogram
#' @param nav_autosave If TRUE, the current segmentation is saved when the user
#'   navigates to another file
#' @param sp_list Species list used in the analysis
#'
#' @return A list with the segmentation preset parameters
#'
#' @export
make_segmentation_preset <- function(
    preset_path = NULL, preset_id = NULL,
    validation_user, soundscapes_path,
    roi_tables_path, cuts_path,
    fastdisp = TRUE, label_angle = 90, show_label = TRUE,
    dyn_range = c(-60, 0), wl = 1024, ovlp = 0,
    color_scale = "inferno", wav_player_type = "R session",
    wav_player_path = "play", session_notes = NULL,
    zoom_freq = c(0, 10), nav_autosave = FALSE,
    sp_list = "CBRO-2021 (Brazil)") {

      # todo Adicionar informação de pitch_shift
  res <- list()

  user <- gsub(",", "", validation_user)

  if (!is.null(user)) {
    res$validation_user <- as.character(user)
  } else {
    stop("Error! Inform the user name")
  }

  res$soundscapes_path <- soundscapes_path
  if (!dir.exists(soundscapes_path)) {
    warning("Warning! The path informed in 'soundscapes_path' was not found locally.")
  }

  if (!dir.exists(roi_tables_path)) {
    dir.create(roi_tables_path)
    warning("Warning! The path informed in 'roi_tables_path' was not found locally. A new folder was created.")
  } else {
    res$roi_tables_path <- roi_tables_path
  }

  if (!dir.exists(cuts_path)) {
    dir.create(cuts_path)
    warning("Warning! The path informed in 'cuts_path' was not found locally. A new folder was created.")
  } else {
    res$cuts_path <- cuts_path
  }

  if (is.logical(fastdisp)) {
    res$auto_next <- fastdisp
  } else {
    stop("Error! 'auto_next' must be logical (TRUE or FALSE).")
  }

  # check of "label_angle" variable is within 0-180 range and is a multiple of 10
  if (is.numeric(label_angle)) {
    if (label_angle >= 0 & label_angle <= 180) {
      if (label_angle %% 10 == 0) {
        res$label_angle <- label_angle
      } else {
        stop("Error! 'label_angle' must be a multiple of 10.")
      }
    } else {
      stop("Error! 'label_angle' must be between 0 and 180.")
    }
  } else {
    stop("Error! 'label_angle' must be numeric.")
  }

  if (is.logical(show_label)) {
    res$show_label <- show_label
  } else {
    stop("Error! 'show_label' must be logical (TRUE or FALSE).")
  }

  if (length(dyn_range) == 2) {
    if (all(is.numeric(dyn_range))) {
      if (dyn_range[1] < dyn_range[2]) {
        if (dyn_range[1] >= -100 & dyn_range[2] <= 10) {
          if (dyn_range[1] %% 10 == 0 & dyn_range[2] %% 10 == 0) {
            res$dyn_range <- dyn_range
          } else {
            stop("Error! 'dyn_range' must be a multiple of 10.")
          }
        } else {
          stop("Error! 'dyn_range' must be between -100 and 10.")
        }
      } else {
        res$dyn_range <- sort(dyn_range)
        warning(
          "Warning! The first value of 'dyn_range' must be smaller than the second. Sorting to match the expected order"
        )
      }
    } else {
      stop(
        "Error! At least one value of 'dyn_range' is not numeric"
      )
    }
  } else {
    stop("Error! The 'dyn_range' must be a numeric vector of length equals 2.")
  }

  if (is.numeric(wl)) {
    if (wl %in% c(128, 256, 512, 1024, 2048, 4096)) {
      res$wl <- wl
    } else {
      stop(
        "Error! The value assigned to 'wl' is not among the expected alternatives: 128, 256, 512, 1024, 2048, or 4096."
      )
    }
  } else {
    stop(
      "Error! The value assigned to 'wl' is not among the expected alternatives: 128, 256, 512, 1024, 2048, or 4096."
    )
  }

  # check if the 'ovelp' is numeric, is a multiple of 10 and is between 0 and 80
  if (is.numeric(ovlp)) {
    if (ovlp %in% seq(0, 80, 10)) {
      res$ovlp <- ovlp
    } else {
      stop(
        "Error! The value assigned to 'ovlp' is outside the expected interval. Provide a numeric value between 0 and 80 in steps of 10."
      )
    }
  } else {
    stop(
      "Error! The value assigned to 'time_pads' is not numeric. Provide a value between 0 and 80 in steps of 10."
    )
  }

  # check if the 'color_scale' variable is within the expected alternatives ("viridis", "magma", "inferno", "cividis", "greyscale 1", and "greyscale 2")
  if (is.character(color_scale)) {
    if (color_scale %in% c("viridis", "magma", "inferno", "cividis", "greyscale 1", "greyscale 2")) {
      res$color_scale <- color_scale
    } else {
      stop(
        "Error! The value assigned to 'color_scale' is not among the expected alternatives: 'viridis', 'magma', 'inferno', 'cividis', 'greyscale 1', or 'greyscale 2'."
      )
    }
  } else {
    stop(
      "Error! The value assigned to 'color_scale' is not among the expected alternatives: 'viridis', 'magma', 'inferno', 'cividis', 'greyscale 1', or 'greyscale 2'."
    )
  }

  if (wav_player_type %in% c("HTML player", "R session", "External player")) {
    if (wav_player_type == "External player") {
      if (file.exists(wav_player_path)) {
        res$wav_player_path <- wav_player_path
      } else {
        stop(
          "Error! The path informed in 'wav_player_path' was not found locally."
        )
      }
    }
    res$wav_player_type <- wav_player_type
  } else {
    stop(
      "Error! The selected WAV player method is not valid. Select one of the following: 'HTML player', 'R session', 'External player'"
    )
  }

  res$session_notes <- session_notes

  # check if the variable 'zoom_freq' is an integer vector of length equals 2, is between 0 and 10 and the first value is smaller than the second
  if (length(zoom_freq) == 2) {
    if (all(is.numeric(zoom_freq))) {
      if (zoom_freq[1] < zoom_freq[2]) {
        if (zoom_freq[1] >= 0 & zoom_freq[2] <= 10) {
          if (zoom_freq[1] %% 1 == 0 & zoom_freq[2] %% 1 == 0) {
            res$zoom_freq <- zoom_freq
          } else {
            stop("Error! 'zoom_freq' must be an integer.")
          }
        } else {
          stop("Error! 'zoom_freq' must be between 0 and 10.")
        }
      } else {
        res$zoom_freq <- sort(zoom_freq)
        warning(
          "Warning! The first value of 'zoom_freq' must be smaller than the second. Sorting to match the expected order"
        )
      }
    } else {
      stop(
        "Error! At least one value of 'zoom_freq' is not numeric"
      )
    }
  } else {
    stop("Error! The 'zoom_freq' must be a numeric vector of length equals 2.")
  }

  # check if the variable 'nav_autosave' is a logical variable
  if (isTRUE(nav_autosave) | isFALSE(nav_autosave)) {
    res$nav_autosave <- nav_autosave
  } else {
    stop(
      "Error! The value assigned to 'nav_autosave' is not logical. Set it to TRUE or FALSE."
    )
  }

  # check if the variable 'sp_list' is a character vector
  if (is.character(sp_list)) {
    res$sp_list <- sp_list
  } else {
    stop(
      "Error! The value assigned to 'sp_list' is not a character vector."
    )
  }

  if (!is.null(preset_path) & !is.null(preset_id)) {
    if (!dir.exists(preset_path)) {
      dir.create(preset_path)
      warning("Warning! The selected preset destination folder did not exist. It was created automatically.")
    }
    preset_file <- file.path(
      preset_path, paste0("segmentation_preset_", preset_id, ".rds")
    )
    saveRDS(object = res, file = preset_file)
    message("Preset sucessfully exported to the selected destination!")
  }

  return(res)
}
