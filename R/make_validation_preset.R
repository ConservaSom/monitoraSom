
#' Title
#'
#' @param preset_path
#' @param preset_name
#' @param validation_user
#' @param templates_path
#' @param soundscapes_path
#' @param input_path
#' @param output_path
#' @param wav_cuts_path
#' @param spec_path
#' @param diag_tab_path
#' @param wav_player_path
#' @param wav_player_type
#' @param val_subset
#' @param min_score
#' @param time_pads
#' @param ovlp
#' @param wl
#' @param dyn_range
#' @param color_scale
#' @param zoom_freq
#' @param nav_shuffle
#' @param seed
#' @param auto_next
#' @param nav_autosave
#' @param overwrite
#' @param session_notes
#'
#' @return
#' @export
#'
#' @examples
make_validation_preset <- function(preset_path = NULL, preset_name = NULL,
                                   validation_user, templates_path, soundscapes_path,
                                   input_path, output_path, wav_cuts_path, spec_path, diag_tab_path,
                                   wav_player_path = "play", wav_player_type = "HTML player",
                                   val_subset = c("NA", "TP", "FP", "UN"), min_score = 0,
                                   time_pads = 1, ovlp = 0, wl = 2048, dyn_range = c(-60, 0),
                                   color_scale = "inferno", zoom_freq = c(0, 4),
                                   nav_shuffle = FALSE, seed = 123,
                                   auto_next = FALSE, nav_autosave = FALSE, overwrite = FALSE,
                                   session_notes) {
  res <- list()

  validation_user_temp <- gsub(",", "", validation_user)

  if (!is.null(validation_user_temp)) {
    res$validation_user <- as.character(validation_user_temp)
  } else {
    stop("Error! Inform the user name")
  }


  res$templates_path <- templates_path
  if (!dir.exists(templates_path)) {
    warning("Warning! The path informed in 'templates_path' was not found locally.")
  }

  res$soundscapes_path <- soundscapes_path
  if (!dir.exists(soundscapes_path)) {
    warning("Warning! The path informed in 'soundscapes_path' was not found locally.")
  }

  res$input_path <- input_path
  if (!file.exists(input_path)) {
    warning("Warning! The informed 'input_file' was not found locally.")
  }

  res$output_path <- output_path
  if (!file.exists(output_path)) {
    warning("Warning! The informed 'output_file' was not found locally.")
  }

  if (wav_player_type %in% c("HTML player", "R session", "External player")) {
    # todo Add condition to check if the player is executable
    res$wav_playexr_path <- wav_player_path
    res$wav_player_type <- wav_player_type
  } else {
    stop(
      "Error! The selected WAV player method is not valid. Select one of the following: 'HTML player', 'R session', 'External player'"
    )
  }

  if (all(val_subset %in% c("NA", "TP", "FP", "UN"))) {
    res$val_subset <- val_subset
  } else {
    stop(
      "Error! At least one of the values assigned to 'val_subset' are not within the accepted alternatives ('NA', 'TP', 'FP', 'UN'). Observe that 'NA' is handled as a character string in this function"
    )
  }

  if (is.numeric(min_score)) {
    if (0 <= min_score & min_score < 1) {
      res$min_score <- min_score
    } else {
      stop(
        "Error! The value assigned to 'min_score' is not within the expected interval. Provide a numeric value equal to or higher than zero and smaller than one"
      )
    }
  } else {
    stop(
      "Error! The value assigned to 'min_score' is not numeric. Provide a numeric value equal to or higher than zero and smaller than one"
    )
  }

  if (is.numeric(time_pads)) {
    if (0 <= time_pads & time_pads <= 16) {
      res$time_pads <- time_pads
    } else {
      stop(
        "Error! The value assigned to 'time_pads' is outside the expected interval. Provide an integer between 0 and 16."
      )
    }
  } else {
    stop(
      "Error! The value assigned to 'time_pads' is not numeric."
    )
  }

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

  if (length(dyn_range) == 2) {
    if (all(is.numeric(dyn_range))) {
      # todo Adicionar checagem dos limites max e min de dyn_range
      if (dyn_range[1] < dyn_range[2]) {
        res$dyn_range <- dyn_range
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
  #

  if (color_scale %in% c("viridis", "magma", "inferno", "cividis", "greyscale")) {
    res$color_scale <- color_scale
  } else {
    stop(
      "Error! The provided 'color_scale' is outside the expected set of alternatives: 'viridis', 'magma', 'inferno', 'cividis', or 'greyscale'."
    )
  }
  #

  if (length(zoom_freq) == 2) {
    if (all(is.numeric(zoom_freq))) {
      # todo Adicionar checagem dos limites max e min de zoom_freq
      if (zoom_freq[1] < zoom_freq[2]) {
        res$zoom_freq <- zoom_freq
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

  if (isTRUE(nav_shuffle) | isFALSE(nav_shuffle)) {
    res$nav_shuffle <- nav_shuffle
  } else {
    stop("Error! 'nav_shuffle' must be set to TRUE or FALSE.")
  }

  if (is.numeric(seed)) {
    res$seed <- seed
  } else {
    stop("Error! Non-numeric value input provided to 'seed'")
  }

  if (isTRUE(auto_next) | isFALSE(auto_next)) {
    res$auto_next <- auto_next
  } else {
    stop("Error! 'auto_next' must be set to TRUE or FALSE.")
  }

  if (isTRUE(nav_autosave) | isFALSE(nav_autosave)) {
    res$nav_autosave <- nav_autosave
  } else {
    stop("Error! 'nav_autosave' must be set to TRUE or FALSE.")
  }

  if (isTRUE(overwrite) | isFALSE(overwrite)) {
    res$overwrite <- overwrite
  } else {
    stop("Error! 'overwrite' must be set to TRUE or FALSE.")
  }

  res$session_notes <- session_notes

  res$wav_cuts_path <- wav_cuts_path
  if (!dir.exists(wav_cuts_path)) {
    warning("Warning! The informed 'wav_cuts_path' was not found locally.")
  }
  #
  res$spec_path <- spec_path
  if (!dir.exists(spec_path)) {
    warning("Warning! The informed 'spec_path' was not found locally.")
  }
  #
  res$diag_tab_path <- diag_tab_path
  if (!dir.exists(diag_tab_path)) {
    warning("Warning! The informed 'diag_tab_path' was not found locally.")
  }

  if (!is.null(preset_path) & !is.null(preset_name)) {
    preset_file <- file.path(preset_path, paste0("preset_", preset_name, ".rds"))
    saveRDS(res, preset_file)

    message("Preset sucessfully exported to the selected destination!")
  }

  return(res)
}

