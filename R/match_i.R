#' Run one iteration of the matching algorithm to obtain the matching score
#' vector between one template and one soundscape
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   This function takes uses the metadata contained in one row of the output of
#'   the function 'fetch_match_grid()' to calculate the matching score of the
#'   template spectrogram and a portion of the soundscape spectrogram of same
#'   dimensions. The available matching algorithms are the Pearson correlation
#'   coefficient ("cor") or dynamic time warping ("dtw").
#'
#' @param df_grid_i One row of the output of the function 'fetch_match_grid()
#' @param score_method A character string indicating the method to use for
#'   matching. The two methods available are: "cor" (Pearson correlation
#'   coefficient) or "dtw" (dynamic time warping). Defaults to "cor".
#' @param output A character string indicating the output of the function. The
#'   two options are: "detections" (default) or "scores".
#' @param buffer_size A character string indicating the size of the buffer.
#' @param min_score Minimum score threshold (numeric between 0 and 1).
#' @param min_quant Minimum quantile threshold (numeric between 0 and 1).
#' @param top_n Number of top detections to return (positive integer).
#'
#' @return A tibble containing either detection results or raw scores.
#'
#' @export
#' @import dplyr
#' @importFrom tuneR readWave
#' @importFrom seewave spectro
#' @importFrom slider slide
#' @importFrom tibble as_tibble
#' @importFrom tidyr replace_na
match_i <- function(
    df_grid_i, score_method = "cor", output = "detections",
    buffer_size = "template", min_score = NULL, min_quant = NULL,
    top_n = NULL) {

  score_method <- match.arg(score_method, c("cor", "dtw"))
  output <- match.arg(output, c("detections", "scores"))

  if (!is.null(min_score) && (!is.numeric(min_score) || min_score < 0 || min_score > 1)) {
    stop("min_score must be NULL or a number between 0 and 1")
  }
  if (!is.null(min_quant) && (!is.numeric(min_quant) || min_quant < 0 || min_quant > 1)) {
    stop("min_quant must be NULL or a number between 0 and 1")
  }
  if (!is.null(top_n) && (!is.numeric(top_n) || top_n < 1)) {
    stop("top_n must be NULL or a positive integer")
  }

  spec_params <- list(
    wl = df_grid_i$template_wl, ovlp = df_grid_i$template_ovlp,
    flim = c(df_grid_i$template_min_freq, df_grid_i$template_max_freq),
    plot = FALSE, norm = TRUE
  )

  tryCatch(
    {
      soundscape_spectro <- tuneR::readWave(df_grid_i$soundscape_path)
      soundscape_spectro <- do.call(
        seewave::spectro, c(list(soundscape_spectro), spec_params)
      )
      spectro_template <- tuneR::readWave(df_grid_i$template_path)
      spectro_template <- do.call(
        seewave::spectro,
        c(
          list(spectro_template), spec_params,
          list(
            tlim = c(df_grid_i$template_start, df_grid_i$template_end)
          )
        )
      )
    },
    error = function(e) {
      stop(paste("Error generating spectrograms:", e$message))
    }
  )
  mat_soundscape <- t(soundscape_spectro$amp)
  mat_template <- t(spectro_template$amp)
  sliding_window <- nrow(mat_template)
  sw_start <- sliding_window %/% 2
  sw_end <- (sliding_window - sw_start) - 1
  ind <- slider::slide(
    1:nrow(mat_soundscape), ~.x,
    .before = sw_start, .after = sw_end,
    .complete = TRUE
  ) %>%
    base::Filter(base::Negate(is.null), .)

  if (score_method == "cor") {
    score_vec <- lapply(
      1:length(ind),
      function(x) {
        cor(
          c(mat_soundscape[ind[[x]], ]), c(mat_template),
          method = "pearson", use = "complete.obs"
        )
      }
    ) %>%
      unlist() %>%
      c(rep(min(.), sw_start - 1), ., rep(min(.), sw_end + 1)) %>%
      tidyr::replace_na(min(., na.rm = TRUE))
  } else if (score_method == "dtw") {
    if (!requireNamespace("dtw", quietly = TRUE)) {
      stop("Package 'dtw' is required for 'dtw' method. Please install it.")
    }
    stretch <- 0.2
    norm <- "L1"
    step.pattern <- dtw::symmetric1
    score_vec <- lapply(
      1:length(ind),
      function(x) {
        dtwclust::dtw_basic(
          mat_soundscape[ind[[x]], ], mat_template,
          backtrack = F, norm = norm, step.pattern = step.pattern,
          window.size = round(
            sliding_window + (stretch * sliding_window), 0
          ),
          normalize = FALSE
        )
      }
    ) %>%
      unlist() %>%
      {
        1 - (. / max(.)) # 1 - dtw score to match the format of the cor score
      } %>%
      c(rep(min(.), sw_start - 1), ., rep(min(.), sw_end + 1)) %>%
      tidyr::replace_na(min(., na.rm = TRUE))
  }

  if (length(score_vec) > length(soundscape_spectro$time)) {
    score_vec <- head(score_vec, -1)
  }

  res_raw <- as_tibble(df_grid_i)
  res_raw$score_sliding_window <- sliding_window
  res_raw$score_method <- score_method
  res_raw$score_vec <- list(
    data.frame(
      time_vec = soundscape_spectro$time,
      score_vec = score_vec
    )
  )

  if (output == "detections") {
    fetch_score_peaks_i(
      res_raw,
      buffer_size = buffer_size,
      min_score = min_score,
      min_quant = min_quant,
      top_n = top_n
    )
  } else {
    res_raw
  }
}
