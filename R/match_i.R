
#' Run one iteration of the matching algorithm to obtain the matching score
#' vector between one template and one soundscape
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function takes uses the metadata contained in one row of the output of
#' the function 'fetch_match_grid()' to calculate the matching score of the
#' template spectrogram and a portion of the soundscape spectrogram of same
#' dimensions. The available matching algorithms are the Pearson correlation
#' coefficient ("cor") or dynamic time warping ("dtw").
#'
#' @param df_grid_i One row of the output of the function 'fetch_match_grid()
#' @param score_method A character string indicating the method to use for
#'   matching. The two methods available are: "cor" (Pearson correlation
#'   coefficient) or "dtw" (dynamic time warping). Defaults to "cor".
#'
#' @return A tibble row containing the input data frame with an additional
#'   column "score_vec", which is a list of length one containing a data frame
#'   with the columns "time_vec" (the time value of each spectrogram frame) and
#'   "score_vec" (the matching score obtained when the template and the
#'   soundscape spectrogram of samew dimensions are alligned at that frame). The
#'   length of the "score_vec" is equal to the number of frames of the
#'   soundscape spectrogram minus the number of frames of the template
#'   spectrogram (i.e. the number of possible allignments between the two
#'   spectrograms. The score is not available for the first and last frames of
#'   the soundscape spectrogram because score cannot be calculated between
#'   spectrograms of different dimensions. To produce a score vector with the
#'   same number of frames of the soundscape spectrogram, pads with length quals
#'   half the number of frames from the template are added to the beginning and
#'   end of the score vector.
#'
#' @export
#' @import purrr
#' @importFrom tuneR readWave
#' @importFrom seewave spectro
#' @importFrom dtwclust dtw_basic
#' @importFrom slider slide
match_i <- function(df_grid_i, score_method = "cor") {

  wav_query <- readWave(df_grid_i$soundscape_path)
  wav_template <- readWave(df_grid_i$template_path)

  spectro_template <- seewave::spectro(
    wav_template,
    wl = df_grid_i$template_wl, ovlp = df_grid_i$template_ovlp,
    tlim = c(df_grid_i$template_start, df_grid_i$template_end),
    flim = c(df_grid_i$template_min_freq, df_grid_i$template_max_freq),
    plot = F, norm = T
  )
  spectro_soundscape <- seewave::spectro(
    wav_query,
    wl = df_grid_i$template_wl, ovlp = df_grid_i$template_ovlp,
    flim = c(df_grid_i$template_min_freq, df_grid_i$template_max_freq),
    plot = F, norm = T
  )

  mat_soundscape <- t(spectro_soundscape$amp)
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
    score_vec <- purrr::map(
      1:length(ind),
      function(x) {
        cor(
          c(mat_soundscape[ind[[x]], ]), c(mat_template),
          method = "pearson", use = "complete.obs"
        )
      }
    ) |>
      unlist() %>%
      c(rep(min(.), sw_start - 1), ., rep(min(.), sw_end + 1))
  } else if (score_method == "dtw") {
    stretch <- 0.2
    norm <- "L1"
    step.pattern <- dtw::symmetric1
    score_vec <- lapply(
      1:length(ind),
      function(x) {
        dtwclust::dtw_basic(
          mat_soundscape[ind[[x]], ], mat_template,
          backtrack = F, norm = norm, step.pattern = step.pattern,
          window.size = round(sliding_window + (stretch * sliding_window), 0),
          normalize = FALSE
        )
      }
    ) %>%
      unlist() %>%
        {
          1 - (. / max(.))
        } %>%
        c(rep(min(.), sw_start - 1), ., rep(min(.), sw_end + 1))
  }

  if (length(score_vec) > length(spectro_soundscape$time)) {
    score_vec <- head(score_vec, -1)
  }

  res <- as_tibble(df_grid_i)
  res$score_sliding_window <- sliding_window
  res$score_method <- score_method
  res$score_vec <- list(
    data.frame(
      time_vec = spectro_soundscape$time,
      score_vec = score_vec
    )
  )
  return(res)
}
