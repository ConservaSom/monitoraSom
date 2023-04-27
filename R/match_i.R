
#' Match template to soundscape
#'
#' This function takes in a data frame of soundscapes and their associated templates, and matches each template to its corresponding soundscape. The matching is done using either the Pearson correlation coefficient ("cor") or dynamic time warping ("dtw").
#'
#' @param df_grid_i A data frame containing information about the soundscape and template to match. The data frame must contain the following columns:
#' \itemize{
#'   \item soundscape_path: A string specifying the file path of the soundscape.
#'   \item template_path: A string specifying the file path of the template.
#'   \item template_wl: An integer specifying the window length (in samples) for the spectrogram.
#'   \item template_ovlp: A numeric between 0 and 1 specifying the overlap between windows for the spectrogram.
#'   \item template_start: A numeric specifying the start time (in seconds) of the template within the soundscape.
#'   \item template_end: A numeric specifying the end time (in seconds) of the template within the soundscape.
#'   \item template_min_freq: A numeric specifying the minimum frequency (in Hz) for the spectrogram.
#'   \item template_max_freq: A numeric specifying the maximum frequency (in Hz) for the spectrogram.
#' }
#'
#' @param score_method A character string indicating the method to use for matching. Currently, two methods are available: "cor" (Pearson correlation coefficient) or "dtw" (dynamic time warping). Defaults to "cor".
#'
#' @return A tibble containing the input data frame with an additional column "score_vec", which is a data frame with columns "time_vec" (the time points of the spectrogram) and "score_vec" (the matching scores).
#'
#' @export
#'
#' @examples
#' match_i(df_grid_i = my_data_frame, score_method = "cor")

match_i <- function(df_grid_i, score_method = "cor") {
  wav_query <- readWave(df_grid_i$soundscape_path)
  wav_template <- readWave(df_grid_i$template_path)

  spectro_template <- spectro(
    wav_template,
    wl = df_grid_i$template_wl, ovlp = df_grid_i$template_ovlp,
    tlim = c(df_grid_i$template_start, df_grid_i$template_end),
    flim = c(df_grid_i$template_min_freq, df_grid_i$template_max_freq),
    plot = F, norm = T
  )
  spectro_soundscape <- spectro(
    wav_query,
    wl = df_grid_i$template_wl, ovlp = df_grid_i$template_ovlp,
    flim = c(df_grid_i$template_min_freq, df_grid_i$template_max_freq),
    plot = F, norm = T
  )

  mat_soundscape <- t(spectro_soundscape$amp)
  mat_template <- t(spectro_template$amp)
  sliding_window <- nrow(mat_template)
  ind <- slider::slide(
    1:nrow(mat_soundscape), ~.x,
    .before = sliding_window %/% 2, .after = (sliding_window %/% 2) - 1,
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
      c(
        rep(min(.), sliding_window %/% 2), ., rep(
          min(.),
          (sliding_window %/% 2) - 1
        )
      )
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
      c(
        rep(min(.), sliding_window %/% 2), ., rep(
          min(.),
          (sliding_window %/% 2) - 1
        )
      )
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
