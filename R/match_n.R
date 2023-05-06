#' Batch template matching
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function is wrapper of 'match_i()' to perform a batch computation of the
#' matching vectors of the template and soundscape matches defined in the
#' metadata grid obtained as output of the function 'fetch_match_grid()'. The
#' available algorithms to compare spectrograms and compute matching scores are
#' the Pearson correlation coefficient ("cor") or dynamic time warping ("dtw").
#'
#' @param df_grid The output of the function 'fetch_match_grid()
#' @param save_res A character string indicating the path to save the results in
#'   the format of an RDS file. Default is FALSE.
#' @param score_method A character string indicating the method to use for
#'   matching. The two methods available are: "cor" (Pearson correlation
#'   coefficient) or "dtw" (dynamic time warping). Defaults to "cor".
#' @param par_strat A character string indicating the parallelization strategy
#'   to be used. The available options are: "foreach" (default), "future" and
#'   "pbapply". The 'future' and 'pbapply' strategies do not work on Windows,
#'   but are more efficient in linux (especially when running and R session
#'   outside of Rstudio). The 'foreach' strategy works on all platforms, but is
#'   less efficient than the other two. See the documentation of the 'future'
#'   and 'pbapply' packages for more details.
#' @param ncores An integer indicating the number of cores to be used for
#'   parallelization. Default is 1.
#'
#' @return A tibble containing input data frame with an additional column
#'   "score_vec", which is a list of dataframes with the columns "time_vec" (the
#'   time value of each spectrogram frame) and "score_vec" (the matching score
#'   obtained when the template and the soundscape spectrogram of samew
#'   dimensions are alligned at that frame) for each match. The length of the
#'   "score_vec" is equal to the number of frames of the soundscape spectrogram
#'   minus the number of frames of the template spectrogram (i.e. the number of
#'   possible allignments between the two spectrograms. The score is not
#'   available for the first and last frames of the soundscape spectrogram
#'   because score cannot be calculated between spectrograms of different
#'   dimensions. To produce a score vector with the same number of frames of the
#'   soundscape spectrogram, pads with length quals half the number of frames
#'   from the template are added to the beginning and end of the
#' @export
#'
#' @examples
#' # Load example data
#' data("df_grid")
#'
#' # Match templates
#' res <- match_n(df_grid, score_method = "cor", ncores = 2, save_res = "res.rds")
match_n <- function(df_grid, score_method = "cor", save_res = FALSE, par_strat = "future", ncores = 1) {

  grid_list <- group_split(rowwise(df_grid))

  handlers(
    handler_pbcol(
      adjust = 1.0,
      complete = function(s) cli::bg_cyan(cli::col_black(s)),
      incomplete = function(s) cli::bg_red(cli::col_black(s))
    )
  )

  match_i_wrap <- function(grid_list, score_method) {
    p <- progressor(along = grid_list)
    res <- future_map(
      grid_list,
      function(x) {
        res <- match_i(x, score_method = score_method)
        p(message = "Template matching")
        return(res)
      } #, .options = furrr_options(scheduling = 2)
    ) |>
      list_rbind()
  }

  if (par_strat == "future") {
    if (ncores > 1) {
      plan(multicore, workers = ncores)
    } else {
      plan(sequential)
    }
    with_progress({
      res <- match_i_wrap(grid_list, score_method = score_method)
    })
    plan(sequential)
  }


  if (par_strat == "foreach") {
      if (ncores > 1) {
        cl <- makeCluster(ncores)
        plan(cluster, workers = cl)
        with_progress({
          p <- progressor(along = 1:length(grid_list)) # iniciando a barra
          res <- foreach(i = 1:length(grid_list), .combine = rbind) %dopar% {
            source("/home/grosa/R_repos/monitoraSom/R/match_i.R")
            require(parallel)
            require(doParallel)
            require(foreach)
            require(dplyr)
            p(message = "Template matching")
            res <- match_i(grid_list[[i]], score_method = score_method)
            return(res)
          }
        })
        # todo Adicionar método para parar os clusters no caso de interrupção do processo
        stopCluster(cl)
      } else {
        stop("The number of cores must be greater than 1")
      }
  }

  if (par_strat == "pbapply") {
    if (ncores > 1) {
      res <- pbapply::pblapply(grid_list, function(x) match_i(x, score_method = score_method), cl = ncores) |>
        list_rbind()
    } else {
      res <- pbapply::pblapply(grid_list, function(x) match_i(x, score_method = score_method)) |>
        list_rbind()
    }
  }

  if (save_res != FALSE) {
    if (dir.exists(dirname(save_res))) {
      saveRDS(res, save_res)
    } else {
      # ! mover essa condição para o começo da função
      stop("The path for saving the results does not exist")
    }
  }
  # todo Adicionar checagem do objeto para que salvamento possa ser incremental e não sobrescrever dados preexistentes

  message("Template matching completed")
  return(res)
}

