#' Match templates in a grid of images
#'
#' This function matches templates in a grid of images using the `match_i` function. O chatGPT viajou na maionese, não esquecer de corrigir isso aqui!!!
#'
#' @param df_grid A data frame containing the paths of the images to be analyzed.
#' @param score_method A character string indicating the method to be used to calculate the matching score. Default is "cor".
#' @param ncores An integer indicating the number of cores to be used for parallelization. Default is 1.
#' @param save_res A character string indicating the path to save the results as an RDS file. Default is FALSE.
#'
#' @return A data frame containing the matching scores for each template in the grid.
#' @export
#'
#' @examples
#' # Load example data
#' data("df_grid")
#'
#' # Match templates
#' res <- match_n(df_grid, score_method = "ssim", ncores = 2, save_res = "res.rds")
match_n <- function(df_grid, score_method = "cor", ncores = 1, save_res = FALSE, par_strat = "future") {

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

