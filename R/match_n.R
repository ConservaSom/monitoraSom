#' Title
#'
#' @param df_grid
#' @param score_method
#' @param ncores
#' @param save_res
#'
#' @return
#' @export
#'
#' @examples
match_n <- function(df_grid, score_method = "cor", ncores = 1, save_res = FALSE) {

  grid_list <- group_split(rowwise(df_grid))

  handlers(handler_pbcol(
    adjust = 1.0,
    complete = function(s) cli::bg_cyan(cli::col_black(s)),
    incomplete = function(s) cli::bg_red(cli::col_black(s))
  ))

  match_i_wrap <- function(grid_list, score_method) {
    p <- progressor(along = grid_list)
    res <- future_map(
      grid_list,
      function(x) {
        res <- match_i(x, score_method = score_method)
        p(message = "Template matching")
        return(res)
      }
    ) |>
      list_rbind()
  }

  if (ncores > 1) {
    plan(multicore, workers = ncores)
  } else {
    plan(sequential)
  }

  with_progress({
    res <- match_i_wrap(grid_list, score_method = score_method)
  })
  plan(sequential)

  if (save_res != FALSE) {
    if (dir.exists(dirname(save_res))) {
      saveRDS(res, save_res)
    } else {
      stop("The path for saving the results does not exist")
    }
  }
  message("Template matching completed")
  return(res)
}

