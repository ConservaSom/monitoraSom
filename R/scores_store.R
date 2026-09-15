#' Raw-scores persistence: DuckDB (Stage 4 gate §2.2)
#'
#' @description Persists the `run_matching(output = "scores")` tibble (one row per
#'   pair, list-column `score_vec = data.frame(time_vec, score_vec)`) in a
#'   **DuckDB** `scores` table. It uses a long layout (one row per (pair, frame)).
#'
#'   The regular-time array-column compression (store `time_vec` as
#'   `(time_start, time_step, n)`) is a recorded optimization, deferred — the long
#'   layout round-trips the frozen in-memory contract faithfully.
#'
#' @keywords internal
#' @noRd

# Explode the scores tibble's list-column into a long data.frame.
.scores_to_long <- function(scores_tbl) {
  meta_cols <- intersect(
    c("soundscape_path", "soundscape_file", "template_path", "template_file",
      "template_id", "template_name", "score_method", "score_sliding_window"),
    names(scores_tbl)
  )
  parts <- lapply(seq_len(nrow(scores_tbl)), function(i) {
    sv <- scores_tbl$score_vec[[i]]
    meta <- scores_tbl[i, meta_cols, drop = FALSE]
    pid <- digest::digest(
      paste(meta$soundscape_path, meta$template_path, meta$score_method, sep = "|"),
      algo = "sha256"
    )
    data.frame(
      score_pair_id = substr(pid, 1L, 16L),
      meta[rep(1L, nrow(sv)), , drop = FALSE],
      frame_index = seq_len(nrow(sv)),
      time_vec = sv$time_vec, score_vec = sv$score_vec,
      row.names = NULL, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, parts)
}

# One row per pair: score_vec as a DOUBLE[] array column, time_vec compressed to
# (time_start, time_step, n_frames) since frame times are regular (~halves size).
.scores_to_array_rows <- function(scores_tbl) {
  meta_cols <- intersect(
    c("soundscape_path", "soundscape_file", "template_path", "template_file",
      "template_id", "template_name", "score_method", "score_sliding_window"),
    names(scores_tbl)
  )
  rows <- lapply(seq_len(nrow(scores_tbl)), function(i) {
    sv <- scores_tbl$score_vec[[i]]
    n <- nrow(sv)
    step <- if (n > 1L) (sv$time_vec[n] - sv$time_vec[1]) / (n - 1) else 0
    meta <- scores_tbl[i, meta_cols, drop = FALSE]
    pid <- digest::digest(
      paste(meta$soundscape_path, meta$template_path, meta$score_method, sep = "|"),
      algo = "sha256"
    )
    df <- data.frame(score_pair_id = substr(pid, 1L, 16L), meta,
                     time_start = sv$time_vec[1], time_step = step,
                     n_frames = n, row.names = NULL, stringsAsFactors = FALSE)
    df$score_vec <- list(sv$score_vec)
    df
  })
  do.call(rbind, rows)
}

# Write scores to a DuckDB `scores` table in the compressed array layout.
.scores_write_duckdb <- function(path, scores_tbl, replace = TRUE) {
  arr <- .scores_to_array_rows(scores_tbl)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "scores", arr, append = !replace, overwrite = replace)
  invisible(arr)
}

# Read the array scores table back, reconstructing the frozen-contract shape
# (one row per pair, list-column score_vec = data.frame(time_vec, score_vec)).
.scores_read_duckdb <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  raw <- DBI::dbGetQuery(con, "SELECT * FROM scores ORDER BY score_pair_id;")
  raw$score_vec <- lapply(seq_len(nrow(raw)), function(i) {
    n <- raw$n_frames[i]
    tv <- raw$time_start[i] + (seq_len(n) - 1L) * raw$time_step[i]
    data.frame(time_vec = tv, score_vec = as.numeric(raw$score_vec[[i]]))
  })
  tibble::as_tibble(raw)
}

