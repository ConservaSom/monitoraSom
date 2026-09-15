#' Capture and filter detections from scores of multiple matches
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Captures the detections for **every** soundscape-template pair in the
#'   `scores` table produced by [run_matching()], then applies the optional
#'   detection filters (minimum score, minimum quantile, top-n). It is the
#'   batch entry point to peak detection. This is the same path
#'   [run_matching()] runs internally when asked for detections, so calling it
#'   here or through [run_matching()] gives identical results.
#'
#' @details
#'   Use this function when you kept the raw per-frame `scores` from
#'   [run_matching()] (via `output = "scores"`) and want to (re)threshold them
#'   without re-running the expensive matching engine (for example, sweeping
#'   `min_score` or `top_n` and comparing detection counts). If you do not
#'   need the raw scores, the one-call [template_matching()] or
#'   `run_matching(output = "detections")` gives you detections directly.
#'
#'   Capture and filtering are separate steps on purpose: capturing peaks is a
#'   cheap pass, so re-filtering an already-captured table is fast. The `scope`
#'   argument decides whether `min_quant`/`top_n` apply within each pair
#'   (`"pair"`, the classic behaviour) or across the whole grid (`"grid"`).
#'
#'   Things to know: (1) the input must be a real [run_matching()] `scores`
#'   table: it must carry the `score_vec`/`score_sliding_window` columns. A
#'   detections table is not accepted. (2) A single soundscape-template pair
#'   is valid input: one row is enough. (3) When `output_db` is set, the
#'   detections are written to a DuckDB database in one transaction, keyed on
#'   `detection_id`. Use [export_detections_duckdb_to_csv()] if you later
#'   need a CSV.
#'
#' @section Pipeline context:
#'   Step 8 of the monitoraSom analysis flow (batch entry point). Reads the
#'   `scores` table from [run_matching()] (`output = "scores"`). Produces a
#'   filtered detections tibble in the standard format, fed to [detecs_to_rois()]
#'   (step 10) or [validate_by_overlap()] (step 11).
#'
#' @param df_scores The `scores` output of [run_matching()]: a tibble in the
#'   R session with one or more rows, or a path to an `.rds` file holding it
#'   (a convenience from earlier versions). No default; this is the table to
#'   detect peaks in.
#' @param buffer_size Exclusion buffer in spectrogram frames. Default
#'   `"template"` uses each template's frame
#'   count; a non-negative whole number sets it explicitly, `0` disables
#'   suppression. Larger values give fewer, more spread-out detections. Use
#'   it to avoid many overlapping detections of the same event: a peak
#'   suppresses its neighbours inside the buffer, so the surviving detections
#'   stay spread out. That keeps redundant detections away whether they are
#'   true or false positives, which is especially valuable when you will
#'   review the detections by hand in the validation app.
#' @param min_score,min_quant,top_n Optional thresholds: keep detections with
#'   peak score at or above
#'   `min_score` (`[0, 1]`), with score quantile at or above `min_quant`
#'   (`[0, 1]`), and at most `top_n` detections by raw score. Each defaults to
#'   `NULL` (no threshold); they compose (all supplied thresholds must pass).
#' @param scope How `min_quant`/`top_n` are applied: `"pair"` (default; within
#'   each soundscape-template score vector) or `"grid"` (across the whole
#'   table). `min_score` always applies to every detection, regardless of
#'   `scope`.
#' @param output_db Optional path to a detections DuckDB database; detections
#'   are written there (rows are updated when they already exist, in one
#'   transaction keyed on `detection_id`). `NULL` (default) returns them in
#'   the R session only.
#' @param autosave_action `"replace"` (default; clear the detections table and
#'   insert the new rows) or `"append"` (update existing rows and keep prior
#'   ones). Applies only when `output_db` is set.
#'
#' @return A tibble of detections in the standard detections format (26
#'   columns), one row per surviving detection (zero rows, typed, when none pass
#'   the filters). Returned in memory; also written to `output_db` when supplied.
#' @seealso [run_matching()] (previous step), [detecs_to_rois()] (next step).
#' @export
#' @examples
#' \dontrun{
#' # Load the package
#' library(monitoraSom)
#' # Step 8: capture + filter detections from a run_matching() scores table.
#' # Build a small synthetic score table: two soundscape-template pairs, each
#' # with two clear peaks (no files needed).
#' mk_row <- function(tid, peaks_at, heights, n = 200L) {
#'   sc <- 0.1
#'   for (k in seq_along(peaks_at))
#'     sc <- sc + heights[k] * exp(-((seq_len(n) - peaks_at[k])^2) / 50)
#'   row <- data.frame(template_id = tid, score_sliding_window = 20L,
#'                     score_method = "cor", stringsAsFactors = FALSE)
#'   row$score_vec <- list(data.frame(time_vec = seq_len(n) / 100, score_vec = sc))
#'   row
#' }
#' scores <- rbind(mk_row("t1", c(60, 140), c(0.8, 0.5)),
#'                 mk_row("t2", c(30, 120), c(0.9, 0.7)))
#'
#' # 1) No filters: every captured peak survives.
#' df_detecs <- fetch_score_peaks(scores)
#' df_detecs[, c("template_id", "detection_start", "peak_score", "peak_quant")]
#'
#' # 2) Absolute floor: keep only detections scoring at least 0.7.
#' fetch_score_peaks(scores, min_score = 0.7)[, c("template_id", "peak_score")]
#'
#' # 3) Relative floor: keep the top quarter of scores within each pair.
#' fetch_score_peaks(scores, min_quant = 0.75)[, c("template_id", "peak_score")]
#'
#' # 4) Top-n per pair: the single strongest detection of each template.
#' fetch_score_peaks(scores, top_n = 1)[, c("template_id", "peak_score")]
#'
#' # 5) Top-n across the whole grid: the single strongest detection overall.
#' fetch_score_peaks(scores, top_n = 1, scope = "grid")
#'
#' # 6) A larger exclusion buffer keeps overlapping detections apart:
#' fetch_score_peaks(scores, buffer_size = 80)[, c("template_id", "detection_start")]
#' }
fetch_score_peaks <- function(
  df_scores, buffer_size = "template", min_score = NULL, min_quant = NULL,
  top_n = NULL, scope = c("pair", "grid"), output_db = NULL,
  autosave_action = c("replace", "append")
) {
  scope <- match.arg(scope)
  autosave_action <- match.arg(autosave_action)
  df_scores <- .load_scores(df_scores)                       # FSPB-02/03

  det <- .detect_peaks_batch(df_scores, buffer_size)         # FSPB-08 (pure capture)
  det <- filter_detections_i(det, min_score, min_quant, top_n, scope)  # FSPB-10
  message(sprintf("Detections extracted from scores: %d detection(s).", nrow(det)))  # FSPB-09

  if (!is.null(output_db)) {                                  # FSPB-04 (DuckDB)
    if (!dir.exists(dirname(output_db))) {
      stop("The output_db directory does not exist: ", dirname(output_db))
    }
    # FEAT-07: warn once when persisting to an unmarked explicit path.
    .require_explicit_workspace(output_db, label = "output_db",
                                caller = "fetch_score_peaks")
    con <- .signals_duckdb_connect(output_db)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    .signals_duckdb_upsert(con, .detections_as_signals(det),
                           replace = autosave_action == "replace")
    message("Detections upserted to ", output_db, ".")
  }
  det
}

#' Load + validate a `scores` table from memory or a legacy `.rds` path
#' (FSPB-02/03 — same contract check on both branches).
#' @keywords internal
#' @noRd
.load_scores <- function(df_scores) {
  if (is.character(df_scores)) {
    if (length(df_scores) != 1L || !file.exists(df_scores)) {
      stop("`df_scores` path does not exist: ", df_scores)
    }
    df_scores <- readRDS(df_scores)
  }
  if (!is.data.frame(df_scores) || nrow(df_scores) < 1L) {
    stop("`df_scores` must be a non-empty `scores` table (>= 1 row) or a path ",
         "to an `.rds` of one.")
  }
  if (!("score_vec" %in% names(df_scores)) ||
      !("score_sliding_window" %in% names(df_scores))) {
    stop("`df_scores` is missing the score contract columns ",
         "(`score_vec`, `score_sliding_window`). Is it a run_matching() output?")
  }
  df_scores
}
