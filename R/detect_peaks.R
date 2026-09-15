# Shared peak-detection batch + composable filtering (FSPB-08 / FSPB-10).
# Single source of truth for turning a `scores` table into a `detections` table,
# used by both fetch_score_peaks() and the run_matching() orchestrator
# (_run_matching_orchestration.R) so the two paths can never diverge (FSPB-08,
# option A). Capture is pure (fetch_score_peaks_i()); filtering is a separate,
# composable step (filter_detections_i(), FSPB-10 option C) applied once over the
# canonical table — "capture once, filter many". Cycle:
# DEBT_INVENTORY_LOG-fetch_score_peaks.md (FSPB) +
# DEBT_INVENTORY_LOG-fetch_score_peaks_i.md (FSP). Reuses the canonical
# detections schema (_schema_detections.R) and the filter validator shared with
# the matching engine (.validate_score_filters()).

#' Capture detections for every row of a `scores` table (FSPB-05/08)
#'
#' @description Pure per-pair capture ([fetch_score_peaks_i()]) row-bound over the
#'   **standard zero-row schema**, so empty pairs contribute a typed frame (no
#'   `do.call(rbind)` column-mismatch, FSPB-05) and the result always carries the
#'   full 26-column schema. No filtering happens here.
#' @param scores a `scores` table (>= 0 rows) in the frozen contract.
#' @param buffer_size exclusion buffer forwarded to [fetch_score_peaks_i()].
#' @return standard detections `data.frame` (26 cols; zero rows when none).
#' @keywords internal
#' @noRd
.detect_peaks_batch <- function(scores, buffer_size = "template") {
  if (is.null(scores) || nrow(scores) == 0L) return(.schema_detections(0L))
  per_pair <- lapply(seq_len(nrow(scores)), function(i) {
    fetch_score_peaks_i(scores[i, ], buffer_size = buffer_size)
  })
  det <- dplyr::bind_rows(per_pair)          # binds over the shared schema
  .coerce_detections(det)
}

#' Filter a standard detections table (composable, FSPB-10 option C)
#'
#' @description Applies the optional score / quantile / top-n filters **after**
#'   capture, over the standard detections table — so the same capture can be
#'   filtered many ways without recomputing peaks. Each filter is recorded in its
#'   provenance column (`detection_min_score` / `detection_min_quant` /
#'   `detection_top_n`, FSP-10), and `top_n` selects by **raw score** (FSP-11).
#'
#'   `scope` controls the reference population:
#'   - `"pair"` (default; legacy behaviour) — `min_quant` uses each detection's
#'     per-pair `peak_quant` (FSP-09, valid-region ECDF) and `top_n` keeps the top
#'     detections **within each soundscape-template pair**.
#'   - `"grid"` — `min_quant` ranks each detection against the **whole table**'s
#'     scores, and `top_n` keeps the top detections across the entire grid. This
#'     makes a numeric `min_quant`/`top_n` comparable across pairs (dissolves the
#'     per-pair incomparability of FSPB-10).
#'
#'   **AFL-21:** the stored `peak_quant` column is always the **pair**-scope ECDF
#'   set at capture time and is never overwritten by a `scope="grid"` call here —
#'   it does not reflect which `scope` a given filter ran under. The grid-wide
#'   quantile computed when `scope="grid"` is used only to decide inclusion and is
#'   not persisted anywhere; `detection_min_quant` records the requested
#'   threshold, not the achieved quantile.
#'
#'   **Filter population & order (AUD-42).** Filters apply in the order
#'   `min_score` -> `min_quant` -> `top_n`. Under `scope = "grid"` the
#'   `min_quant` quantile is computed over the population *surviving* the
#'   preceding `min_score` filter (an ECDF over the current `peak_score`), so
#'   composing filters is order-dependent and differs from `scope = "pair"`,
#'   which ranks against each pair's capture-time `peak_quant` over the
#'   unfiltered pair population.
#'
#' @param detec a detections table (any subset of the standard schema; coerced).
#' @param min_score keep `peak_score >= min_score` (`NULL` = no filter).
#' @param min_quant keep detections at/above this score quantile (`NULL` = none).
#' @param top_n keep at most this many detections, by raw score (`NULL` = all).
#' @param scope `"pair"` (default) or `"grid"`.
#' @return a tibble of filtered detections in the standard schema.
#' @seealso [fetch_score_peaks()].
#' @name filter_detections_i
#' @keywords internal
#' @noRd
filter_detections_i <- function(detec, min_score = NULL, min_quant = NULL,
                              top_n = NULL, scope = c("pair", "grid")) {
  scope <- match.arg(scope)
  .validate_score_filters(min_score, min_quant, top_n)
  out <- .coerce_detections(detec)
  if (nrow(out) == 0L) return(tibble::as_tibble(out))

  if (!is.null(min_score)) {
    out <- out[out$peak_score >= min_score, , drop = FALSE]
    if (nrow(out) > 0L) out$detection_min_score <- min_score
  }
  if (!is.null(min_quant) && nrow(out) > 0L) {
    q <- if (scope == "grid") stats::ecdf(out$peak_score)(out$peak_score) else out$peak_quant
    out <- out[q >= min_quant, , drop = FALSE]
    if (nrow(out) > 0L) out$detection_min_quant <- min_quant
  }
  if (!is.null(top_n) && nrow(out) > 0L) {
    out <- .apply_top_n(out, as.integer(top_n), scope)
  }
  tibble::as_tibble(out)
}

#' Apply per-template detection thresholds in batch (AFL-16)
#'
#' @description Closes the diagnostics -> filter loop: applies a tidy table of
#'   per-template thresholds to the **raw** detections in one pass, reusing
#'   the internal `filter_detections_i()` per template. Thresholds typically come from
#'   [diagnostic_validations()] `score_cut` (supplied as `min_score`), but any
#'   combination of `min_score` / `min_quant` / `top_n` columns is honoured. Only
#'   templates listed in `thresholds` appear in the result.
#'
#' @param detec a detections table (standard schema; coerced).
#' @param thresholds a tidy data.frame, one row per template: a `key` column plus
#'   any of `min_score` / `min_quant` / `top_n` (an `NA` cell skips that filter for
#'   that template).
#' @param key the column joining `thresholds` to `detec`. Default `"template_name"`.
#' @param scope `"grid"` (default; the quantile / `top_n` are computed within each
#'   template's pooled detections) or `"pair"` (per-pair `peak_quant`). Forwarded
#'   to the internal `filter_detections_i()`.
#' @return a tibble of filtered detections (standard schema), row-bound across the
#'   listed templates.
#' @seealso `filter_detections_i()` (internal), [diagnostic_validations()].
#' @name filter_detections
#' @export
filter_detections <- function(detec, thresholds, key = "template_name",
                                    scope = c("grid", "pair")) {
  scope <- match.arg(scope)
  if (!is.data.frame(thresholds) || nrow(thresholds) == 0L) {
    stop("`thresholds` must be a non-empty data.frame, one row per template.")
  }
  if (!key %in% names(thresholds)) {
    stop("`thresholds` is missing the key column: ", key)
  }
  # AUD-41: a duplicate key filters that template's detections twice and
  # bind_rows keeps BOTH copies (silent duplication into validation/diagnostics).
  dup_keys <- unique(thresholds[[key]][duplicated(thresholds[[key]])])
  if (length(dup_keys) > 0L) {
    stop("`thresholds` has duplicate ", key, " value(s): ",
         paste(dup_keys, collapse = ", "),
         " (each template must appear once, else its detections are duplicated).")
  }
  if (length(intersect(c("min_score", "min_quant", "top_n"),
                       names(thresholds))) == 0L) {
    stop("`thresholds` needs at least one of min_score / min_quant / top_n.")
  }
  out <- .coerce_detections(detec)
  if (!key %in% names(out)) {
    stop("`detec` is missing the key column: ", key)
  }
  thr_val <- function(i, col) {
    if (!col %in% names(thresholds)) return(NULL)
    v <- thresholds[[col]][i]
    if (is.na(v)) NULL else v
  }
  parts <- lapply(seq_len(nrow(thresholds)), function(i) {
    sub <- out[!is.na(out[[key]]) & out[[key]] == thresholds[[key]][i], ,
               drop = FALSE]
    if (nrow(sub) == 0L) return(NULL)
    filter_detections_i(sub, min_score = thr_val(i, "min_score"),
                      min_quant = thr_val(i, "min_quant"),
                      top_n = thr_val(i, "top_n"), scope = scope)
  })
  res <- dplyr::bind_rows(parts)
  if (nrow(res) == 0L) return(tibble::as_tibble(out[0, , drop = FALSE]))
  res
}

#' Validate the optional numeric detection filters (shared with the matching
#' dispatcher [run_matching_i()] and [filter_detections_i()] / [fetch_score_peaks()]).
#' @keywords internal
#' @noRd
.validate_score_filters <- function(min_score, min_quant, top_n) {
  if (!is.null(min_score) &&
      (!is.numeric(min_score) || min_score < 0 || min_score > 1)) {
    stop("min_score must be NULL or a number between 0 and 1")
  }
  if (!is.null(min_quant) &&
      (!is.numeric(min_quant) || min_quant < 0 || min_quant > 1)) {
    stop("min_quant must be NULL or a number between 0 and 1")
  }
  # AUD-43: require a whole number so top_n = 2.7 no longer silently keeps 2.
  if (!is.null(top_n) && (!is.numeric(top_n) || length(top_n) != 1L ||
      is.na(top_n) || top_n < 1 || top_n != trunc(top_n))) {
    stop("top_n must be NULL or a single whole number >= 1")
  }
  invisible(TRUE)
}

#' Keep the top-n detections by raw score, per-pair or grid-wide (FSP-10/11)
#' @keywords internal
#' @noRd
.apply_top_n <- function(out, top_n, scope) {
  keep_top <- function(df) {
    if (top_n > nrow(df)) {
      warning(sprintf(
        "top_n (%d) exceeds the %d available detection(s)%s; keeping all.",
        top_n, nrow(df),
        if (scope == "pair") " in a pair" else ""
      ), call. = FALSE)
    }
    ord <- order(df$peak_score, decreasing = TRUE)        # FSP-11: raw score
    df <- df[ord[seq_len(min(top_n, nrow(df)))], , drop = FALSE]
    df$detection_top_n <- top_n                            # FSP-10: always recorded
    df[order(df$peak_index), , drop = FALSE]
  }
  if (scope == "grid") return(keep_top(out))
  # Character grouping key (NA -> "NA") so an all-NA template_id is not dropped by
  # split() the way an NA factor level would be.
  grp <- paste(out$soundscape_path, out$template_id, out$score_method, sep = "\r")
  parts <- split(out, factor(grp, levels = unique(grp)))
  res <- do.call(rbind, lapply(parts, keep_top))
  rownames(res) <- NULL
  res
}
