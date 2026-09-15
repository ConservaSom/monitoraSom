#' Score detection quality for every template in a validated run
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Turns validated detections into per-template performance diagnostics. It
#'   splits the validation results by template and runs the single-template
#'   diagnostics on each one, so a whole matching run is summarised in a single
#'   call. It reads the output of [validate_by_overlap()] and produces, for
#'   each template, a metrics table and its diagnostic plots.
#'
#' @details
#'   Each template is evaluated independently: its detections and its false
#'   negatives are pulled out (matched by `template_name`) and diagnosed with
#'   the automatic score threshold, so every template gets its own
#'   precision/recall/F1 table and score threshold. The return value is a list named by
#'   `template_name`, letting you index a single template's diagnostics directly.
#'
#'   Although built for template validation in the monitoraSom flow, this
#'   function also validates the classes of multiclass models such as BirdNET
#'   (imported with [import_birdnet_detections()]): each class is evaluated as
#'   if it were one template. Note the limit of this reading: the evaluation is
#'   per class only, never for the model as a whole.
#'
#'   The two metrics to watch are **precision** (of the detections kept, how
#'   many were real?) and **recall** (of the real signals present, how many
#'   were found?). The diagnostics themselves explain each metric in
#'   plain terms when you read one template's report.
#'
#'   Use this at the end of the analysis flow, once detections have been
#'   validated, to answer "how well did each template perform, and where should
#'   its score threshold sit?".
#'
#'   Things to know: a template whose diagnostics fail (for example too few
#'   detections to fit the model) does **not** abort the run, but returns a
#'   slot with `NULL`; a warning names the template and the reason, so always
#'   check for `NULL` slots before using a result. Set `build_plots = FALSE`
#'   when you only need the metrics tables, as it skips the plot construction
#'   and makes a large batch sweep much cheaper.
#'
#' @section Pipeline context:
#'   Step 12 of the monitoraSom analysis flow (metrics). Reads the validated
#'   detections from [validate_by_overlap()] (step 11). Produces one
#'   per-template diagnostics result (metrics table, score threshold, plots) for
#'   inspection or reporting. See also [validate_by_overlap()] and
#'   [plot_scores()].
#'
#' @param df_validated The validated detections to diagnose (required). Accepts
#'   the [validate_by_overlap()] split list
#'   (`list(detections_validated =, false_negatives =)`, the routine input), a
#'   single wide validation data.frame carrying a `template_name` column
#'   (back-compatible), or a path to a unified signals `.duckdb` store: the
#'   TP/FP verdicts are read from the `val_*` columns and the false negatives
#'   are computed by query (DEC-24/STEP-20b).
#' @param pos_prob Numeric in `[0, 1]` (default `0.95`). Target probability that
#'   a detection at the automatic threshold is a true positive; raise it for a
#'   stricter threshold (fewer higher-confidence detections), lower it for a
#'   more inclusive threshold (more lower-confidence detections).
#' @param build_plots Logical (default `TRUE`). `TRUE` builds the per-template
#'   plots; set `FALSE` to return metrics only (much faster for large batches).
#' @param theme_mode `"light"` (default) or `"dark"`, the shared plot theme,
#'   matching [plot_scores()]. Ignored when `build_plots = FALSE`.
#'
#' @return A named list (one element per `template_name`); each element holds
#'   `diagnostics` (metrics table), `score_cut` (the chosen score threshold),
#'   `bin_mod` (the fitted GLM) and five diagnostic plots, or `NULL` when that
#'   template's diagnostics failed.
#'
#' @seealso [validate_by_overlap()] (upstream, step 11), [plot_scores()]
#'   (step 13).
#' @export
#' @examples
#' \donttest{
#' # Step 12: diagnose a validated run (output of validate_by_overlap(), step 11).
#' # Load the package
#' library(monitoraSom)
#' data(df_detecs_val_tovlp)
#' diags <- diagnostic_validations(df_detecs_val_tovlp)
#' names(diags)                       # one result per template
#' head(diags[[1]]$diagnostics)       # precision / recall / F1 table
#'
#' # Compare templates at a glance: for each template, the diagnostics row
#' # with the best F1 (the operating point you would pick by hand). High
#' # performance means high precision AND high recall; low values in either
#' # one point to a weak template. The `selected` column, in turn, flags the
#' # row chosen by the automatic threshold; it can be empty when that
#' # threshold sits above every observed score.
#' best <- do.call(rbind, lapply(diags, function(d)
#'   d$diagnostics[which.max(d$diagnostics$F1_score),
#'     c("template_name", "peak_score", "precision", "recall", "F1_score")]))
#' best
#'
#' # Read the plots of one template (each slot is a ggplot object):
#' d1 <- diags[[1]]
#' d1$mod_plot      # score vs TP/FP: good separation supports a reliable
#'                  # automatic threshold
#' d1$roc_plot      # ROC: curve hugging the top-left corner means high recall
#'                  # at a low false-positive cost
#' d1$precrec_plot  # precision-recall: stays high as recall grows in a robust
#'                  # template
#' d1$f1_plot       # F1 along the threshold: a peak far from the edges marks a
#'                  # clear operating point
#' d1$plot_dens     # TP and FP score distributions: little overlap means the
#'                  # template separates well
#'
#' # Simulated contrast: what a clean and a noisy template look like.
#' sim_template <- function(name, n_tp, n_fp, tp_mean, sd) {
#'   data.frame(
#'     template_name = name,
#'     peak_score    = c(rnorm(n_tp, tp_mean, sd), rnorm(n_fp, 0.15, sd)),
#'     validation    = c(rep("TP", n_tp), rep("FP", n_fp)))
#' }
#' set.seed(1)
#' df_sim <- rbind(
#'   sim_template("clean", 30, 30, 0.60, 0.06),  # TP scores far above FP
#'   sim_template("noisy", 30, 30, 0.35, 0.15))  # TP scores overlap FP
#' diags_sim <- diagnostic_validations(df_sim)
#' diags_sim$clean$f1_plot   # clear peak: an obvious working threshold
#' diags_sim$noisy$f1_plot   # low, flat curve: weak template; consider
#'                           # re-cutting it or building a new one
#'
#' # The chosen threshold is the central decision of the diagnostics, and it is
#' # drawn in every plot as a dashed vertical line: mod_plot and plot_dens at
#' # score_cut, f1_plot at the selected row's score, precrec_plot and roc_plot
#' # at the selected operating point. Read it off the plots and check the value:
#' diags_sim$clean$mod_plot   # the dashed line splits TP from FP cleanly
#' diags_sim$clean$score_cut  # the numeric threshold behind the line
#' diags_sim$clean$f1_plot    # the line sits at the F1 peak
#'
#' # Metrics only (no plots) for a fast batch sweep:
#' diags_fast <- diagnostic_validations(df_detecs_val_tovlp, build_plots = FALSE)
#' }
diagnostic_validations <- function(df_validated, pos_prob = 0.95,
                                   build_plots = TRUE,
                                   theme_mode = "light") {
  # DEC-24 (STEP-20b): a signals store path is a valid input. The store holds
  # the TP/FP verdicts (val_*) on detection rows; false negatives are computed
  # by query (ROIs of ground truth with no overlapping TP of that template),
  # so the diagnostics stay complete without persisting FN as rows.
  if (is.character(df_validated) && length(df_validated) == 1L &&
      grepl("(?i)\\.duckdb$", df_validated) && file.exists(df_validated)) {
    df_validated <- .dv_from_signals_store(df_validated)
  }
  parts <- .dv_split_by_template(df_validated)                          # DV-01

  res <- lapply(seq_along(parts$items), function(i) {
    tryCatch(
      suppressWarnings(diagnostic_validations_i(
        val_i = parts$items[[i]], diag_method = "auto",
        pos_prob = pos_prob,
        build_plots = build_plots, theme_mode = theme_mode)),
      error = function(e) {                                             # DV-02
        warning("diagnostic_validations: template '", parts$names[i],
                "' failed -- ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  })
  names(res) <- parts$names

  if (all(vapply(res, is.null, logical(1)))) {
    message("All diagnostic validations failed.")
  } else {
    message("Validation diagnostics completed successfully.")
  }
  res
}

# Read a unified signals store into the VBO-shaped split list (DEC-24,
# STEP-20b): TP/FP come from the val_* columns on detection rows; false
# negatives are computed here (ground-truth ROIs of a template's species that
# no TP of that template overlapped), mirroring .vbo_per_template_fn().
.dv_from_signals_store <- function(path) {
  con <- .signals_duckdb_connect(path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  sg <- .signals_duckdb_read(con)
  dv <- .signals_as_validations(sg)
  rois <- .signals_as_rois(sg)
  fn <- .dv_compute_fn(dv, rois)
  list(detections_validated = dv, false_negatives = fn)
}

# FN by query: for each template with detections, the ground-truth ROIs of its
# species that no TP detection of that template overlapped.
.dv_compute_fn <- function(dv, rois) {
  if (nrow(dv) == 0L || nrow(rois) == 0L) {
    out <- .coerce_rois(rois[0, , drop = FALSE])
    out$template_name <- character(0)
    out$template_file <- character(0)
    return(out)
  }
  tp <- dv[!is.na(dv$validation) & dv$validation == "TP", , drop = FALSE]
  tmpl <- dv[!duplicated(dv$template_file),
             c("template_file", "template_name", "template_label"),
             drop = FALSE]
  parts <- lapply(seq_len(nrow(tmpl)), function(i) {
    tf <- tmpl$template_file[i]
    sp <- if (!is.na(tmpl$template_label[i]) &&
              nzchar(tmpl$template_label[i])) tmpl$template_label[i] else
      tmpl$template_name[i]
    tps <- tp[tp$template_file == tf, , drop = FALSE]
    if (nrow(tps) == 0L) return(NULL)
    sp_rois <- rois[!is.na(rois$roi_label) & rois$roi_label == sp, ,
                    drop = FALSE]
    if (nrow(sp_rois) == 0L) return(NULL)
    hit <- logical(nrow(sp_rois))
    for (j in seq_len(nrow(sp_rois))) {
      same <- tps$soundscape_file == sp_rois$soundscape_file[j]
      ov <- same & dplyr::between(tps$detection_start, sp_rois$roi_start[j],
                                  sp_rois$roi_end[j]) |
        same & dplyr::between(tps$detection_end, sp_rois$roi_start[j],
                              sp_rois$roi_end[j]) |
        same & tps$detection_start <= sp_rois$roi_start[j] &
        tps$detection_end >= sp_rois$roi_end[j]
      hit[j] <- any(ov)
    }
    miss <- sp_rois[!hit, , drop = FALSE]
    if (nrow(miss) == 0L) return(NULL)
    out <- .coerce_rois(miss)
    out$template_name <- rep(tmpl$template_name[i], nrow(out))
    out$template_file <- rep(tf, nrow(out))
    out
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0L) {
    out <- .coerce_rois(rois[0, , drop = FALSE])
    out$template_name <- character(0)
    out$template_file <- character(0)
    return(out)
  }
  fn <- do.call(rbind, parts)
  fn[order(fn$template_name, fn$soundscape_file, fn$roi_start), ,
     drop = FALSE]
}

# Split the input into one per-template engine input (DV-01). The VBO split list
# is sliced so each template keeps its own detections AND its own FN rows; a
# legacy wide frame is split by template_name (the original behaviour).
.dv_split_by_template <- function(x) {
  if (is.list(x) && !is.data.frame(x) &&
      all(c("detections_validated", "false_negatives") %in% names(x))) {
    dv <- as.data.frame(x$detections_validated)
    fn <- as.data.frame(x$false_negatives)
    tns <- unique(dv$template_name)
    items <- lapply(tns, function(tn) {
      list(
        detections_validated = dv[dv$template_name == tn, , drop = FALSE],
        false_negatives      = fn[fn$template_name == tn, , drop = FALSE]
      )
    })
    return(list(items = items, names = tns))
  }
  if (is.data.frame(x)) {
    if (!"template_name" %in% names(x)) {
      stop("`df_validated` must have a `template_name` column.")
    }
    tns <- unique(x$template_name)
    items <- lapply(tns, function(tn) x[x$template_name == tn, , drop = FALSE])
    return(list(items = items, names = tns))
  }
  stop("`df_validated` must be a `validate_by_overlap()` split list or a ",
       "validation data.frame.")
}
