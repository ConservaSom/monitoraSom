#' Pure ROI helpers for the segmentation app (LSA-05)
#'
#' @description Side-effect-free ROI helpers extracted from the
#'   `launch_segmentation_app()` closures so they become reachable by testthat
#'   and reusable by the Julia port. This file is part of the LSA-05 structural
#'   refactor; behaviour is preserved unchanged from the alt app, except for the
#'   LSA-04 status consolidation (see [.status_from_labels()]).
#'
#'   The empty/skeleton ROI frame (alt app `empty_roi_df()`) now lives in
#'   [_schema_rois.R] as `.schema_rois()`.
#'
#' @keywords internal
#' @noRd

#' Is `new_roi` a near-duplicate of any row in `existing_rois`?
#'
#' Matches the alt app `is_duplicate_roi`: same `roi_label` and all four bounds
#' equal within `tol`. Reimplemented in base R (no dplyr NSE) so it is safe at
#' file scope; NA comparisons are ignored (as dplyr::filter dropped them).
#'
#' @param new_roi one-row data.frame/list with the bound + label fields.
#' @param existing_rois data.frame of stored ROIs (may be NULL/empty).
#' @param tol numeric tolerance on the four bounds. Default 0.001.
#' @return single logical.
#' @noRd
.is_duplicate_roi <- function(new_roi, existing_rois, tol = 0.001) {
  if (is.null(existing_rois) || nrow(existing_rois) == 0) return(FALSE)
  any(
    existing_rois$roi_label == new_roi$roi_label &
      abs(existing_rois$roi_start    - new_roi$roi_start)    < tol &
      abs(existing_rois$roi_end      - new_roi$roi_end)      < tol &
      abs(existing_rois$roi_min_freq - new_roi$roi_min_freq) < tol &
      abs(existing_rois$roi_max_freq - new_roi$roi_max_freq) < tol,
    na.rm = TRUE
  )
}

#' Reject a write batch that carries duplicate ROIs (LSA-204, D6 = B).
#'
#' Interim write-time guard: a duplicate is the same `roi_label` + all four
#' bounds within `tol` (the interactive rule [.is_duplicate_roi()]) **on the
#' same `roi_channel`** — two channels legitimately carry the same
#' vocalization. `no_soi` sentinel rows never conflict (the sentinel is a
#' status marker, not an annotation; LSA-04 allows several). O(n^2) pairwise
#' scan — n is one write batch (one soundscape's ROIs), hundreds at most.
#' VBO-104 tracks the refined design (unique index tolerating
#' delete-then-reinsert) that supersedes this check.
#'
#' @param df data.frame of ROI rows (signals-store shape acceptable).
#' @param tol numeric tolerance on the four bounds. Default 0.001.
#' @return invisible(NULL); `stop()` naming label and bounds of the first
#'   duplicate pair when one exists.
#' @noRd
.reject_duplicate_roi_batch <- function(df, tol = 0.001) {
  if (is.null(df) || nrow(df) < 2L) return(invisible(NULL))
  lab <- if ("roi_label" %in% names(df)) df$roi_label else rep(NA_character_, nrow(df))
  ch  <- if ("roi_channel" %in% names(df)) df$roi_channel else rep(NA_character_, nrow(df))
  keep <- !.is_no_soi_label(lab)
  idx <- which(keep)
  for (i in idx) {
    for (j in idx[idx > i]) {
      same_ch <- (is.na(ch[i]) && is.na(ch[j])) || (!is.na(ch[i]) && ch[i] == ch[j])
      if (!same_ch || is.na(lab[i]) || is.na(lab[j])) next
      if (isTRUE(.is_duplicate_roi(df[i, , drop = FALSE], df[j, , drop = FALSE], tol))) {
        fmt <- function(r) {
          sprintf("label '%s' [%s-%s s, %s-%s kHz, channel %s]",
                  r$roi_label, format(r$roi_start), format(r$roi_end),
                  format(r$roi_min_freq), format(r$roi_max_freq),
                  if (is.na(r$roi_channel)) "NA" else r$roi_channel)
        }
        stop("LSA-204: duplicate ROI in this batch -- rows ", i, " and ", j,
             " are the same annotation:\n  ", fmt(df[i, ]), "\n  ",
             fmt(df[j, ]),
             "\nRemove one of them before saving.")
      }
    }
  }
  invisible(NULL)
}

#' Classify a soundscape's status from its ROI labels (LSA-04).
#'
#' Single source of truth for status classification, shared by the in-memory
#' path ([.derive_status_from_rois()]) and mirrored by the DB classifier
#' ([.roi_duckdb_statuses()], which applies the identical rule in SQL via
#' `n_soi == n_total`). Rules:
#'   - no labels (length 0)                -> "unsegmented"
#'   - every label is the no_soi sentinel  -> "no_soi"
#'   - otherwise                           -> "segmented"
#' This resolves the LSA-04 divergence: the old in-memory rule treated only a
#' *single* sentinel row as `no_soi`, while the DB rule required *all* rows to be
#' sentinels. The all-sentinels rule is now standard on both paths. NA-safe via
#' `.is_no_soi_label()` (a real ROI with an NA label is not a sentinel).
#'
#' @param labels character vector of `roi_label` values (may be length 0).
#' @return one of "unsegmented", "segmented", "no_soi".
#' @noRd
.status_from_labels <- function(labels) {
  if (length(labels) == 0) return("unsegmented")
  if (all(.is_no_soi_label(labels))) return("no_soi")
  "segmented"
}

#' Derive a soundscape's status from its in-memory ROI data.frame.
#'
#' Thin wrapper over [.status_from_labels()] (LSA-04 consolidation): an empty /
#' NULL frame is `unsegmented`, otherwise the shared label rule decides.
#'
#' @param roi_df ROI data.frame (may be NULL/empty).
#' @return one of "unsegmented", "segmented", "no_soi".
#' @noRd
.derive_status_from_rois <- function(roi_df) {
  if (is.null(roi_df) || nrow(roi_df) == 0) return("unsegmented")
  .status_from_labels(roi_df$roi_label)
}

#' Index of the nearest unsegmented soundscape in a direction (LSA-18).
#'
#' Pure navigation helper extracted so the "prev/next unsegmented" workflow is
#' testable. Given the per-file segmented flags (in display order) and the
#' current global row index, return the index of the nearest file with
#' `has_table == FALSE` strictly before (`"prev"`) or after (`"next"`) the
#' current index, or `NA_integer_` if there is none. Basing navigation on the
#' global index (not on a filtered subset that still includes the current file)
#' fixes the LSA-18 off-by-one/skip after an autosave flips the current file to
#' segmented.
#'
#' @param has_table logical vector; TRUE = already segmented, in display order.
#' @param current_index 1-based index of the current file in that order.
#' @param direction `"prev"` or `"next"`.
#' @return integer index, or `NA_integer_` when no unsegmented neighbour exists.
#' @noRd
.next_unsegmented_index <- function(has_table, current_index, direction) {
  unseg <- which(!has_table)
  cand <- switch(direction,
    "prev" = unseg[unseg < current_index],
    "next" = unseg[unseg > current_index],
    stop(sprintf("Unknown direction: %s", direction))
  )
  if (length(cand) == 0) return(NA_integer_)
  as.integer(switch(direction, "prev" = max(cand), "next" = min(cand)))
}

#' Build the "no signals of interest" sentinel ROI row (LSA-06 / LSA-23).
#'
#' Pure single-row tibble builder for the no_soi sentinel, extracted from the app
#' so the two former call sites (`perform_no_soi` and the `confirm_no_soi`
#' observer) share one definition and so it is reachable by testthat. Honours the
#' sentinel contract in [_schema_rois.R]: `roi_label == .ROI_NO_SOI_LABEL`,
#' `roi_start = 0`, `roi_end = duration`, frequency bounds spanning the visible
#' window, and the `roi_type`/confidence/completeness/comment fields `NA`.
#'
#' @param soundscape_path durable recording path (LSA-19); `soundscape_file` is
#'   derived as its basename.
#' @param user current `roi_user`.
#' @param duration full recording duration -> `roi_end`.
#' @param freq_min,freq_max visible frequency window bounds (kHz).
#' @param wl,ovlp,sample_rate,pitch_shift session parameters.
#' @param channel `roi_channel` (LSA-109): `"mono"` for mono recordings, else the
#'   displayed channel (`"left"`/`"right"`). Default `"mono"`.
#' @param timestamp `roi_input_timestamp` string; defaults to now (parameterised
#'   so tests are deterministic).
#' @return one-row tibble in (a superset-free subset of) the standard ROI schema.
#' @noRd
.make_no_soi_roi <- function(soundscape_path, user, duration, freq_min, freq_max,
                             wl, ovlp, sample_rate, pitch_shift,
                             channel = "mono",
                             timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")) {
  tibble::tibble(
    soundscape_path = soundscape_path,
    soundscape_file = basename(soundscape_path),
    roi_user = user,
    roi_input_timestamp = timestamp,
    roi_label = .ROI_NO_SOI_LABEL,
    roi_start = 0,
    roi_end = duration,
    roi_min_freq = freq_min,
    roi_max_freq = freq_max,
    roi_type = NA_character_,
    roi_label_confidence = NA_character_,
    roi_is_complete = NA_character_,
    roi_comment = NA_character_,
    roi_wl = wl,
    roi_ovlp = ovlp,
    roi_sample_rate = sample_rate,
    roi_pitch_shift = pitch_shift,
    roi_channel = channel
  )
}

#' Identify the active ROI under a clicked point (LSA-104).
#'
#' Pure hit-test for the shared active-ROI selection model (left-click on the
#' spectrogram). Given the in-memory ROI frame and the click coordinates in data
#' units (time in seconds on x, frequency in kHz on y — Shiny returns plot-click
#' coordinates already converted from pixels), return the stable row id (the
#' 1-based `row_number()` over the *unfiltered* `roi_values()`, matching the plot
#' `id` and the DT selection from LSA-22) of the ROI to activate, or
#' `NA_integer_` when the click lands on empty space.
#'
#' A point hits a ROI when `roi_start <= x <= roi_end` and
#' `roi_min_freq <= y <= roi_max_freq`. When several ROIs overlap the point, the
#' smallest-area ROI wins (the most specific target; ties broken by the first
#' such row), so a small ROI nested inside a broad one stays selectable. Rows
#' with any NA bound (e.g. a partially-NA frame) never match.
#'
#' @param rois ROI data.frame/tibble with numeric `roi_start`, `roi_end`,
#'   `roi_min_freq`, `roi_max_freq` columns (may be NULL/empty).
#' @param x,y click coordinates in data units (seconds, kHz).
#' @return integer row id of the hit ROI, or `NA_integer_` if none.
#' @noRd
.roi_at_point <- function(rois, x, y) {
  if (is.null(rois) || nrow(rois) == 0) return(NA_integer_)
  if (is.null(x) || is.null(y) || is.na(x) || is.na(y)) return(NA_integer_)
  xmin <- as.numeric(rois$roi_start)
  xmax <- as.numeric(rois$roi_end)
  ymin <- as.numeric(rois$roi_min_freq)
  ymax <- as.numeric(rois$roi_max_freq)
  hit <- !is.na(xmin) & !is.na(xmax) & !is.na(ymin) & !is.na(ymax) &
    x >= xmin & x <= xmax & y >= ymin & y <= ymax
  if (!any(hit)) return(NA_integer_)
  area <- (xmax - xmin) * (ymax - ymin)
  area[!hit] <- Inf
  as.integer(which.min(area))
}

#' Identify the detection under a clicked point (LSA-208).
#'
#' Pure hit-test mirroring [.roi_at_point()], but over a signals-shaped
#' detection frame (as read from the signals store), returning the stable
#' `signal_id` of the hit detection instead of a row number: the overlay frame
#' is a reactive subset (toggle + score/template filters), so a positional id
#' would go stale between the click and the promotion. A point hits a
#' detection when its box contains `(x, y)` in data units (seconds, kHz);
#' overlapping detections resolve to the smallest-area box (most specific).
#'
#' @param dets signals-shaped data.frame with `signal_id`, `roi_start`,
#'   `roi_end`, `roi_min_freq`, `roi_max_freq` columns (may be NULL/empty).
#' @param x,y click coordinates in data units (seconds, kHz).
#' @return character `signal_id` of the hit detection, or `NA_character_`.
#' @noRd
.signal_at_point <- function(dets, x, y) {
  if (is.null(dets) || nrow(dets) == 0) return(NA_character_)
  if (is.null(x) || is.null(y) || is.na(x) || is.na(y)) return(NA_character_)
  xmin <- as.numeric(dets$roi_start)
  xmax <- as.numeric(dets$roi_end)
  ymin <- as.numeric(dets$roi_min_freq)
  ymax <- as.numeric(dets$roi_max_freq)
  hit <- !is.na(xmin) & !is.na(xmax) & !is.na(ymin) & !is.na(ymax) &
    x >= xmin & x <= xmax & y >= ymin & y <= ymax
  if (!any(hit)) return(NA_character_)
  area <- (xmax - xmin) * (ymax - ymin)
  area[!hit] <- Inf
  dets$signal_id[which.min(area)]
}

#' Draw the read-only detection overlay onto the spectrogram plot (LSA-208).
#'
#' Pure ggplot layer: draws four corner brackets per detection box (minimal
#' visual footprint -- gate review 2026-08-18: full dashed outlines cluttered
#' dense overlays; the complete contour appears only on the selected
#' detection and, after promotion, as a regular ROI box). Detections outside
#' the visible time window are skipped, mirroring the ROI overlap filter, so
#' a dense overlay never draws off-screen marks.
#'
#' @param plot a ggplot (the current spectrogram plot).
#' @param dets signals-shaped detection frame (NULL/empty = no layer).
#' @param selected_id `signal_id` of the selected detection (`NA_character_`
#'   when none) -- it gets the full solid outline + a light fill.
#' @param zoom_time length-2 numeric visible time window (seconds).
#' @return the plot with the overlay layers added.
#' @noRd
.annotate_detection_overlay <- function(plot, dets, selected_id = NA_character_,
                                          zoom_time) {
  if (is.null(dets) || nrow(dets) == 0) return(plot)
  keep <- !is.na(dets$roi_start) & !is.na(dets$roi_end) &
    dets$roi_start < zoom_time[2] & dets$roi_end > zoom_time[1]
  dets <- dets[keep, , drop = FALSE]
  if (nrow(dets) == 0) return(plot)
  col <- "#00bcd4"                       # cyan: theme-agnostic, unlike ROI white/black
  xs <- dets$roi_start
  xe <- dets$roi_end
  yo <- dets$roi_min_freq
  ym <- dets$roi_max_freq
  lx <- pmax(0.05, 0.2 * (xe - xs))        # bracket arm length: 20% per side,
  ly <- pmax(0.05, 0.2 * (ym - yo))        # floored so tiny boxes stay visible
  # Eight segments per box (2 per corner): TL, TR, BL, BR.
  plot <- plot +
    ggplot2::annotate("segment",
      x = c(xs, xs, xe, xe, xs, xs, xe, xe),
      xend = c(xs + lx, xs, xe - lx, xe, xs + lx, xs, xe - lx, xe),
      y = c(ym, ym, ym, ym, yo, yo, yo, yo),
      yend = c(ym, ym - ly, ym, ym - ly, yo, yo + ly, yo, yo + ly),
      color = col, linewidth = 0.7, linetype = "solid", alpha = 1
    )
  if (!is.na(selected_id) && selected_id %in% dets$signal_id) {
    sel <- dets[dets$signal_id == selected_id, , drop = FALSE]
    plot <- plot +
      ggplot2::annotate("rect",
        alpha = 0.15, linewidth = 1.2, linetype = "solid",
        fill = col, color = col,
        xmin = sel$roi_start, xmax = sel$roi_end,
        ymin = sel$roi_min_freq, ymax = sel$roi_max_freq
      )
  }
  plot
}

# LSA-106: incremental-resize steps for the active ROI (applied per key press,
# symmetrically to both edges of the axis). Tuned for bioacoustic ROIs; kept as
# named constants so they are easy to find and adjust.
# LSA-118 (user, 2026-05-30): time expansion felt too slow and frequency too
# fast, so the time step was raised (0.05 -> 0.10 s, x2) and the frequency step
# lowered (0.5 -> 0.30 kHz, x0.6). Fine-tune at the manual gate if needed.
.ROI_TIME_STEP_S   <- 0.10   # seconds added/removed at each time edge
.ROI_FREQ_STEP_KHZ <- 0.30   # kHz added/removed at each frequency edge
.ROI_MIN_TIME_S    <- 0.01   # never contract a ROI shorter than this (s)
.ROI_MIN_FREQ_KHZ  <- 0.1    # never contract a ROI narrower than this (kHz)

#' Expand or contract one ROI axis interval, clamped (LSA-106).
#'
#' Pure resize for one axis of the active ROI: `"expand"` grows the `(lo, hi)`
#' interval by `step` at *each* edge (total `+2*step`), clamped to
#' `[limit_lo, limit_hi]`; `"contract"` shrinks it by `step` at each edge but
#' never below `min_extent` (a contract that would invert/over-shrink collapses
#' the interval to a `min_extent`-wide window centred on the original midpoint).
#' Used for both the time axis (`roi_start`/`roi_end`, limits `[0, duration]`)
#' and the frequency axis (`roi_min_freq`/`roi_max_freq`, limits `[0, nyquist]`).
#'
#' @param lo,hi current interval bounds (`lo <= hi`).
#' @param step positive increment per edge.
#' @param direction `"expand"` or `"contract"`.
#' @param limit_lo,limit_hi hard clamp bounds for expansion.
#' @param min_extent minimum allowed `hi - lo` after a contract.
#' @return length-2 numeric `c(lo, hi)`.
#' @noRd
.resize_interval <- function(lo, hi, step, direction,
                             limit_lo, limit_hi, min_extent = 0) {
  if (direction == "expand") {
    lo <- max(limit_lo, lo - step)
    hi <- min(limit_hi, hi + step)
  } else if (direction == "contract") {
    new_lo <- lo + step
    new_hi <- hi - step
    if (new_hi - new_lo < min_extent) {
      mid <- (lo + hi) / 2
      new_lo <- mid - min_extent / 2
      new_hi <- mid + min_extent / 2
    }
    lo <- new_lo
    hi <- new_hi
  } else {
    stop(sprintf("Unknown direction: %s", direction))
  }
  c(lo, hi)
}

# Built-in ROI signal-type vocabulary, shared by the inline Type selector and the
# LSA-112 metadata popup (single source of truth; LSA-101 will later allow custom
# user-defined types on top of this list).
.ROI_TYPE_CHOICES <- c(
  "advertisement", "alarm", "amplectant", "anthopophony", "call",
  "courtship", "displacement", "distress", "echolocation call",
  "encounter", "feeding buzz", "fighting", "geophony", "mechanical",
  "other", "rain", "release", "social call", "song", "territorial",
  "warning"
)

#' Write metadata fields into one ROI row (LSA-112).
#'
#' Pure helper: overwrite the five user-editable metadata columns of row `id`
#' (`roi_label`, `roi_type`, `roi_label_confidence`, `roi_is_complete`,
#' `roi_comment`) with the supplied values and return the updated frame. The
#' geometry columns (`roi_start`/`roi_end`/`roi_min_freq`/`roi_max_freq`) and all
#' provenance columns are left untouched. Used by the metadata popup to apply its
#' values to the active ROI; extracted so the field mapping is testable.
#'
#' @param rois ROI data.frame/tibble.
#' @param id 1-based row index to update.
#' @param label,type,confidence,complete,comment replacement values.
#' @return the updated `rois`.
#' @noRd
.apply_roi_metadata <- function(rois, id, label, type, confidence,
                                complete, comment) {
  rois$roi_label[id]            <- label
  rois$roi_type[id]             <- type
  rois$roi_label_confidence[id] <- confidence
  rois$roi_is_complete[id]      <- complete
  rois$roi_comment[id]          <- comment
  rois
}

#' Overwrite one ROI row's geometry bounds (LSA-115).
#'
#' Pure helper: replace the four geometry columns of row `id` (`roi_start`,
#' `roi_end`, `roi_min_freq`, `roi_max_freq`) with new values and return the
#' updated frame; metadata and provenance columns are left untouched. Used by the
#' boundary re-selection mode to commit a freshly drawn brush onto the active
#' ROI. The brush already yields `xmin <= xmax` and `ymin <= ymax`, so no
#' re-ordering is done here.
#'
#' @param rois ROI data.frame/tibble.
#' @param id 1-based row index to update.
#' @param start,end new time bounds (s).
#' @param min_freq,max_freq new frequency bounds (kHz).
#' @return the updated `rois`.
#' @noRd
.set_roi_bounds <- function(rois, id, start, end, min_freq, max_freq) {
  rois$roi_start[id]    <- start
  rois$roi_end[id]      <- end
  rois$roi_min_freq[id] <- min_freq
  rois$roi_max_freq[id] <- max_freq
  rois
}

#' Select template ROIs for auto-export on soundscape advance (LSA-110).
#'
#' Pure filter for the user's "templates are flagged by the word `template` in
#' `roi_comment`" convention (user decision 2026-05-30, keeping the template
#' metadata encoded in the exported cut filename). Returns only **real, cuttable**
#' template rows: `no_soi` sentinels and rows with an NA time bound are excluded
#' so `export_templates()` never tries to cut a sentinel or an incomplete ROI.
#' Matching on `roi_comment` is a case-insensitive substring (e.g. "Template",
#' "song template" all match).
#'
#' @param rois ROI data.frame/tibble (may be NULL/empty).
#' @return a data.frame of the template rows (0 rows if none), schema preserved.
#' @noRd
.select_template_rois <- function(rois) {
  if (is.null(rois) || nrow(rois) == 0) return(.schema_rois(0L))
  is_tmpl <- !is.na(rois$roi_comment) &
    grepl("template", rois$roi_comment, ignore.case = TRUE)
  is_real <- !.is_no_soi_label(rois$roi_label) &
    !is.na(rois$roi_start) & !is.na(rois$roi_end)
  rois[is_tmpl & is_real, , drop = FALSE]
}
