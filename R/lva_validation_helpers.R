#' Validate dynamic range vectors
#'
#' Validates that a dynamic range vector is a numeric vector of length 2 with the
#' first value less than the second. Sorts if necessary.
#'
#' @param dyn_range A numeric vector of length 2.
#' @param name The name of the parameter being validated (for error messages).
#' @return A sorted numeric vector of length 2.
#' @keywords internal
validate_dyn_range <- function(dyn_range, name) {
  if (length(dyn_range) != 2 || !all(is.numeric(dyn_range))) {
    stop(
      paste(
        "Error! '",
        name,
        "' must be a numeric vector of length 2 with all values numeric.",
        sep = ""
      )
    )
  }
  if (dyn_range[1] >= dyn_range[2]) {
    warning(
      paste(
        "Warning! The first value of '",
        name,
        "' must be smaller than the second. Sorting to match the expected order.",
        sep = ""
      )
    )
    return(sort(dyn_range))
  }
  return(dyn_range)
}

#' Validate and set a path, with auto-creation
#'
#' Validates a path for storing files. If the path is NULL, the default path is
#' used. If the default path does not exist, it is created automatically. If a
#' custom path is provided and does not exist, an error is raised.
#'
#' @param path The path to validate. May be NULL.
#' @param default_path The default path to use when path is NULL.
#' @param session_key A human-readable label for the path (used in messages).
#' @return The validated (and possibly default) path.
#' @keywords internal
validate_and_set_path <- function(path, default_path, session_key) {
  if (is.null(path)) {
    if (dir.exists(default_path)) {
      warning(paste(
        "Warning! The informed '",
        session_key,
        "' was not found locally; using the existing default path '",
        default_path,
        "'.",
        sep = ""
      ))
    } else {
      dir.create(default_path, recursive = TRUE)
      warning(paste(
        "Warning! The informed '",
        session_key,
        "' was not found locally and was created automatically as '",
        default_path,
        "'.",
        sep = ""
      ))
    }
    return(default_path)
  } else {
    if (dir.exists(path)) {
      return(path)
    } else {
      stop(paste(
        "Error! The provided path to store ",
        session_key,
        " was not found locally.",
        sep = ""
      ))
    }
  }
}

#' Validate a logical parameter
#'
#' Checks that a value is logical (TRUE or FALSE). If not, raises an error with
#' the parameter name in the message.
#'
#' @param value The value to check.
#' @param name The name of the parameter (for error messages).
#' @return The logical value, unchanged.
#' @keywords internal
validate_logical_param <- function(value, name) {
  if (is.logical(value)) {
    return(value)
  } else {
    stop(
      "Error! The value assigned to '", name,
      "' is not logical. Set it to TRUE or FALSE."
    )
  }
}

#' Validate zoom_freq parameter
#'
#' Validates the zoom_freq parameter: sorts the values first, then checks that
#' all values are within `[0, 192]`, and rounds to 0.1 intervals unconditionally.
#'
#' @param zoom_freq A numeric vector of length 2 representing the frequency
#'   range in kHz.
#' @return A sorted numeric vector of length 2, rounded to 0.1 kHz, with a
#'   warning if rounding changed the values.
#' @keywords internal
validate_zoom_freq <- function(zoom_freq) {
  # Sort first
  zoom_freq <- sort(zoom_freq)

  # Range-check on sorted values
  if (any(zoom_freq < 0) || any(zoom_freq > 192)) {
    stop("Error! 'zoom_freq' values must be between 0 and 192.")
  }

  # Round unconditionally
  rounded <- round(zoom_freq * 10) / 10
  if (any(rounded != zoom_freq)) {
    warning(
      "Warning! The values of 'zoom_freq' were rounded to the nearest 0.1 ",
      "interval. The values are now: ",
      rounded[1], " and ", rounded[2]
    )
  }
  return(rounded)
}

#' Padded cut window `[from, to]` for a detection cell (seconds) — LVA-107
#'
#' @description The time window read from the soundscape for one detection cell:
#'   the detection bounds expanded by `time_pads` on each side, with the start
#'   clamped at 0. Shared by the synchronous build and the background prefetch so
#'   both read EXACTLY the same window (and so the dashed-rectangle offset agrees).
#'
#' @param detection_start,detection_end Detection bounds in seconds.
#' @param time_pads Padding in seconds added to each side.
#' @return A numeric length-2 vector `c(from, to)`.
#' @keywords internal
#' @noRd
.lva_cut_window <- function(detection_start, detection_end, time_pads) {
  from <- detection_start - time_pads
  if (from < 0) from <- 0
  c(from, detection_end + time_pads)
}

#' Build one detection-cell spectrogram plot (pure, prefetch-safe) — LVA-107
#'
#' @description Computes the detection-grid cell spectrogram for a single
#'   `df_cut()` row from plain data only — it reads NO Shiny reactives or
#'   `input$` values. This purity lets the in-server `.lva_cell_plot()` wrapper
#'   gather the live parameters and delegate here, AND lets the LVA-107 prefetch
#'   pass a pre-read `Wave` via `rec` so the page-change critical path skips the
#'   disk read entirely.
#'
#'   For an R programmer: think of `params`/`rec` as the explicit arguments of a
#'   `parallel` worker closure — nothing is taken from the enclosing scope.
#'
#' @param row A one-row data.frame from `df_cut()` with at least
#'   `soundscape_path`, `detection_start`, `detection_end`, `template_min_freq`,
#'   `template_max_freq`, `peak_score`.
#' @param params A plain list of spectrogram parameters (no reactive values):
#'   `wl`, `ovlp`, `zoom_freq` (length-2), `dyn_range_detec` (length-2),
#'   `color_scale`, `pitch_shift`, `time_guide_interval`, `freq_guide_interval`,
#'   `time_pads`.
#' @param rec Optional pre-read `tuneR::Wave` for the cell's padded cut window
#'   (LVA-107 prefetch). When `NULL` the window is read synchronously here.
#' @return A `ggplot` object identical to what the inline cell renderer produced
#'   (dashed detection rectangle + in-plot score label + stripped axes).
#' @keywords internal
#' @noRd
.lva_build_cell_plot <- function(row, params, rec = NULL) {
  win <- .lva_cut_window(row$detection_start, row$detection_end, params$time_pads)
  ps <- win[1]
  if (is.null(rec)) {
    # STEP-2 (plan 2026-09-14_01): a row whose recording could not be resolved
    # carries `soundscape_path = NA` (or a path that vanished since setup).
    # `readWave` would throw and take the whole cell -- and the mirai daemon
    # that renders it -- down. Draw an explicit placeholder instead, so the
    # review session keeps running with the rest of the page.
    if (!.lva_path_readable(row$soundscape_path)) {
      return(.lva_missing_audio_plot(row, params))
    }
    rec <- tuneR::readWave(
      filename = row$soundscape_path,
      from = ps, to = win[2], units = "seconds"
    )
  }

  box_color <- ifelse(
    params$color_scale %in% c("greyscale 1", "greyscale 2"), "black", "white"
  )
  # Gate review (2026-08-19): a label correction is rendered ON the
  # spectrogram, alongside the original, so the user sees the fix at a
  # glance. "original -> corrected".
  base_label <- if (is.na(row$peak_score)) {
    as.character(row$roi_label)
  } else {
    paste0("score ", round(row$peak_score, 3))
  }
  corr <- if ("roi_label_updated" %in% names(row) &&
               length(row$roi_label_updated) == 1L &&
               !is.na(row$roi_label_updated) &&
               nzchar(row$roi_label_updated)) {
    as.character(row$roi_label_updated)
  } else {
    NA_character_
  }
  if (!is.na(corr) && !identical(corr, as.character(row$roi_label))) {
    base_label <- paste0(base_label, "  \u2192 ", corr)
  }
  fast_spectro(
    rec = rec,
    wl = params$wl,
    ovlp = params$ovlp,
    flim = c(params$zoom_freq[1], params$zoom_freq[2]),
    dyn_range = c(params$dyn_range_detec[1], params$dyn_range_detec[2]),
    time_guide_interval = params$time_guide_interval,
    freq_guide_interval = params$freq_guide_interval,
    color_scale = params$color_scale,
    pitch_shift = params$pitch_shift,
    norm = FALSE
  ) +
    # Dashed rectangle around the detection (time bounds within the padded cut ×
    # the detection's frequency band). Mirrors spectro_detection().
    ggplot2::annotate(
      "rect",
      xmin = row$detection_start - ps,
      xmax = row$detection_end - ps,
      ymin = row$template_min_freq,
      ymax = row$template_max_freq,
      linetype = "dashed", linewidth = 0.6, alpha = 0,
      color = box_color, fill = box_color
    ) +
    # In-plot score label at the TOP-LEFT corner (R4.3: was the panel title).
    # ROI-mode rows carry peak_score = NA -> the label degrades to the ROI label
    # so the cell still identifies its subject.
    ggplot2::annotate(
      "label",
      x = -Inf, y = Inf, hjust = -0.05, vjust = 1.05,
      label = base_label,
      size = 4.5, color = "white", fill = "black", alpha = 0.55
    ) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}

#' TRUE when `path` is a usable single recording path (STEP-2)
#'
#' @description Single predicate for "can this column be handed to `readWave`":
#'   character, length 1, not NA, non-empty and present on disk. Guards every
#'   read site so a row whose recording could not be resolved fails soft.
#'
#' @param path A path candidate from a review row (`soundscape_path`).
#' @return `TRUE`/`FALSE`.
#' @keywords internal
#' @noRd
.lva_path_readable <- function(path) {
  length(path) == 1L && is.character(path) && !is.na(path) && nzchar(path) &&
    file.exists(path)
}

#' Placeholder cell for a row whose recording is unavailable (STEP-2)
#'
#' @description Draws the same empty axes as a real cell, with a centred
#'   message naming the missing recording. Keeps the page grid complete and the
#'   image cache key stable, so navigating past a broken row is harmless.
#'
#' @param row The one-row `df_cut()` frame (uses `soundscape_file`).
#' @param params The spectrogram parameter list (uses `color_scale`).
#' @return A `ggplot` object.
#' @keywords internal
#' @noRd
.lva_missing_audio_plot <- function(row, params) {
  label <- if (!is.null(row$soundscape_file) &&
               length(row$soundscape_file) == 1L &&
               !is.na(row$soundscape_file)) {
    as.character(row$soundscape_file)
  } else {
    "(unknown recording)"
  }
  box_color <- ifelse(
    isTRUE(params$color_scale %in% c("greyscale 1", "greyscale 2")),
    "black", "white"
  )
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0, y = 0,
      label = paste0("Audio not found:\n", label),
      size = 4, color = box_color
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(
        fill = if (identical(box_color, "white")) "grey20" else "grey90",
        color = NA
      )
    )
}


#' Full image key for a rendered detection cell (LVA-107 extended)
#'
#' @description Identifies the FINAL cell PNG: the detection identity plus every
#'   spectrogram parameter that affects the drawn image (unlike `.lva_wave_key()`,
#'   which keys only the audio read). The daemon-render buffer and the on-demand
#'   render agree on this key, so a pre-rendered PNG is reused when the user lands
#'   on the page. Fields mirror the old `renderCachedPlot(cacheKeyExpr=)` list.
#'
#' @param detection_id The cell's detection id.
#' @param params The parameter list from `.lva_gather_params()`.
#' @param label Optional per-cell label (ROI mode: `roi_label`); enters the key
#'   so a re-label invalidates the cached PNG (plan §8.7).
#' @return A single character scalar.
#' @keywords internal
#' @noRd
#' Full image key for a rendered detection cell (LVA-107 extended)
#'
#' @param detection_id The cell's detection id.
#' @param params The parameter list from `.lva_gather_params()` (carries
#'   `grid_dim` since LVA-109: the rendered PNG size is a pure function of the
#'   grid dimension, so a dim change must not reuse the cached file).
#' @return A single character scalar.
#' @keywords internal
#' @noRd
.lva_img_key <- function(detection_id, params, label = NULL) {
  paste(
    detection_id, params$wl, params$ovlp,
    params$zoom_freq[1], params$zoom_freq[2],
    params$dyn_range_detec[1], params$dyn_range_detec[2],
    params$color_scale, params$pitch_shift,
    params$time_guide_interval, params$freq_guide_interval,
    params$time_pads, params$grid_dim,
    if (is.null(label) || length(label) == 0L) "" else as.character(label[1]),
    sep = "|"
  )
}

#' Render one detection cell straight to a PNG file (LVA-107 extended)
#'
#' @description Builds the cell spectrogram with [.lva_build_cell_plot()] and
#'   rasterises it to `out_path`. Pure and dependency-explicit, so it runs inside
#'   a background `mirai` daemon (primed with `fast_spectro.R` +
#'   `_lva_validation_helpers.R`): the daemon does the WHOLE per-cell cost
#'   (readWave + `fast_spectro` + raster→PNG) off the main thread, and the app
#'   serves the finished PNG via `renderImage`, bypassing the main-thread
#'   `renderCachedPlot`. This is the version that actually offloads the dominant
#'   rasterisation cost (the read-only prefetch saved ~1% on warm small files).
#'
#' @param row,params As in [.lva_build_cell_plot()].
#' @param out_path Destination PNG path (allocated by the caller so it controls
#'   the file lifecycle).
#' @param rec Optional pre-read `Wave` (usually `NULL` here — the daemon reads).
#' @param grid_dim Grid dimension (LVA-109). Drives the default raster size:
#'   cells grow on screen as the grid shrinks, so 1×1/2×2 need a bigger PNG
#'   than 3×3/4×4 or they upscale blurry. Base cell = 4.2×3 in; scale factor
#'   `max(1, 3 / grid_dim)`.
#' @param width,height Image size in inches; `dpi` its resolution. Defaults
#'   `NULL` = derived from `grid_dim`. The cell box is CSS-scaled to fit, so
#'   these set the rendered detail, not the on-screen size.
#' @return `out_path` (the written file).
#' @keywords internal
#' @noRd
.lva_render_cell_png <- function(row, params, out_path, rec = NULL,
                                 grid_dim = 3L,
                                 width = NULL, height = NULL, dpi = 96) {
  if (is.null(width) || is.null(height)) {
    cell_scale <- max(1, 3 / grid_dim)
    width <- 4.2 * cell_scale
    height <- 3 * cell_scale
  }
  g <- .lva_build_cell_plot(row, params, rec = rec)
  # LVA-153: `device = "png"` already rasterises through ragg on this stack.
  # ggplot2 (>= 4.0) routes ggsave's "png" device to `ragg::agg_png` whenever
  # ragg is installed (see `ggplot2:::validate_device`), so the cell render is
  # already AGG-backed and at device-parity with the seg-app's `renderPlot`
  # (which uses ragg via `shiny.useragg`). Switching to an explicit
  # `device = ragg::agg_png` was verified byte-identical and same-speed -> kept
  # as the default string to avoid a redundant hard ragg dependency here.
  suppressWarnings(
    ggplot2::ggsave(
      filename = out_path, plot = g,
      width = width, height = height, units = "in", dpi = dpi, device = "png"
    )
  )
  out_path
}

#' Validate the input path
#'
#' Validates that the input path is not NULL, the file exists, and carries a
#' supported extension. LVA-142: the routine input is a detections `.duckdb`
#' store (aligning with [detecs_to_rois()] / [validate_by_overlap()]); a legacy
#' `.csv` file is still accepted but is deprecated (the caller warns on use).
#' A `.csv` input must additionally be non-empty. Guarded against NULL
#' dereference.
#'
#' @param input_path The path to the input file (`.duckdb` or, deprecated,
#'   `.csv`).
#' @return The validated path, unchanged.
#' @keywords internal
validate_input_path <- function(input_path) {
  if (is.null(input_path)) {
    stop(
      "Error! The input file path was not provided (NULL). ",
      "Provide a valid path to a detections `.duckdb` store (or a legacy CSV)."
    )
  }
  if (!file.exists(input_path)) {
    stop(
      "Error! The input file '", input_path, "' does not exist."
    )
  }
  ext <- tolower(tools::file_ext(input_path))
  if (!ext %in% c("duckdb", "csv")) {
    stop(
      "Error! The input file '", input_path, "' must be a detections `.duckdb` ",
      "store or a legacy `.csv` file."
    )
  }
  if (ext == "csv" && file.size(input_path) == 0) {
    stop(
      "Error! The input file '", input_path, "' is empty."
    )
  }
  input_path
}

# LVA-142 / LVA-155: DuckDB-first validation I/O on the unified signals
# store (signals plan F0-F6, decision D8). The routine input is a detections
# `.duckdb` store and the routine output a validations `.duckdb` store; both
# hold the `signals` table -- input reads `signal_class = "detection"` rows,
# output upserts `val_*` on them by `signal_id`. Legacy layouts (a `detections`
# or `validations` table) are tolerated on read, mirroring fetch_rois(). A
# legacy `.csv` path is still accepted on both sides but emits a deprecation
# warning. Format is autodetected by file extension. These run in the main
# session only (never in the LVA-107 render daemon), so they may call the
# package's DuckDB helpers directly; the daemon sources this file too, but
# never calls these (symbol references resolve only at call time).

# LVA-155: probe whether a `.duckdb` file carries the unified signals table.
# READ-ONLY connect -- connecting through the signals layer would CREATE an
# empty `signals` table on a legacy store and mask its layout (same guard as
# `.fetch_rois_duckdb()`).
.lva_has_signals_table <- function(path) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  "signals" %in% DBI::dbListTables(con)
}

# Read the detections to validate. `.duckdb` with a `signals` table -> the
# detection rows of the signals store (the routine layout fetch_score_peaks()
# writes); `.duckdb` without one -> the legacy `detections` table (tolerated);
# `.csv` (deprecated) -> data.table::fread. Returns a plain data.frame in the
# legacy detections shape.
#
# LVA item 8 / plan 2026-08-18_02 §5 passo 3 (single-store): the input now also
# carries the `val_*` columns (projected onto `validation_*`), so a reopened
# session resumes its verdicts directly — no separate output-store merge.
.lva_read_detections_input <- function(path) {
  if (grepl("(?i)\\.duckdb$", path)) {
    if (.lva_has_signals_table(path)) {
      con <- .signals_duckdb_connect(path)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
      sg <- .signals_duckdb_read(con, signal_class = .SIGNAL_CLASS_DETECTION)
      return(.lva_signals_to_detections_with_vals(sg))
    }
    con <- .detections_duckdb_connect(path)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    return(.detections_duckdb_read(con))
  }
  warning(
    "Reading the validation input from a CSV is deprecated; persist detections ",
    "in a `.duckdb` store and pass that path instead.",
    call. = FALSE
  )
  data.table::fread(path, data.table = FALSE, header = TRUE)
}

# Project detection-class signals onto the legacy detections shape AND overlay
# the `val_*` columns (single-store resume). Shared by
# `.lva_read_detections_input()` and `.lva_signals_as_validations_all()`.
.lva_signals_to_detections_with_vals <- function(sg) {
  out <- .signals_as_detections(sg)
  out$validation_user <- sg$val_user
  out$validation_time <- sg$val_time
  out$validation <- sg$val_verdict
  out$validation_note <- sg$val_note
  out$validation_order <- sg$val_order
  out$validation_subset <- sg$val_subset
  # SIG-08 / DTR-101 (plan §8.8(c)): expose the species correction
  # out-of-band (the frozen legacy 27-col shape has no such column).
  out$label_updated <- sg$roi_label_updated
  as.data.frame(out)
}

# LVA item 8 / plan 2026-08-18_02 §5 passo 6: read the ROI rows under review.
# `signal_class IN ('roi', 'detection_to_roi')` (decisão 2); a raw `detection`
# row is excluded by construction (its class is neither). Projects each row onto
# the LEGACY DETECTIONS shape with an OPAQUE key (A2): `detection_id :=
# signal_id`, detection bounds mapped from the roi_* core columns, and the
# `val_*` overlay — so the whole cell/pagination/dirty/autosave engine works
# unchanged. Also carries `roi_label` and `roi_label_updated` for re-labelling.
.lva_read_rois_input <- function(path) {
  if (!grepl("(?i)\\.duckdb$", path)) {
    warning(
      "ROI review requires a `.duckdb` signals store; reading a CSV is not ",
      "supported in `mode = 'rois'`.", call. = FALSE
    )
    return(data.frame())
  }
  if (!.lva_has_signals_table(path)) {
    warning(
      "The store has no unified `signals` table; `mode = 'rois'` needs it. ",
      "Legacy `rois`-only stores are not supported here.", call. = FALSE
    )
    return(data.frame())
  }
  con <- .signals_duckdb_connect(path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  sg <- .signals_duckdb_read(
    con, signal_class = c(.SIGNAL_CLASS_ROI, .SIGNAL_CLASS_DETECTION_TO_ROI)
  )
  .lva_signals_to_rois_with_vals(sg)
}

# Project roi/detection_to_roi signals onto the legacy detections shape with an
# opaque `detection_id := signal_id` key, mapped bounds, val_* overlay, and the
# re-labelling columns carried through.
.lva_signals_to_rois_with_vals <- function(sg) {
  if (is.null(sg) || nrow(sg) == 0L) {
    empty <- .schema_detections(0L)
    empty$roi_label <- character(0)
    empty$roi_label_updated <- character(0)
    empty$created_at <- character(0)
    return(empty)
  }
  out <- .schema_detections(nrow(sg))
  out$detection_id <- sg$signal_id
  out$soundscape_path <- sg$soundscape_path
  out$soundscape_file <- sg$soundscape_file
  # LVA item 8.2 (plan 2026-08-18_03 §8.2): the "Signal" dropdown filters ROIs
  # by label, so the frame carries the label in the template_name slot (the
  # field's value is reused as the dropdown choice). SIG-08 (item 8.8): the
  # EFFECTIVE label (roi_label_updated when present).
  out$template_name <- .signal_effective_label(sg$roi_label,
                                               sg$roi_label_updated)
  out$template_min_freq <- sg$roi_min_freq
  out$template_max_freq <- sg$roi_max_freq
  out$detection_start <- sg$roi_start
  out$detection_end <- sg$roi_end
  out$detection_wl <- sg$roi_wl
  out$detection_ovlp <- sg$roi_ovlp
  out$detection_sample_rate <- sg$roi_sample_rate
  out$peak_score <- NA_real_
  out$validation_user <- sg$val_user
  out$validation_time <- sg$val_time
  out$validation <- sg$val_verdict
  out$validation_note <- sg$val_note
  out$validation_order <- sg$val_order
  out$validation_subset <- sg$val_subset
  # re-labelling columns (single source for the whole app)
  out$roi_label <- sg$roi_label
  out$roi_label_updated <- sg$roi_label_updated
  # LVA item 8.3: created_at backs the "Segmentation order" ordering (the
  # in-session frame is read-only here; the store keeps the canonical value).
  out$created_at <- sg$created_at
  as.data.frame(out)
}

# LVA item 8 / plan 2026-08-18_02 §5 passo 10 (A3): convert an in-session
# review frame back to signals for the idempotent upsert. Detection rows keep
# `signal_id == detection_id`; ROI-mode rows preserve their opaque `detection_id`
# as `signal_id` and carry `roi_label` / `roi_label_updated` through.
.lva_frame_to_signals <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(.schema_signals(0L))
  if ("roi_label" %in% names(df)) {
    # ROI-review frame: project onto signals via the roi shape, preserving the
    # opaque key as signal_id and the re-labelling columns.
    out <- .schema_signals(nrow(df))
    out$signal_id <- df$detection_id
    out$signal_class <- .SIGNAL_CLASS_ROI
    out$soundscape_path <- df$soundscape_path
    out$soundscape_file <- df$soundscape_file
    out$roi_start <- df$detection_start
    out$roi_end <- df$detection_end
    out$roi_min_freq <- df$template_min_freq
    out$roi_max_freq <- df$template_max_freq
    out$roi_label <- df$roi_label
    out$roi_label_updated <- df$roi_label_updated
    out$roi_wl <- df$detection_wl
    out$roi_ovlp <- df$detection_ovlp
    out$roi_sample_rate <- df$detection_sample_rate
    out$val_user <- df$validation_user
    out$val_time <- df$validation_time
    out$val_verdict <- df$validation
    out$val_note <- df$validation_note
    out$val_order <- df$validation_order
    out$val_subset <- df$validation_subset
    .coerce_signals(out)
  } else {
    out <- .validations_as_signals(df)
    # SIG-08 / DTR-101: a species correction entered in detections mode
    # lands in roi_label_updated (out-of-band `label_updated` column).
    if ("label_updated" %in% names(df)) {
      out$roi_label_updated <- df$label_updated
    }
    out
  }
}

# Persist the validated frame (the whole in-session table). `.duckdb` ->
# `.lva_frame_to_signals()` + `.signals_duckdb_upsert(replace = FALSE)` —
# idempotent upsert by signal_id (A5/D2: no flow deletes rows). Unvalidated
# rows carry `val_* = NA` and are kept. `.csv` (deprecated) -> data.table::fwrite.
.lva_write_validations_output <- function(df, path) {
  if (grepl("(?i)\\.duckdb$", path)) {
    con <- .signals_duckdb_connect(path)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    .signals_duckdb_upsert(con, .lva_frame_to_signals(df), replace = FALSE)
    return(invisible(df))
  }
  warning(
    "Writing validations to a CSV is deprecated; pass a `.duckdb` output store ",
    "instead.",
    call. = FALSE
  )
  data.table::fwrite(x = df, file = path, na = NA, row.names = FALSE)
  invisible(df)
}

# --- Recording-path resolution (STEP-1, plan 2026-09-14_01) --------------------
#
# The review frame already carries `soundscape_path` (and `det_template_path`)
# from the store. The old setup threw that away and re-resolved every row by
# BASENAME against a recursive scan of `soundscapes_path`: a 244k-file walk to
# resolve 4.5k references that already resolved, and a hard crash when the tree
# held any duplicate basename (`rows_update` needs unique keys).
#
# The cascade below is STORED-FIRST:
#   1. the path recorded in the store, when it is a readable `.wav`;
#   2. otherwise a single unambiguous basename match under `soundscapes_path`
#      (only when that root was supplied), never an ambiguous one (DEC-4);
#   3. otherwise NA -> the cell shows the STEP-2 placeholder.

#' Replace a `*_path` column with the stored-first resolution (STEP-1)
#'
#' @description Resolves one path column of the review frame. A value that is
#'   a readable `.wav` on disk is kept as recorded (paths are tested once for
#'   the whole column -- a soundscape tree can be huge and slow to stat). The
#'   remaining rows are looked up by basename in `scan` (a pre-built index); a
#'   basename with more than one hit stays `NA` (DEC-4).
#'
#' @param res The review data.frame.
#' @param path_col Name of the path column to fill (e.g. `"soundscape_path"`).
#' @param file_col Name of the matching basename column (fallback lookup key).
#' @param base_dir Root that anchors a RELATIVE stored path (DEC-9:
#'   `project_path`), or `NULL` when the caller has none.
#' @param scan Named list: basename -> character vector of paths (built once
#'   per session by `.lva_build_basename_index()`), or `NULL` for no fallback.
#' @return A list with `res` (the updated frame) and `stats` (counts of
#'   `stored`, `fallback` and `missing` rows).
#' @keywords internal
#' @noRd
.lva_resolve_one_path_column <- function(res, path_col, file_col, base_dir,
                                        scan = NULL) {
  n <- nrow(res)
  resolved <- rep(NA_character_, n)
  stored <- rep(FALSE, n)
  raw <- as.character(res[[path_col]])
  if (n > 0L) {
    rel <- !is.na(raw) & !grepl("^(/|\\\\|[A-Za-z]:[/\\\\])", raw)
    # DEC-9: a relative stored path is anchored under project_path, the same
    # rule every other path argument of the app follows (see
    # `.resolve_under_project()`).
    anchored <- if (!is.null(base_dir)) file.path(base_dir, raw) else raw
    anchored[is.na(raw)] <- NA_character_
    cand <- ifelse(rel, anchored, raw)
    # A soundscape directory holds tens of thousands of files on a slow
    # external disk, so test existence ONCE for the whole column: the verdict
    # is only needed for `.wav` candidates, and the vectorised call avoids
    # one stat() round trip per row.
    ok <- !is.na(cand) & grepl("(?i)\\.wav$", cand)
    ok[ok] <- file.exists(cand[ok])
    resolved[ok] <- cand[ok]
    stored[ok] <- TRUE
  }
  fallback <- rep(FALSE, n)
  missing <- is.na(resolved)
  if (!is.null(scan) && any(missing)) {
    files <- as.character(res[[file_col]])
    # Only UNIQUE unresolved basenames are looked up: 7.5k detections over 4.5k
    # recordings would otherwise repeat the same list probe. A basename with
    # more than one hit is left unresolved (DEC-4) -- reviewing the wrong
    # recording is worse than showing no audio.
    for (f in unique(files[missing])) {
      hits <- scan[[f]]
      if (length(hits) == 1L) {
        idx <- which(missing & !is.na(files) & files == f)
        resolved[idx] <- hits
        fallback[idx] <- TRUE
        missing[idx] <- FALSE
      }
    }
  }
  res[[path_col]] <- resolved
  list(
    res = res,
    stats = c(stored = sum(stored), fallback = sum(fallback),
              missing = sum(is.na(resolved)))
  )
}

#' Build the basename -> paths index for the fallback scan (STEP-1)
#'
#' @description One recursive scan of `base_dir`, indexed by basename. Built
#'   only when the user supplied a relocation root AND at least one row failed
#'   the stored-first step.
#'
#' @param base_dir Existing directory to scan (or `NULL`).
#' @param pattern Case-insensitive extension filter (e.g. `"wav"`).
#' @return A named list, or `NULL` when `base_dir` is unusable.
#' @keywords internal
#' @noRd
.lva_build_basename_index <- function(base_dir, pattern = "wav") {
  if (is.null(base_dir) || !dir.exists(base_dir)) return(NULL)
  files <- as.character(fs::dir_ls(
    base_dir, type = "file", glob = paste0("*.", pattern), recurse = TRUE,
    ignore.case = TRUE
  ))
  if (length(files) == 0L) return(NULL)
  split(files, basename(files))
}

#' Shorten a character vector for one notification (STEP-1)
#'
#' @param x Character vector of file names.
#' @param n Maximum entries shown.
#' @return A single string.
#' @keywords internal
#' @noRd
.lva_shorten_list <- function(x, n = 10L) {
  x <- unique(x[!is.na(x)])
  if (length(x) == 0L) return("")
  shown <- utils::head(x, n)
  out <- paste(shown, collapse = ", ")
  if (length(x) > length(shown)) {
    out <- paste0(out, " (+", length(x) - length(shown), " more)")
  }
  out
}

# LVA-155: project the detection rows of a signals connection onto the legacy
# `validations` shape WITHOUT the verdict filter -- end_session reconciliation
# and session resume (LVA-147) count unvalidated rows too, so
# `.signals_as_validations()` (which drops NA-verdict rows) cannot be used
# here. `signal_id == detection_id` on detection rows (frozen tuple), so the
# end_session anti_join stays type-safe (detection_id returns character,
# cf. LVA-141).
.lva_signals_as_validations_all <- function(con) {
  sg <- .signals_duckdb_read(con, signal_class = .SIGNAL_CLASS_DETECTION)
  .lva_signals_to_detections_with_vals(sg)
}

# Read back the persisted validations for end_session reconciliation.
# `.duckdb` with a `signals` table -> all detection rows with their `val_*`
# (no NA filter, see above); `.duckdb` without one -> the legacy `validations`
# table (tolerated); `.csv` (deprecated) -> data.table::fread.
.lva_read_validations_output <- function(path) {
  if (grepl("(?i)\\.duckdb$", path)) {
    if (.lva_has_signals_table(path)) {
      con <- .signals_duckdb_connect(path)
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
      return(.lva_signals_as_validations_all(con))
    }
    con <- .validations_duckdb_connect(path)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    return(.validations_duckdb_read(con))
  }
  as.data.frame(data.table::fread(file = path))
}

# LVA-110: TP/FP score-stabilization series for one template. Returns a long
# data.frame (validation, n_validated, cum_mean_score) — per class, the
# cumulative mean peak_score over that class's detections taken in
# validation_order — or NULL when nothing is validated yet. Only rows with a
# validation_order (validated in-app / a prior session), a TP/FP verdict and a
# non-NA peak_score for `template_name` contribute.
.lva_stabilization_series <- function(df, template_name) {
  if (is.null(df) || !nrow(df)) return(NULL)
  need <- c("validation", "validation_order", "peak_score", "template_name")
  if (!all(need %in% names(df))) return(NULL)
  d <- df[
    !is.na(df$validation_order) &
      df$template_name == template_name &
      df$validation %in% c("TP", "FP") &
      !is.na(df$peak_score), ,
    drop = FALSE
  ]
  if (!nrow(d)) return(NULL)
  d <- d[order(d$validation_order), , drop = FALSE]
  parts <- lapply(c("TP", "FP"), function(cl) {
    dc <- d[d$validation == cl, , drop = FALSE]
    if (!nrow(dc)) return(NULL)
    data.frame(
      validation = cl,
      n_validated = seq_len(nrow(dc)),
      cum_mean_score = cumsum(dc$peak_score) / seq_len(nrow(dc)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, parts)
  if (is.null(out) || !nrow(out)) return(NULL)
  out
}
# LVA item 8.2 (plan 2026-08-18_03 §8.2): the sentinel VALUE of the
# "All labels" choice. A real, non-empty choice value (segmentation-app
# "(all)" pattern) — an empty string sentinel vanishes from the dropdown
# once a specific label is selected (gate review 2026-08-19).
.lva_all_labels <- function() "(all labels)"

# LVA item 8.3 (plan 2026-08-18_03 §8.3): mode-specific ordering choices for
# the "Order by" dropdown. ROI mode orders by label / segmentation order
# (created_at with detection_id as tiebreak) and hides the score options (ROIs
# carry no peak_score); detections mode keeps the classic set. Every key the
# function returns must exist in the app's `order_options` list (the confirm
# indexes it directly).
.lva_order_choices <- function(mode) {
  if (identical(mode, "rois")) {
    c(
      "Original file order", "Random",
      "Soundscape file name (ASC)", "Soundscape file name (DESC)",
      "Label (ASC)", "Label (DESC)",
      "Segmentation order (ASC)", "Segmentation order (DESC)",
      "Soundscape file name (ASC) and Label (ASC)"
    )
  } else {
    c(
      "Original file order",
      "Soundscape file name (ASC)", "Soundscape file name (DESC)",
      "Score (ASC)", "Score (DESC)",
      "Soundscape file name (ASC) and Score (ASC)",
      "Soundscape file name (ASC) and Score (DESC)",
      "Soundscape file name (DESC) and Score (ASC)",
      "Soundscape file name (DESC) and Score (DESC)",
      "Score (ASC) and Soundscape file name (ASC)",
      "Score (ASC) and Soundscape file name (DESC)",
      "Score (DESC) and Soundscape file name (ASC)",
      "Score (DESC) and Soundscape file name (DESC)",
      "Random"
    )
  }
}
