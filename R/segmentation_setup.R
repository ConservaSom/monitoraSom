#' Setup-phase helpers for the segmentation app (LSA-05)
#'
#' @description Helpers used while the app initialises a session: the
#'   soundscape-dropdown choice builder, its status palette, and the ROI
#'   label-list loader. Extracted from the `launch_segmentation_app()` closures
#'   so they are reachable by testthat and reusable by the Julia port.
#'
#'   Applied here:
#'   - **LSA-19** — `.build_soundscape_choices()` uses `soundscape_path` as the
#'     (durable, unique) selectize *value*, keeping `soundscape_file` only as the
#'     visible *label*.
#'   - **LSA-24** — the `soundscape_file` is HTML-escaped before it is embedded
#'     in the status `<span>` label (selectize renders names as HTML), so
#'     filenames containing `<`, `>`, `&` or quotes cannot corrupt the dropdown.
#'   - **LSA-13** — `.read_xlsx_preserve_names()` (openxlsx-based) returns UTF-8
#'     character data on every platform (XLSX strings are UTF-8 by spec), so
#'     accented labels round-trip correctly; no explicit encoding argument is
#'     needed (CSV export, the other I/O site flagged by LSA-13, is dropped under
#'     LSA-117).
#'   - **LSA-209** — `.discover_soundscapes_df()` normalizes the directory
#'     mode's navigation keys with `.normalize_path_key()`, so a trailing slash
#'     or `"./"` prefix in the setup path cannot leak into the keys.
#'
#' @keywords internal
#' @noRd

# Colors applied to soundscape names in the dropdown. Unsegmented files use an
# empty string (inherits the default dropdown text color).
.SOUNDSCAPE_STATUS_COLORS <- c(
  segmented   = "#27ae60",  # green
  no_soi      = "#e74c3c",  # red
  unsegmented = ""
)

#' Build the named vector for the soundscape `updateSelectizeInput`.
#'
#' Values are durable `soundscape_path`s (LSA-19); names are HTML `<span>`
#' labels carrying the status colour over the readable `soundscape_file`.
#' Requires the selectizeInput to have the custom HTML render option enabled.
#'
#' @param soundscape_df data.frame with `soundscape_path`, `soundscape_file`,
#'   `status`.
#' @return named character vector (`names` = HTML labels, values = paths).
#' @noRd
.build_soundscape_choices <- function(soundscape_df) {
  colors <- .SOUNDSCAPE_STATUS_COLORS[soundscape_df$status]
  colors[is.na(colors)] <- ""
  # LSA-24: escape the filename before it goes into the HTML label (selectize
  # renders names as HTML). Escape in both branches: even the uncoloured label
  # is rendered as HTML.
  safe_file <- htmltools::htmlEscape(soundscape_df$soundscape_file)
  labels <- ifelse(
    colors == "",
    safe_file,
    paste0(
      '<span style="color:', colors, '">',
      safe_file,
      "</span>"
    )
  )
  stats::setNames(soundscape_df$soundscape_path, labels)
}

#' Validate and normalise a caller-supplied `df_soundscapes` table. (LSA-203)
#'
#' Turns a metadata table (a [fetch_soundscape_metadata()] result or any
#' data.frame carrying a `soundscape_path` column) into the two-column
#' navigation frame the app reads, **preserving caller row order** (never
#' sorts) so the table dictates the segmentation order. Applies the gate
#' contract:
#'   - **Q2** minimum column contract: any data.frame with a character
#'     `soundscape_path` column (>= 1 row); `soundscape_file` is derived via
#'     `basename()` when the column is absent.
#'   - **Q3** duplicate `soundscape_path` rows are dropped keeping the first
#'     occurrence, with a `warning()` (the app surfaces it as a notification).
#'   - **Q1** rows whose file is missing on disk are a **hard error**, listing
#'     the offending paths (dedup runs first, so duplicates are not re-reported).
#'
#' @param df a data.frame with at least a `soundscape_path` column.
#' @return a data.frame(`soundscape_path`, `soundscape_file`) in caller row
#'   order, deduplicated, every file verified to exist.
#' @noRd
.validate_soundscape_table <- function(df) {
  if (!is.data.frame(df)) {
    stop("`df_soundscapes` must be a data.frame; got ", class(df)[1], ".",
         call. = FALSE)
  }
  if (!"soundscape_path" %in% names(df)) {
    stop("`df_soundscapes` must contain a `soundscape_path` column.",
         call. = FALSE)
  }
  paths <- as.character(df$soundscape_path)
  if (length(paths) == 0L) {
    stop("`df_soundscapes` has no rows; nothing to segment.", call. = FALSE)
  }
  # Q2: derive soundscape_file from basename when the column is absent.
  files <- if ("soundscape_file" %in% names(df)) {
    as.character(df$soundscape_file)
  } else {
    basename(paths)
  }
  out <- data.frame(
    soundscape_path = paths,
    soundscape_file = files,
    stringsAsFactors = FALSE
  )
  # Q3: dedup-first on soundscape_path (keep first), then warn.
  dup <- duplicated(out$soundscape_path)
  if (any(dup)) {
    warning(sum(dup), " duplicate soundscape_path row(s) removed ",
            "(kept first occurrence).", call. = FALSE)
    out <- out[!dup, , drop = FALSE]
    rownames(out) <- NULL
  }
  # Q1: hard error on files missing from disk, listing the offenders.
  missing <- out$soundscape_path[!file.exists(out$soundscape_path)]
  if (length(missing) > 0L) {
    stop("These soundscape files do not exist on disk:\n",
         paste0("  - ", missing, collapse = "\n"), call. = FALSE)
  }
  out
}

#' Read a saved metadata store into a `df_soundscapes` data.frame. (LSA-203)
#'
#' Backend is deduced from the file extension by [.resolve_backend()]
#' (`.duckdb`/`.db` -> DuckDB, else CSV), reusing the cache reader
#' [.read_cache()]. **Caveat (Q6):** a custom navigation order is guaranteed
#' only via CSV (or the launch argument); a DuckDB store returns rows in
#' insertion order (the incremental-cache append order), which is not a
#' user-controlled ordering. The returned frame is handed to
#' [.validate_soundscape_table()] by the caller.
#'
#' @param path path to a saved `.csv` or `.duckdb`/`.db` metadata store.
#' @return a data.frame (at least `soundscape_path`); errors on an unreadable
#'   file or a store with no `soundscapes` rows.
#' @noRd
.read_soundscape_table <- function(path) {
  if (!file.exists(path)) {
    stop("Metadata table file not found: ", path, call. = FALSE)
  }
  backend <- .resolve_backend(path, "auto")
  if (backend == "duckdb") {
    con <- .duckdb_connect(path)
    on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE),
            add = TRUE)
    df <- .read_cache(path, "duckdb", con = con)
    if (is.null(df) || nrow(df) == 0L) {
      stop("DuckDB store has no `soundscapes` rows: ", path, call. = FALSE)
    }
  } else {
    df <- .read_cache(path, "csv")
    if (is.null(df)) {
      stop("Could not read CSV metadata table: ", path, call. = FALSE)
    }
  }
  df
}

#' Discover a soundscape directory into the directory-mode navigation frame. (LSA-209)
#'
#' Thin wrapper over [.discover_soundscapes()] (the pipeline's discovery:
#' anchored WAV filter, `.txt` guard, deterministic sort, FSM-07/201/15) that
#' normalizes the keys with [.normalize_path_key()] (AFL-06). Every other
#' producer/consumer of `soundscape_path` keys (store rows via
#' `.rois_as_signals()`, `fetch_rois()`, statuses) uses normalized keys, so a
#' cosmetic difference in the setup argument (trailing slash, `"./"` prefix)
#' must not reach the dropdown values — a divergent key silently zeroes the
#' status classification, the ROI loads and the class-scoped saves (LSA-209).
#' Keys stay physically resolvable (forward slashes read fine on every
#' platform), so WAV loading is unaffected.
#'
#' @param soundscapes_path directory to scan (recursive; WAV files).
#' @return a data.frame(`soundscape_path`, `soundscape_file`) with standard,
#'   deterministically sorted keys; errors (via [.discover_soundscapes()]) when
#'   the path holds no WAV file.
#' @noRd
.discover_soundscapes_df <- function(soundscapes_path) {
  paths <- .normalize_path_key(
    .discover_soundscapes(soundscapes_path, recursive = TRUE)
  )
  data.frame(
    soundscape_path = paths,
    soundscape_file = basename(paths),
    stringsAsFactors = FALSE
  )
}

#' Channel-selection choices for a loaded recording. (item 6)
#'
#' Builds the named vector for the "Channel" dropdown from a recording's channel
#' count: **L/R** for mono/stereo (values `"left"`/`"right"`, kept for
#' downstream compatibility), **1..N** for a multichannel recording (values
#' `"1"`..`"N"`). A `WaveMC` object (tuneR's >2-channel class) reports
#' `ncol(@.Data)`; a plain `Wave` is mono (1) or stereo (2). The multichannel
#' branch is inert until multichannel reading lands (see the 4-channel
#' follow-up) but keeps the widget forward-compatible.
#'
#' @param rec a tuneR `Wave` or `WaveMC` object.
#' @return a named character vector (names = labels, values = channel tokens).
#' @noRd
.channel_choices <- function(rec) {
  n <- if (methods::is(rec, "WaveMC")) {
    ncol(rec@.Data)
  } else if (isTRUE(rec@stereo)) {
    2L
  } else {
    1L
  }
  if (n <= 2L) {
    stats::setNames(c("left", "right")[seq_len(n)], c("L", "R")[seq_len(n)])
  } else {
    stats::setNames(as.character(seq_len(n)), as.character(seq_len(n)))
  }
}

#' Load the ROI label lists (explicit file > project preset > bundled dataset).
#'
#' Extracted unchanged from the alt app. UTF-8 hardening of the XLSX read is
#' deferred to LSA-13 (Phase 4).
#'
#' @param labels_file optional explicit `.xlsx` path (highest priority). All
#'   columns are read as **character** (AUD-14; the reader preserves original
#'   names by reading header-as-data), so numeric-looking label columns come
#'   back as character.
#' @param project_path optional project directory (preset fallback).
#' @return a data.frame of label lists (one column per list; character columns).
#' @noRd

# AUD-09: openxlsx::read.xlsx sanitizes column names (make.names) even with
# check.names=FALSE on this version. Read raw (colNames=FALSE), extract the
# first row as column names, then drop it — preserving the exact original names
# as readxl::read_xlsx would.
# AUD-14: reading with colNames=FALSE puts the header strings in row 1, so EVERY
# column is returned as character (readxl returned typed columns). This is the
# intended contract here — label lists are consumed as character — but a caller
# supplying a labels_file with numeric-looking columns should expect character
# output, not the source types.
.read_xlsx_preserve_names <- function(file) {
  raw <- openxlsx::read.xlsx(file, colNames = FALSE)
  col_names <- as.character(raw[1, ])
  data <- raw[-1, , drop = FALSE]
  names(data) <- col_names
  rownames(data) <- NULL
  data
}

# Read one label-list .txt (DEC-5): one label per line; the list name is the
# file name without the `label_list_` prefix.
.read_label_list_file <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0L) {
    return(data.frame(x = character(0), stringsAsFactors = FALSE,
                      check.names = FALSE))
  }
  name <- sub("^label_list_", "", basename(path))
  name <- sub("\\.txt$", "", name)
  df <- data.frame(lines, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- name
  df
}

# Read a directory of label-list .txt files into one data.frame (DEC-5).
.read_label_list_dir <- function(dir_path) {
  files <- sort(list.files(dir_path, pattern = "^(label_list_).*\\.txt$",
                           full.names = TRUE))
  if (length(files) == 0L) {
    stop("No `label_list_*.txt` files found in: ", dir_path)
  }
  parts <- lapply(files, .read_label_list_file)
  # align to the longest list (NA-pad shorter ones)
  n <- max(vapply(parts, nrow, integer(1)))
  for (i in seq_along(parts)) {
    if (nrow(parts[[i]]) < n) {
      extra <- data.frame(rep(NA_character_, n - nrow(parts[[i]])))
      names(extra) <- names(parts[[i]])
      parts[[i]] <- rbind(parts[[i]], extra)
    }
  }
  do.call(cbind, parts)
}

.load_roi_label_lists <- function(labels_file, project_path) {
  # DEC-5 (STEP-6): one .txt file per label list (prefix `label_list_`) or a
  # directory of such files; the old single .xlsx stays readable for
  # back-compatibility. Precedence: explicit file/dir > project preset > bundled.
  if (!is.null(labels_file) && file.exists(labels_file)) {
    if (dir.exists(labels_file)) {
      return(.read_label_list_dir(labels_file))
    }
    if (grepl("(?i)\\.xlsx$", labels_file)) {
      return(.read_xlsx_preserve_names(labels_file))
    }
    return(.read_label_list_file(labels_file))
  }
  # Fall back to the preset files already in the project directory (txt first,
  # the legacy single xlsx as back-compat).
  if (!is.null(project_path)) {
    preset_txt <- file.path(project_path, "app_presets", "label_list")
    if (dir.exists(preset_txt)) {
      return(.read_label_list_dir(preset_txt))
    }
    preset_file <- file.path(
      project_path, "app_presets", "roi_label_lists.xlsx"
    )
    if (file.exists(preset_file)) {
      return(.read_xlsx_preserve_names(preset_file))
    }
  }
  # Last resort: the package-bundled dataset. In a deployed install this resolves
  # via the installed package; when the code is only sourced (tests / CI, where
  # monitoraSom is not installed) fall back to a `roi_label_lists` object already
  # on the search path. (ENG-02: makes the loader install- and location-agnostic.)
  if (requireNamespace("monitoraSom", quietly = TRUE)) {
    utils::data("roi_label_lists", package = "monitoraSom", envir = environment())
    return(roi_label_lists)
  }
  if (exists("roi_label_lists", inherits = TRUE)) {
    return(get("roi_label_lists", inherits = TRUE))
  }
  stop("`roi_label_lists` is unavailable: install monitoraSom or preload the dataset")
}

#' Load the custom ROI type vocabulary (project preset > built-in list). (LSA-101)
#'
#' Decision "project preset only" (user, 2026-05-30): unlike the species label
#' lists, there is no selectable multi-list mechanism — a single editable list.
#' If `app_presets/roi_types.xlsx` exists under the project directory, its first
#' column supplies the ROI type names (UTF-8 by the XLSX spec; readxl returns
#' UTF-8 on every platform, so no encoding handling is needed). Otherwise the
#' built-in `.ROI_TYPE_CHOICES` vocabulary is used. Blank/NA cells are dropped and
#' the result is de-duplicated. Free-text `roi_type` values stay valid regardless
#' of this list.
#'
#' @param project_path optional project directory (preset lookup).
#' @return a non-empty character vector of ROI type names.
#' @noRd
.load_roi_type_list <- function(project_path) {
  if (!is.null(project_path)) {
    preset_file <- file.path(project_path, "app_presets", "roi_types.xlsx")
    if (file.exists(preset_file)) {
      types <- as.character(openxlsx::read.xlsx(preset_file)[[1]])
      types <- types[!is.na(types) & nzchar(trimws(types))]
      if (length(types) > 0) return(unique(types))
    }
  }
  .ROI_TYPE_CHOICES
}

#' Seed the project's `app_presets/` with editable default files. (LSA-101 / A6)
#'
#' On launch, write `roi_types.xlsx` and `roi_label_lists.xlsx` into the
#' project's `app_presets/` directory **when they are missing**, so the user
#' edits ready-made files instead of authoring them from scratch (a frequent
#' source of shape/column mistakes). Existing presets are never overwritten.
#' The written shapes match what the loaders read back
#' ([[.load_roi_type_list]] reads the first column; [[.load_roi_label_lists]]
#' reads the whole table). No-op when `preset_dir` is NULL (no project path).
#'
#' @param preset_dir the `app_presets` directory path, or NULL.
#' @return invisibly, a named logical of which files were written this call.
#' @noRd
.seed_app_presets <- function(preset_dir) {
  written <- c(roi_types = FALSE, roi_label_lists = FALSE)
  if (is.null(preset_dir)) return(invisible(written))
  if (!dir.exists(preset_dir)) {
    dir.create(preset_dir, recursive = TRUE, showWarnings = FALSE)
  }
  types_file <- file.path(preset_dir, "roi_types.xlsx")
  if (!file.exists(types_file)) {
    openxlsx::write.xlsx(
      data.frame(roi_type = .ROI_TYPE_CHOICES, stringsAsFactors = FALSE),
      types_file
    )
    written["roi_types"] <- TRUE
  }
  labels_file <- file.path(preset_dir, "roi_label_lists.xlsx")
  if (!file.exists(labels_file)) {
    # Resolve the canonical label lists through the loader itself (project_path
    # NULL skips any existing preset), so the seeded file is exactly what the
    # app would otherwise load as the bundled default.
    labels <- suppressWarnings(.load_roi_label_lists(NULL, NULL))
    openxlsx::write.xlsx(labels, labels_file)
    written["roi_label_lists"] <- TRUE
  }
  invisible(written)
}

#' Build a blank spectrogram canvas carrying a centred message. (A2)
#'
#' Used when the current soundscape cannot be read (e.g. an intentionally
#' malformed WAV in the test corpora): instead of leaving a stale spectrogram
#' from the previously loaded file, the render shows an empty plot with a
#' centred explanation. Kept as a pure, file-scope helper so it is testable and
#' reusable by the Julia port.
#'
#' @param message a single string to display at the centre of the canvas.
#' @return a `ggplot` object with no data, the message annotated at (0.5, 0.5),
#'   and the spectrogram axis labels for visual continuity.
#' @noRd
.blank_plot_message <- function(message) {
  stopifnot(is.character(message), length(message) == 1L)
  ggplot2::ggplot() +
    ggplot2::annotate(
      "text", x = 0.5, y = 0.5, label = message,
      hjust = 0.5, vjust = 0.5, size = 5, color = "grey20"
    ) +
    ggplot2::lims(x = c(0, 1), y = c(0, 1)) +
    ggplot2::labs(x = "Time (s)", y = "Frequency (kHz)") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}
