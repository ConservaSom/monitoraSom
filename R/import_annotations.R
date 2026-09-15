# Family file — exports: read_raven_selection(), read_audacity_labels(),
# write_raven_selection(), write_audacity_labels() (DQ-02 policy,
# specs/README.md "File-family exception").
#' Read and write Raven & Audacity annotations as ROI data
#'
#' @description Convert between two common manual annotation formats, Raven
#'   selection tables or Audacity label tracks, and the monitoraSom standard
#'   ROI format. The four functions
#'   ([read_raven_selection()], [read_audacity_labels()],
#'   [write_raven_selection()], [write_audacity_labels()]) let you bring
#'   annotations into the package as ROI tables and export ROI tables back to
#'   either format.
#'
#' @details
#'   Both formats hold **manual annotations/selections** (ground truth), so they
#'   map to ROI tables (`roi_tables`), never to detections, because a detection
#'   needs a `peak_score` that these formats do not carry. Use these functions
#'   to prepare ground truth for [validate_by_overlap()] from an existing
#'   Raven/Audacity workflow, or to round trip ROIs out for editing in those
#'   tools. Two conversions happen
#'   automatically and are the usual source of confusion: frequency is stored in
#'   **Hz** by Raven/Audacity but in **kHz** by the ROI format (`roi_min_freq`/
#'   `roi_max_freq`), so it is divided by 1000 on read and multiplied on write; and
#'   every imported row records where it came from in `roi_source`
#'   (`"import_raven"` for Raven tables, `"import_audacity"` for Audacity
#'   label tracks; the `signals` store's `origin`
#'   column carries them through unchanged). Both formats are simple
#'   tab-separated text, so reading and writing use base R only and add no
#'   dependency to the package.
#'
#' @section Pipeline context:
#'   A utility used across the flow, outside the numbered steps. Reads and writes
#'   Raven `.txt` selection tables or Audacity label tracks on one side and
#'   ROI data on the other; the ROI side feeds ground-truth-based
#'   steps such as [validate_by_overlap()].
#'
#' @seealso [fetch_rois()], [validate_by_overlap()]
#' @name import_annotations
NULL

# Map a Raven `Channel` (1/2/…) to the ROI-schema channel vocabulary.
.raven_channel_to_roi <- function(ch) {
  ch <- suppressWarnings(as.integer(ch))
  out <- rep(NA_character_, length(ch))
  out[!is.na(ch) & ch == 1L] <- "left"
  out[!is.na(ch) & ch == 2L] <- "right"
  out
}
.roi_channel_to_raven <- function(ch) {
  out <- rep(1L, length(ch))          # default/mono/left -> channel 1
  out[!is.na(ch) & ch == "right"] <- 2L
  out
}

#' Read a Raven selection table into ROI data
#'
#' @description Reads a Raven selection table (tab-separated, with a header) and
#'   returns it in the monitoraSom standard ROI format. Frequencies are converted from
#'   Hz to kHz and every row is stamped as a manual annotation.
#'
#' @details
#'   Raven exports often carry two rows per selection (a Waveform view and a
#'   Spectrogram view); the frequency band lives on the spectrogram row, so when a
#'   `View` column is present only the spectrogram rows are kept. `Begin Time (s)`
#'   and `End Time (s)` are required; `Low Freq (Hz)`/`High Freq (Hz)` and
#'   `Channel` are used when present and become `NA` otherwise. The source
#'   recording is taken from the table's own `Begin File`/`Begin Path` columns
#'   when they exist, else from `soundscape_path`.
#'
#' @param path Path to a Raven selection table (a tab-separated `.txt` file
#'   with a header).
#' @param soundscape_path Optional recording path stamped on every row when the
#'   table has no `Begin Path`/`Begin File` columns. Default `NULL` leaves those
#'   fields `NA`.
#' @param label_col Optional name of the annotation column. Default `NULL`
#'   auto-detects among `Annotation`/`Species` (case-insensitive); set it to
#'   force a specific column (see the example below).
#' @param roi_source Origin stamp written to `roi_source` (default
#'   `"import_raven"`; carried through unchanged in the `origin` column).
#' @return ROI data in the standard format: `roi_start`,
#'   `roi_end`, `roi_min_freq`/`roi_max_freq` in kHz, `roi_label`, `roi_channel`,
#'   `roi_source`, `soundscape_file`, `soundscape_path`.
#' @seealso [write_raven_selection()], [read_audacity_labels()],
#'   [validate_by_overlap()]
#' @examples
#' # Off-flow utility: round-trip ROIs out to Raven format and back in tempdir.
#' \donttest{
#'   # Load the package
#'   library(monitoraSom)
#' rois <- data.frame(
#'   roi_start = 1, roi_end = 2, roi_min_freq = 2, roi_max_freq = 6,
#'   roi_label = "sp1", soundscape_file = "rec.wav")
#' f <- file.path(tempdir(), "sel.txt")
#' write_raven_selection(rois, f)
#'
#' back <- read_raven_selection(f)
#' back[, c("roi_start", "roi_end", "roi_min_freq", "roi_label")]
#'
#' # Force a custom annotation column with label_col (no auto-detection):
#' tbl <- data.frame(
#'   "Begin Time (s)" = 1, "End Time (s)" = 2,
#'   "Low Freq (Hz)" = 2000, "High Freq (Hz)" = 6000,
#'   "Field Note" = "sp1", check.names = FALSE)
#' f2 <- file.path(tempdir(), "sel_custom.txt")
#' utils::write.table(tbl, f2, sep = "\t", quote = FALSE, row.names = FALSE)
#' back2 <- read_raven_selection(f2, label_col = "Field Note")
#' back2[, c("roi_start", "roi_end", "roi_label")]
#' }
#' @export
read_raven_selection <- function(path, soundscape_path = NULL,
                                 label_col = NULL, roi_source = "import_raven") {
  if (length(path) != 1L || !is.character(path) || is.na(path) ||
      !file.exists(path)) {
    stop("read_raven_selection: 'path' must be a single existing file path.")
  }
  tbl <- utils::read.delim(path, sep = "\t", header = TRUE, check.names = FALSE,
                           stringsAsFactors = FALSE, quote = "")
  need <- c("Begin Time (s)", "End Time (s)")
  miss <- setdiff(need, names(tbl))
  if (length(miss) > 0) {
    stop("read_raven_selection: missing required column(s): ",
         paste(miss, collapse = ", "), ".")
  }
  # A Raven export can carry two rows per selection (Waveform + Spectrogram
  # views); the spectrogram row holds the frequency band. Keep the spectrogram
  # view when the distinction is present.
  if ("View" %in% names(tbl)) {
    spec_rows <- grepl("spectrogram", tbl[["View"]], ignore.case = TRUE)
    if (any(spec_rows)) tbl <- tbl[spec_rows, , drop = FALSE]
  }

  has <- function(col) col %in% names(tbl)
  if (is.null(label_col)) {
    label_col <- intersect(c("Annotation", "annotation", "Species", "species"),
                           names(tbl))
    label_col <- if (length(label_col) > 0) label_col[1] else NA_character_
  }

  df <- data.frame(
    roi_start    = suppressWarnings(as.numeric(tbl[["Begin Time (s)"]])),
    roi_end      = suppressWarnings(as.numeric(tbl[["End Time (s)"]])),
    roi_min_freq = if (has("Low Freq (Hz)"))
      suppressWarnings(as.numeric(tbl[["Low Freq (Hz)"]])) / 1000 else NA_real_,
    roi_max_freq = if (has("High Freq (Hz)"))
      suppressWarnings(as.numeric(tbl[["High Freq (Hz)"]])) / 1000 else NA_real_,
    roi_label    = if (!is.na(label_col)) as.character(tbl[[label_col]])
                   else NA_character_,
    roi_channel  = if (has("Channel")) .raven_channel_to_roi(tbl[["Channel"]])
                   else NA_character_,
    roi_source   = roi_source,
    stringsAsFactors = FALSE
  )
  # DEC-17 (STEP-17): keep information the ROI schema has no column for. Raven
  # extras that would otherwise be dropped travel in the free-text comment:
  # the view name (Waveform rows kept when no Spectrogram view exists) and the
  # Tags column. Only stamped when non-NA and non-empty, so the comment stays
  # human text.
  extras <- rep(NA_character_, nrow(df))
  if (has("Tags")) {
    tags <- as.character(tbl[["Tags"]])
    has_tag <- !is.na(tags) & nzchar(trimws(tags))
    extras[has_tag] <- paste0("tags: ", tags[has_tag])
  }
  if (has("View") && any(!is.na(tbl[["View"]]) & nzchar(as.character(tbl[["View"]])))) {
    views <- as.character(tbl[["View"]])
    has_view <- !is.na(views) & nzchar(trimws(views)) &
      !grepl("spectrogram", views, ignore.case = TRUE)
    if (any(has_view)) {
      extras[has_view] <- ifelse(is.na(extras[has_view]),
                                 paste0("view: ", views[has_view]),
                                 paste0(extras[has_view], "; view: ", views[has_view]))
    }
  }
  df$roi_comment <- extras
  # Recording provenance: prefer the table's own columns, else the argument.
  df$soundscape_file <- if (has("Begin File")) as.character(tbl[["Begin File"]])
    else if (!is.null(soundscape_path)) basename(soundscape_path) else NA_character_
  df$soundscape_path <- if (has("Begin Path")) as.character(tbl[["Begin Path"]])
    else if (!is.null(soundscape_path)) soundscape_path else NA_character_

  .coerce_rois(df)
}

#' Read an Audacity label track into ROI data
#'
#' @description Reads an Audacity label track (tab-separated, no header) and
#'   returns it in the monitoraSom standard ROI format, converting any frequency
#'   bounds
#'   from Hz to kHz and stamping each row as a manual annotation.
#'
#' @details
#'   A standard Audacity label is `start<TAB>end<TAB>label`. Frequency-bearing
#'   labels add a continuation line `\<TAB>lowHz<TAB>highHz` immediately after the
#'   label (Audacity's extended format); those Hz bounds map to `roi_min_freq`/
#'   `roi_max_freq` (kHz). Labels without a continuation get `NA` frequencies.
#'   Malformed lines (fewer than three fields, or a stray continuation) are
#'   skipped, and an empty or all-blank file yields empty ROI data rather than
#'   an error.
#'
#' @param path Path to an Audacity label track (tab-separated, no header).
#' @param soundscape_path Optional recording path stamped on every row. Default
#'   `NULL` leaves the `soundscape_*` fields `NA`.
#' @param roi_source Origin stamp written to `roi_source` (default
#'   `"import_audacity"`; carried through unchanged in the `origin` column).
#' @return ROI data in the standard format; frequencies in kHz.
#' @seealso [write_audacity_labels()], [read_raven_selection()],
#'   [validate_by_overlap()]
#' @examples
#' # Off-flow utility: round-trip ROIs out to Audacity format and back in tempdir.
#' \donttest{
#' rois <- data.frame(
#'   roi_start = 1, roi_end = 2, roi_min_freq = 2, roi_max_freq = 6,
#'   roi_label = "sp1", soundscape_file = "rec.wav")
#' f <- file.path(tempdir(), "labels.txt")
#' write_audacity_labels(rois, f)
#'
#' back <- read_audacity_labels(f)
#' back[, c("roi_start", "roi_end", "roi_min_freq", "roi_label")]
#' }
#' @export
read_audacity_labels <- function(path, soundscape_path = NULL,
                                 roi_source = "import_audacity") {
  if (length(path) != 1L || !is.character(path) || is.na(path) ||
      !file.exists(path)) {
    stop("read_audacity_labels: 'path' must be a single existing file path.")
  }
  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))]
  starts <- numeric(0)
  ends <- numeric(0)
  labels <- character(0)
  lo <- numeric(0)
  hi <- numeric(0)
  for (ln in lines) {
    f <- strsplit(ln, "\t", fixed = TRUE)[[1]]
    if (length(f) >= 1L && f[1] == "\\") {
      # Frequency continuation for the previous label.
      if (length(labels) == 0L) next
      lo[length(lo)] <- suppressWarnings(as.numeric(f[2]))
      hi[length(hi)] <- suppressWarnings(as.numeric(f[3]))
      next
    }
    if (length(f) < 3L) next            # not a well-formed label line
    starts <- c(starts, suppressWarnings(as.numeric(f[1])))
    ends   <- c(ends,   suppressWarnings(as.numeric(f[2])))
    labels <- c(labels, f[3])
    lo <- c(lo, NA_real_)
    hi <- c(hi, NA_real_)
  }
  if (length(starts) == 0L) return(.schema_rois(0L))

  df <- data.frame(
    roi_start    = starts,
    roi_end      = ends,
    roi_label    = labels,
    roi_min_freq = lo / 1000,
    roi_max_freq = hi / 1000,
    roi_source   = roi_source,
    soundscape_path = if (!is.null(soundscape_path)) soundscape_path else NA_character_,
    soundscape_file = if (!is.null(soundscape_path)) basename(soundscape_path)
                      else NA_character_,
    stringsAsFactors = FALSE
  )
  .coerce_rois(df)
}

#' Write ROI data as a Raven selection table
#'
#' @description Exports a monitoraSom ROI table to a Raven selection
#'   table, so ROIs can be opened and edited in Raven. Frequencies are converted
#'   from kHz back to Hz. The inverse of [read_raven_selection()].
#'
#' @details
#'   Rows are written with `View = "Spectrogram 1"` and a `Channel` column mapped
#'   from `roi_channel` (`"right"` -> 2, everything else -> 1). The output is
#'   tab-separated with a header, the format [read_raven_selection()] reads back,
#'   so a write-then-read round trip returns the same ROIs (frequencies restored
#'   to kHz).
#'
#' @param rois A data.frame of ROIs. Required columns: `roi_start`, `roi_end`
#'   (seconds) and `roi_label`; optional `roi_min_freq`/`roi_max_freq` (kHz,
#'   written as Hz) add the frequency band, `roi_channel` maps to the Raven
#'   `Channel` column (`"right"` -> 2, else 1), and `soundscape_file` is written
#'   to `Begin File` so the round trip restores provenance (DEC-18). Any
#'   [fetch_rois()] output qualifies.
#' @param path Output file path for the Raven selection table.
#' @return `path`, invisibly (the function is called for its file side effect).
#' @seealso [read_raven_selection()], [write_audacity_labels()]
#' @examples
#' # Off-flow utility: export an ROI table to Raven format in tempdir.
#' \donttest{
#' rois <- data.frame(
#'   roi_start = 1, roi_end = 2, roi_min_freq = 2, roi_max_freq = 6,
#'   roi_label = "sp1", soundscape_file = "rec.wav")
#' f <- write_raven_selection(rois, file.path(tempdir(), "sel.txt"))
#' readLines(f, n = 2)   # header + first selection row
#' }
#' @export
write_raven_selection <- function(rois, path) {
  rois <- .coerce_rois(rois)
  out <- data.frame(
    Selection            = seq_len(nrow(rois)),
    View                 = "Spectrogram 1",
    Channel              = .roi_channel_to_raven(rois$roi_channel),
    `Begin Time (s)`     = rois$roi_start,
    `End Time (s)`       = rois$roi_end,
    `Low Freq (Hz)`      = rois$roi_min_freq * 1000,
    `High Freq (Hz)`     = rois$roi_max_freq * 1000,
    `Begin File`         = rois$soundscape_file,
    Annotation           = rois$roi_label,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  utils::write.table(out, path, sep = "\t", quote = FALSE, row.names = FALSE)
  invisible(path)
}

#' Write ROI data as an Audacity label track
#'
#' @description Exports a monitoraSom ROI table to an Audacity label
#'   track, so ROIs can be opened and edited in Audacity. Frequencies are
#'   converted from kHz back to Hz. The inverse of [read_audacity_labels()].
#'
#' @details
#'   Each ROI is written as `start<TAB>end<TAB>label`; rows carrying a frequency
#'   band add the `\<TAB>lowHz<TAB>highHz` continuation line (Audacity's extended
#'   format), while rows with `NA` frequencies omit it. This is exactly the layout
#'   [read_audacity_labels()] parses, so a write-then-read round trip returns the
#'   same ROIs (frequencies restored to kHz).
#'
#' @param rois A data.frame of ROIs. Required columns: `roi_start`, `roi_end`
#'   (seconds) and `roi_label`; optional `roi_min_freq`/`roi_max_freq` (kHz,
#'   written as Hz) add the frequency band. Any [fetch_rois()] output qualifies.
#' @param path Output file path for the Audacity label track.
#' @return `path`, invisibly (the function is called for its file side effect).
#' @seealso [read_audacity_labels()], [write_raven_selection()]
#' @examples
#' # Off-flow utility: export an ROI table to Audacity format in tempdir.
#' \donttest{
#' rois <- data.frame(
#'   roi_start = 1, roi_end = 2, roi_min_freq = 2, roi_max_freq = 6,
#'   roi_label = "sp1", soundscape_file = "rec.wav")
#' f <- write_audacity_labels(rois, file.path(tempdir(), "labels.txt"))
#' readLines(f)   # label line + frequency continuation line
#' }
#' @export
write_audacity_labels <- function(rois, path) {
  rois <- .coerce_rois(rois)
  con <- file(path, open = "wt")
  on.exit(close(con), add = TRUE)
  for (i in seq_len(nrow(rois))) {
    writeLines(paste(rois$roi_start[i], rois$roi_end[i],
                     rois$roi_label[i], sep = "\t"), con)
    if (!is.na(rois$roi_min_freq[i]) && !is.na(rois$roi_max_freq[i])) {
      writeLines(paste("\\", rois$roi_min_freq[i] * 1000,
                       rois$roi_max_freq[i] * 1000, sep = "\t"), con)
    }
  }
  invisible(path)
}
