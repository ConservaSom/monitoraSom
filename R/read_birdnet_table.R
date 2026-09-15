# Family file — exports: import_birdnet_detections() (DQ-02 policy,
# specs/README.md "File-family exception").
#' Import BirdNET-Analyzer detection tables as monitoraSom detections
#'
#' @description Reads one BirdNET-Analyzer result file (`table` or `csv`),
#'   converts it to the standard `signals` frame with `signal_class =
#'   "detection"`, and idempotently writes it into a `signals.duckdb` database
#'   (re-importing updates the same rows). One row per BirdNET detection, in
#'   the standard 44-column signals format. Returns the frame invisibly.
#'
#' @details
#'   BirdNET-Analyzer's default result type is `table`: a tab-separated,
#'   Raven-style selection table with the detection box (`Begin Time (s)` /
#'   `End Time (s)`, `Low Freq (Hz)` / `High Freq (Hz)`), the species
#'   (`Common Name` / `Species Code`) and a model `Confidence` score. The `csv`
#'   result type is lossy (no frequency band, no channel) and is also
#'   supported. The importer parses the file **directly** (it does not reuse
#'   [read_raven_selection()]); the channel vocabulary is the only part shared
#'   with the Raven importer.
#'
#'   **Semantic mapping (fixed):** BirdNET's `Confidence` column is a numeric
#'   **model score** (0–1), not a label category. It maps **directly** to
#'   `det_peak_score` (numeric). It does **NOT** map to `roi_label_confidence`
#'   (character category like `"certain"` in ROI data). The English
#'   `Common Name` is stored in the detection's free-text `roi_comment`; the
#'   stable `Species Code` (eBird code) becomes `roi_label`,
#'   and `det_template_name`.
#'
#'   A multiclass model such as BirdNET works like several single-class
#'   models running side by side. From this import on, monitoraSom treats
#'   every species (class) as its own "template", which is what the
#'   validation and diagnostics steps expect; see [diagnostic_validations()]
#'   for the per-class reading. Diagnosing the model as a whole (all classes
#'   together) is not available yet.
#'
#'   Detection rows are `signal_class = "detection"` and are **never**
#'   eligible as validation ground truth, so BirdNET rows validate only as
#'   TP/FP candidates.
#'
#'   Importing the same file twice is safe: `signal_id` is a deterministic
#'   sha256 over (normalized path, start, end, species code), so a re-import
#'   updates the same rows instead of duplicating them.
#'
#' @param path Path to one BirdNET result file (`table` or `csv`).
#' @param signals_db Path to the `signals.duckdb` database (created if missing).
#' @param min_conf Numeric threshold on `Confidence`; rows with
#'   `Confidence < min_conf` are dropped. Default `0` keeps every row
#'   (lossless; BirdNET's own 0.1 export filter is a user export-time
#'   decision).
#' @param format `"table"`, `"csv"`, or `NULL` (default) to auto-detect by
#'   extension and header sniff.
#' @param origin Origin stamp for the `origin` column (default `"birdnet"`).
#' @return The imported signals frame, invisibly.
#' @seealso [read_raven_selection()], [validate_by_overlap()]
#' @examples
#' \donttest{
#' # Off-flow utility: import a synthetic BirdNET table into a temporary
#' # database.
#' # Load the package
#' library(monitoraSom)
#' f <- tempfile(fileext = ".BirdNET.results.table.txt")
#' hdr <- paste(c("Selection", "View", "Channel", "Begin Time (s)",
#'               "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)",
#'               "Common Name", "Species Code", "Confidence"),
#'             collapse = "\t")
#' writeLines(c(hdr,
#'   "1\tSpectrogram 1\t1\t0.5\t1.5\t2000\t6500\tMyiothlypis flaveola\tmyifla\t0.87"
#' ), f)
#' db <- tempfile(fileext = ".duckdb")
#' sig <- import_birdnet_detections(f, db)
#' sig[, c("roi_label", "roi_comment", "det_peak_score")]
#' }
#' @export
import_birdnet_detections <- function(path, signals_db, min_conf = 0,
                                      format = NULL, origin = "birdnet") {
  if (length(path) != 1L || !is.character(path) || is.na(path) ||
      !file.exists(path)) {
    stop("import_birdnet_detections: 'path' must be a single existing file path.")
  }
  if (!is.numeric(min_conf) || length(min_conf) != 1L || is.na(min_conf) ||
      min_conf < 0 || min_conf > 1) {
    stop("import_birdnet_detections: 'min_conf' must be a single number in [0, 1].")
  }
  if (!is.null(format) && !format %in% c("table", "csv")) {
    stop("import_birdnet_detections: 'format' must be 'table', 'csv' or NULL.")
  }

  tbl <- .read_birdnet_table(path, format)
  sig <- .birdnet_as_signals(tbl, path, origin)
  if (nrow(sig) > 0L) sig <- sig[sig$det_peak_score >= min_conf, , drop = FALSE]

  con <- .signals_duckdb_connect(signals_db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .signals_duckdb_upsert(con, sig)
  invisible(sig)
}

# Read one BirdNET result file into a plain data.frame (direct parse, Q2 = C).
# `format = NULL` auto-detects: `.csv` extension -> csv reader; otherwise the
# tab-separated Raven-style header -> table reader. Required columns fail fast.
.read_birdnet_table <- function(path, format = NULL) {
  if (is.null(format)) {
    format <- if (grepl("\\.csv$", path, ignore.case = TRUE)) "csv" else "table"
  }
  if (format == "csv") return(.read_birdnet_csv(path))
  .read_birdnet_table_tsv(path)
}

# Parse the default `table` output (tab-separated Raven-style selection table).
.read_birdnet_table_tsv <- function(path) {
  tbl <- utils::read.delim(path, sep = "\t", header = TRUE, check.names = FALSE,
                           stringsAsFactors = FALSE, quote = "")
  need <- c("Begin Time (s)", "End Time (s)", "Common Name", "Species Code",
            "Confidence")
  miss <- setdiff(need, names(tbl))
  if (length(miss) > 0) {
    stop("read_birdnet_table: missing required column(s): ",
         paste(miss, collapse = ", "), ".")
  }
  if ("View" %in% names(tbl)) {
    spec_rows <- grepl("spectrogram", tbl[["View"]], ignore.case = TRUE)
    if (any(spec_rows)) tbl <- tbl[spec_rows, , drop = FALSE]
  }
  tbl
}

# Parse the `csv` output (flat column set, no frequency band / channel).
.read_birdnet_csv <- function(path) {
  tbl <- utils::read.csv(path, header = TRUE, check.names = FALSE,
                         stringsAsFactors = FALSE)
  need <- c("Start (s)", "End (s)", "Common name", "Confidence")
  miss <- setdiff(need, names(tbl))
  if (length(miss) > 0) {
    stop("read_birdnet_table: missing required column(s): ",
         paste(miss, collapse = ", "), ".")
  }
  tbl
}

# Convert a parsed BirdNET frame to the canonical signals schema. Every row is
# `signal_class = "detection"`, `origin = "birdnet"`. The label columns carry
# the Species Code; the English Common Name goes to roi_comment; the numeric
# Confidence score goes to det_peak_score (never roi_label_confidence).
.birdnet_as_signals <- function(tbl, path, origin = "birdnet") {
  n <- nrow(tbl)
  out <- .schema_signals(n)
  out$signal_class <- .SIGNAL_CLASS_DETECTION
  out$origin <- origin
  has <- function(col) col %in% names(tbl)

  start_col <- if (has("Begin Time (s)")) "Begin Time (s)" else "Start (s)"
  end_col   <- if (has("End Time (s)")) "End Time (s)" else "End (s)"
  out$roi_start <- suppressWarnings(as.numeric(tbl[[start_col]]))
  out$roi_end   <- suppressWarnings(as.numeric(tbl[[end_col]]))
  if (has("Low Freq (Hz)")) {
    out$roi_min_freq <- suppressWarnings(as.numeric(tbl[["Low Freq (Hz)"]])) / 1000
  }
  if (has("High Freq (Hz)")) {
    out$roi_max_freq <- suppressWarnings(as.numeric(tbl[["High Freq (Hz)"]])) / 1000
  }
  # The label columns carry the Species Code (table) or Scientific name (csv);
  # the Common Name goes to the free-text comment in both formats.
  code_col <- if (has("Species Code")) "Species Code" else "Scientific name"
  out$roi_label <- as.character(tbl[[code_col]])
  name_col <- if (has("Common Name")) "Common Name" else "Common name"
  out$roi_comment <- as.character(tbl[[name_col]])
  out$roi_channel <- if (has("Channel")) .raven_channel_to_roi(tbl[["Channel"]])
                     else NA_character_
  out$det_template_name <- as.character(tbl[[code_col]])
  out$det_score_method <- "birdnet"
  out$det_peak_score <- suppressWarnings(as.numeric(tbl[["Confidence"]]))

  out$soundscape_file <- if (has("Begin File")) as.character(tbl[["Begin File"]])
    else if (has("File")) as.character(tbl[["File"]])
    else basename(path)
  out$soundscape_path <- if (has("Begin Path")) as.character(tbl[["Begin Path"]])
    else if (!is.null(path)) path else NA_character_

  # DEC-17 (STEP-17): preserve fields the detections schema has no column for.
  # The BirdNET confidence already maps to det_peak_score; anything else
  # (e.g. an "Overlap" or site column in a csv export) is kept in the
  # free-text comment rather than dropped.
  extras <- rep(NA_character_, n)
  if (has("Overlap")) {
    ov <- suppressWarnings(as.numeric(tbl[["Overlap"]]))
    has_ov <- !is.na(ov)
    extras[has_ov] <- paste0("overlap: ", ov[has_ov])
  }
  has_extra <- !is.na(extras) & nzchar(extras)
  out$roi_comment[has_extra] <- ifelse(
    is.na(out$roi_comment[has_extra]), extras[has_extra],
    paste(out$roi_comment[has_extra], extras[has_extra], sep = "; "))

  out$signal_id <- .signal_id_birdnet(out)
  .coerce_signals(out)
}

# Deterministic detection id: sha256 over (normalized path, start, end, species
# code), first 16 hex chars (AFL-24). Re-import of the same box yields the same
# id, so upserts are idempotent.
.signal_id_birdnet <- function(sig) {
  n <- nrow(sig)
  if (n == 0L) return(character(0))
  path <- .normalize_path_key(sig$soundscape_path)
  num  <- function(x) sprintf("%.15g", x)
  key  <- paste(path, num(sig$roi_start), num(sig$roi_end),
                sig$roi_label, sep = "|")
  vapply(key, function(k) {
    substr(digest::digest(k, algo = "sha256", serialize = FALSE), 1L, 16L)
  }, character(1), USE.NAMES = FALSE)
}
