#' Export ROI cuts as templates (standard format + template database)
#'
#' @description
#' Turns the focal (template) branch of `df_rois` into a set of template audio
#' cuts plus a DuckDB template database describing them. For each ROI it writes a short,
#' stably named mono WAV cut of the source recording and records the template's
#' metadata (times, frequency band, channel, source origin and integrity hashes)
#' in a `templates.duckdb` template database. This is where segmented ROIs become the
#' template library the matching engine searches for.
#'
#' @details
#' Feed it the ROIs you want to use as templates, usually the focal ROIs you
#' selected with [fetch_rois()]. Since monitoraSom 1.2.0 the template metadata
#' lives in the template database (the `templates.duckdb` database), not in the file
#' names. File names are short and never collide, because `template_id` is a
#' content hash. So read templates back inside the R session with
#' [fetch_template_metadata()]; do not parse the file names.
#'
#' Two modes control what is written. `"standalone_audio"` (default) writes a
#' portable WAV cut per ROI. Use it when the templates must work independently
#' of the source recordings. `"reference_metadata"` writes template database rows only
#' and cuts the audio on demand, at read time. Use it to avoid duplicating
#' audio when the source recordings stay at a stable path. Cuts are always
#' **mono**, taken from the channel the ROI was segmented on (`roi_channel`).
#'
#' Things to know: the target directory must exist unless you pass
#' `create_dir = TRUE`; existing cuts are **not** overwritten unless you pass
#' `overwrite = TRUE`. A skipped cut still refreshes its template database row from the
#' file on disk, so the stored metadata always matches the existing cut.
#' ROIs assigned the "no signals of interest" placeholder are dropped; and a
#' physically impossible frequency
#' band or a missing/degenerate label or time span stops the function before
#' anything is written (frequencies are **kHz**; a value above Nyquist, half
#' the sample rate and the highest frequency a recording can represent,
#' usually means Hz were entered by mistake).
#'
#' @section Pipeline context:
#' Step 3 of the monitoraSom analysis flow. Reads the focal (template) branch
#' of the `df_rois` returned by \code{\link{fetch_rois}} (step 2). Produces
#' template WAV cuts and the `templates.duckdb` template database that
#' \code{\link{fetch_template_metadata}} (step 4) reads back.
#'
#' @param df_rois ROI data (a data.frame of ROIs in the standard format); extra
#'   columns are kept. Usually the focal branch of
#'   [fetch_rois()]. `NULL` (default) reads the standard ROI database
#'   `rois.duckdb` via [fetch_rois()] and stops if it is absent.
#'   Must carry `soundscape_path`, `soundscape_file`, `roi_label`,
#'   `roi_start`, `roi_end`, `roi_min_freq`, `roi_max_freq`, `roi_wl` and
#'   `roi_ovlp`.
#' @param templates_path Directory to write the template WAV files
#'   into, and the default location of the template database when `template_db` is `NULL`.
#'   Defaults to `"templates/"`.
#' @param overwrite Logical. When `TRUE`, re-cut and overwrite template files that
#'   already exist; when `FALSE` (default) existing cuts are kept and reported as
#'   `"skipped"` (their template database row is still refreshed from the file on disk).
#' @param template_db Optional path to the template database `.duckdb`. When
#'   `NULL` (default) it is `<templates_path>/templates.duckdb`. DuckDB only,
#'   and the directory of the template database must already exist. Change it only to
#'   keep the template database apart from the cuts.
#' @param mode Either `"standalone_audio"` (default; write a portable WAV cut
#'   per ROI) or `"reference_metadata"` (write template database rows only, cut on
#'   demand at read time). Choose by whether the templates must work without
#'   their source recordings.
#' @param create_dir Logical. When `TRUE`, create `templates_path` if it is
#'   missing; when `FALSE` (default) a missing directory is an error.
#'
#' @return Invisibly, the template database data.frame (standard template format plus a
#'   transient `template_status` column, one of `"written"`/`"skipped"`/
#'   `"failed"`/`"referenced"`), one row per input ROI. The persisted
#'   `templates.duckdb` holds only the standard columns.
#'
#' @seealso \code{\link{fetch_rois}} (the previous step that provides the ROIs);
#'   \code{\link{fetch_template_metadata}} (the next step that reads the template database
#'   back).
#'
#' @export
#' @examples
#' \dontrun{
#' # Load the package
#' library(monitoraSom)
#' # Step 3: cut the focal (template) ROIs into WAVs + a templates.duckdb template database.
#' # (The package ships no field recordings, so this example synthesizes one;
#' # for the same steps on real data see [fetch_example_data()].)
#' wav <- file.path(tempdir(), "rec_01.wav")
#' rec <- tuneR::normalize(tuneR::sine(4000, duration = 3 * 16000,
#'                                     samp.rate = 16000), unit = "16")
#' tuneR::writeWave(rec, wav)
#' df_rois <- data.frame(
#'   soundscape_path = wav, soundscape_file = "rec_01.wav",
#'   roi_label = "burst", roi_start = 0.5, roi_end = 1.0,
#'   roi_min_freq = 2, roi_max_freq = 6, roi_wl = 512, roi_ovlp = 50,
#'   stringsAsFactors = FALSE)
#'
#' out_dir <- file.path(tempdir(), "templates")
#' template_database <- export_templates(df_rois, templates_path = out_dir,
#'                                       create_dir = TRUE)
#' str(template_database)   # standard template format + transient template_status
#'
#' # Variation: template database-only, cut on demand later (no WAV duplication):
#' export_templates(df_rois, templates_path = out_dir,
#'                 mode = "reference_metadata", overwrite = TRUE)
#' list.files(out_dir)     # no new WAV cuts; only templates.duckdb updated
#' }
export_templates <- function(df_rois = NULL,
                            templates_path = "templates/",
                            overwrite = FALSE,
                            template_db = NULL,
                            mode = c("standalone_audio", "reference_metadata"),
                            create_dir = FALSE) {
  mode <- match.arg(mode)

  # FEAT-07: warn (and confirm, interactively) before writing to an unmarked
  # default path. Silent inside a workspace or the session tempdir().
  .require_explicit_workspace(
    templates_path,
    default_target = !"templates_path" %in% names(match.call())[-1L],
    label = "templates_path", caller = "export_templates")

  # CRAN item 7 (F4): no ROI object -> read the canonical ROI store
  # (rois.duckdb at the project root) and stop if it is absent.
  if (is.null(df_rois)) {
    df_rois <- fetch_rois()
  }

  # ERC-03: validate the input carries the columns the function reads, then
  # coerce to the canonical ROI schema (single source of truth).
  required <- c("soundscape_path", "soundscape_file", "roi_label",
                "roi_start", "roi_end", "roi_min_freq", "roi_max_freq",
                "roi_wl", "roi_ovlp")
  missing_cols <- setdiff(required, names(df_rois))
  if (length(missing_cols) > 0) {
    stop("df_rois is missing required column(s): ",
         paste(missing_cols, collapse = ", "))
  }
  if (nrow(df_rois) == 0) {
    stop("No ROIs available in the provided ROI table")
  }
  # Keep optional provenance (origin_*) columns through the coercion.
  origin_cols <- grep("^origin_", names(df_rois), value = TRUE)
  rois <- .coerce_rois(df_rois)
  for (col in origin_cols) rois[[col]] <- df_rois[[col]]

  # ERC-107 (plan §8.8): the effective label names the cut and the template database,
  # so a label corrected in the validation app is reflected here. The column
  # is optional (legacy 19-col frames yield the creation-time label).
  if ("roi_label_updated" %in% names(df_rois)) {
    rois$roi_label_updated <- df_rois$roi_label_updated
    rois$roi_label <- .signal_effective_label(
      rois$roi_label, rois$roi_label_updated)
  }

  # ERC-09: drop no_soi sentinel rows (meaningless as templates).
  is_sentinel <- .is_no_soi_label(rois$roi_label)
  if (any(is_sentinel)) {
    warning(sprintf("Dropped %d no_soi sentinel row(s) before export",
                    sum(is_sentinel)))
    rois <- rois[!is_sentinel, , drop = FALSE]
  }
  if (nrow(rois) == 0) {
    stop("No ROIs available after dropping no_soi sentinel rows")
  }

  # AFL-03: reject a physically impossible frequency band before any I/O.
  .validate_roi_freq_range(rois)

  # AUD-28: identity-critical values must be present and sane before they flow
  # into the template_id, the cut filename (an NA roi_label becomes a literal
  # "NA") and the template database. NA/inverted times would otherwise surface only as a
  # raw tuneR error mid-write, or silently for degenerate spans.
  .validate_roi_identity(rois)

  # ERC-08: directory handling. dir.exists() only tests existence (not
  # writability); optionally create it, otherwise fail with an honest message.
  if (!dir.exists(templates_path)) {
    if (create_dir) {
      dir.create(templates_path, recursive = TRUE, showWarnings = FALSE)
    } else {
      stop("Specified path does not exist: ", templates_path,
           " (pass create_dir = TRUE to create it)")
    }
  }
  if (file.access(templates_path, mode = 2L) != 0L) {
    stop("Specified path is not writable: ", templates_path)
  }

  template_db_path <- if (is.null(template_db)) {
    # AFL-17: canonical template database filename from the single source of truth.
    file.path(templates_path, .monitora_db_names()[["templates"]])
  } else {
    template_db
  }

  # AUD-29: fail fast if a custom template database points into a nonexistent directory,
  # BEFORE any cut is written (otherwise every WAV is written first and only then
  # .template_duckdb_connect fails with a raw duckdb error, leaving partial state
  # and no template database, with no hint that only the template database path was wrong).
  template_db_dir <- dirname(template_db_path)
  if (!dir.exists(template_db_dir)) {
    stop("export_templates: the template database directory does not exist: ", template_db_dir,
         " (create it, or pass a template database under an existing directory).")
  }

  # ERC-05: build a list of row records (no repeated data.frame row-subsetting).
  records <- lapply(seq_len(nrow(rois)), function(i) {
    .build_template_record(rois[i, , drop = FALSE], templates_path, mode,
                           overwrite)
  })
  template_db_df <- dplyr::bind_rows(records)

  # Persist canonical columns to the template database .duckdb (ERC-102). The transient
  # template_status column is dropped by .coerce_templates.
  persistable <- template_db_df[
    !is.na(template_db_df$template_id) &
      template_db_df$template_status != "failed", , drop = FALSE]
  if (nrow(persistable) > 0) {
    con <- .template_duckdb_connect(template_db_path)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    .template_duckdb_upsert(con, persistable)
  }

  n_written <- sum(template_db_df$template_status == "written")
  n_ref     <- sum(template_db_df$template_status == "referenced")
  n_skip    <- sum(template_db_df$template_status == "skipped")
  n_fail    <- sum(template_db_df$template_status == "failed")
  if (n_fail == 0) {
    message(sprintf(
      "All %d template(s) processed successfully (%d written, %d referenced, %d skipped)",
      nrow(template_db_df), n_written, n_ref, n_skip))
  } else {
    message(sprintf(
      "%d of %d template(s) failed. Check the output for details.",
      n_fail, nrow(template_db_df)))
  }

  invisible(template_db_df)
}

# --- AFL-03: frequency unit / range validation -------------------------------

#' Validate the ROI frequency band against physical limits (AFL-03).
#'
#' Frequencies are **kHz** by schema convention ([_schema_rois.R]) while
#' `roi_sample_rate` is **Hz**. A band outside `[0, Nyquist]` (Nyquist =
#' `roi_sample_rate / 2`, expressed in kHz) is physically impossible and is the
#' signature of a Hz/kHz unit mix-up (e.g. `22050` entered instead of `22.05`).
#' Front-door hard error naming the offending rows; the *inverted* band
#' (`roi_min_freq > roi_max_freq`) is AFL-01's concern, not this one. The Nyquist
#' bound is only enforced for rows that carry a positive `roi_sample_rate`.
#' @noRd
.validate_roi_freq_range <- function(rois) {
  min_f <- rois$roi_min_freq
  max_f <- rois$roi_max_freq
  sr    <- rois$roi_sample_rate

  neg <- (!is.na(min_f) & min_f < 0) | (!is.na(max_f) & max_f < 0)

  nyq_khz <- sr / 2000
  has_nyq <- !is.na(sr) & sr > 0
  over <- has_nyq &
    ((!is.na(min_f) & min_f > nyq_khz) | (!is.na(max_f) & max_f > nyq_khz))

  bad <- which(neg | over)
  if (length(bad) > 0) {
    stop("export_templates: ", length(bad),
         " ROI(s) with a frequency band outside [0, Nyquist] kHz ",
         "(frequencies are kHz; a value above Nyquist = roi_sample_rate / 2 ",
         "usually means Hz were entered instead of kHz): rows ",
         paste(bad, collapse = ", "), ".")
  }
  invisible(rois)
}

#' Validate the identity-critical ROI values (AUD-28).
#'
#' `roi_label` names the template (id + file name); `roi_start`/`roi_end` define
#' its audio span. A missing label yields a literal `"NA"` in the file name and
#' template database; NA/non-finite or non-increasing times would only surface later as a
#' raw `readWave` error (or silently for a degenerate span). Front-door hard
#' error naming the offending rows, mirroring [.validate_roi_freq_range()].
#' @noRd
.validate_roi_identity <- function(rois) {
  lab <- as.character(rois$roi_label)
  bad_label <- which(is.na(lab) | !nzchar(trimws(lab)))
  if (length(bad_label) > 0) {
    stop("export_templates: ", length(bad_label),
         " ROI(s) with a missing/empty roi_label (rows ",
         paste(bad_label, collapse = ", "),
         "); a template needs a non-empty label for its id and file name.")
  }
  s <- rois$roi_start
  e <- rois$roi_end
  bad_time <- which(is.na(s) | is.na(e) | !is.finite(s) | !is.finite(e) | s >= e)
  if (length(bad_time) > 0) {
    stop("export_templates: ", length(bad_time),
         " ROI(s) with NA/non-finite or non-increasing roi_start/roi_end (rows ",
         paste(bad_time, collapse = ", "), ").")
  }
  invisible(rois)
}

# --- per-ROI record builder --------------------------------------------------

#' Build one template database record (+ write the cut in standalone mode).
#'
#' Returns a one-row data.frame in the canonical template schema plus a
#' transient `template_status` column. Computes the source provenance hash
#' (ERC-104), the stable `template_id`, the channel-correct mono cut (ERC-105)
#' and the short stable file name (ERC-101).
#' @noRd
.build_template_record <- function(roi, templates_path, mode, overwrite) {
  src_path <- roi$soundscape_path
  # ERC-105: channel the ROI was segmented on; legacy NA defaults to "left"
  # ([_schema_rois.R]; on a mono source this passes through unchanged).
  channel <- if (is.na(roi$roi_channel)) "left" else roi$roi_channel

  origin_sha256 <- .resolve_origin_sha256(roi, src_path)

  template_id <- .template_id(
    origin_sha256, src_path, roi$roi_start, roi$roi_end, channel,
    roi$roi_wl, roi$roi_ovlp, roi$roi_label
  )
  cut_name <- .template_cut_name(roi$soundscape_file, roi$roi_label,
                                 template_id, roi$roi_type)
  out_path <- file.path(templates_path, cut_name)

  rec <- .template_metadata_row(template_id, roi, src_path, channel,
                                origin_sha256, mode)

  if (mode == "reference_metadata") {
    # template database-only: no WAV. template_path/sha256 stay NA; resolved on demand
    # later (FTM-104) against the source, verifying origin_soundscape_sha256.
    rec$template_status <- "referenced"
    return(rec)
  }

  # standalone_audio: write the channel-correct mono cut.
  .write_template_cut(rec, roi, src_path, out_path, cut_name, channel,
                      overwrite)
}

#' ERC-104: source-recording sha256 for a template cut. Prefers the FR-103
#' origin_ provenance when it was joined upstream; otherwise computes it from
#' the file (when present). AFL-26: with no provenance hash and an
#' inaccessible source file, .template_id() falls back to the literal source
#' path as the identity component -- an id that is order/environment
#' dependent (a different path string for the same audio content yields a
#' different template_id); this case warns.
#' @noRd
.resolve_origin_sha256 <- function(roi, src_path) {
  origin_sha256 <- if (!is.null(roi$origin_soundscape_sha256) &&
                        !is.na(roi$origin_soundscape_sha256)) {
    roi$origin_soundscape_sha256
  } else if (file.exists(src_path)) {
    digest::digest(src_path, algo = "sha256", file = TRUE)
  } else {
    NA_character_
  }
  if (is.na(origin_sha256)) {
    warning("No source-recording sha256 available for ", src_path,
            " (no provenance hash and the file is not accessible); template_id ",
            "falls back to a path-based identity, which is not stable across ",
            "environments.")
  }
  origin_sha256
}

#' ROI-derived metadata half of a template record: fills the canonical
#' one-row schema (.schema_templates) from the ROI plus the resolved identity
#' (template_id, channel, origin provenance). Path/sha/rate fields stay NA
#' until the cut is written or refilled from disk (AUD-27); the transient
#' `template_status` column starts NA and is set by the mode/write helpers.
#' @noRd
.template_metadata_row <- function(template_id, roi, src_path, channel,
                                   origin_sha256, mode) {
  rec <- .schema_templates(1L)
  rec$template_id              <- template_id
  rec$template_label           <- roi$roi_label
  rec$template_mode            <- mode
  rec$template_start           <- roi$roi_start
  rec$template_end             <- roi$roi_end
  rec$template_min_freq        <- roi$roi_min_freq
  rec$template_max_freq        <- roi$roi_max_freq
  rec$template_wl              <- roi$roi_wl
  rec$template_ovlp            <- roi$roi_ovlp
  rec$template_pitch_shift     <- roi$roi_pitch_shift
  rec$template_channel         <- channel
  rec$origin_soundscape_path   <- .to_forward_slashes(src_path)
  rec$origin_soundscape_file   <- roi$soundscape_file
  rec$origin_soundscape_sha256 <- origin_sha256
  rec$roi_type                 <- roi$roi_type
  rec$roi_user                 <- roi$roi_user
  rec$roi_input_timestamp      <- roi$roi_input_timestamp
  rec$roi_comment              <- roi$roi_comment
  rec$template_status          <- NA_character_
  rec
}

#' standalone_audio half of a template record: writes the channel-correct mono
#' cut (or, when the cut exists and overwrite is FALSE, refills the integrity
#' metadata from the existing file -- AUD-27) and stamps `template_status`.
#' @noRd
.write_template_cut <- function(rec, roi, src_path, out_path, cut_name,
                                channel, overwrite) {
  if (!file.exists(src_path)) {
    warning("File does not exist: ", src_path)
    rec$template_status <- "failed"
    return(rec)
  }
  if (file.exists(out_path) && !overwrite) {
    # AUD-27: a skip must NOT degrade the template database. The upsert replaces the whole
    # row (DELETE + APPEND, [_template_duckdb.R]), so leaving template_sha256 /
    # template_sample_rate NA here would clobber the ERC-104 integrity metadata a
    # prior "written" run recorded (and downstream verify_integrity reads NA as
    # "not verifiable => kept", losing the check silently). Refill both from the
    # existing cut on disk so the template database always reflects disk truth.
    # Cross-mode note: a "reference_metadata" and a "standalone_audio" export of
    # the same ROI share a template_id and intentionally replace each other;
    # `mode` is deliberately NOT part of the id, to keep the id stable across
    # environments (AFL-26). Filling from disk here ensures the standalone row is
    # never left metadata-poor after such a replacement.
    message("Skipping overwrite of existing file: ", out_path)
    rec$template_path        <- .to_forward_slashes(out_path)
    rec$template_file        <- cut_name
    rec$template_sha256      <- digest::digest(out_path, algo = "sha256", file = TRUE)
    rec$template_sample_rate <- as.integer(
      tuneR::readWave(out_path, header = TRUE)$sample.rate)
    rec$template_status      <- "skipped"
    return(rec)
  }

  written <- tryCatch({
    sound <- tuneR::readWave(src_path, from = roi$roi_start, to = roi$roi_end,
                             units = "seconds")
    # AFL-13: defaulting NA channel to "left" is harmless on a mono source (it
    # passes through unchanged), but on a stereo source it silently picks a side.
    # Warn only in that genuinely ambiguous case.
    if (is.na(roi$roi_channel) && isTRUE(sound@stereo)) {
      warning("Stereo source with no recorded roi_channel; defaulted to \"left\" ",
              "for ", cut_name, ". Verify this is the intended channel.")
    }
    cut <- .select_channel_mono(sound, channel)
    tuneR::writeWave(cut, out_path)
    rec$template_sample_rate <- as.integer(cut@samp.rate)
    TRUE
  }, error = function(e) {
    message("Failed to process: ", out_path, " Error: ", e$message)
    FALSE
  })

  if (!written) {
    rec$template_status <- "failed"
    return(rec)
  }

  rec$template_path   <- .to_forward_slashes(out_path)
  rec$template_file   <- cut_name
  rec$template_sha256 <- digest::digest(out_path, algo = "sha256", file = TRUE)
  rec$template_status <- "written"
  rec
}

#' Reduce a (possibly stereo) Wave to the mono channel a ROI was segmented on
#' (ERC-04/105). Mono input passes through; `"left"`/`"right"` pick that channel
#' of a stereo source; any other value falls back to the left channel.
#' @noRd
.select_channel_mono <- function(wave, channel) {
  if (!isTRUE(wave@stereo)) return(wave)
  which <- if (identical(channel, "right")) "right" else "left"
  tuneR::mono(wave, which = which)
}

# --- CRAN item 3: compatibility shim (roi_cuts -> templates rename) -----------

#' Cut ROI templates into a directory (deprecated alias of export_templates).
#'
#' Kept so scripts written before the CRAN-item-3 rename (`export_roi_cuts()` /
#' `roi_cuts_path`) keep running; new code should call
#' [export_templates()] with `templates_path`.
#'
#' @param df_rois,roi_cuts_path,overwrite,template_db,mode,create_dir See
#'   [export_templates()]; `roi_cuts_path` maps to `templates_path`.
#' @return Invisibly, the template database data.frame (see [export_templates()]).
#' @keywords internal
#' @export
export_roi_cuts <- function(df_rois = NULL,
                            roi_cuts_path = "roi_cuts/",
                            overwrite = FALSE,
                            template_db = NULL,
                            mode = c("standalone_audio", "reference_metadata"),
                            create_dir = FALSE) {
  .Deprecated("export_templates",
              old = "export_roi_cuts(roi_cuts_path = ...)",
              msg = paste0(
                "'export_roi_cuts' is deprecated; use ",
                "'export_templates(templates_path = ...)' ",
                "(CRAN item 3, templates/ consolidation)."
              ))
  export_templates(
    df_rois = df_rois,
    templates_path = roi_cuts_path,
    overwrite = overwrite,
    template_db = template_db,
    mode = mode,
    create_dir = create_dir
  )
}
