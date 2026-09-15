#' Import template metadata
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Reads the metadata for a set of templates and returns it as a single
#'   data.frame in the standard monitoraSom template format (`template_*`
#'   columns). The routine input is the DuckDB template database written
#'   by [export_templates()], or by the segmentation app, when you cut
#'   templates from ROIs. This is the template-side counterpart of
#'   [fetch_soundscape_metadata()]: the two frames are later crossed by
#'   [fetch_match_grid()] to produce the search grid.
#'
#' @details
#'   Each template is one acoustic pattern you want to detect (a bird call, a
#'   frog note), described by its frequency band and time bounds. A template
#'   comes in one of two **modes**: `standalone_audio` (a short WAV cut on
#'   disk) or `reference_metadata` (bounds only, cut on demand from the source
#'   recording). This reader does not open the audio of `standalone_audio`
#'   templates; it gets everything from the template database.
#'
#'   Use the default `source = "duckdb"` for any template set produced by the
#'   current pipeline. Pass either the template database `.duckdb` or the cuts
#'   directory that holds it (the `templates/` folder; the template database inside it
#'   is found automatically). Use `source = "legacy_filename"` only to import
#'   an old template set whose parameters live in long file names (the
#'   alternative, [migrate_templates_to_db()], converts such a set into
#'   a template database once, so you never parse names again).
#'
#'   Things to know:
#'   (1) `resolve_references = TRUE` verifies, for each `reference_metadata`
#'   template, that its source recording still exists and is unchanged. It
#'   works by hashing files, so it is slower than the plain read. It also needs
#'   `soundscapes` (a `df_soundscapes` frame or a metadata `.duckdb`).
#'   (2) The returned `template_resolved` column is a diagnostic computed at
#'   fetch time. It is not part of the template database, and [fetch_match_grid()] drops
#'   it from the search grid.
#'   (3) Under the default `on_hash_mismatch = "warn"`, a template whose
#'   source hash does not match is still returned, but with
#'   `template_resolved = FALSE`.
#'   (4) `template_name` defaults to `template_file`, and later steps group
#'   detections by this column: [diagnostic_validations()] fits one model per
#'   distinct value, and [launch_validation_app()] builds its template selector
#'   from it. So one group per template cut is the useful default. Change it
#'   after the fetch when you want to group several cuts together (for
#'   example, one model per species instead of one per cut).
#'
#' @section Pipeline context:
#'   Step 4 of the monitoraSom analysis flow. Reads the template database
#'   `.duckdb` produced by [export_templates()] (step 3). Produces the
#'   `df_templates` frame used, together with `df_soundscapes`, by
#'   [fetch_match_grid()] (step 6). See also: \code{\link{export_templates}},
#'   \code{\link{fetch_match_grid}}.
#'
#' @param templates_path Single character path. `NULL` (default) reads the
#'   standard templates folder (`templates/`). For `source = "duckdb"`
#'   (routine): the template database `.duckdb` **or** the cuts directory that
#'   holds it (the `templates/` folder [export_templates()] wrote; the
#'   template database inside it is read). For `source = "legacy_filename"`: a
#'   directory of old long-name template WAVs. A directory is resolved by
#'   content: the template database inside takes priority when present, otherwise the
#'   directory is read in the file-name format used before 1.2.0.
#' @param recursive Logical, default `FALSE`. Recurse into sub-directories.
#'   Applies only to the old-format directory path; ignored for
#'   `source = "duckdb"`.
#' @param source `"duckdb"` (default, routine reader) or `"legacy_filename"`
#'   (one-off filename import). No autodetection.
#' @param resolve_references Logical, default `FALSE`. When `TRUE`, verify each
#'   `reference_metadata` template's source recording is present and matches its
#'   recorded sha256, filling `template_resolved`. A matching hash guarantees
#'   the recording is exactly the one the template was cut from, so the
#'   on-demand cut stays faithful. It costs one file hash per template; leave
#'   `FALSE` for a fast metadata read. Requires `soundscapes`.
#' @param soundscapes Source of the recordings' metadata for resolution: a
#'   `df_soundscapes` data.frame **or** a soundscape-metadata `.duckdb` path.
#'   Used only when `resolve_references = TRUE`.
#' @param soundscapes_dir Optional directory of source recordings, a fallback for
#'   users who have the audio but not the metadata `.duckdb` (files are
#'   re-hashed). Secondary to `soundscapes`; used only when resolving.
#' @param on_hash_mismatch `"warn"` (default), `"error"` or `"skip"`, applied
#'   when a resolved source's sha256 differs from the recorded value. The hash
#'   is a fingerprint of the file content: a different hash means the recording
#'   was modified or replaced after the template was created, so an on-demand
#'   cut would no longer match the template. `"warn"` returns the row marked
#'   `template_resolved = FALSE` and warns; `"skip"` marks it the same way
#'   without a warning; `"error"` stops. Relevant only when
#'   `resolve_references = TRUE`.
#'
#' @return A data.frame of templates in the standard `template_*` column
#'   layout, one row per template, plus two columns that are added at fetch time
#'   rather than stored in the template database: `template_name` (character, the
#'   grouping label carried through the grid into the detections format --
#'   defaults to `template_file` when the template database does not supply it) and the
#'   trailing logical `template_resolved` (`NA` when
#'   `resolve_references = FALSE`, else `TRUE`/`FALSE` per template, located AND
#'   verified).
#'
#' @seealso [export_templates()] (upstream, step 3), [fetch_match_grid()]
#'   (downstream, step 6), [catalog_templates()], [migrate_templates_to_db()].
#' @export
#' @examples
#' \dontrun{
#' # Load the package
#' library(monitoraSom)
#' # Step 4: read back the templates cut in step 3 ([export_templates()]).
#' # (The package ships no field recordings, so this example synthesizes them;
#' # for the same steps on real data see [fetch_example_data()].)
#' rec_dir <- file.path(tempdir(), "recs"); dir.create(rec_dir, showWarnings = FALSE)
#' wav <- file.path(rec_dir, "rec_01.wav")
#' rec <- tuneR::normalize(tuneR::sine(4000, duration = 3 * 16000,
#'                                     samp.rate = 16000), unit = "16")
#' tuneR::writeWave(rec, wav)
#' df_rois <- data.frame(
#'   soundscape_path = wav, soundscape_file = "rec_01.wav",
#'   roi_label = "burst", roi_start = 0.5, roi_end = 1.0,
#'   roi_min_freq = 2, roi_max_freq = 6, roi_wl = 512, roi_ovlp = 50,
#'   stringsAsFactors = FALSE)
#' out_dir <- file.path(tempdir(), "templates")
#' export_templates(df_rois, templates_path = out_dir, create_dir = TRUE)
#'
#' df_templates <- fetch_template_metadata(out_dir)   # directory: template database found
#' head(df_templates[, c("template_id", "template_label", "template_mode")])
#'
#' # Verify reference_metadata sources against their recorded hashes: the
#' # source recording must still exist and match the sha256 recorded when the
#' # template was cut. The trailing template_resolved column reports the
#' # outcome per template (TRUE = located and verified).
#' export_templates(df_rois, templates_path = out_dir,
#'                  mode = "reference_metadata", overwrite = TRUE)
#' df_soundscapes <- fetch_soundscape_metadata(rec_dir)
#' df_templates <- fetch_template_metadata(
#'   out_dir, resolve_references = TRUE, soundscapes = df_soundscapes)
#' df_templates[, c("template_file", "template_resolved")]
fetch_template_metadata <- function(templates_path = NULL,
                                    recursive = FALSE,
                                    source = c("duckdb", "legacy_filename"),
                                    resolve_references = FALSE,
                                    soundscapes = NULL,
                                    soundscapes_dir = NULL,
                                    on_hash_mismatch = c("warn", "error", "skip")) {
  source <- match.arg(source)
  on_hash_mismatch <- match.arg(on_hash_mismatch)

  # CRAN item 7 (F1): NULL resolves to the canonical templates folder
  # (cwd-relative, like template_matching()'s defaults). No fallback creates
  # anything: an absent folder stops below with an actionable message.
  if (is.null(templates_path)) {
    templates_path <- .monitora_db_homes()[["templates"]]
  }

  # AUD-25: fail with an actionable message on a non-scalar/non-character
  # templates_path instead of a raw "length > 1" / "invalid 'file' argument".
  if (length(templates_path) != 1L ||
      !is.character(templates_path) || is.na(templates_path)) {
    stop("fetch_template_metadata: 'templates_path' must be a single non-NA ",
         "character path.")
  }
  if (!file.exists(templates_path)) {
    stop("The provided path to the templates does not exist: ", templates_path)
  }

  # FTM-110: accept the cuts directory, not only the template database file inside it.
  # A directory is resolved BEFORE the source dispatch: if it holds the
  # export_templates() template database, read that (the routine case -- the flow diagram
  # draws the "template cuts *.wav" arrow at this folder); otherwise it is a
  # legacy long-name template-WAV folder (the migration path). This is additive:
  # a FILE argument behaves exactly as before, and the FTM-109 guidance still
  # applies to the template database-less legacy branch below.
  if (dir.exists(templates_path)) {
    template_db <- file.path(templates_path, .monitora_db_names()[["templates"]])
    if (file.exists(template_db)) {
      # (FTM-110-a) template database precedence over any legacy-named WAVs in the same
      # folder: warn rather than error. The routine cuts folder (template database + cut
      # WAVs named by template_id, which never carry the legacy markers) stays
      # silent; only a genuinely mixed folder surfaces the warning.
      wavs <- list.files(templates_path, pattern = "\\.wav$",
                         ignore.case = TRUE)
      legacy <- wavs[vapply(wavs, function(w) {
        !is.null(.parse_legacy_template_name(file.path(templates_path, w)))
      }, logical(1))]
      if (length(legacy) > 0) {
        warning("Directory ", templates_path, " holds both a template database ",
                "and legacy-named template WAV(s) (e.g. ", legacy[1], "). ",
                "Reading the template database; the legacy WAV(s) were ignored. To ",
                "import them instead, point `templates_path` at a folder ",
                "without a template database or use migrate_templates_to_db().")
      }
      templates_path <- template_db
      source <- "duckdb"
    } else {
      # FTM-111 (CRAN item 7, F1): a directory without a template database is only
      # usable as a legacy WAV folder. An empty folder (no WAVs) is a
      # configuration error, not a valid empty template set: stop instead of
      # silently returning a 0-row frame.
      wavs <- list.files(templates_path, pattern = "\\.wav$",
                         ignore.case = TRUE, recursive = recursive)
      if (length(wavs) == 0L) {
        stop("No templates found in '", templates_path, "': the directory ",
             "holds neither a template database (",
             .monitora_db_names()[["templates"]], ") nor template WAV files.")
      }
      source <- "legacy_filename"
    }
  }

  df_templates <- switch(
    source,
    duckdb = .read_templates_duckdb(templates_path),
    legacy_filename = .read_templates_legacy(templates_path, recursive)
  )

  # FTM-108: `template_name` is declared by the DETECTIONS schema
  # (_schema_detections.R) but by no template schema, so nothing upstream ever
  # filled it and it reached `diagnostic_validations` (which splits on it) and
  # `launch_validation_app` (which keys its selector on it) as NA -- collapsing
  # every template into one group and emptying the app's dropdown, silently.
  # Derive it here, in the producer, when the template database does not carry it. Both
  # source readers converge above, so this one assignment covers duckdb and
  # legacy alike. Deliberately NOT added to `.template_schema_spec()`: staying
  # off the spec is what makes `.coerce_templates()` drop it again before
  # `.template_duckdb_upsert()` writes, so no existing template database needs migrating.
  # NOTE: `.vbo_species()` reads the species off the LAST `_` token of
  # `template_name`; on a filename-derived value that token is the template_id
  # hash, not the species. This is safe only because AFL-04 made
  # `template_label` take precedence and left that parse as a legacy fallback --
  # do not reintroduce a species dependency on this column.
  if (!"template_name" %in% names(df_templates) ||
      all(is.na(df_templates$template_name))) {
    df_templates$template_name <- df_templates$template_file
  }

  # AFL-27: `template_resolved` is always present (logical) so consumers need not
  # guard it. NA = not resolved (resolve_references = FALSE); the resolver below
  # overwrites it with TRUE/FALSE per template when resolve_references = TRUE.
  # rep() keeps a 0-row template database valid (scalar assignment would error there).
  df_templates$template_resolved <- rep(NA, nrow(df_templates))
  if (resolve_references) {
    df_templates <- .resolve_reference_templates(
      df_templates, soundscapes, soundscapes_dir, on_hash_mismatch
    )
  }

  message(sprintf("Template metadata successfully extracted (%d template(s))",
                  nrow(df_templates)))
  df_templates
}

# --- source readers ----------------------------------------------------------

#' Read the canonical template database from a template `.duckdb` (FTM-101 routine path).
#' @noRd
.read_templates_duckdb <- function(templates_path) {
  con <- .template_duckdb_connect(templates_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  df <- .template_duckdb_read(con)
  if (nrow(df) == 0) {
    warning("The template database is empty: ", templates_path)
  }
  df
}

#' Parse a directory of legacy long-name template WAVs (FTM-101 migration path).
#'
#' Width-agnostic (FTM-03) and label-verbatim (FTM-02): the label is everything
#' after the last `ovlp_` marker, so labels containing `_` survive. Source-
#' relative bounds are taken from the name (better than the original's hardcoded
#' `template_start = 0`). Unreadable files are collected and warned (FTM-04).
#' @noRd
.read_templates_legacy <- function(templates_path, recursive) {
  if (!dir.exists(templates_path)) {
    stop("For source = 'legacy_filename', templates_path must be a directory: ",
         templates_path)
  }
  wavs <- list.files(
    templates_path, pattern = "\\.wav$", ignore.case = TRUE,
    full.names = TRUE, recursive = recursive
  )
  if (length(wavs) == 0) {
    stop("There are no WAV files in the provided path")
  }

  parsed <- lapply(wavs, .parse_legacy_template_name)
  ok <- !vapply(parsed, is.null, logical(1))
  if (!any(ok)) {
    # FTM-109: the common way to land here is pointing at a CURRENT templates/
    # folder. Cuts written by export_templates() are named
    # <soundscape>_<label>_<roi_type>_<template_id>.wav, which carries no
    # parameters to parse -- they are read from the template database beside them. Say so
    # instead of leaving the user to guess at a naming convention they should
    # never have to meet.
    template_db <- file.path(templates_path, "templates.duckdb")
    stop(
      "None of the WAV files in this directory match the legacy template ",
      "naming convention: ", templates_path,
      if (file.exists(template_db)) paste0(
        "\nThis folder holds a template database. Point `templates_path` at it ",
        "instead:\n  ", template_db
      ) else paste0(
        "\n`source = \"legacy_filename\"` (which a DIRECTORY path selects) is ",
        "only for importing old template sets whose parameters live in long ",
        "file names, like\n  ",
        "recording_012.500-014.750s_01.5-08.0kHz_512wl_75ovlp_Species name.wav",
        "\nTemplates cut by export_templates() are read from their ",
        "`templates.duckdb` template database -- pass that file as `templates_path`."
      )
    )
  }
  if (any(!ok)) {
    warning(sprintf(
      paste("%d file(s) do not match the legacy template naming convention",
            "and were skipped:\n%s"),
      sum(!ok), paste(basename(wavs[!ok]), collapse = "\n")
    ))
  }

  # FTM-04: read WAV headers, collecting unreadable files into a structured log.
  unreadable <- character(0)
  records <- lapply(parsed[ok], function(rec) {
    hdr <- tryCatch(
      as.data.frame(tuneR::readWave(rec$template_path, header = TRUE)),
      error = function(e) NULL
    )
    if (is.null(hdr)) {
      unreadable <<- c(unreadable, rec$template_path)
      return(NULL)
    }
    rec$template_sample_rate <- as.integer(hdr$sample.rate)
    rec
  })
  records <- records[!vapply(records, is.null, logical(1))]
  if (length(unreadable) > 0) {
    warning(sprintf("%d template WAV(s) could not be read (first: %s)",
                    length(unreadable), unreadable[1]))
  }

  # FTM-106: collapse any rows that share a template_id (the durable template database
  # key). The DuckDB upsert would later collapse them, but the in-memory legacy
  # frame must not carry duplicates into the template database write or a returned set.
  .dedupe_template_id(.coerce_templates(dplyr::bind_rows(records)))
}

#' Collapse template rows sharing a `template_id`, keeping the first (FTM-106).
#'
#' `template_id` is the durable template database key (its DuckDB PK). A duplicate would
#' make the in-memory frame disagree with what the template database stores after upsert.
#' Warns with the collapsed count. A defensive guard: the standard legacy parse
#' folds the source path into the id, so duplicates are not expected on that path.
#' @noRd
.dedupe_template_id <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return(df)
  dup <- duplicated(df$template_id)
  if (any(dup)) {
    warning(sprintf("%d duplicate template_id(s) collapsed (kept first)",
                    sum(dup)))
    df <- df[!dup, , drop = FALSE]
  }
  df
}

#' Parse a single legacy template file name into a partial template record.
#'
#' Legacy scheme:
#' `{soundscape}_{start}-{end}s_{min}-{max}kHz_{wl}wl_{ovlp}ovlp_{label}.wav`.
#' Returns `NULL` when the name does not carry the legacy markers.
#' @noRd
.parse_legacy_template_name <- function(path) {
  name <- tools::file_path_sans_ext(basename(path))
  # Width-agnostic anchored extraction (FTM-03).
  bounds <- regmatches(name, regexec(
    "_(\\d+\\.\\d+)-(\\d+\\.\\d+)s_", name))[[1]]
  freqs  <- regmatches(name, regexec(
    "_(\\d+\\.\\d+)-(\\d+\\.\\d+)kHz_", name))[[1]]
  wl     <- regmatches(name, regexec("_(\\d+)wl_", name))[[1]]
  ovlp   <- regmatches(name, regexec("_(\\d+)ovlp_", name))[[1]]
  # Label: everything after the last `ovlp_` (FTM-02, verbatim, keeps `_`).
  label  <- regmatches(name, regexec("ovlp_(.+)$", name))[[1]]
  if (length(bounds) < 3 || length(freqs) < 3 || length(wl) < 2 ||
      length(ovlp) < 2 || length(label) < 2) {
    return(NULL)
  }

  rec <- .schema_templates(1L)
  rec$template_id        <- .template_id(
    NA_character_, path, as.numeric(bounds[2]), as.numeric(bounds[3]),
    NA_character_, as.integer(wl[2]), as.integer(ovlp[2]), label[2]
  )
  rec$template_path      <- .to_forward_slashes(path)
  rec$template_file      <- basename(path)
  rec$template_label     <- label[2]
  rec$template_mode      <- "standalone_audio"
  rec$template_start     <- as.numeric(bounds[2])
  rec$template_end       <- as.numeric(bounds[3])
  rec$template_min_freq  <- as.numeric(freqs[2])
  rec$template_max_freq  <- as.numeric(freqs[3])
  rec$template_wl        <- as.integer(wl[2])
  rec$template_ovlp      <- as.integer(ovlp[2])
  rec
}

#' Resolve `reference_metadata` templates against the source soundscapes (FTM-104).
#'
#' For each reference row: locate the source via `origin_soundscape_path`, verify
#' its sha256 against `origin_soundscape_sha256`, and mark `template_resolved`.
#' `standalone_audio` rows are passed through as already resolved. The actual
#' cut-on-demand for matching is produced downstream (`template_matching`).
#' `template_resolved = TRUE` means the source was both **located AND verified**
#' against the recorded sha256 (AUD-31); a mismatch under
#' `on_hash_mismatch = "warn"` still proceeds (with a warning) but leaves
#' `template_resolved = FALSE`, so the diagnostic column tells the truth.
#' @noRd
.resolve_reference_templates <- function(df_templates, soundscapes,
                                         soundscapes_dir, on_hash_mismatch) {
  df_templates$template_resolved <- df_templates$template_mode == "standalone_audio"
  ref <- which(df_templates$template_mode == "reference_metadata")
  if (length(ref) == 0) return(df_templates)

  meta <- .resolve_soundscapes_arg(soundscapes)
  unresolved <- character(0)
  unverified <- character(0)

  for (i in ref) {
    src <- df_templates$origin_soundscape_path[i]
    if ((is.na(src) || !file.exists(src)) && !is.null(soundscapes_dir)) {
      cand <- file.path(soundscapes_dir, df_templates$origin_soundscape_file[i])
      if (file.exists(cand)) src <- cand
    }
    if (is.na(src) || !file.exists(src)) {
      unresolved <- c(unresolved, df_templates$template_id[i])
      df_templates$template_resolved[i] <- FALSE
      next
    }
    # Expected hash: from the template database, or from the soundscapes metadata join.
    expected <- df_templates$origin_soundscape_sha256[i]
    if (is.na(expected) && !is.null(meta)) {
      # AUD-32: equality-match on the AFL-06 canonical key (normalized on BOTH
      # sides) so a "./"/"//" cosmetic difference does not silently miss the join
      # and downgrade the template to the FTM-105 "unverified" path.
      j <- match(.normalize_path_key(src),
                 .normalize_path_key(meta$soundscape_path))
      if (!is.na(j)) expected <- meta$soundscape_sha256[j]
    }
    if (is.na(expected)) {
      # FTM-105: the source file is present but NO recorded sha256 exists to
      # verify it against (template database NA and no match in the soundscape metadata).
      # Resolve it (the file is there) but do not silently imply it is the exact
      # recorded version — collect it for an explicit "unverified" warning.
      unverified <- c(unverified, df_templates$template_id[i])
      df_templates$template_resolved[i] <- TRUE
      next
    }
    actual <- digest::digest(src, algo = "sha256", file = TRUE)
    if (!identical(expected, actual)) {
      msg <- sprintf("Source hash mismatch for template %s (source: %s)",
                     df_templates$template_id[i], src)
      if (on_hash_mismatch == "error") stop(msg)
      if (on_hash_mismatch == "warn")  warning(msg)
      # AUD-31: a mismatched source is NOT verified. resolved := located AND
      # verified, so "warn"/"skip" both leave it FALSE (only "error" stops).
      df_templates$template_resolved[i] <- FALSE
    } else {
      df_templates$template_resolved[i] <- TRUE
    }
  }

  if (length(unresolved) > 0) {
    warning(sprintf(
      "%d reference template(s) could not be resolved (source missing): %s",
      length(unresolved), paste(unresolved, collapse = ", ")
    ))
  }
  if (length(unverified) > 0) {
    warning(sprintf(
      paste("%d reference template(s) resolved WITHOUT hash verification",
            "(no recorded source sha256 to check against): %s"),
      length(unverified), paste(unverified, collapse = ", ")
    ))
  }
  df_templates
}

#' Accept a `df_soundscapes` data.frame or a soundscapes `.duckdb` path.
#' @noRd
.resolve_soundscapes_arg <- function(soundscapes) {
  if (is.null(soundscapes)) return(NULL)
  if (is.data.frame(soundscapes)) return(soundscapes)
  if (is.character(soundscapes) && length(soundscapes) == 1L &&
      file.exists(soundscapes)) {
    con <- .duckdb_connect(soundscapes)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    return(.duckdb_read_metadata(con))
  }
  stop("soundscapes must be a data.frame or a path to a .duckdb file")
}

# --- migration converter (FTM-101) -------------------------------------------

#' Convert an old template-WAV folder into a DuckDB template database
#'
#' @description Migrates an old template set (short WAV cuts whose parameters
#'   are encoded in long file names) into the standard DuckDB template database the
#'   pipeline expects. A one-off converter you run once per old folder: after
#'   it, [fetch_template_metadata()] reads the template database directly and never parses
#'   a file name again.
#'
#' @details
#'   The old naming scheme packs the time bounds, frequency band, window length,
#'   overlap and label into the file name. This function parses them
#'   width-agnostically (labels containing `_` survive unchanged) and writes a
#'   `templates` template database. Provenance a file name cannot carry
#'   (`origin_soundscape_sha256`, channel, user, timestamp) is left `NA`; to
#'   populate it, re-export the templates from ROIs with [export_templates()]
#'   instead of migrating. Set `recompute_sha256 = TRUE` to hash the existing
#'   WAVs into `template_sha256` for later integrity checks. This is an
#'   off-flow utility, not a numbered analysis step.
#'
#' @param templates_dir Directory of template WAVs named the old way
#'   (parameters in the file name, the format used before 1.2.0).
#' @param template_db_path Destination template `.duckdb` path.
#' @param recursive Logical, default `FALSE`. Recurse into sub-directories of
#'   `templates_dir`.
#' @param recompute_sha256 Logical, default `FALSE`. Hash each WAV into
#'   `template_sha256`. Enable it if you want content-integrity checks later; it
#'   reads every file, so it is slower.
#' @param overwrite Logical, default `FALSE`. Replace an existing template database at
#'   `template_db_path`. When `FALSE` and the file exists, the call errors rather
#'   than clobbering data.
#' @return Invisibly, the migrated `df_templates` data.frame (standard format).
#' @seealso [fetch_template_metadata()], [export_templates()].
#' @export
#' @examples
#' \dontrun{
#' # One-off: convert an old long-name template folder into a template database.
#' # (Old names carry the parameters; the package ships none, so this
#' # example writes one synthesized file in that format.)
#' old_dir <- file.path(tempdir(), "legacy_templates")
#' dir.create(old_dir, showWarnings = FALSE)
#' tone <- tuneR::normalize(tuneR::sine(4000, duration = 16000,
#'                                      samp.rate = 16000), unit = "16")
#' tuneR::writeWave(
#'   tone, file.path(old_dir, paste0(
#'     "siteA_20240116_001.000-002.000s_02.000-06.000kHz_512wl_50ovlp_burst.wav")))
#' template_db <- file.path(tempdir(), "templates.duckdb")
#' df_templates <- migrate_templates_to_db(
#'   old_dir, template database, overwrite = TRUE
#' )
#' nrow(df_templates)
#' # Downstream: read it back the routine way.
#' df_templates <- fetch_template_metadata(template database)
#' }
migrate_templates_to_db <- function(templates_dir, template_db_path,
                                          recursive = FALSE,
                                          recompute_sha256 = FALSE,
                                          overwrite = FALSE) {
  if (file.exists(template_db_path) && !overwrite) {
    stop("Destination template database exists; pass overwrite = TRUE to replace it.")
  }
  if (file.exists(template_db_path) && overwrite) unlink(template_db_path)

  df <- .read_templates_legacy(templates_dir, recursive)
  if (recompute_sha256) {
    df$template_sha256 <- vapply(df$template_path, function(p) {
      if (!is.na(p) && file.exists(p)) {
        digest::digest(p, algo = "sha256", file = TRUE)
      } else {
        NA_character_
      }
    }, character(1))
  }
  # AUD-33: this note fires on EVERY successful migration and reflects an
  # inherent limitation of legacy file names (nothing the user did wrong), so it
  # is a message(), not a warning() — an always-on warning in an exported
  # function is noise that trains users to ignore warnings (WARN-0 hygiene).
  message(sprintf(
    paste("Migrated %d legacy template(s). Source provenance",
          "(origin_soundscape_sha256, channel, user/timestamp) is unavailable",
          "from file names and left NA; re-export from ROIs to populate it."),
    nrow(df)))

  con <- .template_duckdb_connect(template_db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  .template_duckdb_upsert(con, df)
  invisible(df)
}

# --- catalog (FTM-103) -------------------------------------------------------

#' Summarise a template set as a table (and optional contact sheet)
#'
#' @description Produces a scannable overview of a template database **without
#'   opening each audio file**: a compact metadata table and, optionally, a
#'   spectrogram contact-sheet PNG with one thumbnail per template. A leaf
#'   reporting utility for inspecting a template set before matching; nothing
#'   downstream depends on its output.
#'
#' @details
#'   The returned table is a subset of the standard format (label, mode, bounds,
#'   band, sample rate, hash), enough to eyeball a template set at a glance.
#'   The
#'   optional PNG renders each template with [fast_spectro()]: `standalone_audio`
#'   templates show their cut, `reference_metadata` templates are cut on demand
#'   from the source recording. Templates whose audio cannot be located draw a
#'   placeholder panel rather than failing the whole sheet.
#'
#'   Use it as a sanity check after [export_templates()] or a migration. Things to
#'   know:
#'   (1) `thumbnails` defaults to `TRUE` only when `output` is set; pass an
#'   `output` path to get an image at all. (2) Rendering `reference_metadata`
#'   thumbnails needs the source recordings reachable; supply `soundscapes_dir`
#'   if their recorded `origin_soundscape_path` no longer resolves.
#'
#' @section Pipeline context:
#'   Off-flow inspection utility for step 4. Reads a template database
#'   `.duckdb` (as [fetch_template_metadata()] does). Produces a summary table
#'   and, optionally, a PNG contact sheet; no pipeline object. See also:
#'   \code{\link{fetch_template_metadata}}.
#'
#' @param templates_path A template database `.duckdb` path.
#' @param output Optional path for a PNG contact sheet. When `NULL` (default) no
#'   image is written and only the table is returned.
#' @param thumbnails Logical. Render spectrogram thumbnails into the contact
#'   sheet. Defaults to `TRUE` when `output` is set, `FALSE` otherwise; only has
#'   an effect together with `output`.
#' @param ncol Integer, default `4`. Thumbnails per row in the contact sheet.
#' @param soundscapes_dir Optional fallback directory of source recordings, used
#'   to locate the source for `reference_metadata` thumbnails when their recorded
#'   `origin_soundscape_path` is not reachable.
#' @param ... Passed through to [fast_spectro()] (e.g. window length, overlap).
#' @return Invisibly, the catalog table (a subset of the standard template
#'   format), one row per template.
#' @seealso [fetch_template_metadata()], [fast_spectro()].
#' @export
#' @examples
#' \dontrun{
#' # Inspect a template set at a glance (table only, no audio opened).
#' wav <- file.path(tempdir(), "rec_01.wav")
#' rec <- tuneR::normalize(tuneR::sine(4000, duration = 3 * 16000,
#'                                     samp.rate = 16000), unit = "16")
#' tuneR::writeWave(rec, wav)
#' df_rois <- data.frame(
#'   soundscape_path = wav, soundscape_file = "rec_01.wav",
#'   roi_label = "burst", roi_start = 0.5, roi_end = 1.0,
#'   roi_min_freq = 2, roi_max_freq = 6, roi_wl = 512, roi_ovlp = 50,
#'   stringsAsFactors = FALSE)
#' out_dir <- file.path(tempdir(), "templates")
#' export_templates(df_rois, templates_path = out_dir, create_dir = TRUE)
#' catalog <- catalog_templates(file.path(out_dir, "templates.duckdb"))
#' catalog[, c("template_label", "template_mode", "template_min_freq",
#'             "template_max_freq")]
#'
#' # Also write a spectrogram contact sheet:
#' catalog_templates(file.path(trio, "templates.duckdb"),
#'                   output = file.path(tempdir(), "templates.png"))
#' }
catalog_templates <- function(templates_path, output = NULL,
                              thumbnails = !is.null(output), ncol = 4,
                              soundscapes_dir = NULL, ...) {
  df <- fetch_template_metadata(templates_path, source = "duckdb")

  catalog <- df[, c("template_id", "template_label", "template_mode",
                    "origin_soundscape_file", "template_start", "template_end",
                    "template_min_freq", "template_max_freq", "template_channel",
                    "template_sample_rate", "template_sha256"), drop = FALSE]

  if (thumbnails && !is.null(output) && nrow(df) > 0) {
    .render_template_contact_sheet(df, output, ncol, soundscapes_dir, ...)
    message("Template contact sheet written to: ", output)
  }

  invisible(catalog)
}

#' Load a template's audio for a thumbnail (FTM-107).
#'
#' `standalone_audio` templates read their cut WAV directly. `reference_metadata`
#' templates have no cut on disk (`template_path` is `NA`); they are cut on demand
#' from the source recording — located via `origin_soundscape_path` (or, as a
#' fallback, `soundscapes_dir` + `origin_soundscape_file`), sliced to
#' `[template_start, template_end]` and reduced to the recorded channel (reuses
#' [.select_channel_mono()] from `export_templates.R`). Returns `NULL` when the
#' audio cannot be obtained (caller then draws a placeholder). No hash
#' verification here — that is `fetch_template_metadata(resolve_references=TRUE)`.
#' @noRd
.load_template_audio <- function(rec, soundscapes_dir = NULL) {
  # standalone_audio: the cut is on disk.
  if (!is.na(rec$template_path) && file.exists(rec$template_path)) {
    return(tryCatch(tuneR::readWave(rec$template_path), error = function(e) NULL))
  }
  # reference_metadata: cut on demand from the source.
  if (!identical(rec$template_mode, "reference_metadata")) return(NULL)
  src <- rec$origin_soundscape_path
  if ((is.na(src) || !file.exists(src)) && !is.null(soundscapes_dir) &&
      !is.na(rec$origin_soundscape_file)) {
    cand <- file.path(soundscapes_dir, rec$origin_soundscape_file)
    if (file.exists(cand)) src <- cand
  }
  if (is.na(src) || !file.exists(src) ||
      is.na(rec$template_start) || is.na(rec$template_end)) {
    return(NULL)
  }
  tryCatch({
    w <- tuneR::readWave(src, from = rec$template_start, to = rec$template_end,
                         units = "seconds")
    .select_channel_mono(
      w, if (is.na(rec$template_channel)) "left" else rec$template_channel)
  }, error = function(e) NULL)
}

#' Render a spectrogram contact-sheet PNG for a template set.
#'
#' Builds one [fast_spectro()] thumbnail (a ggplot) per template, titles each with
#' the label + id, and assembles them into an `ncol`-wide grid via `patchwork`,
#' saved to `output`. `standalone_audio` templates render their cut;
#' `reference_metadata` templates are cut on demand from their source (FTM-107,
#' via [.load_template_audio()]). Templates whose audio cannot be obtained get a
#' placeholder panel. (`fast_spectro()` returns a ggplot, so the sheet is composed
#' in the ggplot ecosystem, not base graphics.)
#' @noRd
.render_template_contact_sheet <- function(df, output, ncol,
                                           soundscapes_dir = NULL, ...) {
  panels <- lapply(seq_len(nrow(df)), function(i) {
    title <- sprintf("%s  (%s)", df$template_label[i], df$template_id[i])
    wav <- .load_template_audio(df[i, , drop = FALSE], soundscapes_dir)
    if (!is.null(wav)) {
      p <- tryCatch(
        fast_spectro(wav, ...) + ggplot2::ggtitle(title),
        error = function(e) NULL)
      if (!is.null(p)) return(p)
    }
    # placeholder for unavailable audio (e.g. missing reference source)
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0, y = 0, label = "audio unavailable") +
      ggplot2::ggtitle(title) + ggplot2::theme_void()
  })
  sheet <- patchwork::wrap_plots(panels, ncol = ncol)
  nrow_grid <- ceiling(nrow(df) / ncol)
  ggplot2::ggsave(output, sheet, width = ncol * 3.2,
                  height = nrow_grid * 2.4, dpi = 150, limitsize = FALSE)
  invisible(NULL)
}
