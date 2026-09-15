#' Build the template × soundscape search grid
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Crosses every soundscape with every template into one row per match, each
#'   carrying the complete metadata of both sources. Pairs that cannot be
#'   matched (incomplete metadata, unresolvable files, incompatible frequency
#'   bands or channels) are dropped from the grid. The result is the search
#'   grid given to the matching engine: one scored comparison per row.
#'
#' @details
#'   The grid is a full cartesian product of the two inputs, filtered in a fixed
#'   order. Rows are removed for four reasons, and the count for each is reported
#'   in the `"dropped"` attribute of the result (each count is *after* the earlier
#'   filters, so together they account for every excluded row):
#'   \itemize{
#'     \item **incomplete metadata**: an `NA` in any compared column;
#'     \item **missing source**: the template's audio cut (or, for a reference
#'       template, its source recording) cannot be found on disk;
#'     \item **integrity mismatch**: only when `verify_integrity = TRUE`;
#'     \item **channel incompatibility**: only when `channel_policy = "strict"`.
#'   }
#'   Run it right after metadata collection, before any expensive matching, to
#'   inspect exactly which pairs will be scored and why the others were
#'   excluded. Malformed template metadata, such as an inverted band
#'   (`template_min_freq > template_max_freq`), a non-positive duration
#'   (`template_start >= template_end`) or a non-numeric compared column,
#'   stops the function with an error naming the rows that hold the bad data.
#'   So an empty grid always means a genuine "no compatible pairs"; it never
#'   hides a data error.
#'
#' @section Pipeline context:
#'   Step 6 of the monitoraSom analysis flow. Reads soundscape metadata from
#'   [fetch_soundscape_metadata()] (step 5) and template metadata from
#'   [fetch_template_metadata()] (step 4). Produces the search grid that
#'   [run_matching()] (step 7) scores, one row per pair.
#'
#' @param soundscape_data data.frame of soundscapes, as returned by
#'   [fetch_soundscape_metadata()]. `NULL` (default) reads the standard
#'   soundscape metadata cache `soundscapes/soundscapes_metadata.duckdb` and
#'   stops if it is absent.
#'   Required columns: `soundscape_path`,
#'   `soundscape_sample_rate`, `soundscape_duration`. Any extra columns are
#'   carried through to the grid.
#' @param template_data data.frame of templates, as returned by
#'   [fetch_template_metadata()]. `NULL` (default) falls back to the standard
#'   templates folder `templates/` and stops if it holds no template.
#'   Required columns: `template_path`,
#'   `template_sample_rate`, `template_min_freq` and `template_max_freq` (kHz),
#'   `template_start` and `template_end` (s). Optional `template_mode` and
#'   `*_sha256` columns are used when present. Extra columns are carried through
#'   (an internal `template_resolved` column, if present, is dropped).
#' @param channel_policy How stereo/multichannel layouts are handled.
#'   `"permissive"` (default) carries the channel columns and never excludes a
#'   pair; the engine reads channel 1 (left) of every soundscape.
#'   `"strict_left"` drops pairs whose template side is `"right"` (the engine
#'   would not analyze that channel). `"strict_right"` is the mirror: it drops
#'   pairs whose template side is `"left"`. A `"mono"` template or soundscape is
#'   always kept. `"per_channel"` scores a soundscape-template pair once per
#'   soundscape channel: for a stereo soundscape each pair becomes two rows
#'   (one per channel, recorded in `soundscape_channel`); mono soundscapes keep
#'   a single row. No effect when channel columns are absent.
#' @param verify_integrity Single logical, default `FALSE`. When `TRUE`, hash
#'   the referenced files again and drop pairs whose sha256 differs from the
#'   recorded hash columns. Off by default because hashing is costly; when
#'   `TRUE` but no `*_sha256` column is present, nothing is verified and a
#'   warning is emitted.
#'
#' @return A data.frame with one row per compatible soundscape-template pair,
#'   keeping all columns of both inputs. It carries a `"dropped"` attribute: a
#'   named integer vector of per-reason exclusion counts. When a VIEW is
#'   requested, it is created as a side effect and the same data.frame is returned.
#'
#' @seealso [fetch_soundscape_metadata()], [fetch_template_metadata()],
#'   [run_matching()]
#' @export
#' @examples
#' # Step 6 of the analysis flow: cross soundscape metadata (step 5) with
#' # template metadata (step 4) to produce the search grid that run_matching
#' # (step 7) scores: one row per soundscape-template pair.
#' \dontrun{
#' # Load the package
#' library(monitoraSom)
#' # (The package ships no field recordings, so this example synthesizes them;
#' # for the same steps on real data see [fetch_example_data()].)
#' rec_dir <- file.path(tempdir(), "recs"); dir.create(rec_dir, showWarnings = FALSE)
#' rec <- tuneR::normalize(tuneR::sine(4000, duration = 10 * 16000,
#'                                     samp.rate = 16000), unit = "16")
#' for (f in c("siteA_01.wav", "siteA_02.wav"))
#'   tuneR::writeWave(rec, file.path(rec_dir, f))
#' df_soundscapes <- fetch_soundscape_metadata(rec_dir)
#'
#' wav <- file.path(rec_dir, "siteA_01.wav")
#' df_rois <- data.frame(
#'   soundscape_path = wav, soundscape_file = "siteA_01.wav",
#'   roi_label = "burst", roi_start = 1, roi_end = 1.5,
#'   roi_min_freq = 2, roi_max_freq = 6, roi_wl = 512, roi_ovlp = 50,
#'   stringsAsFactors = FALSE)
#' str(df_rois)
#'
#' out_dir <- file.path(tempdir(), "templates")
#' export_templates(df_rois, templates_path = out_dir, create_dir = TRUE)
#' df_templates <- fetch_template_metadata(out_dir)
#' str(df_templates)
#'
#' df_grid <- fetch_match_grid(df_soundscapes, df_templates)
#' str(df_grid)                 # one row per soundscape-template pair
#' attr(df_grid, "dropped")      # why any pairs were excluded
#'
#' # 'strict' channel policy also drops pairs whose channel layouts disagree:
#' fetch_match_grid(df_soundscapes, df_templates, channel_policy = "strict")
#' }
fetch_match_grid <- function(soundscape_data = NULL, template_data = NULL,
                             channel_policy = "permissive",
                             verify_integrity = FALSE) {

  channel_policy <- match.arg(channel_policy, c("permissive", "strict_left",
                                                "strict_right", "per_channel"))

  # CRAN item 7 (F3): NULL inputs fall back to the canonical stores — the
  # templates template database/folder (fetch_template_metadata's default) and the
  # soundscape metadata cache (soundscapes/soundscapes_metadata.duckdb). Each
  # resolver stops with an actionable message when its store is absent. No
  # fallback auto-creates or scans anything: a silent WAV scan would be far
  # too expensive for a fallback path.
  if (is.null(template_data)) {
    template_data <- fetch_template_metadata()
  }
  if (is.null(soundscape_data)) {
    ss_cache <- .monitora_db_default_path("soundscapes_metadata")
    if (!file.exists(ss_cache)) {
      stop("No 'soundscape_data' provided and the default soundscape cache ",
           "does not exist: ", ss_cache, ". Export it with ",
           "fetch_soundscape_metadata(output_file = \"", ss_cache,
           "\") or pass the data explicitly.")
    }
    soundscape_data <- .read_soundscape_table(ss_cache)
  }

  .validate_grid_inputs(soundscape_data, template_data)
  .validate_grid_values(soundscape_data, template_data)

  # AUD-03: verify_integrity must be a non-NA scalar logical (silent "off" on NA
  # is the worst failure mode for a data-integrity gate).
  if (!is.logical(verify_integrity) || length(verify_integrity) != 1L ||
      is.na(verify_integrity)) {
    stop("verify_integrity must be a single logical value (TRUE or FALSE), not ",
         deparse(verify_integrity))
  }

  # AFL-27: `template_resolved` is fetch_template_metadata's fetch-time resolution
  # diagnostic, not a matching dimension — keep it out of the cross-joined grid.
  template_data$template_resolved <- NULL
  res <- dplyr::cross_join(soundscape_data, template_data)

  dropped <- integer(0)
  steps <- list(
    .filter_incomplete, .filter_missing_files,
    function(r) .filter_acoustic(r),
    function(r) .filter_channel(r, channel_policy),
    function(r) .filter_integrity(r, verify_integrity)
  )
  for (step in steps) {
    out <- step(res)
    res <- out$res
    dropped <- c(dropped, out$dropped)
  }

  if (length(dropped) > 0) {
    warning(paste(.format_grid_drops(dropped), collapse = "\n"))
  } else {
    message("All files are compatible and included in the matching grid.")
  }

  rownames(res) <- NULL
  attr(res, "dropped") <- dropped
  res
}

# --- Private helpers ---------------------------------------------------------

# Required (minimal) columns the compatibility logic reads. Optional columns
# (template_mode, *_channel, *_sha256) are used only when present.
.grid_required_cols <- function() {
  list(
    soundscape = c("soundscape_path", "soundscape_sample_rate",
                   "soundscape_duration"),
    template   = c("template_path", "template_sample_rate", "template_min_freq",
                   "template_max_freq", "template_start", "template_end")
  )
}

# FMG-04: required-column + disjoint-names validation. cross_join would silently
# suffix a name clash; the trio's origin_ prefix keeps them disjoint today.
.validate_grid_inputs <- function(soundscape_data, template_data) {
  req <- .grid_required_cols()
  miss_s <- setdiff(req$soundscape, names(soundscape_data))
  miss_t <- setdiff(req$template, names(template_data))
  if (length(miss_s) > 0) {
    stop("soundscape_data is missing required column(s): ",
         paste(miss_s, collapse = ", "))
  }
  if (length(miss_t) > 0) {
    stop("template_data is missing required column(s): ",
         paste(miss_t, collapse = ", "))
  }
  dup <- intersect(names(soundscape_data), names(template_data))
  if (length(dup) > 0) {
    stop("soundscape_data and template_data share column name(s): ",
         paste(dup, collapse = ", "),
         ". Disjoint names are required (template provenance must use the ",
         "origin_ prefix).")
  }
  invisible(TRUE)
}

# AFL-01 (FMG-106/107/108): value + type validation, run before the cartesian
# join. Unlike the soft per-pair drops below, malformed template metadata is a
# caller error (not an incompatible pair), so it stops with the offending rows
# instead of silently clearing the isolated Nyquist/duration filters. NA values
# are left to the incomplete-metadata drop (FMG-01).
.validate_grid_values <- function(soundscape_data, template_data) {
  ss_num <- c("soundscape_sample_rate", "soundscape_duration")
  tm_num <- c("template_sample_rate", "template_min_freq", "template_max_freq",
              "template_start", "template_end")
  ss_bad <- ss_num[!vapply(ss_num,
                           function(col) is.numeric(soundscape_data[[col]]),
                           logical(1))]
  tm_bad <- tm_num[!vapply(tm_num,
                           function(col) is.numeric(template_data[[col]]),
                           logical(1))]
  bad_type <- c(if (length(ss_bad)) paste0("soundscape_data$", ss_bad),
                if (length(tm_bad)) paste0("template_data$", tm_bad))
  if (length(bad_type) > 0) {
    stop("fetch_match_grid: non-numeric column(s) where numeric is required: ",
         paste(bad_type, collapse = ", "),
         ". Coerce them to numeric before building the grid.")
  }
  inv <- !is.na(template_data$template_min_freq) &
         !is.na(template_data$template_max_freq) &
         template_data$template_min_freq > template_data$template_max_freq
  if (any(inv)) {
    stop("fetch_match_grid: ", sum(inv),
         " template(s) with an inverted frequency band ",
         "(template_min_freq > template_max_freq): ",
         .grid_row_ids(template_data, inv), ".")
  }
  npd <- !is.na(template_data$template_start) &
         !is.na(template_data$template_end) &
         template_data$template_start >= template_data$template_end
  if (any(npd)) {
    stop("fetch_match_grid: ", sum(npd),
         " template(s) with a non-positive duration ",
         "(template_start >= template_end): ",
         .grid_row_ids(template_data, npd), ".")
  }
  # AFL-07: a reference_metadata template is cut on demand from its source
  # recording, so it MUST carry an `origin_soundscape_path` (TM-12). A missing
  # column or NA value is a structurally broken template spec (caller error),
  # not an incompatible pair; reject it here instead of only deep in
  # run_matching_i after the soundscape I/O. NB: a *present, non-NA* origin path
  # pointing at a missing file stays a soft per-pair drop (FMG-02).
  if ("template_mode" %in% names(template_data)) {
    is_ref <- !is.na(template_data$template_mode) &
              template_data$template_mode == "reference_metadata"
    miss_origin <- if ("origin_soundscape_path" %in% names(template_data)) {
      is_ref & is.na(template_data$origin_soundscape_path)
    } else {
      is_ref
    }
    if (any(miss_origin)) {
      stop("fetch_match_grid: ", sum(miss_origin),
           " reference_metadata template(s) without `origin_soundscape_path` ",
           "(cannot resolve the source recording to cut the template): ",
           .grid_row_ids(template_data, miss_origin), ".")
    }
  }
  invisible(TRUE)
}

# Compact identifier for the offending template rows in a validation error.
.grid_row_ids <- function(template_data, bad) {
  idx <- which(bad)
  ids <- if ("template_path" %in% names(template_data)) {
    basename(as.character(template_data$template_path[idx]))
  } else {
    paste0("row ", idx)
  }
  paste(ids, collapse = ", ")
}

# FMG-03: existence tested once per unique path, mapped back to every row.
.paths_exist <- function(paths) {
  u <- unique(paths)
  file.exists(u)[match(paths, u)]
}

# Drop `pred` rows from `res`, returning the new res + a named count. NA in the
# predicate is treated as "drop" defensively (FMG-01 removes NAs up front, so
# this is belt-and-suspenders and never indexes with NA — FMG-09).
.grid_drop <- function(res, pred, reason) {
  pred <- pred & !is.na(pred)
  n <- sum(pred)
  cnt <- stats::setNames(as.integer(n), reason)
  list(res = res[!pred, , drop = FALSE], dropped = if (n > 0) cnt else integer(0))
}

# Step 0 (FMG-01): drop rows with NA in any compared column — these would have
# crashed `if (any(<NA>))` in the original.
.filter_incomplete <- function(res) {
  cols <- c("soundscape_sample_rate", "soundscape_duration",
            "template_sample_rate", "template_min_freq", "template_max_freq",
            "template_start", "template_end")
  incomplete <- Reduce(`|`, lapply(cols, function(c) is.na(res[[c]])))
  .grid_drop(res, incomplete, "incomplete_metadata")
}

# Steps 1-4: sequential missing-file and mode-based filters (FMG-02, FMG-07).
# AUD-01: split into self-contained helpers so every mask is recomputed from
# the step's own current res — fixes the stale `cache` positional truncation
# that let a spectrogram_cache row survive when a preceding row is soft-dropped.
.filter_missing_files <- function(res) {
  out1 <- .filter_missing_soundscape(res)
  out2 <- .filter_missing_template(out1$res)
  out3 <- .filter_unresolvable_reference(out2$res)
  out4 <- .filter_cache_unsupported(out3$res)
  list(res = out4$res,
       dropped = c(out1$dropped, out2$dropped, out3$dropped, out4$dropped))
}

.filter_missing_soundscape <- function(res) {
  .grid_drop(res, !.paths_exist(res$soundscape_path), "missing_soundscape_file")
}

.filter_missing_template <- function(res) {
  mode <- if ("template_mode" %in% names(res)) res$template_mode else
    rep("standalone_audio", nrow(res))
  std <- is.na(mode) | mode == "standalone_audio"
  miss <- std & !.paths_exist(res$template_path)
  .grid_drop(res, miss, "missing_template_file")
}

# AFL-07 + AUD-01: reference_metadata rows whose origin_soundscape_path is
# missing or unresolvable are dropped. Mask is always recomputed from the
# current res (no stale-variable carry from a previous step).
.filter_unresolvable_reference <- function(res) {
  mode <- if ("template_mode" %in% names(res)) res$template_mode else
    rep("standalone_audio", nrow(res))
  ref <- mode == "reference_metadata"
  if (!any(ref)) return(list(res = res, dropped = integer(0)))
  miss <- if ("origin_soundscape_path" %in% names(res)) {
    ref & !.paths_exist(res$origin_soundscape_path)
  } else {
    ref
  }
  .grid_drop(res, miss, "unresolvable_reference_template")
}

# AFL-30 + AUD-01: `spectrogram_cache` is an EXPERIMENTAL, not-yet-implemented
# mode (ERC-106, adiado). It is not on any public surface — `export_templates()`'s
# `match.arg` cannot produce it — so a row only reaches here via a hand-crafted
# template database. Handled defensively (dropped with a clear reason) rather than removed,
# so such a row fails loudly instead of being matched as a stray template.
.filter_cache_unsupported <- function(res) {
  mode <- if ("template_mode" %in% names(res)) res$template_mode else
    rep("standalone_audio", nrow(res))
  cache <- mode == "spectrogram_cache"
  .grid_drop(res, cache, "spectrogram_cache_unsupported")
}

# Steps 3-6 (FMG-09 fix): sample-rate, min/max freq vs Nyquist (recomputed on
# the current rows each time), then template duration vs soundscape duration.
.filter_acoustic <- function(res) {
  o_sr <- .grid_drop(res, res$soundscape_sample_rate != res$template_sample_rate,
                     "incompatible_sample_rate")
  res <- o_sr$res
  o_min <- .grid_drop(res,
    res$template_min_freq * 1000 >= res$soundscape_sample_rate / 2,
    "min_freq_above_nyquist")
  res <- o_min$res
  o_max <- .grid_drop(res,
    res$template_max_freq * 1000 >= res$soundscape_sample_rate / 2,
    "max_freq_above_nyquist")
  res <- o_max$res
  o_dur <- .grid_drop(res,
    (res$template_end - res$template_start) >= res$soundscape_duration,
    "template_longer_than_soundscape")
  list(res = o_dur$res,
       dropped = c(o_sr$dropped, o_min$dropped, o_max$dropped, o_dur$dropped))
}

# Step 7 (FMG-103, AUD-02 option B): channel compatibility — matcher-aware.
# The engine analyzes the channel the row names. `strict_left` drops a pair
# whose template side is "right" (the engine would analyze the left channel);
# `strict_right` is the mirror. A "mono" template or soundscape is always kept.
# `per_channel` duplicates a pair for each soundscape channel (stereo -> 2 rows,
# one per channel recorded in `soundscape_channel`); the engine then reads the
# named channel. No-op when channel columns are absent.
.filter_channel <- function(res, channel_policy) {
  have <- all(c("soundscape_channel", "template_channel") %in% names(res))
  if (!have || nrow(res) == 0) {
    return(list(res = res, dropped = integer(0)))
  }
  if (channel_policy == "per_channel") {
    return(.expand_per_channel(res))
  }
  if (channel_policy == "permissive") {
    return(list(res = res, dropped = integer(0)))
  }
  sc <- res$soundscape_channel
  tc <- res$template_channel
  # Mono on either side is always compatible. Otherwise the template side must
  # match the strict policy's analyzed channel.
  analyzed <- if (channel_policy == "strict_left") "left" else "right"
  incompatible <- !is.na(sc) & !is.na(tc) &
    sc != "mono" & tc != "mono" & tc != analyzed
  .grid_drop(res, incompatible, "channel_not_analyzed")
}

# DEC-29 (`per_channel`): duplicate each pair once per soundscape channel. A
# stereo soundscape yields two rows ("left"/"right" in soundscape_channel); a
# mono soundscape keeps one row. A row with no channel info keeps one row
# unchanged.
.expand_per_channel <- function(res) {
  n <- nrow(res)
  if (n == 0L) return(list(res = res, dropped = integer(0)))
  ch_list <- lapply(seq_len(n), function(i) {
    sc <- res$soundscape_channel[i]
    if (is.na(sc) || sc == "mono") {
      "mono"
    } else if (sc == "stereo") {
      c("left", "right")
    } else {
      "left"     # multichannel without side info: analyze channel 1
    }
  })
  idx <- rep(seq_len(n), vapply(ch_list, length, integer(1)))
  out <- res[idx, , drop = FALSE]
  out$soundscape_channel <- unlist(ch_list, use.names = FALSE)
  list(res = out, dropped = integer(0))
}

# Step 8 (FMG-102): opt-in sha256 integrity gate. Re-hash referenced files and
# drop pairs whose hash diverges from the recorded value.
.filter_integrity <- function(res, verify_integrity) {
  if (!isTRUE(verify_integrity)) {
    return(list(res = res, dropped = integer(0)))
  }
  # AFL-05: without any recorded *_sha256 column the gate verifies nothing; warn
  # instead of silently passing every pair (false sense of verification).
  hash_cols <- c("soundscape_sha256", "template_sha256",
                 "origin_soundscape_sha256")
  if (!any(hash_cols %in% names(res))) {
    warning("verify_integrity = TRUE but no *_sha256 column is present; ",
            "no integrity check was performed.")
    return(list(res = res, dropped = integer(0)))
  }
  if (nrow(res) == 0) {
    return(list(res = res, dropped = integer(0)))
  }
  bad <- rep(FALSE, nrow(res))
  bad <- bad | .hash_mismatch(res, "soundscape_path", "soundscape_sha256")
  bad <- bad | .hash_mismatch(res, "template_path", "template_sha256")
  bad <- bad | .hash_mismatch(res, "origin_soundscape_path",
                              "origin_soundscape_sha256")
  .grid_drop(res, bad, "sha256_mismatch")
}

# TRUE where the file's current sha256 differs from the recorded column. Pairs
# with no path/hash recorded (NA) are treated as compatible (not verifiable).
.hash_mismatch <- function(res, path_col, hash_col) {
  if (!all(c(path_col, hash_col) %in% names(res))) return(rep(FALSE, nrow(res)))
  paths <- res[[path_col]]
  recorded <- res[[hash_col]]
  u <- unique(paths[!is.na(paths)])
  current <- stats::setNames(
    vapply(u, function(p) {
      if (file.exists(p)) {
        digest::digest(p, algo = "sha256", file = TRUE)
      } else {
        NA_character_
      }
    }, character(1)), u)
  got <- unname(current[match(paths, names(current))])
  !is.na(recorded) & !is.na(got) & got != recorded
}

# Clarified, behaviour-preserving warning text (FMG-07): each count is reported
# as conditional on the previous filters.
.format_grid_drops <- function(dropped) {
  labels <- c(
    incomplete_metadata = paste0(
      "%d crossing(s) had incomplete metadata ",
      "(NA in a compared column)"
    ),
    missing_soundscape_file = paste0(
      "%d crossing(s) whose soundscape file ",
      "does not exist"
    ),
    missing_template_file = "%d crossing(s) whose template file does not exist",
    unresolvable_reference_template = paste0(
      "%d reference_metadata crossing(s) whose ",
      "source recording is unavailable"
    ),
    spectrogram_cache_unsupported = paste0(
      "%d spectrogram_cache crossing(s) are not ",
      "yet supported (ERC-106)"
    ),
    incompatible_sample_rate = "%d crossing(s) with incompatible sample rates",
    min_freq_above_nyquist = paste0(
      "%d crossing(s) whose template minimum frequency ",
      "is >= the Nyquist frequency"
    ),
    max_freq_above_nyquist = paste0(
      "%d crossing(s) whose template maximum frequency ",
      "is >= the Nyquist frequency"
    ),
    template_longer_than_soundscape = paste0(
      "%d crossing(s) whose template is at least as long as the soundscape"
    ),
    channel_not_analyzed = paste0(
      "%d crossing(s) whose template channel is not analyzed ",
      "by the matching engine"
    ),
    sha256_mismatch = "%d crossing(s) failed the sha256 integrity check"
  )
  msgs <- vapply(names(dropped), function(k) sprintf(labels[[k]], dropped[[k]]),
                 character(1))
  c(msgs, "(counts are sequential: each is after the previous filters.)")
}
