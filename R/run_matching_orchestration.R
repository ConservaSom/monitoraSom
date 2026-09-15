#' Batch orchestration for run_matching (Stage 4 gate §2.1 C2 + RMB-02/RMC-10)
#'
#' @description Replaces the original flat S*T `pblapply` (which re-read and
#'   re-spectrogrammed every soundscape and template on **every** iteration) with
#'   a grouped traversal: rows sharing a soundscape **and** its spec parameters
#'   form a group whose soundscape spectrogram is built **once** and reused across
#'   the group's templates (RMC-10 cache, realised per group). Groups are mapped
#'   with the project's cross-platform [.par_map()] (`mirai` -> mclapply -> serial,
#'   FSM-212), distributing by **soundscape group** so each worker reads its own
#'   soundscape from disk (little data on the wire). Workers compute **scores**
#'   only (the expensive part); peak detection and persistence happen in the
#'   single main process (no concurrent writers — dissolves RMB-06).
#'
#' @keywords internal
#' @noRd

# Source files mirai daemons must load to run a score group (RMB-02 worker
# bootstrap). They cover the engines + dispatcher + this file + `_detect_peaks.R`
# (the dispatcher calls the shared `.validate_score_filters` from it). The score
# path never calls fetch_score_peaks_i (peak detection runs in the main process),
# so the capture file itself is not needed on workers.
.RUN_MATCHING_WORKER_SOURCES <- c(
  "_detect_peaks.R", "_run_matching_helpers.R", "_run_matching_engines.R",
  "run_matching_i.R", "_run_matching_orchestration.R"
)

# Split df_grid into per-soundscape groups (each soundscape WAV read once across
# its templates/specs — C1.2 "read each file once"). One worker handles one group.
.match_soundscape_groups <- function(df_grid) {
  key <- df_grid$soundscape_path
  unname(split(df_grid, factor(key, levels = unique(key))))
}

# Preload every UNIQUE template's spectrogram ONCE (C1.2 "templates preloaded";
# templates are small cuts). Keyed by [.template_cache_key()] (path + slice,
# TM-12: reference templates can share one origin recording); the read target
# and tlim come from [.template_read_spec()], which resolves the canonical
# source-relative bounds per template_mode. Returns a named list of {mat, time}.
.preload_template_spectros <- function(df_grid) {
  keys <- vapply(seq_len(nrow(df_grid)), function(i) {
    .template_cache_key(df_grid[i, ])
  }, character(1))
  idx <- !duplicated(keys)
  uniq <- df_grid[idx, , drop = FALSE]
  cache <- lapply(seq_len(nrow(uniq)), function(i) {
    r <- uniq[i, ]
    tspec <- .template_read_spec(r)
    .build_spectro_matrix(
      tspec$path, .spec_params_from_row(r, side = "template"),
      tlim = tspec$tlim, which_file = "template"
    )
  })
  stats::setNames(cache, keys[idx])
}

# Score every row of ONE soundscape group. The soundscape WAV is read once; its
# spectrogram is memoized per spec (rows may carry different wl/ovlp/flim). C1.2:
# soundscapes <= `stream_threshold_s` are PRELOADED (audio held, reused across
# specs); longer ones are STREAMED (audio re-read + freed per spec, capping peak
# memory). Templates come from the preloaded cache. Returns rbind of scores rows.
.run_score_group <- function(rows, score_method, dtw_slack, stream_threshold_s,
                             template_cache) {
  ss_path <- rows[[1, "soundscape_path"]]

  # AUD-39: contain failures so one bad pair/soundscape is reported and skipped
  # instead of aborting the whole run_matching sweep (report-don't-drop posture,
  # FSM-204 shape). `failures` collects {soundscape_path, template_id, message}.
  failures <- list()
  add_failure <- function(row_i, msg) {
    failures[[length(failures) + 1L]] <<- data.frame(
      soundscape_path = ss_path,
      template_id = if ("template_id" %in% names(row_i))
        as.character(row_i$template_id) else NA_character_,
      message = msg, stringsAsFactors = FALSE)
  }

  # A group-level soundscape read failure (a WAV that went unreadable between
  # grid-build and matching) fails only this group's pairs, not the run.
  ss_read <- tryCatch({
    preload <- .wav_duration_s(ss_path) <= stream_threshold_s
    list(preload = preload,
         wav = if (preload) tuneR::readWave(ss_path) else NULL)
  }, error = function(e) e)
  if (inherits(ss_read, "error")) {
    for (i in seq_len(nrow(rows))) add_failure(rows[i, ], conditionMessage(ss_read))
    return(list(scores = NULL, failures = dplyr::bind_rows(failures)))
  }
  preload <- ss_read$preload
  wav <- ss_read$wav
  ss_cache <- new.env(parent = emptyenv())

  per_row <- lapply(seq_len(nrow(rows)), function(i) {
    row_i <- rows[i, ]
    # Per-pair containment (an RMC-13 short pair, digital silence under fft, ...).
    tryCatch({
      sp <- .spec_params_from_row(row_i, side = "soundscape")
      key <- .spec_key(row_i)
      if (!exists(key, envir = ss_cache, inherits = FALSE)) {
        ss <- if (preload) .build_spectro_from_wave(wav, sp, which_file = "soundscape")
              else .build_spectro_matrix(ss_path, sp, which_file = "soundscape")
        assign(key, ss, envir = ss_cache)
      }
      ss <- get(key, envir = ss_cache, inherits = FALSE)
      tp <- template_cache[[.template_cache_key(row_i)]]
      .assert_match_matrices(ss$mat, tp$mat)
      run_matching_i(
        row_i, score_method = score_method, output = "scores",
        dtw_slack = dtw_slack, mat_soundscape = ss$mat, mat_template = tp$mat,
        soundscape_time = ss$time
      )
    }, error = function(e) {
      add_failure(row_i, conditionMessage(e))
      NULL
    })
  })
  list(scores = dplyr::bind_rows(per_row),
       failures = if (length(failures)) dplyr::bind_rows(failures) else NULL)
}

# Parallel scores over all soundscape groups (RMB-02/RMB-08, C1.2/C2). Templates
# are preloaded once in the main process and shipped into each group call.
# Order-preserving across groups.
.run_matching_scores <- function(df_grid, score_method, ncores, dtw_slack,
                                  stream_threshold_s, pb = FALSE) {
  groups <- .match_soundscape_groups(df_grid)
  template_cache <- .preload_template_spectros(df_grid)
  res <- .par_map(
    groups,
    function(g) {
      .run_score_group(g, score_method, dtw_slack,
                       stream_threshold_s, template_cache)
    },
    ncores = ncores, worker_setup = .RUN_MATCHING_WORKER_SOURCES, pb = pb
  )
  scores <- dplyr::bind_rows(lapply(res, `[[`, "scores"))
  # AUD-39: surface contained per-pair failures as one structured warning plus a
  # `failed_pairs` attribute on the scores; successful pairs are kept.
  failures <- dplyr::bind_rows(lapply(res, `[[`, "failures"))
  if (!is.null(failures) && nrow(failures) > 0L) {
    warning(sprintf(
      "run_matching: %d pair(s) failed and were skipped (first: template %s on %s -- %s).",
      nrow(failures), failures$template_id[1L],
      basename(failures$soundscape_path[1L]), failures$message[1L]))
    attr(scores, "failed_pairs") <- failures
  }
  scores
}

# Peak-detection over a scores tibble (main process). Delegates to the SHARED
# peak-detection batch routine (FSPB-08 option A) — pure per-pair capture via
# fetch_score_peaks_i() (which already emits the canonical schema with
# template_id, score_method and the durable detection_id) bound over the canonical
# schema, then the composable filter (FSPB-10). This replaces the former ad-hoc
# mapping so `fetch_score_peaks()` and the orchestrator can never diverge.
.scores_to_detections <- function(scores_tbl, buffer_size, min_score,
                                   min_quant, top_n, scope = "pair") {
  det <- .detect_peaks_batch(scores_tbl, buffer_size)
  filter_detections_i(det, min_score = min_score, min_quant = min_quant,
                    top_n = top_n, scope = scope)
}
