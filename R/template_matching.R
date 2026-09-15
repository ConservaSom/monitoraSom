#' Run the whole template-matching pipeline in one call
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   The one-call simplification of the analysis flow: point it at a folder of
#'   soundscape recordings and a template database, and it returns the
#'   detections. It is a **wrapper**: one function that chains other
#'   functions for you, in the correct order and wiring. The four stages it
#'   chains are template metadata ([fetch_template_metadata()]), soundscape
#'   metadata ([fetch_soundscape_metadata()]), the search grid
#'   ([fetch_match_grid()]) and the matching engine ([run_matching()]).
#'
#' @details Internally it runs, in order: [fetch_template_metadata()] (read
#'   the template database, or migrate an old template directory),
#'   [fetch_soundscape_metadata()] (scan the recordings), [fetch_match_grid()]
#'   (cross templates with soundscapes, dropping incompatible pairs) and
#'   [run_matching()] (score every pair and reduce scores to detections).
#'
#'   Use this wrapper for the common end-to-end case. When you need to
#'   inspect or tune the intermediate results (the search grid, or the raw
#'   score vectors), call the four stages yourself instead (see Examples).
#'   Raw scores are reachable only through `run_matching(output = "scores")`,
#'   never through this wrapper.
#'
#'   Two things to watch. (1) `templates_path` is dispatched by path type: a
#'   `.duckdb` **file** is read as a template database. A **directory** is resolved to
#'   the template database inside it when present (the `templates/` folder
#'   [export_templates()] wrote); only when there is no template database does it fall
#'   back to the old long-name template WAVs. (2) The detection filters (`min_score`,
#'   `min_quant`, `top_n`, `scope`) are the main knobs that decide how many
#'   detections you get back: start permissive and tighten.
#'
#' @section Pipeline context:
#'   Step 9 of the monitoraSom analysis flow, the top-level orchestrator. It
#'   subsumes steps 4-7 (template metadata, soundscape metadata,
#'   [fetch_match_grid()], [run_matching()]). Reads a soundscape folder and
#'   a template database from [export_templates()]. Produces a detections
#'   table for [detecs_to_rois()] (step 10) / [validate_by_overlap()]
#'   (step 11).
#'
#' @param soundscapes_path Directory holding the soundscape recordings to analyze.
#'   Default `"soundscapes/"`.
#' @param recursive_soundscapes Logical. Search `soundscapes_path` recursively.
#'   Default `FALSE`.
#' @param templates_path Either a template database `.duckdb` written by
#'   [export_templates()] (the routine path; the default
#'   `"templates/templates.duckdb"` is where [export_templates()] writes it), the
#'   cuts **directory** that holds it (`"templates"`: its template database is read), or
#'   a directory of old long-name template WAV files (migration path, when
#'   the directory holds no template database).
#' @param recursive_templates Logical. Search an old-format template directory
#'   recursively (ignored for a template database `.duckdb`). Default `FALSE`.
#' @param score_method Character, the matching engine: `"fft"` (default,
#'   FFT-accelerated, roughly 10× faster than and numerically equal to
#'   `"cor"` within 1e-6), `"cor"` (Pearson reference) or `"dtw"`
#'   (time-warping tolerant, experimental).
#' @param min_score Numeric in `[0, 1]` or `NULL`. Keep only detections with a
#'   peak score at or above this. `NULL` (default) keeps all.
#' @param min_quant Numeric in `[0, 1]` or `NULL`. Keep only detections at or
#'   above this score quantile. `NULL` (default) keeps all. Applied within the
#'   region set by `scope`.
#' @param top_n Integer or `NULL`. Keep at most this many detections, by raw
#'   score. `NULL` (default) keeps all. Applied within the region set by `scope`.
#' @param scope Character, how `min_quant`/`top_n` are applied: `"pair"`
#'   (default; within each soundscape-template score vector) or `"grid"`
#'   (across the whole grid).
#' @param output_db Optional path to a detections DuckDB database. When
#'   supplied the detections are written there (updated when they already
#'   exist); `NULL` (default) persists nothing. Use
#'   [export_detections_duckdb_to_csv()] to get a CSV out of the store.
#' @param autosave_action Character. `"replace"` (default; clear the
#'   detections table and insert the new rows) or `"append"` (update rows
#'   keyed on `detection_id`, keeping prior rows). Both run in one
#'   transaction. Only used when `output_db` is set.
#' @param output_file `r lifecycle::badge("deprecated")` Old CSV target. When
#'   supplied, the detections are additionally written to this path in the
#'   21-column layout from earlier versions, with a deprecation warning.
#'   Prefer `output_db`.
#' @param ncores Positive integer, default `1`. Number of CPU cores used by
#'   the parallel stages (the soundscape-metadata scan and the matching
#'   engine). Raise it to speed up large collections; it is capped at the
#'   number of available cores.
#' @param ... Further engine arguments forwarded to [run_matching()]:
#'   `buffer_size`, `stream_threshold_s`, `dtw_slack`. Scores-storage options
#'   (`scores_format`, `output = "scores"`) do not apply here and are rejected;
#'   call [run_matching()] directly for scores.
#'
#' @return A tibble of detections in the standard detections format, always
#'   returned; invisibly when persisted via `output_db` and/or `output_file`.
#'
#' @seealso [fetch_match_grid()], [run_matching()] (the stages it wraps),
#'   [detecs_to_rois()] and [validate_by_overlap()] (next steps),
#'   [export_templates()] (writes the template database).
#' @export
#' @examples
#' \dontrun{
#' # Load the package
#' library(monitoraSom)
#' # Step 9: one call from a soundscape folder + template database to detections.
#' # (The package ships no field recordings, so this example synthesizes them;
#' # for the same steps on real data see [fetch_example_data()].)
#' ss <- file.path(tempdir(), "recs"); dir.create(ss, showWarnings = FALSE)
#' rec <- tuneR::normalize(tuneR::sine(4000, duration = 10 * 16000,
#'                                     samp.rate = 16000), unit = "16")
#' for (f in c("siteA_01.wav", "siteA_02.wav"))
#'   tuneR::writeWave(rec, file.path(ss, f))
#' df_rois <- data.frame(
#'   soundscape_path = file.path(ss, "siteA_01.wav"),
#'   soundscape_file = "siteA_01.wav",
#'   roi_label = "burst", roi_start = 1, roi_end = 1.5,
#'   roi_min_freq = 2, roi_max_freq = 6, roi_wl = 512, roi_ovlp = 50,
#'   stringsAsFactors = FALSE)
#' tpl <- file.path(tempdir(), "templates")
#' export_templates(df_rois, templates_path = tpl, create_dir = TRUE)
#' tpl <- file.path(tpl, "templates.duckdb")
#' out <- file.path(tempdir(), "detections.duckdb")
#' df_detecs <- template_matching(soundscapes_path = ss, templates_path = tpl,
#'                                min_score = 0.3, output_db = out)
#' head(df_detecs)
#'
#' # Granular workflow: run the same four stages yourself to inspect the grid.
#' df_templates   <- fetch_template_metadata(tpl)
#' df_soundscapes <- fetch_soundscape_metadata(ss)
#' df_grid        <- fetch_match_grid(df_soundscapes, df_templates)
#' df_detecs2     <- run_matching(df_grid, score_method = "fft", min_score = 0.3)
#'
#' # The wrapper and the four stages give the same detections:
#' identical(sort(df_detecs$detection_id), sort(df_detecs2$detection_id))
#' nrow(df_detecs); nrow(df_detecs2)
#' }
template_matching <- function(
    soundscapes_path = "soundscapes/", recursive_soundscapes = FALSE,
    templates_path = "templates/templates.duckdb", recursive_templates = FALSE,
    score_method = c("fft", "cor", "dtw"),
    min_score = NULL, min_quant = NULL, top_n = NULL, scope = c("pair", "grid"),
    output_db = NULL, autosave_action = c("replace", "append"),
    output_file = NULL, ncores = 1, ...) {

  # --- front door (TM-04): fail fast, friendly messages, before any stage ----
  score_method <- match.arg(score_method)

  # FEAT-07: warn once when persisting to an unmarked explicit path.
  if (!is.null(output_db)) {
    .require_explicit_workspace(output_db, label = "output_db",
                                caller = "template_matching")
  }
  if (!is.null(output_file)) {
    .require_explicit_workspace(output_file, label = "output_file",
                                caller = "template_matching")
  }
  scope <- match.arg(scope)
  autosave_action <- match.arg(autosave_action)
  ncores <- .validate_ncores(ncores)
  .validate_score_filters(min_score, min_quant, top_n)
  if (!is.character(soundscapes_path) || length(soundscapes_path) != 1L ||
      !dir.exists(soundscapes_path)) {
    stop("`soundscapes_path` is not an existing directory: ",
         if (is.character(soundscapes_path)) soundscapes_path else "(not a path)",
         "\nPoint it at the folder holding your soundscape recordings.")
  }
  template_source <- .resolve_template_source(templates_path)
  if (!is.null(output_db)) .validate_output_db(output_db, "detections", "duckdb")
  .validate_legacy_output_file(output_file, autosave_action)
  .validate_dots(list(...), .TM_ALLOWED_DOTS)                   # AFL-10

  # --- the four stages, on the committed contracts (TM-02) -------------------
  df_templates <- fetch_template_metadata(
    templates_path = templates_path, recursive = recursive_templates,
    source = template_source
  )
  df_soundscapes <- fetch_soundscape_metadata(
    soundscapes_path = soundscapes_path, recursive = recursive_soundscapes,
    ncores = ncores
  )
  df_grid <- fetch_match_grid(
    soundscape_data = df_soundscapes, template_data = df_templates
  )
  detections <- run_matching(
    df_grid = df_grid, score_method = score_method, ncores = ncores,
    output = "detections", output_db = output_db,
    autosave_action = autosave_action, min_score = min_score,
    min_quant = min_quant, top_n = top_n, scope = scope, ...
  )

  # Deprecated legacy CSV compat (TM-11): written once, never forwarded to the
  # metadata scan (TM-05), always the legacy 21-column layout.
  if (!is.null(output_file)) {
    utils::write.csv(
      detections[, .detections_legacy_cols(), drop = FALSE],
      output_file, row.names = FALSE, fileEncoding = "UTF-8"
    )
    message("Detections also exported to the deprecated legacy CSV: ",
            output_file)
  }

  # TM-03: one return contract — the detections, invisibly when persisted.
  if (!is.null(output_db) || !is.null(output_file)) {
    invisible(detections)
  } else {
    detections
  }
}

# --- Private helpers ----------------------------------------------------------

# AFL-10: the only arguments meaningfully forwarded through `...` to run_matching
# are these engine knobs (run_matching has no `...`, so anything else would error
# downstream with an opaque "unused argument"). Validate at the façade door and
# raise a clear, named error instead.
# AFL-27: `scores_format` is intentionally NOT allowed — this façade returns
# detections only (TM-01), so a scores-storage option is inert here; passing it
# now raises the clear named error instead of being silently forwarded and
# ignored. Use run_matching(output = "scores", scores_format = ...) for scores.
.TM_ALLOWED_DOTS <- c("buffer_size", "stream_threshold_s", "dtw_slack")

.validate_dots <- function(dots, allowed) {
  if (length(dots) == 0L) return(invisible(TRUE))
  nm <- names(dots)
  bad <- if (is.null(nm)) rep(TRUE, length(dots)) else
    !nzchar(nm) | !(nm %in% allowed)
  if (any(bad)) {
    labels <- if (is.null(nm)) paste0("#", which(bad)) else
      ifelse(nzchar(nm[bad]), nm[bad], paste0("#", which(bad)))
    stop("template_matching: unrecognized argument(s) passed via `...`: ",
         paste(labels, collapse = ", "),
         ". Allowed pass-through arguments are: ",
         paste(allowed, collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}

# Map the single beginner-facing `templates_path` onto fetch_template_metadata's
# explicit `source` contract (FTM-101 has no "auto"): a file is a template database
# .duckdb; a directory is the export_templates() cuts folder when it holds the
# template database, else a legacy template-WAV folder (TM-10, FTM-110). Path-type +
# content dispatch, kept in sync with fetch_template_metadata's own resolution.
.resolve_template_source <- function(templates_path) {
  if (!is.character(templates_path) || length(templates_path) != 1L ||
      is.na(templates_path)) {
    stop("`templates_path` must be a single path: a template database ",
         "`.duckdb` (written by export_templates()) or a directory of legacy ",
         "template WAV files.")
  }
  if (dir.exists(templates_path)) {
    template_db <- file.path(templates_path, .monitora_db_names()[["templates"]])
    if (file.exists(template_db)) return("duckdb")
    return("legacy_filename")
  }
  if (file.exists(templates_path)) return("duckdb")
  stop("`templates_path` does not exist: ", templates_path,
       "\nExpected a template database `.duckdb` (see export_templates()) or ",
       "a directory of legacy template WAV files.")
}

# Front-door checks for the deprecated `output_file` CSV alias (TM-11): warn
# about the deprecation up front (before the pipeline spends minutes matching),
# require an existing destination directory, and flag that the CSV path has
# replace-only semantics.
.validate_legacy_output_file <- function(output_file, autosave_action) {
  if (is.null(output_file)) return(invisible(TRUE))
  warning("`output_file` is deprecated: persist with `output_db` (DuckDB) and ",
          "use export_detections_duckdb_to_csv() for a CSV. Writing a legacy ",
          "21-column CSV to `output_file` this time.", call. = FALSE)
  if (!dir.exists(dirname(output_file))) {
    stop("The `output_file` directory does not exist: ", dirname(output_file))
  }
  if (autosave_action == "append") {
    warning("`autosave_action = \"append\"` is not supported on the deprecated ",
            "`output_file` CSV path; the CSV is replaced. Use `output_db` for ",
            "appendable persistence.", call. = FALSE)
  }
  invisible(TRUE)
}
