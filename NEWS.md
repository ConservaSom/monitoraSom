# monitoraSom 1.2.0.3

Documentation patch. No code changes.

* The README explains how to avoid the long first install on Ubuntu. `pak`
  builds `duckdb` from source, which can take 20 minutes or more. The README
  now shows how to copy a ready-made `duckdb` binary into the R user library
  before `pak`. It also warns that the first install can be slow.

# monitoraSom 1.2.0.2

Patch release. It replaces the `1.2.0.1` build that the GitHub repository
carried until 2026-09-25.

* `read_raven_selection()` and `read_audacity_labels()` require `roi_user`,
  the name of who made the annotations. The files carry no author.
* The writers and both apps take `csv_mirror`, a CSV snapshot of the DuckDB
  store (see the `1.2.0` notes below).
* Reading the soundscape-metadata store no longer writes to it. Every read
  path opens the store read-only.
* `run_matching()` with `ncores > 1` no longer drops the pairs of a worker
  that died. Before this fix, it could return zero rows with no warning.

# monitoraSom 1.2.0

First CRAN submission. Relative to the previous release, this version adds a
number of exported functions and reworks the pipeline's persistence and
matching internals. The changes below are the user-visible differences.

## New functions

* Example data access: `fetch_example_data()` and `is_example_data_cached()`
  download and cache the example dataset on demand, keeping the installed
  package small.
* DuckDB <-> CSV interchange: `migrate_detections_csv_to_duckdb()`,
  `migrate_metadata_csv_to_duckdb()`, `migrate_rois_csv_to_duckdb()`,
  `migrate_templates_to_db()`, and the `export_detections_duckdb_to_csv()`,
  `export_metadata_duckdb_to_csv()`, `export_rois_duckdb_to_csv()`,
  `export_templates_duckdb_to_csv()` and `export_validations_duckdb_to_csv()`
  helpers.
* Third-party annotations: `read_raven_selection()` / `write_raven_selection()`
  and `read_audacity_labels()` / `write_audacity_labels()`.
  The two readers require `roi_user`, the name of who made the annotations
  (the files carry no author); it is stamped on every row and becomes the
  `created_by` of the imported ROIs.
* Detection filtering: `filter_detections()` and `filter_detections_i()`.
* Visualization: `plot_scores_overview()` and `plot_spectro()` complement
  `plot_scores()`.
* Utilities: `extract_spectro()` and `catalog_templates()`.
* Matching summaries (experimental): `estimate_matching()` and
  `summarise_matching()`.

## Documentation

* A getting started vignette ships with the package, `vignette("monitoraSom")`.
  It walks the full workflow and runs the validation and diagnostics steps on
  the bundled example tables, so it works without downloading any audio.

## Major changes

* The pipeline now persists to DuckDB stores with a versioned schema. Earlier
  versions wrote and read results as CSV files (`write.csv()` / `sink()`);
  detections, ROIs, metadata, templates and validations now live in DuckDB.
  To keep a copy that opens without DuckDB, `template_matching()`,
  `run_matching()`, `fetch_score_peaks()`, `validate_by_overlap()` and
  `import_birdnet_detections()` take `csv_mirror`: with `csv_mirror = TRUE`
  they write `<database name>_mirror.csv` next to the signals database, once,
  after the last write. `fetch_soundscape_metadata()` takes the same argument
  for the metadata database. `launch_segmentation_app()` and
  `launch_validation_app()` take it too, and write the copy when you end the
  session. The CSV is a snapshot for reading only.
* A new FFT-accelerated backend was added to `run_matching_i()` /
  `run_matching()` and is now the default `score_method`. The previous
  cross-correlation (`"cor"`) and dynamic-time-warping (`"dtw"`) backends
  remain available.
* The DTW backend switched its distance normalization to `symmetric2`
  warping with a mean per-cell path cost. DTW score thresholds calibrated
  with earlier versions do not transfer; recalibrate them.
* The matching front-end no longer normalizes each recording's spectrogram
  to its own maximum: scores are now comparable across recordings.
  Detection scores stored by earlier versions do not transfer; rerun the
  matching.
* The parallel backend was reworked to use `mirai`, replacing the previous
  `parallel::makePSOCKcluster()` / `future` approach.
* ROI `soundscape_path` keys are now stored workspace-relative; legacy
  absolute paths are rewritten on migration with
  `migrate_rois_csv_to_duckdb(workspace_root = ...)`. The segmentation app
  refuses duplicate ROIs and zero-area rectangles at the source.
* The segmentation app records the active label list with each ROI
  (`roi_label_list`), so an identification is traceable to the list it came
  from.
* `validate_by_overlap()` now only validates recordings whose review state
  is known: a recording must carry ROIs or the "no signals of interest"
  sentinel. Detections on never-reviewed recordings are excluded with a
  warning, never counted as species absence.
* `fetch_rois()` now returns the standard signals format (45 columns) by
  default: `roi_user` is now `created_by`, `roi_input_timestamp` is now
  `created_at`, and `roi_source` is now the pair `signal_class` + `origin`.
  `shape = "rois"` returns the previous 20-column ROI format unchanged. The
  first call per session without `shape` warns about the change. Databases
  and CSV tables in the previous format are still read. By default only ROI
  rows return, so the ROIs that `detecs_to_rois()` wrote into an older
  database now need `include_detections = TRUE`. `export_templates()` and
  `validate_by_overlap()` accept both formats.

## Interactive apps

* The segmentation app gains an adjustable validation grid (`grid_dim`),
  from 1x1 to 4x4 cells per page.
* The segmentation app gains detection-to-ROI promotion (hotkey `Alt+P`):
  a detection can be promoted to a manual ROI without leaving the app.
* `fetch_rois()` exposes a spectrogram-parameter summary (attribute
  `spectro_summary`): which `wl`/`ovlp`/rate/pitch tuples the ROIs were
  segmented under. ROI bounds are only comparable within one tuple, and a
  message flags heterogeneous inputs.
* The validation app now validates manual ROIs alongside detections and
  reads and writes the unified signals store (one DuckDB file holds both).
  Re-labelling is traceable: the original `roi_label` is kept and each
  correction is stamped with the user and time.

## Changes since the first draft (docs-review-extra plan)

* The `pipeline.yml` project config and the `config` argument of
  `run_matching()` / `template_matching()` were removed; matching knobs are
  function arguments only.
* `fetch_soundscape_metadata()` dropped the deprecated `skip_processed`
  argument (`cache_policy` covers it) and gained `min_duration_s`: readable
  recordings shorter than the floor (default 1 second) go to the error log
  and stay out of the flow.
* `run_matching()` scores persist only to DuckDB; the Parquet option was
  removed. The detections example shows the DuckDB-to-CSV conversion.
* `fetch_match_grid()` channel handling was redesigned: `"strict"` split into
  `"strict_left"`/`"strict_right"`, and `"per_channel"` scores each
  soundscape channel separately. `output_con`/`output_view` were removed.
* `diagnostic_validations()` no longer takes `val_a_priori`; recall and the
  ROC curve are computed when false negatives exist for the template/class,
  with a clear warning otherwise.
* `validate_by_overlap()` now promotes every true-positive detection to a ROI
  row in the signals store automatically (opt-out with `promote_to_roi =
  FALSE`), closing the detection-to-ROI cycle.
* `set_workspace()` no longer creates `match_scores/` up front; the folder is
  created on demand when scores are persisted.
* The segmentation app derives `preset_path` from `project_path`; the
  argument is gone. Label lists can be plain `.txt` files (one list per file,
  `label_list_` prefix); the old `.xlsx` remains readable.
* "manifest" is renamed to "template database" in code and docs; the
  importer is now `migrate_templates_to_db()`.
* The package help page (`?monitoraSom-package`) documents the before/after
  1.2.0 differences.
