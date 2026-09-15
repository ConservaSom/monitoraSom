# Golden NOTES — `set_workspace` (prefix SW)

Captured by `R/sandbox/set_workspace.R` over the **original** read-only baseline
`R/original/set_workspace.R`. These goldens record the original's behaviour
**faithfully, bugs included**. They are the reference for the refactor; the
**intentional divergences** (listed below) are asserted as *derived* expectations
in the testthat step, **not** as verbatim equality with these files.

Cycle plan: `plans/plan-logs/plan-log-2026-06-05-002.md`. DEBT log:
`R/refactored/refactoring_log/DEBT_INVENTORY_LOG-set_workspace.md`.

## How the original was made runnable (faithful, not a behaviour change)

- **`data(<name>, package = "monitoraSom")` → shimmed** to `load("data/<name>.rda")`.
  The installed `monitoraSom` (v0.3.2) is the **stale pre-refactor** package and
  must never be loaded for refactored work; the shim loads the **committed repo
  fixtures**, reproducing the same objects the original would reference. This is
  the mechanism `SW-05` flags as broken in this repo. The shim is in the
  generator only — it is not part of the function under test.
- **`usethis::create_project` / `rstudioapi` left to run for real** (both
  installed). Their artifacts (`.Rproj`, `.gitignore`, `R/`) appear in the tree
  snapshots **by name only**; their *content* is usethis-version dependent and is
  not fingerprinted (`content_md5 = "<skipped: usethis/marker artifact>"`).
- **Working directory.** The defaults are relative (`"./x/"`), so the original
  creates dirs under `getwd()` (this is bug `SW-01`). The realistic cases set
  `getwd() == project_path` (docstring: "run from within the new project dir");
  `SW-G5` deliberately decouples them to record the bug.
- **Temp-path scrubbing.** Absolute session-temp paths echoed in messages (e.g.
  usethis "Created new R project at: …") are normalised to `<TMP>` so the message
  goldens are reproducible.

## Fixtures used

All from committed `data/*.rda` (object names match file names):
`roi_label_lists` (1972×4), `ls_soundscapes` (12 Waves), `ls_recordings` (2),
`ls_templates` (6), `ls_roi_tables` (13), `df_soundscapes` (12×6), `df_templates`
(6×11), `df_grid` (72×17), `df_detecs` (4363×21), `df_scores` (72×20 tbl),
`df_detecs_val_manual` (4363×26), `df_detecs_val_tovlp` (4365×45). No fixture was
mutated; every case runs in a fresh `tempdir()` workspace.

## Cases and artifacts

| Case | Scenario | Artifacts |
|---|---|---|
| **SW-G1** `dirs_only` | `example_data=FALSE`, `getwd()==project_path`, defaults | `g1_dirs_only_tree.csv`, `g1_dirs_only_files.csv`, `g1_dirs_only_messages.txt` |
| **SW-G2** `example_full` | `example_data=TRUE`, defaults | `g2_example_full_tree.csv`, `g2_example_full_files.csv` (42 files), `g2_example_full_messages.txt` |
| **SW-G3** `rerun_idempotent` | run `example_data=TRUE` twice | `g3_rerun_files.csv`, `g3_rerun_messages.txt` (2nd-run "ALREADY EXISTS"), `g3_rerun_unchanged.txt` (=TRUE) |
| **SW-G4** `na_skip` | `example_data=FALSE`, `soundscapes_path=NA` | `g4_na_skip_tree.csv` (no `soundscapes/`), `g4_na_skip_messages.txt` |
| **SW-G5** `sw01_wd_bug` | `example_data=FALSE`, `getwd() != project_path` | `g5_sw01_workdir_tree.csv` (dirs land here), `g5_sw01_projectpath_tree.csv` (only usethis artifacts), `g5_sw01_wd_bug_messages.txt` |
| **SW-G6** `errors` | (a) non-existent `project_path`; (b) `project_path=NULL` | `g6a_nonexistent_path_messages.txt`, `g6b_null_path_messages.txt` |
| **SW-G7** `existing_rproj` | `project_path` already has an `.Rproj` | `g7_existing_rproj_tree.csv`, `g7_existing_rproj_messages.txt` (warning + skip) |

`*_tree.csv` = sorted relative paths with `dir`/`file` type. `*_files.csv` =
content fingerprints: WAV → `n_samples`/`samp_rate`/`channels`/file-md5; CSV →
file-md5; XLSX/RDS → md5 of the *deserialised object* (avoids zip/gzip timestamp
nondeterminism); usethis artifacts → skipped.

## Behaviours captured FAITHFULLY (bugs included)

- **SW-01** (`g5`): with relative defaults, all 15 dirs **and** the label file are
  created under `getwd()`, **not** `project_path`. `project_path` receives only
  the usethis `.Rproj`/`.gitignore`/`R/`.
- **SW-06** (`g2` messages): the two `validation_outputs` writes report
  "WAS CREATED at **'./detections/'**" — the wrong directory in the log line
  (variable reuse). Both `df_detecs_val_manual.csv` and `df_detecs_val_tovlp.csv`
  are nonetheless written into `validation_outputs/` (see `g2…_files.csv`).
- **SW-03 / SW-04** (`g3`): the no-overwrite guards hold on a clean re-run
  (fingerprints unchanged, "will not be overwritten" messages). The partial-state
  overwrite hazard (some-but-not-all files exist → `all(file.exists())` is FALSE →
  block rewrites the existing ones) is **not** exercised by these cases; it is a
  derived expectation for the refactor's per-file check.
- **SW-05** (all `example_data` writes): values come from the shimmed fixtures,
  documenting that the original `data(package=)` path is non-functional here.
- **SW-09 / SW-103** (`g1`,`g6b`): `project_path=NULL` errors via the
  `rstudioapi` branch; the `usethis` artifacts appear in every success tree.

## Intentional divergences the refactor WILL change (derived in testthat, not verbatim)

| DEBT | Original (golden) | Refactor target |
|---|---|---|
| **SW-01 / SW-13** | dirs anchored to `getwd()` via `"./x/"` | anchor to `file.path(project_path, name)` |
| **SW-03 / SW-04** | coarse `all(file.exists())`, doc says "overwrites" | per-file existence check; doc says no-overwrite |
| **SW-05 / SW-102** | `data(package="monitoraSom")` (broken) | re-point to `data/*.rda`; build `rois.duckdb` fresh (option B) |
| **SW-06** | log says `./detections/` for validation outputs | correct path in message (table-driven rewrite SW-16) |
| **SW-07** | `dir.create()` non-recursive | `recursive=TRUE` + success check |
| **SW-09 / SW-103** | `rstudioapi` + `usethis::create_project` (`.Rproj`,`.gitignore`,`R/`) | **pure base R**: required `project_path` + plain-text marker (`monitoraSom.proj`) |
| **SW-14** | seeds only `roi_label_lists.xlsx` | also seed `roi_types.xlsx` via `_segmentation_setup.R` |
| **SW-101** | (n/a) | dirs-yes / DBs-no skeleton; no `.duckdb` pre-created |
| **SW-104** | writes `df_scores.rds`, `df_detecs.csv`, validation CSVs, `df_grid.csv` | downstream example outputs **deferred** (format undecided) |

Because of SW-101/SW-103/SW-104, the refactored tree will differ structurally
from these goldens (no usethis artifacts; a base-R marker instead of `.Rproj`;
`example_data` produces a DuckDB-coherent subset, not the legacy CSV/RDS dump).
The testthat step asserts the **refactored** contract against these goldens as
*transformed* expectations, plus equality only where behaviour is preserved
(e.g. the directory-name set, the WAV fingerprints of the example soundscapes).

## Regenerate

```sh
Rscript R/sandbox/set_workspace.R
```
Deterministic and root-relative (walks up to `data/` + `R/`). Does not commit.
