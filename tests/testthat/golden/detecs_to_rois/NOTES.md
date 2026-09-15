# Golden notes — `detecs_to_rois` (#11, detections → ROIs)

Generator: `R/sandbox/detecs_to_rois.R`. These goldens capture the **ORIGINAL**
behaviour of `R/original/detecs_to_rois.R` **faithfully (bugs included)**.

DEBT log: `R/refactored/refactoring_log/DEBT_INVENTORY_LOG-detecs_to_rois.md`
(prefix `DTR`). Gate CLOSED autonomously under the push (plan §0); the single
`feature` item (DTR-10) is `adiado`.

## Fixture (deterministic subset — committed fixtures never mutated)

- **Source:** `data/df_detecs_val_manual.rda` (4363 validated detections, the
  a-posteriori validation-app output: columns `validation`,
  `validation_user`/`_time`/`_note` on top of the canonical detection columns).
- **Subset (`input_subset_validated.rds`):** first 2 `template_file`s ×
  {TP, FP, NV} × `slice_head(2)` → a small multi-row frame **with** `validation`.
- **Legacy view (`input_subset_legacy.rds`):** the same rows reduced to the 21
  legacy detection columns (no validation-app columns) — drives the
  no-`validation` branch.
- **Two-label view (`input_two_labels.rds`):** the legacy subset with half the
  rows' `template_name` relabelled `Myiothlypis flaveola` (the committed set is
  single-species), so the **DTR-02** non-vectorized-label bug is observable.

## Artifacts

| File | What | Frozen fact |
|---|---|---|
| `G1_rois_inmem.rds` | `output_path = NULL` | 18-col ROI frame (16 frozen — see C9 below); message-only; `roi_type = "detection"` |
| `G2_rois_filter_tp.rds` | `filter_tp = TRUE`, has `validation` | only `validation == "TP"` rows; `roi_label_confidence = "certain"` (4 rows) |
| `G3_rois_no_validation.rds` | `filter_tp = TRUE`, no `validation` | warns "Data was not filtered…", does **not** filter; `roi_label_confidence = NA` (12 rows) |
| `G4_rois_persisted_return.rds` (+ `G4_persist_summary.txt`) | `output_path = <dir>` | 3 per-soundscape CSV tables written (12 rows total); the frame is **also returned** |
| `G6_rois_two_labels.rds` (+ `input_two_labels.rds`) | 2 distinct input labels | **DTR-02**: ALL output rows carry **row 1's** label (`Basileuterus culicivorus`), never `Myiothlypis flaveola` |
| `conditions_log.csv` | per-case returns_df / n_rows / messages / warnings / errors | — |
| `fixture_provenance.txt` | exact fixture derivation | — |

(No RDS for G5: it errors before producing a value.)

## C9 — non-deterministic fields dropped before freezing

`roi_file` and `roi_input_timestamp` embed `Sys.time()` (DTR-07). The generator
**drops both** before `saveRDS`, so the frozen frames are 16-col. The testthat
step compares on the remaining 16 columns and asserts the two time fields exist
and are well-formed, not their values. The G4 CSV file **names** also embed the
timestamp, so only file **count** and total **row count** are frozen
(`G4_persist_summary.txt`), not byte content.

## Frozen ORIGINAL facts (reproduced, except the divergences below)

- **Mapping.** detections → 18-col ROI frame: `roi_start/end` =
  `detection_start/end`; `roi_min/max_freq` = `template_min/max_freq`;
  `roi_type = "detection"`; `roi_comment` = the `peak_score|peak_quant|…`
  provenance string; `roi_wl/ovlp/sample_rate` = `detection_*`;
  `roi_pitch_shift = 1`.
- **`filter_tp` semantics.** Filters to `validation == "TP"` only when the
  column exists; otherwise warns and passes everything through (G2 vs G3).
- **`roi_label_confidence`.** `"certain"` for TP rows when `validation` exists,
  else `NA` (G2 vs G1/G3).
- **Persistence.** One CSV per distinct `roi_path` (= per soundscape), plus the
  returned frame (G4).
- **Required-column stop.** A hand-listed 21-col `required_cols` check errors on
  any missing column (G5).

## Intentional divergences (asserted as DERIVED in the testthat step)

- **DTR-02 — non-vectorized `roi_label` (bug).** ORIGINAL: every output row gets
  the FIRST row's label (G6 proves it: 2 input labels → 1 output label). Refactor
  → per-row label (last `_`-token of `template_name` sans extension). Test
  asserts the refactored G6 output carries BOTH labels, row-aligned.
- **DTR-05 — case-sensitive `.wav` strip (bug).** ORIGINAL strips only lowercase
  `.wav`; the project's `.WAV` fixtures leave the extension embedded in
  `roi_file` and (when the name ends in `.WAV`) in `roi_label`. Refactor →
  case-insensitive `(?i)\\.wav$`. (Not visible in these goldens because
  `template_name` here ends in `.wav`; asserted directly on a `.WAV` case in the
  test.)
- **DTR-01 — input contract & schema.** ORIGINAL reads only a data.frame or a
  CSV path and checks a hand-listed 21-col layout (G5). Refactor → also accept a
  detections `.duckdb`; required columns derive from the schema helpers (C8); CSV
  intake deprecated. G5's error is asserted as the legacy behaviour the refactor
  replaces with a friendlier, schema-driven message.
- **DTR-03 — persistence.** ORIGINAL writes per-soundscape CSV tables (G4).
  Refactor → canonical ROI **DuckDB** store (`_roi_duckdb.R`), with the **C7
  guard** (detection-derived ROIs in a SEPARATE store from hand-segmented ground
  truth). CSV path kept as deprecated opt-in compat. Test asserts the DuckDB
  round-trip (write → `fetch_rois`) and the C7 separation, not CSV byte-equality.
- **DTR-04 — canonical ROI schema.** ORIGINAL emits `roi_file` and omits
  `roi_channel`; `roi_is_complete` is logical, `roi_pitch_shift` numeric.
  Refactor → canonical 18-col schema via `.coerce_rois()` (`roi_channel` default,
  `roi_file` dropped from the in-memory frame, types fixed). So G1/G2/G4 are
  compared on the **mapped value columns**, not column-set-verbatim equality.
- **DTR-07 — single `Sys.time()` capture.** ORIGINAL calls `Sys.time()` twice;
  refactor captures once. Covered by the C9 freeze above.
- **F6 — `roi_comment` payload dies (signals program, SIG-03).** ORIGINAL packs
  seven detection values into `roi_comment` as a write-only
  `key=value|...` string (AFL-22). The unified `signals` store carries them as
  real `det_*` columns, so the legacy ROI shape now gets `roi_comment = NA`
  (G1/G2/G3/G6). `roi_comment` is therefore excluded from the shared-column
  comparison; the G1 test asserts the refactor emits all-NA while the frozen
  original was all-populated.
