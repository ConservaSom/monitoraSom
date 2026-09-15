# Golden NOTES — `validate_by_overlap` (flow #13, prefix VBO)

Generator: `R/sandbox/validate_by_overlap.R` (sources `R/original/`, captures the
ORIGINAL behaviour faithfully). Fixtures: a deterministic subset of
`data/df_detecs.rda` (1090 detections, **6 templates of one species**,
*Basileuterus culicivorus*) + `data/df_rois.rda` (30 ground-truth ROIs of that
species). The committed fixtures are never mutated; `validation_time` is dropped
before freezing (C9).

## Frozen ORIGINAL behaviour (reference, not the target)

`G1_validated.rds` / `G1_summary.txt` — the original single **wide** frame:

| metric | value |
|---|---|
| rows × cols | 1198 × 43 |
| TP | 157 |
| FP | 933 |
| FN | **108** |
| note values | `instersection with a ROI`, `no intersection with a ROI`, `no detections to intersect with` |

`G2_persist_summary.txt` — the original CSV had the same 1198 rows.
`conditions_log.csv` — captured messages/warnings/errors of the original runs.

## Divergences — counts faithful, only the SHAPE changed

The 2026-06-13 user gate decided two things; after the clarification later that
day, the refactor keeps the original **TP/FP/FN counts** and changes only the
return shape (plus one orphan-label tweak). `test-validate_by_overlap.R` asserts
the original totals against the golden plus the structural divergences.

- **VBO-100 — FN per template (kept), orphan→FP (changed).** An earlier
  species-level FN attempt was **reverted** (2026-06-13 clarification): the whole
  diagnostics flow downstream is per template (`diagnostic_validations` splits by
  `template_name`), so FN must be per template. The refactor reproduces the
  original per-template FN exactly: **108** FN rows = 6 templates × 18 missed
  ROIs, each tagged with its `template_name`. TP=157, FP=933 unchanged. The ONE
  kept change: detections of a species with NO ground-truth ROIs are labelled
  **FP** (`"no ROIs of this species"`) instead of being silently dropped — not
  exercised by this single-species subset (covered by a synthetic-orphan unit
  test).
- **VBO-101 — split return (the real structural change).** The wide 1198×43
  union frame is replaced by a named list: `$detections_validated` (1090 rows,
  one TP/FP verdict per detection; canonical validations schema = 24 + 4 cols)
  and `$false_negatives` (108 rows; canonical ROI schema + `template_name`/
  `template_file` tag + 4 validation cols). `bind_rows` of the two reproduces the
  original 1198-row union (the deprecated CSV path).
- **VBO-08** — the misspelled note value `"instersection with a ROI"` is kept
  verbatim (a downstream string-matched data value; fix deferred).

## Why the golden RDS is kept as-is

`G1_validated.rds` is the **original wide-frame witness** so the structural
divergence stays auditable (the test reads `ORIG_TP/FP/FN` from it). The
committed example dataset `df_detecs_val_tovlp` (old wide shape) is migrated to
the split return together with the diagnostics pair (#14/#15), which consumes
it — see the `VBO-101` decision and the diagnostics cycle.
