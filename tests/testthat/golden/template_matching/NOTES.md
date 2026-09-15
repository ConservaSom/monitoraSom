# Golden notes — `template_matching` (#10, top-level orchestrator)

Generator: `R/sandbox/template_matching.R`. These goldens capture the **ORIGINAL**
behaviour of `R/original/template_matching.R` **faithfully (bugs included)**.
`template_matching` is a thin façade chaining four already-cycled stages
(`fetch_template_metadata` → `fetch_soundscape_metadata` → `fetch_match_grid` →
`run_matching(output = "detections")`), so these goldens freeze **orchestration**
facts; the analysis internals are owned by the per-stage goldens.

DEBT log: `R/refactored/refactoring_log/DEBT_INVENTORY_LOG-template_matching.md`
(prefix `TM`). Gate decision: **TM-01 = Option A** (thin aligned façade) with two
binding constraints — (1) simplified file management for the user, (2) detections
only, never full scores.

## Fixture (temp copies — committed fixtures never mutated)

- **Soundscapes:** `data/soundscapes_audiomoth/{…_075000.WAV, …_092000.WAV}` — the
  two recordings the templates were cut from.
- **Templates:** `R/sandbox/shared_inputs/template_trio/roi_cuts_legacy/` — 2 WAVs
  ([[project-template-trio-fixture]]; hand-made, not regenerable).
- Grid = full cross = **2 templates × 2 soundscapes = 4 pairs**.
- `score_method = "cor"`, `ncores = 1` (fast + deterministic).

## Artifacts

| File | What | Frozen fact |
|---|---|---|
| `G1_detections_inmem.rds` | default run, `output_file = NULL` | returns a **21-col** detection `data.frame`, **45 rows** (per-pair 14/11/10/10); emits `"Template matching finished"` |
| `G2_detections_persisted.csv` (+`.md5`) | run with `output_file=<csv>`, `autosave="replace"` | the legacy CSV writer output (45 rows + header) |
| `G3_detections_min_score.rds` | `min_score = 0.3` forwarded | **27 rows** (filter pass-through, fewer than G1) |
| `conditions_log.csv` | per-case returns_df / n_rows / messages / errors | — |
| `fixture_provenance.txt` | exact fixture files used | — |

(No RDS for G2/G4: G2's return is `NULL`; G4 errors before producing a value.)

## Frozen ORIGINAL facts (must be reproduced, except the divergences below)

- **Grid fan-out & counts.** The full 2×2 cross yields 45 detections under `cor`
  with default filters; `min_score = 0.3` reduces to 27. (Exact per-pair
  localization/scores are owned by the matching + peaks goldens; here we anchor
  the **orchestration-level counts** and that the filters/score_method are
  forwarded correctly.)
- **Stage messages.** `"Template metadata successfully extracted"` (FTM),
  `"All files are compatible and included in the matching grid."` (FMG), plus the
  run_matching tail message, then template_matching's own `"Template matching
  finished"` and (when persisting) `"Detections have been saved to <file>"`.

## Intentional divergences (asserted as DERIVED in the testthat step — not verbatim)

- **TM-03 — return contract.** ORIGINAL returns the tibble only when
  `output_file = NULL`; when persisting it returns **`NULL`** (see G2,
  `returns_df = FALSE`). Refactor → **always return the detections tibble**
  (invisibly when persisted). The testthat step asserts the refactored function
  returns a data.frame in BOTH modes.
- **TM-02 — persistence format.** ORIGINAL writes a **legacy CSV** (G2 + its
  `.md5` are the byte reference). Refactor → **DuckDB** via the `output_db`
  contract (CSV deprecated, per b81c1d0/b0feb25). The CSV golden is kept as the
  historical "before"; the refactored test asserts the DuckDB path instead of CSV
  byte-equality.
- **TM-05 — `output_file` overload.** In the ORIGINAL the single `output_file` is
  passed to **both** `fetch_soundscape_metadata` (which writes soundscape metadata
  to it, `fetch_soundscape_metadata.R:173`) **and** `run_matching` (which then
  overwrites it with detections). G2's message trace shows the double write.
  Refactor → **separate concerns / simplified file management** (TM-01 constraint
  1); the detections target is no longer clobbered by a metadata side-write.
- **Detection schema.** ORIGINAL emits the **21-col** legacy detection schema
  (see `G1` columns). The refactored detection path (committed b0feb25) emits the
  **canonical 24-col schema** (`_schema_detections.R`) via
  `filter_detections(scope = "pair")`. So G1/G3 are compared on **row counts +
  localization invariants**, not column-verbatim equality; the canonical-schema
  contract itself is already covered by the FSP/FSPB suites.
- **TM-04 — validation (partial).** Note the ORIGINAL is **not** fully
  unvalidated: a bad `templates_path` already errors early via
  `fetch_template_metadata` — G4 captures `"The provided path to the templates
  does not exist"`. The gap TM-04 addresses is the **orchestrator-level** absence
  of validation for the *other* inputs (e.g. `score_method` membership incl. the
  new `"fft"`, `soundscapes_path`, filter ranges) and friendlier, earlier
  messages. The refactored test asserts the added front-door checks; G4 anchors
  the one check that already exists downstream.
- **TM-06 — doc drift.** No runtime artifact (documentation only): `score_method`
  must list `cor`/`fft`/`dtw`; `buffer_size` semantics; vestigial `ncores`.
- **TM-13 — inherited peak-count divergence (discovered at the smoke step,
  2026-06-12).** The "grid fan-out & counts" anchor above holds for the ORIGINAL
  only. The refactored chain inherits the peaks-stage divergences FSP-04 (edge
  margin = `pad_length`, not one full window) and FSP-05 (greedy NMS replaces
  the window-dominance test), composed with RMC-01 (one-frame padding shift):
  G1's 45 detections become **81** (per-pair 21/21/19/20) and G3's 27 become
  **28**. The refactored output is a verified **strict superset**: all 45 G1
  peaks present at `peak_index + 1` with scores equal within 7.6e-14; the 36
  extras are FSP-04/05-admitted weak local maxima (35 score `< 0.3`; 1 scores
  0.366 — the +1 in the filtered run). Assert as DERIVED: superset + the new
  counts, not 45/27 equality.
- **TM-12 — engine template-read fix (smoke discovery).** Not a golden
  divergence per se but required to run FTM-produced grids at all: the engine
  now resolves the template read per `template_mode`
  (`.template_read_spec()`, see DEBT log TM-12) instead of cutting the
  standalone WAV with source-relative bounds.
