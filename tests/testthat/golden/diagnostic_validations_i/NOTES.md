# Golden NOTES — `diagnostic_validations_i` (flow #14, prefix DVI)

Generator: `R/sandbox/diagnostic_validations_i.R` (sources `R/original/`,
captures the ORIGINAL behaviour faithfully — bugs included). Fixtures: the
committed example datasets `data(df_detecs_val_tovlp)` (a-priori, the
`validate_by_overlap` output) and `data(df_detecs_val_manual)` (a-posteriori,
validation-app output); never mutated. The engine takes ONE template, so each
golden feeds a single template's rows. No RNG — GLM + `cutpointr` are
deterministic.

## Frozen ORIGINAL behaviour (the equality target)

Per template, `<tag>_diagnostics.rds` = the `diagnostics` data.frame and
`<tag>_summary.rds` = a scalar summary (`score_cut`, GLM coefficients/deviance/
AIC, row count, selected `peak_score`, prAUC, AUC, plot classes, plot layer
counts).

| tag | branch | score_cut | rows | prAUC | AUC | roc layers |
|---|---|---|---|---|---|---|
| G1_auto_cutpointr | auto, `cutpointr` (n_fp,n_tp ≥ 4) | 0.650 | 638 | 0.530 | 0.674 | 4 |
| G2_manual_cut | manual `diag_cut = 0.30` | 0.300 | 638 | 0.530 | 0.674 | 4 |
| G3_manual_sweep | auto, manual-sweep (n_fp = 3) | 0.130 | 1 | 0.000 | 0.000 | 4 |
| G4_aposteriori | auto, `val_a_priori = FALSE` | 0.430 | 620 | 0.769 | 0.915 | 1 |
| G5_multi_template | (error) | — | — | — | — | — |

- `mod_plot`/`precrec_plot`/`f1_plot`/`plot_dens` layer counts = 3/4/3/2 in all
  branches. `roc_plot` has **4** layers when `val_a_priori = TRUE` (real ROC) and
  **1** when `FALSE` (the "ROC plot not available" placeholder — G4).
- **G3 captures a real artifact of the manual-sweep branch:** with only 3 FP rows
  the NaN→column-max imputation (DVI-03) plus `distinct()` collapse the diag
  table to a **single row** (rows = 1, prAUC/AUC = 0). Captured faithfully.
- **G5** errors with the single-template guard message (faithful).

## DVI-06 — statistical redesign re-baselines the diagnostics table

**The G1–G5 goldens above are now the original-behaviour WITNESS only** (frozen,
never the equality target). DVI-06 (gate 2026-06-15,
`plans/plan-logs/plan-log-2026-06-15-001.md`) replaces the diagnostics-table computation
with **one empirical sweep over the observed scores** — a detection is positive
iff `peak_score >= t`, with `tp/fp/tn/fn` read off cumulative counts over the
descending-sorted scores (the exact, data-driven PR/ROC thresholds, identical to
what `cutpointr::roc(direction=">=")` built internally). `cutpointr` and the
`>= 4`-per-class gate are **dropped**; undefined metrics use standard conventions
(replacing the NaN→column-max imputation): `precision = 1` when `tp + fp = 0`,
`recall = 0` when `tp + fn = 0`, `specificity = 1` when `tn + fp = 0` (a
defensible, non-universal convention), `F1 = 0` when `precision + recall = 0`.

Redesigned expectations live in `redesigned/` (generator
`R/sandbox/diagnostic_validations_i_redesigned.R`, which sources `R/refactored/`)
and are the regression lock for the suite; the tests additionally assert sanity
properties and hand-verified points.

| tag | input | score_cut | rows | prAUC | AUC | note |
|---|---|---|---|---|---|---|
| R1_auto | `df_detecs_val_tovlp` tpl 1 | 0.650 | 638 | 0.530 | 0.674 | **== G1 verbatim** (top score is TP, so no convention fires) |
| R2_manual_cut | same, `diag_cut = 0.30` | 0.300 | 638 | 0.530 | 0.674 | == G2 (table is cutpoint-independent) |
| R3_small_n | same, trimmed to 3 FP | 0.130 | **93** | 0.964 | 0.641 | original G3 degenerated to **1** row; now a full sweep |
| R4_aposteriori | `df_detecs_val_manual` tpl 1 | 0.430 | **620** | 0.769 | 0.915 | original G4 was 620 rows too, values re-baselined |

**Original→new divergence.** Where the highest observed score is a TP every row
has `tp >= 1`, so no 0/0 occurs and the redesigned table equals the original
`cutpointr` table verbatim (R1 == G1). The divergences are: (1) the small-n input
no longer collapses to a single row (G3: 1 → 93) — the old NaN→max + `distinct()`
artifact is gone; (2) any row whose threshold leaves only FP detections
(`tp = 0`) now reports `F1 = 0` instead of `NaN`.

## Unchanged divergences (input plumbing / hardening)

- **DVI-01 (input contract).** The refactored engine consumes the
  `validate_by_overlap` **split list** (`$detections_validated` +
  `$false_negatives`), not the wide frame, and counts FN **per template**. Fed the
  equivalent per-template data it reproduces the split-list and wide-frame results
  identically.
- **DVI-02 / DVI-04 (edge hardening).** Dead `selected` assignment removed and the
  `max(which())` empty-set crash guarded (nothing selected when `score_cut`
  exceeds every score, minus the spurious `-Inf` warning).
- **DVI-05 / DVI-08 / DVI-09 (validation/doc/style).** `match.arg(diag_method)`,
  required-column checks, roxygen rewrite, `T → TRUE`, `seq_len`.
