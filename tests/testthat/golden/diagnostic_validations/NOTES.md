# Golden NOTES — `diagnostic_validations` (flow #15 wrapper; audit CQ-06)

Generator: `R/sandbox/diagnostic_validations.R` (sources `R/refactored/`,
frozen 2026-08-29, audit triage Lote 1). Regenerate deliberately — a re-run
re-baselines silently otherwise.

Scope: the WRAPPER assembly contract — per-template slot names, `score_cut`,
diagnostics dimensions + column names, plot classes — on both input paths:

- `W1_wide_summary.rds` — wide frame, fixture `data/df_detecs_val_tovlp.rda`
  (a-priori, committed), `pos_prob = 0.90`. 6 templates.
- `W2_vbo_summary.rds` — `validate_by_overlap` split list built from the VBO
  golden inputs (`tests/golden/validate_by_overlap/input_{detecs,rois}.rds`),
  `validation_user = "T"`, `pos_prob = 0.90`. 6 templates.

Engine-level numbers (diagnostics table values, GLM coefficients) are locked
by the `diagnostic_validations_i` goldens (`redesigned/`); this golden only
catches split/name/assembly drift — the wrapper adds no statistics. No RNG.

Consumed by `test-diagnostic_validations.R` ("golden" test). The LSA half of
audit CQ-06 is out of scope here (interactive app, no stable oracle — audit
§12-E2).
