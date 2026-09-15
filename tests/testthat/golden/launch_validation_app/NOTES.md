# Golden NOTES — `launch_validation_app` (flow #14, prefix LVA)

Generator: `R/sandbox/launch_validation_app.R` (patches `shiny::shinyApp` →
`return(session_data)` so input validation runs without launching the UI, then
tests various argument combinations). Baseline:
`R/original/alt_validation_app/launch_validation_app.R` (3677 lines, LVA-100
pre-applied 2026-06-17).

Fixtures: temporary directories created under `tempdir()`/`lva_golden_fixtures/`
— a minimal valid CSV (header only), dummy WAV files in `soundscapes/` and
`templates/`, and a fake player executable. Regenerated each run; never committed.

## Frozen behaviour (reference — the alt_validation_app baseline with LVA-100)

### G1 — Input validation errors (`G1_input_validation_errors.rds`)

14 test cases exercising `stop()` paths in the function-argument validation.
Each entry captures the error message (or `NA` if none) plus any warnings.

| Case | Expected behaviour |
|---|---|
| NULL_input_path | Error (NULL dereference — **LVA-03**, to be fixed in refactor) |
| nonexistent_input_path | Error ("must exist, be a CSV file, and not be empty") |
| nonexistent_soundscapes | Error ("path to the soundscape wave files was not found") |
| invalid_val_subset | Error ("not within the accepted alternatives") |
| invalid_wl | Error ("must be numeric and among the expected alternatives") |
| invalid_ovlp_negative | Error ("must be a numeric value between 0 and 80") |
| invalid_color_scale | Error ("must be one of the following: viridis, magma, …") |
| invalid_wav_player_type | Error ("selected WAV player method is not valid") |
| external_player_no_exe | Error ("path informed in 'wav_player_path' was not found") |
| zoom_freq_out_of_range | Error ("values must be between 0 and 192") |
| non_numeric_subset_seed | Error ("Non-numeric value input provided to 'seed'") |
| non_logical_visible_bp | Error ("is not logical. Set it to TRUE or FALSE.") |
| time_pads_out_of_range | Error ("must be a numeric value between 0 and 16") |
| pitch_shift_invalid | Error ("is not numeric or not among the expected alternatives") |

### G2 — Input validation warnings (`G2_input_validation_warnings.rds`)

8 test cases exercising non-blocking validation paths (warnings + session_data
structure). Each entry captures the resulting `session_data` subset plus
warnings/errors.

| Case | Expected behaviour |
|---|---|
| NULL_validation_user | Warning about missing user; `validation_user = NA_character_` |
| templates_absent_LVA100 | **LVA-100**: warning "No template wave files were found", `templates_available = FALSE`, no error |
| templates_empty_dir_LVA100 | **LVA-100**: warning about missing WAVs, `templates_available = FALSE`, no error |
| templates_present_LVA100 | **LVA-100**: `templates_available = TRUE`, `templates_path` set, no template warning |
| zoom_freq_reversed | Warning about sorting; values swapped |
| zoom_freq_rounded | Warning about rounding to 0.1 intervals |
| dyn_range_reversed | Warning about sorting; values swapped |
| output_path_null | Warning that output = input; `output_path` copied from `input_path` |

### G3 — Valid session_data structure (`G3_valid_session_data.rds`)

4 test cases exercising successful validation → `session_data` list structure.

| Case | Description |
|---|---|
| defaults_with_templates | Default settings + templates present → 30 fields |
| defaults_no_templates_LVA100 | Default settings + no templates → 29 fields (`templates_path` absent), `templates_available = FALSE` |
| custom_settings | Custom values for all spectrogram/navigation params → 30 fields |
| external_player_valid | External player with valid executable → 31 fields (includes `wav_player_path`) |

### G4 — Pure helpers (`G4_pure_helpers.rds`)

Behaviour of `validate_dyn_range` and `validate_and_set_path` for various inputs.
Captures return values, errors, and warnings (via `capture_conditions`).

## Intentional divergences (Round 1 refactor)

The refactor will change these behaviours; the testthat step asserts the
divergences as DERIVED expectations (not verbatim equality with goldens).

| DEBT item | Golden(s) affected | Expected change |
|---|---|---|
| **LVA-03** | G1 NULL_input_path | Clear guard message instead of cryptic "invalid 'file' argument" |
| **LVA-04** | G3 defaults_* | `soundscapes_path` assigned only after validation (internal; G3 unchanged) |
| **LVA-05** | (server-side, not in goldens) | Dead NA logic removed |
| **LVA-06** | (server-side, not in goldens) | `order_by` DESC options fixed via `decreasing = TRUE` |
| **LVA-10** | (server-side, not in goldens) | `end_session` compares `df_output()` not `df_cut()` |
| **LVA-11** | (server-side, not in goldens) | `anti_join` includes `detection_id` in key |
| **LVA-12** | (server-side, not in goldens) | `wav_filename`/`spec_filename` initialized |
| **LVA-13** | (server-side, not in goldens) | "TP" twice → "TP" and "FP" check |
| **LVA-24** | G4 set_path_null_* | Warning text changed to distinguish "created" vs "existing" |
| **LVA-26** | G2 zoom_freq_* | Sort first, then range-check, then round unconditionally |
| **LVA-15** | (server-side, not in goldens) | Full-soundscape spectrogram view removed |
| **LVA-02** | (server-side, not in goldens) | `abs(pitch_shift)` on all paths; non-HTML players disabled |
| **LVA-27** | structural | Pure helpers extracted to `_*.R`; app assembled via `_assemble_lva_app.R` |

**LVA-100 is pre-applied in the baseline** — no divergence expected; the goldens
already capture the optional-templates behaviour.

**Items explicitly NOT changing in Round 1 (Round 2):** LVA-14, LVA-16, LVA-22,
LVA-28, LVA-30, LVA-31, LVA-32.

## Why no full-app goldens

`launch_validation_app` is a Shiny app; its server logic runs inside reactive
observers that require a Shiny session. The goldens capture the **non-reactive**
surface (input validation + pure helpers + `session_data` structure), which is
the layer most affected by Round 1 bugs. Server-side changes (LVA-05/06/10/11/
12/13/15/02) are tested in the testthat step via extracted helpers and
data-contract assertions, following the LSA precedent (no `shinytest`, no pixel
goldens).
