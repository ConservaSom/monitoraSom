# Golden NOTES — `plot_scores` (flow #17, prefix PS, stage `visualization`)

Generator: `R/sandbox/plot_scores.R` (sources `R/original/plot_scores.R` +
`R/original/fetch_score_peaks_i.R` + `R/original/fast_spectro.R`, capturing the
ORIGINAL behaviour faithfully — bugs included). Fixtures: the committed example
datasets `data(df_scores)` (the 72-row scores tibble) and `data(ls_soundscapes)`
(the backing WAVs); never mutated. Each chosen row is copied and its
`soundscape_path` repointed to a tempfile WAV (the repo root has no
`./soundscapes/` dir by layout policy). Determinism: `fetch_score_peaks_i` +
`fast_spectro` are deterministic, no RNG.

## What is (and isn't) frozen

`plot_scores` returns a **2-panel `patchwork`** for human inspection. The project
renv has **no `vdiffr`**, so these goldens **do not pixel-compare** the plot.
They freeze the deterministic **data contract** the refactor must preserve:

Per case `<G>` (all on row 20, the docstring's canonical Bcu example):
- `<G>_detecs.rds` — the detections the **original** `fetch_score_peaks_i`
  produced for the case's filter args (the data driving both panels).
- `<G>_summary.rds` — `caption`, default `zoom_score`/`zoom_time`/`zoom_freq`,
  `buffer_resolved`, `selection_color`, the returned object's `class` + panel
  count, and the captured `warnings`/`messages`/`error`.
- `MANIFEST.csv` — one row per case.

| case | params | detecs (orig) | buffer | sel_color |
|---|---|---|---|---|
| G1_default | defaults | 54 | 36 | white |
| G2_filtered | min_score=0.3, min_quant=0.9, ovlp=50, wl=512 | 4 | 36 | white |
| G3_nofilter | buffer_size=0, all filters NULL | 684 | 0 | white |
| G4_zoom | min_score=0.3, zoom_freq=c(2,10), zoom_time=c(20,25) | 4 | 36 | white |
| G5_greyscale | color_scale="greyscale 1", min_score=0.3, min_quant=0.9 | 4 | 36 | black |
| G6_top_n | top_n=5 | 5 | 36 | white |

All six return `class` ⊇ `patchwork` with **2 panels**. G1 additionally emits the
ggplot2 `size`→`linewidth` deprecation **warning** — a **session-once** artifact
(not asserted; it does not reproduce deterministically). Every case emits the
**"Coordinate system already present"** message (see PS-07 below).

## Intentional divergences (asserted as DERIVED in the testthat step)

These goldens are the **original-behaviour witness**. The refactor deliberately
changes the following (DEBT item IDs in brackets); the testthat step asserts the
**new** behaviour structurally against the **refactored** function, not verbatim
equality with these goldens.

- **PS-13 (decoupled detection — the big one).** The refactored
  `fetch_score_peaks_i` only **detects** peaks; filtering is the separate
  `filter_detections()` step. The refactored `plot_scores` composes
  `fetch_score_peaks_i(row, buffer_size)` + `filter_detections(min_score,
  min_quant, top_n, scope="pair")`. Because the refactored detector keeps the
  plateau/tie/edge peaks the original dropped (its own FSP divergences), the
  **unfiltered** cases yield **more** detections than the witness (G1: 54→91,
  G3: 684→691); the **filtered** cases (G2/G4/G6) match the witness **exactly**.
  → The test asserts `plot_scores`'s detections equal the **refactored**
  composition (delegation), and that filtered cases also equal these goldens.
- **PS-05 / PS-07 (fast_spectro rewire + double-coord removal).** The original
  builds the spectro panel with the old `fast_spectro(f=…)` API and a trailing
  `coord_cartesian`, which triggers the captured **"Coordinate system already
  present"** message. The refactor passes `flim`/`tlim` to the refactored
  `fast_spectro` and drops the redundant coord → the message **disappears**. The
  test asserts the refactored call emits **no** such message.
- **PS-06 (axis-blank ordering).** The original's `theme_bw()` after the
  axis-blanking `theme()` re-shows the top panel's x-axis. The refactor hides the
  top x-axis effectively (asserted on the rebuilt panel's theme).
- **PS-01 (n_colors).** The original hardcodes `n_colors=124`; the refactor
  forwards the user's `n_colors`. Rendering-only — asserted by checking the value
  reaches `fast_spectro`, not via these data goldens.
- **PS-02 / PS-03 (colormap / color_scale).** Dead `colormap` removed;
  `selection_color` (greyscale→black, else white — captured per case) preserved;
  `color_scale` gains `match.arg` (new error path; the original silently accepts).
- **PS-08 (single-row guard).** New `nrow(df_scores_i)==1` error (the original
  silently used row 1).
- **PS-09 (`...` forwarding) / PS-11 (`@return` doc).** `...` now forwarded to
  `fast_spectro`; `@return` clarified as a patchwork.
- **PS-12 (presentation params).** Six new `fast_spectro` passthrough args
  (Option A) — backward-compatible; asserted by their reaching the spectro panel.

## Reproduced verbatim (the refactor must NOT change these)

The `plot_scores`-**owned** scalars are unaffected by the upstream rewire and must
match the goldens exactly: the **filter `caption`** string, the default
`zoom_score` (= `range(score_vec)`), `zoom_time` (= `c(0, dur)`), `zoom_freq`
(= `c(0, nyquist_khz)`), `buffer_resolved` (`"template"`→`score_sliding_window`),
`selection_color`, and the **2-panel patchwork** structure.
