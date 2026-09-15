# Golden notes — `run_matching_i_fft` (no ORIGINAL golden — criterion: reproduce `cor`)

The original `R/original/run_matching_i.R` has **no `fft` path** (`score_method`
was only `"cor"`/`"dtw"`; the FFT prototype lived in
`R/original/alt_run_matching/raw_fft_matching.R` and was found NOT to implement
Lewis — it omitted mean-centring). There is therefore **no faithful original
behaviour to capture** for `fft`, so this directory holds **no `.rds` golden** —
only this note.

The Stage-4 gate (`plans/plan-logs/plan-log-2026-06-07-001.md`) fixed the `fft` theoretical
basis as **Lewis (1995) mean-centred normalized cross-correlation**: `fft` is the
**fast backend for `cor`** and must **reproduce `cor` within numerical
tolerance**. Its conformance reference is therefore the **refactored `cor`**
engine (`golden/run_matching_i_cor/`, the verified Pearson reference), computed
in-test — not a stored original golden.

## Acceptance criteria (asserted in `test-run_matching_i_fft.R`)
- **RMF-01..04 (Lewis basis).** `.score_fft(...) == .score_cor(...)` within
  tolerance (~1e-8; observed ~6e-14) on both trio ground-truth pairs. The FFT
  obtains only the **cross term** by frequency-domain cross-correlation along the
  **time axis**; the mean-centred normalization is the shared
  `.pearson_windows_from_cross` combine, so the result is the **same Pearson
  score** as `cor`.
- **RMF-05 (alignment).** Through the dispatcher, `fft` is padded with the **same
  geometry as `cor`** → the detection peak lands at the **same frame/time** as
  `cor` (no half-template start-vs-center offset). Asserted by equal peak time.
- **RMF-06 (non-finite guard).** Non-finite spectrogram values raise a clear
  error instead of producing a silent `NaN` correlation.
- **RMF-13 (nextn perf path).** `.score_fft` zero-pads to
  `stats::nextn(n, c(2,3,5))` before `mvfft` so an `n` with a large prime factor
  does not fall back to a slow DFT; the padded valid region still equals `cor`.

## Fixtures
The 2 trio ground-truth pairs — `[[project_template_trio_fixture]]`.
Toolchain at writing: R 4.6.0 (base `stats::mvfft`/`fft`/`nextn`; no extra pkg).
