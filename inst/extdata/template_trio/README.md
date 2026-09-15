<!-- markdownlint-disable -->
# Shared input fixture — template trio (ERC / FR / FTM)

Real-world test fixture for the refactor round covering `export_roi_cuts` (ERC),
`fetch_rois` (FR) and `fetch_template_metadata` (FTM). **This data can only be
produced by hand** — it is the output of an interactive segmentation session in
the sandbox app — so it is preserved here as a durable, version-controlled
fixture that complements the synthetic goldens of the cycle.

> ⚠️ **Do not normalize, "fix", edit or move the files in this directory.**
> Like the corpora in `data/`, several traits below are *intentional* and are
> what make the fixture useful. Treat it read-only; open the `.duckdb` with
> `read_only = TRUE`.

## Provenance

- **Created:** 2026-06-02 manual segmentation session in the sandbox segmentation
  app (`R/sandbox/run_lsa_manual_test.R`).
- **Snapshot of origin:** `R/sandbox/.lsa_tree_snapshots/diff_report_20260602-112305.txt`.
- **Copied from** the live manual-run dir `R/sandbox/lsa_manual_run/` (which is
  **rewritten on every manual app run**, hence this stable copy). Integrity:
  `rois.duckdb` and both cut WAVs are byte-identical to the originals (md5
  verified at copy time).

## Contents

```
template_trio/
├── rois.duckdb              # 161 ROIs, table `rois`, canonical 18-col schema (+ _created_at)
├── roi_cuts_legacy/         # 2 real template cuts in the LEGACY filename scheme
│   ├── …075000_009.247-011.470s_02.161-06.880kHz_1024wl_40ovlp_Myiothlypis flaveola.wav
│   └── …092000_036.225-038.483s_01.768-07.546kHz_2048wl_50ovlp_Myiothlypis flaveola.wav
├── app_presets/             # custom vocab from the session (optional; for reproducibility)
│   ├── roi_label_lists.xlsx
│   └── roi_types.xlsx
└── README.md
```

### `rois.duckdb` — what's inside (161 ROIs)

Schema: the 18 canonical columns of `.roi_schema_spec()` (`R/refactored/_schema_rois.R`),
**plus** a DB-managed `_created_at` (TIMESTAMP). Note `roi_channel` sits **after**
`_created_at` because it was added by `ALTER TABLE` (LSA-109) on a pre-existing
DB — this is realistic and faithful to what the app writes; `.coerce_rois()`
reorders to canonical order and drops the stray `_created_at`.

Intentional diversity (verified):

| Trait | Values | Exercises |
|---|---|---|
| Recorders / path styles | audiomoth: **absolute** path, `.WAV` (15 files); SM4: **relative** `data/soundscapes_SM4/Data/…`, `.wav` (6 files) | FR-06 (path normalization), FR-101 multi-soundscape union |
| Species (`roi_label`) | *Myiothlypis flaveola* 89 (focal), *Turdus leucomelas* 22, *Leptotila verreauxi* 21, *Saltator similis* 15, *Cyclarhis gujanensis* 14 | realistic label set, label with a space |
| `roi_channel` | mono 113, right 22, left 19, **NA 7 (legacy, pre-LSA-109)** | ERC-105 channel-correct cut; NA→default |
| `roi_comment` | `"template"` × 4, **`"teste"` × 1 (typo/decoy)** | template selection by the observations field; exact-match filter |
| `roi_type` (free text) | "" 91, `canto` 63, `song` 7 | custom types (LSA-101) survive as free text |
| `roi_wl` / `roi_ovlp` | 2048/50 ×152, 1024/40 ×7, **1024/0 ×2 (ovlp=0 edge)** | window/overlap variety |
| `no_soi` sentinel | none | — |

### `roi_cuts_legacy/` — 2 real template WAVs

Both *Myiothlypis flaveola*, mono, 48 kHz; named in the **legacy filename scheme**
that the current `export_roi_cuts` encodes (the scheme ERC-101 replaces). They are
the materialized cuts of 2 of the 4 `template`-marked ROIs (the app's LSA-110
auto-export fires on file advance, so only the last-advanced files were cut).

## `template`-marked rows → source recordings

All 4 source recordings exist on disk under `data/` (already-versioned corpora).

| soundscape_file | roi_label | bounds (s) | wl/ovlp | channel | cut materialized? |
|---|---|---|---|---|---|
| `…075000.WAV` | Myiothlypis flaveola | 9.247–11.470 | 1024/40 | **NA** (legacy) | ✅ in `roi_cuts_legacy/` |
| `…080000.WAV` | Myiothlypis flaveola | 26.944–28.927 | 1024/0 | mono | ❌ marked, not cut |
| `…081000.WAV` | Myiothlypis flaveola | 9.253–11.502 | 2048/50 | mono | ❌ marked, not cut |
| `…092000.WAV` | Myiothlypis flaveola | 36.225–38.483 | 2048/50 | mono | ✅ in `roi_cuts_legacy/` |
| `…141000.wav` (SM4) | Turdus leucomelas | 0.000–1.960 | 2048/50 | right | decoy `roi_comment = "teste"` |

The "marked but not cut" rows (080000, 081000) give a realistic ERC scenario; the
`"teste"` decoy verifies the selection filter is exact (`"template"`, not a
substring/typo).

## How the trio's tests can use this fixture

Reuse existing readers — do not reimplement DuckDB I/O:
`.roi_duckdb_connect()` / `.roi_duckdb_read()` (`R/refactored/_roi_duckdb.R`),
`.coerce_rois()` / `.roi_schema_spec()` (`R/refactored/_schema_rois.R`). Tests
resolve paths relative to the project root via `setwd(proj_root)`, as in
`R/refactored/tests/testthat/test-fetch_soundscape_metadata.R`.

| Use | Function | Notes |
|---|---|---|
| Read `rois.duckdb` → canonical `df_rois` | **FR** (FR-101/102) | multi-soundscape union, 5 species, 2 path styles, NA-channel default |
| Cut `template`-marked focal ROIs from `data/` sources | **ERC** | compute `origin_soundscape_sha256` (source) + `template_sha256` (cut) |
| Sample-level audio equivalence vs the 2 legacy WAVs | **ERC** (golden) | refactored cut of 075000 / 092000 must equal the hand-exported WAV, regardless of the new naming |
| Channel-correct cut on stereo SM4 + left/right ROIs | **ERC** (ERC-105) | audiomoth is mono → passthrough |
| Parse legacy names → manifest | **FTM** (FTM-101 `legacy_filename`) | label with a space ("Myiothlypis flaveola") tests FTM-02; width-agnostic tests FTM-03 |
| Round-trip ERC manifest → `df_templates` | **FTM** (FTM-101/102) | canonical `template_*` schema |
| Thumbnails / reference resolver | **FTM** (FTM-103/104) | `catalog_templates`; resolve via `origin_soundscape_path` + sha256 verify |

This is a **real-world integration fixture**, complementary to the cycle's step-1
goldens (which run the **original** functions on representative inputs, incl. the
legacy CSV form for FR). It gives the test suite the *option* to validate
refactored behaviour against data that only exists because it was created by hand.
