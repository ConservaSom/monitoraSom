## Submission

This is the first submission of monitoraSom to CRAN.

## Test environments

* Local: Ubuntu 24.04, R 4.6.1 (`R CMD check --as-cran`)

## R CMD check results

0 errors | 0 warnings | 1 note

The single note is the expected first-submission note:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Gabriel L. M. Rosa <gabrielrosa@conservasom.com.br>'
New submission
```

## Notes for the reviewer

* **Examples.** The package implements a bioacoustic template-matching
  pipeline whose realistic examples require a corpus of field audio recordings
  that is too large to bundle under CRAN's data-size limits. Those pipeline
  examples (e.g. `run_matching()`, `template_matching()`,
  `fetch_soundscape_metadata()`, `fetch_template_metadata()`) are wrapped in
  `\dontrun{}` because they cannot execute without that corpus. Examples that
  operate on the small bundled datasets (`data/`) or the tiny fixtures under
  `inst/extdata/` are left runnable and pass under `--run-donttest`.

* **Interactive apps.** `launch_segmentation_app()` and
  `launch_validation_app()` open Shiny applications; their examples are guarded
  with `if (interactive())`.

* **Suggested packages.** `dtw`, `dtwclust` and `fftw` are used only on
  optional code paths and are guarded so the package checks cleanly when they
  are absent.

* **URLs.** Two URLs in `README.md` may be reported as unreachable by automated
  checks but are valid:
  * `https://CRAN.R-project.org/package=monitoraSom` (the canonical CRAN badge
    link) returns 404 only because this is the first submission and the package
    page does not exist yet; it resolves once the package is on CRAN.
  * The bioRxiv preprint link returns 403 to automated agents because of
    bioRxiv's bot protection, but is reachable from a browser.
  * The Bioacoustics article link
    (`https://www.tandfonline.com/doi/full/10.1080/09524622.2026.2642040`)
    may likewise return 403 to automated agents (Taylor & Francis bot
    protection) but is reachable from a browser.

## Main reference

The method paper is published (open access) as:

  Rosa, G. L. M.; Zurano, J. P.; Torres, I. M. D.; Simoes, C. R.; dos Anjos,
  L.; de Araujo, C. B. (2026). MonitoraSom: an easy path from soundscapes to
  ecology. Bioacoustics 35(3), 257-276.
  doi:10.1080/09524622.2026.2642040

The bioRxiv preprint (2025, doi:10.1101/2025.06.23.661148) remains available
as an earlier version.

## Downstream dependencies

There are currently no downstream dependencies (new package).
