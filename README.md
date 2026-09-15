# monitoraSom

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/monitoraSom)](https://CRAN.R-project.org/package=monitoraSom)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

Welcome to `monitoraSom`! This R package is designed as an easy path from raw soundscape recordings to ecological data. It provides a complete workflow for template matching analysis, including tools for segmentation, validation, and performance evaluation. Check the published article at [Bioacoustics](https://www.tandfonline.com/doi/full/10.1080/09524622.2026.2642040) for a complete guide and overview of its functionalities (preprint also available at [bioRxiv](https://www.biorxiv.org/content/10.1101/2025.06.23.661148v1)).

## Key Features

- **Template Matching**: automated detection of animal sounds in soundscape recordings.
- **Interactivity**: graphical interfaces for manual segmentation and validation.
- **Validation Diagnostics**: quantitative assessment of detection performance.
- **Modular Design**: customizable functions supporting methodological experimentation.
- **Data Reproducibility**: ensures methodological rigor in bioacoustic studies.

The package ships a getting started vignette that walks the whole workflow and
runs the validation and diagnostics steps on bundled data:

```r
vignette("monitoraSom")
```

## Setup and installation

This package can be installed from [GitHub](https://github.com/ConservaSom/monitoraSom). To install, you will need an R version 4.1.0 or newer and [devtools](https://cran.r-project.org/package=devtools) package installed on your [R program](https://www.r-project.org/). The `monitoraSom` package is still under development, so be aware of potential breaking changes.

**Note for Windows users**: Ensure you have Rtools compatible with your R version installed. Download from [CRAN](https://cran.r-project.org/bin/windows/Rtools/).

Please follow the code below to install the latest released version:

```r
devtools::install_github("ConservaSom/monitoraSom", dependencies = TRUE)
```

## The example project

The package has a complete example project: 14 soundscape and recording WAV
files of the bird *Basileuterus culicivorus*, a database of manually drawn
ROIs, and a walkthrough script. The package would grow too large if all of
this travelled inside it, so the example is a separate download of about
39 MB, hosted in
[ConservaSom/monitoraSom-example-data](https://github.com/ConservaSom/monitoraSom-example-data).

Get it with one call:

```r
library(monitoraSom)

is_example_data_cached()                # FALSE on a fresh install
example_project <- fetch_example_data() # downloads once
setwd(example_project)                  # or pass it as project_path
```

The first call downloads the project to a cache folder on your computer
(`tools::R_user_dir("monitoraSom", "cache")`) and unpacks it. The download
happens only once: later calls find the files there and return the same
path, and the folder survives R restarts. To download again, pass
`overwrite = TRUE`. If the download fails, the function prints a message
and returns `NULL`. It never raises an error.

If you cannot reach the internet, or prefer your own copy, point the
function at a local bundle first:

```r
options(monitoraSom.data_url = "file:///path/to/monitoraSom-example-basileuterus-culicivorus.tar.gz")
example_project <- fetch_example_data()
```

The walkthrough inside the project (`basileuterus-culicivorus.Rmd`, also in
`.qmd` and `.R`) repeats the whole pipeline on these files.
`vignette("monitoraSom")` runs the validation and diagnostics steps on the
bundled result tables, and it needs no download.

## Upgrading from 1.0.x

Version 1.2.0 brings the largest change in the package so far. The function
names you know are mostly the same, but files created with earlier versions
cannot be used by the new one. Read this section before you upgrade.

### What changed

- **Where results are saved.** Earlier versions saved detections, ROIs,
  metadata, templates and validations as CSV files. The package now saves
  them in DuckDB database files. A DuckDB database is a single file that
  holds many tables, and you read those tables back from R.
- **How matching scores are computed.** `run_matching()` now uses a much
  faster method by default. Scores are also on one shared scale: earlier
  versions scaled each recording against its own maximum, so scores from
  different recordings could not be compared. Now they can.
- **How the DTW method measures distance.** The DTW (dynamic time warping)
  scores changed. Any threshold value that worked before needs to be chosen
  again.

### What you must redo

- Run the matching again. Old scores and new scores are not comparable.
- Choose your DTW thresholds again.
- Download the example data with `fetch_example_data()` if you used it.
  The data no longer ships inside the package. It downloads once and is
  saved on your computer.

### Moving your earlier results into the new version

Four functions read the CSV files from earlier versions and write them into
the new databases:

```r
migrate_metadata_csv_to_duckdb()    # soundscape metadata
migrate_templates_to_db()           # template database
migrate_rois_csv_to_duckdb()        # ROI tables
migrate_detections_csv_to_duckdb()  # detections
```

During the ROI migration, full file paths stored in your tables become
relative to your project folder, as the new version expects.

The `export_*_duckdb_to_csv()` functions write tables back to CSV files
whenever you need them. You can also read and write annotations from Raven
(`read_raven_selection()`, `write_raven_selection()`) and Audacity
(`read_audacity_labels()`, `write_audacity_labels()`).

For the complete list of changes, see [NEWS.md](NEWS.md). The help page
`?monitoraSom-package` explains the differences in detail, and
`vignette("monitoraSom")` walks the new workflow.

### Staying on version 1.0.2

If you are not ready to move, you can install the previous version from the
same repository:

```r
devtools::install_github("ConservaSom/monitoraSom@v1.0.2")
```

A project made with 1.0.2 uses CSV files, and the new version does not read
them directly. When you decide to move, the migration functions above bring
those files into the new databases.

## Citation

If you use `monitoraSom` in your research, please cite:

```r
citation("monitoraSom")
```

## Support

- **Issues**: Report bugs and request features on [GitHub](https://github.com/ConservaSom/monitoraSom/issues)
- **Questions**: Use GitHub Discussions for general questions
- **Email**: Contact the maintainer at <gabrielrosa@conservasom.com.br>
