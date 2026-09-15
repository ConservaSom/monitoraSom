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

## Citation

If you use `monitoraSom` in your research, please cite:

```r
citation("monitoraSom")
```

## Support

- **Issues**: Report bugs and request features on [GitHub](https://github.com/ConservaSom/monitoraSom/issues)
- **Questions**: Use GitHub Discussions for general questions
- **Email**: Contact the maintainer at <gabrielrosa@conservasom.com.br>
