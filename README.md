# monitoraSom

Welcome to `monitoraSom`! This R package designed as an easy path from raw soundscape recordings to ecologiocal data. It provides a complete workflow for template matching analysis, including tools for segmentation, validation, and performance evaluation. Check the release preprint at [bioRxiv](https://www.biorxiv.org/content/10.1101/2025.06.23.661148v1) for a complete guide and overview of its funcitonalities.

## Key Features

- **Template Matching**: automated detection of animal sounds in soundscape recordings.
- **Interactivity**: graphical interfaces for manual segmentation and validation.
- **Validation Diagnostics**: quantitative assessment of detection performance.
- **Modular Design**: customizable functions supporting methodological experimentation.
- **Data Reproducibility**: ensures methodological rigor in bioacoustic studies.

View the complete template matching detection workflow vignette [in the monitoraSom documentation](https://github.com/ConservaSom/monitoraSom/blob/main/example/vignette_01_Essentials/Template_matching_monitoraSom.Rmd).

## Setup and installation

This package can be installed from GitHub on [github](http://github.com/). To install, you will need the [devtools](https://cran.r-project.org/package=devtools) package installed on your [R program](https://www.r-project.org/). The `monitoraSom` package is still under development, so be aware of potential breaking changes.

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
