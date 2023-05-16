# https://r-pkgs.org/workflow101.html
library(devtools)
library(usethis)
library(pkgload)
library(available)
library(roxygen2)


load_all()
monitoraSom::fast_spectro()

roxygen2::roxygenize()

check()
devtools::dev_sitrep()
lifecycle::badge("deprecated")


available("monitoraSom")
