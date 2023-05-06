# https://r-pkgs.org/workflow101.html
library(devtools)
library(usethis)
library(pkgload)
library(available)


load_all()
monitoraSom::fast_spectro()

check()
devtools::dev_sitrep()
lifecycle::badge("deprecated")


available("monitoraSom")
