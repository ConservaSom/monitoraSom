# https://r-pkgs.org/workflow101.html
library(devtools)
library(usethis)
library(pkgload)
library(available)
library(roxygen2)


load_all()
roxygen2::roxygenize()
# usethis::use_build_ignore("example/")
# usethis::use_build_ignore(".vscode/")

check()
devtools::dev_sitrep()
lifecycle::badge("deprecated")


available("monitoraSom")

