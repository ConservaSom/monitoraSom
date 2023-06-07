# https://r-pkgs.org/workflow101.html
library(devtools)
library(usethis)
library(pkgload)
library(available)
library(roxygen2)


load_all()
roxygen2::roxygenize()
check()
build()

create_package()
# usethis::use_build_ignore("example/")
# usethis::use_build_ignore(".vscode/")

check()
devtools::dev_sitrep()
lifecycle::badge("deprecated")


available("monitoraSom")

# library(monitoraSom)
# launch_segmentation_app_v2(
#   project_path = "teste/", soundscapes_path = "/home/grosa/R_home/20230526_monitoraSom_example/soundscapes/"
#
# )
