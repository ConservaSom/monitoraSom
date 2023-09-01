# https://r-pkgs.org/workflow101.html
# library(usethis)
# library(pkgload)
# library(available)
# library(roxygen2)

library(devtools)
roxygen2::roxygenize()
check()
document()
build()
devtools::install()


# ‘dplyr::group_rows’ by ‘kableExtra::group_rows’
# ‘collapse::is.Date’ by ‘lubridate::is.Date’
# ‘foreach::when’ by ‘purrr::when’
# ‘foreach::accumulate’ by ‘purrr::accumulate’
# ‘parabar::export’ by ‘seewave::export’
# ‘lubridate::duration’ by ‘seewave::duration’
# ‘DT::dataTableOutput’ by ‘shiny::dataTableOutput’
# ‘DT::renderDataTable’ by ‘shiny::renderDataTable’
# ‘shiny::runExample’ by ‘shinyalert::runExample’
# ‘shinyBS::closeAlert’ by ‘shinyalert::closeAlert’
# ‘shinyWidgets::alert’ by ‘shinyjs::alert’
# ‘shinyalert::runExample’ by ‘shinyjs::runExample’
# ‘lubridate::show’ by ‘shinyjs::show’
# ‘shinyjs::show’ by ‘tuneR::show’


# # https://uptake.github.io/pkgnet/articles/pkgnet-intro.html
# library(pkgnet)
# report2 <- CreatePackageReport(
#   pkg_name = "monitoraSom", pkg_path = "/"
# )

load_all()
use_gpl3_license()

use_package("dplyr")
use_package("progressr")
use_package("here")
use_package("collapse")
use_package("purrr")
use_package("furrr")
use_package("av")
use_package("tuneR")
use_package("seewave")
use_package("data.table")
use_package("dtwclust")
use_package("slider")
use_package("fftw")
use_package("viridis")
use_package("ggplot2")
use_package("ROSE")
use_package("caret")
use_package("cli")
use_package("cowplot")
use_package("cutpointr")
use_package("dtw")
use_package("farver")
use_package("openxlsx")
use_package("pbapply")
use_package("readxl")
use_package("shiny")
use_package("shinyBS")
use_package("shinyFiles")
use_package("shinyjs")
use_package("stringr")
use_package("tibble")
use_package("tidyr")

# document()



# load_all()



available("monitoraSom")




# dependencias ------------------------------------------------------------

pak::pkg_deps_tree("progressr")
pak::pkg_deps_tree("here")
pak::pkg_deps_tree("collapse")

