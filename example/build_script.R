# https://r-pkgs.org/workflow101.html
library(devtools)
library(usethis)
library(pkgload)
library(available)
library(roxygen2)

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

roxygen2::roxygenize()


library(devtools)
check()
build()
devtools::install()

# load_all()



available("monitoraSom")
