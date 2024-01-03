# https://r-pkgs.org/workflow101.html
# library(usethis)
# library(pkgload)
# library(available)
# library(roxygen2)

library(devtools)
load_all()
# roxygen2::roxygenise(clean = TRUE)
# document()
# build()
check()
devtools::install()



use_gpl3_license()

use_package("dplyr")




check()
use_package("DT")
use_package("ROSE")
use_package("caret")
use_package("collapse")
use_package("cutpointr")
use_package("data.table")
use_package("dtwclust")
use_package("farver")
use_package("foreach")
use_package("furrr")
use_package("future")
use_package("ggplot2")
use_package("keys")
use_package("lubridate")
use_package("parabar")
use_package("patchwork")
use_package("pbapply")
use_package("purrr")
use_package("seewave")
use_package("shiny")
use_package("shinyBS")
use_package("shinyFiles")
use_package("shinyWidgets")
use_package("shinydashboard")
use_package("shinyjs")
use_package("shinyjqui")
use_package("slider")
use_package("stringr")
use_package("tibble")
use_package("tidyr")
use_package("tuneR")
use_package("viridis")
use_package("readxl")

# Documentation ----------------------------------------------------
document()
warnings()

# available("monitoraSom")




# Data ------------------------------------------------------------

library(dplyr)

sp_labels <- readxl::read_xlsx(
    "example/presets/MonitoraSom_UI_label_lists.xlsx"
) %>%
    as.data.frame() %>%
    glimpse()
usethis::use_data(sp_labels, overwrite = TRUE)

# Tests ------------------------------------------------------------

use_test()
