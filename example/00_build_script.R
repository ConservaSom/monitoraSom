# https://r-pkgs.org/workflow101.html
# library(usethis)
# library(pkgload)
# library(available)
# library(roxygen2)

# install.packages("devtools", dependencies = TRUE)
library(devtools)
library(usethis)
library(roxygen2)

roxygen2::roxygenise(clean = TRUE)
# usethis::edit_r_environ() # _R_CHECK_USE_CODETOOLS_= FALSE para bugs do shiny

# para identificar caracteres não ASCII
tools::showNonASCIIfile("R/launch_segmentation_app.R")

# documentação
document()
document(roclets = c("rd", "collate", "namespace"))

# checagem da estrutura do pacote
check()

# montar um tar.gz do pacote
# build()

# instalar o pacote
devtools::install()

# install.packages("/home/grosa/R_repos/monitoraSom_0.1.1.9005.tar.gz", repos = NULL, type = "source")

# atualizar a versão do pacote
utils::packageVersion("monitoraSom")
use_version()

# licença
use_gpl3_license()

# adicionar pacotes
# check()
# use_package("dplyr")
# use_package("dtw")
# use_package("dtwclust")
# use_package("rstudioapi")
# use_package("DT")
# use_package("ROSE")
# use_package("caret")
# use_package("cutpointr")
# use_package("data.table")
# use_package("dtwclust")
# use_package("farver")
# use_package("foreach")
# use_package("furrr")
# use_package("future")
# use_package("keys")
# use_package("ggplot2")
# use_package("lubridate")
# use_package("parabar")
# use_package("patchwork")
# use_package("pbapply")
# use_package("purrr")
# use_package("seewave")
# use_package("shiny")
# use_package("shinyBS")
# use_package("shinyWidgets")
# use_package("shinydashboard")
# use_package("shinyjs")
# use_package("shinyjqui")
# use_package("slider")
# use_package("stringr")
# use_package("tibble")
# use_package("tidyr")
# use_package("tuneR")
# use_package("viridis")
# use_package("readxl")
# use_package("openxlsx")

# available("monitoraSom")


# Data ------------------------------------------------------------

library(dplyr)
library(tuneR)

# Species lists
sp_labels <- readxl::read_xlsx(
    "example/000_app_presets/sp_labels.xlsx"
) %>%
    as.data.frame() %>%
    glimpse()
# sp_labels$`CBRO-2021 (Birds - Brazil)` %>% tail()
usethis::use_data(sp_labels, overwrite = TRUE)

# Soundscape recordings
ls_soundscapes_raw <- list.files("example/010_soundscapes", full.names = TRUE)
ls_soundscapes <- lapply(ls_soundscapes_raw, \(x) readWave(x))
names(ls_soundscapes) <- basename(ls_soundscapes_raw)
usethis::use_data(ls_soundscapes, overwrite = TRUE)

# Templates
ls_templates_raw <- list.files("example/040_roi_cuts", full.names = TRUE)
ls_templates <- lapply(ls_templates_raw, \(x) readWave(x))
names(ls_templates) <- basename(ls_templates_raw)
usethis::use_data(ls_templates, overwrite = TRUE)

# Roi tables
ls_roi_tables_raw <- list.files("example/030_roi_tables", full.names = TRUE)
ls_roi_tables <- lapply(ls_roi_tables_raw, \(x) read.csv(x))
names(ls_roi_tables) <- basename(ls_roi_tables_raw)
usethis::use_data(ls_roi_tables, overwrite = TRUE)
df_rois <- do.call(rbind, ls_roi_tables) %>%
    glimpse()
usethis::use_data(df_rois, overwrite = TRUE)

# todo - construir novamente o df_grid com os caminhos corretos

# Soundscapes metadata
df_soundscapes <- read.csv("example/020_soundscapes_metadata/df_soundscapes.csv") %>%
    # mutate(
    #     soundscape_path = gsub(
    #         "soundscapes", "010_soundscapes", soundscape_path
    #     )
    # ) %>%
    glimpse()
usethis::use_data(df_soundscapes, overwrite = TRUE)

# Templates metadata
df_templates <- read.csv("example/050_templates_metadata/df_templates.csv") %>%
    # mutate(
    #     template_path = gsub(
    #         "roi_cuts", "040_roi_cuts", template_path
    #     )
    # ) %>%
    glimpse()
usethis::use_data(df_templates, overwrite = TRUE)

# Match grid metadata
df_grid <- read.csv("example/060_match_grid_metadata/df_grid.csv") %>%
    glimpse()
usethis::use_data(df_grid, overwrite = TRUE)

# Match scores
df_scores <- readRDS("example/070_match_scores/df_scores.rds") %>%
    glimpse()
usethis::use_data(df_scores, overwrite = TRUE)

# Detections
df_detecs <- read.csv("example/080_detections/df_detecs.csv") %>%
    glimpse()
usethis::use_data(df_detecs, overwrite = TRUE)

# Validated detections
df_detecs_val_manual <- read.csv(
    "example/110_validation_outputs/df_detecs_val_manual.csv"
    ) %>%
    glimpse()
usethis::use_data(df_detecs_val_manual, overwrite = TRUE)

# Validated detections
df_detecs_val_tovlp <- read.csv(
    "example/110_validation_outputs/df_detecs_val_tovlp.csv"
    ) %>%
    glimpse()
usethis::use_data(df_detecs_val_tovlp, overwrite = TRUE)


# Tests ------------------------------------------------------------

use_test()
