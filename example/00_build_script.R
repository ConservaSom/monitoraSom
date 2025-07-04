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

# usethis::edit_r_environ()
# _R_CHECK_USE_CODETOOLS_= FALSE
# _R_CHECK_SYSTEM_CLOCK_ = 0

# documentação
# document()
# document(roclets = c("rd", "collate", "namespace"))

# checagem da estrutura do pacote
check()

# montar um tar.gz do pacote
build()

# use_build_ignore(
#     list.files("example", recursive = TRUE),
#     escape = FALSE
# )

# instalar o pacote
devtools::install()

# install.packages("/home/grosa/R_repos/monitoraSom_0.1.1.9005.tar.gz", repos = NULL, type = "source")

# atualizar a versão do pacote
utils::packageVersion("monitoraSom")
use_version()


# licença
# use_gpl3_license()

# adicionar pacotes
# check()
# use_package("dplyr")
# use_package("dtw")
# use_package("usethis")
# use_package("stats")
# use_package("utils")
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
    "example/app_presets/sp_labels.xlsx"
) %>%
    as.data.frame() %>%
    glimpse()
# sp_labels$`CBRO-2021 (Birds - Brazil)` %>% tail()

# Focal recordings
ls_recordings_raw <- list.files("example/recordings", full.names = TRUE)
ls_recordings <- lapply(ls_recordings_raw, \(x) readWave(x))
names(ls_recordings) <- basename(ls_recordings_raw)

# Soundscape recordings
ls_soundscapes_raw <- list.files("example/soundscapes", full.names = TRUE)
ls_soundscapes <- lapply(ls_soundscapes_raw, \(x) readWave(x))
names(ls_soundscapes) <- basename(ls_soundscapes_raw)

# Templates
ls_templates_raw <- list.files("example/templates", full.names = TRUE)
ls_templates <- lapply(ls_templates_raw, \(x) readWave(x))
names(ls_templates) <- basename(ls_templates_raw)

# Roi tables
ls_roi_tables_raw <- list.files("example/roi_tables", full.names = TRUE)
ls_roi_tables <- lapply(ls_roi_tables_raw, \(x) read.csv(x))
names(ls_roi_tables) <- basename(ls_roi_tables_raw)
df_rois <- do.call(rbind, ls_roi_tables) %>%
    glimpse()

# Soundscapes metadata
df_soundscapes <- read.csv("example/soundscapes_metadata/df_soundscapes.csv") %>%
    glimpse()

# Templates metadata
df_templates <- read.csv("example/templates_metadata/df_templates.csv") %>%
    glimpse()

# Match grid metadata
df_grid <- read.csv("example/match_grid_metadata/df_grid.csv") %>%
    glimpse()

# Match scores
df_scores <- readRDS("example/match_scores/df_scores.rds") %>%
    glimpse()

# Detections
df_detecs <- read.csv("example/detections/df_detecs.csv") %>%
    glimpse()

# Validated detections
df_detecs_val_manual <- read.csv(
    "example/validation_outputs/df_detecs_val_manual.csv"
    ) %>%
    glimpse()

# Validated detections
df_detecs_val_tovlp <- read.csv(
    "example/validation_outputs/df_detecs_val_tovlp.csv"
    ) %>%
    glimpse()

{
    usethis::use_data(sp_labels, overwrite = TRUE)
    usethis::use_data(ls_recordings, overwrite = TRUE)
    usethis::use_data(ls_soundscapes, overwrite = TRUE)
    usethis::use_data(ls_templates, overwrite = TRUE)
    usethis::use_data(ls_roi_tables, overwrite = TRUE)
    usethis::use_data(df_rois, overwrite = TRUE)
    usethis::use_data(df_soundscapes, overwrite = TRUE)
    usethis::use_data(df_templates, overwrite = TRUE)
    usethis::use_data(df_grid, overwrite = TRUE)
    usethis::use_data(df_scores, overwrite = TRUE)
    usethis::use_data(df_detecs, overwrite = TRUE)
    usethis::use_data(df_detecs_val_manual, overwrite = TRUE)
    usethis::use_data(df_detecs_val_tovlp, overwrite = TRUE)
}

{
    tools::resaveRdaFiles("data/df_detecs_val_manual.rda", compress = "xz")
    tools::resaveRdaFiles("data/df_detecs_val_tovlp.rda", compress = "xz")
    tools::resaveRdaFiles("data/df_detecs.rda", compress = "xz")
    tools::resaveRdaFiles("data/df_grid.rda", compress = "xz")
    tools::resaveRdaFiles("data/df_rois.rda", compress = "xz")
    tools::resaveRdaFiles("data/df_scores.rda", compress = "xz")
    tools::resaveRdaFiles("data/df_soundscapes.rda", compress = "xz")
    tools::resaveRdaFiles("data/df_templates.rda", compress = "xz")
    tools::resaveRdaFiles("data/ls_recordings.rda", compress = "xz")
    tools::resaveRdaFiles("data/ls_roi_tables.rda", compress = "xz")
    tools::resaveRdaFiles("data/ls_soundscapes.rda", compress = "xz")
    tools::resaveRdaFiles("data/ls_templates.rda", compress = "xz")
    tools::resaveRdaFiles("data/sp_labels.rda", compress = "xz")
}

# tools::checkRdaFiles("data/df_detecs_val_manual.rda")
# tools::checkRdaFiles("data/df_detecs_val_tovlp.rda")
# tools::checkRdaFiles("data/df_detecs.rda")
# tools::checkRdaFiles("data/df_grid.rda")
# tools::checkRdaFiles("data/df_rois.rda")
# tools::checkRdaFiles("data/df_scores.rda")
# tools::checkRdaFiles("data/df_soundscapes.rda")
# tools::checkRdaFiles("data/df_templates.rda")
# tools::checkRdaFiles("data/ls_recordings.rda")
# tools::checkRdaFiles("data/ls_roi_tables.rda")
# tools::checkRdaFiles("data/ls_soundscapes.rda")
# tools::checkRdaFiles("data/ls_templates.rda")
# tools::checkRdaFiles("data/sp_labels.rda")

# Para descrever os datasets

library(monitoraSom)

data(df_detecs_val_manual)
data(df_detecs_val_tovlp)
data(df_detecs)
data(df_grid)
data(df_rois)
data(df_scores)
data(df_soundscapes)
data(df_templates)
data(ls_recordings)
data(ls_roi_tables)
data(ls_soundscapes)
data(ls_templates)
data(sp_labels)

str(df_detecs_val_manual)
str(df_detecs_val_tovlp)
str(df_detecs)
str(df_grid)
str(df_rois)
str(df_scores)
str(df_soundscapes)
str(df_templates)
str(ls_recordings)
str(ls_roi_tables)
str(ls_soundscapes)
str(ls_templates)
str(sp_labels)

# Tests ------------------------------------------------------------

use_test()



