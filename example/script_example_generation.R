########### Demonstration of the complete workflow ################################

## Preparando o ambiente
# Carregando os pacotes necessários

library(dplyr)
library(progressr)
library(here)
library(collapse)
library(purrr)
library(furrr)
library(av)
library(tuneR)
library(seewave)
library(data.table)
library(dtwclust)
library(slider)
library(ggplot2)
library(farver)
library(parallel)
library(doParallel)
library(parabar)
library(monitoraSom)
library(warbleR)


# Para o pacote here funcionar corretamente, é necessário que este script esteja
# em uma sessão baseada no projeto localizado na raiz do repositório

path_base <- here()
path_soundscapes <- here("soundscapes")
path_templates <- here("roi_cuts")
path_roi_tabs <- here("roi_tables")
path_data <- here("data")
path_backup <- here("backup")
path_plots <- here("plots")
path_presets <- here("app_presets")

# path_data <- "/home/grosa/R_repos/monitoraSom/data/"

c(
  dir.exists(path_soundscapes), dir.exists(path_templates),
  dir.exists(path_roi_tabs), dir.exists(path_data),
  dir.exists(path_backup), dir.exists(path_plots),
  dir.exists(path_presets), dir.exists(path_data)
)



# Load the labels for the segmentation app: data(sp_labels) -----------------------



sp_labels <- readxl::read_xlsx(
  "example/presets/MonitoraSom_UI_label_lists.xlsx"
  ) %>%
  as.data.frame() %>%
  glimpse()



save(sp_labels, file = "/home/grosa/R_repos/monitoraSom/data/sp_labels.RData")

# openxlsx::write.xlsx(sp_labels, file = "teste/cuts/teste.xlsx")



# Soundscapes ---------------------------------------------------------------------



wav_list <- list.files(
  "/home/grosa/R_repos/MonitoraSomUI/ex_seg_small/soundscapes/",
  pattern = ".*\\.wav$", full.names = TRUE
)
# load the soundscapes
soundscapes <- lapply(wav_list, readWave)
# get the object size in megabytes
object.size(soundscapes) / 1024^2
# Save in the directory data in the repo
save(soundscapes, file = "soundscapes.RData")



# Templates -----------------------------------------------------------------------



df_templates



# ROI tables ----------------------------------------------------------------------



df_rois <- fetch_rois_n("roi_tables/")



# Cuts ----------------------------------------------------------------------------



ls_cuts



# Detections ----------------------------------------------------------------------



df_detections



# Validated detections ------------------------------------------------------------



df_detections_validated



# Diagnostics ---------------------------------------------------------------------



df_diagnostics






