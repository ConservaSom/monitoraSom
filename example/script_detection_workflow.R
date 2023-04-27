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

# Para o pacote here funcionar corretamente, é necessário que este script esteja em uma sessão baseada no projeto localizado na raiz do repositório

path_base <- here()
path_soundscapes <- here("example", "soundscapes")
path_templates <- here("example", "roi_cuts")
path_data <- here("example", "data")
path_backup <- here("example", "backup")
path_plots <- here("example", "plots")
path_scripts <- here("R/")
# path_scripts <- "C:/R_repos/monitoraSom/R/"

c(
  dir.exists(path_soundscapes), dir.exists(path_templates),
  dir.exists(path_data), dir.exists(path_backup),
  dir.exists(path_plots), dir.exists(path_scripts)
)

# todo Criar a função set_paths() copm checagens para facilitar a definição dos diretórios

invisible(
  list.files(path_scripts, full.names = TRUE) %>%
    gsub("//", "/", .) %>%
    map(~ source(.x))
)




## Template metadata -------------------------------------------------------------------

# todo Arrumar uma forma de essa função executar o app de segmentação repassando os argumentos para variáveis globais a serem usadas no app. Fazer a confirmação automático do setup?
# ! Bug:o app não consegue ler arquivos de ROIs disponíveis. Pode ser alguma coisa relacionada à estrutura diferente de diretṕrios.

# teste <- make_segmentation_preset(
#   preset_path = here("example", "app_presets"),
#   preset_id = "default2",
#   validation_user = "Gabriel",
#   soundscapes_path = here("example", "soundscapes"),
#   roi_tables_path = here("example", "roi_tables"),
#   cuts_path = here("example", "roi_cuts"),
#   fastdisp = TRUE, label_angle = 90, show_label = TRUE,
#   dyn_range = c(-60, 0), color_scale = "inferno",
#   wav_player_type = "R session", wav_player_path = "play",
#   session_notes = "Teste de preset", zoom_freq = c(0, 8),
#   nav_autosave = TRUE, sp_list = "CBRO-2021 (Brazil)"
# )
#
# # # todo 'segmentation_app()' function
# shiny::runApp(
#   "/home/grosa/R_repos/MonitoraSomUI/app_segmentation.R",
#   launch.browser = FALSE
# )
#
# shiny::runApp(
#   "C:/R_repos/MonitoraSomUI/app_segmentation.R",
#   launch.browser = FALSE
# )


# Get metadata ------------------------------------------------------------------------



invisible(
  list.files(path_scripts, full.names = TRUE) %>%
    gsub("//", "/", .) %>%
    map(~ source(.x))
)

# 1. Get template metadata
# 1.a. Get metadata from standalone cuts
df_templates_A <- fetch_template_metadata(
  path = here("example", "roi_cuts"), method = "standalone"
)
glimpse(df_templates_A)

# 1.b. Get metadata from ROI tables
df_templates_B <- fetch_template_metadata(
  path = here("example", "roi_tables"), method = "roi_table"
)
glimpse(df_templates_B)

# 2. Get soundscape metadata
df_soundscapes <- fetch_soundscape_metadata(
  path = path_soundscapes, ncores = 1
)
glimpse(df_soundscapes)


# teste <- bench::mark(
#   seq = fetch_soundscape_metadata(path = path_soundscapes, ncores = 1),
#   par = fetch_soundscape_metadata(path = path_soundscapes, ncores = 5),
#   iterations = 10, check = FALSE, memory = FALSE
# )
# plot(teste)


# 3. Get match grid
df_grid <- fetch_match_grid(
  soundscape_data = df_soundscapes, template_data = df_templates_A
)
glimpse(df_grid)

invisible(
  list.files(path_scripts, full.names = TRUE) %>%
    gsub("//", "/", .) %>%
    map(~ source(.x))
)


library(foreach)
library(parallel)
library(doParallel)
library(foreach)
library(progressr)

teste <- bench::mark(
  # pbapply_seq = match_n(
  #   df_grid = df_grid[1:10, ], score_method = "cor", ncores = 1,
  #   save_res = FALSE, par_strat = "pbapply"
  # ),
  pbapply_par = match_n(
    df_grid = df_grid[1:10, ], score_method = "cor", ncores = 10,
    save_res = FALSE, par_strat = "pbapply"
  ),
  # future_seq = match_n(
  #   df_grid = df_grid[1:10, ], score_method = "cor", ncores = 1,
  #   save_res = FALSE, par_strat = "future"
  # ),
  future_par = match_n(
    df_grid = df_grid[1:10, ], score_method = "cor", ncores = 10,
    save_res = FALSE, par_strat = "future"
  ),
  foreach = match_n(
    df_grid = df_grid[1:10, ], score_method = "cor", ncores = 10,
    save_res = FALSE, par_strat = "foreach"
  ),
  iterations = 10, check = FALSE, memory = FALSE
)
x11(); plot(teste)
match_n(
    df_grid = df_grid[1:50, ], score_method = "cor", ncores = 10,
    save_res = FALSE, par_strat = "foreach"
  ) %>% glimpse()
match_n(
    df_grid = df_grid[1:50, ], score_method = "cor", ncores = 10,
    save_res = FALSE, par_strat = "future"
  ) %>% glimpse()
match_n(
    df_grid = df_grid[1:50, ], score_method = "cor", ncores = 10,
    save_res = FALSE, par_strat = "pbapply"
  ) %>% glimpse()


# 4. Match templates to soundscape
# 4.a. Match templates to soundscape using correlation
df_matches_cor <- match_n(
  df_grid = df_grid, score_method = "cor", ncores = 8,
  save_res = "/home/grosa/R_repos/MonitoraSomDev/example/data/matches/matches_cor.rds"
)
glimpse(df_matches_cor)

# 4.b. Match templates to soundscape using dynamic time warping (#188)
df_matches_dtw <- match_n(
  df_grid = df_grid, score_method = "dtw", ncores = 8,
  save_res = "/home/grosa/R_repos/MonitoraSomDev/example/data/matches/matches_dtw.rds"
)
glimpse(df_matches_dtw)







# teste <- bench::mark(
#   seq = match_n(
#     df_grid = df_grid[1:5, ], score_method = "cor", ncores = 1, save_res = FALSE
#   ),
#   par = match_n(
#     df_grid = df_grid[1:5, ], score_method = "cor", ncores = 10, save_res = FALSE
#   ),
#   check = FALSE, iterations = 10, memory = FALSE
# )
# # x11()
# plot(teste)





# 5. Get detections
df_detections <- fetch_score_peaks_n(
  tib_match = df_matches_cor, buffer_size = "template"
)
glimpse(df_detections)



# # 6. Whole workflow in a single pipeline
# df_detections <- fetch_match_grid(
#    template_data = fetch_template_metadata(
#     path = here("example", "roi_cuts"), method = "standalone"
#   ),
#   soundscape_data = fetch_soundscape_metadata(
#     path = path_soundscapes, ncores = 6
#   )
# ) %>%
#   match_n(score_method = "cor", ncores = 8) %>%
#   fetch_score_peaks_n(buffer_size = "template") %>%
#   glimpse()
#
# # 7. Whole workflow in a single function
# df_detections <- template_matching(
#   path_soundscapes = here("example", "soundscapes"),
#   path_templates = here("example", "roi_cuts"),
#   template_type = "standalone",
#   score_method = "cor", buffer_size = "template", ncores = 8
# )


df_matches_cor[20, ]$score_vec %>% glimpse()

fetch_score_peaks_i(df_matches_cor[20, ]) %>% glimpse()


match_i_res <- df_matches_cor[20, ] %>% glimpse()

invisible(
  list.files(path_scripts, full.names = TRUE) %>%
    gsub("//", "/", .) %>%
    map(~ source(.x))
)

co
plot_match_i(
  df_matches_cor[188, ], buffer_size = "template", interpolate = FALSE,
  )
plot_match_i(
  df_matches_dtw, buffer_size = "template", top_n = 4, interpolate = FALSE,
  )

