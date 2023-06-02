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

# Para o pacote here funcionar corretamente, é necessário que este script esteja
# em uma sessão baseada no projeto localizado na raiz do repositório

path_base <- here()
path_soundscapes <- here("example", "soundscapes")
path_templates <- here("example", "roi_cuts")
path_roi_tabs <- here("example", "roi_tables")
path_data <- here("example", "data")
path_backup <- here("example", "backup")
path_plots <- here("example", "plots")
path_presets <- here("example", "presets")

c(
  dir.exists(path_soundscapes), dir.exists(path_templates),
  dir.exists(path_roi_tabs), dir.exists(path_data),
  dir.exists(path_backup), dir.exists(path_plots),
  dir.exists(path_presets)
)

# todo Session notes não está recebendo o input adequado. Fazer a checagem entre as opções disponíveis no input. Converter os tipos para não gerar erros posteriores.
# todo Adicionar um argumento para o usuário escolher se quer salvar o preset ou não
# todo Adicionar opção de ler presets de um arquivo rds

launch_segmentation_app(
  preset_path = path_presets,
  preset_id = "linux_example",
  user = "Gabriel",
  soundscapes_path = path_soundscapes,
  roi_tables_path = path_roi_tabs,
  cuts_path = path_templates,
  labels_file = here("example", "presets", "MonitoraSom_UI_label_lists.xlsx"),
  fastdisp = TRUE, label_angle = 90, show_label = TRUE,
  dyn_range = c(-60, 0), wl = 1024, ovlp = 0, color_scale = "inferno",
  wav_player_type = "HTML player", wav_player_path = "play",
  session_notes = as.character("teste"), zoom_freq = c(0, 10), nav_autosave = FALSE,
  sp_list = "CBRO-2021 (Brazil)"
)

# todo Lançar sem inputs

# 1. Get template metadata
# 1.a. Get metadata from standalone cuts
df_templates_A <- fetch_template_metadata(
  path = path_templates, method = "standalone"
)
glimpse(df_templates_A)

# 1.b. Get metadata from ROI tables
df_templates_B <- fetch_template_metadata(
  path = path_roi_tabs, method = "roi_table"
)
# todo O nome do template está muito longo e detalhado
glimpse(df_templates_B)

# 2. Get soundscape metadata
df_soundscapes <- fetch_soundscape_metadata(
  path = path_soundscapes, recursive = TRUE, ncores = 1
)
glimpse(df_soundscapes)
# todo Capturar metadados de gravações dos audiomoths
# todo Verificar o que acontece com os arquivos 0kb
# todo Adicionar verbose = TRUE pra mostrar um diagnóstico basico
# todo Fazer a extraçãop de dados espaciais do nome do arquivo como processo opcional
# todo Arrumar uma alternativa para codificar o ponto da amostra e dar a opção em um argumento para o usuário escolher entre opções disponíveis
# todo Fazer uma função que cria novas variáveis a partir de campos do nome da soundscape

# 3. Get match grid
df_grid <- fetch_match_grid(
  soundscape_data = df_soundscapes, template_data = df_templates_A
)
glimpse(df_grid)


# 4. Match templates to soundscape
# 4.a. Match templates to soundscape using correlation
df_matches_cor <- match_n(
  df_grid = df_grid, score_method = "cor",
  ncores = 10, par_strat = "pbapply",
  save_res =
    "/home/grosa/R_repos/MonitoraSomDev/example/data/matches/matches_cor.rds"
) %>% glimpse()
df_matches_dtw <- match_n(
  df_grid = df_grid, score_method = "dtw",
  ncores = 10, par_strat = "pbapply",
  save_res =
    "/home/grosa/R_repos/MonitoraSomDev/example/data/matches/matches_dtw.rds"
) %>% glimpse()
# todo Adicionar os metadados com os parâmetros do template matching
# todo Mudar a quantificação do buffer para a % de frames do template
# todo Fazer uma função que segmenta a grade

# 5. Get detections
# 5.a. From a match oject within the session environment
df_detectionsA <- fetch_score_peaks_n(
  tib_match = df_matches_cor,
  buffer_size = "template", min_score = NULL, min_quant = NULL, top_n = NULL
) %>% glimpse()
# # 5.b. From multiple match objects stored in rds files ouside the session environment
# df_detectionsB <- fetch_score_peaks_n(
#   tib_match = "/home/grosa/R_repos/monitoraSom/example/data/matches/",
#   buffer_size = "template"
# )
# glimpse(df_detectionsB)

# 6. Whole workflow in a single pipeline
df_detections <- fetch_match_grid(
   template_data = fetch_template_metadata(
    path = here("example", "roi_cuts"),
    method = "standalone"
  ),
  soundscape_data = fetch_soundscape_metadata(
    path = path_soundscapes, ncores = 6
  )
) %>%
  match_n(score_method = "cor", ncores = 8, par_strat = "pbapply") %>%
  fetch_score_peaks_n(buffer_size = "template") %>%
  glimpse()

# # 7. Whole workflow in a single function (detectR)
# df_detections <- template_matching(
#   path_soundscapes = here("example", "soundscapes"),
#   path_templates = here("example", "roi_cuts"),
#   template_type = "standalone", score_method = "cor",
#   buffer_size = "template", min_score = NA, min_quant = NA, top_n = NA,
#   ncores = 8, par_strat = "foreach" # todo Implementação pendente
# )

# 8. Plotting
# 8.a. Without filters
plot_match_i(df_matches_cor[188, ], buffer_size = 0)
# 8.b. With template buffer
plot_match_i(df_matches_cor[188, ], buffer_size = "template")
# 8.c. With min_score (cutoff) filter
plot_match_i(df_matches_cor[188, ], buffer_size = 0, min_score = 0.2)
# 8.d. With top_n filter
plot_match_i(df_matches_cor[188, ], buffer_size = 0, top_n = 4)
# 8.e. With quantile filter
# without buffer
plot_match_i(df_matches_cor[188, ], buffer_size = 0, min_quant = 0.975)
# with buffer
plot_match_i(df_matches_cor[188, ], buffer_size = "template", min_quant = 0.975)


# 8. Plotting
# 8.a. Without filters
plot_match_i(df_matches_dtw[188, ], buffer_size = 0)
# 8.b. With template buffer
plot_match_i(df_matches_dtw[188, ], buffer_size = "template")
# 8.c. With min_score (cutoff) filter
plot_match_i(df_matches_dtw[188, ], buffer_size = 0, min_score = 0.1)
# 8.d. With top_n filter
plot_match_i(df_matches_dtw[188, ], buffer_size = 0, top_n = 4)
# 8.e. With quantile filter
# without buffer
plot_match_i(df_matches_dtw[188, ], buffer_size = 0, min_quant = 0.975)
# with buffer
plot_match_i(df_matches_dtw[188, ], buffer_size = "template", min_quant = 0.975)




# A <- df_matches_cor$score_vec[[188]]$score_vec
# B <- df_matches_dtw$score_vec[[188]]$score_vec
# AB <- (A + B) / 2
# x11()
# plot(A, type = "l", col = "red") +
#   lines(B, col = "blue") +
#   lines(AB, col = "green")
