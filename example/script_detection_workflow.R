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

# Para o pacote here funcionar corretamente, é necessário que este script esteja
# em uma sessão baseada no projeto localizado na raiz do repositório

path_base <- here()
path_soundscapes <- here("example", "soundscapes")
path_templates <- here("example", "roi_cuts")
path_data <- here("example", "data")
path_backup <- here("example", "backup")
path_plots <- here("example", "plots")
path_scripts <- here("R/")
# path_scripts <- "C:/R_repos/monitoraSom/R/"
# path_scripts <- "C:/Users/grosa/Documents/R/R_repos/monitoraSom/R/"

c(
  dir.exists(path_soundscapes), dir.exists(path_templates),
  dir.exists(path_data), dir.exists(path_backup),
  dir.exists(path_plots), dir.exists(path_scripts)
)

# todo Criar a função set_paths() copm checagens para facilitar a definição dos diretórios
# todo Fazer uma função para cortar rois tables em standalone templates em batch

# Carregando os scripts necessários
invisible(
  list.files(path_scripts, full.names = TRUE) %>%
    gsub("//", "/", .) %>%
    map(~ source(.x))
)

# 1. Get template metadata
# 1.a. Get metadata from standalone cuts
df_templates_A <- fetch_template_metadata(
  path = "/home/grosa/R_repos/monitoraSom/example/roi_cuts",
  recursive = TRUE, method = "standalone"
)
glimpse(df_templates_A)

# 1.b. Get metadata from ROI tables
df_templates_B <- fetch_template_metadata(
  path = "/home/grosa/R_repos/monitoraSom/example/roi_tables",
  method = "roi_table"
)
# todo Resolver nome de template file aqui
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

# 3. Get match grid
df_grid <- fetch_match_grid(
  soundscape_data = df_soundscapes, template_data = df_templates_A
)
glimpse(df_grid)

# 4. Match templates to soundscape
# 4.a. Match templates to soundscape using correlation
# df_matches_cor <- match_n(
#   df_grid = df_grid, score_method = "cor",
#   ncores = 8, par_strat = "future",
#   save_res =
#     "/home/grosa/R_repos/MonitoraSomDev/example/data/matches/matches_cor.rds"
# )
df_matches_cor <- readRDS(
  "/home/grosa/R_repos/monitoraSom/example/data/matches/matches_cor.rds"
)
glimpse(df_matches_cor)


teste <- bench::mark(
  future = match_n(
    df_grid = df_grid[1:10, ], score_method = "cor", par_strat = "future", ncores = 5
  ),
  foreach = match_n(
    df_grid = df_grid[1:10, ], score_method = "cor", par_strat = "foreach", ncores = 5
  ),
  pbapply = match_n(
    df_grid = df_grid[1:10, ], score_method = "cor", par_strat = "pbapply", ncores = 5
  ),
  parabar_async_sock = match_n(
    df_grid = df_grid[1:10, ], score_method = "cor", par_strat = "parabar", ncores = 5,
    backend_type = "async", cluster_type = "psock"
  ),
  parabar_sync_sock = match_n(
    df_grid = df_grid[1:10, ], score_method = "cor", par_strat = "parabar", ncores = 5,
    backend_type = "sync", cluster_type = "psock"
  ),
  parabar_async_fork = match_n(
    df_grid = df_grid[1:10, ], score_method = "cor", par_strat = "parabar", ncores = 5,
    backend_type = "async", cluster_type = "fork"
  ),
  parabar_sync_fork = match_n(
    df_grid = df_grid[1:10, ], score_method = "cor", par_strat = "parabar", ncores = 5,
    backend_type = "sync", cluster_type = "fork"
  ),
  iterations = 10, check = FALSE, memory = FALSE
)
plot(teste)


invisible(
  list.files(path_scripts, full.names = TRUE) %>%
    gsub("//", "/", .) %>%
    map(~ source(.x))
)

teste <- match_n(
  df_grid = df_grid[1:10, ], score_method = "cor",
  par_strat = "parabar", ncores = 5, backend_type = "async", cluster_type = "fork"
  ) %>%
  glimpse()


set_option("progress_track", TRUE)
configure_bar(type = "modern", format = "[:bar] :percent")
backend <- start_backend(cores = 4, cluster_type = "psock", backend_type = "async")
grid_list <- group_split(rowwise(df_grid))

results <- par_lapply(
  backend, grid_list,
  function(x, score_method = score_method) {
    require(dplyr); require(here); require(collapse)
    require(dtwclust); require(slider)
    source("/home/grosa/R_repos/monitoraSom/R/match_i.R") # temporário
    res <- match_i(x, score_method = "cor")
    return(res)
    }
  )

stop_backend(backend)
results %>% glimpse()

res <- par_lapply(
  backend = backend,
  x = grid_list, fun = match_i, score_method = score_method
) |>
  list_rbind()
stop_backend(backend)




# fetch_score_peaks_i(
#   match_res_i = df_matches_cor[188, ],
#   buffer_size = df_matches_cor[188, ]$score_sliding_window
# ) %>% glimpse()
# todo Adicionar os metadados com os parâmetros do template matching
# todo Mudar a quantificação do buffer para a % de frames do template

# 5. Get detections
# 5.a. From a match oject within the session environment
df_detectionsA <- fetch_score_peaks_n(
  tib_match = df_matches_cor, buffer_size = "template"
)
# 5.b. From multiple match objects stored in rds files ouside the session environment
df_detectionsB <- fetch_score_peaks_n(
  tib_match = "/home/grosa/R_repos/monitoraSom/example/data/matches/",
  buffer_size = "template"
)

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
  match_n(score_method = "cor", ncores = 8, par_strat = "foreach") %>%
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


