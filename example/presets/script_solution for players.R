lista <- list.files(soundscapes_path, full.names = TRUE, recursive = TRUE)

# send a command to the system terminal
system2(command = "sox", args = c("play", lista[[1]]))
system2(command = "play", args = lista[[1]])

switch(Sys.info()[["sysname"]],
    Windows = {
        system2(command = "play", args = c(path_to_wav, "-t", "waveaudio"))
    },
    Linux = {
        system2(command = "play", args = path_to_wav)
    },
    Darwin = {
        system2(command = "play", args = path_to_wav)
    }
)

sox(lista[[1]], "play")
## path with simple slash
path <-
    ## or path with double backslash
    ## path <- "C:\Program Files (x86)\sox-14-4-2"
    sox("mysound.wav", path2exe = "C:/Program Files (x86)/sox-14-4-2", option = "-t waveaudio")