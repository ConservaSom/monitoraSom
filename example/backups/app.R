library(shiny)
library(tuneR)
library(seewave)
library(ggplot2)
library(viridis)
library(farver)
library(shinyWidgets)
library(bslib)
library(RSQLite)
library(tidyverse)
library(kableExtra)

# Source the local fast_spectro function
source("fast_spectro.R")

run_simple_spectro_viewer <- function(
    soundscape_path = "./soundscapes/",
    roi_db = "./rois.db",
    window = 1024,
    overlap = 20,
    freq_range = c(0, 10),
    dyn_range = c(-96, 0)) {
    ui <- page_fluid(
        theme = bs_theme(version = 5),

        tags$style(type = "text/css", ".recalculating {opacity: 1.0;}"),

        # Dark mode toggle in top right
        div(
            style = "position: absolute; top: 10px; right: 20px; z-index: 1000;",
            input_dark_mode(id = "dark_mode", value = FALSE)
        ),
        titlePanel("Segmentation App"),

        # File controls at the top
        fluidRow(
            style = "margin-bottom: 20px;",
            column(12,
                wellPanel(
                    fluidRow(
                        column(5,
                            textInput("dir_path", "WAV Directory Path:", value = soundscape_path),
                            textInput("roi_db", "ROI Database Path:", value = roi_db)
                        ),
                        column(2,
                            actionButton("refresh", "Refresh File List", class = "btn-primary", style = "margin-top: 25px;")
                        ),
                        column(5,
                            selectInput("wav_file", "Select WAV File:", choices = NULL),
                            div(style = "display: flex; justify-content: space-between; margin-top: 10px;",
                                actionButton("prev_unsegmented",
                                    label = div(icon("arrow-left"), "Prev Unsegmented"),
                                    class = "btn-info"
                                ),
                                actionButton("prev_file",
                                    label = div(icon("arrow-left"), "Previous"),
                                    class = "btn-primary"
                                ),
                                actionButton("next_file",
                                    label = div("Next", icon("arrow-right")),
                                    class = "btn-primary"
                                ),
                                actionButton("next_unsegmented",
                                    label = div("Next Unsegmented", icon("arrow-right")),
                                    class = "btn-info"
                                )
                            )
                        )
                    )
                )
            )
        ),

        # Main content
        fluidRow(
            # Time slider at the top
            column(12,
                div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 10px; padding: 0 15px;",
                    div(style = "width: 120px;", "Time (seconds):"),
                        noUiSliderInput(
                            inputId = "tlim",
                            label = NULL,
                            min = 0, max = 1,
                            value = c(0, 1),
                            step = 0.1,
                            orientation = "horizontal",
                            direction = "ltr",
                            format = wNumbFormat(decimals = 1),
                            tooltips = TRUE,
                            behaviour = "none",
                            width = "100%"
                        )
                )
            )
        ),
        fluidRow(
            # Frequency slider (narrower with less padding)
            column(1, style = "padding-right: 0;",
                div(style = "width: 50px; height: 600px; padding-top: 20px;",
                    noUiSliderInput(
                        inputId = "flim",
                        label = "Frequency (kHz)",
                        min = 0, max = 20,
                        value = freq_range,
                        step = 0.1,
                        orientation = "vertical",
                        direction = "rtl",
                        height = "500px",
                        format = wNumbFormat(decimals = 1),
                        tooltips = TRUE,
                        behaviour = "none"
                    )
                )
            ),
            # Spectrogram (wider with less padding)
            column(11, style = "padding-left: 0;",
                plotOutput("spectrogram",
                    height = "600px",
                    brush = "roi_limits",
                    click = "select_roi"
                )
            )
        ),

        # Add this after the spectrogram section in the UI (before the ROI controls)
        fluidRow(
            column(12,
                div(
                    style = "margin-top: 10px; margin-bottom: 10px;",
                    uiOutput("audio_player")
                )
            )
        ),

        # ROI controls in a box
        fluidRow(
            column(12,
                wellPanel(
                    style = "margin-top: 20px;",
                    fluidRow(
                        column(4,
                            textInput("roi_label", "ROI Label:", value = "")
                        ),
                        column(2,
                            div(style = "margin-top: 25px;",
                                checkboxInput("roi_label_locked", "Lock Label", value = FALSE)
                            )
                        ),
                        column(6,
                            div(style = "margin-top: 25px; display: flex; gap: 10px; justify-content: flex-end;",
                                actionButton("clear_rois", "Clear All ROIs",
                                    class = "btn-danger"
                                ),
                                actionButton("delete_last_selection", "Delete Last ROI (Q)",
                                    class = "btn-warning"
                                ),
                                actionButton("store_selection", "Store ROI (E)",
                                    class = "btn-primary"
                                )
                            )
                        )
                    )
                )
            )
        ),

        # Bottom panel with settings and ROI table side by side
        fluidRow(
            style = "margin-top: 20px;",
            # Spectrogram Settings
            column(6,
                wellPanel(
                    h4("Spectrogram Settings"),
                    fluidRow(
                        column(6,
                            sliderTextInput(
                                "window", "Window length",
                                choices = c(128, 256, 512, 1024, 2048, 4096, 8192, 16384),
                                selected = window, grid = TRUE, width = "100%"
                            )
                        ),
                        column(6,
                            sliderInput(
                              "overlap", "Overlap (%)",
                              min = 0, max = 80, value = overlap,
                              post = "%", step = 10, width = "100%"
                            )
                        )
                    ),
                    fluidRow(
                        column(6,
                            sliderInput(
                              "dyn_range", "Dynamic range (dB)",
                              min = -200,
                              max = 10,
                              step = 6, value = c(-100, 0),
                              width = "100%", post = "dB"
                            )
                        ),
                        column(6,
                            selectInput("color_scale", "Color Scale:",
                                choices = c(
                                    "Inferno" = "inferno",
                                    "Viridis" = "viridis",
                                    "Magma" = "magma",
                                    "Cividis" = "cividis",
                                    "Greyscale 1" = "greyscale 1",
                                    "Greyscale 2" = "greyscale 2"
                                ),
                                selected = "inferno"
                            )
                        )
                    ),
                    fluidRow(
                        column(6,
                            checkboxInput("show_freq_guides", "Frequency Guides", value = TRUE),
                            checkboxInput("show_time_guides", "Time Guides", value = TRUE)
                        ),
                        column(6,
                            numericInput("time_guide_interval", "Time Guide Interval (s)",
                                value = 3, min = 0.5, max = 10, step = 0.5
                            ),
                            numericInput("freq_guide_interval", "Freq Guide Interval (kHz)",
                                value = 1, min = 0.5, max = 5, step = 0.5
                            )
                        )
                    )
                )
            ),
            # ROI Table
            column(6,
                wellPanel(
                    div(
                        style = "display: flex; justify-content: space-between; align-items: center;",
                        h4(style = "margin: 0;", "Regions of Interest (ROIs)"),
                        actionButton("toggle_roi_table", "",
                            icon = icon("chevron-up"),
                            class = "btn-link",
                            style = "padding: 0;"
                        )
                    ),
                    div(id = "roi_table_container",
                        uiOutput("roi_table")
                    )
                )
            )
        ),

        # Add this CSS to control table visibility (add near the top of UI, after theme)
        tags$head(
            tags$style("
                #roi_table_container {
                    max-height: 300px;
                    transition: max-height 0.3s ease-out;
                    overflow: hidden;
                }
                #roi_table_container.collapsed {
                    max-height: 0;
                }
                #toggle_roi_table .fa-chevron-up {
                    transition: transform 0.3s ease;
                }
                #toggle_roi_table .fa-chevron-up.collapsed {
                    transform: rotate(180deg);
                }
            ")
        ),

        # Add this JavaScript to handle toggle behavior (add with other scripts)
        tags$script("
            $(document).ready(function() {
                var tableVisible = true;

                $('#toggle_roi_table').on('click', function() {
                    tableVisible = !tableVisible;
                    $('#roi_table_container').toggleClass('collapsed');
                    $(this).find('.fa').toggleClass('collapsed');
                    Shiny.setInputValue('table_visible', tableVisible);
                });
            });
        "),

        # Keyboard shortcuts at the bottom
        fluidRow(
            style = "margin-top: 20px;",
            column(12,
                div(
                    style = "text-align: center; padding: 10px; background-color: rgba(0,0,0,0.05); border-radius: 5px;",
                    h5("Keyboard Shortcuts:", style = "margin-bottom: 10px;"),
                    div(
                        style = "display: inline-block; text-align: left;",
                        tags$ul(
                            style = "list-style-type: none; padding: 0; margin: 0;",
                            tags$li(style = "display: inline-block; margin: 0 15px;", "A/D - Time left/right"),
                            tags$li(style = "display: inline-block; margin: 0 15px;", "R/F - Freq up/down"),
                            tags$li(style = "display: inline-block; margin: 0 15px;", "W/S - Time zoom in/out"),
                            tags$li(style = "display: inline-block; margin: 0 15px;", "G/T - Freq zoom in/out"),
                            tags$li(style = "display: inline-block; margin: 0 15px;", "E - Store ROI"),
                            tags$li(style = "display: inline-block; margin: 0 15px;", "Q - Delete Last ROI"),
                            tags$li(style = "display: inline-block; margin: 0 15px;", "Space - Play/Pause Audio")
                        )
                    )
                )
            )
        ),

        # Keyboard shortcut handling script
        tags$script("
            $(document).on('keydown', function(e) {
                if (e.target.tagName === 'INPUT') return true;
                switch(e.key.toLowerCase()) {
                    case 'a':
                        Shiny.setInputValue('nav_back', Math.random());
                        break;
                    case 'd':
                        Shiny.setInputValue('nav_forward', Math.random());
                        break;
                    case 'r':
                        Shiny.setInputValue('freq_nav_up', Math.random());
                        break;
                    case 'f':
                        Shiny.setInputValue('freq_nav_down', Math.random());
                        break;
                    case 'w':
                        Shiny.setInputValue('zoom_in', Math.random());
                        break;
                    case 's':
                        Shiny.setInputValue('zoom_out', Math.random());
                        break;
                    case 'g':
                        Shiny.setInputValue('freq_zoom_in', Math.random());
                        break;
                    case 't':
                        Shiny.setInputValue('freq_zoom_out', Math.random());
                        break;
                    case 'e':
                        Shiny.setInputValue('store_selection', Math.random());
                        break;
                    case 'q':
                        Shiny.setInputValue('delete_last_selection', Math.random());
                        break;
                    case ' ':
                        e.preventDefault();
                        var audio = document.querySelector('audio');
                        if (audio) {
                            if (audio.paused) {
                                audio.play();
                            } else {
                                audio.pause();
                            }
                        }
                        break;
                }
            });
        "),

        # Rest of the UI code remains the same...
    )

    server <- function(input, output, session) {
        wav_files <- reactiveVal(NULL)
        wav_data <- reactiveVal(NULL)
        wave_duration <- reactiveVal(0)
        base_spectrogram <- reactiveVal(NULL) # Store the precomputed spectrogram

        # Calculate time step for navigation
        time_step <- reactive({
            req(input$tlim)
            diff(input$tlim) * 0.1
        })

        # Calculate frequency step for navigation
        freq_step <- reactive({
            req(input$flim)
            diff(input$flim) * 0.1
        })

        # Precompute spectrogram when file or core parameters change
        observe({
            req(
                wav_data(), input$window, input$overlap,
                input$color_scale, input$dyn_range, input$dark_mode
            )

            tryCatch(
                {
                    p <- fast_spectro(
                        rec = wav_data(),
                        wl = input$window,
                        ovlp = input$overlap,
                        color_scale = input$color_scale,
                        flim = c(0, 20), # Full frequency range
                        tlim = NULL, # Full time range
                        dyn_range = input$dyn_range,
                        interpolate = FALSE,
                        show_freq_guides = input$show_freq_guides,
                        show_time_guides = input$show_time_guides,
                        time_guide_interval = input$time_guide_interval,
                        freq_guide_interval = input$freq_guide_interval,
                        theme_mode = input$dark_mode
                    )
                    base_spectrogram(p)
                },
                error = function(e) {
                    showNotification(paste("Error creating spectrogram:", e$message), type = "error")
                    NULL
                }
            )
        })

        # Render spectrogram with current view window
        output$spectrogram <- renderPlot({
            req(base_spectrogram(), input$tlim, input$flim)

            p <- base_spectrogram() +
                coord_cartesian(
                    xlim = input$tlim,
                    ylim = input$flim,
                    expand = FALSE
                )

            # Add ROIs if they exist
            roi_data <- rois()
            if (nrow(roi_data) > 0) {
                p <- p +
                    geom_rect(
                        data = roi_data,
                        aes(
                            xmin = start_time, xmax = end_time,
                            ymin = min_freq, ymax = max_freq
                        ),
                        fill = NA, color = "white", linetype = "dashed",
                        inherit.aes = FALSE
                    ) +
                    geom_text(
                        data = roi_data,
                        aes(
                            x = (start_time + end_time) / 2,
                            y = max_freq,
                            label = label
                        ),
                        color = "white",
                        vjust = -0.5,
                        size = 3,
                        inherit.aes = FALSE
                    )
            }
            p
        })

        # Validate directory and refresh file list
        observeEvent(input$refresh, {
            if (!dir.exists(input$dir_path)) {
                showNotification("Directory does not exist", type = "error")
                return()
            }

            files <- list.files(input$dir_path, pattern = "\\.wav$", ignore.case = TRUE)
            if (length(files) == 0) {
                showNotification("No WAV files found in directory", type = "error")
                return()
            }

            wav_files(files)
            updateSelectInput(session, "wav_file",
                choices = files,
                selected = files[1]
            )
        })

        # Load WAV file when selected
        observeEvent(input$wav_file, {
            req(input$wav_file)
            file_path <- file.path(input$dir_path, input$wav_file)

            tryCatch(
                {
                    new_wav <- readWave(file_path)
                    wav_data(new_wav)
                    duration <- length(new_wav@left) / new_wav@samp.rate
                    wave_duration(duration)

                    # Update time slider range
                    updateNoUiSliderInput(session, "tlim",
                        range = c(0, duration),
                        value = c(0, duration)
                    )
                },
                error = function(e) {
                    showNotification(paste("Error loading file:", e$message), type = "error")
                }
            )
        })

        # Replace time slider UI with direct update of noUiSlider
        observeEvent(wav_data(), {
            req(wave_duration())
            updateNoUiSliderInput(session, "tlim",
                range = c(0, wave_duration()),
                value = c(0, wave_duration())
            )
        })

        # Update time navigation handlers to use updateNoUiSliderInput
        observeEvent(input$nav_back, {
            req(input$tlim, wave_duration())
            current_start <- input$tlim[1]
            current_end <- input$tlim[2]
            window_size <- current_end - current_start
            step <- window_size / 2
            new_start <- max(0, current_start - step)
            new_end <- new_start + window_size
            updateNoUiSliderInput(session, "tlim",
                value = c(new_start, new_end)
            )
        })

        observeEvent(input$nav_forward, {
            req(input$tlim, wave_duration())
            current_start <- input$tlim[1]
            current_end <- input$tlim[2]
            window_size <- current_end - current_start
            step <- window_size / 2
            new_end <- min(wave_duration(), current_end + step)
            new_start <- new_end - window_size
            updateNoUiSliderInput(session, "tlim",
                value = c(new_start, new_end)
            )
        })

        observeEvent(input$zoom_in, {
            req(input$tlim, wave_duration())
            center <- mean(input$tlim)
            window_size <- diff(input$tlim) * 0.5
            new_start <- max(0, center - window_size / 2)
            new_end <- min(wave_duration(), center + window_size / 2)
            updateNoUiSliderInput(session, "tlim",
                value = c(new_start, new_end)
            )
        })

        observeEvent(input$zoom_out, {
            req(input$tlim, wave_duration())
            center <- mean(input$tlim)
            window_size <- diff(input$tlim)
            new_start <- max(0, center - window_size)
            new_end <- min(wave_duration(), center + window_size)
            updateNoUiSliderInput(session, "tlim",
                value = c(new_start, new_end)
            )
        })

        # Update frequency navigation handlers to use 1 kHz steps
        observeEvent(input$freq_nav_up, {
            req(input$flim)
            current_min <- input$flim[1]
            current_max <- input$flim[2]
            window_size <- current_max - current_min
            new_max <- min(20, current_max + 1) # Step up by 1 kHz
            new_min <- new_max - window_size
            if (new_min >= 0) {
                updateNoUiSliderInput(session, "flim",
                    value = c(new_min, new_max)
                )
            }
        })

        observeEvent(input$freq_nav_down, {
            req(input$flim)
            current_min <- input$flim[1]
            current_max <- input$flim[2]
            window_size <- current_max - current_min
            new_min <- max(0, current_min - 1) # Step down by 1 kHz
            new_max <- new_min + window_size
            if (new_max <= 20) {
                updateNoUiSliderInput(session, "flim",
                    value = c(new_min, new_max)
                )
            }
        })

        # Update frequency zoom handlers to anchor at minimum frequency
        observeEvent(input$freq_zoom_in, {
            req(input$flim)
            current_min <- input$flim[1]
            current_max <- input$flim[2]
            window_size <- diff(input$flim) * 0.5 # Halve the window size
            new_max <- current_min + window_size # New max based on min
            updateNoUiSliderInput(session, "flim",
                value = c(current_min, new_max)
            )
        })

        observeEvent(input$freq_zoom_out, {
            req(input$flim)
            current_min <- input$flim[1]
            current_max <- input$flim[2]
            window_size <- diff(input$flim) * 2 # Double the window size
            new_max <- min(20, current_min + window_size) # Limit to max 20 kHz
            updateNoUiSliderInput(session, "flim",
                value = c(current_min, new_max)
            )
        })

        # Update theme based on dark mode switch
        observe({
            req(input$dark_mode)
            bslib::bs_theme_update(theme = bs_theme(
                version = 5,
                preset = if (input$dark_mode == "dark") "darkly" else "default"
            ))
        })

        # Add navigation handlers
        observeEvent(input$next_file, {
            req(validate_paths(), input$wav_file, wav_files())
            current_files <- wav_files()
            current_index <- match(input$wav_file, current_files)

            if (!is.na(current_index) && current_index < length(current_files)) {
                next_file <- current_files[current_index + 1]
                updateSelectInput(session, "wav_file",
                    choices = current_files,
                    selected = next_file
                )
            } else {
                showNotification("Already at the last file", type = "warning")
            }
        })

        observeEvent(input$prev_file, {
            req(validate_paths(), input$wav_file, wav_files())
            current_files <- wav_files()
            current_index <- match(input$wav_file, current_files)

            if (!is.na(current_index) && current_index > 1) {
                prev_file <- current_files[current_index - 1]
                updateSelectInput(session, "wav_file",
                    choices = current_files,
                    selected = prev_file
                )
            } else {
                showNotification("Already at the first file", type = "warning")
            }
        })

        # Add validation function (needed for the navigation handlers)
        validate_paths <- reactive({
            if (!dir.exists(input$dir_path)) {
                showNotification("WAV directory does not exist", type = "error")
                return(FALSE)
            }

            wav_count <- length(list.files(input$dir_path, pattern = "\\.wav$", ignore.case = TRUE))
            if (wav_count == 0) {
                showNotification("No WAV files found in directory", type = "error")
                return(FALSE)
            }

            return(TRUE)
        })

        # Add ROI reactive values
        rois <- reactiveVal(data.frame(
            soundscape_file = character(),
            start_time = numeric(),
            end_time = numeric(),
            min_freq = numeric(),
            max_freq = numeric(),
            label = character(),
            timestamp = character(),
            stringsAsFactors = FALSE
        ))

        # Store ROI selection
        observeEvent(input$store_selection, {
            if (!is.null(input$roi_limits)) {
                new_roi <- data.frame(
                    soundscape_file = input$wav_file,
                    start_time = input$roi_limits$xmin,
                    end_time = input$roi_limits$xmax,
                    min_freq = input$roi_limits$ymin,
                    max_freq = input$roi_limits$ymax,
                    label = input$roi_label,
                    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    stringsAsFactors = FALSE
                )
                current_rois <- rois()
                rois(rbind(current_rois, new_roi))
                rois_changed(TRUE)

                # Save to database immediately
                save_rois_to_db(rois(), input$wav_file)

                if (!input$roi_label_locked) {
                    updateTextInput(session, "roi_label", value = "")
                }
            }
        })

        # Delete last ROI
        observeEvent(input$delete_last_selection, {
            current_rois <- rois()
            if (nrow(current_rois) > 0) {
                new_rois <- current_rois[-nrow(current_rois), ]
                rois(new_rois)
                rois_changed(TRUE)

                # Save the updated ROIs
                save_rois_to_db(new_rois, input$wav_file)

                showNotification("Last ROI removed", type = "message")
            } else {
                showNotification("No ROIs to remove", type = "warning")
            }
        })

        # Clear all ROIs
        observeEvent(input$clear_rois, {
            rois(data.frame(
                soundscape_file = character(),
                start_time = numeric(),
                end_time = numeric(),
                min_freq = numeric(),
                max_freq = numeric(),
                label = character(),
                timestamp = character(),
                stringsAsFactors = FALSE
            ))
        })

        # Add these reactive values
        table_data <- reactiveVal(NULL)
        table_visible <- reactiveVal(TRUE)

        # Observer to update table data when ROIs change
        observe({
            current_rois <- rois()
            if (nrow(current_rois) > 0) {
                formatted_data <- current_rois %>%
                    kbl(
                        col.names = c(
                            "Soundscape File",
                            "Start Time (s)",
                            "End Time (s)",
                            "Min Freq (kHz)",
                            "Max Freq (kHz)",
                            "Label",
                            "Timestamp"
                        ),
                        digits = 2,
                        row.names = FALSE,
                        table.attr = 'class="table table-striped table-bordered table-hover"'
                    ) %>%
                    kable_styling(
                        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                        full_width = TRUE,
                        position = "left",
                        font_size = 14
                    ) %>%
                    scroll_box(height = "300px")

                table_data(formatted_data)
            } else {
                table_data(NULL)
            }
        })

        # Update table visibility based on toggle
        observeEvent(input$table_visible, {
            table_visible(input$table_visible)
        })

        # Render ROI table more efficiently
        output$roi_table <- renderUI({
            if (!table_visible()) {
                return(NULL)
            }

            current_table <- table_data()
            if (is.null(current_table)) {
                return(HTML("<p>No ROIs defined yet.</p>"))
            }

            HTML(current_table)
        })

        # Add these reactive values
        previous_wav_file <- reactiveVal(NULL)
        rois_changed <- reactiveVal(FALSE)

        # Initialize database function
        initialize_database <- function(db_path) {
            tryCatch({
                con <- dbConnect(SQLite(), db_path)
                on.exit(dbDisconnect(con))

                # Create table if it doesn't exist
                dbExecute(con, "
                    CREATE TABLE IF NOT EXISTS rois (
                        id INTEGER PRIMARY KEY AUTOINCREMENT,
                        soundscape_file TEXT,
                        start_time REAL,
                        end_time REAL,
                        min_freq REAL,
                        max_freq REAL,
                        label TEXT,
                        timestamp TEXT
                    )
                ")
            }, error = function(e) {
                showNotification(paste("Error initializing database:", e$message), type = "error")
            })
        }

        # Add database operations helper functions
        load_rois_from_db <- function(wav_filename) {
            req(wav_filename, input$roi_db)

            tryCatch({
                con <- dbConnect(SQLite(), input$roi_db)
                on.exit(dbDisconnect(con))

                new_rois <- dbGetQuery(con,
                    "SELECT soundscape_file, start_time, end_time, min_freq, max_freq,
                            label, timestamp
                     FROM rois
                     WHERE soundscape_file = ?
                     ORDER BY timestamp DESC",
                    params = list(wav_filename)
                )

                if (nrow(new_rois) > 0) {
                    # Ensure proper column types
                    new_rois$start_time <- as.numeric(new_rois$start_time)
                    new_rois$end_time <- as.numeric(new_rois$end_time)
                    new_rois$min_freq <- as.numeric(new_rois$min_freq)
                    new_rois$max_freq <- as.numeric(new_rois$max_freq)
                    rois(new_rois)
                } else {
                    clear_rois()
                }
            }, error = function(e) {
                showNotification(paste("Error loading ROIs:", e$message), type = "warning")
            })
        }

        # Add save function
        save_rois_to_db <- function(rois_df, wav_filename) {
            req(input$roi_db, wav_filename)
            if (!rois_changed()) return()

            tryCatch({
                con <- dbConnect(SQLite(), input$roi_db)
                on.exit(dbDisconnect(con))

                # Delete existing ROIs for this file
                dbExecute(con,
                    "DELETE FROM rois WHERE soundscape_file = ?",
                    params = list(wav_filename)
                )

                if (nrow(rois_df) > 0) {
                    dbWriteTable(con, "rois", rois_df, append = TRUE)
                }
                rois_changed(FALSE)
            }, error = function(e) {
                showNotification(paste("Error saving ROIs:", e$message), type = "error")
            })
        }

        # Add observer for WAV file changes
        observeEvent(input$wav_file, {
            req(input$wav_file, input$roi_db)

            # Save previous file's ROIs if changed
            prev_file <- previous_wav_file()
            if (!is.null(prev_file) && rois_changed()) {
                save_rois_to_db(isolate(rois()), prev_file)
            }

            # Load ROIs for new file
            load_rois_from_db(input$wav_file)
            previous_wav_file(input$wav_file)
        }, priority = 1)

        # Add validation to the refresh observer
        observeEvent(input$refresh, {
            # Initialize database when refreshing
            if (!dir.exists(input$dir_path)) {
                showNotification("Directory does not exist", type = "error")
                return()
            }

            # Initialize database
            tryCatch({
                initialize_database(input$roi_db)
            }, error = function(e) {
                showNotification(paste("Database error:", e$message), type = "error")
                return()
            })

            files <- list.files(input$dir_path, pattern = "\\.wav$", ignore.case = TRUE)
            if (length(files) == 0) {
                showNotification("No WAV files found in directory", type = "error")
                return()
            }

            wav_files(files)
            updateSelectInput(session, "wav_file",
                choices = files,
                selected = files[1]
            )
        })

        # Add these reactive values at the top of the server
        audio_path <- reactiveVal()

        # Add this observer near the start of the server
        observe({
            temp_dir <- file.path(tempdir(), "audio_files")
            if (!dir.exists(temp_dir)) dir.create(temp_dir)
            unlink(list.files(temp_dir, full.names = TRUE))
            shiny::addResourcePath("audio_files", temp_dir)
            audio_path(temp_dir)
        })

        # Add the create_audio_segment helper function
        create_audio_segment <- function(wav_data, start_time, end_time) {
            req(wav_data, start_time, end_time)

            # Convert times to samples
            start_sample <- round(start_time * wav_data@samp.rate) + 1
            end_sample <- round(end_time * wav_data@samp.rate)

            # Ensure samples are within bounds
            start_sample <- max(1, start_sample)
            end_sample <- min(length(wav_data@left), end_sample)

            # Create new Wave object with the segment
            if (wav_data@stereo) {
                # Stereo audio
                segment <- Wave(
                    left = wav_data@left[start_sample:end_sample],
                    right = wav_data@right[start_sample:end_sample],
                    samp.rate = wav_data@samp.rate,
                    bit = wav_data@bit
                )
            } else {
                # Mono audio
                segment <- Wave(
                    left = wav_data@left[start_sample:end_sample],
                    samp.rate = wav_data@samp.rate,
                    bit = wav_data@bit
                )
            }

            return(segment)
        }

        # Add the audio player output
        output$audio_player <- renderUI({
            req(validate_paths(), input$refresh, input$wav_file, wav_files(), audio_path(), input$tlim)

            file_path <- file.path(input$dir_path, input$wav_file)
            tryCatch(
                {
                    # Read the full WAV file
                    wav_data <- readWave(file_path)

                    # Create segment for current view
                    segment <- create_audio_segment(wav_data, input$tlim[1], input$tlim[2])

                    # Create unique filename for segment
                    segment_filename <- paste0(
                        "segment_", gsub("[^a-zA-Z0-9]", "", input$wav_file), "_",
                        format(Sys.time(), "%H%M%S"),
                        ".wav"
                    )
                    temp_file <- file.path(audio_path(), segment_filename)

                    # Save segment
                    writeWave(segment, temp_file)

                    # Create audio player
                    audio_id <- paste0("audio_", gsub("[^a-zA-Z0-9]", "", input$wav_file))
                    tags$div(
                        tags$audio(
                            id = audio_id,
                            controls = TRUE,
                            style = "width: 100%; margin-bottom: 20px;",
                            tags$source(
                                src = file.path("/audio_files", segment_filename),
                                type = "audio/wav"
                            )
                        )
                    )
                },
                error = function(e) {
                    showNotification(paste("Error creating audio segment:", e$message),
                        type = "error"
                    )
                    return(NULL)
                }
            )
        })

        # Add these functions in the server section

        # Function to check if a WAV file has been segmented
        is_file_segmented <- function(wav_filename) {
            if (is.null(wav_filename) || is.null(input$roi_db)) {
                return(FALSE)
            }

            tryCatch(
                {
                    con <- dbConnect(SQLite(), input$roi_db)
                    result <- dbGetQuery(con,
                        "SELECT COUNT(*) as count FROM rois WHERE soundscape_file = ?",
                        params = list(wav_filename)
                    )
                    dbDisconnect(con)

                    return(result$count > 0)
                },
                error = function(e) {
                    showNotification(paste("Database error:", e$message), type = "error")
                    return(FALSE)
                }
            )
        }

        # Function to find next unsegmented file
        find_next_unsegmented <- function(current_file, files, forward = TRUE) {
            if (is.null(current_file) || length(files) == 0) {
                return(NULL)
            }

            current_index <- match(current_file, files)
            if (is.na(current_index)) {
                return(NULL)
            }

            # Determine search sequence based on direction
            if (forward) {
                search_indices <- seq(current_index + 1, length(files))
            } else {
                search_indices <- seq(current_index - 1, 1)
            }

            # Search for next unsegmented file
            for (i in search_indices) {
                if (!is_file_segmented(files[i])) {
                    return(files[i])
                }
            }
            return(NULL)
        }

        # Add observers for unsegmented navigation buttons
        observeEvent(input$next_unsegmented, {
            req(validate_paths(), input$wav_file, wav_files())
            next_file <- find_next_unsegmented(input$wav_file, wav_files(), forward = TRUE)

            if (!is.null(next_file)) {
                updateSelectInput(session, "wav_file",
                    choices = wav_files(),
                    selected = next_file
                )
            } else {
                showNotification("No more unsegmented files ahead", type = "warning")
            }
        })

        observeEvent(input$prev_unsegmented, {
            req(validate_paths(), input$wav_file, wav_files())
            prev_file <- find_next_unsegmented(input$wav_file, wav_files(), forward = FALSE)

            if (!is.null(prev_file)) {
                updateSelectInput(session, "wav_file",
                    choices = wav_files(),
                    selected = prev_file
                )
            } else {
                showNotification("No more unsegmented files before", type = "warning")
            }
        })

        # Add this helper function in the server section, near the other ROI-related functions
        clear_rois <- function() {
            rois(data.frame(
                soundscape_file = character(),
                start_time = numeric(),
                end_time = numeric(),
                min_freq = numeric(),
                max_freq = numeric(),
                label = character(),
                timestamp = character(),
                stringsAsFactors = FALSE
            ))
        }

        # Then modify the load_rois_from_db function to use it
        load_rois_from_db <- function(wav_filename) {
            req(wav_filename, input$roi_db)

            tryCatch({
                con <- dbConnect(SQLite(), input$roi_db)
                on.exit(dbDisconnect(con))

                new_rois <- dbGetQuery(con,
                    "SELECT soundscape_file, start_time, end_time, min_freq, max_freq,
                            label, timestamp
                     FROM rois
                     WHERE soundscape_file = ?
                     ORDER BY timestamp DESC",
                    params = list(wav_filename)
                )

                if (nrow(new_rois) > 0) {
                    # Ensure proper column types
                    new_rois$start_time <- as.numeric(new_rois$start_time)
                    new_rois$end_time <- as.numeric(new_rois$end_time)
                    new_rois$min_freq <- as.numeric(new_rois$min_freq)
                    new_rois$max_freq <- as.numeric(new_rois$max_freq)
                    rois(new_rois)
                } else {
                    clear_rois()
                }
            }, error = function(e) {
                showNotification(paste("Error loading ROIs:", e$message), type = "warning")
            })
        }
    }

    shinyApp(ui = ui, server = server)
}

# Example usage:
run_simple_spectro_viewer(
    soundscape_path = "./soundscapes/",
    roi_db = "./rois.db",
    window = 2048,
    overlap = 30,
    freq_range = c(0, 15),
    dyn_range = c(-90, -36)
)
