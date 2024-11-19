  # Render plot with optimized dependencies
  output$spectrogram_plot <- renderPlot({
    req(
      spectro_soundscape_raw(),
      view_params(),
      roi_values(),
      duration_val()
    )

    # Initialize base plot with view parameters
    zoom_freq <- view_params()$zoom_freq
    if (zoom_freq[1] >= zoom_freq[2]) {
      zoom_freq[2] <- zoom_freq[1] + 1
    }
    zoom_freq <- sort(zoom_freq)
    zoom_time <- view_params()$zoom_time

    selection_color <- ifelse(
      input$color_scale %in% c("greyscale 1", "greyscale 2"),
      "black", "white"
    )

    # Create base plot once
    spectro_plot <- spectro_soundscape_raw() +
      coord_cartesian(
        xlim = zoom_time, ylim = zoom_freq
      ) +
      annotate(
        "label",
        label = paste0(
          input$soundscape_file, " (sr = ", rec_soundscape()@samp.rate,
          "; wl = ", input$wl, "; ovlp = ", input$ovlp,
          "; pitch_shift = ", input$pitch_shift, ")"
        ),
        x = -Inf, y = Inf, hjust = 0, vjust = 1,
        color = "white", fill = "black"
      ) +
      labs(x = "Time (s)", y = "Frequency (kHz)") +
      theme(legend.position = "none")

    # Add ROIs in a single layer if they exist
    rois_to_plot <- roi_values() |>
      mutate(id = row_number()) |>
      fsubset(
 