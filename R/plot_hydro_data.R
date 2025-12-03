waterlevel_qaqc_plot <- function(qaqc_data,
                                 select_station) {
  
  # Basic checks: only core columns are strictly required ---------------------
  required_cols <- c(
    "site_station_code",
    "timestamp",
    "waterlevel_m",
    "waterlevel_m_adj",
    "watertemp_C",
    "watertemp_C_adj",
    "airtemp_C"
  )
  
  missing_cols <- setdiff(required_cols, names(qaqc_data))
  if (length(missing_cols) > 0) {
    stop(
      "waterlevel_qaqc_plot(): missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (!inherits(qaqc_data$timestamp, "POSIXt")) {
    qaqc_data <- qaqc_data %>%
      dplyr::mutate(
        timestamp = lubridate::ymd_hms(timestamp)
      )
  }
  
  # Subset + sort --------------------------------------------------------------
  plot_data <- qaqc_data %>%
    dplyr::filter(site_station_code == select_station) %>%
    dplyr::arrange(timestamp)
  
  if (nrow(plot_data) == 0) {
    stop("waterlevel_qaqc_plot(): no rows found for station '", select_station, "'.")
  }
  
  # Find any logical flag_* or edit_* columns ----------------------------------
  all_logical <- vapply(plot_data, is.logical, logical(1))
  flag_cols   <- names(plot_data)[all_logical & grepl("^flag_", names(plot_data))]
  edit_cols   <- names(plot_data)[all_logical & grepl("^edit_", names(plot_data))]
  mark_cols   <- c(flag_cols, edit_cols)
  
  if (length(mark_cols) > 0) {
    flagged_points <- plot_data %>%
      dplyr::select(timestamp, dplyr::all_of(mark_cols)) %>%
      tidyr::pivot_longer(
        cols      = dplyr::all_of(mark_cols),
        names_to  = "mark_type",
        values_to = "mark_value"
      ) %>%
      dplyr::filter(mark_value) %>%
      dplyr::mutate(
        mark_type = factor(mark_type)
      )
  } else {
    flagged_points <- NULL
  }
  
  # Top panel: water level -----------------------------------------------------
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = timestamp)) +
    ggplot2::geom_line(
      ggplot2::aes(y = waterlevel_m,     color = "waterlevel_m"),
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = waterlevel_m_adj, color = "waterlevel_m_adj")
    ) +
    ggplot2::scale_color_manual(values = c(
      "waterlevel_m"     = "#233d4d",
      "waterlevel_m_adj" = "#233d4d"
    )) +
    ggplot2::labs(
      title = paste("QAQC for:", select_station),
      y     = "Water level (m)",
      x     = "Timestamp",
      color = ""
    ) +
    ggplot2::theme_classic()
  
  # Middle panel: temperatures -------------------------------------------------
  q <- ggplot2::ggplot(plot_data, ggplot2::aes(x = timestamp)) +
    ggplot2::geom_line(
      ggplot2::aes(y = airtemp_C,       color = "airtemp_C"),
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = watertemp_C,     color = "watertemp_C"),
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = watertemp_C_adj, color = "watertemp_C_adj")
    ) +
    ggplot2::scale_color_manual(values = c(
      "watertemp_C"     = "#619b8a",
      "watertemp_C_adj" = "#619b8a",
      "airtemp_C"       = "#fe7f2d"
    )) +
    ggplot2::labs(
      title = select_station,
      y     = "Temperature (°C)",
      x     = "Timestamp",
      color = ""
    ) +
    ggplot2::theme_classic()
  
  # Bottom panel: flag / edit markers only ------------------------------------
  if (!is.null(flagged_points) && nrow(flagged_points) > 0) {
    
    r <- ggplot2::ggplot(
      flagged_points,
      ggplot2::aes(
        x     = timestamp,
        y     = mark_type,
        shape = mark_type,
        colour = mark_type
      )
    ) +
      ggplot2::geom_point(size = 1.8) +
      ggplot2::scale_shape_manual(
        values = c(
          flag_ice         = 16,  # circle
          flag_disturbance = 17,  # triangle
          flag_dry         = 15,  # square
          edit_spike_flat  = 3,   # plus
          edit_offset      = 4    # cross
        )
      ) +
      ggplot2::scale_color_manual(
        values = c(
          flag_ice         = "navy",
          flag_disturbance = "black",
          flag_dry         = "burlywood",
          edit_spike_flat  = "darkslategrey",
          edit_offset      = "darkslategrey"
        )
      ) +
      ggplot2::labs(
        title = "QA/QC flags & edits",
        x     = "Timestamp",
        y     = "Marker type",
        shape = "",
        colour = ""
      ) +
      ggplot2::theme_classic()
    
    plots <- plotly::subplot(
      suppressWarnings(plotly::ggplotly(p)),
      suppressWarnings(plotly::ggplotly(q)),
      suppressWarnings(plotly::ggplotly(r)),
      heights = c(0.45, 0.35, 0.20),
      nrows   = 3,
      shareX  = TRUE,
      margin  = 0.05
    ) %>%
      plotly::layout(
        xaxis = list(
          rangeslider = list(visible = TRUE),
          type        = "date"
        ),
        plot_bgcolor = "#e5ecf6"
      )
    
  } else {
    # fallback: just WL + temps if no flags/edits ------------------------------
    plots <- plotly::subplot(
      suppressWarnings(plotly::ggplotly(p)),
      suppressWarnings(plotly::ggplotly(q)),
      heights = c(0.5, 0.5),
      nrows   = 2,
      shareX  = TRUE,
      margin  = 0.05
    ) %>%
      plotly::layout(
        xaxis = list(
          rangeslider = list(visible = TRUE),
          type        = "date"
        ),
        plot_bgcolor = "#e5ecf6"
      )
  }
  
  return(plots)
}
