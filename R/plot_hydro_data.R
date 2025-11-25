# plot hydro

waterlevel_qaqc_plot <- function(qaqc_data,
                                 select_station) {
  
  # Basic checks ---------------------------------------------------------------
  required_cols <- c(
    "site_station_code",
    "timestamp",
    "waterlevel_m",
    "waterlevel_m_adj",
    "watertemp_C",
    "watertemp_C_adj",
    "airtemp_C",
    "wl_qaqc_code"
  )
  missing_cols <- setdiff(required_cols, names(qaqc_data))
  if (length(missing_cols) > 0) {
    stop(
      "waterlevel_qaqc_plot(): missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  plot_data <- qaqc_data %>%
    dplyr::filter(site_station_code == select_station) %>%
    dplyr::arrange(timestamp)
  
  if (nrow(plot_data) == 0) {
    stop("waterlevel_qaqc_plot(): no rows found for station '", select_station, "'.")
  }
  
  flagged_points <- plot_data %>%
    tidyr::drop_na(wl_qaqc_code) %>%
    dplyr::filter(grepl("FLAG", wl_qaqc_code))
  
  # Top panel: water level -----------------------------------------------------
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = timestamp)) +
    ggplot2::geom_line(
      ggplot2::aes(y = waterlevel_m,     color = "waterlevel_m"),
      alpha = 0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = waterlevel_m_adj, color = "waterlevel_m_adj")
    ) +
    ggplot2::geom_point(
      data  = flagged_points,
      ggplot2::aes(y = 0, shape = as.factor(wl_qaqc_code)),
      color = "black"
    ) +
    ggplot2::scale_color_manual(values = c(
      "waterlevel_m"     = "#233d4d",
      "waterlevel_m_adj" = "#233d4d"
    )) +
    ggplot2::scale_shape_manual(values = c(
      "FLAG_ICE"         = 1,
      "FLAG_DISTURBANCE" = 2,
      "FLAG_DRY"         = 4
    )) +
    ggplot2::labs(
      title = paste("QAQC for:", select_station),
      y     = "Water level (m)",
      x     = "Timestamp",
      color = "",
      shape = ""
    ) +
    ggplot2::theme_classic()
  
  # Bottom panel: temperatures -------------------------------------------------
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
        type        = "date",
        tickmode    = "auto",
        nticks      = 20
      ),
      yaxis  = list(
        zerolinecolor = "black",
        zerolinewidth = 2,
        gridcolor     = "grey30"
      ),
      yaxis2 = list(
        zerolinecolor = "#ffffff",
        zerolinewidth = 2,
        gridcolor     = "#ffffff"
      ),
      plot_bgcolor = "#e5ecf6"
    )
  
  return(plots)
}
