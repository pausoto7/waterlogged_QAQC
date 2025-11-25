waterlevel_qaqc <- function(input_data,
                            select_station,
                            plot_only = FALSE) {

    # 1. Basic checks and subsetting

  required_cols <- c(
    "site_station_code",
    "timestamp",
    "waterlevel_m",
    "watertemp_C",
    "waterpress_kPa",
    "airpress_kPa",
    "airtemp_C"
  )
  
  missing_cols <- setdiff(required_cols, names(input_data))
  if (length(missing_cols) > 0) {
    stop(
      "waterlevel_qaqc(): missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  

  #  Subset to station + init QA/QC cols ---------------------------------
  output_data <- input_data %>%
    dplyr::filter(site_station_code == select_station) %>%
    dplyr::mutate(
      waterlevel_m_adj = waterlevel_m,  # reserved if you ever adjust WL
      watertemp_C_adj  = watertemp_C,
      wl_qaqc_adj      = NA_character_,
      wl_qaqc_code     = NA_character_,
      wl_qaqc_note     = NA_character_
    )
  
  if (nrow(output_data) == 0) {
    stop("waterlevel_qaqc(): no rows found for station '", select_station, "'.")
  }
  
  
  
  # 3. Apply QA/QC rules (only if we're actually adjusting, not plot-only)

  if (!plot_only) {
    output_data <- output_data %>%
      dplyr::mutate(
        # helper columns
        wl_lead   = dplyr::lead(waterlevel_m),
        wl_diff   = waterlevel_m - wl_lead,
        
        flag_disturbance = !is.na(waterlevel_m) & !is.na(wl_diff) &
          abs(wl_diff) > 0.1,
        flag_dry = !is.na(waterlevel_m) & !is.na(watertemp_C) &
          !is.na(waterpress_kPa) & !is.na(airpress_kPa) &
          (watertemp_C > 25 | waterpress_kPa <= airpress_kPa),
        flag_ice = !is.na(waterlevel_m) & !is.na(watertemp_C) &
          watertemp_C < 0.2,
        neg_lt_minus1 = !is.na(waterlevel_m) & !is.na(watertemp_C) &
          watertemp_C < -1,
        neg_between   = !is.na(waterlevel_m) & !is.na(watertemp_C) &
          watertemp_C < 0 & watertemp_C > -1,
        
        watertemp_C_adj = dplyr::case_when(
          neg_lt_minus1 ~ NA_real_,
          neg_between   ~ 0,
          TRUE          ~ watertemp_C_adj
        ),
        wl_qaqc_adj = dplyr::case_when(
          neg_lt_minus1 ~ "REMOVED",
          neg_between   ~ "REPLACED",
          TRUE          ~ wl_qaqc_adj
        ),
        wl_qaqc_note = dplyr::case_when(
          neg_lt_minus1 ~ "water temp out of range",
          neg_between   ~ "corrected to zero",
          TRUE          ~ wl_qaqc_note
        ),
        wl_qaqc_code = dplyr::case_when(
          neg_lt_minus1 ~ "NEGATIVE_WT",
          neg_between   ~ "NEGATIVE_WT",
          flag_ice      ~ "FLAG_ICE",
          flag_disturbance ~ "FLAG_DISTURBANCE",
          flag_dry      ~ "FLAG_DRY",
          TRUE          ~ wl_qaqc_code
        )
      ) %>%
      dplyr::select(
        -wl_lead, -wl_diff,
        -flag_disturbance, -flag_dry, -flag_ice,
        -neg_lt_minus1, -neg_between
      )
  }
  
  # 4. Plots
  flagged_points <- output_data %>%
    tidyr::drop_na(wl_qaqc_code) %>%
    dplyr::filter(grepl("FLAG", wl_qaqc_code))
  
  p <- ggplot2::ggplot(output_data, ggplot2::aes(x = timestamp)) +
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
  
  q <- ggplot2::ggplot(output_data, ggplot2::aes(x = timestamp)) +
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
        tickmode = "auto", 
        nticks = 20
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
      plot_bgcolor = "#e5ecf6")
  
  # 5. Return --------------------------------------------------------------
  
  if (!plot_only) {
    message(
      "Results returned as list: [[1]] = waterlevel data with QAQC flags, ",
      "[[2]] = interactive plots."
    )
    return(list(output_data, plots))
  } else {
    return(plots)
  }
}