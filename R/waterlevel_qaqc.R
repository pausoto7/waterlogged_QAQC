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
  
  # Work on a copy, subset to the selected station
  
  output_data <- input_data %>%
    filter(site_station_code == select_station)
  
  rownames(output_data) <- NULL
  
  if (nrow(output_data) == 0) {
    stop("waterlevel_qaqc(): no rows found for station '", select_station, "'.")
  }
  
  n <- nrow(output_data)
  
  # 2. Initialise QA/QC columns (always, so plotting works even if plot_only = TRUE)

  output_data <- output_data %>%
    dplyr::mutate(
      waterlevel_m_adj = waterlevel_m,   # optional if you ever adjust WL
      watertemp_C_adj  = watertemp_C,    # required
      wl_qaqc_adj      = NA_character_,
      wl_qaqc_code     = NA_character_,
      wl_qaqc_note     = NA_character_
    )
  
  # 3. Apply QA/QC rules (only if we're actually adjusting, not plot-only)

  if (!plot_only) {
    # --- 3.1 Disturbance flag: > 0.1 m change in 1 time-step (assumed 1 hr) ----
    wl     <- output_data$waterlevel_m
    wl_lead <- c(wl[-1], NA_real_)  # lead(wl) without needing dplyr
    
    wl_diff <- wl - wl_lead
    flag_disturbance <- !is.na(wl) & !is.na(wl_diff) & abs(wl_diff) > 0.1
    
    output_data$wl_qaqc_code[flag_disturbance] <- "FLAG_DISTURBANCE"
    
    # --- 3.2 Dry flag: high water temp OR water pressure <= air pressure ----
    wt   <- output_data$watertemp_C
    wp   <- output_data$waterpress_kPa
    ap   <- output_data$airpress_kPa
    
    flag_dry <-
      !is.na(wl) & !is.na(wt) & !is.na(wp) & !is.na(ap) &
      (wt > 25 | wp <= ap)
    
    output_data$wl_qaqc_code[flag_dry] <- "FLAG_DRY"
    
    # --- 3.3 Ice flag: watertemp_C < 0.2 °C ----
    flag_ice <-
      !is.na(wl) & !is.na(wt) &
      wt < 0.2
    
    output_data$wl_qaqc_code[flag_ice] <- "FLAG_ICE"
    
    # --- 3.4 Negative water temperatures ----
    # (order matters: NEGATIVE_WT should override other codes)
    
    # wt < -1: remove as out-of-range
    neg_lt_minus1 <-
      !is.na(wl) & !is.na(wt) &
      wt < -1
    
    output_data$watertemp_C_adj[neg_lt_minus1] <- NA_real_
    output_data$wl_qaqc_note[neg_lt_minus1]    <- "water temp out of range"
    output_data$wl_qaqc_adj[neg_lt_minus1]     <- "REMOVED"
    output_data$wl_qaqc_code[neg_lt_minus1]    <- "NEGATIVE_WT"
    
    # -1 < wt < 0: corrected to zero
    neg_between <-
      !is.na(wl) & !is.na(wt) &
      wt < 0 & wt > -1
    
    output_data$watertemp_C_adj[neg_between] <- 0
    output_data$wl_qaqc_note[neg_between]    <- "corrected to zero"
    output_data$wl_qaqc_adj[neg_between]     <- "REPLACED"
    output_data$wl_qaqc_code[neg_between]    <- "NEGATIVE_WT"
  }
  
  # 4. Plots

  p <- ggplot(data = output_data, aes(x = timestamp)) +
    geom_line(aes(y = waterlevel_m,     color = "waterlevel_m"),     alpha = 0.5) +
    geom_line(aes(y = waterlevel_m_adj, color = "waterlevel_m_adj")) +
    geom_point(
      data = output_data %>%
        tidyr::drop_na(wl_qaqc_code) %>%
        dplyr::filter(grepl("FLAG", wl_qaqc_code)),
      aes(y = 0, shape = as.factor(wl_qaqc_code)),
      color = "black"
    ) +
    scale_color_manual(values = c(
      "waterlevel_m"     = "#233d4d",
      "waterlevel_m_adj" = "#233d4d"
    )) +
    scale_shape_manual(values = c(
      "FLAG_ICE"         = 1,
      "FLAG_DISTURBANCE" = 2,
      "FLAG_DRY"         = 4
    )) +
    labs(
      title = paste("QAQC for:", select_station),
      y     = "Water level (m)",
      x     = "Timestamp",
      color = "",
      shape = ""
    ) +
    theme_classic()
  
  q <- ggplot(data = output_data, aes(x = timestamp)) +
    geom_line(aes(y = airtemp_C,        color = "airtemp_C"),        alpha = 0.5) +
    geom_line(aes(y = watertemp_C,      color = "watertemp_C"),      alpha = 0.5) +
    geom_line(aes(y = watertemp_C_adj,  color = "watertemp_C_adj")) +
    scale_color_manual(values = c(
      "watertemp_C"     = "#619b8a",
      "watertemp_C_adj" = "#619b8a",
      "airtemp_C"       = "#fe7f2d"
    )) +
    labs(
      title = select_station,
      y     = "Temperature (°C)",
      x     = "Timestamp",
      color = "",
      shape = ""
    ) +
    theme_classic()
  
  plots <- plotly::subplot(
    suppressWarnings(plotly::ggplotly(p)),
    suppressWarnings(plotly::ggplotly(q)),
    heights = c(0.5, 0.5),
    nrows   = 2,
    shareX  = TRUE,
    margin  = 0.05
  )
  
  # 5. Return

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
