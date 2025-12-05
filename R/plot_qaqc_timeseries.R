plot_qaqc_timeseries <- function(wl_data   = NULL,
                                 do_data   = NULL,
                                 baro_data = NULL,
                                 cond_data   = NULL,
                                 select_station) {
  
  # ---- internal helpers ------------------------------------------------------
  
  normalize_df <- function(df) {
    if (is.null(df)) return(NULL)
    
    req_cols <- c("site_station_code", "timestamp")
    missing  <- setdiff(req_cols, names(df))
    if (length(missing) > 0) {
      stop(
        "plot_qaqc_timeseries(): input data is missing required column(s): ",
        paste(missing, collapse = ", ")
      )
    }
    
    if (!inherits(df$timestamp, "POSIXt")) {
      df <- df %>%
        dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp))
    }
    
    df <- df %>%
      dplyr::filter(site_station_code == select_station) %>%
      dplyr::arrange(timestamp)
    
    if (nrow(df) == 0) return(NULL)
    df
  }
  
  build_qaqc_points <- function(df, code_col) {
    if (is.null(df) || !code_col %in% names(df)) return(NULL)
    
    out <- df %>%
      dplyr::select(timestamp, !!rlang::sym(code_col)) %>%
      dplyr::filter(!is.na(.data[[code_col]]),
                    .data[[code_col]] != "") %>%
      dplyr::mutate(code = factor(.data[[code_col]])) %>%
      dplyr::select(timestamp, code)
    
    if (nrow(out) == 0) return(NULL)
    out
  }
  
  build_shape_colour <- function(levels_vec) {
    base_shapes <- c(16, 17, 15, 3, 4, 1, 2)
    base_cols   <- c("navy", "black", "burlywood", "darkslategrey",
                     "firebrick", "dodgerblue4", "darkgreen")
    
    n <- length(levels_vec)
    shape_map <- base_shapes[seq_len(n)]
    names(shape_map) <- levels_vec
    
    colour_map <- base_cols[seq_len(n)]
    names(colour_map) <- levels_vec
    
    list(shape = shape_map, colour = colour_map)
  }
  
  # ---- normalize / filter each dataset --------------------------------------
  
  wl_df   <- normalize_df(wl_data)
  do_df   <- normalize_df(do_data)
  baro_df <- normalize_df(baro_data)
  co_df   <- normalize_df(cond_data)
  
  # If no explicit baro_data, see if WL carries baro columns
  if (is.null(baro_df) && !is.null(wl_df)) {
    needed_baro_cols <- c("airpress_kPa", "airpress_kPa_adj",
                          "airtemp_C", "airtemp_C_adj")
    if (all(needed_baro_cols %in% names(wl_df))) {
      baro_df <- wl_df[, c("site_station_code", "timestamp",
                           needed_baro_cols,
                           intersect("baro_qaqc_code", names(wl_df)))]
    }
  }
  
  if (is.null(wl_df) && is.null(do_df) && is.null(baro_df) && is.null(co_df)) {
    stop("plot_qaqc_timeseries(): no data for station '", select_station,
         "' in wl_data, do_data, baro_data, or cond_data.")
  }
  
  has_wl   <- !is.null(wl_df)   && all(c("waterlevel_m", "waterlevel_m_adj") %in% names(wl_df))
  has_do   <- !is.null(do_df)   && all(c("do_mgl", "do_mgl_adj") %in% names(do_df))
  has_baro <- !is.null(baro_df) && all(c("airpress_kPa", "airpress_kPa_adj",
                                         "airtemp_C", "airtemp_C_adj") %in% names(baro_df))
  has_co   <- !is.null(co_df)   && all(c("conduct_uScm", "conduct_uScm_adj") %in% names(co_df))
  
  panels  <- list()
  heights <- numeric(0)
  
  # ---- DO PANELS -------------------------------------------------------------
  if (has_do) {
    
    p_do <- ggplot2::ggplot(do_df, ggplot2::aes(x = timestamp)) +
      ggplot2::geom_line(
        ggplot2::aes(y = do_mgl,     colour = "do_mgl"),
        alpha = 0.5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = do_mgl_adj, colour = "do_mgl_adj")
      ) +
      ggplot2::scale_color_manual(values = c(
        do_mgl     = "#264653",
        do_mgl_adj = "#2a9d8f"
      )) +
      ggplot2::labs(
        title  = paste("Dissolved Oxygen -", select_station),
        y      = "DO (mg/L)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()
    
    panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_do))))
    heights <- c(heights, 0.22)
    
    do_flags <- build_qaqc_points(do_df, "do_qaqc_code")
    
    if (!is.null(do_flags)) {
      levels_do <- levels(do_flags$code)
      maps_do   <- build_shape_colour(levels_do)
      
      p_do_qaqc <- ggplot2::ggplot(
        do_flags,
        ggplot2::aes(
          x      = timestamp,
          y      = code,
          shape  = code,
          colour = code
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = maps_do$shape) +
        ggplot2::scale_color_manual(values = maps_do$colour) +
        ggplot2::labs(
          title = "DO QA/QC",
          x     = "",
          y     = "",
          shape = "",
          colour = ""
        ) +
        ggplot2::theme_classic()
      
      panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_do_qaqc))))
      heights <- c(heights, 0.12)
    }
  }
  
  # ---- CONDUCTIVITY PANELS ---------------------------------------------------
  if (has_co) {
    
    # conductivity time series
    p_co <- ggplot2::ggplot(co_df, ggplot2::aes(x = timestamp)) +
      ggplot2::geom_line(
        ggplot2::aes(y = conduct_uScm,     colour = "conduct_uScm"),
        alpha = 0.5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = conduct_uScm_adj, colour = "conduct_uScm_adj")
      ) +
      ggplot2::scale_color_manual(values = c(
        conduct_uScm     = "#00008B",
        conduct_uScm_adj = "#00008B"
      )) +
      ggplot2::labs(
        title  = paste("Conductivity -", select_station),
        y      = "Conductivity (µS/cm)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()
    
    panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_co))))
    heights <- c(heights, 0.22)
    
    # conductivity QAQC panel (if co_qaqc_code exists)
    co_flags <- build_qaqc_points(co_df, "co_qaqc_code")
    
    if (!is.null(co_flags)) {
      levels_co <- levels(co_flags$code)
      maps_co   <- build_shape_colour(levels_co)
      
      p_co_qaqc <- ggplot2::ggplot(
        co_flags,
        ggplot2::aes(
          x      = timestamp,
          y      = code,
          shape  = code,
          colour = code
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = maps_co$shape) +
        ggplot2::scale_color_manual(values = maps_co$colour) +
        ggplot2::labs(
          title = "Conductivity QA/QC",
          x     = "",
          y     = "",
          shape = "",
          colour = ""
        ) +
        ggplot2::theme_classic()
      
      panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_co_qaqc))))
      heights <- c(heights, 0.12)
    }
  }
  
  # ---- WL PANELS -------------------------------------------------------------
  if (has_wl) {
    
    p_wl <- ggplot2::ggplot(wl_df, ggplot2::aes(x = timestamp)) +
      ggplot2::geom_line(
        ggplot2::aes(y = waterlevel_m, colour = "waterlevel_m"),
        alpha = 0.5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = waterlevel_m_adj, colour = "waterlevel_m_adj")
      )
    
    if ("watertemp_C" %in% names(wl_df)) {
      p_wl <- p_wl +
        ggplot2::geom_line(
          ggplot2::aes(y = watertemp_C, colour = "watertemp_C"),
          alpha = 0.5
        )
    }
    if ("watertemp_C_adj" %in% names(wl_df)) {
      p_wl <- p_wl +
        ggplot2::geom_line(
          ggplot2::aes(y = watertemp_C_adj, colour = "watertemp_C_adj")
        )
    }
    if ("airtemp_C" %in% names(wl_df)) {
      p_wl <- p_wl +
        ggplot2::geom_line(
          ggplot2::aes(y = airtemp_C, colour = "airtemp_C"),
          alpha = 0.5
        )
    }
    
    p_wl <- p_wl +
      ggplot2::scale_color_manual(values = c(
        waterlevel_m     = "#233d4d",
        waterlevel_m_adj = "#233d4d",
        watertemp_C      = "#619b8a",
        watertemp_C_adj  = "#619b8a",
        airtemp_C        = "#fe7f2d"
      )) +
      ggplot2::labs(
        title  = paste("Water Level & Temperatures -", select_station),
        y      = "WL (m) / Temp (°C)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()
    
    panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_wl))))
    heights <- c(heights, 0.22)
    
    wl_flags <- build_qaqc_points(wl_df, "wl_qaqc_code")
    
    if (!is.null(wl_flags)) {
      levels_wl <- levels(wl_flags$code)
      maps_wl   <- build_shape_colour(levels_wl)
      
      p_wl_qaqc <- ggplot2::ggplot(
        wl_flags,
        ggplot2::aes(
          x      = timestamp,
          y      = code,
          shape  = code,
          colour = code
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = maps_wl$shape) +
        ggplot2::scale_color_manual(values = maps_wl$colour) +
        ggplot2::labs(
          title = "WL QA/QC",
          x     = "",
          y     = "",
          shape = "",
          colour = ""
        ) +
        ggplot2::theme_classic()
      
      panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_wl_qaqc))))
      heights <- c(heights, 0.12)
    }
  }
  
  # ---- BARO PANELS -----------------------------------------------------------
  if (has_baro) {
    
    mean_press <- mean(baro_df$airpress_kPa, na.rm = TRUE)
    
    p_baro <- ggplot2::ggplot(baro_df, ggplot2::aes(x = timestamp)) +
      ggplot2::geom_line(
        ggplot2::aes(y = airtemp_C,     colour = "airtemp_C")
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = airtemp_C_adj, colour = "airtemp_C_adj"),
        alpha = 0.7
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = (airpress_kPa - mean_press) * 10,
                     colour = "airpress_kPa"),
        alpha = 0.6
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = (airpress_kPa_adj - mean_press) * 10,
                     colour = "airpress_kPa_adj"),
        alpha = 0.6
      ) +
      ggplot2::scale_color_manual(values = c(
        airtemp_C        = "orange",
        airtemp_C_adj    = "gold",
        airpress_kPa     = "black",
        airpress_kPa_adj = "darkgreen"
      )) +
      ggplot2::labs(
        title  = paste("Barometric pressure & air temperature -", select_station),
        y      = "Air temp (°C) / (pressure, scaled)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()
    
    panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_baro))))
    heights <- c(heights, 0.22)
    
    baro_flags <- build_qaqc_points(baro_df, "baro_qaqc_code")
    
    if (!is.null(baro_flags)) {
      levels_baro <- levels(baro_flags$code)
      maps_baro   <- build_shape_colour(levels_baro)
      
      p_baro_qaqc <- ggplot2::ggplot(
        baro_flags,
        ggplot2::aes(
          x      = timestamp,
          y      = code,
          shape  = code,
          colour = code
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = maps_baro$shape) +
        ggplot2::scale_color_manual(values = maps_baro$colour) +
        ggplot2::labs(
          title = "BARO QA/QC",
          x     = "Timestamp",
          y     = "",
          shape = "",
          colour = ""
        ) +
        ggplot2::theme_classic()
      
      panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_baro_qaqc))))
      heights <- c(heights, 0.12)
    }
  }
  
  # ---- combine panels --------------------------------------------------------
  plots <- plotly::subplot(
    panels,
    nrows   = length(panels),
    shareX  = TRUE,
    heights = heights,
    margin  = 0.03
  ) %>%
    plotly::layout(
      xaxis = list(
        rangeslider = list(visible = TRUE),
        type        = "date"
      ),
      plot_bgcolor = "#e5ecf6"
    )
  
  return(plots)
}
