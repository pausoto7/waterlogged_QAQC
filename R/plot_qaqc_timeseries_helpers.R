#' Plot QA/QC time series for logger data
#'
#' Creates an interactive multi-panel Plotly widget showing:
#' water level (WL), dissolved oxygen (DO), barometric pressure (BARO),
#' and conductivity (COND) time series (raw vs. adjusted), corresponding
#' logger/air temperatures, and QA/QC flags.
#'
#' Each data set is filtered to \code{select_station}, and any available
#' QA/QC flags are shown in separate panels. A common time axis with
#' rangeslider is used for navigation.
#'
#' @param wl_data   Optional data frame of water-level records.
#' @param do_data   Optional data frame of dissolved-oxygen records.
#' @param baro_data Optional data frame of barometric records. If \code{NULL}
#'   and WL data contain barometric columns, those are used.
#' @param cond_data Optional data frame of conductivity records.
#' @param select_station Character station code to plot.
#'
#' @return A Plotly \code{htmlwidget} object.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_qaqc_timeseries(
#'   wl_data   = waterlevel_complete_QAQC,
#'   do_data   = station_do_dat,
#'   baro_data = all_checked_baro_data,
#'   cond_data = station_cond_dat,
#'   select_station = "WL_ALBR_ST_30"
#' )
#' }
plot_qaqc_timeseries <- function(wl_data   = NULL,
                                 do_data   = NULL,
                                 baro_data = NULL,
                                 cond_data = NULL,
                                 select_station) {
  
  cols <- qaqc_plot_cols()
  
  # ---- normalise / filter each dataset --------------------------------------
  wl_df   <- normalize_qaqc_df(wl_data,   select_station)
  do_df   <- normalize_qaqc_df(do_data,   select_station)
  baro_df <- normalize_qaqc_df(baro_data, select_station)
  co_df   <- normalize_qaqc_df(cond_data, select_station)
  
  # If no explicit baro_data, see if WL carries baro columns
  if (is.null(baro_df) && !is.null(wl_df)) {
    needed_baro_cols <- c("airpress_kPa", "airpress_kPa_adj",
                          "airtemp_C", "airtemp_C_adj")
    if (all(needed_baro_cols %in% names(wl_df))) {
      baro_df <- wl_df[, c("site_station_code", "timestamp",
                           needed_baro_cols,
                           intersect("baro_qaqc_code", names(wl_df)))]
      baro_df <- normalize_qaqc_df(baro_df, select_station)
    }
  }
  
  if (is.null(wl_df) && is.null(do_df) && is.null(baro_df) && is.null(co_df)) {
    stop("plot_qaqc_timeseries(): no data for station '", select_station,
         "' in wl_data, do_data, baro_data, or cond_data.")
  }
  
  has_wl   <- !is.null(wl_df)   && all(c("waterlevel_m", "waterlevel_m_adj") %in% names(wl_df))
  has_do   <- !is.null(do_df)   && all(c("do_mgl", "do_mgl_adj")           %in% names(do_df))
  has_baro <- !is.null(baro_df) && all(c("airpress_kPa", "airpress_kPa_adj",
                                         "airtemp_C", "airtemp_C_adj")    %in% names(baro_df))
  has_co   <- !is.null(co_df)   && all(c("conduct_uScm", "conduct_uScm_adj") %in% names(co_df))
  
  # x-ranges for each dataset (used so flag panels don't shrink the view)
  if (!is.null(do_df))   do_x_range   <- range(do_df$timestamp,   na.rm = TRUE)
  if (!is.null(wl_df))   wl_x_range   <- range(wl_df$timestamp,   na.rm = TRUE)
  if (!is.null(co_df))   co_x_range   <- range(co_df$timestamp,   na.rm = TRUE)
  if (!is.null(baro_df)) baro_x_range <- range(baro_df$timestamp, na.rm = TRUE)
  
  panels  <- list()
  heights <- numeric(0)
  
  # ---- DO PANELS -------------------------------------------------------------
  if (has_do) {
    # 1) DO timeseries (mg/L)
    p_do <- ggplot2::ggplot(do_df, ggplot2::aes(x = .data$timestamp)) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$do_mgl, colour = "do_raw"),
        alpha = 0.5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$do_mgl_adj, colour = "do_adj")
      ) +
      ggplot2::scale_color_manual(values = c(
        do_raw = cols$do_raw,
        do_adj = cols$do_adj
      )) +
      ggplot2::labs(
        title  = paste("Dissolved oxygen -", select_station),
        y      = "DO (mg/L)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()
    
    panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_do))))
    heights <- c(heights, 0.22)
    
    # 2) DO temperature panel
    has_do_temp      <- "watertemp_C_do"      %in% names(do_df)
    has_do_temp_adj  <- "watertemp_C_do_adj"  %in% names(do_df)
    has_air_baro     <- "airtemp_C_baro"      %in% names(do_df)
    
    if (has_do_temp || has_do_temp_adj || has_air_baro) {
      p_do_temp <- ggplot2::ggplot(do_df, ggplot2::aes(x = .data$timestamp))
      line_cols <- c()
      
      if (has_do_temp) {
        p_do_temp <- p_do_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = .data$watertemp_C_do, colour = "water_temp"),
            alpha = 0.5
          )
        line_cols["water_temp"] <- cols$do_temp_raw
      }
      
      if (has_do_temp_adj) {
        p_do_temp <- p_do_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = .data$watertemp_C_do_adj, colour = "water_temp_adj")
          )
        line_cols["water_temp_adj"] <- cols$do_temp_adj
      }
      
      if (has_air_baro) {
        p_do_temp <- p_do_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = .data$airtemp_C_baro, colour = "airtemp"),
            alpha = 0.5
          )
        line_cols["airtemp"] <- cols$do_air_temp
      }
      
      p_do_temp <- p_do_temp +
        ggplot2::scale_color_manual(values = line_cols) +
        ggplot2::labs(
          title  = "DO logger & baro temperature",
          y      = "Temperature (\u00B0C)",
          x      = "",
          colour = ""
        ) +
        ggplot2::theme_classic()
      
      panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_do_temp))))
      heights <- c(heights, 0.18)
    }
    
    # 3) DO QA/QC flags panel
    do_flag_cols <- c(
      "do_error",
      "do_neg_between",
      "do_neg_lt_minus1",
      "do_high",
      "temp_flag_ice",
      "temp_neg_between",
      "temp_neg_leq_minus1",
      "flag_do_dry"
    )
    
    do_flags <- build_flag_long(do_df, do_flag_cols)
    
    if (!is.null(do_flags)) {
      levels_do <- levels(do_flags$flag_type)
      style_do  <- qaqc_flag_style(levels_do)
      
      p_do_qaqc <- ggplot2::ggplot(
        do_flags,
        ggplot2::aes(
          x      = .data$timestamp,
          y      = .data$flag_type,
          shape  = .data$flag_type,
          colour = .data$flag_type
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = style_do$shape) +
        ggplot2::scale_color_manual(values = style_do$colour) +
        ggplot2::scale_x_datetime(limits = do_x_range) +
        ggplot2::labs(
          title = "DO QA/QC flags",
          x     = "",
          y     = "",
          shape = "",
          colour = ""
        ) +
        ggplot2::theme_classic()
      
      panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_do_qaqc))))
      heights <- c(heights, 0.10)
    }
  }
  
  # ---- CONDUCTIVITY PANELS ---------------------------------------------------
  if (has_co) {
    # 1) Conductivity timeseries
    p_co <- ggplot2::ggplot(co_df, ggplot2::aes(x = .data$timestamp)) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$conduct_uScm, colour = "cond_raw"),
        alpha = 0.5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$conduct_uScm_adj, colour = "cond_adj")
      ) +
      ggplot2::scale_color_manual(values = c(
        cond_raw = cols$cond_raw,
        cond_adj = cols$cond_adj
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
    
    # 2) Conductivity temperature panel
    has_co_temp      <- "watertemp_C"      %in% names(co_df)
    has_co_temp_adj  <- "watertemp_C_adj"  %in% names(co_df)
    has_co_airtemp   <- "airtemp_C"        %in% names(co_df)
    
    if (has_co_temp || has_co_temp_adj || has_co_airtemp) {
      p_co_temp <- ggplot2::ggplot(co_df, ggplot2::aes(x = .data$timestamp))
      line_cols <- c()
      
      if (has_co_temp) {
        p_co_temp <- p_co_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = .data$watertemp_C, colour = "water_temp"),
            alpha = 0.5
          )
        line_cols["water_temp"] <- cols$cond_temp
      }
      
      if (has_co_temp_adj) {
        p_co_temp <- p_co_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = .data$watertemp_C_adj, colour = "water_temp_adj")
          )
        line_cols["water_temp_adj"] <- cols$cond_temp_adj
      }
      
      if (has_co_airtemp) {
        p_co_temp <- p_co_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = .data$airtemp_C, colour = "airtemp"),
            alpha = 0.5
          )
        line_cols["airtemp"] <- cols$cond_air_temp
      }
      
      p_co_temp <- p_co_temp +
        ggplot2::scale_color_manual(values = line_cols) +
        ggplot2::labs(
          title  = "Conductivity logger temperature",
          y      = "Temperature (\u00B0C)",
          x      = "",
          colour = ""
        ) +
        ggplot2::theme_classic()
      
      panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_co_temp))))
      heights <- c(heights, 0.18)
    }
    
    # 3) Conductivity QA/QC flags panel
    co_flags <- build_qaqc_code_points(co_df, "cond_qaqc_code")
    
    if (!is.null(co_flags)) {
      levels_co <- levels(co_flags$code)
      style_co  <- qaqc_flag_style(levels_co)
      
      p_co_qaqc <- ggplot2::ggplot(
        co_flags,
        ggplot2::aes(
          x      = .data$timestamp,
          y      = .data$code,
          shape  = .data$code,
          colour = .data$code
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = style_co$shape) +
        ggplot2::scale_color_manual(values = style_co$colour) +
        ggplot2::scale_x_datetime(limits = co_x_range) +
        ggplot2::labs(
          title = "Conductivity QA/QC flags",
          x     = "",
          y     = "",
          shape = "",
          colour = ""
        ) +
        ggplot2::theme_classic()
      
      panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_co_qaqc))))
      heights <- c(heights, 0.10)
    }
  }
  
  # ---- WL PANELS -------------------------------------------------------------
  if (has_wl) {
    # WL time series
    p_wl <- ggplot2::ggplot(wl_df, ggplot2::aes(x = .data$timestamp)) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$waterlevel_m, colour = "wl_raw"),
        alpha = 0.5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$waterlevel_m_adj, colour = "wl_adj")
      ) +
      ggplot2::scale_color_manual(values = c(
        wl_raw = cols$wl_raw,
        wl_adj = cols$wl_adj
      )) +
      ggplot2::labs(
        title  = paste("Water level -", select_station),
        y      = "Water level (m)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()
    
    panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_wl))))
    heights <- c(heights, 0.20)
    
    # WL temperature panel
    p_wl_temp <- ggplot2::ggplot(wl_df, ggplot2::aes(x = .data$timestamp))
    line_cols <- c()
    
    if ("watertemp_C" %in% names(wl_df)) {
      p_wl_temp <- p_wl_temp +
        ggplot2::geom_line(
          ggplot2::aes(y = .data$watertemp_C, colour = "water_temp"),
          alpha = 0.5
        )
      line_cols["water_temp"] <- cols$wl_temp
    }
    if ("watertemp_C_adj" %in% names(wl_df)) {
      p_wl_temp <- p_wl_temp +
        ggplot2::geom_line(
          ggplot2::aes(y = .data$watertemp_C_adj, colour = "water_temp_adj")
        )
      line_cols["water_temp_adj"] <- cols$wl_temp_adj
    }
    if ("airtemp_C" %in% names(wl_df)) {
      p_wl_temp <- p_wl_temp +
        ggplot2::geom_line(
          ggplot2::aes(y = .data$airtemp_C, colour = "airtemp"),
          alpha = 0.5
        )
      line_cols["airtemp"] <- cols$wl_air_temp
    }
    
    p_wl_temp <- p_wl_temp +
      ggplot2::scale_color_manual(values = line_cols) +
      ggplot2::labs(
        title  = "WL logger temperatures",
        y      = "Temperature (\u00B0C)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()
    
    panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_wl_temp))))
    heights <- c(heights, 0.18)
    
    # WL flags
    wl_flag_cols <- c(
      "flag_disturbance",
      "flag_ice",
      "neg_lt_minus1",
      "neg_between",
      "flag_wl_dry"
    )
    
    wl_flags <- build_flag_long(wl_df, wl_flag_cols)
    
    if (!is.null(wl_flags)) {
      levels_wl <- levels(wl_flags$flag_type)
      style_wl  <- qaqc_flag_style(levels_wl)
      
      p_wl_qaqc <- ggplot2::ggplot(
        wl_flags,
        ggplot2::aes(
          x      = .data$timestamp,
          y      = .data$flag_type,
          shape  = .data$flag_type,
          colour = .data$flag_type
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = style_wl$shape) +
        ggplot2::scale_color_manual(values = style_wl$colour) +
        ggplot2::scale_x_datetime(limits = wl_x_range) +
        ggplot2::labs(
          title = "WL QA/QC flags",
          x     = "",
          y     = "",
          shape = "",
          colour = ""
        ) +
        ggplot2::theme_classic()
      
      panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_wl_qaqc))))
      heights <- c(heights, 0.10)
    }
  }
  
  # ---- BARO PANELS -----------------------------------------------------------
  if (has_baro) {
    # Air temperature panel
    has_baro_air     <- "airtemp_C"     %in% names(baro_df)
    has_baro_air_adj <- "airtemp_C_adj" %in% names(baro_df)
    
    if (has_baro_air || has_baro_air_adj) {
      p_baro_temp <- ggplot2::ggplot(baro_df, ggplot2::aes(x = .data$timestamp))
      line_cols   <- c()
      
      if (has_baro_air) {
        p_baro_temp <- p_baro_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = .data$airtemp_C, colour = "airtemp"),
            alpha = 0.6
          )
        line_cols["airtemp"] <- cols$baro_air_raw
      }
      
      if (has_baro_air_adj) {
        p_baro_temp <- p_baro_temp +
          ggplot2::geom_line(
            ggplot2::aes(y = .data$airtemp_C_adj, colour = "airtemp_adj"),
            alpha = 0.9
          )
        line_cols["airtemp_adj"] <- cols$baro_air_adj
      }
      
      p_baro_temp <- p_baro_temp +
        ggplot2::scale_color_manual(values = line_cols) +
        ggplot2::labs(
          title  = paste("Barometric air temperature -", select_station),
          y      = "Air temperature (\u00B0C)",
          x      = "",
          colour = ""
        ) +
        ggplot2::theme_classic()
      
      panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_baro_temp))))
      heights <- c(heights, 0.18)
    }
    
    # Air pressure panel
    p_baro_press <- ggplot2::ggplot(baro_df, ggplot2::aes(x = .data$timestamp)) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$airpress_kPa, colour = "press_raw"),
        alpha = 0.5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$airpress_kPa_adj, colour = "press_adj"),
        alpha = 0.9
      ) +
      ggplot2::scale_color_manual(values = c(
        press_raw = cols$baro_p_raw,
        press_adj = cols$baro_p_adj
      )) +
      ggplot2::labs(
        title  = paste("Barometric pressure -", select_station),
        y      = "Air pressure (kPa)",
        x      = "",
        colour = ""
      ) +
      ggplot2::theme_classic()
    
    panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_baro_press))))
    heights <- c(heights, 0.20)
    
    # BARO flags panel
    baro_flags <- build_qaqc_code_points(baro_df, "baro_qaqc_code")
    
    if (!is.null(baro_flags)) {
      levels_baro <- levels(baro_flags$code)
      style_baro  <- qaqc_flag_style(levels_baro)
      
      p_baro_qaqc <- ggplot2::ggplot(
        baro_flags,
        ggplot2::aes(
          x      = .data$timestamp,
          y      = .data$code,
          shape  = .data$code,
          colour = .data$code
        )
      ) +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::scale_shape_manual(values = style_baro$shape) +
        ggplot2::scale_color_manual(values = style_baro$colour) +
        ggplot2::scale_x_datetime(limits = baro_x_range) +
        ggplot2::labs(
          title = "BARO QA/QC flags",
          x     = "Timestamp",
          y     = "",
          shape = "",
          colour = ""
        ) +
        ggplot2::theme_classic()
      
      panels  <- c(panels, list(suppressWarnings(plotly::ggplotly(p_baro_qaqc))))
      heights <- c(heights, 0.10)
    }
  }
  
  # ---- combine panels --------------------------------------------------------
  if (!length(panels)) {
    stop("plot_qaqc_timeseries(): no panels were built.")
  }
  
  heights <- heights / sum(heights)
  
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
  
  plots
}
