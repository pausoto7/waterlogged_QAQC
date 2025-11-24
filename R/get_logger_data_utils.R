



# ---- Helpers ---------------------------------------------------------------
normalise_metric <- function(metric, allow_all = FALSE) {
  if (is.null(metric) || length(metric) != 1L) {
    stop("`metric` must be a single, non-NULL value.", call. = FALSE)
  }
  
  m <- tolower(trimws(metric))
  m <- gsub("\\s+", "", m)
  
  if (m == "all") {
    if (allow_all) return("all")
    stop("`metric = 'all'` is only allowed when `temporal_scale = 'none'`.", call. = FALSE)
  }
  
  if (m %in% c("waterlevel", "stage"))               return("waterlevel")
  if (m %in% c("do", "dissolvedoxygen", "oxygen"))   return("dissolvedoxygen")
  if (m %in% c("baro", "barometric", "airpressure")) return("barometric")
  if (m %in% c("watertemp", "watertemperature"))     return("watertemp")
  if (m %in% c("airtemp", "airtemperature"))         return("airtemp")
  
  stop(
    "Unknown `metric`. Expected one of: ",
    "'waterlevel', 'dissolvedoxygen', 'barometric', 'watertemp', 'airtemp'",
    " (case and spaces ignored). You gave: '", metric, "'.",
    call. = FALSE
  )
}

normalise_temporal_scale <- function(x) {
  x_clean <- tolower(trimws(x))
  x_clean <- gsub("\\s+", "", x_clean)
  
  if (x_clean %in% c("", "none", "raw"))              return("none")
  if (x_clean %in% c("hour", "hours", "hourly", "1h", "perhour")) return("hourly")
  if (x_clean %in% c("day", "daily", "perday"))       return("daily")
  if (x_clean %in% c("week", "weekly"))               return("weekly")
  if (x_clean %in% c("month", "monthly"))             return("monthly")
  if (x_clean %in% c("year", "yearly", "annual", "annually")) return("yearly")
  
  stop(
    "Invalid `temporal_scale`. Expected one of: none, hourly, daily, weekly, ",
    "monthly, yearly (case and spaces ignored). Got: ", x,
    call. = FALSE
  )
}

normalise_metric <- function(metric) {
  if (is.null(metric)) {
    stop("`metric` must be supplied when `temporal_scale` is not 'none'.",
         call. = FALSE)
  }
  m <- tolower(trimws(metric))
  m <- gsub("\\s+", "", m)
  
  if (m %in% c("waterlevel", "stage"))              return("waterlevel")
  if (m %in% c("do", "dissolvedoxygen", "oxygen"))  return("dissolvedoxygen")
  if (m %in% c("baro", "barometric", "airpressure"))return("barometric")
  if (m %in% c("watertemp", "watertemperature"))    return("watertemp")
  if (m %in% c("airtemp", "airtemperature"))        return("airtemp")
  
  stop(
    "Unknown `metric`. Expected one of: waterlevel, dissolvedoxygen, ",
    "barometric, watertemp, airtemp. Got: ", metric,
    call. = FALSE
  )
}

resolve_data_processing_pattern <- function(data_processing) {
  
  # --- Basic input check ------------------------------------------------------
  if (is.null(data_processing) || length(data_processing) != 1L) {
    stop("`data_processing` must be a single value.", call. = FALSE)
  }
  
  # --- Normalise input --------------------------------------------------------
  dp <- tolower(trimws(data_processing))
  dp <- gsub("\\s+", "", dp)  # remove internal spaces
  
  # --- Exact mappings ---------------------------------------------------------
  if (dp %in% c("raw"))                     dp <- "raw"
  else if (dp %in% c("v0.1", "v01", "0.1")) dp <- "v0.1"
  else if (dp %in% c("v0.2", "v02", "0.2")) dp <- "v0.2"
  else if (dp %in% c("v0.3", "v03", "0.3")) dp <- "v0.3"
  else if (dp %in% c("v1.0", "v10", "1.0", "1", "final", "processed")) dp <- "v1.0"
  else {
    # --- Helpful partial matching ---------------------------------------------
    if (grepl("^raw", dp))        dp <- "raw"
    else if (grepl("0\\.1$", dp)) dp <- "v0.1"
    else if (grepl("0\\.2$", dp)) dp <- "v0.2"
    else if (grepl("0\\.3$", dp)) dp <- "v0.3"
    else if (grepl("^v1", dp))    dp <- "v1.0"
    else {
      stop(
        "`data_processing` must be one of:\n",
        "  - 'raw'\n",
        "  - 'v0.1'\n",
        "  - 'v0.2'\n",
        "  - 'v0.3'\n",
        "  - 'v1.0' (final version)\n\n",
        "You gave: '", data_processing, "'.",
        call. = FALSE
      )
    }
  }
  
  # --- Map to filename pattern ------------------------------------------------
  pattern <- switch(
    dp,
    "raw"  = "\\.csv$",          # ANY csv
    "v0.1" = "v0.1\\.csv$",      # only v0.1
    "v0.2" = "v0.2\\.csv$",
    "v0.3" = "v0.3\\.csv$",
    "v1.0" = "v1.0\\.csv$",
    stop("Unexpected `data_processing` value after QC: ", dp, call. = FALSE)
  )
  
  list(data_processing = dp, pattern = pattern)
}

read_logger_file <- function(file) {
  dat <- utils::read.csv(
    file,
    header           = TRUE,
    stringsAsFactors = FALSE
  )
  
  if (!"timestamp" %in% names(dat)) {
    stop("File '", basename(file), "' does not contain a 'timestamp' column.", call. = FALSE)
  }
  if (!"site_station_code" %in% names(dat)) {
    stop("File '", basename(file), "' does not contain 'site_station_code'.", call. = FALSE)
  }
  
  # timestamps in UTC (consistent with earlier QA/QC)
  dat$timestamp <- lubridate::ymd_hms(dat$timestamp, tz = "UTC", quiet = TRUE)
  
  dat$site_station_code <- as.factor(dat$site_station_code)
  
  # QA/QC factors if present (keep behaviour similar to original)
  factor_cols <- c(
    "sn", "baro_code", "qaqc_code", "wl_qaqc_code",
    "do_qaqc_code", "wt_qaqc_code", "at_qaqc_code", "baro_qaqc_code"
  )
  for (col in intersect(factor_cols, names(dat))) {
    dat[[col]] <- as.factor(dat[[col]])
  }
  
  dat
}

summarise_clean_data <- function(dat, metric_norm, temporal_scale) {
  # summarisation only allowed for v1.0/clean, enforced before calling
  
  # create time_group
  dat <- dat %>%
    dplyr::mutate(
      time_group = dplyr::case_when(
        temporal_scale == "hourly"  ~ lubridate::floor_date(timestamp, "hour"),
        temporal_scale == "daily"   ~ as.Date(timestamp),
        temporal_scale == "weekly"  ~ lubridate::floor_date(timestamp, "week", week_start = 1),
        temporal_scale == "monthly" ~ lubridate::floor_date(timestamp, "month"),
        temporal_scale == "yearly"  ~ lubridate::floor_date(timestamp, "year"),
        TRUE ~ NA
      )
    )
  
  if (all(is.na(dat$time_group))) {
    stop("Failed to create `time_group` for temporal_scale = ", temporal_scale, call. = FALSE)
  }
  
  group_vars <- c("site_station_code", "time_group")
  
  if (metric_norm == "dissolvedoxygen") {
    required <- c("do_mgl_U26_adj", "do_percsat_U26_adj", "watertemp_C_U26_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'dissolvedoxygen', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct data_processing / folder?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
      dplyr::summarise(
        mean_do_mgl      = mean(do_mgl_U26_adj,      na.rm = TRUE),
        mean_do_percsat  = mean(do_percsat_U26_adj,  na.rm = TRUE),
        mean_watertemp_C = mean(watertemp_C_U26_adj, na.rm = TRUE),
        min_do_mgl       = min(do_mgl_U26_adj,       na.rm = TRUE),
        min_do_percsat   = min(do_percsat_U26_adj,   na.rm = TRUE),
        min_watertemp_C  = min(watertemp_C_U26_adj,  na.rm = TRUE),
        max_do_mgl       = max(do_mgl_U26_adj,       na.rm = TRUE),
        max_do_percsat   = max(do_percsat_U26_adj,   na.rm = TRUE),
        max_watertemp_C  = max(watertemp_C_U26_adj,  na.rm = TRUE),
        .groups          = "drop"
      )
    
  } else if (metric_norm == "waterlevel") {
    required <- c("waterlevel_m_U20_adj", "watertemp_C_U20_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'waterlevel', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct data_processing / folder?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
      dplyr::summarise(
        mean_waterlevel_m = mean(waterlevel_m_U20_adj, na.rm = TRUE),
        mean_watertemp_C  = mean(watertemp_C_U20_adj,  na.rm = TRUE),
        min_waterlevel_m  = min(waterlevel_m_U20_adj,  na.rm = TRUE),
        min_watertemp_C   = min(watertemp_C_U20_adj,   na.rm = TRUE),
        max_waterlevel_m  = max(waterlevel_m_U20_adj,  na.rm = TRUE),
        max_watertemp_C   = max(watertemp_C_U20_adj,   na.rm = TRUE),
        .groups           = "drop"
      )
    
  } else if (metric_norm == "barometric") {
    required <- c("airpress_kPa_U20", "airtemp_C_U20_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'barometric', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct data_processing / folder?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
      dplyr::summarise(
        mean_airpress_kPa = mean(airpress_kPa_U20,    na.rm = TRUE),
        mean_airtemp_C    = mean(airtemp_C_U20_adj,   na.rm = TRUE),
        min_airpress_kPa  = min(airpress_kPa_U20,     na.rm = TRUE),
        min_airtemp_C     = min(airtemp_C_U20_adj,    na.rm = TRUE),
        max_airpress_kPa  = max(airpress_kPa_U20,     na.rm = TRUE),
        max_airtemp_C     = max(airtemp_C_U20_adj,    na.rm = TRUE),
        .groups           = "drop"
      )
    
  } else if (metric_norm == "airtemp") {
    required <- c("airtemp_C_TidbiT_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'airtemp', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct data_processing / folder?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
      dplyr::summarise(
        mean_airtemp_C = mean(airtemp_C_TidbiT_adj, na.rm = TRUE),
        min_airtemp_C  = min(airtemp_C_TidbiT_adj,  na.rm = TRUE),
        max_airtemp_C  = max(airtemp_C_TidbiT_adj,  na.rm = TRUE),
        .groups        = "drop"
      )
    
  } else if (metric_norm == "watertemp") {
    required <- c("watertemp_C_TidbiT_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'watertemp', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct data_processing / folder?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(all_of(group_vars))) %>%
      dplyr::summarise(
        mean_watertemp_C = mean(watertemp_C_TidbiT_adj, na.rm = TRUE),
        min_watertemp_C  = min(watertemp_C_TidbiT_adj,  na.rm = TRUE),
        max_watertemp_C  = max(watertemp_C_TidbiT_adj,  na.rm = TRUE),
        .groups          = "drop"
      )
    
  } else {
    stop("Metric '", metric_norm, "' not implemented.", call. = FALSE)
  }
  
  out <- out %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))
  
  out[] <- lapply(out, function(x) {
    if (is.numeric(x)) {
      x[is.infinite(x) | is.nan(x)] <- NA_real_
    }
    x
  })
  
  out
}

build_plots <- function(dat_sum, metric_norm, temporal_scale) {
  timescale_label <- switch(
    temporal_scale,
    "hourly"  = "Hour",
    "daily"   = "Date",
    "weekly"  = "Week starting",
    "monthly" = "Month",
    "yearly"  = "Year"
  )
  
  plot1 <- plot2 <- plot3 <- NULL
  
  if (metric_norm == "dissolvedoxygen") {
    ylab_temp <- "Water temperature (\u00b0C)"
    ylab_do   <- bquote('Dissolved oxygen (mg·'~'L'^-1~')')
    ylab_sat  <- "Dissolved oxygen (% air sat.)"
    
    plot1 <- ggplot2::ggplot(dat_sum, ggplot2::aes(x = time_group, y = mean_watertemp_C)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = min_watertemp_C, ymax = max_watertemp_C), alpha = 0.3) +
      ggplot2::facet_wrap(~site_station_code) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::labs(x = timescale_label, y = ylab_temp)
    
    plot2 <- ggplot2::ggplot(dat_sum, ggplot2::aes(x = time_group, y = mean_do_mgl)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = min_do_mgl, ymax = max_do_mgl), alpha = 0.3) +
      ggplot2::facet_wrap(~site_station_code) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::labs(x = timescale_label, y = ylab_do)
    
    plot3 <- ggplot2::ggplot(dat_sum, ggplot2::aes(x = time_group, y = mean_do_percsat)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = min_do_percsat, ymax = max_do_percsat), alpha = 0.3) +
      ggplot2::facet_wrap(~site_station_code) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::labs(x = timescale_label, y = ylab_sat)
    
  } else if (metric_norm == "waterlevel") {
    ylab_temp <- "Water temperature (\u00b0C)"
    ylab_wl   <- "Water level (m)"
    
    plot1 <- ggplot2::ggplot(dat_sum, ggplot2::aes(x = time_group, y = mean_watertemp_C)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = min_watertemp_C, ymax = max_watertemp_C), alpha = 0.3) +
      ggplot2::facet_wrap(~site_station_code) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::labs(x = timescale_label, y = ylab_temp)
    
    plot2 <- ggplot2::ggplot(dat_sum, ggplot2::aes(x = time_group, y = mean_waterlevel_m)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = min_waterlevel_m, ymax = max_waterlevel_m), alpha = 0.3) +
      ggplot2::facet_wrap(~site_station_code) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::labs(x = timescale_label, y = ylab_wl)
    
  } else if (metric_norm == "barometric") {
    ylab_temp  <- "Air temperature (\u00b0C)"
    ylab_press <- "Air pressure (kPa)"
    
    plot1 <- ggplot2::ggplot(dat_sum, ggplot2::aes(x = time_group, y = mean_airtemp_C)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = min_airtemp_C, ymax = max_airtemp_C), alpha = 0.3) +
      ggplot2::facet_wrap(~site_station_code) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::labs(x = timescale_label, y = ylab_temp)
    
    plot2 <- ggplot2::ggplot(dat_sum, ggplot2::aes(x = time_group, y = mean_airpress_kPa)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = min_airpress_kPa, ymax = max_airpress_kPa), alpha = 0.3) +
      ggplot2::facet_wrap(~site_station_code) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::labs(x = timescale_label, y = ylab_press)
    
  } else if (metric_norm == "airtemp") {
    ylab_temp <- "Air temperature (\u00b0C)"
    
    plot1 <- ggplot2::ggplot(dat_sum, ggplot2::aes(x = time_group, y = mean_airtemp_C)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = min_airtemp_C, ymax = max_airtemp_C), alpha = 0.3) +
      ggplot2::facet_wrap(~site_station_code) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::labs(x = timescale_label, y = ylab_temp)
    
  } else if (metric_norm == "watertemp") {
    ylab_temp <- "Water temperature (\u00b0C)"
    
    plot1 <- ggplot2::ggplot(dat_sum, ggplot2::aes(x = time_group, y = mean_watertemp_C)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = min_watertemp_C, ymax = max_watertemp_C), alpha = 0.3) +
      ggplot2::facet_wrap(~site_station_code) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::labs(x = timescale_label, y = ylab_temp)
  }
  
  list(plot1 = plot1, plot2 = plot2, plot3 = plot3)
}
