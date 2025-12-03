# ---- Helpers ---------------------------------------------------------------

normalise_metric <- function(metric, allow_all = FALSE) {
  if (is.null(metric) || length(metric) != 1L) {
    stop("`metric` must be a single, non-NULL value.", call. = FALSE)
  }
  
  m <- tolower(trimws(metric))
  m <- gsub("\\s+", "", m)
  
  if (m == "all") {
    if (allow_all) return("all")
    stop("`metric = 'all'` is only allowed when explicitly permitted.", call. = FALSE)
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


resolve_data_processing_pattern <- function(data_processing) {
  
  if (is.null(data_processing) || length(data_processing) != 1L) {
    stop("`data_processing` must be a single value.", call. = FALSE)
  }
  
  dp <- tolower(trimws(data_processing))
  dp <- gsub("\\s+", "", dp)
  
  # explicit mappings
  if (dp %in% c("raw")) {
    dp <- "raw"
  } else if (dp %in% c("v0.1", "v01", "0.1")) {
    dp <- "v0.1"
  } else if (dp %in% c("v0.2", "v02", "0.2")) {
    dp <- "v0.2"
  } else if (dp %in% c("v0.3", "v03", "0.3")) {
    dp <- "v0.3"
  } else if (dp %in% c("v1.0", "v10", "1.0", "1", "final", "processed", "clean")) {
    dp <- "v1.0"
  } else {
    # partial / fuzzy mappings as backup
    if (grepl("^raw", dp))           dp <- "raw"
    else if (grepl("0\\.1$", dp))    dp <- "v0.1"
    else if (grepl("0\\.2$", dp))    dp <- "v0.2"
    else if (grepl("0\\.3$", dp))    dp <- "v0.3"
    else if (grepl("^v1", dp))       dp <- "v1.0"
    else {
      stop(
        "`data_processing` must be one of:\n",
        "  - 'raw'\n",
        "  - 'v0.1'\n",
        "  - 'v0.2'\n",
        "  - 'v0.3'\n",
        "  - 'v1.0' / 'clean' (final version)\n\n",
        "You gave: '", data_processing, "'.",
        call. = FALSE
      )
    }
  }
  
  pattern <- switch(
    dp,
    "raw"  = "\\.csv$",      # any csv
    "v0.1" = "v0.1\\.csv$",
    "v0.2" = "v0.2\\.csv$",
    "v0.3" = "v0.3\\.csv$",
    "v1.0" = "v1.0\\.csv$",
    stop("Unexpected `data_processing` value after QC: ", dp, call. = FALSE)
  )
  
  list(data_processing = dp, pattern = pattern)
}



summarise_clean_data <- function(dat, metric_norm, temporal_scale) {
  # temporal_scale already validated upstream
  
  # --- create time_group ---------------------------------------------
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
  
  # --- metric-specific summaries -------------------------------------
  if (metric_norm == "dissolvedoxygen") {
    
    required <- c("do_mgl_adj", "do_percsat_adj", "watertemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'dissolvedoxygen', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 DO data)?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        mean_do_mgl      = mean(do_mgl_adj,      na.rm = TRUE),
        mean_do_percsat  = mean(do_percsat_adj,  na.rm = TRUE),
        mean_watertemp_C = mean(watertemp_C_adj, na.rm = TRUE),
        min_do_mgl       = min(do_mgl_adj,       na.rm = TRUE),
        min_do_percsat   = min(do_percsat_adj,   na.rm = TRUE),
        min_watertemp_C  = min(watertemp_C_adj,  na.rm = TRUE),
        max_do_mgl       = max(do_mgl_adj,       na.rm = TRUE),
        max_do_percsat   = max(do_percsat_adj,   na.rm = TRUE),
        max_watertemp_C  = max(watertemp_C_adj,  na.rm = TRUE),
        .groups          = "drop"
      )
    
  } else if (metric_norm == "waterlevel") {
    
    required <- c("waterlevel_m_adj", "watertemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'waterlevel', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 WL data)?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        mean_waterlevel_m = mean(waterlevel_m_adj, na.rm = TRUE),
        mean_watertemp_C  = mean(watertemp_C_adj,  na.rm = TRUE),
        min_waterlevel_m  = min(waterlevel_m_adj,  na.rm = TRUE),
        min_watertemp_C   = min(watertemp_C_adj,   na.rm = TRUE),
        max_waterlevel_m  = max(waterlevel_m_adj,  na.rm = TRUE),
        max_watertemp_C   = max(watertemp_C_adj,   na.rm = TRUE),
        .groups           = "drop"
      )
    
  } else if (metric_norm == "barometric") {
    
    required <- c("airpress_kPa_adj", "airtemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'barometric', expected columns: ",
        paste(required, collapse = ", "),
        ". Missing: ", paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 BARO data)?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        mean_airpress_kPa = mean(airpress_kPa_adj, na.rm = TRUE),
        mean_airtemp_C    = mean(airtemp_C_adj,    na.rm = TRUE),
        min_airpress_kPa  = min(airpress_kPa_adj,  na.rm = TRUE),
        min_airtemp_C     = min(airtemp_C_adj,     na.rm = TRUE),
        max_airpress_kPa  = max(airpress_kPa_adj,  na.rm = TRUE),
        max_airtemp_C     = max(airtemp_C_adj,     na.rm = TRUE),
        .groups           = "drop"
      )
    
  } else if (metric_norm == "airtemp") {
    
    required <- c("airtemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'airtemp', expected column: airtemp_C_adj. Missing: ",
        paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 AT data)?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        mean_airtemp_C = mean(airtemp_C_adj, na.rm = TRUE),
        min_airtemp_C  = min(airtemp_C_adj,  na.rm = TRUE),
        max_airtemp_C  = max(airtemp_C_adj,  na.rm = TRUE),
        .groups        = "drop"
      )
    
  } else if (metric_norm == "watertemp") {
    
    required <- c("watertemp_C_adj")
    missing  <- setdiff(required, names(dat))
    if (length(missing) > 0L) {
      stop(
        "For metric 'watertemp', expected column: watertemp_C_adj. Missing: ",
        paste(missing, collapse = ", "),
        ". Did you select the correct folder (clean v1.0 WT data)?",
        call. = FALSE
      )
    }
    
    out <- dat %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        mean_watertemp_C = mean(watertemp_C_adj, na.rm = TRUE),
        min_watertemp_C  = min(watertemp_C_adj,  na.rm = TRUE),
        max_watertemp_C  = max(watertemp_C_adj,  na.rm = TRUE),
        .groups          = "drop"
      )
    
  } else {
    stop("Metric '", metric_norm, "' not implemented in summarise_clean_data().", call. = FALSE)
  }
  
  # round numeric, clean Inf/NaN ------------------------------------------------
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


factor_cols <- c(
  "sn", "baro_code", "qaqc_code", "wl_qaqc_code",
  "do_qaqc_code", "wt_qaqc_code", "at_qaqc_code", "baro_qaqc_code"
)

