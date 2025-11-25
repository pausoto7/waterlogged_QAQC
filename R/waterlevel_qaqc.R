
source("R/qaqc_log_helpers.R")

waterlevel_qaqc <- function(input_data,
                            select_station,
                            log_root,
                            user = Sys.info()[["user"]]) {
  
  # 1. Basic checks ------------------------------------------------------------
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
  
  # 2. Subset to station + init adjusted cols ----------------------------------
  output_data <- input_data %>%
    dplyr::filter(site_station_code == select_station) %>%
    dplyr::arrange(timestamp) %>%
    dplyr::mutate(
      waterlevel_m_adj = waterlevel_m,
      watertemp_C_adj  = watertemp_C
    )
  
  if (nrow(output_data) == 0) {
    stop("waterlevel_qaqc(): no rows found for station '", select_station, "'.")
  }
  
  # 3. Apply QA/QC rules -------------------------------------------------------
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
      neg_lt_minus1 = !is.na(watertemp_C) & watertemp_C < -1,
      neg_between   = !is.na(watertemp_C) & !is.na(watertemp_C) &
        watertemp_C < 0 & watertemp_C > -1,
      
      # adjust temperature based on negatives
      watertemp_C_adj = dplyr::case_when(
        neg_lt_minus1 ~ NA_real_,
        neg_between   ~ 0,
        TRUE          ~ watertemp_C_adj
      )
    ) %>%
    # drop only the internal numeric helpers
    dplyr::select(
      -wl_lead, -wl_diff
    )
  
  # 4. Build QA/QC log rows from flags -----------------------------------------

  field       <- "waterlevel_m_adj"
  action      <- "AUTO_QAQC"
  fun_name    <- "waterlevel_qaqc"
  metric <- "WL"
  manual_note <- "Automatic water level QA/QC; no manual note provided."
  run_time <- Sys.time()
  
  # make list of qaqc codes nad notes to be iterated through later
  rules <- list(
    list(
      flag        = output_data$neg_lt_minus1 | output_data$neg_between,
      code        = "NEGATIVE_WT",
      action_note = "Negative water temperature detected; removed or corrected to 0°C."
    ),
    list(
      flag        = output_data$flag_ice %in% TRUE,
      code        = "FLAG_ICE",
      action_note = "Possible ice conditions: water temperature < 0.2°C; flagged for review."
    ),
    list(
      flag        = output_data$flag_disturbance %in% TRUE,
      code        = "FLAG_DISTURBANCE",
      action_note = "Large step change in water level; flagged as potential logger disturbance."
    ),
    list(
      flag        = output_data$flag_dry %in% TRUE,
      code        = "FLAG_DRY",
      action_note = "Possible dry sensor (warm water or water pressure ~ barometric); flagged."
    )
  )
  
  logs_list <- lapply(rules, function(r) {
    make_qaqc_log_row(
      timestamps  = output_data$timestamp[r$flag],
      station     = select_station,
      metric      = metric,
      field       = field,
      action      = action,
      code        = r$code,
      action_note = r$action_note,
      manual_note = manual_note,
      fun_name    = fun_name,
      user        = user,
      run_time    = run_time
    )
  })
  
  log_this <- dplyr::bind_rows(logs_list)
  
  # If nothing flagged, still log that QAQC ran --------------------------------
  if (nrow(log_this) == 0) {
    log_this <- dplyr::tibble(
      station        = select_station,
      metric         = metric,
      field          = field,
      action         = action,
      code           = NA_character_,
      action_note    = "Automatic water level QA/QC ran; no flags applied.",
      manual_note    = manual_note,
      ts_start       = NA_character_,
      ts_end         = NA_character_,
      duration_hours = NA_real_,
      fun_name       = fun_name,
      run_at         = run_time,
      user           = user
    )
  }
  
  # 5. Append to on-disk log (silently) ----------------------------------------\
  log_path <- qaqc_log_path(log_root, station, metric)
  
  
  qaqc_log_append(
    log_path = log_path,
    station  = select_station,
    metric   = metric,
    log_rows = log_this
  )

  
  message("QA/QC log updated: ", as.character(log_path))

    return(output_data)
}
