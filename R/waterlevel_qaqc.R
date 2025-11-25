waterlevel_qaqc <- function(input_data,
                            select_station) {
  
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
  
  # 2. Subset to station + init QA/QC cols -------------------------------------
  output_data <- input_data %>%
    dplyr::filter(site_station_code == select_station) %>%
    dplyr::mutate(
      waterlevel_m_adj = waterlevel_m,  # baseline; later functions can adjust this
      watertemp_C_adj  = watertemp_C,
      wl_qaqc_adj      = NA_character_,
      wl_qaqc_code     = NA_character_,
      wl_qaqc_note     = NA_character_
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
      neg_between   = !is.na(watertemp_C) & watertemp_C < 0 & watertemp_C > -1,
      
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
        neg_lt_minus1    ~ "NEGATIVE_WT",
        neg_between      ~ "NEGATIVE_WT",
        flag_ice         ~ "FLAG_ICE",
        flag_disturbance ~ "FLAG_DISTURBANCE",
        flag_dry         ~ "FLAG_DRY",
        TRUE             ~ wl_qaqc_code
      )
    ) %>%
    dplyr::select(
      -wl_lead, -wl_diff,
      -flag_disturbance, -flag_dry, -flag_ice,
      -neg_lt_minus1, -neg_between
    )
  
  return(output_data)
}