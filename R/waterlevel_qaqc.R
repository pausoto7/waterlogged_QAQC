#' Automatic QA/QC for water level and water temperature data
#'
#' Applies automated quality assurance and quality control rules to water level
#' and water temperature time series data. Detects and flags potential issues
#' including sensor disturbances, dry conditions, ice presence, and invalid
#' temperature readings. All QA/QC actions are logged for traceability.
#'
#' @param input_data Data frame containing water level logger data with columns:
#'   `site_station_code`, `timestamp`, `waterlevel_m`, `watertemp_C`,
#'   `waterpress_kPa`, `airpress_kPa`, and `airtemp_C`. Typically the output
#'   from [convert_waterlevel_kPa_m()].
#' @param select_station Character; the station code to process (must match
#'   a value in `site_station_code` column).
#' @param log_root Character; root directory where QA/QC logs will be written.
#'   Log files are created at `<log_root>/logs/<station>_<metric>_QAQC_log.csv`.
#' @param user Character; username to record in the QA/QC log. Defaults to
#'   `Sys.info()[["user"]]`.
#'
#' @details
#' The function applies the following QA/QC rules:
#'
#' **Water Temperature Rules:**
#' \itemize{
#'   \item Temperatures < -1\u00B0C are set to NA (invalid readings)
#'   \item Temperatures between -1\u00B0C and 0\u00B0C are corrected to 0\u00B0C
#'   \item Temperatures < 0.3\u00B0C are flagged as potential ice conditions
#' }
#'
#' **Water Level Rules:**
#' \itemize{
#'   \item Step changes > 0.1 m between consecutive readings flagged as disturbance
#'   \item Dry sensor detection based on:
#'     \itemize{
#'       \item Water temperature > 19\u00B0C
#'       \item Water pressure <= air pressure
#'       \item Water-air temperature difference < 2\u00B0C
#'       \item Air temperature warmer than water temperature
#'     }
#' }
#'
#' Creates adjusted columns `waterlevel_m_adj` and `watertemp_C_adj` along with
#' flag columns: `flag_disturbance`, `flag_ice`, `flag_wl_dry`, `neg_lt_minus1`,
#' and `neg_between`.
#'
#' @return A data frame containing the input data for `select_station` with
#'   additional adjusted columns (`*_adj`) and flag columns. Rows are sorted
#'   by timestamp. QA/QC log entries are written to disk as a side effect.
#'
#' @seealso [convert_waterlevel_kPa_m()], [qaqc_log_append()], [plot_qaqc_timeseries()]
#'
#' @importFrom dplyr filter arrange mutate lead case_when select bind_rows tibble
#' @importFrom lubridate ymd_hms
#' @export
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
  
  # Ensure timestamp is POSIXct (just in case)
  if (!inherits(output_data$timestamp, "POSIXct")) {
    output_data$timestamp <- lubridate::ymd_hms(output_data$timestamp, tz = "UTC")
  }
  
  # 3. Apply QA/QC rules (WL + WT) --------------------------------------------
  output_data <- output_data %>%
    dplyr::mutate(
      # --- helper columns for waterlevel disturbance ---
      wl_lead = dplyr::lead(waterlevel_m),
      wl_diff = waterlevel_m - wl_lead,
      
      flag_disturbance = !is.na(waterlevel_m) & !is.na(wl_diff) &
        abs(wl_diff) > 0.1,
      
      # --- water temperature helpers (from old watertemp_qaqc) ---
      wt_high_dry   = !is.na(watertemp_C) & watertemp_C > 19,
      flag_ice      = !is.na(watertemp_C) & watertemp_C < 0.3,
      neg_lt_minus1 = !is.na(watertemp_C) & watertemp_C < -1,
      neg_between   = !is.na(watertemp_C) & watertemp_C < 0 & watertemp_C > -1,
      
      wt_air_close  = !is.na(watertemp_C) & !is.na(airtemp_C) &
        abs(watertemp_C - airtemp_C) < 2,
      wt_air_warmer = !is.na(watertemp_C) & !is.na(airtemp_C) &
        (airtemp_C - watertemp_C) < 0,
      
      # old WL-based dry rule, updated to 19\u00B0C to match watertemp_qaqc doc
      flag_dry_wl = !is.na(waterlevel_m) & !is.na(watertemp_C) &
        !is.na(waterpress_kPa) & !is.na(airpress_kPa) &
        (watertemp_C > 19 | waterpress_kPa <= airpress_kPa),
      
      # combine temperature-based dry signals
      flag_dry_temp = wt_high_dry | wt_air_close | wt_air_warmer,
      
      # final dry flag (WL + WT signals) – make it WL-specific
      flag_wl_dry = flag_dry_wl | flag_dry_temp,
      
      # adjust temperature based on negatives
      watertemp_C_adj = dplyr::case_when(
        neg_lt_minus1 ~ NA_real_,  # remove clearly impossible temps
        neg_between   ~ 0,         # small negatives corrected to 0 \u00B0C
        TRUE          ~ watertemp_C_adj
      )
    ) %>%
    # drop only internal helpers we don't want in the output
    dplyr::select(
      -wl_lead, -wl_diff,
      -wt_high_dry, -wt_air_close, -wt_air_warmer,
      -flag_dry_wl, -flag_dry_temp
    )
  
  # 4. Build QA/QC log rows from flags ----------------------------------------
  metric      <- "WL"
  action      <- "AUTO_QAQC"
  fun_name    <- "waterlevel_qaqc"
  manual_note <- "Automatic water level & water temperature QA/QC; no manual note provided."
  run_time    <- Sys.time()
  
  rules <- list(
    # temperature-related rules (watertemp_C_adj field)
    list(
      flag        = output_data$neg_lt_minus1 | output_data$neg_between,
      field       = "watertemp_C_adj",
      code        = "NEGATIVE_WT",
      action_note = "Negative water temperature detected; removed (< -1\u00B0C) or corrected to 0\u00B0C (-1 to 0\u00B0C)."
    ),
    list(
      flag        = output_data$flag_ice %in% TRUE,
      field       = "watertemp_C_adj",
      code        = "FLAG_ICE",
      action_note = "Possible ice conditions: water temperature < 0.3\u00B0C; flagged for review."
    ),
    # WL-related rules (waterlevel_m_adj field)
    list(
      flag        = output_data$flag_disturbance %in% TRUE,
      field       = "waterlevel_m_adj",
      code        = "FLAG_DISTURBANCE",
      action_note = "Large step change in water level (> 0.1 m); flagged as potential logger disturbance."
    ),
    list(
      flag        = output_data$flag_wl_dry %in% TRUE,
      field       = "waterlevel_m_adj",
      code        = "FLAG_WL_DRY",
      action_note = "Possible dry sensor (warm water, low pressure, or air–water temperature behaviour); flagged."
    )
  )
  
  logs_list <- lapply(rules, function(r) {
    make_qaqc_log_row(
      timestamps  = output_data$timestamp[r$flag],
      station     = select_station,
      metric      = metric,
      field       = r$field,
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
  
  # 5. If nothing flagged, still log that QAQC ran -----------------------------
  if (nrow(log_this) == 0) {
    log_this <- dplyr::tibble(
      station        = select_station,
      metric         = metric,
      field          = "waterlevel_m_adj",
      action         = action,
      code           = NA_character_,
      action_note    = "Automatic water level & water temperature QA/QC ran; no flags applied.",
      manual_note    = manual_note,
      ts_start       = NA_character_,
      ts_end         = NA_character_,
      duration_hours = NA_real_,
      fun_name       = fun_name,
      run_at         = run_time,
      user           = user
    )
  }
  
  # 6. Append to on-disk log ---------------------------------------------------
  log_path <- qaqc_log_path(log_root, select_station, metric)
  
  qaqc_log_append(
    log_path = log_path,
    station  = select_station,
    metric   = metric,
    log_rows = log_this
  )
  
  message("QA/QC log updated: ", as.character(log_path))
  
  return(output_data)
}
