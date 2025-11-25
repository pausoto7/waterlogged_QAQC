# adjust water level shift (previously known as adjust waterlevel cable break function)



adjust_WL_offset <- function(input_data,
                                         select_station,
                                         timestamp_start,
                                         timestamp_end,
                                         manual_note,
                                         log_root,
                                         user = Sys.info()[["user"]]) {
  
  
  #  QAQC inputs
  if (missing(manual_note) || trimws(manual_note) == "") {
    stop("adjust_waterlevel_cablebreak(): 'manual_note' must be a non-empty string.")
  }
  
  # ---- Filter to selected station & sort -------------------------------------
  waterlevel_data <- input_data %>%
    dplyr::filter(site_station_code == select_station) %>%
    dplyr::arrange(timestamp)
  
  required_cols <- c(
    "timestamp",
    "site_station_code",
    "waterlevel_m",
    "waterlevel_m_adj"
  )
  
  missing_cols <- setdiff(required_cols, names(waterlevel_data))
  if (length(missing_cols) > 0) {
    stop(
      "adjust_waterlevel_cablebreak(): Missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (nrow(waterlevel_data) == 0) {
    stop("adjust_waterlevel_cablebreak(): no rows found for station '", select_station, "'.")
  }
  
  # ---- Parse timestamps (character -> POSIXct) -------------------------------
  ts_start <- lubridate::ymd_hms(timestamp_start)
  ts_end   <- lubridate::ymd_hms(timestamp_end)
  
  if (is.na(ts_start) || is.na(ts_end)) {
    stop(
      "Could not parse timestamp_start or timestamp_end with ymd_hms(). ",
      "Use 'YYYY-MM-DD HH:MM:SS' format if passing character values."
    )
  }
  
  if (ts_end <= ts_start) {
    stop("timestamp_end must be after timestamp_start.")
  }
  
  # ---- Get start/end water levels (raw) --------------------------------------
  before_break <- waterlevel_data %>%
    dplyr::filter(timestamp == ts_start) %>%
    dplyr::pull(waterlevel_m) %>%
    round(3)
  
  after_break <- waterlevel_data %>%
    dplyr::filter(timestamp == ts_end) %>%
    dplyr::pull(waterlevel_m) %>%
    round(3)
  
  if (length(before_break) != 1L || length(after_break) != 1L) {
    stop(
      "Could not find unique waterlevel_m at both timestamp_start and ",
      "timestamp_end for station ", select_station, "."
    )
  }
  
  # Offset needed to realign post-break data to pre-break level
  corr_break <- after_break - before_break
  
  # ---- Ensure flag column exists ---------------------------------------------
  if (!"edit_offset" %in% names(waterlevel_data)) {
    waterlevel_data$edit_offset <- FALSE
  }
  
  # ---- Apply constant offset after the break ---------------------------------
  waterlevel_data <- waterlevel_data %>%
    dplyr::mutate(
      after_break = timestamp >= ts_end,
      
      # Only ever touch the adjusted series; keep raw intact
      waterlevel_m_adj = dplyr::case_when(
        after_break ~ waterlevel_m_adj - corr_break,
        TRUE        ~ waterlevel_m_adj
      ),
      
      edit_offset = dplyr::if_else(
        after_break,
        TRUE,
        edit_offset
      )
    ) %>%
    dplyr::select(-after_break)
  
  # ---- Build QA/QC log entry -------------------------------------------------
  metric   <- "WL"
  field    <- "waterlevel_m_adj"
  action   <- "CABLE_OFFSET"
  fun_name <- "adjust_waterlevel_cablebreak"
  run_time <- Sys.time()
  code     <- "LOGGER_DISTURBANCE"
  
  ts_affected <- waterlevel_data$timestamp[
    waterlevel_data$timestamp >= ts_end
  ]
  
  log_this <- make_qaqc_log_row(
    timestamps  = ts_affected,
    station     = select_station,
    metric      = metric,
    field       = field,
    action      = action,
    code        = code,
    action_note = paste0(
      "Applied constant offset of ", round(corr_break, 3),
      " m to waterlevel_m_adj for timestamps >= ", timestamp_end,
      ". Pre-break reference at ", timestamp_start, "."
    ),
    manual_note = manual_note,
    fun_name    = fun_name,
    user        = user,
    run_time    = run_time
  )
  
  log_path <- qaqc_log_path(log_root, select_station, metric)
  
  if (!is.null(log_this)) {
    qaqc_log_append(
      log_path = log_path,
      station  = select_station,
      metric   = metric,
      log_rows = log_this
    )
    message("QA/QC log updated: ", log_path)
  } else {
    warning("No rows adjusted; no log entry written.")
  }
  
  return(waterlevel_data)
}
