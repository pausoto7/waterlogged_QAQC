

adjust_waterlevel_spike <- function(input_data,
                                    select_station,
                                    timestamp_start,
                                    timestamp_end,
                                    reason_to_adjust, 
                                    manual_note, 
                                    log_root, 
                                    user = Sys.info()[["user"]]) {
  # QAQC inputs  ---------------------------------------------------
  allowed_reasons <- c("ice", "disturbance")
  
  if (length(reason_to_adjust) != 1L) {
    stop(
      "reason_to_adjust must be a single value, one of: ",
      paste(allowed_reasons, collapse = ", ")
    )
  }
  
  reason_to_adjust <- tolower(reason_to_adjust)
  
  if (!reason_to_adjust %in% allowed_reasons) {
    stop(
      "reason_to_adjust must be one of: ",
      paste(allowed_reasons, collapse = ", "),
      ". You provided: ", reason_to_adjust
    )
  }
  
  if (missing(manual_note) || trimws(manual_note) == "")
    stop("adjust_waterlevel_spike(): 'manual_note' must be a non-empty string.")
  
  # --- Filter to selected station & sort --------------------------------------
  waterlevel_data <- input_data %>%
    dplyr::filter(site_station_code == !!select_station) %>%
    dplyr::arrange(timestamp)
  
  required_cols <- c(
    "timestamp",
    "site_station_code",
    "waterlevel_m",
    "waterlevel_m_adj"
  )
  
  
  missing_cols  <- setdiff(required_cols, names(waterlevel_data))
  if (length(missing_cols) > 0) {
    stop(
      "adjust_waterlevel_spike(): Missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # --- Parse timestamps --------------------------------
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
  
  # --- Get start/end water levels (raw) ---------------------------------------

   start_val <- waterlevel_data %>%
    dplyr::filter(timestamp == ts_start) %>%
    dplyr::pull(waterlevel_m) %>%
    round(3)
  
  end_val <- waterlevel_data %>%
    dplyr::filter(timestamp == ts_end) %>%
    dplyr::pull(waterlevel_m) %>%
    round(3)
  
  if (length(start_val) != 1L || length(end_val) != 1L) {
    stop(
      "Could not find unique waterlevel_m at both timestamp_start and ",
      "timestamp_end for station ", select_station, "."
    )
  }
  
  # Total change and total time between start and end (seconds)
  total_change <- end_val - start_val
  total_time   <- as.numeric(difftime(ts_end, ts_start, units = "secs"))
  
  # --- If edit_spike_flat doesn't exist yet, create it as FALSE ----------------
  if (!"edit_spike_flat" %in% names(waterlevel_data)) {
    waterlevel_data$edit_spike_flat <- FALSE
  }
  
  # --- Apply spike flattening (time-based interpolation) ----------------------
  
  waterlevel_data <- waterlevel_data %>%
    dplyr::mutate(
      in_event = timestamp >= ts_start & timestamp <= ts_end,
      
      interpolated_value = start_val + total_change *
        (as.numeric(difftime(timestamp, ts_start, units = "secs")) / total_time),
      
      waterlevel_m_adj = dplyr::case_when(
        in_event ~ interpolated_value,
        TRUE     ~ waterlevel_m_adj
      ),
      
      # new simple flag for rows touched by this function
      edit_spike_flat = dplyr::if_else(
        in_event,
        TRUE,
        edit_spike_flat
      ))%>%
    dplyr::select(-in_event, -interpolated_value)
  
  
  # log entry ------------------------------------------------------------
  metric <- "WL"
  field    <- "waterlevel_m_adj"
  action   <- "SPIKE_FLATTENED"
  fun_name <- "adjust_waterlevel_spike"
  run_time <- Sys.time()
  
  code <- if (reason_to_adjust == "ice") "LOGGER_ICE" else "LOGGER_DISTURBANCE"
  
  ts_affected <- waterlevel_data$timestamp[
    waterlevel_data$timestamp >= ts_start &
      waterlevel_data$timestamp <= ts_end
  ]
  
  log_this <- make_qaqc_log_row(
    timestamps  = ts_affected,
    station     = select_station,
    metric      = metric,
    field       = field,
    action      = action,
    code        = code,
    action_note = paste0("Spike flattened from ", timestamp_start, " to ", timestamp_end,
                         " (reason: ", reason_to_adjust, ")."),
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
