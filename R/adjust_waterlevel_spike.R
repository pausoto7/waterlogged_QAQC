adjust_waterlevel_spike <- function(input_data,
                                    select_station,
                                    timestamp_start,
                                    timestamp_end,
                                    reason_to_adjust) {
  # --- QC: reason_to_adjust ---------------------------------------------------
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
  
  # --- Filter to selected station & sort --------------------------------------
  waterlevel_data <- input_data %>%
    dplyr::filter(site_station_code == !!select_station) %>%
    dplyr::arrange(timestamp)
  
  # --- Required columns must exist when entering this function ----------------
  required_cols <- c(
    "timestamp",
    "site_station_code",
    "waterlevel_m",
    "waterlevel_m_adj",
    "wl_qaqc_adj",
    "wl_qaqc_note",
    "wl_qaqc_code"
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
  
  # --- Apply spike flattening (time-based interpolation) ----------------------
  waterlevel_data <- waterlevel_data %>%
    dplyr::mutate(
      in_event = timestamp >= ts_start & timestamp <= ts_end,
      
      # Inline time-based interpolation (no intermediate columns)
      interpolated_value = start_val + total_change *
        (as.numeric(difftime(timestamp, ts_start, units = "secs")) / total_time),
      
      # QA/QC metadata
      wl_qaqc_adj = dplyr::if_else(
        in_event, "AVERAGE", wl_qaqc_adj
      ),
      wl_qaqc_note = dplyr::if_else(
        in_event,
        "spike flattened to average between before and after event",
        wl_qaqc_note
      ),
      wl_qaqc_code = dplyr::if_else(
        in_event,
        dplyr::case_when(
          reason_to_adjust == "ice"         ~ "LOGGER_ICE",
          reason_to_adjust == "disturbance" ~ "LOGGER_DISTURBANCE"
        ),
        wl_qaqc_code
      ),
      
      # ALWAYS modify adjusted column; raw baseline remains intact
      waterlevel_m_adj = dplyr::case_when(
        in_event ~ interpolated_value,
        TRUE     ~ waterlevel_m_adj
      )
    ) %>%
    dplyr::select(-in_event, -interpolated_value)
  
  return(waterlevel_data)
}
