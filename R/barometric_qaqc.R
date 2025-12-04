barometric_qaqc <- function(input_data,
                            select_station,
                            log_root,
                            user = Sys.info()[["user"]],
                            temp_low_limit  = -40,
                            temp_high_limit = 50,
                            pressure_low_kpa  = 85,
                            pressure_high_kpa = 105,
                            spike_threshold_kpa = 1.5,   # sudden >1.5 kPa jump
                            flatline_n = 6               # 6 consecutive identical values
) {
  
  # ---- 1. Filter & basic checks ---------------------------------------------
  df <- input_data %>%
    dplyr::filter(site_station_code == select_station) %>%
    dplyr::arrange(timestamp)
  
  if (nrow(df) == 0) stop("No rows for station ", select_station)
  
  required <- c("timestamp", "airpress_kPa", "airtemp_C")
  miss <- setdiff(required, names(df))
  if (length(miss) > 0) stop("Missing required columns: ", paste(miss, collapse=", "))
  
  # Create adjusted columns if missing
  if (!"airpress_kPa_adj" %in% names(df)) df$airpress_kPa_adj <- df$airpress_kPa
  if (!"airtemp_C_adj"   %in% names(df)) df$airtemp_C_adj   <- df$airtemp_C
  
  # Create edit flags if missing
  flags <- c("edit_press_high","edit_press_low","edit_flatline",
             "edit_spike","edit_temp_range")
  for (f in flags) if (!f %in% names(df)) df[[f]] <- FALSE
  
  
  # ---- 2. Detection Rules ---------------------------------------------------
  # Temperature outside realistic range
  bad_temp <- df$airtemp_C < temp_low_limit | df$airtemp_C > temp_high_limit
  
  # Pressure outside expected terrestrial range
  bad_press_low  <- df$airpress_kPa < pressure_low_kpa
  bad_press_high <- df$airpress_kPa > pressure_high_kpa
  
  # Spikes (sudden jump > threshold)
  diff_press <- c(NA, abs(diff(df$airpress_kPa)))
  spikes     <- diff_press > spike_threshold_kpa
  
  # Flatline detection (n identical values in a row)
  rle_vals <- rle(df$airpress_kPa)
  flat_idx <- rep(rle_vals$lengths >= flatline_n, rle_vals$lengths)
  flatline <- flat_idx
  

  # ---- 3. Apply Adjustments -------------------------------------------------
  df <- df %>%
    dplyr::mutate(
      # Remove obviously wrong values
      airpress_kPa_adj = dplyr::if_else(bad_press_low | bad_press_high, NA_real_, airpress_kPa_adj),
      airtemp_C_adj    = dplyr::if_else(bad_temp, NA_real_, airtemp_C_adj),
      
      edit_press_high = bad_press_high,
      edit_press_low  = bad_press_low,
      edit_temp_range = bad_temp,
      edit_spike      = spikes,
      edit_flatline   = flatline
    )
  
  
  # ---- 4. Log Entries -------------------------------------------------------
  metric <- "BARO"
  field  <- "airpress_kPa_adj"
  
  events <- list(
    list(name="PRESSURE_HIGH", idx=which(bad_press_high),
         note=paste0("Pressure above ", pressure_high_kpa, " kPa removed")),
    list(name="PRESSURE_LOW", idx=which(bad_press_low),
         note=paste0("Pressure below ", pressure_low_kpa, " kPa removed")),
    list(name="TEMP_OUT_RANGE", idx=which(bad_temp),
         note=paste0("Temperature outside [", temp_low_limit,", ", temp_high_limit, "] °C")),
    list(name="SPIKE", idx=which(spikes),
         note=paste0("Spike > ", spike_threshold_kpa, " kPa detected")),
    list(name="FLATLINE", idx=which(flatline),
         note=paste0("Flatline ≥ ", flatline_n, " identical readings"))
  )
  
  for (ev in events) {
    if (length(ev$idx) > 0) {
      ts_vals <- df$timestamp[ev$idx]
      
      log_row <- make_qaqc_log_row(
        timestamps = ts_vals,
        station    = select_station,
        metric     = metric,
        field      = field,
        action     = ev$name,
        code       = ev$name,
        action_note= ev$note,
        manual_note= "AUTOMATIC QC",
        fun_name   = "barometric_qaqc",
        user       = user
      )
      
      log_path <- qaqc_log_path(log_root, select_station, metric)
      
      qaqc_log_append(
        log_path = log_path,
        station  = select_station,
        metric   = metric,
        log_rows = log_row
      )
    }
  }
  
  message("Barometric QAQC completed for: ", select_station)
  
  return(df)
}
