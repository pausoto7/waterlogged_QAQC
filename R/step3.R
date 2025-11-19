#' Waterlevel conversion (kPa -> m) using barometric compensation
#'
#' @param input_data Dataframe containing timeseries of raw water level and baro:
#'   required cols: site_station_code, timestamp, waterpress_kPa, watertemp_C, baro_data
#' @param select_station Character; site_station_code to process
#' @param reference_data Dataframe of manual measurements with at least:
#'   site_station_code, and either
#'   - stage_timestamp & stage_m   (if reference_type = "stage"), or
#'   - depth_timestamp & depth_m   (if reference_type = "depth").
#' @param reference_type Either "stage" or "depth". Determines which columns are used.
#' @param select_measurement Integer; which manual measurement (row) to use as reference (default = 1)
#' @param logger_type_expected Pattern to check in input_data$logger_type (default "u20")
#' @param path_to_output_folder Folder where yearly /processed CSVs will be written
#'
#' @return list(
#'   site_wl   = dataframe of converted waterlevel for the site,
#'   ref_dat   = dataframe of reference measurements used,
#'   wl_plot   = interactive (plotly) QAQC plot
#' )
#' @export
#' 
#' 
#' 
#' 



input_wl_data <- add_nearest_baro(input_data = level_bound[[1]],
                             path_to_output_folder = "data/testing/processed", 
                             baro_data_path = "data/testing/processed",
                             baro_site_selection = "auto",
                             metadata_path)



converted_data <- convert_waterlevel_kPa_m(input_data = input_wl_data[1],
                                     select_station = "WL_ALBR_ST_30",
                                     reference_data,
                                     reference_type = "stage",
                                     select_measurement = 1,
                                     logger_type_expected = "u20",
                                     path_to_output_folder) 



 
convert_waterlevel_kPa_m <- function(input_data,
                                     select_station,
                                     reference_data,
                                     reference_type = "stage",
                                     select_measurement = 1,
                                     logger_type_expected = "u20",
                                     path_to_output_folder) {
  
  # Make sure path_to_output_folder has trailing slash
  if (!endsWith(path_to_output_folder, "/")) {
    path_to_output_folder <- paste0(path_to_output_folder, "/")
  }
  
  ## 1) QAQC input_data ----------------------------------------------
  input_data <- qaqc_wl_input(input_data, logger_type_expected = logger_type_expected)
  
  # Filter for this station
  output_data <- input_data[input_data$site_station_code == select_station, , drop = FALSE]
  if (!nrow(output_data)) {
    stop("No rows found in input_data for site_station_code = '", select_station, "'.")
  }
  
  ts_range <- range(output_data$timestamp, na.rm = TRUE)
  
  ## 2) Handle reference_type -> column names ------------------------
  ref_type_clean <- tolower(trimws(reference_type))
  if (!ref_type_clean %in% c("stage", "depth")) {
    stop("reference_type must be either 'stage' or 'depth'.")
  }
  
  if (ref_type_clean == "stage") {
    ref_level_col <- "stage_m"
    ref_ts_col    <- "stage_timestamp"
  } else {  # "depth"
    ref_level_col <- "depth_m"
    ref_ts_col    <- "depth_timestamp"
  }
  
  # Check that reference_data has the expected columns
  needed_ref_cols <- c("site_station_code", ref_ts_col, ref_level_col)
  missing_ref_cols <- setdiff(needed_ref_cols, names(reference_data))
  
  if (length(missing_ref_cols) > 0) {
    stop(
      "Reference data is missing required column(s) for reference_type = '", ref_type_clean, "': ",
      paste(missing_ref_cols, collapse = ", "),
      "\nExpected (for '", ref_type_clean, "') columns: ",
      paste(needed_ref_cols, collapse = ", ")
    )
  }
  
  ref_df <- reference_data %>%
    dplyr::filter(site_station_code == select_station) %>%
    dplyr::rename(
      timestamp = !!ref_ts_col,
      ref_m     = !!ref_level_col
    )
  
  # Ensure timestamp is POSIXct
  if (!inherits(ref_df$timestamp, "POSIXct")) {
    ts_parsed <- suppressWarnings(
      lubridate::ymd_hms(ref_df$timestamp, tz = "UTC")
    )
    if (all(is.na(ts_parsed))) {
      # fallback to more flexible parser
      ts_parsed <- suppressWarnings(
        lubridate::parse_date_time(
          ref_df$timestamp,
          orders = c("ymd HM", "ymd HMS", "mdy HM", "mdy HMS"),
          tz = "UTC"
        )
      )
    }
    if (any(is.na(ts_parsed))) {
      stop(
        "Failed to parse reference_data timestamp column (",
        ref_ts_col, ") as POSIXct. Check format."
      )
    }
    ref_df$timestamp <- ts_parsed
  }
  
  # Round to nearest hour
  ref_df$timestamp <- lubridate::round_date(ref_df$timestamp, "hour")
  
  # Filter to reference obs within instrument time range
  ref_dat <- ref_df %>%
    dplyr::filter(timestamp >= ts_range[1], timestamp <= ts_range[2]) %>%
    dplyr::select(timestamp, ref_m) %>%
    dplyr::arrange(timestamp)
  
  if (!nrow(ref_dat)) {
    stop(
      "No reference measurements for ", select_station,
      " fall within the input_data timestamp range."
    )
  }
  
  # Make sure select_measurement exists
  if (select_measurement < 1 || select_measurement > nrow(ref_dat)) {
    stop(
      "select_measurement = ", select_measurement,
      " but there are only ", nrow(ref_dat), " reference rows in-range."
    )
  }
  
  # Join reference values onto time series (for plotting)
  output_data <- dplyr::left_join(output_data, ref_dat, by = "timestamp")
  
  ## 3) Metric conversions & constants --------------------------------
  FEET_TO_METERS <- 0.3048
  KPA_TO_PSI     <- 0.1450377
  PSI_TO_PSF     <- 144.0
  
  ## 4) Fluid density of water -----------------------------------------
  output_data <- output_data %>%
    dplyr::mutate(
      density_kgm3 =
        (999.83952 +
           16.945176 * watertemp_C -
           7.9870401e-03 * watertemp_C^2 -
           46.170461e-06 * watertemp_C^3 +
           105.56302e-09 * watertemp_C^4 -
           280.54253e-12 * watertemp_C^5) /
        (1 + 16.879850e-03 * watertemp_C),
      density_lbft3 = 0.0624279606 * density_kgm3
    )
  
  ## 5) Convert pressures to depth (m) ---------------------------------
  output_data <- output_data %>%
    dplyr::mutate(
      sensor_depth_m = FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * waterpress_kPa) / density_lbft3,
      baro_sensor_depth_m = FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * baro_data) / density_lbft3
    )
  
  ## 6) Reference point & compensation constant k ----------------------
  T_meas <- ref_dat$timestamp[select_measurement]
  L_meas <- ref_dat$ref_m[select_measurement]
  
  # Grab depths at reference time
  D_ref   <- output_data$sensor_depth_m[output_data$timestamp == T_meas]
  D_baro0 <- output_data$baro_sensor_depth_m[output_data$timestamp == T_meas]
  
  if (!length(D_ref) || !length(D_baro0)) {
    stop(
      "Could not find matching timestamp in output_data for selected reference measurement #",
      select_measurement,
      ". Check that reference timestamps align (after rounding) with logger timestamps."
    )
  }
  
  k <- L_meas - (D_ref - D_baro0)
  
  ## 7) Apply compensation to entire series ----------------------------
  output_data <- output_data %>%
    dplyr::mutate(
      waterlevel_m = sensor_depth_m - baro_sensor_depth_m + k,
      waterlevel_reference = ref_type_clean
    )
  
  ## 8) QAQC plot ------------------------------------------------------
  p <- ggplot2::ggplot(output_data, ggplot2::aes(x = timestamp, y = waterlevel_m)) +
    ggplot2::geom_line(color = "#233d4d") +
    ggplot2::geom_point(
      ggplot2::aes(y = ref_m),
      color = "#fe7f2d",
      size = 3
    ) +
    ggplot2::labs(
      title = paste("kPa to m conversion for:", select_station),
      x = "Timestamp",
      y = "Water level (m)"
    ) +
    ggplot2::theme_classic()
  
  ## 9) Write yearly processed files -----------------------------------
  years_i <- unique(lubridate::year(output_data$timestamp))
  
  for (year_n in years_i) {
    site_year_n <- dplyr::filter(output_data, lubridate::year(timestamp) == year_n)
    if (!nrow(site_year_n)) next
    
    start_n <- gsub("\\D", "", as.character(min(lubridate::date(site_year_n$timestamp))))
    end_n   <- gsub("\\D", "", as.character(max(lubridate::date(site_year_n$timestamp))))
    
    year_dir <- file.path(path_to_output_folder, year_n)
    proc_dir <- file.path(year_dir, "processed")
    
    dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Write timestamp as character to avoid Excel shenanigans
    site_year_n$timestamp <- format(site_year_n$timestamp, "%Y-%m-%d %H:%M:%S")
    
    out_file <- sprintf("%s_WL_%s_%s_v0.3.csv", select_station, start_n, end_n)
    out_path <- file.path(proc_dir, out_file)
    
    utils::write.csv(site_year_n, out_path, row.names = FALSE)
    message(sprintf("Barometric compensation (v0.3) for %s written to %s", select_station, out_path))
  }
  
  message("Results returned as list: [[1]] waterlevel data, [[2]] reference data, [[3]] QAQC plot")
  message("Tip: do the orange points (manual refs) lie on top of the navy line? If yes, you're good. If not, try a different select_measurement.")
  
  return(list(
    site_wl = output_data,
    ref_dat = ref_dat,
    wl_plot = plotly::ggplotly(p)
  ))
}
