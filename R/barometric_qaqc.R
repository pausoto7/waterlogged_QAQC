#' QA/QC a single barometric pressure time series
#'
#' Applies automatic QA/QC rules to a barometric logger time series for one
#' station: removes out-of-range pressures and temperatures, flags spikes
#' and flatlines, and writes QA/QC log entries. Returns the input data with
#' adjusted columns (`airpress_kPa_adj`, `airtemp_C_adj`) and boolean edit
#' flags.
#'
#' @param input_data Data frame of barometric logger data containing at least
#'   `timestamp`, `site_station_code`, `airpress_kPa` and `airtemp_C`, possibly
#'   for multiple stations.
#' @param select_station Character, station code to process
#'   (matches `site_station_code`).
#' @param log_root Root folder where QA/QC logs are written by
#'   [qaqc_log_append()], typically the same root used elsewhere in the
#'   workflow (e.g. `"data/testing/processed"`).
#' @param user Character username written into the QA/QC log. Defaults to
#'   `Sys.info()[["user"]]`.
#' @param temp_low_limit Lower bound (°C) for plausible barometric temperatures.
#'   Values outside `[temp_low_limit, temp_high_limit]` are set to `NA` in
#'   `airtemp_C_adj` and flagged.
#' @param temp_high_limit Upper bound (°C) for plausible barometric temperatures.
#' @param pressure_low_kpa Lower bound (kPa) for plausible barometric pressure.
#'   Values below this are set to `NA` in `airpress_kPa_adj` and flagged.
#' @param pressure_high_kpa Upper bound (kPa) for plausible barometric pressure.
#'   Values above this are set to `NA` in `airpress_kPa_adj` and flagged.
#' @param spike_threshold_kpa Threshold (kPa) for flagging sudden pressure
#'   spikes between consecutive timesteps (absolute difference in kPa).
#' @param flatline_n Integer; number of consecutive identical values required
#'   to flag a potential flatline in the pressure series.
#'
#' @details
#' If `airpress_kPa_adj` or `airtemp_C_adj` do not exist, they are created as
#' copies of the raw `airpress_kPa` / `airtemp_C` columns and then modified
#' by the QA/QC rules. Edit flags (`edit_press_high`, `edit_press_low`,
#' `edit_temp_range`, `edit_spike`, `edit_flatline`) are created if missing
#' and set to `TRUE` where each rule applies.
#'
#' For each rule that triggers, a corresponding QA/QC log entry is appended via
#' [make_qaqc_log_row()] and [qaqc_log_append()], using metric `"BARO"` and
#' field `"airpress_kPa_adj"`.
#'
#' @return A data frame containing only rows for `select_station`, sorted by
#'   `timestamp`, with updated `*_adj` columns and edit flags. The input is not
#'   modified in-place.
#'
#' @seealso [barometric_qaqc_all()], [add_nearest_baro()]
#'
#' @import dplyr
#' @export
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
