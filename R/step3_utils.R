#' QA/QC for step 3 input data (waterlevel conversion)
#'
#' Ensures required columns exist, logger_type looks reasonable, timestamp is POSIXct,
#' and does some light range checks on temperature and pressure.
#'
#' @param input_data Data frame passed into convert_waterlevel_kPa_m().
#'        Must contain at least:
#'        site_station_code, timestamp, waterpress_kPa, watertemp_C, baro_data.
#' @param logger_type_expected Character pattern for allowed logger types
#'        (e.g., "u20"), matched case-insensitively in `logger_type` if present.
#' @param fun_name Name of the calling function (for nicer error messages).
#'
#' @return input_data with timestamp coerced to POSIXct if needed.
QAQC_wl_kpa_inputs <- function(input_data,
                             logger_type_expected = "u20",
                             fun_name = "convert_waterlevel_kPa_m") {
  
  ## 1) Required columns ---------------------------------------------
  required_cols <- c(
    "site_station_code",
    "timestamp",
    "waterpress_kPa",
    "watertemp_C",
    "baro_data"
  )
  
  missing_cols <- setdiff(required_cols, names(input_data))
  if (length(missing_cols) > 0) {
    stop(
      fun_name, "(): input_data is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  ## 2) Logger type QC (if present) ---------------------------------
  if ("logger_type" %in% names(input_data)) {
    lt_raw   <- unique(input_data$logger_type)
    lt_clean <- tolower(trimws(as.character(lt_raw)))
    
    if (!any(grepl(logger_type_expected, lt_clean, fixed = TRUE))) {
      warning(
        fun_name, "(): logger type(s) in input_data (",
        paste(lt_raw, collapse = ", "),
        ") do not match expected pattern '", logger_type_expected, "'.\n",
        "Proceeding anyway, but check that this really is the expected pressure logger."
      )
    }
  } else {
    warning(
      fun_name,
      "(): column 'logger_type' not found in input_data; skipping logger-type QC."
    )
  }
  
  ## 3) Timestamp QC: ensure POSIXct --------------------------------
  if (!inherits(input_data$timestamp, "POSIXct")) {
    ts_parsed <- suppressWarnings(
      lubridate::ymd_hms(input_data$timestamp, tz = "UTC")
    )
    if (any(is.na(ts_parsed))) {
      stop(
        fun_name,
        "(): failed to parse input_data$timestamp as POSIXct with ymd_hms(). ",
        "Check timestamp format."
      )
    }
    input_data$timestamp <- ts_parsed
  }
  
  ## 4) Light unit / range sanity checks -----------------------------
  # Water temperature: we expect something like -5 to 40 °C
  if ("watertemp_C" %in% names(input_data)) {
    wt <- input_data$watertemp_C
    bad_temp <- which(!is.na(wt) & (wt < -5 | wt > 40))
    if (length(bad_temp)) {
      warning(
        fun_name, "(): watertemp_C has ", length(bad_temp),
        " value(s) outside [-5, 40] °C. Check units (should be °C)."
      )
    }
  }
  
  # Barometric pressure: typically ~80–120 kPa at Earth’s surface
  if ("baro_data" %in% names(input_data)) {
    bp <- input_data$baro_data
    bad_baro <- which(!is.na(bp) & (bp < 80 | bp > 120))
    if (length(bad_baro)) {
      warning(
        fun_name, "(): baro_data has ", length(bad_baro),
        " value(s) outside [80, 120] kPa. Check units (should be kPa)."
      )
    }
  }
  
  # Water pressure: we just enforce >0 and < 600 kPa as a sanity range
  if ("waterpress_kPa" %in% names(input_data)) {
    wp <- input_data$waterpress_kPa
    bad_wp <- which(!is.na(wp) & (wp <= 0 | wp > 600))
    if (length(bad_wp)) {
      warning(
        fun_name, "(): waterpress_kPa has ", length(bad_wp),
        " value(s) outside (0, 600] kPa. Check units (should be kPa absolute)."
      )
    }
  }
  
  return(input_data)
}
