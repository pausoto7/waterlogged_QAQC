#' Automatic QA/QC for dissolved oxygen data
#'
#' Applies automated quality assurance and quality control rules to dissolved
#' oxygen (DO) time series data. Detects and removes or corrects logger errors,
#' invalid DO concentrations, negative values, ice conditions, and dry sensor
#' periods. All QA/QC actions are logged for traceability.
#'
#' @param input_data Data frame containing dissolved oxygen logger data with
#'   columns: `site_station_code`, `timestamp`, `do_mgl`, `do_percsat`,
#'   `watertemp_C`, and `airtemp_C`. Typically the output from
#'   [convert_do_mgl_percsat()].
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
#' **Dissolved Oxygen Rules:**
#' \itemize{
#'   \item Logger error codes (-888.88) are removed (set to NA)
#'   \item DO values < -1 mg/L are removed (set to NA)
#'   \item DO values between -1 and 0 mg/L are corrected to 0
#'   \item DO values > 21 mg/L are removed as outliers
#'   \item 12-point dry sensor detection: flags periods where DO percent
#'     saturation >= 100% and air-water temperature difference <= 2\u00B0C for
#'     12 or more consecutive readings
#' }
#'
#' **Water Temperature Rules (DO logger):**
#' \itemize{
#'   \item Temperatures <= -1\u00B0C are set to NA (invalid readings)
#'   \item Temperatures between -1\u00B0C and 0\u00B0C are corrected to 0\u00B0C
#'   \item Temperatures < 0.3\u00B0C are flagged as potential ice conditions
#' }
#'
#' Creates adjusted columns `do_mgl_adj`, `do_percsat_adj`, and
#' `watertemp_C_do_adj` along with flag columns including `flag_do_dry`,
#' `temp_flag_ice`, and various error flags.
#'
#' @return A data frame containing the input data for `select_station` with
#'   additional adjusted columns (`*_adj`) and flag columns. Rows are sorted
#'   by timestamp. QA/QC log entries are written to disk as a side effect.
#'
#' @seealso [convert_do_mgl_percsat()], [qaqc_log_append()], [plot_qaqc_timeseries()]
#'
#' @importFrom dplyr filter arrange mutate case_when bind_rows tibble
#' @importFrom lubridate ymd_hms
#' @export
dissox_qaqc <- function(input_data,
                        select_station,
                        log_root,
                        user = Sys.info()[["user"]]) {
  
  # ---- 0. Required columns ----
  required_cols <- c(
    "site_station_code",
    "timestamp",
    "do_mgl",
    "do_percsat",
    "watertemp_C",
    "airtemp_C"
  )
  
  missing_cols <- setdiff(required_cols, names(input_data))
  if (length(missing_cols) > 0) {
    stop(
      "dissox_qaqc(): input_data is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ---- 1. Filter to station & sort ----
  output_data <- input_data %>%
    dplyr::filter(.data$site_station_code == !!select_station) %>%
    dplyr::arrange(.data$timestamp)
  
  if (nrow(output_data) == 0) {
    warning("dissox_qaqc(): no rows for station ", select_station)
    return(output_data)
  }
  
  # Ensure timestamp is POSIXct
  if (!inherits(output_data$timestamp, "POSIXct")) {
    output_data$timestamp <- lubridate::ymd_hms(output_data$timestamp, tz = "UTC")
  }
  
  # ---- 2. Initialise internal temp fields + *_adj columns --------------------
  # Internal temp names (consistent with plotting / other funcs)
  if (!"watertemp_C_do" %in% names(output_data)) {
    output_data$watertemp_C_do <- output_data$watertemp_C
  }
  if (!"airtemp_C_baro" %in% names(output_data)) {
    output_data$airtemp_C_baro <- output_data$airtemp_C
  }
  
  # Only initialise *_adj if they don't already exist
  if (!"do_mgl_adj" %in% names(output_data)) {
    output_data$do_mgl_adj <- output_data$do_mgl
  }
  if (!"do_percsat_adj" %in% names(output_data)) {
    output_data$do_percsat_adj <- output_data$do_percsat
  }
  if (!"watertemp_C_do_adj" %in% names(output_data)) {
    output_data$watertemp_C_do_adj <- output_data$watertemp_C_do
  }
  
  # ---- 3. Derived helpers & flags (vectorised) -------------------------------
  # All conditions are based on the *current* adj values so we don't
  # overwrite manual corrections.
  output_data <- output_data %>%
    dplyr::mutate(
      # temp difference for dryness test (using current temp adj)
      air_watertemp_diff = airtemp_C_baro - watertemp_C_do_adj,
      
      # DO flags (using adj as working column)
      do_error         = (do_mgl == -888.88) | (watertemp_C_do == -888.88),
      do_neg_between   = !is.na(do_mgl_adj) & do_mgl_adj < 0  & do_mgl_adj >= -1,
      do_neg_lt_minus1 = !is.na(do_mgl_adj) & do_mgl_adj < -1,
      do_high          = !is.na(do_mgl_adj) & do_mgl_adj > 21,
      
      # temperature flags (using adj as working col)
      temp_flag_ice       = !is.na(watertemp_C_do_adj) & watertemp_C_do_adj < 0.3,
      temp_neg_between    = !is.na(watertemp_C_do_adj) & watertemp_C_do_adj < 0 & watertemp_C_do_adj > -1,
      temp_neg_leq_minus1 = !is.na(watertemp_C_do_adj) & watertemp_C_do_adj <= -1
    )
  
  # ---- 4. Apply DO / temp adjustments (vectorised) ---------------------------
  output_data <- output_data %>%
    dplyr::mutate(
      # DO adjustments, applied to adj column only
      do_mgl_adj = dplyr::case_when(
        do_error | do_neg_lt_minus1 | do_high ~ NA_real_,
        do_neg_between                         ~ 0,
        TRUE                                   ~ do_mgl_adj
      ),
      do_percsat_adj = dplyr::case_when(
        do_error | do_neg_lt_minus1 | do_high ~ NA_real_,
        do_neg_between                         ~ 0,
        TRUE                                   ~ do_percsat_adj
      ),
      # DO temp adjustments
      watertemp_C_do_adj = dplyr::case_when(
        temp_neg_leq_minus1 ~ NA_real_,
        temp_neg_between    ~ 0,
        TRUE                ~ watertemp_C_do_adj
      )
    )
  
  # ---- 5. 12-point dry flag (FLAG_DO_DRY) using adj DO -----------------------
  n <- nrow(output_data)
  output_data$flag_do_dry <- FALSE
  
  if (n >= 12) {
    for (j in 1:(n - 11)) {
      window_diff <- output_data$air_watertemp_diff[j:(j + 11)]
      window_do   <- output_data$do_percsat_adj[j:(j + 11)]
      
      if (any(!is.na(window_diff)) && any(!is.na(window_do))) {
        if (max(abs(window_diff), na.rm = TRUE) <= 2 &&
            min(window_do, na.rm = TRUE) >= 100) {
          output_data$flag_do_dry[j:(j + 11)] <- TRUE
        }
      }
    }
  }
  
  # ---- 6. Build QAQC log rows (WL-style rules list) -------------------------
  metric      <- "DO"
  fun_name    <- "dissox_qaqc"
  action      <- "AUTO_QAQC"
  manual_note <- "Automatic dissolved oxygen QA/QC; no manual note provided."
  run_time    <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  
  rules <- list(
    # DO logger error
    list(
      flag        = output_data$do_error %in% TRUE,
      field       = "do_mgl_adj",
      code        = "DO_ERROR",
      action_note = "Logger error code (-888.88) in DO or DO temperature; DO and %sat removed."
    ),
    # negative DO
    list(
      flag        = output_data$do_neg_between %in% TRUE,
      field       = "do_mgl_adj",
      code        = "NEGATIVE_DO",
      action_note = "DO between 0 and -1 mg/L corrected to 0 (and %sat to 0)."
    ),
    list(
      flag        = output_data$do_neg_lt_minus1 %in% TRUE,
      field       = "do_mgl_adj",
      code        = "NEGATIVE_DO",
      action_note = "DO less than -1 mg/L removed (DO and %sat set to NA)."
    ),
    # very high DO
    list(
      flag        = output_data$do_high %in% TRUE,
      field       = "do_mgl_adj",
      code        = "OUTLIER_DO",
      action_note = "DO greater than 21 mg/L removed (DO and %sat set to NA)."
    ),
    # temperature-driven rules
    list(
      flag        = output_data$temp_neg_between %in% TRUE,
      field       = "watertemp_C_do_adj",
      code        = "NEGATIVE_TEMP",
      action_note = "DO water temp between 0 and -1 \u00B0C corrected to 0 \u00B0C."
    ),
    list(
      flag        = output_data$temp_neg_leq_minus1 %in% TRUE,
      field       = "watertemp_C_do_adj",
      code        = "LOGGER_ICE",
      action_note = "DO water temp ≤ -1 \u00B0C removed (set to NA)."
    ),
    list(
      flag        = output_data$temp_flag_ice %in% TRUE,
      field       = "watertemp_C_do_adj",
      code        = "FLAG_ICE",
      action_note = "Possible ice conditions: DO water temp < 0.3 \u00B0C; flagged for review."
    ),
    # dry periods (12-point rule) – DO-specific
    list(
      flag        = output_data$flag_do_dry %in% TRUE,
      field       = "do_mgl_adj",
      code        = "FLAG_DO_DRY",
      action_note = "Possible dry sensor: DO %sat ≥ 100 and air–water temps within 2 \u00B0C for ≥ 12 points."
    )
  )
  
  logs_list <- lapply(rules, function(r) {
    ts_sel <- output_data$timestamp[r$flag]
    if (length(ts_sel) == 0) return(NULL)
    
    make_qaqc_log_row(
      timestamps  = ts_sel,
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
  
  log_this <- logs_list[!vapply(logs_list, is.null, logical(1))]
  log_this <- if (length(log_this) > 0) {
    dplyr::bind_rows(log_this)
  } else {
    dplyr::tibble(
      station        = select_station,
      metric         = metric,
      field          = "do_mgl_adj",
      action         = action,
      code           = NA_character_,
      action_note    = "Automatic dissolved oxygen QA/QC ran; no flags applied.",
      manual_note    = manual_note,
      ts_start       = NA_character_,
      ts_end         = NA_character_,
      duration_hours = NA_real_,
      fun_name       = fun_name,
      run_at         = run_time,
      user           = user
    )
  }
  
  # ---- 7. Append to on-disk log ---------------------------------------------
  log_path <- qaqc_log_path(log_root, select_station, metric)
  
  qaqc_log_append(
    log_path = log_path,
    station  = select_station,
    metric   = metric,
    log_rows = log_this
  )
  
  message("QA/QC log updated: ", log_path)
  
  return(output_data)
}
