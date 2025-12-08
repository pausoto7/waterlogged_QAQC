#' Automatic QA/QC for conductivity data
#'
#' Applies automated quality assurance and quality control rules to electrical
#' conductivity time series data. Detects and flags conductivity spikes, dry
#' sensor periods (very low conductivity), temperature anomalies, and ice
#' conditions. All QA/QC actions are logged for traceability.
#'
#' @param input_data Data frame containing conductivity logger data with
#'   columns: `site_station_code`, `timestamp`, `conduct_uScm`, and
#'   `watertemp_C`. Optional: `airtemp_C` for enhanced dry detection.
#' @param select_station Character; the station code to process (must match
#'   a value in `site_station_code` column).
#' @param log_root Character; root directory where QA/QC logs will be written.
#'   Log files are created at `<log_root>/logs/<station>_<metric>_QAQC_log.csv`.
#' @param user Character; username to record in the QA/QC log. Defaults to
#'   `Sys.info()[["user"]]`.
#' @param temp_low_limit_C Numeric; lower temperature limit (\u00B0C). Values below
#'   this threshold are flagged as out of range. Defaults to -2\u00B0C.
#' @param temp_high_limit_C Numeric; upper temperature limit (\u00B0C). Values above
#'   this threshold are flagged as out of range. Defaults to 40\u00B0C.
#' @param disturbance_threshold_uScm Numeric; threshold (µS/cm) for flagging
#'   sudden conductivity changes between consecutive readings. Defaults to 200 µS/cm.
#' @param dry_threshold_uScm Numeric; conductivity threshold (µS/cm) below which
#'   the sensor is likely dry. Defaults to 10 µS/cm.
#' @param air_water_diff_threshold_C Numeric; temperature difference threshold (\u00B0C)
#'   between air and water to support dry sensor inference. Defaults to 2\u00B0C.
#'
#' @details
#' The function applies the following QA/QC rules:
#'
#' **Conductivity Rules:**
#' \itemize{
#'   \item Spikes: consecutive readings differing by > 200 µS/cm are flagged
#'   \item Dry detection: conductivity < 10 µS/cm flagged as potential dry sensor
#'   \item Enhanced dry detection: if `airtemp_C` is present, dry is also flagged
#'     when conductivity is low AND air-water temperature difference <= 2\u00B0C
#' }
#'
#' **Water Temperature Rules:**
#' \itemize{
#'   \item Temperatures < -2\u00B0C or > 40\u00B0C are flagged as out of range
#'   \item Temperatures < 0.1\u00B0C are flagged as potential ice conditions
#'   \item Temperatures between -1\u00B0C and 0\u00B0C are corrected to 0\u00B0C
#' }
#'
#' Creates adjusted columns `conduct_uScm_adj` and `watertemp_C_adj` along with
#' flag columns including `flag_co_spike`, `flag_co_dry`, and `flag_co_ice`.
#'
#' @return A data frame containing the input data for `select_station` with
#'   additional adjusted columns (`*_adj`) and flag columns. Rows are sorted
#'   by timestamp. QA/QC log entries are written to disk as a side effect.
#'
#' @seealso [conductivity_qaqc_all()], [conductivity_temp_compensation()], [plot_qaqc_timeseries()]
#'
#' @importFrom dplyr filter arrange mutate case_when bind_rows tibble
#' @importFrom lubridate ymd_hms
#' @export
conductivity_qaqc <- function(
    input_data,
    select_station,
    log_root,
    user                       = Sys.info()[["user"]],
    temp_low_limit_C           = -2,
    temp_high_limit_C          = 40,
    disturbance_threshold_uScm = 200,
    dry_threshold_uScm         = 10,
    air_water_diff_threshold_C = 2
) {
  # ---------------------------------------------------------------------------
  # Hard-coded small-temp rules:
  #   * Water temperature < 0.1 \u00B0C is considered possible ICE.
  #   * Slightly negative water temperatures between -1 and 0 \u00B0C are set to 0 \u00B0C.
  # ---------------------------------------------------------------------------
  ice_threshold_C        <- 0.1
  near_zero_temp_window  <- c(-1, 0)
  
  # ---- 1. Filter & basic checks ---------------------------------------------
  df <- input_data %>%
    dplyr::filter(site_station_code == select_station) %>%
    dplyr::arrange(timestamp)
  
  if (nrow(df) == 0) {
    stop("conductivity_qaqc(): no rows for station ", select_station)
  }
  
  required <- c("timestamp", "conduct_uScm", "watertemp_C")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "conductivity_qaqc(): missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Ensure timestamp is POSIXct
  if (!inherits(df$timestamp, "POSIXct")) {
    df$timestamp <- lubridate::ymd_hms(df$timestamp, tz = "UTC")
  }
  
  # ---- 2. Create adjusted columns if missing --------------------------------
  if (!"conduct_uScm_adj" %in% names(df)) {
    df$conduct_uScm_adj <- df$conduct_uScm
  }
  if (!"watertemp_C_adj" %in% names(df)) {
    df$watertemp_C_adj <- df$watertemp_C
  }
  
  # ---- 3. Detection helpers (use adj as working values) ---------------------
  # Temperature conditions
  temp_low   <- df$watertemp_C_adj < temp_low_limit_C
  temp_high  <- df$watertemp_C_adj > temp_high_limit_C
  temp_ice   <- df$watertemp_C_adj < ice_threshold_C
  
  near_zero  <- df$watertemp_C_adj >= near_zero_temp_window[1] &
    df$watertemp_C_adj <  near_zero_temp_window[2]
  
  # Conductivity spike (use adj)
  diff_cond  <- c(NA_real_, diff(df$conduct_uScm_adj))
  spike      <- abs(diff_cond) > disturbance_threshold_uScm
  
  # Very low conductivity
  very_low_cond <- df$conduct_uScm_adj < dry_threshold_uScm
  
  # Air–water support for dry inference (optional)
  if ("airtemp_C" %in% names(df)) {
    air_water_close <- abs(df$airtemp_C - df$watertemp_C_adj) <= air_water_diff_threshold_C
  } else {
    air_water_close <- rep(FALSE, nrow(df))
  }
  
  dry_air_based <- very_low_cond & air_water_close
  
  # Combined dry flag (COND-specific)
  flag_co_dry <- very_low_cond | dry_air_based
  
  # ---- 4. Apply *temperature* adjustments; cond remains as-is ---------------
  df <- df %>%
    dplyr::mutate(
      watertemp_C_adj = dplyr::case_when(
        near_zero ~ 0,            # small negatives -> 0
        temp_low  ~ NA_real_,     # out-of-range low
        temp_high ~ NA_real_,     # out-of-range high
        TRUE      ~ watertemp_C_adj
      ),
      
      # Keep key flags for plotting / diagnostics
      flag_co_spike         = spike,
      flag_co_temp_ice      = temp_ice,
      flag_co_temp_low      = temp_low,
      flag_co_temp_high     = temp_high,
      flag_co_temp_neg_near = near_zero,
      flag_co_dry           = flag_co_dry
    ) %>%
    # Drop purely helper columns we don't need to expose
    dplyr::select(
      -dplyr::all_of(c("air_water_close")) %>% 
        intersect(names(.)),  # safe if column didn't exist
      -dplyr::all_of(c("diff_cond"))      %>% 
        intersect(names(.))
    )
  
  # ---- 5. Build QA/QC log rows (COND-style rules list) ----------------------
  metric      <- "COND"
  fun_name    <- "conductivity_qaqc"
  action      <- "AUTO_QAQC"
  manual_note <- "Automatic conductivity QA/QC; no manual note provided."
  run_time    <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  
  rules <- list(
    # Spikes (no automatic removal, just flag & log)
    list(
      flag        = df$flag_co_spike %in% TRUE,
      field       = "conduct_uScm_adj",
      code        = "FLAG_CO_SPIKE",
      action_note = paste0("Spike > ", disturbance_threshold_uScm, " µS/cm between samples; flagged for review.")
    ),
    # Ice-like temperatures
    list(
      flag        = df$flag_co_temp_ice %in% TRUE,
      field       = "watertemp_C_adj",
      code        = "FLAG_ICE",
      action_note = "Water temperature < 0.1 \u00B0C (likely ice); flagged for review."
    ),
    # Slight negatives corrected to 0
    list(
      flag        = df$flag_co_temp_neg_near %in% TRUE,
      field       = "watertemp_C_adj",
      code        = "NEGATIVE_WT",
      action_note = "Water temperature between -1 and 0 \u00B0C corrected to 0 \u00B0C."
    ),
    # Out-of-range temps (low or high)
    list(
      flag        = (df$flag_co_temp_low | df$flag_co_temp_high) %in% TRUE,
      field       = "watertemp_C_adj",
      code        = "TEMP_RANGE",
      action_note = paste0(
        "Water temperature outside acceptable range (",
        temp_low_limit_C, " to ", temp_high_limit_C, " \u00B0C); set to NA."
      )
    ),
    # Dry conditions – COND-specific
    list(
      flag        = df$flag_co_dry %in% TRUE,
      field       = "conduct_uScm_adj",
      code        = "FLAG_CO_DRY",
      action_note = paste0(
        "Possible dry sensor: conductivity < ", dry_threshold_uScm,
        " µS/cm (and, where available, air–water temperatures behave like air exposure)."
      )
    )
  )
  
  logs_list <- lapply(rules, function(r) {
    ts_sel <- df$timestamp[r$flag]
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
      field          = "conduct_uScm_adj",
      action         = action,
      code           = NA_character_,
      action_note    = "Automatic conductivity QA/QC ran; no flags applied.",
      manual_note    = manual_note,
      ts_start       = NA_character_,
      ts_end         = NA_character_,
      duration_hours = NA_real_,
      fun_name       = fun_name,
      run_at         = run_time,
      user           = user
    )
  }
  
  # ---- 6. Append to on-disk log ---------------------------------------------
  log_path <- qaqc_log_path(log_root, select_station, metric)
  
  qaqc_log_append(
    log_path = log_path,
    station  = select_station,
    metric   = metric,
    log_rows = log_this
  )
  
  message("Conductivity QAQC completed for: ", select_station)
  df
}
