#' Conductivity QA/QC for a Single Station
#'
#' Performs automatic QA/QC on conductivity logger data for a single station.
#' The function:
#' \itemize{
#'   \item Ensures timestamps are in POSIXct (UTC).
#'   \item Creates adjusted columns \code{conduct_uScm_adj} and
#'         \code{watertemp_C_adj} if they do not already exist.
#'   \item Flags and annotates rows with a single QA/QC code in
#'         \code{cond_qaqc_code} and a human-readable explanation in
#'         \code{cond_qaqc_note}.
#'   \item Writes QA/QC log entries to disk using \code{make_qaqc_log_row()},
#'         \code{qaqc_log_path()}, and \code{qaqc_log_append()}.
#' }
#'
#' The following automatic rules are applied:
#' \itemize{
#'   \item Water temperature < \code{temp_low_limit_C} or
#'         > \code{temp_high_limit_C} is considered out of range.
#'   \item Slightly negative water temperatures in [-1, 0) \u00B0C are treated as
#'         sensor noise and corrected to 0 \u00B0C, flagged as \code{"NEGATIVE_WT"}.
#'   \item Water temperature in [0, 0.1) \u00B0C is flagged as \code{"ICE"}.
#'   \item Sudden changes in conductivity > \code{disturbance_threshold_uScm}
#'         between samples are flagged as \code{"SPIKE"}.
#'   \item Very low conductivity (< \code{dry_threshold_uScm} µS/cm) is used
#'         to infer dry conditions, optionally strengthened by air–water
#'         temperature similarity (\code{"DRY_AIRTEMP"} vs
#'         \code{"DRY_COND_ONLY"}).
#' }
#'
#' @param input_data Data frame containing conductivity time series for one or
#'   more stations.
#' @param select_station Character station code to filter on
#'   (\code{site_station_code}).
#' @param log_root Root directory where QA/QC logs are stored.
#' @param user Character user name stored in QA/QC logs.
#' @param temp_low_limit_C Minimum acceptable water temperature (\u00B0C) before
#'   being flagged as out of range.
#' @param temp_high_limit_C Maximum acceptable water temperature (\u00B0C) before
#'   being flagged as out of range.
#' @param disturbance_threshold_uScm Threshold (µS/cm) for detecting
#'   conductivity spikes between consecutive samples.
#' @param dry_threshold_uScm Threshold (µS/cm) below which conductivity is
#'   considered “very low” and may indicate dry conditions.
#' @param air_water_diff_threshold_C Maximum allowed absolute difference (\u00B0C)
#'   between air and water temperature to support a dry inference when
#'   conductivity is very low.
#'
#' @details
#' Required columns in \code{input_data} are:
#' \itemize{
#'   \item \code{timestamp} (POSIXct or character parsable by
#'         \code{lubridate::ymd_hms()}).
#'   \item \code{conduct_uScm} (raw conductivity).
#'   \item \code{watertemp_C} (raw water temperature).
#'   \item \code{airtemp_C} is optional, but improves “dry” detection.
#' }
#'
#' If not already present, the function creates:
#' \itemize{
#'   \item \code{conduct_uScm_adj}, \code{watertemp_C_adj}
#'   \item \code{cond_qaqc_code}, \code{cond_qaqc_note}
#'   \item logical flags \code{edit_cond_spike}, \code{edit_cond_dry},
#'         \code{edit_temp_range}, \code{edit_ice}
#' }
#'
#' The original raw data columns are never overwritten; only the \code{*_adj}
#' columns are modified.
#'
#' @return The input data filtered to \code{select_station}, with updated
#'   adjusted columns, QA/QC codes, notes, and edit flags. QA/QC events are
#'   also appended to the station log on disk.
#' @export
conductivity_qaqc <- function(
    input_data,
    select_station,
    log_root,
    user                       = Sys.info()[["user"]],
    temp_low_limit_C           = -2,    # below this = out of range (likely logger / ice)
    temp_high_limit_C          = 40,    # above this = out of range
    disturbance_threshold_uScm = 200,   # sudden >200 uS/cm change between samples
    dry_threshold_uScm         = 10,    # very low cond = probable dry
    air_water_diff_threshold_C = 2      # |air - water| <= 2\u00B0C to support dry inference
) {
  
  # ---------------------------------------------------------------------------
  # NOTE: This function automatically:
  #   1) Flags water temperature < 0.1 \u00B0C as ICE (for non-negative temps).
  #   2) Corrects slightly negative water temperatures between -1 and 0 \u00B0C to 0.
  # These thresholds are intentionally hard-coded because small negative temps
  # are almost always sensor noise, not real liquid-water temperatures.
  ice_threshold_C        <- 0.1
  near_zero_temp_window  <- c(-1, 0)
  # ---------------------------------------------------------------------------
  
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
  
  # ---- 2. Create adjusted & QAQC columns if missing -------------------------
  if (!"conduct_uScm_adj" %in% names(df)) {
    df$conduct_uScm_adj <- df$conduct_uScm
  }
  if (!"watertemp_C_adj" %in% names(df)) {
    df$watertemp_C_adj <- df$watertemp_C
  }
  
  if (!"cond_qaqc_code" %in% names(df)) {
    df$cond_qaqc_code <- NA_character_
  }
  if (!"cond_qaqc_note" %in% names(df)) {
    df$cond_qaqc_note <- NA_character_
  }
  
  flag_cols <- c(
    "edit_cond_spike",
    "edit_cond_dry",
    "edit_temp_range",
    "edit_ice"
  )
  for (fc in flag_cols) {
    if (!fc %in% names(df)) df[[fc]] <- FALSE
  }
  
  # ---- 3. Detection rules ---------------------------------------------------
  temp_low   <- df$watertemp_C < temp_low_limit_C
  temp_high  <- df$watertemp_C > temp_high_limit_C
  
  # ICE: only non-negative temps < threshold
  temp_ice   <- df$watertemp_C >= 0 & df$watertemp_C < ice_threshold_C
  
  near_zero  <- df$watertemp_C >= near_zero_temp_window[1] &
    df$watertemp_C <  near_zero_temp_window[2]
  
  diff_cond  <- c(NA_real_, diff(df$conduct_uScm))
  spike      <- abs(diff_cond) > disturbance_threshold_uScm
  
  very_low_cond <- df$conduct_uScm < dry_threshold_uScm
  
  if ("airtemp_C" %in% names(df)) {
    air_water_close <- abs(df$airtemp_C - df$watertemp_C) <= air_water_diff_threshold_C
  } else {
    air_water_close <- rep(FALSE, nrow(df))
  }
  
  dry_air_based <- very_low_cond & air_water_close
  
  # ---- 4. Apply adjustments -------------------------------------------------
  df <- df %>%
    dplyr::mutate(
      watertemp_C_adj = dplyr::case_when(
        near_zero ~ 0,                            # small negatives -> 0
        temp_low  ~ NA_real_,                     # out of range low
        temp_high ~ NA_real_,                     # out of range high
        TRUE      ~ watertemp_C_adj
      ),
      edit_temp_range = temp_low | temp_high,
      edit_ice        = temp_ice,
      edit_cond_spike = spike,
      edit_cond_dry   = very_low_cond | dry_air_based
    )
  
  # one QAQC code per row (priority order)
  df$cond_qaqc_code <- dplyr::case_when(
    dry_air_based                  ~ "DRY_AIRTEMP",
    very_low_cond & !dry_air_based ~ "DRY_COND_ONLY",
    spike                          ~ "SPIKE",
    near_zero                      ~ "NEGATIVE_WT",
    temp_ice                       ~ "ICE",
    temp_low                       ~ "TEMP_LOW",
    temp_high                      ~ "TEMP_HIGH",
    TRUE                           ~ df$cond_qaqc_code
  )
  
  df$cond_qaqc_note <- dplyr::case_when(
    df$cond_qaqc_code == "DRY_AIRTEMP"   ~ paste0(
      "Likely dry: cond < ", dry_threshold_uScm,
      " uS/cm and |air - water| <= ", air_water_diff_threshold_C, " \u00B0C"
    ),
    df$cond_qaqc_code == "DRY_COND_ONLY" ~ paste0(
      "Very low conductivity (< ", dry_threshold_uScm, " uS/cm)"
    ),
    df$cond_qaqc_code == "SPIKE"         ~ paste0(
      "Spike > ", disturbance_threshold_uScm, " uS/cm"
    ),
    df$cond_qaqc_code == "NEGATIVE_WT"   ~ "Negative water temperature corrected to 0 \u00B0C",
    df$cond_qaqc_code == "ICE"           ~ "Water temperature < 0.1 \u00B0C (likely ice)",
    df$cond_qaqc_code == "TEMP_LOW"      ~ paste0(
      "Water temperature < ", temp_low_limit_C, " \u00B0C"
    ),
    df$cond_qaqc_code == "TEMP_HIGH"     ~ paste0(
      "Water temperature > ", temp_high_limit_C, " \u00B0C"
    ),
    TRUE                                 ~ df$cond_qaqc_note
  )
  
  # ---- 5. Logging -----------------------------------------------------------
  metric <- "COND"
  field  <- "conduct_uScm_adj"
  
  events <- list(
    list(name = "SPIKE",
         idx  = which(spike),
         note = paste0("Spike > ", disturbance_threshold_uScm, " uS/cm")),
    list(name = "DRY_AIRTEMP",
         idx  = which(dry_air_based),
         note = "Likely dry based on conductivity + air/water temperature"),
    list(name = "DRY_COND_ONLY",
         idx  = which(very_low_cond & !dry_air_based),
         note = "Conductivity below dry threshold"),
    list(name = "ICE",
         idx  = which(temp_ice),
         note = "Water temperature < 0.1 \u00B0C (likely ice)"),
    list(name = "NEGATIVE_WT",
         idx  = which(near_zero),
         note = "Negative water temperature corrected to 0 \u00B0C"),
    list(name = "TEMP_RANGE",
         idx  = which(temp_low | temp_high),
         note = "Water temperature outside acceptable range")
  )
  
  for (ev in events) {
    if (length(ev$idx) > 0) {
      ts_vals <- df$timestamp[ev$idx]
      
      log_row <- make_qaqc_log_row(
        timestamps  = ts_vals,
        station     = select_station,
        metric      = metric,
        field       = field,
        action      = ev$name,
        code        = ev$name,
        action_note = ev$note,
        manual_note = "AUTOMATIC QC",
        fun_name    = "conductivity_qaqc",
        user        = user
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
  
  message("Conductivity QAQC completed for: ", select_station)
  df
}
