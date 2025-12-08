#' Conductivity logger drift correction
#'
#' Correct conductivity data for sensor drift based on discrete reference
#' measurements (e.g., YSI). Works on `conduct_uScm_adj` and
#' `watertemp_C_adj` (outputs from conductivity_qaqc_all) and adds
#' drift–corrected columns `conduct_uScm_dc` and `watertemp_C_dc`.
#'
#' @param input_data Logger data frame for one or more stations. Must contain:
#'   `site_station_code`, `timestamp`, `conduct_uScm_adj`, `watertemp_C_adj`.
#' @param ref_data Reference data frame. Must contain:
#'   `site_station_code`, `ysi_timestamp`, `ysi_conduct_uScm`, `ysi_watertemp_C`.
#' @param select_station Character. `site_station_code` to correct.
#'
#' @return A list with:
#'   \itemize{
#'     \item `output_data` – logger data for `select_station` with
#'       `conduct_uScm_dc`, `watertemp_C_dc` and updated `cond_qaqc_*` cols.
#'     \item `ref_data_use` – reference rows actually used for this station.
#'     \item `cal_devs` – calibration points with >10% deviation (may be empty).
#'   }
#'
#' @import dplyr
#' @import tidyr
#' @importFrom lubridate dminutes
#' @importFrom fuzzyjoin difference_left_join difference_anti_join
#' @export
conductivity_drift <- function(input_data,
                          ref_data,
                          select_station) {
  
  # ---- Basic input checks ----------------------------------------------------
  required_logger_cols <- c(
    "site_station_code",
    "timestamp",
    "conduct_uScm_adj",
    "watertemp_C_adj"
  )
  missing_logger <- setdiff(required_logger_cols, names(input_data))
  if (length(missing_logger) > 0) {
    stop(
      "correct_drift(): input_data is missing required column(s): ",
      paste(missing_logger, collapse = ", ")
    )
  }
  
  required_ref_cols <- c(
    "site_station_code",
    "ysi_timestamp",
    "ysi_conduct_uScm",
    "ysi_watertemp_C"
  )
  missing_ref <- setdiff(required_ref_cols, names(ref_data))
  if (length(missing_ref) > 0) {
    stop(
      "correct_drift(): ref_data is missing required column(s): ",
      paste(missing_ref, collapse = ", ")
    )
  }
  
  # ---- Filter to station & ensure POSIXct ------------------------------------
  logger_data <- input_data %>%
    dplyr::filter(site_station_code == !!select_station)
  
  if (!nrow(logger_data)) {
    stop(
      "correct_drift(): no rows found in input_data for station '",
      select_station, "'."
    )
  }
  
  if (!inherits(logger_data$timestamp, "POSIXct")) {
    logger_data$timestamp <- as.POSIXct(logger_data$timestamp, tz = "UTC")
    if (any(is.na(logger_data$timestamp))) {
      warning(
        "Some logger timestamps could not be converted to POSIXct. ",
        "Check the format of input_data$timestamp."
      )
    }
  }
  
  time_range <- range(logger_data$timestamp, na.rm = TRUE)
  
  # ---- Prepare reference data for this station ------------------------------
  ref_data_site <- ref_data %>%
    dplyr::filter(site_station_code == !!select_station) %>%
    dplyr::rename(
      ref_timestamp    = ysi_timestamp,
      ref_conduct_uScm = ysi_conduct_uScm,
      ref_watertemp_C  = ysi_watertemp_C
    )
  
  if (!nrow(ref_data_site)) {
    stop(
      "correct_drift(): no reference measurements found for station '",
      select_station, "'."
    )
  }
  
  if (!inherits(ref_data_site$ref_timestamp, "POSIXct")) {
    ref_data_site$ref_timestamp <- as.POSIXct(ref_data_site$ref_timestamp, tz = "UTC")
    if (any(is.na(ref_data_site$ref_timestamp))) {
      warning(
        "Some reference timestamps could not be converted to POSIXct. ",
        "Check the format of ref_data$ysi_timestamp."
      )
    }
  }
  
  # keep only refs within +/- 2h of logger record
  ref_data_site <- ref_data_site %>%
    dplyr::filter(
      ref_timestamp >= (time_range[1] - 60 * 120),
      ref_timestamp <= (time_range[2] + 60 * 120)
    ) %>%
    dplyr::select(ref_timestamp,
                  ref_conduct_uScm,
                  ref_watertemp_C)
  
  if (!nrow(ref_data_site)) {
    stop(
      "correct_drift(): no reference measurements within +/- 2 hours of logger ",
      "record for station '", select_station, "'."
    )
  }
  
  # ---- Join reference values onto logger timestamps -------------------------
  # 1) First pass: within 29 minutes
  joined_refs <- fuzzyjoin::difference_left_join(
    logger_data,
    ref_data_site,
    by       = c("timestamp" = "ref_timestamp"),
    max_dist = lubridate::dminutes(29)
  ) %>%
    dplyr::mutate(difftime = timestamp - ref_timestamp)
  
  # 2) Unmatched reference points (pre/post deployment)
  refs_unmatched <- fuzzyjoin::difference_anti_join(
    ref_data_site,
    logger_data,
    by       = c("ref_timestamp" = "timestamp"),
    max_dist = lubridate::dminutes(29)
  )
  
  # 3) Second pass: within 1 hour
  joined_remainder <- fuzzyjoin::difference_left_join(
    logger_data,
    refs_unmatched,
    by       = c("timestamp" = "ref_timestamp"),
    max_dist = lubridate::dminutes(60)
  ) %>%
    dplyr::mutate(difftime = timestamp - ref_timestamp) %>%
    dplyr::filter(!is.na(ref_timestamp))
  
  # 4) If still nothing, widen to 2 hours
  if (!nrow(joined_remainder)) {
    joined_remainder <- fuzzyjoin::difference_left_join(
      logger_data,
      refs_unmatched,
      by       = c("timestamp" = "ref_timestamp"),
      max_dist = lubridate::dminutes(120)
    ) %>%
      dplyr::mutate(difftime = timestamp - ref_timestamp) %>%
      dplyr::filter(!is.na(ref_timestamp))
  }
  
  # 5) Combine: remainder first so it wins when deduping
  joined_data_all <- dplyr::bind_rows(joined_remainder, joined_refs) %>%
    dplyr::distinct(timestamp, .keep_all = TRUE) %>%
    dplyr::arrange(timestamp) %>%
    dplyr::select(-difftime)
  
  # ---- Ensure an initial reference exists -----------------------------------
  if (is.na(joined_data_all$ref_conduct_uScm[1])) {
    joined_data_all$ref_conduct_uScm[1] <- joined_data_all$conduct_uScm_adj[1]
    joined_data_all$ref_watertemp_C[1]  <- joined_data_all$watertemp_C_adj[1]
    joined_data_all$ref_timestamp[1]    <- joined_data_all$timestamp[1]
    
    warning(
      "correct_drift(): No initial reference measurement at start of logger ",
      "record. Assuming perfect calibration at first logger timestamp.",
      call. = FALSE
    )
  }
  
  # ---- Compute drift between reference points --------------------------------
  diff_ref_points <- joined_data_all %>%
    dplyr::filter(!is.na(ref_conduct_uScm)) %>%
    dplyr::mutate(
      ref_time_intv = dplyr::coalesce(
        as.numeric(timestamp - dplyr::lag(timestamp),
                   units = "hours"),
        0
      ),
      per_c_offset = ref_conduct_uScm / conduct_uScm_adj,
      per_t_offset = ref_watertemp_C  / watertemp_C_adj,
      c_offset_diff = dplyr::coalesce(
        per_c_offset - dplyr::lag(per_c_offset),
        0
      ),
      t_offset_diff = dplyr::coalesce(
        per_t_offset - dplyr::lag(per_t_offset),
        0
      )
    )
  
  joined_data_drift <- joined_data_all %>%
    dplyr::left_join(
      diff_ref_points %>%
        dplyr::select(
          ref_timestamp,
          ref_time_intv,
          per_c_offset,
          c_offset_diff,
          per_t_offset,
          t_offset_diff
        ),
      by = "ref_timestamp"
    )
  
  joined_data_drift <- joined_data_drift %>%
    tidyr::fill(ref_time_intv, .direction = "up") %>%
    tidyr::fill(ref_timestamp, .direction = "down") %>%
    dplyr::mutate(
      time_elapsed = as.numeric(timestamp - ref_timestamp,
                                units = "hours"),
      time_ratio   = dplyr::coalesce(time_elapsed / ref_time_intv, 0),
      time_ratio   = dplyr::if_else(is.infinite(time_ratio), 0, time_ratio)
    )
  
  # ---- Apply drift correction -------------------------------------------------
  output_data <- joined_data_drift %>%
    tidyr::fill(
      per_c_offset, per_t_offset,
      c_offset_diff, t_offset_diff,
      .direction = "down"
    ) %>%
    dplyr::mutate(
      c_drift = per_c_offset + c_offset_diff * time_ratio,
      t_drift = per_t_offset + t_offset_diff * time_ratio
    ) %>%
    tidyr::fill(c_drift, t_drift, .direction = "down") %>%
    dplyr::mutate(
      conduct_uScm_dc = conduct_uScm_adj * c_drift,
      watertemp_C_dc  = watertemp_C_adj * t_drift
    )
  
  # ---- Flag large calibration deviations -------------------------------------
  cal_devs <- output_data %>%
    dplyr::mutate(
      cal_deviation = round(
        abs(ref_conduct_uScm - conduct_uScm_adj) /
          conduct_uScm_adj,
        2
      ),
      row_index = dplyr::row_number()
    ) %>%
    dplyr::filter(!is.na(cal_deviation),
                  cal_deviation > 0.1) %>%
    dplyr::select(
      row_index,
      timestamp,
      ref_conduct_uScm,
      conduct_uScm_adj,
      cal_deviation
    )
  
  # ---- Update cond_qaqc_* columns --------------------------------------------
  append_qaqc <- function(existing, new) {
    ifelse(is.na(existing) | existing == "", new,
           paste(existing, new, sep = "; "))
  }
  
  if (!"cond_qaqc_code" %in% names(output_data)) {
    output_data$cond_qaqc_code <- NA_character_
  }
  if (!"cond_qaqc_note" %in% names(output_data)) {
    output_data$cond_qaqc_note <- NA_character_
  }
  if (!"cond_qaqc_adj" %in% names(output_data)) {
    output_data$cond_qaqc_adj <- NA_character_
  }
  
  output_data <- output_data %>%
    dplyr::mutate(
      cond_qaqc_code = dplyr::if_else(
        !is.na(conduct_uScm_dc) &
          conduct_uScm_dc != conduct_uScm_adj,
        append_qaqc(cond_qaqc_code, "DRIFT_CORR"),
        cond_qaqc_code
      ),
      cond_qaqc_note = dplyr::if_else(
        !is.na(conduct_uScm_dc) &
          conduct_uScm_dc != conduct_uScm_adj,
        append_qaqc(cond_qaqc_note, "Applied drift correction"),
        cond_qaqc_note
      ),
      cond_qaqc_adj = dplyr::if_else(
        !is.na(conduct_uScm_dc) &
          conduct_uScm_dc != conduct_uScm_adj,
        append_qaqc(
          cond_qaqc_adj,
          paste0("Multiplied conductivity by ", round(c_drift, 3))
        ),
        cond_qaqc_adj
      )
    ) %>%
    dplyr::arrange(timestamp)
  
  if (nrow(cal_devs) > 0) {
    warning(
      "correct_drift(): One or more reference measurements deviate >10% ",
      "from the logger reading. Inspect `cal_devs` for details.",
      call. = FALSE
    )
  }
  
  return(list(
    output_data  = output_data,
    ref_data_use = ref_data_site,
    cal_devs     = cal_devs
  ))
}
