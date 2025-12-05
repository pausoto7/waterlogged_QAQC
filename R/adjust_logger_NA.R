
#' Set logger values to NA over a time window and log the change
#'
#' Convenience wrapper to set adjusted logger values to \code{NA} over a
#' specified time window (or, optionally, for all data below a dry threshold)
#' for a given station and metric. Typical use cases are removing periods
#' affected by ice, dry conditions, or logger disturbances after visual QA/QC.
#'
#' The function updates the appropriate \code{*_adj} columns
#' (e.g. \code{waterlevel_m_adj}, \code{do_mgl_adj}, \code{do_percsat_adj},
#' \code{watertemp_C_adj}, \code{airpress_kPa_adj},
#' \code{conductivity_uScm_adj}) and writes an entry to the QA/QC log via
#' \code{make_qaqc_log_row()} and \code{qaqc_log_append()}.
#'
#' @param input_data Data frame containing logger data for one or more stations,
#'   including a \code{timestamp} column, \code{site_station_code}, and the
#'   relevant adjusted fields (e.g. \code{waterlevel_m_adj},
#'   \code{do_mgl_adj}, etc.).
#' @param select_station Station code to adjust (value of
#'   \code{site_station_code}).
#' @param metric Character string specifying which metric to adjust. Accepted
#'   values include \code{"waterlevel"}, \code{"dissolvedoxygen"},
#'   \code{"watertemp"}, \code{"barometric"}, \code{"conductivity"} and
#'   common variants (e.g. \code{"water level"}, \code{"DO"}, \code{"BARO"}).
#' @param reason_to_adjust Character; reason for the adjustment. One of
#'   \code{"ice"}, \code{"dry"}, or \code{"disturbance"}. This controls which
#'   \code{edit_*} flag column is set and how the NA adjustment is applied.
#' @param timestamp_start,timestamp_end Character date-time values defining
#'   the adjustment window, in \code{"YYYY-MM-DD HH:MM:SS"} format, parsed
#'   with \code{lubridate::ymd_hms()}.
#' @param keep_temp Logical; if \code{TRUE}, water temperature adjustments are
#'   skipped for disturbance events (where applicable). Ignored for metrics
#'   where no temperature is associated.
#' @param apply_to_all_data Logical; if \code{FALSE} (default), only values
#'   within \code{timestamp_start}–\code{timestamp_end} are set to NA. If
#'   \code{TRUE} and a water level column is present, a dry threshold is
#'   derived from the window and applied to the whole record (for supported
#'   metrics and reasons).
#' @param manual_note Character; required free-text explanation describing why
#'   this adjustment is being made. Stored in the QA/QC log.
#' @param log_root Root directory where QA/QC logs are stored (e.g.
#'   \code{"data/testing/processed"}). The actual log path is derived from
#'   this root, station, and metric.
#' @param user Character; user name recorded in the log entry. Defaults to
#'   \code{Sys.info()[["user"]]}.
#'
#' @return A data frame for the selected station with updated adjusted fields
#'   and edit flags. Two internal helper columns (\code{in_window} and
#'   \code{edit_na}) are removed before returning.
#'
#' @details
#' The function is metric-aware: it only modifies the adjusted fields relevant
#' to the requested \code{metric} and sets a matching \code{edit_*} flag
#' (e.g. \code{edit_remove_ice}, \code{edit_remove_dry},
#' \code{edit_remove_disturbance}). If no rows fall within the time window
#' (or dry threshold) for the given station, a warning is issued and no log
#' entry is written.
#'
#' @seealso \code{\link{waterlevel_qaqc}}, \code{\link{dissox_qaqc}},
#'   \code{\link{conductivity_qaqc_all}}, \code{\link{make_qaqc_log_row}},
#'   \code{\link{qaqc_log_append}}
#'
#' @import dplyr
#' @importFrom lubridate ymd_hms
#'
#' @export
adjust_logger_NA <- function(
    input_data,
    select_station,
    metric,              # "waterlevel", "dissolvedoxygen", "watertemp", "barometric", "conductivity"
    reason_to_adjust,    # "ice", "dry", "disturbance"
    timestamp_start,     # "YYYY-MM-DD HH:MM:SS"
    timestamp_end,       # "YYYY-MM-DD HH:MM:SS"
    keep_temp         = FALSE,
    apply_to_all_data = FALSE,
    manual_note,         # required free-text explanation
    log_root,            # root folder where /logs lives
    user = Sys.info()[["user"]]
) {
  # ---- QC inputs -------------------------------------------------------------
  allowed_reasons <- c("ice", "dry", "disturbance")
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
  
  if (missing(manual_note) || trimws(manual_note) == "") {
    stop("adjust_logger_NA(): 'manual_note' must be a non-empty string.")
  }
  
  # Normalize metric to short code used in logs
  metric_norm <- dplyr::case_when(
    metric %in% c("waterlevel", "water level", "WL")                  ~ "WL",
    metric %in% c("dissolvedoxygen", "dissolved_oxygen", "DO", "do")  ~ "DO",
    metric %in% c("watertemp", "watertemp_TidbiT", "WT", "temperature") ~ "WT",
    metric %in% c("barometric", "baro", "BARO")                       ~ "BARO",
    metric %in% c("conductivity", "cond", "EC")                       ~ "COND",
    TRUE ~ NA_character_
  )
  if (is.na(metric_norm)) {
    stop(
      "adjust_logger_NA(): Unrecognized metric '", metric,
      "'. Expected one of waterlevel, dissolvedoxygen, watertemp, barometric, conductivity."
    )
  }
  
  # Map metric to primary adjusted field for logging
  primary_field <- dplyr::case_when(
    metric_norm == "WL"   ~ "waterlevel_m_adj",
    metric_norm == "DO"   ~ "do_mgl_adj",
    metric_norm == "WT"   ~ "watertemp_C_adj",
    metric_norm == "BARO" ~ "airpress_kPa_adj",
    metric_norm == "COND" ~ "conduct_uScm_adj",
    TRUE                  ~ NA_character_
  )
  
  # Which edit_* flag are we setting?
  edit_flag_col <- dplyr::case_when(
    reason_to_adjust == "ice"         ~ "edit_remove_ice",
    reason_to_adjust == "dry"         ~ "edit_remove_dry",
    reason_to_adjust == "disturbance" ~ "edit_remove_disturbance",
    TRUE                              ~ "edit_remove_unknown"
  )
  
  # ---- Subset station + sort + basic checks ---------------------------------
  df <- input_data %>%
    dplyr::filter(site_station_code == select_station) %>%
    dplyr::arrange(timestamp)
  
  if (nrow(df) == 0) {
    stop("adjust_logger_NA(): no rows found for station '", select_station, "'.")
  }
  
  # primary adjusted field must exist
  if (!primary_field %in% names(df)) {
    stop(
      "adjust_logger_NA(): expected adjusted field '", primary_field,
      "' for metric '", metric_norm, "' not found in data."
    )
  }
  
  # timestamp + site required
  if (!all(c("timestamp", "site_station_code") %in% names(df))) {
    stop("adjust_logger_NA(): data must contain 'timestamp' and 'site_station_code'.")
  }
  
  has_wl <- "waterlevel_m" %in% names(df)
  
  # ---- Parse timestamps ------------------------------------------------------
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
  
  # ---- Internal flags --------------------------------------------------------
  df$in_window <- df$timestamp >= ts_start & df$timestamp <= ts_end
  df$edit_na   <- FALSE   # internal flag: rows touched by this function
  
  # Ensure edit_* flag column exists and is logical
  if (!edit_flag_col %in% names(df)) {
    df[[edit_flag_col]] <- FALSE
  } else {
    # coerce to logical if needed
    if (!is.logical(df[[edit_flag_col]])) {
      df[[edit_flag_col]] <- as.logical(df[[edit_flag_col]])
    }
    df[[edit_flag_col]][is.na(df[[edit_flag_col]])] <- FALSE
  }
  
  # ---- Metric-specific logic (only touch *_adj + edit_na) --------------------
  # ------------------------- WATER LEVEL --------------------------------------
  if (metric_norm == "WL") {
    
    if (!"watertemp_C_adj" %in% names(df)) {
      df$watertemp_C_adj <- NA_real_
    }
    
    if (reason_to_adjust == "ice") {
      df <- df %>%
        dplyr::mutate(
          edit_na          = dplyr::if_else(in_window, TRUE, edit_na),
          waterlevel_m_adj = dplyr::if_else(in_window, NA_real_, waterlevel_m_adj),
          watertemp_C_adj  = dplyr::if_else(in_window, NA_real_, watertemp_C_adj)
        )
    }
    
    if (reason_to_adjust == "dry") {
      
      if (apply_to_all_data) {
        dry_wl <- df %>%
          dplyr::filter(in_window, !is.na(waterlevel_m_adj)) %>%
          dplyr::summarise(dry_wl = max(waterlevel_m_adj, na.rm = TRUE)) %>%
          dplyr::pull(dry_wl)
        
        if (length(dry_wl) == 0 || is.infinite(dry_wl)) {
          warning("No non-NA waterlevel_m_adj in dry window; no WL dry adjustment applied.")
        } else {
          df <- df %>%
            dplyr::mutate(
              is_dry          = !is.na(waterlevel_m_adj) & waterlevel_m_adj <= dry_wl,
              edit_na         = dplyr::if_else(is_dry, TRUE, edit_na),
              waterlevel_m_adj = dplyr::if_else(is_dry, NA_real_, waterlevel_m_adj),
              watertemp_C_adj  = dplyr::if_else(is_dry, NA_real_, watertemp_C_adj)
            ) %>%
            dplyr::select(-is_dry)
        }
      } else {
        df <- df %>%
          dplyr::mutate(
            edit_na          = dplyr::if_else(in_window, TRUE, edit_na),
            waterlevel_m_adj = dplyr::if_else(in_window, NA_real_, waterlevel_m_adj),
            watertemp_C_adj  = dplyr::if_else(in_window, NA_real_, watertemp_C_adj)
          )
      }
    }
    
    if (reason_to_adjust == "disturbance") {
      if (keep_temp) {
        df <- df %>%
          dplyr::mutate(
            edit_na          = dplyr::if_else(in_window, TRUE, edit_na),
            waterlevel_m_adj = dplyr::if_else(in_window, NA_real_, waterlevel_m_adj)
          )
      } else {
        df <- df %>%
          dplyr::mutate(
            edit_na          = dplyr::if_else(in_window, TRUE, edit_na),
            waterlevel_m_adj = dplyr::if_else(in_window, NA_real_, waterlevel_m_adj),
            watertemp_C_adj  = dplyr::if_else(in_window, NA_real_, watertemp_C_adj)
          )
      }
    }
  }
  
  # ------------------------- DISSOLVED OXYGEN ---------------------------------
  if (metric_norm == "DO") {
    
    if (!all(c("do_mgl_adj", "do_percsat_adj") %in% names(df))) {
      stop("adjust_logger_NA(): DO metric expects 'do_mgl_adj' and 'do_percsat_adj'.")
    }
    if (!"watertemp_C_adj" %in% names(df)) {
      df$watertemp_C_adj <- NA_real_
    }
    
    if (reason_to_adjust == "ice") {
      df <- df %>%
        dplyr::mutate(
          edit_na         = dplyr::if_else(in_window, TRUE, edit_na),
          do_mgl_adj      = dplyr::if_else(in_window, NA_real_, do_mgl_adj),
          do_percsat_adj  = dplyr::if_else(in_window, NA_real_, do_percsat_adj),
          watertemp_C_adj = dplyr::if_else(in_window, NA_real_, watertemp_C_adj)
        )
    }
    
    if (reason_to_adjust == "disturbance") {
      if (keep_temp) {
        df <- df %>%
          dplyr::mutate(
            edit_na        = dplyr::if_else(in_window, TRUE, edit_na),
            do_mgl_adj     = dplyr::if_else(in_window, NA_real_, do_mgl_adj),
            do_percsat_adj = dplyr::if_else(in_window, NA_real_, do_percsat_adj)
          )
      } else {
        df <- df %>%
          dplyr::mutate(
            edit_na         = dplyr::if_else(in_window, TRUE, edit_na),
            do_mgl_adj      = dplyr::if_else(in_window, NA_real_, do_mgl_adj),
            do_percsat_adj  = dplyr::if_else(in_window, NA_real_, do_percsat_adj),
            watertemp_C_adj = dplyr::if_else(in_window, NA_real_, watertemp_C_adj)
          )
      }
    }
    
    if (reason_to_adjust == "dry") {
      
      if (apply_to_all_data && has_wl) {
        dry_wl <- df %>%
          dplyr::filter(in_window, !is.na(waterlevel_m)) %>%
          dplyr::summarise(dry_wl = max(waterlevel_m, na.rm = TRUE)) %>%
          dplyr::pull(dry_wl)
        
        if (length(dry_wl) == 0 || is.infinite(dry_wl)) {
          warning("No non-NA waterlevel_m in dry window; no DO dry adjustment applied.")
        } else {
          df <- df %>%
            dplyr::mutate(
              is_dry          = !is.na(waterlevel_m) & waterlevel_m <= dry_wl,
              edit_na         = dplyr::if_else(is_dry, TRUE, edit_na),
              do_percsat_adj  = dplyr::if_else(is_dry, NA_real_, do_percsat_adj),
              do_mgl_adj      = dplyr::if_else(is_dry, NA_real_, do_mgl_adj),
              watertemp_C_adj = dplyr::if_else(is_dry, NA_real_, watertemp_C_adj)
            ) %>%
            dplyr::select(-is_dry)
        }
      } else if (apply_to_all_data && !has_wl) {
        warning("Waterlevel column 'waterlevel_m' not found. Cannot apply DO dry adjustment to all data.")
      } else {
        df <- df %>%
          dplyr::mutate(
            edit_na         = dplyr::if_else(in_window, TRUE, edit_na),
            do_mgl_adj      = dplyr::if_else(in_window, NA_real_, do_mgl_adj),
            do_percsat_adj  = dplyr::if_else(in_window, NA_real_, do_percsat_adj),
            watertemp_C_adj = dplyr::if_else(in_window, NA_real_, watertemp_C_adj)
          )
      }
    }
  }
  
  # ------------------------- WATERTEMP (TidbiT) -------------------------------
  if (metric_norm == "WT") {
    
    if (!"watertemp_C_adj" %in% names(df)) {
      stop("adjust_logger_NA(): WT metric expects 'watertemp_C_adj'.")
    }
    
    if (reason_to_adjust %in% c("ice", "dry", "disturbance")) {
      df <- df %>%
        dplyr::mutate(
          edit_na        = dplyr::if_else(in_window, TRUE, edit_na),
          watertemp_C_adj = dplyr::if_else(in_window, NA_real_, watertemp_C_adj)
        )
    }
  }
  
  # ------------------------- BAROMETRIC ---------------------------------------
  if (metric_norm == "BARO") {
    
    if (!"airpress_kPa_adj" %in% names(df)) {
      stop("adjust_logger_NA(): BARO metric expects 'airpress_kPa_adj'.")
    }
    
    if (reason_to_adjust %in% c("ice", "dry", "disturbance")) {
      df <- df %>%
        dplyr::mutate(
          edit_na          = dplyr::if_else(in_window, TRUE, edit_na),
          airpress_kPa_adj = dplyr::if_else(in_window, NA_real_, airpress_kPa_adj)
        )
    }
  }
  
  # ------------------------- CONDUCTIVITY -------------------------------------
  if (metric_norm == "COND") {
    
    if (!"conductivity_uScm_adj" %in% names(df)) {
      stop("adjust_logger_NA(): COND metric expects 'conductivity_uScm_adj'.")
    }
    if (!"watertemp_C_adj" %in% names(df)) {
      df$watertemp_C_adj <- NA_real_
    }
    
    if (reason_to_adjust == "ice") {
      df <- df %>%
        dplyr::mutate(
          edit_na               = dplyr::if_else(in_window, TRUE, edit_na),
          conductivity_uScm_adj = dplyr::if_else(in_window, NA_real_, conductivity_uScm_adj),
          watertemp_C_adj       = dplyr::if_else(in_window, NA_real_, watertemp_C_adj)
        )
    }
    
    if (reason_to_adjust == "disturbance") {
      if (keep_temp) {
        df <- df %>%
          dplyr::mutate(
            edit_na               = dplyr::if_else(in_window, TRUE, edit_na),
            conductivity_uScm_adj = dplyr::if_else(in_window, NA_real_, conductivity_uScm_adj)
          )
      } else {
        df <- df %>%
          dplyr::mutate(
            edit_na               = dplyr::if_else(in_window, TRUE, edit_na),
            conductivity_uScm_adj = dplyr::if_else(in_window, NA_real_, conductivity_uScm_adj),
            watertemp_C_adj       = dplyr::if_else(in_window, NA_real_, watertemp_C_adj)
          )
      }
    }
    
    if (reason_to_adjust == "dry") {
      
      if (apply_to_all_data && has_wl) {
        dry_wl <- df %>%
          dplyr::filter(in_window, !is.na(waterlevel_m)) %>%
          dplyr::summarise(dry_wl = max(waterlevel_m, na.rm = TRUE)) %>%
          dplyr::pull(dry_wl)
        
        if (length(dry_wl) == 0 || is.infinite(dry_wl)) {
          warning("No non-NA waterlevel_m in dry window; no conductivity dry adjustment applied.")
        } else {
          df <- df %>%
            dplyr::mutate(
              is_dry                = !is.na(waterlevel_m) & waterlevel_m <= dry_wl,
              edit_na               = dplyr::if_else(is_dry, TRUE, edit_na),
              conductivity_uScm_adj = dplyr::if_else(is_dry, NA_real_, conductivity_uScm_adj),
              watertemp_C_adj       = dplyr::if_else(is_dry, NA_real_, watertemp_C_adj)
            ) %>%
            dplyr::select(-is_dry)
        }
      } else if (apply_to_all_data && !has_wl) {
        warning("Waterlevel column 'waterlevel_m' not found. Cannot apply conductivity dry adjustment to all data.")
      } else {
        df <- df %>%
          dplyr::mutate(
            edit_na               = dplyr::if_else(in_window, TRUE, edit_na),
            conductivity_uScm_adj = dplyr::if_else(in_window, NA_real_, conductivity_uScm_adj),
            watertemp_C_adj       = dplyr::if_else(in_window, NA_real_, watertemp_C_adj)
          )
      }
    }
  }
  
  # ---- Update the specific edit_* column -------------------------------------
  if (any(df$edit_na, na.rm = TRUE)) {
    df[[edit_flag_col]] <- df[[edit_flag_col]] | df$edit_na
  }
  
  # ---- Build & append QA/QC log entry ----------------------------------------
  ts_affected <- df$timestamp[df$edit_na]
  
  if (length(ts_affected) > 0) {
    
    fun_name <- "adjust_logger_NA"
    run_time <- Sys.time()
    
    scope_txt <- if (apply_to_all_data) {
      "window-based threshold applied to full record"
    } else {
      "fixed time window"
    }
    
    action_note <- paste0(
      "Set ", primary_field, " to NA for reason '", reason_to_adjust,
      "' using ", scope_txt, " from ", timestamp_start,
      " to ", timestamp_end, "."
    )
    
    log_row <- make_qaqc_log_row(
      timestamps  = ts_affected,
      station     = select_station,
      metric      = metric_norm,
      field       = primary_field,
      action      = "SET_NA",
      code        = paste0("LOGGER_", toupper(reason_to_adjust)),
      action_note = action_note,
      manual_note = manual_note,
      fun_name    = fun_name,
      user        = user,
      run_time    = run_time
    )
    
    log_path <- qaqc_log_path(log_root, select_station, metric_norm)
    
    if (!is.null(log_row)) {
      qaqc_log_append(
        log_path = log_path,
        station  = select_station,
        metric   = metric_norm,
        log_rows = log_row
      )
      message("QA/QC log updated: ", log_path)
    }
  } else {
    warning("adjust_logger_NA(): no rows were modified; no log entry written.")
  }
  
  # ---- Clean up internal columns and return ----------------------------------
  df %>%
    dplyr::select(-in_window, -edit_na)
}
