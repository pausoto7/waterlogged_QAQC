#' Adjust water level zero/reference
#'
#' Shift adjusted water levels by a constant offset to change the zero/reference
#' level. If `change_value` is `NULL`, the offset is computed from the mean
#' water level in a specified time window (assumed to represent the new zero).
#'
#' @param input_data Data frame containing water level data for one or more
#'   stations. Must include `timestamp`, `site_station_code`, `waterlevel_m`,
#'   and `waterlevel_m_adj`.
#' @param select_station Character; station code to adjust
#'   (value of `site_station_code`).
#' @param change_value Numeric offset (m) to add to `waterlevel_m_adj`.
#'   Positive values shift levels up, negative values shift them down. If
#'   `NULL`, the offset is computed from the average water level between
#'   `timestamp_start` and `timestamp_end`.
#' @param timestamp_start Start of window used to compute automatic zero
#'   (character `"YYYY-MM-DD HH:MM:SS"`, typically UTC), only used if
#'   `change_value` is `NULL`.
#' @param timestamp_end End of window used to compute automatic zero
#'   (character `"YYYY-MM-DD HH:MM:SS"`, typically UTC), only used if
#'   `change_value` is `NULL`.
#' @param manual_note Character, required. Description of why the zero shift
#'   was applied (stored in the QA/QC log).
#' @param log_root Root folder used to build the QA/QC log path (passed to
#'   `qaqc_log_path()`).
#' @param user Username written to the QA/QC log. Defaults to
#'   `Sys.info()[["user"]]`.
#'
#' @return Modified data frame for `select_station` with updated
#'   `waterlevel_m_adj` and `edit_zero` flag, and a QA/QC log entry written
#'   to disk.
#'
#' @details
#' If `change_value` is supplied, that constant is added to the entire
#' `waterlevel_m_adj` series.
#'
#' If `change_value` is `NULL`, the function:
#' \itemize{
#'   \item Filters rows between `timestamp_start` and `timestamp_end`.
#'   \item Computes the mean of `waterlevel_m_adj` in that window, if present;
#'         otherwise falls back to `waterlevel_m`.
#'   \item Uses the negative of this mean as the offset, so that the mean level
#'         in that window becomes approximately 0 m.
#'   \item Applies this offset to all `waterlevel_m_adj` values.
#' }
#' In both cases, `edit_zero` is set to `TRUE` for all rows for which the
#' adjusted series was shifted.
#'
#' @seealso [adjust_WL_offset()], [adjust_waterlevel_spike()],
#'   [adjust_logger_NA()], [make_qaqc_log_row()], [qaqc_log_path()],
#'   [qaqc_log_append()]
#'
#' @importFrom dplyr filter mutate
#' @importFrom lubridate ymd_hms
#' @export
adjust_WL_zero <- function(input_data,
                           select_station,
                           change_value    = NULL,
                           timestamp_start = NULL,
                           timestamp_end   = NULL,
                           manual_note,
                           log_root,
                           user = Sys.info()[["user"]]) {
  # --- Basic input checks -----------------------------------------------------
  if (missing(manual_note) || trimws(manual_note) == "") {
    stop("adjust_WL_zero(): 'manual_note' must be a non-empty string.")
  }
  
  # Filter + basic structure
  waterlevel_data <- input_data %>%
    dplyr::filter(site_station_code == !!select_station) %>%
    dplyr::arrange(timestamp)
  
  required_cols <- c(
    "timestamp",
    "site_station_code",
    "waterlevel_m",
    "waterlevel_m_adj"
  )
  missing_cols <- setdiff(required_cols, names(waterlevel_data))
  if (length(missing_cols) > 0) {
    stop(
      "adjust_WL_zero(): Missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (nrow(waterlevel_data) == 0) {
    stop("adjust_WL_zero(): no rows found for station '", select_station, "'.")
  }
  
  # Ensure edit_zero flag exists
  if (!"edit_zero" %in% names(waterlevel_data)) {
    waterlevel_data$edit_zero <- FALSE
  }
  
  # --- Determine change_value -------------------------------------------------
  # Case 1: change_value explicitly supplied
  if (!is.null(change_value)) {
    if (!is.numeric(change_value) || length(change_value) != 1L || is.na(change_value)) {
      stop("adjust_WL_zero(): 'change_value' must be a single numeric value (m).")
    }
    offset_m     <- change_value
    window_text  <- NA_character_
    
  } else {
    # Case 2: change_value == NULL → compute from window
    if (is.null(timestamp_start) || is.null(timestamp_end)) {
      stop(
        "adjust_WL_zero(): When 'change_value' is NULL, both ",
        "'timestamp_start' and 'timestamp_end' must be supplied."
      )
    }
    
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
    
    window_data <- waterlevel_data %>%
      dplyr::filter(timestamp >= ts_start, timestamp <= ts_end)
    
    if (nrow(window_data) == 0) {
      stop(
        "adjust_WL_zero(): No rows found between timestamp_start and timestamp_end ",
        "for station ", select_station, "."
      )
    }
    
    # Prefer adjusted series if present, fall back to raw waterlevel_m
    mean_wl <- if ("waterlevel_m_adj" %in% names(window_data)) {
      mean(window_data$waterlevel_m_adj, na.rm = TRUE)
    } else {
      mean(window_data$waterlevel_m, na.rm = TRUE)
    }
    
    if (is.na(mean_wl)) {
      stop(
        "adjust_WL_zero(): All water level values in the specified window are NA; ",
        "cannot compute automatic zero shift."
      )
    }
    
    # Shift so that mean in the window becomes ~0
    offset_m    <- -mean_wl
    window_text <- paste0(timestamp_start, " to ", timestamp_end)
  }
  
  # --- Apply constant offset to adjusted series -------------------------------
  waterlevel_data <- waterlevel_data %>%
    dplyr::mutate(
      waterlevel_m_adj = waterlevel_m_adj + offset_m,
      edit_zero        = TRUE
    )
  
  # --- Build QA/QC log entry --------------------------------------------------
  metric   <- "WL"
  field    <- "waterlevel_m_adj"
  action   <- "ZERO_SHIFT"
  fun_name <- "adjust_WL_zero"
  run_time <- Sys.time()
  code     <- "SHIFT_CORRECTION"
  
  ts_affected <- waterlevel_data$timestamp  # entire series shifted
  
  auto_note <- if (!is.na(window_text)) {
    paste0(
      "Automatic zero shift computed from mean adjusted water level between ",
      window_text, " (offset = ", round(offset_m, 3), " m)."
    )
  } else {
    paste0("User-specified zero shift of ", round(offset_m, 3), " m applied.")
  }
  
  log_this <- make_qaqc_log_row(
    timestamps  = ts_affected,
    station     = select_station,
    metric      = metric,
    field       = field,
    action      = action,
    code        = code,
    action_note = auto_note,
    manual_note = manual_note,
    fun_name    = fun_name,
    user        = user,
    run_time    = run_time
  )
  
  log_path <- qaqc_log_path(log_root, select_station, metric)
  
  if (!is.null(log_this)) {
    qaqc_log_append(
      log_path = log_path,
      station  = select_station,
      metric   = metric,
      log_rows = log_this
    )
    message("QA/QC log updated: ", log_path)
  } else {
    warning("No rows adjusted; no log entry written.")
  }
  
  return(waterlevel_data)
}
