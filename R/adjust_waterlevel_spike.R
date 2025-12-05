
#' Flatten a water-level spike over a specified time window
#'
#' Replaces a spike (or other unrealistic jump) in water level with a straight
#' line between two timestamps for a single station, and records the change in
#' the QA/QC log.
#'
#' The function:
#' \itemize{
#'   \item Filters \code{input_data} to \code{select_station}.
#'   \item Locates the raw water level at \code{timestamp_start} and
#'         \code{timestamp_end}.
#'   \item Linearly interpolates between these two values over the full
#'         time window.
#'   \item Writes the interpolated values into \code{waterlevel_m_adj} for
#'         all timestamps in the window.
#'   \item Sets \code{edit_spike_flat = TRUE} for affected rows.
#'   \item Appends an entry to the QA/QC log via \code{make_qaqc_log_row()}
#'         and \code{qaqc_log_append()}.
#' }
#'
#' @param input_data Data frame containing water-level data for one or more
#'   stations, including \code{timestamp}, \code{site_station_code},
#'   \code{waterlevel_m}, and \code{waterlevel_m_adj}.
#' @param select_station Character; station code to adjust (value of
#'   \code{site_station_code}).
#' @param timestamp_start,timestamp_end Character date-time values defining
#'   the spike window, in \code{"YYYY-MM-DD HH:MM:SS"} format, parsed with
#'   \code{lubridate::ymd_hms()}.
#' @param reason_to_adjust Character; reason for flattening. One of
#'   \code{"ice"} or \code{"disturbance"}. Used to choose the QA/QC code
#'   written to the log.
#' @param manual_note Character; required free-text explanation describing why
#'   this adjustment is being made. Stored in the QA/QC log.
#' @param log_root Root directory where QA/QC logs are stored (for example,
#'   \code{"data/testing/processed"}). The actual log path is derived from
#'   this root, station, and metric.
#' @param user Character; user name recorded in the log entry. Defaults to
#'   \code{Sys.info()[["user"]]}.
#'
#' @return A data frame for \code{select_station} with updated
#'   \code{waterlevel_m_adj} values and \code{edit_spike_flat} flag. The input
#'   columns are preserved; only adjusted values and flags are changed.
#'
#' @details
#' The start and end water levels are taken from the raw
#' \code{waterlevel_m} column at \code{timestamp_start} and
#' \code{timestamp_end}. The interpolation is performed in time units of
#' seconds and written into \code{waterlevel_m_adj}. If the function cannot
#' find unique raw values at both timestamps, it errors.
#'
#' @seealso \code{\link{waterlevel_qaqc}}, \code{\link{adjust_logger_NA}},
#'   \code{\link{make_qaqc_log_row}}, \code{\link{qaqc_log_append}}
#'
#' @import dplyr
#' @importFrom lubridate ymd_hms
#'
#' @examples
#' \dontrun{
#' adjusted_wl <- adjust_waterlevel_spike(
#'   input_data      = waterlevel_complete_QAQC,
#'   select_station  = "WL_ALBR_ST_30",
#'   timestamp_start = "2025-02-11 21:00:00",
#'   timestamp_end   = "2025-02-20 16:00:00",
#'   reason_to_adjust = "ice",
#'   manual_note      = "Suspected ice formation due to freezing temps",
#'   log_root         = "data/testing/processed"
#' )
#' }
#'
#' @export
adjust_waterlevel_spike <- function(input_data,
                                    select_station,
                                    timestamp_start,
                                    timestamp_end,
                                    reason_to_adjust, 
                                    manual_note, 
                                    log_root, 
                                    user = Sys.info()[["user"]]) {
  # QAQC inputs  ---------------------------------------------------
  allowed_reasons <- c("ice", "disturbance")
  
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
  
  if (missing(manual_note) || trimws(manual_note) == "")
    stop("adjust_waterlevel_spike(): 'manual_note' must be a non-empty string.")
  
  # --- Filter to selected station & sort --------------------------------------
  waterlevel_data <- input_data %>%
    dplyr::filter(site_station_code == !!select_station) %>%
    dplyr::arrange(timestamp)
  
  required_cols <- c(
    "timestamp",
    "site_station_code",
    "waterlevel_m",
    "waterlevel_m_adj"
  )
  
  
  missing_cols  <- setdiff(required_cols, names(waterlevel_data))
  if (length(missing_cols) > 0) {
    stop(
      "adjust_waterlevel_spike(): Missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # --- Parse timestamps --------------------------------
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
  
  # --- Get start/end water levels (raw) ---------------------------------------

   start_val <- waterlevel_data %>%
    dplyr::filter(timestamp == ts_start) %>%
    dplyr::pull(waterlevel_m) %>%
    round(3)
  
  end_val <- waterlevel_data %>%
    dplyr::filter(timestamp == ts_end) %>%
    dplyr::pull(waterlevel_m) %>%
    round(3)
  
  if (length(start_val) != 1L || length(end_val) != 1L) {
    stop(
      "Could not find unique waterlevel_m at both timestamp_start and ",
      "timestamp_end for station ", select_station, "."
    )
  }
  
  # Total change and total time between start and end (seconds)
  total_change <- end_val - start_val
  total_time   <- as.numeric(difftime(ts_end, ts_start, units = "secs"))
  
  # --- If edit_spike_flat doesn't exist yet, create it as FALSE ----------------
  if (!"edit_spike_flat" %in% names(waterlevel_data)) {
    waterlevel_data$edit_spike_flat <- FALSE
  }
  
  # --- Apply spike flattening (time-based interpolation) ----------------------
  
  waterlevel_data <- waterlevel_data %>%
    dplyr::mutate(
      in_event = timestamp >= ts_start & timestamp <= ts_end,
      
      interpolated_value = start_val + total_change *
        (as.numeric(difftime(timestamp, ts_start, units = "secs")) / total_time),
      
      waterlevel_m_adj = dplyr::case_when(
        in_event ~ interpolated_value,
        TRUE     ~ waterlevel_m_adj
      ),
      
      # new simple flag for rows touched by this function
      edit_spike_flat = dplyr::if_else(
        in_event,
        TRUE,
        edit_spike_flat
      ))%>%
    dplyr::select(-in_event, -interpolated_value)
  
  
  # log entry ------------------------------------------------------------
  metric <- "WL"
  field    <- "waterlevel_m_adj"
  action   <- "SPIKE_FLATTENED"
  fun_name <- "adjust_waterlevel_spike"
  run_time <- Sys.time()
  
  code <- if (reason_to_adjust == "ice") "LOGGER_ICE" else "LOGGER_DISTURBANCE"
  
  ts_affected <- waterlevel_data$timestamp[
    waterlevel_data$timestamp >= ts_start &
      waterlevel_data$timestamp <= ts_end
  ]
  
  log_this <- make_qaqc_log_row(
    timestamps  = ts_affected,
    station     = select_station,
    metric      = metric,
    field       = field,
    action      = action,
    code        = code,
    action_note = paste0("Spike flattened from ", timestamp_start, " to ", timestamp_end,
                         " (reason: ", reason_to_adjust, ")."),
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
