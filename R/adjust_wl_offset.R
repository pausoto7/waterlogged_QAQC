#' Apply a constant offset to water level after a break
#'
#' Applies a constant offset to the adjusted water level series
#' (\code{waterlevel_m_adj}) after a specified time, typically to correct
#' for a cable break, logger shift, or other step change. The offset is
#' computed from the difference between raw water levels at two reference
#' timestamps and applied to all timestamps at or after \code{timestamp_end}.
#' A QA/QC log entry is written for all affected rows.
#'
#' @param input_data Data frame containing water-level data for one or more
#'   stations, including \code{timestamp}, \code{site_station_code},
#'   \code{waterlevel_m}, and \code{waterlevel_m_adj}.
#' @param select_station Character; station code to adjust (value of
#'   \code{site_station_code}).
#' @param timestamp_start Character date-time for the pre-offset reference
#'   point (in \code{"YYYY-MM-DD HH:MM:SS"} format), parsed with
#'   \code{lubridate::ymd_hms()}.
#' @param timestamp_end Character date-time for the post-offset reference
#'   point and the time from which the offset will be applied onward
#'   (same format as \code{timestamp_start}).
#' @param manual_note Character; required free-text explanation describing why
#'   this adjustment is being made. Stored in the QA/QC log.
#' @param log_root Root directory where QA/QC logs are stored (for example,
#'   \code{"data/testing/processed"}). The actual log path is derived from
#'   this root, station, and metric.
#' @param user Character; user name recorded in the log entry. Defaults to
#'   \code{Sys.info()[["user"]]}.
#'
#' @return A data frame for \code{select_station} with updated
#'   \code{waterlevel_m_adj} values and \code{edit_offset} flag. The input
#'   columns are preserved; only adjusted values and flags are changed.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Filters \code{input_data} to \code{select_station}.
#'   \item Extracts raw \code{waterlevel_m} at \code{timestamp_start} and
#'         \code{timestamp_end}.
#'   \item Computes a constant offset as \code{after\_break - before\_break}.
#'   \item Subtracts this offset from \code{waterlevel_m_adj} for all
#'         timestamps \code{>= timestamp_end}.
#'   \item Sets \code{edit_offset = TRUE} for affected rows.
#'   \item Writes a QA/QC log entry via \code{make_qaqc_log_row()} and
#'         \code{qaqc_log_append()}.
#' }
#'
#' @seealso \code{\link{adjust_waterlevel_spike}},
#'   \code{\link{adjust_logger_NA}},
#'   \code{\link{make_qaqc_log_row}},
#'   \code{\link{qaqc_log_append}}
#'
#' @importFrom dplyr filter mutate
#' @importFrom lubridate ymd_hms
#' @examples
#' \dontrun{
#' station_wl_qc2 <- adjust_WL_offset(
#'   input_data      = station_wl_qc,
#'   select_station  = "WL_ALBR_ST_30",
#'   timestamp_start = "2025-02-04 07:00:00",
#'   timestamp_end   = "2025-02-09 00:00:00",
#'   manual_note     = "Animal suspected to have moved logger",
#'   log_root        = "data/testing/processed"
#' )
#' }
#'
#' @export

adjust_WL_offset <- function(input_data,
                                         select_station,
                                         timestamp_start,
                                         timestamp_end,
                                         manual_note,
                                         log_root,
                                         user = Sys.info()[["user"]]) {
  
  
  #  QAQC inputs
  if (missing(manual_note) || trimws(manual_note) == "") {
    stop("adjust_wwl_offset(): 'manual_note' must be a non-empty string.")
  }
  
  # ---- Filter to selected station & sort -------------------------------------
  waterlevel_data <- input_data %>%
    dplyr::filter(site_station_code == select_station) %>%
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
      "adjust_waterlevel_cablebreak(): Missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (nrow(waterlevel_data) == 0) {
    stop("adjust_waterlevel_cablebreak(): no rows found for station '", select_station, "'.")
  }
  
  # ---- Parse timestamps (character -> POSIXct) -------------------------------
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
  
  # ---- Get start/end water levels (raw) --------------------------------------
  before_break <- waterlevel_data %>%
    dplyr::filter(timestamp == ts_start) %>%
    dplyr::pull(waterlevel_m) %>%
    round(3)
  
  after_break <- waterlevel_data %>%
    dplyr::filter(timestamp == ts_end) %>%
    dplyr::pull(waterlevel_m) %>%
    round(3)
  
  if (length(before_break) != 1L || length(after_break) != 1L) {
    stop(
      "Could not find unique waterlevel_m at both timestamp_start and ",
      "timestamp_end for station ", select_station, "."
    )
  }
  
  # Offset needed to realign post-break data to pre-break level
  corr_break <- after_break - before_break
  
  # ---- Ensure flag column exists ---------------------------------------------
  if (!"edit_offset" %in% names(waterlevel_data)) {
    waterlevel_data$edit_offset <- FALSE
  }
  
  # ---- Apply constant offset after the break ---------------------------------
  waterlevel_data <- waterlevel_data %>%
    dplyr::mutate(
      after_break = timestamp >= ts_end,
      
      # Only ever touch the adjusted series; keep raw intact
      waterlevel_m_adj = dplyr::case_when(
        after_break ~ waterlevel_m_adj - corr_break,
        TRUE        ~ waterlevel_m_adj
      ),
      
      edit_offset = dplyr::if_else(
        after_break,
        TRUE,
        edit_offset
      )
    ) %>%
    dplyr::select(-after_break)
  
  # ---- Build QA/QC log entry -------------------------------------------------
  metric   <- "WL"
  field    <- "waterlevel_m_adj"
  action   <- "CABLE_OFFSET"
  fun_name <- "adjust_waterlevel_cablebreak"
  run_time <- Sys.time()
  code     <- "LOGGER_DISTURBANCE"
  
  ts_affected <- waterlevel_data$timestamp[
    waterlevel_data$timestamp >= ts_end
  ]
  
  log_this <- make_qaqc_log_row(
    timestamps  = ts_affected,
    station     = select_station,
    metric      = metric,
    field       = field,
    action      = action,
    code        = code,
    action_note = paste0(
      "Applied constant offset of ", round(corr_break, 3),
      " m to waterlevel_m_adj for timestamps >= ", timestamp_end,
      ". Pre-break reference at ", timestamp_start, "."
    ),
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
