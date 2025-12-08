#' Apply multi-point linear drift correction to water level
#'
#' This function applies a piecewise-linear drift correction to
#' `waterlevel_m_adj` for a single station. You provide a set of
#' time-stamped drift control points , each with a desired drift value in metres. 
#' The function linearly interpolates between successive control points and
#' subtracts that correction from `waterlevel_m_adj`. Data before the first 
#' control point and after the last control point are left unchanged.
#'
#' @param input_data Data frame containing at least:
#'   `site_station_code`, `timestamp`, `waterlevel_m` or `waterlevel_m_adj`.
#' @param select_station Character. Station code to correct.
#' @param log_root Character. Root folder for QA/QC logs.
#' @param drift_points A data frame or tibble with columns:
#'   \describe{
#'     \item{timestamp}{Drift control timestamps (POSIXct or character).}
#'     \item{drift_m}{Numeric drift (m) at each timestamp. Positive means
#'     the logger is reading too low (we will add this amount to bring
#'     the series up).}
#'   }
#'   Must contain at least two rows.
#' @param user Character. Username for the QA/QC log; defaults to system user.
#' @param manual_note Optional character note to include in the log.
#'
#' @return A modified data frame with updated `waterlevel_m_adj` for
#'   `select_station`. All other stations/rows are unchanged.
#'
#' @details
#' For each interval \eqn{[t_i, t_{i+1}]} between control points, a linear
#' correction is constructed:
#'
#' \deqn{
#'   c(t) = d_i + (d_{i+1} - d_i) * \frac{t - t_i}{t_{i+1} - t_i}
#' }
#'
#' where \eqn{d_i} and \eqn{d_{i+1}} are the drift values (in metres) at
#' times \eqn{t_i} and \eqn{t_{i+1}}. The adjusted water level is then:
#'
#' \deqn{
#'   waterlevel\_m\_adj = waterlevel\_m\_adj - c(t)
#' }
#'
#' No correction is applied outside the range of the earliest and latest
#' control timestamps.
#'
#' A single QA/QC log row is written with code `MULTI_WL_DRIFT` summarising
#' the time window and drift values.
#'
#' @export
wl_multipoint_correction <- function(input_data,
                                      select_station,
                                      log_root,
                                      drift_points,
                                      user        = Sys.info()[["user"]],
                                      manual_note = NULL) {
  
  # ---- basic checks on input_data -------------------------------------------
  required_cols <- c("site_station_code", "timestamp")
  missing_cols  <- setdiff(required_cols, names(input_data))
  if (length(missing_cols) > 0) {
    stop(
      "wl_apply_multipoint_drift(): input_data is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # ---- check drift_points ----------------------------------------------------
  if (is.null(drift_points) || nrow(drift_points) < 2) {
    stop("wl_apply_multipoint_drift(): drift_points must have at least 2 rows.")
  }
  
  if (!all(c("timestamp", "drift_m") %in% names(drift_points))) {
    stop("wl_apply_multipoint_drift(): drift_points must have columns 'timestamp' and 'drift_m'.")
  }
  
  # Coerce drift timestamps to POSIXct (UTC)
  if (!inherits(drift_points$timestamp, "POSIXct")) {
    drift_points$timestamp <- lubridate::ymd_hms(drift_points$timestamp, tz = "UTC")
  }
  
  # Sort drift points by time
  drift_points <- drift_points[order(drift_points$timestamp), ]
  
  # Remove any duplicated timestamps
  dup_ts <- duplicated(drift_points$timestamp)
  if (any(dup_ts)) {
    warning("wl_apply_multipoint_drift(): duplicate timestamps in drift_points removed.")
    drift_points <- drift_points[!dup_ts, ]
  }
  
  if (nrow(drift_points) < 2) {
    stop("wl_apply_multipoint_drift(): need at least 2 unique drift timestamps.")
  }
  
  # ---- subset to station & ensure waterlevel_m_adj --------------------------
  output_data <- input_data
  
  idx_station <- which(output_data$site_station_code == select_station)
  if (length(idx_station) == 0) {
    stop("wl_apply_multipoint_drift(): no rows found for station '", select_station, "'.")
  }
  
  station_df <- output_data[idx_station, ]
  
  # Ensure timestamp is POSIXct
  if (!inherits(station_df$timestamp, "POSIXct")) {
    station_df$timestamp <- lubridate::ymd_hms(station_df$timestamp, tz = "UTC")
  }
  
  # Make sure we have an adjusted column to work on
  if (!"waterlevel_m_adj" %in% names(station_df)) {
    if (!"waterlevel_m" %in% names(station_df)) {
      stop("wl_apply_multipoint_drift(): need 'waterlevel_m' or 'waterlevel_m_adj' in station data.")
    }
    station_df$waterlevel_m_adj <- station_df$waterlevel_m
  }
  
  # ---- build correction vector (piecewise-linear) ---------------------------
  # Initialise correction = 0 everywhere
  correction <- rep(0, nrow(station_df))
  
  # Only apply correction between min and max drift timestamps
  t_min <- min(drift_points$timestamp, na.rm = TRUE)
  t_max <- max(drift_points$timestamp, na.rm = TRUE)
  
  # If station has no data in that window, just return unchanged
  has_overlap <- any(station_df$timestamp >= t_min & station_df$timestamp <= t_max)
  if (!has_overlap) {
    warning("wl_apply_multipoint_drift(): station data has no overlap with drift_points window; returning unchanged data.")
    output_data[idx_station, ] <- station_df
    return(output_data)
  }
  
  # For each segment [t_i, t_{i+1}], interpolate linearly
  for (i in seq_len(nrow(drift_points) - 1L)) {
    t_start <- drift_points$timestamp[i]
    t_end   <- drift_points$timestamp[i + 1L]
    d_start <- drift_points$drift_m[i]
    d_end   <- drift_points$drift_m[i + 1L]
    
    # indices of rows in this segment
    seg_idx <- which(station_df$timestamp >= t_start &
                       station_df$timestamp <= t_end)
    if (length(seg_idx) == 0) next
    
    t_seg <- station_df$timestamp[seg_idx]
    # normalised time from 0 to 1
    frac <- as.numeric(t_seg - t_start, units = "secs") /
      as.numeric(t_end - t_start, units = "secs")
    
    # linear interpolation: d(t) = d_start + (d_end - d_start) * frac
    correction[seg_idx] <- d_start + (d_end - d_start) * frac
  }
  
  # NOTE: outside [t_min, t_max], correction remains 0 by design (no carry-forward)
  
  # ---- apply correction to waterlevel_m_adj ---------------------------------
  station_df$waterlevel_m_adj <- station_df$waterlevel_m_adj + correction
  
  # ---- write QA/QC log row --------------------------------------------------
  metric   <- "WL"
  fun_name <- "wl_apply_multipoint_drift"
  action   <- "DRIFT_CORRECTION"
  code     <- "MULTI_WL_DRIFT"
  run_time <- Sys.time()
  
  # build a short summary of control points for the log
  drift_summary <- paste0(
    "Multi-point linear drift correction: ",
    "from ", format(t_min, "%Y-%m-%d %H:%M:%S"), " (", drift_points$drift_m[1], " m) ",
    "to ", format(t_max, "%Y-%m-%d %H:%M:%S"), " (", tail(drift_points$drift_m, 1), " m). ",
    "Intermediate points: ",
    paste(
      paste0("[", format(drift_points$timestamp, "%Y-%m-%d %H:%M:%S"),
             " -> ", drift_points$drift_m, " m]"),
      collapse = "; "
    )
  )
  
  if (!is.null(manual_note) && nzchar(manual_note)) {
    action_note <- paste(drift_summary, "User note:", manual_note)
  } else {
    action_note <- drift_summary
  }
  
  log_row <- make_qaqc_log_row(
    timestamps  = c(t_min, t_max),
    station     = select_station,
    metric      = metric,
    field       = "waterlevel_m_adj",
    action      = action,
    code        = code,
    action_note = action_note,
    manual_note = manual_note %||% "Multi-point linear drift correction.",
    fun_name    = fun_name,
    user        = user,
    run_time    = run_time
  )
  
  log_path <- qaqc_log_path(log_root, select_station, metric)
  
  qaqc_log_append(
    log_path = log_path,
    station  = select_station,
    metric   = metric,
    log_rows = log_row
  )
  
  message("WL drift correction applied for station ", select_station,
          " and logged to: ", log_path)
  
  # ---- stitch back into full dataset ----------------------------------------
  output_data[idx_station, ] <- station_df
  output_data
}
