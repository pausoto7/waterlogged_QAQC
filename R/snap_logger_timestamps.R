#' Align (snap) logger timestamps to a regular interval
#'
#' This function standardises the `timestamp` column onto a regular time
#' grid (e.g., 5, 10, 15, 30 minutes, 1 hour) using rounding, flooring,
#' or ceiling. The original logger times are preserved in `timestamp_raw`
#' (created once if missing).
#'
#' Importantly, this function **does not change the sampling frequency**:
#' it does not drop or average rows. If your logger is at 5-minute
#' resolution and you specify `interval = "30 min"`, all 5-minute rows
#' are kept; their timestamps are simply moved to the nearest 30-minute
#' grid line. This may result in multiple rows sharing the same
#' `timestamp`.
#'
#' For true resampling/aggregation (e.g., converting 5-minute data to
#' 30-minute means), use a separate aggregation function.
#'
#' If `log_root`, `metric`, and `select_station` are provided, a QA/QC log
#' entry is written using the package logging helpers.
#'
#' @param input_data A data frame containing at least a `timestamp` column.
#'   If `select_station` is used, a `site_station_code` column is also
#'   required.
#' @param interval Character string giving the target time interval, passed
#'   to \code{lubridate::round_date()}, e.g. `"5 min"`, `"10 min"`,
#'   `"15 min"`, `"30 min"`, `"1 hour"`.
#' @param align How to align timestamps to the grid. One of `"round"`,
#'   `"floor"`, `"ceiling"`. See \code{lubridate::round_date()}.
#' @param select_station Optional station code. If supplied and
#'   `site_station_code` exists, the function will operate only on that
#'   station's rows and return the filtered data frame.
#' @param metric Optional character code for the metric (e.g. `"WL"`,
#'   `"DO"`, `"COND"`, `"BARO"`), used only for QA/QC logging.
#' @param log_root Optional path to the root QA/QC log folder. If provided
#'   together with `metric` and `select_station`, a log entry is appended
#'   for this operation.
#' @param user Username recorded in QA/QC logs. Defaults to the system user.
#'
#' @return A data frame with:
#'   \itemize{
#'     \item \code{timestamp_raw}: original logger timestamps (created if
#'       missing).
#'     \item \code{timestamp}: snapped timestamps on the specified grid.
#'   }
#'
#'   All other columns are returned unchanged.
#'
#' @examples
#' \dontrun{
#' aligned_wl <- resample_timestamps(
#'   input_data     = wl_raw,
#'   interval       = "15 min",
#'   align          = "round",
#'   select_station = "WL_ALBR_ST_30",
#'   metric         = "WL",
#'   log_root       = "data/processed"
#' )
#' }
#'
#' @export
resample_timestamps <- function(input_data,
                                interval       = "15 min",
                                align          = c("round", "floor", "ceiling"),
                                select_station = NULL,
                                metric         = NULL,
                                log_root       = NULL,
                                user           = Sys.info()[["user"]]) {
  align <- match.arg(align)
  
  if (!"timestamp" %in% names(input_data)) {
    stop("resample_timestamps(): input_data must contain a 'timestamp' column.")
  }
  
  # Optional station filter ----------------------------------------------------
  if (!is.null(select_station)) {
    if (!"site_station_code" %in% names(input_data)) {
      stop(
        "resample_timestamps(): `select_station` provided but ",
        "'site_station_code' column is missing."
      )
    }
    
    df <- input_data[input_data$site_station_code == select_station, , drop = FALSE]
    
    if (nrow(df) == 0) {
      warning("resample_timestamps(): no rows for station ", select_station)
      return(df)
    }
  } else {
    df <- input_data
  }
  
  # Ensure POSIXct -------------------------------------------------------------
  if (!inherits(df$timestamp, "POSIXct")) {
    df$timestamp <- lubridate::ymd_hms(df$timestamp, tz = "UTC")
  }
  
  # Sort by time to make interval check sensible
  df <- df[order(df$timestamp), , drop = FALSE]
  
  # Preserve original times in timestamp_raw -----------------------------------
  if (!"timestamp_raw" %in% names(df)) {
    df$timestamp_raw <- df$timestamp
  }
  
  # Warn if requested interval is finer than native logger interval -----------
  if (nrow(df) > 1) {
    # Use raw timestamps if present to reflect logger behaviour
    ts_vec <- df$timestamp_raw
    orig_interval_sec <- stats::median(diff(as.numeric(ts_vec)), na.rm = TRUE)
    
    target_interval_sec <- as.numeric(lubridate::duration(interval))
    
    if (!is.na(orig_interval_sec) && !is.na(target_interval_sec) &&
        orig_interval_sec > target_interval_sec) {
      warning(
        "resample_timestamps(): The requested interval (", interval,
        ") is finer than the original logger interval (~",
        round(orig_interval_sec / 60, 2), " minutes). ",
        "Timestamps will be snapped to the nearest ", interval,
        " grid, but no new rows will be created and no interpolation will occur.\n"
      )
    }
  }
  
  # Compute snapped timestamps -------------------------------------------------
  snap_fun <- switch(
    align,
    round   = lubridate::round_date,
    floor   = lubridate::floor_date,
    ceiling = lubridate::ceiling_date
  )
  
  snapped_ts <- snap_fun(df$timestamp, unit = interval)
  
  # Identify changed rows (for logging)
  changed_idx <- which(df$timestamp_raw != snapped_ts)
  
  # Apply snapped timestamps
  df$timestamp <- snapped_ts
  
  # QA/QC logging --------------------------------------------------------------
  do_logging <- !is.null(log_root) && !is.null(metric) && !is.null(select_station)
  
  if (do_logging) {
    run_time    <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
    fun_name    <- "resample_timestamps"
    manual_note <- "Timestamps snapped to a regular interval."
    action      <- "SNAP_TIME"
    field       <- "timestamp"
    
    if (length(changed_idx) > 0) {
      ts_changed <- df$timestamp_raw[changed_idx]
      
      log_rows <- make_qaqc_log_row(
        timestamps  = ts_changed,
        station     = select_station,
        metric      = metric,
        field       = field,
        action      = action,
        code        = "SNAP_TIME",
        action_note = paste0(
          "Timestamps snapped to ", interval,
          " using ", align, " alignment."
        ),
        manual_note = manual_note,
        fun_name    = fun_name,
        user        = user,
        run_time    = run_time
      )
    } else {
      log_rows <- dplyr::tibble(
        station        = select_station,
        metric         = metric,
        field          = field,
        action         = action,
        code           = NA_character_,
        action_note    = paste0(
          "resample_timestamps() ran; timestamps already aligned to ",
          interval, " (no changes applied)."
        ),
        manual_note    = manual_note,
        ts_start       = NA_character_,
        ts_end         = NA_character_,
        duration_hours = NA_real_,
        fun_name       = fun_name,
        run_at         = run_time,
        user           = user
      )
    }
    
    log_path <- qaqc_log_path(log_root, select_station, metric)
    
    qaqc_log_append(
      log_path = log_path,
      station  = select_station,
      metric   = metric,
      log_rows = log_rows
    )
    
    message("QA/QC log updated (resample_timestamps): ", log_path)
  }
  
  return(df)
}
