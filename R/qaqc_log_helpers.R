# Internal: build path to a station+metric log file
qaqc_log_path <- function(log_root, station, metric) {
  # logs live above the year folders, in a "logs" dir
  log_dir  <- file.path(log_root, "logs")
  log_file <- paste0(station, "_", metric, "_qaqc_log.csv")
  file.path(log_dir, log_file)
}

# Internal: append rows to a station+metric log (create if missing)
qaqc_log_append <- function(log_path, station, metric, log_rows) {
  
  # Ensure log directory exists
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  
  # Coerce time fields to character for stable CSV I/O
  log_rows <- log_rows %>%
    dplyr::mutate(
      ts_start = as.character(ts_start),
      ts_end   = as.character(ts_end),
      run_at   = as.character(run_at)
    )
  
  if (file.exists(log_path)) {
    existing <- utils::read.csv(log_path, stringsAsFactors = FALSE)
    log_out  <- dplyr::bind_rows(existing, log_rows)
  } else {
    log_out <- log_rows
  }
  
  utils::write.csv(log_out, log_path, row.names = FALSE)
  invisible(log_path)
}

# Internal: create a single QA/QC log row
make_qaqc_log_row <- function(timestamps,
                               station,
                               metric,
                               field,
                               action,
                               code,
                               action_note,
                               manual_note,
                               fun_name,
                               user,
                               run_time = Sys.time()) {
  # No affected timestamps: nothing to log for this code
  if (length(timestamps) == 0 || all(is.na(timestamps))) {
    return(NULL)
  }
  
  ts_start <- min(timestamps, na.rm = TRUE)
  ts_end   <- max(timestamps, na.rm = TRUE)
  
  duration_hours <- as.numeric(difftime(ts_end, ts_start, units = "hours"))
  
  tibble::tibble(
    station        = station,
    metric         = metric,
    field          = field,
    action         = action,
    code           = code,
    action_note    = action_note,
    manual_note    = manual_note,
    ts_start       = ts_start,
    ts_end         = ts_end,
    duration_hours = duration_hours,
    fun_name       = fun_name,
    run_at         = run_time,
    user           = user
  )
}
