# Internal: append rows to a station+metric log (create if missing)
qaqc_log_append <- function(log_path, station, metric, log_rows) {
  
  if (is.null(log_rows) || !is.data.frame(log_rows) || nrow(log_rows) == 0) {
    return(invisible(log_path))
  }
  
  # Ensure log directory exists
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  
  # Coerce time fields to character for stable CSV I/O (use stable format)
  log_rows <- log_rows %>%
    dplyr::mutate(
      ts_start = as.character(ts_start),
      ts_end   = as.character(ts_end),
      run_at   = format(as.POSIXct(run_at, tz = "UTC"), "%Y-%m-%dT%H:%M:%S")
    )
  
  # Create a stable dedupe key for new rows (does NOT need to be written)
  key_new <- paste(
    log_rows$station,
    log_rows$metric,
    log_rows$field,
    log_rows$action,
    log_rows$code,
    log_rows$fun_name,
    log_rows$ts_start,
    log_rows$ts_end,
    sep = "|"
  )
  
  if (file.exists(log_path)) {
    existing <- utils::read.csv(log_path, stringsAsFactors = FALSE, check.names = FALSE)
    
    # Back-compat: if existing has no necessary cols, fall back to append
    need <- c("station","metric","field","action","code","fun_name","ts_start","ts_end")
    if (!all(need %in% names(existing))) {
      log_out <- dplyr::bind_rows(existing, log_rows)
      utils::write.csv(log_out, log_path, row.names = FALSE)
      return(invisible(log_path))
    }
    
    existing <- existing %>%
      dplyr::mutate(
        ts_start = as.character(ts_start),
        ts_end   = as.character(ts_end)
      )
    
    key_old <- paste(
      existing$station,
      existing$metric,
      existing$field,
      existing$action,
      existing$code,
      existing$fun_name,
      existing$ts_start,
      existing$ts_end,
      sep = "|"
    )
    
    keep <- !key_new %in% key_old
    log_rows <- log_rows[keep, , drop = FALSE]
    
    # nothing new to add
    if (nrow(log_rows) == 0) {
      return(invisible(log_path))
    }
    
    log_out <- dplyr::bind_rows(existing, log_rows)
  } else {
    log_out <- log_rows
  }
  
  utils::write.csv(log_out, log_path, row.names = FALSE)
  invisible(log_path)
}
