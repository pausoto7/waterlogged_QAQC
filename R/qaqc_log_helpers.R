

# Internal: build path to a station+metric log file
qaqc_log_path <- function(log_root, station, metric) {
  # logs live above the year folders, in a "logs" dir
  log_dir  <- file.path(log_root, "logs")
  log_file <- paste0(station, "_", metric, "_qaqc_log.csv")
  file.path(log_dir, log_file)
}

# Internal: append rows to a station+metric log (create if missing)
qaqc_log_append <- function(log_root, station, metric, log_rows) {
  log_path <- .qaqc_log_path(log_root, station, metric)
  
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
