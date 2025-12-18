#' QA/QC log helpers
#'
#' Internal helpers for writing station+metric QA/QC logs to disk.
#' Logs are appended (or created if missing) in `file.path(log_root, "logs")`.
#' Duplicate log entries are prevented when re-running QA/QC functions.
#'
#' @name qaqc_log_helpers
#' @keywords internal
NULL

#' Build path to a station+metric QA/QC log file
#'
#' Logs live in `file.path(log_root, "logs")` and are named:
#' `<station>_<metric>_qaqc_log.csv`.
#'
#' @param log_root Character. Root directory where QA/QC logs are stored.
#' @param station Character. Station code (e.g., "ALBR_ST_30").
#' @param metric Character. Metric label used in the log filename (e.g., "WL", "DO", "COND").
#'
#' @return Character scalar path to the log file.
#' @keywords internal
qaqc_log_path <- function(log_root, station, metric) {
  log_dir  <- file.path(log_root, "logs")
  log_file <- paste0(station, "_", metric, "_qaqc_log.csv")
  file.path(log_dir, log_file)
}

#' Create a single QA/QC log row
#'
#' Creates one summary log entry spanning the min/max of `timestamps`.
#' Intended for segment-style logging (not one-row-per-sample).
#'
#' @param timestamps POSIXct vector of affected timestamps (or a 2-length range).
#' @param station Character. Station code.
#' @param metric Character. Metric label (e.g., "WL", "DO", "COND").
#' @param field Character. Field/column name that was affected (e.g., "wl_elev_m").
#' @param action Character. High-level action label (e.g., "DATUM_SET", "DRIFT_CORR").
#' @param code Character. QA/QC code (e.g., "WL_DATUM_APPLIED").
#' @param action_note Character. Machine-readable-ish note describing what happened.
#' @param manual_note Character. User/analyst note, if applicable.
#' @param fun_name Character. Function name generating the entry.
#' @param user Character. Analyst identifier/name.
#' @param run_time POSIXct. Run time stamp. Defaults to `Sys.time()`.
#'
#' @return A one-row tibble, or NULL if `timestamps` is empty/all NA.
#' @keywords internal
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

#' Append QA/QC log rows to disk with deduplication
#'
#' Appends `log_rows` to the log at `log_path`, creating the file if needed.
#' Deduplicates entries so re-running the same QA/QC function does not
#' write repeated identical log rows.
#'
#' Deduplication key:
#' `station, metric, field, action, code, ts_start, ts_end, fun_name`.
#'
#' @param log_path Character. Output path (use `qaqc_log_path()`).
#' @param station Character. Station code.
#' @param metric Character. Metric label.
#' @param log_rows Data frame/tibble of log rows (from `make_qaqc_log_row()` or bind_rows).
#'
#' @return Invisibly returns `log_path`.
#' @keywords internal
qaqc_log_append <- function(log_path, station, metric, log_rows) {
  
  if (is.null(log_rows) || nrow(log_rows) == 0) {
    return(invisible(log_path))
  }
  
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
    
    log_out <- dplyr::bind_rows(existing, log_rows) %>%
      dplyr::distinct(
        station,
        metric,
        field,
        action,
        code,
        ts_start,
        ts_end,
        fun_name,
        .keep_all = TRUE
      )
  } else {
    log_out <- log_rows
  }
  
  utils::write.csv(log_out, log_path, row.names = FALSE)
  
  invisible(log_path)
}
