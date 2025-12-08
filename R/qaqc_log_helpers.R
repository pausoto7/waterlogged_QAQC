#' Build path to a station+metric QA/QC log file
#'
#' Internal helper to construct the file path for a QA/QC log CSV based on
#' station code and metric type.
#'
#' @param log_root Root directory where logs are stored.
#' @param station Character; station code.
#' @param metric Character; metric type (e.g., "WL", "DO", "BARO", "COND").
#'
#' @return A character string representing the full path to the log file:
#'   \code{<log_root>/logs/<station>_<metric>_qaqc_log.csv}
#'
#' @keywords internal
#' @noRd
qaqc_log_path <- function(log_root, station, metric) {
  # logs live above the year folders, in a "logs" dir
  log_dir  <- file.path(log_root, "logs")
  log_file <- paste0(station, "_", metric, "_qaqc_log.csv")
  file.path(log_dir, log_file)
}

#' Append rows to a station+metric QA/QC log
#'
#' Internal helper to append new QA/QC log entries to an existing log file,
#' or create a new log file if it doesn't exist. Ensures the log directory
#' is created and timestamps are properly formatted for CSV storage.
#'
#' @param log_path Character; full path to the log file (typically from
#'   \code{qaqc_log_path()}).
#' @param station Character; station code (not currently used but passed for
#'   consistency).
#' @param metric Character; metric type (not currently used but passed for
#'   consistency).
#' @param log_rows Data frame of log rows to append, typically created by
#'   \code{make_qaqc_log_row()}.
#'
#' @return Invisibly returns the log path. Side effect: writes or appends
#'   to the CSV log file.
#'
#' @keywords internal
#' @noRd
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

#' Create a single QA/QC log row
#'
#' Internal helper to construct a standardized QA/QC log entry as a one-row
#' tibble. Used by all QA/QC functions to document what actions were taken,
#' when, and by whom.
#'
#' @param timestamps POSIXct vector of timestamps affected by the QA/QC action.
#'   If empty or all \code{NA}, returns \code{NULL} (nothing to log).
#' @param station Character; station code.
#' @param metric Character; metric type (e.g., "WL", "DO", "BARO", "COND").
#' @param field Character; name of the field/column that was modified.
#' @param action Character; short action code (e.g., "SPIKE", "DRY", "ICE").
#' @param code Character; QA/QC code (often same as \code{action}).
#' @param action_note Character; human-readable description of the action.
#' @param manual_note Character; additional user notes (e.g., "AUTOMATIC QC"
#'   or user-supplied reasoning).
#' @param fun_name Character; name of the function that performed the action.
#' @param user Character; username (typically from \code{Sys.info()[["user"]]}).
#' @param run_time POSIXct; timestamp when the function was run. Default is
#'   \code{Sys.time()}.
#'
#' @return A one-row tibble with columns: \code{station}, \code{metric},
#'   \code{field}, \code{action}, \code{code}, \code{action_note},
#'   \code{manual_note}, \code{ts_start}, \code{ts_end}, \code{duration_hours},
#'   \code{fun_name}, \code{run_at}, \code{user}. Returns \code{NULL} if no
#'   valid timestamps are provided.
#'
#' @keywords internal
#' @noRd
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
