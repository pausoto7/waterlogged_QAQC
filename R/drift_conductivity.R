# R/quality/wq_drift_correction.R

#' Water-quality drift correction (COND / DO / pH)
#'
#' Forward, linear drift between visits, restarting at each visit.
#' - Segment 0: from series start to first visit's end-sample.
#' - "check": end-target uses nearest logger sample at visit time; restart at that sample.
#' - "calibration": end-target uses last logger sample strictly before calibration_datetime;
#'    restart at first sample at/after calibration_datetime (post-cal is trusted).
#' - No segment after the last event.
#'
#' @export
#' @import dplyr
#' @importFrom lubridate parse_date_time ymd_hms
#' @importFrom utils read.csv
wq_drift_correction <- function(input_data,
                                ref_data_path,
                                select_station,
                                metric,
                                method        = c("ratio", "offset"),
                                max_ref_gap_h = 1,
                                log_root,
                                user = Sys.info()[["user"]]) {
  
  method <- match.arg(method)
  
  # ---- Map metric to columns
  metric_norm <- normalize_string(metric)
  if (metric_norm %in% c("cond", "conductivity")) {
    metric_norm    <- "conductivity"
    value_col      <- "conduct_uScm_adj"
    code_col       <- "cond_qaqc_code"
    note_col       <- "cond_qaqc_note"
    adjnote_col    <- "cond_qaqc_adj"
    edit_flag_col  <- "edit_cond_drift"
    accepted_units <- c("uscm","µs/cm","μs/cm","uS/cm")
    metric_log     <- "COND"
  } else if (metric_norm %in% c("do", "dissolvedoxygen")) {
    metric_norm    <- "dissolvedoxygen"
    value_col      <- "do_percsat_adj"
    code_col       <- "do_qaqc_code"
    note_col       <- "do_qaqc_note"
    adjnote_col    <- "do_qaqc_adj"
    edit_flag_col  <- "edit_do_drift"
    accepted_units <- c("%","percent","percent saturation","%sat","percsat")
    metric_log     <- "DO"
  } else if (metric_norm == "ph") {
    metric_norm    <- "ph"
    value_col      <- "ph_adj"
    code_col       <- "ph_qaqc_code"
    note_col       <- "ph_qaqc_note"
    adjnote_col    <- "ph_qaqc_adj"
    edit_flag_col  <- "edit_ph_drift"
    accepted_units <- c("ph","pH")
    metric_log     <- "PH"
  } else {
    stop("wq_drift_correction(): unsupported metric '", metric, "'.")
  }
  
  # ---- Logger data checks
  req_logger_cols <- c("site_station_code", "timestamp", value_col)
  missing_logger  <- setdiff(req_logger_cols, names(input_data))
  if (length(missing_logger) > 0) {
    stop("wq_drift_correction(): input_data missing: ", paste(missing_logger, collapse = ", "))
  }
  
  logger_data <- input_data %>%
    dplyr::filter(.data$site_station_code == select_station) %>%
    dplyr::arrange(.data$timestamp)
  if (nrow(logger_data) == 0) stop("wq_drift_correction(): no rows for station '", select_station, "'.")
  
  if (!inherits(logger_data$timestamp, "POSIXct")) {
    logger_data$timestamp <- lubridate::ymd_hms(logger_data$timestamp, tz = "UTC")
    if (any(is.na(logger_data$timestamp))) stop("wq_drift_correction(): bad logger timestamps.")
  }
  
  log_ts   <- logger_data$timestamp
  log_vals <- logger_data[[value_col]]
  time_range <- range(log_ts, na.rm = TRUE)
  
  # ---- Reference data
  if (!is.character(ref_data_path) || length(ref_data_path) != 1L) {
    stop("wq_drift_correction(): `ref_data_path` must be a single character path.")
  }
  if (!file.exists(ref_data_path)) stop("wq_drift_correction(): reference file not found: ", ref_data_path)
  
  ref_data <- utils::read.csv(ref_data_path, stringsAsFactors = FALSE, check.names = FALSE)
  
  req_ref_cols <- c("metric","site_station_code","timestamp","watertemp_C","value","unit",
                    "event_type","calibration_datetime")
  missing_ref <- setdiff(req_ref_cols, names(ref_data))
  if (length(missing_ref) > 0) {
    stop("wq_drift_correction(): reference file missing: ", paste(missing_ref, collapse = ", "))
  }
  
  ref_data$._row_id <- seq_len(nrow(ref_data))
  
  ref_data <- ref_data %>%
    dplyr::mutate(
      metric_norm_file = dplyr::case_when(
        normalize_string(.data$metric) %in% c("cond","conductivity")   ~ "conductivity",
        normalize_string(.data$metric) %in% c("do","dissolvedoxygen") ~ "dissolvedoxygen",
        normalize_string(.data$metric) %in% c("ph")                   ~ "ph",
        TRUE ~ NA_character_
      ),
      site_station_code = as.character(.data$site_station_code)
    ) %>%
    dplyr::filter(.data$metric_norm_file == metric_norm,
                  .data$site_station_code == select_station)
  
  if (nrow(ref_data) == 0) {
    stop("wq_drift_correction(): no refs for station '", select_station, "' & metric '", metric, "'.")
  }
  
  # ---- Parse ref times & QC
  parse_orders <- c("ymd HMS","ymd HM","mdy HMS","mdy HM","dmy HMS","dmy HM")
  ref_data$timestamp <- lubridate::parse_date_time(ref_data$timestamp, orders = parse_orders, tz = "UTC")
  ref_data$calibration_datetime[
    ref_data$calibration_datetime == "" | trimws(ref_data$calibration_datetime) == ""
  ] <- NA
  ref_data$calibration_datetime <- lubridate::parse_date_time(
    ref_data$calibration_datetime, orders = parse_orders, tz = "UTC"
  )
  if (any(is.na(ref_data$timestamp))) {
    bad_rows <- ref_data$._row_id[is.na(ref_data$timestamp)]
    stop("wq_drift_correction(): bad reference `timestamp` rows: ", paste(bad_rows, collapse = ", "))
  }
  
  ref_data$event_type <- tolower(trimws(ref_data$event_type))
  bad_event <- which(!ref_data$event_type %in% c("calibration","check","ignore") | is.na(ref_data$event_type))
  if (length(bad_event) > 0) {
    stop("wq_drift_correction(): invalid `event_type` on rows: ",
         paste(ref_data$._row_id[bad_event], collapse = ", "),
         " (allowed: calibration, check, ignore).")
  }
  
  cal_missing <- which(ref_data$event_type == "calibration" & is.na(ref_data$calibration_datetime))
  if (length(cal_missing) > 0) {
    stop("wq_drift_correction(): calibration rows require `calibration_datetime` on rows: ",
         paste(ref_data$._row_id[cal_missing], collapse = ", "))
  }
  
  noncal_with_cal <- which(ref_data$event_type != "calibration" & !is.na(ref_data$calibration_datetime))
  if (length(noncal_with_cal) > 0) {
    warning("wq_drift_correction(): `calibration_datetime` supplied on non-cal rows: ",
            paste(ref_data$._row_id[noncal_with_cal], collapse = ", "),
            "; ignored.")
    ref_data$calibration_datetime[noncal_with_cal] <- NA
  }
  
  ref_data$unit_clean <- tolower(trimws(ref_data$unit))
  unit_ok <- ref_data$unit_clean %in% tolower(accepted_units)
  if (!all(unit_ok)) {
    bad_rows <- ref_data$._row_id[!unit_ok]
    stop("wq_drift_correction(): unexpected `unit` for '", metric,
         "' on rows: ", paste(bad_rows, collapse = ", "),
         ". Accepted: ", paste(accepted_units, collapse = ", "), ".")
  }
  
  ref_data$value <- suppressWarnings(as.numeric(ref_data$value))
  bad_val <- which(is.na(ref_data$value))
  if (length(bad_val) > 0) {
    stop("wq_drift_correction(): non-numeric `value` on rows: ", paste(ref_data$._row_id[bad_val], collapse = ", "))
  }
  
  # Keep refs if timestamp OR calibration_datetime intersects window (±2h)
  win_start <- time_range[1] - 2*3600
  win_end   <- time_range[2] + 2*3600
  in_window <- function(t) !is.na(t) & t >= win_start & t <= win_end
  ref_data <- ref_data %>%
    dplyr::filter(in_window(.data$timestamp) | in_window(.data$calibration_datetime))
  
  if (nrow(ref_data) == 0) {
    stop("wq_drift_correction(): no reference measurements within logger window for '", select_station, "'.")
  }
  
  # ---- Build events
  max_gap_sec <- max_ref_gap_h * 3600
  nearest_idx <- function(target_time) {
    diffs <- abs(as.numeric(log_ts - target_time, units = "secs"))
    if (!length(diffs)) return(NA_integer_)
    which.min(diffs)
  }
  last_before_idx <- function(target_time) {
    idx <- which(log_ts < target_time)
    if (length(idx) == 0) NA_integer_ else tail(idx, 1L)
  }
  first_at_or_after_idx <- function(target_time) {
    idx <- which(log_ts >= target_time)
    if (length(idx) == 0) NA_integer_ else idx[1L]
  }
  
  events_list <- list()
  for (i in seq_len(nrow(ref_data))) {
    r  <- ref_data[i, ]
    et <- r$event_type
    if (et == "ignore") next
    
    if (et == "check") {
      j_end <- nearest_idx(r$timestamp)
      if (is.na(j_end)) next
      gap <- abs(as.numeric(log_ts[j_end] - r$timestamp, units = "secs"))
      if (!is.finite(gap) || gap > max_gap_sec) next
      logger_end <- log_vals[j_end]
      if (is.na(logger_end)) next
      
      ratio_end  <- if (logger_end == 0) NA_real_ else r$value / logger_end
      offset_end <- r$value - logger_end
      
      events_list[[length(events_list)+1L]] <- data.frame(
        event_end_time    = log_ts[j_end],
        event_type        = "check",
        ratio_end         = ratio_end,
        offset_end        = offset_end,
        anchor_start_time = log_ts[j_end],   # restart at check
        stringsAsFactors  = FALSE
      )
      
    } else if (et == "calibration") {
      cal_time <- r$calibration_datetime
      j_pre  <- last_before_idx(cal_time)        # strictly pre-cal
      j_post <- first_at_or_after_idx(cal_time)  # post-cal anchor
      if (is.na(j_pre) || is.na(j_post)) next
      
      gap <- as.numeric(cal_time - log_ts[j_pre], units = "secs")
      if (!is.finite(gap) || gap > max_gap_sec) next
      
      logger_end <- log_vals[j_pre]
      if (is.na(logger_end)) next
      
      ratio_end  <- if (logger_end == 0) NA_real_ else r$value / logger_end
      offset_end <- r$value - logger_end
      
      events_list[[length(events_list)+1L]] <- data.frame(
        event_end_time    = log_ts[j_pre],    # last pre-cal sample gets full correction
        event_type        = "calibration",
        ratio_end         = ratio_end,
        offset_end        = offset_end,
        anchor_start_time = log_ts[j_post],   # restart after cal
        stringsAsFactors  = FALSE
      )
    }
  }
  
  if (!length(events_list)) {
    message("wq_drift_correction(): no usable events; returning input unchanged.")
    return(input_data)
  }
  
  events <- dplyr::bind_rows(events_list) %>%
    dplyr::arrange(.data$event_end_time) %>%
    dplyr::distinct(.data$event_end_time, .keep_all = TRUE)
  
  # ---- Build forward segments
  segments <- list()
  # Segment 0: start -> first event end
  segments[[length(segments)+1L]] <- list(
    start_time = log_ts[1],
    end_time   = events$event_end_time[1],
    ratio_end  = events$ratio_end[1],
    offset_end = events$offset_end[1]
  )
  # Subsequent segments: restart at k -> end at k+1
  if (nrow(events) >= 2) {
    for (k in 1:(nrow(events)-1)) {
      segments[[length(segments)+1L]] <- list(
        start_time = events$anchor_start_time[k],
        end_time   = events$event_end_time[k+1],
        ratio_end  = events$ratio_end[k+1],
        offset_end = events$offset_end[k+1]
      )
    }
  }
  # No segment after last event.
  
  # ---- Apply linear drift per segment
  new_vals <- log_vals
  edited   <- rep(FALSE, length(log_vals))
  
  apply_full_target_at <- function(t_end, ratio_end, offset_end) {
    idx <- which(log_ts == t_end & !is.na(log_vals))
    if (!length(idx)) return()
    if (method == "ratio") {
      if (!is.na(ratio_end) && is.finite(ratio_end)) new_vals[idx] <<- log_vals[idx] * ratio_end
    } else {
      if (!is.na(offset_end) && is.finite(offset_end)) new_vals[idx] <<- log_vals[idx] + offset_end
    }
    edited[idx] <<- TRUE
  }
  
  for (seg in segments) {
    st <- seg$start_time
    et <- seg$end_time
    if (is.na(st) || is.na(et)) next
    
    # Degenerate/instant segment: only end sample gets full target
    if (et <= st) {
      apply_full_target_at(et, seg$ratio_end, seg$offset_end)
      next
    }
    
    idx <- which(log_ts > st & log_ts <= et & !is.na(log_vals))
    if (!length(idx)) next
    
    dur <- as.numeric(et - st, units = "secs")
    if (dur <= 0) {
      apply_full_target_at(et, seg$ratio_end, seg$offset_end)
      next
    }
    
    w <- as.numeric(log_ts[idx] - st, units = "secs") / dur
    w[w < 0] <- 0; w[w > 1] <- 1
    
    if (method == "ratio") {
      r_end <- seg$ratio_end
      if (!is.na(r_end) && is.finite(r_end)) {
        factor_t <- 1 + w * (r_end - 1)    # 1 → r_end
        new_vals[idx] <- log_vals[idx] * factor_t
        edited[idx] <- TRUE
      }
    } else {
      off_end <- seg$offset_end
      if (!is.na(off_end) && is.finite(off_end)) {
        offset_t <- w * off_end            # 0 → off_end
        new_vals[idx] <- log_vals[idx] + offset_t
        edited[idx] <- TRUE
      }
    }
  }
  
  # ---- Write back & QAQC fields
  logger_data[[value_col]] <- new_vals
  if (!edit_flag_col %in% names(logger_data)) logger_data[[edit_flag_col]] <- FALSE
  logger_data[[edit_flag_col]] <- logger_data[[edit_flag_col]] | edited
  
  # ensure columns exist in input_data
  new_cols <- setdiff(names(logger_data), names(input_data))
  if (length(new_cols) > 0) for (nm in new_cols) input_data[[nm]] <- NA
  
  input_data <- dplyr::rows_update(input_data, logger_data, by = c("site_station_code","timestamp"))
  edited_ts <- logger_data$timestamp[edited]
  
  append_qaqc <- function(existing, new_txt) {
    ifelse(is.na(existing) | existing == "", new_txt, paste(existing, new_txt, sep = "; "))
  }
  if (!code_col %in% names(input_data))     input_data[[code_col]] <- NA_character_
  if (!note_col %in% names(input_data))     input_data[[note_col]] <- NA_character_
  if (!adjnote_col %in% names(input_data))  input_data[[adjnote_col]] <- NA_character_
  
  idx_station <- input_data$site_station_code == select_station
  idx_edit    <- idx_station & input_data$timestamp %in% edited_ts
  drift_label <- paste0("DRIFT_", toupper(method))
  
  if (any(idx_edit)) {
    input_data[[code_col]][idx_edit] <- append_qaqc(input_data[[code_col]][idx_edit], drift_label)
    input_data[[note_col]][idx_edit] <- append_qaqc(
      input_data[[note_col]][idx_edit],
      paste0("Forward linear drift (", method, "); refs: ", basename(ref_data_path))
    )
    input_data[[adjnote_col]][idx_edit] <- append_qaqc(
      input_data[[adjnote_col]][idx_edit],
      paste0("Restart at each visit; calibration end uses pre-cal; max_ref_gap_h=", max_ref_gap_h)
    )
  }
  
  # QA/QC log (uses your existing helpers)
  if (any(idx_edit)) {
    ts_edit <- input_data$timestamp[idx_edit]
    log_row <- make_qaqc_log_row(
      timestamps  = ts_edit,
      station     = select_station,
      metric      = metric_log,
      field       = value_col,
      action      = "DRIFT_CORR",
      code        = drift_label,
      action_note = "Forward linear drift; restart at each visit; calibration end uses pre-cal.",
      manual_note = "AUTOMATIC DRIFT CORRECTION",
      fun_name    = "wq_drift_correction",
      user        = user
    )
    log_path <- qaqc_log_path(log_root, select_station, metric_log)
    qaqc_log_append(log_path = log_path, station = select_station, metric = metric_log, log_rows = log_row)
  } else {
    message("wq_drift_correction(): no values changed for '", select_station, "'.")
  }
  
  input_data
}
