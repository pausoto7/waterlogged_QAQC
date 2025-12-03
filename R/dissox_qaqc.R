dissox_qaqc <- function(input_data,
                        select_station,
                        log_root,
                        user = Sys.info()[["user"]]) {
  
  # ---- 0. Required columns ----
  required_cols <- c(
    "site_station_code",
    "timestamp",
    "do_mgl",
    "do_percsat",
    "watertemp_C",
    "airtemp_C"
  )
  
  missing_cols <- setdiff(required_cols, names(input_data))
  if (length(missing_cols) > 0) {
    stop(
      "dissox_qaqc(): input_data is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ---- 1. Filter to station ----
  output_data <- input_data %>%
    dplyr::filter(.data$site_station_code == !!select_station) %>%
    dplyr::arrange(.data$timestamp)
  
  rownames(output_data) <- NULL
  n <- nrow(output_data)
  if (n == 0) {
    warning("dissox_qaqc(): no rows for station ", select_station)
    return(output_data)
  }
  
  # ---- 2. Add internal semantic names + *_adj columns ----
  # Internal DO temp / baro temp names (do NOT require them on input)
  if (!"watertemp_C_do" %in% names(output_data)) {
    output_data$watertemp_C_do <- output_data$watertemp_C
  }
  if (!"airtemp_C_baro" %in% names(output_data)) {
    output_data$airtemp_C_baro <- output_data$airtemp_C
  }
  
  # Adjusted DO / temp columns
  if (!"do_mgl_adj" %in% names(output_data)) {
    output_data$do_mgl_adj <- output_data$do_mgl
  }
  if (!"do_percsat_adj" %in% names(output_data)) {
    output_data$do_percsat_adj <- output_data$do_percsat
  }
  if (!"watertemp_C_do_adj" %in% names(output_data)) {
    output_data$watertemp_C_do_adj <- output_data$watertemp_C_do
  }
  
  # QAQC meta columns (wide; kept for now)
  if (!"do_qaqc_code" %in% names(output_data)) {
    output_data$do_qaqc_code <- NA_character_
  }
  if (!"do_qaqc_adj" %in% names(output_data)) {
    output_data$do_qaqc_adj <- NA_character_
  }
  if (!"do_qaqc_note" %in% names(output_data)) {
    output_data$do_qaqc_note <- NA_character_
  }
  
  # ---- 3. Derived variables ----
  output_data$air_watertemp_diff <- output_data$airtemp_C_baro - output_data$watertemp_C_do
  
  # ---- 4. Row-wise QAQC (same logic as original) ----
  for (i in seq_len(n)) {
    
    # -------- DO QAQC --------
    if (!is.na(output_data$do_mgl[i])) {
      
      # logger error code for DO
      if (output_data$do_mgl[i] == -888.88) {
        output_data$do_qaqc_code[i]    <- "DO_ERROR"
        output_data$do_qaqc_adj[i]     <- "REMOVED"
        output_data$do_qaqc_note[i]    <- "Logger error code"
        output_data$do_mgl_adj[i]      <- NA
        output_data$do_percsat_adj[i]  <- NA
      }
      
      # logger error code for temp
      if (output_data$watertemp_C_do[i] == -888.88) {
        output_data$do_qaqc_code[i]       <- "TEMP_ERROR"
        output_data$do_qaqc_adj[i]        <- "REMOVED"
        output_data$do_qaqc_note[i]       <- "Logger error code"
        output_data$watertemp_C_do_adj[i] <- NA
      }
      
      # negative DO in [-1, 0)
      if (output_data$do_mgl[i] < 0 && output_data$do_mgl[i] >= -1) {
        output_data$do_mgl_adj[i]     <- 0
        output_data$do_percsat_adj[i] <- 0
        output_data$do_qaqc_code[i]   <- "NEGATIVE_DO"
        output_data$do_qaqc_adj[i]    <- "REPLACED"
        output_data$do_qaqc_note[i]   <- "DO within bounds of 0 to -1 corrected to 0"
      }
      
      # negative DO < -1
      if (output_data$do_mgl[i] < -1) {
        output_data$do_mgl_adj[i]     <- NA
        output_data$do_percsat_adj[i] <- NA
        output_data$do_qaqc_code[i]   <- "NEGATIVE_DO"
        output_data$do_qaqc_adj[i]    <- "REMOVED"
        output_data$do_qaqc_note[i]   <- "DO less than -1 removed"
      }
      
      # high DO > 21 mg/L
      if (output_data$do_mgl[i] > 21) {
        output_data$do_mgl_adj[i]     <- NA
        output_data$do_percsat_adj[i] <- NA
        output_data$do_qaqc_code[i]   <- "OUTLIER_DO"
        output_data$do_qaqc_adj[i]    <- "REMOVED"
        output_data$do_qaqc_note[i]   <- "DO greater than 21 mg/L removed"
      }
    }
    
    # -------- Temperature QAQC (DO temp) --------
    if (!is.na(output_data$watertemp_C_do[i])) {
      
      # flag ice
      if (output_data$watertemp_C_do[i] < 0.3) {
        output_data$do_qaqc_code[i] <- "FLAG_ICE"
      }
      
      # small negative temps in [-1, 0)
      if (output_data$watertemp_C_do[i] < 0 &&
          output_data$watertemp_C_do[i] > -1) {
        output_data$do_qaqc_code[i] <- "NEGATIVE_TEMP"
        output_data$do_qaqc_adj[i]  <- "REPLACED"
        output_data$do_qaqc_note[i] <- "Water temp within bounds of 0 to -1 corrected to 0"
      }
      
      # temps ≤ -1
      if (output_data$watertemp_C_do[i] <= -1) {
        output_data$watertemp_C_do_adj[i] <- NA
        output_data$do_qaqc_note[i]       <- "water temp less than or equal to -1 removed"
        output_data$do_qaqc_adj[i]        <- "REMOVED"
        output_data$do_qaqc_code[i]       <- "LOGGER_ICE"
      }
    }
  }
  
  # ---- 5. 12-point FLAG_DRY logic ----
  if (n >= 12) {
    for (j in 1:(n - 11)) {
      if (!is.na(output_data$do_mgl[j])) {
        window_diff <- output_data$air_watertemp_diff[j:(j + 11)]
        window_do   <- output_data$do_percsat[j:(j + 11)]
        
        if (any(!is.na(window_diff))) {
          if (max(abs(window_diff), na.rm = TRUE) <= 2 &&
              min(window_do, na.rm = TRUE) >= 100) {
            output_data$do_qaqc_code[j:(j + 11)] <- "FLAG_DRY"
          }
        }
      }
    }
  }
  
  # ---------------------------------------------------------------------------
  # 6. Build QA/QC log rows (auto log, no manual_note required)
  # ---------------------------------------------------------------------------
  metric    <- "DO"
  run_time  <- Sys.time()
  fun_name  <- "dissox_qaqc"
  log_rows_list <- list()
  
  # A) Events where we actually changed values (REMOVED / REPLACED)
  changed <- output_data %>%
    dplyr::filter(!is.na(.data$do_qaqc_adj))
  
  if (nrow(changed) > 0) {
    # distinguish DO-related vs TEMP-related codes for 'field'
    temp_codes <- c("TEMP_ERROR", "NEGATIVE_TEMP", "LOGGER_ICE")
    
    grp <- changed %>%
      dplyr::group_by(do_qaqc_code, do_qaqc_adj) %>%
      dplyr::summarise(
        ts_vec     = list(.data$timestamp),
        note_first = dplyr::first(.data$do_qaqc_note),
        .groups    = "drop"
      )
    
    for (k in seq_len(nrow(grp))) {
      code_k   <- grp$do_qaqc_code[k]
      adj_k    <- grp$do_qaqc_adj[k]      # "REMOVED" / "REPLACED"
      ts_vec_k <- grp$ts_vec[[k]]
      note_k   <- grp$note_first[k]
      
      field_k <- if (code_k %in% temp_codes) {
        "watertemp_C_do_adj"
      } else {
        "do_mgl_adj"
      }
      
      action_note_k <- if (!is.na(note_k) && nzchar(note_k)) {
        note_k
      } else {
        paste0("Automatic DO QAQC: ", code_k, " (", adj_k, ").")
      }
      
      log_rows_list[[length(log_rows_list) + 1L]] <- make_qaqc_log_row(
        timestamps  = ts_vec_k,
        station     = select_station,
        metric      = metric,
        field       = field_k,
        action      = adj_k,  # REMOVED / REPLACED
        code        = code_k,
        action_note = action_note_k,
        manual_note = "Automatic dissolved oxygen QA/QC; no manual note provided.",
        fun_name    = fun_name,
        user        = user,
        run_time    = run_time
      )
    }
  }
  
  # B) Flags only (no direct value change) – FLAG_ICE, FLAG_DRY
  flag_codes <- c("FLAG_ICE", "FLAG_DRY")
  flagged <- output_data %>%
    dplyr::filter(.data$do_qaqc_code %in% flag_codes)
  
  if (nrow(flagged) > 0) {
    grp_flag <- flagged %>%
      dplyr::group_by(do_qaqc_code) %>%
      dplyr::summarise(
        ts_vec     = list(.data$timestamp),
        note_first = dplyr::first(.data$do_qaqc_note),
        .groups    = "drop"
      )
    
    for (k in seq_len(nrow(grp_flag))) {
      code_k   <- grp_flag$do_qaqc_code[k]
      ts_vec_k <- grp_flag$ts_vec[[k]]
      note_k   <- grp_flag$note_first[k]
      
      field_k <- if (code_k == "FLAG_ICE") {
        "watertemp_C_do_adj"
      } else {
        "do_mgl_adj"
      }
      
      action_note_k <- if (!is.na(note_k) && nzchar(note_k)) {
        note_k
      } else {
        paste0("Automatic DO QAQC flag: ", code_k, ".")
      }
      
      log_rows_list[[length(log_rows_list) + 1L]] <- make_qaqc_log_row(
        timestamps  = ts_vec_k,
        station     = select_station,
        metric      = metric,
        field       = field_k,
        action      = "FLAG",
        code        = code_k,
        action_note = action_note_k,
        manual_note = "Automatic dissolved oxygen QA/QC; no manual note provided.",
        fun_name    = fun_name,
        user        = user,
        run_time    = run_time
      )
    }
  }
  
  # Bind and write log if we actually have any rows
  log_rows <- log_rows_list[!vapply(log_rows_list, is.null, logical(1))]
  if (length(log_rows) > 0) {
    log_rows_df <- dplyr::bind_rows(log_rows)
    
    log_path <- qaqc_log_path(log_root, select_station, metric)
    
    qaqc_log_append(
      log_path = log_path,
      station  = select_station,
      metric   = metric,
      log_rows = log_rows_df
    )
    
    message("QA/QC log updated: ", log_path)
  }
  
  return(output_data)
}
