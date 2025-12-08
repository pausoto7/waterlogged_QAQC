conductivity_qaqc_all <- function(
    cond_data_path,
    metadata_path,
    path_to_output_folder,
    log_root,
    temp_low_limit_C           = -2,
    temp_high_limit_C          = 40,
    disturbance_threshold_uScm = 200,
    dry_threshold_uScm         = 10,
    air_water_diff_threshold_C = 2,
    file_suffix                = "COND",  # for pattern: STATION_COND_*.csv
    user                       = Sys.info()[["user"]]
) {
  
  metadata <- QAQC_metadata(metadata_path)
  
  cond_sites <- metadata %>%
    dplyr::filter(metric == "conductivity") %>%
    dplyr::pull(site_station_code) %>%
    unique()
  
  if (!length(cond_sites)) {
    stop("conductivity_qaqc_all(): No conductivity stations found in metadata (metric == 'conductivity').")
  }
  
  all_cond_list <- list()
  
  for (stn in cond_sites) {
    message("\n--- Conductivity QA/QC for station: ", stn, " ---")
    
    pattern <- paste0("^", escape_regex(stn), "_", file_suffix, "_.*\\.csv$")
    file_paths <- list.files(
      cond_data_path,
      pattern    = pattern,
      full.names = TRUE,
      recursive  = TRUE
    )
    
    if (!length(file_paths)) {
      warning("  No ", file_suffix, " files found for station ", stn,
              " under ", cond_data_path, ". Skipping.")
      next
    }
    
    raw_cond <- dplyr::bind_rows(
      lapply(
        file_paths,
        utils::read.csv,
        stringsAsFactors = FALSE,
        check.names      = FALSE
      )
    )
    
    # Ensure timestamp column exists & is POSIXct
    if (!"timestamp" %in% names(raw_cond)) {
      ts_cand <- intersect(
        c("timestamp", "Timestamp", "Date Time", "datetime", "date_time"),
        names(raw_cond)
      )
      if (!length(ts_cand)) {
        warning("  No timestamp column found for station ", stn, ". Skipping.")
        next
      }
      raw_cond <- dplyr::rename(raw_cond, timestamp = !!rlang::sym(ts_cand[1]))
    }
    
    if (!inherits(raw_cond$timestamp, "POSIXct")) {
      raw_cond$timestamp <- lubridate::ymd_hms(raw_cond$timestamp, tz = "UTC")
    }
    
    # Ensure site_station_code column
    if (!"site_station_code" %in% names(raw_cond)) {
      raw_cond$site_station_code <- stn
    }
    
    # Ensure core cond columns are present with expected names
    req_cols <- c("conduct_uScm", "watertemp_C")
    missing_req <- setdiff(req_cols, names(raw_cond))
    if (length(missing_req) > 0) {
      warning("  Station ", stn,
              " is missing required column(s) for conductivity: ",
              paste(missing_req, collapse = ", "),
              ". Skipping.")
      next
    }
    
    # Run single-station QA/QC
    checked <- conductivity_qaqc(
      input_data               = raw_cond,
      select_station           = stn,
      log_root                 = log_root,
      user                     = user,
      temp_low_limit_C         = temp_low_limit_C,
      temp_high_limit_C        = temp_high_limit_C,
      disturbance_threshold_uScm = disturbance_threshold_uScm,
      dry_threshold_uScm       = dry_threshold_uScm,
      air_water_diff_threshold_C = air_water_diff_threshold_C
    )
    
    # Simple printed summary
    n_tot  <- nrow(checked)
    n_flag <- sum(!is.na(checked$cond_qaqc_code))
    n_spike <- sum(checked$cond_qaqc_code == "SPIKE", na.rm = TRUE)
    n_dry   <- sum(grepl("^DRY", checked$cond_qaqc_code), na.rm = TRUE)
    n_temp  <- sum(
      checked$cond_qaqc_code %in% c("TEMP_LOW", "TEMP_HIGH", "NEGATIVE_WT", "ICE"),
      na.rm = TRUE
    )
    
    message("  Total rows:       ", n_tot)
    message("  Any flags:        ", n_flag)
    message("    SPIKE:          ", n_spike)
    message("    DRY (all):      ", n_dry)
    message("    TEMP / ICE etc: ", n_temp)
    
    # Store for combined return
    all_cond_list[[stn]] <- checked
    
    # ---- Write yearly v0.1 COND files --------------------------------------
    checked_year <- checked %>%
      dplyr::mutate(year = lubridate::year(timestamp))
    
    years_i <- sort(unique(checked_year$year))
    
    for (yy in years_i) {
      sub <- checked_year %>%
        dplyr::filter(year == yy) %>%
        dplyr::arrange(timestamp)
      
      if (!nrow(sub)) next
      
      start_date <- min(lubridate::as_date(sub$timestamp))
      end_date   <- max(lubridate::as_date(sub$timestamp))
      
      start_str <- gsub("\\D", "", as.character(start_date))
      end_str   <- gsub("\\D", "", as.character(end_date))
      
      year_dir <- file.path(path_to_output_folder, yy)
      proc_dir <- file.path(year_dir, "processed")
      if (!dir.exists(year_dir)) dir.create(year_dir, recursive = TRUE)
      if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)
      
      sub_write <- sub %>%
        dplyr::select(-year)
      sub_write$timestamp <- format(sub_write$timestamp, tz = "UTC", usetz = FALSE)
      
      out_name <- sprintf("%s_%s_%s_%s_v0.1.csv",
                          stn, file_suffix, start_str, end_str)
      out_path <- file.path(proc_dir, out_name)
      
      utils::write.csv(sub_write, out_path, row.names = FALSE)
      message("  QA/QC ", file_suffix, " written: ", basename(out_path))
    }
  }
  
  # ---- Combined return ------------------------------------------------------
  if (!length(all_cond_list)) {
    warning("conductivity_qaqc_all(): No stations processed successfully; returning empty tibble.")
    return(tibble::tibble())
  }
  
  combined <- dplyr::bind_rows(all_cond_list)
  rownames(combined) <- NULL
  combined
}
