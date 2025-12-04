source("R/barometric_qaqc.R")

barometric_qaqc_all <- function(baro_data_path,
                                metadata_path,
                                path_to_output_folder = baro_data_path,
                                log_root             = path_to_output_folder,
                                user                 = Sys.info()[["user"]],
                                version_label        = "v0.1",
                                temp_low_limit       = -40,
                                temp_high_limit      = 50,
                                pressure_low_kpa     = 85,
                                pressure_high_kpa    = 105,
                                spike_threshold_kpa  = 1.5,
                                flatline_n           = 6) {
  
  # small helper (copied from add_nearest_baro)
  escape_regex <- function(x) gsub("([][{}()+*^$|\\.?\\\\-])", "\\\\\\1", x)
  
  # ---- 1. Read metadata and find barometric stations -------------------------
  metadata <- QAQC_metadata(metadata_path)
  
  baro_meta <- metadata %>%
    dplyr::filter(.data$metric == "barometric")
  
  baro_stations <- unique(baro_meta$site_station_code)
  
  if (!length(baro_stations)) {
    stop("barometric_qaqc_all(): No barometric stations found in metadata (metric == 'barometric').")
  }
  
  # Ensure output root exists
  if (!dir.exists(path_to_output_folder)) {
    dir.create(path_to_output_folder, recursive = TRUE)
  }
  
  processed_summary <- dplyr::tibble(
    site_station_code = character(),
    n_rows_written    = integer(),
    files_written     = character()
  )
  
  # ---- 2. Loop over each barometric station ---------------------------------
  for (stn in baro_stations) {
    
    message("\n--- Barometric QA/QC for station: ", stn, " ---")
    
    # Find all BARO files for this station
    baro_pattern <- paste0("^", escape_regex(stn), "_BARO_.*\\.csv$")
    
    baro_files <- list.files(
      path       = baro_data_path,
      pattern    = baro_pattern,
      full.names = TRUE,
      recursive  = TRUE
    )
    
    if (!length(baro_files)) {
      warning("barometric_qaqc_all(): No BARO files found for station ", stn,
              " under ", baro_data_path, ". Skipping.")
      next
    }
    
    # ---- 2a. Read and combine all files for this station --------------------
    baro_raw <- purrr::map_df(
      baro_files,
      ~ utils::read.csv(.x, stringsAsFactors = FALSE, check.names = FALSE)
    )
    
    # Ensure there is a site_station_code column
    if (!"site_station_code" %in% names(baro_raw)) {
      baro_raw$site_station_code <- stn
    }
    
    # Ensure timestamp column exists and is parsed
    if (!"timestamp" %in% names(baro_raw)) {
      ts_cand <- intersect(
        c("timestamp", "Timestamp", "Date Time", "datetime", "date_time"),
        names(baro_raw)
      )
      if (!length(ts_cand)) {
        warning("barometric_qaqc_all(): No timestamp column found for station ", stn,
                " in files:\n  ", paste(basename(baro_files), collapse = ", "))
        next
      }
      baro_raw <- dplyr::rename(baro_raw, timestamp = !!rlang::sym(ts_cand[1]))
    }
    
    baro_raw$timestamp <- lubridate::ymd_hms(baro_raw$timestamp, tz = "UTC")
    
    baro_raw <- baro_raw %>%
      dplyr::filter(.data$site_station_code == !!stn) %>%
      dplyr::arrange(.data$timestamp)
    
    if (!nrow(baro_raw)) {
      warning("barometric_qaqc_all(): After filtering, no rows remain for station ", stn, ". Skipping.")
      next
    }
    
    # ---- 2b. Run single-station barometric_qaqc -----------------------------
    baro_qc <- barometric_qaqc(
      input_data          = baro_raw,
      select_station      = stn,
      log_root            = log_root,
      user                = user,
      temp_low_limit      = temp_low_limit,
      temp_high_limit     = temp_high_limit,
      pressure_low_kpa    = pressure_low_kpa,
      pressure_high_kpa   = pressure_high_kpa,
      spike_threshold_kpa = spike_threshold_kpa,
      flatline_n          = flatline_n
    )
    
    if (!is.data.frame(baro_qc) || !nrow(baro_qc)) {
      warning("barometric_qaqc_all(): barometric_qaqc() returned no rows for station ", stn, ". Skipping write.")
      next
    }
    
    # Ensure timestamp is POSIXct
    if (!lubridate::is.POSIXct(baro_qc$timestamp)) {
      baro_qc$timestamp <- lubridate::ymd_hms(baro_qc$timestamp, tz = "UTC")
    }
    
    # ---- 2c. Split by year and write QC’d files -----------------------------
    years <- unique(lubridate::year(baro_qc$timestamp))
    years <- years[!is.na(years)]
    
    if (!length(years)) {
      warning("barometric_qaqc_all(): Could not determine year(s) from timestamps for station ", stn, ". Skipping.")
      next
    }
    
    files_written <- character()
    
    for (yy in years) {
      site_year <- baro_qc[lubridate::year(baro_qc$timestamp) == yy, , drop = FALSE]
      if (!nrow(site_year)) next
      
      year_dir <- file.path(path_to_output_folder, yy)
      proc_dir <- file.path(year_dir, "processed")
      if (!dir.exists(year_dir)) dir.create(year_dir, recursive = TRUE)
      if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)
      
      start_date <- suppressWarnings(min(lubridate::as_date(site_year$timestamp), na.rm = TRUE))
      end_date   <- suppressWarnings(max(lubridate::as_date(site_year$timestamp), na.rm = TRUE))
      
      start_j <- gsub("\\D", "", as.character(start_date))
      end_j   <- gsub("\\D", "", as.character(end_date))
      
      dat_write <- site_year
      dat_write$timestamp <- format(dat_write$timestamp, "%Y-%m-%d %H:%M:%S")
      
      file_out <- file.path(
        proc_dir,
        paste0(
          stn, "_BARO_",
          start_j, "_", end_j, "_",
          version_label, ".csv"
        )
      )
      
      utils::write.csv(dat_write, file_out, row.names = FALSE)
      
      message("Barometric QA/QC for ", stn,
              " written to ", yy,
              " (", basename(file_out), ")")
      
      files_written <- c(files_written, file_out)
    }
    
    processed_summary <- dplyr::bind_rows(
      processed_summary,
      dplyr::tibble(
        site_station_code = stn,
        n_rows_written    = nrow(baro_qc),
        files_written     = paste(basename(files_written), collapse = "; ")
      )
    )
  }
  
  invisible(processed_summary)
}
