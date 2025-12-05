#' Run barometric QA/QC for all baro stations in metadata
#'
#' Reads all barometric logger files for stations listed in the metadata
#' (where `metric == "barometric"`), applies `barometric_qaqc()` to each
#' station, writes yearly QA/QC CSVs (v0.1), and returns a combined data
#' frame of all QA/QC'd barometric data.
#'
#' Files are expected to follow the naming pattern
#' `"<site_station_code>_BARO_YYYYMMDD_YYYYMMDD_v*.csv"` and live somewhere
#' under `baro_data_path`.
#'
#' @param baro_data_path Root directory containing processed barometric CSV
#'   files. All subdirectories are searched recursively for files matching
#'   `"<station>_BARO_*.csv"`, where `<station>` comes from the metadata.
#'   For example: `"data/testing/processed"`.
#' @param metadata_path Path to the metadata CSV used by [QAQC_metadata()],
#'   e.g. `"data/testing/raw/testing_metadata.csv"`. Must include
#'   `site_station_code`, `metric`, and barometric station definitions
#'   (`metric == "barometric"`).
#' @param path_to_output_folder Root directory where yearly QA/QC barometric
#'   files will be written. Year- and `processed`-subfolders are created as
#'   needed, e.g. `"data/testing/processed"`.
#' @param log_root Root folder where QA/QC logs are written by
#'   [barometric_qaqc()] and related functions, typically the same as
#'   `path_to_output_folder`, e.g. `"data/testing/processed"`.
#' @param temp_low_limit Lower bound (°C) for plausible barometric logger
#'   temperatures. Values outside this range may be flagged or removed.
#' @param temp_high_limit Upper bound (°C) for plausible barometric logger
#'   temperatures.
#' @param pressure_low_kpa Lower bound (kPa) for plausible barometric
#'   pressure.
#' @param pressure_high_kpa Upper bound (kPa) for plausible barometric
#'   pressure.
#' @param spike_threshold_kpa Threshold (kPa) for flagging sudden pressure
#'   spikes between consecutive timesteps.
#' @param flatline_n Integer; number of consecutive identical values used to
#'   flag potential flatlining in the pressure series.
#' @param user Character username written into the QA/QC log. Defaults to
#'   `Sys.info()[["user"]]`.
#'
#'
#' @return A single long-format data frame created by binding the QA/QC'd
#'    barometric data for all stations. If no stations are processed
#'   successfully, an empty tibble is returned and a warning is issued.
#'
#' @seealso [barometric_qaqc()], [QAQC_metadata()], [add_nearest_baro()]
#'
#' @import dplyr
#' @importFrom lubridate ymd_hms year as_date
#' @importFrom tibble tibble
#' @export
barometric_qaqc_all <- function(
    baro_data_path,
    metadata_path,
    path_to_output_folder,
    log_root,
    temp_low_limit     = -40,
    temp_high_limit    = 50,
    pressure_low_kpa   = 85,
    pressure_high_kpa  = 105,
    spike_threshold_kpa = 1.5,
    flatline_n          = 6,
    user = Sys.info()[["user"]]
) {
  # helper to escape regex specials
  escape_regex <- function(x) gsub("([][{}()+*^$|\\.?\\\\-])", "\\\\\\1", x)
  
  metadata <- QAQC_metadata(metadata_path)
  
  baro_sites <- metadata %>%
    dplyr::filter(metric == "barometric") %>%
    dplyr::pull(site_station_code) %>%
    unique()
  
  if (!length(baro_sites)) {
    stop("barometric_qaqc_all(): No barometric stations found in metadata (metric == 'barometric').")
  }
  
  all_baro_list <- list()
  
  for (stn in baro_sites) {
    message("\n--- Barometric QA/QC for station: ", stn, " ---")
    
    pattern <- paste0("^", escape_regex(stn), "_BARO_.*\\.csv$")
    file_paths <- list.files(
      baro_data_path,
      pattern    = pattern,
      full.names = TRUE,
      recursive  = TRUE
    )
    
    if (!length(file_paths)) {
      warning("  No BARO files found for station ", stn,
              " under ", baro_data_path, ". Skipping.")
      next
    }
    
    raw_baro <- dplyr::bind_rows(
      lapply(file_paths, utils::read.csv, stringsAsFactors = FALSE, check.names = FALSE)
    )
    
    # ensure timestamp
    if (!"timestamp" %in% names(raw_baro)) {
      ts_cand <- intersect(
        c("timestamp", "Timestamp", "Date Time", "datetime", "date_time"),
        names(raw_baro)
      )
      if (!length(ts_cand)) {
        warning("  No timestamp column found for station ", stn, ". Skipping.")
        next
      }
      raw_baro <- dplyr::rename(raw_baro, timestamp = !!rlang::sym(ts_cand[1]))
    }
    raw_baro$timestamp <- lubridate::ymd_hms(raw_baro$timestamp, tz = "UTC")
    
    # ensure site_station_code
    if (!"site_station_code" %in% names(raw_baro)) {
      raw_baro$site_station_code <- stn
    }
    
    # run single-station QA/QC (this keeps all original cols + *_adj + baro_qaqc_*)
    checked <- barometric_qaqc(
      input_data         = raw_baro,
      select_station     = stn,
      log_root           = log_root,
      user               = user,
      temp_low_limit     = temp_low_limit,
      temp_high_limit    = temp_high_limit,
      pressure_low_kpa   = pressure_low_kpa,
      pressure_high_kpa  = pressure_high_kpa,
      spike_threshold_kpa = spike_threshold_kpa,
      flatline_n          = flatline_n
    )
    
    # simple printed summary (not returned)
    n_tot   <- nrow(checked)
    n_flag  <- sum(!is.na(checked$baro_qaqc_code))
    n_spike <- sum(checked$baro_qaqc_code == "SPIKE", na.rm = TRUE)
    n_flat  <- sum(checked$baro_qaqc_code == "FLATLINE", na.rm = TRUE)
    n_press <- sum(checked$baro_qaqc_code == "PRESSURE_RANGE", na.rm = TRUE)
    
    message("  Total rows:      ", n_tot)
    message("  Any flags:       ", n_flag)
    message("    SPIKE:         ", n_spike)
    message("    FLATLINE:      ", n_flat)
    message("    PRESSURE_RANGE:", n_press)
    
    # store for combined return
    all_baro_list[[stn]] <- checked
    
    # ---- write yearly v0.1 BARO files (adjusted) ----
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
      
      out_name <- sprintf("%s_BARO_%s_%s_v0.1.csv", stn, start_str, end_str)
      out_path <- file.path(proc_dir, out_name)
      
      utils::write.csv(sub_write, out_path, row.names = FALSE)
      message("  QA/QC BARO written: ", basename(out_path))
    }
  }
  
  # ---- RETURN: combined QA/QC’d BARO data (bind_hobo_files-style long DF) ----
  if (!length(all_baro_list)) {
    warning("barometric_qaqc_all(): No stations processed successfully; returning empty tibble.")
    return(tibble::tibble())
  }
  
  combined <- dplyr::bind_rows(all_baro_list)
  rownames(combined) <- NULL
  combined
}
