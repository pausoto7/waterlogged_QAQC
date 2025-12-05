#' Bind HOBO logger CSVs and attach site metadata
#'
#' Reads all HOBO CSV files in a folder, links each logger serial number to
#' deployment metadata, trims records to deployment/removal windows, standardises
#' core column names by metric (barometric, waterlevel, DO, TidbiT temperature,
#' conductivity), writes per-station/year v0.1 CSVs, and returns a compiled
#' long data frame plus a quick QC plot.
#'
#' @param path_to_raw_folder Path to the folder containing raw HOBO CSV files
#'   (one or more loggers, all of the same metric). For example
#'   `"data/raw/level"` or `"data/testing/raw/COND"`.
#' @param path_to_output_folder Root folder where processed v0.1 CSVs will be
#'   written. Files are created under `<path_to_output_folder>/<year>/processed`,
#'   e.g. `"data/processed"`.
#' @param metadata_path Path to a metadata CSV that has at least logger serial
#'   numbers and deployment/removal information (e.g. `sn`, `metric`,
#'   `site_station_code`, `timestamp_deploy`, `timestamp_remove`, `model`).
#' @param timestamp_timezone Time zone used when filling in open-ended
#'   deployments (where `timestamp_remove` is `NA`). Defaults to `"UTC"`.
#'
#' @details
#' The function:
#'
#' * Reads all `*.csv` files in `path_to_raw_folder` using
#'   [extract_alldata_from_file()] and binds them into a single data frame.
#' * Uses [QAQC_metadata()] to clean/standardise the metadata file.
#' * Assigns `site_station_code` to each record based on `sn` and the
#'   deployment/removal timestamps.
#' * Enforces a single `metric` per folder (e.g., all barometric, all
#'   waterlevel, all conductivity).
#' * Standardises core column names depending on logger/metric, e.g.
#'   `airpress_kPa`, `waterpress_kPa`, `watertemp_C`, `airtemp_C`,
#'   `do_mgl`, `conduct_uScm`, and `conduct_range_used`.
#' * For conductivity (U24) loggers, unifies low/full range columns into a
#'   single `conduct_uScm` column and records which range was used.
#' * Writes one v0.1 CSV per station per year to
#'   `<path_to_output_folder>/<year>/processed` with filenames of the form
#'   `<site>_<LOGGER>_<startdate>_<enddate>_v0.1.csv`.
#'
#' @return A list of length 2:
#' \describe{
#'   \item{[[1]]}{A data frame of compiled logger data (`sites_compiled`),
#'   with `site_station_code`, `sn`, `timestamp`, `logger_type`, `metric`, and
#'   metric-specific value columns.}
#'   \item{[[2]]}{A `ggplot` object showing the primary value column vs time,
#'   faceted by `site_station_code`, for quick visual QA/QC.}
#' }
#'
#' @seealso [QAQC_metadata()], [extract_alldata_from_file()]
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom purrr map_df
#' @importFrom lubridate mdy_hms now year date
#' @export
bind_hobo_files <- function(path_to_raw_folder, path_to_output_folder, metadata_path,  timestamp_timezone = "UTC") {
  
  # QAQC and format metadata file
  metadata <- QAQC_metadata(metadata_path)
  
  path_to_files <- list.files(
    path    = path_to_raw_folder,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  # extract data from hobo file(s)
  hobo_data_raw <- purrr::map_df(path_to_files, extract_alldata_from_file)
  
  
  # Error messages for file binding
  if (length(unique(hobo_data_raw$data1_type)) > 1) {
    warning(
      "Folder contains different types of HOBO loggers. ",
      "Check that all files in input folder are of the same logger type ",
      "(refer to 'file_summary')."
    )
  }
  
  # Safely check units for channel 1 and (if present) channel 2
  units1_mixed <- length(unique(hobo_data_raw$data1_unit)) > 1
  
  units2_mixed <- "dat2_unit" %in% names(hobo_data_raw) &&
    length(unique(hobo_data_raw$dat2_unit)) > 1
  
  is_conductivity_folder <- any(grepl("conduct", tolower(hobo_data_raw$data1_type)))
  
  if (units1_mixed || units2_mixed) {
    if (is_conductivity_folder) {
      warning(
        "Conductivity files have different HOBO channel layouts (e.g., Low Range vs Full Range). ",
        "All channels are in µS/cm. bind_hobo_files() will merge them into a single ",
        "`conduct_uScm` column and record the source in `conduct_source`."
      )
    } else {
      warning("Some files are in different units (refer to 'file_summary').")
    }
  }
  
  
  # format timestamp
  # NOTE: all times are in PDT (setting timezone to UTC tricks R to ignore daylight savings time)
  hobo_data_raw$timestamp <- lubridate::mdy_hms(hobo_data_raw$timestamp)
  
  if(any(is.na(hobo_data_raw$timestamp))) {
    missing_ts <- hobo_data_raw %>% filter(is.na(timestamp))
    missing_ts_sn <- unique(missing_ts$sn)
    
    warning(paste("NAs produced in timestamp for logger SN:",missing_ts_sn,". Make sure the timestamp column in csv is formatted as mm/dd/yy HH:MM:SS AM/PM (%m/%d/%y %I:%M:%S %p). Correct and try again."))
  }
  
  
  if(any(is.na(metadata$timestamp_deploy))) {
    missing_ts <- metadata %>% 
      dplyr::filter(is.na(timestamp_deploy))
    
    missing_ts_sn <- unique(missing_ts$sn)
    
    warning(paste("NAs produced in timestamp_deploy for logger SN:",missing_ts_sn,". Make sure the timestamp column in csv is formatted the same as the input data and is noted correctly in timestamp_format. Correct and try again."))
  }
  
  # link sn to site_station_code in metadata
  metadat_link <- metadata %>% filter(sn %in% unique(hobo_data_raw$sn))
  
  measurement_type <- unique(metadat_link$metric)
  
  # make sure it's only one type per folder
  if (length(measurement_type) != 1) {
    stop("Multiple metric types found for these loggers: ",
         paste(measurement_type, collapse = ", "),
         "\nMake sure your input folder only contains one metric type (e.g., all barometric or all waterlevel).")
  }
  
  # make dummy columns and datasets to fill in
  hobo_data_raw$site_station_code <- NA
  data_i <- data.frame()
  sites_compiled <- data.frame()
  
  uniq_sn_list <- unique(hobo_data_raw$sn)
  
  for(serial_num in 1:length(uniq_sn_list)){
    logger_i <- uniq_sn_list[serial_num]
    data_i <- hobo_data_raw %>% filter(sn ==logger_i)
    
    # get site codes for each sn
    metadat_i <- metadat_link %>% filter(sn==logger_i)
    
    # if no metadat for that logger produce error message
    if(length(metadat_i$sn)==0){
      stop(paste(c("Metadata missing for logger serial number:", logger_i)))
    }
    
    for(metadata_sn_j in 1:length(metadat_i$sn)){
      site_j <- metadat_i$site_station_code[metadata_sn_j]
      deploy_j <- metadat_i$timestamp_deploy[metadata_sn_j]
      remove_j<- metadat_i$timestamp_remove[metadata_sn_j]
      
      # if logger is still logging it wont have a remove date so make it today
      if(is.na(remove_j)){remove_j <- lubridate::now(tzone=timestamp_timezone)}
      
      # assign location based on matching timestamps in metadata
      data_i$site_station_code[data_i$timestamp >= deploy_j & data_i$timestamp <= remove_j] <- site_j
      
    } # end of j loop
    
    # trim data to within install/removal at each site
    data_i <- data_i %>% drop_na(site_station_code)
    sites_compiled <- rbind(sites_compiled, data_i)
    
  } # end of i loop
  
  ## CLEAN UP ####
  
  # remove duplicate rows in case multiple downloads per logger results in overlapping dates
  sites_compiled <- sites_compiled %>%
    dplyr::distinct(site_station_code, sn, timestamp, .keep_all = TRUE)
  
  # order by site and timestamp
  sites_compiled<- sites_compiled[order(sites_compiled$site_station_code, sites_compiled$timestamp), ]
  
  
  
  #####
  # NEED TO ADD CODE FOR TIDBIT QAQC STILL, ALSO WHAT HAPPENS IF USER HAS MORE THAN 1 LOGGER TYPE?
  #####
  
  logger_type <- unique(metadat_link$model)
  if (length(logger_type) != 1) {
    warning("Multiple logger models found for these files: ",
            paste(logger_type, collapse = ", "),
            ". Using first.")
  }
  
  logger_type <- logger_type[[1]] 
  
  
  # mutate a column for logger type and metric  
  sites_compiled <- sites_compiled %>%
    mutate(logger_type = logger_type, 
           metric = measurement_type)
  
  
  # setting this sa default. That way if logger_header is not assigned in anything below we can create an error.
  logger_header <- NA_character_ 
  

  # DO LOGGER -----------------------------------------------------------
  if (colnames(sites_compiled)[3] == "DO conc mg/L") {
    colnames(sites_compiled)[3] <- "do_mgl"
    logger_header <- "DO"
    
    # Temp handling
    if (ncol(sites_compiled) >= 4) {
      if (colnames(sites_compiled)[4] == "Temp °C") {
        colnames(sites_compiled)[4] <- "watertemp_C"
      }
      if (colnames(sites_compiled)[4] == "Temp °F") {
        sites_compiled[[4]] <- convert_F_to_C(sites_compiled[[4]])
        colnames(sites_compiled)[4] <- "watertemp_C"
      }
    }
  }
  
  # BAROMETRIC LOGGER ---------------------------------------------------
  if (colnames(sites_compiled)[3] == "Abs Pres kPa" & measurement_type == "barometric") {
    colnames(sites_compiled)[3] <- "airpress_kPa"
    sites_compiled <- sites_compiled[complete.cases(sites_compiled$airpress_kPa),]
    logger_header <- "BARO"
    
    if (ncol(sites_compiled) >= 4) {
      if (colnames(sites_compiled)[4] == "Temp °C") {
        colnames(sites_compiled)[4] <- "airtemp_C"
      }
      if (colnames(sites_compiled)[4] == "Temp °F") {
        sites_compiled[[4]] <- convert_F_to_C(sites_compiled[[4]])
        colnames(sites_compiled)[4] <- "airtemp_C"
      }
    }
  }
  
  # WATER LEVEL LOGGER --------------------------------------------------
  if (colnames(sites_compiled)[3] == "Abs Pres kPa" & measurement_type == "waterlevel") {
    colnames(sites_compiled)[3] <- "waterpress_kPa"
    logger_header <- "WL"
    
    if (ncol(sites_compiled) >= 4) {
      if (colnames(sites_compiled)[4] == "Temp °C") {
        colnames(sites_compiled)[4] <- "watertemp_C"
      }
      if (colnames(sites_compiled)[4] == "Temp °F") {
        sites_compiled[[4]] <- convert_F_to_C(sites_compiled[[4]])
        colnames(sites_compiled)[4] <- "watertemp_C"
      }
    }
  }
  
  # TIDBIT WATER TEMP LOGGER -------------------------------------------
  if (colnames(sites_compiled)[3] == "Temp °C" & logger_type == "tidbit") {
    logger_header <- "WT"
    colnames(sites_compiled)[3] <- "watertemp_C"
  }
  
  if (colnames(sites_compiled)[3] == "Temp °F" & logger_type == "tidbit") {
    logger_header <- "WT"
    sites_compiled[[3]] <- convert_F_to_C(sites_compiled[[3]])
    colnames(sites_compiled)[3] <- "watertemp_C"
  }
  
  # TIDBIT AIR TEMP LOGGER ---------------------------------------------
  if (colnames(sites_compiled)[3] == "Temp °C" & logger_type == "tidbit") {
    logger_header <- "AT"
    colnames(sites_compiled)[3] <- "airtemp_C"
  }
  
  if (colnames(sites_compiled)[3] == "Temp °F" & logger_type == "tidbit") {
    logger_header <- "AT"
    sites_compiled[[3]] <- convert_F_to_C(sites_compiled[[3]])
    colnames(sites_compiled)[3] <- "airtemp_C"
  }
  
  # CONDUCTIVITY LOGGER U24 ---------------------------------------------
  # Only run this block if the folder metric is conductivity
  
  # CONDUCTIVITY LOGGER U24 ---------------------------------------------
  # CONDUCTIVITY LOGGER U24 ---------------------------------------------
  if (measurement_type == "conductivity") {
    
    nm <- names(sites_compiled)
    
    # Much looser patterns: just look for "Low Range" / "Full Range" and "S/cm"
    low_col  <- nm[grepl("Low Range",  nm, ignore.case = TRUE) &
                     grepl("S/cm",       nm, ignore.case = TRUE)]
    full_col <- nm[grepl("Full Range", nm, ignore.case = TRUE) &
                     grepl("S/cm",       nm, ignore.case = TRUE)]
    
    # Temp column: "Temp °C" or "Temp, °C", any variant with C
    temp_col <- nm[grepl("^Temp", nm, ignore.case = TRUE) &
                     grepl("C",     nm, ignore.case = TRUE)]
    
    if (length(low_col)  > 1L) stop("Multiple 'Low Range' columns found.")
    if (length(full_col) > 1L) stop("Multiple 'Full Range' columns found.")
    if (length(temp_col) > 1L) stop("Multiple temperature (°C) columns found.")
    
    low_col  <- if (length(low_col))  low_col[[1]]  else NA_character_
    full_col <- if (length(full_col)) full_col[[1]] else NA_character_
    temp_col <- if (length(temp_col)) temp_col[[1]] else NA_character_
    
    # Extract numeric vectors safely (avoid .data[[NA]])
    n <- nrow(sites_compiled)
    
    low_vals  <- if (!is.na(low_col))  suppressWarnings(as.numeric(sites_compiled[[low_col]]))  else rep(NA_real_, n)
    full_vals <- if (!is.na(full_col)) suppressWarnings(as.numeric(sites_compiled[[full_col]])) else rep(NA_real_, n)
    temp_vals <- if (!is.na(temp_col)) suppressWarnings(as.numeric(sites_compiled[[temp_col]])) else rep(NA_real_, n)
    
    # HOBO U24 low-range ~0–1000 µS/cm
    low_max <- 1000
    
    sites_compiled <- sites_compiled %>%
      dplyr::mutate(
        conduct_uScm = dplyr::case_when(
          !is.na(low_vals)  & low_vals <= low_max ~ low_vals,   # prefer low range when valid & in range
          !is.na(full_vals)                      ~ full_vals,   # else full range
          !is.na(low_vals)                       ~ low_vals,    # fallback to low
          TRUE                                   ~ NA_real_
        ),
        conduct_source = dplyr::case_when(
          !is.na(low_vals)  & low_vals <= low_max ~ "low",
          !is.na(full_vals)                      ~ "full",
          !is.na(low_vals)                       ~ "low",
          TRUE                                   ~ NA_character_
        ),
        watertemp_C = temp_vals
      )
    
    logger_header <- "COND"
    
    # Standardise metadata so "data1" = cond, "data2" = temp for ALL cond files
    sites_compiled$data1_type <- "conductivity"
    sites_compiled$data1_unit <- "µS/cm"
    sites_compiled$data2_type <- "Temp"
    sites_compiled$dat2_unit  <- "°C"
  }
  
  
  # If logger_header is not succesfully reassigned above
  if (is.na(logger_header)) {
    stop("Could not determine logger_header from column names and measurement_type.")
  }
  
  # Remove HOBO event columns we don't need
  sites_compiled <- sites_compiled %>%
    dplyr::select(
      -dplyr::matches("Coupler Detached|Coupler Attached|Stopped|End Of File")
    )
  
  #### EXPORT CSV BY LOGGER TYPE LOCATION AND YEAR
  # some stations may have multiple logger files, one from a download in spring and another for a download in fall
  # this loop will split data by type, location and year for ease of access and processing
  
  for(i in 1:length(unique(sites_compiled$site_station_code))) {
    
    site_i <- unique(sites_compiled$site_station_code)[i]
    site_dat <- sites_compiled %>% filter(site_station_code==site_i)
    
    # split by year
    years_i <- unique(lubridate::year(site_dat$timestamp))
    
    for (j in seq_along(years_i)) {
      year_j <- years_i[j]
      
      site_year_j <- site_dat %>% 
        dplyr::filter(lubridate::year(timestamp) == year_j)
      
      # skip if no data for this year
      if (!nrow(site_year_j)) next
      
      # name by timestamp range
      start_j <- gsub("\\D", "", as.character(min(lubridate::date(site_year_j$timestamp))))
      end_j   <- gsub("\\D", "", as.character(max(lubridate::date(site_year_j$timestamp))))
      
      # create paths for output folders
      year_dir <- file.path(path_to_output_folder, year_j)
      proc_dir <- file.path(year_dir, "processed")
      
      # Create them recursively (no warnings if they already exist)
      dir.create(year_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
      
      # correct missing midnight timestamps & put into character so no formatting issues arise when in Excel
      site_year_j$timestamp <- format(site_year_j$timestamp, "%Y-%m-%d %H:%M:%S")
      
      # build output path using file.path for consistency
      out_file <- sprintf("%s_%s_%s_%s_v0.1.csv", site_i, logger_header, start_j, end_j)
      out_path <- file.path(proc_dir, out_file)
      
      utils::write.csv(site_year_j, out_path, row.names = FALSE)
      
      message(sprintf("Data (v0.1) from %s added to %s/processed", site_i, year_j))
      
      
    } #end of year level loop
    
  } # end of site level loop
  
  print("List returned: [1] trimmed and compiled dataset of csvs in raw folder \n [2] summary plots of data by site_station_code and timestamp")
  
  # Make sure the numeric columns are actually numeric
  sites_compiled <- sites_compiled %>%
    dplyr::mutate(
      dplyr::across(3:4, as.numeric)
    )
  
  # Grab the names explicitly instead of relying on position
  time_col  <- names(sites_compiled)[2]
  value_col <- names(sites_compiled)[3]
  
  multi_plots <- ggplot(
    data = sites_compiled,
    aes(x = .data[[time_col]], y = .data[[value_col]])
  ) +
    geom_line(linewidth = 1) +
    facet_wrap(~ site_station_code, scales = "free") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_x_datetime(date_labels = "%y-%m") +
    labs(
      x = "Timestamp (YY-MM)",
      y = value_col
    )
  
  return(list(
    sites_compiled, 
    multi_plots
  ))
  
} 

