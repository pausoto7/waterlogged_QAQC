
library(tidyverse)


source("R/step1_utils.R")

bind_hobo_files <- function(path_to_raw_folder, path_to_output_folder, metadata_path,  timestamp_timezone = "UTC") {
  
  # QAQC and format metadata file
  metadata <- QAQC_metadata(metadata_path)
  
  file_names <- list.files(path = path_to_raw_folder, pattern = "\\.csv$") # extract file names ending in csv
  path_to_files <- paste0(path_to_raw_folder, file_names) # makes string of full file path for all csv files
  
  hobo_data_raw <- purrr::map_df(path_to_files, extract_alldata_from_file)
  

  # Error messages for file binding
  if(length(unique(hobo_data_raw$data1_type))>1){
    warning("Folder contains different types of HOBO loggers.  Check that all files in input folder are of the same logger type (refer to 'file_summary').")
  }
  if(length(unique(hobo_data_raw$data1_unit))>1|length(unique(hobo_data_raw$data2_unit))>1){
    warning("Some files are in different units (refer to 'file_summary').")
  }
  if(length(unique(hobo_data_raw$timezone))>1){
    warning("Some files are in different timezones (refer to 'file_summary').")
  }
  
  ## TIMESTAMPS ####
  # format timestamp
  # NOTE: all times are in PDT (setting timezone to UTC tricks R to ignore daylight savings time)
  hobo_data_raw$timestamp <- lubridate::mdy_hms(hobo_data_raw$timestamp)
  
  if(any(is.na(hobo_data_raw$timestamp))) {
    missing_ts <- hobo_data_raw %>% filter(is.na(timestamp))
    missing_ts_sn <- unique(missing_ts$sn)
    
    warning(paste("NAs produced in timestamp for logger SN:",missing_ts_sn,". Make sure the timestamp column in csv is formatted as mm/dd/yy HH:MM:SS AM/PM (%m/%d/%y %I:%M:%S %p). Correct and try again."))
  }
  

  if(any(is.na(metadata$timestamp_deploy))) {
    missing_ts <- hobo_data_raw %>% filter(is.na(timestamp_deploy))
    missing_ts_sn <- unique(missing_ts$sn)
    
    warning(paste("NAs produced in timestamp_deploy for logger SN:",missing_ts_sn,". Make sure the timestamp column in csv is formatted the same as the input data and is noted correctly in timestamp_format. Correct and try again."))
  }
  
  # link sn to site_station_code in metadata
  metadat_link <- metadata %>% filter(sn %in% unique(hobo_data_raw$sn))
  
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
  
  measurement_type_raw <- unique(metadat_link$metric)
  
  
  #QAQC
  measurement_type <- qc_measurement_type(measurement_type_raw)
  
  
  
  #####
  # NEED TO ADD CODE FOR TIDBIT QAQC STILL, ALSO WHAT HAPPENS IF USER HAS MORE THAN 1 LOGGER TYPE?
  #####
  
  logger_type <- unique(metadata$model)
  
  # mutate a column for logger type and metric  
  sites_compiled <- sites_compiled %>%
    mutate(logger_type = logger_type, 
           metric = measurement_type)
  
  
  # change column names to remove spaces and symbols for data processing
  # if columns are in different units, then just keep original headers
  if(colnames(sites_compiled)[3]=="DO conc mg/L"){
    colnames(sites_compiled)[3]<- "do_mgl"
    logger_header <- "DO"
    
    if(colnames(sites_compiled)[4]=="Temp °C"){
      colnames(sites_compiled)[4]<- "watertemp_C"
    }
  }
  if(colnames(sites_compiled)[3]=="Abs Pres kPa" & measurement_type == "barometric"){
    colnames(sites_compiled)[3]<- "airpress_kPa"
    sites_compiled <- sites_compiled[complete.cases(sites_compiled$airpress_kPa),]
    
    logger_header <- "BARO"
    if(colnames(sites_compiled)[4]=="Temp °C"){
      colnames(sites_compiled)[4]<- "airtemp_C"
    }
  }
  if(colnames(sites_compiled)[3]=="Abs Pres kPa" & measurement_type == "waterlevel"){
    colnames(sites_compiled)[3]<- "waterpress_kPa"
    logger_header <- "WL"
    
    if(colnames(sites_compiled)[4]=="Temp °C"){
      colnames(sites_compiled)[4]<- "watertemp_C"
    }
  }
  if(colnames(sites_compiled)[3]=="Temp °C" & logger_type == "watertemp_tidbit"){
    logger_header <- "WT"
    colnames(sites_compiled)[3]<- "watertemp_C"
    
  }
  if(colnames(sites_compiled)[3]=="Temp °C" & logger_type == "airtemp_tidbiT"){
    logger_header <- "AT"
    colnames(sites_compiled)[3]<- "airtemp_C"
    
  }
  
  
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
  
  ## Graph Raw Data ####
  sites_compiled[,3] <- as.numeric(sites_compiled[,3])
  sites_compiled[,4] <- as.numeric(sites_compiled[,4])
  
  multi_plots <- ggplot(data=sites_compiled, aes(x = sites_compiled[,2], y = sites_compiled[,3])) +
    geom_line(linewidth = 1)+
    facet_wrap(~site_station_code, scales = "free")+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90))+
    scale_x_datetime(date_labels = "%y-%m")+
    labs(x = "Timestamp (YY-MM)", y = colnames(sites_compiled[3]))
  
  
  return(list(sites_compiled, multi_plots))
} # end of function

