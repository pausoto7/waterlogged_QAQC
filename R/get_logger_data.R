#' Retrieve data
#'
#' Retrieve processed data by selecting desired variable, waterbody type, station, clean or raw, and temporal scale
#' @param path_to_data path to location of desired files encased in quotations
#' @param select_station unique site_station_code
#' @param logger_type either dissolvedoxygen_U26, waterlevel_U20, or baro_U20
#' @param data_processing either raw or clean, clean data has been processed via QAQC protocols, raw data should not be summarized ie. set temporal_scale to hourly
#' @param temporal_scale either hourly, daily or monthly, daily and monthly return mean values of selected variable
#' @param timestamp_format Formatting of timestamp column of input data, following POSTIX convention. Default is "%Y-%m-%d %H:%M:%S"
#' @param timestamp_timezone Timezone of timestamp column of input data, following POSTIX convention. Default is UTC to force ignore daylight savings time.
#' @return dataframe
#' @export
#' @import dplyr
#' @import ggplot2
#' @import lubridate
#' @import purrr

get_logger_data <- function(path_to_data, logger_type, timestamp_format = "%Y-%m-%d %H:%M:%S", timestamp_timezone = "UTC", data_processing = "clean", select_station = "all", temporal_scale = "hourly") {
  
  #make sure all paths have trailing slash
  if (!endsWith(path_to_data, "/")) {
    path_to_data <- paste0(path_to_data, "/")
  }
  
  # input errors
  if(logger_type != "baro_U20" & logger_type != "waterlevel_U20"& logger_type != "dissolvedoxygen_U26" & logger_type != "airtemp_TidbiT" & logger_type != "watertemp_TidbiT") {
    print("Uh oh! logger_type not recognized. Must be either dissolvedoxygen_U26, waterlevel_U20, baro_U20, airtemp_TidbiT or watertemp_TidbiT.")
  }
  
  # raw data
  if(data_processing=="raw") {
    file_names <- list.files(path = path_to_data, pattern = "\\.csv$") # extract file names ending in csv
  }
  # the processed folder contains output of several stages of data processed.
  # For processed data that is the output of bind_hobo_files
  # extract ONLY file names ending in "v0.1.csv"
  if(data_processing=="v0.1") {
    file_names <- list.files(path = path_to_data, pattern = "v0.1.csv$")
  }
  # For processed data that is the output of add_nearest_baro
  # extract ONLY file names ending in "v0.2.csv"
  if(data_processing=="v0.2") {
    file_names <- list.files(path = path_to_data, pattern = "v0.2.csv$")
  }
  # For processed data that is the output of convert_waterlevel_m or convert_dissox_mgl_percsat
  # extract ONLY file names ending in "v0.3.csv"
  if(data_processing=="v0.3") {
    file_names <- list.files(path = path_to_data, pattern = "v0.3.csv$")
  }
  
  # For processed data that is the output of write_clean_data
  # extract ONLY file names ending in "v1.0.csv"
  if(data_processing=="clean"|data_processing=="v1.0") {
    file_names <- list.files(path = path_to_data, pattern = "v1.0.csv$")
  }
  
  path_to_files <- paste0(path_to_data, file_names) # makes string of full file path for all csv files
  
  extract_data_from_file <- function(file) {
    data <- read.csv(file, header = TRUE,
                     stringsAsFactors = FALSE)
    
    #format timestamp
    data$timestamp <-as.POSIXct(data$timestamp, format = timestamp_format, tz = timestamp_timezone)
    
    # format factor variables
    try(data$sn <- as.factor(data$sn), silent = TRUE)
    try(data$baro_code <- as.factor(data$baro_code), silent = TRUE)
    
    data$site_station_code <- as.factor(data$site_station_code)
    try(data$qaqc_code <- as.factor(data$qaqc_code), silent = TRUE)
    try(data$wl_qaqc_code <- as.factor(data$wl_qaqc_code), silent = TRUE)
    try(data$do_qaqc_code <- as.factor(data$do_qaqc_code), silent = TRUE)
    try(data$wt_qaqc_code <- as.factor(data$wt_qaqc_code), silent = TRUE)
    try(data$at_qaqc_code <- as.factor(data$at_qaqc_code), silent = TRUE)
    try(data$baro_qaqc_code <- as.factor(data$baro_qaqc_code), silent = TRUE)
    
    return(data)
  } # end of extract data files loop
  
  dat1 <- map_df(path_to_files, extract_data_from_file)
  
  # optional select station
  if(select_station != "all") {
    dat1 <- dat1 %>% filter(site_station_code %in% select_station)
  }
  
  # optional select scale ONLY FOR CLEAN DATA
  dat2<- dat1
  
  if(data_processing=="clean"&temporal_scale!="hourly") {
    # make new grouping_var column
    dat2$grouping_var <- NA
    #group by selected scale
    if(temporal_scale == "daily") {
      dat2$grouping_var <- date(dat2$timestamp)
    }
    if(temporal_scale == "weekly") {
      dat2$grouping_var <- paste0(year(dat2$timestamp),"-", week(dat2$timestamp))
      
    }
    if(temporal_scale == "monthly") {
      dat2$grouping_var <- paste0(year(dat2$timestamp),"-", month(dat2$timestamp))
    }
    if(temporal_scale == "yearly") {
      dat2$grouping_var <- year(dat2$timestamp)
    }
    # columns differ by logger type
    if(logger_type=="dissolvedoxygen_U26"){
      var_ylab2 <- bquote('Dissolved oxygen (mg·'~'L'^-1~')')
      var_ylab1 <- "Water temperature (\u00b0C)"
      var_ylab3 <- "Dissolved oxygen (% air sat.)"
      
      dat2 <- dat2 %>%
        dplyr::group_by(site_station_code, grouping_var) %>%
        dplyr::summarize(mean_do_mgl = mean(do_mgl_U26_adj, na.rm = TRUE),
                         mean_do_percsat = mean(do_percsat_U26_adj, na.rm = TRUE),
                         mean_watertemp_C = mean(watertemp_C_U26_adj, na.rm = TRUE),
                         min_do_mgl = min(do_mgl_U26_adj, na.rm = TRUE),
                         min_do_percsat = min(do_percsat_U26_adj, na.rm = TRUE),
                         min_watertemp_C = min(watertemp_C_U26_adj, na.rm = TRUE),
                         max_do_mgl = max(do_mgl_U26_adj, na.rm = TRUE),
                         max_do_percsat = max(do_percsat_U26_adj, na.rm = TRUE),
                         max_watertemp_C = max(watertemp_C_U26_adj, na.rm = TRUE))
    }
    if(logger_type=="waterlevel_U20"){
      var_ylab2 <- "Water level (m)"
      var_ylab1 <- "Water temperature (\u00b0C)"
      
      dat2 <- dat2 %>%
        dplyr::group_by(site_station_code, grouping_var) %>%
        dplyr::summarize(mean_waterlevel_m = mean(waterlevel_m_U20_adj, na.rm = TRUE),
                         mean_watertemp_C = mean(watertemp_C_U20_adj, na.rm = TRUE),
                         max_waterlevel_m = max(waterlevel_m_U20_adj, na.rm = TRUE),
                         max_watertemp_C = max(watertemp_C_U20_adj, na.rm = TRUE),
                         min_waterlevel_m = min(waterlevel_m_U20_adj, na.rm = TRUE),
                         min_watertemp_C = min(watertemp_C_U20_adj, na.rm = TRUE))
    }
    if(logger_type=="baro_U20"){
      
      var_ylab2 <- "Air pressure (kPa)"
      var_ylab1 <- "Air temperature (\u00b0C)"
      
      dat2 <- dat2 %>%
        dplyr::group_by(site_station_code, grouping_var) %>%
        dplyr::summarize(mean_airpress_kPa = mean(airpress_kPa_U20, na.rm = TRUE),
                         mean_airtemp_C = mean(airtemp_C_U20_adj, na.rm = TRUE),
                         min_airpress_kPa = min(airpress_kPa_U20, na.rm = TRUE),
                         min_airtemp_C = min(airtemp_C_U20_adj, na.rm = TRUE),
                         max_airpress_kPa = max(airpress_kPa_U20, na.rm = TRUE),
                         max_airtemp_C = max(airtemp_C_U20_adj, na.rm = TRUE))
    }
    if(logger_type=="airtemp_TidbiT"){
      var_ylab1 <- "Air temperature (\u00b0C)"
      
      dat2 <- dat2 %>%
        dplyr::group_by(site_station_code, grouping_var) %>%
        dplyr::summarize(mean_airtemp_C = mean(airtemp_C_TidbiT_adj, na.rm = TRUE),
                         min_airtemp_C = min(airtemp_C_TidbiT_adj, na.rm = TRUE),
                         max_airtemp_C = max(airtemp_C_TidbiT_adj, na.rm = TRUE))
    }
    if(logger_type=="watertemp_TidbiT"){
      var_ylab1 <- "Water temperature (\u00b0C)"
      
      dat2 <- dat2 %>%
        dplyr::group_by(site_station_code, grouping_var) %>%
        dplyr::summarize(mean_watertemp_C = mean(watertemp_C_TidbiT_adj, na.rm = TRUE),
                         min_watertemp_C = min(watertemp_C_TidbiT_adj, na.rm = TRUE),
                         max_watertemp_C = max(watertemp_C_TidbiT_adj, na.rm = TRUE))
    }
    
    # rename columns
    if(temporal_scale == "daily") {
      colnames(dat2)[colnames(dat2) == 'grouping_var'] <- 'date'
      timescale_lab <- "Date"
      timescale_format <- "%y-%m-%d"
      
    }
    if(temporal_scale == "weekly") {
      colnames(dat2)[colnames(dat2) == 'grouping_var'] <- 'week'
      timescale_lab <- "Week"
      timescale_format <- "%y-%v"
    }
    if(temporal_scale == "monthly") {
      colnames(dat2)[colnames(dat2) == 'grouping_var'] <- 'month'
      timescale_lab <- "Timestamp (YY-MM)"
      timescale_format <- "%y-%m"
      
    }
    if(temporal_scale == "yearly") {
      colnames(dat2)[colnames(dat2) == 'grouping_var'] <- 'year'
      timescale_lab <- "Year"
      timescale_format <- "%Y"
      
    }
    
    dat2 <- dat2 %>% mutate(across(where(is.numeric), round, 2))
    dat2 <- as.data.frame(dat2)
    dat2[sapply(dat2, is.infinite)] <- NA
    dat2[sapply(dat2, is.nan)] <- NA
    
    # return requested dataset
    print("List returned: [1] clean data summarized by selected temporal scale [2] summary plots of data by site_station_code and timestamp")
    
    ## Graph Data ####
    
    try(plot1 <- ggplot(data=dat2, aes(x = dat2[,2])) +
          geom_line(aes(y = mean_watertemp_C), size = 1)+
          geom_ribbon(alpha = 0.3, aes(ymin = min_watertemp_C, ymax = max_watertemp_C))+
          facet_wrap(~site_station_code)+
          theme_classic()+
          theme(axis.text.x = element_text(angle = 90))+
          # scale_x_datetime(date_labels = timescale_format)+
          labs(x = timescale_lab, y = var_ylab1))
    
    try(plot2 <- ggplot(data=dat2, aes(x = dat2[,2])) +
          geom_line(aes(y = dat2[,3]), size = 1)+
          facet_wrap(~site_station_code)+
          theme_classic()+
          theme(axis.text.x = element_text(angle = 90))+
          #scale_x_datetime(date_labels = timescale_format)+
          labs(x = timescale_lab, y = var_ylab2))
    
    # add ribbons separately
    if(logger_type=="dissolvedoxygen_U26"){
      
      plot2<- plot2+geom_ribbon(alpha = 0.3, aes(ymin = min_do_mgl, ymax = max_do_mgl))
      
      plot3 <- ggplot(data=dat2, aes(x = dat2[,2])) +
        geom_line(aes(y = mean_do_percsat), size = 1)+
        geom_ribbon(alpha = 0.3, aes(ymin = min_do_percsat, ymax = max_do_percsat))+
        facet_wrap(~site_station_code)+
        theme_classic()+
        theme(axis.text.x = element_text(angle = 90))+
        # scale_x_datetime(date_labels = timescale_format)+
        labs(x = timescale_lab, y = var_ylab3)
    }
    
    if(logger_type=="waterlevel_U20"){
      plot2<- plot2 + geom_ribbon(alpha = 0.3, aes(ymin = min_waterlevel_m, ymax = max_waterlevel_m))
    }
    
    return(list(dat2, try(plot1), try(plot2), try(plot3)))
    
    
  } # end of clean data select_temporal_scale loop
  
  return(dat2)
  
} # end of get_logger_data loop
