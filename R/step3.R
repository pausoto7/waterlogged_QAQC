#' Waterlevel conversion
#'
#' Convert water level from kPa to metres using barometric compensation
#' @param input_data Dataframe containing timeseries of raw water level in kPa, water temperature
#' @param var_watertemp_C Name of column for water temperature (in degrees Celcius) in input data
#' @param var_waterpress_kPa Name of column for water pressure (in kPa) in input data
#' @param var_airpress_kPa Name of column for air pressure (in kPa) in input data
#' @param var_airtemp_C Name of column for air temperature (in degrees Celcius) in input data
#' @param var_reference_waterlevel_m Name of column for reference water level in metres
#' @param var_reference_timestamp Name of column for reference waterlevel timestamp (as.POSIXct)
#' @param reference_type Type of manual measurement used for correcting water level; Either stage or depth
#' @param reference_data File containing manual measurements of water level: required variables site_station_code, stage_timestamp, stage_m or depth_m
#' @param select_station site_station_code to ID station for qaqc
#' @param select_measurement Numeric; Select which manual measurement will be used for correcting (default is first measurement, 1)
#' @param path_to_output_folder The desired folder for output of baro compensation
#' @return List of 3 objects: 1. site_wl: dataframe of converted waterlevel for specified station, 2. ref_dat: dataframe of manual measurements for specified site, 3. plot of waterlevel data for specified site to aid in QAQC
#' @export
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @import lubridate

convert_waterlevel_kPa_m <- function(input_data,
                                     select_station,
                                     reference_data,
                                     var_reference_waterlevel_m = "stage_m",
                                     var_reference_timestamp = "stage_timestamp",
                                     reference_type = "stage",
                                     select_measurement = 1,
                                     var_waterpress_kPa = "waterpress_kPa_U20",
                                     var_watertemp_C = "watertemp_C_U20",
                                     var_airpress_kPa = "airpress_kPa_U20_adj",
                                     var_airtemp_C = "airtemp_C_U20_adj",
                                     path_to_output_folder
) {
  
  #make sure all paths have trailing slash
  if (!endsWith(path_to_output_folder, "/")) {
    path_to_output_folder <- paste0(path_to_output_folder, "/")
  }
  
  ## metric conversions
  FEET_TO_METERS  <-  0.3048
  KPA_TO_PSI  <-  0.1450377
  PSI_TO_PSF  <-  144.0
  
  #### Define Target Site and Prepare Data Frames ####
  ## filter datasets
  ## rename input columns to local columns
  output_data <- input_data
  output_data <- output_data[output_data$site_station_code == select_station,]
  
  names(output_data)[names(output_data) == var_waterpress_kPa ] <- "waterpress_kPa_U20"
  names(output_data)[names(output_data) == var_airpress_kPa ] <- "airpress_kPa_U20_adj"
  names(output_data)[names(output_data) == var_watertemp_C ] <- "watertemp_C_U20"
  names(output_data)[names(output_data) == var_airtemp_C ] <- "airtemp_C_U20_adj"
  
  
  ts_range <- range(output_data$timestamp)
  
  if(reference_type != "stage" & reference_type != "depth") {
    print("Uh oh! manual_type must be either stage or depth. Silly Goose!")
  }
  
  #correct default variable name from stage to depth if selected
  if(reference_type == "depth" & var_reference_waterlevel_m == "stage_m") {
    var_reference_waterlevel_m = "depth_m"
  }
  
  names(reference_data)[names(reference_data) == var_reference_timestamp] <- 'timestamp'
  names(reference_data)[names(reference_data) == var_reference_waterlevel_m] <- 'ref_m'
  # round to nearest hour
  reference_data$timestamp <- round_date(reference_data$timestamp, "hour")
  #filter for only reference obs within data time period
  ref_dat <- reference_data %>%
    filter(site_station_code == select_station)%>%
    filter(timestamp >= ts_range[1] & timestamp <= ts_range[2]) %>%
    select(timestamp, ref_m)
  
  if(length(ref_dat$timestamp)==0){
    print(paste("Error: ", reference_type," data not found for", select_station, ". Check reference file or change reference_type."))
  }
  
  output_data <- left_join(output_data, ref_dat, by = "timestamp")
  
  #### Run Barometric Compensation ####
  
  ## Step 1 - Calculate Fluid Density of Water ##
  
  # To compute this array, first the fluid density is computed. This is either determined
  # by the user-selected density, or is computed from the temperature at the reference time,
  # via:
  #
  # ρ = (999.83952 + 16.945176 Tref - 7.9870401e-03 Tref^2 - 46.170461e-06 Tref^3 +
  #        105.56302e-09 Tref^4 - 280.54253e-12 Tref^5) / (1 + 16.879850e-03 Tref)  [1]
  
  output_data <- output_data %>%
    mutate(density_kgm3 = (999.83952 + (16.945176*watertemp_C_U20) - ((7.9870401e-03)*(watertemp_C_U20^2)) - ((46.170461e-06)*(watertemp_C_U20^3)) +
                             ((105.56302e-09) * (watertemp_C_U20^4)) - ((280.54253e-12)*(watertemp_C_U20^5))) / (1 + (16.879850e-03)*watertemp_C_U20) )
  
  # Density is converted to lb/ft3 via:
  #   ρ = 0.0624279606 ρ    [2]
  
  output_data <- output_data %>%
    mutate(density_lbft3 = 0.0624279606 * density_kgm3)
  
  ## Step 2 - Convert Density to Sensor Depth
  
  # The array of downwell pressure values, P,  are then converted to a density dependent fluid depth array, D[], via:
  #
  #   D[]= FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * P) / ρ           [3]
  #
  # Where,
  #
  # FEET_TO_METERS = 0.3048
  # KPA_TO_PSI = 0.1450377
  # PSI_TO_PSF = 144.0
  ## Step 3 - Calculate Barometric Fluid Density
  #
  # Next, the fluid density at the reference time is determined. This is either the user-entered density,
  # or is computed from Equations 1 & 2, resulting in ρref.
  
  # ρ = (999.83952 + 16.945176 Tref - 7.9870401e-03 Tref^2 - 46.170461e-06 Tref^3 +
  #        105.56302e-09 Tref^4 - 280.54253e-12 Tref^5) / (1 + 16.879850e-03 Tref)  [1]
  #   ρ = 0.0624279606 ρ    [2]
  
  #The pressure at the reference time is converted to a barometric “depth” using Equation 3,
  # resulting in Dbaro0.
  
  #   D[]= FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * P) / ρ           [3]
  #
  output_data <- output_data %>%
    mutate(sensor_depth_m = FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * waterpress_kPa_U20)/ density_lbft3,
           baro_sensor_depth_m = FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * airpress_kPa_U20_adj)/ density_lbft3)
  
  ## Step 4 -  Determine reference values and compensation constant (k)
  #
  # The density dependent depth value at the reference time is then extracted from the array:
  #
  #   Dref = D[Ref Time]
  
  ## define which manual stage measurement to use to as a reference level for the barometric compensation
  
  # selects corresponding timestamp from the nth row (may need to add/subtract an hour if logger was out of water)
  T_meas <- as.numeric(ref_dat[select_measurement,"timestamp"]) # + (60*60)
  T_meas <- ref_dat[select_measurement,"timestamp"] # + (60*60)
  
  #T_meas <- T_meas + 60*60
  
  # selects reference water level measurement from the nth row
  L_meas <- as.numeric(ref_dat[select_measurement,"ref_m"])
  
  # select water level and barometric sensor depths at reference time
  D_ref <- output_data$sensor_depth_m[output_data$timestamp == T_meas]
  D_baro0 <- output_data$baro_sensor_depth_m[output_data$timestamp == T_meas]
  
  ## define compensation constant (k)
  k <- L_meas - (D_ref - D_baro0)
  
  ## Step 5 - Determine Water Level by Applying Compensation Constant to Entire Dataset
  
  # At this point, the compensation constant is applied to each downwell barometric depth reading in the array,
  # D. An important step here is to determine the proper barometric pressure value to use.  Since the BCA does
  # not require that then barometric pressure channel have the same sample times as the downwell pressure channel,
  # individual values for barometric pressure, Pbaro, may sometimes be interpolated between the points closest to
  # the downwell pressure value of interest.
  #
  # Loop through the entire downwell channel, applying the compensation constant to the density dependent fluid
  # depth values computed above. This is the step that adjusts the density dependent depth values for fluctuations
  # in barometric pressure. This is determined by:
  #
  #   Lreal[]= D[] – Dbaro[] + k       [5]
  #
  # Where Lreal[] is an array of the actual water level values (from a fixed reference point),
  # D is the density dependent fluid depth array computed earlier, Dbaro is the barometric depth at the time
  # index in the array (using Equation 3), and k is the compensation constant. The values of Lreal are stored
  # in a new Water Level channel and added to the list of available channels to plot.
  
  output_data <- output_data %>%
    mutate(waterlevel_m_U20 = output_data$sensor_depth_m - output_data$baro_sensor_depth_m + k)
  
  output_data$waterlevel_reference <- reference_type
  ## Step 6 - Review Data
  
  # manual measurements (points) should plot on top of trace.
  # if measurements are wildly off try adding or subtracting an hour from the reference timestamp or selecting a different reference
  # don't worry if pieces from different dates don't line up, this can be adjusted during QA-QC
  p <- ggplot(data=output_data, aes(y = waterlevel_m_U20, x = timestamp))+
    geom_line(color ="#233d4d")+
    geom_point(aes(y = ref_m, x = timestamp), color = "#fe7f2d", size = 4)+
    labs(title = paste("kPa to m conversion for:", select_station),
         y = "Water level (m)",
         x = "Timestamp") +
    theme_classic()
  
  
  # reverse local name changes
  names(output_data)[names(output_data) == "waterpress_kPa_U20" ] <- var_waterpress_kPa
  names(output_data)[names(output_data) == "airpress_kPa_U20_adj" ] <- var_airpress_kPa
  names(output_data)[names(output_data) == "watertemp_C_U20" ] <- var_watertemp_C
  names(output_data)[names(output_data) == "airtemp_C_U20_adj" ] <- var_airtemp_C
  
  if(var_waterpress_kPa != "waterpress_kPa_U20"){
    names(output_data)[names(output_data) == "waterlevel_m_U20" ] <- paste0(var_waterpress_kPa, "_m")
    
  }
  # save as new version
  # split by year
  years_i <- unique(lubridate::year(output_data$timestamp))
  
  for(n in 1:length(years_i)){
    year_n <- years_i[n]
    site_year_n <- output_data %>% filter(lubridate::year(timestamp)==year_n)
    
    #name by timestamp range
    start_n <- as.character(min(lubridate::date(site_year_n$timestamp)))
    start_n <- gsub("\\D", "", start_n)
    end_n <- as.character(max(lubridate::date(site_year_n$timestamp)))
    end_n <- gsub("\\D", "", end_n)
    
    # search for year folder, if does not exist, write new one
    # if(!dir.exists(paste0(path_to_output_folder, year_j))){dir.create(paste0(path_to_output_folder, year_j))}
    if(!dir.exists(paste0(path_to_output_folder, year_n))){dir.create(paste0(path_to_output_folder, year_n))}
    if(!dir.exists(paste0(path_to_output_folder, year_n,"/processed/"))){dir.create(paste0(path_to_output_folder, year_n,"/processed/"))}
    
    # correct missing midnight timestamps
    site_year_n$timestamp <- as.character(format(site_year_n$timestamp))
    
    # write csv for each year and site_station_code
    write.csv(site_year_n, paste0(path_to_output_folder, year_n,"/processed/", select_station,"_WL_", start_n,"_", end_n, "_v0.3", ".csv"), row.names = FALSE)
    print(paste("Barometric compensation applied to waterlevel (v0.3) from",select_station,"added to", year_n, "folder"))
    
  } #end of year level loop
  
  print("Results returned as list: select [1] for waterlevel data in metres, [2] for reference waterlevel dataset, [3] for waterlevel plot to visually check reference values")
  
  print("Tip: Do reference water levels observed (orange points) align with the predicted water level (navy line)? If so, great work! Continue onto QAQC. If not, try choosing a different reference water level (ie. select_measurement = 2, etc)")
  
  return(list(output_data, ref_dat, suppressWarnings(ggplotly(p))))
  
} # end of function

