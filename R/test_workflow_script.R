# test workflow
#
# Follows the same flow outlined in the waterlogged vingette 

# ---------------------------------------------------------------------

# NOTES:

# misc

  # a universal function could probably be created for writing csv names
  # Add field data functionality + correst stage from field
  # Would be nice to have a function to compare visually between stations - probably similar code to plot_qaqc_timeseries but would be same metric diff stations
  # should we add the ability to remove flags/edits? Would need to be removed in both the log and associated col
  # within the logging function, when it prints to the csv the "run_at" col automatically shows date/time as minutes:seconds.. would be nice if it showed properly as date time. 
    # I'm guessing a as.character() might fix this but haven't had time to rectify this yet

# metadata file 

  # should we add a comments col?
  # check metadata qc script - does it use plot title to garner logger ID? If so may want to change b/c cond loggers had diff titles vs numbs

# Time zones
  
  # if using lubridate, time zones will be automatic to the area which the script is run... how can we know the user is putting in the right
    # timezone for where they are and R is reading it properly?
  




library(tidyverse) # for use while this is still not in package format

# STEP 1 - GRAB ALL CSV FILES AND PUT INTO SINGLE OBJECT FOR EACH METRIC -----------------------------------------------
  # PRELIM QAQC

# still need to add tidbit QAQC
# if you have multiple metric types in one folder; this function will give three of the same errors for this. Could be simplified so only one is given. 

source("R/utils.R")
source("R/bind_hobo_files_helpers.R")
source("R/bind_hobo_files.R") # bind_hobo_files -> output v0.1

baro_bound <- bind_hobo_files(path_to_raw_folder = "data/testing/raw/baro", 
                              path_to_output_folder = "data/testing/processed", 
                              metadata_path = "data/testing/raw/testing_metadata.csv")

level_bound <- bind_hobo_files(path_to_raw_folder = "data/testing/raw/level", 
                               path_to_output_folder = "data/testing/processed", 
                               metadata_path = "data/testing/raw/testing_metadata.csv")

DO_bound <- bind_hobo_files(path_to_raw_folder = "data/testing/raw/DO", 
                               path_to_output_folder = "data/testing/processed", 
                               metadata_path = "data/testing/raw/testing_metadata.csv")

cond_bound <- bind_hobo_files(path_to_raw_folder = "data/testing/raw/COND", 
                            path_to_output_folder = "data/testing/processed", 
                            metadata_path = "data/testing/raw/testing_metadata.csv")


# NEW: Resample time interval --------------------------------
 # resample time so that intervals round to the nearest, 10, 15, 30 etc. Useful for joining later

source("R/snap_logger_timestamps.R")

wl_resampled_20mins <- resample_timestamps(
  input_data     = level_bound[[1]],
  interval       = "20 min",
  align          = "round",
  select_station = "ALBR_ST_30",
  metric         = "WL",                               
  log_root       = "data/testing/processed"              
)



# STEP 2 -  QAQC BAROMETRIC DATA ------------------------------------------------------------------
   # CHECK FOR UNUSUAL TEMPS, PRESSURS, SPIKES OR FLATLINES IN DATA
   # BELOW FUNCTION USES BAROMETRIC_QAQC.R FUNCTION TO RUN. ALL DATA DOES NOT HAVE TO BE RUN AT ONCE (e.g only one station can be run at a time). 


  # this qaqc function + plotting could use some more robustness testing in terms of weird inputs 

source("R/barometric_qaqc.R")
source("R/baro_qaqc_all.R")


all_checked_baro_data <- barometric_qaqc_all(
  baro_data_path        = "data/testing/processed",
  metadata_path         = "data/testing/raw/testing_metadata.csv",
  path_to_output_folder = "data/testing/processed",
  log_root              = "data/testing/processed",
  temp_low_limit        = -35,
  temp_high_limit       = 45, 
  pressure_low_kpa     = 85,
  pressure_high_kpa    = 105,
  spike_threshold_kpa  = 1.5,
  flatline_n           = 6
)

source("R/plot_qaqc_timeseries.R")
source("R/plot_qaqc_timeseries.R")
source("R/utils_plotting.R")

plot_qaqc_timeseries(
  wl_data   = NULL,
  do_data   = NULL,
  baro_data = all_checked_baro_data,   # from barometric_qaqc(), optional
  select_station = "DARL_ST_30"
)



# STEP 3 - BASED OFF OF A SPECIFIC STATION AN APPROPRIATE BARO STATION CAN BE SELECT -------------------------------------------
  # PREVIOUSLY, THIS WAS DONE ENTIRELY BASED OFF OF PROXIMITY; HOWEVER, FUNCTIONALITY TO CHOOSE
    # A SPECIFIC STATION WAS ADDED IN CASE CLOSEST STATION WAS NOT APPROPRIATE 


  # Further QC needs to be done on the QC columns that are added in this step. Want to make sure notes are being correctly added. 
     # e.g. if diff baro is used, there should be a col that says what the correction is compared to first baro used
  # "baro_site_selection" non auto options could benefit from more testing. 
     # to run the non-auto functionality enter a specific baro station name.
  # double check that this is using "qaqc'd barometric data (aka **_adj df's; not the raw stuff)

source("R/add_nearest_baro_helpers.R")
source("R/add_nearest_baro.R") # add nearest baro -> output v0.2

#TO DO: 
# can add baro logger to cond data to get air temp - talk to Julien
input_wl_data <- add_nearest_baro(input_data = level_bound[[1]],
                                  path_to_output_folder = "data/testing/processed", 
                                  baro_data_path = "data/testing/processed",
                                  metric = "both", 
                                  baro_site_selection = "auto",
                                  metadata_path = "data/testing/raw/testing_metadata.csv")


DO_with_Baro <- add_nearest_baro(input_data = DO_bound[[1]],
                                 path_to_output_folder = "data/testing/processed", 
                                 baro_data_path = "data/testing/processed",
                                 metric = "DO", 
                                 baro_site_selection = "auto",
                                 metadata_path = "data/testing/raw/testing_metadata.csv")


# STEP 3 - CONVERT WATERLEVEL FROM PRESSURE TO m's --------------------------------------------------------------------------------

  # To make more robust - in the future could make it so select_station accepts a list of stations
  # I'm not sure what the value is for returning the reference data..
  # double check workflow happening here 

source("R/convert_wl_kpa_m.R") # convert_waterlevel_kPa_m -> output v0.3

converted_data <- convert_waterlevel_kPa_m(input_data = input_wl_data[[1]],
                                           select_station = "all",
                                           reference_data = "data/testing/raw/NT_manual_waterlevel_20251119.csv",
                                           reference_type = "stage",
                                           select_measurement = 1,
                                           logger_type_expected = "u20",
                                           path_to_output_folder= "data/testing/processed") 




# WATER LEVEL QAQC  -----------------------------------------------------------------------------



source("R/waterlevel_qaqc.R")
source("R/qaqc_log_helpers.R")

# FYI - This code has now been split into two functions 1) waterlevel_complete_QAQC   2) waterlevel_qaqc_plot
waterlevel_complete_QAQC <- waterlevel_qaqc(converted_data[[1]],
                                            log_root = "data/testing/processed", 
                                            select_station = "ALBR_ST_30") 

waterlevel_complete_QAQC



# improve plotting -> right now when you zoom in there's few or none x axis labels. Would be beneficial to add more for better understanding of scale. 
#                  -> Could also benefit from at least the zero line being shown 
# Currently the log file is logging every single flag that's just automated... not sure if we should keep this or just keep actual changes..
# would be nice to add a proper warning about if you can't open file xx: Permission denied when you have the file open; would be a helpul addition for non coders. Same for the following logger functions
# Add field data to qaqc plots!! 

source("R/utils_plotting.R")
source("R/plot_qaqc_timeseries_helpers.R")
source("R/plot_qaqc_timeseries.R")

plot_qaqc_timeseries(
  wl_data   = waterlevel_complete_QAQC,
  do_data   = NULL,
  baro_data = NULL,   # from barometric_qaqc(), optional
  select_station = "ALBR_ST_30"
)

# NEW: Multi point drift correction -------------------------------------------

  # drift correction tool for wl sensor drift. Typically used to correct to field 
  # survey easurements 

source("R/drift_wl.R")

drift_pts <- tibble::tibble(
  timestamp = c("2024-10-10 10:00:00",
                "2025-02-04 00:00:00",
                "2025-06-01 00:00:00"),
  drift_m   = c(0.0, 1.20, -0.50)
)

station_wl_qc2 <- wl_multipoint_correction(
  input_data     = waterlevel_complete_QAQC,
  select_station = "ALBR_ST_30",
  log_root       = "data/testing/processed",
  drift_points   = drift_pts,
  manual_note    = "Multi-point drift based on  manual checks."
)


plot_qaqc_timeseries(
  wl_data   = station_wl_qc2,
  do_data   = NULL,
  baro_data = NULL,   
  select_station = "ALBR_ST_30"
)




# adjust water level offset --------------------------------------------------------------
#(PREVIOUSLY, ADJUST WATER LEVEL CABLE BREAK - RENAMED TO MAKE MORE UNIVERSAL)

# shouldn't we have to add what direction and how much to move this offset?? We just assume that the start/end is right? Not sure about this logic
# I think adding a manual offset could make sense too

source("R/adjust_wl_offset.R")

station_wl_qc2 <- adjust_WL_offset(input_data = station_wl_qc,
                                   select_station = "ALBR_ST_30",
                                   timestamp_start = "2025-02-04 07:00:00",
                                   timestamp_end = "2025-02-09 00:00:00" ,
                                   manual_note = "Suspected storm moved logger",
                                   log_root = "data/testing/processed",
                                   user = Sys.info()[["user"]])


# adjust water level spike ---------------------------------------------------------------

   # this function doesn't have capabilities of reading csv input data, should it?

source("R/adjust_waterlevel_spike.R")

station_wl_qc <- adjust_waterlevel_spike(input_data = waterlevel_complete_QAQC, 
                                           select_station = "ALBR_ST_30",
                                           timestamp_start = "2025-02-11 21:00:00",
                                           timestamp_end = "2025-02-20 16:00:00", 
                                           reason_to_adjust = "ice", 
                                           manual_note = "suspected ice formation due to freezing temps",
                                           log_root = "data/testing/processed", 
                                           user = Sys.info()[["user"]])





# plot -------------------------------------------------------------------------------
  # would like to look into playing around with x axis changing with amount of zoom in? 
    # Currently if you zoom in too far the date fully disappears

plot_qaqc_timeseries(
  wl_data   = station_wl_qc2,
  do_data   = NULL,
  baro_data = NULL,   # from barometric_qaqc(), optional
  select_station = "ALBR_ST_30"
)

#  Adjust logger NA -----------------------------------------------------------------------

source("R/adjust_logger_NA.R")

adjusted_WL_NA <- adjust_logger_NA( station_wl_qc2,
    select_station = "ALBR_ST_30",
    metric = "waterlevel",              
    reason_to_adjust = "ice", 
    timestamp_start = "2025-06-04 07:00:00",
    timestamp_end = "2025-06-09 00:00:00" ,  
    keep_temp         = FALSE,
    apply_to_all_data = FALSE,
    manual_note = "Ice present - could see in field pics",         
    log_root = "data/testing/processed",            
) 
  


# adjust waterlevel zero ---------------------------------------------------------------------

source("R/adjust_wl_zero.R")

# testing when change value is manually input
wl_zero <- adjust_WL_zero(
  input_data      = station_wl_qc,
  select_station  = "ALBR_ST_30",
  change_value    = 0.05,
  manual_note     = "Datum shifted +0.05 m to match survey benchmark",
  log_root        = "data/testing/processed"
)


# testing "auto" change value - unsure about the science behind this. 
wl_zero <- adjust_WL_zero(
  input_data      = station_wl_qc,
  select_station  = "ALBR_ST_30",
  change_value    = NULL,
  timestamp_start = "2025-02-10 00:00:00",
  timestamp_end   = "2025-02-12 00:00:00",
  manual_note     = "Zero set using observed dry period",
  log_root        = "data/testing/processed"
)




# write clean data -------------------------------------------------------------------------
source("R/write_clean_data.R")

clean_WL <- write_clean_data(
    input_data = adjusted_WL_NA,
    path_to_output_folder = "data/testing/processed",
    metric = "waterlevel"
) 

# get logger data ----------------------------------------------------------------------

  # This function does different stuff depending on what version of data the user is on 
    # - I wonder if it could benefit from being split up? What purpose does this serve? - see original code in 'waterlogged' for uses

source("R/get_logger_data.R")

res_wl <- get_logger_data(
  path_to_data   = "data/testing/processed",
  data_processing = "v1.0",
  metric          = "waterlevel",
  temporal_scale  = "monthly"
)



# DISSOLVED OXYGEN -----------------------------------------------------------------------

# convert do mgl percsat

# double check code to check that that math still makes sense for conversion after some code changes

source("R/convert_do_mgl_percsat.R")

converted_percSat <- convert_do_mgl_percsat(DO_with_Baro[[1]],
                                            output_dir = "data/testing/processed",
                                            version_label = "v0.3")




source("R/dissox_qaqc.R")

station_do_dat<- dissox_qaqc(input_data = converted_percSat, 
                             select_station = "COOK_WE_40", 
                             log_root = "data/testing/processed") 


# I would like this plotting to have the capabilities of showing multiple wq metrics at a time,
# as you can see below we only see DO even when there's wl selected as well

plot_qaqc_timeseries(do_data = station_do_dat,
                     wl_data   = station_wl_qc2,
                     select_station = "COOK_WE_40")




# CONDUCTIVITY QAQC -----------------------------------------------------------------------------------


cond_bound <- bind_hobo_files(path_to_raw_folder = "data/testing/raw/COND", 
                              path_to_output_folder = "data/testing/processed", 
                              metadata_path = "data/testing/raw/testing_metadata.csv")


source("R/conductivity_qaqc.R")
source("R/conductivity_qaqc_all.R")

all_checked_cond_data <- conductivity_qaqc_all(
  cond_data_path            = "data/testing/processed",
  metadata_path             = "data/testing/raw/testing_metadata.csv",
  path_to_output_folder     = "data/testing/processed",
  log_root                  = "data/testing/processed",
  temp_low_limit_C          = -2,
  temp_high_limit_C         = 40,
  disturbance_threshold_uScm = 200,
  dry_threshold_uScm        = 10,
  air_water_diff_threshold_C = 2
)


reference_data <- QAQC_reference_data(reference_data)

# ADD DRIFT CORRECTION - 
source("R/drift_conductivity.R") # missing reference data with these cols: ysi_timestamp, ysi_conduct_uScm, ysi_watertemp_C
                                 # to be able to QAQC this function

cond_drift_corr <- wq_drift_correction(
  input_data   = all_checked_cond_data,
  ref_data     = "data/testing/raw/site_station_waterquality.csv",
  select_station = "TUMT_WE_40",
  metric         = "conductivity",      # optional in your design, but explicit is safer
  method         = "ratio",     # or "offset"
  max_ref_gap_h  = 1,           # only accept ref within 1 hr
  log_root       = "data/testing/processed",
  user           = Sys.info()[["user"]]
)


plot_qaqc_timeseries(cond_data = cond_drift_corr,
                     select_station = "TUMT_WE_40")

source("R/conductivity_temp_compensation.R")

compensated_cond <- conductivity_temp_compensation(all_checked_cond_data)




