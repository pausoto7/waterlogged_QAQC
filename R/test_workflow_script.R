# test workflow
#
# ---------------------------------------------------------------------

# NOTES:

# misc

  # a universal function could probably be created for writing csv names

  # Would be nice to have a function to compare visually between stations - probably similar code to plot_qaqc_timeseries but would be same metric diff stations
  # should we add the ability to remove flags/edits? Would need to be removed in both the log and associated col
  # within the logging function, when it prints to the csv the "run_at" col automatically shows date/time as minutes:seconds.. would be nice if it showed properly as date time. 
    # I'm guessing a as.character() might fix this but haven't had time to rectify this yet
  # drifting with wq - can we add a non-linear drift correction

# metadata file 

  # should we add a comments col?
  # check metadata qc script - does it use plot title to garner logger ID? If so may want to change b/c cond loggers had diff titles vs numbs

# Time zones
  
  # if using lubridate, time zones will be automatic to the area which the script is run... how can we know the user is putting in the right
    # timezone for where they are and R is reading it properly?
  

# improve plotting -> right now when you zoom in there's few or none x axis labels. Would be beneficial to add more for better understanding of scale. 
#                  -> Could also benefit from at least the zero line being shown 
# Currently the log file is logging every single flag that's just automated... not sure if we should keep this or just keep actual changes..
# would be nice to add a proper warning about if you can't open file xx: Permission denied when you have the file open; would be a helpul addition for non coders. Same for the following logger functions
# Add field data to qaqc plots!! 




library(tidyverse) # for use while this is still not in package format


#---------------------------------------------------------------------------------------------
# THIS SCRIPT HAS BEEN UPDATED AND IS NOW ONLY FOR WQ - SEE WL TEST SCRIPT FOR WL QAQC DETAILS
# -------------------------------------------------------------------------------------------


# STEP 1 - GRAB ALL CSV FILES AND PUT INTO SINGLE OBJECT FOR EACH METRIC --------------------
  # PRELIM QAQC

# still need to add tidbit QAQC
# if you have multiple metric types in one folder; this function will give three of the same errors for this. Could be simplified so only one is given. 

source("R/utils.R")
source("R/bind_hobo_files_helpers.R")
source("R/bind_hobo_files.R") # bind_hobo_files -> output v0.1

baro_bound <- bind_hobo_files(path_to_raw_folder = "data/testing/raw/baro", 
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




source("R/plot_qaqc_timeseries.R")
source("R/plot_qaqc_timeseries.R")
source("R/utils_plotting.R")



# BASED OFF OF A SPECIFIC STATION AN APPROPRIATE BARO STATION CAN BE SELECT -------------------------------------------
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
# can add baro logger to cond data to get air temp - talk to Julian



DO_with_Baro <- add_nearest_baro(input_data = DO_bound[[1]],
                                 path_to_output_folder = "data/testing/processed", 
                                 baro_data_path = "data/testing/processed",
                                 metric = "DO", 
                                 baro_site_selection = "auto",
                                 metadata_path = "data/testing/raw/testing_metadata.csv")




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

source("R/drift_conductivity.R") #


do_drift_corr <- wq_drift_correction(
  input_data   = station_do_dat,
  ref_data     = "data/testing/raw/site_station_waterquality.csv",
  select_station = "COOK_WE_40",
  metric         = "do",      
  method         = "ratio",     # or "offset"
  max_ref_gap_h  = 1,           # only accept ref within 1 hr
  log_root       = "data/testing/processed",
  user           = Sys.info()[["user"]]
)


plot_qaqc_timeseries(do_data = do_drift_corr,
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


# ADD DRIFT CORRECTION - 
source("R/drift_conductivity.R") #

cond_drift_corr <- wq_drift_correction(
  input_data   = all_checked_cond_data,
  ref_data     = "data/testing/raw/site_station_waterquality.csv",
  select_station = "TUMT_WE_40",
  metric         = "cond",      # optional in your design, but explicit is safer
  method         = "ratio",     # or "offset"
  max_ref_gap_h  = 1,           # only accept ref within 1 hr
  log_root       = "data/testing/processed",
  user           = Sys.info()[["user"]]
)


plot_qaqc_timeseries(cond_data = cond_drift_corr,
                     select_station = "TUMT_WE_40")

source("R/conductivity_temp_compensation.R")

compensated_cond <- conductivity_temp_compensation(all_checked_cond_data)




