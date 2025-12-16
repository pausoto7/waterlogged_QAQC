# wl test workflow


# NOTES
  # Add field data functionality + correct stage from field -> requires having a datum. 




# STEP 1 - BIND ALL HOBO FILES

level_bound <- bind_hobo_files(path_to_raw_folder = "data/testing/raw/level", 
                               path_to_output_folder = "data/testing/processed", 
                               metadata_path = "data/testing/raw/testing_metadata.csv")



baro_bound <- bind_hobo_files(path_to_raw_folder = "data/testing/raw/baro", 
                              path_to_output_folder = "data/testing/processed", 
                              metadata_path = "data/testing/raw/testing_metadata.csv")


 # STEP 1 B - IF NEEDED 
# --------------------------------------------
# COULD USE RESAMPLE FUNCTION HERE IF NEEDED; THIS PARTICULAR WORKFLOW DID NOT NEED IT (It's made though, see other workflow function for use)
# --------------------------------------------


# STEP 2 -  QAQC BAROMETRIC DATA ------------------------------------------------------------------
# CHECK FOR UNUSUAL TEMPS, PRESSURES, SPIKES OR FLATLINES IN DATA
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

# NEXT STEPS:
  # - need to create a datum that's referenced to continuous WL (and will we have an arbitary or geodetic datum)
  # - relate that to benchmarks and surveyed water surface

  # - add in a spot for a staff gauge?
  # - need to add in BM's to reference/field data



#NEW FUNCITON - STILL IN TESTING CURRENTLY -
wl_with_datum <- wl_set_datum(
  input_data       = converted_data,
  level_runs_path  = "data/testing/raw/example_wl_level_runs.csv" ,
  select_station   = "ALBR_ST_30",
  log_root         = "data/testing/processed",
  user             = Sys.info()[["user"]]
)



# WATER LEVEL QAQC  -----------------------------------------------------------------------------



source("R/waterlevel_qaqc.R")
source("R/qaqc_log_helpers.R")

# FYI - This code has now been split into two functions 1) waterlevel_complete_QAQC   2) waterlevel_qaqc_plot
waterlevel_complete_QAQC <- waterlevel_qaqc(converted_data[[1]],
                                            log_root = "data/testing/processed", 
                                            select_station = "ALBR_ST_30") 

waterlevel_complete_QAQC


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



