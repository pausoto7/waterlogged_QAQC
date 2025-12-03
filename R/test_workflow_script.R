# test workflow
#
# Follows the same flow outlined in the waterlogged vingette 

# ---------------------------------------------------------------------

# NOTES:

# misc

  # what to do with temp qaqc script - should we incorporate it into bind_hobo_files?
  # a universal function could probably be created for writing csv names
  # We're still missing a drift correct function - always make correction linear?
  # once initial overall qaqc is done, I think there is value to creating new file names to
    # make it easier to follow what function is for what and what order they're typically completed in


# metadata file 

  # should we add a comments col?
  # check metadata qc script - does it use plot title to garner logger ID? If so may want to change b/c cond loggers had diff titles vs numbs

# field data
  
  # How can we ensure people are putting in reference data in the correct time zone. 


# step 1 ----------------------------------------------------------------------------------

# still need to add tidbit QAQC
# if you have multiple metric types in one folder; this function will give three of the same errors for this. Could be simplified so only one is given. 
# Low/Range vs high range conductivity read file issue - see issue in github Issues

source("R/step1.R") # bind_hobo_files -> output v0.1

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

# step 2-------------------------------------------------------------------------------


# Further QC needs to be done on the QC columns that are added in this step. I think some/all the type they're not adding notes as needed. 
    # e.g. if diff baro is used, there should be a col that says what the correction is compared to first baro used

source("R/step2.R") # add nearest baro -> output v0.2

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


# step 3--------------------------------------------------------------------------------

# To make more robust - in the future could make it so select_station accepts a list of stations

source("R/step3.R") # convert_waterlevel_kPa_m -> output v0.3

converted_data <- convert_waterlevel_kPa_m(input_data = input_wl_data[[1]],
                                           select_station = "all",
                                           reference_data = "data/testing/raw/NT_manual_waterlevel_20251119.csv",
                                           reference_type = "stage",
                                           select_measurement = 1,
                                           logger_type_expected = "u20",
                                           path_to_output_folder= "data/testing/processed") 


# get logger data --------------------------------------------------------------------

# haven't qaqc'd much yet beause need a complete V1.0 dataset to do so - haven't gotten there yet

source("R/get_logger_data.R")

# need to go back and test this once I have "clean" data - aka V1.0
# I've noticed a lot of col's are hard coded in - that will likely need to be fixed later on
res_wl <- get_logger_data(
  path_to_data   = "data/testing/processed",
  data_processing = "v0.1",
  metric          = "gvhgvm",
  temporal_scale  = "none"
)


# convert do mgl percsat -----------------------------------------------------------------

# double check code to check that that math still makes sense for conversion after some code changes
# do we really need version input here?

# COMPLETE

source("R/convert_do_mgl_percsat.R")

converted_percSat <- convert_do_mgl_percsat(DO_with_Baro[[1]],
                                   output_dir = "data/testing/processed",
                                   version_label = "v0.3")


# waterlevel_qaqc -----------------------------------------------------------------------

# improve plotting -> right now when you zoom in there's few or none x axis labels. Would be beneficial to add more for better understanding of scale. 
#                  -> Could also benefit from at least the zero line being shown 
# Currently the log file is logging every single flag that's just automated... not sure if we should keep this or just keep actual changes..
# would be nice to add a proper warning about if you can't open file xx: Permission denied when you have the file open; would be a helpul addition for non coders. Same for the following logger functions


source("R/waterlevel_qaqc.R")

# FYI - SPLIT THIS CODE UP TO BE TWO FUNCTIONS 1) waterlevel_complete_QAQC   2) waterlevel_qaqc_plot
waterlevel_complete_QAQC <- waterlevel_qaqc(converted_data[[1]],
                                            log_root = "data/testing/processed", 
                                            select_station = "WL_ALBR_ST_30") 

waterlevel_complete_QAQC


# adjust water level spike ---------------------------------------------------------------

   # this function doesn't have capabilities of reading csv input data, should it?

source("R/adjust_waterlevel_spike.R")

station_wl_qc <- adjust_waterlevel_spike(input_data = waterlevel_complete_QAQC, 
                                           select_station = "WL_ALBR_ST_30",
                                           timestamp_start = "2025-02-11 21:00:00",
                                           timestamp_end = "2025-02-20 16:00:00", 
                                           reason_to_adjust = "ice", 
                                           manual_note = "suspected ice formation due to freezing temps",
                                           log_root = "data/testing/processed", 
                                           user = Sys.info()[["user"]])

# plot hydro -----------------------------------------------------------------------------------

# symbology for flags and edits could be a little clearer if we wanted to

source("R/plot_hydro_data.R")

# plot
waterlevel_qaqc_plot(qaqc_data = station_wl_qc,
                select_station = "WL_ALBR_ST_30")


# adjust water level offset --------------------------------------------------------------
  #(PREVIOUSLY, ADJUST WATER LEVEL CABLE BREAK - RENAMED TO MAKE MORE UNIVERSAL)

# shouldn't we have to add what direction and how much to move this offset??

source("R/adjust_wl_offset.R")

station_wl_qc2 <- adjust_WL_offset(input_data = station_wl_qc,
                                 select_station = "WL_ALBR_ST_30",
                                 timestamp_start = "2025-02-04 07:00:00",
                                 timestamp_end = "2025-02-09 00:00:00" ,
                                 manual_note = "Suspected storm moved logger",
                                 log_root = "data/testing/processed",
                                 user = Sys.info()[["user"]])

# -------------------------------------------------------------------------------
  # would like to look into playing arounx with x axis changing with amount of zoom in? 
    # Currently if you zoom in too far the date fully disappears

source("R/plot_hydro_data.R")
# plot
waterlevel_qaqc_plot(qaqc_data = station_wl_qc2,
                     select_station = "WL_ALBR_ST_30")



    ## WANT TO MAKE A DRIFT FUNCTION for WL and also other params! # ------------------------------------------

# DISSOLVED OXYGEN -----------------------------------------------------------------------

source("R/dissox_qaqc.R")

      # NEEED TO ADD LOG FUNCTIONALITY
station_do_dat<- dissox_qaqc(converted_percSat, "DO_COOK_WE_40") 


source("R/plot_do_data.R")

#do_baro_sat_wl<- left_join(station_wl_qc2, do_baro_sat_wl)

plot_qaqc_timeseries(do_data = station_do_dat,
                     wl_data = station_wl_qc2, 
                     select_station = "DO_COOK_WE_40")




#  Adjust logger NA -----------------------------------------------------------------------

source("R/adjust_logger_NA.R")

adjusted_WL_NA <- adjust_logger_NA( station_wl_qc2,
    select_station = "WL_ALBR_ST_30",
    metric = "waterlevel",              
    reason_to_adjust = "ice", 
    timestamp_start = "2025-06-04 07:00:00",
    timestamp_end = "2025-06-09 00:00:00" ,  
    keep_temp         = FALSE,
    apply_to_all_data = FALSE,
    manual_note = "Ice present - could see in field pics",         # required free-text explanation
    log_root = "data/testing/processed",            # root folder where /logs lives
) 
  
waterlevel_qaqc_plot(qaqc_data = adjusted_WL_NA,
                     select_station = "WL_ALBR_ST_30")



# adjust waterlevel zero ---------------------------------------------------------------------

  # 2025-12-02

wl_zero <- adjust_WL_zero(
  input_data      = station_wl_qc,
  select_station  = "WL_ALBR_ST_30",
  change_value    = 0.05,
  manual_note     = "Datum shifted +0.05 m to match survey benchmark",
  log_root        = "data/testing/processed"
)



wl_zero <- adjust_WL_zero(
  input_data      = station_wl_qc,
  select_station  = "WL_ALBR_ST_30",
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

source("R/get_logger_data.R")

res_wl <- get_logger_data(
  path_to_data   = "data/testing/processed",
  data_processing = "v1.0",
  metric          = "waterlevel",
  temporal_scale  = "monthly"
)


