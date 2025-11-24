# test workflow


source("R/step1.R") # bind_hobo_files -> output v0.1
source("R/step2.R") # add nearest baro -> output v0.2
source("R/step3.R") # convert_waterlevel_kPa_m -> output v0.3



# misc

  # what to do with temp qaqc script - should we incorporate it into bind_hobo_files?
  # a universal function could probably be created for writing csv names


# metadata file 

  # should we add a comments col?
  # check metadata qc script - does it use plot title to garner logger ID? If so may want to change b/c cond loggers had diff titles vs numbs

# field data
  
  # How can we ensure people are putting in reference data in the correct time zone. 


# step 1 ----------------------------------------------------------------------------------

# still need to add tidbit QAQC
# if you have multiple metric types in one folder; this function will give three of the same errors for this. Could be simplified so only one is given. 


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


converted_data <- convert_waterlevel_kPa_m(input_data = input_wl_data[[1]],
                                           select_station = "all",
                                           reference_data = "data/testing/raw/NT_manual_waterlevel_20251119.csv",
                                           reference_type = "stage",
                                           select_measurement = 1,
                                           logger_type_expected = "u20",
                                           path_to_output_folder= "data/testing/processed") 


# get logger data --------------------------------------------------------------------

# haven't qaqc'd much yet beause need a complete dataset to do so

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

# check code to check that after all this rewriting math still makes sense for conversion
# do we really need version input here?

# COMPLETE

source("R/convert_do_mgl_percsat.R")

converted_percSat <- convert_do_mgl_percsat(DO_with_Baro[[1]],
                                   output_dir = "data/testing/processed",
                                   version_label = "v0.3")


