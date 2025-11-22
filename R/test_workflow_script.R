# test workflow


source("R/step1.R") # bind_hobo_files -> output v0.1
source("R/step2.R") # add nearest baro -> output v0.2
source("R/step3.R") # convert_waterlevel_kPa_m -> output v0.3


# step 1 ----------------------------------------------------------------------------------
baro_bound <- bind_hobo_files(path_to_raw_folder = "data/testing/raw/baro", 
                              path_to_output_folder = "data/testing/processed", 
                              metadata_path = "data/testing/raw/testing_metadata.csv")

level_bound <- bind_hobo_files(path_to_raw_folder = "data/testing/raw/level", 
                               path_to_output_folder = "data/testing/processed", 
                               metadata_path = "data/testing/raw/testing_metadata.csv")


# step 2-------------------------------------------------------------------------------

input_wl_data <- add_nearest_baro(input_data = level_bound[[1]],
                                  path_to_output_folder = "data/testing/processed", 
                                  baro_data_path = "data/testing/processed",
                                  baro_site_selection = "auto",
                                  metadata_path = "data/testing/raw/testing_metadata.csv")


# step 3--------------------------------------------------------------------------------

converted_data <- convert_waterlevel_kPa_m(input_data = input_wl_data[[1]],
                                           select_station = "all",
                                           reference_data = "data/testing/raw/NT_manual_waterlevel_20251119.csv",
                                           reference_type = "stage",
                                           select_measurement = 1,
                                           logger_type_expected = "u20",
                                           path_to_output_folder= "data/testing/processed") 


# get logger data --------------------------------------------------------------------


source("R/get_logger_data.R")

# need to go back and test this once I have "clean" data - aka V1.0
# I've noticed a lot of col's are hard coded in - that will likely need to be fixed later on
res_wl <- get_logger_data(
  path_to_data   = "data/testing/processed",
  data_processing = "v0.1",
  metric          = "gvhgvm",
  temporal_scale  = "none"
)

