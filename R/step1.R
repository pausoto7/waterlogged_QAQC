
#library(waterlogged)
library(purrr)
library(tidyverse)




# to run this example using our data, you can access raw hobo files here:
metadata_path <- "data/deadwood/raw/deadwood_metadata.csv"


path_to_raw_folder <- "data/deadwood/raw/baro"


path_to_output_folder <- "data/deadwood/processed"  #from this folder (ie. "data/baro/2024/raw" or simply the Desktop of your local computer), bind_hobo_files will look for a year folder corresponding to your logger data, and if one isn't made, it will create a new year folder and a "processed" folder to store your processed data (ie. "data/baro/2024/processed")


logger_type <- "U20"



timestamp_timezone = "UTC"


source("R/step1_utils.R")
#library(tidyverse)

baro_bound <- bind_hobo_files(path_to_raw_folder = path_to_raw_folder, 
                              path_to_output_folder = path_to_output_folder, 
                              logger_type = logger_type, 
                              metadata_path = metadata_path)
