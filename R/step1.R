
library(waterlogged)
library(purrr)
library(tidyverse)


# to run this example using our data, you can access raw hobo files here:
metadat <-read.csv("data/deadwood/raw/deadwood_metadata.csv" )
#metadat <-"data/deadwood/deadwood_metadata.csv" 



raw_dir <- "data/deadwood/raw/baro"


out_dir <- "data/deadwood/processed"  #from this folder (ie. "data/baro/2024/raw" or simply the Desktop of your local computer), bind_hobo_files will look for a year folder corresponding to your logger data, and if one isn't made, it will create a new year folder and a "processed" folder to store your processed data (ie. "data/baro/2024/processed")

baro_bound <- bind_hobo_files(path_to_raw_folder = raw_dir, 
                              path_to_output_folder = out_dir, 
                              logger_type = "baro_U20", 
                              metadata = metadat)
