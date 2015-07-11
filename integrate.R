# This script runs all the scripts to process data, construct measures, estimate models, and output results

#----- 1. Load functionality -----
# This should generally be done in the individual scripts during development, then migrated here

library(magrittr)
library(dplyr) # Always load last!

#----- 2. Set parameters -----
data_directory <- "E:/Data/zakupki/"
data_download_date <- "2015-06-13"

