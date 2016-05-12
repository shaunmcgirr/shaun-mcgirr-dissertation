# This script runs all the scripts to process data, construct measures, estimate models, and output results

#----- 1. Download data, load functionality -----
# First download your data according to instructions here:
# https://github.com/shaunmcgirr/shaun-mcgirr-dissertation/blob/master/2-Obtain/download-raw-procurement-data.md

# Packages are loaded here unless there is a special reason to load them in the scripts below
library(xml2)         # install.packages('xml2')
library(magrittr)     # install.packages('magrittr')
library(openxlsx)     # install.packages('openxlsx')
library(parallel)     # install.packages('parallel')
# library(foreach)      # install.packages('foreach')
# library(doParallel)   # install.packages('doParallel')
# library(plyr)       # install.packages('plyr')
# library(rlist)      # install.packages('rlist')
library(zoo)
library(tidyr)        # install.packages('tidyr')
library(ggplot2)      # install.packages('ggplot2')
library(scales)       # install.packages('scales')
library(ggthemes)     # install.packages('ggthemes')
# Always load dplyr last!  
library(dplyr)        # install.packages('dplyr')

options(digits = 15) # So display of lat/lon isn't truncated

#----- 2. Set project-wide parameters -----
# The scripts run below will assume data is available in this directory, named according to the pattern described in section 1 above, eg E:/Data/zakupki/2015-06-13/zakupki-2015-06-13-raw-data
#data_directory <- "E:/Data/zakupki/" # When running off external HDD on laptop
data_directory <- "~/data/zakupki/" # When running on internal laptop HDD
#data_directory <- "/media/ext3tb/Data/zakupki/" # When running off external HDD on server
#data_directory <- "~/data-ssd/zakupki/" # When running on internal server HDD
#data_directory <- "../../SHAUN-HDD/Data/zakupki/" # When running ext HDD on an unrelated Mac
data_download_date <- "2015-06-13"
#setwd("~/git/shaun-mcgirr-dissertation/") # Only needed when running in batch on Linux

# What kind of documents to parse?
document_types_list <- as.list(c("contracts", "notifications"))

# Configure number of cores available (will default to 1 on Windows due to non-availability of easy parallelisation via library(parallel))
number_of_cores <- max(1, detectCores() - 1) # Can change last number to 1 or 2 depending on tolerance for responsiveness of other processes


# Are we running on a Mac/Linux machine, which allows for easy parallelisation (eg mclapply for lapply?)
# Probably not needed as mclapply can run with mc.cores = 1
# arch <- "Mac/Linux"
# arch <- "Windows"

# Set up R to display Russian characters as best it can on Windows (generally no problem on Mac/Linux)
Sys.setlocale("LC_CTYPE", "Russian") # This may not be necessary on Mac/Linux

#----- 3. Run scripts for each step -----
# source("3-Unpack//unzip-files.R") # unzips files from raw-data to unzipped-data
# source("3-Unpack//parse-files.R") # parses the unzipped xml files from unzipped-data to parsed-data

