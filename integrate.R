# This script runs all the scripts to process data, construct measures, estimate models, and output results

#----- 1. Download data, load functionality -----
# First download your data according to instructions here:
# https://github.com/shaunmcgirr/shaun-mcgirr-dissertation/blob/master/2-Obtain/download-raw-procurement-data.md

# Packages are loaded here unless there is a special reason to load them in the scripts below
# install.packages('xml2')
# install.packages('magrittr')
# install.packages('openxlsx')
# install.packages('foreach')
# install.packages('doParallel')
# install.packages('plyr')
# install.packages('dplyr')
library(xml2)
library(magrittr)
library(openxlsx)
library(foreach)
library(doParallel)
library(plyr)
library(dplyr) # Always load last!

#----- 2. Set project-wide parameters -----
# The scripts run below will assume data is available in this directory, named according to the pattern described in section 1 above, eg E:/Data/zakupki/2015-06-13/zakupki-2015-06-13-raw-data
#data_directory <- "E:/Data/zakupki/" # When running off external HDD on laptop
data_directory <- "~/data/zakupki/" # When running on internal laptop HDD
#data_directory <- "/media/ext3tb/Data/zakupki/" # When running off external HDD on server
#data_directory <- "~/data-ssd/zakupki/" # When running on internal server HDD
data_download_date <- "2015-06-13"

# Set up R to display Russian characters as best it can on Windows (generally no problem on Mac/Linux)
Sys.setlocale("LC_CTYPE", "Russian") # This may not be necessary on Mac/Linux

#----- 3. Run scripts for each step -----
source("3-Unpack//unzip-files.R") # unzips files from raw-data to unzipped-data
source("3-Unpack//parse-files.R") # parses the unzipped xml files from unzipped-data to parsed-data

