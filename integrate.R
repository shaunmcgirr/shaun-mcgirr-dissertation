# This script runs all the scripts to process data, construct measures, estimate models, and output results

#----- 1. Download data, load functionality -----
# First download your data according to instructions here: https://github.com/shaunmcgirr/shaun-mcgirr-dissertation/blob/master/2-Obtain/download-raw-procurement-data.md

# Packages should generally be loaded in the individual scripts during development, then migrated here
#install.packages('xml2')
#install.packages('magrittr')
#install.packages('rJava') # You may strike problems here depending on your operating system. If so,
                           # Google is your friend! If running linux, install rJava from terminal
                           # instead with: sudo apt-get install r-cran-rjava
#install.packages('xlsx')
#install.packages('openxlsx')
#install.packages('plyr')
#install.packages('dplyr')
library(xml2)
library(magrittr)
# library(rJava) # rJava and xlsx deprecated over openxlsx below
# library(xlsx) # rJava and xlsx deprecated over openxlsx below
library(openxlsx)
library(plyr)
library(dplyr) # Always load last!

#----- 2. Set parameters -----
# The scripts run below will assume data is available in this directory, named according to the pattern described in section 1 above, eg E:/Data/zakupki/2015-06-13/zakupki-2015-06-13-raw-data
#data_directory <- "E:/Data/zakupki/"
data_directory <- "~/data-ssd/zakupki/"
data_download_date <- "2015-06-13"

#----- 3. Run scripts for each step -----
source("3-Unpack//unzip-files.R") # unzips files from raw-data to unzipped-data
source("3-Unpack//parse-files.R") # parses the unzipped xml files from unzipped-data to parsed-data

