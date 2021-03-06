# This script runs all the scripts to process data, construct measures, estimate models, and output results

#----- 1. Download data, load functionality -----
# First download your data according to instructions here:
# https://github.com/shaunmcgirr/shaun-mcgirr-dissertation/blob/master/2-Obtain/download-raw-procurement-data.md

# Packages are loaded here unless there is a special reason to load them in the scripts below
# Run this to install all packages in one go: source("install-packages.R")
library(xml2)         # install.packages('xml2')
library(magrittr)     # install.packages('magrittr')
library(readxl)       # install.packages('readxl')
library(openxlsx)     # install.packages('openxlsx')
library(parallel)     # install.packages('parallel')
library(lmerTest)     # install.packages("lmerTest") # Load this first so lme4 vanilla works first
library(lme4)         # install.packages('lme4')
library(betareg)      # install.packages('betareg')
library(devtools)     # install.packages("devtools") # Needed for latest fixes to some packages
# library(plm)          # install.packages('plm')
library(diverse)      # install.packages('diverse')
# library(foreach)      # install.packages('foreach')
# library(doParallel)   # install.packages('doParallel')
# library(plyr)       # install.packages('plyr')
# library(rlist)      # install.packages('rlist')
library(foreign)      # install.packages('foreign')
library(countrycode)  # install.packages('countrycode')
# library(WDI)          # install.packages('WDI')
library(zoo)          # install.packages('zoo')
library(xtable)       # install.packages('xtable')
library(stargazer)    # install.packages('stargazer')
library(ggplot2)      # install.packages('ggplot2')
# library(interplot)    # install.packages('interplot') # Version on CRAN does not handle mixed effects models
library(interplot)    # devtools::install_github("sammo3182/interplot")
library(scales)       # install.packages('scales')
library(ggthemes)     # install.packages('ggthemes')
library(tidyr)        # install.packages('tidyr')
# Always load dplyr last!  
library(dplyr)        # install.packages('dplyr')

# Helpers for aggregation
mean_na <- function(x){mean(x, na.rm = T)}
median_na <- function(x){median(x, na.rm = T)}
options(digits = 15) # So display of lat/lon isn't truncated
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]} # To convert factors to underlying numeric values
na_count <- function(x) sapply(x, function(y) sum(is.na(y)))

#----- 2. Set project-wide parameters -----
# The scripts run below will assume data is available in this directory, named according to the pattern described in section 1 above, eg E:/Data/zakupki/2015-06-13/zakupki-2015-06-13-raw-data
#data_directory <- "E:/Data/zakupki/" # When running off external HDD on laptop
data_directory <- "~/data/zakupki/" # When running on internal laptop HDD
#data_directory <- "/media/ext3tb/Data/zakupki/" # When running off external HDD on server
#data_directory <- "~/data-ssd/zakupki/" # When running on internal server HDD
#data_directory <- "../../SHAUN-HDD/Data/zakupki/" # When running ext HDD on an unrelated Mac
data_download_date <- "2015-06-13"
#setwd("~/git/shaun-mcgirr-dissertation/") # Only needed when running in batch on Linux
downloads_directory <- "~/Downloads/data/" # For data not tracked by Git

# What kind of documents to parse?
document_types_list <- as.list(c("contracts", "notifications", "protocols"))

# Configure number of cores available (will default to 1 on Windows due to non-availability of easy parallelisation via library(parallel))
number_of_cores <- max(1, detectCores() - 1) # Can change last number to 1 or 2 depending on tolerance for responsiveness of other processes

# Set up R to display Russian characters as best it can on Windows (generally no problem on Mac/Linux)
Sys.setlocale("LC_CTYPE", "Russian") # This may not be necessary on Mac/Linux

#----- 3. Run scripts for each step -----

# Other data
# source("3-Unpack//load-other-data.R") # Loads non-procurement data sources
# source("6-Present//scatterplots-GCB.R") # Draws the GCB scatterplots for introduction

# Procurement data
# source("3-Unpack//load-classifications.R") # Loads classifications used to parse procurement data
# source("3-Unpack//unzip-files.R") # unzips files from raw-data to unzipped-data
# source("3-Unpack//parse-files-functionwise.R") # parses the unzipped xml files from unzipped-data to parsed-data
# source("4-Construct//clean-data.R") # Cleans the parsed data (mostly deduplicating)
# source("4-Construct//pivot-and-merge.R") # Implement what we learned from analyze-before-merge.R
# source("4-Construct//extract-bidder-information.R") # Pull out number of bidders vs number admitted from protocols
# source("4-Construct//build-purchase-level-data.R") # Re-build data at purchase level with just the details needed for Chapter 4
# source("4-Construct//build-agency-level-data.R") # 
# source("4-Construct//build-regional-level-data.R") 
# source("4-Construct//construct-measures.R") # Builds measures for the procurement data at agency level
# source("4-Construct//prove-measures.R") # Explores and tests potential measures for Chapter 3
# source("5-Test//final-tests-chapter-4.R") # Full tests of the theory across agencies and regions, for Chapter 4
# source("6-Present//present-mixed-models.R") # Output nice looking tables for mixed effect models in Chapter 4
