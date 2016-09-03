# 4-Construct\build-regional-level-data.R

# Goals of this script are:
#   - Obtain list of regions which have completed purchase-level data build
#   - Load their agency-level measures
#   - Pull in other regional data from ICSID loaded in load-other-data.R

###################
# 1. Housekeeping #
###################

# Load functions
source(file="3-Unpack/parse-files-functions.R")
source(file="4-Construct/construct-measures-functions.R")

# Helpers for aggregation
mean_na <- function(x){mean(x, na.rm = T)}
median_na <- function(x){median(x, na.rm = T)}

# Load classifications
source(file="3-Unpack/load-classifications.R")

############################################
# 2. Gather parameters about the job ahead #
############################################

# Where does data come in from?
data_output_directory <- set_data_subdirectory(data_directory, data_download_date, "output")
data_purchases_directory <- set_data_subdirectory(data_directory, data_download_date, "purchases")
data_purchases_directory_regions <- paste0(data_purchases_directory, "regions/")

# Define where outputs should go
data_agencies_directory <- set_data_subdirectory(data_directory, data_download_date, "agencies")
data_agencies_directory_regions <- paste0(data_agencies_directory, "regions/")

# Obtain list of regions for which consolidated data is available
regions_in_database <- list.dirs(paste0(data_directory, data_download_date, "/zakupki-", data_download_date, "-output-data/94fz/"), full.names = F, recursive = F) %>%
  data_frame()
colnames(regions_in_database) <- c("reg_database")
# write.csv(regions_in_database, file = "2-Obtain/data_other/classifications/regions_in_database.csv", row.names = F)

# Join these regions to the classification derived from ICSID data
  # Too many differences in spelling!
# library(fuzzyjoin) # install.packages('fuzzyjoin')
# region_classification <- region_classification_raw %>%
#   stringdist_left_join(regions_in_database, by = c(reg_translit_short = "reg_database_short"), max_dist = 2)

# Hand-coded and saved as a csv to import
region_classification <- read.csv(file = "2-Obtain/data_other/classifications/regions_classification.csv", stringsAsFactors = F)
  rm(region_classification_raw); rm(regions_in_database)

#################################################
# 3. Load data, check quality, recode variables #
#################################################


  


  ###########################################################
  # 5. Save the dataframes containing measures to new files #
  ###########################################################

  # First specify variables to keep
  # agency_variables <- c("Year")

  #
  # save(agencies_all, file = "~/data/zakupki/2015-06-13/zakupki-2015-06-13-agencies-data/all_agencies_2015-06-13_compress.rda", compress = T)
  # 
  # rm(agencies_all); rm(agencies); gc()
  
  
  

# ENDS
