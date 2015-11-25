# 3-Construct\parse-files-functions.R

# Goals of this script are:
#   - Provide functions to other parsing scripts to make their lives easier

###################
# 1. Housekeeping #
###################



#######################
# 2. Helper functions #
#######################

# Helper function for creating subdirectory paths
set_data_subdirectory <- function(data_directory, data_download_date, subdirectory){
  data_subdirectory <- paste0(data_directory, data_download_date, "/zakupki-", data_download_date, "-", subdirectory, "-data/94fz/")
  # return(data_subdirectory)
}

# Function to generate metadata about the regions to be processed
generate_regions_list <- function(data_subdirectory){
  regions_list <- as.list(list.files(path=data_subdirectory))
  regions_list <- Filter((function(x) !grepl('Sevastopol_g', x)), regions_list) # Remove Sevastopol
  # Filter out non-regions in to their own list, remove the log files from list of regions
  # others_list <- Filter((function(x) (x %in% c("_FCS_nsi", "nsi"))), regions_list)
}

# Function to make a list of the files inside a directory
generate_files_list <- function(directory){
  files_list <- as.list(list.files(directory, pattern="xml$", recursive=TRUE, full.names=TRUE))  
  # files_list_length <- length(files_list) 
  }





###########################
# 3. Processing functions #
###########################


