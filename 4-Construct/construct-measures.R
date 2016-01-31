# 4-Construct\construct-measures.R

# Goals of this script are:
#   - Obtain list of regions which have completed step 3 (unpack)
#   - Process these to create measures to test my theory
#   - Profit!!!

###################
# 1. Housekeeping #
###################

# Change in to the directory where parsed data are stored (at the end of step 3)
# setwd('~/data/zakupki/2015-06-13/zakupki-2015-06-13-parsed-data/')
data_parsed_directory <- set_data_subdirectory(data_directory, data_download_date, "parsed")

############################################
# 2. Gather parameters about the job ahead #
############################################

# Obtain list of regions for which consolidated data is available
# regions_list <- generate_regions_list(data_parsed_directory)
regions_list <- as.list("Adygeja_Resp")
regions_number <- length(regions_list)

# Define target of processed data
data_constructed_directory <- set_data_subdirectory(data_directory, data_download_date, "constructed")

##############################################
# 3. Define functions to process each region #
##############################################

# Wrap the code written below in to functions

for(r in 1:regions_number){
  # r <- 1
  current_region <- as.character(regions_list[r])

  # Begin control loop over document types
  for(d in 1:length(document_types_list)){
    document_type <- "notifications"
    # document_type <- as.character(document_types_list[d])  
    
    file_to_process <- paste0(data_parsed_directory, current_region, "/", document_type, "/",
                              current_region, "_", document_type, "_parsed_key_value_",
                              data_download_date, ".rda")
    
    # Measure 1: what is the difference between notified and contracted price?
    

################################################
# 4. Loop over regions to process them in turn #
################################################

# List from 2. above is called here
region.processed <- ("Moskva")

data.consolidated.notifications.filename <- paste(region.processed,"notifications","df","RData", sep=".")

data.consolidated.notifications <- load(data.consolidated.notifications.filename)

