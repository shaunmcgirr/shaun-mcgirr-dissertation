# 3-Construct\unzip-files.R

# Goals of this script are:
#   - To loop through the downloaded files, unzipping them in to .xml on the fly

###################
# 1. Housekeeping #
###################

# Change in to the directory where downloaded data is stored (at the end of step 2)
data_raw_directory <- paste(data_directory, data_download_date, "/", "zakupki-", data_download_date, "-raw-data/94fz/", sep="")

# Define target directory for processed data
data_unzipped_directory <- paste(data_directory, data_download_date, "/", "zakupki-", data_download_date, "-unzipped-data/94fz/", sep="")


############################################
# 2. Gather parameters about the job ahead #
############################################

# Obtain list of regions for which downloaded data is available
regions_list <- as.list(list.files(path=data_raw_directory))

# Filter out non-regions in to their own list, remove the log files from list of regions
others_list <- Filter((function(x) (x %in% c("_FCS_nsi", "nsi"))), regions_list)
regions_list <- Filter((function(x) (!x %in% c("_FCS_nsi", "_readme.txt", "nsi"))), regions_list)
regions_list <- Filter((function(x) !grepl('.log', x)), regions_list)
regions_list <- Filter((function(x) !grepl('Krym_Resp', x)), regions_list) # Remove Crimea
regions_list <- Filter((function(x) !grepl('Sevastopol_g', x)), regions_list) # Remove Sevastopol
regions_number <- length(regions_list)


##############################################
# 3. Define functions to process each region #
##############################################

# Define a function to do all the hard work unzipping, taking two inputs: type of document and region
unzip_files <- function(document_type, current_region){
  from_directory <- paste(data_raw_directory, current_region, "/", document_type, sep="")
  to_directory <- paste(data_unzipped_directory, current_region, "/", document_type, sep="")
  dir.create(to_directory, recursive=TRUE)  
  files_list <- as.list(list.files(path=from_directory, pattern="zip$", recursive=TRUE, full.names=TRUE))
  files_list_length <- length(files_list)
  for (l in 1:files_list_length){
    unzip(as.character(files_list[l]), exdir=to_directory, overwrite=TRUE)
    print(paste(l, " of ", files_list_length, " files unzipped", sep=""))
  }
}


################################################
# 4. Loop over regions to process them in turn #
################################################

# List of document types to process
# document_types <- as.list(c("contracts", "notifications", "placement_result", "plans", "protocols"))
document_types <- as.list(c("contracts", "notifications", "protocols")) # only interested in these docs
document_types_number <- length(document_types)

# List of regions from 2. above is called here, looped through by region, then documents loop inside
for (r in 69:regions_number){ # Manually restarted after empty Crimea/Sevastopol directory stopped it
current_region <- as.character(regions_list[r])

  for (d in 1:document_types_number){
    document_type <- as.character(document_types[d])
    print(paste("Processing ", document_type, " documents from ", current_region, sep=""))
    unzip_files(document_type, current_region)
  }
}

# ENDS