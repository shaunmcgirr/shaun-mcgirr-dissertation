# 3-Construct\load-other-data.R

# Goals of this script are:
#   - Load and parse the data that comes from sources outside the procurement dataset

###################
# 1. Housekeeping #
###################

# Load functions

# Tell R where unzipped data is stored (at the end of 3-Unpack/unzip-files.R) and where to send it
data_other_directory <- "2-Obtain/data_other/"

############################################
# 2. Gather parameters about the job ahead #
############################################

# Gather metadata about the data to be loaded
data_other_list <- as.list(c("clearspending-regional-budgets"))
data_other_number <- length(data_other_list)

###############################################
# 3. Load clearspending regional budgets data #
###############################################

# Note, files are semicolon-separated and in UTF-8 format
clearspending_regional_budgets_years <- c("2011", "2012", "2013", "2014", "2015")
clearspending_regional_budgets_filenames <- paste0("clearspending-regional-budgets-",
                                                   clearspending_regional_budgets_years,
                                                   ".csv")

test_import <- read.csv(file = "2-Obtain/data_other/clearspending/clearspending-regional-budgets-2015.csv",
                        sep = ";", encoding = "UTF-8", strip.white = T)
# Need to run over columns and overwrite without spaces 
# http://stackoverflow.com/questions/5992082/how-to-remove-all-whitespace-from-a-string

# ENDS