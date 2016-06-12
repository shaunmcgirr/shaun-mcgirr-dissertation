# 3-Construct\load-classifications.R

# Goals of this script are:
#   - Load and parse the official statistical classifications 

###################
# 1. Housekeeping #
###################

# Load functions

# Tell R where unzipped data is stored (at the end of 3-Unpack/unzip-files.R) and where to send it
data_classifications_directory <- "2-Obtain/data_other/classifications"
# Recall that integrate.R defines a "downloads_directory" for files not tracked by Git

############################################
# 2. Gather parameters about the job ahead #
############################################

# Gather metadata about the data to be loaded
# data_other_list <- as.list(c("clearspending-regional-budgets"))
# data_other_number <- length(data_other_list)

############################################
# 3. Load ОКПД/OKDP product classification #
############################################

okdp_product_classification <- read.csv(file = "2-Obtain/data_other/classifications/ОКДП.csv",
                                        header = T, stringsAsFactors = F, encoding = "utf8",
                                        colClasses = "character") %>%
                                rename(ProductCode = Код, ProductName = Наименование.группировки) %>%
                                filter(ProductCode != "")
  
# ENDS