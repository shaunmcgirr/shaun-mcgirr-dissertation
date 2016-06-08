# 4-Construct\analyze-specific-product.R

# Goals of this script are:
#   - Create reusable framework for pulling out particular products
#   - Apply it to gasoline

###################
# 1. Housekeeping #
###################

# Load functions
source(file="3-Unpack/parse-files-functions.R")

# Specify directory where output data are stored (after construct-measures.R)
data_output_directory <- set_data_subdirectory(data_directory, data_download_date, "output")

############################################
# 2. Gather parameters about the job ahead #
############################################

# Obtain list of regions for which consolidated data is available
# regions_list <- generate_regions_list(data_parsed_directory)
regions_list <- as.list("Adygeja_Resp")
# regions_list <- as.list("Burjatija_Resp")
regions_number <- length(regions_list)

# Define target directory


##############################################
# 3. Define functions to process each region #
##############################################

# Wrap the code written below in to functions


##########################
# 4. Test case: gasoline #
##########################

# Prototype case gasoline: BusinessKey = 0176100000913000001
# http://zakupki.gov.ru/pgz/public/action/orders/info/common_info/show?notificationId=7647280
# oos:products/oos:product/oos:price = 34.17
# oos:products/oos:product/oos:quantity = 4000
# oos:products/oos:product/oos:sum = 136680
# oos:price = 136680
# For gasoline, may not want to limit to just contracts with a notification, may want to use all contracts?
                    
# Prototype case transport: BusinessKey = 0173100004511000790
# No notification...
# Contract is for times and distances
# 2 x oos:products/oos:product/oos:sum add to 450,000, same as oos:price
# according to fields_with_duplicates, oos:price doesn't have duplicates so should maybe just use that as the number
# should be fine as long as the product code is the same

# ENDS
