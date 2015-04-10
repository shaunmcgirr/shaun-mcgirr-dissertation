# 4-Construct\construct-measures.R

# Goals of this script are:
#   - Obtain list of regions which have completed step 3 (unpack)
#   - Process these to create measures to test my theory
#   - Profit!!!

###################
# 1. Housekeeping #
###################

# Change in to the directory where consolidated data is stored (at the end of step 3)
# setwd('~/data/zakupki/2015-03-23/zakupki-2015-03-23-consolidated-data')
setwd('~/data/test02output')

############################################
# 2. Gather parameters about the job ahead #
############################################

# Obtain list of regions for which consolidated data is available
# Code goes here

# Define target of processed data
# Code goes here

##############################################
# 3. Define functions to process each region #
##############################################

# Wrap the code written below in to functions

################################################
# 4. Loop over regions to process them in turn #
################################################

# List from 2. above is called here
region.processed <- ("Moskva")

data.consolidated.notifications.filename <- paste(region.processed,"notifications","df","RData", sep=".")

load(data.consolidated.notifications.filename)

