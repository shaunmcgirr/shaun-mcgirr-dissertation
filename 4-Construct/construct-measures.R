# 4-Construct\construct-measures.R

# Goals of this script are:
#   - Obtain list of regions which have completed step 3 (unpack)
#   - Process these to create measures to test my theory
#   - Profit!!!

###################
# 1. Housekeeping #
###################

# Load functions
source(file="3-Unpack/parse-files-functions.R")

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

# Define target of cleaned data
data_cleaned_directory <- set_data_subdirectory(data_directory, data_download_date, "cleaned")

##############################################
# 3. Define functions to process each region #
##############################################

# Wrap the code written below in to functions

for(r in 1:regions_number){
  # r <- 1
  current_region <- as.character(regions_list[r])

  # Begin control loop over document types
  for(d in 1:length(document_types_list)){
      # document_type <- "notifications"; # document_type <- "contracts"
    document_type <- as.character(document_types_list[d])
    
    # Load the next file to process (could be a useful function)
    file_to_process <- paste0(data_cleaned_directory, current_region, "/",
                              current_region, "_", document_type, "_cleaned_",
                              data_download_date, ".rda")
    load(file=file_to_process)
  } # Closes control loop over document types loaded
    
  # Distributions of the raw variables of interest
  notifications_maxPrice <- notifications_cleaned %>% 
    filter(Key == "oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:maxPrice") %>%
    mutate(NotificationMaxPrice = as.numeric(Value)) %>%
    select(NotificationMaxPrice)
  hist(log(notifications_maxPrice$NotificationMaxPrice), breaks = 100, col=rgb(1, 0, 0, 0.5))
  
  notifications_maxPrice_three_quartiles <- notifications_maxPrice %>%
    filter(NotificationMaxPrice <= as.numeric(summary(notifications_maxPrice$NotificationMaxPrice)[5]))
  hist((notifications_maxPrice_three_quartiles$NotificationMaxPrice), breaks = 100)
  # A lot of notifications right below 500,000 rubles, see http://www.otc.ru/academy/articles/Kak_rabotat_s_zakupkami

  contracts_price <- contracts_cleaned %>% 
    filter(Key == "oos:price") %>%
    mutate(ContractsPrice = as.numeric(Value)) %>%
    select(ContractsPrice)
  hist(log(contracts_price$ContractsPrice), breaks = 100, col=rgb(0, 0, 1, 0.5), add=T)  
  
    # Measure 1: what is the difference between notified and contracted price?
    notifications <- notifications_cleaned %>%
      filter(Key == "oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:maxPrice") %>%
      group_by(BusinessKey) %>%
      summarise(NotificationTotalMaxPrice = sum(as.numeric(Value)))
    
    contracts <- contracts_cleaned %>%
      filter(Key == "oos:price") %>%
      group_by(BusinessKey) %>%
      summarise(ContractTotalPrice = sum(as.numeric(Value)))
      
    contracts_per_notification <- notifications %>%
      left_join(contracts, by = "BusinessKey") # Same length as notifications
    
    notifications_per_contract <- contracts %>%
      left_join(notifications, by = "BusinessKey") # Same length as contracts
    
    notifications_and_contracts <- notifications %>%
      full_join(contracts, by = "BusinessKey") %>%
      mutate(NotificationMaxPriceMinusContractPrice = NotificationTotalMaxPrice - ContractTotalPrice,
             ContractPriceMinusNotificationMaxPrice = ContractTotalPrice - NotificationTotalMaxPrice,
             AuctionEfficiency = (ContractTotalPrice/NotificationTotalMaxPrice*100))
    hist(log(notifications_and_contracts$NotificationMaxPriceMinusContractPrice), breaks = 100)
    hist((notifications_and_contracts$AuctionEfficiency), breaks = 500)
    
    
    
  } # Closes control loop over document_types_list in this region
  
} # Closes control loop over regions_list
                    
    
################################################
# 4. ? #
################################################

# ENDS