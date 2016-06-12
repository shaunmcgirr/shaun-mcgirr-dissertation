# 4-Construct\pivot-and-merge.R

# Goals of this script are:
#   - Obtain list of regions which have come from clean-data.R
#   - Implement what we learned in analyze-before-merge.R to deduplicate products
#   - Pivot long-format data to tidy wide-format data, ready to merge notifications and contracts

###################
# 1. Housekeeping #
###################

# Load functions
source(file="3-Unpack/parse-files-functions.R")
source(file="4-Construct/construct-measures-functions.R")

# Load classifications
source(file="3-Unpack/load-classifications.R")

# Define location of cleaned data
data_cleaned_directory <- set_data_subdirectory(data_directory, data_download_date, "cleaned")
# Define where outputs (eg pivoted data, graphs) should go
data_output_directory <- set_data_subdirectory(data_directory, data_download_date, "output")

# Load and process the configuration file that tells later functions what to process
parsing_configuration <- na.omit(read.xlsx(xlsxFile="3-Unpack/how-I-parse-the-xml.xlsx", 1))

############################################
# 2. Gather parameters about the job ahead #
############################################

# Obtain list of regions for which consolidated data is available
# regions_list <- generate_regions_list(data_parsed_directory)
regions_list <- as.list("Adygeja_Resp")
# regions_list <- as.list("Moskva")
regions_number <- length(regions_list)

##############################################
# 3. Define functions to process each region #
##############################################

# Wrap the code written below in to functions, as much as possible

# Loop over regions, processing them in turn
for(r in 1:regions_number){
  # r <- 1
  current_region <- as.character(regions_list[r])
  current_region_english <- generate_english_region_name(current_region)
  
  # Create output directory
  data_output_directory_region <- paste0(data_output_directory, current_region, "/")
    suppressWarnings(dir.create(data_output_directory_region, recursive = T))

  # Begin control loop over document types to load both datasets for the region
  for(d in 1:length(document_types_list)){
      # document_type <- "notifications"; # document_type <- "contracts"
    document_type <- as.character(document_types_list[d])
    
    # Load the next file to process (could be a useful function)
    file_to_process <- paste0(data_cleaned_directory, current_region, "/",
                              current_region, "_", document_type, "_cleaned_",
                              data_download_date, ".rda")
    load(file=file_to_process)
  } # Closes control loop over document types loaded

  # REMOVE Keys found to be of no value (could also be moved to clean-data.R, would save memory)
  notifications_cleaned <- notifications_cleaned %>% filter(Key != "oos:lots/oos:lot/oos:notificationFeatures/oos:notificationFeature/oos:placementFeature/oos:name")
  contracts_cleaned <- contracts_cleaned %>% filter(BusinessKey != "")
  
  ###################
  ## Notifications ##
  ###################
  
  # ADD LEVEL 1 OF OKDP PRODUCT CLASSIFICATION (PAUSED FOR NOW)
  # notifications_cleaned_okdp_level_1 <- notifications_cleaned %>%
  #                                         filter(Key == "oos:lots/oos:lot/oos:products/oos:product/oos:code") %>%
  #                                         left_join(okdp_product_classification,
  #                                                   by = c("Value" = "ProductCode")) %>%
  #                                         mutate(Key = "NotificationLotProductCodeLevel1",
  #                                                Value = ProductCodeLevel1) %>%
  #                                         select(BusinessKey, DocumentVersionUniqueID, Key, Value)

  # SUBSET DATA TO JUST NOTIFICATIONS/CONTRACTS WITH ONE LOT/PRODUCT/CUSTOMER
  notifications_cleaned_unique_business_keys_number <- length(unique(notifications_cleaned$BusinessKey))
  
  # Identify and assess size of multiple-lot notifications
  notifications_lots_number <- notifications_cleaned %>%
                                filter(Key == "oos:lots/oos:lot/oos:ordinalNumber") %>%
                                group_by(BusinessKey) %>%
                                summarize(NumberOfLots = as.numeric(max(Value)))
  # table(notifications_lots_number$NumberOfLots)
  
  # Subset to just the single-lot notifications
  notifications_with_one_lot <- notifications_lots_number %>% filter(NumberOfLots == 1) %>% select(BusinessKey)
  notifications_single_lot <- notifications_cleaned %>%
                                right_join(notifications_with_one_lot, by = "BusinessKey")
  rm(notifications_lots_number); rm(notifications_with_one_lot); rm(notifications_cleaned)
  
  # Identify and assess size of multiple-product notifications (within single-lot subset)
  notifications_products_number <- notifications_single_lot %>%
                                    filter(Key == "oos:lots/oos:lot/oos:products/oos:product/oos:code") %>%
                                    group_by(BusinessKey) %>%
                                    summarize(NumberOfProducts = n(), NumberOfUniqueProducts = n_distinct(Value))

  notifications_with_one_lot_one_product <- notifications_products_number %>% filter(NumberOfProducts == 1) %>% select(BusinessKey)
  notifications_single_lot_single_product <- notifications_single_lot %>%
                                              right_join(notifications_with_one_lot_one_product,
                                                         by = "BusinessKey")
  rm(notifications_products_number); rm(notifications_with_one_lot_one_product); rm(notifications_single_lot)
  
  # Final restriction to the simplest notfications (see analyze-before-merge.R for more detail)
  notifications_with_one_lot_one_product_one_customer <- notifications_single_lot_single_product %>%
                                                          group_by(BusinessKey, Key) %>%
                                                            summarise(ValuesPerKey = n()) %>% ungroup() %>%
                                                          group_by(BusinessKey) %>%
                                                            summarise(MaxFieldsPerNotification = max(ValuesPerKey)) %>% ungroup() %>%
                                                          filter(MaxFieldsPerNotification == 1) %>%
                                                          select(BusinessKey)
                                                          
  notifications_single_lot_single_product_single_customer <- notifications_single_lot_single_product %>%
                                                              right_join(notifications_with_one_lot_one_product_one_customer, 
                                                                         by = "BusinessKey")
  rm(notifications_with_one_lot_one_product_one_customer); rm(notifications_single_lot_single_product)
  
  ## PIVOT THE DEDUPLICATED DATA IN TO ONE ROW PER NOTIFICATION, SAVE
  notifications <- notifications_single_lot_single_product_single_customer %>%
                    spread(key = Key, value = Value)
  rm(notifications_single_lot_single_product_single_customer)
  
  # Rename variables sensibly
  column_names_notifications_old <- data.frame(XMLFieldName = colnames(notifications), stringsAsFactors = F)
  column_names_notifications_new <- parsing_configuration %>%
                                      filter(DocumentType == "notifications") %>%
                                      select(XMLFieldName, VariableName) %>%
                                      right_join(column_names_notifications_old, by = "XMLFieldName") %>%
                                      mutate(VariableName = ifelse(is.na(VariableName), XMLFieldName,
                                                                   VariableName)) %>%
                                      select(VariableName) 
  colnames(notifications) <- column_names_notifications_new$VariableName
  rm(column_names_notifications_new); rm(column_names_notifications_old)
  
  # Output proportion of tidy notifications from all parsed
  tidy_notifications_percentage <- length(unique(notifications$BusinessKey))/notifications_cleaned_unique_business_keys_number
  print(paste0("Proportion of tidy notifications in ", current_region, " is ", tidy_notifications_percentage))
  
  # Save the 'wide' format file with a sensible name and clean up
  notifications_wide_file_name <- paste0(data_output_directory_region, current_region,
                                         "_notifications_wide_", data_download_date, ".rda")
  save(notifications, file = notifications_wide_file_name)
  # rm(notifications)
      
  ###################
  ## Contracts     ##
  ###################    
  
  # ADD LEVEL 1 OF OKDP PRODUCT CLASSIFICATION (PAUSED FOR NOW)
  # Adapt code from notifications above
  
  # SUBSET DATA TO JUST NOTIFICATIONS/CONTRACTS WITH ONE LOT/PRODUCT/CUSTOMER
  contracts_cleaned_unique_business_keys_number <- length(unique(contracts_cleaned$BusinessKey))
  
  # How many products and unique products per contract?
  contracts_products_combined <- contracts_cleaned %>%
                                   filter(Key == "oos:products/oos:product/oos:OKDP/oos:code") %>%
                                   group_by(BusinessKey) %>%
                                   summarize(NumberOfProducts = n(), NumberOfUniqueProducts = n_distinct(Value))

  # Subset to just the single-product (code) contracts  
  contracts_with_one_product <- contracts_products_combined %>% filter(NumberOfUniqueProducts == 1) %>% select(BusinessKey)
  contracts_single_product <- contracts_cleaned %>%
                                right_join(contracts_with_one_product, by = "BusinessKey")
  rm(contracts_products_combined); rm(contracts_with_one_product); rm(contracts_cleaned)
  
  # Count fields per business key (to check for remaining duplicate attributes)
  fields_per_business_key <- contracts_single_product %>%
                              group_by(BusinessKey, Key) %>%
                              summarise(KeysPerBusinessKey = n())
  
  # Examine which fields from notifications file are duplicated
  fields_with_duplicates <- fields_per_business_key %>%
                              filter(KeysPerBusinessKey > 1) %>%
                              group_by(Key) %>%
                              summarise(NumberOfTimesDuplicated = n())
  rm(fields_per_business_key)
  
  # Start with contracts having single unique product code, extract one instance of the following fields from each:
  #   oos:products/oos:product/oos:OKDP/oos:code
  #   oos:products/oos:product/oos:OKDP/oos:name

  # Drop the following fields (because can have multiple different values for same product, eg time & distance for transport)
  #   oos:finances/oos:budgetary/oos:KBK
  #   oos:finances/oos:budgetary/oos:price
  #   oos:finances/oos:extrabudgetary/oos:KOSGU
  #   oos:finances/oos:extrabudgetary/oos:price
  #   oos:products/oos:product/oos:OKEI/oos:code
  #   oos:products/oos:product/oos:OKEI/oos:name 
  #   oos:products/oos:product/oos:price
  #   oos:products/oos:product/oos:quantity
  #   oos:products/oos:product/oos:sum
  #   oos:suppliers/oos:supplier/oos:inn
  #   oos:suppliers/oos:supplier/oos:kpp
  #   oos:suppliers/oos:supplier/oos:organizationName
  #   oos:suppliers/oos:supplier/oos:participantType
  duplicated_contract_fields_to_drop <- fields_with_duplicates %>%
                                          filter(!Key %in% c("oos:products/oos:product/oos:OKDP/oos:code",
                                                             "oos:products/oos:product/oos:OKDP/oos:name")) %>%
                                          select(Key) %>% as.list()
  rm(fields_with_duplicates)
  
  contracts_with_one_product_fields_collapsed <- contracts_single_product %>%
                                                  filter(!Key %in% duplicated_contract_fields_to_drop[[1]]) %>%
                                                  group_by(BusinessKey, DocumentVersionUniqueID, Key) %>%
                                                    filter(row_number() == 1) %>% # Leaves one instance of OKDP code/name per contract
                                                    summarise(ValuesPerKey = n()) %>% ungroup() %>% 
                                                  group_by(BusinessKey) %>%
                                                    summarise(MaxFieldsPerContract = max(ValuesPerKey)) %>% ungroup() %>%
                                                  filter(MaxFieldsPerContract == 1) %>% # Might as well be defensive
                                                  select(BusinessKey)
  
  contracts_single_product_fields_collapsed <- contracts_single_product %>%
                                                filter(!Key %in% duplicated_contract_fields_to_drop[[1]]) %>%
                                                group_by(BusinessKey, DocumentVersionUniqueID, Key) %>%
                                                  filter(row_number() == 1) %>% ungroup() %>%
                                                right_join(contracts_with_one_product_fields_collapsed, 
                                                           by = "BusinessKey")
  rm(contracts_single_product); rm(contracts_with_one_product_fields_collapsed)
  
  ## PIVOT THE DEDUPLICATED DATA IN TO ONE ROW PER NOTIFICATION
  contracts <- contracts_single_product_fields_collapsed %>%
                spread(key = Key, value = Value)
  rm(contracts_single_product_fields_collapsed)
  
  # Rename variables sensibly
  column_names_contracts_old <- data.frame(XMLFieldName = colnames(contracts), stringsAsFactors = F)
  column_names_contracts_new <- parsing_configuration %>%
                                  filter(DocumentType == "contracts") %>%
                                  select(XMLFieldName, VariableName) %>%
                                  right_join(column_names_contracts_old, by = "XMLFieldName") %>%
                                  mutate(VariableName = ifelse(is.na(VariableName), XMLFieldName,
                                                               VariableName)) %>%
                                  select(VariableName) 
  colnames(contracts) <- column_names_contracts_new$VariableName
  rm(column_names_contracts_new); rm(column_names_contracts_old)
  
  # Output proportion of tidy contracts from all parsed
  tidy_contracts_percentage <- length(unique(contracts$BusinessKey))/contracts_cleaned_unique_business_keys_number
  print(paste0("Proportion of tidy contracts in ", current_region, " is ", tidy_contracts_percentage))
  
  # Save the 'wide' format file with a sensible name and clean up
  contracts_wide_file_name <- paste0(data_output_directory_region, current_region,
                                     "_contracts_wide_", data_download_date, ".rda")
  save(contracts, file = contracts_wide_file_name)
  # rm(contracts)
  

  ###################
  ## Merge         ##
  ###################  
  
  notifications_number <- length(unique(notifications$BusinessKey))
  contracts_number <- length(unique(contracts$BusinessKey))
  
  # contracts_per_notification <- notifications %>%
  #                                 left_join(contracts, by = c("NotificationNumber" = "ContractFoundationNotificationNumber")) %>%
  #                                 group_by(NotificationNumber, ContractID) %>%
  #                                   summarize(ContractsPerNotification = n())
  
  notification_contract_matches <- notifications %>% 
                                    inner_join(contracts, by = c("NotificationNumber" = "ContractFoundationNotificationNumber"))
  
  notifications_without_contract <- notifications %>%
                                      anti_join(contracts, by = c("NotificationNumber" = "ContractFoundationNotificationNumber"))
  # Many reasons, including cancellation of contract, or external control eg 0876100001413000009
  
  contracts_without_notification <- contracts %>%
                                      anti_join(notifications, by = c("ContractFoundationNotificationNumber" = "NotificationNumber"))
  # Most of these due to multiple-lot auction but single contract Eg 0376300005311000009
  
  # Check that the merge went as expected
  all_notification_numbers <- c(notification_contract_matches$NotificationNumber,
                                notifications_without_contract$NotificationNumber,
                                contracts_without_notification$ContractFoundationNotificationNumber)
  if(length(all_notification_numbers) != length(unique(all_notification_numbers))) print("Merge failed, investigate")
  rm(all_notification_numbers)
  
  # Report statistics on matching, append to file matching_statistic.csv
  number_of_cases <- length(notification_contract_matches$NotificationNumber) + length(notifications_without_contract$NotificationNumber) + length(contracts_without_notification$ContractFoundationNotificationNumber)
  
  notification_contract_matches_proportion <- c(current_region, format(Sys.Date(), "%Y-%m-%d"),
                                                "Notifications with matching contract",
                                                length(notification_contract_matches$NotificationID),
                                                length(notification_contract_matches$NotificationID)/number_of_cases)                                              
  # print(paste0("Notifications with matching contract: ", notification_contract_matches_proportion))
  notifications_without_contract_proportion <- c(current_region, format(Sys.Date(), "%Y-%m-%d"),
                                                 "Notifications without matching contract",
                                                 length(notifications_without_contract$NotificationID),
                                                 length(notifications_without_contract$NotificationID)/number_of_cases)
  # print(paste0("Notifications without matching contract: ", notifications_without_contract_proportion))
  contracts_without_notification_proportion <- c(current_region, format(Sys.Date(), "%Y-%m-%d"),
                                                 "Contracts without matching notification",
                                                 length(contracts_without_notification$ContractFoundationNotificationNumber),
                                                 length(contracts_without_notification$ContractFoundationNotificationNumber)/number_of_cases)
  # print(paste0("Contracts without matching notification: ", contracts_without_notification_proportion))
  # notification_contract_matches_proportion + notifications_without_contract_proportion + contracts_without_notification_proportion
  matching_statistics <- (rbind.data.frame(notification_contract_matches_proportion,
                               notifications_without_contract_proportion,
                               contracts_without_notification_proportion))
  colnames(matching_statistics) <- c("Region", "Date", "Type", "Count", "Proportion")
  suppressWarnings(write.table(matching_statistics, file = "4-Construct/matching_statistics.csv",
                               sep = ",", row.names = F, append = T, col.names = F))
  rm(matching_statistics)
  
  # Save files of matches/non-matches with sensible names and clean up this region
  matches_file_name <- paste0(data_output_directory_region, current_region,
                              "_notification_contract_matches_", data_download_date, ".rda")
    save(notification_contract_matches, file = matches_file_name)
  notifications_without_contract_file_name <- paste0(data_output_directory_region, current_region, 
                                                     "_notifications_without_contract_", data_download_date, ".rda")
    save(notifications_without_contract, file = notifications_without_contract_file_name)
  contracts_without_notification_file_name <- paste0(data_output_directory_region, current_region, 
                                                     "_contracts_without_notification", data_download_date, ".rda")
    save(contracts_without_notification, file = contracts_without_notification_file_name)
    
  rm(contracts); rm(contracts_without_notification)
  rm(notifications); rm(notifications_without_contract)
  
    
} # Closes control loop over this region

# ENDS