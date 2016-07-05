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
# regions_list <- generate_regions_list(data_cleaned_directory)
# regions_list <- as.list("Adygeja_Resp")
regions_list <- as.list(c("Moskva", "Moskovskaja_obl"))
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
  
  # How many total cases are there, and how many matches?
  notifications_cleaned_unique_business_keys <- data.frame(Notification = "Notification", BusinessKey = unique(notifications_cleaned$BusinessKey), stringsAsFactors = F)
    notifications_cleaned_unique_business_keys_number <- length(notifications_cleaned_unique_business_keys$BusinessKey)
  contracts_cleaned_unique_business_keys <- data.frame(Contract = "Contract", BusinessKey = unique(contracts_cleaned$BusinessKey), stringsAsFactors = F)
    contracts_cleaned_unique_business_keys_number <- length(contracts_cleaned_unique_business_keys$BusinessKey)
  
  # Create a "spine" of all the business keys in the data, and note whether they are matches, or which document is missing
  all_business_keys <- full_join(notifications_cleaned_unique_business_keys, contracts_cleaned_unique_business_keys, by = "BusinessKey") %>%
                          mutate(NotificationMissingReason = as.character(NA),
                                 ContractMissingReason = as.character(NA),
                                 Match = ifelse(!is.na(Notification) & !is.na(Contract), "Notification matches contract", 
                                                ifelse(!is.na(Notification) & is.na(Contract), "Notification without contract", 
                                                       ifelse(is.na(Notification) & !is.na(Contract), "Contract without notification", NA)))) %>%
                          select(BusinessKey, Match, NotificationMissingReason, ContractMissingReason)
  # table(all_business_keys$Match)    
  if(sum(is.na(all_business_keys$Match)) != 0) print("Merge failed, investigate")
  all_business_keys_length <- length(all_business_keys$BusinessKey)
  
  # Cut down notifications and contracts to a dataframe containing just the data universal to every business key, regardless of filtering below
  notifications_universal <- notifications_cleaned %>%
                              filter(Key %in% c("oos:id", "oos:notificationNumber", "oos:versionNumber", "oos:publishDate",
                                                "oos:placingWay/oos:name", "oos:orderName", "oos:order/oos:placer/oos:regNum",
                                                "oos:order/oos:placer/oos:fullName", "oos:href")) %>%
                              select(-DocumentVersionUniqueID) %>%
                              spread(key = Key, value = Value)
  if(length(notifications_universal$BusinessKey) != length(notifications_cleaned_unique_business_keys$BusinessKey)) print("Merge failed, investigate")
  
  # Rename variables sensibly
  column_names_notifications_old <- data.frame(XMLFieldName = colnames(notifications_universal), stringsAsFactors = F)
  column_names_notifications_new <- parsing_configuration %>%
    filter(DocumentType == "notifications") %>%
    select(XMLFieldName, VariableName) %>%
    right_join(column_names_notifications_old, by = "XMLFieldName") %>%
    mutate(VariableName = ifelse(is.na(VariableName), XMLFieldName,
                                 VariableName)) %>%
    select(VariableName) 
  colnames(notifications_universal) <- column_names_notifications_new$VariableName
  rm(column_names_notifications_new); rm(column_names_notifications_old)
  
  contracts_universal <- contracts_cleaned %>%
                          filter(Key %in% c("oos:id", "oos:regNum", "oos:versionNumber", "oos:publishDate", "oos:signDate",
                                            "oos:foundation/oos:order/oos:notificationNumber", "oos:foundation/oos:singleCustomer",
                                            "oos:customer/oos:regNum", "oos:customer/oos:fullName", "oos:customer/oos:inn", 
                                            "oos:customer/oos:kpp", "oos:customer/oos:tofk", "oos:price", "oos:currency/oos:code",
                                            "oos:execution/oos:month", "oos:execution/oos:year", "oos:finances/oos:financeSource",
                                            "oos:currentContractStage", "oos:finances/oos:budget/oos:code",
                                            "oos:finances/oos:budget/oos:name", "oos:finances/oos:extrabudget/oos:name")) %>%
                          select(-DocumentVersionUniqueID) %>%
                          spread(key = Key, value = Value)
  if(length(contracts_universal$BusinessKey) != length(contracts_cleaned_unique_business_keys$BusinessKey)) print("Merge failed, investigate")
  
  # Rename variables sensibly
  column_names_contracts_old <- data.frame(XMLFieldName = colnames(contracts_universal), stringsAsFactors = F)
  column_names_contracts_new <- parsing_configuration %>%
    filter(DocumentType == "contracts") %>%
    select(XMLFieldName, VariableName) %>%
    right_join(column_names_contracts_old, by = "XMLFieldName") %>%
    mutate(VariableName = ifelse(is.na(VariableName), XMLFieldName,
                                 VariableName)) %>%
    select(VariableName) 
  colnames(contracts_universal) <- column_names_contracts_new$VariableName
  rm(column_names_contracts_new); rm(column_names_contracts_old)
  
  # Join together this universal data
  notifications_contracts <- all_business_keys %>%
                              left_join(notifications_universal, by = "BusinessKey") %>%
                              left_join(contracts_universal, by = "BusinessKey")
  rm(all_business_keys); rm(notifications_cleaned_unique_business_keys); rm(contracts_cleaned_unique_business_keys); rm(notifications_universal); rm(contracts_universal); gc();
    
  ###################
  ## Notifications ##
  ###################
  
  # SUBSET DATA TO JUST NOTIFICATIONS/CONTRACTS WITH ONE LOT/PRODUCT/CUSTOMER
  # (merging those with multiple lots/product/customer in to notifications_contracts as we go)
  
  # Identify and assess size of multiple-lot notifications
  notifications_lots_number <- notifications_cleaned %>%
                                filter(Key == "oos:lots/oos:lot/oos:ordinalNumber") %>%
                                group_by(BusinessKey) %>%
                                summarize(NumberOfLots = as.numeric(max(Value)))
  # table(notifications_lots_number$NumberOfLots)
  
  # Which notifications are for multiple lots (and so have to be dropped later)?
  notifications_with_multiple_lots <- notifications_lots_number %>%
                                        filter(NumberOfLots > 1) %>%
                                        select(BusinessKey) %>%
                                        mutate(NotificationMissingReason = "Notification with multiple lots")
  # Send these to master DF
  notifications_contracts <- notifications_contracts %>% 
                              left_join(notifications_with_multiple_lots, by = c("BusinessKey")) %>%
                              rename(NotificationMissingReason = NotificationMissingReason.x) %>%
                              mutate(NotificationMissingReason = ifelse(is.na(NotificationMissingReason.y), NotificationMissingReason, NotificationMissingReason.y)) %>%
                              select(-NotificationMissingReason.y)
  
  # Subset to just the single-lot notifications
  notifications_with_one_lot <- notifications_lots_number %>% filter(NumberOfLots == 1) %>% select(BusinessKey)
  notifications_single_lot <- notifications_cleaned %>%
                                right_join(notifications_with_one_lot, by = "BusinessKey")
  rm(notifications_lots_number); rm(notifications_with_one_lot);
  
  # ADD LEVEL 1 OF OKDP PRODUCT CLASSIFICATION
  notifications_cleaned_okdp_level_1 <- notifications_cleaned %>%
                                          filter(Key == "oos:lots/oos:lot/oos:products/oos:product/oos:code") %>%
                                          left_join(okdp_product_classification,
                                            by = c("Value" = "ProductCode")) %>%
                                          mutate(Key = "NotificationLotProductCodeLevel1",
                                             Value = ProductCodeLevel1) %>%
                                          select(BusinessKey, DocumentVersionUniqueID, Key, Value)
  # Add these new lines to single-lot
  notifications_single_lot <- rbind(notifications_single_lot, notifications_cleaned_okdp_level_1)
  rm(notifications_cleaned_okdp_level_1); rm(notifications_cleaned); gc();
  
  # Now identify multiple-customer notifications
  # customer_keys_with_duplicates <- notifications_single_lot %>% group_by(BusinessKey, Key) %>% summarise(ValuesPerKey = n()) %>% group_by(Key) %>% summarise(MaxValuesPerKey = max(ValuesPerKey))
  notifications_with_one_lot_multiple_customers <- notifications_single_lot %>% 
                                                    filter(Key %in% c("oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:customer/oos:regNum")) %>%
                                                    group_by(BusinessKey) %>%
                                                      summarise(ValuesPerKey = n()) %>% ungroup() %>%
                                                    filter(ValuesPerKey > 1) %>%
                                                    select(BusinessKey) %>%
                                                    mutate(NotificationMissingReason = "Notification with single lot, multiple customers")
  
  # Send these off to master data frame
  notifications_contracts <- notifications_contracts %>%
                              left_join(notifications_with_one_lot_multiple_customers, by = c("BusinessKey")) %>%
                              rename(NotificationMissingReason = NotificationMissingReason.x) %>%
                              mutate(NotificationMissingReason = ifelse(is.na(NotificationMissingReason.y), NotificationMissingReason, NotificationMissingReason.y)) %>%
                              select(-NotificationMissingReason.y)
  # table(notifications_contracts$NotificationMissingReason, useNA = "always")
  
  notifications_with_one_lot_one_customer <- notifications_single_lot %>%
    filter(Key %in% c("oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:customer/oos:regNum")) %>%
    group_by(BusinessKey) %>%
      summarise(ValuesPerKey = n()) %>% ungroup() %>%
    filter(ValuesPerKey == 1) %>%
    select(BusinessKey)

  # Check our cases so far add up to total notifications
  if(length(notifications_with_one_lot_one_customer$BusinessKey) + length(notifications_with_one_lot_multiple_customers$BusinessKey) + length(notifications_with_multiple_lots$BusinessKey) != notifications_cleaned_unique_business_keys_number) print("Merge failed, investigate")

  # Subset single lot to those with one customer
  notifications_single_lot_single_customer <- notifications_single_lot %>%
                                                right_join(notifications_with_one_lot_one_customer, by = "BusinessKey")
  # table(notifications_product_grouped$NotificationMissingReason, useNA = "always")
  rm(notifications_single_lot); rm(notifications_with_one_lot_one_customer)
    
  #
  # Easiest to create two DFs from here: one for each product level
  # notifications_contracts_products_grouped & notifications_contracts_products_ungrouped
  #
  
  ## FIRST GROUPED PRODUCTS
  
  # Identify and assess size of multiple-product notifications at level 1 (within single-lot, single-customer subset)
  notifications_products_number_level_1 <- notifications_single_lot_single_customer %>%
                                            filter(Key == "NotificationLotProductCodeLevel1") %>%
                                            group_by(BusinessKey) %>%
                                              summarize(NumberOfProducts = n(), 
                                                        NumberOfUniqueProducts = n_distinct(Value))
  
  # Send these off to the master data frame with some metadata (for the rest, use NumberOfUniqueProducts and sum across oos:customerRequirement/oos:maxPrice)
  notifications_with_multiple_products_level_1 <- notifications_products_number_level_1 %>%
                                                    filter(NumberOfUniqueProducts > 1) %>%
                                                    transmute(BusinessKey = BusinessKey, NotificationMissingReason = "Notification with single lot, single customer, multiple products (grouped)")
  
  notifications_contracts_products_grouped <- notifications_contracts %>%
                                                left_join(notifications_with_multiple_products_level_1, by = c("BusinessKey")) %>%
                                                rename(NotificationMissingReason = NotificationMissingReason.x) %>%
                                                mutate(NotificationMissingReason = ifelse(is.na(NotificationMissingReason.y), NotificationMissingReason, NotificationMissingReason.y)) %>%
                                                select(-NotificationMissingReason.y)
  # table(notifications_contracts_products_grouped$NotificationMissingReason, useNA = "always")
  
  # Identify remaining records, sum over maxPrice (sum not actually necessary)
  notifications_with_one_product_level_1 <- notifications_products_number_level_1 %>%
                                              filter(NumberOfUniqueProducts == 1) %>%
                                              select(BusinessKey)
  
  # Check our cases so far add up to total notifications
  if(length(unique(notifications_with_one_product_level_1$BusinessKey)) + length(unique(notifications_with_multiple_products_level_1$BusinessKey)) + length(notifications_with_one_lot_multiple_customers$BusinessKey) + length(notifications_with_multiple_lots$BusinessKey) != notifications_cleaned_unique_business_keys_number) print("Merge failed, investigate")
  
  notifications_single_lot_single_customer_single_product_grouped <- notifications_single_lot_single_customer %>%
                                                                      right_join(notifications_with_one_product_level_1, by = "BusinessKey")
  # Clean up
  rm(notifications_products_number_level_1); rm(notifications_with_multiple_products_level_1); # rm(notifications_with_multiple_lots); rm(notifications_with_one_lot_multiple_customers); 
  
  # Any remaining duplicate fields unrelated to product code?
  duplicate_fields <- notifications_single_lot_single_customer_single_product_grouped %>%
    group_by(BusinessKey, Key) %>%
    summarise(ValuesPerKey = n()) %>% ungroup() %>%
    group_by(Key) %>%
    summarise(MaxFieldsPerNotification = max(ValuesPerKey)) %>%
    filter(!Key %in% c("NotificationLotProductCodeLevel1", "oos:lots/oos:lot/oos:products/oos:product/oos:code", "oos:lots/oos:lot/oos:products/oos:product/oos:name"))
  # Check that we are safe to proceed without need to sum over maxPrice
  if(max(duplicate_fields$MaxFieldsPerNotification) > 1) print("Unexpected duplicate keys, investigate")
    
  # Filter out the duplicated lower-level product information
  notifications_product_grouped <- notifications_single_lot_single_customer_single_product_grouped %>%
                                    filter(!Key %in% c("oos:lots/oos:lot/oos:products/oos:product/oos:code", 
                                                       "oos:lots/oos:lot/oos:products/oos:product/oos:name")) %>%
                                    group_by(BusinessKey, DocumentVersionUniqueID, Key) %>%
                                      filter(row_number() == 1) %>% # Leaves one instance of NotificationLotProductCodeLevel1
                                    spread(key = Key, value = Value) #%>%
                                    #mutate(NotificationMissingReason = "Notification data complete (products grouped)")
  if(length(notifications_product_grouped$BusinessKey) != length(unique(notifications_with_one_product_level_1$BusinessKey))) print("Number of records mismatch, investigate")
  
  # Clean up
  rm(notifications_single_lot_single_customer_single_product_grouped); rm(notifications_with_one_product_level_1);
  
  # Rename variables sensibly
  column_names_notifications_old <- data.frame(XMLFieldName = colnames(notifications_product_grouped), stringsAsFactors = F)
  column_names_notifications_new <- parsing_configuration %>%
    filter(DocumentType == "notifications") %>%
    select(XMLFieldName, VariableName) %>%
    right_join(column_names_notifications_old, by = "XMLFieldName") %>%
    mutate(VariableName = ifelse(is.na(VariableName), XMLFieldName,
                                 VariableName)) %>%
    select(VariableName) 
  colnames(notifications_product_grouped) <- column_names_notifications_new$VariableName
  rm(column_names_notifications_new); rm(column_names_notifications_old)
  
  # Finally, join on the information we have about the notifications not filtered out above
  notifications_contracts_products_grouped <- notifications_contracts_products_grouped %>%
                                                left_join(notifications_product_grouped)
  if(sum(is.na(notifications_contracts_products_grouped$NotificationMissingReason)) - length(notifications_contracts$Match[notifications_contracts$Match == "Contract without notification"]) != length(notifications_product_grouped$BusinessKey)) print("Reconciliation failed, check")
  
  
  ## NOW SAME THING FOR UNGROUPED PRODUCTS
    
  # Identify and assess size of multiple-product notifications at level 4 (within single-lot subset)
  notifications_products_number_level_4 <- notifications_single_lot_single_customer %>%
                                            filter(Key == "oos:lots/oos:lot/oos:products/oos:product/oos:code") %>%
                                            group_by(BusinessKey) %>%
                                              summarize(NumberOfProducts = n(), 
                                                        NumberOfUniqueProducts = n_distinct(Value))
  
  # Send these off to the master data frame with some metadata
  notifications_with_multiple_products_level_4 <- notifications_products_number_level_4 %>%
                                                    filter(NumberOfUniqueProducts > 1) %>%
                                                    transmute(BusinessKey = BusinessKey, NotificationMissingReason = "Notification with single lot, single customer, multiple products (ungrouped)")
  
  notifications_contracts_products_ungrouped <- notifications_contracts %>%
                                                  left_join(notifications_with_multiple_products_level_4, by = c("BusinessKey")) %>%
                                                  rename(NotificationMissingReason = NotificationMissingReason.x) %>%
                                                  mutate(NotificationMissingReason = ifelse(is.na(NotificationMissingReason.y), NotificationMissingReason, NotificationMissingReason.y)) %>%
                                                  select(-NotificationMissingReason.y)
  # table(notifications_contracts_products_ungrouped$NotificationMissingReason, useNA = "always")

  # Identify remaining records, sum over maxPrice (sum necessary?)
  notifications_with_one_product_level_4 <- notifications_products_number_level_4 %>%
                                              filter(NumberOfUniqueProducts == 1) %>%
                                              select(BusinessKey)
  
  # Check our cases so far add up to total notifications
  if(length(unique(notifications_with_one_product_level_4$BusinessKey)) + length(unique(notifications_with_multiple_products_level_4$BusinessKey)) + length(notifications_with_one_lot_multiple_customers$BusinessKey) + length(notifications_with_multiple_lots$BusinessKey) != notifications_cleaned_unique_business_keys_number) print("Merge failed, investigate")
  
  notifications_single_lot_single_customer_single_product_ungrouped <- notifications_single_lot_single_customer %>%
                                                                        right_join(notifications_with_one_product_level_4, by = "BusinessKey")
  
  # Clean up
  rm(notifications_with_multiple_lots); rm(notifications_with_one_lot_multiple_customers); rm(notifications_products_number_level_4); rm(notifications_with_multiple_products_level_4); gc()
  
  # Any remaining duplicate fields unrelated to product code?
  duplicate_fields <- notifications_single_lot_single_customer_single_product_ungrouped %>%
    group_by(BusinessKey, Key) %>%
    summarise(ValuesPerKey = n()) %>% ungroup() %>%
    group_by(Key) %>%
    summarise(MaxFieldsPerNotification = max(ValuesPerKey))

  # Check that we are safe to proceed without need to sum over maxPrice
  if(max(duplicate_fields$MaxFieldsPerNotification) > 1) print("Unexpected duplicate keys, investigate")
  
  # Spread to wide format (don't need to filter out any fields as with grouped products)
  notifications_product_ungrouped <- notifications_single_lot_single_customer_single_product_ungrouped %>%
                                      spread(key = Key, value = Value) #%>%
  #mutate(NotificationMissingReason = "Notification data complete (products grouped)")
  if(length(notifications_product_ungrouped$BusinessKey) != length(unique(notifications_with_one_product_level_4$BusinessKey))) print("Number of records mismatch, investigate")
  
  # Clean up
  rm(notifications_single_lot_single_customer_single_product_ungrouped); rm(notifications_with_one_product_level_4); rm(duplicate_fields); rm(notifications_single_lot_single_customer); gc()
  
  # Rename variables sensibly
  column_names_notifications_old <- data.frame(XMLFieldName = colnames(notifications_product_ungrouped), stringsAsFactors = F)
  column_names_notifications_new <- parsing_configuration %>%
    filter(DocumentType == "notifications") %>%
    select(XMLFieldName, VariableName) %>%
    right_join(column_names_notifications_old, by = "XMLFieldName") %>%
    mutate(VariableName = ifelse(is.na(VariableName), XMLFieldName,
                                 VariableName)) %>%
    select(VariableName) 
  colnames(notifications_product_ungrouped) <- column_names_notifications_new$VariableName
  rm(column_names_notifications_new); rm(column_names_notifications_old)
  
  # Finally, join on the information we have about the notifications not filtered out above
  notifications_contracts_products_ungrouped <- notifications_contracts_products_ungrouped %>%
                                                  left_join(notifications_product_ungrouped)
  if(sum(is.na(notifications_contracts_products_ungrouped$NotificationMissingReason)) - length(notifications_contracts$Match[notifications_contracts$Match == "Contract without notification"]) != length(notifications_product_ungrouped$BusinessKey)) print("Reconciliation failed, check")
  # table(notifications_contracts_products_ungrouped$NotificationMissingReason, useNA = "always")
  
  # Which columns are different between the two?
  # anti_join(data.frame(ungrouped = colnames(notifications_contracts_products_ungrouped)), data.frame(grouped = colnames(notifications_contracts_products_grouped)), by = c("ungrouped"="grouped"))
  # NotificationLotProductName; NotificationLotProductCode (as expected, only the level 4 product code/name)
  
  # Clean up
  rm(notifications_contracts); gc()
  
  # Output proportion of tidy notifications from all parsed
  tidy_notifications_percentage <- length(unique(notifications_product_grouped$BusinessKey))/notifications_cleaned_unique_business_keys_number
  print(paste0("Proportion of tidy notifications (products grouped) in ", current_region, " is ", tidy_notifications_percentage))

  tidy_notifications_percentage <- length(unique(notifications_product_ungrouped$BusinessKey))/notifications_cleaned_unique_business_keys_number
  print(paste0("Proportion of tidy notifications (products ungrouped) in ", current_region, " is ", tidy_notifications_percentage))
  rm(tidy_notifications_percentage)
  
  # SAVE BOTH VERSIONS OF NOTIFICATION DATA
  notifications_wide_file_name_product_grouped <- paste0(data_output_directory_region, current_region,
                                                          "_notifications_wide_product_grouped_",
                                                          data_download_date, ".rda")
  save(notifications_product_grouped, file = notifications_wide_file_name_product_grouped)
  rm(notifications_product_grouped); rm(notifications_wide_file_name_product_grouped);
  
  notifications_wide_file_name_product_ungrouped <- paste0(data_output_directory_region, current_region,
                                                         "_notifications_wide_product_ungrouped_",
                                                         data_download_date, ".rda")
  save(notifications_product_ungrouped, file = notifications_wide_file_name_product_ungrouped)
  rm(notifications_product_ungrouped); rm(notifications_wide_file_name_product_ungrouped);
  gc()
      
  ###################
  ## Contracts     ##
  ###################    
  
  # Drop some fields that are of limited use (Budgetary/extrabudgetary accounting codes)
  contracts_cleaned <- contracts_cleaned %>%
                        filter(!Key %in% c("oos:finances/oos:extrabudgetary/oos:KOSGU",
                                           "oos:finances/oos:extrabudgetary/oos:price",
                                           "oos:finances/oos:budgetary/oos:KBK",
                                           "oos:finances/oos:budgetary/oos:price")) 
  
  # Store fine product-related details of contracts (with multiple values per contract) separately
  # These are relevant only for notifications_contracts_ungrouped!
  contracts_products_details <- contracts_cleaned %>%
                        filter(Key %in% c("oos:products/oos:product/oos:OKDP/oos:code",
                                          "oos:products/oos:product/oos:OKDP/oos:name",
                                          "oos:products/oos:product/oos:OKEI/oos:code",
                                          "oos:products/oos:product/oos:OKEI/oos:name",
                                          "oos:products/oos:product/oos:price",
                                          "oos:products/oos:product/oos:quantity",
                                          "oos:products/oos:product/oos:sum"))
  
  # Save this off and clear it out
  contracts_products_details_file_name <- paste0(data_output_directory_region, current_region,
                                                 "_contracts_products_details_",
                                                 data_download_date, ".rda")
  save(contracts_products_details, file = contracts_products_details_file_name)
  rm(contracts_products_details); rm(contracts_products_details_file_name);
  
  # ADD LEVEL 1 OF OKDP PRODUCT CLASSIFICATION (PAUSED FOR NOW)
  contracts_cleaned_okdp_level_1 <- contracts_cleaned %>%
                                      filter(Key == "oos:products/oos:product/oos:OKDP/oos:code") %>%
                                      left_join(okdp_product_classification,
                                                by = c("Value" = "ProductCode")) %>%
                                      mutate(Key = "ContractProductCodeLevel1",
                                             Value = ProductCodeLevel1) %>%
                                      select(BusinessKey, DocumentVersionUniqueID, Key, Value)
  # Add these new lines 
  contracts_cleaned <- rbind(contracts_cleaned, contracts_cleaned_okdp_level_1)
  rm(contracts_cleaned_okdp_level_1); gc();
    
  # What Keys are duplicated for contracts?
  duplicate_fields <- contracts_cleaned %>%
    group_by(BusinessKey, Key) %>%
    summarise(ValuesPerKey = n()) %>% ungroup() %>%
    group_by(Key) %>%
    summarise(MaxFieldsPerContract = max(ValuesPerKey))
  
  ### So need to filter out
  ## Multiple suppliers 
  # oos:suppliers/oos:supplier/oos:inn
  # oos:suppliers/oos:supplier/oos:kpp
  # oos:suppliers/oos:supplier/oos:organizationName
  # oos:suppliers/oos:supplier/oos:participantType
  contracts_suppliers_number <- contracts_cleaned %>%
                                  filter(Key == "oos:suppliers/oos:supplier/oos:inn") %>%
                                  group_by(BusinessKey, Key) %>%
                                    summarize(NumberOfSuppliers = n()) %>% ungroup() 
  # table(contracts_suppliers_number$NumberOfSuppliers)
  
  # Which contracts are with multiple suppliers?
  contracts_with_multiple_suppliers <- contracts_suppliers_number %>%
                                        filter(NumberOfSuppliers > 1) %>%
                                        select(BusinessKey) %>%
                                        mutate(ContractMissingReason = "Contract with multiple suppliers")
  
  # Send these to both notifications data frames (grouped and ungrouped products)
  notifications_contracts_products_grouped <- notifications_contracts_products_grouped %>%
                                                left_join(contracts_with_multiple_suppliers, by = "BusinessKey") %>%
                                                rename(ContractMissingReason = ContractMissingReason.x) %>%
                                                mutate(ContractMissingReason = ifelse(is.na(ContractMissingReason.y), ContractMissingReason, ContractMissingReason.y)) %>%
                                                select(-ContractMissingReason.y)
  # table(notifications_contracts_products_grouped$ContractMissingReason, useNA = "always")
  
  notifications_contracts_products_ungrouped <- notifications_contracts_products_ungrouped %>%
    left_join(contracts_with_multiple_suppliers, by = "BusinessKey") %>%
    rename(ContractMissingReason = ContractMissingReason.x) %>%
    mutate(ContractMissingReason = ifelse(is.na(ContractMissingReason.y), ContractMissingReason, ContractMissingReason.y)) %>%
    select(-ContractMissingReason.y)
  # table(notifications_contracts_products_ungrouped$ContractMissingReason, useNA = "always")
  
  # Retain the rest for further analysis
  contracts_with_single_supplier <- contracts_suppliers_number %>% filter(NumberOfSuppliers == 1) %>% select(BusinessKey)
  contracts_single_supplier <- contracts_cleaned %>%
                                right_join(contracts_with_single_supplier, by = "BusinessKey")
  
  # Check
  if(length(unique(contracts_single_supplier$BusinessKey)) + length(unique(contracts_with_multiple_suppliers$BusinessKey)) != contracts_cleaned_unique_business_keys_number) print("Reconciliation failed")
  
  rm(contracts_suppliers_number); rm(contracts_with_single_supplier); rm(contracts_cleaned)
  # rm(contracts_with_multiple_suppliers) # Needed for reconciliation check later
  
  # Attach supplier information to main DF now
  contracts_single_supplier_information <- contracts_single_supplier %>%
    filter(Key %in% c("oos:suppliers/oos:supplier/oos:participantType",
                      "oos:suppliers/oos:supplier/oos:inn",
                      "oos:suppliers/oos:supplier/oos:kpp",
                      "oos:suppliers/oos:supplier/oos:organizationName")) %>%
    # group_by(BusinessKey, Key) %>%
    #   filter(row_number() == 1) %>% ungroup() %>%
    spread(key = Key, value = Value) %>%
    select(-DocumentVersionUniqueID)
  
  # Rename columns
  column_names_contracts_old <- data.frame(XMLFieldName = colnames(contracts_single_supplier_information), stringsAsFactors = F)
  column_names_contracts_new <- parsing_configuration %>%
    filter(DocumentType == "contracts") %>%
    select(XMLFieldName, VariableName) %>%
    right_join(column_names_contracts_old, by = "XMLFieldName") %>%
    mutate(VariableName = ifelse(is.na(VariableName), XMLFieldName,
                                 VariableName)) %>%
    select(VariableName) 
  colnames(contracts_single_supplier_information) <- column_names_contracts_new$VariableName
  rm(column_names_contracts_new); rm(column_names_contracts_old)
  
  # Send single supplier information to both master DFs
  notifications_contracts_products_grouped <- notifications_contracts_products_grouped %>%
    left_join(contracts_single_supplier_information, by = "BusinessKey")

  notifications_contracts_products_ungrouped <- notifications_contracts_products_ungrouped %>%
    left_join(contracts_single_supplier_information, by = "BusinessKey")

  rm(contracts_single_supplier_information);
  
  # Now repeat technique above for identifying products within contracts
  ## FIRST GROUPED PRODUCTS
  contracts_products_number_level_1 <- contracts_single_supplier %>%
                                        filter(Key == "ContractProductCodeLevel1") %>%
                                        group_by(BusinessKey) %>%
                                          summarize(NumberOfProducts = n(),
                                                    NumberOfUniqueProducts = n_distinct(Value))

  # Send these off to the master data frame with some metadata (for the rest, use NumberOfUniqueProducts and sum across oos:customerRequirement/oos:maxPrice)
  contracts_with_multiple_products_level_1 <- contracts_products_number_level_1 %>%
    filter(NumberOfUniqueProducts > 1) %>%
    transmute(BusinessKey = BusinessKey, ContractMissingReason = "Contract with single supplier, multiple products (grouped)")
  
  notifications_contracts_products_grouped <- notifications_contracts_products_grouped %>%
    left_join(contracts_with_multiple_products_level_1, by = c("BusinessKey")) %>%
    rename(ContractMissingReason = ContractMissingReason.x) %>%
    mutate(ContractMissingReason = ifelse(is.na(ContractMissingReason.y), ContractMissingReason, ContractMissingReason.y)) %>%
    select(-ContractMissingReason.y)
  # table(notifications_contracts_products_grouped$ContractMissingReason, useNA = "always")
  
  # Identify remaining records, sum over maxPrice (sum not actually necessary)
  contracts_with_one_product_level_1 <- contracts_products_number_level_1 %>%
    filter(NumberOfUniqueProducts == 1) %>%
    select(BusinessKey)
  
  # Check our cases so far add up to total contracts
  if(length(unique(contracts_with_one_product_level_1$BusinessKey)) + length(unique(contracts_with_multiple_products_level_1$BusinessKey)) + length(contracts_with_multiple_suppliers$BusinessKey) != contracts_cleaned_unique_business_keys_number) print("Merge failed, investigate")
  
  contracts_single_supplier_single_product_grouped <- contracts_single_supplier %>%
    right_join(contracts_with_one_product_level_1, by = "BusinessKey")
  
  # Clean up
  rm(contracts_products_number_level_1); rm(contracts_with_multiple_products_level_1); rm(contracts_with_one_product_level_1)
  
  # Any remaining duplicate fields unrelated to product code?
  duplicate_fields <- contracts_single_supplier_single_product_grouped %>%
    group_by(BusinessKey, Key) %>%
      summarise(ValuesPerKey = n()) %>% ungroup() %>%
    group_by(Key) %>%
      summarise(MaxFieldsPerNotification = max(ValuesPerKey))
  
  ## Product-related duplicate fields
  # Only need to retain first one for the grouped version (one instance per contract)
  # ContractProductCodeLevel1
  # oos:products/oos:product/oos:OKDP/oos:code
  # oos:products/oos:product/oos:OKDP/oos:name
  # oos:products/oos:product/oos:OKEI/oos:code
  # oos:products/oos:product/oos:OKEI/oos:name
  # oos:products/oos:product/oos:price
  # oos:products/oos:product/oos:quantity
  # oos:products/oos:product/oos:sum
  
  # Send single product (grouped) code to main DF, along with other non-duplicated details
  contracts_product_grouped <- contracts_single_supplier_single_product_grouped %>%
    filter(Key %in% c("ContractProductCodeLevel1")) %>%
    group_by(BusinessKey, Key) %>%
      filter(row_number() == 1) %>% ungroup() %>%
    spread(key = Key, value = Value) %>%
    select(-DocumentVersionUniqueID)
  
  # Rename columns
  column_names_contracts_old <- data.frame(XMLFieldName = colnames(contracts_product_grouped), stringsAsFactors = F)
  column_names_contracts_new <- parsing_configuration %>%
    filter(DocumentType == "contracts") %>%
    select(XMLFieldName, VariableName) %>%
    right_join(column_names_contracts_old, by = "XMLFieldName") %>%
    mutate(VariableName = ifelse(is.na(VariableName), XMLFieldName,
                                 VariableName)) %>%
    select(VariableName) 
  colnames(contracts_product_grouped) <- column_names_contracts_new$VariableName
  rm(column_names_contracts_new); rm(column_names_contracts_old)
  
  # Join on to main data frame
  notifications_contracts_products_grouped <- notifications_contracts_products_grouped %>%
    left_join(contracts_product_grouped, by = "BusinessKey")
  # Saving happens later
  # rm(contracts_product_grouped); 
  rm(contracts_single_supplier_single_product_grouped); gc();
  
  
  ## NOW UNGROUPED PRODUCTS (to the extent possible given multiple duplicate fields per product)
  contracts_products_number_level_4 <- contracts_single_supplier %>%
                                        filter(Key == "oos:products/oos:product/oos:OKDP/oos:code") %>%
                                        group_by(BusinessKey) %>%
                                          summarize(NumberOfProducts = n(),
                                                    NumberOfUniqueProducts = n_distinct(Value))
  # table(contracts_products_number_level_4$NumberOfUniqueProducts)
  
  # Unique products are a problem here because can't sum across prices and quantities
  # Need to send these to master DF noting as such
  contracts_with_multiple_products_level_4 <- contracts_products_number_level_4 %>%
    filter(NumberOfUniqueProducts > 1) %>%
    transmute(BusinessKey = BusinessKey, ContractMissingReason = "Contract with single supplier, multiple products (ungrouped)")
  
  notifications_contracts_products_ungrouped <- notifications_contracts_products_ungrouped %>%
    left_join(contracts_with_multiple_products_level_4, by = c("BusinessKey")) %>%
    rename(ContractMissingReason = ContractMissingReason.x) %>%
    mutate(ContractMissingReason = ifelse(is.na(ContractMissingReason.y), ContractMissingReason, ContractMissingReason.y)) %>%
    select(-ContractMissingReason.y)
  # table(notifications_contracts_products_ungrouped$ContractMissingReason, useNA = "always")
  
  # Identify remaining records, sum over maxPrice (sum not actually necessary)
  contracts_with_one_product_level_4 <- contracts_products_number_level_4 %>%
    filter(NumberOfUniqueProducts == 1) %>%
    select(BusinessKey)
  
  # Check our cases so far add up to total contracts
  if(length(unique(contracts_with_one_product_level_4$BusinessKey)) + length(unique(contracts_with_multiple_products_level_4$BusinessKey)) + length(contracts_with_multiple_suppliers$BusinessKey) != contracts_cleaned_unique_business_keys_number) print("Merge failed, investigate")
  
  contracts_single_supplier_single_product_ungrouped <- contracts_single_supplier %>%
    right_join(contracts_with_one_product_level_4, by = "BusinessKey")
  
  # Clean up
  rm(contracts_products_number_level_4); rm(contracts_single_supplier)
  # rm(contracts_with_multiple_products_level_4); rm(contracts_with_multiple_suppliers); 
  rm(contracts_with_one_product_level_4)

  # Any remaining duplicate fields unrelated to product code?
  # Particularly concerned about multiple units (OKEI) as prevents summing over price, quantity and sum
  duplicate_fields <- contracts_single_supplier_single_product_ungrouped %>%
    group_by(BusinessKey, Key) %>%
      summarise(ValuesPerKey = n_distinct(Value)) %>% ungroup() %>%
    group_by(Key) %>%
      summarise(MaxFieldsPerNotification = max(ValuesPerKey))
  
  # How many units of measurement (OKEI code) per contract?
  contracts_single_supplier_single_product_ungrouped_units_number <- contracts_single_supplier_single_product_ungrouped %>%
    filter(Key == "oos:products/oos:product/oos:OKEI/oos:code") %>%
    group_by(BusinessKey, Key) %>%
      summarize(NumberOfUnits = n_distinct(Value)) %>% ungroup()
  
  contracts_with_one_product_level_4_multiple_units <- contracts_single_supplier_single_product_ungrouped_units_number %>%
    filter(NumberOfUnits > 1) %>%
    transmute(BusinessKey = BusinessKey, ContractMissingReason = "Contract with single supplier, single product (ungrouped), multiple units")
  
  notifications_contracts_products_ungrouped <- notifications_contracts_products_ungrouped %>%
    left_join(contracts_with_one_product_level_4_multiple_units, by = c("BusinessKey")) %>%
    rename(ContractMissingReason = ContractMissingReason.x) %>%
    mutate(ContractMissingReason = ifelse(is.na(ContractMissingReason.y), ContractMissingReason, ContractMissingReason.y)) %>%
    select(-ContractMissingReason.y)
  # table(notifications_contracts_products_ungrouped$ContractMissingReason, useNA = "always")
  
  # Identify remaining records, sum over price, quantity, sum
  contracts_with_one_product_level_4_one_unit <- contracts_single_supplier_single_product_ungrouped_units_number %>%
    filter(NumberOfUnits == 1) %>%
    select(BusinessKey)

  contracts_single_supplier_single_product_ungrouped_single_unit <- contracts_single_supplier_single_product_ungrouped %>%
    right_join(contracts_with_one_product_level_4_one_unit, by = "BusinessKey")
  
  rm(contracts_single_supplier_single_product_ungrouped_units_number); rm(contracts_with_one_product_level_4_one_unit)
  rm(contracts_single_supplier_single_product_ungrouped);
  
  # Any remaining duplicate fields unrelated to product code?
  # duplicate_fields <- contracts_single_supplier_single_product_ungrouped_single_unit %>%
  #   group_by(BusinessKey, Key) %>%
  #     summarise(ValuesPerKey = n_distinct(Value)) %>% ungroup() %>%
  #   group_by(Key) %>%
  #     summarise(MaxFieldsPerNotification = max(ValuesPerKey))

  contracts_product_ungrouped_non_additive <- contracts_single_supplier_single_product_ungrouped_single_unit %>%
    filter(Key %in% c("oos:products/oos:product/oos:OKDP/oos:code",
                      "oos:products/oos:product/oos:OKDP/oos:name",
                      "oos:products/oos:product/oos:OKEI/oos:code",
                      "oos:products/oos:product/oos:OKEI/oos:name")) %>%
    group_by(BusinessKey, Key) %>%
      filter(row_number() == 1) %>% ungroup() %>%
    spread(key = Key, value = Value) %>%
    select(-DocumentVersionUniqueID)
  
  contracts_product_ungrouped_additive <- contracts_single_supplier_single_product_ungrouped_single_unit %>%
    filter(Key %in% c("oos:products/oos:product/oos:sum",
                      "oos:products/oos:product/oos:quantity")) %>%
    group_by(BusinessKey, Key) %>%
      summarize(Sum = sum(as.numeric(Value))) %>% ungroup() %>%
    spread(key = Key, value = Sum)
  
  contracts_product_ungrouped_semi_additive <- contracts_single_supplier_single_product_ungrouped_single_unit %>%
    filter(Key %in% c("oos:products/oos:product/oos:price")) %>%
    group_by(BusinessKey, Key) %>%
      summarize(Mean = mean(as.numeric(Value))) %>% ungroup() %>%
    spread(key = Key, value = Mean)
  
  contracts_product_ungrouped <- contracts_product_ungrouped_non_additive %>%
    inner_join(contracts_product_ungrouped_additive, by = "BusinessKey") %>%
    inner_join(contracts_product_ungrouped_semi_additive, by = "BusinessKey")
  
  # Check number of cases after summing over price/quantity/sum
  if(length(unique(contracts_product_ungrouped$BusinessKey)) + length(unique(contracts_with_one_product_level_4_multiple_units$BusinessKey)) + length(unique(contracts_with_multiple_products_level_4$BusinessKey)) + length(unique(contracts_with_multiple_suppliers$BusinessKey)) != contracts_cleaned_unique_business_keys_number) print("Missing contracts")
  
  rm(contracts_single_supplier_single_product_ungrouped_single_unit); 
  rm(contracts_product_ungrouped_additive); rm(contracts_product_ungrouped_non_additive); rm(contracts_product_ungrouped_semi_additive); rm(contracts_with_one_product_level_4_multiple_units)
  rm(contracts_with_multiple_products_level_4); rm(contracts_with_multiple_suppliers)
  gc()
  
  # Rename columns
  column_names_contracts_old <- data.frame(XMLFieldName = colnames(contracts_product_ungrouped), stringsAsFactors = F)
  column_names_contracts_new <- parsing_configuration %>%
    filter(DocumentType == "contracts") %>%
    select(XMLFieldName, VariableName) %>%
    right_join(column_names_contracts_old, by = "XMLFieldName") %>%
    mutate(VariableName = ifelse(is.na(VariableName), XMLFieldName,
                                 VariableName)) %>%
    select(VariableName) 
  colnames(contracts_product_ungrouped) <- column_names_contracts_new$VariableName
  rm(column_names_contracts_new); rm(column_names_contracts_old)
  
  # Join on to master DF
  notifications_contracts_products_ungrouped <- notifications_contracts_products_ungrouped %>%
    left_join(contracts_product_ungrouped, by = "BusinessKey")
  
  
  # Scatterplot of petrol prices over time
  petrol_prices <- notifications_contracts_products_ungrouped %>%
    filter(ContractProductOKDPCode == "2320212" & ContractProductOKEICode == "112" & ContractProductPrice < 500 & ContractProductPrice > 1) %>%
    transmute(Date = (paste(substr(ContractPublishDate, 1, 4), substr(ContractPublishDate, 6, 7), sep = ".")),
              `Price per liter` = ContractProductPrice,
              `Number of liters` = ContractProductQuantity,
              `Total purchase` = (ContractProductSum),
              `Total purchase (log)` = log10(ContractProductSum),
              Procedure = ifelse(NotificationPlacingWayName == "Открытый аукцион в электронной форме", "Less discretionary", 
                                 ifelse(NotificationPlacingWayName == "Запрос котировок", "More discretionary", "Unknown")),
              Agency = (ContractCustomerFullName),
              INN = as.factor(as.character(ContractCustomerINN)),
              Purchaser = NotificationOrderPlacerRegNum)
  
  agency_per_INN <- petrol_prices %>%
    group_by(INN) %>%
      summarize(AgenciesPerINN = n_distinct(Agency))
  
  agency_INN_correspondence <- petrol_prices %>%
    mutate(LengthOfAgencyName = nchar(Agency)) %>%
    group_by(INN, Agency) %>%
      summarize(ShortestAgencyName = min(LengthOfAgencyName)) %>%
    filter(row_number() == 1) %>% ungroup() %>%
  transmute(INN = INN, `Agency (definitive)` = Agency)
  
  petrol_prices <- petrol_prices %>%
    left_join(agency_INN_correspondence, by = "INN")
  
  petrol_prices_by_agency <- petrol_prices %>%
    group_by(INN, `Agency (definitive)`) %>%
      summarize(`Average price per liter` = mean(`Price per liter`),
                `Median price per liter` = median(`Price per liter`)) %>% ungroup() %>%
    arrange(`Average price per liter`) 
  
  petrol_prices_by_agency$`Agency (definitive)` <- factor(petrol_prices_by_agency$`Agency (definitive)`, levels = petrol_prices_by_agency$`Agency (definitive)`[order(petrol_prices_by_agency$`Average price per liter`)])
  petrol_prices$`Agency (definitive)` <- factor(petrol_prices$`Agency (definitive)`, levels = petrol_prices_by_agency$`Agency (definitive)`)
  petrol_prices$Procedure <- factor(petrol_prices$Procedure, levels = c("More discretionary", "Less discretionary"))

  petrol_prices <- petrol_prices %>%
    left_join(petrol_prices_by_agency)
  
  # First show petrol prices don't change much over time
  # Might want to de-trend with official price data https://yandex.ru/search/site/?searchid=2252602&text=%D0%B1%D0%B5%D0%BD%D0%B7%D0%B8%D0%BD&web=0&l10n=ru
  graph_title <- paste0("Price per liter paid for gasoline, by month in ", current_region_english, "\n")
    graph_file_name <- paste0(data_output_directory_region, current_region, "_petrol_price_per_liter_by_month.pdf")
  petrol_prices_by_month_graph <- petrol_prices %>%
    ggplot(aes(x = Date, y = `Price per liter`, colour = Procedure)) +
            geom_point() +
            theme(axis.text.x = element_text(angle=60, vjust=0.5, size=8)) +
            labs(title = graph_title, x = "\nMonth", y = "Price per liter (rubles)") 
  print(petrol_prices_by_month_graph)
    ggsave(plot = petrol_prices_by_month_graph, filename = graph_file_name, device = "pdf")
  
  # The best one: petrol prices
  graph_title <- paste0("Price per liter paid for gasoline, by agencies in ", current_region_english, "\n")
    graph_file_name <- paste0(data_output_directory_region, current_region, "_petrol_price_per_liter_by_agency.pdf")
  petrol_prices_by_agency_graph <- petrol_prices %>%
    ggplot(aes(x = `Agency (definitive)`, y = `Price per liter`, colour = Procedure)) +
      geom_point() +
      theme(axis.ticks = element_blank(), axis.text.x  = element_blank()) +
      labs(title = graph_title, x = "\nAgencies arranged by average price per liter", y = "Price per liter (rubles)") +
      theme(legend.justification=c(0.05,0.95), legend.position=c(0.05,0.95))
  print(petrol_prices_by_agency_graph)
    ggsave(plot = petrol_prices_by_agency_graph, filename = graph_file_name, device = "pdf")
  
  petrol_prices_by_procedure <- petrol_prices %>%
    ggplot(aes(x = `Total purchase`, y = `Price per liter`, colour = Procedure)) +
      geom_point()
      # + geom_smooth(method = "lm", se = F)
  print(petrol_prices_by_procedure)
  
  products_per_agency_ungrouped <- notifications_contracts_products_ungrouped %>%
    group_by(ContractCustomerINN) %>%
      summarize(NumberOfProductsPurchasedUngrouped = n_distinct(ContractProductOKDPCode))

  products_per_agency_grouped <- notifications_contracts_products_grouped %>%
    group_by(ContractCustomerINN) %>%
      summarize(NumberOfProductsPurchasedGrouped = n_distinct(ContractProductCodeLevel1))
    
  petrol_price_vs_products_purchased <- petrol_prices_by_agency %>%
    left_join(products_per_agency_ungrouped, by = c("INN" = "ContractCustomerINN")) %>%
    left_join(products_per_agency_grouped, by = c("INN" = "ContractCustomerINN"))
  
  testing_1 <- lm(`Average price per liter` ~ NumberOfProductsPurchasedUngrouped, data = petrol_price_vs_products_purchased)
  summary(testing_1)
  testing_2 <- lm(`Average price per liter` ~ NumberOfProductsPurchasedGrouped, data = petrol_price_vs_products_purchased)
  summary(testing_2)
  testing_3 <- lm(`Average price per liter` ~ log10(NumberOfProductsPurchasedUngrouped), data = petrol_price_vs_products_purchased)
  summary(testing_3)
  
  # Average amount spent on each grouped category
  total_spent <- notifications_contracts_products_grouped %>%
    filter(!is.na(ContractProductCodeLevel1) & !is.na(ContractPrice)) %>%
    summarize(TotalSpent = sum(as.numeric(ContractPrice)))
  total_spent <- total_spent[1,1]
    
  total_and_average_spend_per_product_grouped <- notifications_contracts_products_grouped %>%
    filter(!is.na(ContractProductCodeLevel1) & !is.na(ContractPrice)) %>%
    group_by(ContractProductCodeLevel1) %>%
      summarize(TotalSpendPerProductGrouped = sum(as.numeric(ContractPrice)),
                MeanSpendPerProductGrouped = mean(as.numeric(ContractPrice)),
                MedianSpendPerProductGrouped = median(as.numeric(ContractPrice))) %>%
      mutate(ProportionOfTotalSpend = as.numeric(TotalSpendPerProductGrouped) / total_spent)
  sum(total_and_average_spend_per_product_grouped$ProportionOfTotalSpend)
  
  agency_INN_correspondence <- notifications_contracts_products_ungrouped %>%
    filter(!is.na(ContractCustomerFullName) & !is.na(ContractCustomerINN)) %>%
    transmute(Agency = ContractCustomerFullName,
              INN = ContractCustomerINN) %>%
    mutate(LengthOfAgencyName = nchar(Agency)) %>%
    group_by(INN, Agency) %>%
    summarize(ShortestAgencyName = min(LengthOfAgencyName)) %>%
    arrange(ShortestAgencyName) %>%
    filter(row_number() == 1) %>% ungroup() %>%
    transmute(INN = INN, `Agency (definitive)` = Agency)
  
  agency_total_and_average_spend_per_product_grouped <- notifications_contracts_products_grouped %>%
    filter(!is.na(ContractProductCodeLevel1) & !is.na(ContractPrice)) %>%
    group_by(ContractCustomerINN) %>%
      mutate(AgencyTotalSpend =  sum(as.numeric(ContractPrice))) %>% ungroup() %>%
    group_by(ContractCustomerINN, ContractProductCodeLevel1, AgencyTotalSpend) %>%
      summarize(AgencyTotalSpendPerProductGrouped = sum(as.numeric(ContractPrice)),
                AgencyMeanSpendPerProductGrouped = mean(as.numeric(ContractPrice)),
                AgencyMedianSpendPerProductGrouped = median(as.numeric(ContractPrice))) %>%
      mutate(ProportionOfAgencySpend = AgencyTotalSpendPerProductGrouped / AgencyTotalSpend) %>%
    left_join(total_and_average_spend_per_product_grouped) %>%
    mutate(DeviationFromAverageSpend = ProportionOfTotalSpend - ProportionOfAgencySpend) %>%
    group_by(ContractCustomerINN) %>%
      summarize(MeanAbsoluteDeviationFromAverageSpendPerProduct = mean(abs(DeviationFromAverageSpend)),
                SumAbsoluteDeviationFromAverageSpendPerProduct = sum(abs(DeviationFromAverageSpend))) %>%
    left_join(agency_INN_correspondence, by = c("ContractCustomerINN" = "INN"))
  
  petrol_and_difference <- petrol_prices_by_agency %>%
    inner_join(agency_total_and_average_spend_per_product_grouped)
  
  plot(petrol_and_difference$SumAbsoluteDeviationFromAverageSpendPerProduct, petrol_and_difference$`Average price per liter`)
 
  
  landscaping <- notifications_contracts_products_ungrouped %>%
    filter(ContractProductOKDPCode == "4560227" & ContractProductOKEICode == "055" & ContractProductQuantity > 5) %>%
    transmute(Date = (paste(substr(ContractPublishDate, 1, 4), substr(ContractPublishDate, 6, 7), sep = ".")),
              `Price per square meter (log)` = log10(ContractProductPrice),
              `Number of square meters` = ContractProductQuantity)
  
  landscaping_graph <- landscaping %>%
    ggplot(aes(x = Date, y = `Price per square meter (log)`)) +
    geom_point()
  print(landscaping_graph)
  
  car_rental <- notifications_contracts_products_ungrouped %>%
    filter(ContractProductOKDPCode == "6022020" & ContractProductOKEICode == "356" & ContractProductQuantity > 1) %>%
    transmute(Date = (paste(substr(ContractPublishDate, 1, 4), substr(ContractPublishDate, 6, 7), sep = ".")),
              `Price per hour (log)` = log10(ContractProductPrice),
              `Number of square meters` = ContractProductQuantity)
  
  car_rental_graph <- car_rental %>%
    ggplot(aes(x = Date, y = `Price per hour (log)`)) +
    geom_point()
  print(car_rental_graph)
  
  # Most common products
  product_frequency <- notifications_contracts_products_ungrouped %>%
    group_by(ContractProductOKDPCode, ContractProductOKDPName, ContractProductOKEICode, ContractProductOKEIName) %>%
      summarize(PurchasesPerProduct = n())
  
  # Number of agencies per product/unit combo
  agencies_per_product_unit <- notifications_contracts_products_ungrouped %>%
    group_by(ContractProductOKDPCode, ContractProductOKDPName, ContractProductOKEICode, ContractProductOKEIName) %>%
      summarize(AgenciesPerProductUnit = n_distinct(ContractCustomerINN))
  
  # In general, time/size/weight measures are less discretionary than "piece" etc
  # So some kind of "amount of budget spent on discretionary units"?

  # Correlate procedure choice with gas price paid
  
  
  # 4560227 Благоустройство и озеленение Landscaping and gardening - how much variation reasonable per sq m?
  # 6022020 Услуги по сдаче в аренду легковых автомобилей с водителями - rental hourly rates vs "piece"
  # 2320212 Бензины автомобильные - Литр; Кубический дециметр (but multiple grades so only the "exceptional" count)
    # Actually, prices quite consistent across grades (and diesel) http://www.gks.ru/bgd/free/B04_03/IssWWW.exe/Stg/d06/17.htm
    # Seems to be discontinuity between high 30s and low 50s, no clear time trend
  # 3311415 Перчатки хирургические Пара (2 шт.) vs 1 - gloves? 
  # 7492060 Услуги охранников - per person/hour or as "package"
  
  # 2930274 Кондиционеры бытовые, электровоздухоохладители
  # 2101511 Бумага для копировально - множительной техники
  
  
  

  # Output proportion of tidy contracts from all parsed
  tidy_contracts_percentage <- length(unique(contracts_product_ungrouped$BusinessKey))/contracts_cleaned_unique_business_keys_number
  print(paste0("Proportion of tidy contracts (products ungrouped) in ", current_region, " is ", tidy_contracts_percentage))
  
  tidy_contracts_percentage <- length(unique(contracts_product_grouped$BusinessKey))/contracts_cleaned_unique_business_keys_number
  print(paste0("Proportion of tidy contracts (products grouped) in ", current_region, " is ", tidy_contracts_percentage))
  
  # Save the 'wide' format file with a sensible name and clean up
  contracts_wide_file_name_product_grouped <- paste0(data_output_directory_region, current_region,
                                                     "_contracts_wide_product_grouped_", data_download_date, ".rda")
  save(contracts_product_grouped, file = contracts_wide_file_name_product_grouped)
  rm(contracts_product_grouped); rm(contracts_wide_file_name_product_grouped)
  
  contracts_wide_file_name_product_ungrouped <- paste0(data_output_directory_region, current_region,
                                                     "_contracts_wide_product_ungrouped_", data_download_date, ".rda")
  save(contracts_product_ungrouped, file = contracts_wide_file_name_product_ungrouped)
  rm(contracts_product_ungrouped); rm(contracts_wide_file_name_product_ungrouped)
  gc()
  
  ## DELETE VARIABLES THAT NEVER VARY
  notifications_contracts_products_grouped$NotificationLotOrdinalNumber <- NULL # Always = 1
    notifications_contracts_products_ungrouped$NotificationLotOrdinalNumber <- NULL # Always = 1
  notifications_contracts_products_grouped$ContractFoundationSingleCustomer <- NULL # Always empty
    notifications_contracts_products_ungrouped$ContractFoundationSingleCustomer <- NULL # Always empty
  
  # Save the joined master DFs (product grouped and ungrouped)
  notifications_contracts_products_grouped_file_name <- paste0(data_output_directory_region, current_region,
                                                               "_notifications_contracts_products_grouped_",
                                                               data_download_date, ".rda")
  save(notifications_contracts_products_grouped, file = notifications_contracts_products_grouped_file_name)
  
  notifications_contracts_products_ungrouped_file_name <- paste0(data_output_directory_region, current_region,
                                                               "_notifications_contracts_products_ungrouped_",
                                                               data_download_date, ".rda")
  save(notifications_contracts_products_ungrouped, file = notifications_contracts_products_ungrouped_file_name)
    
  ###################
  ## Report stats  ##
  ###################  
  
  # Report statistics on matching, append to file matching_statistic.csv
  number_of_cases <- length(notifications_contracts_products_ungrouped$BusinessKey)
  
  notification_matches_contract <- length(notifications_contracts_products_ungrouped$Match[notifications_contracts_products_ungrouped$Match == "Notification matches contract"])
  notification_without_contract <- length(notifications_contracts_products_ungrouped$Match[notifications_contracts_products_ungrouped$Match == "Notification without contract"])
  contract_without_notification <- length(notifications_contracts_products_ungrouped$Match[notifications_contracts_products_ungrouped$Match == "Contract without notification"])
  
  notification_contract_matches_proportion <- c(current_region, format(Sys.Date(), "%Y-%m-%d"),
                                                "Notifications with matching contract",
                                                notification_matches_contract,
                                                notification_matches_contract/number_of_cases)                                              
  # print(paste0("Notifications with matching contract: ", notification_contract_matches_proportion))
  notifications_without_contract_proportion <- c(current_region, format(Sys.Date(), "%Y-%m-%d"),
                                                 "Notifications without matching contract",
                                                 notification_without_contract,
                                                 notification_without_contract/number_of_cases)
  # print(paste0("Notifications without matching contract: ", notifications_without_contract_proportion))
  contracts_without_notification_proportion <- c(current_region, format(Sys.Date(), "%Y-%m-%d"),
                                                 "Contracts without matching notification",
                                                 contract_without_notification,
                                                 contract_without_notification/number_of_cases)
  # print(paste0("Contracts without matching notification: ", contracts_without_notification_proportion))
  # notification_contract_matches_proportion + notifications_without_contract_proportion + contracts_without_notification_proportion
  matching_statistics <- (rbind.data.frame(notification_contract_matches_proportion,
                               notifications_without_contract_proportion,
                               contracts_without_notification_proportion))
  colnames(matching_statistics) <- c("Region", "Date", "Type", "Count", "Proportion")
  suppressWarnings(write.table(matching_statistics, file = "4-Construct/matching_statistics.csv",
                               sep = ",", row.names = F, append = T, col.names = F))
  rm(matching_statistics); rm(contract_without_notification); rm(notification_without_contract); rm(notification_matches_contract); gc()
    
  # Clean up
  rm(notifications_contracts_products_grouped); rm(notifications_contracts_products_ungrouped)
} # Closes control loop over this region
gc()
# ENDS