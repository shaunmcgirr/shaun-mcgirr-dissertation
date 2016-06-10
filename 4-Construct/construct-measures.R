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
source(file="4-Construct/construct-measures-functions.R")

# Change in to the directory where parsed data are stored (at the end of step 3)
# setwd('~/data/zakupki/2015-06-13/zakupki-2015-06-13-parsed-data/')
data_parsed_directory <- set_data_subdirectory(data_directory, data_download_date, "parsed")

############################################
# 2. Gather parameters about the job ahead #
############################################

# Obtain list of regions for which consolidated data is available
# regions_list <- generate_regions_list(data_parsed_directory)
regions_list <- as.list("Adygeja_Resp")
# regions_list <- as.list("Moskva")
regions_number <- length(regions_list)

# Define target of cleaned data
data_cleaned_directory <- set_data_subdirectory(data_directory, data_download_date, "cleaned")
# Define where outputs (eg graphs) should go
data_output_directory <- set_data_subdirectory(data_directory, data_download_date, "output")

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
  
  # Distributions of the raw variables of interest
  notifications_maxPrice <- notifications_cleaned %>% 
    filter(Key == "oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:maxPrice") %>%
    mutate(NotificationMaxPrice = as.numeric(Value)) %>%
    select(NotificationMaxPrice)
  # hist(log(notifications_maxPrice$NotificationMaxPrice), breaks = 100, col=rgb(1, 0, 0, 0.5))

  contracts_price <- contracts_cleaned %>% 
    filter(Key == "oos:price") %>%
    mutate(ContractsPrice = as.numeric(Value)) %>%
    select(ContractsPrice)
  # hist(log(contracts_price$ContractsPrice), breaks = 100, col=rgb(0, 0, 1, 0.5), add=T)  
  
  # HISTOGRAMS OF NOTIFICATION PRICES
  # Output three histograms for this region: listed price of notifications; same but only bottom three quartiles to show discontinuity at 500k; logged values across all dist
  notifications_maxPrice_three_quartiles <- notifications_maxPrice %>%
    filter(NotificationMaxPrice <= as.numeric(summary(notifications_maxPrice$NotificationMaxPrice)[5]))
  hist((notifications_maxPrice_three_quartiles$NotificationMaxPrice), breaks = 100)
  # A lot of notifications right below 500,000 rubles, see http://www.otc.ru/academy/articles/Kak_rabotat_s_zakupkami

  # All notifications
  graph_title <- paste0("Distribution of initial listing prices for tenders in ", current_region_english, " (all quartiles)\n")
    graph_file_name <- paste0(data_output_directory_region, current_region, "_notification_maxPrice_histogram_raw_all_quartiles.pdf")
  notification_maxPrice_histogram_raw_all_quartiles <- ggplot(notifications_maxPrice, aes(x=NotificationMaxPrice)) +
                                                        geom_histogram(bins = 200) +
                                                        labs(title = graph_title, x = "\nInitial listing price (rubles)") +
                                                        scale_x_continuous(labels = comma) +
                                                        theme_bw() +
                                                        annotate("text", x = 0.95*(max(notifications_maxPrice$NotificationMaxPrice, na.rm = T)), y = 0, label = "\nA few very large\ntenders in each\nregion skew the\ndistribution right", vjust = 0, hjust = 1)
  print(notification_maxPrice_histogram_raw_all_quartiles)
  ggsave(plot = notification_maxPrice_histogram_raw_all_quartiles, filename = graph_file_name, device = "pdf")
  
  # Bottom three quarters of notifications
  graph_title <- paste0("Distribution of initial listing prices for tenders in ", current_region_english, " (excluding fourth quartile)\n")
    graph_file_name <- paste0(data_output_directory_region, current_region, "_notification_maxPrice_histogram_raw_three_quartiles.pdf")
  notification_maxPrice_histogram_raw_three_quartiles <- ggplot(notifications_maxPrice_three_quartiles, aes(x=NotificationMaxPrice)) +
    geom_histogram(bins = 200) +
    labs(title = graph_title, x = "\nInitial listing price (rubles)") +
    scale_x_continuous(labels = comma) +
    theme_bw() +
    annotate("text", x = 450000, y = Inf, label = "\nRules change for\ntenders > 500k", vjust = 1, hjust = 1)
  print(notification_maxPrice_histogram_raw_three_quartiles)
  ggsave(plot = notification_maxPrice_histogram_raw_three_quartiles, filename = graph_file_name, device = "pdf")
  
  # All notifications, logged
  graph_title <- paste0("Distribution of initial listing prices (log base 10) for tenders in ", current_region_english, " (all quartiles)\n")
    graph_file_name <- paste0(data_output_directory_region, current_region, "_notification_maxPrice_histogram_log_all_quartiles.pdf")
  notification_maxPrice_histogram_log_all_quartiles <- ggplot(notifications_maxPrice, aes(x=NotificationMaxPrice)) +
    geom_histogram(bins = 200) +
    labs(title = graph_title, x = "\nInitial listing price (rubles, log base 10)") +
    scale_x_continuous(labels = comma, trans = "log10") +
    theme_bw() +
    annotate("text", x = 350000, y = Inf, label = "\nRules change for\ntenders > 500k", vjust = 1, hjust = 1)
  print(notification_maxPrice_histogram_log_all_quartiles)
  ggsave(plot = notification_maxPrice_histogram_log_all_quartiles, filename = graph_file_name, device = "pdf")
  
  
  # SUBSET DATA TO JUST NOTIFICATIONS/CONTRACTS WITH ONE LOT/PRODUCT/CUSTOMER
  # Identify and assess size of multiple-lot notifications
  notifications_lots_number <- notifications_cleaned %>%
                                filter(Key == "oos:lots/oos:lot/oos:ordinalNumber") %>%
                                group_by(BusinessKey) %>%
                                summarize(NumberOfLots = as.numeric(max(Value)))
  # table(notifications_lots_number$NumberOfLots) # 99.5% of Adygeja_Resp notifications had 1 lot; 98.9% of Moskva
  
  # Subset to just the single-lot notifications
  notifications_with_one_lot <- notifications_lots_number %>% filter(NumberOfLots == 1) %>% select(BusinessKey)
  notifications_single_lot <- notifications_cleaned %>%
                                right_join(notifications_with_one_lot)
  
  # Identify and assess size of multiple-product notifications (within single-lot subset)
  notifications_products_number <- notifications_single_lot %>%
                                    filter(Key == "oos:lots/oos:lot/oos:products/oos:product/oos:code") %>%
                                    group_by(BusinessKey) %>%
                                    summarize(NumberOfProducts = n(), NumberOfUniqueProducts = n_distinct(Value))
  # table(notifications_products_number$NumberOfProducts) 
    # 85.4% of Adygeja_Resp single-lot notifications had one product code
    # 92.3% in Moscow
    # eg Notification 0176200000113002872 has 42, but only one maxPrice, so can't easily split accross the products
    # (maybe possible later at higher level of product classification, eg these all start with 2423)
  # Note: reduction to single-lot probably accounts for fact NumberOfProducts = NumberOfUniqueProducts in all cases
  
  notifications_with_one_lot_one_product <- notifications_products_number %>% filter(NumberOfProducts == 1) %>% select(BusinessKey)
  notifications_single_lot_single_product <- notifications_single_lot %>%
                                              right_join(notifications_with_one_lot_one_product)
  
  # Count fields per business key (to check for remaining duplicate attributes)
  fields_per_business_key <- notifications_single_lot_single_product %>%
                              group_by(BusinessKey, Key) %>%
                              summarise(KeysPerBusinessKey = n())
  # Still a lot of duplicate fields! Eg http://zakupki.gov.ru/pgz/public/action/orders/info/common_info/show?notificationId=4853921
  # This is an auction for electricity covering many different departments, all at once, made by ministry of finance
  # Auction was cancelled by Federal Antimonopoly Service, so should be no contract
  # Good news: sum of max prices across the various parties equals published max price
  # Good example from Moscow: http://zakupki.gov.ru/pgz/public/action/orders/info/common_info/show?notificationId=2293597
  # One tender for many different contracts covering transport
  
  # Another example of pooled purchasing: http://zakupki.gov.ru/pgz/public/action/orders/info/common_info/show?notificationId=2501511
  # Many contracts resulted, sum of max prices across notifications equals published max price
  # DIFFICULTY IS OBVIOUSLY IN DETERMINING LINK BETWEEN NOTIFICATION AND CONTRACT, may need to sum contracts?
  # max_notification <- notifications_cleaned %>% filter(BusinessKey == "0176200000111001357" & Key == "oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:maxPrice")
    # sum(as.numeric(max_notification$Value))
  # max_contract <- contracts_cleaned %>% filter(BusinessKey == "0176200000111001357" & grepl("oos:product/oos:price", Key))
    # sum(as.numeric(max_contract$Value))
  # Notification max price: 1 639 423,44; but not as many linked contracts created as there should be, so need to restrict further
 
  # Examine which fields from notifications file are duplicated
  fields_with_duplicates <- fields_per_business_key %>%
                              filter(KeysPerBusinessKey > 1) %>%
                              group_by(Key) %>%
                              summarise(NumberOfTimesDuplicated = n())
  # Shows some cases come from notifications for more than one customer at a time, as the case above
  # Most come from oos:lots/oos:lot/oos:notificationFeatures/oos:notificationFeature/oos:placementFeature/oos:name
  # This appears to just be free-text commentary anyway, so can drop (now done back in clean-data.R)
  # Duplicates in Moscow from:
  # oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:customer/oos:fullName
  # oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:customer/oos:regNum
  # oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:maxPrice
  # oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:quantity
  # Eg http://zakupki.gov.ru/pgz/public/action/orders/info/common_info/show?notificationId=2293597
  # Easiest to drop these
  
  # Final restriction to the simplest notfications
  notifications_with_one_lot_one_product_one_customer <- notifications_single_lot_single_product %>%
                                                          group_by(BusinessKey, Key) %>%
                                                            summarise(ValuesPerKey = n()) %>% ungroup() %>%
                                                          group_by(BusinessKey) %>%
                                                            summarise(MaxFieldsPerNotification = max(ValuesPerKey)) %>% ungroup() %>%
                                                          filter(MaxFieldsPerNotification == 1) %>%
                                                          select(BusinessKey)
                                                          
  notifications_single_lot_single_product_single_customer <- notifications_single_lot_single_product %>%
                                                              right_join(notifications_with_one_lot_one_product_one_customer)
  
  # What percentage of all notifications remain? 84.7% for Adygeja, 90.7% for Moscow
  length(unique(notifications_single_lot_single_product_single_customer$BusinessKey)) / length(unique(notifications_cleaned$BusinessKey))
    
  ## PIVOT THE DEDUPLICATED DATA IN TO ONE ROW PER NOTIFICATION
  notifications <- notifications_single_lot_single_product_single_customer %>%
                    spread(key = Key, value = Value)
  
  # 497,844 notifications in Moscow; 12,581 in Adygeja
  notifications_wide_file_name <- paste0(data_output_directory_region, current_region, "_notifications_wide.rda")
  save(notifications, file = notifications_wide_file_name)
      
  ###################
  ## Contracts     ##
  ###################    
  
  # SUBSET DATA TO JUST NOTIFICATIONS/CONTRACTS WITH ONE LOT/PRODUCT/CUSTOMER
  # Identify and assess size of multiple-product contracts
  contracts_products_number <- contracts_cleaned %>%
                                filter(Key == "oos:products/oos:product/oos:OKDP/oos:code") %>%
                                group_by(BusinessKey) %>%
                                  summarize(NumberOfProducts = n())
  # table(contracts_products_number$NumberOfProducts)[1]/length(unique(contracts_cleaned$BusinessKey))
  # 60.8% of Adygeja_Resp contracts had one product code, means we are losing too much if just exclude all
  # 81% for Moscow
  # Eg 0176200000112000978 shows two product codes, but they are the same (and appear as a single lot/product/customer in notification)
  # Worst offenders seem to be libraries contracting for lots of books at once, probably all same product!
  
  # Can do better: only exclude those with multiple unique product codes
  contracts_products_number_unique <- contracts_cleaned %>%
                                        filter(Key == "oos:products/oos:product/oos:OKDP/oos:code") %>%
                                        group_by(BusinessKey) %>%
                                          summarize(NumberOfUniqueProducts = n_distinct(Value))
  # table(contracts_products_number_unique$NumberOfUniqueProducts)[1]/length(unique(contracts_cleaned$BusinessKey))
  # 82.8% of Adygeja_Resp contracts had one unique product code
  # 92% for Moscow
  
  # Do that all in one step
  contracts_products_combined <- contracts_cleaned %>%
                                   filter(Key == "oos:products/oos:product/oos:OKDP/oos:code") %>%
                                   group_by(BusinessKey) %>%
                                   summarize(NumberOfProducts = n(), NumberOfUniqueProducts = n_distinct(Value))
  # Notification 0376300000112000347 is good example: passed my checks to make it in to notifications dataset
  # When in contract stage the product codes become more diversified (from 1520000 to eg 1520111), still best to discard now
  # We can safely sum across prices for the same product code though, if needed later

  # Subset to just the single-product (code) contracts  
  contracts_with_one_product <- contracts_products_combined %>% filter(NumberOfUniqueProducts == 1) %>% select(BusinessKey)
  contracts_single_product <- contracts_cleaned %>%
                                right_join(contracts_with_one_product)
  
  # Count fields per business key (to check for remaining duplicate attributes)
  fields_per_business_key <- contracts_single_product %>%
                              group_by(BusinessKey, Key) %>%
                              summarise(KeysPerBusinessKey = n())
  
  # Examine which fields from notifications file are duplicated
  fields_with_duplicates <- fields_per_business_key %>%
                              filter(KeysPerBusinessKey > 1) %>%
                              group_by(Key) %>%
                              summarise(NumberOfTimesDuplicated = n())
  # Duplicate fields related to finance, products, and a few multiple-supplier contracts
  # Eg contract 0876100000413000002
  
  # SIMPLE FIX FOR NOW - DROP ALL THOSE CASES
  contracts_with_one_product_no_duplicates <- contracts_single_product %>%
                                                group_by(BusinessKey, Key) %>%
                                                  summarise(ValuesPerKey = n()) %>% ungroup() %>%
                                                group_by(BusinessKey) %>%
                                                  summarise(MaxFieldsPerContract = max(ValuesPerKey)) %>% ungroup() %>%
                                                filter(MaxFieldsPerContract == 1) %>%
                                                select(BusinessKey)
  
  contracts_single_product_no_duplicates <- contracts_single_product %>%
                                              right_join(contracts_with_one_product_no_duplicates)
  
  # What percentage of all contracts remain? 57.8% for Adygeja, 74.7% for Moscow (we lose quite a lot with blunt force!)
  length(unique(contracts_single_product_no_duplicates$BusinessKey)) / length(unique(contracts_cleaned$BusinessKey))
  
  # Can we do better by summing across same products? (or more problems because often for different orgs...)
  # contract_prices_summed <- contracts_single_product %>%
  #                             filter(Key == "oos:products/oos:product/oos:price") %>%
  #                             group_by(BusinessKey, DocumentVersionUniqueID, Key) %>%
  #                               summarise(Value = sum(as.numeric(Value))) %>%
  #                               mutate(Key = "ProductsPriceTotal")
  # 
  # contract_sums_summed <- contracts_single_product %>%
  #                             filter(Key == "oos:products/oos:product/oos:sum") %>%
  #                             group_by(BusinessKey, DocumentVersionUniqueID, Key) %>%
  #                               summarise(Value = sum(as.numeric(Value))) %>%
  #                               mutate(Key = "ProductsSumTotal")
  
  # Neither of these is a ton of help, better to just use oos:price for our purposes here, as it is never duplicated
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
                                                right_join(contracts_with_one_product_fields_collapsed)
                                                
  # What percentage of all contracts remain? 82.9% for Adygeja, % for Moscow (way better than brute force!)
  length(unique(contracts_single_product_fields_collapsed$BusinessKey)) / length(unique(contracts_cleaned$BusinessKey))
  
  ## PIVOT THE DEDUPLICATED DATA IN TO ONE ROW PER NOTIFICATION
  contracts <- contracts_single_product_fields_collapsed %>%
                spread(key = Key, value = Value)
  
  # 309,727 contracts in Moscow; 6,747 Adygeja
  contracts_wide_file_name <- paste0(data_output_directory_region, current_region, "_contracts_wide.rda")
  save(contracts, file = contracts_wide_file_name)
  

  ###################
  ## Merge         ##
  ###################  
  
  merged <- notifications %>% 
    left_join(contracts, by = c("oos:notificationNumber" = "oos:foundation/oos:order/oos:notificationNumber"))
  # Gives about a 50% match rate in Adygeja, almost 60% in Moscow
  # With improvements above, 73% in Adygeja, 
  
  matched <- merged %>% 
    filter(!is.na(BusinessKey.y)) %>%
    mutate(PriceChange = as.numeric(`oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:maxPrice`) - as.numeric(`oos:products/oos:product/oos:price`),
           PriceChangePercent = -100 * PriceChange / as.numeric(`oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:maxPrice`)) %>%
    filter(PriceChangePercent <= 5)
  
  # Quick graph
  hist(matched$PriceChangePercent, breaks = 100, main = "Moscow: change in price between tender notification and final contract (all procedures)",
       xlab = "Percentage difference between initial notification of tender and final contract")
  
  auctions <- matched %>% filter(`oos:placingWay/oos:name` == "Открытый аукцион в электронной форме")
  
  hist(auctions$PriceChangePercent, breaks = 100, main = "Moscow: change in price between tender notification and final contract (auction procedures)",
       xlab = "Percentage difference between initial notification of tender and final contract")
  
  # TO DO 
  # Roll back "single lot" on notifications, may not be necessary (LEAVE, don't lose much by it)
  # Go back and sum over the unique products to improve match rate (oos:products/oos:product/oos:price or oos:products/oos:product/oos:sum)
    # Also creates more problems than solves, for creating matches just use oos:price and drop the offending price/sum attributes
  # Double-check uniqueness on both sides of merge
  # Translate agency names to English
  # Recode the variables I will split by in to factor variables
  # Split by agency, procedure, then both
  # Then zoom in on canonical products
  # Reproduce the old scatterplot of agency budget by single-supplier
  
  #####
#  OLD CODE
  ####
  
  
  
      # Measure 1: what is the difference between notified and contracted price?
    notifications <- notifications_cleaned %>%
      filter(Key == "oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:maxPrice") %>%
      group_by(BusinessKey) %>%
      summarise(NotificationTotalMaxPrice = sum(as.numeric(Value)))
    
    notification_attributes_numeric <- c("oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:maxPrice")
    
    notification_attributes_character <- c("oos:order/oos:placer/oos:fullName")
    
    # Create a data frame with measures that are one-per-notification (so need to first aggregate things like maxPrice)
    notifications_wide <- notifications_cleaned %>%
      filter(Key %in% notification_attributes_singular) %>%
      spread_(key_col = "Key", value_col = "Value")

    notifications_wide <- notifications_cleaned %>%
      filter(Key %in% notification_attributes_numeric) %>%
      group_by(BusinessKey) %>%
      summarise(NotificationTotalMaxPrice = sum(as.numeric(Value)))
    
    notifications_numeric <- notifications_cleaned %>%
      filter(Key %in% notification_attributes_numeric) %>%
      group_by(BusinessKey, Key) %>%
        transmute(Value = as.numeric(Value)) %>%
        summarise(Value = sum(Value)) %>% # Could be combined with above
      ungroup() %>%
      spread(key = Key, value = Value) %>%
        rename(maxPrice = `oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:maxPrice`) # Move earlier
      
    test <- notifications_cleaned %>%
      group_by(BusinessKey, Key) %>%
      tally()
    
    
    # Split the data in to separate tables with appropriate grain
    
    # Add a counter argument to this
    select_keys_to_split <- function(OutputTable){
      keys_to_split <- parsing_configuration[parsing_configuration$OutputTable == OutputTable, 4]
    }
    
    # Easy case
    keys_to_split <- select_keys_to_split("notifications")
    output_table_easy <- notifications_cleaned %>%
      filter(Key %in% keys_to_split) %>%
      spread(key = Key, value = Value)
    
    # Tough case (needs counter argument)
    keys_to_split <- select_keys_to_split("notification_lots")
        
    output_table_hard <- notifications_cleaned %>%
      filter(Key %in% keys_to_split) %>%
      mutate(Counter = ifelse(Key == "oos:lots/oos:lot/oos:ordinalNumber", Value, NA))
    
    rows_with_UniqueID <- batch_output_key_value %>%
      add_rownames() %>%
      filter(Key == "oos:id") %>%
      select(rowname) %>%
      cbind(UniqueID)
    
 
          
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
    
    # Draw one histogram of efficiency measure per organisation
    
    
  } # Closes control loop over document_types_list in this region
  
} # Closes control loop over regions_list
                    
    
################################################
# 4. ? #
################################################

# ENDS