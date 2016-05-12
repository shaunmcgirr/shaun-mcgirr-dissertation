# 4-Construct\construct-measures.R

# Goals of this script are:
#   - Obtain list of regions which have completed step 3 (unpack)
#   - Process these to create measures to test my theory
#   - Profit!!!

###################
# 1. Housekeeping #
###################

# Load functions
# source(file="3-Unpack/parse-files-functions.R")
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
  
  
  # SUBSET DATA TO JUST NOTIFICATIONS/CONTRACTS WITH ONE LOT
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
  
  # Identify and assess size of multiple-product notifications (within multiple-lot subset)
  notifications_products_number <- notifications_single_lot %>%
                                    filter(Key == "oos:lots/oos:lot/oos:products/oos:product/oos:code") %>%
                                    group_by(BusinessKey) %>%
                                    summarize(NumberOfProducts = n())
  # table(notifications_products_number$NumberOfProducts) 
    # 85.4% of Adygeja_Resp single-lot notifications had one product code
    # eg Notification 0176200000113002872 has 42, but only one maxPrice, so can't easily split accross the products
    # (maybe possible later at higher level of product classification, eg these all start with 2423)
  
  notifications_with_one_lot_one_product <- notifications_products_number %>% filter(NumberOfProducts == 1) %>% select(BusinessKey)
  notifications_single_lot_single_product <- notifications_single_lot %>%
                                              right_join(notifications_with_one_lot_one_product)
  
  
  
  
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