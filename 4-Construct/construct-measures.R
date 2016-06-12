# 4-Construct\construct-measures.R

# Goals of this script are:
#   - Obtain list of regions which have completed matching from pivot-and-merge.R
#   - Process these to create measures to test my theory
#   - Profit!!!

###################
# 1. Housekeeping #
###################

# Load functions
source(file="3-Unpack/parse-files-functions.R")
source(file="4-Construct/construct-measures-functions.R")

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
# 3. Load data #
##############################################

# Loop over regions, processing them in turn
for(r in 1:regions_number){
  # r <- 1
  current_region <- as.character(regions_list[r])
  current_region_english <- generate_english_region_name(current_region)
  
    # Load the file of matches
    file_to_process <- paste0(data_output_directory, current_region, "/",
                              current_region, "_notification_contract_matches_",
                              data_download_date, ".rda")
    load(file=file_to_process)
  
  # REMOVE matches of low quality
  
  
###############
# 4. Analysis #
###############
    
    # TO DO 
    # Translate agency names to English
    # Recode the variables I will split by in to factor variables
    # Split by agency, procedure, then both
    # Then zoom in on canonical products
    # Reproduce the old scatterplot of agency budget by single-supplier
    
  
  # Distributions of the raw variables of interest
  # notifications_maxPrice 
  # contracts_price  
  
  # HISTOGRAMS OF NOTIFICATION PRICES
  # Output three histograms for this region: listed price of notifications; same but only bottom three quartiles to show discontinuity at 500k; logged values across all dist

    
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
  
  
  
  
  matched <- merged %>% 
    filter(!is.na(BusinessKey.y)) %>%
    mutate(PriceChange = as.numeric(`oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:maxPrice`) - as.numeric(`oos:price`),
           PriceChangePercent = -100 * PriceChange / as.numeric(`oos:lots/oos:lot/oos:customerRequirements/oos:customerRequirement/oos:maxPrice`)) %>%
    filter(PriceChangePercent <= 5)
  
  # Quick graph
  hist(matched$PriceChangePercent, breaks = 100, main = "Moscow: change in price between tender notification and final contract (all procedures)",
       xlab = "Percentage difference between initial notification of tender and final contract")
  
  auctions <- matched %>% filter(`oos:placingWay/oos:name` == "Открытый аукцион в электронной форме")
  
  hist(auctions$PriceChangePercent, breaks = 100, main = "Moscow: change in price between tender notification and final contract (auction procedures)",
       xlab = "Percentage difference between initial notification of tender and final contract")
  
  
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
    
    
} # Closes control loop over regions_list
                    
    
################################################
# 4. ? #
################################################

# ENDS