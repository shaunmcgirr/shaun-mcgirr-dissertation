# 4-Construct\extract-bidder-information.R

# Goals of this script are:
#   - Obtain list of regions which have come from clean-data.R
#   - Extract number of bidders vs number of bidders admitted from protocols

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
regions_list <- as.list("Adygeja_Resp")
# regions_list <- as.list(c("Moskva"))
regions_number <- length(regions_list)

##############################################
# 3. Define functions to process each region #
##############################################

# Loop over regions, processing them in turn
for(r in 1:regions_number){
  # r <- 1
  current_region <- as.character(regions_list[r])
  current_region_english <- generate_english_region_name(current_region)
  print(paste0("Starting to extract bidder information from protocols in ", current_region))
  
  # Create output directory
  data_output_directory_region <- paste0(data_output_directory, current_region, "/")
    suppressWarnings(dir.create(data_output_directory_region, recursive = T))

    # Load the next file to process (could be a useful function)
    file_to_process <- paste0(data_cleaned_directory, current_region, "/",
                              current_region, "_protocols_cleaned_",
                              data_download_date, ".rda")
    load(file=file_to_process)

    
  # Which protocols are for multi-lot auctions? Throw these away.
  protocols_lots_number <- protocols_cleaned %>%
      filter(Key == "oos:protocolLots/oos:protocolLot/oos:lotNumber") %>%
      group_by(BusinessKey, Key) %>%
        summarize(NumberOfLots = as.numeric(max(Value)))
  # table(protocols_lots_number$NumberOfLots)
  
  protocols_single_lot <- protocols_lots_number %>%
    filter(!is.na(NumberOfLots) & NumberOfLots == 1) %>%
    select(BusinessKey) %>%
    inner_join(protocols_cleaned)
  
  rm(protocols_cleaned); rm(protocols_lots_number); gc();
  
  # Alternate approach from here: just take the maximum number of applicants an admittees as that's by definition first step in the process
  # OK that it's goofy for Open Tender because that's the nature of procedure: everybody passes first stage to judging
  # Mass bidding for many contracts at once; whole point is cannot exclude up front (which is why rarely used)
  
  # Tally up, for each business key, rows with journalNumber!=NA per document
  applicants_per_business_key <- protocols_single_lot %>%
    filter(Key == "oos:protocolLots/oos:protocolLot/oos:applications/oos:application/oos:journalNumber" & !is.na(Value)) %>%
    group_by(BusinessKey, DocumentVersionUniqueID) %>%
      tally() %>% ungroup() %>%
    group_by(BusinessKey) %>%
      summarize(NumberOfApplicants = max(n)) %>% ungroup()
  
  # Tally up, for each business key, rows with admitted!=NA per document
  admitted_per_business_key <- protocols_single_lot %>%
    filter(Key == "oos:protocolLots/oos:protocolLot/oos:applications/oos:application/oos:admitted" & !is.na(Value) & Value == 1) %>%
    group_by(BusinessKey, DocumentVersionUniqueID) %>%
      tally() %>% ungroup() %>%
    group_by(BusinessKey) %>%
      summarize(NumberOfAdmittedApplicants = max(n)) %>% ungroup()
  
  # Merge both on to spine of business keys
  protocols_business_keys <- data.frame(BusinessKey = unique(protocols_single_lot$BusinessKey), stringsAsFactors = F)
  
  bidder_statistics <- unique(protocols_business_keys) %>%
    left_join(applicants_per_business_key) %>%
    left_join(admitted_per_business_key) %>%
    replace_na(list(NumberOfApplicants = 0, NumberOfAdmittedApplicants = 0)) %>%
    mutate(DisqualifiedApplicants = NumberOfApplicants - NumberOfAdmittedApplicants)
  
  # Still not quite right, shown by test case 0148200000511000011 http://zakupki.gov.ru/pgz/public/action/orders/info/commission_work_result/show?notificationId=171332
  # Should be 9 applicants, and 4 admitted, so need Value == 1 above
  # Fixed! Shows 5 bidders disqualified
  
  ### PREVIOUS APPROACH BEGINS HERE
  # Generally seems to be that the protocol I'm interest has no foundation protocol (ie its the first)
  # Some have up to five "first protocol", especially when cancellation involved http://zakupki.gov.ru/pgz/public/action/orders/info/common_info/show?notificationId=4341278
  # Comfortable restricting for now to those with one foundation protocol
  # Great example here, showing how successive protocols narrow the field, only need to worry about first stage though
  # http://zakupki.gov.ru/pgz/public/action/orders/info/commission_work_result/show?notificationId=6744059
  business_keys_with_multiple_foundation_protocols <- protocols_single_lot %>%
    filter(Key == "oos:foundationProtocolNumber" & is.na(Value)) %>%
    group_by(BusinessKey) %>%
      tally() %>% ungroup() %>% # table(business_keys_with_multiple_foundation_protocols$n)
    filter(n > 1) %>%
    select(BusinessKey)
  
  # Remove these then recalculate
  protocols_single_lot_single_foundation <- protocols_single_lot %>%
    anti_join(business_keys_with_multiple_foundation_protocols) %>% ungroup()
  
  foundation_protocol_identifiers <- protocols_single_lot_single_foundation %>%
    filter(Key == "oos:foundationProtocolNumber" & is.na(Value)) %>%
    group_by(BusinessKey, DocumentVersionUniqueID) %>%
    tally() %>% ungroup() %>%
    select(DocumentVersionUniqueID)
  
  # Reduce to just the foundation protocols then check
  foundation_protocols <- protocols_single_lot_single_foundation %>%
    inner_join(foundation_protocol_identifiers)
  stopifnot(length(unique(foundation_protocols$BusinessKey)) == length(unique(foundation_protocol_identifiers$DocumentVersionUniqueID)))
  
  rm(protocols_single_lot); rm(business_keys_with_multiple_foundation_protocols);
  rm(protocols_single_lot_single_foundation); rm(foundation_protocol_identifiers); gc();
  
  # How many applicants (journalNumber) and how many admitted through first stage?
  applicants_per_business_key <- foundation_protocols %>%
    filter(Key == "oos:protocolLots/oos:protocolLot/oos:applications/oos:application/oos:journalNumber" & !is.na(Value)) %>%
    group_by(BusinessKey) %>%
      summarize(NumberOfApplicants = n())
  
  admitted_per_business_key <- foundation_protocols %>%
    filter(Key == "oos:protocolLots/oos:protocolLot/oos:applications/oos:application/oos:admitted" & Value == 1) %>%
    group_by(BusinessKey) %>%
      summarize(NumberAdmittedApplications = n())
  
  foundation_protocols_business_keys <- data.frame(BusinessKey = unique(foundation_protocols$BusinessKey), stringsAsFactors = F)
  
  bidder_statistics <- unique(foundation_protocols_business_keys) %>%
    left_join(applicants_per_business_key) %>%
    left_join(admitted_per_business_key) %>%
    replace_na(list(NumberOfApplicants = 0, NumberAdmittedApplications = 0))
  # Doesn't work for open tender, admitted is next stage of proc, need to be robust to this

  rm(applicants_per_business_key); rm(admitted_per_business_key); rm(foundation_protocols_business_keys); rm(foundation_protocols); gc();

  # When joining, declare protocols with NA for these to have 0 (ie no applicants/none admitted)
  # With na.locf fill thing




  
  
  # Save the joined master DFs (product grouped and ungrouped)

    
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

    
  # Clean up
  rm(notifications_contracts_products_grouped); rm(notifications_contracts_products_ungrouped)
  print(paste0("Completed extracting bidder information from ", current_region))
  
} # Closes control loop over this region
gc()

# ENDS
