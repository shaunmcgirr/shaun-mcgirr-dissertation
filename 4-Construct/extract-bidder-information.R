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

  rm(protocols_single_lot); rm(protocols_business_keys); rm(applicants_per_business_key); rm(admitted_per_business_key); gc();
  
  # Save the bidder statistics
  bidder_statistics_file_name <- paste0(data_output_directory_region,
                                        current_region, "_bidder_statistics_",
                                        data_download_date, ".rda")
  save(bidder_statistics, file = bidder_statistics_file_name)
    
  ###################
  ## Report stats  ##
  ###################  

  # Clean up
  rm(bidder_statistics); rm(bidder_statistics_file_name)
  print(paste0("Completed extracting bidder information from ", current_region))
  
} # Closes control loop over this region
gc()

# ENDS
