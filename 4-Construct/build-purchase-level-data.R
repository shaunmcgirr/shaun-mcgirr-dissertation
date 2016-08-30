# 4-Construct\build-purchase-level-data.R

# Goals of this script are:
#   - Obtain list of regions which have completed matching from pivot-and-merge.R
#   - Set up various data frames, then call the scripts that construct individual measures
#   - Store the resulting measures somewhere

###################
# 1. Housekeeping #
###################

# Load functions
source(file="3-Unpack/parse-files-functions.R")
source(file="4-Construct/construct-measures-functions.R")

# Load classifications
source(file="3-Unpack/load-classifications.R")

# Procedure group (user friendly to do this via config file in Excel)
mapping_NotificationPlacingWayName_to_procedure_group <- (read.xlsx("4-Construct/NotificationPlacingWayName_groupings.xlsx", 1)) %>%
  select(-Freq)

# Function to count na in columns of data frame
na_count <- function(x) sapply(x, function(y) sum(is.na(y)))
# na_count(notification_contract_matches)

############################################
# 2. Gather parameters about the job ahead #
############################################

# Where does data come in from?
data_output_directory <- set_data_subdirectory(data_directory, data_download_date, "output")

# Define where outputs should go
data_purchases_directory <- set_data_subdirectory(data_directory, data_download_date, "purchases")
data_purchases_directory_regions <- paste0(data_purchases_directory, "regions/")

# Obtain list of regions for which consolidated data is available
# regions_list <- generate_regions_list(data_output_directory)
# regions_list <- as.list("Adygeja_Resp")
regions_list <- as.list("Moskva")
regions_number <- length(regions_list)

#################################################
# 3. Load data, check quality, recode variables #
#################################################

# Loop over regions, processing them in turn
for(r in 1:regions_number){
  # r <- 1
  current_region <- as.character(regions_list[r])
  current_region_english <- generate_english_region_name(current_region)
  data_output_directory_region <- paste0(data_output_directory, current_region, "/")
  
    # Load both files: notifications_contracts_products_grouped, notifications_contracts_products_ungrouped
    # file_to_process <- paste0(data_output_directory, current_region, "/",
    #                           current_region, "_notifications_contracts_products_grouped_",
    #                           data_download_date, ".rda")
    # load(file=file_to_process)
    
    file_to_process <- paste0(data_output_directory, current_region, "/",
                              current_region, "_notifications_contracts_products_ungrouped_",
                              data_download_date, ".rda")
    load(file=file_to_process)

    
  ## RECODE VARIABLES
  # Convert several to numeric
  columns_to_numeric <- c("NotificationLotCustomerRequirementMaxPrice",
                          "NotificationVersionNumber",
                          "ContractPrice",
                          "ContractVersionNumber")
  # notifications_contracts_products_grouped[columns_to_numeric] <- lapply(notifications_contracts_products_grouped[columns_to_numeric], as.numeric)
  notifications_contracts_products_ungrouped[columns_to_numeric] <- lapply(notifications_contracts_products_ungrouped[columns_to_numeric], as.numeric)
    # hist(notifications_contracts_products_ungrouped$NotificationLotCustomerRequirementMaxPrice)
    # hist(notification_contract_matches$ContractPrice)
    # NotificationVersionNumber_table <- as.data.frame(table(notifications_contracts_products_ungrouped$NotificationVersionNumber))
    # ContractVersionNumber_table <- as.data.frame(table(notifications_contracts_products_ungrouped$ContractVersionNumber))
  
  # Convert several to factor
    columns_to_factor <- c("NotificationPlacingWayName",
                           "ContractCurrentContractStage")
    # notifications_contracts_products_grouped[columns_to_factor] <- lapply(notifications_contracts_products_grouped[columns_to_factor], as.factor)
    notifications_contracts_products_ungrouped[columns_to_factor] <- lapply(notifications_contracts_products_ungrouped[columns_to_factor], as.factor)
      # NotificationPlacingWayName_table <- as.data.frame(table(notifications_contracts_products_ungrouped$NotificationPlacingWayName))
      # ContractCurrentContractStage_table <- as.data.frame(table(notifications_contracts_products_ungrouped$ContractCurrentContractStage))
    
  # Convert other one-offs
  # notifications_contracts_products_grouped$NotificationPublishDate <- as.Date(substr(notifications_contracts_products_grouped$NotificationPublishDate, 1, 10))
    notifications_contracts_products_ungrouped$NotificationPublishDate <- as.Date(substr(notifications_contracts_products_ungrouped$NotificationPublishDate, 1, 10))
  # notifications_contracts_products_grouped$ContractSignDate <- as.Date(notifications_contracts_products_grouped$ContractSignDate)
    notifications_contracts_products_ungrouped$ContractSignDate <- as.Date(notifications_contracts_products_ungrouped$ContractSignDate)
  # notifications_contracts_products_grouped$ProcedureDuration <- as.numeric(notifications_contracts_products_grouped$ContractSignDate - notifications_contracts_products_grouped$NotificationPublishDate)
    notifications_contracts_products_ungrouped$ProcedureDuration <- as.numeric(notifications_contracts_products_ungrouped$ContractSignDate - notifications_contracts_products_ungrouped$NotificationPublishDate)
    # Draw histogram by date
    # hist(as.numeric(notifications_contracts_products_grouped$ProcedureDuration), breaks = 100)
  
  ## NEW VARIABLES
  # Add region this tender came from
  # notifications_contracts_products_grouped$TenderPostingRegion <- current_region
    notifications_contracts_products_ungrouped$TenderPostingRegion <- current_region
  # notifications_contracts_products_grouped$TenderPostingRegionEnglish <- current_region_english
    notifications_contracts_products_ungrouped$TenderPostingRegionEnglish <- current_region_english
  
  # Price difference between notification max price and final contract price (and percentage change, and cleaned version)
  # notifications_contracts_products_grouped$PriceChange <- -1 * (notifications_contracts_products_grouped$NotificationLotCustomerRequirementMaxPrice - notifications_contracts_products_grouped$ContractPrice)
    # hist(notifications_contracts_products_grouped$PriceChange, breaks = 100)  
  # notifications_contracts_products_grouped$PriceChangePercentage <- notifications_contracts_products_grouped$PriceChange/notifications_contracts_products_grouped$NotificationLotCustomerRequirementMaxPrice * 100
    # summary(notifications_contracts_products_grouped$PriceChangePercentage)
    # hist(notifications_contracts_products_grouped$PriceChangePercentage, breaks = 100)
    # price_change_percentage_no_outliers <- notifications_contracts_products_grouped %>%
    #                                         filter(PriceChangePercentage <= 200) %>%
    #                                         select(PriceChangePercentage)
    # hist(price_change_percentage_no_outliers$PriceChangePercentage, breaks = 100)
  # notifications_contracts_products_grouped$PriceChangePercentageNoOutliers <- ifelse(notifications_contracts_products_grouped$PriceChangePercentage <= 100,
  #                                                                                   notifications_contracts_products_grouped$PriceChangePercentage, NA)
    # hist(notifications_contracts_products_grouped$PriceChangePercentageNoOutliers, breaks = 100)
  
  # Same thing for notifications with ungrouped product info attached
  notifications_contracts_products_ungrouped$PriceChange <- -1 * (notifications_contracts_products_ungrouped$NotificationLotCustomerRequirementMaxPrice - notifications_contracts_products_ungrouped$ContractPrice)
  # hist(notifications_contracts_products_ungrouped$PriceChange, breaks = 100)  
  notifications_contracts_products_ungrouped$PriceChangePercentage <- notifications_contracts_products_ungrouped$PriceChange/notifications_contracts_products_ungrouped$NotificationLotCustomerRequirementMaxPrice * 100
  notifications_contracts_products_ungrouped$PriceChangePercentageNoOutliers <- ifelse(notifications_contracts_products_ungrouped$PriceChangePercentage <= 100,
                                                                                     notifications_contracts_products_ungrouped$PriceChangePercentage, NA)
  # hist(notifications_contracts_products_ungrouped$PriceChangePercentageNoOutliers, breaks = 100)
  

  
  # notifications_contracts_products_grouped <- notifications_contracts_products_grouped %>%
  #   left_join(mapping_NotificationPlacingWayName_to_procedure_group, by = "NotificationPlacingWayName") %>%
  #   mutate(TenderProcedureGroup = factor(TenderProcedureGroup, 
  #                                        levels = c("Open electronic auction", "Open tender", "Request for quotes", "Olympic construction", "Preliminary selection", "Registration of interest in open tender"),
  #                                        ordered = F),
  #          TenderProcedureDiscretion = factor(TenderProcedureDiscretion, 
  #                                             levels = c("Lower discretion", "Medium discretion", "Higher discretion", "Other"), 
  #                                             ordered = T))
  
  notifications_contracts_products_ungrouped <- notifications_contracts_products_ungrouped %>%
    left_join(mapping_NotificationPlacingWayName_to_procedure_group, by = "NotificationPlacingWayName") %>%
    mutate(TenderProcedureGroup = factor(TenderProcedureGroup, 
                                         levels = c("Open electronic auction", "Open tender", "Request for quotes", "Olympic construction", "Preliminary selection", "Registration of interest in open tender"),
                                         ordered = F),
           TenderProcedureDiscretion = factor(TenderProcedureDiscretion, 
                                              levels = c("Lower discretion", "Medium discretion", "Higher discretion", "Other"), 
                                              ordered = T))
  
  # Check that missing is only for a good reason
  stopifnot(sum(is.na(notifications_contracts_products_ungrouped$NotificationPlacingWayName)) == sum(is.na(notifications_contracts_products_ungrouped$TenderProcedureDiscretion)))
  

  # Notification revised (binary)
  # notifications_contracts_products_grouped$NotificationRevised <- ifelse(notifications_contracts_products_grouped$NotificationVersionNumber == 1,
  #                                                                        "N", "Y")
  notifications_contracts_products_ungrouped$NotificationRevised <- ifelse(notifications_contracts_products_ungrouped$NotificationVersionNumber == 1,
                                                                         "N", "Y")
  # table(notifications_contracts_products_ungrouped$NotificationVersionNumber); table(notifications_contracts_products_ungrouped$NotificationRevised);
  
  # Contract revised (binary)
  # notifications_contracts_products_grouped$ContractRevised <- ifelse(notifications_contracts_products_grouped$ContractVersionNumber == 0,
  #                                                                        "N", "Y")
  notifications_contracts_products_ungrouped$ContractRevised <- ifelse(notifications_contracts_products_ungrouped$ContractVersionNumber == 0,
                                                                           "N", "Y")
  # table(notifications_contracts_products_ungrouped$ContractVersionNumber); table(notifications_contracts_products_ungrouped$ContractRevised);
  
  # Total revisions (continuous)
  notifications_contracts_products_ungrouped$TotalRevisions <- (notifications_contracts_products_ungrouped$NotificationVersionNumber - 1) + notifications_contracts_products_ungrouped$ContractVersionNumber
  # table(notifications_contracts_products_ungrouped$TotalRevisions)
  
  # Current contract stage (user friendly)
  
  
###########
  
  ## TODO
  # Turn the individual measures in prove-measures.R in to individual scripts and call from below
  
  ######################################################
  # 4. Run scripts that create the individual measures #
  ######################################################

  source("4-Construct/measures/identify-agencies.R")
  # source("4-Construct/measures/prices-for-specific-products.R")
  
  # Probably merge in the protocols data here
  
  
  # Put in the purchase-level recodings here
  purchases <- notifications_contracts_products_ungrouped %>%
    # filter(!is.na(NotificationLotCustomerRequirementMaxPrice)) %>%
    rename(AgencyID = ContractCustomerRegNum) %>%
    mutate(BunchedAtThreshold = ifelse(TenderProcedureGroup == "Request for quotes" & NotificationLotCustomerRequirementMaxPrice > (0.95 * 500000) & NotificationLotCustomerRequirementMaxPrice < (1.05 * 500000), "Bunched at request for quotes threshold",
                                       ifelse(TenderProcedureGroup == "Open electronic auction" & NotificationLotCustomerRequirementMaxPrice > (0.95 * 3000000) & NotificationLotCustomerRequirementMaxPrice < (1.05 * 3000000), "Bunched at open electronic auction threshold", "Not bunched")),
           AnyBunching = as.factor(ifelse(BunchedAtThreshold == "Not bunched", 0, 1)),
           SubstantialRevisions = as.factor(ifelse(ContractRevised == "N" | NotificationRevised == "N", 0, 1)),
           Year = as.factor(substr(NotificationPublishDate, 1, 4)),
           ProximityRuleThreshold = ifelse(TenderProcedureGroup == "Request for quotes" & NotificationLotCustomerRequirementMaxPrice <= 505000, 1*(1-abs((NotificationLotCustomerRequirementMaxPrice - 500000)/500000)),
                                         ifelse(TenderProcedureGroup == "Open electronic auction" & NotificationLotCustomerRequirementMaxPrice <= 3030000, 1*(1-abs((NotificationLotCustomerRequirementMaxPrice - 3000000)/3000000)), NA)),
           PriceChangePercentageNegativeOnly = ifelse(PriceChangePercentageNoOutliers <= 0, PriceChangePercentageNoOutliers, NA),
           PriceChangePercentageLessThanTen = ifelse(PriceChangePercentageNoOutliers <= 10, PriceChangePercentageNoOutliers, NA))
  
  
  # Vector of level 1 product probabilities
  product_probabilities_level_1 <- notifications_contracts_products_ungrouped %>%
    filter(!is.na(NotificationLotProductCodeLevel1)) %>%
    group_by(NotificationLotProductCodeLevel1) %>%
    count(NotificationLotProductCodeLevel1) %>%
    mutate(ProductProbabilityLevel1 = prop.table(n)) %>% ungroup() %>%
    select(-n)
  # hist(product_probabilities_level_1$ProductProbabilityLevel1, breaks = 30)
  # Most common is construction/repair, least is chemicals, makes sense
  
  product_probabilities_level_4 <- notifications_contracts_products_ungrouped %>%
    filter(!is.na(NotificationLotProductCode)) %>%
    group_by(NotificationLotProductCode) %>%
    count(NotificationLotProductCode) %>%
    mutate(ProductProbabilityLevel4 = prop.table(n)) %>% ungroup() %>%
    select(-n)
  # hist(log10(product_probabilities_level_4$ProductProbabilityLevel4), breaks = 30)
  # Most common is landscaping, then building, also makes sense
  
  # Agency spending as proxy for capacity
  agency_spending <- notifications_contracts_products_ungrouped %>%
    filter(!is.na(ContractPrice)) %>%
    rename(AgencyID = ContractCustomerRegNum) %>%
    group_by(AgencyID) %>%
    summarize(TotalAgencySpending = sum(ContractPrice))
  
  # Merge these on to test data
  purchases <- purchases %>%
    left_join(product_probabilities_level_1) %>%
    left_join(product_probabilities_level_4) %>%
    left_join(agency_spending)
  
  # Create scaled version of product probability
  ProductProbabilityLevel4Scaled_max <- max((purchases$ProductProbabilityLevel4), na.rm = T)
  purchases$ProductProbabilityLevel4Scaled <- (purchases$ProductProbabilityLevel4)/ProductProbabilityLevel4Scaled_max
  purchases$PurchaseSpecificity <- -1*purchases$ProductProbabilityLevel4Scaled
  
  
  
  
  ###########################################################
  # 5. Save the dataframes containing measures to new files #
  ###########################################################

  # First specify variables to keep
  purchase_variables <- c("NotificationNumber", "NotificationOrderName", "Year", "TenderProcedureGroup",
                          "NotificationPublishDate", "ContractCustomerFullName", "AgencyID",
                          "NotificationLotCustomerRequirementMaxPrice", "PriceChange", "PriceChangePercentage",
                          "PriceChangePercentageNoOutliers", "PriceChangePercentageNegativeOnly", "PriceChangePercentageLessThanTen",
                          "ProximityRuleThreshold", "ProductProbabilityLevel1", "ProductProbabilityLevel4Scaled",
                          "AnyBunching", "TotalRevisions",
                          "NotificationPage", "TenderPostingRegion", "TenderPostingRegionEnglish", "TotalAgencySpending",
                          "NotificationLotProductCodeLevel1", "NotificationLotProductCode", "NotificationLotProductName", "NotificationLotSubject",
                          "ContractCurrentContractStage", "ContractSignDate", "ContractSupplierParticipantINN", "ContractSupplierParticipantOrganizationName",
                          "Match", "NotificationMissingReason", "ContractMissingReason", "ProcedureDuration")
                          # "ContractFinanceBudgetName", "NotificationVersionNumber", "ContractVersionNumber",

  # Load bidder statistics here
  bidder_statistics_file_name <- paste0(data_output_directory_region, current_region, "_bidder_statistics_", data_download_date, ".rda")
  load(bidder_statistics_file_name)
  
  # Drop those we don't need
  purchases <- select(purchases, one_of(purchase_variables)) %>% 
    left_join(bidder_statistics, by = c("NotificationNumber" = "BusinessKey"))
  rm(notifications_contracts_products_ungrouped); rm(bidder_statistics); rm(bidder_statistics_file_name);
  
  # Save the file
  suppressWarnings(dir.create(data_purchases_directory_regions, recursive=TRUE))
  filename <- paste0(data_purchases_directory, "regions/", current_region, "_purchases_",
                     data_download_date, ".rda")
  save(purchases, file = filename)
  print(paste0("Saved ", current_region, " purchases"))
  
} # Closes control loop over regions_list
gc()

###################################################################
# 6. Read all those dataframes back in and combine them in to one #
###################################################################

purchases_file_list <- list.files(path = paste0(data_purchases_directory_regions), full.names = T)
purchases_file_list_length <- length(purchases_file_list)

load(file = purchases_file_list[1])
purchases_all <- purchases

for(p in 2:purchases_file_list_length){
  load(file = purchases_file_list[p])
  purchases_all <- rbind(purchases_all, purchases)
  gc()
  print(p)
}

save(purchases_all, file = "~/data/zakupki/2015-06-13/zakupki-2015-06-13-purchases-data/all_purchases_2015-06-13_compress.rda", compress = T)


# ENDS
