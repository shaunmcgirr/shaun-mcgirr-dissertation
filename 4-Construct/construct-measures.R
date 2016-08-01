# 4-Construct\construct-measures.R

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

# Function to count na in columns of data frame
na_count <- function(x) sapply(x, function(y) sum(is.na(y)))
# na_count(notification_contract_matches)

############################################
# 2. Gather parameters about the job ahead #
############################################

# Define where outputs (eg graphs) should go
data_output_directory <- set_data_subdirectory(data_directory, data_download_date, "output")

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
    file_to_process <- paste0(data_output_directory, current_region, "/",
                              current_region, "_notifications_contracts_products_grouped_",
                              data_download_date, ".rda")
    load(file=file_to_process)
    
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
  notifications_contracts_products_grouped[columns_to_numeric] <- lapply(notifications_contracts_products_grouped[columns_to_numeric], as.numeric)
  notifications_contracts_products_ungrouped[columns_to_numeric] <- lapply(notifications_contracts_products_ungrouped[columns_to_numeric], as.numeric)
    # hist(notifications_contracts_products_ungrouped$NotificationLotCustomerRequirementMaxPrice)
    # hist(notification_contract_matches$ContractPrice)
    # NotificationVersionNumber_table <- as.data.frame(table(notifications_contracts_products_ungrouped$NotificationVersionNumber))
    # ContractVersionNumber_table <- as.data.frame(table(notifications_contracts_products_ungrouped$ContractVersionNumber))
  
  # Convert several to factor
    columns_to_factor <- c("NotificationPlacingWayName",
                           "ContractCurrentContractStage")
    notifications_contracts_products_grouped[columns_to_factor] <- lapply(notifications_contracts_products_grouped[columns_to_factor], as.factor)
    notifications_contracts_products_ungrouped[columns_to_factor] <- lapply(notifications_contracts_products_ungrouped[columns_to_factor], as.factor)
      # NotificationPlacingWayName_table <- as.data.frame(table(notifications_contracts_products_ungrouped$NotificationPlacingWayName))
      # ContractCurrentContractStage_table <- as.data.frame(table(notifications_contracts_products_ungrouped$ContractCurrentContractStage))
    
  # Convert other one-offs
  notifications_contracts_products_grouped$NotificationPublishDate <- as.Date(substr(notifications_contracts_products_grouped$NotificationPublishDate, 1, 10))
    notifications_contracts_products_ungrouped$NotificationPublishDate <- as.Date(substr(notifications_contracts_products_ungrouped$NotificationPublishDate, 1, 10))
  notifications_contracts_products_grouped$ContractSignDate <- as.Date(notifications_contracts_products_grouped$ContractSignDate)
    notifications_contracts_products_ungrouped$ContractSignDate <- as.Date(notifications_contracts_products_ungrouped$ContractSignDate)
  notifications_contracts_products_grouped$ProcedureDuration <- notifications_contracts_products_grouped$ContractSignDate - notifications_contracts_products_grouped$NotificationPublishDate
    notifications_contracts_products_ungrouped$ProcedureDuration <- notifications_contracts_products_ungrouped$ContractSignDate - notifications_contracts_products_ungrouped$NotificationPublishDate
    # Draw histogram by date
    hist(as.numeric(notifications_contracts_products_grouped$ProcedureDuration), breaks = 100)
  
  ## NEW VARIABLES
  # Add region this tender came from
  notifications_contracts_products_grouped$TenderPostingRegion <- current_region
    notifications_contracts_products_ungrouped$TenderPostingRegion <- current_region
  notifications_contracts_products_grouped$TenderPostingRegionEnglish <- current_region_english
    notifications_contracts_products_ungrouped$TenderPostingRegionEnglish <- current_region_english
  
  # Price difference between notification max price and final contract price (and percentage change, and cleaned version)
  notifications_contracts_products_grouped$PriceChange <- -1 * (notifications_contracts_products_grouped$NotificationLotCustomerRequirementMaxPrice - notifications_contracts_products_grouped$ContractPrice)
    # hist(notifications_contracts_products_grouped$PriceChange, breaks = 100)  
  notifications_contracts_products_grouped$PriceChangePercentage <- notifications_contracts_products_grouped$PriceChange/notifications_contracts_products_grouped$NotificationLotCustomerRequirementMaxPrice * 100
    # summary(notifications_contracts_products_grouped$PriceChangePercentage)
    # hist(notifications_contracts_products_grouped$PriceChangePercentage, breaks = 100)
    # price_change_percentage_no_outliers <- notifications_contracts_products_grouped %>%
    #                                         filter(PriceChangePercentage <= 200) %>%
    #                                         select(PriceChangePercentage)
    # hist(price_change_percentage_no_outliers$PriceChangePercentage, breaks = 100)
  notifications_contracts_products_grouped$PriceChangePercentageNoOutliers <- ifelse(notifications_contracts_products_grouped$PriceChangePercentage <= 100,
                                                                                     notifications_contracts_products_grouped$PriceChangePercentage, NA)
    # hist(notifications_contracts_products_grouped$PriceChangePercentageNoOutliers, breaks = 100)
  
  # Same thing for notifications with ungrouped product info attached
  notifications_contracts_products_ungrouped$PriceChange <- -1 * (notifications_contracts_products_ungrouped$NotificationLotCustomerRequirementMaxPrice - notifications_contracts_products_ungrouped$ContractPrice)
  # hist(notifications_contracts_products_ungrouped$PriceChange, breaks = 100)  
  notifications_contracts_products_ungrouped$PriceChangePercentage <- notifications_contracts_products_ungrouped$PriceChange/notifications_contracts_products_ungrouped$NotificationLotCustomerRequirementMaxPrice * 100
  notifications_contracts_products_ungrouped$PriceChangePercentageNoOutliers <- ifelse(notifications_contracts_products_ungrouped$PriceChangePercentage <= 100,
                                                                                     notifications_contracts_products_ungrouped$PriceChangePercentage, NA)
  # hist(notifications_contracts_products_ungrouped$PriceChangePercentageNoOutliers, breaks = 100)
  
  # Procedure group (user friendly to do this via config file in Excel)
  mapping_NotificationPlacingWayName_to_procedure_group <- (read.xlsx("4-Construct/NotificationPlacingWayName_groupings.xlsx", 1)) %>%
    select(-Freq)
  
  notifications_contracts_products_grouped <- notifications_contracts_products_grouped %>%
    left_join(mapping_NotificationPlacingWayName_to_procedure_group, by = "NotificationPlacingWayName") %>%
    mutate(TenderProcedureGroup = factor(TenderProcedureGroup, 
                                         levels = c("Open electronic auction", "Open tender", "Request for quotes", "Olympic construction", "Preliminary selection", "Registration of interest in open tender"),
                                         ordered = F),
           TenderProcedureDiscretion = factor(TenderProcedureDiscretion, 
                                              levels = c("Lower discretion", "Medium discretion", "Higher discretion", "Other"), 
                                              ordered = T))
  
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
  notifications_contracts_products_grouped$NotificationRevised <- ifelse(notifications_contracts_products_grouped$NotificationVersionNumber == 1,
                                                                         "N", "Y")
  notifications_contracts_products_ungrouped$NotificationRevised <- ifelse(notifications_contracts_products_ungrouped$NotificationVersionNumber == 1,
                                                                         "N", "Y")
  table(notifications_contracts_products_ungrouped$NotificationVersionNumber); table(notifications_contracts_products_ungrouped$NotificationRevised);
  
  # Contract revised (binary)
  notifications_contracts_products_grouped$ContractRevised <- ifelse(notifications_contracts_products_grouped$ContractVersionNumber == 0,
                                                                         "N", "Y")
  notifications_contracts_products_ungrouped$ContractRevised <- ifelse(notifications_contracts_products_ungrouped$ContractVersionNumber == 0,
                                                                           "N", "Y")
  table(notifications_contracts_products_ungrouped$ContractVersionNumber); table(notifications_contracts_products_ungrouped$ContractRevised);
  
  # Current contract stage (user friendly)
  
  
###########
  
  ## TODO

    # MEASURES OF CORRUPTION
    # Initial notification listing price (by tender type) - distribution; by agency
    # Shares of each tender type (eg single supplier) - by agency


    # MEASURES OF AGENCY CHARACTERISTICS
    # Number of unique products (lowest and highest levels of OKDP) - by agency
    # Customer != placer (proxy for capacity) - by agency
    # •	Specificity of goods purchased by agency, defined as the inverse of the rarity with which a given product is purchased across all other agencies (the database includes detailed product codings that make such a measure possible: the score will be highest when only the given agency purchases this product, and lowest when every other agency also purchases it)
    # •	The Audit Chamber, the main oversight agency of the Executive, releases yearly reports on its activities, including the number of inquiries initiated in to each agency, and the `yield' of these inquiries in terms of cases forwarded to prosecutors. I am planning to use this data to proxy for constraints on agencies.
    # •	Corruption revelations from press reports, in which I would tag the agency mentioned. Exactly what this data would be measuring depends on how the formal model works out.
 
  
  # National official rating http://nrpz.ru/
  #   Criteria
  # Таб. 5. Основные показатели рынка закупок государственных закупщиков *
  #   - Средняя начальная цена конкурентного способа определения поставщиков, млн. руб.
  # - Среднее число поданных заявок
  # - Доля отклоненных заявок
  # - Среднее число участников закупочных процедур (или допущенных котировочных заявок)
  # - Доля закупочных процедур с 0 или 1 участником
  
  # •	Procedure choice
  #   o	Proportion of tenders in which the agency chooses a single supplier instead of any kind of competitive procedure (well-demonstrated in literature on public procurement to be correlated with procurement fraud)
  #   o	Correlation between proc choice and purchase size
  # •	Bidding environment
  #   o	Number of bidders
  #   o	Number of disqualifications
  # •	Procedure efficiency (start vs end)
  #   o	By procedure type
  #   o	Mean, median, and a measure of “crowding” in to space just below 100%
  #   o	Average increase in contract cost over the lifetime of the procedure (this detects variation in cost changes, another standard place to hide kickbacks)
  #   o Average price decrease in competitive auctions (bureaucrats choose a maximum/starting price, and potential suppliers bid down; procurement literature identifies this as a good measure for the tolerance of cartel behavior by suppliers)
  #   o	Increases as irregularities
  # •	Irregularities
  #   o	Start prices right at thresholds
  
 
  
  ######################################################
  # 4. Run scripts that create the individual measures #
  ######################################################

  source("4-Construct/measures/identify-agencies.R")
  source("4-Construct/measures/prices-for-specific-products.R")
  
  
  ###########################################################
  # 5. Save the dataframes containing measures to new files #
  ###########################################################

  # Measure scripts should return some kind of key-value pair? Maybe even CSV?
  # Perhaps Region/Agency/Measure/Month/Value?
  
    
} # Closes control loop over regions_list

# ENDS