# 4-Construct\build-agency-level-data.R

# Goals of this script are:
#   - Obtain list of regions which have completed purchase-level data build
#   - Calculate all the agency-level measures
#   - Save it region-by-region then join it all together for final tests

###################
# 1. Housekeeping #
###################

# Load functions
source(file="3-Unpack/parse-files-functions.R")
source(file="4-Construct/construct-measures-functions.R")

# Load classifications
source(file="3-Unpack/load-classifications.R")

############################################
# 2. Gather parameters about the job ahead #
############################################

# Where does data come in from?
data_purchases_directory <- set_data_subdirectory(data_directory, data_download_date, "purchases")
data_purchases_directory_regions <- paste0(data_purchases_directory, "regions/")

# Define where outputs should go
data_agencies_directory <- set_data_subdirectory(data_directory, data_download_date, "agencies")

# Obtain list of regions for which consolidated data is available
# regions_list <- generate_regions_list(data_purchases_directory_regions)
regions_list <- as.list("Adygeja_Resp")
# regions_list <- as.list("Moskva")
regions_number <- length(regions_list)

#################################################
# 3. Load data, check quality, recode variables #
#################################################

# Loop over regions, processing them in turn
for(r in 1:regions_number){
  # r <- 1
  current_region <- as.character(regions_list[r])
  current_region_english <- generate_english_region_name(current_region)
  
    # Load both files: notifications_contracts_products_grouped, notifications_contracts_products_ungrouped
    # file_to_process <- paste0(data_output_directory, current_region, "/",
    #                           current_region, "_notifications_contracts_products_grouped_",
    #                           data_download_date, ".rda")
    # load(file=file_to_process)
    
    file_to_process <- paste0(data_purchases_directory_regions, "/",
                              current_region, "_purchases_",
                              data_download_date, ".rda")
    load(file=file_to_process)

  ## RECODE VARIABLES
  
    # Region-specific metadata
    source("4-Construct/measures/identify-agencies.R")
    
    
    ## DV
    # Bunching, Revisions, Disqualifications
    # Single-supplier
    # Favoritism
    # Price increase/dramatic drop (but maybe not enough coverage?)
    # Less auditible quantities?
    # Proportion of spending in last month of financial (calendar) year
    
    ## IV from theory
    # Commonness aggregated several ways (and weighted by spending?)
    # MaxPrice (Mean/Median of transformed variable), or total agency spend?
    # Role of auction efficiency or moot here?
    # Price change/auction efficiency
  
    ## Controls from elsewhere
    # What to do about procedure group? Add as a multi-factor interaction, or simply use "dominant procedure" dummies?
    # Misc: listing duration; bidders per notification; single bidder
    # Capacity: round number listing prices?
    
    
      
  

  
  ######################################################
  # 4. Run scripts that create the individual measures #
  ######################################################


  
  
  
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
  
  # Drop those variables we don't need, merge on bidder stats
  purchases <- select(purchases, one_of(purchase_variables)) %>% 
    left_join(bidder_statistics, by = c("NotificationNumber" = "BusinessKey")) %>%
    mutate(ProportionDisqualified = ifelse(is.na(NumberOfApplicants) | NumberOfApplicants == 0, NA, DisqualifiedApplicants/NumberOfApplicants))
  
  rm(notifications_contracts_products_ungrouped); rm(bidder_statistics); rm(bidder_statistics_file_name);
  rm(product_probabilities_level_1); rm(product_probabilities_level_4); gc();
  
  # Add favoritism measures at product level
  suppliers_per_product <- purchases %>%
    filter(!is.na(NotificationLotProductCode) & !is.na(ContractSupplierParticipantINN)) %>%
    group_by(NotificationLotProductCode, ProductProbabilityLevel4Scaled) %>%
    summarize(NumberOfSuppliersProduct = n_distinct(ContractSupplierParticipantINN)) %>%
    left_join(okdp_product_classification, by = c("NotificationLotProductCode" = "ProductCode"))
  
  repeat_winners_product <- purchases %>%
    filter(!is.na(AgencyID) & !is.na(ContractSupplierParticipantINN) & !is.na(NotificationLotProductCode)) %>%
    group_by(AgencyID, NotificationLotProductCode) %>%
    summarize(NumberOfSuppliers = n_distinct(ContractSupplierParticipantINN),
              NumberOfPurchases = n()) %>% ungroup() %>%
    mutate(PurchasesPerSupplier = NumberOfPurchases/NumberOfSuppliers) %>%
    left_join(suppliers_per_product, by = c("NotificationLotProductCode" = "NotificationLotProductCode")) %>%
    mutate(SupplierUnderusage = 1 - (NumberOfSuppliers/NumberOfSuppliersProduct),
           FavoritismRaw = SupplierUnderusage * (NumberOfPurchases - NumberOfSuppliers),
           FavoritismRawLog = log(FavoritismRaw+1),
           FavoritismSimple = ifelse(NumberOfSuppliers==NumberOfPurchases, 0, (NumberOfSuppliersProduct/NumberOfSuppliers)*(NumberOfPurchases-NumberOfSuppliers)), 
           FavoritismSimpleLog = ifelse(NumberOfSuppliers==NumberOfPurchases, 0, log((NumberOfSuppliersProduct/NumberOfSuppliers)*(NumberOfPurchases-NumberOfSuppliers))),
           FavoritismOdds = (NumberOfPurchases/NumberOfSuppliers)/(NumberOfPurchases/NumberOfSuppliersProduct),
           FavoritismOddsLog = log(FavoritismOdds)) %>%
    dplyr::select(AgencyID, NotificationLotProductCode, FavoritismRawLog, FavoritismSimpleLog, FavoritismOdds, FavoritismOddsLog)
    # Odds still not quite right, should 1 purchase from 1 supplier (when there are 2134 available) be the maximum?
    # Simple log is better conceptually
  
  purchases <- purchases %>%
    left_join(repeat_winners_product)
  
  # Save the file
  suppressWarnings(dir.create(data_purchases_directory_regions, recursive=TRUE))
  filename <- paste0(data_purchases_directory, "regions/", current_region, "_purchases_",
                     data_download_date, ".rda")
  save(purchases, file = filename)
  print(paste0("Saved ", current_region, " purchases"))

  rm(suppliers_per_product); rm(repeat_winners_product); rm(purchases); gc();
  
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
