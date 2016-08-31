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

# Helpers for aggregation
mean_na <- function(x){mean(x, na.rm = T)}
median_na <- function(x){median(x, na.rm = T)}

# Load classifications
source(file="3-Unpack/load-classifications.R")

############################################
# 2. Gather parameters about the job ahead #
############################################

# Where does data come in from?
data_output_directory <- set_data_subdirectory(data_directory, data_download_date, "output")
data_purchases_directory <- set_data_subdirectory(data_directory, data_download_date, "purchases")
data_purchases_directory_regions <- paste0(data_purchases_directory, "regions/")

# Define where outputs should go
data_agencies_directory <- set_data_subdirectory(data_directory, data_download_date, "agencies")
data_agencies_directory_regions <- paste0(data_agencies_directory, "regions/")

# Obtain list of regions for which consolidated data is available
# regions_list <- generate_regions_list(data_output_directory)
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

    print(paste0("Loading purchase data for ", current_region))

    file_to_process <- paste0(data_purchases_directory_regions,
                              current_region, "_purchases_",
                              data_download_date, ".rda")
    load(file=file_to_process)

  ## RECODE VARIABLES
  
    # Region-specific metadata
    source("4-Construct/measures/identify-agencies-purchases.R")
    
    
    ## DV
    # Bunching, Revisions, Disqualifications DONE
    # Favoritism DONE
    # Price increase/dramatic drop (but maybe not enough coverage?)
    # Less auditible quantities? SCRATCH
    # Proportion of spending in last month of financial (calendar) year
    # Single-supplier DONE
    
    ## IV from theory
    # Commonness aggregated several ways (and weighted by spending?) DONE (easy ones)
    # MaxPrice (Mean/Median of non-transformed variable), or total agency spend? DONE
    # Price change/auction efficiency DONE
  
    ## Controls from elsewhere
    # What to do about procedure group? Add as a multi-factor interaction, or simply use "dominant procedure" dummies?
    # Misc: listing duration; bidders per notification; single bidder DONE
    # Capacity: round number listing prices?
    # Dummy for listed commodity good?
    # Dominant procedure
    
    
    ## Check distributions of DVs and key IVs
    # hist(as.numeric.factor(purchases$AnyBunching))
    # hist(purchases$ProximityRuleThreshold)
    # hist(purchases$TotalRevisions)
    # hist(purchases$ProportionDisqualified)
    # hist(purchases$ProductProbabilityLevel1/max(purchases$ProductProbabilityLevel1, na.rm = T))
    # hist(purchases$ProductProbabilityLevel4Scaled)
    
    # Start with the easily summarized
    agencies <- purchases %>%
      filter(!is.na(AgencyID)) %>%
      left_join(agency_metadata) %>%
      group_by(TenderPostingRegion, AgencyID, AgencyName, AgencyRegion, AgencyRegionEnglish) %>%
      summarize(NumberOfPurchases = n(),
                MeanBinaryBunching = mean_na(as.numeric.factor(AnyBunching)),
                MeanContinuousBunching = mean_na(ProximityRuleThreshold),
                MedianContinuousBunching = median_na(ProximityRuleThreshold),
                MeanRevisions = mean_na(TotalRevisions),
                MedianRevisions = median_na(TotalRevisions),
                MeanDisqualifications = mean_na(ProportionDisqualified),
                MedianDisqualifications = median_na(ProportionDisqualified),
                MeanSupplierFavoritism = mean_na(FavoritismSimpleLog),
                MedianSupplierFavoritism = median_na(FavoritismSimpleLog),
                MeanProductCommonnessLevel4 = mean_na(ProductProbabilityLevel4Scaled),
                MedianProductCommonnessLevel4 = median_na(ProductProbabilityLevel4Scaled),
                MeanMaximumPrice = mean_na(NotificationLotCustomerRequirementMaxPrice),
                MedianMaximumPrice = median_na(NotificationLotCustomerRequirementMaxPrice),
                TotalAgencySpendInitial = (1 + sum(NotificationLotCustomerRequirementMaxPrice, na.rm = T)),
                # TotalAgencySpendFinal = sum(Price, na.rm = T),
                MeanAuctionEfficiency = mean_na(PriceChangePercentageNoOutliers),
                MedianAuctionEfficiency = median_na(PriceChangePercentageNoOutliers),
                MeanListingDuration = mean_na(ProcedureDuration),
                MedianListingDuration = median_na(ProcedureDuration),
                MeanBiddersApplied = mean_na(NumberOfApplicants),
                MedianBiddersApplied = median_na(NumberOfApplicants),
                MeanBiddersAdmitted = mean_na(NumberOfAdmittedApplicants),
                MedianBiddersAdmitted = median_na(NumberOfAdmittedApplicants)) %>% ungroup() %>%
                filter(AgencyID != "03731000866") %>% # Social investment fund, very unusual, huge leverage in models
                filter(NumberOfPurchases > 0)
    
                # Check matching between incoming regions and identified regions
                agencies$RegionMatch = ifelse(agencies$TenderPostingRegion == agencies$AgencyRegion, "Y", "N")
                stopifnot(length(agencies$RegionMatch) == length(agencies$RegionMatch[agencies$RegionMatch == "Y"]))
    
                
    # Single-supplier (only 10\% of Moscow agencies use it at least once, the worst offenders are really bad)
    single_supplier_usage <- purchases %>%
      group_by(AgencyID, Match) %>%
      count(Match) %>%
      mutate(MatchProportion = prop.table(n)) %>% ungroup() %>%
      filter(Match == "Contract without notification") %>%
      transmute(AgencyID, ProportionSingleSupplier = MatchProportion)
    # hist(single_supplier_usage$MatchProportion, breaks = 20)
    # plot(density(single_supplier_usage$MatchProportion))
    agencies <- agencies %>%
      left_join(single_supplier_usage)
    
    
    ## Now the tricky ones that need additional calculation at purchase level
    # Price increase/dramatic drop (but maybe not enough coverage?)
    # Good to separately calculate an agency-level purchase spec/product comm measure and compare to mean/median purchase level?
    # MeanProductCommonnessLevel1 = mean_na(ProductProbabilityLevel1/max(purchases$ProductProbabilityLevel1)),
    # MedianProductCommonnessLevel1 = median_na(ProductProbabilityLevel1/max(purchases$ProductProbabilityLevel1)),

    # Now the tricky ones that need their own sub-table
    # Dominant procedure
    # Proportion of spending in last month of financial (calendar) year
    # Proportion single-bidder up-front vs single bidder after disqual
    # Capacity: round number listing prices?
    # Dummy for listed commodity good?
    # ProportionSingleSupplier = length(purchases$Match[purchases$Match == "Contract without notification" & !is.na(purchases$Match)])/length(purchases$Match),
    # ProportionComplexPurchases = length(NotificationMissingReason) etc
    
    
    # Check every variable for NA, NaN, -Inf, Inf
    # Check every variable for NA, NaN, -Inf, Inf
    # Check every variable for NA, NaN, -Inf, Inf

    
    ## WHAT TO DO ABOUT TIME?
    # Try to graph some agency-level product commonness over time? Might be crazy, might work for larger agencies, eg ministry-level?

  
  ######################################################
  # 4. Run scripts that create the individual measures #
  ######################################################


  ###########################################################
  # 5. Save the dataframes containing measures to new files #
  ###########################################################

  # First specify variables to keep
  # agency_variables <- c("Year")

  rm(purchases); rm(single_supplier_usage); rm(agency_metadata); gc();

  # Save the file
  suppressWarnings(dir.create(data_agencies_directory_regions, recursive=TRUE))
  filename <- paste0(data_agencies_directory, "regions/", current_region, "_agencies_",
                     data_download_date, ".rda")
  save(agencies, file = filename)
  print(paste0("Saved ", current_region, " agencies"))

  rm(agencies); gc();
  
} # Closes control loop over regions_list

gc()

###################################################################
# 6. Read all those dataframes back in and combine them in to one #
###################################################################

agencies_file_list <- list.files(path = paste0(data_agencies_directory_regions), full.names = T)
agencies_file_list_length <- length(agencies_file_list)

load(file = agencies_file_list[1])
agencies_all <- agencies

for(p in 2:agencies_file_list_length){
  load(file = agencies_file_list[p])
  agencies_all <- rbind(agencies_all, agencies)
  gc()
  print(p)
}

save(agencies_all, file = "~/data/zakupki/2015-06-13/zakupki-2015-06-13-agencies-data/all_agencies_2015-06-13_compress.rda", compress = T)

rm(agencies_all); rm(agencies); gc()

# ENDS
