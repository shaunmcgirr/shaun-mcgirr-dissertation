# 4-Construct\measures\identify-agencies-purchases.R

# Select the best metadata for each unique agency in the system (from purchase-level data)
# Identified by NotificationP
                                        
agency_metadata_products_ungrouped <- purchases %>%
  transmute(AgencyID = AgencyID,
            AgencyName = ContractCustomerFullName,
            LengthOfAgencyName = nchar(AgencyName)) %>%
  filter(!is.na(AgencyID) & !is.na(AgencyName)) %>%
  group_by(AgencyID, AgencyName) %>%
    summarize(ShortestAgencyName = min(LengthOfAgencyName)) %>%
    arrange(-ShortestAgencyName) %>%
    filter(row_number() == 1) %>% ungroup() %>%
  select(-ShortestAgencyName)  

# Did we get the same for the two versions?
# stopifnot(identical(agency_metadata_products_grouped, agency_metadata_products_ungrouped))

# If so, simplify
agency_metadata <- agency_metadata_products_ungrouped %>%
                    mutate(AgencyRegion = current_region,
                           AgencyRegionEnglish = current_region_english)
# rm(agency_metadata_products_grouped);
rm(agency_metadata_products_ungrouped)

# First add a variable to main data frame (if relevant)


# Now aggregate by agency


# Do we want a version of this over time?



#####################
# 3. Store measures #
#####################




# ENDS