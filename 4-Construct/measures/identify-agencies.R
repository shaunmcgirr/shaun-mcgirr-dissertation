# 4-Construct\measures\identify-agencies.R

# Select the best metadata for each unique agency in the system
# Identified by NotificationP

###################
# 1. Prepare data #
###################

# Analysis
### What is an agency?

## Names and INN per agency
# agencies_notification_regnum_INN_name <- notifications_contracts_products_grouped %>%
#   filter(!is.na(ContractCustomerRegNum) & !is.na(ContractCustomerINN)) %>%
#   group_by(NotificationOrderPlacerRegNum) %>%
#   summarize(`Distinct INN (contract)` = n_distinct(ContractCustomerINN, na.rm = T),
#             `Distinct agency name (notification)` = n_distinct(NotificationOrderPlacerFullName, na.rm = T),
#             `Distinct agency name (contract)` = n_distinct(ContractCustomerFullName, na.rm = T),
#             `Distinct regnum (contract)` = n_distinct(ContractCustomerRegNum, na.rm = T)) %>%
#   mutate(`Distinct entity` = (`Distinct INN (contract)` + `Distinct regnum (contract)`)/2) %>%
#   filter(`Distinct entity` >1)

## Filtering out NAs from missing contract details drastically reduces number of "agencies"
# sapply(agencies_notification_regnum_INN_name, table)

# Good example is federal protective service (01731000048), buys for itself and for it's engineering division
# Or this municipal agency (03732000671) which buys for subordinate orgs in its area of Moscow
# Comfortable assuming that we can use the notifying customer as the stand-in for "agency"? Or try contract

# agencies_contract_regnum_INN_name <- notifications_contracts_products_grouped %>%
#   filter(!is.na(NotificationOrderPlacerRegNum) & !is.na(ContractCustomerINN)) %>%
#   group_by(ContractCustomerRegNum) %>%
#   summarize(`Distinct INN (contract)` = n_distinct(ContractCustomerINN, na.rm = T),
#             `Distinct agency name (notification)` = n_distinct(NotificationOrderPlacerFullName, na.rm = T),
#             `Distinct agency name (contract)` = n_distinct(ContractCustomerFullName, na.rm = T),
#             `Distinct regnum (notification)` = n_distinct(NotificationOrderPlacerRegNum, na.rm = T)) %>%
#   mutate(`Distinct entity` = (`Distinct INN (contract)` + `Distinct regnum (notification)`)/2)
# %>%
#   filter(`Distinct entity` >1)

## Filtering out NAs from missing contract details drastically reduces number of "agencies"
# sapply(agencies_contract_regnum_INN_name, table)

# notifications_contracts_products_grouped$NotifierIsContracterName <- ifelse(notifications_contracts_products_grouped$NotificationOrderPlacerFullName == notifications_contracts_products_grouped$ContractCustomerFullName, "Same", "Different")
# table(notifications_contracts_products_grouped$NotifierIsContracterName)
## 85% have exactly the same name Moscow, 29% in Adygeja
# notifications_contracts_products_grouped$NotifierIsContracterRegNum <- ifelse(notifications_contracts_products_grouped$NotificationOrderPlacerRegNum == notifications_contracts_products_grouped$ContractCustomerRegNum, "Same", "Different")
# table(notifications_contracts_products_grouped$NotifierIsContracterRegNum)
## 88% have exactly the same number Moscow, 

## Seem to have two choices: either rely on on RegNum or the other
## More variation if go with Contract RegNum as definitive
## Also the agency signing contract is the one paying
# notifier_is_contracter <- notifications_contracts_products_grouped %>%
#   filter(NotifierIsContracterRegNum == "Different") %>%
#   group_by(ContractCustomerRegNum) %>%
#   summarize(`Distinct INN (contract)` = n_distinct(ContractCustomerINN, na.rm = T),
#             `Distinct agency name (notification)` = n_distinct(NotificationLotCustomerRequirementCustomerFullName, na.rm = T),
#             `Distinct agency name (contract)` = n_distinct(ContractCustomerFullName, na.rm = T),
#             `Distinct regnum (notification)` = n_distinct(NotificationOrderPlacerRegNum, na.rm = T)) %>%
#   mutate(`Distinct entity` = (`Distinct INN (contract)` + `Distinct regnum (notification)`)/2)
# 
# table(notifier_is_contracter$`Distinct entity`)

# Can't I just join it all together in to one file, grouped and ungrouped? Because grouped only adds one column: level 1 product code
# everything <- left_join(notifications_contracts_products_grouped, notifications_contracts_products_ungrouped)

# columns_not_shared <- data.frame(columns = colnames(notifications_contracts_products_ungrouped)) %>%
#   anti_join(data.frame(columns = colnames(notifications_contracts_products_grouped)))
# ProductLevel1 code not shared, and the detailed data about low-level products
# Keep separate for now, consequences unknown



########################
# 2. Calculate measure #
########################

# First select a definitive agency name for each of AgencyID (which I've decided should be ContractCustomerRegNum)
# Go with the longest name, as it is most descriptive
agency_metadata_products_grouped <- notifications_contracts_products_grouped %>%
                                      transmute(AgencyID = ContractCustomerRegNum,
                                                AgencyName = ContractCustomerFullName,
                                                LengthOfAgencyName = nchar(AgencyName)) %>%
                                      filter(!is.na(AgencyID) & !is.na(AgencyName)) %>%
                                      group_by(AgencyID, AgencyName) %>%
                                        summarize(ShortestAgencyName = min(LengthOfAgencyName)) %>%
                                        arrange(-ShortestAgencyName) %>%
                                        filter(row_number() == 1) %>% ungroup() %>%
                                      select(-ShortestAgencyName)
                                        
agency_metadata_products_ungrouped <- notifications_contracts_products_ungrouped %>%
  transmute(AgencyID = ContractCustomerRegNum,
            AgencyName = ContractCustomerFullName,
            LengthOfAgencyName = nchar(AgencyName)) %>%
  filter(!is.na(AgencyID) & !is.na(AgencyName)) %>%
  group_by(AgencyID, AgencyName) %>%
    summarize(ShortestAgencyName = min(LengthOfAgencyName)) %>%
    arrange(-ShortestAgencyName) %>%
    filter(row_number() == 1) %>% ungroup() %>%
  select(-ShortestAgencyName)  

# Did we get the same for the two versions?
stopifnot(identical(agency_metadata_products_grouped, agency_metadata_products_ungrouped))

# If so, simplify
agency_metadata <- agency_metadata_products_grouped %>%
                    mutate(AgencyRegion = current_region,
                           AgencyRegionEnglish = current_region_english)
rm(agency_metadata_products_grouped); rm(agency_metadata_products_ungrouped)

# First add a variable to main data frame (if relevant)


# Now aggregate by agency


# Do we want a version of this over time?



#####################
# 3. Store measures #
#####################




# ENDS