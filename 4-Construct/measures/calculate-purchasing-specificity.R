# 4-Construct\measures\calculate-purchasing-specificity.R

###################
# 1. Prepare data #
###################


########################
# 2. Calculate measure #
########################

# First add a variable to main data frame (if relevant)


# Average amount spent on each grouped category
total_spent <- notifications_contracts_products_grouped %>%
  filter(!is.na(ContractProductCodeLevel1) & !is.na(ContractPrice)) %>%
  summarize(TotalSpent = sum(as.numeric(ContractPrice)))
total_spent <- total_spent[1,1]

total_and_average_spend_per_product_grouped <- notifications_contracts_products_grouped %>%
  filter(!is.na(ContractProductCodeLevel1) & !is.na(ContractPrice)) %>%
  group_by(ContractProductCodeLevel1) %>%
  summarize(TotalSpendPerProductGrouped = sum(as.numeric(ContractPrice)),
            MeanSpendPerProductGrouped = mean(as.numeric(ContractPrice)),
            MedianSpendPerProductGrouped = median(as.numeric(ContractPrice))) %>%
  mutate(ProportionOfTotalSpend = as.numeric(TotalSpendPerProductGrouped) / total_spent)
sum(total_and_average_spend_per_product_grouped$ProportionOfTotalSpend)

# Now aggregate by agency
agency_total_and_average_spend_per_product_grouped <- notifications_contracts_products_grouped %>%
  filter(!is.na(ContractProductCodeLevel1) & !is.na(ContractPrice)) %>%
  group_by(ContractCustomerRegNum) %>%
  mutate(AgencyTotalSpend =  sum(as.numeric(ContractPrice))) %>% ungroup() %>%
  group_by(ContractCustomerRegNum, ContractProductCodeLevel1, AgencyTotalSpend) %>%
  summarize(AgencyTotalSpendPerProductGrouped = sum(as.numeric(ContractPrice)),
            AgencyMeanSpendPerProductGrouped = mean(as.numeric(ContractPrice)),
            AgencyMedianSpendPerProductGrouped = median(as.numeric(ContractPrice))) %>%
  mutate(ProportionOfAgencySpend = AgencyTotalSpendPerProductGrouped / AgencyTotalSpend) %>%
  left_join(total_and_average_spend_per_product_grouped) %>%
  mutate(DeviationFromAverageSpend = ProportionOfTotalSpend - ProportionOfAgencySpend) %>%
  group_by(ContractCustomerRegNum) %>%
  summarize(MeanAbsoluteDeviationFromAverageSpendPerProduct = mean(abs(DeviationFromAverageSpend)),
            SumAbsoluteDeviationFromAverageSpendPerProduct = sum(abs(DeviationFromAverageSpend))) %>%
  left_join(agency_metadata, by = c("ContractCustomerRegNum" = "AgencyID"))

# Do we want a version of this over time?



#####################
# 3. Store measures #
#####################




# ENDS