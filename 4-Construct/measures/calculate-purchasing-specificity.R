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
    left_join(total_and_average_spend_per_product_grouped, by = "ContractProductCodeLevel1") %>%
    mutate(DeviationFromAverageSpend = ProportionOfTotalSpend - ProportionOfAgencySpend) %>%
  group_by(ContractCustomerRegNum) %>%
    summarize(MeanAbsoluteDeviationFromAverageSpendPerProduct = mean(abs(DeviationFromAverageSpend)),
              SumAbsoluteDeviationFromAverageSpendPerProduct = sum(abs(DeviationFromAverageSpend))) %>%
    left_join(agency_metadata, by = c("ContractCustomerRegNum" = "AgencyID")) %>%
    rename(AgencyID = ContractCustomerRegNum)

# How does it look?
hist(agency_total_and_average_spend_per_product_grouped$SumAbsoluteDeviationFromAverageSpendPerProduct, breaks = 300)
hist(agency_total_and_average_spend_per_product_grouped$MeanAbsoluteDeviationFromAverageSpendPerProduct, breaks = 300)

## What about ungrouped products?
total_spent_ungrouped <- notifications_contracts_products_ungrouped %>%
  filter(!is.na(ContractProductOKDPCode) & !is.na(ContractPrice)) %>%
  summarize(TotalSpent = sum(as.numeric(ContractPrice)))
total_spent_ungrouped <- total_spent_ungrouped[1,1]

total_and_average_spend_per_product_ungrouped <- notifications_contracts_products_ungrouped %>%
  filter(!is.na(ContractProductOKDPCode) & !is.na(ContractPrice)) %>%
  group_by(ContractProductOKDPCode) %>%
  summarize(TotalSpendPerProductUngrouped = sum(as.numeric(ContractPrice)),
            MeanSpendPerProductGrouped = mean(as.numeric(ContractPrice)),
            MedianSpendPerProductGrouped = median(as.numeric(ContractPrice))) %>%
  mutate(ProportionOfTotalSpend = as.numeric(TotalSpendPerProductUngrouped) / total_spent_ungrouped)
sum(total_and_average_spend_per_product_ungrouped$ProportionOfTotalSpend)

# Now aggregate by agency
agency_total_and_average_spend_per_product_ungrouped <- notifications_contracts_products_ungrouped %>%
  filter(!is.na(ContractProductOKDPCode) & !is.na(ContractPrice)) %>%
  group_by(ContractCustomerRegNum) %>%
    mutate(AgencyTotalSpend =  sum(as.numeric(ContractPrice))) %>% ungroup() %>%
  group_by(ContractCustomerRegNum, ContractProductOKDPCode, AgencyTotalSpend) %>%
    summarize(AgencyTotalSpendPerProductGrouped = sum(as.numeric(ContractPrice)),
              AgencyMeanSpendPerProductGrouped = mean(as.numeric(ContractPrice)),
              AgencyMedianSpendPerProductGrouped = median(as.numeric(ContractPrice))) %>%
    mutate(ProportionOfAgencySpend = AgencyTotalSpendPerProductGrouped / AgencyTotalSpend) %>%
    left_join(total_and_average_spend_per_product_ungrouped, by = "ContractProductOKDPCode") %>%
    mutate(DeviationFromAverageSpend = ProportionOfTotalSpend - ProportionOfAgencySpend) %>%
  group_by(ContractCustomerRegNum) %>%
    summarize(MeanAbsoluteDeviationFromAverageSpendPerProduct = mean(abs(DeviationFromAverageSpend)),
              SumAbsoluteDeviationFromAverageSpendPerProduct = sum(abs(DeviationFromAverageSpend))) %>%
    left_join(agency_metadata, by = c("ContractCustomerRegNum" = "AgencyID")) %>%
    rename(AgencyID = ContractCustomerRegNum)

# How does it look?
hist(agency_total_and_average_spend_per_product_ungrouped$SumAbsoluteDeviationFromAverageSpendPerProduct, breaks = 300)
hist(agency_total_and_average_spend_per_product_ungrouped$MeanAbsoluteDeviationFromAverageSpendPerProduct, breaks = 300)


# Join specificity to efficiency
efficiency_vs_specificity <- agency_total_and_average_spend_per_product_grouped %>%
  inner_join(auction_efficiency_by_agency) %>%
  mutate(TotalSpent = `Total spent`) %>% filter(MeanAbsoluteDeviationFromAverageSpendPerProduct < 0.08)
# At the agency level better to use grouped products otherwise too much noise

# How does specificity look now?
hist(efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct, breaks = 300)
hist(efficiency_vs_specificity$MeanAbsoluteDeviationFromAverageSpendPerProduct, breaks = 300)
# Much better

# Test relationship between Summed deviations and auction efficiency
plot(efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity$`Median auction efficiency`)
  lines(lowess(efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity$`Median auction efficiency`), col = "blue")
  # Only have low efficiency (making corruption more likely) when buying weird stuff
  # When buying generic stuff, have both low and high efficiency
  
plot(efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity$`Mean auction efficiency`)
  lines(lowess(efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity$`Mean auction efficiency`), col = "blue")
  
efficiency_vs_specificity_model_1 <- lm(`Median auction efficiency` ~ SumAbsoluteDeviationFromAverageSpendPerProduct, data = efficiency_vs_specificity)
  summary(efficiency_vs_specificity_model_1)
  # plot(efficiency_vs_specificity_model_1)
  
efficiency_vs_specificity_model_1_mean <- lm(`Mean auction efficiency` ~ SumAbsoluteDeviationFromAverageSpendPerProduct, data = efficiency_vs_specificity)
  summary(efficiency_vs_specificity_model_1_mean)

# Test relationship between Mean deviations and auction efficiency (don't expect to be positive, given distribution of mean deviations)
plot(efficiency_vs_specificity$MeanAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity$`Median auction efficiency`)
  lines(lowess(efficiency_vs_specificity$MeanAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity$`Median auction efficiency`), col = "blue")
  
plot(efficiency_vs_specificity$MeanAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity$`Mean auction efficiency`)
  lines(lowess(efficiency_vs_specificity$MeanAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity$`Mean auction efficiency`), col = "blue")

efficiency_vs_specificity_model_2 <- lm(`Median auction efficiency` ~ MeanAbsoluteDeviationFromAverageSpendPerProduct, data = efficiency_vs_specificity)
  summary(efficiency_vs_specificity_model_2)
  # This one is interesting when put ungrouped data through it: strongly negative (cf positive when grouped)
  
efficiency_vs_specificity_model_2_mean <- lm(`Mean auction efficiency` ~ MeanAbsoluteDeviationFromAverageSpendPerProduct, data = efficiency_vs_specificity)
  summary(efficiency_vs_specificity_model_2_mean)
  
# Add spend to model 1
plot(efficiency_vs_specificity$`Total spent`, efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct)
plot(efficiency_vs_specificity$`Total spent (log)`, efficiency_vs_specificity$MeanAbsoluteDeviationFromAverageSpendPerProduct)
plot(efficiency_vs_specificity$MeanAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity$`Median auction efficiency`)

  cor(efficiency_vs_specificity$`Total spent (log)`, efficiency_vs_specificity$MeanAbsoluteDeviationFromAverageSpendPerProduct)
  cor(efficiency_vs_specificity$`Median auction efficiency`, efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct)
  cor(efficiency_vs_specificity$`Mean auction efficiency`, efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct)
# Specificity and spending not highly correlated, can include both in model
  
efficiency_vs_specificity_model_3 <- lm(`Median auction efficiency` ~ MeanAbsoluteDeviationFromAverageSpendPerProduct + `Total spent`, data = efficiency_vs_specificity)
  summary(efficiency_vs_specificity_model_3)
  # plot(efficiency_vs_specificity_model_3)
  # When changed to MeanAbsolute with ungrouped data, strong negative assoc remains
  # Supports more weird -> less opportunity story
  
efficiency_vs_specificity_model_3_mean <- lm(`Mean auction efficiency` ~ SumAbsoluteDeviationFromAverageSpendPerProduct + `Total spent`, data = efficiency_vs_specificity)
  summary(efficiency_vs_specificity_model_3_mean)
  
efficiency_vs_specificity_model_3_int <- lm(`Median auction efficiency` ~ SumAbsoluteDeviationFromAverageSpendPerProduct + TotalSpent + (SumAbsoluteDeviationFromAverageSpendPerProduct * TotalSpent), data = efficiency_vs_specificity)
  summary(efficiency_vs_specificity_model_3_int)
  interplot(m = efficiency_vs_specificity_model_3_int, var1 = "TotalSpent", var2 = "SumAbsoluteDeviationFromAverageSpendPerProduct", hist = T)
  # With increasing specificity, effect of spending on auction efficiency decreases (opposite to theory?)
  # If not too weird agency, effect of increase in weirdness is decrease in auction efficiency
  
efficiency_vs_specificity_model_3_mean_int <- lm(`Mean auction efficiency` ~ SumAbsoluteDeviationFromAverageSpendPerProduct + TotalSpent + (SumAbsoluteDeviationFromAverageSpendPerProduct * TotalSpent), data = efficiency_vs_specificity)
  summary(efficiency_vs_specificity_model_3_mean_int)
  interplot(m = efficiency_vs_specificity_model_3_mean_int, var1 = "TotalSpent", var2 = "SumAbsoluteDeviationFromAverageSpendPerProduct", hist = T)
  
# Same thing but GLM because predicting a proportion (0-1 scaled 0-100); need quasibinomial to allow for over-dispersion
efficiency_vs_specificity$`Median auction efficiency (0-1)` <- efficiency_vs_specificity$`Median auction efficiency`/-100
plot(efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity$`Median auction efficiency (0-1)`)
  lines(lowess(efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity$`Median auction efficiency (0-1)`), col = "blue")

efficiency_vs_specificity$`Mean auction efficiency (0-1)` <- efficiency_vs_specificity$`Mean auction efficiency`/-100
  plot(efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity$`Mean auction efficiency (0-1)`)
  lines(lowess(efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity$`Mean auction efficiency (0-1)`), col = "blue")
  
    
efficiency_vs_specificity_model_4 <- glm(`Median auction efficiency (0-1)` ~ SumAbsoluteDeviationFromAverageSpendPerProduct, data = efficiency_vs_specificity, family = "quasibinomial")
  summary(efficiency_vs_specificity_model_4)
  # MeanAbs with ungrouped data also supports more weird -> less opp
  
efficiency_vs_specificity_model_5 <- glm(`Median auction efficiency (0-1)` ~ SumAbsoluteDeviationFromAverageSpendPerProduct + `Total spent`, data = efficiency_vs_specificity, family = "quasibinomial")
  summary(efficiency_vs_specificity_model_5)
# Coefficients now negative (reversed during scaling) but almost significant
  
efficiency_vs_specificity_model_5_int <- glm(`Median auction efficiency (0-1)` ~ SumAbsoluteDeviationFromAverageSpendPerProduct + TotalSpent + (SumAbsoluteDeviationFromAverageSpendPerProduct * TotalSpent), data = efficiency_vs_specificity, family = "quasibinomial")
  summary(efficiency_vs_specificity_model_5_int) 
  interplot(m = efficiency_vs_specificity_model_5_int, var1 = "TotalSpent", var2 = "SumAbsoluteDeviationFromAverageSpendPerProduct", hist = T)
  # With increasing specificity, (negative) effect of spending on auction efficiency decreases
  # Where bulk of distribution is (medium-weird agencies), effect of specificity on (reversed) efficiency is negative, not as predicted if efficiency = corruption
  # For very weird agencies, effect indistinguishable from zero regardless of spending
  
efficiency_vs_specificity_model_5_mean_int <- glm(`Mean auction efficiency (0-1)` ~ SumAbsoluteDeviationFromAverageSpendPerProduct + TotalSpent + (SumAbsoluteDeviationFromAverageSpendPerProduct * TotalSpent), data = efficiency_vs_specificity, family = "quasibinomial")
  summary(efficiency_vs_specificity_model_5_mean_int) 
  interplot(m = efficiency_vs_specificity_model_5_mean_int, var1 = "TotalSpent", var2 = "SumAbsoluteDeviationFromAverageSpendPerProduct", hist = T)
  # Very good fit here based on Q-Q plot

# But binomial assumes we know the number of trials, which we don't here percentage is a measure of something else
# http://support.sas.com/kb/57/480.html "While the binomial distribution is generally applicable to a dependent (response) variable that represents proportions from aggregated binary responses, it might not apply to underlying continuous data."
# http://support.sas.com/kb/56/992.html "However, if the values are proportions of area covered or affected by some agent, or are proportions of a mixture, then they do not represent a set of trials and might not have a binomial distribution."
# If going with Median, need fractional response model: http://www.stata.com/new-in-stata/fractional-outcome-models/
hist(efficiency_vs_specificity$`Median auction efficiency`, breaks = 40)
hist(efficiency_vs_specificity$`Median auction efficiency (0-1)`, breaks = 40)

# Mean efficiency is normally distributed, and probably a better measure of average opportunity, vs opp in average auction
hist(efficiency_vs_specificity$`Mean auction efficiency`, breaks = 40)
hist(efficiency_vs_specificity$`Mean auction efficiency (0-1)`, breaks = 40)

# What about just treating it as a normally-distributed number? http://www.r-bloggers.com/not-all-proportion-data-are-binomial-outcomes/
efficiency_vs_specificity_no_zeroes <- efficiency_vs_specificity %>%
  filter(`Median auction efficiency (0-1)` != 0)
  
efficiency_vs_specificity_model_6 <- lm(log10(`Median auction efficiency (0-1)`) ~ SumAbsoluteDeviationFromAverageSpendPerProduct + `Total spent`, data = efficiency_vs_specificity_no_zeroes)
  summary(efficiency_vs_specificity_model_6)
  
efficiency_vs_specificity_model_7 <- glm(log10(`Median auction efficiency (0-1)`) ~ SumAbsoluteDeviationFromAverageSpendPerProduct + `Total spent`, data = efficiency_vs_specificity_no_zeroes)
  summary(efficiency_vs_specificity_model_7)  
  
# What about beta regression? 
# http://stats.stackexchange.com/questions/77376/generalized-linear-models-with-continuous-proportions
# https://cran.r-project.org/web/packages/betareg/vignettes/betareg.pdf
efficiency_vs_specificity_model_8 <- betareg(`Median auction efficiency (0-1)` ~ SumAbsoluteDeviationFromAverageSpendPerProduct + `Total spent`, data = efficiency_vs_specificity_no_zeroes)
  summary(efficiency_vs_specificity_model_8)
  # plot(efficiency_vs_specificity_model_8) # Shows some very high-leverage points
  
efficiency_vs_specificity_model_8_int <- betareg(`Median auction efficiency (0-1)` ~ SumAbsoluteDeviationFromAverageSpendPerProduct + TotalSpent + (SumAbsoluteDeviationFromAverageSpendPerProduct * TotalSpent), data = efficiency_vs_specificity_no_zeroes)
  summary(efficiency_vs_specificity_model_8_int)
  # interplot(m = efficiency_vs_specificity_model_8_int, var1 = "TotalSpent", var2 = "SumAbsoluteDeviationFromAverageSpendPerProduct", hist = T)
  # https://cran.r-project.org/web/packages/mfx/mfx.pdf
  
# How about adding in petrol to the model as DV?
efficiency_vs_specificity_vs_petrol <- efficiency_vs_specificity_no_zeroes %>%
  left_join(petrol_prices_by_agency) %>%
  filter(!is.na(`Average price per liter`))

efficiency_vs_specificity_model_9 <- lm(`Average price per liter` ~ SumAbsoluteDeviationFromAverageSpendPerProduct + TotalSpent + `Median auction efficiency` + (SumAbsoluteDeviationFromAverageSpendPerProduct * TotalSpent), data = efficiency_vs_specificity_vs_petrol)
  summary(efficiency_vs_specificity_model_9)
  
  
# Compare procedure choice to efficiency  
efficiency_vs_procedure <- procedure_choice_by_agency %>%
  filter(`Procedure type` == "Higher discretion") %>%
  transmute(AgencyID = Agency,
         ProportionHigherDiscretion = Proportion) %>%
  inner_join(efficiency_vs_specificity)
  
plot(efficiency_vs_procedure$`Median auction efficiency`, efficiency_vs_procedure$ProportionHigherDiscretion)  
# If your average auction is low-efficiency, more likely to be using fewer 'request for quotes' procedures
plot(efficiency_vs_procedure$ProportionHigherDiscretion, efficiency_vs_procedure$`Median auction efficiency`)  
  lines(lowess(efficiency_vs_procedure$ProportionHigherDiscretion, efficiency_vs_procedure$`Median auction efficiency`), col = "blue")
efficiency_vs_procedure_model_1 <- (lm(`Median auction efficiency` ~ ProportionHigherDiscretion + SumAbsoluteDeviationFromAverageSpendPerProduct + TotalSpent, data = efficiency_vs_procedure))
  summary(efficiency_vs_procedure_model_1)
# Shows that increasing use of discretionary procedures associated with lower corruption opportunity
# Controlling for this, increasing weirdness associated with more corruption opportunity
# Greater use of more discretionary procedures associated with lower corruption opportunities (on average w/in agencies)
# Unless I have discretion wrong, and electronic auction is more discretionary?  
  
# What about weirdness vs single-supplier
single_supplier_by_agency <- notifications_contracts_products_grouped %>%
  filter(Match == "Contract without notification") %>%
  group_by(ContractCustomerRegNum) %>%
    summarize(NumberOfSingleSupplierPurchases = n()) %>%
    rename(AgencyID = ContractCustomerRegNum)

specificity_vs_single_supplier <- agency_total_and_average_spend_per_product_grouped %>%
  inner_join(single_supplier_by_agency)

plot(specificity_vs_single_supplier$SumAbsoluteDeviationFromAverageSpendPerProduct, specificity_vs_single_supplier$NumberOfSingleSupplierPurchases)
# Those buying very very weird things don't use most discretionary procedure

# Revision to notifications and contracts
revisions_by_agency <- notifications_contracts_products_grouped %>%
  filter(Match == "Notification matches contract") %>%
  transmute(AgencyID = ContractCustomerRegNum,
            NotificationRevised,
            ContractRevised,
            AnyRevision = ifelse((NotificationRevised == "Y" | ContractRevised == "Y"), 1, 0)) %>%
  group_by(AgencyID) %>%
    summarize(ProportionOfPurchasesModified = mean(AnyRevision))

efficiency_vs_specificity_vs_revisions <- efficiency_vs_specificity %>%
  inner_join(revisions_by_agency) %>%
  mutate(MeanAuctionEfficiency = `Mean auction efficiency`,
         MedianAuctionEfficiency = `Median auction efficiency`,
         PurchaseSpecificity = MeanAbsoluteDeviationFromAverageSpendPerProduct/max(MeanAbsoluteDeviationFromAverageSpendPerProduct),
         CorruptionOpportunities = 1-(MedianAuctionEfficiency/min(MedianAuctionEfficiency))) %>%
  filter(!is.na(ProportionOfPurchasesModified))

plot(efficiency_vs_specificity_vs_revisions$SumAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity_vs_revisions$ProportionOfPurchasesModified)

plot(efficiency_vs_specificity_vs_revisions$MeanAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity_vs_revisions$ProportionOfPurchasesModified)

plot(efficiency_vs_specificity_vs_revisions$MeanAuctionEfficiency, efficiency_vs_specificity_vs_revisions$ProportionOfPurchasesModified)

efficiency_vs_specificity_vs_revisions_model_1 <- lm(ProportionOfPurchasesModified ~ MeanAbsoluteDeviationFromAverageSpendPerProduct + MeanAuctionEfficiency, data = efficiency_vs_specificity_vs_revisions)
  summary(efficiency_vs_specificity_vs_revisions_model_1)  

cor(efficiency_vs_specificity_vs_revisions$MeanAbsoluteDeviationFromAverageSpendPerProduct,
    efficiency_vs_specificity_vs_revisions$MedianAuctionEfficiency)  
# IVs basically orthogonal

efficiency_vs_specificity_vs_revisions_model_2 <- lm(ProportionOfPurchasesModified ~ `Total spent` + MeanAbsoluteDeviationFromAverageSpendPerProduct + MedianAuctionEfficiency + (MeanAbsoluteDeviationFromAverageSpendPerProduct * MedianAuctionEfficiency), data = efficiency_vs_specificity_vs_revisions)
  summary(efficiency_vs_specificity_vs_revisions_model_2)  
  interplot(m = efficiency_vs_specificity_vs_revisions_model_2, var1 = "MedianAuctionEfficiency", var2 = "MeanAbsoluteDeviationFromAverageSpendPerProduct", hist = T) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    xlab("\nPurchase specificity (higher value indicates agency\ndeviates more from average agency's purchasing pattern)") +
    ylab("Estimated coefficient for corruption opportunities\n")
  # For very weird agencies, effect of corruption opportunities on this corruption "red flag" is negative
  # Slightly weaker for Mean auction efficiency measure cf Median auction efficiency
  # Distributions of variables look ok
  # Story more marginal for ungrouped products (higher resolution) than grouped products
  plot(efficiency_vs_specificity_vs_revisions$MeanAbsoluteDeviationFromAverageSpendPerProduct, efficiency_vs_specificity_vs_revisions$MedianAuctionEfficiency)
  # As per line 100 above, only have very low corruption opportunities when buying non-weird stuff
  # When buying weird stuff, corruption opportunities always higher
  # Opportunities + Specificity explain Corruption?
  
# Double-check all that with more intuitively-scaled variables
efficiency_vs_specificity_vs_revisions_model_3 <- lm(ProportionOfPurchasesModified ~ `Total spent` + PurchaseSpecificity + CorruptionOpportunities + (PurchaseSpecificity * CorruptionOpportunities), data = efficiency_vs_specificity_vs_revisions)
  summary(efficiency_vs_specificity_vs_revisions_model_3)  
  interplot(m = efficiency_vs_specificity_vs_revisions_model_3, var1 = "CorruptionOpportunities", var2 = "PurchaseSpecificity", hist = T) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    xlab("\nPurchase specificity (higher value indicates agency\ndeviates more from average agency's purchasing pattern)") +
    ylab("Estimated coefficient for corruption opportunities\n")
  
  # For agencies purchasing very specific goods, increase in corruption opportunities is associated with fewer revised purchases
  # Agencies purchasing generically, effect of increase in corruption opportunities no different than zero
  
  # Check main variables
  hist(efficiency_vs_specificity_vs_revisions$CorruptionOpportunities) # Higher closer to zero auction efficiency - correct
  hist(efficiency_vs_specificity_vs_revisions$PurchaseSpecificity, breaks = 40)  # Looks right
  hist(efficiency_vs_specificity_vs_revisions$ProportionOfPurchasesModified)
  

  
# Big old model - with notification revisions as predicted?
plot(density(notifications_contracts_products_grouped$PriceChangePercentageNoOutliers[notifications_contracts_products_grouped$TenderProcedureGroup == "Open electronic auction"], na.rm = T))
plot(density(notifications_contracts_products_grouped$PriceChangePercentageNoOutliers[notifications_contracts_products_grouped$TenderProcedureGroup == "Open tender"], na.rm = T))
plot(density(notifications_contracts_products_grouped$PriceChangePercentageNoOutliers[notifications_contracts_products_grouped$TenderProcedureGroup == "Request for quotes"], na.rm = T))

# Seems peakyness right on 0 is highest for electronic, then tender, then request
# Can I make argument that auction has highest discretion because of control over who turns up?
# Whereas request for quotes allows (through sealed bid) anybody to waltz in at last moment?

purchases_model_1 <- lm(PriceChangePercentageNoOutliers ~ TenderProcedureGroup, data = notifications_contracts_products_grouped[notifications_contracts_products_grouped$TenderProcedureDiscretion != "Other",])
  summary(purchases_model_1)
  
# Do we want a version of this over time?



#####################
# 3. Store measures #
#####################




# ENDS