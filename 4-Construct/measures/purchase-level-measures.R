# 4-Construct\measures\purchase-level-measures.R

###################
# 1. Prepare data #
###################


########################
# 2. Calculate measure #
########################

# Bunching near thresholds
test <- notifications_contracts_products_ungrouped %>%
          filter(!is.na(NotificationLotCustomerRequirementMaxPrice)) %>%
          rename(AgencyID = ContractCustomerRegNum) %>%
          mutate(BunchedAtThreshold = ifelse(TenderProcedureGroup == "Request for quotes" & NotificationLotCustomerRequirementMaxPrice > (0.95 * 500000) & NotificationLotCustomerRequirementMaxPrice < (1.05 * 500000), "Bunched at request for quotes threshold",
                                             ifelse(TenderProcedureGroup == "Open electronic auction" & NotificationLotCustomerRequirementMaxPrice > (0.95 * 3000000) & NotificationLotCustomerRequirementMaxPrice < (1.05 * 3000000), "Bunched at open electronic auction threshold", "Not bunched")),
                 # AnyBunching = as.factor(ifelse(BunchedAtThreshold == "Not bunched", "NotBunched", "Bunched")),
                 AnyBunching = as.factor(ifelse(BunchedAtThreshold == "Not bunched", 0, 1)),
                 SurplusAvailableForCorruption = ifelse(PriceChangePercentageNoOutliers > 0, NA, 100+PriceChangePercentageNoOutliers),
                 SubstantialRevisions = as.factor(ifelse(ContractRevised == "N" | NotificationRevised == "N", 0, 1)),
                 Year = as.factor(substr(NotificationPublishDate, 1, 4)),
                 CorruptionLikelihood = ifelse(TenderProcedureGroup == "Request for quotes" & NotificationLotCustomerRequirementMaxPrice <= 505000, 1*(1-abs((NotificationLotCustomerRequirementMaxPrice - 500000)/500000)),
                                             ifelse(TenderProcedureGroup == "Open electronic auction" & NotificationLotCustomerRequirementMaxPrice <= 3030000, 1*(1-abs((NotificationLotCustomerRequirementMaxPrice - 3000000)/3000000)), NA)),
                 PriceChangePercentageNegativeOnly = ifelse(PriceChangePercentageNoOutliers <= 0, PriceChangePercentageNoOutliers, NA),
                 PriceChangePercentageLessThanTen = ifelse(PriceChangePercentageNoOutliers <= 10, PriceChangePercentageNoOutliers, NA))
  

# Quick models
# test_model <- glm(PriceChange ~ NotificationLotCustomerRequirementMaxPrice + AnyBunching + TenderProcedureDiscretion + NotificationRevised + ProcedureDuration + factor(AgencyID) - 1, data = test)
# test_model <- glm(PriceChange ~ NotificationLotCustomerRequirementMaxPrice + AnyBunching + TenderProcedureDiscretion + NotificationRevised + ProcedureDuration, data = test)
# summary(test_model)

# test_model_1 <- glm(PriceChangePercentage ~ AnyBunching, data = test)
# summary(test_model_1)


# What do we expect at individual purchase level (ignore agencies for now)
# Incr purchase specificity ->  Less negative price change (perhaps rescale to positive "corruption opportunities")
# "As expected, if a purchase is more unusual there is less price competition"
# This relationship is stronger when a red flag is present (when it isn't, corruption has failed)

# Specificity at the individual purchase level means:
# - how likely is it that a purchase is for this product
# - how likely is it that a purchase is for this amount
# - both combined

# Vector of level 1 product probabilities
product_probabilities_level_1 <- notifications_contracts_products_ungrouped %>%
                                  filter(!is.na(NotificationLotProductCodeLevel1)) %>%
                                  group_by(NotificationLotProductCodeLevel1) %>%
                                    count(NotificationLotProductCodeLevel1) %>%
                                    mutate(ProductProbabilityLevel1 = prop.table(n)) %>% ungroup() %>%
                                  select(-n)
hist(product_probabilities_level_1$ProductProbabilityLevel1, breaks = 30)
# Most common is construction/repair, least is chemicals, makes sense

product_probabilities_level_4 <- notifications_contracts_products_ungrouped %>%
                                  filter(!is.na(NotificationLotProductCode)) %>%
                                  group_by(NotificationLotProductCode) %>%
                                    count(NotificationLotProductCode) %>%
                                    mutate(ProductProbabilityLevel4 = prop.table(n)) %>% ungroup() %>%
                                  select(-n)
hist(log10(product_probabilities_level_4$ProductProbabilityLevel4), breaks = 30)
# Most common is landscaping, then building, also makes sense

# Agency spending as proxy for capacity
agency_spending <- notifications_contracts_products_ungrouped %>%
                    filter(!is.na(ContractPrice)) %>%
                    rename(AgencyID = ContractCustomerRegNum) %>%
                    group_by(AgencyID) %>%
                      summarize(TotalAgencySpending = sum(ContractPrice))

# Merge these on to test data
test <- test %>%
          left_join(product_probabilities_level_1) %>%
          left_join(product_probabilities_level_4) %>%
          left_join(agency_spending)

# First, all price changes
test_model_2 <- lm(PriceChangePercentageNegativeOnly ~ ProductProbabilityLevel1, data = test)
summary(test_model_2)
# Average purchase price decreases ~13%; more generic good, less price change, so we should already be suspcious

test_model_3 <- lm(PriceChangePercentageNegativeOnly ~ NotificationLotCustomerRequirementMaxPrice + ProductProbabilityLevel1 + TenderProcedureGroup, data = test)
summary(test_model_3)
# Holds controlling for amount spent and procedure type
# Interestingly, auction-> tender decreases auction efficiency; auction-> quotes increases auction efficiency

test_model_4 <- lm(PriceChangePercentageNegativeOnly ~ NotificationLotCustomerRequirementMaxPrice + ProductProbabilityLevel4 + TenderProcedureGroup, data = test)
summary(test_model_4)
# Holds too at more detailed level

# Are auctions with red flags different? Yes
test_model_5 <- lm(PriceChangePercentageNegativeOnly ~ NotificationLotCustomerRequirementMaxPrice + (ProductProbabilityLevel1 * AnyBunching), data = test)
summary(test_model_5)
interplot(test_model_5, var1 = "ProductProbabilityLevel1", var2 = "AnyBunching")
# In auctions with bunching, price does not decrease as much

test_model_6 <- lm(PriceChangePercentageNegativeOnly ~ NotificationLotCustomerRequirementMaxPrice + (ProductProbabilityLevel4 * AnyBunching), data = test)
summary(test_model_6)
interplot(test_model_6, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching")
# At greater level of detail, purchases without bunching obey market mech, those with bunching do not

# test_model_7 <- lm(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel1, data = test)
# summary(test_model_7)
# 
# test_model_8 <- lm(SurplusAvailableForCorruption ~ NotificationLotCustomerRequirementMaxPrice + ProductProbabilityLevel1, data = test)
# summary(test_model_8)
# 
# test_model_9 <- glm(AnyBunching ~ SurplusAvailableForCorruption + ProductProbabilityLevel1, data = test, family = binomial())
# summary(test_model_9)
# 
# test_model_10 <- glm(AnyBunching ~ SurplusAvailableForCorruption + ProductProbabilityLevel1 + (SurplusAvailableForCorruption * ProductProbabilityLevel1), data = test, family = binomial())
# summary(test_model_10)
# interplot(test_model_10, var1 = "ProductProbabilityLevel1", var2 = "SurplusAvailableForCorruption", hist = T)
# When surplus available for corruption is low, increase in commonness of product increases prob of bunching
# When surplus available for corruption is high, this effect weakens
# interplot(test_model_10, var2 = "ProductProbabilityLevel1", var1 = "SurplusAvailableForCorruption", hist = T)
# When a purchase is very unusual, increase in surplus available for corruption associated with increased prob of bunching
# When a purchase is very generic, increase in surplus available associated with decreased prob of bunching
# THIS IS GIBBERISH: NEED TO BE PREDICTING PRICE CHANGES AS THAT'S HOW EXTRACTION HAPPENS!

# test_model_11 <- lm(SurplusAvailableForCorruption ~ ProductProbabilityLevel4, data = test)
# summary(test_model_11)
# The more generic the good, the less the price changes (level 1; opposite when level 4)

# Now excluding outliers (but including auctions where price increased)
test_model_12 <- lm(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel1, data = test)
summary(test_model_12)
# Intercept is -13 so when compeletely weird product, expect 13% drop in price between notification and contract
# As product becomes more generic, expect a lower drop at level 1

test_model_13 <- lm(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel4, data = test)
summary(test_model_13)
# Intercept is -13 so when compeletely weird product, expect 13% drop in price between notification and contract
# As product becomes more generic, expect a *greater* drop at level 4

test_model_14 <- lm(PriceChangePercentageNoOutliers ~ (ProductProbabilityLevel4 * AnyBunching), data = test)
summary(test_model_14)
interplot(test_model_14, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching")
# When there is no `bunching' red flag, then the more generic a good is, the more the price decreases over the auction, making corruption less likely
# When there is this red flag, the effect is weaker but estimate also crosses zero, so corruption kills the normal relationship

# Double-check the interaction worked as expected
test_bunched <- test %>% filter(BunchedAtThreshold != "Not bunched")
summary(lm(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel4, data = test_bunched))

test_not_bunched <- test %>% filter(BunchedAtThreshold == "Not bunched")
summary(lm(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel4, data = test_not_bunched))

# Create scaled version of product probability
ProductProbabilityLevel4Scaled_max <- max((test$ProductProbabilityLevel4), na.rm = T)
test$ProductProbabilityLevel4Scaled <- (test$ProductProbabilityLevel4)/ProductProbabilityLevel4Scaled_max
hist(test$ProductProbabilityLevel4Scaled, breaks = 30)
test$PurchaseSpecificity <- -1*test$ProductProbabilityLevel4Scaled

# Continuous version
test_model_14_cont <- lm(PriceChangePercentageNegativeOnly ~ NotificationLotCustomerRequirementMaxPrice + ProductProbabilityLevel4Scaled + CorruptionLikelihood + (ProductProbabilityLevel4Scaled * CorruptionLikelihood), data = test)
summary(test_model_14_cont)
interplot(test_model_14_cont, var1 = "ProductProbabilityLevel4Scaled", var2 = "CorruptionLikelihood", hist = T)
# Unconditional effect of buying more general good (ie when far away from threshold) is as the market-based explanation would have us believe: price decreases
# Unconditional effect of increasing corruption likelihood (ie when buying very rare good) is negative; when buying something very unique, trying to hit the threshold doesn't work
# The closer to the corruption threshold, the greater the average price decrease from buying more generic
# Almost consistent with my story: market operates in default case, buying more generic only sustainable when you're not trying to game at the threshold (where probability of oversight is higher)

# Same thing in reverse
interplot(test_model_14_cont, var1 = "CorruptionLikelihood", var2 = "ProductProbabilityLevel4Scaled")
# When buying specific goods, getting closer to the threshold associated with smaller price decrease
# When buying more generic goods, getting closer to threshold associated with larger price decrease
# Market story, generic goods corruption doesn't matter

# Explaining corruption (DV and IV derived from price correlated -0.13)
test_model_14_corr <- lm(CorruptionLikelihood ~ NotificationLotCustomerRequirementMaxPrice + ProductProbabilityLevel4Scaled + PriceChangePercentageNegativeOnly + (ProductProbabilityLevel4Scaled * PriceChangePercentageNegativeOnly), data = test)
summary(test_model_14_corr)
interplot(test_model_14_corr, var1 = "ProductProbabilityLevel4Scaled", var2 = "PriceChangePercentageNegativeOnly")
# When price drops a lot, buying more generic associated with less use of corruption (market working)
# When price drops not much, association is weaker then zero (market not working so much when generic goods+)
# Necessary conditions for corruption are price increases and buying more generic

# In reverse
interplot(test_model_14_corr, var1 = "PriceChangePercentageNoOutliers", var2 = "ProductProbabilityLevel4Scaled")
# When buying specific, less price change (ie -10 to -9) associated with corruption less likely (failure of collusion)
# When buying generic, less price change (increasing opportunity) less associated with corruption less likely (non-failure)

# Binary outcome version
test_model_14_corr_bin <- glm(AnyBunching ~ ProductProbabilityLevel4Scaled + PriceChangePercentageNoOutliers + (ProductProbabilityLevel4Scaled * PriceChangePercentageNoOutliers), data = test, family = "binomial")
summary(test_model_14_corr_bin)
interplot(test_model_14_corr_bin, var1 = "ProductProbabilityLevel4Scaled", var2 = "PriceChangePercentageNoOutliers")
# Same as for proximity measure

# Same models fixed effects (large agencies only)
test_model_14_cont_fe <- glm(PriceChangePercentageNoOutliers ~ NotificationLotCustomerRequirementMaxPrice + ProductProbabilityLevel4Scaled + CorruptionLikelihood + (ProductProbabilityLevel4Scaled * CorruptionLikelihood) + factor(AgencyID) - 1, data = test_large_agencies)
summary(test_model_14_cont_fe)
interplot(test_model_14_cont_fe, var1 = "ProductProbabilityLevel4Scaled", var2 = "CorruptionLikelihood", sims = 100)
# cf  interplot(test_model_14_cont, var1 = "ProductProbabilityLevel4Scaled", var2 = "CorruptionLikelihood")
# Unconditional effect of buying more general good (ie when far away from threshold) is to have less efficient auction
# Slopes same, but opposite side of 0. Right on corruption threshold, no effect (not a good story)
interplot(test_model_14_cont_fe, var1 = "CorruptionLikelihood", var2 = "ProductProbabilityLevel4Scaled", sims = 10)
# cf  interplot(test_model_14_cont, var1 = "CorruptionLikelihood", var2 = "ProductProbabilityLevel4Scaled")
# Slopes are the same, same side of 0; Market story, generic goods corruption doesn't matter

test_model_14_corr_fe <- glm(CorruptionLikelihood ~ NotificationLotCustomerRequirementMaxPrice + ProductProbabilityLevel4Scaled + PriceChangePercentageNegativeOnly + (ProductProbabilityLevel4Scaled * PriceChangePercentageNegativeOnly) + factor(AgencyID) - 1, data = test_large_agencies)
summary(test_model_14_corr_fe)
interplot(test_model_14_corr_fe, var1 = "ProductProbabilityLevel4Scaled", var2 = "PriceChangePercentageNegativeOnly", sims = 10)
# cf  interplot(test_model_14_corr, var1 = "ProductProbabilityLevel4Scaled", var2 = "PriceChangePercentageNoOutliers")
# Slopes are the same, story is good. Corruption, efficient auctions, and generic goods don't go together; corruption, dodgy auctions, and generic goods do
interplot(test_model_14_corr_fe, var1 = "PriceChangePercentageNoOutliers", var2 = "ProductProbabilityLevel4Scaled", sims = 10)
# cf  interplot(test_model_14_corr, var1 = "PriceChangePercentageNoOutliers", var2 = "ProductProbabilityLevel4Scaled")
# Slopes are the same, story is good. For specific goods, dodgier auction not correlated with corruption; for generic goods, closer to a correlation

# Are greater corruption opportunities correlated with more specific goods?
test_opps_vs_spec <- lm(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel4Scaled + NotificationLotCustomerRequirementMaxPrice + (ProductProbabilityLevel4Scaled * NotificationLotCustomerRequirementMaxPrice), data = test)
summary(test_opps_vs_spec) # Yes, in average purchase, auction efficiency 13%, more generic goods -> more efficiency
# Also, more expensive, less efficiency
interplot(test_opps_vs_spec, var1 = "ProductProbabilityLevel4Scaled", "NotificationLotCustomerRequirementMaxPrice")
# The more expensive a purchase

# What about with price of purchase? What about with both?

# Robustness: are there really more suppliers for more generic goods?
suppliers_per_product <- notifications_contracts_products_ungrouped %>%
                          filter(!is.na(NotificationLotProductCode) & !is.na(ContractSupplierParticipantINN)) %>%
                          group_by(NotificationLotProductCode) %>%
                            summarize(NumberOfSuppliers = n_distinct(ContractSupplierParticipantINN)) %>% ungroup() %>%
                          left_join(product_probabilities_level_4)
plot(log10(suppliers_per_product$ProductProbabilityLevel4), suppliers_per_product$NumberOfSuppliers)
  lines(lowess(log10(suppliers_per_product$ProductProbabilityLevel4), suppliers_per_product$NumberOfSuppliers, f = 1/100), col = "blue")


# Specify the per-auction model more fully: include starting price and agency spending
test_model_15 <- lm(PriceChangePercentageNoOutliers ~ NotificationLotCustomerRequirementMaxPrice + log10(TotalAgencySpending) + (ProductProbabilityLevel4 * AnyBunching), data = test)
summary(test_model_15)
interplot(test_model_15, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching")
  
# Reduce to agencies with >99 purchases
large_agencies_only <- test %>%
                          group_by(AgencyID) %>%
                          summarize(NumberOfAgencyPurchases = n()) %>% ungroup() %>%
                          filter(!is.na(AgencyID)) %>%
                          filter(NumberOfAgencyPurchases > 99)
test_large_agencies <- test %>%
                        inner_join(large_agencies_only) %>%
                        filter(!is.na(ProductProbabilityLevel4))

test_model_16 <- lm(PriceChangePercentageNoOutliers ~ NotificationLotCustomerRequirementMaxPrice + log10(TotalAgencySpending) + (ProductProbabilityLevel4 * AnyBunching), data = test_large_agencies)
summary(test_model_16)
interplot(test_model_16, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching")
# Even stronger pattern

# Control for procedure type too
test_model_17 <- lm(PriceChangePercentageNoOutliers ~ NotificationLotCustomerRequirementMaxPrice + log10(TotalAgencySpending) + TenderProcedureGroup + (ProductProbabilityLevel4 * AnyBunching), data = test_large_agencies)
summary(test_model_17)
interplot(test_model_17, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching")
# Still holds

# output a tidy graph
bunching_vs_opportunities_graph <- interplot(test_model_17, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching") +
  labs(title = "Buying more generic goods reduces corruption opportunities,\nbut only in auctions without `bunching'\n",
       x = "\n0 denotes purchases without red flag; 1 denotes purchases with red flag",
       y = "Effect of increased commonality of purchase (more generic good)\non change in price over auction\n")
print(bunching_vs_opportunities_graph)
ggsave(plot = bunching_vs_opportunities_graph, filename = "bunching_vs_opportunities_graph.pdf", device = "pdf", limitsize = T) #, width = 8, height = 8)


# Test another red flag: substantial revisions to the purchase from notification through contract
test_model_18 <- lm(PriceChangePercentageNoOutliers ~ NotificationLotCustomerRequirementMaxPrice + log10(TotalAgencySpending) + (ProductProbabilityLevel4 * SubstantialRevisions), data = test_large_agencies)
summary(test_model_18)
interplot(test_model_18, var1 = "ProductProbabilityLevel4", var2 = "SubstantialRevisions")
# Same pattern

# Try this out within the most-purchasing agency (01731000045, Ministry of Defense)
agencies_by_spending <- agency_metadata %>% left_join(large_agencies_only)
test_mindef <- test %>% filter(AgencyID == "01731000045")

# Test within agency (need to drop agency spending as doesn't vary)
test_model_19 <- lm(PriceChangePercentageNoOutliers ~ (ProductProbabilityLevel4 * AnyBunching), data = test_mindef)
summary(test_model_19)
interplot(test_model_19, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching")

test_model_20 <- lm(PriceChangePercentageNoOutliers ~ (ProductProbabilityLevel4 * SubstantialRevisions), data = test_mindef)
summary(test_model_20)
interplot(test_model_20, var1 = "ProductProbabilityLevel4", var2 = "SubstantialRevisions")

# Second most-purchasing agency (01731000077, Ministry of Culture)
test_culture <- test %>% filter(AgencyID == "01731000077")

# Test within agency (need to drop agency spending as doesn't vary)
test_model_21 <- lm(PriceChangePercentageNoOutliers ~ (ProductProbabilityLevel4 * AnyBunching), data = test_culture)
summary(test_model_21)
interplot(test_model_21, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching")

test_model_22 <- lm(PriceChangePercentageNoOutliers ~ (ProductProbabilityLevel4 * SubstantialRevisions), data = test_culture)
summary(test_model_22)
interplot(test_model_22, var1 = "ProductProbabilityLevel4", var2 = "SubstantialRevisions")

# Expertise-driven agency (01731000083, Ministry of Energy)
test_energy <- test %>% filter(AgencyID == "01731000083")

# Test within agency (need to drop agency spending as doesn't vary)
test_model_23 <- lm(PriceChangePercentageNoOutliers ~ (ProductProbabilityLevel4 * AnyBunching), data = test_energy)
summary(test_model_23)
interplot(test_model_23, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching")

test_model_24 <- lm(PriceChangePercentageNoOutliers ~ (ProductProbabilityLevel4 * SubstantialRevisions), data = test_energy)
summary(test_model_24)
interplot(test_model_24, var1 = "ProductProbabilityLevel4", var2 = "SubstantialRevisions")


# Hierarchical: agency is the random effect as assume different "baseline" PriceChange for each agency
test_model_25_null <- lmer(PriceChangePercentageNoOutliers ~ 1 + (1 | AgencyID), data = test_large_agencies, REML = F)
summary(test_model_25_null) # Shows ~ 10% variance at the Agency level

test_model_25_min <- lmer(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel4 + (1 | AgencyID), data = test_large_agencies, REML = F)
summary(test_model_25_min)

test_model_25_med <- lmer(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel4 + NotificationLotCustomerRequirementMaxPrice + (1 | AgencyID), data = test_large_agencies, REML = F)
summary(test_model_25_med)
# These three have same random effects structure
anova(test_model_25_null, test_model_25_min, test_model_25_med)

test_model_25_alt <- lmer(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel4 + (1+AnyBunching | AgencyID), data = test_large_agencies, REML = F)
summary(test_model_25_alt)

test_model_25_full <- lmer(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel4 + NotificationLotCustomerRequirementMaxPrice + (1+AnyBunching | AgencyID), data = test_large_agencies, REML = F)
summary(test_model_25_full)
# These two can be compared
anova(test_model_25_alt, test_model_25_full)

# This represents another way of thinking about the problem: as nested levels of variance


# GLM with agency-level fixed effects on separate samples (i.e. -1 to remove overall intercept and generate one per agency)
# test_large_agencies_bunched <- test_large_agencies %>% filter(BunchedAtThreshold != "Not bunched")
# summary(lm(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel4, data = test_large_agencies_bunched))
# 
# test_large_agencies_not_bunched <- test_large_agencies %>% filter(BunchedAtThreshold == "Not bunched")
# summary(lm(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel4, data = test_large_agencies_not_bunched))
# 
# test_model_26_bunched <- glm(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel4 + NotificationLotCustomerRequirementMaxPrice + factor(AgencyID) - 1, data = test_large_agencies_bunched)
# summary(test_model_26_bunched)

# test_model_26_not_bunched <- glm(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel4 + NotificationLotCustomerRequirementMaxPrice + factor(AgencyID) - 1, data = test_large_agencies_not_bunched)
# summary(test_model_26_not_bunched)
# Doesn't quite work because we're separating out the per-agency sample before the model

# Try the whole hog (takes 10-20 mins)
test_model_27 <- glm(PriceChangePercentageNoOutliers ~ (ProductProbabilityLevel4 * AnyBunching) + factor(AgencyID) - 1, data = test_large_agencies)
summary(test_model_27)
interplot(test_model_27, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching", sims = 50)
# Within agencies, on the other hand, more generic goods cost more when no bunching, indeterminate when bunching

# Now without forcing intercept
test_model_28 <- glm(PriceChangePercentageNoOutliers ~ (ProductProbabilityLevel4 * AnyBunching) + factor(AgencyID), data = test_large_agencies)
summary(test_model_28)
interplot(test_model_28, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching", sims = 50)
# Shows same thing: can sell this as "some agencies conform to the pooled model, eg MinDef, on average (FE) other way round as per my story"
# Need to be careful about conclusions without red flags, isn't this when corruption supposed to break down?

# Is there a year effect?
test_model_29 <- glm(PriceChangePercentageNoOutliers ~ (ProductProbabilityLevel4 * AnyBunching) + factor(Year) - 1, data = test_large_agencies)
summary(test_model_29)
interplot(test_model_29, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching", sims = 500)
# Robust to within-year version of test

test_model_30 <- glm(PriceChangePercentageNoOutliers ~ ProductProbabilityLevel4 + factor(Year) - 1, data = test_large_agencies)
summary(test_model_30)
# Intercepts are the same for each year
#interplot(test_model_30, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching", sims = 500)



#####################
# 3. Store measures #
#####################




# ENDS