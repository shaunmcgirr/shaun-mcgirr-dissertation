# Final versions of tests for chapter 4

# Metadata
source(file="3-Unpack/load-classifications.R")

## Set up data
# Relies on running build-purchase-level-data.R and build-agency-level-data.R first

### MOSCOW ONLY
# Load Moscow regional file
load("~/data/zakupki/2015-06-13/zakupki-2015-06-13-purchases-data/94fz/regions/Moskva_purchases_2015-06-13.rda")
purchases_moscow <- purchases; rm(purchases);
# load("~/data/zakupki/2015-06-13/zakupki-2015-06-13-purchases-data/94fz/regions/Sankt-Peterburg_purchases_2015-06-13.rda")
# purchases_petersburg <- purchases; rm(purchases)
# load("~/data/zakupki/2015-06-13/zakupki-2015-06-13-purchases-data/94fz/regions/Leningradskaja_obl_purchases_2015-06-13.rda")
# purchases_leningrad <- purchases; rm(purchases)

# Generate pretty-looking variables for modelling
purchases_moscow <- purchases_moscow %>%
  mutate(Disqualifications = ProportionDisqualified,
         Revisions = TotalRevisions,
         Bunching = log(ProximityRuleThreshold+1),
         ProductCommonness = ProductProbabilityLevel4Scaled,
         MaximumPriceLog = log10(NotificationLotCustomerRequirementMaxPrice+1))

### ALL REGIONS
load("~/data/zakupki/2015-06-13/zakupki-2015-06-13-purchases-data/all_purchases_2015-06-13_compress.rda")
# Generate pretty-looking variables for modelling
purchases_all <- purchases_all %>%
  mutate(Disqualifications = ProportionDisqualified,
         Revisions = TotalRevisions,
         Bunching = ProximityRuleThreshold,
         ProductCommonness = ProductProbabilityLevel4Scaled,
         MaximumPriceLog = log10(NotificationLotCustomerRequirementMaxPrice+1))
purchases_all$DatasetID = row.names(purchases_all)


########
## 4.1 #
########

# 4.1.2
# Are auctions with red flags different? Yes
test_model_6 <- lm(PriceChangePercentageNegativeOnly ~ NotificationLotCustomerRequirementMaxPrice + (ProductProbabilityLevel4Scaled * AnyBunching), data = purchases_moscow)
summary(test_model_6)
bunched_different <- interplot(test_model_6, var1 = "ProductProbabilityLevel4Scaled", var2 = "AnyBunching", esize = 0.5) +
                      theme_bw() +
                      scale_fill_tableau() +
                      labs(title = "Effect of 'bunching' red flag on association between product\ncommonness and auction efficiency, Moscow agencies\n",
                           x = "\n0 = Not listed within 5% of threshold; 1 = Listed within 5% of threshold",
                           y = "Expected change in price decrease over course of auction,\n moving from least common to most common good\n")
print(bunched_different)
ggsave(bunched_different, file = "./6-Present/chapter-four/bunched_different_moscow.pdf")

# 4.1.1
# Robustness: are there really more suppliers for more generic goods?
suppliers_per_product <- purchases_moscow %>%
  filter(!is.na(NotificationLotProductCode) & !is.na(ContractSupplierParticipantINN)) %>%
  group_by(NotificationLotProductCode, ProductProbabilityLevel4Scaled) %>%
  summarize(NumberOfSuppliersProduct = n_distinct(ContractSupplierParticipantINN)) %>%
  left_join(okdp_product_classification, by = c("NotificationLotProductCode" = "ProductCode"))
# plot(log10(suppliers_per_product$ProductProbabilityLevel4Scaled), log10(suppliers_per_product$NumberOfSuppliers))
#   lines(lowess(log10(suppliers_per_product$ProductProbabilityLevel4Scaled), log10(suppliers_per_product$NumberOfSuppliers), f = 1/100), col = "blue")
suppliers_per_product_graph <- ggplot(suppliers_per_product, aes(x = ProductProbabilityLevel4Scaled, y = NumberOfSuppliersProduct)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw() +
  stat_smooth(se = F, col = "orange") +
  labs(title = "Relationship between product commoness and\nnumber of unique suppliers, Moscow agencies\n",
       x = "\nProbability a purchase is of a given product\n(higher value = more common/generic product)",
       y = "Number of (unique) suppliers")
print(suppliers_per_product_graph)
ggsave(suppliers_per_product_graph, file = "./6-Present/chapter-four/suppliers_per_product_moscow.pdf", width = 6, height = 7)

# Does it hold across regions? Yes
# load("~/data/zakupki/2015-06-13/zakupki-2015-06-13-purchases-data/all_purchases_2015-06-13_compress.rda")
# suppliers_per_product_all <- purchases_all %>%
#   filter(!is.na(NotificationLotProductCode) & !is.na(ContractSupplierParticipantINN)) %>%
#   group_by(TenderPostingRegion, NotificationLotProductCode, ProductProbabilityLevel4Scaled) %>%
#   summarize(NumberOfSuppliers = n_distinct(ContractSupplierParticipantINN))
# suppliers_per_product_regional <- lm(NumberOfSuppliers ~ ProductProbabilityLevel4Scaled + factor(TenderPostingRegion) - 1, data = suppliers_per_product_all)
# summary(suppliers_per_product_regional)

# 4.1.3
# Same thing for revisions
model_revisions <- lm(PriceChangePercentageNegativeOnly ~ NotificationLotCustomerRequirementMaxPrice + ProductProbabilityLevel4Scaled + TotalRevisions + (ProductProbabilityLevel4Scaled * TotalRevisions), data = purchases_moscow[purchases_moscow$TotalRevisions < 11,])
summary(model_revisions)
total_revisions_graph <- interplot(model_revisions, var1 = "ProductProbabilityLevel4Scaled", var2 = "TotalRevisions", esize = 0.5, point = T) +
  theme_bw() +
  scale_fill_tableau() +
  labs(title = "Effect of 'mid-procedure revisions' red flag on association between\nproduct commonness and auction efficiency, Moscow agencies\n",
       x = "\nTotal number of revisions to purchase",
       y = "Expected change in price decrease over course of auction,\n moving from least common to most common good\n")
print(total_revisions_graph)
ggsave(total_revisions_graph, file = "./6-Present/chapter-four/total_revisions_graph_moscow.pdf")

# Does this one hold within regions?
# model_revisions_fe <- lm(PriceChangePercentageNegativeOnly ~ NotificationLotCustomerRequirementMaxPrice + (ProductProbabilityLevel4Scaled * TotalRevisions) + factor(TenderPostingRegion) - 1, data = purchases_all)#[purchases_all$TotalRevisions < 11,])
# summary(model_revisions_fe)
# interplot(model_revisions_fe, var1 = "ProductProbabilityLevel4Scaled", var2 = "TotalRevisions", esize = 0.5, point = T, sims = 10)
# Effect reverses but now we have agencies to deal with so no big deal...

# 4.1.4
# Number of applicants/admitted/disqualified
model_disqualifications <- lm(PriceChangePercentageNegativeOnly ~ NotificationLotCustomerRequirementMaxPrice + ProductProbabilityLevel4Scaled + ProportionDisqualified + (ProductProbabilityLevel4Scaled * ProportionDisqualified), data = purchases_moscow)
summary(model_disqualifications)
disqualifications_graph <- interplot(model_disqualifications, var1 = "ProductProbabilityLevel4Scaled", var2 = "ProportionDisqualified", esize = 0.5, point = F) +
  theme_bw() +
  scale_fill_tableau() +
  labs(title = "Effect of 'disqualifications' red flag on association between\nproduct commonness and auction efficiency, Moscow agencies\n",
       x = "\nProportion of bidders disqualified from auction",
       y = "Expected change in price decrease over course of auction,\n moving from least common to most common good\n") +
  scale_y_continuous(breaks = c(-2, 0, 2, 4, 6, 8, 10))
print(disqualifications_graph)
ggsave(disqualifications_graph, file = "./6-Present/chapter-four/disqualifications_graph_moscow.pdf")


# 4.1.7
# Histogram of purchase specificity if needed
# purchase_specificity_graph_moscow <- purchases_moscow %>%
#   filter(!is.na(ProductProbabilityLevel4Scaled) & ProductProbabilityLevel4Scaled > 0) %>%
#   ggplot(aes(x = (ProductProbabilityLevel4Scaled))) +
#   geom_histogram(binwidth = 0.1, na.rm = T) +
#   scale_y_log10(labels = comma) +
#   scale_x_log10()
# print(purchase_specificity_graph_moscow)


# Are procedures differently susceptible to corruption?
model_procedure_disqualifications <- lm(Disqualifications ~ TenderProcedureGroup - 1, data = purchases_moscow)
summary(model_procedure_disqualifications)
model_procedure_revisions <- lm(Revisions ~ TenderProcedureGroup - 1, data = purchases_moscow)
summary(model_procedure_revisions)
model_procedure_bunching <- lm(Bunching ~ TenderProcedureGroup - 1, data = purchases_moscow)
summary(model_procedure_bunching)
stargazer(model_procedure_disqualifications, model_procedure_revisions, model_procedure_bunching, title = "Differences in corruption propensity by procedure type, Moscow", style = "apsr", covariate.labels = c("Open electronic auction", "Open tender", "Request for quotes", "Olympic construction", "Preliminary selection"), df = F, omit.stat = c("adj.rsq", "f"), notes = "Note: Some procedures use no initial price, so `bunching' is ruled out", out = "../dissertation-text/tables/corruption_propensity_by_procedure.tex", label = "corruption-propensity-by-procedure")
# Olympic construction most highly correlated, as expected, then the rest are pretty close really

# Models predicting corruption
model_rf_disqualifications <- lm(Disqualifications ~ TenderProcedureGroup + ProductCommonness + MaximumPriceLog + (ProductCommonness * MaximumPriceLog), data = purchases_moscow)
summary(model_rf_disqualifications)
interplot(model_rf_disqualifications, var1 = "ProductCommonness", var2 = "MaximumPriceLog")
# As you buy more expensive things, more generic goods more assoc with increase in corruption (go after big fish)
# Also holds with regional fe
model_rf_disqualifications_fe <- lm(Disqualifications ~ TenderProcedureGroup + ProductCommonness + MaximumPriceLog + (ProductCommonness * MaximumPriceLog) + factor(TenderPostingRegion) - 1, data = purchases_all)
summary(model_rf_disqualifications_fe)
interplot(model_rf_disqualifications_fe, var1 = "ProductCommonness", var2 = "MaximumPriceLog", sims = 20)

model_rf_revisions <- lm(Revisions ~ TenderProcedureGroup + ProductCommonness + MaximumPriceLog + (ProductCommonness * MaximumPriceLog), data = purchases_moscow)
summary(model_rf_revisions)
interplot(model_rf_revisions, var1 = "ProductCommonness", var2 = "MaximumPriceLog")
# Same goes for revisions
model_rf_revisions_fe <- lm(Revisions ~ TenderProcedureGroup + ProductCommonness + MaximumPriceLog + (ProductCommonness * MaximumPriceLog) + factor(TenderPostingRegion) - 1, data = purchases_all)
summary(model_rf_revisions_fe)
interplot(model_rf_revisions_fe, var1 = "ProductCommonness", var2 = "MaximumPriceLog", sims = 20)
# Holds with regional f.e. YES

model_rf_bunching <- lm(Bunching ~ TenderProcedureGroup + ProductCommonness + MaximumPriceLog + (ProductCommonness * MaximumPriceLog), data = purchases_moscow)
summary(model_rf_bunching)
interplot(model_rf_bunching, var1 = "ProductCommonness", var2 = "MaximumPriceLog")
# Market logic dominates more (OK as this is hokey proximity measure)
# At very least, the more expensive
model_rf_bunching_fe <- lm(Bunching ~ TenderProcedureGroup + ProductCommonness + MaximumPriceLog + (ProductCommonness * MaximumPriceLog) + factor(TenderPostingRegion) - 1, data = purchases_all)
summary(model_rf_bunching_fe)
interplot(model_rf_bunching_fe, var1 = "ProductCommonness", var2 = "MaximumPriceLog", sims = 20)
# Opposite story for this measure, but it's too correlated with max price
cor(purchases_all$ProximityRuleThreshold, purchases_all$NotificationLotCustomerRequirementMaxPrice, use = "complete")

# Output table of results
# Why can't I get the order correct?
stargazer(model_rf_disqualifications, model_rf_disqualifications_fe, model_rf_revisions, model_rf_revisions_fe, model_rf_bunching, model_rf_bunching_fe,
          title = "Determinants of purchase-level corruption, Moscow (odds) and all regions (evens)",
          omit = "factor",
          omit.labels = "Region fixed effects",
          # dep.var.labels = c("Bunching", "Revisions", "Disqualifications"),
          # order = c(5, 6, 7, 1, 2, 3),
          covariate.labels = c("Open electronic auction", "Open tender", "Request for quotes", "Olympic construction", "\\textbf{Product commonness}", "\\textbf{Maximum price (log)}", "\\textbf{Commonness x Max price}"),
          # df = F,
          omit.stat = c("adj.rsq", "f", "ll", "ser"), # "aic"; 
          # notes = "Note: Some procedures use no initial price, so `bunching' is ruled out",
          out = "../dissertation-text/tables/purchase_corruption_models.tex",
          label = "purchase-corruption-models",
          style = "apsr",
          float.env = "sidewaystable")

# Output graph of disqualifications marginal effects (fe version)
disqualifications_model_purchases_fe_graph <- interplot(model_rf_disqualifications_fe, var1 = "ProductCommonness", var2 = "MaximumPriceLog", sims = 1000) +
  theme_bw() +
  scale_fill_tableau() +
  # scale_y_continuous(breaks = c(-2, 0, 2, 4, 6, 8, 10)) +
  labs(title = "Effect of product commonness on 'disqualifications' corruption proxy\nfor different purchase values, all regions (model 2, region fixed effects)\n",
       x = "\nStarting price of reverse auction (a.k.a. initial/maximum price) chosen by agency, log base 10",
       y = "Expected change in purchase-level corruption proxy\nmoving from least common to most common good\n")+
  geom_hline(yintercept = 0, linetype = "dotted")
print(disqualifications_model_purchases_fe_graph)
ggsave(disqualifications_model_purchases_fe_graph, file = "./6-Present/chapter-four/disqualifications_model_purchases_fe_graph.pdf")

# Calculate threshold where it crosses zero
threshold <- interplot(model_rf_disqualifications_fe, var1 = "PurchaseSpecificity", var2 = "MaximumPriceLog", sims = 1000)
threshold_data <- threshold$data
threshold <- threshold_data %>% filter(coef1 == min(abs(coef1))) %>% select(fake)
print(paste0("Estimated coefficient crosses zero at approximately ", 10^threshold, " rubles"))



########
## 4.2 #
########

# Load agency-level data for Moscow
load("~/data/zakupki/2015-06-13/zakupki-2015-06-13-agencies-data/94fz/regions/Moskva_agencies_2015-06-13.rda")
agencies_moscow <- agencies; rm(agencies);
agencies_moscow$DatasetID <- row.names(agencies_moscow)

# Also load all agencies all regions
load("~/data/zakupki/2015-06-13/zakupki-2015-06-13-agencies-data/all_agencies_2015-06-13_compress.rda")

# Generate table of Moscow-based ministries across the three measures
moscow_ministry_names <- read.csv(file = "./6-Present/moscow_ministry_names.csv", stringsAsFactors = F, colClasses = "character")
moscow_ministries <- agencies_moscow %>%
  filter(grepl("Министерство", AgencyName)) %>%
  inner_join(moscow_ministry_names, by = c("AgencyID")) %>%
  transmute(AgencyID = AgencyID,
            `Ministry name` = AgencyNameEnglish,
            `Disqual.` = dense_rank(-MeanDisqualifications),
            # `Disqualifications (mean)` = MeanDisqualifications,
            `Revisions` = dense_rank(-MeanRevisions),
            # `Revisions (mean)` = MeanRevisions,
            `Bunching` = dense_rank(-MeanContinuousBunching),
            # `Bunching (mean)` = MeanContinuousBunching,
            `Wage` = AverageWage2012USD) %>%
  rowwise %>%
    mutate(`Average rank` = mean(c(`Disqual.`,
                                   `Revisions`,
                                   `Bunching`))) %>%
  arrange(`Average rank`) %>%
  select(-`Average rank`, -AgencyID)

# Output for Latex
moscow_ministries_output <- xtable(moscow_ministries, caption = "Rankings of federal ministries by various corruption measures, and 2012 average monthly wage in USD", align = c("l", "l", "r", "r", "r", "r"))
print(moscow_ministries_output, include.rownames = F, latex.environments = "center")

# Check DVs and key IVs after aggregation
# hist(agencies$MeanBinaryBunching) # Heavily skewed as expected (Moscow ok)
# hist(agencies$MeanContinuousBunching) # Good
# hist(agencies$MedianContinuousBunching) # Good
# hist(agencies$MeanRevisions) # Power law
# hist(agencies$MedianRevisions) # Too sparse
# hist((agencies$MeanDisqualifications)) # Power law
# hist(agencies$MedianDisqualifications) # Very skewed/sparse (b/c count variable underneath)
# hist(agencies$MeanSupplierFavoritism) # Power law
# hist(agencies$MedianSupplierFavoritism) # Very skewed
# hist(agencies$MeanProductCommonnessLevel4) # Power law
# hist(agencies$MedianProductCommonnessLevel4) # More skewed/sparse than mean

## 4.2.1
# Moscow first quick models
quick_model_1 <- lm(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MedianProductCommonnessLevel4, data = agencies_moscow)
summary(quick_model_1)
# Seems to be robust positive unconditional association between median commonnes and each DV, except MeanBinaryBunching

quick_model_2 <- lm(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MedianProductCommonnessLevel4 + MedianAuctionEfficiency + (MedianProductCommonnessLevel4 * MedianAuctionEfficiency), data = agencies_moscow)
summary(quick_model_2)
interplot(quick_model_2, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency")

quick_model_3 <- lm(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MeanListingDuration + MeanBiddersApplied + MeanSupplierFavoritism + MedianProductCommonnessLevel4 + MedianAuctionEfficiency + (MedianProductCommonnessLevel4 * MedianAuctionEfficiency), data = agencies_moscow)
summary(quick_model_3)
interplot(quick_model_3, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency")
# Variables with most reasonable distributions and conceptual fit
# Spending: log10(TotalAgencySpendInitial)
# Commonness: MedianProductCommonnessLevel4 (pattern holds but makes little sense to log a median!)
# DV: revisions and disqualifications both work, bunching correlated with auction efficiency
# Bidders applied vs admitted: MeanBiddersAdmitted less peaky
quick_model_3_no_int <- lm(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MeanListingDuration + MeanBiddersApplied + MeanSupplierFavoritism + MedianProductCommonnessLevel4 + MedianAuctionEfficiency + (MedianProductCommonnessLevel4 * MedianAuctionEfficiency) - 1, data = agencies_moscow)
summary(quick_model_3_no_int)
interplot(quick_model_3_no_int, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency", hist = T)

# What about weighted
quick_model_3_no_int_weighted <- lm(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MeanListingDuration + MeanBiddersApplied + MeanSupplierFavoritism + MedianProductCommonnessLevel4 + MedianAuctionEfficiency + (MedianProductCommonnessLevel4 * MedianAuctionEfficiency) - 1, data = agencies_moscow, weights = agencies_moscow$NumberOfPurchases)
summary(quick_model_3_no_int_weighted)
interplot(quick_model_3_no_int_weighted, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency")


# Check correlations
cor(agencies_moscow$MeanListingDuration, agencies_moscow$MeanBiddersApplied, use = "complete")
cor(agencies_moscow$MeanDisqualifications, agencies_moscow$MeanBiddersApplied, use = "complete")
cor(agencies_moscow$MeanSupplierFavoritism, agencies_moscow$MedianAuctionEfficiency, use = "complete")
cor(agencies_moscow$MeanSupplierFavoritism, agencies_moscow$TotalAgencySpendInitial, use = "complete")

# Draw marginal effects graph for Moscow
agency_corruption_simple_graph_moscow <- interplot(quick_model_3_no_int, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency", hist = T) +
  theme_bw() +
  scale_fill_tableau() +
  # scale_y_continuous(breaks = c(-2, 0, 2, 4, 6, 8, 10)) +
  labs(title = "Effect of agency-level product commonness on 'disqualifications' corruption proxy\nfor different levels of corruption opportunity, Moscow (model 4)\n",
       x = "\nAverage corruption opportunities\n(change in price over course of auction; more negative = less opportunity)",
       y = "Expected change in agency-level corruption proxy\nmoving from least common to most common good\n")+
  geom_hline(yintercept = 0, linetype = "dotted")
print(agency_corruption_simple_graph_moscow)
ggsave(agency_corruption_simple_graph_moscow, file = "./6-Present/chapter-four/agency_corruption_simple_graph_moscow.pdf", width = 8, height = 8)


# Formula corruption = (agency size) + More common goods + Less efficient auctions seems to hold
# Robust to alternative summary measures for each parameter, output
stargazer(quick_model_1, quick_model_2, quick_model_3, quick_model_3_no_int,
          title = "Determinants of agency-level corruption, Moscow",
          omit = "factor",
          # omit.labels = "Region fixed effects",
          dep.var.labels = c("Mean Disqualifications"),
          multicolumn = TRUE,
          # order = c(5, 6, 7, 1, 2, 3),
          covariate.labels = c("Agency spend (log base 10)", "Mean listing duration", "Mean bidders applied", "Mean supplier favoritism", "\\textbf{Median product commonness}", "\\textbf{Median auction efficiency}", "\\textbf{Commonness x Efficiency}"),
          # df = F,
          omit.stat = c("adj.rsq", "f", "ll", "ser"), # "aic"; 
          # notes = "Note: Some procedures use no initial price, so `bunching' is ruled out",
          out = "../dissertation-text/tables/agency_corruption_simple_models.tex",
          label = "agency-corruption-simple-models",
          # float.env = "sidewaystable",
          style = "apsr")

# Check correlation of bunching RF with pricechange, not high
# cor(agencies_moscow$MeanContinuousBunching, agencies_moscow$MedianAuctionEfficiency, use = "complete")

## 4.2.3 
# Residuals from a market model: use the red flags to predict 
# model_disqualifications <- lm(PriceChangePercentageNegativeOnly ~ NotificationLotCustomerRequirementMaxPrice + ProductProbabilityLevel4Scaled + ProportionDisqualified + (ProductProbabilityLevel4Scaled * ProportionDisqualified), data = purchases_moscow)
# quick_model_3_no_int <- lm(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MeanListingDuration + MeanBiddersApplied + MeanSupplierFavoritism + MedianProductCommonnessLevel4 + MedianAuctionEfficiency + (MedianProductCommonnessLevel4 * MedianAuctionEfficiency) - 1, data = agencies_moscow)

# Naive model, what if no corruption and no agencies?
market_model_1 <- lm(MedianAuctionEfficiency ~ log10(TotalAgencySpendInitial) + MedianProductCommonnessLevel4 + MeanListingDuration + MeanBiddersApplied, data = agencies_moscow)
summary(market_model_1)

# Add number of purchases
market_model_2 <- lm(MedianAuctionEfficiency ~ NumberOfPurchases + log10(TotalAgencySpendInitial) + MeanListingDuration + MeanBiddersApplied, data = agencies_moscow)
summary(market_model_2)

# Do it for all regions
market_model_3 <- lm(MedianAuctionEfficiency ~ NumberOfPurchases + log10(TotalAgencySpendInitial) + MeanListingDuration + MeanBiddersApplied + factor(AgencyRegion), data = agencies_all)
summary(market_model_3)

agency_residuals <- data.frame(market_model_2$residuals)
agency_residuals <- data.frame(DatasetID = row.names(agency_residuals),
                               AgencyResidual = market_model_2$residuals,
                               stringsAsFactors = F)

agencies_moscow_residuals <- agencies_moscow %>%
  inner_join(agency_residuals) %>%
  filter(AgencyResidual > 0)

# What are correlations between measures and absolute values of residuals?
cor(agencies_moscow_residuals$MeanDisqualifications, abs(agencies_moscow_residuals$AgencyResidual))
cor(agencies_moscow_residuals$MeanRevisions, abs(agencies_moscow_residuals$AgencyResidual))
cor(agencies_moscow_residuals$MeanContinuousBunching, abs(agencies_moscow_residuals$AgencyResidual), use = "complete")

# Can we predict residuals with our red flag measures?
residuals_model_1 <- lm((AgencyResidual) ~ MeanDisqualifications + MeanRevisions + MeanContinuousBunching - 1, data = agencies_moscow_residuals)
summary(residuals_model_1)

# Try same thing at purchase level for validation
# market_model_1 <- lm(MedianAuctionEfficiency ~ log10(TotalAgencySpendInitial) + MedianProductCommonnessLevel4 + MeanListingDuration + MeanBiddersApplied, data = agencies_moscow)

market_model_purchases_1 <- lm(PriceChangePercentageNegativeOnly ~ MaximumPriceLog + ProductProbabilityLevel4Scaled + ProcedureDuration + NumberOfApplicants, data = purchases_all)
summary(market_model_purchases_1)

# Everything works when the residuals are generated at regional level too!
# market_model_purchases_1_fe <- lm(PriceChangePercentageNegativeOnly ~ MaximumPriceLog + ProductProbabilityLevel4Scaled + ProcedureDuration + NumberOfApplicants + factor(TenderPostingRegion) - 1, data = purchases_all)
# summary(market_model_purchases_1_fe)

purchase_residuals <- data.frame(market_model_purchases_1$residuals)
purchase_residuals <- data.frame(DatasetID = row.names(purchase_residuals),
                                 PurchaseResidual = market_model_purchases_1$residuals,
                                 stringsAsFactors = F)

purchases_all_residuals <- purchases_all %>%
  inner_join(purchase_residuals) %>%
  filter(PurchaseResidual > 0) %>%
  select(PurchaseResidual, Disqualifications, Revisions, Bunching)

residuals_model_purchases_1 <- lm(PurchaseResidual ~ Disqualifications + Revisions + Bunching - 1, data = purchases_all_residuals)
summary(residuals_model_purchases_1)  

########
## 4.3 #
########

# Start by replicating the models in 4.2 with regional fixed effects

# Check distributions of key variables in agencies_all
# hist(agencies_all$MeanBinaryBunching) # Heavily skewed as expected (Moscow ok)
# hist(agencies_all$MeanContinuousBunching) # Good
# hist(agencies_all$MedianContinuousBunching) # Good
# hist(agencies_all$MeanRevisions) # Power law (all agencies very skewed)
# hist(agencies_all$MedianRevisions) # Too sparse (all agencies very skewed)
# hist((agencies_all$MeanDisqualifications)) # Power law
# hist(agencies_all$MedianDisqualifications) # Very skewed/sparse (b/c count variable underneath), same for all agencies
# hist(agencies_all$MeanSupplierFavoritism) # Power law
# hist(agencies_all$MedianSupplierFavoritism) # Very skewed
# hist(agencies_all$MeanProductCommonnessLevel4) # Power law (all agencies beautiful)
# hist(agencies_all$MedianProductCommonnessLevel4) # More skewed/sparse than mean

summary(agencies_all$MeanDisqualifications)
summary(agencies_all$TotalAgencySpendInitial) # Need to add one ruble to all agency budgets back in build!
summary(agencies_all$MedianProductCommonnessLevel4)

agencies_all_more_than_one_purchase <- agencies_all %>%
  filter(MedianAuctionEfficiency <= 1)
  # filter(NumberOfPurchases > 1)

# All agencies, with region fixed effects
quick_model_1_fe <- lm(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MedianProductCommonnessLevel4 + factor(AgencyRegion) - 1, data = agencies_all_more_than_one_purchase)
summary(quick_model_1_fe)
# Seems to be robust positive unconditional association between median commonnes and each DV, except MeanBinaryBunching

quick_model_2_fe <- lm(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MedianProductCommonnessLevel4 + MedianAuctionEfficiency + (MedianProductCommonnessLevel4 * MedianAuctionEfficiency) + factor(AgencyRegion) - 1, data = agencies_all_more_than_one_purchase)
summary(quick_model_2_fe)
interplot(quick_model_2_fe, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency", sims = 2000)
# Very poorly estimated, quite kooky

quick_model_3_fe <- lm(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MeanListingDuration + MeanBiddersApplied + MeanSupplierFavoritism + MedianProductCommonnessLevel4 + MedianAuctionEfficiency + (MedianProductCommonnessLevel4 * MedianAuctionEfficiency) + factor(AgencyRegion) - 1, data = agencies_all_more_than_one_purchase)
summary(quick_model_3_fe)
interplot(quick_model_3_fe, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency", sims = 2000, hist = T)
# Do region intercepts from this work?

# Same as above but weight agencies by number of purchases
quick_model_3_fe_weighted <- lm(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MeanListingDuration + MeanBiddersApplied + MeanSupplierFavoritism + MedianProductCommonnessLevel4 + MedianAuctionEfficiency + (MedianProductCommonnessLevel4 * MedianAuctionEfficiency) + factor(AgencyRegion) - 1, data = agencies_all_more_than_one_purchase, weights = log(agencies_all_more_than_one_purchase$NumberOfPurchases))
summary(quick_model_3_fe_weighted)
interplot(quick_model_3_fe_weighted, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency", sims = 2000, hist = T)
# No substantive difference, although diagnostic plots improve

# Draw marginal effects graph for all regions
agency_corruption_simple_graph_all <- interplot(quick_model_3_fe, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency", sims = 2000, hist = T) +
  theme_bw() +
  scale_fill_tableau() +
  # scale_y_continuous(breaks = c(-2, 0, 2, 4, 6, 8, 10)) +
  labs(title = "Effect of agency-level product commonness on 'disqualifications' corruption proxy\nfor different levels of corruption opportunity, all regions (region fixed effects)\n",
       x = "\nAverage corruption opportunities\n(change in price over course of auction; more negative = less opportunity)",
       y = "Expected change in agency-level corruption proxy\nmoving from least common to most common good\n")+
  geom_hline(yintercept = 0, linetype = "dotted")
print(agency_corruption_simple_graph_all)
ggsave(agency_corruption_simple_graph_all, file = "./6-Present/chapter-four/agency_corruption_simple_graph_all.pdf", width = 8.5, height = 8)


# Can I put the fixed effects on a map? Pain in the ass, as always
# library(rgdal)
# russia_base_layer <- readOGR(dsn = path.expand("~/Downloads/RUS_adm_shp"), layer = "RUS_adm1")
# # Crashes as too detailed? test <- plot(russia_base_layer)

# library(maps)
# russia_base_layer <- map(database = "world", regions = "Russia")

# library(maptools) # install.packages("maptools")
# library(raster)   # install.packages("raster")
# ru1 <- getData("GADM", country = "RU", level = )

# Problem here is still throwing away all that amazing purchase-level variation! Means/medians of these things not very meaningful.
# What do the intercepts say about regional variations?
regional_intercepts <- data.frame(Coefficient = names(quick_model_3_fe$coefficients),
                                  Estimate = quick_model_3_fe$coefficients,
                                  stringsAsFactors = F) %>%
  filter(grepl("factor", Coefficient))
row.names(regional_intercepts) <- NULL
regional_intercepts$Coefficient <- sapply(strsplit(regional_intercepts$Coefficient, split = ")", fixed = T), function(x) (x[2]))
# Not really worth putting on a map anyway as nothing interesting going on!


## 4.2.2
# Load other data to get regions
source("./3-Unpack/load-other-data.R")
# source("4-Construct/build-regional-level-data.R")

load(file = "~/data/zakupki/2015-06-13/zakupki-2015-06-13-regions-data/94fz/icsid_filtered_2015-06-13.rda")

# Does a simple model explain residuals?
# regional_intercepts_icsid <- regional_intercepts %>%
#   left_join(icsid_filtered, by = c("Coefficient" = "RegionNameDB")) %>%
#   filter(!is.na(ISO_id))
# 
# explain_residuals_1 <- lm(Estimate ~ PopulationLog + DistanceFromMoscowLog + GRPPerCapita, data = regional_intercepts_icsid)
#   summary(explain_residuals_1)
# explain_residuals_2 <- lm(Estimate ~ EfficiencyPublicSpendingIndex + BureaucratWagePremium + ShareOfEconomicCrimes + TaxCapacityIndex, data = regional_intercepts_icsid)
#   summary(explain_residuals_2)
# explain_residuals_3 <- lm(Estimate ~ ExecutiveEmployeesPercentage, data = regional_intercepts_icsid)
#   summary(explain_residuals_3)
# Not much going on here, skip straight to hierarchical approach
  
  
## Comprehensive regional tests strategy
# Just do everything based on 2011 or 2012 as "setting the stage", let panel be a committee suggestion (need Alton anyway)
# Compute region-level average, model as function of cross-regional variables; de facto "cross national replication"
regions_from_agencies <- agencies_all %>%
  group_by(TenderPostingRegion) %>%
  summarize(MeanDisqualifications = mean_na(MeanDisqualifications),
            MedianDisqualifications = median_na(MedianDisqualifications),
            MeanRevisions = mean_na(MeanRevisions),
            MedianRevisions = median_na(MedianRevisions),
            MeanContinuousBunching = mean_na(MeanContinuousBunching),
            MedianContinuousBunching = median_na(MedianContinuousBunching),
            MeanAuctionEfficiency = mean_na(MeanAuctionEfficiency),
            MedianAuctionEfficiency = median_na(MedianAuctionEfficiency),
            MeanProductCommonnessLevel4 = mean_na(MeanProductCommonnessLevel4),
            MedianProductCommonnessLevel4 = median_na(MedianProductCommonnessLevel4),
            TotalRegionalSpend = sum(TotalAgencySpendInitial, na.rm = T)) %>% ungroup() %>%
  left_join(icsid_filtered, by = c("TenderPostingRegion" = "RegionNameDB")) %>%
  filter(!is.na(ISO_id))

simple_regional_model_1 <- lm(MeanDisqualifications ~ MeanAuctionEfficiency, data = regions_from_agencies)
summary(simple_regional_model_1)

simple_regional_model_2 <- lm(MeanDisqualifications ~ TotalRegionalSpend + MeanProductCommonnessLevel4  + MeanAuctionEfficiency + (MeanProductCommonnessLevel4 * MeanAuctionEfficiency), data = regions_from_agencies)
summary(simple_regional_model_2)
interplot(simple_regional_model_2, var1 = "MeanAuctionEfficiency", var2 = "MeanProductCommonnessLevel4")
# Nothing much informative here, just too much variation has been sucked away by means of means etc

## Hierarchical model
# Start with dead-simple, add justifiable controls, cut back down to a parsimonious specification
# Hieararchical model structure
#   - Agency-level: Spending, median product commonness, median auction efficiency, interaction
#   - Region-level: population, distance, and max one variable each for politics/economics/bureaucracy/media/wages/capacity/resources/enterprises/transfers, 
#   - Be careful to separate the levels, eg may not need regional-level spending/enterprises as agency measures include them
#     - Also the bureaucracy measures may not make sense if I am saying "agencies are what they buy"

agencies_in_regions <- agencies_all %>%
  left_join(icsid_filtered, by = c("TenderPostingRegion" = "RegionNameDB")) %>%
  filter(!is.na(ISO_id)) %>%
  filter(MeanAuctionEfficiency <= 1 & MedianAuctionEfficiency <= 1) %>%
  filter(NumberOfPurchases > 1) %>%
  filter(MeanListingDuration > 0)

## Start with null models as per http://tutorials.iq.harvard.edu/R/Rstatistics/Rstatistics.html#orgheadline36
null_model_disqualifications <- lmer(MeanDisqualifications ~ 1 + (1 | RegionNameEnglish), data = agencies_in_regions, REML = F)
summary(null_model_disqualifications)
0.00029185864540/(0.00029185864540 + 0.00964504595041) * 100
# 2.9% variance at region level

null_model_revisions <- lmer(MeanRevisions ~ 1 + (1 | RegionNameEnglish), data = agencies_in_regions, REML = F)
summary(null_model_revisions)
0.00140397855486/(0.00140397855486 + 0.06252440221057) * 100
# 2.2% variance at region level

null_model_bunching <- lmer(MeanContinuousBunching ~ 1 + (1|RegionNameEnglish), data = agencies_in_regions, REML = F)
summary(null_model_bunching)
0.00439675510765/(0.00439675510765 + 0.04276807374024) * 100
# 9.3% variance at region level

## Now add agency-level predictors to reproduce FE models from above
fe_model_disqualifications <- lmer(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MeanListingDuration + MeanBiddersApplied + MeanSupplierFavoritism + MedianProductCommonnessLevel4 + MedianAuctionEfficiency + (MedianProductCommonnessLevel4 * MedianAuctionEfficiency) + (1|RegionNameEnglish), data = agencies_in_regions, REML = F)
summary(fe_model_disqualifications)
# Commonness -; Efficiency + without interaction
interplot(fe_model_disqualifications, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency")

fe_model_revisions <- lmer(MeanRevisions ~ log10(TotalAgencySpendInitial) + MeanListingDuration + MeanBiddersApplied + MeanSupplierFavoritism + MedianProductCommonnessLevel4 + MedianAuctionEfficiency + (MedianProductCommonnessLevel4 * MedianAuctionEfficiency) + (1|RegionNameEnglish), data = agencies_in_regions, REML = F)
summary(fe_model_revisions)
# Commonness +; Efficiency + without interactions
interplot(fe_model_revisions, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency")

fe_model_bunching <- lmer(MeanContinuousBunching ~ log10(TotalAgencySpendInitial) + MeanListingDuration + MeanBiddersApplied + MeanSupplierFavoritism + MedianProductCommonnessLevel4 + MedianAuctionEfficiency + (MedianProductCommonnessLevel4 * MedianAuctionEfficiency) + (1|RegionNameEnglish), data = agencies_in_regions, REML = F)
summary(fe_model_bunching)
# Commonness -; Efficiency +
interplot(fe_model_bunching, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency")

## Now add some random slopes for the groups (keep it minimal, maybe just one var at a time even)
re_model_disqualifications <- lmer(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MeanListingDuration + MeanBiddersApplied + MeanSupplierFavoritism + MedianProductCommonnessLevel4 + MedianAuctionEfficiency + (MedianProductCommonnessLevel4 * MedianAuctionEfficiency) + GRPPerCapita + (1 + GRPPerCapita | RegionNameEnglish), data = agencies_in_regions, REML = F)
summary(re_model_disqualifications)
interplot(re_model_disqualifications, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency")
# The within-region story still holds
# GRP only
0.000282967547179/(0.000282967547179+0.000527064043992+0.006050828721820) * 100 # 4.1% regional
0.000527064043992/(0.000282967547179+0.000527064043992+0.006050828721820) * 100 # 7.7% GRP
0.006050828721820/(0.000282967547179+0.000527064043992+0.006050828721820) * 100 # 88% residual

# Take all the way back to basics: do it wrong first (model 1 table 4.3 but with regional variables)
# Then tidy up the variance with HLM
# lm_model_disqualifications <- lm(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MedianProductCommonnessLevel4 * MedianAuctionEfficiency + GRPPerCapita + URNationalVoteShare2011 + EfficiencyPublicSpendingIndex + NewspaperCoveragePer1000 + BureaucratWagePremium + TaxCapacityIndex, data = agencies_in_regions)
lm_model_disqualifications <- lm(MeanDisqualifications ~ log10(TotalAgencySpendInitial) + MeanListingDuration + MeanBiddersApplied + MeanSupplierFavoritism + MedianProductCommonnessLevel4 * MedianAuctionEfficiency + GRPVolumeIndex + URNationalVoteShare2011 + TaxCapacityIndex + NewspaperCoveragePer1000 + GRPFromMining + BureaucratWagePremium, data = agencies_in_regions)
summary(lm_model_disqualifications)
interplot(lm_model_disqualifications, var1 = "MedianProductCommonnessLevel4", var2 = "MedianAuctionEfficiency")

# Compare this to without the agency variables
lm_model_disqualifications_no_agency <- lm(MeanDisqualifications ~ GRPVolumeIndex + URNationalVoteShare2011 + TaxCapacityIndex + NewspaperCoveragePer1000 + GRPFromMining + BureaucratWagePremium, data = agencies_in_regions)
summary(lm_model_disqualifications_no_agency)
# Some of the coefficients go away
# Looking at them, they're all plausibly correlated with at least one other, so should be in there to avoid OVB


## Final models for 4.3.2
# Create separate data frame with best-as-possible labelling of variable names
agencies_in_regions_simplified <- agencies_in_regions %>%
  transmute(Region = RegionNameEnglish,
            Disqualifications = MeanDisqualifications,
            Revisions = MeanRevisions,
            Bunching = MeanBinaryBunching,
            AgencySpendLog = log10(TotalAgencySpendInitial),
            ListingDuration = MeanListingDuration,
            BiddersApplied = MeanBiddersApplied,
            SupplierFavoritism = MeanSupplierFavoritism,
            Commonness = MedianProductCommonnessLevel4,
            Efficiency = MedianAuctionEfficiency,
            GRPConstantPrices = GRPVolumeIndex,
            PartyDominance = URNationalVoteShare2011,
            TaxCapacity = TaxCapacityIndex,
            Newspapers = NewspaperCoveragePer1000,
            GRPFromMining,
            BureaucratWages = BureaucratWagePremium)
# Can I safely restrict to complete cases?
summary(agencies_in_regions_simplified) # Yes, only 42 agencies missing DV
agencies_in_regions_simplified <- agencies_in_regions_simplified %>% filter(complete.cases(.))


## Reproduce model (3) within mixed framework
final_model_disqualifications_baseline <- lmer(Disqualifications ~ AgencySpendLog + ListingDuration + BiddersApplied + SupplierFavoritism + Commonness * Efficiency + (1 |Region), data = agencies_in_regions_simplified)
summary(final_model_disqualifications_baseline)
interplot(final_model_disqualifications_baseline, var1 = "Commonness", var2 = "Efficiency")
# This may show that when UR has it locked up, more common -> less corruption, so locking up politics allows free-for all?
# With revisions instead
final_model_disqualifications_baseline_revisions <- lmer((Revisions) ~ AgencySpendLog + ListingDuration + BiddersApplied + SupplierFavoritism + Commonness * Efficiency + (1 |Region), data = agencies_in_regions_simplified)
summary(final_model_disqualifications_baseline_revisions)
interplot(final_model_disqualifications_baseline_revisions, var1 = "Commonness", var2 = "Efficiency")


## Now make the interaction a random effect
final_model_disqualifications_random_agency <- lmer(Disqualifications ~ AgencySpendLog + ListingDuration + BiddersApplied + SupplierFavoritism + Commonness * Efficiency + (1 + Commonness * Efficiency|Region), data = agencies_in_regions_simplified)
summary(final_model_disqualifications_random_agency)
interplot(final_model_disqualifications_random_agency, var1 = "Commonness", var2 = "Efficiency")

## Now add region-level predictors as fixed effects
final_model_disqualifications_random_agency_fixed_region <- lmer(Disqualifications ~ AgencySpendLog + ListingDuration + BiddersApplied + SupplierFavoritism + Commonness * Efficiency + GRPConstantPrices + PartyDominance + TaxCapacity + Newspapers + GRPFromMining + BureaucratWages + (1 + Commonness * Efficiency|Region), data = agencies_in_regions_simplified)
summary(final_model_disqualifications_random_agency_fixed_region)
interplot(final_model_disqualifications_random_agency_fixed_region, var1 = "Commonness", var2 = "Efficiency")

## Finally make some region-level predictors random effects
# For what variables does the effect of them on corruption vary by region? Move in to random component
# All the agency-level controls are already scaled to their own context, so only need to worry about regionals
  # Unless AgencySpendLog, BiddersApplied have differing effects by region? (neither scaled to region)
# Looking in particular for things not already scaled to their region, and measuring broader/vaguer concepts
# GRP, competition make natural sense, maybe TaxCapacityIndex? 
# GRP because transfers, PartyDom b/c obviously; all others already relative to region
final_model_disqualifications_random_agency_random_region <- lmer(Disqualifications ~ AgencySpendLog + ListingDuration + BiddersApplied + SupplierFavoritism + Commonness * Efficiency + GRPConstantPrices + PartyDominance + TaxCapacity + Newspapers + GRPFromMining + BureaucratWages + (1 + Commonness * Efficiency + GRPConstantPrices + PartyDominance|Region), data = agencies_in_regions_simplified)
summary(final_model_disqualifications_random_agency_random_region)
interplot(final_model_disqualifications_random_agency_random_region, var1 = "Commonness", var2 = "Efficiency", hist = T)
# With revisions
final_model_disqualifications_random_agency_random_region_revisions <- lmer(Revisions ~ AgencySpendLog + ListingDuration + BiddersApplied + SupplierFavoritism + Commonness * Efficiency + GRPConstantPrices + PartyDominance + TaxCapacity + Newspapers + GRPFromMining + BureaucratWages + (1 + Commonness * Efficiency + GRPConstantPrices + PartyDominance|Region), data = agencies_in_regions_simplified)
summary(final_model_disqualifications_random_agency_random_region_revisions)
interplot(final_model_disqualifications_random_agency_random_region_revisions, var1 = "Commonness", var2 = "Efficiency")


# Draw graph of last model
agency_region_corruption_mixed_effects_graph <- interplot(final_model_disqualifications_random_agency_random_region, var1 = "Commonness", var2 = "Efficiency", hist = T) +
  theme_bw() +
  scale_fill_tableau() +
  # scale_y_continuous(breaks = c(-2, 0, 2, 4, 6, 8, 10)) +
  labs(title = "Effect of agency-level product commonness on 'disqualifications' corruption proxy\nfor different levels of corruption opportunity, all regions (mixed effects model (4))\n",
       x = "\nAverage corruption opportunities\n(change in price over course of auction; more negative = less opportunity)",
       y = "Expected change in agency-level corruption proxy\nmoving from least common to most common good\n")+
  geom_hline(yintercept = 0, linetype = "dotted")
print(agency_region_corruption_mixed_effects_graph)
ggsave(agency_region_corruption_mixed_effects_graph, file = "./6-Present/chapter-four/agency_region_corruption_mixed_effects_graph.pdf", width = 8.5, height = 8)


# How much variance explained at regional level?
v_c_matrix <- as.data.frame(VarCorr(final_model_disqualifications_random_agency_random_region))
variance_intercept <- filter(v_c_matrix, var1 == "(Intercept)" & is.na(var2)) %>% select(vcov) %>% as.numeric()
variance_all <- filter(v_c_matrix, is.na(var2)) %>% summarize(variance_all = sum(vcov)) %>% as.numeric()
percentage_variance_explained_by_region_intercept <- variance_intercept/variance_all * 100
percentage_variance_explained_by_region_intercept

# And at agency level?
variance_agencies <- filter(v_c_matrix, var1 %in% c("Commonness", "Efficiency", "Commonness:Efficiency") & is.na(var2)) %>% summarize(variance_all = sum(vcov)) %>% as.numeric()
percentage_variance_explained_by_agency <- variance_agencies/variance_all * 100
percentage_variance_explained_by_agency


# Output models
source("6-Present/present-mixed-models.R")


# From baseline to final, substantive `product commonness' effect increases 50%

#######
## Tests not used
#######


# 4.1.X
# Repeat winners, code now in build-purchase-level-data.R; Model this as a red flag
model_favoritism_simple <- lm(PriceChangePercentageNegativeOnly ~ NotificationLotCustomerRequirementMaxPrice + ProductProbabilityLevel4Scaled + FavoritismSimpleLog + (ProductProbabilityLevel4Scaled * FavoritismSimpleLog), data = purchases_moscow)
summary(model_favoritism_simple)
interplot(model_favoritism_simple, var1 = "ProductProbabilityLevel4Scaled", var2 = "FavoritismSimpleLog", esize = 0.5, point = F)
# Shows that market logic wears off with increasing favoritism
# At purchase level, isnt favoritism just increasing in number of other potential suppliers?
# Not quite, because even random allocation would yield that
# Leave out of 4.1, probably better to deploy interacted with specificity later on

# Most common goods purchased by Defense in Moscow (INN: 01731000045)
defense_products_in_moscow <- purchases_all %>%
  filter(TenderPostingRegionEnglish == "Moscow" & AgencyID == "01731000045" & !is.na(NotificationLotProductCode)) %>%
  group_by(NotificationLotProductCode, NotificationLotProductName) %>%
  summarize(NumberOfPurchases = n(), AmountOfPurchases = sum(NotificationLotCustomerRequirementMaxPrice))
write.csv(defense_products_in_moscow, file = "~/Downloads/defense_purchases_in_moscow.csv", row.names = F)
