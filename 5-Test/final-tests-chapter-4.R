# Final versions of tests for chapter 4


## Set up data
# Relies on running build-purchase-level-data first

### MOSCOW ONLY

# Load Moscow regional file
load("~/data/zakupki/2015-06-13/zakupki-2015-06-13-purchases-data/94fz/regions/Moskva_purchases_2015-06-13.rda")
purchases_moscow <- purchases; rm(purchases);
# load("~/data/zakupki/2015-06-13/zakupki-2015-06-13-purchases-data/94fz/regions/Sankt-Peterburg_purchases_2015-06-13.rda")
# purchases_petersburg <- purchases; rm(purchases)
# load("~/data/zakupki/2015-06-13/zakupki-2015-06-13-purchases-data/94fz/regions/Leningradskaja_obl_purchases_2015-06-13.rda")
# purchases_leningrad <- purchases; rm(purchases)

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
                      labs(title = "Effect of 'bunching' red flag on association between purchase\nspecificity and auction efficiency, Moscow agencies\n",
                           x = "\n0 = Not listed within 5% of threshold; 1 = Listed within 5% of threshold",
                           y = "Expected change in price decrease over course of auction\nwhen moving from most specific to most generic good\n")
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
  labs(title = "Relationship between purchase specificity and\nnumber of unique suppliers, Moscow agencies\n",
       x = "\nProbability a purchase is of a given product\n(higher value = more generic product)",
       y = "Number of (unique) suppliers")
print(suppliers_per_product_graph)
ggsave(suppliers_per_product_graph, file = "./6-Present/chapter-four/suppliers_per_product_moscow.pdf", width = 5, height = 6)

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
  labs(title = "Effect of 'mid-procedure revisions' red flag on association between\npurchase specificity and auction efficiency, Moscow agencies\n",
       x = "\nTotal number of revisions to purchase",
       y = "Expected change in price decrease over course of auction\nwhen moving from most specific to most generic good\n")
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
  labs(title = "Effect of 'disqualifications' red flag on association between\npurchase specificity and auction efficiency, Moscow agencies\n",
       x = "\nProportion of bidders disqualified from auction",
       y = "Expected change in price decrease over course of auction\nwhen moving from most specific to most generic good\n") +
  scale_y_continuous(breaks = c(-2, 0, 2, 4, 6, 8, 10))
print(disqualifications_graph)
ggsave(disqualifications_graph, file = "./6-Present/chapter-four/disqualifications_graph_moscow.pdf")


# 4.1.X
# Repeat winners, code now in build-purchase-level-data.R; Model this as a red flag
model_favoritism_simple <- lm(PriceChangePercentageNegativeOnly ~ NotificationLotCustomerRequirementMaxPrice + ProductProbabilityLevel4Scaled + FavoritismSimpleLog + (ProductProbabilityLevel4Scaled * FavoritismSimpleLog), data = purchases_moscow)
summary(model_favoritism_simple)
interplot(model_favoritism_simple, var1 = "ProductProbabilityLevel4Scaled", var2 = "FavoritismSimpleLog", esize = 0.5, point = F)
# Shows that market logic wears off with increasing favoritism
# At purchase level, isn't favoritism just increasing in number of other potential suppliers?
# Not quite, because even random allocation would yield that
# Leave out of 4.1, probably better to deploy interacted with specificity later on


########
## 4.2 #
########

# Are procedures differently susceptible to corruption?
model_procedure_disqualifications <- lm(ProportionDisqualified ~ TenderProcedureGroup - 1, data = purchases_moscow)
summary(model_procedure_disqualifications)
model_procedure_revisions <- lm(TotalRevisions ~ TenderProcedureGroup - 1, data = purchases_moscow)
summary(model_procedure_revisions)
model_procedure_bunching <- lm(ProximityRuleThreshold ~ TenderProcedureGroup - 1, data = purchases_moscow)
summary(model_procedure_bunching)
# Olympic construction most highly correlated, as expected, then the rest are pretty close really

model_rf_disqualifications <- lm(ProportionDisqualified ~ ProductProbabilityLevel4Scaled + NotificationLotCustomerRequirementMaxPrice + (ProductProbabilityLevel4Scaled * NotificationLotCustomerRequirementMaxPrice), data = purchases_moscow)
summary(model_rf_disqualifications)
interplot(model_rf_disqualifications, var1 = "ProductProbabilityLevel4Scaled", var2 = "NotificationLotCustomerRequirementMaxPrice")
# As you buy more expensive things, more generic goods more assoc with increase in corruption (go after big fish)


model_rf_revisions <- lm(TotalRevisions ~ ProductProbabilityLevel4Scaled + NotificationLotCustomerRequirementMaxPrice + (ProductProbabilityLevel4Scaled * NotificationLotCustomerRequirementMaxPrice), data = purchases_moscow)
summary(model_rf_revisions)
interplot(model_rf_revisions, var1 = "ProductProbabilityLevel4Scaled", var2 = "NotificationLotCustomerRequirementMaxPrice")
# Same goes for revisions

model_rf_bunching <- lm(ProximityRuleThreshold ~ ProductProbabilityLevel4Scaled + NotificationLotCustomerRequirementMaxPrice + (ProductProbabilityLevel4Scaled * NotificationLotCustomerRequirementMaxPrice), data = purchases_moscow)
summary(model_rf_bunching)
interplot(model_rf_bunching, var1 = "ProductProbabilityLevel4Scaled", var2 = "NotificationLotCustomerRequirementMaxPrice")
# Market logic dominates more (OK as this is hokey proximity measure)
# At very least, the more expensive

### ALL REGIONS

# load("~/data/zakupki/2015-06-13/zakupki-2015-06-13-purchases-data/all_purchases_2015-06-13_compress.rda")

# Single-supplier
single_supplier_usage <- purchases_all %>%
  group_by(AgencyID, Match) %>%
  count(Match) %>%
  mutate(MatchProportion = prop.table(n)) %>% ungroup() %>%
  filter(Match == "Contract without notification")
hist(single_supplier_usage$MatchProportion, breaks = 20)
plot(density(single_supplier_usage$MatchProportion))
worst_offenders <- single_supplier_usage %>% filter(MatchProportion >= 0.8)


# What is the story behind this? Do I need it any longer?
test_model_14_corr <- lm(ProximityRuleThreshold ~ NotificationLotCustomerRequirementMaxPrice + ProductProbabilityLevel4Scaled + PriceChangePercentageNegativeOnly + (ProductProbabilityLevel4Scaled * PriceChangePercentageNegativeOnly), data = purchases_all)
summary(test_model_14_corr)
interplot(test_model_14_corr, var1 = "ProductProbabilityLevel4Scaled", var2 = "PriceChangePercentageNegativeOnly")

