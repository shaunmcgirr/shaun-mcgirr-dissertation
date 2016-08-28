# Final versions of tests for chapter 4


## Set up data
# Relies on running build-purchase-level-data first


# Are auctions with red flags different? Yes
test_model_6 <- lm(PriceChangePercentageNegativeOnly ~ NotificationLotCustomerRequirementMaxPrice + (ProductProbabilityLevel4 * AnyBunching), data = purchases)
summary(test_model_6)
interplot(test_model_6, var1 = "ProductProbabilityLevel4", var2 = "AnyBunching")
# At greater level of detail, purchases without bunching obey market mech, those with bunching do not


