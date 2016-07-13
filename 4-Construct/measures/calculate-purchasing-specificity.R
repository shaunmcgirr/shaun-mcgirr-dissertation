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

# Join specificity to efficiency
efficiency_vs_specificity <- agency_total_and_average_spend_per_product_grouped %>%
  inner_join(auction_efficiency_by_agency) %>%
  mutate(TotalSpent = `Total spent`) #%>% filter(MeanAbsoluteDeviationFromAverageSpendPerProduct < 0.2)

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
  
efficiency_vs_specificity_model_2_mean <- lm(`Mean auction efficiency` ~ MeanAbsoluteDeviationFromAverageSpendPerProduct, data = efficiency_vs_specificity)
  summary(efficiency_vs_specificity_model_2_mean)
  
# Add spend to model 1
plot(efficiency_vs_specificity$`Total spent`, efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct)
  cor(efficiency_vs_specificity$`Total spent`, efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct)
  cor(efficiency_vs_specificity$`Median auction efficiency`, efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct)
  cor(efficiency_vs_specificity$`Mean auction efficiency`, efficiency_vs_specificity$SumAbsoluteDeviationFromAverageSpendPerProduct)
# Specificity and spending not highly correlated, can include both in model
  
efficiency_vs_specificity_model_3 <- lm(`Median auction efficiency` ~ SumAbsoluteDeviationFromAverageSpendPerProduct + `Total spent`, data = efficiency_vs_specificity)
  summary(efficiency_vs_specificity_model_3)
  # plot(efficiency_vs_specificity_model_3)
  
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
hist(efficiency_vs_specificity$`Median auction efficiency`, breaks = 40)
hist(efficiency_vs_specificity$`Median auction efficiency (0-1)`, breaks = 40)

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
# Do we want a version of this over time?



#####################
# 3. Store measures #
#####################




# ENDS