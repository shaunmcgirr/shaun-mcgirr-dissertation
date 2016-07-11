# 4-Construct\measures\prices-for-specific-products.R

###################
# 1. Prepare data #
###################


########################
# 2. Calculate measure #
########################

# First add a variable to main data frame (if relevant)


# Now aggregate by agency


# Do we want a version of this over time?


# Scatterplot of petrol prices over time
petrol_prices <- notifications_contracts_products_ungrouped %>%
  filter(ContractProductOKDPCode == "2320212" & ContractProductOKEICode == "112" & ContractProductPrice < 500 & ContractProductPrice > 1) %>%
  transmute(Date = (paste(substr(ContractPublishDate, 1, 4), substr(ContractPublishDate, 6, 7), sep = ".")),
            `Price per liter` = ContractProductPrice,
            `Number of liters` = ContractProductQuantity,
            `Total purchase` = (ContractProductSum),
            `Total purchase (log)` = log10(ContractProductSum),
            Procedure = ifelse(NotificationPlacingWayName == "Открытый аукцион в электронной форме", "Less discretionary", 
                               ifelse(NotificationPlacingWayName == "Запрос котировок", "More discretionary", "Unknown")),
            Agency = (ContractCustomerFullName),
            INN = as.factor(as.character(ContractCustomerINN)),
            Purchaser = NotificationOrderPlacerRegNum)

agency_per_INN <- petrol_prices %>%
  group_by(INN) %>%
  summarize(AgenciesPerINN = n_distinct(Agency))

agency_INN_correspondence <- petrol_prices %>%
  mutate(LengthOfAgencyName = nchar(Agency)) %>%
  group_by(INN, Agency) %>%
  summarize(ShortestAgencyName = min(LengthOfAgencyName)) %>%
  filter(row_number() == 1) %>% ungroup() %>%
  transmute(INN = INN, `Agency (definitive)` = Agency)

petrol_prices <- petrol_prices %>%
  left_join(agency_INN_correspondence, by = "INN")

petrol_prices_by_agency <- petrol_prices %>%
  group_by(INN, `Agency (definitive)`) %>%
  summarize(`Average price per liter` = mean(`Price per liter`),
            `Median price per liter` = median(`Price per liter`)) %>% ungroup() %>%
  arrange(`Average price per liter`) 

petrol_prices_by_agency$`Agency (definitive)` <- factor(petrol_prices_by_agency$`Agency (definitive)`, levels = petrol_prices_by_agency$`Agency (definitive)`[order(petrol_prices_by_agency$`Average price per liter`)])
petrol_prices$`Agency (definitive)` <- factor(petrol_prices$`Agency (definitive)`, levels = petrol_prices_by_agency$`Agency (definitive)`)
petrol_prices$Procedure <- factor(petrol_prices$Procedure, levels = c("More discretionary", "Less discretionary"))

petrol_prices <- petrol_prices %>%
  left_join(petrol_prices_by_agency)

# First show petrol prices don't change much over time
# Might want to de-trend with official price data https://yandex.ru/search/site/?searchid=2252602&text=%D0%B1%D0%B5%D0%BD%D0%B7%D0%B8%D0%BD&web=0&l10n=ru
graph_title <- paste0("Price per liter paid for gasoline, by month in ", current_region_english, "\n")
graph_file_name <- paste0(data_output_directory_region, current_region, "_petrol_price_per_liter_by_month.pdf")
petrol_prices_by_month_graph <- petrol_prices %>%
  ggplot(aes(x = Date, y = `Price per liter`, colour = Procedure)) +
  geom_point() +
  theme(axis.text.x = element_text(angle=60, vjust=0.5, size=8)) +
  labs(title = graph_title, x = "\nMonth", y = "Price per liter (rubles)") 
print(petrol_prices_by_month_graph)
ggsave(plot = petrol_prices_by_month_graph, filename = graph_file_name, device = "pdf")

# The best one: petrol prices
graph_title <- paste0("Price per liter paid for gasoline, by agencies in ", current_region_english, "\n")
graph_file_name <- paste0(data_output_directory_region, current_region, "_petrol_price_per_liter_by_agency.pdf")
petrol_prices_by_agency_graph <- petrol_prices %>%
  ggplot(aes(x = `Agency (definitive)`, y = `Price per liter`, colour = Procedure)) +
  geom_point() +
  theme(axis.ticks = element_blank(), axis.text.x  = element_blank()) +
  labs(title = graph_title, x = "\nAgencies arranged by average price per liter", y = "Price per liter (rubles)") +
  theme(legend.justification=c(0.05,0.95), legend.position=c(0.05,0.95))
print(petrol_prices_by_agency_graph)
ggsave(plot = petrol_prices_by_agency_graph, filename = graph_file_name, device = "pdf")

petrol_prices_by_procedure <- petrol_prices %>%
  ggplot(aes(x = `Total purchase`, y = `Price per liter`, colour = Procedure)) +
  geom_point()
# + geom_smooth(method = "lm", se = F)
print(petrol_prices_by_procedure)

products_per_agency_ungrouped <- notifications_contracts_products_ungrouped %>%
  group_by(ContractCustomerINN) %>%
  summarize(NumberOfProductsPurchasedUngrouped = n_distinct(ContractProductOKDPCode))

products_per_agency_grouped <- notifications_contracts_products_grouped %>%
  group_by(ContractCustomerINN) %>%
  summarize(NumberOfProductsPurchasedGrouped = n_distinct(ContractProductCodeLevel1))

petrol_price_vs_products_purchased <- petrol_prices_by_agency %>%
  left_join(products_per_agency_ungrouped, by = c("INN" = "ContractCustomerINN")) %>%
  left_join(products_per_agency_grouped, by = c("INN" = "ContractCustomerINN"))

testing_1 <- lm(`Average price per liter` ~ NumberOfProductsPurchasedUngrouped, data = petrol_price_vs_products_purchased)
summary(testing_1)
testing_2 <- lm(`Average price per liter` ~ NumberOfProductsPurchasedGrouped, data = petrol_price_vs_products_purchased)
summary(testing_2)
testing_3 <- lm(`Average price per liter` ~ log10(NumberOfProductsPurchasedUngrouped), data = petrol_price_vs_products_purchased)
summary(testing_3)

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

agency_INN_correspondence <- notifications_contracts_products_ungrouped %>%
  filter(!is.na(ContractCustomerFullName) & !is.na(ContractCustomerINN)) %>%
  transmute(Agency = ContractCustomerFullName,
            INN = ContractCustomerINN) %>%
  mutate(LengthOfAgencyName = nchar(Agency)) %>%
  group_by(INN, Agency) %>%
  summarize(ShortestAgencyName = min(LengthOfAgencyName)) %>%
  arrange(ShortestAgencyName) %>%
  filter(row_number() == 1) %>% ungroup() %>%
  transmute(INN = INN, `Agency (definitive)` = Agency)

agency_total_and_average_spend_per_product_grouped <- notifications_contracts_products_grouped %>%
  filter(!is.na(ContractProductCodeLevel1) & !is.na(ContractPrice)) %>%
  group_by(ContractCustomerINN) %>%
  mutate(AgencyTotalSpend =  sum(as.numeric(ContractPrice))) %>% ungroup() %>%
  group_by(ContractCustomerINN, ContractProductCodeLevel1, AgencyTotalSpend) %>%
  summarize(AgencyTotalSpendPerProductGrouped = sum(as.numeric(ContractPrice)),
            AgencyMeanSpendPerProductGrouped = mean(as.numeric(ContractPrice)),
            AgencyMedianSpendPerProductGrouped = median(as.numeric(ContractPrice))) %>%
  mutate(ProportionOfAgencySpend = AgencyTotalSpendPerProductGrouped / AgencyTotalSpend) %>%
  left_join(total_and_average_spend_per_product_grouped) %>%
  mutate(DeviationFromAverageSpend = ProportionOfTotalSpend - ProportionOfAgencySpend) %>%
  group_by(ContractCustomerINN) %>%
  summarize(MeanAbsoluteDeviationFromAverageSpendPerProduct = mean(abs(DeviationFromAverageSpend)),
            SumAbsoluteDeviationFromAverageSpendPerProduct = sum(abs(DeviationFromAverageSpend))) %>%
  left_join(agency_INN_correspondence, by = c("ContractCustomerINN" = "INN"))

petrol_and_difference <- petrol_prices_by_agency %>%
  inner_join(agency_total_and_average_spend_per_product_grouped)

plot(petrol_and_difference$SumAbsoluteDeviationFromAverageSpendPerProduct, petrol_and_difference$`Average price per liter`)


landscaping <- notifications_contracts_products_ungrouped %>%
  filter(ContractProductOKDPCode == "4560227" & ContractProductOKEICode == "055" & ContractProductQuantity > 5) %>%
  transmute(Date = (paste(substr(ContractPublishDate, 1, 4), substr(ContractPublishDate, 6, 7), sep = ".")),
            `Price per square meter (log)` = log10(ContractProductPrice),
            `Number of square meters` = ContractProductQuantity)

landscaping_graph <- landscaping %>%
  ggplot(aes(x = Date, y = `Price per square meter (log)`)) +
  geom_point()
print(landscaping_graph)

car_rental <- notifications_contracts_products_ungrouped %>%
  filter(ContractProductOKDPCode == "6022020" & ContractProductOKEICode == "356" & ContractProductQuantity > 1) %>%
  transmute(Date = (paste(substr(ContractPublishDate, 1, 4), substr(ContractPublishDate, 6, 7), sep = ".")),
            `Price per hour (log)` = log10(ContractProductPrice),
            `Number of square meters` = ContractProductQuantity)

car_rental_graph <- car_rental %>%
  ggplot(aes(x = Date, y = `Price per hour (log)`)) +
  geom_point()
print(car_rental_graph)

# Most common products
product_frequency <- notifications_contracts_products_ungrouped %>%
  group_by(ContractProductOKDPCode, ContractProductOKDPName, ContractProductOKEICode, ContractProductOKEIName) %>%
  summarize(PurchasesPerProduct = n())

# Number of agencies per product/unit combo
agencies_per_product_unit <- notifications_contracts_products_ungrouped %>%
  group_by(ContractProductOKDPCode, ContractProductOKDPName, ContractProductOKEICode, ContractProductOKEIName) %>%
  summarize(AgenciesPerProductUnit = n_distinct(ContractCustomerINN))

# In general, time/size/weight measures are less discretionary than "piece" etc
# So some kind of "amount of budget spent on discretionary units"?

# Correlate procedure choice with gas price paid


# 4560227 Благоустройство и озеленение Landscaping and gardening - how much variation reasonable per sq m?
# 6022020 Услуги по сдаче в аренду легковых автомобилей с водителями - rental hourly rates vs "piece"
# 2320212 Бензины автомобильные - Литр; Кубический дециметр (but multiple grades so only the "exceptional" count)
# Actually, prices quite consistent across grades (and diesel) http://www.gks.ru/bgd/free/B04_03/IssWWW.exe/Stg/d06/17.htm
# Seems to be discontinuity between high 30s and low 50s, no clear time trend
# 3311415 Перчатки хирургические Пара (2 шт.) vs 1 - gloves? 
# 7492060 Услуги охранников - per person/hour or as "package"

# 2930274 Кондиционеры бытовые, электровоздухоохладители
# 2101511 Бумага для копировально - множительной техники





#####################
# 3. Store measures #
#####################




# ENDS