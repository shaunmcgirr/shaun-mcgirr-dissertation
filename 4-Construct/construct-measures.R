# 4-Construct\construct-measures.R

# Goals of this script are:
#   - Obtain list of regions which have completed matching from pivot-and-merge.R
#   - Set up various data frames, then call the scripts that construct individual measures
#   - Store the resulting measures somewhere

###################
# 1. Housekeeping #
###################

# Load functions
source(file="3-Unpack/parse-files-functions.R")
source(file="4-Construct/construct-measures-functions.R")

# Load classifications
source(file="3-Unpack/load-classifications.R")

############################################
# 2. Gather parameters about the job ahead #
############################################

# Obtain list of regions for which consolidated data is available
# regions_list <- generate_regions_list(data_parsed_directory)
regions_list <- as.list("Adygeja_Resp")
# regions_list <- as.list("Moskva")
regions_number <- length(regions_list)

# Define where outputs (eg graphs) should go
data_output_directory <- set_data_subdirectory(data_directory, data_download_date, "output")

#################################################
# 3. Load data, check quality, recode variables #
#################################################

# Loop over regions, processing them in turn
for(r in 1:regions_number){
  # r <- 1
  current_region <- as.character(regions_list[r])
  current_region_english <- generate_english_region_name(current_region)
  data_output_directory_region <- paste0(data_output_directory, current_region, "/")
  
    # Load the file of matches
    file_to_process <- paste0(data_output_directory, current_region, "/",
                              current_region, "_notification_contract_matches_",
                              data_download_date, ".rda")
    load(file=file_to_process)
  
  ## REMOVE matches of low quality
  # Function to count na in columns of data frame
  na_count <- function(x) sapply(x, function(y) sum(is.na(y)))
  # na_count(notification_contract_matches)

  # Turns out the notifications with a matching contract, but where many contract details NA, are interesting
  # eg http://zakupki.gov.ru/pgz/public/action/orders/info/common_info/show?notificationId=7443504 (contract not linked)
  # eg http://zakupki.gov.ru/pgz/public/action/orders/info/common_info/show?notificationId=7598466 (contract 'there' but not)
  # Perhaps these should be measures? Provisionally, treat any final data quality issues as potential measures

  ## DELETE VARIABLES THAT NEVER VARY
  notification_contract_matches$NotificationLotOrdinalNumber <- NULL # Always = 1
  notification_contract_matches$ContractFoundationSingleCustomer <- NULL # Always empty

  ## RECODE VARIABLES
  # Notifications
  notification_contract_matches$NotificationLotCustomerRequirementMaxPrice <- as.numeric(notification_contract_matches$NotificationLotCustomerRequirementMaxPrice)
    # hist(notification_contract_matches$NotificationLotCustomerRequirementMaxPrice)
  notification_contract_matches$NotificationPlacingWayName <- as.factor(notification_contract_matches$NotificationPlacingWayName)
    NotificationPlacingWayName_table <- as.data.frame(table(notification_contract_matches$NotificationPlacingWayName))
  notification_contract_matches$NotificationPublishDate <- substr(notification_contract_matches$NotificationPublishDate, 1, 10)
    # Draw histogram by date
  notification_contract_matches$NotificationVersionNumber <- as.numeric(notification_contract_matches$NotificationVersionNumber)
    NotificationVersionNumber_table <- as.data.frame(table(notification_contract_matches$NotificationVersionNumber))

  # Contracts
  notification_contract_matches$ContractCurrentContractStage <- as.factor(notification_contract_matches$ContractCurrentContractStage)
    ContractCurrentContractStage_table <- as.data.frame(table(notification_contract_matches$ContractCurrentContractStage))
  notification_contract_matches$ContractPrice <- as.numeric(notification_contract_matches$ContractPrice)
    # hist(notification_contract_matches$ContractPrice)
  notification_contract_matches$ContractVersionNumber <- as.numeric(notification_contract_matches$ContractVersionNumber)
    ContractVersionNumber_table <- as.data.frame(table(notification_contract_matches$ContractVersionNumber))
  
  ## NEW VARIABLES
  # Add region this tender came from
  notification_contract_matches$TenderPostingRegion <- current_region
  notification_contract_matches$TenderPostingRegionEnglish <- current_region_english
    
  # Price difference between notification max price and final contract price (and percentage change, and cleaned version)
  notification_contract_matches$PriceChange <- -1 * (notification_contract_matches$NotificationLotCustomerRequirementMaxPrice - notification_contract_matches$ContractPrice)
    # hist(notification_contract_matches$PriceChange, breaks = 100)  
  notification_contract_matches$PriceChangePercentage <- notification_contract_matches$PriceChange/notification_contract_matches$NotificationLotCustomerRequirementMaxPrice * 100
    # summary(notification_contract_matches$PriceChangePercentage)
    # hist(notification_contract_matches$PriceChangePercentage, breaks = 100)
    # price_change_percentage_no_outliers <- notification_contract_matches %>%
    #                                         filter(PriceChangePercentage <= 110 & PriceChangePercentage > 0) %>%
    #                                         select(PriceChangePercentage)
    # hist(price_change_percentage_no_outliers$PriceChangePercentage, breaks = 100)
  notification_contract_matches$PriceChangePercentageNoOutliers <- ifelse(notification_contract_matches$PriceChangePercentage <= 100,
                                                                          notification_contract_matches$PriceChangePercentage, NA)
    # hist(notification_contract_matches$PriceChangePercentageNoOutliers, breaks = 100)
  
  # Product code (higher level of classification)
  notification_contract_matches$NotificationLotProductCodeLevel1 <- paste0(substr(notification_contract_matches$NotificationLotProductCode, 1, 2), "00000")
    table(notification_contract_matches$NotificationLotProductCodeLevel1)
                                                                           
  # Procedure group (user friendly)
  notification_contract_matches$TenderProcedureGroup <- NA
  notification_contract_matches$TenderProcedureGroup[notification_contract_matches$NotificationPlacingWayName == "Заказ на выполнение проектных, изыскательских работ, работ по строительству, реконструкции, капитальному ремонту объектов капитального строительства, относящихся в соответствии с Федеральным законом от 1 декабря 2007 года № 310-ФЗ к олимпийским объектам федерального значения, олимпийским объектам краевого значения или олимпийским объектам муниципального значения (ч.34 ст.65 Федерального закона №94-ФЗ), размещаемый путем проведения запроса котировок"] <- "Olympic construction"
  notification_contract_matches$TenderProcedureGroup[notification_contract_matches$NotificationPlacingWayName == "Открытый конкурс"] <- "Open tender"
    notification_contract_matches$TenderProcedureGroup[notification_contract_matches$NotificationPlacingWayName == "Открытый конкурс (научно-исследовательская, опытно-конструкторская или технологическая работа)"] <- "Open tender"
    notification_contract_matches$TenderProcedureGroup[notification_contract_matches$NotificationPlacingWayName == "Открытый конкурс (размещение заказа на поставку технических средств реабилитации инвалидов, оказание услуг в сфере образования, услуг по санаторно-курортному лечению)"] <- "Open tender"
    notification_contract_matches$TenderProcedureGroup[notification_contract_matches$NotificationPlacingWayName == "Открытый конкурс (создание произведения литературы или искусства, исполнения)"] <- "Open tender"
    notification_contract_matches$TenderProcedureGroup[notification_contract_matches$NotificationPlacingWayName == "Открытый конкурс (финансирование проката или показа национального фильма)"] <- "Open tender"
    notification_contract_matches$TenderProcedureGroup[notification_contract_matches$NotificationPlacingWayName == "Открытый конкурс на размещение заказа на энергосервис для нужд заказчиков (гл. 7.1 Федерального закона №94-ФЗ)"] <- "Open tender"
  notification_contract_matches$TenderProcedureGroup[notification_contract_matches$NotificationPlacingWayName == "Открытый аукцион в электронной форме"] <- "Open electronic auction"
    notification_contract_matches$TenderProcedureGroup[notification_contract_matches$NotificationPlacingWayName == "Открытый аукцион в электронной форме на размещение заказа на энергосервис для нужд заказчиков (гл. 7.1 Федерального закона №94-ФЗ)"] <- "Open electronic auction"
  notification_contract_matches$TenderProcedureGroup[notification_contract_matches$NotificationPlacingWayName == "Запрос котировок"] <- "Request for quotes"   
    notification_contract_matches$TenderProcedureGroup[notification_contract_matches$NotificationPlacingWayName == " Размещение заказа путем запроса котировок цен товаров, работ, услуг, соответственно производство, выполнение, оказание которых осуществляется не по конкретным заявкам заказчика и для которых есть функционирующий рынок, для обеспечения своей деятельности на территории иностранного государства, на которой находится заказчик, у иностранных поставщиков (исполнителей, подрядчиков) (ч. 5 ст. 42 Федерального закона №94-ФЗ) "] <- "Request for quotes"   
    notification_contract_matches$TenderProcedureGroup[notification_contract_matches$NotificationPlacingWayName == "Размещение заказа путем запроса котировок на энергосервис для нужд заказчиков (гл. 7.1 Федерального закона №94-ФЗ)"] <- "Request for quotes"   
    notification_contract_matches$TenderProcedureGroup[notification_contract_matches$NotificationPlacingWayName == "Размещение заказа путем проведения запроса котировок на поставку продовольствия, средств, необходимых для оказания скорой или неотложной медицинской помощи, лекарственных средств, топлива, которые необходимы для нормального жизнеобеспечения граждан и отсутствие которых приведет к нарушению их нормального жизнеобеспечения в случаях, указанных в ч. 6 ст. 42 Федерального закона №94-ФЗ "] <- "Request for quotes"   
  # table(notification_contract_matches$NotificationPlacingWayName, useNA = "always")
  # table(notification_contract_matches$TenderProcedureGroup, useNA = "always")
    # hist(notification_contract_matches$PriceChangePercentageNoOutliers[notification_contract_matches$TenderProcedureGroup == "Open electronic auction"], breaks = 100, col=rgb(0, 0, 1, 0.5))  
    # hist(notification_contract_matches$PriceChangePercentageNoOutliers[notification_contract_matches$TenderProcedureGroup == "Request for quotes"], breaks = 100, col=rgb(1, 0, 0, 0.5), add=T)  
  stopifnot(!anyNA(notification_contract_matches$TenderProcedureGroup))
  notification_contract_matches$TenderProcedureGroup <- factor(notification_contract_matches$TenderProcedureGroup, 
                                                               levels = c("Open electronic auction", "Open tender", "Request for quotes", "Olympic construction"),
                                                               ordered = F)

  # Procedure level of discretion
  notification_contract_matches$TenderProcedureDiscretion <- NA
  notification_contract_matches$TenderProcedureDiscretion[notification_contract_matches$TenderProcedureGroup == "Open tender"] <- "Medium discretion"
  notification_contract_matches$TenderProcedureDiscretion[notification_contract_matches$TenderProcedureGroup == "Open electronic auction"] <- "Lower discretion"
  notification_contract_matches$TenderProcedureDiscretion[notification_contract_matches$TenderProcedureGroup == "Request for quotes"] <- "Higher discretion"
  notification_contract_matches$TenderProcedureDiscretion[notification_contract_matches$TenderProcedureGroup == "Olympic construction"] <- "Other"
      # table(notification_contract_matches$TenderProcedureDiscretion, useNA = "always")
  notification_contract_matches$TenderProcedureDiscretion <- factor(notification_contract_matches$TenderProcedureDiscretion, 
                                                                    levels = c("Lower discretion", "Medium discretion", "Higher discretion", "Other"), 
                                                                    ordered = T)
    # table(notification_contract_matches$TenderProcedureDiscretion, useNA = "always")
  
  # Notification revised (binary)
  
  # Contract revised (binary)
  
  # Current contract stage (user friendly)
  
###########
  
  ## TODO

    # MEASURES OF CORRUPTION
    # Initial notification listing price (by tender type) - distribution; by agency
    # Percentage change between notification and contract price (by tender type) - distribution; by agency
    # Number of revisions to notification/contract - by agency
    # Shares of each tender type (eg single supplier) - by agency
    # Notification product code != Contract product code - by agency
    # Number of revisions to notifications and contracts (proxy for capacity or corruption?) - by agency
    # Proportion of contracts near 10%/100% over the maximum notified price - by agency
    # Applicants/bidders per notification, share of contracts with single bidder - by agency
    # Dramatic price drop from notification to contract - indicates risk of 'cheap analogue' being supplied
    #   - a valid measure because FL-44 prevented it in cases more than 15% without contract explaining why
    #   - concrete case https://www.hse.ru/pubs/share/direct/document/64729394

    # MEASURES OF AGENCY CHARACTERISTICS
    # Number of unique products (lowest and highest levels of OKDP) - by agency
    # Customer != placer (proxy for capacity) - by agency
    # •	Specificity of goods purchased by agency, defined as the inverse of the rarity with which a given product is purchased across all other agencies (the database includes detailed product codings that make such a measure possible: the score will be highest when only the given agency purchases this product, and lowest when every other agency also purchases it)
    # •	The Audit Chamber, the main oversight agency of the Executive, releases yearly reports on its activities, including the number of inquiries initiated in to each agency, and the `yield' of these inquiries in terms of cases forwarded to prosecutors. I am planning to use this data to proxy for constraints on agencies.
    # •	Corruption revelations from press reports, in which I would tag the agency mentioned. Exactly what this data would be measuring depends on how the formal model works out.
 
  
    # CONTROLS
    # Total spent by agency
    # Time elapsed between notification and contract
    # Procedure type (when modelling reduction of price, eg auctions might incorportate more pre-market info
    #   in this regard, need two-stage model: first what determines initial price)
  
  # National official rating http://nrpz.ru/
  #   Criteria
  # Таб. 5. Основные показатели рынка закупок государственных закупщиков *
  #   - Средняя начальная цена конкурентного способа определения поставщиков, млн. руб.
  # - Среднее число поданных заявок
  # - Доля отклоненных заявок
  # - Среднее число участников закупочных процедур (или допущенных котировочных заявок)
  # - Доля закупочных процедур с 0 или 1 участником
  
  # •	Procedure choice
  #   o	Proportion of tenders in which the agency chooses a single supplier instead of any kind of competitive procedure (well-demonstrated in literature on public procurement to be correlated with procurement fraud)
  #   o	Correlation between proc choice and purchase size
  # •	Bidding environment
  #   o	Number of bidders
  #   o	Number of disqualifications
  # •	Procedure efficiency (start vs end)
  #   o	By procedure type
  #   o	Mean, median, and a measure of “crowding” in to space just below 100%
  #   o	Average increase in contract cost over the lifetime of the procedure (this detects variation in cost changes, another standard place to hide kickbacks)
  #   o Average price decrease in competitive auctions (bureaucrats choose a maximum/starting price, and potential suppliers bid down; procurement literature identifies this as a good measure for the tolerance of cartel behavior by suppliers)
  #   o	Increases as irregularities
  # •	Irregularities
  #   o	Start prices right at thresholds
  
 
  
  
  ############################################################
  #  4. Save the dataframes containing measures to new files #
  ############################################################

  
    
} # Closes control loop over regions_list

# ENDS