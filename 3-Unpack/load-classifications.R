# 3-Construct\load-classifications.R

# Goals of this script are:
#   - Load and parse the official statistical classifications 
#   - Good example of what we might want to achieve with this:
#     - http://zakupki.gov.ru/pgz/public/action/orders/info/common_info/show?notificationId=6070886
#     - The classification of goods, works and services	
#       221 for the construction works, reconstruction, repair of capital construction of public roads, temporary buildings, stalls, sheds and other similar structures <*>
#         4540214 Filling of barge board doorways
#         4540256 Arrangement of parquet flooring
#         4540301 Pasting of walls, ceilings with wall
#     - We would like the ability to aggregate the individual purchases in this tender to code 4540000 so it 
#       'makes the cut' in 4-Construct/pivot-and-merge.R where we must discard multi-product notifications

###################
# 1. Housekeeping #
###################

# Load functions

# Tell R where unzipped data is stored (at the end of 3-Unpack/unzip-files.R) and where to send it
data_classifications_directory <- "2-Obtain/data_other/classifications"
# Recall that integrate.R defines a "downloads_directory" for files not tracked by Git

############################################
# 2. Gather parameters about the job ahead #
############################################

# Gather metadata about the data to be loaded
# data_other_list <- as.list(c("clearspending-regional-budgets"))
# data_other_number <- length(data_other_list)

############################################
# 3. Load ОКПД/OKDP product classification #
############################################

# Browseable here http://classifikators.ru/okdp
# Download instructions in 2-Obtain/download-other-data.md

okdp_product_classification_raw <- read.csv(file = "2-Obtain/data_other/classifications/ОКДП.csv",
                                        header = T, stringsAsFactors = F, encoding = "utf8",
                                        colClasses = "character") %>%
                                rename(ProductCode = Код, ProductName = Наименование.группировки) %>%
                                filter(ProductCode != "") %>%
                                mutate(ProductCodeLevel1 = paste0(substr(ProductCode, 1, 2), "00000"),
                                       ProductCodeLevel2 = paste0(substr(ProductCode, 1, 3), "0000"),
                                       ProductCodeLevel3 = paste0(substr(ProductCode, 1, 4), "000"))

# CODE BELOW SETS UP FULL BRIDGING TABLE
okdp_product_classification_level_1 <- okdp_product_classification_raw %>%
                                        filter(ProductCode == ProductCodeLevel1) %>%
                                        transmute(ProductCodeLevel1 = ProductCodeLevel1,
                                                  ProductNameLevel1 = ProductName)

okdp_product_classification_level_2 <- okdp_product_classification_raw %>%
                                        filter(ProductCode == ProductCodeLevel2) %>%
                                        transmute(ProductCodeLevel2 = ProductCodeLevel2,
                                                  ProductNameLevel2 = ProductName) %>%
                                        anti_join(okdp_product_classification_level_1,
                                                  by = c("ProductCodeLevel2" = "ProductCodeLevel1")) %>%
                                        arrange(ProductCodeLevel2)

okdp_product_classification_level_3 <- okdp_product_classification_raw %>%
                                        filter(ProductCode == ProductCodeLevel3) %>%
                                        transmute(ProductCodeLevel3 = ProductCodeLevel3,
                                                  ProductNameLevel3 = ProductName) %>%
                                        anti_join(okdp_product_classification_level_1,
                                                  by = c("ProductCodeLevel3" = "ProductCodeLevel1")) %>%
                                        anti_join(okdp_product_classification_level_2,
                                                  by = c("ProductCodeLevel3" = "ProductCodeLevel2"))
                                       
okdp_product_classification <- okdp_product_classification_raw %>%
                                left_join(okdp_product_classification_level_1,
                                          by = c("ProductCodeLevel1")) %>%
                                left_join(okdp_product_classification_level_2,
                                          by = c("ProductCodeLevel2")) %>%
                                left_join(okdp_product_classification_level_3,
                                          by = c("ProductCodeLevel3"))

okdp_product_classification_level_4 <- okdp_product_classification %>%
                                        filter(ProductCode != ProductCodeLevel1 & ProductCode != ProductCodeLevel2 & ProductCode != ProductCodeLevel3)

rm(okdp_product_classification_raw)
rm(okdp_product_classification_level_4); rm(okdp_product_classification_level_3); rm(okdp_product_classification_level_2); rm(okdp_product_classification_level_1)

# ENDS