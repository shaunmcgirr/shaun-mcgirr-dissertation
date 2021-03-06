# 3-Construct\load-other-data.R

# Goals of this script are:
#   - Load and parse the data that comes from sources outside the procurement dataset (other than classifications loaded in load-classifications.R)

###################
# 1. Housekeeping #
###################

# Load functions

# Tell R where unzipped data is stored (at the end of 3-Unpack/unzip-files.R) and where to send it
data_other_directory <- "2-Obtain/data_other/"
# Recall that integrate.R defines a "downloads_directory" for files not tracked by Git

############################################
# 2. Gather parameters about the job ahead #
############################################

# Gather metadata about the data to be loaded
data_other_list <- as.list(c("clearspending-regional-budgets"))
data_other_number <- length(data_other_list)

###############################################
# 3. Load clearspending regional budgets data #
###############################################

# Note, files are semicolon-separated and in UTF-8 format
clearspending_regional_budgets_years <- c("2011", "2012", "2013", "2014", "2015")
clearspending_regional_budgets_filenames <- paste0("clearspending-regional-budgets-",
                                                   clearspending_regional_budgets_years,
                                                   ".csv")

test_import <- read.csv(file = "2-Obtain/data_other/clearspending/clearspending-regional-budgets-2015.csv",
                        sep = ";", encoding = "UTF-8", strip.white = T)
# Need to run over columns and overwrite without spaces 
# http://stackoverflow.com/questions/5992082/how-to-remove-all-whitespace-from-a-string


############################################
# 4. Load Global Corruption Barometer data #
############################################

## Prepackaged survey responses for the intro

gcb_data_aggregated_file <- paste0(data_other_directory, "gcb/GCB2013_DataPack/GCB2013_Data.xls")
  gcb_data_aggregated_columns <- c("Country", "ISO", "Education", "Judiciary", "Medical and health", "Police", "Registry and permit services", "Utilities", "Tax revenue and/or customs", "Land Services", "Any sector")
gcb_data_aggregated_raw <- read_excel(gcb_data_aggregated_file, 
                                      sheet = 8, skip = 5, 
                                      col_names = F)
# Add column names
colnames(gcb_data_aggregated_raw) <- gcb_data_aggregated_columns
# Lop off end of file about contact rates
gcb_data_aggregated_cleaned <- gcb_data_aggregated_raw[1:107,] %>%
                                filter(!is.na(`Any sector`))
rm(gcb_data_aggregated_raw)

## Raw survey data
# gcb_data_file <- paste0(downloads_directory, "GCB data set/GCB full data 2003-2013/2003-2013fullgcbdata.dta")
# gcb_data_raw <- read.dta(gcb_data_file)


###############################################
# 5. Load V-Dem and other cross-national data #
###############################################

# If you have managed to re-save the downloaded STATA file as v12
# v_dem_data_file <- paste0(downloads_directory, 
#                           "/Country_Year_V-Dem_other_STATA_v6.1/V-Dem-DS-CY+Others-v6.1.dta")
# v_dem_data_raw <- read.dta(v_dem_data_file)

# Otherwise use the SPSS file
v_dem_data_file <- paste0(downloads_directory, "/Country_Year_V-Dem_other_SPSS_v6.1/V-Dem-DS-CY+Others-v6.1.sav")
v_dem_data_raw <- read.spss(v_dem_data_file, to.data.frame = T)


#########################
# 6. Load NRPZ rankings #
#########################

# Copied from http://nrpz.ru/raiting_2015.html#content-inner
# Saved in to data_other/nrpz

nrpz_data_file <- paste0(data_other_directory, "/nrpz/National_regional_ratings_from_www.nrpz.ru_2015.xlsx")
nrpz_data_raw <- read_excel(nrpz_data_file, sheet = 1, c("AgencyName", "NRPZscore"))

nrpz_data_cleaned <- nrpz_data_raw %>%
  filter(NRPZscore != "Балл" & AgencyName != "Государственные заказчики федерального уровня") %>%
  mutate(NRPZscore = as.numeric(gsub(" ", "", NRPZscore)))

#########################
# 6. Load ICSID data #
#########################

icsid_raw <- read.xlsx("2-Obtain/data_other/regions/ICSID/ICSID economic database v1.2.1.xlsx", sheet = 1)
region_classification_raw <- icsid_raw %>% select(ISO_id, reg_translit, reg_name) %>% distinct() %>% as.tbl()
  # region_classification_raw$reg_translit_short <- sub(" .*", "", region_classification_raw$reg_translit)
# write.csv(region_classification_raw, file = "2-Obtain/data_other/classifications/regions_in_classification_raw.csv", row.names = F)


# ENDS