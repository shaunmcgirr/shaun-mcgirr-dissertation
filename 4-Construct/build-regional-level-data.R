# 4-Construct\build-regional-level-data.R

# Goals of this script are:
#   - Obtain list of regions which have completed purchase-level data build
#   - Load their agency-level measures
#   - Pull in other regional data from ICSID loaded in load-other-data.R

###################
# 1. Housekeeping #
###################

# Load functions
source(file="3-Unpack/parse-files-functions.R")
source(file="4-Construct/construct-measures-functions.R")

# Helpers for aggregation
mean_na <- function(x){mean(x, na.rm = T)}
median_na <- function(x){median(x, na.rm = T)}

# Load classifications
source(file="3-Unpack/load-classifications.R")

############################################
# 2. Gather parameters about the job ahead #
############################################

# Where does data come in from?
data_output_directory <- set_data_subdirectory(data_directory, data_download_date, "output")
data_purchases_directory <- set_data_subdirectory(data_directory, data_download_date, "purchases")
data_purchases_directory_regions <- paste0(data_purchases_directory, "regions/")

# Define where outputs should go
data_agencies_directory <- set_data_subdirectory(data_directory, data_download_date, "agencies")
data_agencies_directory_regions <- paste0(data_agencies_directory, "regions/")

# Obtain list of regions for which consolidated data is available
regions_in_database <- list.dirs(paste0(data_directory, data_download_date, "/zakupki-", data_download_date, "-output-data/94fz/"), full.names = F, recursive = F) %>%
  data_frame()
colnames(regions_in_database) <- c("reg_database")
# write.csv(regions_in_database, file = "2-Obtain/data_other/classifications/regions_in_database.csv", row.names = F)

# Join these regions to the classification derived from ICSID data
  # Too many differences in spelling!
# library(fuzzyjoin) # install.packages('fuzzyjoin')
# region_classification <- region_classification_raw %>%
#   stringdist_left_join(regions_in_database, by = c(reg_translit_short = "reg_database_short"), max_dist = 2)

# Hand-coded and saved as a csv to import
region_classification <- read.csv(file = "2-Obtain/data_other/classifications/regions_classification.csv", stringsAsFactors = F)
  rm(region_classification_raw); rm(regions_in_database)

#################################################
# 3. Load data, check quality, recode variables #
#################################################

zakupki_regions <- region_classification %>% filter(!is.na(reg_database)) %>% select(reg_translit, reg_database) %>% as.vector()
icsid_year <- 2011
icsid_filtered <- icsid_raw %>%
  filter(reg_year == icsid_year) %>%
  inner_join(zakupki_regions) %>%
  transmute(ISO_id, reg_id,
            RegionNameEnglish = reg_translit,
            RegionNameRussian = reg_name,
            RegionNameDB = reg_database,
            )
  
# Ideas for regional measures - job is to reproduce typical national-level tests
- Change in number of MPs 2007-2011, proportion of "native" UR MPs (ie penetration), from Levels of...csv "setting the stage"
- Regional budgets (perhaps room for a panel here?) from clearspending
- UR vote share, great variation! https://en.wikipedia.org/wiki/Russian_legislative_election,_2011
- Turnout interacted with UR vote to get at "mobilisation"?
- Corruption cases for bribes by region (careful, also street); from http://crimestat.ru/23 and wrangled in to file "Criminal offenses relating to bribes"
- ICSID:
  - Distance to Moscow (reg_disttomoscow)
- Economy and public administration: Efficiency of public spending (econmanspending)
- Citizen satisfaction with executive authorities' transparency and disclosure (reg_media)
- State employees (reg_psexec; reg_psexec_fed)
- Average wage (reg_avwage_pa)
- Newspaper coverage (reg_newspaper)
- UR national election vote share 2003/07/11 (reg_ur2011)
- President vote share 2000/04/08/12 (reg_winner_pres)
- UR regional election vote share (reg_urvote)
- Percentage of population with income below cost of living, % (reg_belowcost)
- Average annual number of employees in public sector (Education and Health/Social) (reg_nemp_m; reg_nemp_n)
- Residential population (reg_pop)
- Expected lifespan at birth (reg_lifeexp)
- Number of economic crimes (reg_econcrime)
- Public expenditure index (reg_ibr)
- Tax capacity index (reg_inp)
- Gross regional product (reg_grp)
- GRP volume index (reg_indgrp)
- % GRP created by Public admin and defense, education, health, social (reg_grp_l:reg_grp_n)
- Share of fuel and energy minerals in the volume of shipped goods (toplextr_structure)
- Number of small enterprises in a region (reg_nfirmssmall_rosstat) / Total number of enterprises at the end of the year (reg_nfirmstotal_rosstat)
- Total number of enterprises at the end of the year vs established vs liquidated (reg_nfirmstotal_egrul, reg_firmscreated, reg_firmsliqtd)
- Consumer prices index (reg_cpi)
- Desnity of public roads (reg_autoroadden)
- Region rating by the quality of the legislative framework on anti-corruption
- 


  ###########################################################
  # 5. Save the dataframes containing measures to new files #
  ###########################################################

  # First specify variables to keep
  # agency_variables <- c("Year")

  #
  # save(agencies_all, file = "~/data/zakupki/2015-06-13/zakupki-2015-06-13-agencies-data/all_agencies_2015-06-13_compress.rda", compress = T)
  # 
  # rm(agencies_all); rm(agencies); gc()
  
  
  

# ENDS
