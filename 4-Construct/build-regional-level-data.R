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

# Define where outputs should go
data_regions_directory <- set_data_subdirectory(data_directory, data_download_date, "regions")

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

# Load the ICSID classification if not done already
source("3-Unpack/load-other-data.R")

# Hand-coded and saved as a csv to import, loaded here
region_classification <- read.csv(file = "2-Obtain/data_other/classifications/regions_classification.csv", stringsAsFactors = F)
  rm(region_classification_raw); rm(regions_in_database)

#################################################
# 3. Load data, check quality, recode variables #
#################################################

zakupki_regions <- region_classification %>% filter(!is.na(reg_database)) %>% select(reg_translit, reg_database) %>% as.vector()
icsid_year <- 2011
icsid_raw$reg_grp[icsid_raw$reg_translit == "Perm krai"] <- 741729.7575 # Transcription error in original data, this linear interpolation

icsid_filtered <- icsid_raw %>%
  inner_join(zakupki_regions) %>%
  arrange(reg_id, reg_year) %>%
  transmute(ISO_id = ISO_id,
            reg_id = as.character(reg_id),
            RegionNameEnglish = reg_translit,
            RegionNameRussian = reg_name,
            RegionNameDB = reg_database,
            Year = reg_year,
            Population = reg_pop,
            PopulationLog = log10(reg_pop),
            LifeExpectancy = as.numeric(reg_lifeexp),
            DistanceFromMoscow = reg_disttomoscow,
            DistanceFromMoscowLog = log10(reg_disttomoscow+1),
            EfficiencyPublicSpendingIndex = lag(econmanspending),
            NewspaperCoveragePer1000 = reg_newspaper,
            URNationalVoteShare2011 = reg_ur2011,
            URNationalVoteShare2007 = reg_ur2007,
            ChangeURVoteShare2007_2011 = (reg_ur2011 - reg_ur2007),
            PutinVoteShare2012 = lead(reg_winner_pres),
            # URRegionalVoteShare2011 = reg_urvote, # Elections take place in all different years
            PopulationBelowCostOfLiving = reg_belowcost, # Already a %
            AverageWageAll = as.numeric(reg_avwage),
            AverageWageBureaucrats = as.numeric(reg_avwage_pa), # Useful to have ratio of bur/all wages as "outside option",
            BureaucratWagePremium = AverageWageBureaucrats/AverageWageAll, # Above 1 their outside options are worse
            NumberEconomicCrimes = reg_econcrime,
            ShareOfEconomicCrimes = reg_econcrime/reg_crime,
            CostOfPublicServicesIndex = reg_ibr, # MinFin uses this to weight transfers, not available for three small regions after 2005
            TaxCapacityIndex = reg_inp, # to adjust for objective regional differences in their own revenue base, ditto for above
            GrossRegionalProduct = as.numeric(reg_grp),
            GRPPerCapita = as.numeric(reg_grp)/as.numeric(reg_pop),
            # GRPPerCapitaLog = log(GRPPerCapita), # Not terribly meaningful, already scaled!
            GRPVolumeIndex = reg_indgrp,
            GRPFromPublicAdmin = (reg_grp_l + reg_grp_m + reg_grp_n),
            GRPFromMining = reg_grp_c, # Already share, no need to log
            # OilProduction = oil_extraction, # Not enough regions with this
            # GasProduction = gas_extraction, # Not enough regions with this
            DensityPublicRoads = reg_autoroadden,
            DensityPublicRoadsLog = log(reg_autoroadden+1),
            ExecutiveEmployees = (as.numeric(reg_psexec) + as.numeric(reg_psexec_fed)), # Not state, govt/exec
            ExecutiveEmployeesPercentage = ExecutiveEmployees/Population*100) %>%
  filter(!is.na(Population) & Year == icsid_year)

# One-off fixes for missing data
icsid_filtered$LifeExpectancy[icsid_filtered$RegionNameEnglish == "Saratov oblast"] <- 68.9 # 2010 value
icsid_filtered$PopulationBelowCostOfLiving[icsid_filtered$RegionNameEnglish == "Chechen Republic"] <- 21.7 # 2012 value

# Check
str(icsid_filtered)
na_count(icsid_filtered)
# Three regions missing Public services/Tax index don't have it after 2005, leave as NA
summary(icsid_filtered)


## Other ideas for regional measures - job is to reproduce typical national-level tests
# - Turnout interacted with UR vote to get at "mobilisation"?
# - Corruption cases for bribes by region (careful, also street); from http://crimestat.ru/23 and wrangled in to file "Criminal offenses relating to bribes"
# - ICSID:
#   - Citizen satisfaction with executive authorities' transparency and disclosure (reg_media)
#   - Average annual number of employees in public sector (Education and Health/Social) (reg_nemp_m; reg_nemp_n)
#   - Number of small enterprises in a region (reg_nfirmssmall_rosstat) / Total number of enterprises at the end of the year (reg_nfirmstotal_rosstat)
#   - Total number of enterprises at the end of the year vs established vs liquidated (reg_nfirmstotal_egrul, reg_firmscreated, reg_firmsliqtd)
#   - Consumer prices index (reg_cpi)


  ###########################################################
  # 5. Save the dataframes containing measures to new files #
  ###########################################################
  
  suppressWarnings(dir.create(data_regions_directory, recursive=TRUE))
  filename <- paste0(data_regions_directory, "icsid_filtered_2015-06-13.rda")
  save(icsid_filtered, file = filename, compress = F)

  rm(icsid_raw); rm(icsid_filtered); rm(zakupki_regions); rm(region_classification); 
  gc()
  
# ENDS
