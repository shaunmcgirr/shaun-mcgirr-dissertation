# 6-Present\scatterplots-GCB.R

# Goals of this script are:
#   - Analyse the GCB data (and cross-national controls) loaded in 3-Unpack/load-other-data

###################
# 1. Housekeeping #
###################

# Load functions

# Tell R where to save plots
gcb_output_directory <- "6-Present/GCB-scatterplots/"

############################################
# 2. Gather parameters about the job ahead #
############################################

# Gather metadata about the data to be loaded


##############################################
# 3. Join GCB data with other cross-national #
##############################################

# Create file with highest year of observation per country
v_dem_years_matching_gcb <- v_dem_data_raw %>%
                              filter(year <= 2013) %>%
                              group_by(country_name, country_text_id) %>%
                                summarise(HighestYear = max(year)) %>% ungroup() %>%
                              select(country_name, country_text_id, HighestYear) %>%
                              filter(HighestYear >= 2012)

# Subset v_dem
v_dem_data_2012_2013 <- v_dem_data_raw %>%
                          select(country_name, country_id, country_text_id, year,
                                 v2x_polyarchy, v2x_libdem, v2xme_altinf, v2xlg_legcon,
                                 v2x_corr, v2x_pubcorr, v2x_execorr,
                                 e_dpi_checks, e_boix_regime, e_fh_polity2, e_wbgi_cce, 
                                 e_Regime, e_polity_s, e_uds_mean, e_polity2, e_exconst, e_ti_cpi,
                                 e_migdppc, e_GDP_Per_Cap_Haber_Men_2) %>%
                          inner_join(v_dem_data_2012_2013, by = c("country_name" = "country_name",
                                                                  "country_text_id" = "country_text_id",
                                                                  "year" = "HighestYear")) 

gcb_data_aggregated_joined <- gcb_data_aggregated_cleaned %>%
                                left_join(v_dem_data_2012_2013, by = c("ISO" = "country_text_id"))

# Create a factor variable for country
gcb_data_aggregated_joined$ISO <- factor(gcb_data_aggregated_joined$ISO, 
                                         levels = gcb_data_aggregated_joined$ISO[order(gcb_data_aggregated_joined$Any)])

#########################################
# 3. Reproduce the original scatterplot #
#########################################

gcb_agencies_only_long <- gcb_data_aggregated_joined %>%
                            select(ISO:Any) %>%
                            rename(Country = ISO) %>%
                            gather(key = Sector, value = `Bribery rate`, Education:Any)

gcb_agencies_only_plot <- ggplot(gcb_agencies_only_long,
                                 aes(x = Country, y = `Bribery rate`)) +
                          geom_point()

print(gcb_agencies_only_plot)
                                 
 
# ENDS