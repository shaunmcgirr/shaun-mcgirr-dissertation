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
                                 v2x_corr, v2x_pubcorr, v2x_execorr, v2psoppaut, v2excrptps,
                                 e_dpi_checks, e_boix_regime, e_fh_polity2, e_wbgi_cce, 
                                 e_Regime, e_polity_s, e_uds_mean, e_polity2, e_exconst, e_ti_cpi,
                                 e_migdppc, e_GDP_Per_Cap_Haber_Men_2) %>%
                          inner_join(v_dem_years_matching_gcb, by = c("country_name" = "country_name",
                                                                  "country_text_id" = "country_text_id",
                                                                  "year" = "HighestYear")) 

gcb_data_aggregated_joined <- gcb_data_aggregated_cleaned %>%
                                  mutate(`Mean bribery rate across sectors` = rowMeans(.[,3:10])) %>%
                                left_join(v_dem_data_2012_2013, by = c("ISO" = "country_text_id"))

# Create a factor variable for country
gcb_data_aggregated_joined$ISO <- factor(gcb_data_aggregated_joined$ISO, 
                                         levels = gcb_data_aggregated_joined$ISO[order(gcb_data_aggregated_joined$Any)])

#########################################
# 3. Reproduce the original scatterplot #
#########################################

gcb_agencies_only_long <- gcb_data_aggregated_joined %>%
                            select(ISO:`Mean bribery rate across sectors`, v2x_polyarchy,
                                   v2x_pubcorr, v2psoppaut, v2xlg_legcon, v2excrptps) %>%
                            rename(Country = ISO) %>%
                            gather(key = Sector, value = `Bribery rate`, Education:`Mean bribery rate across sectors`)

gcb_agencies_only_plot <- ggplot(gcb_agencies_only_long,
                                 aes(x = Country, y = `Bribery rate`)) +
                          geom_point()
print(gcb_agencies_only_plot)

gcb_agencies_vs_polyarchy <- ggplot(gcb_agencies_only_long, aes(x = v2x_polyarchy, y = `Bribery rate`)) +
                              geom_point() +
                               labs(title = "Sector-specific bribery rates vs index of electoral democracy,\nGlobal Corruption Barometer and V-Dem 2013\n",
                               x = "\nIndex derived from V-Dem expert assessments of electoral democracy",
                               y = "Citizen-reported bribery rate by sector\n")
print(gcb_agencies_vs_polyarchy)           
ggsave(plot = gcb_agencies_vs_polyarchy, filename = "6-Present/GCB-scatterplots/gcb_agencies_vs_polyarchy.pdf", device = "pdf", width = 6, height = 5)

gcb_agencies_vs_pubcorr <- ggplot(gcb_agencies_only_long, aes(x = v2x_pubcorr, y = `Bribery rate`)) +
                              geom_point() + # geom_point(aes(colour = Country)) + theme(legend.position = "none")
                              labs(title = "Sector-specific bribery rates vs overall public sector corruption,\nGlobal Corruption Barometer and V-Dem 2013\n",
                                   x = "\nV-Dem expert assessment of public sector corruption at the national level",
                                   y = "Citizen-reported bribery rate by sector\n")
print(gcb_agencies_vs_pubcorr)          
ggsave(plot = gcb_agencies_vs_pubcorr, filename = "6-Present/GCB-scatterplots/gcb_agencies_vs_pubcorr.pdf", device = "pdf", width = 6, height = 5)

gcb_agencies_vs_oppaut <- gcb_agencies_only_long %>% filter(Country != "VNM") %>%
                              ggplot(aes(x = v2psoppaut, y = `Bribery rate`)) +
                                geom_point() + # geom_point(aes(colour = Country)) + theme(legend.position = "none")
                                 labs(title = "Sector-specific bribery rates vs opposition autonomy,\nGlobal Corruption Barometer and V-Dem 2013\n",
                                  x = "\nV-Dem expert assessment of autonomy of opposition parties from ruling regime",
                                  y = "Citizen-reported bribery rate by sector\n")
print(gcb_agencies_vs_oppaut)          
ggsave(plot = gcb_agencies_vs_oppaut, filename = "6-Present/GCB-scatterplots/gcb_agencies_vs_oppaut.pdf", device = "pdf", width = 6, height = 5)

gcb_agencies_vs_excrptps <- ggplot(gcb_agencies_only_long, aes(x = v2excrptps, y = `Bribery rate`)) +
  geom_point() + # geom_point(aes(colour = Country)) + theme(legend.position = "none")
  labs(title = "Sector-specific bribery rates vs overall public sector corrupt exchanges,\nGlobal Corruption Barometer and V-Dem 2013\n",
       x = "\nV-Dem expert assessment of prevalence of public sector corrupt exchanges",
       y = "Citizen-reported bribery rate per sector\n")
print(gcb_agencies_vs_excrptps)          
ggsave(plot = gcb_agencies_vs_excrptps, filename = "6-Present/GCB-scatterplots/gcb_agencies_vs_excrptps.pdf", device = "pdf")


gcb_agencies_any <- gcb_agencies_only_long %>%
  filter(Sector == "Any sector") %>%
  ggplot(aes(x = v2x_pubcorr, y = `Bribery rate`)) +
  geom_point()
print(gcb_agencies_any)

gcb_agencies_mean <- gcb_agencies_only_long %>%
                      filter(Sector == "Mean bribery rate across sectors") %>%
                      ggplot(aes(x = v2x_pubcorr, y = `Bribery rate`)) +
                        geom_point()
print(gcb_agencies_mean)
 
# ENDS