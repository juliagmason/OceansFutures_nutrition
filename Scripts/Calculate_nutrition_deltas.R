# Calculate projected nutrition differences and convert back into %
# 1/22/25
# JGM

library (tidyverse)

# r data frame of nutrient yield and rni equivalents for each species, eez, time period
mcp_nutr <- readRDS("Data/mcp_nutrients.Rds")

mcp_nutr_delta <- mcp_nutr %>%
  group_by (eez_name, period, ssp, nutrient) %>%
  # sum across species for each eez
  summarize (tot_rni = sum (rni_equivalents, na.rm = TRUE), 
             tot_nutr_tonnes = sum (nutr_tonnes, na.rm = TRUE)) %>%
  #pivot wider by period
  pivot_wider (names_from = period, values_from = c(tot_rni, tot_nutr_tonnes)) %>%
  # calculate difference between periods
  mutate (delta_rni = tot_rni_mid_2020_2039 - tot_rni_present_1995_2014,
          delta_rni_perc = delta_rni /tot_rni_present_1995_2014,
          delta_nutr = tot_nutr_tonnes_mid_2020_2039 - tot_nutr_tonnes_present_1995_2014,
          delta_nutr_perc = delta_nutr/tot_nutr_tonnes_present_1995_2014)
