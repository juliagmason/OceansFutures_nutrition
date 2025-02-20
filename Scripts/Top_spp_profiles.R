# Top 5 species profiles
# 1/31/25

# Nutrient content profiles for top 5 species in Ecuador and Indonesia
library (tidyverse)

# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# species nutrient content, from Compile_nutrition_content.R
spp_nutr <- readRDS("Data/dbem_spp_nutr_content.Rds")

# RNI data ----
# from RNI_explore; WHO
rni_child <- readRDS("Data/RNI_child.Rds")


# join nutrients and rni ----
spp_nutr <-  spp_nutr %>%
  # join to rni data
  left_join (rni_child, by = "nutrient") %>%
  
  # this would be the percentage of your daily requirement you could get from a 100g serving of each species. cap at 100%
  mutate (perc_rni = amount/RNI * 100,
          # optional--cap at 100?
          #perc_rni = ifelse (perc_rni > 100, 100, perc_rni),
          # shorten nutrient names so they fit on the x axis
          nutrient = 
            case_when (nutrient == "Vitamin_A" ~ "Vit A",
                       nutrient == "Omega_3" ~ "Omega 3",
                       TRUE ~ nutrient)) %>%
  ungroup()


# from demonstration_cases Plot_Fig_XC_nutrient_cotent_dodged_bar

# function to plot ----
#modify, think we should keep protein for usaid application
plot_colorful_spp_nutr_dodge_bar <- function (species_names, Selenium = FALSE) {
  
  #species_names is a vector of scientific names
  
  if (Selenium == TRUE) {omit_nutrients <- ""} else {omit_nutrients <- "Selenium"}
  
  spp_nutr %>%
    filter (!nutrient %in% omit_nutrients, 
            species %in% species_names) %>%
    # cut off at 100%
    mutate (perc_rni = ifelse (perc_rni >100, 100, perc_rni)) %>%
    group_by (species) %>%
    # order by overall nutrient density, from Maire et al. 2021
    mutate (micronutrient_density = sum (perc_rni),
            # shorten spp names
            # https://stackoverflow.com/questions/8299978/splitting-a-string-on-the-first-space
            spp_short = ifelse (
              grepl(" ", species),
              paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
              species)
    ) %>%
    ungroup() %>%
    
    ggplot (aes (x = reorder(spp_short, -micronutrient_density), fill = nutrient, y = perc_rni)) +
    geom_col (position = "dodge") +
    theme_bw() +
    labs (x = "", y = "% Child RNI met per 100g serving", fill = "Nutrient") 
}


# r data frame of nutrient yield and rni equivalents for each species, eez, time period
mcp_nutr <- readRDS("Data/mcp_nutrients.Rds")

# #top 5 mcp species for ecuador
# ecu_5spp <- mcp_nutr %>%
#   ungroup() %>%
#   filter (eez_name == "Ecuador", period == "present_1995_2014", ssp == "ssp126", nutrient == "Protein") %>%
# 
#   select (mean_mcp, species) %>%
#   top_n (wt = mean_mcp, n = 5)
# # tons of species have identical mcp??
# 
# indo_5spp <- mcp_nutr %>%
#   ungroup() %>%
#   filter (eez_name %in% c("Indonesia (Eastern)", "Indonesia (Central)"), period == "present_1995_2014", ssp == "ssp126", nutrient == "Protein") %>%
#   
#   select (mean_mcp, species) %>%
#   top_n (wt = mean_mcp, n = 3)

# use SAU data instead
# WWF used FA capture quantity...very slow: https://www.fao.org/fishery/statistics-query/en/capture/capture_quantity

# SAu for now. have indonesia copied from demonstration cases

# Indonesia ----
indo_sau <- read_csv ("Data/SAU_landings/SAU EEZ indonesia.csv")

indo_5spp <- indo_sau %>%
  filter (year >= 2015) %>%
  group_by (scientific_name) %>%
  summarize (tot = sum (tonnes)) %>%
  slice_max (tot, n = 9)
# needed 7 in demonstration_cases to get identifiable species 

# check that these are consistent with dbem
mcp_indo <- mcp_nutr %>% filter (eez_name %in% c("Indonesia (Eastern)", "Indonesia (Central)"))
indo_5spp$scientific_name[which (indo_5spp$scientific_name %in% mcp_indo$species)] # yes



plot_colorful_spp_nutr_dodge_bar(indo_5spp$scientific_name, Selenium = TRUE) +
  ggtitle ("Nutrient content of 100g portion for top species, Indonesia") +
  # scale_x_discrete(labels = c ()) +
  labs (y = "% Child RNI met")+
  scale_fill_brewer(palette = "Set1") +
  theme ( 
    axis.text = element_text (size = 11),
    axis.title = element_text (size = 12),
    legend.text = element_text (size = 10),
    legend.title = element_text (size = 12),
    legend.key.size = unit (3.5, "mm"),
    plot.title = element_text (size = 13),
    plot.margin=unit(c(1,1,1,1), 'mm'))
ggsave ("Figures/Indonesia_top5_bar_nutr.png", width = 7, height = 4, units = "in")

# Ecuador ----

# From WWF team:
ecu_spp <- c("Katsuwonus pelamis", "Opisthonema libertate", "Selene peruviana", "Chloroscombrus orqueta", "Prionace glauca", "Mustelus lunulatus")
# M. lunulatus has no nutrition or MCP data. S. peruviana has nutr data but not MCP, so have to re-fetch

# # galapagos and mainland from SAU
ecu1 <- read_csv("Data/SAU_landings/SAU EEZ 218 v50-1.csv")
ecu2 <- read_csv ("Data/SAU_landings/SAU EEZ 219 v50-1.csv")

ecu_5spp <-  ecu1 %>%
    rbind (ecu2) %>%
    filter (year >= 2015, scientific_name %in% ecu_spp) %>%
    group_by (scientific_name) %>%
    summarize (tot = sum (tonnes))
# 
# ecu_5spp <- ecu1 %>%
#   rbind (ecu2) %>%
#   filter (year >= 2015) %>%
#   group_by (scientific_name) %>%
#   summarize (tot = sum (tonnes)) %>%
#   slice_max (tot, n = 7)


# check that these are consistent with dbem
mcp_ecu <- mcp_nutr %>% filter (eez_name == "Ecuador") # no galapagos?
ecu_5spp$scientific_name[which (ecu_5spp$scientific_name %in% mcp_ecu$species)] # yes


plot_colorful_spp_nutr_dodge_bar(ecu_5spp$scientific_name, Selenium = TRUE) +
  ggtitle ("Nutrient content of 100g portion for top species, Ecuador") +
  labs (y = "% Child RNI met")+
  scale_fill_brewer(palette = "Set1") +
  theme ( 
    axis.text = element_text (size = 11),
    axis.title = element_text (size = 12),
    legend.text = element_text (size = 10),
    legend.title = element_text (size = 12),
    legend.key.size = unit (3.5, "mm"),
    plot.title = element_text (size = 13),
    plot.margin=unit(c(1,1,1,1), 'mm'))

ggsave ("Figures/Ecuador_top5_bar_nutr.png", width = 7, height = 4, units = "in")

# What is the projection trajectory for these species? ----
# read compiled csv, collated in compile_MCP_data.R
mcp_full <- read_csv("Data/mcp_full.csv")

mcp_indo5 <- mcp_full %>%
  filter (eez_name %in% c("Indonesia (Eastern)", "Indonesia (Central)"), species %in% indo_5spp$scientific_name)

indo5_perc_change <- mcp_indo5 %>%
  group_by (period, ssp, species) %>%
  summarise (mean_mcp = mean (mean_mcp)) %>%
  pivot_wider (names_from = period, values_from = mean_mcp) %>%
  mutate (perc_change = (mid_2020_2039 - present_1995_2014)/present_1995_2014 * 100)

# connect to SAU landings?
indo_sau_mean_catch <- indo_sau %>%
  filter (year >= 2015) %>%
  group_by (scientific_name) %>%
  summarize (mean_catch_sau = mean (tonnes)) %>%
  rename (species = scientific_name)

indo5_sau_perc_change <- indo5_perc_change %>%
  left_join (indo_sau_mean_catch, by = "species") %>%
  select (species, mean_catch_sau, ssp, perc_change) %>%
  pivot_wider (names_from = ssp, values_from = perc_change) %>%
  arrange (desc (mean_catch_sau)) %>%
  rename (perc_change_ssp126 = ssp126, perc_change_ssp585 = ssp585)

# ecuador
mcp_ecu5 <- mcp_full %>%
  filter (eez_name == "Ecuador", species %in% ecu_5spp$scientific_name)

ecu5_perc_change <- mcp_ecu5 %>%
  group_by (period, ssp, species) %>%
  summarise (mean_mcp = mean (mean_mcp)) %>%
  pivot_wider (names_from = period, values_from = mean_mcp) %>%
  mutate (perc_change = (mid_2020_2039 - present_1995_2014)/present_1995_2014 * 100)

# connect to SAU landings
ecu_sau_mean_catch <- ecu1 %>%
  rbind (ecu2) %>%
  filter (year >= 2015) %>%
  group_by (scientific_name) %>%
  summarize (mean_catch_sau = mean (tonnes, na.rm = TRUE)) %>%
  rename (species = scientific_name)

ecu5_sau_perc_change <- ecu5_perc_change %>%
  left_join (ecu_sau_mean_catch, by = "species") %>%
  select (species, mean_catch_sau, ssp, perc_change) %>%
  pivot_wider (names_from = ssp, values_from = perc_change) %>%
  arrange (desc (mean_catch_sau)) %>%
  rename (perc_change_ssp126 = ssp126, perc_change_ssp585 = ssp585)


# calculate RNIs from SAU data, then show % change? ----

# read species nutrient content, from Compile_nutrition_content.R
spp_nutr <- readRDS("Data/dbem_spp_nutr_content.Rds")

# Child RNIs from WHO (copied from Demonstration_Cases project)
rni_child <- readRDS("Data/RNI_child.Rds")

# Function to convert metric tons of catch for a given species to child RNI equivalents. Modified from Funciton_Convert_catch_amt_children_fed.R in Demonstration_Cases
calc_children_fed_func <- function (species_name, amount_mt) {
  # species name is Latin name separated by a space, e.g. "Gadus morhua"
  # amount_mt is catch/landings/wet weight in metric tons--MCP
  
  nutr_content <- spp_nutr %>% filter (species == species_name)
  
  catch_nutrients <- nutr_content  %>%
    mutate (catch_mt = amount_mt,
            # edible portion conversion values from GENuS/Nutricast, Free et al. 2022 cites Roberts 1998: Roberts, P. Conversion Factors for Estimating the Equivalent Live Weight of Fisheries Products (The Food and Agriculture Organization of the United Nations, 1998).
            p_edible = case_when (
              taxa == "Finfish" ~ 0.87,
              taxa == "Crustacean" ~ 0.36,
              taxa == "Mollusc" ~ 0.17,
              # GENuS/nutricast is 0.21 for cephalopods. Using 0.67, Bianchi et al. 2022 value for D. gigas; only cephalopod in our priority species. They also have a blanket 0.7 value for cephalopods.
              taxa == "Cephalopod" & species == "Dosidicus gigas" ~ 0.67,
              taxa == "Cephalopod" & species != "Dosidicus gigas" ~ 0.21,
              # for unknown conversions, set to 1
              taxa == "Other" ~ 1),
            # convert tons per year to 100g /day, proportion edible
            edible_servings = catch_mt * p_edible * 1000 * 1000 /100 / 365,
            nutrient_servings = edible_servings * amount,
            # also calculate nutrient yield in tons
            scalar = case_when (
              nutrient %in% c("Protein", "Omega_3") ~ 1,
              nutrient %in% c("Calcium", "Zinc", "Iron") ~ 1/1000,
              nutrient %in% c("Vitamin_A", "Selenium") ~ 1/1e6
            ),
            # input (catch_mt) is in metric tons. amount is in units / 100g. so divide by 100 to account for serving size, and multiply by scalar to cancel out g
            nutr_tonnes = catch_mt * p_edible * amount * scalar / 100 
    ) %>%
    left_join (rni_child, by = "nutrient") %>%
    mutate (rni_equivalents = nutrient_servings / RNI) %>%
    select (nutrient, rni_equivalents, nutr_tonnes)
  
  
}

# ecuador--use landings not MCP to preserve S. peruviana
ecu_5spp_rnis <- ecu5_sau_perc_change %>% 
  filter (species %in% ecu_5spp$scientific_name) %>%
  mutate (rni_equivalents = map2 (species, mean_catch_sau, calc_children_fed_func)) %>%
  unnest(cols = c(rni_equivalents),  names_repair = "check_unique") 


indo_5spp_rnis <- indo5_sau_perc_change %>% 
  mutate (rni_equivalents = map2 (species, mean_catch_sau, calc_children_fed_func)) %>%
  unnest(cols = c(rni_equivalents),  names_repair = "check_unique") 


indo_5spp_rnis %>%
  ggplot (aes (x = nutrient, y = rni_equivalents/1000000, fill = species)) +
  geom_col () +
  theme_bw() +
  ggtitle ("Nutrient yield, top 5 species, Indonesia") +
  labs (x = "", y = "Child RNI equivalents, millions", fill = "Species") +
  theme (axis.text.y = element_text (size = 11),
         axis.text.x = element_text (size = 9),
         axis.title = element_text (size = 12),
         legend.text = element_text (size = 9),
         legend.title = element_text (size = 11),
         plot.title = element_text (size = 13))
ggsave ("Figures/Indonesia_top5_nutr_yield.png", width = 7, height = 4, units = "in")

# ecuador--use landings not MCP to preserve S. peruviana
ecu_sau_mean_catch %>% 
  filter (species %in% ecu_5spp$scientific_name) %>%
  mutate (rni_equivalents = map2 (species, mean_catch_sau, calc_children_fed_func)) %>%
  unnest(cols = c(rni_equivalents),  names_repair = "check_unique") %>%
  ggplot (aes (x = nutrient, y = rni_equivalents/1000000, fill = species)) +
  geom_col () +
  theme_bw() +
  ggtitle ("Nutrient yield, top 5 species, Ecuador") +
  labs (x = "", y = "Child RNI equivalents, millions", fill = "Species") +
  theme (axis.text.y = element_text (size = 11),
         axis.text.x = element_text (size = 9),
         axis.title = element_text (size = 12),
         legend.text = element_text (size = 9),
         legend.title = element_text (size = 11),
         plot.title = element_text (size = 13))
ggsave ("Figures/Ecuador_top5_nutr_yield.png", width = 7, height = 4, units = "in")


# this is more informative for overall nutrient yield, but can't bring in projections


ecu_5spp_rnis %>%
  ggplot () +
  geom_col (aes (x = species, y = rni_equivalents/1000000, fill = nutrient)) +
  theme_bw() 
 # geom_point (aes (x = species, y = rni_equivalents/1000000 * perc_change_ssp585/100 + rni_equivalents/1000000)) makes a dot for each nutrient
# this is basically the same information as the dodged bar, but with catch volume. 

# Want to have a dot or dashed line on each column. need to make a different grouped df so just one dot?

# indo
# set order of species to match dodged bar nutrient density
indo_spp_order <- c("R. brachysoma", "S. lemuru", "A. thazard", "S. leptolepis", "S. commerson")

indo_tot_rnis_perc_change <- indo_5spp_rnis %>%
  group_by (species) %>%
  reframe (ssp126 = sum (rni_equivalents) * (1 + perc_change_ssp126/100),
           ssp585 = sum (rni_equivalents) * (1 + perc_change_ssp585/100)) %>%
  pivot_longer (!species, names_to = "SSP", values_to = "tot_rni") %>%
  # shorten species names
  mutate (
    spp_short = ifelse (
      grepl(" ", species),
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species)
  ) 


indo_5spp_rnis %>%
  # shorten species names
  mutate (
    spp_short = ifelse (
      grepl(" ", species),
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species)
  ) %>%
  ggplot () +
  geom_col (aes (x = spp_short, y = rni_equivalents/1000000, fill = nutrient)) +
  theme_bw() +
  geom_point (data = indo_tot_rnis_perc_change, aes (x = spp_short, y = tot_rni/1000000, shape = SSP)) +
  scale_shape_manual (values = c(19, 2)) +
  labs (x = "", y = "Child RNI equivalents, millions", fill = "Nutrient") +
  # unify species order with micronutrient density
  scale_x_discrete (limits = indo_spp_order) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle ("Potential nutrient provisioning, top 5 species, Indonesia") +
  theme (axis.text.y = element_text (size = 11),
         axis.text.x = element_text (size = 9),
         axis.title = element_text (size = 12),
         legend.text = element_text (size = 9),
         legend.title = element_text (size = 11),
         plot.title = element_text (size = 13))
ggsave ("Figures/Indonesia_top5_RNIs_projections.png", width = 7, height = 4, units = "in")

# set order of species to match dodged bar nutrient density
ecu_spp_order <- c("C. orqueta", "K. pelamis", "O. libertate", "P. glauca")
#ecu_spp_order <- c("C. mysticetus", "T. albacares", "K. pelamis", "O. libertate", "S. japonicus")

ecu_tot_rnis_perc_change <- ecu_5spp_rnis %>%
  group_by (species) %>%
  reframe (ssp126 = sum (rni_equivalents) * (1 + perc_change_ssp126/100),
             ssp585 = sum (rni_equivalents) * (1 + perc_change_ssp585/100)) %>%
  pivot_longer (!species, names_to = "SSP", values_to = "tot_rni") %>%
  # shorten species names
  mutate (
    spp_short = ifelse (
      grepl(" ", species),
      paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
      species)
  ) 


ecu_5spp_rnis %>%
  # shorten species names
  mutate (
  spp_short = ifelse (
    grepl(" ", species),
    paste0 (substr(species, 1, 1), ". ", str_split_fixed (species, " ", 2)[,2]),
    species)
  ) %>%
  ggplot () +
  geom_col (aes (x = spp_short, y = rni_equivalents/1000000, fill = nutrient)) +
  theme_bw() +
  geom_point (data = ecu_tot_rnis_perc_change, aes (x = spp_short, y = tot_rni/1000000, shape = SSP)) +
  scale_shape_manual (values = c(19, 2)) +
  labs (x = "", y = "Child RNI equivalents, millions", fill = "Nutrient") +
  # unify species order with micronutrient density
  scale_x_discrete (limits = ecu_spp_order) +
  scale_fill_brewer(palette = "Set1") +
  ggtitle ("Potential nutrient provisioning, top 5 species, Ecuador") +
  theme (axis.text.y = element_text (size = 11),
         axis.text.x = element_text (size = 9),
         axis.title = element_text (size = 12),
         legend.text = element_text (size = 9),
         legend.title = element_text (size = 11),
         plot.title = element_text (size = 13))
ggsave ("Figures/Ecuador_top5_RNIs_projections.png", width = 7, height = 4, units = "in")
