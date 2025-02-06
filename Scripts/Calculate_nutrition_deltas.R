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
          # should be exact same as delta_rni, check
          delta_nutr_perc = delta_nutr/tot_nutr_tonnes_present_1995_2014)


# quick plot
mcp_nutr_delta %>%
  ggplot (aes(y = delta_nutr_perc)) +
  geom_histogram() +
  facet_grid (ssp~nutrient)

# top 20 EEZs losing nutrients? start with protein, SSP 585? ----
mcp_nutr_delta %>%
  ungroup() %>%
  filter (nutrient == "Protein", ssp == "ssp585") %>%
  select (eez_name, delta_nutr_perc) %>%
  slice_min (delta_nutr_perc, n= 20) %>% 
  arrange (delta_nutr_perc) %>% View()

# make a table with these top 20 but show all of the nutrients ----
# pivot wider
t <- mcp_nutr_delta %>%
  ungroup() %>%
  filter (ssp == "ssp585") %>%
  select (eez_name, nutrient, delta_nutr_perc) %>%
  mutate (delta_nutr_perc = 100 * delta_nutr_perc) %>%
  pivot_wider (names_from = nutrient, values_from = delta_nutr_perc) %>%
  arrange (Protein)

# function for copying R output tables into word/excel----
#https://stackoverflow.com/questions/24704344/copy-an-r-data-frame-to-an-excel-spreadsheet
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}


t %>% write.excel()

# scatterplot with nutrition loss and nutrition dependence ----

# Selig et al. 2018 SI; Ocean Futures might have used separate protein FAO balance?
nutr_dep <- read_csv ("Data/Selig_SI_ocean_dependence_rankings.csv") %>%
  select (Country, `Nutritional dependence`) %>%
  rename (selig_name = Country,
          nutr_dependence = `Nutritional dependence`) 

# check different naming conventions
sort(unique(mcp_nutr_delta$eez_name[which (!mcp_nutr_delta$eez_name %in% nutr_dep$selig_name)]))

# because dbem has multiple EEZs per country, make a matching df?
eez_match <- tibble (
  eez_name = sort (unique (mcp_nutr_delta$eez_name)),
  selig_name = case_when (
    grepl ("Canada", eez_name) ~ "Canada",
    grepl ("France", eez_name) ~ "France",
    grepl ("Greece", eez_name) ~ "Greece", 
    grepl ("Egypt", eez_name) ~ "Egypt", 
    grepl ("Guatemala", eez_name) ~ "Guatemala", 
    grepl ("Honduras", eez_name) ~ "Honduras",
    grepl ("Indonesia", eez_name) ~ "Indonesia", 
    grepl ("Kiribati", eez_name) ~ "Kiribati",
    grepl ("Mexico", eez_name) ~ "Mexico",
    grepl ("New Zealand", eez_name) ~ "New Zealand",
    grepl ("Russia", eez_name) ~ "Russia",
    grepl ("Norway", eez_name) ~ "Norway", 
    eez_name == "Congo (ex-Zaire)" ~ "Congo",
    eez_name == "Congo, R. of" ~ "Democratic Republic of Congo", 
    eez_name == "Gambia" ~ "Gambia, The", 
    eez_name == "Korea (South)" ~ "South Korea",
    eez_name == "Marshall Isl." ~ "Marshall Islands", 
    eez_name == "Micronesia (Federated States of)" ~ "Federated States of Micronesia",
    eez_name == "Morocco (South)" ~ "Morocco", 
    eez_name == "Sao Tome & Principe" ~ "Sao Tome and Principe", 
    eez_name == "Solomon Isl." ~ "Solomon Islands", 
    eez_name == "Turkey (Mediterranean Sea)" ~ "Turkey",
    eez_name == "Viet Nam" ~ "Vietnam",
    TRUE ~ eez_name
  )
)

library (ggrepel)
# https://stackoverflow.com/questions/52397363/r-ggplot2-ggrepel-label-a-subset-of-points-while-being-aware-of-all-points
mcp_nutr_delta %>%
  left_join (eez_match, by = "eez_name") %>%
  left_join (nutr_dep, by = "selig_name") %>%
  filter (nutrient == "Protein") %>%
  ggplot (aes (y = 100* delta_nutr_perc, x = nutr_dependence)) + #, col = nutrient)) +
  geom_point () +
  geom_text_repel(data = . %>% 
                    filter (nutrient == "Protein") %>%
                    mutate(label = ifelse(abs(100* delta_nutr_perc) > 20 | nutr_dependence > 0.5,
                                          eez_name, "")),
                  aes(label = label), 
                  box.padding = 0.5,
                  size = 3,
                  max.overlaps = 200,
                  show.legend = FALSE) + #this removes the 'a' from the legend
  facet_wrap (~ssp) +
  theme_bw () +
  geom_hline (yintercept = 0, lty = 2) +
  labs (y = "Projected % change in nutrition yield",  x= "Nutritional dependence on seafood")


ggsave(file = "Figures/Nutr_delta_nutr_dependence.png", width = 6, height = 4, units = "in", dpi = 300)


