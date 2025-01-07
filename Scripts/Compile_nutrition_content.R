# Compile species nutrient data
# JGM
# 1/7/25

# copying Hicks species nutrient predictions from Demonstration_Cases project. will try updated AFCD first though

library (tidyverse)
library (AFCD)

# function for harmonizing species name capitalization
# https://stackoverflow.com/questions/18509527/first-letter-to-upper-case
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

spp_list <- read_csv("Data/mcp_full.csv") %>%
  select (taxon_name) %>%
  unique() %>%
  # create matching column for missing species, found below
  mutate(afcd_name = case_when (
    grepl("Fenneropenaeus", taxon_name) ~ str_replace (taxon_name, "Fenneropenaeus", "Penaeus"),
    taxon_name == "Liza haematocheila" ~ "Chelon haematocheilus",
    taxon_name == "Liza klunzingeri" ~ "Planiliza klunzingeri",
    grepl("Liza", taxon_name) ~ str_replace (taxon_name, "Liza", "Chelon"),
    taxon_name == "Moolgarda seheli" ~ "Crenimugil seheli",
    taxon_name == "Theragra chalcogramma" ~"Gadus chalcogrammus",
    taxon_name == "Trigloporus lastoviza" ~ "Chelidonichthys lastoviza",
    TRUE ~ taxon_name
  )
  )

dbem_spp_afcd_nutr <-  species_nutrients (spp_list$afcd_name,
                                                prep = c ("raw", NA),
                                                part = c("muscle tissue", "whole"),
                                                nut = c("DHA_EPA", "Calcium", "Iron, total", "Selenium", "Zinc", "Vitamin_a_combined", "Protein_total_combined")
  ) %>%
  # reconvert species names to match data
  mutate (afcd_name = firstup(species))

# species with missing data--18
missing_nutr <- dbem_spp_afcd_nutr %>%
  filter (is.na(value))

# are some species names mismatches?
missing_nutr_spp <- sort(unique (missing_nutr$afcd_name))

missing_nutr_spp[which(!missing_nutr_spp %in% afcd_sci$sciname)]
# replace "Fenneropenaeus" with "Penaeus"
# ""Moolgarda seheli" is now "Crenimugil seheli"
# Liza is now Chelon; keep aurata, haematocheila is haematocheilus; C. Klunziringeri is "Planiliza klunzingeri"
# "Theragra chalcogramma" is now "Gadus chalcogrammus"
# "Trigloporus lastoviza" is "Chelidonichthys lastoviza"
# hydrolagus genus is missing, and don't have hagfish
