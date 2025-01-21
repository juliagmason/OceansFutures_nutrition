# Convert MCP projections to nutrients
# JGM
#1/7/25

library (tidyverse)

# read compiled csv, collated in compile_MCP_data.R
mcp <- read_csv("Data/mcp_full.csv")

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
            nutrient_servings = edible_servings * amount) %>%
    left_join (rni_child, by = "nutrient") %>%
    mutate (rni_equivalents = nutrient_servings / RNI) %>%
    select (nutrient, rni_equivalents)
  
  
}

# check on small version of mcp data
mcp_sm <- sample_n(mcp, 100)

mcp_sm_rnis <- mcp_sm %>%
  mutate (rni_equivalents = map2 (species, mean_mcp, calc_children_fed_func)) %>%
            unnest(cols = c(rni_equivalents),  names_repair = "check_unique") 
