# Compile species nutrient data
# JGM
# 1/7/25

# copying Hicks species nutrient predictions from Demonstration_Cases project. 
# JZM email 1/10/25--AFCD update is not complete. Continue to use Hicks for finfish and AFCD Dataverse version for invertebrates


library (tidyverse)
library (AFCD)

# function for harmonizing species name capitalization
# https://stackoverflow.com/questions/18509527/first-letter-to-upper-case
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# species projected in the DBEM model
spp_list <- read_csv("Data/dbem_spp_list.csv") %>%
  # create matching column for missing species, found below
  mutate(nutr_name = case_when (
    taxon_name == "Rhinobatos percellens" ~ "Pseudobatos percellens",
    taxon_name == "Rhinobatos planiceps" ~ "Pseudobatos planiceps",
    taxon_name == "Pterothrissus belloci" ~ "Nemoossis belloci",
    taxon_name == "Oncorhynchus masou masou" ~ "Oncorhynchus masou",
    taxon_name == "Osmerus mordax mordax" ~ "Osmerus mordax",
    taxon_name == "Gadus ogac" ~ "Gadus macrocephalus",
    taxon_name == "Scomberesox saurus saurus" ~ "Scomberesox saurus",
    taxon_name == "Clupea bentincki" ~ "Strangomera bentincki",
    taxon_name == "Diplodus argenteus argenteus" ~  "Diplodus argenteus",
    taxon_name == "Diplodus cervinus cervinus" ~ "Diplodus cervinus",
    taxon_name == "Peprilus alepidotus" ~ "Peprilus paru",
    taxon_name == "Hoplostethus mediterraneus mediterraneus" ~ "Hoplostethus mediterraneus",
    taxon_name == "Clupea harengus membras" ~ "Clupea harengus",
    taxon_name == "Sprattus sprattus balticus" ~ "Sprattus sprattus",
    taxon_name == "Pseudopentaceros richardsoni" ~ "Pentaceros richardsoni",
    taxon_name == "Lepidonotothen mizops" ~ "Lindbergichthys mizops", # not actually the most up to date, should be Nototheniops mizops
    taxon_name == "Lepidonotothen nudifrons" ~ "Lindbergichthys nudifrons",
    taxon_name == "Abudefduf luridus" ~ "Similiparma lurida",
    taxon_name == "Himantura gerrardi" ~ "Maculabatis gerrardi",
    taxon_name == "Dasyatis centroura" ~ "Bathytoshia centroura",
    # comment if using aFCD only, has a different name. DBEM data has two diff names for same species?
    taxon_name %in% c("Liza haematocheila", "Mugil soiuy") ~ "Planiliza haematocheila",
    
    grepl("Fenneropenaeus", taxon_name) ~ str_replace (taxon_name, "Fenneropenaeus", "Penaeus"),
    taxon_name == "Moolgarda seheli" ~ "Crenimugil seheli",
    taxon_name == "Theragra chalcogramma" ~"Gadus chalcogrammus",
    taxon_name == "Trigloporus lastoviza" ~ "Chelidonichthys lastoviza",
    
    # uncomment if using AFCD only
    #grepl("Liza", taxon_name) ~ str_replace (taxon_name, "Liza", "Chelon"), # revisit this, think there are more Lizas in fisnnutr?
    
    #taxon_name == "Liza klunzingeri" ~ "Planiliza klunzingeri",
    #taxon_name == "Liza haematocheila" ~ "Chelon haematocheilus",
    #taxon_name == "Myxine glutinosa" ~ "myxinidae",

    # go to genus or family for those with missing data; AFCD will average 
    #grepl("Callorhinchus", taxon_name) ~ "callorhinchus",
    # go to family for Hydrolagus; only one species in genus and a few chimaeras
   # grepl ("Hydrolagus", taxon_name) ~ "chimaeridae",
    taxon_name == "Echinus esculentus" ~ "echinidae",
    
    
    TRUE ~ taxon_name
  ),
  # create taxa column to use with edible conversion values. in the dataset, taxon ID is all fish up to 657506. then alphabetical by common name?! for inverts. 
  # https://stackoverflow.com/questions/7597559/grep-using-a-character-vector-with-multiple-patterns 
  taxa = case_when (
    taxon_key < 660000 ~ "Finfish", 
    grepl ("shrimp|crab|lobster|krill|prawn|seabob|mantis|Crayfish", common_name) ~ "Crustacean", 
    grepl ("clam|oyster|mussel|scallop|whelk|Whelk|cockle|periwinkle|abalone|quahog|ark|shell|venus|gaper|tivela|semele|callista|pitar", common_name) ~ "Mollusc", 
    grepl ("squid|cuttlefish|octopus", common_name) ~ "Cephalopod", 
    TRUE ~ "Other" #should just be sea urchins
    )
  )

# fish nutrients models, ran code from https://github.com/mamacneil/NutrientFishbase
# copied from Demonstration_Cases project
# this is per 100g raw, edible portion
fishnutr <- read_csv ("Data/Species_Nutrient_Predictions.csv") %>%
  # truncate to just summary predicted value. ask Sarah if we eventually will want range
  select (species, ends_with ("_mu")) %>%
  # pivot to long format
  pivot_longer (Selenium_mu:Vitamin_A_mu,
                names_to = "nutrient",
                values_to = "amount") %>%
  mutate (nutrient = str_sub(nutrient, end = -4))

# grab dbem fish species
spp_nutr_fish <- fishnutr %>%
  filter (species %in% spp_list$nutr_name)

# look at missing--are there any fish?
missing_nutr <- spp_list %>% 
  filter (!nutr_name %in% spp_nutr_fish$species) 

#View (missing_nutr)
# ~ 28 fish and elasmos
# Elasmorhinus brucus--has E. cookei
# Carcharodon carcharias--this can't be contributing that much to nutrition...!?




# invertebrates from AFCD 
dbem_spp_afcd_nutr <-  species_nutrients (missing_nutr$nutr_name,
                                                prep = c ("raw", NA),
                                                part = c("muscle tissue", "whole"),
                                                nut = c("DHA_EPA", "Calcium", "Iron, total", "Selenium", "Zinc", "Vitamin_a_combined", "Protein_total_combined")
  ) %>%
 
  mutate (
    # reconvert species names to match data
    species = firstup(species),
    # rename nutrients to match fishnutrients
    nutrient = case_when (
      nutrient == "DHA_EPA" ~ "Omega_3",
      nutrient == "Iron, total" ~ "Iron",
      nutrient == "Vitamin_a_combined" ~ "Vitamin_A",
      nutrient == "Protein_total_combined" ~ "Protein",
      TRUE ~ nutrient
    )
          )

# match to fishnutr format
spp_nutr_invert <- dbem_spp_afcd_nutr %>%
  select (species, nutrient, value) %>%
  rename (amount = value) %>%
  # remove d gigas, use from bianchi 
  filter (!species == "Dosidicus gigas")

# # d gigas data from Bianchi et al. 2022 supp table 2. Vita A is retinol equiv; omega 3 is n-3 fatty acids
# Dataverse AFCD only has contaminants for d gigas
# afcd_sci %>% filter (sciname == "Dosidicus gigas") %>% View()

d_gigas_nutr <- data.frame (
  species = "Dosidicus gigas", 
  nutrient = c ("Calcium", "Iron", "Omega_3", "Protein", "Selenium", "Vitamin_A", "Zinc"),
  amount = c(37.5, 3.3, 0.6, 16.4, 50.9, 0, 2.8)
) 

# stitch together

# make little dataframe of species name and taxa to add back in
taxa_key <- spp_list %>%
  select (nutr_name, taxa) %>%
  rename (species = nutr_name)

dbem_spp_nutr <- rbind (spp_nutr_fish, spp_nutr_invert, d_gigas_nutr) %>%
  # add back taxa column
  left_join (taxa_key, by = "species") %>%
  replace_na(list(taxa = "Other"))

# add Taxa column to use with edible conversion factors 
saveRDS (dbem_spp_nutr, file = "Data/dbem_spp_nutr_content.Rds")

##########################
# reconcile missing spp afcd

# species with missing data--18
missing_nutr_invert <- dbem_spp_afcd_nutr %>%
  filter (is.na(value))

# are some species names mismatches?
missing_nutr_spp <- sort(unique (missing_nutr_invert$afcd_name))

missing_nutr_spp[which(!missing_nutr_spp %in% afcd_sci$sciname)]
# replace "Fenneropenaeus" with "Penaeus"
# ""Moolgarda seheli" is now "Crenimugil seheli"
# Liza is now Chelon; keep aurata, haematocheila is haematocheilus; C. Klunziringeri is "Planiliza klunzingeri"
# "Theragra chalcogramma" is now "Gadus chalcogrammus"
# "Trigloporus lastoviza" is "Chelidonichthys lastoviza"

# still missing: 
# "Callorhinchus capensis"--has C. callorhynchus
# "Echinus esculentus"--in family echinidae, has Paracentrotus lividus; Loxechinus albus; Sterechinus neumayeri
# hydrolagus genus --has H. affinis, Chimaera monstrosa, and C. phantasma in chimaeridae 
#hagfish; does have an inshore NW Pacific spp, Eptatretus burgeri


family_test <- species_nutrients ("callorhinchus",
                                                         prep = c ("raw", NA),
                                                         part = c("muscle tissue", "whole"),
                                                         nut = c("DHA_EPA", "Calcium", "Iron, total", "Selenium", "Zinc", "Vitamin_a_combined", "Protein_total_combined")
) 
