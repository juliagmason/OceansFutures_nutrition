# JGM
# 1/7/25

# collate csv files from zip
# original folder is mcp_nutrition

library (beepr)
# https://www.statology.org/r-merge-csv-files/
full_df <- list.files (path = "Data/mcp_nutrition", full.names = TRUE) %>%
  lapply (read_csv, col_types = cols(taxon_key = "c",
                                     mean_mcp = "d",
                                     sd_mcp = "d")) %>%
  bind_rows 
beep()

# check for outliers or errors
str(full_df)
hist (full_df$mean_mcp)

full_df %>%
  filter (mean_mcp > 10000) # > 20000: 600318 russia, but seems ok for relative change. > 10000 adds more russia nd china, but hopefully still fine for relative change, doesn't seem like a read error


# combine with species names
# taxon_name column is appropriate for nutrient function
spp_key <- read_csv("Data/dbem_spp_list.csv", 
                    col_types = cols (taxon_key = "c")) %>%
  select (taxon_key, taxon_name, common_name)

full_df <- full_df %>%
  left_join (spp_key, by = "taxon_key") %>%
  # rename species name to "species" to match nutrition data
  rename (species = taxon_name)


write.csv (full_df, file = "Data/mcp_full.csv", row.names = FALSE)



# failed attempt at web scraping
# Download csv files from One Drive

# link from JPA
# https://ubcca-my.sharepoint.com/personal/jepa88_student_ubc_ca/_layouts/15/onedrive.aspx?id=%2Fpersonal%2Fjepa88%5Fstudent%5Fubc%5Fca%2FDocuments%2FData%2Focean%5Ffuture%2FResults%2Fmcp%5Fnutrition&ga=1

# repository/code
# https://github.com/PaulMelloy/Download_from_OneDrive

OD_DL_csv <- function(sharedURL, file_name, save2wd = FALSE){
  
  # Save the shared url 
  URL1 <- unlist(strsplit(sharedURL,"[?]"))[1]
  URL1 <- paste0(URL1,"?download=1") # edit URL to make it a downloadable link
  
  # Download the file to a temp directory using the supplied file name
  curl::curl_download(
    URL1,
    destfile = file.path(tempdir(), file_name),
    mode = "wb"
  )
  
  
  # If the user wants it saved to thier working directory this will copy the file
  if(isTRUE(save2wd)){
    file.copy(
      from = paste0(tempdir(),"\\", file_name),
      to = "./")
  }
  
  # return the CSV as a data.frame
  return(read.csv(paste0(tempdir(), "\\" ,file_name), stringsAsFactors = FALSE))
  
}

sharedURL <- "https://ubcca-my.sharepoint.com/:x:/r/personal/jepa88_student_ubc_ca/_layouts/15/Doc.aspx?sourcedoc=%7B60F40C4B-2B31-411E-BD78-7979792021FF%7D&file=600004_mcp_of.csv&action=default&mobileredirect=true"
# 403 error

# https://jakobtures.github.io/web-scraping/files.html

library (tidyverse)
library (rvest)
website <- "https://ubcca-my.sharepoint.com/personal/jepa88_student_ubc_ca/_layouts/15/onedrive.aspx?id=%2Fpersonal%2Fjepa88%5Fstudent%5Fubc%5Fca%2FDocuments%2FData%2Focean%5Ffuture%2FResults%2Fmcp%5Fnutrition&ga=1" %>% 
  read_html()
