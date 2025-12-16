## --------------------------------------------- ##
# Google Drive Interaction - Download
## --------------------------------------------- ##
# Purpose:
## Download inputs from the La Kretz Center Shared Drive

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive)

# Get set up
source(file = file.path("00_setup.r"))

# Clear environment
rm(list = ls()); gc()

## -------------------------------- ##
# Download Level 0 Data ----
## -------------------------------- ##

# Identify the relevant folder
link_lvl0 <- googledrive::as_id("https://drive.google.com/drive/folders/1EuicPFFm20bE16v9Z-_0OHv2gyQqdbah")

# Check what's in that
(drive_lvl0 <- googledrive::drive_ls(path = link_lvl0) %>% 
  ## And pare down to only the desired files
  dplyr::filter(stringr::str_detect(string = name, pattern = "long|addspp|add_spp") != T) %>% 
  dplyr::filter(stringr::str_detect(string = tolower(name), pattern = "trex|midland")))

# Download all the contents of that
purrr::walk2(.x = drive_lvl0$id, .y = drive_lvl0$name,
  .f = ~ googledrive::drive_download(file = .x, overwrite = T,
    path = file.path("data", "level-0", tolower(.y))))

# Clear environment
rm(list = ls()); gc()

## -------------------------------- ##
# Download Plant Species Codes ----
## -------------------------------- ##

# Identify the relevant folder
link_flora <- googledrive::as_id("https://drive.google.com/drive/folders/1vCC7NfCwTCemx08rqiYUSGyFuBksgDYT")

# Check what's in that
(drive_flora <- googledrive::drive_ls(path = link_flora) %>% 
  ## And pare down to only the desired files
  dplyr::filter(name %in% c("plant_species_codes_20250628.csv", "plant_life_form_codes.xlsx")))

# Download all the contents of that
purrr::walk2(.x = drive_flora$id, .y = drive_flora$name,
  .f = ~ googledrive::drive_download(file = .x, overwrite = T,
    path = file.path("data", "codes", tolower(.y))))

# End ----
