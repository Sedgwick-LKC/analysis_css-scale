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
  dplyr::filter(stringr::str_detect(string = name, pattern = "long|addspp") != T) %>% 
  dplyr::filter(stringr::str_detect(string = tolower(name), pattern = "trex|midland")))

# Download all the contents of that
purrr::walk2(.x = drive_lvl0$id, .y = drive_lvl0$name,
  .f = ~ googledrive::drive_download(file = .x, overwrite = T,
    path = file.path("data", "level-0", tolower(.y))))

# End ----
