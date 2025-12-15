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
(drive_lvl0 <- googledrive::drive_ls(path = link_lvl0))

# Download all the contents of that
purrr::walk2(.x = drive_lvl0$id, .y = drive_lvl0$name,
  .f = ~ googledrive::drive_download(file = .x, overwrite = T,
    path = file.path("data", "level-0", .y)))

# End ----
