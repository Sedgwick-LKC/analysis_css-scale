## --------------------------------------------- ##
# Data Processing - Plant Metadata
## --------------------------------------------- ##
# Purpose:
## Wrangle and perform QC on plant metadata
## 'metadata' includes scientific/common name, native status, and life form

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse)

# Get set up
source(file = file.path("00_setup.r"))

# Clear environment
rm(list = ls()); gc()

## ----------------------------- ##
# Prepare Plant Metadata ----
## ----------------------------- ##

# Read in plant species information
spp_v01 <- read.csv(file = file.path("data", "codes", "plant_species_codes_20250628.csv"))

# Check structure
dplyr::glimpse(spp_v01)

# Grab the life form codes too
form_v01 <- readxl::read_excel(path = file.path("data", "codes", "plant_life_form_codes.xlsx"))

# Check that structure
dplyr::glimpse(form_v01)

# Combine these two and do general wrangling stuff
spp_v02 <- dplyr::left_join(x = spp_v01, y = form_v01,
    by = c("lifeform" = "Code")) %>% 
  # Ditch unwanted columns
  dplyr::select(-count, -lifeform) %>%
  # Rename some others
  dplyr::rename(lifeform = `Plant life form`,
    species.code = key_value) %>% 
  # Separate scientific name from common name
  tidyr::separate_wider_delim(cols = Species, delim = " - ",
    names = c("species.scientific", "species.common"), too_few = "align_start") %>% 
  # Move species code to left-most position
  dplyr::relocate(species.code, .before = dplyr::everything())

# Check structure
dplyr::glimpse(spp_v02)

# Curious about included scientific/common names?
sort(unique(spp_v02$species.scientific))
sort(unique(spp_v02$species.common))

## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
spp_v99 <- spp_v02

# Check structure
dplyr::glimpse(spp_v99)

# Export locally
write.csv(x = spp_v99, na = '', row.names = F,
  file = file.path("data", "codes", "plant-metadata.csv"))

# End ----
