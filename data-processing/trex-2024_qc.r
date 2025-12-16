## --------------------------------------------- ##
# Data Processing - Trex 2024
## --------------------------------------------- ##
# Purpose:
## Wrangle and perform QC on Trex 2024 data (2025 only--for now)
## Note that "Trex 2024" is a site name

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse, tidyxl)

# Get set up
source(file = file.path("00_setup.r"))

# Clear environment
rm(list = ls()); gc()

# Load custom function(s)
purrr::walk(.x = dir(path = file.path("tools"), pattern = "fxn_"),
  .f = ~ source(file = file.path("tools", .x)))

## ----------------------------- ##
# Load Data ----
## ----------------------------- ##

# Identify data from the relevant site
(trx24_files <- dir(path = file.path("data", "level-0"), pattern = "trex24"))

# Read in each of these files
trx24_v01 <- purrr::map(.x = trx24_files,
    .f = ~ readxl::read_excel(path = file.path("data", "level-0", .x))) %>% 
  ## Unlist to a single table
  purrr::list_rbind(x = .) %>% 
  ## Remove any columns that are completely empty
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  ## Remove a weird, unnamed, notes-ish column from transect 15
  dplyr::select(-`...29`)

# Check structure
dplyr::glimpse(trx24_v01)

## ----------------------------- ##
# Reshape to Long Format ----
## ----------------------------- ##

# We need everything in long format
trx24_v02 <- trx24_v01 %>% 
  # Flip species/covers into long format
  tidyr::pivot_longer(cols = -site:-block) %>% 
  # Ditch any rows without cover info
  dplyr::filter(!is.na(value) & nchar(value) != 0) %>% 
  # Sort by all columns except value
  dplyr::arrange(dplyr::across(dplyr::all_of(setdiff(x = names(.), y = c("value"))))) %>% 
  # And remove the distinction between species name and cover (trust me)
  dplyr::mutate(name = gsub(pattern = "cov", replacement = "", x = name)) %>% 
  # Collapse species names and values together (with our chosen delimeter)
  dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(x = names(.), y = c("value"))))) %>% 
  dplyr::summarize(sp.cov = paste(unique(value), collapse = "___"),
    .groups = "keep") %>% 
  dplyr::ungroup() %>% 
  # Do some custom fixes for some probable typos in the data
  dplyr::mutate(sp.cov = dplyr::case_when(
    ## None currently needed
    T ~ sp.cov)) %>% 
  dplyr::filter(!is.na(sp.cov)) %>% 
  # Now split species from cover into separate columns
  tidyr::separate_wider_delim(cols = sp.cov, delim = "___",
    names = c("species.code", "cover.range")) %>% 
  dplyr::select(-name)

# Check structure
dplyr::glimpse(trx24_v02)

## ----------------------------- ##
# General Quality Control ----
## ----------------------------- ##

# Some broadly-useful tweaks
trx24_v03 <- trx24_v02 %>% 
  # Make transect/block non-numbers
  dplyr::mutate(transect = paste0("transect.", transect),
    block = ifelse(nchar(block) == 1,
      yes = paste0("block.0", block),
      no = paste0("block.", block))) %>% 
  # Get a year column
  dplyr::mutate(survey_year = year(survey_date),
    .before = survey_date) %>% 
  # Calculate mid point of cover ranges
  dplyr::mutate(cover.midpoint = dplyr::case_when(
    cover.range == "<1%" ~ 0.5,
    cover.range == "1-10%" ~ 5.5,
    cover.range == "10-25%" ~ 17.5,
    cover.range == "25-50%" ~ 37.5,
    cover.range == "50-75%" ~ 62.5,
    cover.range == "75-100%" ~ 87.5))

# Check structure
dplyr::glimpse(trx24_v03)

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
  dplyr::rename(lifeform = `Plant life form`) %>% 
  # Separate scientific name from common name
  tidyr::separate_wider_delim(cols = Species, delim = " - ",
    names = c("species.scientific", "species.common"), too_few = "align_start")

# Check structure
dplyr::glimpse(spp_v02)

# Curious about included scientific/common names?
sort(unique(spp_v02$species.scientific))
sort(unique(spp_v02$species.common))

## ----------------------------- ##
# Species Quality Control ----
## ----------------------------- ##

# What species are in the data now?
sort(unique(trx24_v03$species.code))

# Any not found in the 'codes' file?
sort(setdiff(x = unique(trx24_v03$species.code), y = unique(spp_v02$key_value)))

# Do any needed tidying here
trx24_v04 <- trx24_v03 %>% 
  dplyr::mutate(species.code = dplyr::case_when(
    species.code == "BROMMADR" ~ "BROMMAMA", # "codes" splits two subsp of 'Bromus madritensis'
    T ~ species.code))

# Re-check species names
sort(setdiff(x = unique(trx24_v04$species.code), y = unique(spp_v02$key_value)))

## ----------------------------- ##
# Integrate Plant Metadata ----
## ----------------------------- ##

# Join on the plant lifeform information
trx24_v05 <- trx24_v04 %>% 
  dplyr::left_join(y = spp_v02, by = c("species.code" = "key_value")) %>% 
  # Reorder columns slightly
  dplyr::relocate(species.scientific:lifeform, .after = species.code)

# Check structure
dplyr::glimpse(trx24_v05)

## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
trx24_v99 <- trx24_v05

# Check the structure
dplyr::glimpse(trx24_v99)

# Export locally
write.csv(x = trx24_v99, na = '', row.names = F,
  file = file.path("data", "level-1", "trex2024_veg-surveys.csv"))

# End ----
