## --------------------------------------------- ##
# Data Processing - Trex 2023
## --------------------------------------------- ##
# Purpose:
## Wrangle and perform QC on Trex 2023 data (2024-2025)
## Note that "Trex 2023" is a site not a single study year

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
(trx23_files <- dir(path = file.path("data", "level-0"), pattern = "trex23"))

# Read in each of these files
trx23_v01 <- purrr::map(.x = trx23_files,
    .f = ~ readxl::read_excel(path = file.path("data", "level-0", .x))) %>% 
  ## Unlist to a single table
  purrr::list_rbind(x = .) %>% 
  ## Remove any columns that are completely empty
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  ## Remove a weird, unnamed, notes-ish column from transect 3
  dplyr::select(-`...29`) %>% 
  ## Coalesce duplicate date columns
  dplyr::mutate(survey_date = dplyr::coalesce(survey_date, date)) %>% 
  dplyr::select(-date)

# Check structure
dplyr::glimpse(trx23_v01)

## ----------------------------- ##
# Reshape to Long Format ----
## ----------------------------- ##

# We need everything in long format
trx23_v02 <- trx23_v01 %>% 
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
    sp.cov %in% c("ACMISTRI", "ACMIGLAB", "BROMDIAN", "ERODCICU") ~ NA,
    T ~ sp.cov)) %>% 
  dplyr::filter(!is.na(sp.cov)) %>% 
  # Now split species from cover into separate columns
  tidyr::separate_wider_delim(cols = sp.cov, delim = "___",
    names = c("species.code", "cover.range")) %>% 
  dplyr::select(-name)

# Check structure
dplyr::glimpse(trx23_v02)

## ----------------------------- ##
# General Quality Control ----
## ----------------------------- ##

# Some broadly-useful tweaks
trx23_v03 <- trx23_v02 %>% 
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
dplyr::glimpse(trx23_v03)

## ----------------------------- ##
# Prepare Plant Metadata ----
## ----------------------------- ##

# Read in plant species metadata
spp_v01 <- read.csv(file = file.path("data", "codes", "plant-metadata.csv"))

# Check structure
dplyr::glimpse(spp_v01)

## ----------------------------- ##
# Species Quality Control ----
## ----------------------------- ##

# What species are in the data now?
sort(unique(trx23_v03$species.code))

# Any not found in the 'codes' file?
sort(setdiff(x = unique(trx23_v03$species.code), y = unique(spp_v01$key_value)))

# Do any needed tidying here
trx23_v04 <- trx23_v03 %>% 
  dplyr::mutate(species.code = dplyr::case_when(
    species.code == "BEOMHORD" ~ "BROMHORD", # Seems like a typo of 'Bromus hordeaceus'
    species.code == "BROMMADR" ~ "BROMMAMA", # "codes" splits two subsp of 'Bromus madritensis'
    species.code == "CRASCONN" ~ "CRASSCONN", # Simple typo
    species.code == "TRIFSPP" ~ "TRIFSPEC", # Likely also typo
    T ~ species.code))

# Re-check species names
sort(setdiff(x = unique(trx23_v04$species.code), y = unique(spp_v01$key_value)))

## ----------------------------- ##
# Integrate Plant Metadata ----
## ----------------------------- ##

# Join on the plant lifeform information
trx23_v05 <- trx23_v04 %>% 
  dplyr::left_join(y = spp_v01, by = c("species.code")) %>% 
  # Reorder columns slightly
  dplyr::relocate(species.scientific:lifeform, .after = species.code)

# Check structure
dplyr::glimpse(trx23_v05)

## ----------------------------- ##
# Export ----
## ----------------------------- ##

# Make a final object
trx23_v99 <- trx23_v05

# Check the structure
dplyr::glimpse(trx23_v99)

# Export locally
write.csv(x = trx23_v99, na = '', row.names = F,
  file = file.path("data", "level-1", "trex2023_veg-surveys.csv"))

# End ----
