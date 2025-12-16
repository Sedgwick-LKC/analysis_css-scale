## --------------------------------------------- ##
# Setup Steps
## --------------------------------------------- ##
# Purpose:
## Perform setup tasks likely to be useful for many downstream scripts

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
(trx22_files <- dir(path = file.path("data", "level-0"), pattern = "trex22"))

# Read in each of these files
trx22_v01 <- purrr::map(.x = trx22_files,
    .f = ~ readxl::read_excel(path = file.path("data", "level-0", .x))) %>% 
  ## Unlist to a single table
  purrr::list_rbind(x = .) %>% 
  ## Remove any columns that are completely empty
  dplyr::select(-dplyr::where(fn = ~ all(is.na(.) | nchar(.) == 0))) %>% 
  ## Remove a weird, unnamed, notes-ish column from transect 3
  dplyr::select(-`...29`)

# Check structure
dplyr::glimpse(trx22_v01)

## ----------------------------- ##
# Reshape to Long Format ----
## ----------------------------- ##

# We need everything in long format
trx22_v02 <- trx22_v01 %>% 
  # Flip species/covers into long format
  tidyr::pivot_longer(cols = -site:-block) %>% 
  # Ditch any rows without cover info
  dplyr::filter(!is.na(value) & nchar(value) != 0) %>% 
  # Standardize old names
  dplyr::mutate(name = dplyr::case_when(
    name == "c0v7" ~ "cov7",
    name == "ssp8" ~ "sp8",
    T ~ name)) %>% 
  dplyr::mutate(name = ifelse(nchar(name) == 4,
    yes = paste0("sp", gsub("cov", "", name), "cov"), no = name)) %>% 
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
    sp.cov == "SALVLEUC___STIPLEPI___1-10%" ~ "STIPLEPI___1-10%",
    sp.cov %in% c("LUPINANU", "BROMDIAN", "BROMMADR") ~ NA,
    T ~ sp.cov)) %>% 
  dplyr::filter(!is.na(sp.cov)) %>% 
  # Now split species from cover into separate columns
  tidyr::separate_wider_delim(cols = sp.cov, delim = "___",
    names = c("species.code", "cover.range")) %>% 
  dplyr::select(-name)

# Check structure
dplyr::glimpse(trx22_v02)

## ----------------------------- ##
# General Quality Control ----
## ----------------------------- ##

# Some broadly-useful tweaks
trx22_v03 <- trx22_v02 %>% 
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
dplyr::glimpse(trx22_v03)

## ----------------------------- ##
# Species Quality Control ----
## ----------------------------- ##

# What species are in the data now?
sort(unique(trx22_v03$species.code))

# Do any needed tidying here
trx22_v04 <- trx22_v03 %>% 
  dplyr::mutate(species.code = dplyr::case_when(
    # No such wrangling currently needed
    T ~ species.code))

# Re-check species names
sort(unique(trx22_v04$species.code))

## ----------------------------- ##

## ----------------------------- ##


# End ----
