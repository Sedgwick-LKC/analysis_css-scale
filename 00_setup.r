## --------------------------------------------- ##
# Setup Steps
## --------------------------------------------- ##
# Purpose:
## Perform setup tasks likely to be useful for many downstream scripts

# Load libraries
# install.packages("librarian")
librarian::shelf(tidyverse)

# Clear environment
rm(list = ls()); gc()

## -------------------------------- ##
# Make Folder(s) ----
## -------------------------------- ##

# Necessary input folders
dir.create(path = file.path("data", "level-0"), showWarnings = F, recursive = T)

# Output folders
dir.create(path = file.path("graphs"), showWarnings = F)

# End ----
