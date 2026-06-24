# ══════════════════════════════════════════════════════════════
#  title:  MB5370 Presentation - Data Wrangling & Mapping
#  author: Ryan Waln
#  date:   `r format(Sys.time(), '%d %B, %Y')`
# ══════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════
#  INTRODUCTION
# ══════════════════════════════════════════════════════════════

# This script formats the AIMS coral monitoring data in the following ways: 

## Fills missing coordinates and merges coordinates from the coral dataset into the all_reefs dataset
## Extracts year each site was surveyed for each visit
## Calculates the average annual change in hard coral cover for each sampling site 


# Housekeeping
rm(list = ls())   
objects()         

# Packages
library(here)       
library(tidyverse)  
library(sf)         

  
## Rules for tidy data:

#   Each variable must have its own column.

#   Each observation must have its own row.

#   Each value must have its own cell.

#   ALWAYS: Put each dataset in a tibble and each variable in a column.

# ══════════════════════════════════════════════════════════════
#    TESTING
# ══════════════════════════════════════════════════════════════

# Hard coral cover by region

ggplot(coral, aes(x = NRM_REGION, y = HC)) + stat_smooth() + geom_boxplot()

ggplot(all_reefs) + aes(x = LONGITUDE, y = LATITUDE, color = HC ) + geom_point()
