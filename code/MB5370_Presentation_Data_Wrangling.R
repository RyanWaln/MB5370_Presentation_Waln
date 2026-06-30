# ══════════════════════════════════════════════════════════════
#  title:  MB5370 Presentation: Data Wrangling
#  author: Ryan Waln
#  date:   `r format(Sys.time(), '%d %B, %Y')`
# ══════════════════════════════════════════════════════════════

# ══════════════════════════════════════════════════════════════
#  INTRODUCTION
# ══════════════════════════════════════════════════════════════

# This script formats the AIMS coral monitoring data in the following ways: 

## Fills missing coordinates and merges coordinates from the coral dataset into the all_reefs dataset
## Merges annual discharge data into all_reefs dataset by sub region
## Extracts year each site was surveyed for each visit
## Calculates mean hard coral cover across all sites in each NRM region for each survey year  
## Calculates the average annual change in hard coral cover for each sampling site 
## Creates summary tables for hard coral cover of each site at the start and end of the survey period
## Creates summary table for average change in hard coral cover of each site throughout survey period

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
#    IMPORT DATA
# ══════════════════════════════════════════════════════════════

## all.reef.csv: contains HC (hard coral cover) measures 
all_reefs <- read_csv(
  here("data/AIMS_Enviornmental_Data/Coral-Index-Reef-Resilience_AIMS_20200522/all.reef.csv"))

## coral.csv: contains geographic coordinates of reefs
coral <- read_csv(here("data/AIMS_Report_2024/coral.csv"))

## discharge.annual: contains total annual discharge (Mega Liters) of all rivers in subregion  
discharge_annual <- read_csv(
  here("data/AIMS_Enviornmental_Data/Coral-Index-Reef-Resilience_AIMS_20200522/discharge.annual.csv"))


# ══════════════════════════════════════════════════════════════
#    FILL MISSING COORDINATES IN coral
# ══════════════════════════════════════════════════════════════
# Two Sample sites per reef at different depths
# Because a given reef's coordinates are essentially the same at each visit,
# can fill each NA with the mean coordinate for that REEF × DEPTH combination.

# Count number of NA entries
cat("Before  NA LATITUDE :", sum(is.na(coral$LATITUDE)),  "\n")
cat("Before  NA LONGITUDE:", sum(is.na(coral$LONGITUDE)), "\n")

coral <- coral |>
  group_by(REEF, DEPTH) |>         # group rows that share the same site
  mutate(
    # replace NA with the mean of all non-NA latitudes for that group
    LATITUDE  = if_else(is.na(LATITUDE),  mean(LATITUDE,  na.rm = TRUE), LATITUDE),
    # same for longitude
    LONGITUDE = if_else(is.na(LONGITUDE), mean(LONGITUDE, na.rm = TRUE), LONGITUDE)
  ) |>
  ungroup()                         # remove grouping so later operations work normally

# Identify any reef × depth sites that STILL have no coordinates after filling
# (can happen if no coordinates were ever taken for reef such as Middle Rf LTMP)
still_na <- coral |>
  filter(is.na(LATITUDE) | is.na(LONGITUDE)) |>
  distinct(REEF, DEPTH)            

cat("After   NA LATITUDE :", sum(is.na(coral$LATITUDE)),  "\n")
cat("After   NA LONGITUDE:", sum(is.na(coral$LONGITUDE)), "\n")

# Report which sites (if any) could not be filled and may have to be excluded to prevent errors later on
if (nrow(still_na) > 0) {
  cat("Sites with NO known coordinates (will be excluded from maps):\n")
  print(still_na)
} else {
  cat("All NAs filled.\n")
}

# ══════════════════════════════════════════════════════════════
#    EXTRACT YEAR FROM all_reefs
# ══════════════════════════════════════════════════════════════
# The Date column in all_reefs is stored as DD/MM/YYYY 
# Sampling occurs once a year so must extract the numeric year.
# Will also need to set to year before merging discharge_annual which only recorded year

all_reefs <- all_reefs |>
  mutate(yr = year(dmy(Date)))   # dmy() breaks up day-month-year strings while year() extracts year

# ══════════════════════════════════════════════════════════════
#    MERGE COORIDINATES INTO all_reefs
# ══════════════════════════════════════════════════════════════
# all_reefs has no geographic coordinates, so must merge with coral dataset.
# Compute the mean lat/lon per REEF × DEPTH from coral, then join to all_reefs.

coord_lookup <- coral |>
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) |>   # exclude any NA values
  group_by(REEF, DEPTH) |>                         # group by unique sites
  summarise(
    LATITUDE  = mean(LATITUDE),    # average lat across all site visits
    LONGITUDE = mean(LONGITUDE),   # average lon across all site visits
    .groups = "drop"
  )

print(coord_lookup) # Check to make sure data is ordered correctly

# Merge w/all_reefs by matching REEF and DEPTH
all_reefs <- all_reefs |>
  left_join(coord_lookup, by = c("REEF", "DEPTH"))

# ══════════════════════════════════════════════════════════════
#    MERGE ANNUAL DISCHARGE INTO all_reefs
# ══════════════════════════════════════════════════════════════
# discharge_annual values listed by subregion which is also a column in all_reefs
# merge by shared column and year

# Rename columns to match all_reefs before joining:
#   "Year" to "yr" so it aligns with the yr column extracted above
discharge_annual <- discharge_annual |>
  rename( yr = Year)  # match the year column name used in all_reefs

# Left-join onto all_reefs by subregion and year.

all_reefs <- all_reefs |>
  left_join(discharge_annual, by = c("subregion", "yr")) # match on both subregion and year

# Data limitations: years before 2005 or after 2018 will receive NA for discharge_annual

print(all_reefs)

# ══════════════════════════════════════════════════════════════
#   CREATE DATASET TO COMPARE ANNUAL DISCHARGE TO HC
# ══════════════════════════════════════════════════════════════
# Need to aggregate all_reefs to the NMR × year level so each row is one NRM region in one year, 
# with mean HC cover and total annual discharge.

discharge_HC <- all_reefs |>
  filter(!is.na(discharge.c.annual), !is.na(HC), !is.na(NRM_REGION)) |>  # drop rows with NA values
  group_by(NRM_REGION, yr) |>                         # group by region and year
  summarise(
    mean_HC      = mean(HC, na.rm = TRUE),        # mean HC cover across all sites in that NRM region × year
    total_discharge = first(discharge.c.annual),  # discharge is the same for all rows, so just take the first value
    .groups = "drop"
  )

print(discharge_HC)   # check structure 

# ══════════════════════════════════════════════════════════════
#    CALCULATE AVERAGE CHANGE IN HC (hard coral cover)
# ══════════════════════════════════════════════════════════════
# Create function to extract slope from a simple linear regression y ~ x.
# Used to estimate the average annual change in HC.

lm_slope <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)          # Only use rows where both x and y are valid to prevent NA errors
  if (sum(keep) < 2) return(NA_real_)    # need at least 2 points to fit a line
  coef(lm(y[keep] ~ x[keep]))[2]         # [2] selects the slope coefficient
}

# ══════════════════════════════════════════════════════════════
#    BUILD SUMMARY TABLES FOR all_reefs TO USE FOR MAP PLOTTING
# ══════════════════════════════════════════════════════════════
##   earliest_HC:   HC in the starting year
##   latest_HC:     HC in the most recent year
##   avg_HC_change: calculated w/linear slope of HC over the whole sampling period

all_reefs_summary <- all_reefs |>
  filter(!is.na(LATITUDE),!is.na(LONGITUDE)) |>              # drop sites with no coordinates
  group_by(REEF, DEPTH, LATITUDE, LONGITUDE, NRM_REGION) |>
  summarise(
    # Earliest HC
    earliest_HC   = HC[which.min(yr)],
    
    # Latest HC
    latest_HC     = HC[which.max(yr)],
    
    # Average annual change
    avg_HC_change = lm_slope(yr, HC),
    
    .groups = "drop"
  ) |>
  mutate(
    # Add columns for coloring scheme in Fig 3 to help denote HC change
    change_dir = case_when(
      avg_HC_change > 0  ~ "Increase",
      avg_HC_change < 0  ~ "Decrease",
      avg_HC_change == 0 ~ "No change",
      TRUE               ~ NA_character_   # catches any remaining NAs
    ))

print(all_reefs_summary) # Check to spot any errors

