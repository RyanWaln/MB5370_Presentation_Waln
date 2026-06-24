# ══════════════════════════════════════════════════════════════
#  title:  MB5370 Presentation - Data Wrangling & Mapping
#  author: Ryan Waln
#  date:   `r format(Sys.time(), '%d %B, %Y')`
# ══════════════════════════════════════════════════════════════
  ## Introduction
  
  # This file is meant to format the AIMs coral data for the class project 
  
  # Houskeeping
  rm(list=ls())
objects()  


## Rules for tidy data:

#   Each variable must have its own column.

#   Each observation must have its own row.

#   Each value must have its own cell.

#   ALWAYS: Put each dataset in a tibble and each variable in a column.


# Library 
library(gert)
library(usethis)
library(here)
library(tidyverse)
library(sf) 
library(terra) 
library(tmap)


# Import Datasets 

coral <- read_csv(here::here("data/AIMS_Report_2024/coral.csv"))

all_reefs <- read_csv(here::here("data/AIMS_Enviornmental_Data/Coral-Index-Reef-Resilience_AIMS_20200522/all.reef.csv"))

# Examine structure of datasets

glimpse(coral)
summary(coral)

glimpse(all_reefs)
summary(all_reefs)

# Fill in NA values for geographic coordinates

cat("Before filling:\n")
cat("  NA LATITUDE: ", sum(is.na(coral$LATITUDE)), "\n")
cat("  NA LONGITUDE:", sum(is.na(coral$LONGITUDE)), "\n\n")

## Fill NAs using the most common coordinate per REEF × DEPTH 

## Coordinates differ slightly between depths on the same reef due to different survey spots

## Within each survey site the coordinates are essentially identical, 
## so taking the mean will still represent each site accurately

coral <- coral |>
  group_by(REEF, DEPTH) |>
  mutate(
    LATITUDE  = if_else(is.na(LATITUDE),  mean(LATITUDE,  na.rm = TRUE), LATITUDE),
    LONGITUDE = if_else(is.na(LONGITUDE), mean(LONGITUDE, na.rm = TRUE), LONGITUDE)
  ) |>
  ungroup()

## Report any rows that weren't filled 

still_na <- coral |>
  filter(is.na(LATITUDE) | is.na(LONGITUDE)) |>
  distinct(REEF, DEPTH)

cat("After filling:\n")
cat("  NA LATITUDE: ", sum(is.na(coral$LATITUDE)), "\n")
cat("  NA LONGITUDE:", sum(is.na(coral$LONGITUDE)), "\n\n")

# Set up for loop to check if all NA's are filled
if (nrow(still_na) > 0) {
  cat("The following REEF × DEPTH combinations had NO known coordinates\n")
  cat("and could not be filled (manual lookup required):\n")
  print(still_na)
} else {
  cat("All NAs successfully filled.\n")
}

## Save output to manually verify

write_csv(coral, file.path("data/AIMS_Report_2024", "coral_filled.csv"))
cat("\nSaved to coral_filled.csv\n")


# Plot reef locations to examine data
ggplot(coral) + aes(x = LONGITUDE, y = LATITUDE, color = HC ) + geom_point()

# Convert to spatially referenced data frame to account for Earth's curvature

coral <- st_as_sf(coral, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Plot reef locations over Queensalnd Coast 
library(sf)
library(tmap)
library(ozmaps)
library(dplyr)

# Load in built in data for QLD coast
qld <- ozmap_states |> filter(NAME == "Queensland")
tm_shape(qld) +
  tm_borders(col = "black", lwd = 0.8) +
  tm_fill(col = "lightgoldenrodyellow") +
  tm_layout(title = "Queensland Coast", frame = FALSE)

# Combine qld with coral
tmap_mode("view")
Coral_Cover <- tm_shape(qld, bbox = coral) + 
  tm_polygons() + 
  tm_shape(qld) + 
  tm_polygons() + 
  tm_shape(coral) + 
  # Plot coral cover as dots and set color contrast
  tm_dots(fill = "HC",
          fill.scale = tm_scale_continuous(values =  "matplotlib.yl_or_rd" )) + 
  tmap_style("natural") + #Set style of contient map
  tm_scalebar(position = c("left", "bottom")) + # add scalebar and set position
  tm_compass(position = c("left", "top"), size = 1) # add compass

Coral_Cover



# Hard coral cover by region

ggplot(coral, aes(x = NRM_REGION, y = HC)) + stat_smooth() + geom_boxplot()

ggplot(all_reefs) + aes(x = LONGITUDE, y = LATITUDE, color = HC ) + geom_point()
