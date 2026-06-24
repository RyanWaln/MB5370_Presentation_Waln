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




rm(list = ls())   # clear the R environment of any pre-existing objects
objects()         # confirm the environment is empty (returns character(0))


# ══════════════════════════════════════════════════════════════
#  LIBRARIES
# ══════════════════════════════════════════════════════════════

library(here)       
library(tidyverse)  
library(sf)         
library(tmap)       


# ══════════════════════════════════════════════════════════════
#    LOAD DATA
# ══════════════════════════════════════════════════════════════

# all_reef.csv – AIMS Resilience database; contains HC time-series but NO coordinates
all_reefs <- read_csv(
  here("data/AIMS_Enviornmental_Data/Coral-Index-Reef-Resilience_AIMS_20200522/all.reef.csv"))

# coral.csv  – AIMS 2024 report data; contains geographic coordinates of reefs
coral <- read_csv(here("data/AIMS_Report_2024/coral.csv"))

# ══════════════════════════════════════════════════════════════
#    FILL MISSING COORDINATES IN coral
# ══════════════════════════════════════════════════════════════
# Two Sample sites per reef at different depths
# Because a given reef's coordinates are essentially the same at each visit,
# can fill each NA with the mean coordinate for that REEF × DEPTH combination.


cat("── coral: coordinate fill ──────────────────\n")
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
  distinct(REEF, DEPTH)             # one row per unfillable site

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
#    MERGE COORIDINATES INTO all_reefs
# ══════════════════════════════════════════════════════════════
# all_reefs has no geographic coordinates, so must merge with coral dataset.
# Compute the mean lat/lon per REEF × DEPTH from coral, then join to all_reefs.

coord_lookup <- coral |>
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) |>   # exclude any NA values
  group_by(REEF, DEPTH) |>
  summarise(
    LATITUDE  = mean(LATITUDE),    # average lat across all site visits
    LONGITUDE = mean(LONGITUDE),   # average lon across all site visits
    .groups = "drop"
  )

coord_lookup #Check to make sure data is ordered correctly

# Join the coordinate lookup onto all_reefs by matching REEF and DEPTH
all_reefs <- all_reefs |>
  left_join(coord_lookup, by = c("REEF", "DEPTH"))

cat("\n── all_reefs: coordinate merge ─────────────\n")
cat("Remaining NA LATITUDE :", sum(is.na(all_reefs$LATITUDE)), "\n")
cat("(Any reef absent from coral will have no coordinates after this join)\n")

# ══════════════════════════════════════════════════════════════
#    EXTRACT YEAR FROM all_reefs
# ══════════════════════════════════════════════════════════════
# The Date column in all_reefs is stored as DD/MM/YYYY 
# Sampling occurs once a year so must extract the numeric year.

all_reefs <- all_reefs |>
  mutate(yr = year(dmy(Date)))   # dmy() breaks up day-month-year strings; year() extracts year

# ══════════════════════════════════════════════════════════════
#    CALCULATE AVERAGE CHANGE IN HC (hard coral cover)
# ══════════════════════════════════════════════════════════════
# This function returns the slope from a simple linear regression y ~ x.
# Used to estimate the average annual change in HC.

lm_slope <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)          # Only use rows where both x and y are valid to prevent NA errors
  if (sum(keep) < 2) return(NA_real_)    # need at least 2 points to fit a line
  coef(lm(y[keep] ~ x[keep]))[2]         # [2] selects the slope coefficient
}


# ══════════════════════════════════════════════════════════════
#    FIND STARING YEAR FOR MONITORING
# ══════════════════════════════════════════════════════════════
# Need to find earliest year where EVERY reef site has a measurement

n_sites <- all_reefs |>
  filter(!is.na(LATITUDE),!is.na(LONGITUDE)) |>               # exclude sites with no coordinates
  distinct(REEF, DEPTH) |>
  nrow()                                    # total number of valid sites

baseline_year <- all_reefs |>
  filter(!is.na(LATITUDE),!is.na(LONGITUDE), !is.na(HC)) |>  # drop sites with missing HC and Coordinates
  group_by(yr) |>
  summarise(
    sites_with_data = n_distinct(paste(REEF, DEPTH))  # count distinct sites in each year
  ) |>
  filter(sites_with_data == n_sites) |>     # keep only years where ALL sites are present
  slice_min(yr, n = 1) |>                  # pick the earliest year
  pull(yr)                                  # extract as a plain numeric value

cat("\nBaseline year (first year all sites measured):", baseline_year, "\n")


# ══════════════════════════════════════════════════════════════
#    BUILD SUMMARY TABLES FOR all_reefs TO USE FOR PLOTTING
# ══════════════════════════════════════════════════════════════
#  Create three per-site summaries from all_reefs:
#   earliest_HC  – HC in the starting year
#   latest_HC    – HC in the most recent year
#   avg_HC_change – calculated w/linear slope of HC over the whole sampling period

all_reefs_summary <- all_reefs |>
  filter(!is.na(LATITUDE),!is.na(LONGITUDE)) |>              # drop sites with no coordinates
  group_by(REEF, DEPTH, LATITUDE, LONGITUDE, NRM_REGION) |>
  summarise(
    # Earliest HC
    earliest_HC   = HC[yr == baseline_year][1],
    
    # Latest HC
    latest_HC     = HC[which.max(yr)],
    
    # Average annual change
    avg_HC_change = lm_slope(yr, HC),
    
    .groups = "drop"
  ) |>
  mutate(
    # Add columns for colouring scheme in Fig 3 to help denote HC change
    change_dir = case_when(
      avg_HC_change > 0  ~ "Increase",
      avg_HC_change < 0  ~ "Decrease",
      avg_HC_change == 0 ~ "No change",
      TRUE               ~ NA_character_   # catches any remaining NAs
    ),
    
    # Absolute magnitude of change (used for dot SIZE in Fig 3)
    abs_change = abs(avg_HC_change),
    
    # Formatted text label showing the average change while displaying label
    label = paste0(
      if_else(avg_HC_change >= 0, "+", ""),  # add "+" for positive slopes
      round(avg_HC_change, 1),
      "% yr⁻¹"
    )
  )

all_reefs_summary # Check to spot any errors

# ══════════════════════════════════════════════════════════════
#    CONVERT SUMMARY TABLE TO sf SPATIAL OBJECT
# ══════════════════════════════════════════════════════════════
# use st_as_sf() to turn data frame into a spatial object to account for earth's curvature; we tell it which
# columns hold coordinates and what CRS the coordinates are in (4326 = WGS84).

all_reefs_sf <- st_as_sf(
  all_reefs_summary,
  coords = c("LONGITUDE", "LATITUDE"),  
  crs    = 4326                          # WGS84 geographic coordinate system
)


# ══════════════════════════════════════════════════════════════
#    LOAD QLD COASTLINE FROM GEOPACKAGE
# ══════════════════════════════════════════════════════════════
# We read the QLD coastline from a local GeoPackage (.gpkg) file.
# st_read() loads the first layer by default; specify layer = "..." if needed.
# We then transform to WGS84 (EPSG:4326) so the CRS matches our reef points.

qld <- st_read(here("data/QLD_Coastline/data.gpkg")) |>
  st_transform(crs = 4326)   # reproject to WGS84 to match reef point data


# ══════════════════════════════════════════════════════════════
#   SHARED MAP SETTINGS
# ══════════════════════════════════════════════════════════════

tmap_mode("plot")   # "plot" produces static PNG/PDF exports; use "view" for interactive HTML

# Bounding box derived from the reef points – used to frame all three maps
bbox_reefs <- st_bbox(all_reefs_sf)

# Common land polygon style applied to every figure
land_style <- list(
  col        = "lightgoldenrodyellow",  # fill colour for the Queensland land mass
  border.col = "grey40",                # outline colour
  lwd        = 0.6                      # outline line width
)

# ══════════════════════════════════════════════════════════════
#   FIGURE 1 – EARLIEST HC COVER
#      (first year where all reefs have a measurement)
# ══════════════════════════════════════════════════════════════

fig1 <- tm_shape(qld, bbox = bbox_reefs) +          # draw QLD coastline, cropped to reef extent
  tm_polygons(
    col        = land_style$col,        # land fill
    border.col = land_style$border.col, # border colour
    lwd        = land_style$lwd         # border width
  ) +
  tm_shape(all_reefs_sf) +             # overlay the reef point layer
  tm_dots(
    fill        = "earliest_HC",        # colour dots by earliest HC value
    fill.scale  = tm_scale_continuous(
      values = "matplotlib.yl_or_rd",   # yellow-orange-red palette (low to high cover)
      limits = c(0, 100)                # fix scale from 0–100 % so all figures are comparable
    ),
    fill.legend = tm_legend(
      title = paste0("Hard Coral Cover (%) in ", baseline_year)  # dynamic title with year
    ),
    size = 0.35                         # dot size
  ) +
  tm_scalebar(position = c("left", "bottom")) +   # scale bar in the bottom-left corner
  tm_compass(position  = c("left", "top"), size = 1) +  # north arrow in the top-left
  tm_title(paste0(
    "Earliest Hard Coral Cover — All Reefs (", baseline_year, ")\n",
    "(first year with measurements at every site)"
  )) +
  tmap_style("natural")               # natural background style (light ocean colour)

fig1   # display the figure


# ══════════════════════════════════════════════════════════════
#  FIGURE 2 – LATEST HC COVER
# ══════════════════════════════════════════════════════════════

fig2 <- tm_shape(qld, bbox = bbox_reefs) +
  tm_polygons(
    col        = land_style$col,
    border.col = land_style$border.col,
    lwd        = land_style$lwd
  ) +
  tm_shape(all_reefs_sf) +
  tm_dots(
    fill        = "latest_HC",          # colour dots by the most recent HC value
    fill.scale  = tm_scale_continuous(
      values = "matplotlib.yl_or_rd",
      limits = c(0, 100)
    ),
    fill.legend = tm_legend(title = "Hard Coral Cover (%) — Latest Survey"),
    size = 0.35
  ) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_compass(position  = c("left", "top"), size = 1) +
  tm_title(
    "Latest Hard Coral Cover — All Reefs\n(most recent survey year per site)"
  ) +
  tmap_style("natural")

fig2


# ══════════════════════════════════════════════════════════════
#   FIGURE 3 – AVERAGE RATE OF CHANGE IN HC
#      Dot SIZE  ∝ magnitude of change (|% yr⁻¹|)
#      Dot COLOUR encodes direction AND degree:
#        Dark colours  → stronger decrease (negative slope)
#        Light colours → stronger increase (positive slope)
# ══════════════════════════════════════════════════════════════
# Strategy:
#   • Split into decrease (avg_HC_change < 0) and increase (avg_HC_change ≥ 0) subsets
#   • Map the raw slope to a diverging colour palette using tm_scale_continuous()
#     with a midpoint at 0; dark-red = large decrease, light-yellow/green = large increase

fig3 <- tm_shape(qld, bbox = bbox_reefs) +
  tm_polygons(
    col        = land_style$col,
    border.col = land_style$border.col,
    lwd        = land_style$lwd
  ) +
  tm_shape(all_reefs_sf) +
  tm_dots(
    # Colour encodes avg_HC_change; negative = dark, positive = light
    fill       = "avg_HC_change",
    fill.scale = tm_scale_continuous(
      values  = "brewer.rd_yl_gn",   # Red (decrease) → Yellow (≈0) → Green (increase)
      midpoint = 0,                  # anchor the colour midpoint at zero change
      limits  = c(                   # symmetric limits centred on 0 for fair comparison
        -max(abs(all_reefs_summary$avg_HC_change), na.rm = TRUE),
        max(abs(all_reefs_summary$avg_HC_change), na.rm = TRUE)
      )
    ),
    fill.legend = tm_legend(title = "Avg HC Change\n(% yr⁻¹)\nDark = decrease\nLight = increase"),
    
    # Dot size is proportional to the ABSOLUTE rate of change
    size       = "abs_change",
    size.scale = tm_scale_continuous(
      limits = c(0, max(all_reefs_summary$abs_change, na.rm = TRUE))
    ),
    size.legend = tm_legend(title = "|Change| (% yr⁻¹)")
  ) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_compass(position  = c("left", "top"), size = 1) +
  tm_title(
    "Average Annual Change in Hard Coral Cover — All Reefs\n(size = magnitude; colour = direction & degree of change)"
  ) +
  tmap_style("natural")

fig3


# ══════════════════════════════════════════════════════════════
#  SAVE ALL THREE FIGURES AS HIGH-RESOLUTION PNGs
# ══════════════════════════════════════════════════════════════

# Width = 8 inches, Height = 12 inches, DPI = 300 → publication-quality output

tmap_save(
  fig1,
  here("outputs/fig1_earliest_HC.png"),
  width = 8, height = 12, dpi = 300
)

tmap_save(
  fig2,
  here("outputs/fig2_latest_HC.png"),
  width = 8, height = 12, dpi = 300
)

tmap_save(
  fig3,
  here("outputs/fig3_HC_change.png"),
  width = 8, height = 12, dpi = 300
)



