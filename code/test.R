---
#  title:  MB5370 Presentation - Data Wrangling & Mapping
#  author: Ryan Waln
#  date:   `r format(Sys.time(), '%d %B, %Y')`
---

# Introduction

  # This file formats AIMS coral data to produce:
#   Figure 1 – coral.csv   (AIMS Report 2024,  surveys up to 2024)
#   Figure 2 – all.reef.csv (AIMS Resilience DB, surveys up to 2017)
#
# Both maps show each reef × depth site as a dot coloured by the
# LATEST hard coral cover (HC), with a label showing the average
# annual change in HC across all sampling periods.

# ── Housekeeping ──────────────────────────────────────────────
  rm(list = ls())

# ── Libraries ─────────────────────────────────────────────────
library(here)
library(tidyverse)
library(sf)
library(tmap)
library(ozmaps)   # built-in QLD coastline — no file download needed


# ══════════════════════════════════════════════════════════════
#  1.  LOAD DATA
# ══════════════════════════════════════════════════════════════

coral     <- read_csv(here("data/AIMS_Report_2024/coral.csv"))
all_reefs <- read_csv(here("data/AIMS_Enviornmental_Data/Coral-Index-Reef-Resilience_AIMS_20200522/all.reef.csv"))


# ══════════════════════════════════════════════════════════════
#  2.  FILL MISSING COORDINATES IN coral
# ══════════════════════════════════════════════════════════════
# Coordinates differ slightly between depth strata on the same
# reef (different survey spots), so we group by REEF × DEPTH
# and fill NAs with the mean of the known values in that group.

cat("── coral: coordinate fill ──────────────────\n")
cat("Before  NA LATITUDE :", sum(is.na(coral$LATITUDE)),  "\n")
cat("Before  NA LONGITUDE:", sum(is.na(coral$LONGITUDE)), "\n")

coral <- coral |>
  group_by(REEF, DEPTH) |>
  mutate(
    LATITUDE  = if_else(is.na(LATITUDE),  mean(LATITUDE,  na.rm = TRUE), LATITUDE),
    LONGITUDE = if_else(is.na(LONGITUDE), mean(LONGITUDE, na.rm = TRUE), LONGITUDE)
  ) |>
  ungroup()

still_na <- coral |>
  filter(is.na(LATITUDE) | is.na(LONGITUDE)) |>
  distinct(REEF, DEPTH)

cat("After   NA LATITUDE :", sum(is.na(coral$LATITUDE)),  "\n")
cat("After   NA LONGITUDE:", sum(is.na(coral$LONGITUDE)), "\n")

if (nrow(still_na) > 0) {
  cat("Sites with NO known coordinates (excluded from map):\n")
  print(still_na)
} else {
  cat("All NAs filled.\n")
}


# ══════════════════════════════════════════════════════════════
#  3.  MERGE COORDINATES INTO all_reefs
# ══════════════════════════════════════════════════════════════
# all_reefs has no coordinate columns — borrow them from coral
# using REEF × DEPTH as the lookup key.

coord_lookup <- coral |>
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) |>
  group_by(REEF, DEPTH) |>
  summarise(LATITUDE  = mean(LATITUDE),
            LONGITUDE = mean(LONGITUDE),
            .groups = "drop")

all_reefs <- all_reefs |>
  left_join(coord_lookup, by = c("REEF", "DEPTH"))

cat("\n── all_reefs: coordinate merge ─────────────\n")
cat("Remaining NA LATITUDE :", sum(is.na(all_reefs$LATITUDE)),  "\n")
cat("(Middle Rf LTMP has no coordinates in either dataset)\n")


# ══════════════════════════════════════════════════════════════
#  4.  BUILD PER-SITE SUMMARY TABLES
# ══════════════════════════════════════════════════════════════
# For each dataset we need one row per reef × depth site with:
#   • latest_HC       – HC value from the most recent survey visit
#   • avg_HC_change   – linear slope of HC over time (% / year)
#   • label           – formatted string for the map annotation

## ── Helper: linear slope ─────────────────────────────────────
lm_slope <- function(x, y) {
  keep <- !is.na(x) & !is.na(y)
  if (sum(keep) < 2) return(NA_real_)
  coef(lm(y[keep] ~ x[keep]))[2]
}


## ── 4a.  coral summary ───────────────────────────────────────
coral_summary <- coral |>
  filter(!is.na(LATITUDE)) |>                    # drop unfillable sites
  group_by(REEF, DEPTH, LATITUDE, LONGITUDE, NRM_REGION) |>
  summarise(
    latest_HC     = HC[which.max(replace(yr, is.na(VISIT_NO), NA))],
    avg_HC_change = lm_slope(yr, HC),
    .groups = "drop"
  ) |>
  mutate(
    label = paste0(if_else(avg_HC_change >= 0, "+", ""),
                   round(avg_HC_change, 1), "% yr⁻¹")
  )


## ── 4b.  all_reefs summary ───────────────────────────────────
# all_reefs has no year column — parse it from Date first.
all_reefs <- all_reefs |>
  mutate(yr = year(dmy(Date)))

all_reefs_summary <- all_reefs |>
  filter(!is.na(LATITUDE)) |>                    # drop Middle Rf LTMP
  group_by(REEF, DEPTH, LATITUDE, LONGITUDE, NRM_REGION) |>
  summarise(
    latest_HC     = HC[which.max(yr)],
    avg_HC_change = lm_slope(yr, HC),
    .groups = "drop"
  ) |>
  mutate(
    label = paste0(if_else(avg_HC_change >= 0, "+", ""),
                   round(avg_HC_change, 1), "% yr⁻¹")
  )


# ══════════════════════════════════════════════════════════════
#  5.  CONVERT TO sf OBJECTS
# ══════════════════════════════════════════════════════════════

coral_sf <- st_as_sf(coral_summary,
                     coords = c("LONGITUDE", "LATITUDE"),
                     crs    = 4326)

all_reefs_sf <- st_as_sf(all_reefs_summary,
                         coords = c("LONGITUDE", "LATITUDE"),
                         crs    = 4326)


# ══════════════════════════════════════════════════════════════
#  6.  LOAD QLD COASTLINE
# ══════════════════════════════════════════════════════════════

qld <- ozmap_states |> filter(NAME == "Queensland")


# ══════════════════════════════════════════════════════════════
#  7.  SHARED MAP SETTINGS
# ══════════════════════════════════════════════════════════════

hc_scale <- tm_scale_continuous(
  values   = "matplotlib.yl_or_rd",
  limits   = c(0, 100)              # fix colour scale across both maps
)

tmap_mode("view")   # interactive; switch to "plot" for static export


# ══════════════════════════════════════════════════════════════
#  8.  FIGURE 1 — AIMS Report 2024  (coral.csv)
# ══════════════════════════════════════════════════════════════

fig1 <- tm_shape(qld, bbox = coral_sf) +
  tm_polygons(col = "lightgoldenrodyellow", border.col = "grey40", lwd = 0.6) +
  tm_shape(coral_sf) +
  tm_dots(
    fill        = "latest_HC",
    fill.scale  = hc_scale,
    fill.legend = tm_legend(title = "Hard Coral Cover (%)"),
    size        = 0.3
  ) +
  tm_text(
    text  = "label",
    size  = 0.55,
    xmod  = 0.4,           # nudge label to the right of the dot
    col   = "black"
  ) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_compass(position  = c("left", "top"), size = 1) +
  tm_title("Hard Coral Cover — AIMS Report 2024\n(dot = latest survey; label = avg annual change)") +
  tmap_style("natural")

fig1


# ══════════════════════════════════════════════════════════════
#  9.  FIGURE 2 — AIMS Resilience DB  (all_reefs.csv)
# ══════════════════════════════════════════════════════════════

fig2 <- tm_shape(qld, bbox = all_reefs_sf) +
  tm_polygons(col = "lightgoldenrodyellow", border.col = "grey40", lwd = 0.6) +
  tm_shape(all_reefs_sf) +
  tm_dots(
    fill        = "latest_HC",
    fill.scale  = hc_scale,
    fill.legend = tm_legend(title = "Hard Coral Cover (%)"),
    size        = 0.3
  ) +
  tm_text(
    text  = "label",
    size  = 0.55,
    xmod  = 0.4,
    col   = "black"
  ) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_compass(position  = c("left", "top"), size = 1) +
  tm_title("Hard Coral Cover — AIMS Resilience DB (to 2017)\n(dot = latest survey; label = avg annual change)") +
  tmap_style("natural")

fig2


# ══════════════════════════════════════════════════════════════
#  10. OPTIONAL — EXPORT STATIC PNGs
# ══════════════════════════════════════════════════════════════

# tmap_mode("plot")
# tmap_save(fig1, here("outputs/fig1_coral_cover_2024.png"), width = 8, height = 12, dpi = 300)
# tmap_save(fig2, here("outputs/fig2_coral_cover_resilience.png"), width = 8, height = 12, dpi = 300)