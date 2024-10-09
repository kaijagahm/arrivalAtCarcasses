library(tidyverse)
library(here)
library(sf)
library(mapview)

bouts <- readRDS(here("data/created/feeding_bouts.RDS"))
carcass_bouts <- readRDS(here("data/created/carcass_bouts.RDS"))
carcasses_inpa <- readRDS(here("data/created/carcasses_inpa.RDS"))
carcasses_focal <- readRDS(here("data/created/carcasses_focal.RDS"))
carcasses_bouts <- readRDS(here("data/created/carcasses_bouts.RDS"))
carcass_stats <- readRDS(here("data/created/carcass_stats.RDS"))
ns_bouts <- readRDS(here("data/created/ns_bouts.RDS"))
ns_carcasses <- readRDS(here("data/created/ns_carcasses.RDS"))
stations <- readRDS(here("data/created/stations.RDS"))
tar_load(all_bouts_annotated)
tar_load(all_carcasses_annotated)

aba <- sf::st_as_sf(all_bouts_annotated)
aca <- sf::st_as_sf(all_carcasses_annotated)

## BOUNDING BOXES FOR MAPPING
### AREA OF ALL FEEDING BOUTS IN THE HF-ACC PERIODS
bbox_bouts_hf <- st_bbox(bouts)
### AREA OF ALL INPA CARCASSES 2018-2024
bbox_inpa_carcasses <- st_bbox(carcasses_inpa)
### AREA OF ALL INPA CARCASSES IN HF-ACC PERIODS
bbox_inpa_carcasses_hf <- st_bbox(carcasses_focal)
### SOUTHERN REGION ONLY
bbox_south <- bbox_inpa_carcasses_hf
bbox_south[4] <- 3500000
bbox_south[2] <- 3350000

# Map with the following
# 1. Known feeding station coordinates, buffered by 500m
# 2. Feeding bout locations, buffered by 1m so they show up normal size, colored by station vs. not
# 3. Known carcass depositions (blue)
# 4. Non-station feeding events identified from feeding bouts, buffered so they show up

mapview(st_buffer(st_crop(stations, bbox_south), 500), 
        col.regions = "gray30",
        layer.name = "Feeding stations")+
  mapview(st_buffer(st_crop(aca, bbox_south), 500), 
          zcol = "carcType",
          layer.name = "Carcasses",
          col.regions = c("lightblue", "pink"))+
  mapview(st_buffer(st_crop(aba, bbox_south), 10), 
          zcol = "carcType",
          layer.name = "Feeding bouts",
          col.regions = c("blue", "red"))
