# Summary data for carcasses and bouts
# SETUP; load and prep data
library(targets)
library(tidyverse)
library(here)
library(lubridate)
library(ggmap)
library(sf)
library(viridis)
library(mapview)
library(ggspatial)
library(readr)
source(here("R/functions.R"))
library(patchwork)
library(spatsoc)

## INPA CARCASS DATA
carcasses_inpa <- readRDS(here("data/created/carcasses_inpa.RDS"))
stations <- readRDS(here("data/created/stations.RDS"))

## FEEDING BOUT DATA
bouts <- readRDS(here("data/created/bouts.RDS"))

## RESTRICT TO HF-ACC PERIODS
### Bouts
#### Feeding bouts are already restricted to this time interval.


### Identify carcasses that do and don't have associated feeding bouts
without_bouts <- carcasses_focal %>% filter(!(carcass_id %in% carcass_bouts_df$carcass_id))
with_bouts <- carcasses_focal %>% filter(carcass_id %in% carcass_bouts_df$carcass_id)

carcasses_bouts <- left_join(carcass_bouts_df, with_bouts, by = "carcass_id")
length(unique(carcasses_bouts$carcass_id)) # 28 unique carcasses
nrow(carcasses_bouts) == nrow(carcass_bouts_df) # same--good.
carcasses_bouts <- bind_rows(carcasses_bouts, without_bouts) # add on the carcasses that have no associated bouts
nrow(carcasses_bouts)
length(unique(carcasses_bouts$carcass_id)) == nrow(carcasses_focal) # same--good.

write_rds(carcasses_bouts, here("data/created/carcasses_bouts.RDS"))
carcasses_bouts <- readRDS(here("data/created/carcasses_bouts.RDS"))

## CARCASS-LEVEL INFORMATION/STATS
carcass_stats <- carcasses_bouts %>%
  st_drop_geometry() %>%
  # making sf object BY CARCASS
  st_as_sf(coords = c("long", "lat"), crs = "WGS84") %>%
  st_transform(32636) %>%
  group_by(carcass_id, datetime) %>%
  summarize(bouts = sum(!is.na(boutID)),
            indivs = length(unique(individual_id[!is.na(individual_id)]))) %>%
  mutate(year = lubridate::year(datetime)) %>%
  bind_cols(st_coordinates(.)) %>%
  arrange(bouts)
write_rds(carcass_stats, here("data/created/carcass_stats.RDS"))