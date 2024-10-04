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

### Alternative to feeding stations: create "feeding stations" by finding clusters of carcasses. But how do we deal with the fact that those clusters will shift over time?
carcasses_inpa_buffered <- st_buffer(carcasses_inpa, 500)
parts <- st_cast(st_union(carcasses_inpa_buffered),"POLYGON")
clust <- unlist(st_intersects(carcasses_inpa_buffered, parts))
diss <- cbind(carcasses_inpa_buffered, clust)
stations_inferred <- diss %>%
  group_by(clust) %>%
  summarize(geometry = st_union(geometry)) %>%
  st_centroid() %>%
  ungroup() %>%
  bind_cols(st_coordinates(.))
mapview(stations_inferred)+mapview(stations, col.regions = "red")

ggplot(st_crop(diss, bbox_south))+
  annotation_map_tile(zoom = 9, type = "cartolight") +
  geom_sf(aes(col = factor(clust)))+
  coord_sf(datum = st_crs(32636))+
  annotation_scale()+
  theme(legend.position = "none")


## FEEDING BOUT DATA
bouts <- readRDS(here("data/created/bouts.RDS"))

## RESTRICT TO HF-ACC PERIODS
### Bouts
#### Feeding bouts are already restricted to this time interval.

### Carcasses
minmax_dates <- readRDS(here("data/created/minmax_dates.RDS"))
carcasses_23 <- carcasses_inpa %>% filter(datetime >= minmax_dates[1], datetime <= minmax_dates[2])
carcasses_24 <- carcasses_inpa %>% filter(datetime >= minmax_dates[3], datetime <= minmax_dates[4])
carcasses_focal <- bind_rows(carcasses_23, carcasses_24)
nrow(carcasses_focal) #96 feeding occurrences during the high-frequency ACC periods.
write_rds(carcasses_focal, here("data/created/carcasses_focal.RDS"))

## MATCH FEEDING BOUTS TO CARCASSES
dist_m <- 750 # bouts within 750 meters of the carcass
hours_before <- 1 # how many hours before the carcass was deposited?
hours_after <- 48 # how many hours after the carcass was deposited?

carcass_bouts <- map(1:nrow(carcasses_focal), ~{
  carcass <- carcasses_focal[.x,]
  id <- carcasses_focal$carcass_id[.x]
  distances <- as.numeric(st_distance(bouts, carcass))
  bouts <- bouts %>%
    mutate(carcass_id = id,
           dist_to_carcass = distances)
  keep_distance <- bouts %>%
    filter(dist_to_carcass <= dist_m)
  keep_time <- keep_distance %>%
    filter(start >= (carcass$datetime - hours(hours_before)), 
           end <= (carcass$datetime + hours(hours_after))) %>% 
    mutate(time_since_carcass = difftime(start, carcass$datetime, units = "hours"))
  return(keep_time)
}) 
map_dbl(carcass_bouts, nrow)

carcass_bouts <- map(carcass_bouts, ~.x %>% mutate(boutID = paste(device_id, bout_id, sep = "_")) %>%
                       relocate(boutID, .after = "device_id"))
write_rds(carcass_bouts, here("data/created/carcass_bouts.RDS"))
carcass_bouts <- readRDS(here("data/created/carcass_bouts.RDS"))

### Combine the carcasses and bouts into a single data frame containing info for both, including carcasses with no bouts.
carcass_bouts_df <- purrr::list_rbind(carcass_bouts)

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

## IDENTIFY/DETECT NON-STATION CARCASSES FROM FEEDING BOUTS
ns_bouts <- bouts %>%
  filter(station == FALSE) %>%
  arrange(timestamp)

### Prepare data for analysis with spatsoc
ns_bouts$timestamp <- as.POSIXct(ns_bouts$timestamp)
ns_bouts <- data.table::data.table(ns_bouts)

group_times(ns_bouts, datetime = 'timestamp', threshold = '24 hours') # group into 24 hour groups
group_pts(ns_bouts, threshold = 100, id ='device_id', coords = c('X', 'Y'), timegroup = 'timegroup')

# convert back to sf object for mapping
ns_bouts <- as.data.frame(ns_bouts) %>%
  st_as_sf(crs = 32636)
write_rds(ns_bouts, here("data/created/ns_bouts.RDS"))

ns_carcasses <- ns_bouts %>%
  group_by(year, group) %>%
  summarize(geometry = st_union(geometry),
            dateOnly = dateOnly[1]) %>%
  st_centroid() %>%
  ungroup() %>%
  bind_cols(st_coordinates(.))
write_rds(ns_bouts, here("data/created/ns_carcasses.RDS"))