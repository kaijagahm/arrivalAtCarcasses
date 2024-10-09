library(tidyverse)
library(here)
library(sf)
library(mapview)

tar_load(stations)
tar_load(carcasses_inpa)
tar_load(carcasses_focal)
tar_load(feeding_bouts)
tar_load(all_bouts_annotated)
tar_load(all_carcasses_annotated)
source(here("R/functions.R"))

aba <- sf::st_as_sf(all_bouts_annotated)
aca <- sf::st_as_sf(all_carcasses_annotated)

## BOUNDING BOXES FOR MAPPING
### AREA OF ALL FEEDING BOUTS IN THE HF-ACC PERIODS
bbox_bouts_hf <- st_bbox(feeding_bouts)
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
  mapview(st_buffer(st_crop(aca %>% filter(nIndivs > 1 |  # only including wild carcasses that have more than one individual
                                             is.na(nIndivs)), bbox_south), 500), 
          zcol = "carcType",
          layer.name = "Carcasses",
          col.regions = c("lightblue", "pink"))+
  mapview(st_buffer(st_crop(aba, bbox_south), 10), 
          zcol = "carcType",
          layer.name = "Feeding bouts",
          col.regions = c("blue", "red"))

# Now let's map all the wild carcasses, by time and number of bouts
aca %>%
  dg() %>% # shortcut for st_drop_geometry defined in functions.R
  group_by(year, carcType) %>%
  summarize(n = n()) # This is so far off that it suggests our definitions still are not stringent enough.
# Let's look at how many of these wild "carcasses" are visited by more than one individual. 
# Also need to give it a 48 hour threshold instead of 24 hours.

st_crop(aca, bbox_south) %>%
  ggplot()+
  annotation_map_tile(zoom = 9, type = "cartolight")+
  geom_sf(aes(col = carcType))

st_crop(aca, bbox_south) %>%
  filter(carcType == "wild") %>%
  ggplot()+
  annotation_map_tile(zoom = 9, type = "cartolight")+
  geom_sf(aes(col = nIndivs))+
  scale_color_viridis()

st_crop(aca, bbox_south) %>%
  filter(carcType == "wild") %>%
  ggplot()+
  annotation_map_tile(zoom = 9, type = "cartolight")+
  geom_sf(aes(col = nBouts))+
  scale_color_viridis()

st_crop(aca, bbox_south) %>%
  filter(carcType == "wild") %>%
  ggplot()+
  annotation_map_tile(zoom = 9, type = "cartolight")+
  geom_sf(aes(col = nIndivs, size = nBouts), alpha = 0.5)+
  scale_color_viridis()

# How many of these are just a single individual?
aca %>%
  filter(carcType == "wild") %>%
  ggplot(aes(x = nIndivs))+
  geom_histogram()+
  theme_classic()

aca %>%
  filter(carcType == "wild") %>%
  ggplot(aes(x = nIndivs, y = nBouts))+
  geom_point(alpha = 0.1, size = 2)+
  theme_classic()
 # grouping in space and time is actually really not trivial!

aca %>%
  filter(carcType == "wild", nBouts < 50, nIndivs < 10) %>%
  ggplot(aes(x = nIndivs, y = nBouts))+
  geom_point(alpha = 0.1, size = 2)+
  theme_classic() # yeah there are just no obvious cutoff points.

# 2024-10-09 At this point I emailed Gideon to ask how he's done this in the past, since I'm sure he has some criteria and I don't want to reinvent the wheel.
