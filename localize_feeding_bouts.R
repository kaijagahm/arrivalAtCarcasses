# Get 2024 feeding bouts and localize them on the map

library(tidyverse)
library(sf)
library(vultureUtils)
library(here)
source(here("R/functions.R"))

# Get the ACC data and the predictions derived from it (from classify_bouts_2024.R)
# data <- as.data.frame(data.table::fread(here("data/ACC/2024_hf_period/created/devices/202362.csv")))
pred <- as.data.frame(data.table::fread(here("data/ACC/2024_hf_period/created/predictions/202362.csv")))
pred <- distinct(pred)
nrow(pred)

## Get the GPS data to do along with the ACC data, so we can match the bouts
obj <- get_loginObject(here("movebankCredentials/pw.Rda"))
rm(pw)
from_movebank <- vultureUtils::downloadVultures(loginObject = obj,
                                           removeDup = T, dfConvert = T,
                                           quiet = T,
                                           dateTimeStartUTC = 
                                             lubridate::ymd_hms(min(data$UTC_datetime)),
                                           dateTimeEndUTC = 
                                             lubridate::ymd_hms(max(data$UTC_datetime)))

gps <- from_movebank %>%
  select(tag_id, timestamp, dateOnly, ground_speed, location_lat, location_long, individual_id, tag_local_identifier)

focal_indiv <- gps %>%
  filter(tag_local_identifier == pred$device_id[1])
dim(focal_indiv) # about 2500 GPS fixes for this individual over the course of the month.

# From ACC manuscript: "We matched the accelerometer ‘Feeding’ bouts with a GPS location using three criteria.  

nrow(pred)
gps_matches <- vector(mode = "list", length = nrow(pred))
for(i in 1:nrow(pred)){
  bout <- pred$bout_id[i]
  start_time <- pred$start[i]
  end_time <- pred$end[i]
  middle <- start_time + (end_time - start_time)/2
  # "First, if they were collected within 5 min of each other, and if the GPS ground speed was below 4m/sec (indicating the bird was not flying)."
  before_5min <- start_time-minutes(5)
  after_5min <- end_time+minutes(5)
  within_5min_gs <- focal_indiv %>%
    filter(before_5min <= timestamp & timestamp <= after_5min) %>%
    filter(ground_speed < 4)
  if(nrow(within_5min_gs) > 0){
    match <- within_5min_gs
  }else{
    #Second, if no GPS position matched these criteria, we matched ACC bouts with GPS locations if they were collected within 11 min of each other (while maintaining the ground speed criteria).
    before_11min <- start_time-minutes(11)
    after_11min <- end_time+minutes(11)
    within_11min_gs <- focal_indiv %>%
      filter(before_11min <= timestamp & timestamp <= after_11min) %>%
      filter(ground_speed < 4)
    if(nrow(within_11min_gs) > 0){
      match <- within_11min_gs
    }else{
      # Third, if no position matched the previous two filters, we used the 5 min time frame, but excluded the ground speed filter (rarely, very short feeding events may occur during the interval between two GPS locations indicating flight, or between two GPS locations when one was on the ground and the following was flying).
      within_5min <- focal_indiv %>%
        filter(before_5min <= timestamp & timestamp <= after_5min)
      if(nrow(within_5min) > 0){
        match <- within_5min
      }else{
        match <- focal_indiv[0,]
        cat("No match found\n")
      }
    }
  }
  match <- match %>%
    mutate(bout_id = bout)
  if(nrow(match) > 1){
    match <- match[which.min(abs(match$timestamp - middle)),]
  }
  gps_matches[[i]] <- match
  cat("Completed bout", i, "\n")
}

# Make sure there's just one matching GPS point for each bout
max(map_dbl(gps_matches, nrow)) # good!

# Now put these together into one data frame
matches <- purrr::list_rbind(gps_matches)

# And now we can attach the gps locations onto the predictions
joined <- left_join(pred, matches, by = c("device_id" = "tag_local_identifier", "bout_id"))
nrow(pred) == nrow(joined)

# Now find feeding bouts that we can localize
feeding_bouts <- joined %>%
  filter(pred == "Eating" & !is.na(location_lat))

# What is the level of certainty for these feeding bouts?
feeding_bouts %>%
  ggplot(aes(x = .pred_Eating))+
  geom_histogram()

# In their paper, they excluded bouts with a certainty score below 0.5, so I'll do the same
feeding_bouts <- feeding_bouts %>%
  filter(.pred_Eating > 0.5)

# Also need to exclude bouts that happened inside roost locations
# XXX do this

# Get feeding station coords, buffer them by, say, 50m, and classify points as station or non-station.
fs <- readxl::read_excel(here("data/FeedingData from 2018_2024_Translated.xlsx")) %>%
  select(contains("LONG") | contains("LAT")) %>%
  rename("itmLong" = `ITM - LONG`,
         "itmLat" = `ITM - LAT`,
         "long" = `WGS84 - LONG`,
         "lat" = `WGS84 - LAT`) %>%
  select(long, lat, itmLong, itmLat) %>%
  distinct() %>%
  st_as_sf(coords = c("long", "lat"), crs = "WGS84") %>%
  st_transform(32636)
mapview(fs)
# This has a lot of repeats, but let's just use it for now. Going to buffer all of these by 100m and exclude anything that falls within the buffer.
fs_buffered <- fs %>%
  st_buffer(dist = 100)
fs_union <- st_union(fs_buffered) %>%
  st_transform("WGS84")

as.numeric(st_intersects(feeding_bouts, fs_union))
# This suggests that the first 5 bouts are non-station. Let's see if that makes sense
mapview(feeding_bouts[1:5,]) + mapview(fs_buffered, col.regions = "red", col = "red") # indeed it looks like they don't overlap.

# Bouts 28-40 should all be feeding station bouts. Let's check those.
mapview(feeding_bouts[28:40,]) + mapview(fs_buffered, col.regions = "red", col = "red") # yes indeed, those fall right in the middle of a feeding station! This is awesome.

# Add this as a designation:
feeding_bouts$station <- !is.na(as.numeric(st_intersects(feeding_bouts, fs_union)))
table(feeding_bouts$station) # as expected, most of these happen at the stations, but there are also a bunch of non-station bouts.
mapview(fs_buffered, col = "red", col.regions = "red") + mapview(feeding_bouts, zcol = "station") 

# Brilliant! Apart from excluding the roosts, we have ourselves a classification pipeline.

# The next step is going to be to match these feeding bouts with particular carcasses. But first, let's make this into a full pipeline so we can classify everything. 
