# Test script for making animations
library(ggplot2)
library(ggmap)
library(move2)
library(moveVis)
library(targets)
library(tidyverse)
library(sf)

# Let's just get one day of data
tar_load(hires_tags)

test <- hires_tags %>% filter(dateOnly == lubridate::date("2023-08-03"))
length(unique(test$Nili_id)) # looks like all individuals were tracked on that day, hmm.
set.seed(3)
vultures <- sample(unique(test$Nili_id), 10)
vultures <- vultures[vultures != "kat"]
test <- test %>% filter(Nili_id %in% vultures)
dim(test) # okay, this is hopefully a manageable amount of data.

move <- moveVis::df2move(df = test, proj = "WGS84", x = "location_long", y = "location_lat", time = "timestamp", track_id = "Nili_id")

m <- align_move(move, res = 10, unit = "mins")

frames <- frames_spatial(m, path_colours = c("red", "orange", "yellow", "green", "blue", "purple", "brown", "gray", "pink"),
                         map_service = "osm_stamen", 
                         map_type = "terrain_bg",
                         map_token = "a047c304-af0d-45c7-89ed-c712362070d8",
                         alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(type = "label") %>%
  add_progress()

animate_frames(frames, out_file = "test.gif", fps = 2, overwrite = T)

