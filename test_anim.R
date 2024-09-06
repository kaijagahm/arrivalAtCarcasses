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

test <- hires_tags %>% filter(dateOnly_il == lubridate::date("2023-08-03"))
test2 <- hires_tags %>% filter(dateOnly_il %in% c(lubridate::date("2023-06-13"), lubridate::date("2023-06-14"), lubridate::date("2023-06-15")))
length(unique(test$Nili_id)) # looks like all individuals were tracked on that day, hmm.
length(unique(test2$Nili_id))
set.seed(3)
# vultures <- sample(unique(test$Nili_id), 10)
# vultures <- vultures[vultures != "kat"]
# test <- test %>% filter(Nili_id %in% vultures)
dim(test) # we're going to attempt to animate all of this... maybe I will regret it.
dim(test2)

testtest <- test
testtest <- testtest %>%
  group_by(Nili_id) %>%
  filter(n() > 2) %>%
  ungroup()

test2 <- test2 %>%
  group_by(Nili_id) %>%
  filter(n() > 2) %>%
  ungroup()

move <- moveVis::df2move(df = testtest, proj = "WGS84", x = "location_long", y = "location_lat", time = "timestamp_il", track_id = "Nili_id")
move2 <- moveVis::df2move(df = test2, proj = "WGS84", x = "location_long", y = "location_lat", time = "timestamp_il", track_id = "Nili_id")

m <- align_move(move, res = 10, unit = "mins")
m2 <- align_move(move2, res = 10, unit = "mins")

frames <- frames_spatial(m, 
                         path_fade = TRUE,
                         path_legend = FALSE,
                         path_size = 5,
                         tail_length = 0,
                         trace_show = FALSE,
                         map_service = "osm_stamen", 
                         map_type = "terrain_bg",
                         map_token = "a047c304-af0d-45c7-89ed-c712362070d8",
                         map_dir = here("data/maptiles/"),
                         alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(type = "label") %>%
  add_progress()

frames2 <- frames_spatial(m2, 
                         path_fade = TRUE,
                         path_legend = FALSE,
                         path_size = 5,
                         tail_length = 0,
                         trace_show = FALSE,
                         map_service = "osm_stamen", 
                         map_type = "terrain_bg",
                         map_token = "a047c304-af0d-45c7-89ed-c712362070d8",
                         map_dir = here("data/maptiles/"),
                         alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(type = "label") %>%
  add_progress()
frames2 <- add_gg(frames2, gg = expr(geom_point(data = data.frame(long = 35.03684,
                                                                  lat = 30.86458),
                                                aes(x = long, y = lat),
                                                col = "black", pch = 8)))

animate_frames(frames, out_file = "test.gif", fps = 5, overwrite = T)
animate_frames(frames2, out_file = "carcass_2023-06-13.gif", fps = 5, overwrite = T)

# Now time to do it with a longer amount of time

dat <- hires_tags %>% filter(dateOnly_il >= lubridate::date("2023-08-01") & dateOnly_il <= lubridate::date("2023-08-10"))

dat <- dat %>%
  group_by(Nili_id) %>%
  filter(n() > 2) %>%
  ungroup()

move <- moveVis::df2move(df = dat, proj = "WGS84", x = "location_long", y = "location_lat", time = "timestamp_il", track_id = "Nili_id")

m <- align_move(move, res = 10, unit = "mins")

frames <- frames_spatial(m, 
                         path_fade = TRUE,
                         path_legend = FALSE,
                         path_size = 5,
                         tail_length = 0,
                         trace_show = FALSE,
                         map_service = "osm_stamen", 
                         map_type = "terrain_bg",
                         map_token = "a047c304-af0d-45c7-89ed-c712362070d8",
                         map_dir = here("data/maptiles/"),
                         alpha = 0.5) %>%
  add_labels(x = "Longitude", y = "Latitude") %>%
  add_northarrow() %>%
  add_scalebar() %>%
  add_timestamps(type = "label") %>%
  add_progress()

animate_frames(frames, out_file = "tendays.gif", fps = 5, overwrite = T)
