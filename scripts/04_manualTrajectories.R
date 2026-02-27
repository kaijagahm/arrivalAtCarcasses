# Manual trajectory analysis
library(tidyverse)
library(targets)
library(sf)
library(mapview)
library(move2)
library(rnaturalearth)
library(units)
library(circular)
library(paletteer)

tar_load(stn_gps_30days)
tar_load(stn_carcs)
tar_load(stations)

# Choose one to test
carc <- stn_carcs[[24]] # 4417687, 2023-03-22
gps <- stn_gps_30days[[24]] %>% 
  mutate(timestamp_il = lubridate::with_tz(timestamp, tz = 
                                             "Israel")) %>%
  filter(timestamp_il >= carc$datetime_il) %>% # only timestamps after the carcass
  mutate(date_il = lubridate::date(timestamp_il),
         day = as.numeric(difftime(date_il, lubridate::date(carc$datetime_il), units = "days"))) %>% # make a date index
  arrange(timestamp_il) %>%
  select(ground_speed, heading, height_above_msl, timestamp, tag_id, individual_id, individual_local_identifier, nick_name, sex, tag_local_identifier, date, dateOnly, dist_to_carcass, time_since_carcass, carcID, location_long, location_lat, timestamp_il, date_il, day) %>%
  mutate(year = lubridate::year(carc$date))

gps_mt <- gps %>%
  mutate(id = paste(individual_local_identifier, day, sep = "_")) %>%
  arrange(id, day, timestamp_il)

# fix single-point lines
single_point_lines <- gps_mt %>%
  group_by(id) %>%
  filter(n() == 1)
gps_mt <- bind_rows(gps_mt, st_jitter(single_point_lines, factor = 0.00001)) %>%
  arrange(id, timestamp_il) # with the duplicates added

gps_mt <- gps_mt %>%
  mt_as_move2(time_column = "timestamp_il", track_id_column = "id", track_attributes = c("day", "individual_local_identifier", "date_il"))

vulture_lines <- gps_mt %>%
  select_track_data(individual_local_identifier, date_il, day, id) %>%
  mt_set_track_id("id") %>%
  mt_track_lines() %>%
  st_transform(32636)

all(st_is_valid(vulture_lines)) # check that all are valid

ggplot() +
  geom_sf(data = ne_coastline(returnclass = "sf", 50)) +
  theme_linedraw() +
  geom_sf(
    data = vulture_lines,
    aes(color = id)
  )+
  theme(legend.position = "none", panel.grid = element_line(alpha = 0.5))+
  coord_sf(
    crs = "WGS84",
    xlim = c(34.72049, 35.61941), ylim = c(30.09926, 31.47402)
  )

# Buffered carcass locations
carc_buffered <- st_buffer(carc, 2000)
dayzero <- vulture_lines %>% filter(day == 0)
dayone <- vulture_lines %>% filter(day == 1)
daytwo <- vulture_lines %>% filter(day == 2)
daythree <- vulture_lines %>% filter(day == 3)
dayfour <- vulture_lines %>% filter(day == 4)

colors <- paletteer_c("grDevices::rainbow", length(unique(vulture_lines$individual_local_identifier)))
mapview(dayzero, zcol = "id", legend = F, color = colors) + mapview(carc_buffered, col.regions = "black")
mapview(dayone, zcol = "id", legend = F, color = colors)+ mapview(carc_buffered, col.regions = "black")
mapview(daytwo, zcol = "id", legend = F, color = colors)+ mapview(carc_buffered, col.regions = "black")
mapview(daythree, zcol = "id", legend = F, color = colors)+ mapview(carc_buffered, col.regions = "black")
mapview(dayfour, zcol = "id", legend = F, color = colors)+ mapview(carc_buffered, col.regions = "black")

all_indivs <- sort(unique(vulture_lines$individual_local_identifier))

sighted_dayzero <- all_indivs %in% st_intersection(dayzero, carc_buffered)$individual_local_identifier
sighted_dayone <- all_indivs %in% st_intersection(dayone, carc_buffered)$individual_local_identifier
sighted_daytwo <- all_indivs %in% st_intersection(daytwo, carc_buffered)$individual_local_identifier
sighted_daythree <- all_indivs %in% st_intersection(daythree, carc_buffered)$individual_local_identifier
sighted_dayfour <- all_indivs %in% st_intersection(dayfour, carc_buffered)$individual_local_identifier

sightings <- bind_cols(all_indivs, sighted_dayzero, sighted_dayone, sighted_daytwo, sighted_daythree, sighted_dayfour) %>% setNames(c("id", "s0", "s1", "s2", "s3", "s4"))

# Now time to get roostmates
tar_load(roosts_stn)
roosts_test <- roosts_stn[[24]]
roosts_list <- roosts_test %>% arrange(individual_local_identifier, roost_date) %>% group_split(roost_date)
ids <- map(roosts_list, "individual_local_identifier")
dist_list <- map(roosts_list, ~st_distance(.x, .x))
dist_list <- map2(dist_list, ids, ~{colnames(.x) <- .y; rownames(.x) <- .y; return(.x)})
thresh_m <- 500
units(thresh_m) <- "m"
dist_list_bin <- map(dist_list, ~.x <= thresh_m)
dist_list_long <- map(dist_list_bin, ~pivot_longer(mutate(as.data.frame(.x), ID1 = row.names(.x)), cols = -ID1))
names(dist_list_long) <- unique(roosts_test$roost_date)

together <- purrr::list_rbind(dist_list_long, names_to = "roost_date")

# Next: time to analyze who roosted with someone who saw the carcass the day before and could have followed them. Possible pairings going to the carcass together?

# Need to attach days
days <- gps %>% st_drop_geometry() %>% select(date_il, day) %>% distinct()
# day 0 = date of carcass placement
# day 1 = next date after carcass placement
# We want the roost dates to correspond to the *next* day's flight. So a roost date of carcass_date-days(1) will correspond to day 0, a roost date of carcass_date will correspond to day 1, etc.
days_shifted <- mutate(days, roost_date = date_il - lubridate::days(1)) %>% select(roost_date, day)

together <- left_join(mutate(together, roost_date = lubridate::ymd(roost_date)), days_shifted, by = "roost_date") %>%
  rename("ID2" = "name") %>%
  filter(value == TRUE, # only keep indivs that roosted together
         ID1 != ID2) %>% # remove self edges
  select(-value)

sightings_long <- sightings %>% pivot_longer(cols = starts_with("s"), names_to = "day", values_to = "sighted_that_day") %>% mutate(day = as.numeric(str_remove(day, "s"))) %>%
  arrange(id, day) %>%
  group_by(id) %>% 
  mutate(sighted_ever = cumsum(sighted_that_day) > 0) %>%
  ungroup()

together <- left_join(together, sightings_long, by = c("ID2" = "id", "day"))

# Let's focus only on informed roostmates (sighted_ever), assuming vultures' memories can last 4 days, which I think they can.
together <- together %>%
  select(-sighted_that_day) %>%
  rename("informed" = sighted_ever)

prop_informed <- together %>%
  group_by(roost_date, ID1) %>%
  summarize(n_roostmates = length(unique(ID2)),
            n_informed = sum(informed),
            prop_informed = n_informed/n_roostmates) %>%
  ungroup()

# Now need to fill this in for the rest of the dates and IDs
dates <- sort(unique(prop_informed$roost_date))
ids <- sort(unique(prop_informed$ID1))
filler <- expand_grid("roost_date" = dates, "ID1" = ids)
all <- bind_rows(prop_informed, filler) %>%
  arrange(roost_date, ID1) %>%
  group_by(roost_date, ID1) %>%
  slice(1) %>%
  mutate(across(c("n_roostmates", "n_informed", "prop_informed"), ~replace_na(.x, 0)))
  
write_rds(prop_informed, file = "data/created/prop_informed.RDS")
