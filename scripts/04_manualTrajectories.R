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
carc2 <- stn_carcs[[27]] # 4422323, 2023-03-24
carc3 <- stn_carcs[[26]] 

gps <- stn_gps_30days[[24]] %>% 
  mutate(timestamp_il = lubridate::with_tz(timestamp, tz = 
                                             "Israel")) %>%
  filter(timestamp_il >= carc$datetime_il) %>% # only timestamps after the carcass
  mutate(date_il = lubridate::date(timestamp_il),
         day = as.numeric(difftime(date_il, lubridate::date(carc$datetime_il), units = "days"))) %>% # make a date index
  arrange(timestamp_il) %>%
  select(ground_speed, heading, height_above_msl, timestamp, tag_id, individual_id, individual_local_identifier, nick_name, sex, tag_local_identifier, date, dateOnly, dist_to_carcass, time_since_carcass, carcID, location_long, location_lat, timestamp_il, date_il, day) %>%
  mutate(year = lubridate::year(carc$date))

gps2 <- stn_gps_30days[[27]] %>% 
  mutate(timestamp_il = lubridate::with_tz(timestamp, tz = 
                                             "Israel")) %>%
  filter(timestamp_il >= carc2$datetime_il) %>% # only timestamps after the carcass
  mutate(date_il = lubridate::date(timestamp_il),
         day = as.numeric(difftime(date_il, lubridate::date(carc2$datetime_il), units = "days"))) %>% # make a date index
  arrange(timestamp_il) %>%
  select(ground_speed, heading, height_above_msl, timestamp, tag_id, individual_id, individual_local_identifier, nick_name, sex, tag_local_identifier, date, dateOnly, dist_to_carcass, time_since_carcass, carcID, location_long, location_lat, timestamp_il, date_il, day) %>%
  mutate(year = lubridate::year(carc2$date))

gps3 <- stn_gps_30days[[26]] %>% 
  mutate(timestamp_il = lubridate::with_tz(timestamp, tz = 
                                             "Israel")) %>%
  filter(timestamp_il >= carc3$datetime_il) %>% # only timestamps after the carcass
  mutate(date_il = lubridate::date(timestamp_il),
         day = as.numeric(difftime(date_il, lubridate::date(carc3$datetime_il), units = "days"))) %>% # make a date index
  arrange(timestamp_il) %>%
  select(ground_speed, heading, height_above_msl, timestamp, tag_id, individual_id, individual_local_identifier, nick_name, sex, tag_local_identifier, date, dateOnly, dist_to_carcass, time_since_carcass, carcID, location_long, location_lat, timestamp_il, date_il, day) %>%
  mutate(year = lubridate::year(carc3$date))

gps_mt <- gps %>%
  mutate(id = paste(individual_local_identifier, day, sep = "_")) %>%
  arrange(id, day, timestamp_il)
gps_mt2 <- gps2 %>%
  mutate(id = paste(individual_local_identifier, day, sep = "_")) %>%
  arrange(id, day, timestamp_il)
gps_mt3 <- gps3 %>%
  mutate(id = paste(individual_local_identifier, day, sep = "_")) %>%
  arrange(id, day, timestamp_il)

# fix single-point lines
single_point_lines <- gps_mt %>%
  group_by(id) %>%
  filter(n() == 1)
single_point_lines2 <- gps_mt2 %>%
  group_by(id) %>%
  filter(n() == 1)
single_point_lines3 <- gps_mt3 %>%
  group_by(id) %>%
  filter(n() == 1)

gps_mt <- bind_rows(gps_mt, st_jitter(single_point_lines, factor = 0.00001)) %>%
  arrange(id, timestamp_il) # with the duplicates added
gps_mt2 <- bind_rows(gps_mt2, st_jitter(single_point_lines2, factor = 0.00001)) %>%
  arrange(id, timestamp_il) # with the duplicates added
gps_mt3 <- bind_rows(gps_mt3, st_jitter(single_point_lines3, factor = 0.00001)) %>%
  arrange(id, timestamp_il) # with the duplicates added

gps_mt <- gps_mt %>%
  mt_as_move2(time_column = "timestamp_il", track_id_column = "id", track_attributes = c("day", "individual_local_identifier", "date_il"))
gps_mt2 <- gps_mt2 %>%
  mt_as_move2(time_column = "timestamp_il", track_id_column = "id", track_attributes = c("day", "individual_local_identifier", "date_il"))
gps_mt3 <- gps_mt3 %>%
  mt_as_move2(time_column = "timestamp_il", track_id_column = "id", track_attributes = c("day", "individual_local_identifier", "date_il"))

vulture_lines <- gps_mt %>%
  select_track_data(individual_local_identifier, date_il, day, id) %>%
  mt_set_track_id("id") %>%
  mt_track_lines() %>%
  st_transform(32636)

vulture_lines2 <- gps_mt2 %>%
  select_track_data(individual_local_identifier, date_il, day, id) %>%
  mt_set_track_id("id") %>%
  mt_track_lines() %>%
  st_transform(32636)

vulture_lines3 <- gps_mt3 %>%
  select_track_data(individual_local_identifier, date_il, day, id) %>%
  mt_set_track_id("id") %>%
  mt_track_lines() %>%
  st_transform(32636)


all(st_is_valid(vulture_lines)) # check that all are valid
all(st_is_valid(vulture_lines2)) # check that all are valid
all(st_is_valid(vulture_lines3)) # check that all are valid


# ggplot() +
#   geom_sf(data = ne_coastline(returnclass = "sf", 50)) +
#   theme_linedraw() +
#   geom_sf(
#     data = vulture_lines,
#     aes(color = id)
#   )+
#   theme(legend.position = "none", panel.grid = element_line(alpha = 0.5))+
#   coord_sf(
#     crs = "WGS84",
#     xlim = c(34.72049, 35.61941), ylim = c(30.09926, 31.47402)
#   )

# Buffered carcass locations
carc_buffered <- st_buffer(carc, 2000)
carc_buffered2 <- st_buffer(carc2, 2000)
carc_buffered3 <- st_buffer(carc3, 2000)

dayzero <- vulture_lines %>% filter(day == 0)
dayone <- vulture_lines %>% filter(day == 1)
daytwo <- vulture_lines %>% filter(day == 2)
daythree <- vulture_lines %>% filter(day == 3)
dayfour <- vulture_lines %>% filter(day == 4)

dayzero2 <- vulture_lines2 %>% filter(day == 0)
dayone2 <- vulture_lines2 %>% filter(day == 1)
daytwo2 <- vulture_lines2 %>% filter(day == 2)
daythree2 <- vulture_lines2 %>% filter(day == 3)
dayfour2 <- vulture_lines2 %>% filter(day == 4)

dayzero3 <- vulture_lines3 %>% filter(day == 0)
dayone3 <- vulture_lines3 %>% filter(day == 1)
daytwo3 <- vulture_lines3 %>% filter(day == 2)
daythree3 <- vulture_lines3 %>% filter(day == 3)
dayfour3 <- vulture_lines3 %>% filter(day == 4)

# colors <- paletteer_c("grDevices::rainbow", length(unique(vulture_lines$individual_local_identifier)))
# mapview(dayzero, zcol = "id", legend = F, color = colors) + mapview(carc_buffered, col.regions = "black")
# mapview(dayone, zcol = "id", legend = F, color = colors)+ mapview(carc_buffered, col.regions = "black")
# mapview(daytwo, zcol = "id", legend = F, color = colors)+ mapview(carc_buffered, col.regions = "black")
# mapview(daythree, zcol = "id", legend = F, color = colors)+ mapview(carc_buffered, col.regions = "black")
# mapview(dayfour, zcol = "id", legend = F, color = colors)+ mapview(carc_buffered, col.regions = "black")

all_indivs <- sort(unique(vulture_lines$individual_local_identifier))
all_indivs2 <- sort(unique(vulture_lines2$individual_local_identifier))
all_indivs3 <- sort(unique(vulture_lines3$individual_local_identifier))

sighted_dayzero <- all_indivs %in% st_intersection(dayzero, carc_buffered)$individual_local_identifier
sighted_dayone <- all_indivs %in% st_intersection(dayone, carc_buffered)$individual_local_identifier
sighted_daytwo <- all_indivs %in% st_intersection(daytwo, carc_buffered)$individual_local_identifier
sighted_daythree <- all_indivs %in% st_intersection(daythree, carc_buffered)$individual_local_identifier
sighted_dayfour <- all_indivs %in% st_intersection(dayfour, carc_buffered)$individual_local_identifier

sighted_dayzero2 <- all_indivs2 %in% st_intersection(dayzero2, carc_buffered2)$individual_local_identifier
sighted_dayone2 <- all_indivs2 %in% st_intersection(dayone2, carc_buffered2)$individual_local_identifier
sighted_daytwo2 <- all_indivs2 %in% st_intersection(daytwo2, carc_buffered2)$individual_local_identifier
sighted_daythree2 <- all_indivs2 %in% st_intersection(daythree2, carc_buffered2)$individual_local_identifier
sighted_dayfour2 <- all_indivs2 %in% st_intersection(dayfour2, carc_buffered2)$individual_local_identifier

sighted_dayzero3 <- all_indivs3 %in% st_intersection(dayzero3, carc_buffered3)$individual_local_identifier
sighted_dayone3 <- all_indivs3 %in% st_intersection(dayone3, carc_buffered3)$individual_local_identifier
sighted_daytwo3 <- all_indivs3 %in% st_intersection(daytwo3, carc_buffered3)$individual_local_identifier
sighted_daythree3 <- all_indivs3 %in% st_intersection(daythree3, carc_buffered3)$individual_local_identifier
sighted_dayfour3 <- all_indivs3 %in% st_intersection(dayfour3, carc_buffered3)$individual_local_identifier

sightings <- bind_cols(all_indivs, sighted_dayzero, sighted_dayone, sighted_daytwo, sighted_daythree, sighted_dayfour) %>% setNames(c("id", "s0", "s1", "s2", "s3", "s4"))

sightings2 <- bind_cols(all_indivs2, sighted_dayzero2, sighted_dayone2, sighted_daytwo2, sighted_daythree2, sighted_dayfour2) %>% setNames(c("id", "s0", "s1", "s2", "s3", "s4"))

sightings3 <- bind_cols(all_indivs3, sighted_dayzero3, sighted_dayone3, sighted_daytwo3, sighted_daythree3, sighted_dayfour3) %>% setNames(c("id", "s0", "s1", "s2", "s3", "s4"))

# Now time to get roostmates
tar_load(roosts_stn)
roosts_test <- roosts_stn[[24]]
roosts_test2 <- roosts_stn[[27]]
roosts_test3 <- roosts_stn[[26]]
roosts_list <- roosts_test %>% arrange(individual_local_identifier, roost_date) %>% group_split(roost_date)
roosts_list2 <- roosts_test2 %>% arrange(individual_local_identifier, roost_date) %>% group_split(roost_date)
roosts_list3 <- roosts_test3 %>% arrange(individual_local_identifier, roost_date) %>% group_split(roost_date)

ids <- map(roosts_list, "individual_local_identifier")
ids2 <- map(roosts_list2, "individual_local_identifier")
ids3 <- map(roosts_list3, "individual_local_identifier")

dist_list <- map(roosts_list, ~st_distance(.x, .x))
dist_list2 <- map(roosts_list2, ~st_distance(.x, .x))
dist_list3 <- map(roosts_list3, ~st_distance(.x, .x))

dist_list <- map2(dist_list, ids, ~{colnames(.x) <- .y; rownames(.x) <- .y; return(.x)})
dist_list2 <- map2(dist_list2, ids2, ~{colnames(.x) <- .y; rownames(.x) <- .y; return(.x)})
dist_list3 <- map2(dist_list3, ids3, ~{colnames(.x) <- .y; rownames(.x) <- .y; return(.x)})

thresh_m <- 500
units(thresh_m) <- "m"

dist_list_bin <- map(dist_list, ~.x <= thresh_m)
dist_list_bin2 <- map(dist_list2, ~.x <= thresh_m)
dist_list_bin3 <- map(dist_list3, ~.x <= thresh_m)

dist_list_long <- map(dist_list_bin, ~pivot_longer(mutate(as.data.frame(.x), ID1 = row.names(.x)), cols = -ID1))
dist_list_long2 <- map(dist_list_bin2, ~pivot_longer(mutate(as.data.frame(.x), ID1 = row.names(.x)), cols = -ID1))
dist_list_long3 <- map(dist_list_bin3, ~pivot_longer(mutate(as.data.frame(.x), ID1 = row.names(.x)), cols = -ID1))

names(dist_list_long) <- unique(roosts_test$roost_date)
names(dist_list_long2) <- unique(roosts_test2$roost_date)
names(dist_list_long3) <- unique(roosts_test3$roost_date)

together <- purrr::list_rbind(dist_list_long, names_to = "roost_date")
together2 <- purrr::list_rbind(dist_list_long2, names_to = "roost_date")
together3 <- purrr::list_rbind(dist_list_long3, names_to = "roost_date")

# Next: time to analyze who roosted with someone who saw the carcass the day before and could have followed them. Possible pairings going to the carcass together?

# Need to attach days
days <- gps %>% st_drop_geometry() %>% select(date_il, day) %>% distinct()
days2 <- gps2 %>% st_drop_geometry() %>% select(date_il, day) %>% distinct()
days3 <- gps3 %>% st_drop_geometry() %>% select(date_il, day) %>% distinct()

# day 0 = date of carcass placement
# day 1 = next date after carcass placement
# We want the roost dates to correspond to the *next* day's flight. So a roost date of carcass_date-days(1) will correspond to day 0, a roost date of carcass_date will correspond to day 1, etc.
days_shifted <- mutate(days, roost_date = date_il - lubridate::days(1)) %>% select(roost_date, day)
days_shifted2 <- mutate(days2, roost_date = date_il - lubridate::days(1)) %>% select(roost_date, day)
days_shifted3 <- mutate(days3, roost_date = date_il - lubridate::days(1)) %>% select(roost_date, day)

together <- left_join(mutate(together, roost_date = lubridate::ymd(roost_date)), days_shifted, by = "roost_date") %>%
  rename("ID2" = "name") %>%
  filter(value == TRUE, # only keep indivs that roosted together
         ID1 != ID2) %>% # remove self edges
  select(-value)

together2 <- left_join(mutate(together2, roost_date = lubridate::ymd(roost_date)), days_shifted2, by = "roost_date") %>%
  rename("ID2" = "name") %>%
  filter(value == TRUE, # only keep indivs that roosted together
         ID1 != ID2) %>% # remove self edges
  select(-value)

together3 <- left_join(mutate(together3, roost_date = lubridate::ymd(roost_date)), days_shifted3, by = "roost_date") %>%
  rename("ID2" = "name") %>%
  filter(value == TRUE, # only keep indivs that roosted together
         ID1 != ID2) %>% # remove self edges
  select(-value)

sightings_long <- sightings %>% pivot_longer(cols = starts_with("s"), names_to = "day", values_to = "sighted_that_day") %>% 
  mutate(sighted_that_day = replace_na(sighted_that_day, FALSE)) %>%
  mutate(day = as.numeric(str_remove(day, "s"))) %>%
  arrange(id, day) %>%
  group_by(id) %>% 
  mutate(sighted_ever = cumsum(sighted_that_day) > 0) %>%
  ungroup()

sightings_long2 <- sightings2 %>% pivot_longer(cols = starts_with("s"), names_to = "day", values_to = "sighted_that_day") %>% 
  mutate(sighted_that_day = replace_na(sighted_that_day, FALSE)) %>%
  mutate(day = as.numeric(str_remove(day, "s"))) %>%
  arrange(id, day) %>%
  group_by(id) %>% 
  mutate(sighted_ever = cumsum(sighted_that_day) > 0) %>%
  ungroup()

sightings_long3 <- sightings3 %>% pivot_longer(cols = starts_with("s"), names_to = "day", values_to = "sighted_that_day") %>% 
  mutate(sighted_that_day = replace_na(sighted_that_day, FALSE)) %>%
  mutate(day = as.numeric(str_remove(day, "s"))) %>%
  arrange(id, day) %>%
  group_by(id) %>% 
  mutate(sighted_ever = cumsum(sighted_that_day) > 0) %>%
  ungroup()

together <- left_join(together, sightings_long, by = c("ID2" = "id", "day")) %>%
  mutate(sighted_that_day = replace_na(sighted_that_day, FALSE),
         sighted_ever = replace_na(sighted_ever, FALSE))
together2 <- left_join(together2, sightings_long2, by = c("ID2" = "id", "day")) %>%
  mutate(sighted_that_day = replace_na(sighted_that_day, FALSE),
         sighted_ever = replace_na(sighted_ever, FALSE))
together3 <- left_join(together3, sightings_long3, by = c("ID2" = "id", "day")) %>%
  mutate(sighted_that_day = replace_na(sighted_that_day, FALSE),
         sighted_ever = replace_na(sighted_ever, FALSE))

# Let's focus only on informed roostmates (sighted_ever), assuming vultures' memories can last 4 days, which I think they can.
together <- together %>%
  select(-sighted_that_day) %>%
  rename("informed" = sighted_ever)

together2 <- together2 %>%
  select(-sighted_that_day) %>%
  rename("informed" = sighted_ever)

together3 <- together3 %>%
  select(-sighted_that_day) %>%
  rename("informed" = sighted_ever)

prop_informed <- together %>%
  group_by(roost_date, ID1) %>%
  summarize(n_roostmates = length(unique(ID2)),
            n_informed = sum(informed),
            prop_informed = n_informed/n_roostmates) %>%
  ungroup()

prop_informed2 <- together2 %>%
  group_by(roost_date, ID1) %>%
  summarize(n_roostmates = length(unique(ID2)),
            n_informed = sum(informed),
            prop_informed = n_informed/n_roostmates) %>%
  ungroup()

prop_informed3 <- together3 %>%
  group_by(roost_date, ID1) %>%
  summarize(n_roostmates = length(unique(ID2)),
            n_informed = sum(informed),
            prop_informed = n_informed/n_roostmates) %>%
  ungroup()

# Now need to fill this in for the rest of the dates and IDs
dates <- sort(unique(prop_informed$roost_date))
dates2 <- sort(unique(prop_informed2$roost_date))
dates3 <- sort(unique(prop_informed3$roost_date))

ids <- sort(unique(prop_informed$ID1))
ids2 <- sort(unique(prop_informed2$ID1))
ids3 <- sort(unique(prop_informed3$ID1))

filler <- expand_grid("roost_date" = dates, "ID1" = ids)
filler2 <- expand_grid("roost_date" = dates, "ID1" = ids2)
filler3 <- expand_grid("roost_date" = dates, "ID1" = ids3)

all <- bind_rows(prop_informed, filler) %>%
  arrange(roost_date, ID1) %>%
  group_by(roost_date, ID1) %>%
  slice(1) %>%
  mutate(across(c("n_roostmates", "n_informed", "prop_informed"), ~replace_na(.x, 0)))

all2 <- bind_rows(prop_informed2, filler2) %>%
  arrange(roost_date, ID1) %>%
  group_by(roost_date, ID1) %>%
  slice(1) %>%
  mutate(across(c("n_roostmates", "n_informed", "prop_informed"), ~replace_na(.x, 0)))

all3 <- bind_rows(prop_informed3, filler3) %>%
  arrange(roost_date, ID1) %>%
  group_by(roost_date, ID1) %>%
  slice(1) %>%
  mutate(across(c("n_roostmates", "n_informed", "prop_informed"), ~replace_na(.x, 0)))
  
write_rds(all, file = "data/created/prop_informed.RDS")
write_rds(all2, file = "data/created/prop_informed2.RDS")
write_rds(all3, file = "data/created/prop_informed3.RDS")