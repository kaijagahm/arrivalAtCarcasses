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
library(future)
library(furrr)

tar_load(stn_gps_30days)
tar_load(wild_gps_30days)
tar_load(stn_carcs)
tar_load(wild_carcs)
tar_load(stations)

gps_stn <- purrr::map2(stn_gps_30days, stn_carcs, ~{
  out <- .x %>%
    mutate(timestamp_il = lubridate::with_tz(timestamp, tz = "Israel")) %>%
    filter(timestamp_il >= .y$datetime_il) %>%
    mutate(date_il = lubridate::date(timestamp_il),
           day = as.numeric(difftime(date_il, lubridate::date(.y$datetime_il), units = "days"))) %>%
    arrange(timestamp_il) %>%
    select(ground_speed, heading, height_above_msl, timestamp, tag_id, individual_id, individual_local_identifier, nick_name, sex, tag_local_identifier, date, dateOnly, dist_to_carcass, time_since_carcass, carcID, location_long, location_lat, timestamp_il, date_il, day) %>%
    mutate(year = lubridate::year(.y$date))
  return(out)
})

gps_wild <- purrr::map2(wild_gps_30days, wild_carcs, ~{
  out <- .x %>%
    mutate(timestamp_il = lubridate::with_tz(timestamp, tz = "Israel")) %>%
    filter(timestamp_il >= .y$datetime_il) %>%
    mutate(date_il = lubridate::date(timestamp_il),
           day = as.numeric(difftime(date_il, lubridate::date(.y$datetime_il), units = "days"))) %>%
    arrange(timestamp_il) %>%
    select(ground_speed, heading, height_above_msl, timestamp, tag_id, individual_id, individual_local_identifier, nick_name, sex, tag_local_identifier, date, dateOnly, dist_to_carcass, time_since_carcass, carcID, location_long, location_lat, timestamp_il, date_il, day) %>%
    mutate(year = lubridate::year(.y$date))
  return(out)
})

gps_mts_stn <- map(gps_stn, ~{.x %>% mutate(id = paste(individual_local_identifier, day, sep = "_")) %>%
    arrange(id, day, timestamp_il)})
gps_mts_wild <- map(gps_wild, ~{.x %>% mutate(id = paste(individual_local_identifier, day, sep = "_")) %>%
    arrange(id, day, timestamp_il)}) # movement tracks

# fix single-point lines
single_point_lines_stn <- map(gps_mts_stn, ~{
  .x %>% group_by(id) %>%
    filter(n() == 1)
})

single_point_lines_wild <- map(gps_mts_wild, ~{
  .x %>% group_by(id) %>%
    filter(n() == 1)
})

gps_mts_stn <- map2(gps_mts_stn, single_point_lines_stn, ~{
  bind_rows(.x, sf::st_jitter(.y, factor = 0.00001)) %>%
    arrange(id, timestamp_il) # with duplicates added
})
gps_mts_wild <- map2(gps_mts_wild, single_point_lines_wild, ~{
  bind_rows(.x, sf::st_jitter(.y, factor = 0.00001)) %>%
    arrange(id, timestamp_il) # with duplicates added
})

gps_mts_stn <- map(gps_mts_stn, ~{
  .x %>% mt_as_move2(time_column = "timestamp_il", track_id_column = "id", track_attributes = c("day", "individual_local_identifier", "date_il"))
})

gps_mts_wild <- map(gps_mts_wild, ~{
  .x %>% mt_as_move2(time_column = "timestamp_il", track_id_column = "id", track_attributes = c("day", "individual_local_identifier", "date_il"))
})

future::plan("multisession", workers = 10)
vulture_lines_stn <- furrr::future_map(gps_mts_stn, ~{
  .x %>%
    select_track_data(individual_local_identifier, date_il, day, id) %>%
    mt_set_track_id("id") %>%
    mt_track_lines() %>%
    st_transform(32636)
}, .progress = T)
vulture_lines_wild <- furrr::future_map(gps_mts_wild, ~{
  .x %>%
    select_track_data(individual_local_identifier, date_il, day, id) %>%
    mt_set_track_id("id") %>%
    mt_track_lines() %>%
    st_transform(32636)
}, .progress = T)

# check that all are valid
map_lgl(vulture_lines_stn, ~all(st_is_valid(.x))) # a few are not valid. I wonder why and what that means?
st_is_valid(vulture_lines_stn[[27]]) # just once in a while. Gonna ignore for now.
map_lgl(vulture_lines_wild, ~all(st_is_valid(.x)))

# Buffered carcass locations
carcs_buffered_stn <- map(stn_carcs, ~sf::st_buffer(.x, 2000))
carcs_buffered_wild <- map(wild_carcs, ~sf::st_buffer(.x, 2000))

dayzero_stn <- map(vulture_lines_stn, ~.x %>% filter(day == 0))
dayone_stn <- map(vulture_lines_stn, ~.x %>% filter(day == 1))
daytwo_stn <- map(vulture_lines_stn, ~.x %>% filter(day == 2))
daythree_stn <- map(vulture_lines_stn, ~.x %>% filter(day == 3))
dayfour_stn <- map(vulture_lines_stn, ~.x %>% filter(day == 4))

dayzero_wild <- map(vulture_lines_wild, ~.x %>% filter(day == 0))
dayone_wild <- map(vulture_lines_wild, ~.x %>% filter(day == 1))
daytwo_wild <- map(vulture_lines_wild, ~.x %>% filter(day == 2))
daythree_wild <- map(vulture_lines_wild, ~.x %>% filter(day == 3))
dayfour_wild <- map(vulture_lines_wild, ~.x %>% filter(day == 4))

all_indivs_stn <- map(vulture_lines_stn, ~sort(unique(.x$individual_local_identifier)))
all_indivs_wild <- map(vulture_lines_wild, ~sort(unique(.x$individual_local_identifier)))

sighted_dayzero_stn <- purrr::pmap(list(a = all_indivs_stn, b = dayzero_stn, c = carcs_buffered_stn), function(a, b, c){a %in% sf::st_intersection(b, c)$individual_local_identifier})
sighted_dayone_stn <- purrr::pmap(list(a = all_indivs_stn, b = dayone_stn, c = carcs_buffered_stn), function(a, b, c){a %in% sf::st_intersection(b, c)$individual_local_identifier})
sighted_daytwo_stn <- purrr::pmap(list(a = all_indivs_stn, b = daytwo_stn, c = carcs_buffered_stn), function(a, b, c){a %in% sf::st_intersection(b, c)$individual_local_identifier})
sighted_daythree_stn <- purrr::pmap(list(a = all_indivs_stn, b = daythree_stn, c = carcs_buffered_stn), function(a, b, c){a %in% sf::st_intersection(b, c)$individual_local_identifier})
sighted_dayfour_stn <- purrr::pmap(list(a = all_indivs_stn, b = dayfour_stn, c = carcs_buffered_stn), function(a, b, c){a %in% sf::st_intersection(b, c)$individual_local_identifier})

sighted_dayzero_wild <- purrr::pmap(list(a = all_indivs_wild, b = dayzero_wild, c = carcs_buffered_wild), function(a, b, c){a %in% sf::st_intersection(b, c)$individual_local_identifier})
sighted_dayone_wild <- purrr::pmap(list(a = all_indivs_wild, b = dayone_wild, c = carcs_buffered_wild), function(a, b, c){a %in% sf::st_intersection(b, c)$individual_local_identifier})
sighted_daytwo_wild <- purrr::pmap(list(a = all_indivs_wild, b = daytwo_wild, c = carcs_buffered_wild), function(a, b, c){a %in% sf::st_intersection(b, c)$individual_local_identifier})
sighted_daythree_wild <- purrr::pmap(list(a = all_indivs_wild, b = daythree_wild, c = carcs_buffered_wild), function(a, b, c){a %in% sf::st_intersection(b, c)$individual_local_identifier})
sighted_dayfour_wild <- purrr::pmap(list(a = all_indivs_wild, b = dayfour_wild, c = carcs_buffered_wild), function(a, b, c){a %in% sf::st_intersection(b, c)$individual_local_identifier})

sightings_stn <- purrr::pmap(list(a = all_indivs_stn, b = sighted_dayzero_stn, c = sighted_dayone_stn, d = sighted_daytwo_stn, e = sighted_daythree_stn, f = sighted_dayfour_stn), function(a, b, c, d, e, f){data.frame("id" = a, "s0" = b, "s1" = c, "s2" = d, "s3" = e, "s4" = f)}, .progress = T)

sightings_wild <- purrr::pmap(list(a = all_indivs_wild, b = sighted_dayzero_wild, c = sighted_dayone_wild, d = sighted_daytwo_wild, e = sighted_daythree_wild, f = sighted_dayfour_wild), function(a, b, c, d, e, f){data.frame("id" = a, "s0" = b, "s1" = c, "s2" = d, "s3" = e, "s4" = f)}, .progress = T)

# Now time to get roostmates
tar_load(roosts_stn)
tar_load(roosts_wild)

roost_lists_stn <- purrr::map(roosts_stn, ~.x %>% arrange(individual_local_identifier, roost_date) %>% group_split(roost_date))
roost_lists_wild <- purrr::map(roosts_wild, ~.x %>% arrange(individual_local_identifier, roost_date) %>% group_split(roost_date))

ids_stn <- map(roost_lists_stn, ~map(.x, "individual_local_identifier"))
ids_wild <- map(roost_lists_wild, ~map(.x, "individual_local_identifier"))

dist_lists_stn <- map(roost_lists_stn, ~{map(.x, ~st_distance(.x, .x))}, .progress = T)
dist_lists_wild <- map(roost_lists_wild, ~map(.x, ~st_distance(.x, .x)), .progress = T)

thresh_m <- 500
units(thresh_m) <- "m"

dist_lists_bin_stn <- map(dist_lists_stn, ~map(.x, ~.x <= thresh_m))
dist_lists_bin_wild <- map(dist_lists_wild, ~map(.x, ~.x <= thresh_m))

getlong <- function(x, ids){
  out <- as.data.frame(x)
  row.names(out) <- ids
  colnames(out) <- ids
  out <- out %>%
    mutate(ID1 = row.names(.)) %>%
    pivot_longer(cols = -ID1)
  return(out)
}

dist_lists_long_stn <- map2(dist_lists_bin_stn, ids_stn, ~map2(.x, .y, ~getlong(.x, .y)))
dist_lists_long_wild <- map2(dist_lists_bin_wild, ids_wild, ~map2(.x, .y, ~getlong(.x, .y)))

dist_lists_long_stn <- map2(dist_lists_long_stn, roosts_stn, ~{
  names(.x) <- unique(.y$roost_date)
  return(.x)
})

dist_lists_long_wild <- map2(dist_lists_long_wild, roosts_wild, ~{
  names(.x) <- unique(.y$roost_date)
  return(.x)
})

together_stn <- map(dist_lists_long_stn, ~purrr::list_rbind(.x, names_to = "roost_date"))
together_wild <- map(dist_lists_long_wild, ~purrr::list_rbind(.x, names_to = "roost_date"))

# Next: time to analyze who roosted with someone who saw the carcass the day before and could have followed them. Possible pairings going to the carcass together?
days_stn <- map(gps_stn, ~{
  .x %>% st_drop_geometry() %>% select(date_il, day) %>% distinct()
})

days_wild <- map(gps_wild, ~{
  .x %>% st_drop_geometry() %>% select(date_il, day) %>% distinct()
})

# day 0 = date of carcass placement
# day 1 = next date after carcass placement
# We want the roost dates to correspond to the *next* day's flight. So a roost date of carcass_date-days(1) will correspond to day 0, a roost date of carcass_date will correspond to day 1, etc.
days_shifted_stn <- map(days_stn, ~{
  mutate(.x, roost_date = date_il - lubridate::days(1)) %>%
    select(roost_date, day)})
days_shifted_wild <- map(days_wild, ~{
  mutate(.x, roost_date = date_il - lubridate::days(1)) %>%
    select(roost_date, day)})

together_stn <- map2(together_stn, days_shifted_stn, ~{
  left_join(mutate(.x, roost_date = lubridate::ymd(roost_date)), .y, by = "roost_date") %>%
    rename("ID2" = "name") %>%
    filter(value == TRUE, # only keep indivs that roosted together
           ID1 != ID2) %>% # remove self edges
    select(-value)
}, .progress = T)

together_wild <- map2(together_wild, days_shifted_wild, ~{
  left_join(mutate(.x, roost_date = lubridate::ymd(roost_date)), .y, by = "roost_date") %>%
    rename("ID2" = "name") %>%
    filter(value == TRUE, # only keep indivs that roosted together
           ID1 != ID2) %>% # remove self edges
    select(-value)
}, .progress = T)

sightings_long_stn <- map(sightings_stn, ~{
  pivot_longer(.x, cols = starts_with("s"), names_to = "day", values_to = "sighted_that_day") %>%
    mutate(sighted_that_day =replace_na(sighted_that_day, FALSE),
           day = as.numeric(str_replace(day, "s", ""))) %>%
    arrange(id, day) %>%
    group_by(id) %>%
    mutate(sighted_ever = cumsum(sighted_that_day) > 0) %>%
    ungroup()
})

sightings_long_wild <- map(sightings_wild, ~{
  pivot_longer(.x, cols = starts_with("s"), names_to = "day", values_to = "sighted_that_day") %>%
    mutate(sighted_that_day =replace_na(sighted_that_day, FALSE),
           day = as.numeric(str_replace(day, "s", ""))) %>%
    arrange(id, day) %>%
    group_by(id) %>%
    mutate(sighted_ever = cumsum(sighted_that_day) > 0) %>%
    ungroup()
})

together_stn <- map2(together_stn, sightings_long_stn, ~{
  left_join(.x, .y, by = c("ID2" = "id", "day")) %>%
    mutate(sighted_that_day = replace_na(sighted_that_day, F),
           sighted_ever = replace_na(sighted_ever, F))
})

together_wild <- map2(together_wild, sightings_long_wild, ~{
  left_join(.x, .y, by = c("ID2" = "id", "day")) %>%
    mutate(sighted_that_day = replace_na(sighted_that_day, F),
           sighted_ever = replace_na(sighted_ever, F))
})

# Let's focus only on informed roostmates (sighted_ever), assuming vultures' memories can last 4 days, which I think they can.
together_stn <- map(together_stn, ~{
  .x %>% select(-sighted_that_day) %>%
    rename("informed" = sighted_ever)
})

together_wild <- map(together_wild, ~{
  .x %>% select(-sighted_that_day) %>%
    rename("informed" = sighted_ever)
})

prop_informed_stn <- map(together_stn, ~{
  .x %>%
    mutate(informed = case_when(day == 0 ~ F, .default = informed)) %>%
    group_by(roost_date, ID1) %>%
    summarize(n_roostmates = length(unique(ID2)),
              n_informed = sum(informed),
              prop_informed = n_informed/n_roostmates) %>%
    ungroup()
})

prop_informed_wild <- map(together_wild, ~{
  .x %>%
    mutate(informed = case_when(day == 0 ~ F, .default = informed)) %>%
    group_by(roost_date, ID1) %>%
    summarize(n_roostmates = length(unique(ID2)),
              n_informed = sum(informed),
              prop_informed = n_informed/n_roostmates) %>%
    ungroup()
})

# Now need to fill this in for the rest of the dates and IDs
dates_stn <- map(prop_informed_stn, ~sort(unique(.x$roost_date)))
dates_wild <- map(prop_informed_wild, ~sort(unique(.x$roost_date)))

ids_stn <- map(prop_informed_stn, ~sort(unique(.x$ID1)))
ids_wild <- map(prop_informed_wild, ~sort(unique(.x$ID1)))

filler_stn <- map2(dates_stn, ids_stn, ~expand_grid("roost_date" = .x, "ID1" = .y))
filler_wild <- map2(dates_wild, ids_wild, ~expand_grid("roost_date" = .x, "ID1" = .y))

all_stn <- map2(prop_informed_stn, filler_stn, ~{
  bind_rows(.x, .y) %>%
    arrange(roost_date, ID1) %>%
    group_by(roost_date, ID1) %>%
    slice(1) %>%
    mutate(across(c("n_roostmates", "n_informed", "prop_informed"), ~replace_na(.x, 0)))
})

all_wild <- map2(prop_informed_wild, filler_wild, ~{
  bind_rows(.x, .y) %>%
    arrange(roost_date, ID1) %>%
    group_by(roost_date, ID1) %>%
    slice(1) %>%
    mutate(across(c("n_roostmates", "n_informed", "prop_informed"), ~replace_na(.x, 0)))
})

iwalk(all_stn, ~{
  nm <- paste0("data/created/prop_informed/stn/prop_informed_", str_pad(.y, width = 3, side = "left", pad = "0"), ".RDS")
  write_rds(.x, file = nm)})

iwalk(all_wild, ~{
  nm <- paste0("data/created/prop_informed/wild/prop_informed_", str_pad(.y, width = 3, side = "left", pad = "0"), ".RDS")
  write_rds(.x, file = nm)})