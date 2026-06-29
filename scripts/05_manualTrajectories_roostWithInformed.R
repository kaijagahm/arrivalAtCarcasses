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

tar_load(gps_stn)
tar_load(gps_wild)
tar_load(stations)

# Get sightings (in targets pipeline)
tar_load(sightings_wild)
tar_load(sightings_stn)

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