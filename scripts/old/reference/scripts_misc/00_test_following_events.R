# Goal: Can we literally detect following events from the roost to the carcass?
library(sf)
library(tidyverse)
library(vultureUtils)
library(mapview)
gps <- data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv") %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>%
  bind_cols(st_coordinates(.)) %>%
  rename("location_long" = X,
         "location_lat" = Y)

roosts <- get_roosts_df(gps, id = "individual_id") %>% #XXX bug here! need to be able to pass different lat/long columns
  sf::st_as_sf(coords = c("location_long", "location_lat"), remove = F, crs=  "WGS84")
glimpse(roosts)
mapview(roosts)

oneday <- gps %>%
  filter(dateOnly == "2024-05-01")
roosts_oneday <- roosts %>%
  filter(roost_date == "2024-04-30") 

# check that we have the same individuals accounted for
length(unique(oneday$individual_id)) == length(unique(roosts_oneday$individual_id))
all(unique(oneday$individual_id) %in% unique(roosts_oneday$individual_id))
all(unique(roosts_oneday$individual_id) %in% unique(oneday$individual_id))

mapview(roosts_oneday)

oneday_list <- oneday %>%
  group_by(individual_id) %>%
  group_split()

combined_list <- vector(mode = "list", length = length(oneday_list))
for(i in 1:length(combined_list)){
  indiv <- oneday_list[[i]]$individual_id[1]
  out <- bind_rows(roosts_oneday %>% filter(individual_id == indiv) %>% mutate(type = "roost"), oneday_list[[i]])
  combined_list[[i]] <- out
}

# Get distance from roost
combined_list <- map(combined_list, ~{.x %>% mutate(dist_from_roost = as.numeric(st_distance(.x[1,], .x)))})

combined <- combined_list %>%
  purrr::list_rbind()

combined %>%
  filter(individual_id %in% sample(unique(combined$individual_id), 10)) %>%
  ggplot(aes(x = timestamp, y = dist_from_roost/1000, col = factor(individual_id)))+
  geom_point()+
  geom_line()+
  theme_classic()+
  theme(legend.position = "none")
  
  