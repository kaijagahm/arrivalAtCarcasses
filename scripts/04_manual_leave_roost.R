# Testing out manual roost departure dyad identification
# Plan:
#   get roost location data
# remove buffered feeding stations from roost polygons to create new roost polygons
# assign roost location data to polygons
# assign each individual's gps data to inside/outside roost polygons
# for each individual, find first GPS point of the day that's outside of the same roost that they roosted in
# find all possible dyads per night and roost location and calculate distances and time differences

library(tidyverse)
library(sf)
library(targets)
library(mapview)

# get roost location data
tar_load(roosts_stn) # this isn't great for our purposes because it's on a per-carcass basis, but that's ok
tar_load(stn_carcs) # should be same length as roosts_stn
# get roost polygons, minus feeding stations
tar_load(rp_minus_stations)

# get gps data
tar_load(stn_gps_30days)

# select just one carcass to work with
test_roosts <- sf::st_transform(roosts_stn[[1]], 32636)
test_carc <- stn_carcs[[1]]
test_gps <- sf::st_transform(stn_gps_30days[[1]], 32636)

# how many roost dates are we working with here?
length(unique(test_roosts$roost_date))

# Assign polygons to the roost locations
st_crs(test_roosts) == st_crs(rp_minus_stations) # make sure these have the same crs before we can intersect them
test_roosts$roostID <- as.numeric(st_intersects(test_roosts, rp_minus_stations))

# Assign polygons to the gps locations
st_crs(test_gps) == st_crs(rp_minus_stations) # needs to be TRUE
test_gps$roostID_gps <- as.numeric(st_intersects(test_gps, rp_minus_stations))

# Okay now let's group this per date_il and individual
# simplify the roost data for the join
test_roosts_tojoin <- test_roosts %>% select(individual_local_identifier, roost_date, roostID) %>% bind_cols(st_coordinates(.)) %>% st_drop_geometry() %>%
  rename("roost_X" = X, "roost_Y" = Y)
test_gps <- test_gps %>%
  mutate(roost_date = date_il - lubridate::days(1)) %>% 
  left_join(test_roosts_tojoin, by = c("individual_local_identifier", "roost_date")) %>%
  mutate(in_a_roost = !is.na(roostID_gps))
glimpse(test_gps) # well now we already have a problem because it looks like some of the roost locations aren't assigned to a polygon, but some of the gps points for the same individual on the same date are assigned to a polygon.

# I don't want to deal with this yet, so let's just restrict this down to the nights when roostID is not NA
test_gps_knownroost <- test_gps %>%
  filter(!is.na(roostID))

test_date <- test_gps_knownroost %>%
  filter(date_il == test_gps_knownroost$date_il[1])

test_date %>%
  ggplot(aes(x = timestamp_il, y = individual_local_identifier, color = in_a_roost))+
  geom_point()+
  theme_minimal()

# On the basis of this graph, I think we should consider individuals to have "left" the roost when they have had two points in a row outside of it, starting from the earliest point that day.

data_timeordered <- test_gps_knownroost %>%
  group_by(date_il, individual_local_identifier) %>%
  group_split()

get_leftroost <- function(ordered_df, threshold){
  n <- nrow(ordered_df)
  rle_obj <- rle(as.numeric(ordered_df$in_a_roost))
  first_seq_out <- min(which(rle_obj$lengths >= threshold & rle_obj$values == 0))
  if(!is.infinite(first_seq_out)){
    first_point_out <- sum(rle_obj$lengths[1:(first_seq_out-1)])+1
    if(first_point_out > n){
      first_point_out <- NA
    }
  }else{
    first_point_out <- NA
  }
  return(first_point_out)
}

leftpoints <- map_dbl(data_timeordered, ~get_leftroost(.x, threshold = 2))

test <- map2(data_timeordered, leftpoints, ~{
  .x$left_roost <- FALSE
  if(!is.na(.y)){
    .x$left_roost[.y] <- TRUE
  }
  return(.x)
})
