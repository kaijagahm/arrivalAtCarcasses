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
library(ggraph)
library(tidygraph)
library(move2)
library(gganimate)
library(ggspatial)

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

data_timeordered <- map2(data_timeordered, leftpoints, ~{
  .x$left_roost <- FALSE
  if(!is.na(.y)){
    .x$left_roost[.y] <- TRUE
  }
  return(.x)
})

data_rejoined <- purrr::list_rbind(data_timeordered)

leaving_points <- data_rejoined %>%
  filter(left_roost)

data_rejoined %>%
  filter(date_il == min(date_il)) %>%
  ggplot(aes(x = timestamp_il, y = individual_local_identifier, color = in_a_roost, size = left_roost))+
  geom_point()+
  theme_minimal()+
  scale_size_manual(values = c(0.5, 1.5), name = "Departure")+
  theme(axis.title.x = element_blank())

# Now apply to each date # XXX start here with moving to targets pipeline
leaving_points_dates <- leaving_points %>% group_by(date_il) %>%
  group_split()

# Make a "same roost" matrix
# RoostID represents the roost that they left from, one point before this
roost_mats <- purrr::map(leaving_points_dates, ~{
  mat <- outer(.x$roostID, .x$roostID, FUN = "==") * 1
  rownames(mat) <- .x$individual_local_identifier
  colnames(mat) <- .x$individual_local_identifier
  return(mat)
})

roost_mats_long <- purrr::map(roost_mats, ~{
  .x %>% as.data.frame() %>% rownames_to_column("ID1") %>%
    pivot_longer(cols = -ID1, names_to = "ID2", values_to = "same_roost")
})

# Make a "time difference" matrix
difftime_mats <- purrr::map(leaving_points_dates, ~{
  mat <- outer(.x$timestamp_il, .x$timestamp_il,
               function(t1, t2) as.numeric(abs(difftime(t1, t2, units = "mins"))))
  rownames(mat) <- .x$individual_local_identifier
  colnames(mat) <- .x$individual_local_identifier
  return(mat)
})

difftime_mats_long <- purrr::map(difftime_mats, ~{
  .x %>% as.data.frame() %>% rownames_to_column("ID1") %>%
    pivot_longer(cols = -ID1, names_to = "ID2", values_to = "time_diff_min")
})

# Find cases where roost_mat == 1 and difftime_mat < 10 mins
both <- purrr::map2(roost_mats_long, difftime_mats_long, left_join)
sync_departures <- purrr::map(both, ~{
  .x %>% filter(same_roost == 1) %>%
    select(-same_roost) %>%
    filter(ID1 < ID2) # remove self loops and repeats
})

# Now let's visualize these as roost departure networks. Is the 10 min threshold too high? Do we have dyads, or a lot of groups leaving together?
edgelists <- map2(sync_departures, leaving_points_dates, ~{
  .x %>% rename("from" = ID1, "to" = ID2) %>%
    mutate(weight = 1 / (time_diff_min + 1))
})

departure_nets <- map2(edgelists, leaving_points_dates, ~{
  tbl_graph(edges = .x, directed = F) %>%
    activate(nodes) %>%
    left_join(distinct(select(.y, individual_local_identifier, roostID)), by = c("name" = "individual_local_identifier"))
})

departure_dates <- map_chr(leaving_points_dates, ~as.character(.x$date_il[1]))

departure_graphs <- map2(departure_nets, departure_dates, ~{
  .x %>% activate(edges) %>%
    filter(time_diff_min <= 10) %>%
    ggraph(layout = "fr") +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    geom_edge_link(aes(width = weight), alpha = 0.7, 
                   show.legend = F) +
    geom_node_point(size = 2, aes(color = factor(roostID)),
                    show.legend = F) +
    theme_graph()+
    scale_color_discrete(name = "Roost polygon")+
    scale_edge_width(name = "Temporal proximity", range = c(0.1, 2.5))+
    facet_nodes(~factor(roostID), nrow = 3) +
    NULL+
    labs(title = paste0("Synchronized roost departures,\n", .y),
         subtitle = "10 min threshold",
         caption = "Heavier edges represent more similar departure times (inverse time interval).\n On this scale, 1 = 0 min difference; 0 = 10 min difference.\n Time differences > 10 min are not shown.")
})

departure_graphs[[1]]
departure_graphs[[2]]
departure_graphs[[3]]

# I could try interpolating locations, but I worry that linear interpolation doesn't make a lot of sense for departure times, since by definition we expect they spent some of the time sitting still and some of the time flying. By definition, linear is an inappropriate interpolation method to use.
# https://bartk.gitlab.io/move2/reference/mt_interpolate.html

# What about the trajectories after the departures?
head(data_rejoined)
data_rejoined <- sf::st_as_sf(data_rejoined)

# Get data for a single day, and perhaps for a single roost
# Plot lines onto a map to show trajectories
# How do we calculate distances between the trajectories to demonstrate co-movement? We can't necessarily assume that points line up with each other.
# I'm sure someone has thought of this--maybe we can make the points into continuous trajectories using the move2 or ctmm package?
# "We tested the ‘following behaviour’ for the subset of dyads that departed synchronously from the roost and performed long daily flights (daily displacement more than 15 km; 518 dyads; see the electronic supplementary material,"
# "We focused on three indices of the ‘following behaviour’, (i) the proportion of the flight time that individuals spent close to each other (within detection range); (ii) the mean distance between individuals during the flight; (iii) whether the informed was leading the dyad, namely closer to the goal site and how this changed along the joint flight."

# So we need to calculate individual daily displacement, restrict the GPS data to only dyads that flew far enough (does this restriction make sense for us, or not?) and then plot it onto the map...
# I think linear interpolation actually might make more sense for the subsequent trajectories, because then we can measure distance along the interpolated points and actually look at dyad distance over time.

# Let's start by just plotting some dyads on the map
departure_gps <- data_rejoined %>% filter(date_il == lubridate::ymd("2022-11-14")) %>% filter(individual_local_identifier %in% c("K39", "A75w"))

# Let's look at K39 and A75w
tar_load(stations)
mapview(departure_gps, zcol = "individual_local_identifier")+mapview(stations, col.regions = "red") # okay, these ones aren't near any stations, but there could still be a carcass... anyway let's move on

mv <- mt_as_move2(
  departure_gps,
  coords = c("location_long", "location_lat"),
  time = "timestamp_il",
  track_id = "individual_local_identifier",
  crs = st_crs(departure_gps)
)

mv <- mv %>%
  arrange(individual_local_identifier, timestamp_il)

mv <- mv %>% # drop any duplicates
  distinct(individual_local_identifier, timestamp_il, .keep_all = TRUE)

tar_load(gps_spd)
# interpolate to 5 min timestamps
interpolated_5min <- mt_interpolate(
  mv[!sf::st_is_empty(mv), ],
  time = seq(
    as.POSIXct("2022-11-14 00:00:00"),
    as.POSIXct("2022-11-14 11:59:00"), "5 mins"
  ),
  max_time_lag = units::as_units(1, "hours"),
  omit = TRUE
) %>%
  mutate(interp = T) %>%
  bind_rows(mutate(mv[!sf::st_is_empty(mv), ], interp = F)) %>%
  arrange(individual_local_identifier, timestamp_il) %>%
  
  select(individual_local_identifier, date_il, timestamp_il, ground_speed, interp, , roost_X, roost_Y, roostID, roostID_gps, in_a_roost, left_roost) %>%
  ungroup()

interpolated_5min <- interpolated_5min %>%
  mutate(flight = ground_speed > gps_spd) %>%
  arrange(individual_local_identifier, timestamp_il) %>%
  fill(date_il) %>%
  group_by(individual_local_identifier, date_il) %>%
  fill(flight) %>%
  fill(roost_X) %>%
  fill(roost_Y) %>%
  fill(roostID) %>%
  fill(left_roost) %>%
  ungroup()

after_departure <- interpolated_5min %>%
  group_by(individual_local_identifier, date_il) %>%
  mutate(after_departure = cumsum(left_roost)) %>%
  filter(after_departure > 0) %>%
  ungroup() %>%
  select(-after_departure)


# calculate distance at each timestep
pairwise_distances <- after_departure %>%
  filter(interp) %>% # keep only interpolated points so they'll be aligned
  group_by(timestamp_il) %>%
  filter(n() == 2) %>%
  group_modify(~ {
    if (nrow(.x) != 2) return(NULL)
    
    tibble(
      timestamp_il = .x$timestamp_il[1],
      id1 = .x$individual_local_identifier[1],
      id2 = .x$individual_local_identifier[2],
      flight1 = .x$flight[1],
      flight2 = .x$flight[2],
      distance_m = as.numeric(st_distance(.x$geometry[1],
                                          .x$geometry[2]))
    )
  }) %>%
  ungroup() %>%
  mutate(flight_status = case_when(flight1 & flight2 ~ "both",
                                   (flight1 & !flight2)|(!flight1 & flight2) ~ "one",
                                   !flight1 & !flight2 ~ "zero"))

distances %>%
  ggplot(aes(x = timestamp_il, y = distance_m, color = flight_status)) +
  geom_point()+
  theme_classic()+
  labs(y = "Distance apart (m)",
       color = "Flight?",
       x = "Timestamp",
       title = c("K39 and A75w on 2022-11-14"))

# This looks like a reasonable way to look at post-departure flights.

# Now we need to calculate daily displacement for each individual
displ <- after_departure %>%
  group_by(individual_local_identifier) %>%
  mutate(displacement = c(st_distance(
    !!!syms(attr(., "sf_column")),
    (!!!syms(attr(., "sf_column")))[row_number() == 1]
  )))

max_displacement <- displ %>%
  group_by(individual_local_identifier, date_il) %>%
  summarize(max_displ_m = max(displacement, na.rm = T))
# XXX will have to join this back on to look at whether the dyad falls within the max displacement range or not.

#"We focused on three indices of the ‘following behaviour’, (i) the proportion of the flight time that individuals spent close to each other (within detection range); (ii) the mean distance between individuals during the flight; (iii) whether the informed was leading the dyad, namely closer to the goal site and how this changed along the joint flight."
tar_load(ddf) # we're using 2km ddf

distances <- distances %>%
  mutate(in_sight = case_when(distance_m <= ddf ~ T, .default = F))

distances %>%
  group_by(id1, id2) %>%
  summarize(prop_both_flying = sum(flight_status == "both")/n(),
            prop_in_sight = sum(in_sight)/n(),
            prop_in_sight_both_flying = sum(flight_status == "both" & in_sight)/n())

# Informed/uninformed status of carcasses
#"Hence, we classified individuals as informed in cases they were within detection range (i.e. less than 4 km) of an existing carcass in the 2 days preceding the feeding event. These included cases in which individuals ate from the carcass at previous days, landed but did not eat or flew above the carcass but did not land. Cases in which individuals were between 4 and 10 km from the food resource were excluded (425 dyads) to avoid false classification of individual’s information status."

tar_load(all_carcasses_south)
all_carcasses_active <- all_carcasses_south %>%
  select(carcID, carcType, date) %>%
  mutate(maxdate = date + lubridate::days(3),
         year = factor(lubridate::year(date))) %>%
  mutate(date_seq = map2(date, maxdate, ~ seq(.x, .y, by = "1 day"))) %>%
  unnest(date_seq) %>%
  select(carcID, carcType, date = date_seq, year, geometry)

active_daily_buffered <- all_carcasses_active %>%
  st_buffer(ddf) %>% # buffer by ddf. Do we want to do something different, like 4km?
  group_by(date) %>%
  group_split()

active_daily_buffered_10km <- all_carcasses_active %>%
  st_buffer(10000) %>%
  group_by(date) %>%
  group_split() # "Cases in which individuals were between 4 and 10 km from the food resource were excluded (425 dyads) to avoid false classification of individual’s information status."

names(active_daily_buffered) <- map_chr(active_daily_buffered, ~as.character(.x$date[1]))
names(active_daily_buffered_10km) <- map_chr(active_daily_buffered_10km, ~as.character(.x$date[1]))

# Graph the availability of carcasses
all_carcasses_active %>%
  st_drop_geometry() %>%
  group_by(year, date, carcType) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = date, y = n, color = carcType))+
  geom_line()+
  facet_wrap(~year, scales = "free_x")+
  theme_minimal() # okay cool, we've seen something like this before.

# Now I'd like to get trajectories for each individual on each day and then figure out
## 1. How many active carcasses did they pass by on each day?
## 2. For each active carcass, were they informed about it or not on each day?
tar_load(downsampled)
tar_load(bbox_south_big)
downsampled_south <- st_crop(downsampled, bbox_south_big)
mv_all <- mt_as_move2(
  downsampled_south,
  coords = c("location_long", "location_lat"),
  time = "timestamp_il",
  track_id = "individual_local_identifier",
  crs = st_crs(downsampled_south)
)

# daily_tracks <- mv_all %>%
#   group_by(individual_local_identifier, date_il) %>%
#   filter(n() > 1) %>%
#   summarise(do_union = FALSE) %>%   # keeps point order
#   st_cast("LINESTRING")
# write_rds(daily_tracks, file = "data/created/daily_tracks.RDS")
daily_tracks <- readRDS("data/created/daily_tracks.RDS")

daily_tracks_list <- daily_tracks %>%
  group_by(date_il) %>%
  group_split()
names(daily_tracks_list) <- map_chr(daily_tracks_list, ~as.character(.x$date_il[1]))

names(daily_tracks_list) %in% names(active_daily_buffered) # I think the reason these don't match up is because the GPS data includes 30 days before(?) the hf carcass periods
tar_load(minmax_dates)
dates <- as.Date(unlist(map(minmax_dates, lubridate::date)))
nms <- names(daily_tracks_list)
tokeep <- c(which(nms >= dates[1] & nms <= dates[2]),
            which(nms >= dates[3] & nms <= dates[4]),
            which(nms >= dates[5] & nms <= dates[6]))
daily_tracks_list <- daily_tracks_list[tokeep]

nms <- names(active_daily_buffered)
tokeep <- c(which(nms >= dates[1] & nms <= dates[2]),
            which(nms >= dates[3] & nms <= dates[4]),
            which(nms >= dates[5] & nms <= dates[6]))
active_daily_buffered <- active_daily_buffered[tokeep]
active_daily_buffered_10km <- active_daily_buffered_10km[tokeep]

length(daily_tracks_list) == length(active_daily_buffered) # FALSE, because there are a couple dates with no active carcasses I guess?

names(active_daily_buffered) %in% names(daily_tracks_list)
names(daily_tracks_list) %in% names(active_daily_buffered)

daily_tracks_list <- daily_tracks_list[names(active_daily_buffered)] # keep only the ones from both dates
length(daily_tracks_list) == length(active_daily_buffered) # TRUE now
length(daily_tracks_list) == length(active_daily_buffered_10km)
names(daily_tracks_list) == names(active_daily_buffered)
names(daily_tracks_list) == names(active_daily_buffered_10km)

informed_matrices <- map2(daily_tracks_list, active_daily_buffered, ~{
  out <- st_intersects(.x, .y, sparse = F)
  rownames(out) <- .x$individual_local_identifier
  colnames(out) <- .y$carcID
  return(out)
  })

informed_matrices_10km <- map2(daily_tracks_list, active_daily_buffered_10km, ~{
  out <- st_intersects(.x, .y, sparse = F)
  rownames(out) <- .x$individual_local_identifier
  colnames(out) <- .y$carcID
  return(out)
})

names(informed_matrices) <- names(daily_tracks_list)
names(informed_matrices_10km) <- names(daily_tracks_list)

informed_long <- map(informed_matrices, ~{
  int_long <- as.data.frame(.x) %>%
    tibble::rownames_to_column("individual_local_identifier") %>%
    pivot_longer(
      -individual_local_identifier,
      names_to = "carcID",
      values_to = "intersects"
    )
  return(int_long)
})

informed_long_10km <- map(informed_matrices_10km, ~{
  int_long <- as.data.frame(.x) %>%
    tibble::rownames_to_column("individual_local_identifier") %>%
    pivot_longer(
      -individual_local_identifier,
      names_to = "carcID",
      values_to = "intersects"
    )
  return(int_long)
})
names(informed_long) <- names(daily_tracks_list)
names(informed_long_10km) <- names(daily_tracks_list)

informed_long_df <- purrr::list_rbind(informed_long, names_to = "date_il") %>%
  mutate(carcID = as.numeric(carcID)) %>%
  left_join(all_carcasses_south %>% select(carcID, carcType, year), by = "carcID")

informed_long_df_10km <- purrr::list_rbind(informed_long_10km, names_to = "date_il") %>%
  mutate(carcID = as.numeric(carcID)) %>% rename("intersects_10km" = intersects)
#this should now have all possible combinations, as well as the info and locations of the carcasses!

informed_joined <- left_join(informed_long_df, informed_long_df_10km, by = c("date_il", "individual_local_identifier", "carcID")) %>%
  relocate(intersects_10km, .after = "intersects") %>%
  mutate(informed = intersects, # considering anything between 4km and 10km as "maybe"
         uninformed = !intersects & !intersects_10km,
         maybe = !intersects & intersects_10km) %>%
  select(date_il, individual_local_identifier, carcID, carcType, year, informed, maybe, uninformed, geometry)

joined_long <- informed_joined %>%
  pivot_longer(cols = c("informed", "maybe", "uninformed"), names_to = "status", values_to = "lgl") %>%
  filter(lgl) %>%
  select(-lgl)

# Individuals per carcass -------------------------------------------------
# How many individuals saw each carcass each day?
indivs_per_carcass <- informed_long_df %>%
  group_by(carcID, carcType, year, date_il) %>%
  summarize(informed = sum(intersects),
            total = n()) %>%
  ungroup() %>%
  group_by(carcID, carcType, year) %>%
  arrange(date_il, .by_group = T) %>%
  mutate(day = 1:n()) %>%
  ungroup() %>%
  mutate(prop = round(informed/total, 4))

## Raw numbers, lines
indivs_per_carcass %>%
  ggplot(aes(x = day, y = informed, group = carcID, color = carcType))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)+
  theme_minimal()

## Proportions, lines
indivs_per_carcass %>%
  ggplot(aes(x = day, y = prop, group = carcID, color = carcType))+
  geom_point()+
  geom_line()+
  facet_wrap(~year)+
  theme_minimal() # very similar

## Proportions, boxplot
indivs_per_carcass %>%
  ggplot(aes(x = factor(day), y = prop, color = carcType, fill = carcType))+
  geom_line(aes(group = carcID), alpha = 0.2)+
  geom_boxplot(alpha = 0.2)+
  facet_wrap(~year)+
  theme_minimal()+
  labs(y = "Proportion informed",
       x = "Day",
       color = "Type", fill = "Type")
# Note that this is only who is directly flying by the carcass that day, not cumulative over time

# What about cumulative?
indivs_per_carcass_cumul <- informed_long_df %>%
  filter(intersects) %>%
  arrange(year, carcID, carcType, date_il) %>%
  group_by(year, carcID, individual_local_identifier) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(year, carcID, carcType, date_il) %>%
  summarize(firsts = length(unique(individual_local_identifier))) %>%
  mutate(day = 1:n(),
         cum_indivs = cumsum(firsts)) %>%
  ungroup()

indivs_per_carcass_cumul %>%
  ggplot(aes(x = factor(day), y = cum_indivs, color = carcType))+
  geom_line(aes(group = carcID), alpha = 0.2)+
  geom_boxplot(alpha = 0.2)+
  facet_wrap(~year, ncol = 1)+
  theme_minimal()+
  labs(y = "Cumulative indivs",
       x = "Day",
       color = "Type")

# Carcasses per individual ------------------------------------------------
## How many carcasses does each individual see on each day?
indiv_carc_stats <- informed_long_df %>%
  group_by(individual_local_identifier, date_il, year) %>%
  summarize(total = length(unique(carcID)),
            missed = length(unique(carcID[!intersects])),
            seen = length(unique(carcID[intersects])),
            total_wild = length(unique(carcID[carcType == "wild"])),
            total_stn = length(unique(carcID[carcType == "stn"])),
            missed_wild = length(unique(carcID[carcType == "wild" & !intersects])),
            missed_stn = length(unique(carcID[carcType == "stn" & !intersects])),
            seen_wild = length(unique(carcID[carcType == "wild" & intersects])),
            seen_stn = length(unique(carcID[carcType == "stn" & intersects]))) %>%
  ungroup()

indiv_carc_stats %>%
  ggplot(aes(x = individual_local_identifier, y = seen))+
  geom_boxplot(outlier.size = 0.5)+
  facet_wrap(~year, scales = "free_x", ncol = 1)+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  labs(y = "Seen per day", x = "Individual")
# Most individuals see 1-5 carcasses per day, but most individuals also have days when they see 0 carcasses.

indiv_carc_stats %>%
  pivot_longer(cols = c("seen", "seen_wild", "seen_stn"), names_to = "type", values_to = "seen") %>%
  mutate(type = str_remove(type, "seen_"),
         type = case_when(type == "seen" ~ "total", .default = type)) %>%
  filter(type != "total") %>%
  ggplot(aes(x = individual_local_identifier, y = seen, color = type))+
  geom_boxplot(outlier.size = 0.5)+
  facet_wrap(~year, scales = "free_x", nrow = 1)+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(y = "Seen per day", x = "Individual")+
  coord_flip()

## What proportion of the active carcasses does each individual see on each day?
indiv_carc_stats %>%
  mutate(prop_seen = seen/total) %>%
  ggplot(aes(x = individual_local_identifier, y = prop_seen))+
  geom_boxplot(outlier.size = 0.5)+
  facet_wrap(~year, scales = "free_x")+
  theme_minimal()+
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())+
  labs(y = "Proportion seen per day", x = "Individual")+
  coord_flip() # on average, individuals are seeing 20-40% of the carcasses available on the landscape each day.

indiv_carc_stats %>%
  mutate(prop_seen = seen/total) %>%
  ggplot(aes(x = jitter(total), y = jitter(prop_seen), color = factor(year)))+
  geom_point(pch = 1, alpha = 0.75)+
  theme_minimal()+
  geom_smooth(method = "lm") # no strong relationship between number of carcasses available and the proportion seen
