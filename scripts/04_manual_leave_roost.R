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

# Now apply to each date
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

# Out of curiosity, how much more specific can we get? First, what are the intervals between consecutive points for individuals?
intervals <- test_gps %>%
  st_drop_geometry() %>%
  group_by(individual_local_identifier, date_il) %>%
  arrange(timestamp_il, .by_group = T) %>%
  mutate(interval = as.numeric(difftime(timestamp_il, lag(timestamp_il), units = "mins")))

intervals %>%
  # removing 200+ minute intervals because most individuals have just a few of those, which are probably during the night
  filter(date_il == lubridate::ymd("2022-10-15"), interval < 200) %>%
  ggplot(aes(x = individual_local_identifier, y = interval))+
  geom_boxplot(outlier.size = 0.5)+
  theme_minimal()+
  theme(axis.text.x = element_blank())+
  labs(y = "Interval (mins)", x = "Individual")

# so we have some very short intervals but also a lot of very long intervals. But an important thing we see here is that almost nobody has intervals less than 10 minutes. So I don't think it's really possible to directly calculate departure intervals less than 10 minutes.

# I could try interpolating locations, but I worry that linear interpolation doesn't make a lot of sense for departure times, since by definition we expect they spent some of the time sitting still and some of the time flying. By definition, linear is an inappropriate interpolation method to use.
# https://bartk.gitlab.io/move2/reference/mt_interpolate.html

# What about the trajectories after the departures?
head(test_gps)

# Get data for a single day, and perhaps for a single roost
# Plot lines onto a map to show trajectories
# How do we calculate distances between the trajectories to demonstrate co-movement? We can't necessarily assume that points line up with each other.
# I'm sure someone has thought of this--maybe we can make the points into continuous trajectories using the move2 or ctmm package?
# "We tested the ‘following behaviour’ for the subset of dyads that departed synchronously from the roost and performed long daily flights (daily displacement more than 15 km; 518 dyads; see the electronic supplementary material,"
# "We focused on three indices of the ‘following behaviour’, (i) the proportion of the flight time that individuals spent close to each other (within detection range); (ii) the mean distance between individuals during the flight; (iii) whether the informed was leading the dyad, namely closer to the goal site and how this changed along the joint flight."

# So we need to calculate individual daily displacement, restrict the GPS data to only dyads that flew far enough (does this restriction make sense for us, or not?) and then plot it onto the map...
# I think linear interpolation actually might make more sense for the subsequent trajectories, because then we can measure distance along the interpolated points and actually look at dyad distance over time.

# Let's start by just plotting some dyads on the map
departure_gps <- test_gps %>% filter(date_il == lubridate::ymd("2022-11-14")) %>% filter(individual_local_identifier %in% c("K39", "A75w"))

departure_lines <- departure_gps %>%
  arrange(individual_local_identifier, timestamp_il) %>%
  group_by(individual_local_identifier) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

# Let's look at K39 and A75w
tar_load(stations)
mapview(departure_gps, zcol = "individual_local_identifier")+mapview(stations, col.regions = "red") # okay, these ones aren't near any stations, but there could still be a carcass... anyway let's move on

ggplot(mapping = aes(color = individual_local_identifier)) +
  geom_sf(data = departure_lines, linewidth = 0.8, aes(alpha = timestamp_il)) +
  geom_sf(data = departure_gps, size = 2)

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
  select(individual_local_identifier, timestamp_il)

# calculate distance at each timestep
distances <- interpolated_5min %>%
  group_by(timestamp_il) %>%
  group_modify(~ {
    if (nrow(.x) != 2) return(NULL)
    
    tibble(
      timestamp_il = .x$timestamp_il[1],
      id1 = .x$individual_local_identifier[1],
      id2 = .x$individual_local_identifier[2],
      distance_m = as.numeric(st_distance(.x$geometry[1],
                                          .x$geometry[2]))
    )
  }) %>%
  ungroup()

distances %>%
  ggplot(aes(x = timestamp_il, y = distance_m)) +
  geom_line()

# Okay, so this is a start in terms of plotting how far apart they are. But in order to do what they did, we would also need to figure out which of these interpolated locations are "flight" so we can calculate proportion of flight time spent together, and figure out when they're flying. Or maybe we just extract the flight segments and then interpolate those? But that sounds really complicated.
