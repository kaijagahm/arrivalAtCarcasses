library(tidyverse)
library(igraph)
library(NBDA)
library(vultureUtils)
library(targets)
library(sf)
library(here)
library(tidygraph)
library(ggraph)

# Now time to look at the NBDA code ---------------------------------------
tar_load(all_carcasses_annotated)
tar_load(all_bouts_annotated)
tar_load(bbox_south)
aca <- all_carcasses_annotated %>% filter(year == "2024") %>% st_crop(bbox_south)
all_bouts_annotated %>%
  filter(carcID %in% aca$carcID) %>%
  group_by(carcID) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) # let's use one with a lot of bouts: 4874955

mycarc <- aca %>%
  filter(carcID == "4874955")
date_placed <- lubridate::date(mycarc$datetime)
plusfour <- date_placed + days(4)

# Downloaded code for tits finding colored wool (Vistalli et al. 2023)
# Determined I need the following:

# *For one single carcass, at first*
#   - dataset of GPS points in the south, from beginning of placement day to +4 days
gps <- data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv") %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>%
  st_transform(32636) %>%
  st_crop(bbox_south)
gps_mycarc <- gps %>%
  filter(dateOnly >= date_placed & dateOnly <= plusfour) %>%
  st_transform("WGS84") %>%
  bind_cols(st_coordinates(.)) %>%
  rename("location_long" = X,
         "location_lat" = Y) %>%
  mutate(dateOnly = lubridate::ymd(dateOnly))
dim(gps_mycarc) # a lot of gps points in the south between day of placement and 4 days later
length(unique(gps_mycarc$individual_id)) # 59 individuals detected in the south between day of placement and 4 days later.

# - co-flight network, from beginning of placement day to +4 days. consecThreshold = 1.
rp <- sf::st_read(here("data/raw/roosts50_kde95_cutOffRegion.kml"))
coflight <- getFlightEdges(gps_mycarc, roostPolygons = rp, roostBuffer = 50,
               consecThreshold = 1, distThreshold = 1000,
               speedThreshUpper = NULL, speedThreshLower = 5,
               timeThreshold = "10 minutes",
               idCol = "individual_id",
               return = "sri")
g <- igraph::graph_from_data_frame(coflight, directed = FALSE)
t_g <- tidygraph::as_tbl_graph(g) %>% activate(edges) %>%
  filter(!is.na(sri) & sri > 0)
ggraph(t_g) +
  geom_edge_link(aes(width = sri), alpha = 0.5)+
  geom_node_point(size = 4, color = "dodgerblue")+
  scale_edge_width(range = c(0, 1))+
  theme_classic()

# - roosts for each vulture on each night, beginning the night before the carcass was placed. In order to get this, we need to add two extra days of data (since both morning and night are necessary for roost computation). Need date_placed-days(1) through plusfour + days(1)
gps_mycarc_forroosts <- gps %>%
  # need to include the previous day
  filter(dateOnly >= (date_placed-days(1)) & dateOnly <= (plusfour + days(1))) %>%
  st_transform("WGS84") %>%
  bind_cols(st_coordinates(.)) %>%
  rename("location_long" = X,
         "location_lat" = Y) %>%
  mutate(dateOnly = lubridate::ymd(dateOnly))
r <- get_roosts_df(gps_mycarc_forroosts, id = "individual_id")
length(unique(r$roost_date)) # we have roosts for nights including the night before the carcass was placed.
unique(r$roost_date)
table(r$roost_date)
r_list <- r %>%
  group_by(roost_date) %>%
  group_split() %>%
  map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"), remove = F, crs = "WGS84") %>% st_transform(32636))

# - matrices of pairwise distances between them
indivs <- map(r_list, ~.x$individual_id)
roost_pairwise_distances <- map(r_list, ~as.data.frame(st_distance(.x))) %>%
  map2(., indivs, ~{
    out <- .x
    names(out) <- .y
    row.names(out) <- .y
    return(out)
    }) %>%
  map(., ~.x %>% mutate(across(everything(), as.numeric)))

# - daily centers of activity for each vulture, excluding roosts
activity_list <- 
  gps_mycarc %>%
  group_by(dateOnly) %>%
  group_split()

activity_centers <- map(activity_list, ~.x %>%
                          group_by(individual_id) %>%
                          summarize(st_union(geometry)) %>%
                          st_centroid() %>%
                          st_transform(32636))
indivs_activity <- map(activity_centers, ~.x$individual_id)

# - matrix of pairwise distances between them
activity_centers_pairwise_distances <- map(activity_centers, ~as.data.frame(st_distance(.x))) %>%
  map2(., indivs_activity, ~{
    out <- .x
    names(out) <- .y
    row.names(out) <- .y
    return(out)
  }) %>%
  map(., ~.x %>% mutate(across(everything(), as.numeric)))

# - overall center of activity for each vulture from carcass placement day to +4 days
overall_activity_centers <- gps_mycarc %>%
  group_by(individual_id) %>%
  summarize(st_union(geometry)) %>%
  st_centroid() %>%
  st_transform(32636)

# - matrix of pairwise distances between them
indivs_overall <- overall_activity_centers$individual_id
overall_activity_centers_pairwise_distances <- as.data.frame(st_distance(overall_activity_centers)) %>%
  mutate(across(everything(), as.numeric))
names(overall_activity_centers_pairwise_distances) <- indivs_overall
row.names(overall_activity_centers_pairwise_distances) <- indivs_overall

# - time of first arrivals to carcass
distances <- as.numeric(st_distance(st_transform(gps_mycarc, 32636), mycarc))
gps_mycarc$dist_to_carc <- distances

at_carcass <- gps_mycarc %>%
  filter(ground_speed < 5) %>%
  filter(dist_to_carc < 250)

first_at_carcass <- at_carcass %>%
  arrange(timestamp) %>%
  group_by(individual_id) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(timestamp) %>%
  mutate(rownumber = 1:n())

# quick viz
first_at_carcass %>% 
  ggplot(aes(x = timestamp, y = rownumber))+
  geom_point()+
  geom_path()+
  labs(y = "Number of unique individuals",
       x = "Time")

# - age and sex for each individual
# it won't be trivial to join this information onto the gps data, since I didn't do a good job maintaining the IDs. Let's just proceed with no ILVs for now and add them later. 