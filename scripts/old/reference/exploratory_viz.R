# Script for exploring the data created in prepare_data.R
# This is data for the INPA carcasses from the focal months in 2023 and 2024
# Packages
library(mapview)
library(sf)
library(tidyverse)
library(here)
library(ggraph)
library(tidygraph)

## 0. Define parameters (same as prepare_data.R)
days_after <- 3
seed_distance <- 4000 # 1000m to be within sight of the carcass
seed_time_before <- hours(1)

# Load data
load(here("test_dynamic_nbda/data/fl_allday_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/fl_cumulative_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/fl_1hr_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/fl_3hr_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/roosts_bin_fixed_see.Rda"))

load(here("test_dynamic_nbda/data/inpa_carcs.Rda"))

load(here("test_dynamic_nbda/data/oa_see.Rda"))
load(here("test_dynamic_nbda/data/oa_see_num.Rda"))
load(here("test_dynamic_nbda/data/firsts_see.Rda"))
load(here("test_dynamic_nbda/data/roosts_bin_nets_see.Rda"))
load(here("test_dynamic_nbda/data/fl_allday_bin_nets_see.Rda"))
load(here("test_dynamic_nbda/data/fl_cumulative_bin_nets_see.Rda"))
load(here("test_dynamic_nbda/data/fl_1h_bin_nets_see.Rda"))
load(here("test_dynamic_nbda/data/fl_3h_bin_nets_see.Rda"))

load(here("test_dynamic_nbda/data/gps.Rda"))

# 1. How many birds arrive/see the carcass, based on weight and time of day placed?
# KG note: in previous versions of this data, I had thought that way more of the carcasses never had any visits at all, which didn't make much sense. Turns out the indexes were misaligned (ughhh). But on the bright side, this makes so much more sense now!!
df <- bind_rows(inpa_carcs) %>%
  mutate(time_of_day = lubridate::hour(datetime),
         year = lubridate::year(datetime)) %>%
  mutate(number_of_seen = map_dbl(firsts_see, ~.x %>% filter(!is.na(local_identifier)) %>% nrow(.))) %>%
  pivot_longer(cols = contains("number_of"), names_to = "Measure", values_to = "number") %>%
  mutate(Measure = case_when(Measure == "number_of_firsts" ~ "Arrivals"))

df %>%
  ggplot(aes(x = time_of_day, y = number))+
  geom_point(aes(size = carcassWeight), alpha = 0.5)+
  geom_smooth(method = "lm", alpha = 0.1)+
  facet_wrap(~year)+
  theme_minimal()+ # doesn't seem to be related to hour of day
  labs(y = "Number of vultures detecting",
       x = "Hour of carcass placement",
       size = "Carcass\nweight",
       caption = "Detections within 4 days of carcass placement.\nArrival: vulture on ground (<5m/s) within 400m of carcass.\nDetection: vulture within 4000m of carcass.")


## maybe it's related to the size of the carcass
df %>%
  ggplot(aes(x = carcassWeight, y = number))+
  geom_point(aes(size = time_of_day), alpha = 0.5)+
  geom_smooth(method = "lm", alpha = 0.1)+
  facet_wrap(~year)+
  theme_minimal()+ # this also doesn't produce a clear relationship.
  labs(y = "Number of vultures detecting",
       x = "Carcass weight",
       size = "Hour of carcass placement",
       caption = "Detections within 4 days of carcass placement.\nArrival: vulture on ground (<5m/s) within 400m of carcass.\nDetection: vulture within 4000m of carcass.")

# For the networks, we are already only dealing with the carcasses that have sightings by vultures
load(here("test_dynamic_nbda/data/has_sightings.Rda"))
carcs <- inpa_carcs[has_sightings] # get the carcasses corresponding to the networks, in case we need them

tolong <- function(list, id, tp){
  df <- map(list, ~.x %>%
              mutate(ID1 = row.names(.)) %>%
              pivot_longer(cols = -ID1, names_to = "ID2", values_to = "inter"))
  out <- data.table::rbindlist(df, idcol = id) %>% mutate(type = tp)
  return(out)
}

# here, "carc" is the numerical index of which carcass we're using, after already filtering by "has_visits"
compile_networks_long <- function(carc){
  # Get acquisition event dates
  idx <- firsts_see[has_sightings][[carc]] %>%
    group_by(dateOnly) %>% summarize(n = n()) %>%
    pull(n)
  idx_day <- data.frame(acq = 1:sum(idx), day = rep(1:length(idx), times = idx))
  
  # Get long-format data for each type of network
  r_long <- tolong(roosts_bin_fixed_see[[carc]], id = "day", tp = "roost")
  f_a_long <- tolong(fl_allday_bin_fixed_see[[carc]], id = "day", tp = "fl_a")
  f_c_long <- tolong(fl_cumulative_bin_fixed_see[[carc]], id = "acq", tp = "fl_c")
  f_1h_long <- tolong(fl_1hr_bin_fixed_see[[carc]], id = "acq", tp = "fl_1h")
  f_3h_long <- tolong(fl_3hr_bin_fixed_see[[carc]], id = "acq", tp = "fl_3h")
  
  # Join the networks and convert to wide (separately for the daily networks and the per-acquisition networks)
  ## daily networks (roost and daily flight)
  inters_daily <- bind_rows(f_a_long, r_long) %>%
    mutate(dyad_id = paste(ID1, ID2, sep = "_"))
  inters_daily_wide <- pivot_wider(inters_daily, id_cols = c("dyad_id", "ID1", "ID2", "day"), names_from = "type", values_from = "inter")
  
  ## per-acquisition networks (cumulative, 1 hour, 3 hour)
  inters <- bind_rows(f_c_long, f_1h_long, f_3h_long) %>%
    left_join(idx_day) %>%
    mutate(dyad_id = paste(ID1, ID2, sep = "_"))
  inters_wide <- inters %>%
    pivot_wider(id_cols = c("dyad_id", "ID1", "ID2", "acq", "day"), names_from = "type", values_from = "inter")
  head(inters_wide)
  
  # Combine into a single wide-format data frame containing all networks
  inters_all_wide <- left_join(inters_wide, inters_daily_wide) # should include all the different networks at different scales in the same data frame.
  return(inters_all_wide)
}
networks_long <- map(1:length(carcs), compile_networks_long) 
names(networks_long) <- map_dbl(carcs, "carcID")
networks_long <- as.data.frame(data.table::rbindlist(networks_long, idcol = "carcID"))

# Visualizations ----------------------------------------------------------
plt <- function(g, title_slug, i){
  title <- paste(title_slug, i, sep = " ")
  ggraph(g)+
    geom_edge_link()+
    geom_node_label(aes(label = name))+
    theme_graph()+
    ggtitle(title)
}
carc <- inpa_carcs[[13]] # carcass information
carc # 2023-03-30 12:54:29, Hahalak_mount
oa_see[[13]] # order of arrivals to this carcass
mapview(carc)
tr <- roosts_bin_nets_see[[13]]
tr_g <- map2(tr, 1:length(tr), ~plt(.x, "Roosts, night", .y))

tfa <- fl_allday_bin_nets_see[[13]]
tfa_g <- map2(tfa, 1:length(tfa), ~plt(.x, "Flight, day", .y))

tfc <- fl_cumulative_bin_nets_see[[13]]
tfc_g <- map2(tfc, 1:length(tfc), ~plt(.x, "Flight (cumulative),\nacquisition event", .y))

tf1 <- fl_1h_bin_nets_see[[13]]
tf1_g <- map2(tf1, 1:length(tf1), ~plt(.x, "Flight (1 hour before),\nacquisition event", .y))

tf3 <- fl_3h_bin_nets_see[[13]]
tf3_g <- map2(tf3, 1:length(tf3), ~plt(.x, "Flight (3 hours before),\nacquisition event", .y))

# Correlations?
# Jamie wanted to know: does who you roost with predict who you fly with?
# DeepSeek suggests running a GLMM to account for the repeated-measures structure of the data, but when I tried, it didn't run (too much data?)

# 2025-03-12: Visualizing arrivals at and detections of carcasses ---------------------------
load(here("test_dynamic_nbda/data/firsts_see.Rda"))
load(here("test_dynamic_nbda/data/firsts.Rda"))

length(inpa_carcs)
length(firsts_see)
length(firsts)

fs <- map(firsts_see, st_drop_geometry) %>% purrr::list_rbind() %>% mutate(type = "detection")
f <- map(firsts, st_drop_geometry) %>% purrr::list_rbind() %>% mutate(type = "arrival")
ic <- purrr::list_rbind(inpa_carcs) %>% st_drop_geometry() %>% select(carcID, datetime)
all <- bind_rows(fs, f) %>% left_join(ic) %>%
  filter(!is.na(local_identifier)) %>%
  mutate(time_since_placement = difftime(timestamp, datetime, units = "hours")) %>%
  group_by(carcID) %>%
  mutate(time_since_first = difftime(timestamp, timestamp[1], units = "hours")) %>%
  mutate(prop = rownumber/max(rownumber)) %>%
  ungroup()

# Accumulation curves since carcass placement
all %>%
  ggplot(aes(x = time_since_placement, y = rownumber, col = factor(carcID)))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~type, nrow = 2)+
  theme(legend.position = "none")+
  labs(y = "Number of vultures",
       x = "Hours since carcass placement")

# Accumulation curves since first arrival/detection
all %>%
  ggplot(aes(x = time_since_first, y = rownumber, col = factor(carcID)))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~type, nrow = 2)+
  theme(legend.position = "none")+
  labs(y = "Number of vultures",
       x = "Hours since first vulture")

# Accumulation curves since carcass placement (proportion)
all %>%
  ggplot(aes(x = time_since_placement, y = prop, col = factor(carcID)))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~type, nrow = 2)+
  theme(legend.position = "none")+
  labs(y = "Proportion of vultures",
       x = "Hours since carcass placement")

# Accumulation curves since first arrival/detection (proportion)
all %>%
  ggplot(aes(x = time_since_first, y = prop, col = factor(carcID)))+
  geom_line()+
  theme_minimal()+
  facet_wrap(~type, nrow = 2)+
  theme(legend.position = "none")+
  labs(y = "Proportion of vultures",
       x = "Hours since carcass placement")

# Simultaneous presence of other carcasses -----------------------
carcs <- purrr::list_rbind(inpa_carcs)
max_times <- carcs$datetime + days(days_after)
carcs <- carcs %>%
  mutate(max_time = max_times)

carcs %>%
  ggplot(aes(y = factor(carcID)))+
  geom_segment(aes(x = datetime, xend = max_time, col = Y, linewidth = carcassWeight))+
  scale_color_viridis()+
  facet_wrap(~year, scales = "free")+
  theme_minimal()+
  labs(y = "Carcass",
       x = "Datetime",
       color = "UTM Northing",
       linewidth = "Carcass weight (kg)",
       title = "Carcass provisioning",
       caption = "Bars begin at carcass placement and end three days later.")

# Difference between arrival and detection --------------------------------
arr_det_diffs <- all %>%
  mutate(year = lubridate::year(datetime)) %>%
  select(-c(prop, time_since_placement, time_since_first)) %>%
  pivot_wider(id_cols = c("carcID", "local_identifier", "year"),
              names_from = "type",
              values_from = "timestamp") %>%
  mutate(diff = difftime(arrival, detection, units = "hours"))

arr_det_diffs %>%
  filter(!is.na(diff)) %>%
  group_by(carcID) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  arrange(n_indivs) %>%
  ggplot(aes(x = fct_reorder(factor(carcID), n), y = diff, fill = n))+
  geom_boxplot(outlier.size = 0.5)+
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")+
  facet_wrap(~year, scales = "free")+
  scale_fill_viridis()+
  labs(y = "Hours between first detection and first arrival",
       x = "Carcass",
       fill = "Number of arrivals",
       caption = "Including vultures that eventually arrived at the carcass.\nDetection <= 4000m; arrival: <= 400m non-flying",
       title = "Time from detection to arrival")
  # As Noa pointed out, there needs to be variation in this in order for it to be interesting to do multi-state NBDA. Some carcasses have very little variation, but some have a lot!

# Preliminaries for Nina model --------------------------------------------
# Each hour: distance to closest active carcass
# - "active" = within the max_time of the `carcs` data frame
# Each hour: Within 4km of another vulture?
gps_2023 <- data.table::fread("data/ACC/2023_hf_period/created/gps_2023.csv")
#gps_2024 <- data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv")
gps_points <- gps_2023 %>% arrange(timestamp) %>% sf::st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>% sf::st_transform(32636)
dim(gps_points)

carcs <- sf::st_as_sf(carcs) %>% sf::st_set_crs(32636)
carcs_buffered <- st_buffer(carcs, dist = 4000) # buffer by 4km
mapview(carcs_buffered)
# Ensure the timestamps are in the correct format
gps_points$timestamp <- as.POSIXct(gps_points$timestamp)
carcs$datetime <- as.POSIXct(carcs$datetime)
carcs$max_time <- as.POSIXct(carcs$max_time)

# Create an empty vector to store the distances
near_active_carcass <- rep(NA, nrow(gps_points))
for(i in 1:nrow(gps_points)){
  active_carcs <- carcs_buffered[carcs_buffered$datetime <= gps_points$timestamp[i] & carcs_buffered$max_time >= gps_points$timestamp[i],]
  if(nrow(active_carcs) > 0){
    near_active_carcass[i] <- sum(lengths(st_intersects(active_carcs, gps_points[i,]))) > 0
  }
  if(i%%1000 == 0){
    cat("done with ", i, "\n")
  }
}

table(near_active_carcass, exclude = NULL)

gps_points$near_active_carcass <- near_active_carcass
gps_points %>%
  ggplot(aes(x = timestamp, y = factor(local_identifier), col = near_active_carcass))+
  geom_point(size = 0.5)+
  labs(y = "Vulture",
       x = "Timestamp",
       col = "Near\nactive\ncarcass",
       caption = "Active = within 3 days from carcass placement.\nNear = within 4km\nNA = no current active carcasses")+
  theme_minimal()
