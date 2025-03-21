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
seed_distance <- 1000 # 1000m to be within sight of the carcass
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
       caption = "Detections within 4 days of carcass placement.\nArrival: vulture on ground (<5m/s) within 400m of carcass.\nDetection: vulture within 1000m of carcass.")


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
       caption = "Detections within 4 days of carcass placement.\nArrival: vulture on ground (<5m/s) within 400m of carcass.\nDetection: vulture within 1000m of carcass.")

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
  ggplot(aes(x = factor(carcID), y = diff))+
  geom_boxplot()+
  theme_minimal()+
  facet_wrap(~year, scales = "free") # As Noa pointed out, there needs to be variation in this in order for it to be interesting to do multi-state NBDA. We see very little variation here.

# Why?
# - Could be the same points--the detection is essentially the same as the arrival
# - Could be that we have too small a detection range--there isn't enough time between when they are within 1km and when they eventually land for a GPS fix to be taken; they're already in the process of going towards it.
# What to do?
# Could check whether they are the same point; could restrict detections to "aerial detections"; could increase the detection radius.
# Let's look at some approach graphs to see how far away the vultures are and when they land and what speed they're going. Do they just drop down really fast, perhaps? XXX TODO

which(has_sightings & has_visits)

set.seed(11)
focal <- sample(unique(gps[[13]]$local_identifier), 3)
max_timestamp <- all %>%
  filter(carcID == gps[[13]]$carcID[1],
         local_identifier %in% focal) %>%
  pull(timestamp) %>%
  max()
gray_data <- gps[[13]] %>%
  filter(timestamp <= max_timestamp,
         !(local_identifier %in% focal))
colored_data <- gps[[13]] %>%
  filter(timestamp <= max_timestamp,
         local_identifier %in% focal)

gray_data %>%
  filter(dist_to_carcass <= 10000) %>%
  ggplot(aes(x = timestamp, y = dist_to_carcass/1000, group = local_identifier))+
  geom_line(col = "gray", alpha = 0.3)+
  geom_line(data = colored_data %>% filter(dist_to_carcass <= 10000), aes(color = ground_speed))+
  scale_color_viridis_c()+
  theme_classic()+
  labs(y = "Distance to carcass (km)",
       x = "Time",
       color = "Speed")
# Okay, at least for these individuals, we see that they are approaching the carcass very very fast from a long way away. Maybe they are going straight from the roost?

colored_data %>%
  ggplot(aes(x = timestamp, y = dist_to_carcass/1000, group = local_identifier))+
  geom_line(aes(color =ground_speed))+
  geom_point(aes(color = ground_speed), size = 0.8)+
  scale_color_viridis_c()+
  theme_classic() # so, yes, these individuals seem to be going directly to the carcass from their roosts, which are 15-20km away (KG: that was a different sample--these ones are flying first). Either way, the 1km detection threshold seems way too small. 

# Let's add some lines and also zoom in on a shorter time range.
colored_data %>%
  filter(timestamp > lubridate::ymd_hm("2023-03-31 04:00")) %>%
  ggplot(aes(x = timestamp, y = dist_to_carcass/1000, group = local_identifier))+
  geom_line(aes(color =ground_speed))+
  geom_point(aes(color = ground_speed), size = 0.8)+
  geom_hline(aes(yintercept = 4), alpha = 0.5, linetype = 3, color = "magenta")+
  geom_hline(aes(yintercept = 2), alpha = 0.6, linetype = 2, color = "magenta")+
  geom_hline(aes(yintercept = 1), alpha = 0.7, linetype = 1, color = "magenta")+
  scale_color_viridis_c()+
  theme_classic() # okay so now we can see the approaches more clearly. And indeed we can see that the time between being 1km away and arriving at the carcass is often very short, so it's hit or miss whether there will be a point in there or not. 

# It seems like a 4km or 5km threshold for detection would be better probably. Let's go with 4km because that was what was in Orr's paper and that's what Cassidy is using.

