# Exploratory visualizations--approaches to carcass
# Altitudes as vultures descend to the carcass
library(sf)
library(tidyverse)
library(here)
library(targets)
library(mapview)
source(here("R/functions.R"))
library(vultureUtils)
tar_load(inpa)
tar_load(wild)
wild_og <- wild

plots_inpa <- readRDS(here("data/plots_inpa.RDS"))
plots_wild_valid <- readRDS(here("data/plots_wild_valid.RDS"))
names(plots_inpa)
length(plots_inpa) # all 81 carcasses
names(plots_wild_valid) 
length(plots_wild_valid) # only 14 wild carcasses that we are considering to be valid at this point.
tar_load(www)

# Let's look at the trajectories of vultures that approach one known inpa carcass and one known wild carcass
tar_load(gps_all_wild)
tar_load(gps_all_inpa)
tar_load(inpa_carcs)
tar_load(wild_carcs)
tar_load(bbox_south_big)
tar_load(roostPolygons, store = "~/Desktop/projects/MvmtSoc/_targets/")
rp <- sf::st_read(roostPolygons) %>% sf::st_transform(32636)
rp_cropped <- sf::st_crop(rp, bbox_south_big)
focal <- gps_all_inpa[[36]] %>% filter(time_since_carcass > -24 & time_since_carcass < 48)
focal %>% count(local_identifier) %>% arrange(desc(n)) 
set.seed(3)
carc <- inpa_carcs[[36]]
id <- carc$carcID
id_wild <- 52

# Can we identify decision points, like Cassidy's turning points?
# let's get each instance of landing near a carcass and then walk the GPS back 10 hours and look at the track.
# each instance of landing near a carcass--need to go to the code with the histograms and get the data for all the INPA carcasses. bind into one df, group by individual, sort by time, identify the first instance of being on the ground nearby, and then grab the previous 10 hours.
tar_load(gps_spd)
tar_load(detection_distance_flight)
tar_load(detection_distance_stationary)
ddf <- detection_distance_flight
dds <- detection_distance_stationary
stn <- purrr::list_rbind(gps_all_inpa) %>% sf::st_drop_geometry() %>% 
  mutate(type = "inpa") %>%
  mutate(hour_bin = floor_date(timestamp, unit = "hours"),
         hour_bin_rel = round(time_since_carcass),
         in_sight = case_when(ground_speed >= 5 & dist_to_carcass <= ddf ~ T,
                              ground_speed < 5 & dist_to_carcass <= dds ~ T,
                              .default = F),
         status = case_when(ground_speed >= 5 & dist_to_carcass <= ddf ~ "flight, in sight (<2km)",
                            ground_speed >= 5 & dist_to_carcass > ddf ~ "flight, >2km",
                            ground_speed < 5 & dist_to_carcass <= dds & dist_to_carcass > 200 ~ "stationary, in sight (1km-200m)",
                            ground_speed <= 5 & dist_to_carcass <= 200 ~ "stationary, <200m",
                            ground_speed <= 5 & dist_to_carcass > dds ~ "stationary, >1km", .default = NA),
         status = factor(status, levels = c("stationary, <200m", "stationary, in sight (1km-200m)", "flight, in sight (<2km)", "flight, >2km", "stationary, >1km")),
         hour = round(time_since_carcass)) %>%
  select(-c("tag_local_identifier", "tag_id", "hour_bin", "hour_bin_rel", "in_sight"))

wild <- purrr::list_rbind(gps_all_wild) %>% sf::st_drop_geometry() %>% 
  mutate(type = "wild") %>%
  mutate(hour_bin = floor_date(timestamp, unit = "hours"),
         hour_bin_rel = round(time_since_carcass),
         in_sight = case_when(ground_speed >= 5 & dist_to_carcass <= ddf ~ T,
                              ground_speed < 5 & dist_to_carcass <= dds ~ T,
                              .default = F),
         status = case_when(ground_speed >= 5 & dist_to_carcass <= ddf ~ "flight, in sight (<2km)",
                            ground_speed >= 5 & dist_to_carcass > ddf ~ "flight, >2km",
                            ground_speed < 5 & dist_to_carcass <= dds & dist_to_carcass > 200 ~ "stationary, in sight (1km-200m)",
                            ground_speed <= 5 & dist_to_carcass <= 200 ~ "stationary, <200m",
                            ground_speed <= 5 & dist_to_carcass > dds ~ "stationary, >1km", .default = NA),
         status = factor(status, levels = c("stationary, <200m", "stationary, in sight (1km-200m)", "flight, in sight (<2km)", "flight, >2km", "stationary, >1km")),
         hour = round(time_since_carcass)) %>%
  select(-c("tag_local_identifier", "tag_id", "hour_bin", "hour_bin_rel", "in_sight"))

# Calculating all approaches to the carcasses
# Step 1: Preprocess - compute dist_diff, runs of decreasing distance, and mark landings
stn_processed <- stn %>%
  filter(time_since_carcass > -1) %>%  # only data after carcass placement
  arrange(carcID, local_identifier, timestamp) %>%
  group_by(carcID, local_identifier) %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    dist_diff = dist_to_carcass - lag(dist_to_carcass),
    neg = dist_diff < 0,
    run_id = data.table::rleid(neg),
    landing = status == "stationary, <200m"
  ) %>%
  ungroup()

wild_processed <- wild %>%
  filter(time_since_carcass > -1) %>%  # only data after carcass placement
  arrange(carcID, local_identifier, timestamp) %>%
  group_by(carcID, local_identifier) %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    dist_diff = dist_to_carcass - lag(dist_to_carcass),
    neg = dist_diff < 0,
    run_id = data.table::rleid(neg),
    landing = status == "stationary, <200m"
  ) %>%
  ungroup()

# Step 2: Identify all landing points
landings <- stn_processed %>%
  group_by(carcID, local_identifier) %>%
  filter(landing & (is.na(lag(landing)) | !lag(landing))) %>% # get any landing that's the first in the carcass/individual group, or the first in a run of landing points for that carcass and individual
  ungroup() %>%
  select(carcID, local_identifier, run_id, timestamp) %>%
  mutate(timestamp = as.POSIXct(timestamp))  # ensure proper type

landings_wild <- wild_processed %>%
  group_by(carcID, local_identifier) %>%
  filter(landing & (is.na(lag(landing)) | !lag(landing))) %>% # get any landing that's the first in the carcass/individual group, or the first in a run of landing points for that carcass and individual
  ungroup() %>%
  select(carcID, local_identifier, run_id, timestamp) %>%
  mutate(timestamp = as.POSIXct(timestamp))  # ensure proper type

# Step 3: For each landing, get points in same run_id that occurred before or at the landing
# approach_list <- map(1:nrow(landings), function(i) {
#   landing <- as.data.frame(landings[i, , drop = FALSE])  # Keep as 1-row tibble
#   cid <- landing$carcID[1]
#   li <- landing$local_identifier[1]
#   ri <- landing$run_id[1]
#   ts <- landing$timestamp[1]
#   
#   stn_processed %>%
#     dplyr::filter(
#       carcID == cid,
#       local_identifier == li,
#       run_id == ri,
#       timestamp <= ts
#     ) %>%
#     mutate(approach_id = i)
# }, .progress = T) # this is really slow!!!
# save(approach_list, file = here("data/approach_list.Rda"))
load(here("data/approach_list.Rda"))

# approach_list_wild <- map(1:nrow(landings_wild), function(i) {
#   landing <- as.data.frame(landings_wild[i, , drop = FALSE])  # Keep as 1-row tibble
#   cid <- landing$carcID[1]
#   li <- landing$local_identifier[1]
#   ri <- landing$run_id[1]
#   ts <- landing$timestamp[1]
#   
#   wild_processed %>%
#     dplyr::filter(
#       carcID == cid,
#       local_identifier == li,
#       run_id == ri,
#       timestamp <= ts
#     ) %>%
#     mutate(approach_id = i)
# }, .progress = T) # this is really slow!!!
# save(approach_list_wild, file = here("data/approach_list_wild.Rda"))
load(here("data/approach_list_wild.Rda"))

approach_points <- list_rbind(approach_list) %>%
  group_by(carcID, local_identifier, approach_id) %>%
  filter(n() > 1) %>% # only keeping "approaches" that include the landing point plus at least one more point
  ungroup() %>%
  group_by(carcID, local_identifier) %>%
  mutate(first_approach = case_when(approach_id == min(approach_id) ~ T,
                                    .default = F))
dim(approach_points)

approach_points_wild <- list_rbind(approach_list_wild) %>%
  group_by(carcID, local_identifier, approach_id) %>%
  filter(n() > 1) %>% # only keeping "approaches" that include the landing point plus at least one more point
  ungroup() %>%
  group_by(carcID, local_identifier) %>%
  mutate(first_approach = case_when(approach_id == min(approach_id) ~ T,
                                    .default = F))
dim(approach_points_wild)


mycarc <- 4436953
mycarc2 <- 4890051
mycarc_wild <- 52
mycarc_info <- inpa_carcs[map_lgl(inpa_carcs, ~.x$carcID ==mycarc)][[1]]
mycarc2_info <- inpa_carcs[map_lgl(inpa_carcs, ~.x$carcID ==mycarc2)][[1]]
mycarc_wild_info <- wild_carcs[map_lgl(wild_carcs, ~.x$carcID ==mycarc_wild)][[1]]

approach_points_mycarc <- approach_points %>%
  filter(carcID == mycarc)
approach_points_mycarc2 <- approach_points %>%
  filter(carcID == mycarc2)
approach_points_mycarc_wild <- approach_points_wild %>%
  filter(carcID == mycarc_wild)


set.seed(3)
indivs <- sample(unique(approach_points_mycarc$local_identifier), 6)
indivs2 <- sample(unique(approach_points_mycarc2$local_identifier), 6)
indivs_wild <- sample(unique(approach_points_mycarc_wild$local_identifier), 6)

stn %>%
  filter(local_identifier %in% indivs & carcID == mycarc) %>%
  filter(height_above_msl < 2500) %>% # remove a single outlier
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, 
             col = height_above_msl, group = local_identifier))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(19, 1))+
  facet_wrap(~local_identifier)+
  scale_color_viridis_c()+
  theme_minimal()+ # all of these have fairly clear declines as the individual approaches, and sometimes those declines continue over many km.
  labs(y = "Dist to carcass (km)",
       x = "Time since carcass (hours)",
       caption = mycarc)

stn %>%
  filter(local_identifier %in% indivs2 & carcID == mycarc2) %>%
  filter(height_above_msl < 2500) %>% # remove a single outlier
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, 
             col = height_above_msl, group = local_identifier))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(19, 1))+
  facet_wrap(~local_identifier)+
  scale_color_viridis_c()+
  theme_minimal()+ # all of these have fairly clear declines as the individual approaches, and sometimes those declines continue over many km.
  labs(y = "Dist to carcass (km)",
       x = "Time since carcass (hours)",
       caption = mycarc2)

wild %>%
  filter(local_identifier %in% indivs_wild & carcID == mycarc_wild) %>%
  filter(height_above_msl < 2500) %>% # remove a single outlier
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, 
             col = height_above_msl, group = local_identifier))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(19, 1))+
  facet_wrap(~local_identifier)+
  scale_color_viridis_c()+
  theme_minimal()+ # all of these have fairly clear declines as the individual approaches, and sometimes those declines continue over many km.
  labs(y = "Dist to carcass (km)",
       x = "Time since carcass (hours)",
       caption = mycarc_wild)

approach_points_mycarc %>%
  filter(local_identifier %in% indivs) %>%
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, 
             col = height_above_msl, shape = landing, group = interaction(local_identifier, approach_id)))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(19, 1))+
  facet_wrap(~local_identifier)+
  scale_color_viridis_c()+
  theme_minimal()+ # all of these have fairly clear declines as the individual approaches, and sometimes those declines continue over many km.
  labs(y = "Dist to carcass (km)",
       x = "Time since carcass (hours)",
       color = "Altitude (m)",
       caption = mycarc) # awesome! we're seeing the distance to the carcass decrease over time as the vulture approaches.

approach_points_mycarc2 %>%
  filter(local_identifier %in% indivs2) %>%
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, 
             col = height_above_msl, shape = landing, group = interaction(local_identifier, approach_id)))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(19, 1))+
  facet_wrap(~local_identifier)+
  scale_color_viridis_c()+
  theme_minimal()+ # all of these have fairly clear declines as the individual approaches, and sometimes those declines continue over many km.
  labs(y = "Dist to carcass (km)",
       x = "Time since carcass (hours)",
       color = "Altitude (m)",
       caption = mycarc2) # awesome! we're seeing the distance to the carcass decrease over time as the vulture approaches.

approach_points_mycarc_wild %>%
  filter(local_identifier %in% indivs_wild) %>%
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, 
             col = height_above_msl, shape = landing, group = interaction(local_identifier, approach_id)))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(19, 1))+
  facet_wrap(~local_identifier)+
  scale_color_viridis_c()+
  theme_minimal()+ # all of these have fairly clear declines as the individual approaches, and sometimes those declines continue over many km.
  labs(y = "Dist to carcass (km)",
       x = "Time since carcass (hours)",
       color = "Altitude (m)",
       caption = mycarc_wild) # awesome! we're seeing the distance to the carcass decrease over time as the vulture approaches.

approach_points_mycarc %>%
  filter(local_identifier %in% indivs & first_approach) %>%
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, 
             col = height_above_msl, shape = landing, group = interaction(local_identifier, approach_id)))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(19, 1))+
  facet_wrap(~local_identifier, 
             scales = "free_x")+
  scale_color_viridis_c()+
  theme_minimal()+
  labs(y = "Dist to carcass (km)",
       x = "Time since carcass (hours)",
       caption = mycarc)

approach_points_mycarc2 %>%
  filter(local_identifier %in% indivs2 & first_approach) %>%
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, 
             col = height_above_msl, shape = landing, group = interaction(local_identifier, approach_id)))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(19, 1))+
  facet_wrap(~local_identifier, 
             scales = "free_x")+
  scale_color_viridis_c()+
  theme_minimal()+
  labs(y = "Dist to carcass (km)",
       x = "Time since carcass (hours)",
       caption = mycarc2)

approach_points_mycarc_wild %>%
  filter(local_identifier %in% indivs_wild & first_approach) %>%
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, 
             col = height_above_msl, shape = landing, group = interaction(local_identifier, approach_id)))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(19, 1))+
  facet_wrap(~local_identifier, 
             scales = "free_x")+
  scale_color_viridis_c()+
  theme_minimal()+
  labs(y = "Dist to carcass (km)",
       x = "Time since carcass (hours)",
       caption = mycarc_wild)

approach_points_mycarc %>% ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, col = local_identifier, group = interaction(local_identifier, approach_id)))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "none")+
  labs(y = "Distance to carcass (km)",
       x = "Hours since carcass",
       title = "All approaches to the carcass",
       subtitle = "Colored by individual",
       caption = mycarc)

approach_points_mycarc2 %>% ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, col = local_identifier, group = interaction(local_identifier, approach_id)))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "none")+
  labs(y = "Distance to carcass (km)",
       x = "Hours since carcass",
       title = "All approaches to the carcass",
       subtitle = "Colored by individual",
       caption = mycarc2)

approach_points_mycarc_wild %>% ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, col = local_identifier, group = interaction(local_identifier, approach_id)))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "none")+
  labs(y = "Distance to carcass (km)",
       x = "Hours since carcass",
       title = "All approaches to the carcass",
       subtitle = "Colored by individual",
       caption = mycarc_wild)

approach_points_mycarc %>% 
  filter(local_identifier %in% indivs) %>%
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, group = interaction(local_identifier, approach_id)))+
  geom_line(linewidth = 1.5, alpha = 0.75, aes(col = local_identifier))+
  geom_line(data = approach_points_mycarc %>% 
              filter(!(local_identifier %in% indivs)),
            alpha = 0.1)+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(y = "Distance to carcass (km)",
       x = "Hours since carcass",
       title = "All approaches to the carcass",
       subtitle = "6 individuals highlighted",
       caption = mycarc)

approach_points_mycarc2 %>% 
  filter(local_identifier %in% indivs2) %>%
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, group = interaction(local_identifier, approach_id)))+
  geom_line(linewidth = 1.5, alpha = 0.75, aes(col = local_identifier))+
  geom_line(data = approach_points_mycarc2 %>% 
              filter(!(local_identifier %in% indivs2)),
            alpha = 0.1)+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(y = "Distance to carcass (km)",
       x = "Hours since carcass",
       title = "All approaches to the carcass",
       subtitle = "6 individuals highlighted",
       caption = mycarc2)

approach_points_mycarc_wild %>% 
  filter(local_identifier %in% indivs_wild) %>%
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, group = interaction(local_identifier, approach_id)))+
  geom_line(linewidth = 1.5, alpha = 0.75, aes(col = local_identifier))+
  geom_line(data = approach_points_mycarc_wild %>% 
              filter(!(local_identifier %in% indivs_wild)),
            alpha = 0.1)+
  theme_classic()+
  theme(legend.position = "bottom")+
  labs(y = "Distance to carcass (km)",
       x = "Hours since carcass",
       title = "All approaches to the carcass",
       subtitle = "6 individuals highlighted",
       caption = mycarc_wild)

approach_points_mycarc %>% 
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, col = first_approach, group = interaction(local_identifier, approach_id)))+
  geom_line()+
  theme_classic()+
  #theme(legend.position = "none")+
  labs(y = "Distance to carcass (km)",
       x = "Hours since carcass",
       title = "All approaches to the carcass",
       subtitle = "Colored by first/later approach",
       caption = mycarc,
       color = "First approach")

approach_points_mycarc2 %>% 
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, col = first_approach, group = interaction(local_identifier, approach_id)))+
  geom_line()+
  theme_classic()+
  #theme(legend.position = "none")+
  labs(y = "Distance to carcass (km)",
       x = "Hours since carcass",
       title = "All approaches to the carcass",
       subtitle = "Colored by first/later approach",
       caption = mycarc2,
       color = "First approach")

# approach_points_mycarc_wild %>% 
#   mutate(first_approach = factor(first_approach, levels =c(FALSE, TRUE))) %>%
#   ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, col = first_approach, group = interaction(local_identifier, approach_id)))+
#   geom_line(show.legend = TRUE)+
#   theme_classic()+
#   #theme(legend.position = "none")+
#   labs(y = "Distance to carcass (km)",
#        x = "Hours since carcass",
#        title = "All approaches to the carcass",
#        subtitle = "Colored by first/later approach",
#        caption = mycarc_wild,
#        color = "First approach")

# Okay what about looking at the max distance of each approach
breaks <- c(-24, 0, 24, 48, 72, 96)
approaches_stats <- approach_points_mycarc %>%
  filter(as.numeric(hour) <=72) %>%
  mutate(day = cut(as.numeric(hour), breaks)) %>%
  group_by(carcID, local_identifier, day, first_approach) %>%
  summarize(max_dist_km = max(dist_to_carcass/1000),
            time_start = min(timestamp),
            time_arrive = max(timestamp),
            approach_start = min(time_since_carcass))

approaches_stats2 <- approach_points_mycarc2 %>%
  filter(as.numeric(hour) <=72) %>%
  mutate(day = cut(as.numeric(hour), breaks)) %>%
  group_by(carcID, local_identifier, day, first_approach) %>%
  summarize(max_dist_km = max(dist_to_carcass/1000),
            time_start = min(timestamp),
            time_arrive = max(timestamp),
            approach_start = min(time_since_carcass))

approaches_stats_wild <- approach_points_mycarc_wild %>%
  filter(as.numeric(hour) <=72) %>%
  mutate(day = cut(as.numeric(hour), breaks)) %>%
  group_by(carcID, local_identifier, day, first_approach) %>%
  summarize(max_dist_km = max(dist_to_carcass/1000),
            time_start = min(timestamp),
            time_arrive = max(timestamp),
            approach_start = min(time_since_carcass))

approaches_stats %>%
  ggplot(aes(x = approach_start, y = max_dist_km, col = first_approach, group = interaction(first_approach, day)))+
  geom_smooth(method = "lm")+
  geom_point(pch = 1, size = 2)+
  theme_minimal()+
  labs(y = "Start distance of approach (km)",
       x = "Start time of approach (hours since carcass)",
       color = "First approach",
       title = "Approach distances",
       subtitle = "First 72 hours",
       caption = mycarc)

approaches_stats2 %>%
  ggplot(aes(x = approach_start, y = max_dist_km, col = first_approach, group = interaction(first_approach, day)))+
  geom_smooth(method = "lm")+
  geom_point(pch = 1, size = 2)+
  theme_minimal()+
  labs(y = "Start distance of approach (km)",
       x = "Start time of approach (hours since carcass)",
       color = "First approach",
       title = "Approach distances",
       subtitle = "First 72 hours",
       caption = mycarc2)

approaches_stats_wild %>%
  ggplot(aes(x = approach_start, y = max_dist_km, col = first_approach, group = interaction(first_approach, day)))+
  geom_smooth(method = "lm")+
  geom_point(pch = 1, size = 2)+
  theme_minimal()+
  labs(y = "Start distance of approach (km)",
       x = "Start time of approach (hours since carcass)",
       color = "First approach",
       title = "Approach distances",
       subtitle = "First 72 hours",
       caption = mycarc_wild)

approaches_stats %>%
  ggplot(aes(x = approach_start, y = max_dist_km, group = day))+
  geom_smooth(method = "lm")+
  geom_point(pch = 1, size = 2)+
  theme_minimal()+
  labs(y = "Start distance of approach (km)",
       x = "Start time of approach (hours since carcass)",
       color = "First approach",
       title = "Approach distances",
       subtitle = "First 72 hours",
       caption = mycarc)

approaches_stats2 %>%
  ggplot(aes(x = approach_start, y = max_dist_km, group = day))+
  geom_smooth(method = "lm")+
  geom_point(pch = 1, size = 2)+
  theme_minimal()+
  labs(y = "Start distance of approach (km)",
       x = "Start time of approach (hours since carcass)",
       color = "First approach",
       title = "Approach distances",
       subtitle = "First 72 hours",
       caption = mycarc2)

# Start distance of vultures vs. cumulative number of vultures that have arrived that day (as opposed to raw time)
approaches_stats <- approaches_stats %>%
  ungroup() %>%
  arrange(time_arrive) %>%
  mutate(n_already_arrived = (1:n())-1)%>% 
  group_by(day) %>%
  mutate(n_already_arrived_day = (1:n())-1)

approaches_stats2 <- approaches_stats2 %>%
  ungroup() %>%
  arrange(time_arrive) %>%
  mutate(n_already_arrived = (1:n())-1) %>% 
  group_by(day) %>%
  mutate(n_already_arrived_day = (1:n())-1)

approaches_stats_wild <- approaches_stats_wild %>%
  ungroup() %>%
  arrange(time_arrive) %>%
  mutate(n_already_arrived = (1:n())-1) %>% 
  group_by(day) %>%
  mutate(n_already_arrived_day = (1:n())-1)

approaches_stats %>%
  ggplot(aes(x = n_already_arrived_day, y = max_dist_km))+
  geom_point(pch = 1)+
  theme_minimal()+
  geom_smooth(method = "lm", alpha = 0.2)+
  labs(y = "Start distance of approach (km)",
       x = "# vultures arrived that day",
       caption = mycarc)+
  facet_wrap(~day)

approaches_stats2 %>%
  ggplot(aes(x = n_already_arrived_day, y = max_dist_km))+
  geom_point(pch = 1)+
  theme_minimal()+
  geom_smooth(method = "lm", alpha = 0.2)+
  labs(y = "Start distance of approach (km)",
       x = "# vultures arrived that day",
       caption = mycarc2)+
  facet_wrap(~day)

approaches_stats_wild %>%
  ggplot(aes(x = n_already_arrived_day, y = max_dist_km))+
  geom_point(pch = 1)+
  theme_minimal()+
  geom_smooth(method = "lm", alpha = 0.2)+
  labs(y = "Start distance of approach (km)",
       x = "# vultures arrived that day",
       caption = mycarc_wild)+
  facet_wrap(~day)

approaches_stats %>%
  ggplot(aes(x = n_already_arrived_day, y = max_dist_km, col = first_approach))+
  geom_point(pch = 1)+
  theme_minimal()+
  geom_smooth(method = "lm", alpha = 0.2)+
  labs(color = "First approach",
       y = "Start distance of approach (km)",
       x = "# vultures arrived that day",
       caption = mycarc)+
  facet_wrap(~day)

approaches_stats2 %>%
  ggplot(aes(x = n_already_arrived_day, y = max_dist_km, col = first_approach))+
  geom_point(pch = 1)+
  theme_minimal()+
  geom_smooth(method = "lm", alpha = 0.2)+
  labs(color = "Hours since carcass",
       y = "Start distance of approach (km)",
       x = "# vultures arrived that day",
       caption = mycarc2)+
  facet_wrap(~day) # XXX fix this--it isn't perfect because we need to match starts and ends better

approaches_stats %>%
  ggplot(aes(x = time_arrive, y = n_already_arrived_day))+geom_point()+
  theme_minimal()+
  labs(title = "Arrivals over 72 hours",
       caption = mycarc,
       x = "Arrival time",
       y = "Vultures arrived that day")

approaches_stats2 %>%
  ggplot(aes(x = time_arrive, y = n_already_arrived_day))+geom_point()+
  theme_minimal()+
  labs(title = "Arrivals over 72 hours",
       caption = mycarc2,
       x = "Arrival time",
       y = "Vultures arrived that day")

approaches_stats_wild %>%
  ggplot(aes(x = time_arrive, y = n_already_arrived_day))+geom_point()+
  theme_minimal()+
  labs(title = "Arrivals over 72 hours",
       caption = mycarc_wild,
       x = "Arrival time",
       y = "Vultures arrived that day")


# X-Y coordinates of first_approaches
approach_points_mycarc <- approach_points_mycarc %>%
  ungroup() %>%
  st_as_sf() %>%
  bind_cols(st_coordinates(.))

approach_points_mycarc2 <- approach_points_mycarc2 %>%
  ungroup() %>%
  st_as_sf() %>%
  bind_cols(st_coordinates(.))

approach_points_mycarc_wild <- approach_points_mycarc_wild %>%
  ungroup() %>%
  st_as_sf() %>%
  bind_cols(st_coordinates(.))

approach_points_mycarc %>%
  mutate(day = cut(as.numeric(hour), breaks)) %>%
  arrange(local_identifier, time_since_carcass) %>%
  ggplot()+
  geom_path(aes(X, Y, col = first_approach, 
                group = interaction(local_identifier, approach_id)),
            alpha = 0.7)+
  geom_point(aes(X, Y, col = first_approach,
                 group = interaction(local_identifier, approach_id)),
             alpha = 0.7, size = 0.5)+
  geom_point(data = carc, aes(X, Y), color = "black", size = 2, pch = 3)+
  theme_minimal()+
  coord_equal()+
  labs(y = "UTM Northing",
       x = "UTM Easting",
       color = "First approach",
       caption = mycarc)+
  facet_wrap(~day)

approach_points_mycarc2 %>%
  mutate(day = cut(as.numeric(hour), breaks)) %>%
  arrange(local_identifier, time_since_carcass) %>%
  ggplot()+
  geom_path(aes(X, Y, col = first_approach, 
                group = interaction(local_identifier, approach_id)),
            alpha = 0.7)+
  geom_point(aes(X, Y, col = first_approach,
                 group = interaction(local_identifier, approach_id)),
             alpha = 0.7, size = 0.5)+
  geom_point(data = carc, aes(X, Y), color = "black", size = 2, pch = 3)+
  theme_minimal()+
  coord_equal()+
  labs(y = "UTM Northing",
       x = "UTM Easting",
       color = "First approach",
       caption = mycarc2)+
  facet_wrap(~day)

approach_points_mycarc_wild %>%
  mutate(day = cut(as.numeric(hour), breaks)) %>%
  arrange(local_identifier, time_since_carcass) %>%
  ggplot()+
  geom_path(aes(X, Y, #col = first_approach, 
                group = interaction(local_identifier, approach_id)),
            alpha = 0.5)+
  geom_point(aes(X, Y, #col = first_approach,
                 group = interaction(local_identifier, approach_id)),
             alpha = 0.7, size = 0.5)+
  geom_point(data = carc, aes(X, Y), color = "black", size = 2, pch = 3)+
  theme_minimal()+
  coord_equal()+
  labs(y = "UTM Northing",
       x = "UTM Easting",
       #color = "First approach",
       caption = mycarc_wild)+
  facet_wrap(~day)

# Investigating at least one paired carcass approach
firstday <- approach_points_mycarc %>%
  mutate(day = cut(as.numeric(hour), breaks)) %>%
  filter(day == "(0,24]")
mapview(firstday) # zoming in on the northernmost individuals, we can see that they are E79w and E37w. What do we know about those individuals?
pair1 <- c("E79w", "E37w")
pair2 <- c("T13w", "Y26b")
tar_load(www)
www %>% filter(local_identifier %in% pair1) # both juvies/subadults in 2023
www %>% filter(local_identifier %in% pair2) # one adult and one juvie/subadult in 2023

tar_load(roosts)
length(roosts)
roosts_mycarc <- roosts[map_dbl(inpa_carcs, "carcID") == mycarc][[1]]
roosts_mycarc_pair1 <- roosts_mycarc %>% filter(local_identifier %in% pair1) %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84", remove = F) %>% sf::st_transform(32636) %>% mutate(roost_date_factor = factor(roost_date))
roosts_mycarc_pair2 <- roosts_mycarc %>% filter(local_identifier %in% pair2) %>%
  sf::st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84", remove = F) %>% sf::st_transform(32636) %>% mutate(roost_date_factor = factor(roost_date))

mycarc_sf <- inpa_carcs[map_lgl(inpa_carcs, ~.x$carcID == mycarc)][[1]]


mapview(mycarc_sf, col.regions = "red")+mapview(roosts_mycarc_pair1, zcol = "roost_date_factor")

mapview(mycarc_sf, col.regions = "red")+mapview(roosts_mycarc_pair2, zcol = "roost_date_factor")

informed_inpa <- stn_processed %>%
  select(carcID, local_identifier, status, dateOnly) %>%
  distinct() %>%
  mutate(has = TRUE) %>%
  pivot_wider(id_cols = c("carcID", "local_identifier", "dateOnly"), names_prefix = "status_", names_from = "status", values_from = "has", values_fill = FALSE) %>%
  mutate(informed = case_when(`status_flight, in sight (<2km)` | `status_stationary, in sight (1km-200m)` | `status_stationary, <200m` ~ T, .default = F)) %>%
  left_join(inpa, by = "carcID") %>%
  select(-c("flagGideon", "edited_coords", "explanation", "nBouts", "nIndivs", "mintime", "maxtime"))

informed_wild <- wild_processed %>%
  select(carcID, local_identifier, status, dateOnly) %>%
  distinct() %>%
  mutate(has = TRUE) %>%
  pivot_wider(id_cols = c("carcID", "local_identifier", "dateOnly"), names_prefix = "status_", names_from = "status", values_from = "has", values_fill = FALSE) %>%
  mutate(informed = case_when(`status_flight, in sight (<2km)` | `status_stationary, in sight (1km-200m)` | `status_stationary, <200m` ~ T, .default = F)) %>%
  left_join(wild_og, by = "carcID") %>%
  select(-c("time", "itmLong_orig", "itmLat_orig", "long", "lat", "accuracy_m", "accuracy", "reportTiming", "waterFilled", "carcassType", "newFood", "cage", "flagGideon", "edited_coords", "explanation", "long_orig", "lat_orig")) %>%
  mutate(stationName = "wild", carcType = "wild")

# roost info
tar_load(roosts)
tar_load(roosts_wild)
names(roosts) <- map_dbl(inpa_carcs, "carcID")
names(roosts_wild) <- map_dbl(wild_carcs, "carcID")
roost_dates <- get_roost_dates(roosts, 1:length(roosts))
roost_dates_wild <- get_roost_dates(roosts_wild, 1:length(roosts_wild))
roosts_bin <- get_roosts_bin(roost_dates, roost_thresh = 500)
roosts_bin_wild <- get_roosts_bin(roost_dates_wild, roost_thresh = 500)
roosted_together_inpa <- map(roosts_bin, ~{
  map(.x, ~{
    .x %>%
      rownames_to_column("focal") %>%
      pivot_longer(-focal, names_to = "roostmate", values_to = "roosted_together")
  }) %>%
    purrr::list_rbind(names_to = "night")
}) %>%
  purrr::list_rbind(names_to = "carcID")

roost_date_ids <- map(roosts, ~{select(.x, roost_date) %>% distinct() %>% mutate(night = 1:n())}) %>% purrr::list_rbind(names_to = "carcID")

roosted_together_inpa <- roosted_together_inpa %>%
  left_join(roost_date_ids) %>%
  mutate(roosted_together = as.logical(roosted_together))


roosted_together_wild <- map(roosts_bin_wild, ~{
  map(.x, ~{
    .x %>%
      rownames_to_column("focal") %>%
      pivot_longer(-focal, names_to = "roostmate", values_to = "roosted_together")
  }) %>%
    purrr::list_rbind(names_to = "night")
}) %>%
  purrr::list_rbind(names_to = "carcID")

roost_date_ids_wild <- map(roosts_wild, ~{select(.x, roost_date) %>% distinct() %>% mutate(night = 1:n())}) %>% purrr::list_rbind(names_to = "carcID")

roosted_together_wild <- roosted_together_wild %>%
  left_join(roost_date_ids_wild) %>%
  mutate(roosted_together = as.logical(roosted_together))


# Now we have info about who roosted together and about who was informed. Now we need, for each night, what proportion of an individual's roostmates were informed. Since roost_date is the date that they went to the roost, we can match them up

roosted_together <- bind_rows(roosted_together_inpa, roosted_together_wild)
informed <- bind_rows(informed_inpa, informed_wild)

roostmates_informed <- roosted_together %>%
  left_join(informed %>%
              mutate(carcID = as.character(carcID)) %>%
              select(carcID, local_identifier, dateOnly, informed, carcType),
            by = c("carcID", "roost_date" = "dateOnly", "roostmate" = "local_identifier")) # XXX only accounts for roostmates that were informed directly the day before

roostmates_informed_mycarc <- roostmates_informed %>% filter(carcID == mycarc) %>%
  group_by(carcID, focal, roost_date) %>%
  summarize(prop_roostmates_informed = mean(informed, na.rm = T), .groups = "drop")

roostmates_informed_mycarc2 <- roostmates_informed %>% filter(carcID == mycarc2) %>%
  group_by(carcID, focal, roost_date) %>%
  summarize(prop_roostmates_informed = mean(informed, na.rm = T), .groups = "drop")

roostmates_informed_wild <- roostmates_informed %>% filter(carcID == mycarc_wild) %>%
  group_by(carcID, focal, roost_date) %>%
  summarize(prop_roostmates_informed = mean(informed, na.rm = T), .groups = "drop")

#XXX now bring in the NBDA ordering and see if the number of informed roostmates affects the order in which they found the carcass
load(here("data/test.Rda")) # data for NBDA (single inpa carcass)
load(here("data/test_wild.Rda")) # data for NBDA (single wild carcass)
test$oa_indivs
test$carcID
test_wild$oa_indivs
test_wild$carcID

# XXX get the date when they found the carcass and relate it to the informed roostmates, or maybe restrict to just the first day?

informed_inpa %>% filter(carcID == mycarc) %>% select(dateOnly, local_identifier, informed) %>% ggplot(aes(x = dateOnly, y = local_identifier, color = informed))+geom_point()+theme_minimal()+labs(caption = mycarc)
