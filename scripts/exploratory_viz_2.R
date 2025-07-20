# Exploratory visualizations--approaches to carcass
# Altitudes as vultures descend to the carcass
library(sf)
library(tidyverse)

# Let's look at the trajectories of vultures that approach one known carcass
tar_load(gps_all_inpa)
tar_load(inpa_carcs)
tar_load(bbox_south_big)
tar_load(roostPolygons, store = "~/Desktop/projects/MvmtSoc/_targets/")
rp <- sf::st_read(roostPolygons) %>% sf::st_transform(32636)
rp_cropped <- sf::st_crop(rp, bbox_south_big)
focal <- gps_all_inpa[[36]] %>% filter(time_since_carcass > -24 & time_since_carcass < 48)
focal %>% count(local_identifier) %>% arrange(desc(n)) # let's pick E54w, which has a lot of points
set.seed(5)
indiv <- sample(unique(focal$local_identifier), 1)
focal_indiv <- focal %>% filter(local_identifier == indiv) %>% sf::st_as_sf(crs = "32636")
carc <- inpa_carcs[[36]]
id <- carc$carcID

focal_line <- focal_indiv %>%
  arrange(time_since_carcass) %>%
  summarise(do_union = FALSE) %>%
  summarise(geometry = st_combine(geometry)) %>%
  mutate(geometry = st_cast(geometry, "LINESTRING")) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = focal_line, color = "black", alpha = 0.2) +
  geom_sf(data = focal_indiv, aes(col = height_above_msl), alpha = 0.7) +
  geom_sf(data = rp_cropped) +
  geom_sf(data = carc, col = "red") +
  scale_color_viridis_c()+
  theme_minimal()+
  labs(title = "Track of T90b",
       subtitle = "-24hr through 48h",
       color = "Altitude (m)")

mapview(focal_line)+mapview(focal_indiv, zcol = "height_above_msl")+mapview(rp_cropped, col.regions = "gray")+mapview(carc, col.regions = "red")

focal_indiv %>%
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, col = height_above_msl))+
  geom_point(alpha = 0.7)+
  geom_line()+
  theme_minimal()+
  scale_color_viridis_c()+
  labs(y = "Distance to carcass (km)", x = "Time since carcass (hours)", col = "Height (m)", title = indiv)

fig <- plot_ly(focal_indiv, x = ~location_long, y = ~location_lat, z = ~height_above_msl, type = 'scatter3d', mode = 'lines',
               opacity = 1, line = list(width = 6)) 

fig # can make this better but at least it's possible to make a 3d plot.

# The reason I wanted to see things in 3D in the first place was to determine whether we can identify when an individual is approaching a carcass, sort of like cassidy's turning points.
# let's get each instance of landing near a carcass and then walk the GPS back 10 hours and look at the track.
# each instance of landing near a carcass--need to go to the code with the histograms and get the data for all the INPA carcasses. bind into one df, group by individual, sort by time, identify the first instance of being on the ground nearby, and then grab the previous 10 hours.
tar_load(gps_all_inpa)
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
    run_id = rleid(neg),
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

# Step 3: For each landing, get points in same run_id that occurred before or at the landing
approach_list <- map(1:nrow(landings), function(i) {
  landing <- as.data.frame(landings[i, , drop = FALSE])  # Keep as 1-row tibble
  cid <- landing$carcID[1]
  li <- landing$local_identifier[1]
  ri <- landing$run_id[1]
  ts <- landing$timestamp[1]
  
  stn_processed %>%
    dplyr::filter(
      carcID == cid,
      local_identifier == li,
      run_id == ri,
      timestamp <= ts
    ) %>%
    mutate(approach_id = i)
}, .progress = T) # this is really slow!!!

approach_points <- list_rbind(approach_list) %>%
  group_by(carcID, local_identifier, approach_id) %>%
  filter(n() > 1) %>% # only keeping "approaches" that include the landing point plus at least one more point
  ungroup() %>%
  group_by(carcID, local_identifier) %>%
  mutate(first_approach = case_when(approach_id == min(approach_id) ~ T,
                                    .default = F))
dim(approach_points)

mycarc <- 4436953
mycarc2 <- 4890051
mycarc_info <- inpa_carcs[map_lgl(inpa_carcs, ~.x$carcID ==mycarc)][[1]]
mycarc2_info <- inpa_carcs[map_lgl(inpa_carcs, ~.x$carcID ==mycarc2)][[1]]

approach_points_mycarc <- approach_points %>%
  filter(carcID == mycarc)
approach_points_mycarc2 <- approach_points %>%
  filter(carcID == mycarc2)

set.seed(3)
indivs <- sample(unique(approach_points_mycarc$local_identifier), 6)
indivs2 <- sample(unique(approach_points_mycarc2$local_identifier), 6)

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

approach_points_mycarc %>% 
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, col = first_approach, group = interaction(local_identifier, approach_id)))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "none")+
  labs(y = "Distance to carcass (km)",
       x = "Hours since carcass",
       title = "All approaches to the carcass",
       subtitle = "Colored by individual",
       caption = mycarc)

approach_points_mycarc2 %>% 
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, col = first_approach, group = interaction(local_identifier, approach_id)))+
  geom_line()+
  theme_classic()+
  theme(legend.position = "none")+
  labs(y = "Distance to carcass (km)",
       x = "Hours since carcass",
       title = "All approaches to the carcass",
       subtitle = "Colored by individual",
       caption = mycarc2)

# Okay what about looking at the max distance of each approach
breaks <- c(-24, 0, 24, 48, 72, 96)
approaches_stats <- approach_points_mycarc %>%
  filter(as.numeric(hour) <=72) %>%
  mutate(day = cut(as.numeric(hour), breaks)) %>%
  group_by(carcID, local_identifier, day, first_approach) %>%
  summarize(max_dist_km = max(dist_to_carcass/1000),
            approach_start = min(time_since_carcass))

approaches_stats2 <- approach_points_mycarc2 %>%
  filter(as.numeric(hour) <=72) %>%
  mutate(day = cut(as.numeric(hour), breaks)) %>%
  group_by(carcID, local_identifier, day, first_approach) %>%
  summarize(max_dist_km = max(dist_to_carcass/1000),
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

# X-Y coordinates of first_approaches
approach_points_mycarc <- approach_points_mycarc %>%
  ungroup() %>%
  st_as_sf() %>%
  bind_cols(st_coordinates(.))

approach_points_mycarc2 <- approach_points_mycarc2 %>%
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
