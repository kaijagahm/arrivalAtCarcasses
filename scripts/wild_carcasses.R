library(tidyverse)
library(mapview)
library(sf)
library(targets)
library(ggplot2)
library(viridis)

tar_load(gps_all_wild) # down to 37 carcasses with tightened restrictions and southern bounding box filter
tar_load(gps_all_inpa) # allowing 3 days before, for direct comparison with wild. 57 carcasses
tar_load(detection_distance_flight)
tar_load(detection_distance_stationary)
tar_load(wild_carcasses)
mapview(wild_carcasses) # looks somewhat reasonable?--ah, no, there are still a bunch on cliffs.
tar_load(all_carcasses_cropped)

## Timeline
all_carcasses_cropped %>%
  mutate(year = lubridate::year(datetime)) %>%
  filter(year == 2023) %>%
  ggplot(aes(x = X, y = Y, col = datetime, shape = carcType), alpha = 0.75)+
  geom_point(size =4)+
  theme_minimal()+
  scale_color_viridis()+
  scale_shape_manual(values = c(19, 1))+
  ggtitle("Carcass locations, 2023")

all_carcasses_cropped %>%
  mutate(year = lubridate::year(datetime)) %>%
  filter(year == 2024) %>%
  ggplot(aes(x = X, y = Y, col = datetime, shape = carcType), alpha = 0.75)+
  geom_point(size =4)+
  theme_minimal()+
  scale_color_viridis()+
  scale_shape_manual(values = c(19, 1))+
  ggtitle("Carcass locations, 2024")

all_carcasses_cropped %>%
  mutate(year = lubridate::year(datetime),
         month = lubridate::month(datetime),
         day = lubridate::date(datetime)) %>%
  ggplot(aes(x = day, fill = carcType))+
  geom_histogram()+
  facet_wrap(~year, scales = "free_x")+
  scale_fill_manual(name = "Carcass type", 
                    values = c("darkviolet", "gold"))+
  theme_minimal()+
  labs(y = "Carcass count",
       x = "Date")+
  theme(text = element_text(size = 14),
        legend.position = "bottom")

## Choosing a cliff buffer distance
# Currently, I've arbitrarily chosen a 50m buffer for the cliff linestrings. Let's see if this seems reasonable based on the distances.
tar_load(cliffs)
tar_load(bbox_south_new)
tar_load(feeding_bouts_stationary)

nearest <- sf::st_nearest_feature(feeding_bouts_stationary, sf::st_transform(cliffs, 32636))
dist = sf::st_distance(feeding_bouts_stationary, sf::st_transform(cliffs, 32636)[nearest,], by_element=TRUE)
fbs <- feeding_bouts_stationary %>% mutate(dist_to_nearest_cliff = as.numeric(dist))
hist(fbs$dist_to_nearest_cliff) # the reason this is so insanely skewed is that we are including feeding bouts that are not within the southern region at all. Let's crop it
fbs <- st_crop(fbs, bbox_south_new)
hist(fbs$dist_to_nearest_cliff) # this looks better--but we still have some points over in Jordan that are quite far from the cliffs.

fbs %>%
  filter(dist_to_nearest_cliff < 15000) %>%
  ggplot(aes(x = dist_to_nearest_cliff))+
  geom_histogram()+
  labs(x = "Dist to nearest cliff (m)", y = "Frequency")+
  theme_minimal()

# Let's do the same thing but zoom in on points within 2km of a cliff to see if there's a cutoff.
fbs %>%
  filter(dist_to_nearest_cliff < 2000) %>%
  ggplot(aes(x = dist_to_nearest_cliff))+
  geom_histogram(fill = "skyblue4", col = "skyblue2")+
  labs(x = "Distance to nearest cliff (m)", y = "Frequency")+
  theme_minimal() # There definitely seems to be a cutoff around 500m.

toview <- fbs %>%
  mutate(dist_bins = case_when(dist_to_nearest_cliff < 50 ~ "under 50",
                               dist_to_nearest_cliff >= 50 & dist_to_nearest_cliff < 100 ~ "under 100", 
                               dist_to_nearest_cliff >= 100 & dist_to_nearest_cliff < 250 ~ "under 250",
                               dist_to_nearest_cliff >= 250 & dist_to_nearest_cliff < 500 ~ "under 500",
                               dist_to_nearest_cliff >= 500 ~ "x_over 500", .default = NA))
mapview(st_crop(st_transform(cliffs, 32636), bbox_south_new))+mapview(toview, zcol = "dist_bins") # okay, does this look reasonable? What would be a good threshold to choose? Zoom in.
# In vultour, determined that I should be using a DEM or a better way to measure the cliffs, since this shapefile isn't complete enough.
mapview(cliffs_buffered) + mapview(wild_carcasses) # if you set this to topo and zoom in, you can clearly see that a bunch of them are still on cliffs.

# Let's create a test set of wild carcasses that we know are actually wild based on looking at the map
stn <- pustn <- pustn <- purrr::list_rbind(gps_all_inpa) %>% sf::st_drop_geometry() %>% mutate(type = "inpa")
wild <- purrr::list_rbind(gps_all_wild) %>% sf::st_drop_geometry() %>% mutate(type = "wild")

all <- bind_rows(stn, wild) %>%
  mutate(hour_bin = floor_date(timestamp, 
                               unit = "hours"),
                    hour_bin_rel = round(time_since_carcass),
         in_sight = case_when(ground_speed >= 5 & dist_to_carcass <= detection_distance_flight ~ T,
                              ground_speed < 5 & dist_to_carcass <= detection_distance_stationary ~ T,
                              .default = F),
         status = case_when(ground_speed >= 5 & dist_to_carcass <= detection_distance_flight ~ "flight, in sight (<2km)",
                            ground_speed >= 5 & dist_to_carcass > detection_distance_flight ~ "flight, >2km",
                            ground_speed < 5 & dist_to_carcass <= detection_distance_stationary & dist_to_carcass > 200 ~ "stationary, in sight (1km-200m)",
                            ground_speed <= 5 & dist_to_carcass <= 200 ~ "stationary, <200m",
                            ground_speed <= 5 & dist_to_carcass > detection_distance_stationary ~ "stationary, >1km", .default = NA),
         status = factor(status, levels = c("stationary, <200m", "stationary, in sight (1km-200m)", "flight, in sight (<2km)", "flight, >2km", "stationary, >1km")),
         hour = round(time_since_carcass))

color_scale <- c("red", "orange", "skyblue", "gray", "gray50")

carcass_info <- all_carcasses_cropped %>% 
  group_by(carcID) %>%
  summarize(info = paste0("Carcass (", carcType, "), #", carcID, "\n",
                               "Datetime = ", datetime, "\n",
                               "(", round(X, 2), ", ", round(Y, 2), ")") %>%
              st_drop_geometry())

all <- all %>%
  left_join(carcass_info, by = c("carcID"))
all_inpa <- all %>% filter(type == "inpa")
all_wild <- all %>% filter(type == "wild")

wild_test_box <- sf::st_set_crs(sf::st_bbox(c("xmin" = 34.44266,
                                              "ymin" = 30.89326,
                                              "xmax" = 34.94688,
                                              "ymax" = 31.17904)), "WGS84")
wild_subset <- st_crop(st_transform(wild_carcasses, "WGS84"), wild_test_box) %>% st_transform(32636)

all_wild_subset <- all_wild %>% filter(carcID %in% wild_subset$carcID)

cids_inpa <- unique(all_inpa$carcID)
cids_wild_subset <- unique(all_wild_subset$carcID)

plots_inpa <- vector(mode = "list", length = length(cids_inpa))
for(i in 1:length(cids_inpa)){
  df <- all_inpa %>%
    filter(!(status %in% c("flight, >2km", "stationary, >1km"))) %>%
    filter(carcID == cids_inpa[[i]])
  lab <- df$info[1]
  plt <- df %>% group_by(carcID, hour, status) %>%
    summarize(n = length(unique(local_identifier))) %>%
    ggplot(aes(x = hour, fill = status, y = n))+
    geom_vline(aes(xintercept = 0), linetype = 2, alpha = 0.5)+
    geom_col(position = position_stack(reverse = TRUE))+
    labs(y = "# vultures", x = "Hours since carcass", subtitle = lab)+
    theme_minimal()+
    theme(legend.position = "bottom")+
    scale_fill_manual(name = "", values = color_scale, drop = F)
  plots_inpa[[i]] <- plt
}

plots_wild <- vector(mode = "list", length = length(cids_wild_subset))
for(i in 1:length(cids_wild_subset)){
  df <- all_wild_subset %>%
    filter(!(status %in% c("flight, >2km", "stationary, >1km"))) %>%
    filter(carcID == cids_wild_subset[[i]])
  lab <- df$info[1]
  plt <- df %>% group_by(carcID, hour, status) %>%
    summarize(n = length(unique(local_identifier))) %>%
    ggplot(aes(x = hour, fill = status, y = n))+
    geom_vline(aes(xintercept = 0), linetype = 2, alpha = 0.5)+
    geom_col(position = position_stack(reverse = TRUE))+
    labs(y = "# vultures", x = "Hours since carcass", subtitle = lab)+
    theme_minimal()+
    theme(legend.position = "bottom")+
    scale_fill_manual(name = "", values = color_scale, drop = F)
  plots_wild[[i]] <- plt
}

# At least these all look quite similar! And they potentially have very different dynamics than the feeding station carcasses.


forplot <- all %>%
  filter(in_sight) %>%
  mutate(year = lubridate::year(dateOnly),
         carcID = factor(carcID)) %>%
  group_by(carcID, type, hour_bin_rel, year) %>%
  summarize(n_vultures = length(unique(local_identifier))) %>%
  ungroup() 


forplot %>%
  mutate(carcID = factor(carcID, levels = sample(unique(carcID)))) %>%
  filter(type == "inpa") %>%
  ggplot(aes(x = as.numeric(hour_bin_rel), y = n_vultures, group = carcID, col = carcID))+
  geom_vline(aes(xintercept = 0), alpha = 0.5, linetype = 2)+
  geom_line(alpha = 0.3)+
  theme_classic()+
  facet_wrap(~year)+
  theme(legend.position = "none")+
  labs(title = "INPA carcasses", y = "Number of vultures", x = "Hours since carcass placement")

forplot %>%
  filter(carcID %in% cids_wild_subset) %>%
  mutate(carcID = factor(carcID, levels = sample(unique(carcID)))) %>%
  filter(type == "wild") %>%
  ggplot(aes(x = as.numeric(hour_bin_rel), y = n_vultures, group = carcID, col = carcID))+
  geom_vline(aes(xintercept = 0), alpha = 0.5, linetype = 2)+
  geom_line(alpha = 0.3)+
  theme_classic()+
  facet_wrap(~year)+
  theme(legend.position = "none")+
  labs(title = "Wild carcasses", y = "Number of vultures", x = "Hours since carcass placement")

stats <- all %>%
  filter(type == "inpa" | (type == "wild" & carcID %in% cids_wild_subset)) %>%
  filter(time_since_carcass > 0 & time_since_carcass < 24, in_sight) %>%
  mutate(year = lubridate::year(timestamp)) %>%
  group_by(year, carcID, type) %>%
  summarize(in_sight_instances = n(),
            in_sight_indivs = length(unique(local_identifier))) %>%
  ungroup() 

stats %>%
  ggplot(aes(x = type, y = in_sight_instances, fill = type, col = type))+
  geom_violin(alpha = 0.5)+
  geom_jitter(alpha = 0.75, width = 0.03)+
  facet_wrap(~year)+
  scale_fill_manual(values = c("darkviolet", "gold"))+
  scale_color_manual(values = c("darkviolet", "gold"))+
  theme_minimal()+
  labs(y = "Sightings in first 24 hours",
       x = "Carcass type")+
  theme(text = element_text(size = 14))

stats %>%
  ggplot(aes(x = type, y = in_sight_indivs, fill = type, col = type))+
  geom_violin(alpha = 0.5)+
  geom_jitter(alpha = 0.75, width = 0.03)+
  facet_wrap(~year)+
  scale_fill_manual(values = c("darkviolet", "gold"))+
  scale_color_manual(values = c("darkviolet", "gold"))+
  theme_minimal()+
  labs(y = "Individuals within sight in first 24 hours",
       x = "Carcass type")+
  theme(text = element_text(size = 14))
