library(tidyverse)
library(mapview)
library(sf)
library(targets)
library(ggplot2)
library(viridis)

tar_load(gps_all_wild) 
length(gps_all_wild) # down to 33 carcasses with southern bounding box filter and medium slope criteria (no feeding bouts with slope > 10% considered).
tar_load(gps_all_stn) # allowing 3 days before, for direct comparison with wild.
length(gps_all_stn) #81 carcasses
tar_load(detection_distance_flight)
tar_load(detection_distance_stationary)
tar_load(wild_carcasses)
mapview(wild_carcasses, zcol = "year") 
tar_load(all_carcasses_cropped)

## Timeline
all_carcasses_cropped %>%
  mutate(year = lubridate::year(datetime)) %>%
  #filter(year == 2023) %>%
  ggplot(aes(x = X, y = Y, col = factor(year), shape = carcType), alpha = 0.75)+
  geom_point(size =4)+
  theme_minimal()+
  scale_color_viridis_d()+
  scale_shape_manual(values = c(19, 1))+
  labs(title = "Carcass locations",
       subtitle = "2022-2024 high-frequency periods",
       y = "UTM Northing",
       x = "UTM Easting",
       color = "Year")

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
        legend.position = "bottom",
        panel.grid = element_blank())

# Let's create a test set of wild carcasses that we know are actually wild based on looking at the map
stn <- purrr::list_rbind(gps_all_stn) %>% sf::st_drop_geometry() %>% mutate(type = "stn")
wild <- purrr::list_rbind(gps_all_wild) %>% sf::st_drop_geometry() %>% mutate(type = "wild")

all_gps_data <- bind_rows(stn, wild) %>%
  mutate(ground_speed = as.numeric(ground_speed)) %>%
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

all_gps_data <- all_gps_data %>%
  left_join(carcass_info, by = c("carcID"))
all_gps_data_stn <- all_gps_data %>% filter(type == "stn")
all_gps_data_wild <- all_gps_data %>% filter(type == "wild")

# Making a box that we know should contain valid wild carcasses, based on convos with Orr
wild_test_box <- sf::st_set_crs(sf::st_bbox(c("xmin" = 34.44266,
                                              "ymin" = 30.89326,
                                              "xmax" = 34.94688,
                                              "ymax" = 31.17904)), "WGS84")
mapview(wild_test_box)+mapview(wild_carcasses)
valid_wild_carcasses <- st_crop(st_transform(wild_carcasses, "WGS84"), wild_test_box) %>% st_transform(32636)

all_wild_valid <- all_gps_data_wild %>% filter(carcID %in% valid_wild_carcasses$carcID)

cids_stn <- unique(all_gps_data_stn$carcID)
length(cids_stn) # all of them
cids_wild_valid <- unique(all_wild_valid$carcID)
length(cids_wild_valid) # 16 wild ones that we know/suspect to be valid

plots_stn <- vector(mode = "list", length = length(cids_stn))
for(i in 1:length(cids_stn)){
  df <- all_gps_data_stn %>%
    filter(!(status %in% c("flight, >2km", "stationary, >1km"))) %>%
    filter(carcID == cids_stn[[i]])
  lab <- df$info[1]
  plt <- df %>% group_by(carcID, hour, status) %>%
    summarize(n = length(unique(tag_local_identifier)), .groups = "drop") %>%
    ggplot(aes(x = as.numeric(hour), fill = status, y = n))+
    geom_vline(aes(xintercept = 0), linetype = 2, alpha = 0.5)+
    geom_col(position = position_stack(reverse = TRUE))+
    labs(y = "# vultures", x = "Hours since carcass", subtitle = lab)+
    theme_minimal()+
    theme(legend.position = "bottom")+
    scale_fill_manual(name = "", values = color_scale, drop = F)
  plots_stn[[i]] <- plt
}
names(plots_stn) <- cids_stn
write_rds(plots_stn, file = here("data/plots_stn.RDS"))

plots_wild_valid <- vector(mode = "list", length = length(cids_wild_valid))
for(i in 1:length(cids_wild_valid)){
  df <- all_wild_valid %>%
    filter(!(status %in% c("flight, >2km", "stationary, >1km"))) %>%
    filter(carcID == cids_wild_valid[[i]])
  lab <- df$info[1]
  plt <- df %>% group_by(carcID, hour, status) %>%
    summarize(n = length(unique(tag_local_identifier)), .groups = "drop") %>%
    ggplot(aes(x = as.numeric(hour), fill = status, y = n))+
    geom_vline(aes(xintercept = 0), linetype = 2, alpha = 0.5)+
    geom_col(position = position_stack(reverse = TRUE))+
    labs(y = "# vultures", x = "Hours since carcass", subtitle = lab)+
    theme_minimal()+
    theme(legend.position = "bottom")+
    scale_fill_manual(name = "", values = color_scale, drop = F)
  plots_wild_valid[[i]] <- plt
}

names(plots_wild_valid) <- cids_wild_valid
write_rds(plots_wild_valid, file = here("data/plots_wild_valid.RDS"))

stats <- all_gps_data %>%
  arrange(timestamp) %>%
  filter(type == "stn" | (type == "wild" & carcID %in% cids_wild_valid)) %>%
  filter(time_since_carcass > 0 & time_since_carcass < 24, in_sight) %>%
  mutate(year = lubridate::year(timestamp)) %>%
  group_by(year, carcID, type, tag_local_identifier) %>%
  slice(1) %>% # get everyone's first sighting
  ungroup() %>%
  group_by(year, carcID, type) %>%
  summarize(indivs_sighting = length(unique(tag_local_identifier))) %>%
  ungroup() 

stats %>%
  ggplot(aes(x = type, y = indivs_sighting, fill = type, col = type))+
  geom_violin(alpha = 0.5)+
  geom_jitter(alpha = 0.75, width = 0.03)+
  facet_wrap(~year)+
  scale_fill_manual(values = c("darkviolet", "gold"))+
  scale_color_manual(values = c("darkviolet", "gold"))+
  theme_minimal()+
  labs(y = "Vultures sighting carcass in first 24 hours",
       x = "Carcass type",
       caption = "Including only the 14 wild carcasses in the \nknown/suspected valid bounding box")+
  theme(text = element_text(size = 14))

# Will need to determine a good NBDA starting point for the wild carcasses. Do we want to back up?
walk(plots_wild_valid, print) # in most of these cases, the "carcass time" is right in the middle of a bout of arrivals, so really we would want to back it up by at least a few hours.

# in some cases, backing up 1 hour works. In other cases, it's stil in the middle of the distribution of vultures. Backing up 5 hours seems to work in all three of these cases.

# Let's see a few examples of stn carcasses
set.seed(3)
sample(1:length(plots_stn), 3)
plots_stn[[5]]
plots_stn[[12]]
plots_stn[[39]]
# these are all very different. Sometimes they have a totally different pattern. However, I don't see that backing up 5 hours would necessarily ruin things here either.

# Anyway, we're going to have to make tough decisions in terms of how to compare the two situations. But for now, I just want to test out NBDA on the wild carcasses using the co-roost network. So I actually will just be considering the roost network on the night before.

# Worth considering what to do if there are peaks at the site before the carcass is detected
plots_wild_valid[[1]] # a lot of peaks here just as a matter of course--does this indicate that the carcass was there, or just that this is a common place to fly over and we need to do a comparison with a non-carcass area?

# For the purposes of just the wild ones alone, let's back up 24 hours. Which means I need to add 24 hours onto the beginning of the gps data [edit 7/20: added 30 days to the beginning of each year's data for the sake of being able to get networks for NBDA, so that's all well and good.]