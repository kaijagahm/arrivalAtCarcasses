library(tidyverse)
library(mapview)
library(sf)
library(targets)
library(ggplot2)
library(viridis)

tar_load(gps_all_wild) 
length(gps_all_wild) # down to 17 carcasses with southern bounding box filter and medium slope criteria (no feeding bouts with slope > 10% considered).
tar_load(gps_all_inpa) # allowing 3 days before, for direct comparison with wild.
length(gps_all_inpa) #81 carcasses
tar_load(detection_distance_flight)
tar_load(detection_distance_stationary)
tar_load(wild_carcasses_10) # 33 carcasses
mapview(wild_carcasses_10, zcol = "year") # looks somewhat reasonable?--ah, no, there are still a bunch on cliffs.
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
stn <- purrr::list_rbind(gps_all_inpa) %>% sf::st_drop_geometry() %>% mutate(type = "inpa")
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
wild_subset <- st_crop(st_transform(wild_carcasses_5, "WGS84"), wild_test_box) %>% st_transform(32636)

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

plots_wild[[6]] # FIXED!!! these look great


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
  labs(title = "Wild carcasses", y = "Number of vultures", x = "Hours since carcass placement") # only 1 in 2024 because we're focusing on a single geographical subset that we know to be wild

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

# seeing some real differences between INPA and wild carcasses

# Thinking about NBDA
# We're going to need to determine a good starting point for the diffusions. For that matter, we might want to look at the same starting point for the INPA carcasses.

# Here are three cases of wild carcasses. The dashed line tends to fall in the middle, time-wise, of the feeding bouts. What if we added other dashed lines several hours before?
plots_wild[[1]] + geom_vline(aes(xintercept = -1), alpha = 0.5) + geom_vline(aes(xintercept = -5), alpha = 0.5)
plots_wild[[2]] + geom_vline(aes(xintercept = -1), alpha = 0.5) + geom_vline(aes(xintercept = -5), alpha = 0.5)
plots_wild[[6]] + geom_vline(aes(xintercept = -1), alpha = 0.5) + geom_vline(aes(xintercept = -5), alpha = 0.5)

# in some cases, backing up 1 hour works. In other cases, it's stil in the middle of the distribution of vultures. Backing up 5 hours seems to work in all three of these cases.

# Let's see a few examples of inpa carcasses
set.seed(3)
sample(1:length(plots_inpa), 3)
plots_inpa[[5]]
plots_inpa[[12]]
plots_inpa[[39]]
# these are all very different. Sometimes they have a totally different pattern. However, I don't see that backing up 5 hours would necessarily ruin things here either.

# Anyway, we're going to have to make tough decisions in terms of how to compare the two situations. But for now, I just want to test out NBDA on the wild carcasses using the co-roost network. So I actually will just be considering the roost network on the night before.

# Worth considering what to do if there are peaks at the site before the carcass is detected. Sometimes, such as in this one, they're just flying over regularly:
plots_wild[[3]]
plots_wild[[4]] # or this one, where someone landed the previous day but there weren't enough individuals to have critical mass for it to be considered a carcass. This makes me think we should back up at least 24 hours.
plots_wild[[5]] # this is at the beginning of the season--makes me want to back up the GPS data a bit
plots_wild[[7]] # another repeat-flyover situation
plots_wild[[8]]
plots_wild[[9]] # another beginning-of-season thing I think, in 2024

# For the purposes of just the wild ones alone, let's back up 24 hours. Which means I need to add 24 hours onto the beginning of the gps data

