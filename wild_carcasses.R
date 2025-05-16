library(tidyverse)
library(mapview)
library(sf)
library(targets)
library(ggplot2)

tar_load(gps_all_wild) # down to 98 "carcasses" with the tightened restrictions
tar_load(gps_all_inpa) # allowing 3 days before, for direct comparison with wild
tar_load(detection_distance_flight)
tar_load(detection_distance_stationary)
tar_load(wild_carcasses)
mapview(wild_carcasses) # looks somewhat reasonable?

stn <- purrr::list_rbind(gps_all_inpa) %>% sf::st_drop_geometry() %>% mutate(type = "inpa")
wild <- purrr::list_rbind(gps_all_wild) %>% sf::st_drop_geometry() %>% mutate(type = "wild")

all <- bind_rows(stn, wild) %>%
  mutate(hour_bin = floor_date(timestamp, 
                               unit = "hours"),
                    hour_bin_rel = round(time_since_carcass),
         in_sight = case_when(ground_speed >= 5 & dist_to_carcass <= detection_distance_flight ~ T,
                              ground_speed < 5 & dist_to_carcass <= detection_distance_stationary ~ T,
                              .default = F))

forplot <- all %>%
  filter(in_sight) %>%
  mutate(year = lubridate::year(dateOnly),
         carcID = factor(carcID)) %>%
  group_by(carcID, type, hour_bin_rel, year) %>%
  summarize(n_vultures = length(unique(local_identifier))) %>%
  ungroup() 


forplot %>%
  filter(type == "inpa") %>%
  ggplot(aes(x = as.numeric(hour_bin_rel), y = n_vultures, group = carcID, col = carcID))+
  geom_vline(aes(xintercept = 0), alpha = 0.5, linetype = 2)+
  geom_line(alpha = 0.2)+
  theme_classic()+
  facet_wrap(~year)+
  theme(legend.position = "none")

forplot %>%
  filter(type == "wild") %>%
  ggplot(aes(x = as.numeric(hour_bin_rel), y = n_vultures, group = carcID, col = carcID))+
  geom_vline(aes(xintercept = 0), alpha = 0.5, linetype = 2)+
  geom_line(alpha = 0.2)+
  theme_classic()+
  facet_wrap(~year)+
  theme(legend.position = "none") # looks marginally better than it did before, but still need to look at bouts.

# What about feeding bouts at these sites?
tar_load(all_bouts_assigned)

# By definition, these bouts are only going to be "assigned" to a carcass if they're within a certain time window of it, so we're going to need to include the ones that are nearby.
tar_load(all_carcasses)
# This is getting circular...
