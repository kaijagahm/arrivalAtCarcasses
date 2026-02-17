# experimenting with synchronized trajectories
library(tidyverse)
library(sf)
library(targets)
tar_load(trajectories_sync)
tar_load(sync_departures_df)

sync_departures_df <- sync_departures_df %>%
  mutate(date_il = lubridate::ymd(date_il), year = as.character(year))

joined <- left_join(trajectories_sync, sync_departures_df, by = c("year", "date_il", "id1" = "ID1", "id2" = "ID2")) %>% rename("departure_time_diff_min" = "time_diff_min") %>%
  mutate(hour = lubridate::hour(timestamp_il),
         distance_km = distance_m/1000)

daylight_hours <- c(6:18)

joined_daylight <- joined %>% filter(hour %in% daylight_hours) %>%
  mutate(flight_status = case_when(flight1 & flight2 ~ "both",
                                   (flight1 & !flight2)|(!flight1 & flight2) ~ "one",
                                   !flight1 & !flight2 ~ "zero"))

tar_load(ddf)
flight_stats <- joined_daylight %>%
  group_by(year, date_il, id1, id2) %>%
  summarize(n_pts = n(),
            n_with_distance = sum(!is.na(distance_m)),
            prop_with_distance = sum(!is.na(distance_m))/n(),
            n_both_flying = sum(flight_status == "both"),
            prop_both_flying = sum(flight_status == "both")/n(),
            prop_close = sum(distance_km <= ddf)/n(),
            prop_flying_close = sum(flight_status == "both" & (distance_m <= ddf))/sum(flight_status == "both"),
            mean_flight_dist_km = mean(distance_km[flight_status == "both"]))
glimpse(flight_stats)

flight_stats %>%
  ggplot(aes(x = prop_flying_close, y = mean_flight_dist_km))+
  geom_point(alpha = 0.5, pch = 1)+
  theme_classic()+
  labs(x = "Proportion of co-flight time spent close", y = "Mean co-flight distance (km)")+
  facet_wrap(~year)

#(i) the proportion of the flight time that individuals spent close to each other (within detection range); (ii) the mean distance between individuals during the flight;

joined_daylight %>%
  filter(date_il %in% sample(unique(joined_daylight$date_il), 6)) %>%
  ggplot(aes(x = timestamp_il, y = distance_km, group = interaction(id1, id2)))+
  geom_point(alpha = 0.2, aes(color = flight_status))+
  facet_wrap(~date_il, scales = "free_x")+
  theme_classic()
  