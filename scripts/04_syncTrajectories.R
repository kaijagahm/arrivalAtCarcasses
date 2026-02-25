# experimenting with synchronized trajectories
library(tidyverse)
library(sf)
library(targets)
tar_load(trajectories_sync)
tar_load(sync_departures_df)
tar_load(after_departure_interp_only)

adios <- purrr::map(after_departure_interp_only, ~{
  out <- .x %>% group_by(individual_local_identifier, date_il) %>%
    mutate(displacement = c(st_distance(
      !!!syms(attr(., "sf_column")),
      (!!!syms(attr(., "sf_column")))[row_number() == 1]
    )))
  return(out)
})
max_displs <- setNames(purrr::map(adios, ~{
  .x %>% st_drop_geometry() %>% group_by(individual_local_identifier, date_il) %>%
    summarize(max_displacement_m = max(displacement, na.rm = T)) %>%
    mutate(max_displacement_km = max_displacement_m/1000)
}), c("2022", "2023", "2024")) %>% purrr::list_rbind(names_to = "year") %>% mutate(year = factor(year))

sync_departures_df <- sync_departures_df %>%
  mutate(date_il = lubridate::ymd(date_il), year = as.character(year))

joined <- left_join(trajectories_sync, sync_departures_df, by = c("year", "date_il", "id1" = "ID1", "id2" = "ID2")) %>% rename("departure_time_diff_min" = "time_diff_min") %>%
  mutate(hour = lubridate::hour(timestamp_il),
         distance_km = distance_m/1000)

daylight_hours <- c(7:18)

joined_daylight <- joined %>% filter(hour %in% daylight_hours) %>%
  mutate(flight_status = case_when(flight1 & flight2 ~ "both",
                                   (flight1 & !flight2)|(!flight1 & flight2) ~ "one",
                                   !flight1 & !flight2 ~ "zero")) %>%
  left_join(max_displs, by = c("id1" = "individual_local_identifier", "date_il", "year")) %>%
  select(-max_displacement_m) %>%
  rename("id1_max_displ_km" = max_displacement_km) %>%
  left_join(max_displs, by = c("id2" = "individual_local_identifier", "date_il", "year")) %>%
  select(-max_displacement_m) %>%
  rename("id2_max_displ_km" = max_displacement_km) %>%
  mutate(across(contains("max_displ"), as.numeric)) %>%
  mutate(both_over_15km = case_when(id1_max_displ_km > 15 & id2_max_displ_km > 15 ~ T,
                                    .default = F))

tar_load(ddf)
flight_stats <- joined_daylight %>%
  group_by(year, date_il, id1, id2, id1_max_displ_km, id2_max_displ_km, both_over_15km) %>%
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
  filter(both_over_15km) %>%
  ggplot(aes(x = prop_flying_close, y = mean_flight_dist_km))+
  geom_point(alpha = 0.1, pch = 1)+
  theme_classic()+
  labs(x = "Prop. flight time spent <2km apart", y = "Mean distance apart in flight (km)",
       caption = "Co-departing dyads where both individuals displaced >= 15km")+
  facet_wrap(~year)

#(i) the proportion of the flight time that individuals spent close to each other (within detection range); (ii) the mean distance between individuals during the flight;
set.seed(3)
six_dates <- sample(unique(joined_daylight$date_il), 6)
joined_daylight %>%
  filter(flight_status == "both", distance_km < 100, both_over_15km) %>%
  filter(date_il %in% six_dates) %>%
  ggplot(aes(x = timestamp_il, y = distance_km, group = interaction(id1, id2)))+
  geom_line(alpha = 0.2)+
  #geom_point(alpha = 0.2, pch = 1)+
  facet_wrap(~date_il, scales = "free_x")+
  theme_classic()+
  labs(x = "Timestamp",
       y = "Distance apart (km)",
       title = "In-flight separation after sync. departure",
       subtitle = "After sync departure from same roost (<10min)",
       caption = "Each line is a co-departing dyad.\nExcluded distances > 100km for visual clarity.\nOnly timepoints when both individuals were in flight are shown.\nOnly dates/dyads when both indivs flew >= 15km.")
  