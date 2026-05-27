# experimenting with synchronized trajectories
library(tidyverse)
library(sf)
library(targets)
tar_load(trajectories_sync)
tar_load(sync_departures_df)
tar_load(after_departure_interp_only)

adios <- purrr::map(after_departure_interp_only, ~{
  df <- .x
  df %>%
    group_by(individual_local_identifier, date_il) %>%
    mutate(displacement = {
      pts <- st_geometry(df)[cur_group_rows()]
      as.numeric(st_distance(pts, pts[1]))  # [1] not [[1]] to keep geometry
    }) %>%
    ungroup()
}, .progress = T)

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

# Now we need to get arrivals to the carcass.
tar_load(gps_diffusion)
tar_load(gps_diffusion_wild)
tar_load(ddf)
tar_load(dds)
tar_load(gps_spd)

gd <- map(gps_diffusion, ~select(.x, individual_local_identifier, carcID, timestamp_il, dist_to_carcass, time_since_carcass, daytime_since_carcass, ground_speed))
gdw <- map(gps_diffusion_wild, ~select(.x, individual_local_identifier, carcID, timestamp_il, dist_to_carcass, time_since_carcass, daytime_since_carcass, ground_speed))

glimpse(gdw[[1]])

sightings <- purrr::list_rbind(gd) %>% bind_rows(purrr::list_rbind(gdw)) %>%
  arrange(individual_local_identifier, timestamp_il, carcID) %>%
  filter((ground_speed <= gps_spd & dist_to_carcass <= dds)|(ground_speed > gps_spd & dist_to_carcass <= ddf)) %>%
  rename("id" = individual_local_identifier)

# I think I'm way overthinking this.
# Did the dyad start at the same roost on the same morning? (have from previous part)
# Did they both arrive at the same carcass on the same day? (calculating now)
first_daily_sightings <- sightings %>%
  arrange(timestamp_il) %>%
  group_by("date_il" = lubridate::date(timestamp_il), id, carcID) %>%
  slice(1) %>% # earliest arrival at each carcass on each day
  ungroup() %>%
  arrange(date_il, carcID)

result <- first_daily_sightings %>%
  group_by(carcID, date_il) %>%
  reframe(
    pairs = {
      ids <- id
      times <- timestamp_il
      dates <- date_il
      if (length(ids) < 2) {
        tibble(id1 = character(), id2 = character(), timediff = as.difftime(numeric(0), units = "secs"))
      } else {
        combos <- combn(seq_along(ids), 2)
        tibble(
          id1 = ids[combos[1,]],
          id2 = ids[combos[2,]],
          timediff = abs(difftime(times[combos[2,]], times[combos[1,]], units = "secs"))
        )
      }
    }
  ) %>%
  unnest(pairs) %>%
  mutate(dyad = paste(id1, id2, date_il, sep = "_"))

# Then look at whether their trajectories meet the criteria of following. (calculated above; just need to filter)

# Structure list by carcass ("for each carcass...")
# Get all dyads that left the same roost together and traveled at least 15km that day?
# How many of them stayed within following distance?

result <- result %>%
  arrange(carcID, date_il) %>%
  group_by(carcID, date_il) %>%
  mutate(dyads_carc_date = length(unique(dyad)))

# I'm getting stuck here so I'm going to stop.
# Need to figure out exactly what to measure.
# Possible sticking points: what if they go to multiple carcasses on the same day? Do we only care about the one they visited first after the roost?
  


