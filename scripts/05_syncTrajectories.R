# experimenting with synchronized trajectories
library(tidyverse)
library(sf)
library(targets)
tar_load(sync_departures_df)
tar_load(arrival_dyads)

# Compare departure and arrival dyads -------------------------------------
departures <- sync_departures_df %>% rename("depart_time_diff_min" = "time_diff_min")
arrivals_simple <- arrival_dyads %>%
  select(date_il, carcID, id1, id2, "arrive_time_diff_hrs" = daytime_since_carcass_diff, "arrive_dist_apart_m" = dist_apart) %>%
  mutate(date_il = lubridate::date(date_il))

# 3. What proportion of departure pairs go to the same carcass?
tar_load(minmax_dates)
depart_lookahead <- departures %>%
  filter((date_il >= minmax_dates[[1]] & date_il <= minmax_dates[[2]])|(date_il >= minmax_dates[[3]] & date_il <= minmax_dates[[4]])|(date_il >= minmax_dates[[5]] & date_il <= minmax_dates[[6]])) %>%
  left_join(mutate(arrivals_simple, date_il = as.character(date_il)), by = c("date_il", "ID1" = "id1", "ID2" = "id2"))

depart_lookahead %>%
  group_by(date_il, ID1, ID2, depart_time_diff_min, year, roostID) %>%
  summarize(n_carcs = length(unique(carcID[!is.na(carcID)]))) %>%
  ungroup() %>%
  group_by(date_il) %>%
  summarize(prop_any_same = sum(n_carcs > 0)/n(),
            prop_1_same = sum(n_carcs == 1)/n(),
            prop_2_same = sum(n_carcs == 2)/n(),
            prop_3_same = sum(n_carcs == 3)/n(),
            prop_4_same = sum(n_carcs == 4)/n(),
            prop_5up_same = sum(n_carcs >= 5)/n())

depart_lookahead %>%
  group_by(date_il, ID1, ID2, depart_time_diff_min, year, roostID) %>%
  summarize(n_carcs = length(unique(carcID[!is.na(carcID)]))) %>%
  ungroup() %>%
  ggplot(aes(x = factor(date_il), fill = factor(n_carcs)))+
  geom_bar(stat = "count", position = position_stack(reverse = T))+
  facet_wrap(~year, scales = "free_x")+
  theme_minimal()+
  theme(axis.text.x = element_blank(), legend.position = "bottom")+
  labs(y = "# co-departing dyads",
       x = "Date",
       fill = "Shared carcs")+
  scale_fill_viridis_d()

depart_lookahead %>%
  group_by(date_il, ID1, ID2, depart_time_diff_min, year, roostID) %>%
  summarize(n_carcs = length(unique(carcID[!is.na(carcID)]))) %>%
  ungroup() %>%
  mutate(shared_carc = case_when(n_carcs > 0 ~ T, .default = F)) %>%
  ggplot(aes(x = factor(date_il), fill = factor(shared_carc)))+
  geom_bar(stat = "count", position = position_stack(reverse = T))+
  facet_wrap(~year, scales = "free_x")+
  theme_minimal()+
  theme(axis.text.x = element_blank(), legend.position = "bottom")+
  labs(y = "# co-departing dyads",
       x = "Date",
       fill = "Went to same carc?")+
  scale_fill_viridis_d()

# 4. What proportion of arrival pairs left the roost together?
arrive_lookback <- arrivals_simple %>%
  left_join(departures, by = c("date_il", "id1" = "ID1", "id2" = "ID2")) %>%
  mutate(year = lubridate::year(date_il),
         same_roost = !is.na(roostID),
         departed_together = !is.na(roostID) & depart_time_diff_min <= 10,
         carcType = case_when(nchar(carcID) == 7 ~ "stn",
                              .default = "wild")) %>%
  arrange(carcID, date_il) %>%
  group_by(carcID) %>%
  mutate(day = match(date_il, unique(date_il)))

arrive_lookback %>%
  filter(day <= 3) %>% 
  ggplot(aes(x = factor(date_il), fill = factor(departed_together)))+
  geom_bar(stat = "count", position = position_stack(reverse = T))+
  facet_wrap(~year, scales = "free_x")+
  theme_minimal()+
  theme(axis.text.x = element_blank(), legend.position = "bottom")+
  labs(y = "# co-arriving dyads",
       x = "Date",
       fill = "Left roost together?")+
  scale_fill_viridis_d()

coroosting_dyads_per_carcass <- arrive_lookback %>%
  mutate(year = lubridate::year(date_il)) %>%
  mutate(same_roost = !is.na(roostID),
         departed_together = !is.na(roostID) & depart_time_diff_min <= 10) %>%
  group_by(year, carcID, carcType, date_il, day) %>%
  summarize(prop_departed_together = mean(departed_together))

coroosting_dyads_per_carcass %>%
  #filter(day <= 3) %>%
  ggplot(aes(x = day, y = prop_departed_together, color = carcType))+
  geom_point(alpha = 0.7, pch = 1, size = 2)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  facet_wrap(~year, scales = "free_x")+
  labs(y = "Prop. co-departing roost",
       x = "Day")# no trend in proportion of carcass-arrival dyads that departed the same roost together

# CAVEATS
# This only includes known roost polygons. Need to find a way to extend this analysis to all roost sites, not just named polygons.
# I'm not sure yet whether I'm only considering dyads whose max displacement was > 15km. Need to go through this and get it into much better shape before presenting it.
# Also need to look at the informed status of the co-arriving dyads.
# Should also have a thing for co-roosting dyads that did not co-depart but did come from the same roost. Different category I guess? (This seems like a lower priority, since we're just trying to quantify actual following from the roost.)

arrive_lookback %>%
  filter(day <= 3) %>%
  ggplot(aes(x = departed_together, y = arrive_time_diff_hrs))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(alpha = 0.1, width = 0.2, pch = 1, size = 0.5)+
  facet_wrap(~year)+
  theme_minimal()+
  labs(y = "Diff. in arrival times (hours)",
       x = "Departed roost together?") # In general, we see that dyads that co-departed from the roost tended to arrive at a given carcass closer in time to each other than dyads that did not co-depart from the roost. Good! This is a gut check.

# What about on a per-carcass basis--do we still see the same pattern?
arrive_lookback %>%
  ggplot(aes(x = factor(carcID), y = arrive_time_diff_hrs, fill = departed_together))+
  geom_boxplot(outlier.shape = NA)+
  facet_wrap(~year*carcType, nrow = 3, ncol = 2, scales = "free_x")+
  theme_minimal() # slight pattern: for station carcasses, dyads that departed together tended to arrive at the carcass closer in time to each other than dyads that didn't depart together. This isn't true as much for the wild caracsses.

# Let's take a couple example carcasses
set.seed(3)
example_carcIDs <- arrive_lookback %>%
  select(carcID, carcType) %>%
  group_by(carcType) %>%
  slice_sample(n = 3) %>%
  pull(carcID)
example_carcIDs 

arrive_lookback %>%
  filter(carcID %in% example_carcIDs, day <= 3) %>%
  ggplot(aes(x = factor(day), y = arrive_time_diff_hrs, fill = departed_together))+
  geom_boxplot()+
  theme_minimal()+
  facet_wrap(~carcID)+
  theme_minimal() # Interesting! I don't see any obvious patterns in the arrival times of co-departing vs. non-co-departing dyads. Some carcasses show clear differences, increasing over time (like 4448034); others show clear differences, decreasing over time (like 4407966). Others are very samey, like 140.

# The difference between station and wild does suggest that there might be some kind of signal in here in terms of frequency of following events to different carcasses of different types/predictabilities! Maybe we can even make some predictions of how this will differ over the course of the three-day span.

# Still need to do a bunch of work on this though, including figuring out whether dyads are informed or not.


# Displacements (for 15km limit) ------------------------------------------

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

# Get flight stats for co-departing dyads
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

flight_stats %>% # Examine distance apart
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


  
  