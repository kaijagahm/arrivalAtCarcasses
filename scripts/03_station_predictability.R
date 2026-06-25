# Defining predictability of feeding stations
# See "Predictability of food resources affects carcass finding" in Obsidian
# How do we define predictability?
# From Riotte-Lambert & Matthiopoulous 2020 TREE: "the value of an environmental variable (e.g., the abundance of a resource) is increasingly predictable at a given spatiotemporal scale if it is characterised by lower variability or higher correlation with itself or another environmental variable, measured at the given spatiotemporal scale."
library(tidyverse)
library(targets)
library(here)
library(sf)
library(mapview)
library(ggspatial)

# Load in the carcass data
tar_load(all_carcasses) # will need to go back to the original data to see the names of the management regions--apparently they are all supposed to be provisioned approximately equally, but aren't, according to Reznikov et al.
tar_load(bbox_south_big)
carcasses_south <- st_crop(all_carcasses, bbox_south_big)

# Simplify--keeping only dates, not datetimes, because sometimes there are multiple carcasses placed very close to each other in time
carcs <- carcasses_south %>%
  select(carcID, carcType, date, time, datetime, datetime_il, long, lat, stationName, carcassWeight, geometry, X, Y)

carcs_simple <- carcasses_south %>%
  select(carcID, carcType, date, stationName, year) %>%
  distinct()
dim(carcs)
dim(carcs_simple)
table(carcs_simple$carcType) # 60 stn and 112 wild

stn <- carcs_simple %>%
  filter(!is.na(stationName))
dim(stn)

tar_load(minmax_dates)
stn %>%
  filter((date >= minmax_dates[[1]] & date <= minmax_dates[[2]]) | (date >= minmax_dates[[3]] & date <= minmax_dates[[4]]) | (date >= minmax_dates[[5]] & date <= minmax_dates[[6]])) %>%
  ggplot(aes(x = date, y = stationName, color = stationName))+
  geom_point(size = 3, alpha = 0.75)+
  theme_bw()+
  facet_wrap(~year, scales = "free_x")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        text = element_text(size = 18),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12))+
  labs(y = "Feeding station", x = "Date")

# Wild carcasses
wild <- carcs_simple %>%
  filter(carcType == "wild")

mapview(wild, zcol = "year")
yearlist <- purrr::map(group_split(wild, year), sf::st_as_sf)

# Intervals
stn <- stn %>%
  arrange(stationName, date) %>%
  group_by(stationName) %>%
  mutate(interval = as.numeric(difftime(date, lag(date), units = "days")))

stn <- stn %>%
  filter(!is.na(interval))

# Probably what's relevant is only the 6-month period before each carcass.

# A simpler measure--how likely is there to be food there? Considering 6-month period before each carcass, how many of the dates in that period were active at that station?
tar_load(stn_carcs)
carcs_focal <- sf::st_as_sf(purrr::list_rbind(stn_carcs))
carcs_buffered <-  st_buffer(carcs_simple, 4000) # 4km radius

stn_days_last6mos <- rep(NA, nrow(carcs_focal))
# area_days_last6mos <- rep(NA, nrow(carcs_focal))
n_deposited_dates_last6mos <- rep(NA, nrow(carcs_focal))
mean_intervals_last6mos <- rep(NA, nrow(carcs_focal))
mean_log_intervals_last6mos <- rep(NA, nrow(carcs_focal))
sd_intervals_last6mos <- rep(NA, nrow(carcs_focal))
sd_log_intervals_last6mos <- rep(NA, nrow(carcs_focal))
var_intervals_last6mos <- rep(NA, nrow(carcs_focal))
for(i in 1:nrow(carcs_focal)){
  current_date <- carcs_focal$date[i]
  current_stn <- carcs_focal$stationName[i]
  prev_6mos <- all_carcasses %>%
    filter(date >= (current_date-months(6)) & date <= current_date,
           stationName == current_stn)
  # prev_6mos_area <- prev_6mos[st_intersects(prev_6mos, carcs_buffered[i,], sparse = F)[,1],]
  all_dates <- seq.Date(from = current_date%m-%months(6), to = current_date) # to account for uneven month lengths
  deposited_dates <- sort(unique(prev_6mos$date))
  n_deposited_dates <- length(deposited_dates)
  intervals <- as.numeric(lead(deposited_dates)-deposited_dates)
  mean_interval <- mean(intervals, na.rm = T)
  mean_log_interval <- mean(log(intervals), na.rm = T)
  sd_interval <- sd(intervals, na.rm = T)
  var_interval <- var(intervals, na.rm = T)
  sd_log_interval <- sd(log(intervals), na.rm = T)
  # deposited_dates_area <- sort(unique(prev_6mos_area$date))
  # active_dates <- sort(unique(c(prev_6mos$date, prev_6mos$date + days(1), prev_6mos$date + days(2))))
  # area_days_last6mos[i] <- length(deposited_dates_area)/length(all_dates)
  stn_days_last6mos[i] <- length(deposited_dates)/length(all_dates)
  n_deposited_dates_last6mos[i] <- n_deposited_dates
  mean_intervals_last6mos[i] <- mean_interval
  sd_intervals_last6mos[i] <- sd_interval
  var_intervals_last6mos[i] <- var_interval
  mean_log_intervals_last6mos[i] <- mean_log_interval
  sd_log_intervals_last6mos[i] <- sd_log_interval
}

carcs_focal$stn_days_last6mos <- stn_days_last6mos
# carcs_focal$area_days_last6mos <- area_days_last6mos
carcs_focal$n_deposited_dates_last6mos <- n_deposited_dates_last6mos
carcs_focal$mean_interval_last6mos <- mean_intervals_last6mos
carcs_focal$mean_log_interval_last6mos <- mean_log_intervals_last6mos
carcs_focal$sd_interval_last6mos <- sd_intervals_last6mos
carcs_focal$var_interval_last6mos <- var_intervals_last6mos
carcs_focal$sd_log_interval_last6mos <- sd_log_intervals_last6mos

carcs_focal %>%
  ggplot(aes(x = stn_days_last6mos*100*3, fill = stationName))+ # multiplying by 3 if we assume carcasses last for approx. 3 days
  geom_histogram(bins = 15)+
  theme(legend.position = "bottom")+ # proportion of days that have carcasses
  theme_minimal()+
  labs(y = "Carcasses", x = "% days w/carcass @ station, last 6 mos", fill = "Station name")+
  theme(text = element_text(size = 18))

write_rds(carcs_focal, file = "data/created/carcs_focal.RDS")

# Defining predictability by % days with carcass present within a 4km radius
mapview(carcs_buffered, zcol = "carcType") # we can clearly see there are some hotspots/areas of overlap.

carcs_buffered <- carcs_buffered %>%
  mutate(end_date = date+lubridate::days(3)) %>%
  glimpse()

predictability_results <- carcs_buffered %>%
  mutate(
    # 1. Define the 6-month window
    window_start = date - lubridate::days(180),
    window_end = date - lubridate::days(1) # Up to the day before start
  ) %>%
  rowwise() %>%
  mutate(prop_days_covered = {
    # 2. Identify spatial neighbors (including itself, or exclude if needed)
    # We use st_intersects to find any polygon that touches our current geometry
    neighbor_indices <- sf::st_intersects(geometry, carcs_buffered)[[1]]
    neighbors <- carcs_buffered[neighbor_indices, ]
    
    # 3. Filter neighbors to only those that overlap our 6-month window
    # Calculation: Interval [A, B] overlaps [C, D] if A <= D and B >= C
    overlapping_neighbors <- neighbors %>%
      filter(date <= window_end & end_date >= window_start)
    
    if (nrow(overlapping_neighbors) == 0) {
      0
    } else {
      # 4. Calculate unique days covered
      # Clip neighbor dates to the window boundaries
      covered_days <- overlapping_neighbors %>%
        mutate(
          clipped_start = pmax(date, window_start),
          clipped_end = pmin(end_date, window_end)
        ) %>%
        # Generate a sequence of days for every overlapping interval
        mutate(days_seq = map2(clipped_start, clipped_end, ~seq(.x, .y, by = "day"))) %>%
        pull(days_seq) %>%
        flatten() %>%
        unique()
      
      # Calculate proportion
      total_window_days <- as.numeric(window_end - window_start) + 1
      length(covered_days) / total_window_days
    }
  }) %>%
  ungroup()

predictability_results %>%
  mutate(carcType = case_when(carcType == "stn" ~ "SFS",
                              carcType == "wild" ~ "Non-SFS",
                              .default = NA)) %>%
  ggplot(aes(x = factor(year), y = prop_days_covered, fill = carcType))+
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75), alpha = 0.2)+
  geom_point(aes(color = carcType, x = factor(year)), position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.2), pch = 1, alpha = 0.7, size = 3)+
  labs(y = "Predictability",
       x = "Year", fill = "Carcass type", color = "Carcass type",
       caption = "Predictability: % days in last 6mos with at least 1 active carcass within 4km.\nActive carcass: within 3 days of placement/discovery")+
  theme_minimal()+
  scale_fill_manual(values = c("darkorange3", "olivedrab3"))+
  scale_color_manual(values = c("darkorange3", "olivedrab3"))+
  theme(text = element_text(size = 18),
        plot.caption = element_text(size = 14))

predictability_results %>%
  mutate(carcType = case_when(carcType == "stn" ~ "SFS",
                              carcType == "wild" ~ "Non-SFS",
                              .default = NA)) %>%
  ggplot(aes(x = prop_days_covered, fill = carcType, color = carcType))+
  geom_density(alpha = 0.2)+
  labs(x = "Predictability", y = "Density",
       fill = "Carcass type", color = "Carcass type",
       caption = "Predictability: % days in last 6mos with at least 1 active carcass within 4km.\nActive carcass: within 3 days of placement/discovery")+
  theme_minimal()+
  scale_fill_manual(values = c("darkorange3", "olivedrab3"))+
  scale_color_manual(values = c("darkorange3", "olivedrab3"))+
  theme(text = element_text(size = 18),
        plot.caption = element_text(size = 14))

predictability_results %>%
  mutate("Predictability" = prop_days_covered) %>%
  mutate(carcType = case_when(carcType == "stn" ~ "SFS",
                              carcType == "wild" ~ "Non-SFS",
                              .default = NA)) %>%
  ggplot()+
  annotation_map_tile(zoom = 9, type = "cartolight")+
  geom_sf(aes(fill = Predictability, color = Predictability), alpha = 0.4)+
  scale_fill_viridis_c()+
  scale_color_viridis_c()+
  theme_minimal()+
  theme(text = element_text(size = 18))

saveRDS(predictability_results, file = "data/created/predictability_results.RDS")
