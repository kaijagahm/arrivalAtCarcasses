# Defining predictability of feeding stations
# See "Predictability of food resources affects carcass finding" in Obsidian
# How do we define predictability?
# From Riotte-Lambert & Matthiopoulous 2020 TREE: "the value of an environmental variable (e.g., the abundance of a resource) is increasingly predictable at a given spatiotemporal scale if it is characterised by lower variability or higher correlation with itself or another environmental variable, measured at the given spatiotemporal scale."
library(tidyverse)
library(targets)
library(here)
library(sf)
library(mapview)

# Load in the carcass data
tar_load(all_carcasses) # will need to go back to the original data to see the names of the management regions--apparently they are all supposed to be provisioned approximately equally, but aren't, according to Reznikov et al.
tar_load(bbox_south_big)
carcasses_south <- st_crop(all_carcasses, bbox_south_big)

# Simplify--keeping only dates, not datetimes, because sometimes there are multiple carcasses placed very close to each other in time
carcs <- carcasses_south %>%
  select(carcID, carcType, date, time, datetime, datetime_il, long, lat, stationName, carcassWeight, geometry, X, Y)

carcs_simple <- carcasses_south %>%
  select(carcType, date, stationName, year) %>%
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
carcs_buffered <-  st_buffer(carcs_focal, 4000)

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