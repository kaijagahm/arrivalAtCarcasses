# Stats and numbers for manuscript
library(tidyverse)
library(targets)
library(sf)

# How many feeding bouts per sampling period
tar_load(non_station_bo_prepped)
non_station_bo_prepped %>% st_drop_geometry() %>% group_by(year) %>% summarize(n = n())

# How many feeding bouts per individual vulture per sampling period?
non_station_bo_prepped %>% st_drop_geometry() %>% group_by(year, individual_id) %>% summarize(n = n()) %>% group_by(year) %>% summarize(min = min(n), max = max(n), mean = mean(n), median = median(n), sd = sd(n))

# How many wild carcasses per sampling period?
tar_load(wild)
table(wild$year)

# How many bouts to define a wild carcass, on average?
tar_load(wild_carcasses_validated)
wild_carcasses_validated %>%
  group_by(year) %>%
  summarize(mnbouts = mean(nBouts))
