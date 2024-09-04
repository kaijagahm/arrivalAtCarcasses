# Get 2024 feeding bouts and localize them on the map
library(tidyverse)
library(sf)

data <- as.data.frame(data.table::fread(here("data/ACC/2024_hf_period/created/devices/202362.csv")))
pred <- as.data.frame(data.table::fread(here("data/ACC/2024_hf_period/created/predictions/202362.csv")))
pred <- distinct(pred)
nrow(pred)

data %>% filter(datatype == "SEN_ACC_20Hz_START") %>% nrow()
max(pred$bout_id)

data <- add_bout_ids(data)
length(unique(data$bout_id))
length(unique(pred$bout_id))

data %>% filter(datatype == "GPS") %>% filter(is.na(bout_id)) # none are left without a bout id
data %>% filter(datatype == "GPS") %>% filter(bout_id == 0) # just the first row
data$bout_id[data$datatype == "GPS" & data$bout_id == 0] <- 1

# How many GPS points do we have per bout?
data %>%
  group_by(bout_id) %>%
  summarize(n = sum(datatype == "GPS")) %>%
  arrange(desc(n))

# In the draft of the ACC manuscript, they say "We matched GPS positions to accelerometer data if they were recorded within 5 minutes of each other." So that implies that it's not actually simple to do this... But I seem to have done it *fairly* simply just by including this data.

# Save just the gps bout locations
bout_locations <- data %>%
  filter(datatype == "GPS") %>%
  select(bout_id, device_id, Latitude, Longitude, UTC_datetime)

