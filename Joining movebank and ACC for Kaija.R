
#Load
library(ggplot2)
library(ggmap)
library(move)
library(tidyverse)
library(plyr)
##Read data

#### Files required: #####

## full.movestack is a movestack file converted to df, with a "study" column ("TAU" or INPA")

## merged is the classification output file (Filtered for Feeding only)


#fix timestamp
full.movestack$timestamp<- as.POSIXct(full.movestack$timestamp, tz = "UTC", format = "%Y-%m-%d %H:%M:%s") 

# keep only relevant 
full.movestack <- full.movestack %>% 
  dplyr::select(timestamp, tag_local_identifier, location_long,location_lat, ground_speed)

full.movestack$sensor <- "GPS"
merged$sensor <- "ACC" ###



names(merged)
colnames(merged)[c(1,4)] <- c("timestamp","tag_local_identifier") #make sure the colnames match time and device_id respectively

#attach/detach plyr, combine predictions with movestack

full.move.2 <- rbind.fill(full.movestack, merged)
detach("package:plyr", unload=TRUE)



#check if last chunk worked
nrow(full.move.2) == nrow(full.movestack) + nrow(merged)

####prepare data for gps crossref#####
full.move.2 <- full.move.2 %>%
  group_by(tag_local_identifier) %>%
  arrange(timestamp) %>%
  mutate(time_diff = as.numeric(difftime(lead(timestamp), timestamp, units = "secs"))) %>%
  ungroup()

#####attach GPS point to ACC #####

#16.4.23 - this should really be a function.
full.move.2 <- full.move.2 %>% 
  group_by(tag_local_identifier) %>%
  arrange(timestamp) %>%
  mutate(
   location_long_2 = case_when(is.na(location_long) & lag(time_diff) <= 300 & lag(ground_speed <= 4) ~ lag(location_long),
                              is.na(location_long) & time_diff < 300 & lead(ground_speed <= 4) ~ lead(location_long),
                              is.na(location_long) & lag(time_diff) <= 700 & lag(ground_speed <= 4) ~ lag(location_long),
                              is.na(location_long) & time_diff < 700 & lead(ground_speed <= 4) ~ lead(location_long),TRUE ~  location_long),
   location_lat_2 = case_when(is.na(location_lat) & lag(time_diff) <= 300 & lag(ground_speed <= 4) ~ lag(location_lat),
                              is.na(location_lat) & time_diff < 300 & lead(ground_speed <= 4) ~ lead(location_lat),
                              is.na(location_lat) & lag(time_diff) <= 700 & lag(ground_speed <= 4) ~ lag(location_lat),
                              is.na(location_lat) & time_diff < 700 & lead(ground_speed <= 4) ~ lead(location_lat),TRUE ~  location_lat),
   ground_speed_2 = case_when(is.na(location_long) & lag(time_diff) <= 300 & lag(ground_speed <= 4) ~ lag(ground_speed),
                              is.na(location_long) & time_diff < 300 & lead(ground_speed <= 4) ~ lead(ground_speed),
                              is.na(location_long) & lag(time_diff) <= 700 & lag(ground_speed <= 4) ~ lag(ground_speed),
                              is.na(location_long) & time_diff < 700 & lead(ground_speed <= 4) ~ lead(ground_speed),TRUE ~  ground_speed)
  )%>% 
  mutate(
    location_long_3 = case_when(is.na(location_long_2) & lag(time_diff) <= 300 ~ lag(location_long),
                                     is.na(location_long_2) & time_diff < 300 ~ lead(location_long), TRUE ~  location_long_2),
    location_lat_3 = case_when(is.na(location_lat_2) & lag(time_diff) <= 300 ~ lag(location_lat),
                                    is.na(location_lat_2) & time_diff < 300 ~ lead(location_lat), TRUE ~  location_lat_2),
    ground_speed_3 = case_when(is.na(location_long_2) & lag(time_diff) <= 300 ~ lag(ground_speed),
                                is.na(location_long_2) & time_diff < 300 ~ lead(ground_speed), TRUE ~  ground_speed_2)
    ) %>%
  ungroup()


# the _2 _1 after the coords were initially kept to compare how many points were caught/missed between different stages, no real reason to keep it that way