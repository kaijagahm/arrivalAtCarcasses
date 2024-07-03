library(here)
library(tidyverse)
library(readxl)
library(sf)
library(mapview)
tar_load(hires_tags)
carcasses <- read_excel(here("data/FeedingData from 2018_2024_Translated.xlsx"))
carcasses_simple <- carcasses %>% select(ID, `Date Event`, `Event time`, `WGS84 - LONG`, `WGS84 - LAT`)
names(carcasses_simple) <- c("ID", "date", "time", "long", "lat")
carcasses_simple <- carcasses_simple %>%
  mutate(across(c("date", "time"), as.character)) %>%
  mutate(datetime = paste0(substr(date, 1, 10), " ", substr(time, 12, 19)),
         datetime = lubridate::ymd_hms(datetime)) %>%
  mutate(time = substr(time, 12, 19))
carcasses_sf <- sf::st_as_sf(carcasses_simple, coords = c("long", "lat"), crs = "WGS84", remove = F)
mapview(carcasses_sf %>% filter(datetime > lubridate::ymd("2024-01-01")), zcol = "date", legend = F)
save(carcasses_sf, file = here("data/carcasses_sf.Rda"))

# Grab just the carcasses that fell during the period of hi-res tagging (the first one, not this year's yet)
mindate <- lubridate::date(min(hires_tags$timestamp))
maxdate <- lubridate::date(max(hires_tags$timestamp))
hires_carcasses <- carcasses_sf %>%
  filter(datetime >= mindate & datetime <= maxdate)
dim(hires_carcasses) # 70 carcasses were placed during this time. That's going to be our maximum sample size.

# choose one at random
set.seed(4)
randcarc <- sample_n(hires_carcasses, 1)

# Get the data to go with it
randdat <- hires_tags %>%
  filter(lubridate::date(timestamp) == lubridate::ymd(randcarc$date))
dim(randdat)
dim(hires_tags) # huh, it's odd that almost 14% of the data occurs on that date.
nrow(randdat)/nrow(hires_tags)
randdat_simple <- randdat %>%
  select(Nili_id, timestamp, ground_speed, location_lat, location_long, sex, birth_year)

grouped <- randdat %>%
  mutate(grp = cut(timestamp, breaks = "hour")) %>%
  group_split(grp)

fn <- function(dat, carc, minlat, maxlat, minlong, maxlong){
  dat %>%
    ggplot(aes(x = location_long, y = location_lat, col = factor(Nili_id)))+
    geom_point(aes(group = Nili_id), alpha = 0.2)+
    theme_classic()+
    theme(legend.position = "none")+
    geom_point(data = carc, aes(x = long, y = lat), col = "black")
}

minlat <- min(randdat$location_lat)
maxlat <- max(randdat$location_lat)
minlong <- min(randdat$location_long)
maxlong <- max(randdat$location_long)
walk(grouped, ~fn(.x, randcarc, minlat, maxlat, minlong, maxlong))

