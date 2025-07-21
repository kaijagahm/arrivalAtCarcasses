# Seeing if we can identify the wild carcasses by GPS fix--based on the info May gave us.
library(tidyverse)
library(sf)
library(mapview)
library(vultureUtils)
library(here)
source(here("R/functions.R"))

non_sfs_examples <- tribble(~lat, ~long, ~date, ~`first arrival time`, ~individuals, ~details,
                            31.096410751342773, 35.085052490234375, "23-03-2025", "11:07", "A03w, J29w, B54w, B79w, B61w, J35w, B97w, B63w, E15w, J52w, E38w, B49w, B14w, J16w, E17w", "Young donkey foal carcass", 
                            30.866781234741207, 34.85071563720703, "16-04-2025", "11:19", "B12w, E12w, E38w, B57w, B49w, E17w, T24b, A03w, A40w, A57w, A78w, B88w, B89w, E11w, B99w, J35w, B83w, E00w, E58w, B62w, E15w, B18w, E57w, E60w, E77w, J16w", "Mature male ibex carcass")

ex_sf <- sf::st_as_sf(non_sfs_examples, coords = c("long", "lat"), remove = F, crs = "WGS84") %>% sf::st_transform(32636) %>%
  mutate(datetime = lubridate::dmy_hm(paste(date, `first arrival time`)),
         date = lubridate::dmy(date))
mapview(ex_sf)

pw <- here("data/movebankCredentials/pw.Rda")
lo <- get_loginObject(pw)
newdata <- vultureUtils::downloadVultures(lo, dateTimeStartUTC = as.POSIXct(lubridate::ymd("2025-03-01")), dateTimeEndUTC = as.POSIXct(lubridate::ymd("2025-06-01")))
dim(newdata)
tar_load(gps_spd)
tar_load(detection_distance_flight)
tar_load(detection_distance_stationary)
newdata_sf <- sf::st_as_sf(newdata, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F) %>% sf::st_transform(32636) # transform to sf and project
# For each carcass, calculate distance and status
gps_each_carc <- map(1:nrow(ex_sf), ~newdata_sf %>%
                       filter(timestamp >= ex_sf$datetime[.x]-days(3), # 3 days before
                              timestamp <= ex_sf$datetime[.x] + days(5)) %>%
                       mutate(dist_to_carc = as.numeric(sf::st_distance(., ex_sf$geometry[.x])),
                              time_since_carcass = difftime(timestamp, ex_sf$datetime[.x], units = "hours")) %>%
                       mutate(in_sight = case_when(ground_speed >= 5 & dist_to_carc <= detection_distance_flight ~ T,
                                                   ground_speed < 5 & dist_to_carc <= detection_distance_stationary ~ T,
                                                   .default = F),
                              status = case_when(ground_speed >= 5 & dist_to_carc <= detection_distance_flight ~ "flight, in sight (<2km)",
                                                 ground_speed >= 5 & dist_to_carc > detection_distance_flight ~ "flight, >2km",
                                                 ground_speed < 5 & dist_to_carc <= detection_distance_stationary & dist_to_carc > 200 ~ "stationary, in sight (1km-200m)",
                                                 ground_speed <= 5 & dist_to_carc <= 200 ~ "stationary, <200m",
                                                 ground_speed <= 5 & dist_to_carc > detection_distance_stationary ~ "stationary, >1km", .default = NA),
                              status = factor(status, levels = c("stationary, <200m", "stationary, in sight (1km-200m)", "flight, in sight (<2km)", "flight, >2km", "stationary, >1km"))) %>% 
                       mutate(hour = as.numeric(round(time_since_carcass, 0))))
map_dbl(gps_each_carc, nrow) # many fewer rows now; good.

# plotting code for wild carcasses
color_scale <- c("red", "orange", "skyblue", "gray", "gray50")
plots <- vector(mode = "list", length = nrow(ex_sf))
for(i in 1:nrow(ex_sf)){
  df <- gps_each_carc[[i]] %>%
    filter(!(status %in% c("flight, >2km", "stationary, >1km")))
  plt <- df %>% group_by(hour, status) %>%
    summarize(n = length(unique(local_identifier))) %>%
    ggplot(aes(x = hour, fill = status, y = n))+
    geom_vline(aes(xintercept = 0), linetype = 2, alpha = 0.5)+
    geom_col(position = position_stack(reverse = TRUE))+
    labs(y = "# vultures", x = "Hours since carcass")+
    theme_minimal()+
    theme(legend.position = "bottom")+
    scale_fill_manual(name = "", values = color_scale, drop = F)
  plots[[i]] <- plt
}

# okay, so we can in fact detect the vultures...
# let's see if these line up with what May reported
indivs_1 <- gps_each_carc[[1]] %>% filter(status == c("stationary, <200m")) %>%
  pull(local_identifier) %>%
  unique() %>%
  sort()
indivs_2 <- gps_each_carc[[2]] %>% filter(status == c("stationary, <200m")) %>%
  pull(local_identifier) %>%
  unique() %>%
  sort()
all_reported_1 <- sort(unlist(str_split(ex_sf$individuals[1], ", ")))
all_reported_2 <- sort(unlist(str_split(ex_sf$individuals[2], ", ")))

indivs_2 %in% all_reported_2 # mostly T, but it's interesting that some of these aren't listed.
indivs_1 %in% all_reported_1 # likewise, mostly T but not all.
