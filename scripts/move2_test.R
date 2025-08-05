# Testing out move2 package--following the vignette
# Hopefully going to incorporate the inpa data, as well as ornitela, into the dataset
library(move2)
library(sf)
library(tidyverse)
library(units)
library(rnaturalearth)
library(ggspatial) # for map tiles
library(gganimate)
# reference: https://cran.r-project.org/web/packages/move2/vignettes/programming_move2_object.html
track <- movebank_download_study("Galapagos Albatrosses", sensor_type_id = "gps")
track

ggplot()+
  geom_sf(data = ne_coastline(returnclass = "sf", 10))+
  theme_linedraw()+
  geom_sf(data = track)+
  geom_sf(data = mt_track_lines(track), aes(color = `individual_local_identifier`))+
  coord_sf(
    crs = sf::st_crs("+proj=aeqd +lon_0=-83 +lat_0=-6 +units=km"), # there has to be a way to put these in using bounding boxes instead of doing it all manually??
    xlim = c(-1000, 600),
    ylim = c(-800, 700)
  )

tuvu_data <- movebank_download_study("Turkey vultures in North and South America")
tuvu_data
# split tracks in case they have large gaps
tuvu_lines <- tuvu_data %>%
  mutate_track_data(name = individual_local_identifier) %>%
  mutate(large_gaps = !(mt_time_lags(.) < set_units(1500, "h") |
                          is.na(mt_time_lags(.))),
         track_sub_id = cumsum(lag(large_gaps, default = FALSE)),
         new_track_id = paste(mt_track_id(.), track_sub_id)) %>%
  mt_set_track_id("new_track_id") %>%
  mt_track_lines()

ggplot()+
  geom_sf(data = ne_coastline(returnclass = "sf", scale = 50))+
  theme_linedraw()+
  geom_sf(data = tuvu_lines, aes(color = name))+
  coord_sf(
    crs = sf::st_crs("+proj=aeqd +lon_0=-83 +lat_0=8 +units=km"),
    xlim = c(-3500, 3800), ylim = c(-4980, 4900)
  )

# Add speeds
tuvu_data <- tuvu_data %>%
  mutate(speed = mt_speed(.), azimuth = mt_azimuth(.))

tuvu_data %>%
  ggplot(aes(x = log(speed), fill = individual_local_identifier))+
  geom_histogram()+
  facet_wrap(~individual_local_identifier)+
  theme_minimal()

# single vulture
leo <- tuvu_data %>%
  filter_track_data(individual_local_identifier == "Leo") %>%
  mutate(speed_categorical = cut(speed, breaks = c(2, 5, 10, 15, 35)))

# Many more things in the vignette. Time to try it with the vulture data.
inpa_2023 <- movebank_download_study(6071688,
                                     sensor_type_id = "gps",
                                     timestamp_start = as.POSIXct("2023-03-01 00:00:00"),
                                     timestamp_end = as.POSIXct("2023-05-01 00:00:00")
)

ornitela_2023 <- movebank_download_study(1252551761,
                                         sensor_type_id = "gps",
                                         timestamp_start = as.POSIXct("2023-03-01 00:00:00"),
                                         timestamp_end = as.POSIXct("2023-05-01 00:00:00"))

both <- mt_stack(inpa_2023, ornitela_2023) %>% st_transform(32636)
mt_track_data(both)
table(mt_track_id(both))
sf::st_crs(both)
sf::st_bbox(both)
tar_load(bbox_south_big)

both_cropped <- st_crop(both, bbox_south_big)
cropped_lines <- mt_track_lines(both_cropped)

## Visualize: basic map of all tracks in the southern area
ggplot(cropped_lines)+
  annotation_map_tile("cartodark", zoom = 9, progress = "none") +
  geom_sf(aes(color = individual_local_identifier), alpha = 0.1)+
  theme(legend.position = "none")

range(mt_time(both_cropped))

# Let's pick one day to animate--make animation following the tutorial, and choose 5 random individuals to animate on that day (XXX followed the tutorial but haven't gotten it to work yet)
# set.seed(3)
# for_animation <- both_cropped %>% filter(timestamp>=lubridate::ymd("2023-03-15"), timestamp <= lubridate::ymd("2023-03-16"))
# indivs <- sample(unique(for_animation$individual_local_identifier), 10)
# for_animation <- for_animation %>%
#   filter(individual_local_identifier %in% indivs)
# 
# ggplot(mt_track_lines(for_animation))+
#   annotation_map_tile("cartodark", zoom = 9, progress = "none") +
#   geom_sf(aes(color = individual_local_identifier), alpha = 0.9)+
#   theme(legend.position = "none")
# 
# date_range <- as.POSIXct(c("2023-03-15", "2023-03-16"))
# ts <- mt_time(for_animation)
# times <- sort(unique((c(date_range, ts[ts < max(date_range) & ts > min(date_range)]))))
# 
# data_interpolated <- mt_interpolate(
#   for_animation[!sf::st_is_empty(for_animation), ],
#   times,
#   omit = TRUE
# )
# 
# label_df <- data.frame(
#   timestamp = date_range,
#   display_time = lubridate::with_tz(date_range, "Israel")
# )
# 
# animation <- ggplot() +
#   annotation_map_tile("cartodark", zoom = 9, progress = "none") +
#   annotation_scale(bar_cols = c("gray80", "gray40"), text_col = "gray80") +
#   geom_sf(data = mt_track_lines(for_animation), color = "grey40") +
#   theme_linedraw() +
#   geom_sf(
#     data = data_interpolated, size = 3,
#     aes(color = individual_local_identifier)
#   ) +
#   scale_color_brewer(palette = "Set1") +
#   guides(color = "none") +
#   xlab("") +
#   ylab("") +
#   geom_text(
#     data = label_df,
#     aes(label = display_time, x = -10100000, y = -1370000),
#     color = "grey80", size = 3, hjust = 0
#   ) +
#   transition_time(timestamp) +
#   shadow_wake(0.2, exclude_layer = 6)
# 
# animate(animation,
#         nframes = 25, detail = 5
# )

# XXX haven't gotten this to work yet

# Ok animation didn't work, but let's convert the timestamps to israel time and look at movement over the course of the day

both_cropped <- both_cropped %>%
  mutate(timestamp = with_tz(timestamp, tzone = "Israel"),
         date = lubridate::date(timestamp),
         hour = lubridate::hour(timestamp),
         min = lubridate::minute(timestamp),
         month = factor(lubridate::month(timestamp), levels = 1:12),
         hour_min = as.numeric(paste0(str_pad(as.character(hour), width = 2, pad = "0"), str_pad(min, width = 2, pad = "0")))) %>%
  mutate(flight = case_when(as.numeric(ground_speed) >=5 ~ T, .default = F))

both_cropped %>%
  filter(flight) %>%
  ggplot(aes(x = hour_min, y = ground_speed))+ # this only includes three months of the year
  geom_point(alpha = 0.05, size = 0.2, aes(color = month))+
  stat_smooth(geom = "line", se = FALSE, alpha = 0.7, 
              aes(color = month))+
  theme_minimal()+
  labs(y = "Ground speed (m/s)",
       x = "Time of day (hour, min)",
       title = "Flight speeds by time of day")
