# Script for creating a predictability raster
## How often are carcasses present in various grid squares?

# Load packages
library(sf)
library(terra)
library(dplyr)
library(lubridate)
library(purrr)
library(magick)
library(patchwork)
library(tidyverse)
library(targets)
library(here)
source(here("R/functions.R"))
library(plotly)

# Load carcass data
tar_load(all_carcasses)
tar_load(carcasses_audited)
# get all INPA carcasses
carcasses_audited <- carcasses_audited %>%
  select(carcID, date, long, lat, geometry, X, Y, carcassWeight) %>%
  mutate(carcType = "inpa")
# get all carcasses for the three hf windows, including both wild and inpa
all_carcasses <- all_carcasses %>%
  select(carcID, date, dateOnly, long, lat, geometry, X, Y, carcType, nBouts, nIndivs, carcassWeight) %>%
  mutate(date = case_when(is.na(date) & !is.na(dateOnly) ~ lubridate::ymd(dateOnly), .default = date)) %>%
  select(-dateOnly)
# add on all the carcasses from the other times besides the hf windows
all_carcasses <- bind_rows(all_carcasses %>% mutate(source = "ac"), carcasses_audited %>% mutate(source = "ca")) 
# deduplicate, defaulting to all_carcasses
all_carcasses <- all_carcasses %>%
  arrange(carcID, source) %>%
  group_by(carcID) %>%
  slice(1)
tar_load(bbox_south_big)
all_carcasses <- st_crop(all_carcasses, bbox_south_big)

all_carcasses %>% 
  mutate(year = lubridate::year(date), dateOnly = lubridate::date(date)) %>%
  group_by(year, dateOnly, carcType) %>%
  filter(year >= 2020) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = dateOnly, y = n, fill = carcType))+
  geom_col()+
  facet_wrap(~year, scales = "free_x", nrow = 1)+
  theme_classic()

rast_all_5km <- points_to_raster(carcasses_sf = all_carcasses, bbox = bbox_south_big, resolution = 5000)
rast_all_1km <- points_to_raster(carcasses_sf = all_carcasses, bbox = bbox_south_big, resolution = 1000)
plot(rast_all_1km)

# Now divide the data into years and run this on all the years
years_list <- all_carcasses %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  group_split()

rasts_1km <- map(years_list, ~points_to_raster(.x, bbox_south_big, 1000))
rasts_5km <- map(years_list, ~points_to_raster(.x, bbox_south_big, 5000))
# Convert each raster to a data frame and tag with year
rasts_df_5km <- map2_dfr(
  rasts_5km,
  map_dbl(years_list, ~.x$year[1]),
  ~as.data.frame(.x, xy = TRUE) %>% 
    rename(value = 3) %>%
    mutate(year = .y)
)

rasts_df_1km <- map2_dfr(
  rasts_1km,
  map_dbl(years_list, ~.x$year[1]),
  ~as.data.frame(.x, xy = TRUE) %>% 
    rename(value = 3) %>%
    mutate(year = .y)
)

# Inspect global value range (for setting color scale)
range_vals_5km <- range(rasts_df_5km[rasts_df_5km$year <= 2020,]$value, na.rm = TRUE)
range_vals_1km <- range(rasts_df_1km[rasts_df_1km$year <= 2020,]$value, na.rm = TRUE)

# Plot using ggplot2
rasts_df_5km %>%
  filter(year >= 2020) %>%
  ggplot(aes(x = x, y = y, fill = value)) +
  geom_raster() +
  facet_wrap(~year, nrow = 1) +
  scale_fill_viridis_c(limits = range_vals_5km, na.value = "transparent") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Number of carcasses per year",
       fill = "Carcasses", y = "", x = "")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")

rasts_df_1km %>%
  filter(year >= 2020) %>%
  ggplot(aes(x = x, y = y, fill = value)) +
  geom_raster() +
  facet_wrap(~year, nrow = 1) +
  scale_fill_viridis_c(limits = range_vals_1km, na.value = "transparent") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Number of carcasses per year",
       fill = "Carcasses", y = "", x = "")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")

startDate <- "2023-03-15"
endDate <- "2023-04-15"
test <- dist_to_carcasses(all_carcasses, bbox_south_big, resolution = 1000, start_date = startDate,
                          end_date = endDate, active_days = 3, visibility_radius = 10000)

png_files <- get_pngs(test)
imgs <- image_read(png_files)
animation <- image_animate(imgs, fps = 2) 
animation
image_write(animation, path = "fig/month_1000_act3_vis10km_decay0.gif") # XXX something's wrong with the coloring here, but we can fix that later.

# Visualizing different decay rates over time
initial_weight <- 500
days <- 1:10
decay_rates <- seq(0.1, 2, by = 0.05)

# Create data frame
decay_df <- expand.grid(day = days, decay_rate = decay_rates)
decay_df$weight <- initial_weight * exp(-decay_df$decay_rate * (decay_df$day - 1))

decay_df %>%
  ggplot(aes(x = day, y = weight, col = decay_rate, group = factor(decay_rate)))+
  geom_line()+
  theme_classic()+
  scale_color_viridis_c()+
  geom_hline(aes(yintercept = 5), col = "red")

# Now with weighted distances to carcasses, assuming decay rate of -2
test_wt <- dist_to_carcasses(all_carcasses, bbox_south_big, resolution = 1000,
                             start_date = startDate, end_date = endDate, 
                             weight_col = "carcassWeight",
                             visibility_radius = 10000, decay_rate = 2, 
                             distance_power = 2, min_weight = 10)

png_files <- get_pngs(test_wt)
imgs <- image_read(png_files)
animation <- image_animate(imgs, fps = 2) 
animation
image_write(animation, path = "fig/month_1000_dist2_vis10km_decay2.gif") # XXX something's wrong with the coloring here, but we can fix that later.

cell_values_long <- as.data.frame(test_wt, cells = T, wide = F)

# cell_values_long %>%
#   ggplot(aes(x = layer, y = values/1000, group = cell))+
#   geom_line(alpha = 0.05)+
#   theme_classic()+
#   labs(y = "Weighted distance (km)",
#        x = "Days",
#        title = "Distance to active carcasses",
#        caption = "Weighted distance takes into account the distance to all carcasses\nwith remaining weight <= 5kg. Carcass weight declines after placement.")

mean_raster <- terra::mean(test_wt, na.rm = TRUE)
var_raster <- terra::app(test_wt, fun = function(x) var(x, na.rm = TRUE))

# Compute min, max, and normalize as before
mean_min <- global(mean_raster, "min", na.rm = TRUE)[[1]]
mean_max <- global(mean_raster, "max", na.rm = TRUE)[[1]]
var_min  <- global(var_raster,  "min", na.rm = TRUE)[[1]]
var_max  <- global(var_raster,  "max", na.rm = TRUE)[[1]]

mean_range <- mean_max - mean_min
var_range  <- var_max  - var_min

mean_norm <- if (mean_range > 0) (mean_raster - mean_min) / mean_range else mean_raster * 0
var_norm  <- if (var_range  > 0) (var_raster  - var_min)  / var_range  else var_raster  * 0

# Convert to data frame
df <- as.data.frame(c(mean_norm, var_norm), xy = TRUE, na.rm = FALSE)
colnames(df)[3:4] <- c("mean", "variance")

# Mean legend plot (grayscale to red)
mean_plot <- ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = mean)) +
  scale_fill_viridis_c(direction = -1) +
  coord_equal() +
  labs(title = "Mean weighted distance (normalized)") +
  theme_minimal()
mean_plot

# Variance legend plot (grayscale to blue)
var_plot <- ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = variance)) +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(title = "Variance in weighted distance (normalized)") +
  theme_minimal()
var_plot

## Let's look at these patterns over a longer timescale--all of 2023
test_wt_year <- dist_to_carcasses(all_carcasses, bbox_south_big, resolution = 5000,
                                  start_date = "2023-01-01", end_date = "2023-12-31", 
                                  weight_col = "carcassWeight",
                                  visibility_radius = 10000, decay_rate = 2, 
                                  distance_power = 2, min_weight = 10)

png_files <- get_pngs(test_wt_year)
imgs <- image_read(png_files)
animation <- image_animate(imgs, fps = 2) 
animation
image_write(animation, path = "fig/year_5000_dist2_vis10km_decay2.gif")

# Carcass availability on the entire landscape over time
carcs_2023 <- all_carcasses %>%
  select(date, carcassWeight, X, Y) %>%
  filter(date >= lubridate::ymd("2023-01-01"), date <= lubridate::ymd("2023-12-31")) %>%
  mutate(date = as.Date(date))

cell_values_long_2023 <- as.data.frame(test_wt_year, cells = T, wide = F) %>%
  mutate(layer = lubridate::ymd(layer))
dim(cell_values_long_2023)
cell_values_long_2023 %>%
  group_by(layer) %>%
  summarize(mn = mean(values/1000)) %>%
  filter(mn < 99936) %>% # restrict to only the cells that sometimes have less than the max distance to a carcass
  ggplot(aes(x = layer))+
  #geom_vline(data = carcs_2023, aes(xintercept = date), alpha = 0.1)+
  geom_line(aes(y = mn), col = "black")+
  theme_classic()+
  labs(title = "Weighted distance to carcasses, 2023",
       subtitle = "Southern region",
       y = "Weighted distance to active carcasses (km)",
       x = "Date",
       caption = "Carcass weights decline exponentially, rate = 1; Distance power = 2 (inverse square)\nBlack line = region-wide mean")

# How far do vultures tend to be from the carcass, compared with the average of pixels? (habitat selection question)
# I don't have full data pulled for any of the years, so let's focus on the high-frequency period in 2023
# Need to extract, for each vulture, the average weighted distance to active carcasses for each day.
tar_load(gps_2023)
start <- min(gps_2023$dateOnly)
end <- max(gps_2023$dateOnly)
layer_dates <- as.Date(names(test_wt_year))
r_subset <- test_wt_year[[layer_dates >= start & layer_dates <= end]]
gps_2023$dateOnly <- as.Date(gps_2023$dateOnly)
layer_index <- match(gps_2023$dateOnly, layer_dates)
gps_2023_sf <- sf::st_as_sf(gps_2023, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F) %>% sf::st_transform(32636)
gps_sf_days <- gps_2023_sf %>% group_by(dateOnly) %>% group_split()
gps_vect_days <- map(gps_sf_days, vect)
dates <- map_chr(gps_vect_days, ~as.character(.x$dateOnly[[1]]))
gps_sf_out <- vector(mode = "list", length = length(gps_vect_days))
for(i in 1:length(dates)){
  rast <- r_subset[[dates[i]]]
  out <- setNames(terra::extract(rast, gps_vect_days[[i]], cells = T, ID = F), c("value", "cell"))
  gps_sf_out[[i]] <- cbind(gps_vect_days[[i]], out)
}
out <- as.data.frame(do.call(rbind, gps_sf_out))

raster_day_means <- as.data.frame(global(r_subset, fun = "mean", na.rm = TRUE)) %>%
  mutate(dateOnly = as.Date(row.names(.))) %>%
  rename("region_mean" = mean)
vulture_day_means <- out %>%
  group_by(local_identifier, dateOnly) %>%
  summarize(mn = mean(value), .groups = "drop") %>%
  left_join(raster_day_means, by = "dateOnly")

# How far are the vultures
vulture_day_means %>%
  ggplot(aes(x = dateOnly, y = mn, group = local_identifier))+
  geom_line(aes(y = region_mean), col = "blue")+
  geom_line(alpha = 0.1)+
  theme_minimal() # trivial result--vultures stay much closer to carcasses than the average pixel. In order to really quantify what's going on, we would need to do habitat selection analyses. This also of course doesn't take into account that you can be really close to one carcass and really far from another.

# Now get the carcass weights over time
test <- all_carcasses %>% filter(date >= start & date <= end) %>%
  select(carcID, date, X, Y, carcType, carcassWeight) %>%
  mutate(carcassWeight = case_when(is.na(carcassWeight) ~ mean(carcassWeight, na.rm = T), .default = carcassWeight)) %>%
  mutate(date = lubridate::date(date))
dates <- seq.Date(start, end)
carcs <- sort(unique(test$carcID))
dec <- 1.5
fill_in_exp <- function(prev, new, decay = dec) {
  if_else(!is.na(new), new, prev * exp(-1*decay))
}
df <- expand_grid("date" = dates, "carcID" = carcs) %>%
  left_join(test, by = c("date", "carcID")) %>%
  arrange(carcID, date) %>% # fill downward and add exponential decline here
  group_by(carcID) %>%
  fill(c("X", "Y", carcType), .direction = "downup") %>%
  mutate(carcassWeight = accumulate(carcassWeight, fill_in_exp)) %>%
  mutate(carcassWeight = case_when(carcassWeight < 10 ~ NA, .default = carcassWeight))

# Carcass decay over time
df %>%
  ggplot(aes(x = date, y = carcassWeight, group = carcID, col = carcType))+
  geom_line()+
  theme_minimal()+
  labs(y = "Carcass weight (kg)",
       x = "Date",
       col = "Carcass type",
       caption = paste0("Exponential decay parameter = -", dec, "\n", "(Wild carcasses set to mean weight of INPA carcasses)"))+
  theme(legend.position = "bottom")+
  scale_color_viridis_d()

# Now, amount of meat on the landscape at a time
meat_on_landscape <- df %>%
  group_by(date) %>%
  summarize(all = sum(carcassWeight, na.rm = T),
            `wild (est)` = sum(carcassWeight[carcType == "wild"], na.rm = T),
            inpa = sum(carcassWeight[carcType == "inpa"], na.rm = T)) %>%
  pivot_longer(cols = c("all", "wild (est)", "inpa"), names_to = "type", values_to = "kg")

meat_on_landscape %>%
  filter(type != "all") %>%
  ggplot(aes(x = date, y = kg, fill = type))+
  geom_area()+
  theme_minimal()+
  labs(y = "Meat on landscape (kg)",
       x = "Date",
       col = "Type of carcass",
       title = "Carcass weight, south, Mar-Apr 2023",
       caption = paste0("Exponential decay parameter = -", dec, "\n", "(Wild carcasses set to mean weight of INPA carcasses)"))+
  scale_fill_manual(values = c("firebrick1", "skyblue"))

# Altitudes as vultures descend to the carcass

# Let's look at the trajectories of vultures that approach one known carcass
tar_load(gps_all_inpa)
tar_load(inpa_carcs)
tar_load(roostPolygons, store = "~/Desktop/projects/MvmtSoc/_targets/")
rp <- sf::st_read(roostPolygons) %>% sf::st_transform(32636)
rp_cropped <- st_crop(rp, bbox_south_big)
focal <- gps_all_inpa[[36]] %>% filter(time_since_carcass > -24 & time_since_carcass < 48)
focal %>% count(local_identifier) %>% arrange(desc(n)) # let's pick E54w, which has a lot of points
set.seed(5)
indiv <- sample(unique(focal$local_identifier), 1)
focal_indiv <- focal %>% filter(local_identifier == indiv) %>% sf::st_as_sf(crs = "32636")
carc <- inpa_carcs[[36]]

focal_line <- focal_indiv %>%
  arrange(time_since_carcass) %>%
  summarise(do_union = FALSE) %>%
  summarise(geometry = st_combine(geometry)) %>%
  mutate(geometry = st_cast(geometry, "LINESTRING")) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = focal_line, color = "black", alpha = 0.2) +
  geom_sf(data = focal_indiv, aes(col = height_above_msl), alpha = 0.7) +
  geom_sf(data = rp_cropped) +
  geom_sf(data = carc, col = "red") +
  scale_color_viridis_c()+
  theme_minimal()

mapview(focal_line)+mapview(focal_indiv, zcol = "height_above_msl")+mapview(rp_cropped, col.regions = "gray")+mapview(carc, col.regions = "red")

focal_indiv %>%
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass, col = height_above_msl))+
  geom_point(alpha = 0.7)+
  geom_line()+
  theme_minimal()+
  scale_color_viridis_c()+
  labs(y = "Distance to carcass", x = "Time since carcass (hours)", col = "Height (m)", title = indiv)

fig <- plot_ly(focal_indiv, x = ~location_long, y = ~location_lat, z = ~height_above_msl, type = 'scatter3d', mode = 'lines',
               opacity = 1, line = list(width = 6)) 

fig # can make this better but at least it's possible to make a 3d plot.

# The reason I wanted to see things in 3D in the first place was to determine whether we can identify when an individual is approaching a carcass, sort of like cassidy's turning points.
# let's get each instance of landing near a carcass and then walk the GPS back 10 hours and look at the track.
# each instance of landing near a carcass--need to go to the code with the histograms and get the data for all the INPA carcasses. bind into one df, group by individual, sort by time, identify the first instance of being on the ground nearby, and then grab the previous 10 hours.
tar_load(gps_all_inpa)
tar_load(gps_spd)
tar_load(detection_distance_flight)
tar_load(detection_distance_stationary)
ddf <- detection_distance_flight
dds <- detection_distance_stationary
stn <- purrr::list_rbind(gps_all_inpa) %>% sf::st_drop_geometry() %>% 
  mutate(type = "inpa") %>%
  mutate(hour_bin = floor_date(timestamp, unit = "hours"),
         hour_bin_rel = round(time_since_carcass),
         in_sight = case_when(ground_speed >= 5 & dist_to_carcass <= ddf ~ T,
                              ground_speed < 5 & dist_to_carcass <= dds ~ T,
                              .default = F),
         status = case_when(ground_speed >= 5 & dist_to_carcass <= ddf ~ "flight, in sight (<2km)",
                            ground_speed >= 5 & dist_to_carcass > ddf ~ "flight, >2km",
                            ground_speed < 5 & dist_to_carcass <= dds & dist_to_carcass > 200 ~ "stationary, in sight (1km-200m)",
                            ground_speed <= 5 & dist_to_carcass <= 200 ~ "stationary, <200m",
                            ground_speed <= 5 & dist_to_carcass > dds ~ "stationary, >1km", .default = NA),
         status = factor(status, levels = c("stationary, <200m", "stationary, in sight (1km-200m)", "flight, in sight (<2km)", "flight, >2km", "stationary, >1km")),
         hour = round(time_since_carcass)) %>%
  select(-c("tag_local_identifier", "tag_id", "hour_bin", "hour_bin_rel", "in_sight"))

stn <- stn %>%
  filter(time_since_carcass > -1) %>% # must be later than 1 hour before carcass placement
  arrange(carcID, local_identifier, timestamp) %>%
  group_by(carcID, local_identifier) %>%
  mutate(cumul_groundpoints = cumsum(status == "stationary, <200m")) %>%
  ungroup() %>%
  group_by(carcID, local_identifier) %>%
  mutate(firstlanding = case_when(cumul_groundpoints == 1 & lag(cumul_groundpoints == 0) ~ TRUE, .default = FALSE),
         has_first_landing = case_when(sum(firstlanding) > 0 ~ T, .default = F)) %>% # does this individual ever land at the carcass?
  mutate(dist_diff = dist_to_carcass - lag(dist_to_carcass))


first_landings <- stn %>%filter(cumul_groundpoints == 0 | firstlanding == TRUE) %>% # remove all points except for the landing itself and the points prior
  filter(has_first_landing) %>% # restrict to individuals that land at the carcass at some point
  ungroup()

first_landings_mycarc <- first_landings %>% filter(carcID == 4436953)
set.seed(3)
indivs <- sample(unique(first_landings_mycarc$local_identifier), 6)

first_landings_mycarc %>%
  filter(local_identifier %in% indivs) %>%
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, 
             col = height_above_msl, shape = firstlanding, group = local_identifier))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(19, 1))+
  facet_wrap(~local_identifier, scales = "free_x")+
  scale_color_viridis_c()+
  theme_minimal()+ # all of these have fairly clear declines as the individual approaches, and sometimes those declines continue over many km.
  labs(y = "Dist to carcass (km)",
       x = "Time since carcass (hours)")

# The approach to the carcass is the last run of negative values for each individual. can we define the slopes as positive or negative and use that to identify the approach?
first_landings_mycarc <- first_landings_mycarc %>%
  group_by(carcID, local_identifier) %>%
  mutate(dist_diff = dist_to_carcass - lag(dist_to_carcass),# apply this to everything
         neg = dist_diff < 0, 
         run = data.table::rleid(neg)) 

approaches <- first_landings_mycarc %>%
  group_by(carcID, local_identifier) %>%
  slice(unique(sort(c(
    which(run == max(run)),
    which(run == max(run)) - 1
  )))) # hopefully this should include the last point before the approach, all points in the approach, and the final landing point.

approaches %>%
  filter(local_identifier %in% indivs) %>%
  ggplot(aes(x = time_since_carcass, y = dist_to_carcass/1000, 
             col = height_above_msl, shape = firstlanding, group = local_identifier))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(19, 1))+
  facet_wrap(~local_identifier, scales = "free_x")+
  scale_color_viridis_c()+
  theme_minimal()+ # all of these have fairly clear declines as the individual approaches, and sometimes those declines continue over many km.
  labs(y = "Dist to carcass (km)",
       x = "Time since carcass (hours)") # awesome! we're seeing the distance to the carcass decrease over time as the vulture approaches.

# My brain keeps thinking that I'm seeing altitude declines over time. Let's do that instead
approaches %>%
  filter(local_identifier %in% indivs) %>%
  ggplot(aes(x = time_since_carcass, y = height_above_msl, shape = firstlanding, group = local_identifier))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(19, 1))+
  facet_wrap(~local_identifier, scales = "free_x")+
  scale_color_viridis_c()+
  theme_minimal()+ 
  labs(y = "Height above MSL (m)",
       x = "Time since carcass (hours)") # this shows the flight profiles of the vultures over time as they approach the carcass.

# Colored by distance to carcass
approaches %>%
  filter(local_identifier %in% indivs) %>%
  ggplot(aes(x = time_since_carcass, y = height_above_msl, shape = firstlanding, 
             col = dist_to_carcass/1000, group = local_identifier))+
  geom_line()+
  geom_point()+
  scale_shape_manual(values = c(19, 1))+
  facet_wrap(~local_identifier, scales = "free_x")+
  scale_color_viridis_c(name = "Dist to carcass (km)")+
  theme_minimal()+ 
  labs(y = "Height above MSL (m)",
       x = "Time since carcass (hours)")+ # this doesn't tell us new information, just shows that the distance to the carcass decreases over time as they make their approach (circular reasoning). But it does start to hint at how far away vultures are when they start to make their approach to the carcass!
  theme(legend.position = "bottom")

# I'd like to visualize these individuals in space
fig <- plot_ly(approaches, x = ~location_long, y = ~location_lat, z = ~height_above_msl, color = ~local_identifier, type = 'scatter3d', mode = 'lines',
               opacity = 1, line = list(width = 6))  # this is a hot mess, but even here I can see that I have some paired flight trajectories!

fig

approaches %>% ggplot(aes(x = timestamp, y = dist_to_carcass, col = local_identifier))+
  geom_line()+
  theme_minimal()+
  theme(legend.position = "none") # approaches over time. We can start to see upward slopes within each day, indicating that the vultures seem to be approaching from farther away as the day goes on. Let's see if that's borne out when we analyze it explicitly.

# Okay what about looking at the max distance of each approach
breaks <- c(-24, 0, 24, 48, 72)
approaches_stats <- approaches %>%
  mutate(day = cut(as.numeric(hour), breaks)) %>%
  group_by(carcID, local_identifier, day) %>%
  summarize(max_dist = max(dist_to_carcass),
         approach_start = min(time_since_carcass))
approaches_stats %>%
  ggplot(aes(x = approach_start, y = max_dist/1000, col = day))+
  geom_smooth(method = "lm")+
  geom_point(pch = 1, size = 2)+
  theme_minimal()+
  labs(y = "Start distance of approach (km)",
       x = "Start time of approach (hours since carcass)")+
  scale_color_viridis_d(name = "Hours since carcass") # for this carcass, vultures are beginning their approaches from farther away as the day goes on--this would support either local enhancement or chains of vultures.
