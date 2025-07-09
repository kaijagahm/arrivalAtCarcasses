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
  scale_fill_viridis(direction = -1) +
  coord_equal() +
  labs(title = "Mean weighted distance (normalized)") +
  theme_minimal()
mean_plot

# Variance legend plot (grayscale to blue)
var_plot <- ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = variance)) +
  scale_fill_viridis() +
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
  ggplot(aes(x = date, y = kg, col = type))+
  geom_line(linewidth = 1.5, alpha = 0.7)+
  theme_minimal()+
  labs(y = "Meat on landscape (kg)",
       x = "Date",
       col = "Type of carcass",
       title = "Carcass weight, south, Mar-Apr 2023")+
  scale_color_manual(values = c("black", "firebrick1", "skyblue"))

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

