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

# Load carcass data
tar_load(carcasses_audited)
tar_load(bbox_south_big)
range(carcasses_audited$date) # will eventually need to update this with more recent data, but this is what we have so far.
class(carcasses_audited) # this is already an sf object

points_to_raster <- function(
    carcasses_sf,            # sf POINT object
    bbox,                    # bounding box (numeric or object convertible to sf bbox)
    resolution = 10000       # grid cell size in meters (default 10km)
) {
  # Ensure carcasses are in a projected CRS (assume UTM if not set)
  if (is.na(st_crs(carcasses_sf))) {
    stop("Input 'carcasses_sf' must have a defined CRS.")
  }
  if (st_is_longlat(carcasses_sf)) {
    stop("Please project 'carcasses_sf' to a projected CRS (e.g., UTM).")
  }
  
  # Convert bbox to sf polygon if needed
  if (is.numeric(bbox) && length(bbox) == 4) {
    bbox_mat <- matrix(c(bbox[1], bbox[2], bbox[3], bbox[4]), ncol = 2, byrow = TRUE)
    bbox_poly <- st_as_sfc(st_bbox(c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4]), crs = st_crs(carcasses_sf)))
  } else if (inherits(bbox, "sf") || inherits(bbox, "sfc") || inherits(bbox, "SpatVector")) {
    bbox_poly <- st_as_sfc(st_bbox(bbox))
    bbox_poly <- st_transform(bbox_poly, st_crs(carcasses_sf))
  } else {
    stop("Invalid 'bbox' format. Provide a numeric vector of length 4 or an sf/sfc/SpatVector object.")
  }
  
  # Create a regular grid over the bounding box
  grid <- st_make_grid(bbox_poly, cellsize = resolution, square = TRUE)
  grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)
  
  # Spatial join: assign carcasses to grid cells
  joined <- st_join(carcasses_sf, grid_sf, join = st_within)
  
  # Count carcasses per grid cell
  counts <- joined |>
    group_by(grid_id) |>
    summarise(carcass_count = n(), .groups = "drop")
  
  # Merge counts back to full grid, fill NAs with 0
  grid_with_counts <- left_join(grid_sf, st_drop_geometry(counts), by = "grid_id") |>
    mutate(carcass_count = ifelse(is.na(carcass_count), 0, carcass_count))
  
  # Convert to SpatVector
  grid_vect <- vect(grid_with_counts)
  
  # Create raster template
  r_template <- rast(grid_vect, resolution = resolution)
  
  # Rasterize
  r <- rasterize(grid_vect, r_template, field = "carcass_count", fun = NULL, background = 0)
  
  return(r)
}

rast_all_5km <- points_to_raster(carcasses_sf = carcasses_audited, bbox = bbox_south_big, resolution = 5000)
plot(rast_all_5km)

# Now divide the data into years and run this on all the years
years_list <- carcasses_audited %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  group_split()

rasts_5km <- map(years_list, ~points_to_raster(.x, bbox_south_big, 5000))
walk(rasts_5km, plot)

# Now looking at average distance to active carcass
dist_to_active <- function(
    carcasses_sf,         # sf POINT object with 'datetime' column
    bbox,                 # bounding box (numeric or spatial)
    resolution = 5000,    # cell size in meters
    start_date = NULL,    # optional start date (Date or character)
    end_date = NULL,      # optional end date (Date or character)
    active_days = 3       # how long a carcass is "active"
) {
  
  # Validate timestamp column
  if (!"datetime" %in% names(carcasses_sf)) stop("Missing 'datetime' column.")
  if (!inherits(carcasses_sf$datetime, "Date")) {
    carcasses_sf$datetime <- as.Date(carcasses_sf$datetime)
  }
  
  # Ensure projected CRS
  if (st_is_longlat(carcasses_sf)) {
    stop("Please project 'carcasses_sf' to a projected CRS (e.g., UTM).")
  }
  
  # Convert start and end dates if character
  if (!is.null(start_date)) start_date <- as.Date(start_date)
  if (!is.null(end_date))   end_date   <- as.Date(end_date)
  
  # Define time range from data if not provided
  all_dates <- sort(unique(carcasses_sf$datetime))
  if (is.null(start_date)) start_date <- min(all_dates)
  if (is.null(end_date)) end_date <- max(all_dates)
  
  date_seq <- seq(start_date, end_date, by = "day")
  
  # Build spatial grid (fixed across time)
  if (is.numeric(bbox) && length(bbox) == 4) {
    bbox_poly <- st_as_sfc(st_bbox(c(xmin = bbox[1], ymin = bbox[2],
                                     xmax = bbox[3], ymax = bbox[4]),
                                   crs = st_crs(carcasses_sf)))
  } else {
    bbox_poly <- st_as_sfc(st_bbox(bbox))
    bbox_poly <- st_transform(bbox_poly, st_crs(carcasses_sf))
  }
  
  # Create template raster directly from bbox and resolution
  bbox_ext <- st_bbox(bbox_poly)
  r_template <- rast(xmin = bbox_ext$xmin,
                     xmax = bbox_ext$xmax,
                     ymin = bbox_ext$ymin,
                     ymax = bbox_ext$ymax,
                     resolution = resolution,
                     crs = st_crs(carcasses_sf)$wkt)
  
  # Convert raster to polygons and find centroids
  grid_vect <- as.polygons(r_template)
  grid_sf <- st_as_sf(grid_vect)
  grid_sf$grid_id <- seq_len(nrow(grid_sf))
  grid_centroids <- st_centroid(grid_sf)
  
  # Prepare raster stack
  dist_stack <- rast()
  
  # Loop over each date
  for (current_date in date_seq) {
    active_window <- current_date - days(active_days - 1)
    active_carcasses <- carcasses_sf %>%
      filter(datetime >= active_window & datetime <= current_date)
    
    if (nrow(active_carcasses) == 0) {
      # No active carcasses: assign max possible distance (diagonal of bbox)
      bbox_coords <- st_bbox(bbox_poly)
      
      # Diagonal from lower-left to upper-right
      bbox_diagonal <- sqrt((bbox_coords["xmax"] - bbox_coords["xmin"])^2 +
                              (bbox_coords["ymax"] - bbox_coords["ymin"])^2)
      
      r <- setValues(r_template, bbox_diagonal)
    } else {
      dist_matrix <- st_distance(grid_centroids, active_carcasses)
      mean_distances <- apply(as.matrix(dist_matrix), 1, mean)
      
      grid_sf$mean_dist <- mean_distances
      grid_vect <- vect(grid_sf)
      r <- rasterize(grid_vect, r_template, field = "mean_dist", fun = mean)
    }
    
    names(r) <- as.character(current_date)
    dist_stack <- c(dist_stack, r)
  }
  
  # Convert names to formatted dates
  numeric_names <- as.numeric(names(dist_stack))
  date_names <- as.Date(numeric_names, origin = "1970-01-01")
  names(dist_stack) <- format(date_names, "%Y-%m-%d")
  
  return(dist_stack)
}

test <- dist_to_active(carcasses_audited, bbox_south_big, resolution = 5000, start_date = "2022-01-01", end_date = "2022-04-01", active_days = 5)

get_pngs <- function(rasterstack){
  dates <- names(rasterstack)
  
  # Compute global min and max for color scale
  global_min <- min(values(rasterstack), na.rm = TRUE)
  global_max <- max(values(rasterstack), na.rm = TRUE)
  
  # Temporary list to store frame file paths
  png_files <- character(nlyr(rasterstack))
  
  # Loop through each raster layer
  for (i in seq_len(nlyr(rasterstack))) {
    r <- rasterstack[[i]]
    date_label <- dates[i]
    
    # Convert raster to data frame for ggplot
    r_df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
    colnames(r_df) <- c("x", "y", "value")
    
    # Create ggplot
    p <- ggplot(r_df) +
      geom_raster(aes(x = x, y = y, fill = value)) +
      coord_equal() +
      scale_fill_viridis_c(
        name = "Avg. Distance (m)",
        limits = c(global_min, global_max),
        na.value = "grey90",
        direction = -1
      ) +
      labs(
        title = paste("Date:", date_label),
        x = NULL,
        y = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.position = "right"
      )
    
    # Save to PNG
    png_file <- tempfile(fileext = ".png")
    ggsave(png_file, plot = p, width = 6, height = 6, dpi = 150)
    png_files[i] <- png_file
  }
  return(png_files)
}

png_files <- get_pngs(test)
imgs <- image_read(png_files)
animation <- image_animate(imgs, fps = 5) 
animation

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

# Now instead, calculating the distance to carcasses, accounting for their decay rate
dist_to_weighted_carcasses <- function(
    carcasses_sf,          # sf POINT object with 'datetime' and weight column
    bbox,                  # bounding box (numeric or spatial object)
    resolution = 5000,     # cell size in meters
    start_date = NULL,     # optional start date (Date or character)
    end_date = NULL,       # optional end date (Date or character)
    weight_col = "carcassWeight", # name of column with weight
    decay_rate = 1.5,      # daily decay rate (higher = faster decay)
    min_weight = 5,        # discard carcasses that fall below this weight
    distance_power = 1     # power to weight distance: 1 = linear, 2 = inverse square, etc.
) {
  # Validate and convert datetime
  if (!"datetime" %in% names(carcasses_sf)) stop("Missing 'datetime' column.")
  if (!inherits(carcasses_sf$datetime, "Date")) {
    carcasses_sf$datetime <- as.Date(carcasses_sf$datetime)
  }
  
  # Validate weight column
  if (!weight_col %in% names(carcasses_sf)) stop(paste("Missing weight column:", weight_col))
  
  # Ensure projected CRS
  if (st_is_longlat(carcasses_sf)) stop("Please project 'carcasses_sf' to a projected CRS.")
  
  # Convert character dates to Date
  if (!is.null(start_date)) start_date <- as.Date(start_date)
  if (!is.null(end_date))   end_date <- as.Date(end_date)
  
  # Determine date range
  all_dates <- sort(unique(carcasses_sf$datetime))
  if (is.null(start_date)) start_date <- min(all_dates)
  if (is.null(end_date))   end_date <- max(all_dates)
  date_seq <- seq(start_date, end_date, by = "day")
  
  # Build raster grid
  if (is.numeric(bbox) && length(bbox) == 4) {
    bbox_poly <- st_as_sfc(st_bbox(c(xmin = bbox[1], ymin = bbox[2],
                                     xmax = bbox[3], ymax = bbox[4]),
                                   crs = st_crs(carcasses_sf)))
  } else {
    bbox_poly <- st_as_sfc(st_bbox(bbox))
    bbox_poly <- st_transform(bbox_poly, st_crs(carcasses_sf))
  }
  
  grid <- st_make_grid(bbox_poly, cellsize = resolution)
  grid_sf <- st_sf(grid_id = seq_along(grid), geometry = grid)
  centroids <- st_centroid(grid_sf)
  grid_vect <- vect(grid_sf)
  r_template <- rast(grid_vect, resolution = resolution)
  
  # Precompute max distance for use when no active carcasses
  bbox_coords <- st_coordinates(st_cast(bbox_poly, "POLYGON"))[, 1:2]
  max_possible_dist <- sqrt(sum((apply(bbox_coords, 2, max) - apply(bbox_coords, 2, min))^2))
  
  if (anyNA(carcasses_sf[[weight_col]])) {
    mean_weight <- mean(carcasses_sf[[weight_col]], na.rm = TRUE)
    carcasses_sf[[weight_col]][is.na(carcasses_sf[[weight_col]])] <- mean_weight
  }
  
  dist_stack <- rast()
  
  for (current_date in date_seq) {
    # Compute decay-adjusted weight
    decay_df <- carcasses_sf %>%
      mutate(days_elapsed = as.numeric(current_date) - as.numeric(datetime)) %>%
      filter(!is.na(.data[[weight_col]]), days_elapsed >= 0) %>%
      mutate(
        decayed_weight = .data[[weight_col]] * exp(-decay_rate * days_elapsed),
        active = decayed_weight >= min_weight
      ) %>%
      filter(active)
    
    if (nrow(decay_df) == 0) {
      # No active carcasses: assign max possible distance
      r <- setValues(r_template, max_possible_dist)
    } else {
      dist_matrix <- st_distance(centroids, decay_df)
      weight_matrix <- matrix(rep(decay_df$decayed_weight, each = nrow(centroids)), 
                              nrow = nrow(centroids))
      
      # Handle power weighting of distances
      dist_weighted <- (as.matrix(dist_matrix)^distance_power) * weight_matrix
      weighted_mean_dist <- rowSums(dist_weighted) / rowSums(weight_matrix)
      
      grid_sf$mean_dist <- weighted_mean_dist
      grid_vect <- vect(grid_sf)
      r <- rasterize(grid_vect, r_template, field = "mean_dist", fun = NULL)
    }
    
    names(r) <- as.character(current_date)
    dist_stack <- c(dist_stack, r)
  }
  
  # Assign readable date labels
  names(dist_stack) <- format(as.Date(as.numeric(names(dist_stack)), origin = "1970-01-01"), "%Y-%m-%d")
  return(dist_stack)
}

test_wt <- dist_to_weighted_carcasses(carcasses_audited, bbox_south_big, 5000, start_date = "2023-01-01", end_date = "2023-04-01", decay_rate = 1, distance_power = 2)

png_files_wt <- get_pngs(test_wt)
imgs_wt <- image_read(png_files_wt)
animation <- image_animate(imgs_wt, fps = 5) 
animation

get_cell_vals_long <- function(stack){
  cell_coords <- map(1:(dim(stack)[1]*dim(stack)[2]), ~xyFromCell(stack, .x))
  pts <- map(cell_coords, ~vect(matrix(.x, ncol = 2), type = "points", crs = crs(stack)))
  ts <- map(pts, ~terra::extract(stack, .x))
  values <-  map(ts, ~as_tibble(as.numeric(.x[1, -1])))
  cell_values_long <- data.table::rbindlist(values, idcol = "cell")
  coords <- data.table::rbindlist(map(cell_coords, as_tibble), idcol = "cell")
  cell_values_long <- left_join(cell_values_long, coords, by = "cell") %>%
    group_by(cell) %>%
    mutate(date = lubridate::ymd(names(stack))) %>%
    ungroup()
  return(cell_values_long)
}

cell_values_long <- get_cell_vals_long(test_wt)

cell_values_long %>%
  ggplot(aes(x = date, y = value/1000, group = cell))+
  geom_line(alpha = 0.05)+
  theme_classic()+
  labs(y = "Weighted distance (km)",
       x = "Days",
       title = "Distance to active carcasses",
       caption = "Weighted distance takes into account the distance to all carcasses\nwith remaining weight <= 5kg. Carcass weight declines after placement.")

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

# Invert (so high = dark, low = light)
mean_inv <- 1 - mean_norm
var_inv  <- 1 - var_norm
# Convert to data frame
df <- as.data.frame(c(mean_inv, var_inv), xy = TRUE, na.rm = FALSE)
colnames(df)[3:4] <- c("mean", "variance")

# RGB hex color
df$hex <- rgb(df$mean, 0, df$variance)

# RGB composite plot
rgb_plot <- ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = hex)) +
  scale_fill_identity() +
  coord_equal() +
  labs(title = "Composite RGB: Mean (Red), Variance (Blue)") +
  theme_minimal()

# Mean legend plot (grayscale to red)
mean_plot <- ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = mean)) +
  scale_fill_gradient(low = "white", high = "red") +
  coord_equal() +
  labs(title = "Mean (red channel)") +
  theme_minimal()

# Variance legend plot (grayscale to blue)
var_plot <- ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = variance)) +
  scale_fill_gradient(low = "white", high = "blue") +
  coord_equal() +
  labs(title = "Variance (blue channel)") +
  theme_minimal()

# Combine with patchwork
(rgb_plot | (mean_plot / var_plot)) +
  plot_layout(widths = c(2, 1)) # eh, not sure how successful this is.

## Let's look at these patterns over a longer timescale--all of 2023
wt_2023_year <- dist_to_weighted_carcasses(carcasses_audited, bbox_south_big, 5000, start_date = "2023-01-01", end_date = "2023-12-31", decay_rate = 1, distance_power = 2)
imgs_wt_2023 <- image_read(get_pngs(wt_2023_year))
animation_2023 <- image_animate(imgs_wt_2023, fps = 5) 
animation_2023

# Carcass availability on the entire landscape over time
carcs_2023 <- carcasses_audited %>%
  select(date, carcassWeight) %>%
  filter(date >= lubridate::ymd("2023-01-01"), date <= lubridate::ymd("2023-12-31")) %>%
  mutate(date = as.Date(date))

cell_vals_long_2023 <- get_cell_vals_long(wt_2023_year)
dim(cell_vals_long_2023)
cell_vals_long_2023 %>%
  group_by(date) %>%
  summarize(mn = mean(value/1000),
            qtl05 = quantile(value/1000, .05),
            qtl95 = quantile(value/1000, .95)) %>%
  ggplot(aes(x = date))+
  geom_vline(data = carcs_2023, aes(xintercept = date), alpha = 0.1)+
  geom_line(aes(y = mn), col = "black")+
  geom_line(aes(y = qtl05), col = "skyblue")+
  geom_line(aes(y = qtl95), col = "darkorange")+
  theme_classic()+
  labs(title = "Weighted distance to carcasses, 2023",
       subtitle = "Southern region",
       y = "Weighted distance to active carcasses (km)",
       x = "Date",
       caption = "Carcass weights decline exponentially, rate = 1; Distance power = 2 (inverse square)\nBlack line = region-wide mean; Blue line = 5th percentile; Orange line = 95th percentile")
