# Example workflow for Shaked--classifying vulture landings from movement data
# 2025-07-04
library(here) # for tidy file paths
library(readr) # for loading in files
library(readxl) # for reading in excel files (feeding station info)
library(dplyr) # for data wrangling
library(stringr) # for data wrangling
library(sf) # for spatial computations
library(terra) # for raster computations (DEMs)
library(mapview) # for visualizations

# 1. Load data
## some example GPS data from 2022. Substitute yours.
gps <- readr::read_csv(here("example_gps_2022.csv")) 
## a bounding box I created for the southern region, somewhat arbitrary. Use a different one if you want.
bbox_south_big <- readr::read_rds(here("bbox_south_big.RDS"))
## loading and processing feeding station data. It was pretty messy--maybe you have a tidier dataset?
# Load and clean feeding station info
north <- readxl::read_excel(here("feeding_stations_north.xlsx"), sheet = 2) %>%
  setNames(c("stationName", "region", "itmLong", "itmLat", "lat", "long"))
south <- readxl::read_excel(here("feeding_station_south_coordinates.xlsx")) %>%
  setNames(c("stationName", "active", "itmLong", "itmLat", "lat", "long"))
stations <- bind_rows(north, south) %>%
  dplyr::mutate(lat = stringr::str_remove_all(lat, "\\s"), # tidy up the coords
         long = stringr::str_remove_all(long, "\\s"),
         across(c("long", "lat"), as.numeric)) %>%
  sf::st_as_sf(coords = c("long", "lat"), remove = F, crs = "WGS84") %>% # make into an sf object
  sf::st_transform(32636) # transform to UTM

# 2. Get landings by ground speed.
## Note: between our work and Gideon's, we've variously used either 4 m/s or 5 m/s. Pick whatever seems reasonable to you.
gps_spd <- 4 # m/s threshold
landings <- gps %>% 
  dplyr::filter(ground_speed <= gps_spd)
## transform to an sf object. I'm using EPSG 32636, which is the code for the UTM region that covers Israel, to project the spatial objects into UTM coordinates so everything will be in units of meters.
landings_sf <- sf::st_as_sf(landings, coords = c("location_long", "location_lat"), 
                            crs = "WGS84", remove = FALSE) %>% 
  sf::st_transform(32636)

# 3. Classify as on/off cliffs
## Note: this shapefile isn't comprehensive; if you view it e.g. in Google Earth, you can see that there are some cliffs that seem to not be included. For my bouts, I'm considering using a DEM to classify them by slope instead of, or in addition to, this shapefile. For the sake of this script, I'll show you how to do all of it, and you can include whichever parts make sense to you.
cliffs <- sf::st_read(here("BNTL202203_Cliff/")) 
cliffs_utm <- sf::st_transform(cliffs, 32636) # Transform to UTM
cliffs_buffer_m <- 100 # set buffer distance to use, in meters. The cliffs are encoded in the shapefile as lines, so in order to classify stationary points as on/off cliffs, we need to buffer the lines out to polygons so we can see if the points fall inside the polygons. I chose 100m arbitrarily; feel free to change.
cliffs_buffered <- sf::st_buffer(cliffs_utm, cliffs_buffer_m) # apply the buffer
cliffs_buffered_multipolygon <- sf::st_union(cliffs_buffered) # merge into a single multipolygon, since we don't care *which* cliff the points are on, just whether or not they're on a cliff
on_cliff <- sf::st_intersects(landings_sf, cliffs_buffered_multipolygon, sparse = F)[,1]

landings_sf$on_cliff <- on_cliff # add cliff info to the landings gps dataset

# 4. Get ground slope values for each landing point
## Get all DEM files for Israel, including some extra ones. We'll pare the list down later.
dem_files <- list.files(here("DEMs/ASTER/"), pattern = ".tif", full.names = T)
demlist <- vector(mode = "list", length = length(dem_files))
for(i in 1:length(demlist)){
  demlist[[i]] <- terra::rast(dem_files[i])
} 
## Prepare bounding box by transforming to vector object
bbox_vect <- terra::vect(st_transform(bbox_south_big, "WGS84")) 
## crop DEM tiles to bounding box
cropped_list <- map(demlist, function(r) { 
  tryCatch({
    crop(r, bbox_vect)
  }, error = function(e) {
    message("Error cropping raster: ", e$message)
    return(NULL) # Return NULL if an error occurs--will return NULL if that tile doesn't overlap with the bounding box. This is normal.
  })
})
## Keep only the tiles that overlap with our bounding box
filtered_list <- cropped_list[!sapply(cropped_list, is.null)]
## Join cropped tiles into a single DEM for our bounding box area
merged_raster <- Reduce(f = merge, x =filtered_list)
## create a raster of slope values from the DEM
slope_raster <- terra::terrain(merged_raster, v = "slope", unit = "degrees", neighbors = 8)
slope_proj <- terra::project(slope_raster, "epsg:32636") # project to same coords
## Now turn the gps points into a vector object so we can extract slope values for them
landings_vect <- terra::vect(landings_sf)
slopes <- terra::extract(slope_proj, landings_vect)

landings_sf$slope <- slopes$slope # add slopes to the landings gps dataset 

# 5. Do points fall within feeding stations?
## Chooose buffer radius for feeding stations. Here I chose 100m; choose something else if you prefer.
fs_buffer_m <- 100 # in meters
stations_buffered <- sf::st_buffer(stations, fs_buffer_m) # buffer feeding stations to polygons
## get intersections for each point
in_station <- purrr::map_lgl(sf::st_intersects(landings_sf, stations_buffered), ~length(.x) >= 1) 

landings_sf$in_station <- in_station # add station intersections to the landings gps dataset 

# 6. Examine results
glimpse(landings_sf)
table(landings_sf$on_cliff)
hist(landings_sf$slope)
table(landings_sf$in_station)

## My favorite way to quickly visualize geographic information is by using mapview()
mapview(landings_sf) # looks like I didn't restrict this by geography--should probably do that!
landings_sf_cropped <- sf::st_crop(landings_sf, bbox_south_big) # only works since they have the same CRS
mapview(landings_sf_cropped)

## color by station
mapview(landings_sf_cropped, zcol = "in_station")

## color by slope
mapview(landings_sf_cropped, zcol = "slope")

## color by cliff
mapview(landings_sf_cropped, zcol = "on_cliff")

# I hope this helps!! Let me know if you have any questions about it :) -Kaija