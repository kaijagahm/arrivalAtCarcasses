# Testing out DEMs
library(sf)
library(terra)
library(stars)
library(here)
library(tidyverse)
library(targets)
library(mapview)

tar_load(feeding_bouts_stationary)
tar_load(bbox_south_new)
tar_load(cliffs_buffered)
mapview(bbox_south_new)+mapview(feeding_bouts_stationary)
bbox_vect <- vect(st_transform(st_as_sfc(bbox_south_new), "WGS84"))

filenames <- list.files(here("data/raw/DEMs/ASTER/"), pattern = ".tif", full.names = T)
demlist <- vector(mode = "list", length = length(filenames))
for(i in 1:length(demlist)){
  demlist[[i]] <- rast(filenames[i])
}

cropped_list <- map(demlist, function(r) {
  tryCatch({
    crop(r, bbox_vect)
  }, error = function(e) {
    message("Error cropping raster: ", e$message)
    return(NULL) # Return NULL if an error occurs
  })
})

filtered_list <- cropped_list[!sapply(cropped_list, is.null)]

merged_raster <- Reduce(f = merge, x = filtered_list)
plot(merged_raster)

# get terrain
terr <- terrain(merged_raster, v = "slope", unit = "degrees", neighbors = 4)
plot(terr)
terr_proj <- project(terr, "epsg:32636")

bouts_vect <- vect(feeding_bouts_stationary)
slopes <- terra::extract(terr_proj, bouts_vect)
feeding_bouts_stationary <- feeding_bouts_stationary %>%
  mutate(slope = slopes$slope)
mapview(cliffs_buffered) + mapview(feeding_bouts_stationary, zcol = "slope") # this does not help me get a clear sense of where I should put the slope cutoff

hist(feeding_bouts_stationary$slope, breaks = 20) # maybe 15 is a good cutoff? Or 10? Let's do 15 for now and see how that goes.

tar_load(wild_carcasses_15)
tar_load(wild_carcasses_10)
tar_load(wild_carcasses_5)
mapview(wild_carcasses_15, col.regions = "yellow", alpha.regions = 1)+ mapview(wild_carcasses_10, col.regions = "orange", alpha.regions = 1) + mapview(wild_carcasses_5, col.regions = "red", alpha.regions = 1)
