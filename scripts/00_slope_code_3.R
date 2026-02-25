# ==============================================================================
# BATCH CLIFF DETECTION (140 TILES - NO CRS TRANSFORM)
# ==============================================================================

library(parallel)
library(terra)

# ------------------------------------------------------------------------------
# 1. SETUP
# ------------------------------------------------------------------------------
dem_folder      <- here("data/raw/DEMs_Shaked/")
output_folder   <- here("data/created/DEMs_Shaked/final_25deg_100buff/")
cliff_threshold <- 25      # Slope degrees
buffer_dist     <- 100     # Buffer distance (same units as input DEM, usually meters)
num_cores       <- (detectCores()-10)       # Adjust based on your CPU

# Get list of files
dem_files <- list.files(dem_folder, pattern = "\\.(hgt|tif|HgT)$", full.names = TRUE)
message(paste("Found", length(dem_files), "tiles to process."))

# ------------------------------------------------------------------------------
# 2. WORKER FUNCTION (Process, Buffer, Save)
# ------------------------------------------------------------------------------
process_and_save <- function(file_path, out_dir, threshold, buf_dist) {
  
  library(terra)
  
  # A. Define Output Filename
  f_name <- basename(file_path)
  out_name <- paste0("buff_", tools::file_path_sans_ext(f_name), ".gpkg")
  out_path <- file.path(out_dir, out_name)
  
  # Skip if already exists (resume capability)
  if(file.exists(out_path)) return(NULL)
  
  # B. Load & Analyze
  r <- terra::rast(file_path)
  
  # Calculate slope (terra handles units automatically based on CRS)
  slope <- terra::terrain(r, v = 'slope', unit = 'degrees')
  
  # Mask
  mask <- slope > threshold
  mask <- terra::subst(mask, 0, NA)
  
  # If empty, stop
  if(all(is.na(values(mask)))) return(NULL)
  
  # C. Vectorize (Dissolve adjacent cells)
  v <- terra::as.polygons(mask, aggregate = TRUE, na.rm = TRUE)
  
  # D. Buffer (Directly on the original geometry)
  # Uses the CRS of the input DEM
  v_buf <- terra::buffer(v, width = buf_dist)

  # E. Save to Disk
  terra::writeVector(v_buf, out_path, overwrite = TRUE)
  
  return(out_path)
}

# ------------------------------------------------------------------------------
# 3. RUN BATCH JOB
# ------------------------------------------------------------------------------
cl <- makeCluster(num_cores)

# Export variables (Removed target_crs)
clusterExport(cl, varlist = c("output_folder", "cliff_threshold", "buffer_dist", "process_and_save"))

message("Starting batch processing...")

# Run Parallel Loop
results <- parLapply(cl, dem_files, function(f) {
  process_and_save(f, output_folder, cliff_threshold, buffer_dist)
})

stopCluster(cl)

message("✅ Batch processing complete. Results saved in CLIFF_OUTPUTS folder.")


cliff_files_100m_buffer_25d <- list.files(output_folder, pattern = "\\.gpkg$", full.names = TRUE)
message(paste("Generated", length(cliff_files_100m_buffer_25d), "cliff buffer files."))
saveRDS(cliff_files_100m_buffer_25d, "data/created/DEMs_Shaked/final_25deg_100buff/cliff_files_100m_buffer_25d.rds")

# Test the files
library(mapview)
g <- st_read(here("data/created/DEMs_Shaked/final_25deg_100buff/buff_N30E034.gpkg"))
mapview(g)