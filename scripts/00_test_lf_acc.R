# Compare feeding bouts between high-frequency period and lower-frequency period
library(mapview)
library(targets)
library(sp)
library(terra)
library(raster)      # bridge between sp/adehabitatHR objects and terra
library(adehabitatHR)
library(sf)
library(tidyverse)

tar_load(feeding_bo_2023)
tar_load(feeding_bo_2023_lf)
tar_load(bbox_south_big)

length(feeding_bo_2023)
length(feeding_bo_2023_lf) # makes sense that these are the around the same length since they're still lists by individual

#XXX will need to update now that feeding_bo_2023_lf has many more data points
floor(map_dbl(feeding_bo_2023, nrow)/2) # dividing by two since this is a month of data vs. two weeks
map_dbl(feeding_bo_2023_lf, nrow) # there we go!! some, just way fewer, which is to be expected.

tar_load(full_2023)
tar_load(full_2023_lf)

bouts_2023 <- purrr::list_rbind(full_2023) %>%
  pivot_longer(cols = starts_with(".pred_"),
               names_to = "which_prob",
               values_to = "prob") %>%
  mutate(which_prob = str_remove(which_prob, ".pred_"))
bouts_2023_lf <- purrr::list_rbind(full_2023_lf) %>%
  pivot_longer(cols = starts_with(".pred_"),
               names_to = "which_prob",
               values_to = "prob") %>%
  mutate(which_prob = str_remove(which_prob, ".pred_"))

all <- bind_rows(
  bouts_2023 %>% mutate(period = "2023"),
  bouts_2023_lf %>% mutate(
    period = "2023_lf",
    across(where(~ is.integer(.x) || inherits(.x, "integer64")), as.numeric)
  )
)

all %>%
  filter(which_prob == pred) %>%
  ggplot(aes(x = prob, color = pred, fill = pred))+
  geom_density()+
  facet_wrap(~period) # luckily, I don't see any systemic differences in the classification probabilities of different behaviors between these two datasets.

all %>%
  filter(which_prob == pred, pred == "Eating") %>%
  ggplot(aes(x = prob, color = pred, fill = pred))+
  geom_density()+
  facet_wrap(~period)+ 
  geom_vline(aes(xintercept = 0.5), color = "blue", linetype = 2) # okay, so with an 0.5 threshold, we should still be getting quite a few bouts, which lines up with what we see above.

# Okay, so we know that this method can successfully identify and localize feeding bouts, just fewer of them. That's good! Some options:

# 1. Downsample the high-frequency bouts to get a comparable rate. What would the rate even be?
bouts_2023 <- map(feeding_bo_2023, ~select(.x, device_id, .pred_Eating, start)) %>% purrr::list_rbind() %>% mutate(start_date = lubridate::date(start)) %>% select(-start) %>% mutate(when = "2023")

bouts_2023_lf <- map(feeding_bo_2023_lf, ~select(.x, device_id, .pred_Eating, start)) %>% purrr::list_rbind() %>% mutate(start_date = lubridate::date(start)) %>% select(-start) %>% mutate(when = "2023_lf")

both <- bind_rows(bouts_2023, bouts_2023_lf)

summ_by_individual <- both %>%
  group_by(when, device_id) %>%
  summarize(n = n()) %>%
  pivot_wider(id_cols = "device_id", names_from = "when", values_from = "n") %>%
  rename("hf" = `2023`,
         "lf" = `2023_lf`) %>%
  mutate(floor_half_hf = floor(hf/2),
         ratio_lf_half = lf/floor_half_hf)

# What about spatial density?
# 1. Make three two-week rasters to look at the density of bouts over time. Normalize them. How similar? (I already suspect this is not going to work because the numbers are just so much lower, but who knows)

# XXX start here with this--some issues with joining
# I wonder if I could simply scale the number of bouts by the ACC frequency? Is it maybe that simple?
all <- all %>%
  mutate(period = case_when(lubridate::date(start) >= lubridate::ymd("2023-04-01") ~ 3,
                            lubridate::date(start) < lubridate::ymd("2023-03-15") ~ 1,
                            .default = 2))
table(all$period) # as expected, we see far more bouts in periods 2 and 3 than in period 1. All periods are two weeks long.

feeding_sf <- all %>% dplyr::select(bout_id, device_id, pred, start, end, location_long, location_lat, geometry, which_prob, prob, period) %>% sf::st_as_sf() %>%
  filter(which_prob == pred,
         pred == "Eating") %>%
  dplyr::select(-which_prob)

feeding_sf %>%
  filter(prob > 0.5) %>%
  ggplot(aes(color = prob))+
  geom_sf()+
  facet_wrap(~factor(period))+
  scale_color_viridis_c()+ # OH HECK YEAH!! This is awesome.
  labs(y = "Latitude", x = "Longitude",
       color = "P(Eating)")

# Some Claude-written, me-edited code for transforming these points into a raster

utm_crs <- 32636  # UTM 36N -- was referenced but never defined in the
# original script; needed by steps 4 and 6 below

# ---------------------------------------------------------------
# 1. Clean: drop empty geometries and missing prob values
#    (done on the FULL dataset, before splitting by period)
# ---------------------------------------------------------------
feeding_clean_all <- feeding_sf %>%
  filter(!sf::st_is_empty(geometry), !is.na(prob))

# ---------------------------------------------------------------
# 2. Reproject + clip the FULL dataset, THEN split by period.
#    Doing this before group_split() means the combined extent used
#    to build the shared grid below reflects your entire study area,
#    not just whichever period happens to be processed.
# ---------------------------------------------------------------
feeding_proj_all <- feeding_clean_all %>%
  st_transform(utm_crs) %>%
  st_filter(bbox_south_big, .predicate = st_intersects)

feeding_proj <- feeding_proj_all %>%
  group_by(period) %>%
  group_split()

table(feeding_proj_all$period) # how many do we have in each period?
# What proportion is that?
203/4663 # 4.35%
203/4131 # 4.9%

# Hmm, for simplicity maybe let's do 5%?

# Create 100 random subsets according to that proportion
random_subsets_p2 <- map(1:100, ~slice_sample(feeding_proj[[2]], prop = 0.05))
random_subsets_p3 <- map(1:100, ~slice_sample(feeding_proj[[3]], prop = 0.05))

# ---------------------------------------------------------------
# 2b. Build ONE shared grid, from the combined extent (bbox_south_big),
#    and pass it to every kernelUD() call below instead of a bare
#    number. A number (like grid = 100, extent = 1) re-derives BOTH
#    extent and cell size from whichever subset it's given -- that's
#    why each period's raster came out a different size. A shared
#    SpatialPixels object fixes extent and resolution across all of them.
# ---------------------------------------------------------------
build_grid <- function(bbox, cellsize, pad_frac = 0) {
  # pad_frac: optional extra buffer as a fraction of bbox width/height
  # on each side, mirroring what kernelUD's `extent` arg did before.
  # Leave at 0 if bbox_south_big already has margin around your points.
  xr <- bbox["xmax"] - bbox["xmin"]
  yr <- bbox["ymax"] - bbox["ymin"]
  
  x_seq <- seq(bbox["xmin"] - pad_frac * xr, bbox["xmax"] + pad_frac * xr, by = cellsize)
  y_seq <- seq(bbox["ymin"] - pad_frac * yr, bbox["ymax"] + pad_frac * yr, by = cellsize)
  
  grid_pts <- expand.grid(x = x_seq, y = y_seq)
  coordinates(grid_pts) <- ~x + y
  gridded(grid_pts) <- TRUE
  proj4string(grid_pts) <- CRS(paste0("EPSG:", utm_crs))
  grid_pts
}

cellsize <- 500  # meters per cell; match whatever resolution grid=100
# was effectively giving you, or pick fresh
common_grid <- build_grid(st_bbox(bbox_south_big), cellsize = cellsize)

# ---------------------------------------------------------------
# 3. Incorporate `prob` as a weight via point replication.
#    kernelUD() has no native `weights` argument, so we approximate
#    weighted KDE by duplicating each point proportional to its prob:
#    a point with prob = 0.9 contributes ~9x the density mass of a
#    point with prob = 0.1. This is a standard trick for getting
#    frequency-weighted kernel estimates out of adehabitatHR.
#
#    `scale_factor` sets the resolution of that weighting (and,
#    directly, how many total points kernelUD has to process --
#    check the sanity-check print below and adjust down if too slow).
# ---------------------------------------------------------------
scale_factor <- 10  # prob resolution ~= 1/scale_factor; raise for finer weighting

coords <- map(feeding_proj, st_coordinates)
coords_subsample_2 <- map(random_subsets_p2, st_coordinates)
coords_subsample_3 <- map(random_subsets_p3, st_coordinates)
n_rep  <- map(feeding_proj, ~{round(.x$prob * scale_factor)})
n_rep_subsample_2  <- map(random_subsets_p2, ~{round(.x$prob * scale_factor)})
n_rep_subsample_3  <- map(random_subsets_p3, ~{round(.x$prob * scale_factor)})
keep   <- map(n_rep, ~.x > 0)
keep_subsample_2   <- map(n_rep_subsample_2, ~.x > 0)
keep_subsample_3   <- map(n_rep_subsample_3, ~.x > 0)

rep_idx    <- map2(keep, n_rep, ~{rep(which(.x), times = .y[.x])})
rep_idx_subsample_2    <- map2(keep_subsample_2, n_rep_subsample_2, ~{rep(which(.x), times = .y[.x])})
rep_idx_subsample_3    <- map2(keep_subsample_3, n_rep_subsample_3, ~{rep(which(.x), times = .y[.x])})

coords_rep <- map2(rep_idx, coords, ~{.y[.x, , drop = FALSE]})
coords_rep_subsample_2 <- map2(rep_idx_subsample_2, coords_subsample_2, ~{.y[.x, , drop = FALSE]})
coords_rep_subsample_3 <- map2(rep_idx_subsample_3, coords_subsample_3, ~{.y[.x, , drop = FALSE]})


map_dbl(coords_rep, nrow)  # sanity check before running kernelUD on this many points
map_dbl(coords_rep_subsample_2, nrow)
map_dbl(coords_rep_subsample_3, nrow)

# ---------------------------------------------------------------
# 4. Build sp::SpatialPoints (what kernelUD expects)
# ---------------------------------------------------------------
xy <- map(coords_rep, ~{SpatialPoints(.x, proj4string = CRS(paste0("EPSG:", utm_crs)))})

xy_subsample_2 <- map(coords_rep_subsample_2, ~{SpatialPoints(.x, proj4string = CRS(paste0("EPSG:", utm_crs)))})
xy_subsample_3 <- map(coords_rep_subsample_3, ~{SpatialPoints(.x, proj4string = CRS(paste0("EPSG:", utm_crs)))})

# ---------------------------------------------------------------
# 5. Kernel UD estimation.
#    h is already fixed at 500 across all periods here (good -- this
#    was already correct in the original script). The change is
#    `grid = common_grid` in place of `grid = 100, extent = 1`, so
#    every period is evaluated on the identical spatial grid.
# ---------------------------------------------------------------
# kud <- map(xy, ~{kernelUD(.x, h = 2000, grid = common_grid)}, .progress = T)
# #map_dbl(kud, ~.x@h$h)  # inspect the bandwidth actually used
# 
# kud_subsample_2 <- map(xy_subsample_2, ~{kernelUD(.x, h = 2000, grid = common_grid)}, .progress = T)
# 
# kud_subsample_3 <- map(xy_subsample_3, ~{kernelUD(.x, h = 2000, grid = common_grid)}, .progress = T)
# 
# write_rds(kud, file = "data/created/kud.RDS")
# write_rds(kud_subsample_2, file = "data/created/kud_subsample_2.RDS")
# write_rds(kud_subsample_3, file = "data/created/kud_subsample_3.RDS")

kud <- readRDS("data/created/kud.RDS")
kud_subsample_2 <- readRDS("data/created/kud_subsample_2.RDS")
kud_subsample_3 <- readRDS("data/created/kud_subsample_3.RDS")

# ---------------------------------------------------------------
# 6. Convert to a terra SpatRaster
#    (estUD extends SpatialPixelsDataFrame -> raster::raster() -> terra)
#    Using map() here instead of walk() -- walk() returns its input
#    unmodified; the original code only worked because crs<- mutates
#    the SpatRaster's underlying C++ object in place. map() is the
#    correct/explicit way to do this reassignment.
# ---------------------------------------------------------------
r <- map(kud, ~rast(raster(.x)))
r <- map(r, ~{crs(.x) <- paste0("EPSG:", utm_crs); .x})

r_subsample_2 <- map(kud_subsample_2, ~rast(raster(.x)))
r_subsample_2 <- map(r_subsample_2, ~{crs(.x) <- paste0("EPSG:", utm_crs); .x})

r_subsample_3 <- map(kud_subsample_3, ~rast(raster(.x)))
r_subsample_3 <- map(r_subsample_3, ~{crs(.x) <- paste0("EPSG:", utm_crs); .x})

# ---------------------------------------------------------------
# 7. Normalize to sum to 1
#    kernelUD's UD is already a probability density (it integrates
#    to ~1 by construction), but re-normalizing over the actual
#    output grid guarantees exact sum-to-1 comparability across
#    periods -- and now that extent + cell size are identical for
#    all three (step 2b), that comparison is actually valid.
# ---------------------------------------------------------------
total  <- map(r, ~global(.x, "sum", na.rm = TRUE)[1, 1])
r_norm <- map2(r, total, ~.x / .y)

total_subsample_2  <- map(r_subsample_2, ~global(.x, "sum", na.rm = TRUE)[1, 1])
r_norm_subsample_2 <- map2(r_subsample_2, total_subsample_2, ~.x / .y)

total_subsample_3  <- map(r_subsample_3, ~global(.x, "sum", na.rm = TRUE)[1, 1])
r_norm_subsample_3 <- map2(r_subsample_3, total_subsample_3, ~.x / .y)

# Compare periods 2 and 3 normalized (full) to distributions of rasters from their own periods but subsampled [this is the most direct comparison]
# Comparison 2: r_norm[[2]] vs. r_norm_subsample_2
length(r_norm[[2]]) # 1 vs 100
length(r_norm_subsample_2)
plot(r_norm[[2]])
plot(r_norm_subsample_2[[1]])

cors_2 <- map_dbl(r_norm_subsample_2, ~cor(values(.x), values(r_norm[[2]]), use = "complete.obs"))
cors_3 <- map_dbl(r_norm_subsample_3, ~cor(values(.x), values(r_norm[[3]]), use = "complete.obs"))

cors_df <- data.frame(period = rep(c(2, 3), each = 100),
                      cor = c(cors_2, cors_3)) %>%
  mutate(period = factor(period))
cors_df %>%
  ggplot(aes(x = cor, fill = period, color = period))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  ggtitle("Subsampling feeding bouts represents their full distribution very well")

# Compare period 1 normalized to distributions of period 2 and period 3 subsampled
## Expect: lower correlation than comparing periods to themselves, but still similar, since it's only a <1mo difference
cors_1_2 <- map_dbl(r_norm_subsample_2, ~cor(values(.x), values(r_norm[[1]]), use = "complete.obs"))
cors_1_3 <- map_dbl(r_norm_subsample_3, ~cor(values(.x), values(r_norm[[1]]), use = "complete.obs"))

cors_df_crossperiod <- data.frame(period = rep(c(2, 3), each = 100),
                      cor = c(cors_1_2, cors_1_3)) %>%
  mutate(comparison_period = factor(period))

cors_df_crossperiod %>%
  ggplot(aes(x = cor, fill = comparison_period, color = comparison_period))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  ggtitle("Period 1 is much less similar to subsamples of periods 2 and 3")# wow, much lower correlation between periods. This doesn't necessarily mean anything bad, since we want to aggregate over a longer period of time anyway, but it's good to know.

# Compare normalized rasters derived from period 1 (LF, two weeks) to period 2 (HF, two weeks) and period 3 (HF, two weeks)
cor(values(r_norm[[1]]), values(r_norm[[2]])) # 65%, which is also the average of comparing period 1 to the subsamples of period 2
cor(values(r_norm[[1]]), values(r_norm[[3]])) # 70%, which is the average of comparing period 1 to the subsamples of period 3
# All of this is as it should be.

# Compare raster derived from carcasses to raster derived from feeding bouts, since the two are not the same
tar_load(all_carcasses)
dim(all_carcasses)

# Clean
all_carcasses <- all_carcasses %>%
  filter(!sf::st_is_empty(geometry))

# 2. Reproject + clip # XXX start here
carcasses_focal_proj <- all_carcasses %>%
  st_transform(utm_crs) %>%
  st_filter(bbox_south_big, .predicate = st_intersects) %>%
  mutate(year = lubridate::year(datetime_il)) %>%
  mutate(period = case_when(date >= lubridate::ymd("2023-03-15", tz = "UTC") & date <= lubridate::ymd("2023-03-31", tz = "UTC") ~ 2,
                            date >= lubridate::ymd("2023-04-01", tz = "UTC") & date <= lubridate::ymd("2023-04-15", tz = "UTC") ~ 3,
                            .default = NA))

carcasses_focal_proj_split <- carcasses_focal_proj %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  group_split()

table(carcasses_focal_proj$period) # how many do we have in each period

# grid: we can use the same common grid from above
# no need for replications--we don't have weight values for the carcasses
coords_carcs <- map(carcasses_focal_proj_split, st_coordinates)

# build spatialpoints for kud
xy_carcs <- map(coords_carcs, ~{SpatialPoints(.x, proj4string = CRS(paste0("EPSG:", utm_crs)))})

# kud with same grid as previous
kud_carcs <- map(xy_carcs, ~{kernelUD(.x, h = 2000, grid = common_grid)}, .progress = T)

write_rds(kud_carcs, file = "data/created/kud_carcs.RDS")

kud_carcasses <- readRDS("data/created/kud_carcs.RDS")

# convert to terra spatraster
r_carcs <- map(kud_carcs, ~rast(raster(.x)))
r_carcs <- map(r_carcs, ~{crs(.x) <- paste0("EPSG:", utm_crs); .x})

# normalize
total_carcs  <- map(r_carcs, ~global(.x, "sum", na.rm = TRUE)[1, 1])
r_norm_carcs <- map2(r_carcs, total_carcs, ~.x / .y)

plot(r_norm_carcs[[1]]) # okay, this looks pretty different from our raster derived from feeding bouts
plot(r_norm[[2]]) # feeding bouts give a pale comparison

# but how correlated are they?
cor(values(r_norm_carcs[[1]]), values(r_norm[[2]])) # oof, yeah, that's not great.

plot(r_norm_carcs[[2]])
plot(r_norm[[3]]) # looks very different!
cor(values(r_norm_carcs[[2]]), values(r_norm[[3]])) # very bad

# okay so we could change the scaling of the carcasses, but the fact remains that these are not very correlated after they've both been normalized, which I don't love. What are the feeding bouts capturing if not the carcasses?
