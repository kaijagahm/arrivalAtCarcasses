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
  bouts_2023 %>% mutate(period = "2023",
                        across(where(~ is.integer(.x) || inherits(.x, "integer64")), as.numeric)),
  bouts_2023_lf %>% mutate(
    period = "2023_lf",
    across(where(~ is.integer(.x) || inherits(.x, "integer64")), as.numeric)
  )
) %>% mutate(pd = case_when(period == "2023" ~ "HF ACC",
                            period == "2023_lf" ~ "LF ACC",
                            .default = NA)) %>%
  sf::st_as_sf(crs = "WGS84") %>%
  sf::st_transform(32636) %>%
  st_crop(., bbox_south_big)

all %>%
  filter(which_prob == pred) %>%
  ggplot(aes(x = prob, color = pred, fill = pred))+
  geom_density(alpha = 0.7)+ theme_bw()+
  facet_wrap(~pd*pred, scales = "free_y", nrow = 2, ncol = 6)+ # luckily, I don't see any systemic differences in the classification probabilities of different behaviors between these two datasets.
  labs(y = "Density", x = "Classification probability")+
  theme(legend.position = "none", text = element_text(size = 18))

all %>%
  filter(which_prob == pred, pred == "Eating") %>%
  ggplot(aes(x = prob, color = pred, fill = pred))+
  geom_density()+
  facet_wrap(~pd)+ 
  geom_vline(aes(xintercept = 0.5), color = "blue", linetype = 2)+ # okay, so with an 0.5 threshold, we should still be getting quite a few bouts, which lines up with what we see above.
  theme_bw()+
  theme(text = element_text(size = 18))+
  labs(x = "Classification probability", y = "Density")

# Okay, so we know that this method can successfully identify and localize feeding bouts, just fewer of them. That's good! Some options:

# 1. Downsample the high-frequency bouts to get a comparable rate. What would the rate even be?
bouts_2023 <- map(feeding_bo_2023, ~dplyr::select(.x, device_id, .pred_Eating, start)) %>% purrr::list_rbind() %>% mutate(start_date = lubridate::date(start)) %>% dplyr::select(-start) %>% mutate(when = "2023")

bouts_2023_lf <- map(feeding_bo_2023_lf, ~dplyr::select(.x, device_id, .pred_Eating, start)) %>% purrr::list_rbind() %>% mutate(start_date = lubridate::date(start)) %>% dplyr::select(-start) %>% mutate(when = "2023_lf")

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
  mutate(pd = case_when(period == 1 ~ "Period 1 (LF)",
                        period == 2 ~ "Period 2 (HF)",
                        period == 3 ~ "Period 3 (HF)")) %>%
  filter(prob > 0.5) %>%
  ggplot(aes(color = prob))+
  geom_sf(pch = 1, size = 3, alpha = 0.7)+
  facet_wrap(~factor(pd))+
  scale_color_viridis_c()+
  labs(y = "Latitude", x = "Longitude",
       color = "P(Eating)")+
  theme_bw()+
  theme(legend.position = "bottom")

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
# n_rep  <- map(feeding_proj, ~{round(.x$prob * scale_factor)})
# n_rep_subsample_2  <- map(random_subsets_p2, ~{round(.x$prob * scale_factor)})
# n_rep_subsample_3  <- map(random_subsets_p3, ~{round(.x$prob * scale_factor)})
# keep   <- map(n_rep, ~.x > 0)
# keep_subsample_2   <- map(n_rep_subsample_2, ~.x > 0)
# keep_subsample_3   <- map(n_rep_subsample_3, ~.x > 0)
# 
# rep_idx    <- map2(keep, n_rep, ~{rep(which(.x), times = .y[.x])})
# rep_idx_subsample_2    <- map2(keep_subsample_2, n_rep_subsample_2, ~{rep(which(.x), times = .y[.x])})
# rep_idx_subsample_3    <- map2(keep_subsample_3, n_rep_subsample_3, ~{rep(which(.x), times = .y[.x])})
# 
# coords_rep <- map2(rep_idx, coords, ~{.y[.x, , drop = FALSE]})
# coords_rep_subsample_2 <- map2(rep_idx_subsample_2, coords_subsample_2, ~{.y[.x, , drop = FALSE]})
# coords_rep_subsample_3 <- map2(rep_idx_subsample_3, coords_subsample_3, ~{.y[.x, , drop = FALSE]})
# 
# 
# map_dbl(coords_rep, nrow)  # sanity check before running kernelUD on this many points
# map_dbl(coords_rep_subsample_2, nrow)
# map_dbl(coords_rep_subsample_3, nrow)

# ---------------------------------------------------------------
# 4. Build sp::SpatialPoints (what kernelUD expects)
# ---------------------------------------------------------------
xy <- map(coords, ~{SpatialPoints(.x, proj4string = CRS(paste0("EPSG:", utm_crs)))})

xy_subsample_2 <- map(coords_subsample_2, ~{SpatialPoints(.x, proj4string = CRS(paste0("EPSG:", utm_crs)))})
xy_subsample_3 <- map(coords_subsample_3, ~{SpatialPoints(.x, proj4string = CRS(paste0("EPSG:", utm_crs)))})

# ---------------------------------------------------------------
# 5. Kernel UD estimation.
#    h is already fixed at 500 across all periods here (good -- this
#    was already correct in the original script). The change is
#    `grid = common_grid` in place of `grid = 100, extent = 1`, so
#    every period is evaluated on the identical spatial grid.
# ---------------------------------------------------------------
kud <- map(xy, ~{kernelUD(.x, h = 2000, grid = common_grid)}, .progress = T)
#map_dbl(kud, ~.x@h$h)  # inspect the bandwidth actually used

kud_subsample_2 <- map(xy_subsample_2, ~{kernelUD(.x, h = 2000, grid = common_grid)}, .progress = T)

kud_subsample_3 <- map(xy_subsample_3, ~{kernelUD(.x, h = 2000, grid = common_grid)}, .progress = T)

write_rds(kud, file = "data/created/kud.RDS")
write_rds(kud_subsample_2, file = "data/created/kud_subsample_2.RDS")
write_rds(kud_subsample_3, file = "data/created/kud_subsample_3.RDS")

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

plot(r_norm[[1]])

cors_2 <- map_dbl(r_norm_subsample_2, ~cor(values(.x), values(r_norm[[2]]), use = "complete.obs"))
cors_3 <- map_dbl(r_norm_subsample_3, ~cor(values(.x), values(r_norm[[3]]), use = "complete.obs"))

cors_df <- data.frame(period = rep(c(2, 3), each = 100),
                      cor = c(cors_2, cors_3)) %>%
  mutate(period = factor(period))
cors_df %>%
  ggplot(aes(x = cor, fill = period, color = period))+
  geom_density(alpha = 0.5)+
  theme_minimal()+
  labs(x = "Correlation (subsample vs. full)", y = "Density", caption = "Correlation between normalized KUDs created from a) all feeding bouts (high-frequency ACC) vs. b) 100 5% subsamples from the same period.")+
  theme(text = element_text(size = 18), plot.caption = element_text(size = 10, hjust = 1))
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
r_carcs <- map(kud_carcasses, ~rast(raster(.x)))
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

# For each (buffered) carcass in periods 2 and 3, check relationship to (subset of) feeding bouts
# Need the following:
# Buffered carcasses (split into periods 2 and 3)
buff <- readRDS("data/created/carcs_buffered.RDS")
buff_period2 <- buff %>%
  filter(end_date >= range(feeding_proj[[2]]$start)[1],
         date <= range(feeding_proj[[2]]$start)[2])

buff_period3 <- buff %>%
  filter(end_date >= range(feeding_proj[[3]]$start)[1],
         date <= range(feeding_proj[[3]]$start)[2])

# Join these together by space and time
join_space_time <- function(polys, pts) {
  
  # Unique ID per polygon row (safer than relying on carcID)
  polys <- polys |> mutate(poly_id = row_number())
  
  # 1. Spatial candidate pairs (point falls within / touches polygon)
  hits <- st_intersects(polys, pts)
  
  pairs <- tibble(
    poly_id   = rep(seq_along(hits), lengths(hits)),
    point_idx = unlist(hits)
  )
  
  # 2. Keep pairs where the point's start time is within the polygon's time range
  end_bound <- polys$end_date[pairs$poly_id]
  
  pairs <- pairs |>
    mutate(
      poly_start = polys$date[poly_id],
      poly_end   = end_bound,
      pt_start   = pts$start[point_idx]
    ) |>
    filter(
      pt_start >= poly_start,
      pt_start <= poly_end
    ) |>
    dplyr::select(poly_id, point_idx)
  
  # 3. Matched rows: polygon attributes + point attributes (point geometry dropped)
  matched <- bind_cols(
    polys[pairs$poly_id, ],
    st_drop_geometry(pts)[pairs$point_idx, ]
  )
  
  # 4. Unmatched polygons: one row each, point columns become NA
  unmatched <- polys |> filter(!poly_id %in% pairs$poly_id)
  
  # 5. Combine (bind_rows fills missing point columns with NA)
  bind_rows(matched, unmatched) |>
    arrange(poly_id) |>
    dplyr::select(-poly_id)
}

# Apply to all 100 subsets, stacking results with a subset identifier
results_all_p2 <- imap(
  random_subsets_p2,
  \(pts, i) join_space_time(buff_period2, pts) |>
    mutate(subset = i, .before = 1)
) |>
  bind_rows() |>
  ungroup() |>
  mutate(bout_id_unique = if_else(is.na(bout_id), NA_character_,
                                  paste(bout_id, device_id, sep = ".")))

results_all_p3 <- imap(
  random_subsets_p3,
  \(pts, i) join_space_time(buff_period3, pts) |>
    mutate(subset = i, .before = 1)
) |>
  bind_rows() |>
  ungroup() |>
  mutate(bout_id_unique = if_else(is.na(bout_id), NA_character_,
                                  paste(bout_id, device_id, sep = ".")))

summ_p2 <- results_all_p2 |>
  st_drop_geometry() |>
  group_by(carcID, carcType, subset) |>
  summarize(n_bouts = n_distinct(bout_id_unique, na.rm = TRUE), .groups = "drop") |>
  mutate(period = 2)

summ_p3 <- results_all_p3 |>
  st_drop_geometry() |>
  group_by(carcID, carcType, subset) |>
  summarize(n_bouts = n_distinct(bout_id_unique, na.rm = TRUE), .groups = "drop") |>
  mutate(period = 3)

summ <- bind_rows(summ_p2, summ_p3) |>
  mutate(carcID = factor(carcID))

summ |> filter(n_bouts == 0)

summ %>%
  mutate(period = paste("Period", period, sep = " ")) %>%
  ggplot(aes(x = carcID, y = n_bouts))+
  geom_boxplot(outlier.shape = NA, aes(fill = carcType))+
  theme_minimal()+
  facet_wrap(~period*carcType, scales = "free_x")+ 
  theme(legend.position = "none", axis.text.x = element_blank(), text = element_text(size = 18))+
  geom_hline(aes(yintercept = 0), color = "gold")+
  labs(y = "# feeding bouts within 8km", x = "Carcass") # some carcasses are getting missed, but not most of them!

summ %>%
  filter(n_bouts <= 10) %>%
  mutate(period = paste("Period", period, sep = " ")) %>%
  ggplot(aes(x = carcID, y = n_bouts))+
  geom_boxplot(outlier.shape = NA, aes(fill = carcType))+
  theme_minimal()+
  facet_wrap(~period*carcType, scales = "free_x")+ 
  theme(legend.position = "none", axis.text.x = element_blank(), text = element_text(size = 18))+
  geom_hline(aes(yintercept = 0), color = "gold")+
  labs(y = "# feeding bouts within 8km", x = "Carcass")

# For each carcass: in what proportion of the 100 subsets does it get zero bouts?
## Can think of this as a proxy for the likelihood of missing the carcass
summ %>%
  st_drop_geometry() %>%
  group_by(period, carcID, carcType) %>%
  summarize(prop_subsets_zero = mean(n_bouts == 0), .groups = "drop") %>%
  arrange(desc(prop_subsets_zero)) %>%
  print(n = 20) %>%
  ggplot(aes(x = prop_subsets_zero*100, fill = carcType))+
  geom_histogram(breaks = seq(from = 0, to = 100, by = 10))+
  facet_wrap(~factor(period)*carcType)+
  theme_bw()+
  geom_vline(aes(xintercept = 50), color = "gold", lwd = 1.5)+
  theme(legend.position = "none", text = element_text(size = 18))+
  labs(y = "# Carcasses", x = "% chance of missing carcass with LF ACC")

# Identifying carcasses by landings instead of feeding bouts --------------
tar_load(downsampled)
downsampled <- st_crop(downsampled, bbox_south_big)
gps_p1 <- downsampled %>% filter(date_il >= lubridate::ymd("2023-03-01") & date_il < lubridate::ymd("2023-03-15"))
gps_p2 <- downsampled %>% filter(date_il >= lubridate::ymd("2023-03-15") & date_il < lubridate::ymd("2023-03-31"))
gps_p3 <- downsampled %>% filter(date_il >= lubridate::ymd("2023-03-31") & date_il < lubridate::ymd("2023-04-15"))
gpses <- list(gps_p1, gps_p2, gps_p3)

rm(downsampled)

stationaries <- map(gpses, ~filter(.x, ground_speed < 5))
map_dbl(stationaries, nrow) #[1] 39630 52760 53018
stationaries_sf <- purrr::map(stationaries, ~sf::st_as_sf(.x, crs = 32636))
tar_load(slopes_layer)
stationaries_sf_nocliffs <- purrr::map(stationaries_sf, ~{
  keep <- sf::st_intersects(.x, slopes_layer) %>% as.numeric() %>% is.na()
  return(.x[keep,])
})
map_dbl(stationaries_sf_nocliffs, nrow) # [1] 5381 8446 5262

mapview(stationaries_sf_nocliffs[[1]])

# Let's see how the landings correspond to carcasses
buff_p2 <- buff %>%
  filter(end_date >= lubridate::ymd("2023-03-15") & date < lubridate::ymd("2023-03-31"))
buff_p3 <- buff %>%
  filter(end_date >= lubridate::ymd("2023-03-31") & date < lubridate::ymd("2023-04-15"))
stationaries_sf_nocliffs <- map(stationaries_sf_nocliffs, ~{.x <- mutate(.x, start = timestamp_il) %>% dplyr::select(ground_speed, timestamp, individual_local_identifier, location_long, location_lat, Nili_id, timestamp_il, date_il, start)})

results_landings_p2 <- join_space_time(buff_p2, stationaries_sf_nocliffs[[2]]) %>% mutate(period = 2, .before = 1)
results_landings_p3 <- join_space_time(buff_p3, stationaries_sf_nocliffs[[3]]) %>% mutate(period = 3, .before = 1)

results_landings <- bind_rows(results_landings_p2, results_landings_p3) |>
  ungroup() %>%
  mutate(landing_id = paste(individual_local_identifier, timestamp_il, sep = "."))

results_landings_summ <- results_landings %>%
  sf::st_drop_geometry() %>%
  group_by(period, carcID, carcType) %>%
  summarize(n_landings = length(unique(landing_id[!(str_detect(landing_id, "NA"))])))

results_landings_summ %>%
  ggplot(aes(x = interaction(carcType, factor(period)), y = n_landings, fill = factor(period)))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.1, size = 4, alpha = 0.7, pch = 1)+
  theme_bw()+
  labs(y = "Landings per carcass",
       x = "2-week period")+
  theme(legend.position = "none",
        text = element_text(size = 18))

# How many carcasses have 0 landings associated?
results_landings_summ %>%
  filter(n_landings < 1)
problem_ids <- results_landings_summ %>%
  filter(n_landings < 1) %>% pull(carcID)

# do those have landings in the other period? i.e. are they temporal edge cases?
results_landings_summ %>%
  filter(carcID %in% problem_ids) %>%
  arrange(carcID, period) # yes, in all cases the ones that have no landings in one period do have landings in the other period. So we do not have any carcasses that are completely unassociated with any landings.

# But of course, there are also many landings not associated with carcasses!
# Let's see if the raster of landings matches the raster of carcasses.

# Make landing rasters
# grid: we can use the same common grid from above
coords_landings <- map(stationaries_sf_nocliffs, ~st_coordinates(.x)[,1:2])

# build spatialpoints for kud
xy_landings <- map(coords_landings, ~{SpatialPoints(.x, proj4string = CRS(paste0("EPSG:", utm_crs)))})

# kud with same grid as previous
kud_landings <- map(xy_landings, ~{kernelUD(.x, h = 2000, grid = common_grid)}, .progress = T)

write_rds(kud_landings, file = "data/created/kud_landings.RDS")

kud_landings <- readRDS("data/created/kud_landings.RDS")

# convert to terra spatraster
r_landings <- map(kud_landings, ~rast(raster(.x)))
r_landings <- map(r_landings, ~{crs(.x) <- paste0("EPSG:", utm_crs); .x})

# normalize
total_landings  <- map(r_landings, ~global(.x, "sum", na.rm = TRUE)[1, 1])
r_norm_landings <- map2(r_landings, total_landings, ~.x / .y)

plot(r_norm_landings[[1]]) # period 1 raster from landings
plot(r_norm[[1]]) # period 1 raster from feeding bouts # these are quite similar!

plot(r_norm_landings[[2]]) # period 2 raster from landings
plot(r_norm[[2]]) # period 2 raster from feeding bouts # these are quite similar!
plot(r_norm_carcs[[1]]) # period 2 raster from carcasses
cor(values(r_norm_landings[[2]]), values(r_norm_carcs[[1]])) #55% correlation for period 2

plot(r_norm_landings[[3]]) # period 3 raster from landings
plot(r_norm[[3]]) # period 3 raster from feeding bouts # these are quite similar!
plot(r_norm_carcs[[2]]) # period 3 raster from carcasses
cor(values(r_norm_landings[[3]]), values(r_norm_carcs[[2]])) #39% correlation for period 2

# correlation between landings and feeding bouts:
cor(values(r_norm_landings[[1]]), values(r_norm[[1]])) #76%
cor(values(r_norm_landings[[2]]), values(r_norm[[2]])) #95%
cor(values(r_norm_landings[[3]]), values(r_norm[[3]])) #82%
# fairly high correlation between landings and feeding bouts, which is logical

plot(r_norm_landings[[2]]) # period 3 raster from landings
plot(r_norm[[3]]) # period 3 raster from feeding bouts
cor(values(r_norm_landings[[2]]), values(r_norm[[3]]))

# Instead of continuous values getting normalized, just make it 0/1 discretized. Size of raster is going to affect correlation a lot. 
# Eyeball instead of correlating. 
r_landings_bin <- map(r_landings, ~{.x > 0 * 1})
r_carcs_bin <- map(r_carcs, ~{.x > 0*1})
plot(r_carcs_bin[[1]])
plot(r_landings_bin[[2]])
cor(values(r_carcs_bin[[1]]), values(r_landings_bin[[2]])) #55%

plot(r_carcs_bin[[2]])
plot(r_landings_bin[[3]])
cor(values(r_carcs_bin[[2]]), values(r_landings_bin[[3]])) #58%

# play with smoothing, value size, etc. to get the correlation to make sense.

h_vals <- c(250, 500, 750, 1000, 1250, 1500, 1750, 2000, 2250)
kuds_carcs <- vector(mode = "list", length = length(h_vals))
kuds_landings <- vector(mode = "list", length = length(h_vals))
for(i in 1:length(h_vals)){
  kuds_carcs[[i]] <- map(xy_carcs, ~{kernelUD(.x, h = h_vals[i], grid = common_grid)}, .progress = T)
  kuds_landings[[i]] <- map(xy_landings, ~{kernelUD(.x, h = h_vals[i], grid = common_grid)}, .progress = T)
  cat("done with h=", h_vals[i], "\n")
}

# convert to terra spatraster
rs_carcs <- map(kuds_carcs, ~map(.x, ~rast(raster(.x))))
rs_landings <- map(kuds_landings, ~map(.x, ~rast(raster(.x))))
rs_landings <- map(rs_landings, ~.x[2:3]) # take only second and third elements, to match rs_carcs
binarize <- function(mylist){
  out <- map(mylist, ~{.x > 0*1})
  return(out)
}

rs_carcs_bin <- map(rs_carcs, binarize)
rs_landings_bin <- map(rs_landings, binarize)
cp2 <- map(rs_carcs_bin, 1)
cp3 <- map(rs_carcs_bin, 2)
lp2 <- map(rs_landings_bin, 1)
lp3 <- map(rs_landings_bin, 2)

# Extract values once as plain numeric vectors
valsCp2 <- lapply(cp2, function(r) values(r, mat = FALSE))
valsCp3 <- lapply(cp3, function(r) values(r, mat = FALSE))
valsLp2 <- lapply(lp2, function(r) values(r, mat = FALSE))
valsLp3 <- lapply(lp3, function(r) values(r, mat = FALSE))

# All pairs, directly in long format
res_p2 <- expand.grid(carcs = seq_along(valsCp2),
                      landings = seq_along(valsLp2))
res_p3 <- expand.grid(carcs = seq_along(valsCp3),
                      landings = seq_along(valsLp3))

res_p2$cor <- mapply(
  function(i, j) cor(valsCp2[[i]], valsLp2[[j]], use = "complete.obs"),
  res_p2$carcs, res_p2$landings
)
res_p2$carcs <- h_vals[res_p2$carcs]
res_p2$landings <- h_vals[res_p2$landings]
res_p2$period <- 2

res_p3$cor <- mapply(
  function(i, j) cor(valsCp3[[i]], valsLp3[[j]], use = "complete.obs"),
  res_p3$carcs, res_p3$landings
)

res_p3$carcs <- h_vals[res_p3$carcs]
res_p3$landings <- h_vals[res_p3$landings]
res_p3$period <- 3

res <- bind_rows(res_p2, res_p3)

res %>%
  ggplot(aes(x = carcs, y = landings, fill = cor))+
  geom_raster()+
  scale_fill_viridis_c(limits = c(0, 1))+ 
  facet_wrap(~period)+
  labs(y = "Smoothing parameter h (landings)",
       x = "Smoothing parameter h (carcasses)",
       fill = "Correlation")+
  theme_bw()+
  theme(text = element_text(size = 18))# interesting! the sweet spot seems to be having the carcass raster at 2250 and the landings raster at 1000, and both periods pretty much agree on that. I'm not sure if this is telling us something or if it would just keep getting better the higher we went with the carcasses. Like is this an optimum or are we just maxing out?

res %>%
  ggplot(aes(x = carcs, y = cor, color = factor(landings), group = factor(landings)))+
  geom_line()+
  facet_wrap(~period)+
  labs(y = "Correlation", x = "h for carcass raster",
       col = "h for landings raster") # hmm this makes it pretty clear that there isn't a single best one, the correlations just get huge at some point.

par(mfrow = c(1, 2))
plot(cp2[[9]]); plot(lp2[[4]])
dev.off()


cors <- map2(rs_carcs_bin, rs_landings_bin, ~data.frame(period = 2:3, cor = map2_dbl(.x, .y, ~{
  cor(values(.x), values(.y))
})))
names(cors) <- h_vals
cors_df <- purrr::list_rbind(cors, names_to = "hval") %>% mutate(hval = as.numeric(hval))
cors_df %>%
  ggplot(aes(x = hval, y = cor, color = factor(period)))+
  geom_point()+
  geom_line()

# Ok, I think this raster approach is not helping me. We need to go back to basics--what so we actually need? We need to be able to construct the variograms. We need to measure carcass presence, including magnitude and variability over time. And we need an indicator that matches this especially well. (There's no guarantee that even if the overall rasters were correlated, that the time series and variograms would be correlated as well)
# 1. Construct some sample ground truth variograms for one of the real periods (I guess let's do period 3, 2 weeks back? That's the best I can do I guess.)
carcs_simple <- readRDS("data/created/carcs_simple.RDS")

carcs_simple_p2 <- carcs_simple %>%
  filter(date >= lubridate::ymd("2023-03-15") & date < lubridate::ymd("2023-03-31"))
carcs_simple_p3 <- carcs_simple %>%
  filter(date >= lubridate::ymd("2023-04-01") & date < lubridate::ymd("2023-04-16"))

neighbor_list_16km_p3 <- sf::st_is_within_distance(carcs_simple_p3, carcs_simple, dist = 16000)
length(neighbor_list_16km_p3) #26

neighbor_list_8km_p3 <- sf::st_is_within_distance(carcs_simple_p3, carcs_simple, dist = 8000)
length(neighbor_list_8km_p3) #26

get_activity_vectors <- function(neighbor_list, days_back = 14, days_active = 3, all = carcs_simple){
  out <- purrr::imap(neighbor_list, function(idx, i) {
    focal_date   <- as.Date(all$date[i])
    window_start <- focal_date - lubridate::days(days_back)
    window_end   <- focal_date - lubridate::days(1)
    window_len   <- as.integer(window_end - window_start) + 1L  # 180
    
    neighbors <- all[idx, ]
    
    act_start <- as.Date(neighbors$date)
    act_end   <- act_start + lubridate::days(days_active-1)  # active for 3 days: date, date+1, date+2
    
    # keep only carcasses whose active window overlaps the 180-day lookback
    keep <- act_start <= window_end & act_end >= window_start
    act_start <- act_start[keep]
    act_end   <- act_end[keep]
    
    if (length(act_start) == 0) return(rep(0L, window_len))
    
    # clip active intervals to the lookback window, convert to day-offsets
    clipped_start <- pmax(act_start, window_start)
    clipped_end   <- pmin(act_end, window_end)
    start_idx <- as.integer(clipped_start - window_start) + 1L
    end_idx   <- as.integer(clipped_end   - window_start) + 1L
    
    # diff-array: +1 at each interval start, -1 just after each interval end,
    # then cumsum gives the running count — no per-day loops, no seq()
    delta <- tabulate(start_idx, nbins = window_len + 1L) -
      tabulate(end_idx + 1L, nbins = window_len + 1L)
    
    as.integer(cumsum(delta[1:window_len]))
  })
  return(out)
}
vectors_16km <- get_activity_vectors(neighbor_list_16km_p3, days_back = 14, days_active = 3, all = carcs_simple) # this will only be valid for the carcasses for which we actually have data 
vectors_16km

vectors_8km <- get_activity_vectors(neighbor_list_8km_p3, days_back = 14, days_active = 3, all = carcs_simple) # this will only be valid for the carcasses for which we actually have data 
vectors_8km

carcs_buffered_p3_16km <- st_buffer(carcs_simple_p3, 16000)
carcs_buffered_p3_8km <- st_buffer(carcs_simple_p3, 8000)
dates <- carcs_simple_p3$date
carcIDs <- carcs_simple_p3$carcID
stationaries <- bind_rows(stationaries_sf_nocliffs)

intersections_16 <- st_intersects(carcs_buffered_p3_16km, stationaries)
intersections_8 <- st_intersects(carcs_buffered_p3_8km, stationaries)
landings_in_range_16 <- map(intersections_16, ~st_drop_geometry(stationaries[.x,])) %>%
  map2(., dates, ~{
    filter(.x, date_il >= .y-lubridate::days(14) & date_il < .y)
  })
landings_in_range_8 <- map(intersections_8, ~st_drop_geometry(stationaries[.x,])) %>%
  map2(., dates, ~{
    filter(.x, date_il >= .y-lubridate::days(14) & date_il < .y)
  })

daily_stats_16 <- map2(landings_in_range_16, dates, ~{
  min_date <- .y-lubridate::days(14)
  max_date <- .y-lubridate::days(1)
  all_dates <- seq.Date(from = min_date, to = max_date)
  out <- .x %>% group_by(date_il) %>% 
    summarize(n_landings = n(),
              n_indivs = length(unique(individual_local_identifier))) 
  out <- full_join(out, data.frame(date_il = all_dates), 
                   by = "date_il") %>%
    mutate(across(everything(), ~replace_na(.x, 0))) %>%
    arrange(date_il) %>%
    mutate(idx = row_number())
})

daily_stats_8 <- map2(landings_in_range_8, dates, ~{
  min_date <- .y-lubridate::days(14)
  max_date <- .y-lubridate::days(1)
  all_dates <- seq.Date(from = min_date, to = max_date)
  out <- .x %>% group_by(date_il) %>% 
    summarize(n_landings = n(),
              n_indivs = length(unique(individual_local_identifier))) 
  out <- full_join(out, data.frame(date_il = all_dates), 
                   by = "date_il") %>%
    mutate(across(everything(), ~replace_na(.x, 0))) %>%
    arrange(date_il) %>%
    mutate(idx = row_number())
})

daily_stats_16 <- map2(daily_stats_16, vectors_16km, ~{
  .x %>% mutate(n_carcs = .y) %>%
    mutate(landings_presence = n_landings > 0,
           n_landings_scaled = scale(n_landings), n_indivs_scaled = scale(n_indivs), n_carcs_scaled = scale(n_carcs)) %>%
    mutate(km = 16)
})

daily_stats_8 <- map2(daily_stats_8, vectors_8km, ~{
  .x %>% mutate(n_carcs = .y) %>%
    mutate(landings_presence = n_landings > 0,
           n_landings_scaled = scale(n_landings), n_indivs_scaled = scale(n_indivs), n_carcs_scaled = scale(n_carcs)) %>%
    mutate(km = 8)
})

names(daily_stats_16) <- carcIDs
names(daily_stats_8) <- carcIDs

daily_stats <- purrr::list_rbind(daily_stats_16, names_to = "carcID") %>% bind_rows(purrr::list_rbind(daily_stats_8, names_to = "carcID"))

daily_stats_long <- daily_stats %>%
  pivot_longer(cols = c("n_landings", "n_indivs", "n_carcs", "n_landings_scaled", "n_indivs_scaled", "n_carcs_scaled", "landings_presence"), names_to = "measure", values_to = "value")

daily_stats_long %>%
  mutate(is_carcs = case_when(measure == "n_carcs_scaled" ~ T, .default = F)) %>%
  filter(km == 16, !(measure %in% c("n_landings", "n_indivs", "n_carcs"))) %>%
  ggplot(aes(x = idx, y = value, color = measure))+
  geom_line(aes(linewidth = is_carcs))+
  facet_wrap(~carcID, ncol = 7)+
  theme_minimal()+
  scale_linewidth_manual(values = c(0.5, 1))+
  labs(y = "Value", x = "Days before carcass",
       color = "Measure", linewidth = NULL)+
  scale_x_continuous(breaks = seq(from = 0, to = 14, by = 2))+
  guides(linewidth = "none")+
  theme(text = element_text(size = 16), legend.position = "bottom")

indices <- c("n_landings", "n_indivs", "landings_presence")  # add your other indices here
max_lag <- 2

# If you have number of tracked birds per day, normalize here, e.g.:
# daily_stats <- daily_stats |> mutate(n_landings = n_landings / n_tracked,
#                                      n_indivs   = n_indivs   / n_tracked)

dat <- daily_stats |>
  mutate(landings_presence = as.numeric(landings_presence)) |>
  arrange(carcID, km, idx)

# ---------- STEP 1: index x lag x radius screening ----------
# Lag k pairs the index on day t with carcasses on day t-k (landings trail carcasses)

auc <- function(score, present) {
  r <- rank(score); n1 <- sum(present); n0 <- sum(!present)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(r[present]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

screen_one <- function(ix, k) {
  d <- dat |>
    mutate(val = .data[[ix]]) |>
    group_by(carcID, km) |>
    mutate(carc_lag = dplyr::lag(n_carcs, k)) |>
    ungroup() |>
    filter(!is.na(carc_lag))
  
  # per-area correlations (NA if either series is constant)
  per_area <- d |>
    group_by(km, carcID) |>
    summarise(
      rho = suppressWarnings(cor(val, carc_lag, method = "spearman")),
      .groups = "drop")
  
  d |>
    group_by(km) |>
    summarise(
      rho_pooled   = suppressWarnings(cor(val, carc_lag, method = "spearman")),
      auc_presence = auc(val, carc_lag > 0),
      .groups = "drop") |>
    left_join(
      per_area |>
        group_by(km) |>
        summarise(rho_median = median(rho, na.rm = TRUE), .groups = "drop"),
      by = "km") |>
    mutate(index = ix, lag = k, .before = 1)
}

screen <- expand_grid(index = indices, lag = 0:max_lag) |>
  pmap_dfr(\(index, lag) screen_one(index, lag)) |>
  arrange(desc(abs(rho_pooled)))

print(screen, n = 30)

# Quick visual check: z-scored index vs carcass count over time
dat |>
  filter(km == min(km)) |>   # pick a radius
  group_by(carcID) |>
  mutate(across(c(n_carcs, all_of(indices)), \(x) as.numeric(scale(x)))) |>
  ungroup() |>
  pivot_longer(c(n_carcs, all_of(indices))) |>
  ggplot(aes(idx, value, colour = name)) +
  geom_line() + facet_wrap(~carcID) + theme_minimal()

# ---------- STEP 2: quantile matching ----------
# Pool carcass counts within a radius, then map each index value to the
# carcass value at the same quantile.

qmatch <- function(index_vals, carc_vals) {
  p <- (rank(index_vals, ties.method = "average") - 0.5) / length(index_vals)
  as.numeric(quantile(carc_vals, probs = p, type = 1))
}

best_index <- "n_indivs"   # <- choose after Step 1
best_lag   <- 0

proxy <- dat |>
  group_by(km) |>
  mutate(proxy = qmatch(.data[[best_index]], n_carcs)) |>
  ungroup()

# ---------- STEP 3: compare temporal variograms ----------
semivar <- function(x, lags = 1:5) {
  map_dbl(lags, \(h) {
    d <- x[(1 + h):length(x)] - x[1:(length(x) - h)]
    mean(0.5 * d^2, na.rm = TRUE)
  })
}

vario <- proxy |>
  group_by(carcID, km) |>
  summarise(
    lag = list(1:5),
    carcass = list(semivar(n_carcs)),
    proxy   = list(semivar(proxy)),
    raw_idx = list(semivar(as.numeric(scale(.data[[best_index]])))),
    .groups = "drop") |>
  unnest(c(lag, carcass, proxy, raw_idx)) |>
  pivot_longer(c(carcass, proxy, raw_idx), names_to = "series", values_to = "gamma")

ggplot(vario, aes(lag, gamma, colour = series, group = interaction(carcID, series))) +
  geom_line(alpha = .5) + facet_wrap(~km) + theme_minimal()

# Numeric summary: error between proxy and carcass variograms
vario |>
  pivot_wider(names_from = series, values_from = gamma) |>
  group_by(km) |>
  summarise(rmse_proxy = sqrt(mean((proxy - carcass)^2)),
            cor_proxy  = cor(proxy, carcass))

library(zoo)

# 1. Compare day-to-day persistence: lag-1 autocorrelation of carcasses vs proxy
acf1 <- function(x) suppressWarnings(cor(x[-1], x[-length(x)]))

proxy |>
  group_by(carcID, km) |>
  summarise(acf_carc  = acf1(n_carcs),
            acf_proxy = acf1(proxy),
            acf_index = acf1(.data[[best_index]]),
            .groups = "drop") |>
  group_by(km) |>
  summarise(across(starts_with("acf"), \(x) median(x, na.rm = TRUE)))

# 2. Add smoothed versions of each index (trailing window = carcass persistence)
dat <- dat |>
  group_by(carcID, km) |>
  mutate(across(all_of(indices),
                list(w3 = \(x) rollapplyr(x, 3, mean, partial = TRUE),
                     w5 = \(x) rollapplyr(x, 5, mean, partial = TRUE)),
                .names = "{.col}_{.fn}")) |>
  ungroup()

indices2 <- c(indices, paste0(rep(indices, each = 2), c("_w3", "_w5")))
# rerun screen with indices <- indices2, then repeat the quantile matching and variogram step

screen <- expand_grid(index = indices2, lag = 0:max_lag) |>
  pmap_dfr(\(index, lag) screen_one(index, lag)) |>
  arrange(desc(abs(rho_pooled)))

print(screen, n = 30)

# Quick visual check: z-scored index vs carcass count over time
dat |>
  filter(km == max(km)) |>   # pick a radius
  group_by(carcID) |>
  mutate(across(c(n_carcs, all_of(indices2)), \(x) as.numeric(scale(x)))) |>
  ungroup() |>
  pivot_longer(c(n_carcs, all_of(indices2))) |>
  ggplot(aes(idx, value, colour = name)) +
  geom_line() + facet_wrap(~carcID) + theme_minimal()

# ---------- STEP 2: quantile matching ----------
# Pool carcass counts within a radius, then map each index value to the
# carcass value at the same quantile.

qmatch <- function(index_vals, carc_vals) {
  p <- (rank(index_vals, ties.method = "average") - 0.5) / length(index_vals)
  as.numeric(quantile(carc_vals, probs = p, type = 1))
}

best_index <- "n_indivs_w3"   # <- choose after Step 1
best_lag   <- 0

proxy <- dat |>
  group_by(km) |>
  mutate(proxy = qmatch(.data[[best_index]], n_carcs)) |>
  ungroup()

# ---------- STEP 3: compare temporal variograms ----------
semivar <- function(x, lags = 1:5) {
  map_dbl(lags, \(h) {
    d <- x[(1 + h):length(x)] - x[1:(length(x) - h)]
    mean(0.5 * d^2, na.rm = TRUE)
  })
}

vario <- proxy |>
  group_by(carcID, km) |>
  summarise(
    lag = list(1:5),
    carcass = list(semivar(n_carcs)),
    proxy   = list(semivar(proxy)),
    raw_idx = list(semivar(as.numeric(scale(.data[[best_index]])))),
    .groups = "drop") |>
  unnest(c(lag, carcass, proxy, raw_idx)) |>
  pivot_longer(c(carcass, proxy, raw_idx), names_to = "series", values_to = "gamma")

ggplot(vario, aes(lag, gamma, colour = series, group = interaction(carcID, series))) +
  geom_line(alpha = .5) + facet_wrap(~km) + theme_minimal()

# Numeric summary: error between proxy and carcass variograms
vario |>
  pivot_wider(names_from = series, values_from = gamma) |>
  group_by(km) |>
  summarise(rmse_proxy = sqrt(mean((proxy - carcass)^2)),
            cor_proxy  = cor(proxy, carcass))

