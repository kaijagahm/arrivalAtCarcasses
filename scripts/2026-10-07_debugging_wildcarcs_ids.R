library(targets)
library(tidyverse)
library(sf)

tar_load(wild_carcs)
length(wild_carcs)
tar_load(all_carcasses_validated)
tar_load(wild_carcasses_validated)
glimpse(wild_carcasses_validated)

test <- st_as_sf(wild_carcasses_validated, coords = c("X", "Y"), crs = 32636)
wild_carcasses_validated %>% filter(is.na(X))
tar_load(wild_carcasses)
glimpse(wild_carcasses)

setdiff(
  tar_read(wild_carcasses_validated)$carcID,
  tar_read(wild_carcs) %>% map_dbl("carcID")
)
tar_load(c(all_carcasses, all_carcasses_south, wild, wild_carcasses_validated))
nrow(wild_carcasses_validated)
sum(all_carcasses$carcType == "wild", na.rm = TRUE)
sum(all_carcasses_south$carcType == "wild", na.rm = TRUE)
nrow(wild)
tar_load(wild_carcasses)
nrow(wild_carcasses)
wild_carcasses_validated |>
  sf::st_drop_geometry() |>
  dplyr::filter(is.na(carcType) | is.na(Y)) |>
  dplyr::select(carcID, dplyr::any_of(c("carcType", "Y")))
nrow(wild_carcasses)
setdiff(wild_carcasses_validated$carcID, wild_carcasses$carcID)
setdiff(wild_carcasses$carcID, wild_carcasses_validated$carcID)
tar_load(non_station_bo_prepped)
nrow(non_station_bo_prepped)
dplyr::count(non_station_bo_prepped, year)
v <- sf::st_transform(wild_carcasses_validated, 32636)
names(wild_carcasses_validated)
sapply(wild_carcasses_validated, function(x) class(x)[1])
g <- sf::st_transform(sf::st_zm(wild_carcasses_validated$geometry), 32636)
xy <- sf::st_coordinates(g)
d <- sqrt((xy[, 1] - wild_carcasses_validated$X)^2 + (xy[, 2] - wild_carcasses_validated$Y)^2)
sf::st_crs(wild_carcasses_validated$geometry)$input
summary(d)
sum(d > 50, na.rm = TRUE)
kml <- sf::st_read("data/raw/wildCarcassValidation/cluster_centroids_200m_24hr_min3_2022_2023_2024_NOCLIFFS_withnames.kml", quiet = TRUE)
kml <- kml[kml$Name != "", ]
kml$carcID <- as.integer(sub("[(_].*", "", kml$Name))
xy_old <- sf::st_coordinates(sf::st_transform(sf::st_zm(kml), 32636))
old <- data.frame(carcID = kml$carcID, x_old = xy_old[, 1], y_old = xy_old[, 2])
new <- as.data.frame(wild_carcasses)[, c("carcID", "X", "Y")]
m <- merge(old, new, by = "carcID")
m$d <- sqrt((m$x_old - m$X)^2 + (m$y_old - m$Y)^2)
nrow(m)
summary(m$d)
sum(m$d > 50, na.rm = TRUE)
old_sf <- sf::st_as_sf(old, coords = c("x_old", "y_old"), crs = 32636)
new_sf <- sf::st_as_sf(new[!is.na(new$X), ], coords = c("X", "Y"), crs = 32636)
nn <- sf::st_nearest_feature(old_sf, new_sf)
old$nn_dist <- as.numeric(sf::st_distance(old_sf, new_sf[nn, ], by_element = TRUE))
nrow(old)
summary(old$nn_dist)
sum(old$nn_dist > 50)
old$Name <- kml$Name
old[old$nn_dist > 50, c("carcID", "Name", "nn_dist")]
tar_load(non_station_bo_prepped)
pts <- old_sf[old_sf$carcID %in% c(48, 69), ]
bouts <- sf::st_transform(non_station_bo_prepped, 32636)
for (i in seq_len(nrow(pts))) {
  near <- bouts[as.numeric(sf::st_distance(bouts, pts[i, ])) < 200, ]
  cat("carcID", pts$carcID[i], ": bouts within 200 m =", nrow(near), "\n")
  print(table(near$year))
}
tar_load(c(feeding_bo_spatial, feeding_bo_stationary, feeding_bo_nocliffs))
for (nm in c("feeding_bo_spatial", "feeding_bo_stationary", "feeding_bo_nocliffs")) {
  b <- sf::st_transform(get(nm), 32636)
  cat(nm, "\n")
  for (i in seq_len(nrow(pts))) {
    n <- sum(as.numeric(sf::st_distance(b, pts[i, ])) < 200)
    cat("  carcID", pts$carcID[i], ":", n, "\n")
  }
}
tar_load(c(bo_pr_2022, bo_pr_2023, bo_pr_2024))
length(bo_pr_2022); length(bo_pr_2023); length(bo_pr_2024)
tar_meta(fields = c("name", "error", "warnings")) |>
  dplyr::filter(!is.na(error) | !is.na(warnings)) |>
  print(n = 50)
tar_load(c(splitup_22, splitup_23, splitup_24))
c(y2022 = splitup_22[[46]]$device_id[1],
  y2023 = splitup_23[[46]]$device_id[1],
  y2024 = splitup_24[[46]]$device_id[1])
for (i in seq_len(nrow(pts))) {
  near <- bouts[as.numeric(sf::st_distance(bouts, pts[i, ])) < 200, ]
  cat("carcID", pts$carcID[i], "\n")
  print(sf::st_drop_geometry(near)[, c("device_id", "year", "start")])