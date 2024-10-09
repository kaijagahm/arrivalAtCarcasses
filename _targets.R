# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  memory = "transient",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 10, seconds_timeout = 120),
  format = "qs",
  error = "null",
  packages = c("vultureUtils", "sf", "tidyverse", "move2", "feather", "readxl", "elevatr", "here", "furrr", "future", "purrr", "igraph", "mapview", "parallel",   "ggplot2", "ggraph", "tidygraph", "moments", "tidymodels", "ranger", "parsnip", "caret", "zoo", "readxl", "data.table", "readr") # Packages that your targets need for their tasks.
  
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
)

# Run the R scripts in the R/ folder with your custom functions:
lapply(list.files("R", full.names = TRUE), source) # source all scripts in the R directory

list(
  tar_target(pw, "movebankCredentials/pw.Rda", format = "file"),
  tar_target(loginObject, get_loginObject(pw)),
  tar_target(ww_file, "data/whoswho_vultures_20230920_new.xlsx", format = "file"),
  
  ## GPS data for the focal periods (in case we need it later)
  tar_target(focal_gps_2023, readRDS(here("data/ACC/2023_hf_period/created/focal_gps_2023.RDS"))),
  tar_target(focal_gps_2024, readRDS(here("data/ACC/2024_hf_period/created/focal_gps_2024.RDS"))),
  
  ## Feeding bouts (2023 and 2024 high-frequency periods only)
  ### These were created in 01_classify_localize_bouts_2023.R because targets was being a poop.
  tar_target(feeding_bouts, readRDS(here("data/created/feeding_bouts.RDS")) %>%
               mutate(boutID = paste(year, device_id, bout_id, sep = "_")) %>%
               select(boutID,
                      "individualID" = device_id,
                      "prob" = .pred_Eating,
                      start, end, dateOnly, year, location_lat, location_long) %>%
               bind_cols(sf::st_coordinates(.))),
  
  ## Feeding stations
  ### Created in 00_carcass_data_translation.R
  ### Only spatial, not time-restricted.
  tar_target(stations, readRDS(here("data/created/stations.RDS"))),
  
  ## INPA carcasses
  ### Created in 00_carcass_data_translation.R
  ### Only spatial, not time-restricted.
  tar_target(carcasses_inpa, readRDS(here("data/created/carcasses_inpa.RDS"))),
  tar_target(dist_stations_inferred, 500),
  tar_target(stations_inferred, cluster_carcasses(carcasses_inpa, dist_stations_inferred) %>% rename("stn_inf" = clust)),
  
  ## Focal carcasses
  ### During the 2023 and 2024 HF-ACC periods
  ### Created in 01_classify_localize_bouts.2023.R
  tar_target(dates, readRDS(here("data/created/minmax_dates.RDS"))),
  tar_target(carcasses_focal, get_focal(carcasses_inpa, dates)),
  
  ## Match bouts to carcasses
  tar_target(dist_bouts_carcasses, 750),
  tar_target(hours_before_carcass, 1),
  tar_target(hours_after_carcass, 48),
  tar_target(carcass_bouts, get_carcass_bouts(bouts = feeding_bouts,
                                              carcasses = carcasses_focal,
                                              dist = dist_bouts_carcasses,
                                              hours_before = hours_before_carcass,
                                              hours_after = hours_after_carcass)),
  tar_target(carcass_bouts_df, purrr::list_rbind(carcass_bouts)), # note: each bout might be affiliated with more than one carcass here!
  tar_target(remaining_bouts, filter(feeding_bouts, !(boutID %in% carcass_bouts_df$boutID))),
  
  ## Cluster the remaining bouts
  tar_target(dist_bouts_wild_carcass_cluster, 100),
  tar_target(time_bouts_wild_carcass_cluster, '24 hours'), # note: cannot be more than 24 hours. If we want more than 24 hours, we need to do this grouping a different way.
  tar_target(wild_carcass_bouts_df, get_wild_carcass_bouts(remaining_bouts,
                                                           time = time_bouts_wild_carcass_cluster,
                                                           dist = dist_bouts_wild_carcass_cluster,
                                                           minBouts = 3)),
  tar_target(wild_carcasses, get_wild_carcasses(wild_carcass_bouts_df) %>%
               mutate(carcType = "wild")),
  tar_target(remaining_bouts_2, left_join(remaining_bouts, 
                                          sf::st_drop_geometry(wild_carcass_bouts_df) %>%
                                            select(boutID, carcID),
                                          by = "boutID") %>%
               mutate(carcType = "wild")),
  tar_target(bouts_double_assigned, group_by(carcass_bouts_df, boutID) %>%
               filter(n() > 1) %>%
               pull(boutID)),
  tar_target(carcass_bouts_dedup, group_by(carcass_bouts_df, boutID) %>%
               arrange(boutID, time_since_carcass) %>%
               slice(1)),  # HEURISTIC: TAKE THE ONE CLOSER TO THE TIME OF CARCASS PLACEMENT
  tar_target(all_bouts_assigned, bind_rows(carcass_bouts_dedup %>%
                                             mutate(carcType = "inpa"),
                                           remaining_bouts_2)), # note: all bouts are now assigned to a "carcass". We might want to consider redefining singleton bouts as not actually representing a wild carcass all on their own, or set some sort of threshold for groups...,

  ## Combine carcasses
  tar_target(all_carcasses, bind_rows(carcasses_focal %>%
                                        select(carcID, "X" = itmLong, "Y" = itmLat, 
                                               stationName, carcassWeight,
                                               datetime, cage) %>%
                                        mutate(dateOnly = lubridate::date(datetime),
                                               carcType = "inpa",
                                               year = lubridate::year(datetime)),
                                      wild_carcasses %>%
                                        select(carcID, X, Y,
                                               year, dateOnly, nBouts, nIndivs) %>%
                                        mutate(carcType = "wild"))),

  ## Assign carcasses (INPA and wild) to stations (documented and inferred)
  tar_target(carcasses_split, group_by(all_carcasses, carcID) %>% group_split()),
  tar_target(bouts_split, sf::st_as_sf(all_bouts_assigned, coords = c("X", "Y"), crs = 32636, remove = F) %>%
               group_by(boutID) %>%
               group_split()),
  ### Carcasses
  tar_target(stn_min_dists_carc, map_dbl(carcasses_split, ~min(st_distance(.x, stations)))),
  tar_target(closest_stn_carc, purrr::list_rbind(map(carcasses_split, ~stations[which.min(st_distance(.x, stations)),]))),
  tar_target(stn_inf_min_dists_carc, map_dbl(carcasses_split, ~min(st_distance(.x, stations_inferred)))),
  tar_target(closest_stn_inf_carc, purrr::list_rbind(map(carcasses_split, ~stations_inferred[which.min(st_distance(.x, stations_inferred)),]))),
  tar_target(all_carcasses_annotated, all_carcasses %>%
               mutate(dist_stn = stn_min_dists_carc,
                      dist_stn_inf = stn_inf_min_dists_carc) %>%
               bind_cols(st_drop_geometry(closest_stn_carc) %>%
                           select("stn" = stationName)) %>%
               bind_cols(st_drop_geometry(closest_stn_inf_carc) %>%
                           select(stn_inf))),
  ### Bouts
  tar_target(stn_min_dists_bouts, map_dbl(bouts_split, ~min(st_distance(.x, stations)))),
  tar_target(closest_stn_bouts, purrr::list_rbind(map(bouts_split, ~stations[which.min(st_distance(.x, stations)),]))),
  tar_target(stn_inf_min_dists_bouts, map_dbl(bouts_split, ~min(st_distance(.x, stations_inferred)))),
  tar_target(closest_stn_inf_bouts, purrr::list_rbind(map(bouts_split, ~stations_inferred[which.min(st_distance(.x, stations_inferred)),]))),
  tar_target(all_bouts_annotated, all_bouts_assigned %>%
               ungroup() %>%
               mutate(dist_stn = stn_min_dists_bouts,
                      dist_stn_inf = stn_inf_min_dists_bouts) %>%
               bind_cols(st_drop_geometry(closest_stn_bouts) %>%
                           select("stn" = stationName)) %>%
               bind_cols(st_drop_geometry(closest_stn_inf_bouts) %>%
                           select(stn_inf)) %>%
               select(-c(individualID, prob, start, end, location_lat, location_long)))
)
