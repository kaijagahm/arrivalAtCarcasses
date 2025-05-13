# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(crew)

# Set target options:
tar_option_set(
  error = "null",
  packages = c("vultureUtils", "tidyverse", "here", "NBDA", "sf", "dplyr")#, # Packages that your targets need for their tasks.
  #controller = crew_controller_local(workers = 4)
)

# Run the R scripts in the R/ folder with your custom functions:
lapply(list.files("R", full.names = TRUE), source) # source all scripts in the R directory

list(
  tar_target(pw, "movebankCredentials/pw.Rda", format = "file"),
  tar_target(loginObject, get_loginObject(pw)),
  tar_target(ww_file, "data/raw/whoswho_vultures_20230920_new.xlsx", format = "file"),
  
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
  tar_target(carcasses_audited, readRDS(here("data/created/carcasses_audited.RDS"))),
  
  ## Focal carcasses
  ### During the 2023 and 2024 HF-ACC periods
  ### Created in 01_classify_localize_bouts.2023.R
  tar_target(dates, readRDS(here("data/created/minmax_dates.RDS"))),
  tar_target(carcasses_focal, get_focal(carcasses_audited, dates)),
  
  ## Match bouts to carcasses
  tar_target(dist_bouts_carcasses, 750),
  tar_target(hours_before_carcass, 1),
  tar_target(hours_after_carcass, 72),
  tar_target(carcass_bouts, get_carcass_bouts(bouts = feeding_bouts,
                                              carcasses = carcasses_focal,
                                              dist = dist_bouts_carcasses,
                                              hours_before = hours_before_carcass,
                                              hours_after = hours_after_carcass)),
  tar_target(carcass_bouts_df, purrr::list_rbind(carcass_bouts)), # note: each bout might be affiliated with more than one carcass here!
  tar_target(remaining_bouts, filter(feeding_bouts, !(boutID %in% carcass_bouts_df$boutID))),
  
  ## Cluster the remaining bouts
  tar_target(dist_bouts_wild_carcass_cluster, 250), # updated to 250 to match Gideon's thresholds
  tar_target(time_bouts_wild_carcass_cluster, '24 hours'), # note: cannot be more than 24 hours. If we want more than 24 hours, we need to do this grouping a different way.
  tar_target(wild_carcass_bouts_df, get_wild_carcass_bouts(remaining_bouts,
                                                           time = time_bouts_wild_carcass_cluster,
                                                           dist = dist_bouts_wild_carcass_cluster,
                                                           minBouts = 3,
                                                           stations = stations,
                                                           stationDist = 750)),
  tar_target(wild_carcasses, get_wild_carcasses(wild_carcass_bouts_df) %>%
               mutate(carcType = "wild")),
  tar_target(remaining_bouts_2, left_join(remaining_bouts,
                                          sf::st_drop_geometry(wild_carcass_bouts_df) %>%
                                            select(boutID, carcID),
                                          by = "boutID") %>%
               mutate(carcType = case_when(!is.na(carcID) ~"wild",
                                           .default = NA))),
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
                                        select(carcID, X, Y,
                                               stationName, carcassWeight,
                                               datetime, cage) %>%
                                        mutate(dateOnly = lubridate::date(datetime),
                                               carcType = "inpa",
                                               year = lubridate::year(datetime)),
                                      wild_carcasses %>%
                                        select(carcID, X, Y,
                                               year, dateOnly, nBouts, nIndivs) %>%
                                        mutate(carcType = "wild"))),
  
  ## Assign carcasses (INPA and wild) to stations
  tar_target(carcasses_split, group_by(all_carcasses, carcID) %>% group_split()),
  tar_target(bouts_split, sf::st_as_sf(all_bouts_assigned, coords = c("X", "Y"), crs = 32636, remove = F) %>%
               group_by(boutID) %>%
               group_split()),
  ### Carcasses
  tar_target(stn_min_dists_carc, map_dbl(carcasses_split, ~min(st_distance(.x, stations)))),
  tar_target(closest_stn_carc, purrr::list_rbind(map(carcasses_split, ~stations[which.min(st_distance(.x, stations)),]))),
  tar_target(all_carcasses_annotated, all_carcasses %>%
               mutate(dist_stn = stn_min_dists_carc) %>%
               bind_cols(st_drop_geometry(closest_stn_carc) %>%
                           select("stn" = stationName))),
  ### Bouts
  tar_target(stn_min_dists_bouts, map_dbl(bouts_split, ~min(st_distance(.x, stations)))),
  tar_target(closest_stn_bouts, purrr::list_rbind(map(bouts_split, ~stations[which.min(st_distance(.x, stations)),]))),
  tar_target(all_bouts_annotated, all_bouts_assigned %>%
               ungroup() %>%
               mutate(dist_stn = stn_min_dists_bouts) %>%
               bind_cols(st_drop_geometry(closest_stn_bouts) %>%
                           select("stn" = stationName)) %>%
               select(-c(prob, start, end, location_lat, location_long))),
  tar_target(bbox_bouts_hf, st_bbox(feeding_bouts)),
  tar_target(bbox_inpa_carcasses, st_bbox(carcasses_audited)),
  tar_target(bbox_inpa_carcasses_hf, st_bbox(carcasses_focal)),
  tar_target(a, st_crs(bbox_bouts_hf)),
  tar_target(bbox_south, 
             st_set_crs(st_bbox(c("xmin" = as.numeric(bbox_inpa_carcasses_hf[1]),
                                  "ymin" = 3350000, 
                                  "xmax" = as.numeric(bbox_inpa_carcasses_hf[3]),
                                  "ymax" = 3500000)), a)),
  ## Dynamic NBDA testing
  ## 0. Define parameters
  tar_target(days_after, 3),
  tar_target(seed_distance_flight, 2000),
  tar_target(seed_distance_stationary, 1000),
  tar_target(seed_time_before, lubridate::minutes(30)),
  tar_target(detection_distance_flight, 2000),
  tar_target(detection_distance_stationary, 1000),
  tar_target(arrival_distance, 400),
  ## 1. Get carcasses and refstrict to south
  tar_target(aca, sf::st_crop(all_carcasses_annotated, bbox_south)),
  ## 1a. Convert carcasses to Israel time
  ##  XXX FIXME
  ## 2. Separate INPA and wild (the rest of the instructions here are just for INPA)
  tar_target(inpa, filter(aca, carcType == "inpa")),
  tar_target(inpa_carcs, group_split(group_by(inpa, carcID))),
  tar_target(wild, filter(aca, carcType == "wild")),
  tar_target(wild_carcs, group_split(group_by(wild, carcID))),
  tar_target(gps_2023, data.table::fread("data/ACC/2023_hf_period/created/gps_2023.csv")),
  tar_target(gps_2024, data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv")),
  tar_target(gps_combined, get_gps_combined(gps_2023, gps_2024, bbox_south)),
  ## 4a. Convert gps data to Israel time 
  ## XXX fixme
  ## 4b. Make gps_all
  tar_target(gps_all, get_gps_all(inpa_carcs, gps_combined, days_after)),
  tar_target(roosts, get_roosts(gps_all)), 
  ## 6. Get seeds
  tar_target(seeds_gps, get_seeds_gps(gps_all, inpa_carcs, seed_time_before, seed_distance_flight, seed_distance_stationary)),
  tar_target(seed_indivs, map(seeds_gps, ~sort(unique(sf::st_drop_geometry(.x)$local_identifier)))),
  ## 7. Get distances from roosts to carcasses
  tar_target(distances, get_distances(roosts, inpa_carcs)),
  ## 8. Load who's who
  tar_target(ww, read_csv(here("data/raw/whoswho_vultures_20230920_new.csv"), col_select = 1:40)),
  ## 9. Get age_group ILV
  tar_target(www, get_www(ww)),
  ## 10. Combine age_group ILV with distances to get ILVs data frame
  tar_target(ilvs, get_ilvs(distances, www)),
  ## 11. Make gps (i.e. remove points before the carcass)
  tar_target(gps, remove_points_before(gps_all, inpa_carcs, days_after)),
  ## 12. Get sightings of the carcass
  tar_target(see_carcass, get_see_carcass(gps, inpa_carcs, detection_distance_flight, detection_distance_stationary)),
  ## 13. Get firsts
  # Get first sighting of each vulture to the carcass
  tar_target(firsts_see, get_firsts_see(see_carcass, inpa_carcs)),
  # Everything after this will be subsetted by has_visits or has_sightings; won't be calculated otherwise.
  ## 15. Get GPS subsets for flight (four different intervals)
  tar_target(gps_flight_allday_see, get_flight_allday(gps, has_enough_sightings)),
  tar_target(gps_flight_cumulative_see, get_gps_flight(gps, has_enough_sightings, see_times)),
  ## 16. Get roost nets
  tar_target(roosts_dates_see, get_roost_dates(roosts, has_enough_sightings)),
  tar_target(roost_thresh, 500),
  tar_target(roosts_bin_see, get_roosts_bin(roosts_dates_see, roost_thresh)),
  #tar_target(roosts_wt_see, get_roosts_weighted(roosts_dates_see)),
  ## 17. Get flight nets (whole days)
  tar_target(fl_allday_bin_see, get_fl_bin_list(gps_flight_allday_see, detection_distance_flight)),
  tar_target(fl_cumulative_bin_see, get_fl_bin_list(gps_flight_cumulative_see, detection_distance_flight)),
  #tar_target(fl_cumulative_wt_see, get_fl_wt_list(gps_flight_cumulative_see, detection_distance_flight)),
  # Fix networks to make sure they include all indivs
  tar_target(fl_allday_bin_fixed_see, fix_nets_list(fl_allday_bin_see, oa_see_indivs_sorted)),
  tar_target(fl_cumulative_bin_fixed_see, fix_nets_list(fl_cumulative_bin_see, oa_see_indivs_sorted)),
  tar_target(roosts_bin_fixed_see, fix_nets_list(roosts_bin_see, oa_see_indivs_sorted)),
  
  # NBDA --------------------------------------------------------------------
  ## Define carcasses to run NBDA on
  ## At least how many sightings?
  tar_target(min_sightings, 5),
  ## Going to use sightings, not arrivals, for NBDA.
  tar_target(has_enough_sightings, get_has_enough_sightings(firsts_see, min_sightings)),
  tar_target(carcs_nbda, inpa_carcs[has_enough_sightings]),
  
  tar_target(oa_see, purrr::map(firsts_see[has_enough_sightings], "local_identifier")),
  tar_target(oa_see_indivs_sorted, purrr::map(oa_see, sort)),
  tar_target(seeds_see, seed_indivs[has_enough_sightings]),
  tar_target(seeds_see_binary, map2(oa_see_indivs_sorted, seeds_see, ~{as.numeric(.x %in% .y)})),
  tar_target(see_times, purrr::map(firsts_see[has_enough_sightings], "timestamp")),
  tar_target(firsts_nbda, firsts_see[has_enough_sightings]),
  tar_target(years, get_years(carcs_nbda, oa_see)),
  tar_target(carcIDs_nbda, map_chr(carcs_nbda, ~as.character(.x$carcID[1]))),
  ## Here we decide to use the all-day flight networks for this. Will have to re-write the arguments to these targets if we decide to use different flight networks instead.
  ## Need to convert the oas into numeric indices instead of a character vector
  tar_target(oas_nbda_numbers, map2(oa_see, oa_see_indivs_sorted, ~match(.x, .y))),
  tar_target(dates_nbda, map2(carcs_nbda, firsts_nbda, ~mutate(data.frame(dateOnly = seq.Date(from = .x$dateOnly, to = max(.y$dateOnly), by = "day")), day = 1:n()))),
  tar_target(firsts_with_dates, map2(firsts_nbda, dates_nbda, ~left_join(.x, .y))),
  tar_target(days_vec_nbda, map(firsts_with_dates, "day")),
  tar_target(roost_mats_expanded, expand_roost_mats(roosts_bin_fixed_see, fl_allday_bin_fixed_see, days_vec_nbda)),
  tar_target(fl_mats_expanded, map(fl_cumulative_bin_fixed_see, ~map(.x, as.matrix))),
  
  ## Fix up ILVs
  # Okay, so now we have the roost and flight networks, in matrix format, that we're going to need to put into the model. Now let's grab the ilvs
  tar_target(ilvs_nbda, ilvs[has_enough_sightings]),
  tar_target(ilvs_lists, get_ilvs_lists(ilvs_nbda, days_vec_nbda)),
  # First step: NBDA for all carcasses using dynamic roost network ----------
  tar_target(n_indivs, map_dbl(roosts_bin_fixed_see, ~nrow(.x[[1]]))),
  tar_target(n_timeperiods, map_dbl(roost_mats_expanded, length)),
  #   
  #Create the empty arrays and slot in the network for each time period
  tar_target(N.RD, get_dynamic_nets(n_indivs, n_timeperiods, roost_mats_expanded)),
  tar_target(N.FD, get_dynamic_nets(n_indivs, n_timeperiods, fl_mats_expanded)),
  
  # Now we need a vector specifying which time period corresponds to which detection event. Since we already did the work of expanding the matrices (oops), this vector will just be 1 through the number of detection events.
  tar_target(assMatrixIndices, map(oas_nbda_numbers, ~1:length(.x))),
  #Now we enter the 4 dimensional network and assMatrixIndex as follows
  tar_target(nbdaData_list_dynamic_roost, get_nbdaData_list_flex(cids = carcIDs_nbda, oas = oas_nbda_numbers, amis = assMatrixIndices, nets1 = N.RD, is_dynamic = T)),
  tar_target(nbdaData_list_dynamic_flight, get_nbdaData_list_flex(cids = carcIDs_nbda, oas = oas_nbda_numbers, amis = assMatrixIndices, nets1 = N.FD, is_dynamic = T)),
  ## Make models
  ### social
  tar_target(Mods_N.RD_So, mod_trycatch(nbdaData_list_dynamic_roost, type = "social", iterations = 1000)),
  tar_target(Mods_N.FD_So, mod_trycatch(nbdaData_list_dynamic_flight, type = "social", iterations = 1000)),
  ### asocial
  tar_target(Mods_N.RD_Aso, mod_trycatch(nbdaData_list_dynamic_roost, type = "asocial", iterations = 1000)),
  tar_target(Mods_N.FD_Aso, mod_trycatch(nbdaData_list_dynamic_flight, type = "asocial", iterations = 1000)),
  ## Get model stats
  tar_target(sums_RD, get_summaries(Mods_N.RD_So, carcIDs_nbda, "dynamic", "roost")),
  tar_target(sums_RD_A, get_summaries(Mods_N.RD_Aso, carcIDs_nbda, "dynamic", "roost")),
  tar_target(sums_FD, get_summaries(Mods_N.FD_So, carcIDs_nbda, "dynamic", "flight")),
  tar_target(sums_FD_A, get_summaries(Mods_N.FD_Aso, carcIDs_nbda, "dynamic", "flight")),
  tar_target(summaries, bind_rows(sums_RD, sums_RD_A, sums_FD, sums_FD_A)),

    # Make single-network models with ILVs ------------------------------------
    tar_target(roost_carc_distances, get_ilv_separate(n_indivs, oas_nbda_numbers, ilvs_lists, ilv = "dist")),
    tar_target(age_groups, get_ilv_separate(n_indivs, oas_nbda_numbers, ilvs_lists, ilv = "age")),
    tar_target(prop_nas_roost_carc_distances, map_dbl(roost_carc_distances, ~sum(is.na(.x))/length(.x))), # XXX probably later we should not use this ILV for any carcasses where too high a proportion of them are NA.
    tar_target(roost_carc_distances_NAs_filled, substitute_na_distances(roost_carc_distances)),
    tar_target(std_roost_carc_distances_NAs_filled, std_dists(roost_carc_distances_NAs_filled)),
    tar_target(age_groups_bin, binarize_ages(age_groups)),
    tar_target(age_groups_reversed, map(age_groups_bin, ~{+(!.x)})),
    ## Get datasets for models containing one network and both ILVs
    tar_target(nbdaData_list_dynamic_roost_ilvs, get_nbdaData_list_flex(cids = carcIDs_nbda, oas = oas_nbda_numbers, amis = assMatrixIndices, nets1 = N.RD, is_dynamic = T, dists = std_roost_carc_distances_NAs_filled, ags = age_groups_bin)),
    tar_target(nbdaData_list_dynamic_flight_ilvs, get_nbdaData_list_flex(cids = carcIDs_nbda, oas = oas_nbda_numbers, amis = assMatrixIndices, nets1 = N.FD, is_dynamic = T, dists = std_roost_carc_distances_NAs_filled, ags = age_groups_bin)),
    ## Make models
    ### social
    tar_target(Mods_N.RD_So_ilvs, mod_trycatch(nbdaData_list_dynamic_roost_ilvs, type = "social", iterations = 1000)),
    tar_target(Mods_N.FD_So_ilvs, mod_trycatch(nbdaData_list_dynamic_flight_ilvs, type = "social", iterations = 1000)),
    ### asocial
    tar_target(Mods_N.RD_Aso_ilvs, mod_trycatch(nbdaData_list_dynamic_roost_ilvs, type = "asocial", iterations = 1000)),
    tar_target(Mods_N.FD_Aso_ilvs, mod_trycatch(nbdaData_list_dynamic_flight_ilvs, type = "asocial", iterations = 1000)),
    ## Get model stats
    tar_target(sums_RD_ilvs, get_summaries(Mods_N.RD_So_ilvs, carcIDs_nbda, "dynamic", "roost")),
    tar_target(sums_RD_A_ilvs, get_summaries(Mods_N.RD_Aso_ilvs, carcIDs_nbda, "dynamic", "roost")),
    tar_target(sums_FD_ilvs, get_summaries(Mods_N.FD_So_ilvs, carcIDs_nbda, "dynamic", "flight")),
    tar_target(sums_FD_A_ilvs, get_summaries(Mods_N.FD_Aso_ilvs, carcIDs_nbda, "dynamic", "flight")),
    tar_target(summaries_ilvs, bind_rows(sums_RD_ilvs, sums_RD_A_ilvs, sums_FD_ilvs, sums_FD_A_ilvs)),

    # Make two-network models, with and without ILVs ---------------------------------------
    tar_target(nbdaData_list_2nets_ilvs, get_nbdaData_list_flex(cids = carcIDs_nbda, oas = oas_nbda_numbers, amis = assMatrixIndices, nets1 = N.RD, nets2 = N.FD, is_dynamic = T, dists = std_roost_carc_distances_NAs_filled, ags = age_groups_bin, n_indivs = n_indivs, n_timeperiods = n_timeperiods)),
    tar_target(nbdaData_list_2nets, get_nbdaData_list_flex(cids = carcIDs_nbda, oas = oas_nbda_numbers, amis = assMatrixIndices, nets1 = N.RD, nets2 = N.FD, is_dynamic = T, n_indivs = n_indivs, n_timeperiods = n_timeperiods)),
    tar_target(Mods_2nets_So_ilvs, mod_trycatch(nbdaData_list_2nets_ilvs, type = "social", iterations = 1000)),
    tar_target(Mods_2nets_So, mod_trycatch(nbdaData_list_2nets, type = "social", iterations = 1000)),
    tar_target(summary_2nets_ilvs, get_summaries(Mods_2nets_So_ilvs, carcIDs_nbda, "dynamic", "both")),
    tar_target(summary_2nets, get_summaries(Mods_2nets_So, carcIDs_nbda, "dynamic", "both"))#,
  # 
  #   # Model averaging ---------------------------------------------------------
  #   # tar_target(constraintsVectMatrix, get_constraintsVectMatrix()),
  #   # tar_target(modelset_list, get_modelset(nbdaData_list_2nets_ilvs, constraintsVectMatrix)),
  #   # tar_target(networksSupport_list, map(modelset_list, networksSupport)),
  #   # tar_target(maes_list, get_maes(modelset_list)),
  #   # tar_target(lowerLimitsByModel_net1, get_lowerlimits(modelset_list, net = 1, conf_level = 0.95)),
  #   # tar_target(lowerLimitsByModel_net2, get_lowerlimits(modelset_list, net = 2, conf_level = 0.95)),
  #   # tar_target(lowerLimits_propST_MA_net1, map_dbl(lowerLimitsByModel_net1, ~sum(.x$propST*.x$adjAkWeight, na.rm = T))),
  #   # tar_target(lowerLimits_propST_MA_net2, map_dbl(lowerLimitsByModel_net2, ~sum(.x$propST*.x$adjAkWeight, na.rm = T)))
)
