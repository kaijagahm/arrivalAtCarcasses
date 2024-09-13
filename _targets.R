# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.
# tar_config_set(
#   seconds_meta_append = 15,
#   seconds_reporter = 0.5
# )

# Set target options:
tar_option_set(
  memory = "transient",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 10),
  format = "qs",
  error = "null",
  packages = c("vultureUtils", "sf", "tidyverse", "move", "feather", "readxl", "elevatr", "here", "furrr", "future", "purrr", "igraph", "mapview", "parallel",   "ggplot2", "ggraph", "tidygraph", "moments", "tidymodels", "ranger", "parsnip", "caret", "zoo", "readxl", "data.table") # Packages that your targets need for their tasks.
  
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
# tar_source("other_functions.R") # Source other scripts as needed.

list(
  # Prepare data (mining period, geofenced)
  tar_target(geofences, get_geofences()),
  tar_target(tag_sns_file, "data/geofence_tags_6Aug23_45.xlsx", format = "file"),
  tar_target(tag_sns, get_sns(tag_sns_file)),
  tar_target(pw, "movebankCredentials/pw.Rda", format = "file"),
  tar_target(loginObject, get_loginObject(pw)),
  tar_target(ornitela, get_ornitela(loginObject)),
  tar_target(ww_file, "data/whoswho_vultures_20230920_new.xlsx", format = "file"),
  tar_target(fixed_names, fix_names(ornitela, ww_file)),
  tar_target(removed_periods, remove_periods(ww_file, fixed_names)),
  tar_target(cleaned, clean_data(removed_periods)),
  tar_target(capture_sites, "data/capture_sites.csv", format = "file"),
  tar_target(carmel, "data/all_captures_carmel_2010-2021.csv", format = "file"),
  tar_target(removed_captures, remove_captures(capture_sites, carmel, cleaned)),
  tar_target(with_age_sex, attach_age_sex(removed_captures, ww_file)),
  tar_target(hires_tags, get_hires_tags(with_age_sex, tag_sns)),
  
  # High-frequency ACC
  ## 2024 period
  ### Get data
  tar_target(calibration_data, read_csv(here("ACC_algo_Marta_draft/Data/example_calibration.csv"))),
  tar_target(gv_model, readRDS(here("ACC_algo_Marta_draft/Data/gv_final_model_fit.rda"))),
  tar_target(data_files_2024, list.files(here("data/ACC/2024_hf_period/raw/"), pattern = ".csv", full.names = T)),
  tar_target(unobs_raw_acc_2024, purrr::list_rbind(map(data_files_2024, ~as.data.frame(data.table::fread(.x, select = c("Latitude", "Longitude", "UTC_datetime", "UTC_date", "UTC_time", "datatype", "device_id", "acc_x", "acc_y", "acc_z")))))),
  tar_target(test, unobs_raw_acc_2024[1:10,]),
  tar_target(a, split_data_fun_forloop(unobs_raw_acc_2024), iteration = "list"), # XXX this is called "a" because i couldn't get the damn pipeline to work. Just leave it for now.
  # tar_target(written_out, for(i in 1:length(split)){
  #   dev <- split[[i]]$device_id[1]  
  #   filename <- paste0("/", dev, ".csv")
  #   data.table::fwrite(split[[i]], 
  #                      file = paste0(here("data/ACC/2024_hf_period/created/devices"), 
  #                                    filename))
  # }),
  # tar_target(split_files_2024, list.files(here("data/ACC/2024_hf_period/created/devices/"), pattern = ".csv", full.names = T)),
  tar_target(split_subset, a[6:9], iteration = "list"),
  ### Classify data
  tar_target(prepared_2024, prepare_dataset(a, calibration = calibration_data),
             pattern = map(a), iteration = "list"),
  tar_target(bouts_2024, prepared_2024[,c("bout_id", "device_id", "start_int")] %>%
               group_by(device_id, bout_id) %>%
               summarize(start = min(start_int),
                         end = max(start_int),
                         .groups = "drop"),
             pattern = map(prepared_2024),
             iteration = "list"), 
  tar_target(predictions_2024, predict(gv_model, prepared_2024),
             pattern = map(prepared_2024),
             iteration = "list"),
  tar_target(scores_2024, predict(gv_model, prepared_2024, type = "prob"),
             pattern = map(prepared_2024),
             iteration = "list"),
  tar_target(bouts_predictions_2024, 
             distinct(get_bouts_predictions(prepared_2024,
                                   predictions_2024,
                                   scores_2024,
                                   bouts_2024)),
             pattern = map(prepared_2024, 
                           predictions_2024, 
                           scores_2024, 
                           bouts_2024),
             iteration = "list"),
  tar_target(ornitela_data_2024,
             vultureUtils::downloadVultures(loginObject = loginObject,
                                            removeDup = T, dfConvert = T,
                                            quiet = T,
                                            dateTimeStartUTC =
                                              lubridate::ymd_hms(min(unobs_raw_acc_2024$UTC_datetime)),
                                            dateTimeEndUTC =
                                              lubridate::ymd_hms(max(unobs_raw_acc_2024$UTC_datetime)))),
  tar_target(gps, dplyr::select(ornitela_data_2024, tag_id, timestamp, dateOnly, ground_speed, location_lat, location_long, individual_id, tag_local_identifier)),
  tar_target(device_ids, purrr::map(bouts_predictions_2024, ~.x$device_id[1]),
             iteration = "list"),
  tar_target(focal_gps, filter(gps, tag_local_identifier == device_ids),
             pattern = map(device_ids),
             iteration = "list"),
  tar_target(matches, get_matches(bouts_predictions_2024, focal_gps),
             pattern = map(bouts_predictions_2024, focal_gps),
             iteration = "list"),
  tar_target(joined, left_join(bouts_predictions_2024, matches,
                               by = c("device_id" = "tag_local_identifier",
                                      "bout_id")),
             pattern = map(bouts_predictions_2024, matches),
             iteration = "list"),
  ### Get feeding bouts
  tar_target(feeding_bouts_certain, filter(joined, pred == "Eating" &
                                             !is.na(location_lat) &
                                             .pred_Eating > 0.5) %>%
               sf::st_as_sf(coords = c("location_long", "location_lat"),
                            crs = "WGS84"),
             pattern = map(joined),
             iteration = "list"),
  ### Classify feeding station vs. not
  tar_target(fs, readxl::read_excel(here("data/FeedingData from 2018_2024_Translated.xlsx")) %>%
               rename("itmLong" = `ITM - LONG`,
                      "itmLat" = `ITM - LAT`,
                      "long" = `WGS84 - LONG`,
                      "lat" = `WGS84 - LAT`,
                      "date" = `Date Event`,
                      "time" = `Event time`) %>%
               select(long, lat, itmLong, itmLat, date, time) %>%
               mutate(datetime = lubridate::ymd_hms(paste0(date, substr(time, 12, 19)))) %>%
               distinct() %>%
               st_as_sf(coords = c("long", "lat"), crs = "WGS84") %>%
               st_transform(32636)),
  tar_target(fs_buffered, sf::st_buffer(fs, dist = 100)),
  tar_target(fs_union, sf::st_union(fs_buffered) %>% st_transform("WGS84")),
  tar_target(feeding_bouts_station, assign_fs(feeding_bouts_certain, fs_union),
             pattern = map(feeding_bouts_certain),
             iteration = "list")#,
  
  # ## 2023 period
  # ### Get data
  # tar_target(data_files_2023, list.files(here("data/ACC/2023_hf_period/raw/"), full.names = T)),
  # tar_target(unobs_raw_acc_2023, data.table::fread(data_files_2023[1], select = c("Latitude", "Longitude", "UTC_datetime", "UTC_date", "UTC_time", "datatype", "device_id", "acc_x", "acc_y", "acc_z"))),
  # tar_target(a_2023, split_data_fun_forloop(unobs_raw_acc_2023), iteration = "list"), # XXX this is called "a_2023" because i couldn't get the damn pipeline to work. Just leave it for now.
  # tar_target(split_subset_2023, a_2023[6:9], iteration = "list"),
  # ### Classify data
  # tar_target(prepared_2023, prepare_dataset(a_2023, 
  #                                           calibration = calibration_data),
  #            pattern = map(a_2023), iteration = "list"),
  # tar_target(bouts_2023, prepared_2023[,c("bout_id", "device_id", "start_int")] %>%
  #              group_by(device_id, bout_id) %>%
  #              summarize(start = min(start_int),
  #                        end = max(start_int),
  #                        .groups = "drop"),
  #            pattern = map(prepared_2023),
  #            iteration = "list"), 
  # tar_target(predictions_2023, predict(gv_model, prepared_2023),
  #            pattern = map(prepared_2023),
  #            iteration = "list"),
  # tar_target(scores_2023, predict(gv_model, prepared_2023, type = "prob"),
  #            pattern = map(prepared_2023),
  #            iteration = "list"),
  # tar_target(bouts_predictions_2023, 
  #            distinct(get_bouts_predictions(prepared_2023,
  #                                           predictions_2023,
  #                                           scores_2023,
  #                                           bouts_2023)),
  #            pattern = map(prepared_2023, 
  #                          predictions_2023, 
  #                          scores_2023, 
  #                          bouts_2023),
  #            iteration = "list"),
  # tar_target(ornitela_data_2023,
  #            vultureUtils::downloadVultures(loginObject = loginObject,
  #                                           removeDup = T, dfConvert = T,
  #                                           quiet = T,
  #                                           dateTimeStartUTC =
  #                                             lubridate::ymd_hms(min(unobs_raw_acc_2023$UTC_datetime)),
  #                                           dateTimeEndUTC =
  #                                             lubridate::ymd_hms(max(unobs_raw_acc_2023$UTC_datetime)))),
  # tar_target(gps_2023, dplyr::select(ornitela_data_2023, tag_id, timestamp, dateOnly, ground_speed, location_lat, location_long, individual_id, tag_local_identifier)),
  # tar_target(device_ids_2023, purrr::map(bouts_predictions_2023, ~.x$device_id[1]),
  #            iteration = "list"),
  # tar_target(focal_gps_2023, filter(gps_2023, tag_local_identifier == device_ids),
  #            pattern = map(device_ids_2023),
  #            iteration = "list"),
  # tar_target(matches_2023, get_matches(bouts_predictions_2023, focal_gps_2023),
  #            pattern = map(bouts_predictions_2023, focal_gps_2023),
  #            iteration = "list"),
  # tar_target(joined_2023, left_join(bouts_predictions_2023, matches,
  #                                   by = c("device_id" = "tag_local_identifier",
  #                                          "bout_id")),
  #            pattern = map(bouts_predictions_2023, matches),
  #            iteration = "list"),
  # ### Get feeding bouts
  # tar_target(feeding_bouts_certain_2023, filter(joined_2023, pred == "Eating" &
  #                                                 !is.na(location_lat) &
  #                                                 .pred_Eating > 0.5) %>%
  #              sf::st_as_sf(coords = c("location_long", "location_lat"),
  #                           crs = "WGS84"),
  #            pattern = map(joined_2023),
  #            iteration = "list"),
  # tar_target(feeding_bouts_station_2023, assign_fs(feeding_bouts_certain_2023, fs_union),
  #            pattern = map(feeding_bouts_certain_2023),
  #            iteration = "list")
)


