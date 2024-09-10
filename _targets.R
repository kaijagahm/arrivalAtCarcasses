# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  memory = "transient", 
  garbage_collection = TRUE,
  format = "qs",
  error = "null",
  seconds_meta_append = 15,
  seconds_reporter = 0.5,
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
  tar_target(split, group_split(unobs_raw_acc_2024 %>% group_by(device_id))),
  tar_target(written_out, for(i in 1:length(split)){
    dev <- split[[i]]$device_id[1]  
    filename <- paste0("/", dev, ".csv")
    data.table::fwrite(split[[i]], 
                       file = paste0(here("data/ACC/2024_hf_period/created/devices"), 
                                     filename))
  }),
  tar_target(split_files_2024, list.files(here("data/ACC/2024_hf_period/created/devices/"), pattern = ".csv", full.names = T)),
  tar_target(split_subset, split[6:7]),
  ### Classify data
  tar_target(prepared_2024, prepare_forloop(split_subset, cal = calibration_data)),
  tar_target(bouts_2024, purrr::map(prepared_2024, ~{
    .x %>%
      dplyr::select(bout_id, device_id, start_int) %>%
      dplyr::group_by(device_id, bout_id) %>%
      dplyr::summarize(start = min(start_int),
                       end = max(start_int)) %>%
      dplyr::ungroup()
  })),
  tar_target(predictions_2024, purrr::map(prepared_2024, 
                                          ~stats::predict(gv_model, .x))),
  tar_target(scores_2024, purrr::map(prepared_2024, 
                                     ~stats::predict(gv_model, .x, type = "prob"))),
  tar_target(bouts_predictions_2024, purrr::pmap(.l = list(prepared_2024, predictions_2024, scores_2024, bouts_2024), ~get_bouts_predictions(..1, ..2, ..3, ..4))),
  tar_target(ornitela_data_2024, 
             vultureUtils::downloadVultures(loginObject = loginObject,
                                            removeDup = T, dfConvert = T,
                                            quiet = T,
                                            dateTimeStartUTC = 
                                              lubridate::ymd_hms(min(unobs_raw_acc_2024$UTC_datetime)),
                                            dateTimeEndUTC = 
                                              lubridate::ymd_hms(max(unobs_raw_acc_2024$UTC_datetime)))),
  tar_target(gps, dplyr::select(ornitela_data_2024, tag_id, timestamp, dateOnly, ground_speed, location_lat, location_long, individual_id, tag_local_identifier)),
  tar_target(device_ids, purrr::map_dbl(bouts_predictions_2024, ~.x$device_id[1])),
  tar_target(focal_gps, purrr::map(device_ids, ~gps %>%
                                     filter(tag_local_identifier == .x))),
  tar_target(bouts_predictions_2024_distinct, map(bouts_predictions_2024, distinct)),
  tar_target(matches, map2(bouts_predictions_2024_distinct, focal_gps, ~{
    gps_matches <- vector(mode = "list", length = nrow(.x))
    for(i in 1:nrow(.x)){
      bout <- .x$bout_id[i]
      start_time <- lubridate::ymd_hms(.x$start[i])
      end_time <- lubridate::ymd_hms(.x$end[i])
      middle <- start_time + difftime(end_time, start_time)/2
      # "First, if they were collected within 5 min of each other, and if the GPS ground speed was below 4m/sec (indicating the bird was not flying)."
      before_5min <- start_time-minutes(5)
      after_5min <- end_time+minutes(5)
      within_5min_gs <- .y %>%
        filter(before_5min <= timestamp & timestamp <= after_5min) %>%
        filter(ground_speed < 4)
      if(nrow(within_5min_gs) > 0){
        match <- within_5min_gs
      }else{
        #Second, if no GPS position matched these criteria, we matched ACC bouts with GPS locations if they were collected within 11 min of each other (while maintaining the ground speed criteria).
        before_11min <- start_time-minutes(11)
        after_11min <- end_time+minutes(11)
        within_11min_gs <- .y %>%
          filter(before_11min <= timestamp & timestamp <= after_11min) %>%
          filter(ground_speed < 4)
        if(nrow(within_11min_gs) > 0){
          match <- within_11min_gs
        }else{
          # Third, if no position matched the previous two filters, we used the 5 min time frame, but excluded the ground speed filter (rarely, very short feeding events may occur during the interval between two GPS locations indicating flight, or between two GPS locations when one was on the ground and the following was flying).
          within_5min <- .y %>%
            filter(before_5min <= timestamp & timestamp <= after_5min)
          if(nrow(within_5min) > 0){
            match <- within_5min
          }else{
            match <- .y[0,]
            cat("No match found\n")
          }
        }
      }
      match <- match %>%
        mutate(bout_id = bout)
      if(nrow(match) > 1){
        match <- match[which.min(abs(match$timestamp - middle)),]
      }
      gps_matches[[i]] <- match
      cat("Completed bout", i, "\n")
    }
    return(gps_matches)
  })),
  tar_target(matches_df, map(matches, ~purrr::list_rbind(.x))),
  tar_target(joined, map2(bouts_predictions_2024_distinct, matches_df, ~{
    left_join(.x, .y, by = c("device_id" = "tag_local_identifier",
                             "bout_id"))
  })),
  ### Get feeding bouts
  tar_target(feeding_bouts_certain, map(joined, ~filter(.x, pred == "Eating" & 
                                                          !is.na(location_lat) &
                                                          .pred_Eating > 0.5) %>% 
                                          sf::st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84"))),
  ### Classify feeding station vs. not
  tar_target(fs, readxl::read_excel(here("data/FeedingData from 2018_2024_Translated.xlsx")) %>%
               dplyr::select(contains("LONG") | contains("LAT")) %>%
               rename("itmLong" = `ITM - LONG`,
                      "itmLat" = `ITM - LAT`,
                      "long" = `WGS84 - LONG`,
                      "lat" = `WGS84 - LAT`) %>%
               select(long, lat, itmLong, itmLat) %>%
               distinct() %>%
               st_as_sf(coords = c("long", "lat"), crs = "WGS84") %>%
               st_transform(32636)),
  tar_target(fs_buffered, sf::st_buffer(fs, dist = 100)),
  tar_target(fs_union, sf::st_union(fs_buffered) %>% st_transform("WGS84")),
  tar_target(feeding_bouts_station, map(feeding_bouts_certain, ~{
    .x$station <- !is.na(as.numeric(st_intersects(.x, fs_union)))
    return(.x)
  }))
  # XXX START HERE--need to properly clean the GPS data before classifying it.
)



