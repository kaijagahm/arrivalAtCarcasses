## 2023 period
### Get data
library(tidyverse)
library(data.table)
library(here)
library(ranger)
library(tidymodels)
library(moments)
library(parsnip)
library(caret)
library(zoo)
source(here("R/functions.R"))

data_files_2023 <- list.files(here("data/ACC/2023_hf_period/raw/"), full.names = T)

unobs_raw_acc_2023 <- data.table::fread(data_files_2023[1], select = c("Latitude", "Longitude", "UTC_datetime", "UTC_date", "UTC_time", "datatype", "device_id", "acc_x", "acc_y", "acc_z"))

a_2023 <- split_data_fun_forloop(unobs_raw_acc_2023)

calibration_data <- read_csv(here("ACC_algo_Marta_draft/Data/example_calibration.csv"))

prepared_2023 <- map(a_2023, ~prepare_dataset(.x, calibration = calibration_data),
                     .progress = T)

bouts_2023 <- map(prepared_2023, ~{
  .x[,c("bout_id", "device_id", "start_int")] %>%
    group_by(device_id, bout_id) %>%
    summarize(start = min(start_int),
              end = max(start_int),
              .groups = "drop")
}, .progress = T)

load(here("ACC_algo_Marta_draft/Data/gv_final_model_fit.rda"))

predictions_2023 <- map(prepared_2023, ~predict(gv_final_model_fit, .x),
                        .progress = T)

scores_2023 <- map(prepared_2023, ~predict(gv_final_model_fit, .x, type = "prob"), .progress = T)

bouts_predictions_2023 <- pmap(list(prepared_2023, predictions_2023, scores_2023, bouts_2023), ~{distinct(get_bouts_predictions(..1, ..2, ..3, ..4))})

#####
tar_target(ornitela_data_2023,
           vultureUtils::downloadVultures(loginObject = loginObject,
                                          removeDup = T, dfConvert = T,
                                          quiet = T,
                                          dateTimeStartUTC =
                                            lubridate::ymd_hms(min(unobs_raw_acc_2023$UTC_datetime)),
                                          dateTimeEndUTC =
                                            lubridate::ymd_hms(max(unobs_raw_acc_2023$UTC_datetime)))),
tar_target(gps_2023, dplyr::select(ornitela_data_2023, tag_id, timestamp, dateOnly, ground_speed, location_lat, location_long, individual_id, tag_local_identifier)),
tar_target(device_ids_2023, purrr::map(bouts_predictions_2023, ~.x$device_id[1]),
           iteration = "list"),
tar_target(focal_gps_2023, filter(gps_2023, tag_local_identifier == device_ids),
           pattern = map(device_ids_2023),
           iteration = "list"),
tar_target(matches_2023, get_matches(bouts_predictions_2023, focal_gps_2023),
           pattern = map(bouts_predictions_2023, focal_gps_2023),
           iteration = "list"),
tar_target(joined_2023, left_join(bouts_predictions_2023, matches,
                                  by = c("device_id" = "tag_local_identifier",
                                         "bout_id")),
           pattern = map(bouts_predictions_2023, matches),
           iteration = "list"),
### Get feeding bouts
tar_target(feeding_bouts_certain_2023, filter(joined_2023, pred == "Eating" &
                                                !is.na(location_lat) &
                                                .pred_Eating > 0.5) %>%
             sf::st_as_sf(coords = c("location_long", "location_lat"),
                          crs = "WGS84"),
           pattern = map(joined_2023),
           iteration = "list"),
tar_target(feeding_bouts_station_2023, assign_fs(feeding_bouts_certain_2023, fs_union),
           pattern = map(feeding_bouts_certain_2023),
           iteration = "list")