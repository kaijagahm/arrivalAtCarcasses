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
  packages = c("plyr", "vultureUtils", "tidyverse", "here", "NBDA", "sf", "dplyr", "lubridate", "ranger", "tidymodels", "moments", "parsnip", "caret", "zoo", "move", "terra"),
  controller = crew_controller_local(workers = 6)
)

lapply(list.files("R", full.names = TRUE), source) 

list(
  # tar_target(pw, "data/movebankCredentials/pw.Rda", format = "file"),
  # tar_target(loginObject, get_loginObject(pw)),
  # tar_target(ww_file, "data/raw/whoswho_vultures_20230920_new.xlsx", format = "file"),
  ## ACC data files for classifying the bouts
  tar_target(data_files_2022, list.files(here("data/ACC/2022_hf_period/raw/"), full.names = T, pattern = ".csv")),
  tar_target(data_files_2023, list.files(here("data/ACC/2023_hf_period/raw/"), full.names = T, pattern = ".csv")),
  tar_target(data_files_2024, list.files(here("data/ACC/2024_hf_period/raw/"), full.names = T, pattern = ".csv")),
  tar_target(unobs_raw_acc_2022, get_acc_data(data_files_2022)),
  tar_target(unobs_raw_acc_2023, get_acc_data(data_files_2023)),
  tar_target(unobs_raw_acc_2024, get_acc_data(data_files_2024)),
  tar_target(acc_2022_flipped, flip_devices(unobs_raw_acc_2022)),
  tar_target(acc_2023_flipped, flip_devices(unobs_raw_acc_2023)),
  tar_target(acc_2024_flipped, flip_devices(unobs_raw_acc_2024)),
  tar_target(mindate_22, lubridate::ymd_hms(min(acc_2022_flipped$UTC_datetime))),
  tar_target(maxdate_22, lubridate::ymd_hms(max(acc_2022_flipped$UTC_datetime)) + lubridate::days(5)),
  tar_target(mindate_23, lubridate::ymd_hms(min(acc_2023_flipped$UTC_datetime))),
  tar_target(maxdate_23, lubridate::ymd_hms(max(acc_2023_flipped$UTC_datetime)) + lubridate::days(5)),
  tar_target(mindate_24, lubridate::ymd_hms(min(acc_2024_flipped$UTC_datetime))),
  tar_target(maxdate_24, lubridate::ymd_hms(max(acc_2024_flipped$UTC_datetime)) + lubridate::days(5)),
  tar_target(minmax_dates, list(mindate_22, maxdate_22, mindate_23, maxdate_23, mindate_24, maxdate_24)),
  
  ## Calibration
  tar_target(calibration_data, read_csv(here("ACC_algo_Marta_draft/Data/example_calibration.csv"))),
  tar_target(splitup_22, group_split(group_by(as.data.frame(acc_2022_flipped)), device_id)),
  tar_target(splitup_23, group_split(group_by(as.data.frame(acc_2023_flipped)), device_id)),
  tar_target(splitup_24, group_split(group_by(as.data.frame(acc_2024_flipped)), device_id)),
  tar_target(calibrated_22_1, calibrate_devices(splitup_22[1:10], calibration_data)),
  tar_target(calibrated_22_2, calibrate_devices(splitup_22[11:20], calibration_data)),
  tar_target(calibrated_22_3, calibrate_devices(splitup_22[21:30], calibration_data)),
  tar_target(calibrated_22_4, calibrate_devices(splitup_22[31:40], calibration_data)),
  tar_target(calibrated_22_5, calibrate_devices(splitup_22[41:50], calibration_data)),
  tar_target(calibrated_22_6, calibrate_devices(splitup_22[51:60], calibration_data)),
  tar_target(calibrated_22_7, calibrate_devices(splitup_22[61:70], calibration_data)),
  tar_target(calibrated_22_8, calibrate_devices(splitup_22[71:length(splitup_22)], calibration_data)),
  tar_target(calibrated_23_1, calibrate_devices(splitup_22[1:10], calibration_data)),
  tar_target(calibrated_23_2, calibrate_devices(splitup_23[11:20], calibration_data)),
  tar_target(calibrated_23_3, calibrate_devices(splitup_23[21:30], calibration_data)),
  tar_target(calibrated_23_4, calibrate_devices(splitup_23[31:40], calibration_data)),
  tar_target(calibrated_23_5, calibrate_devices(splitup_23[41:50], calibration_data)),
  tar_target(calibrated_23_6, calibrate_devices(splitup_23[51:60], calibration_data)),
  tar_target(calibrated_23_7, calibrate_devices(splitup_23[61:70], calibration_data)),
  tar_target(calibrated_23_8, calibrate_devices(splitup_23[71:length(splitup_23)], calibration_data)),
  tar_target(calibrated_24_1, calibrate_devices(splitup_24[1:10], calibration_data)),
  tar_target(calibrated_24_2, calibrate_devices(splitup_24[11:20], calibration_data)),
  tar_target(calibrated_24_3, calibrate_devices(splitup_24[21:30], calibration_data)),
  tar_target(calibrated_24_4, calibrate_devices(splitup_24[31:40], calibration_data)),
  tar_target(calibrated_24_5, calibrate_devices(splitup_24[41:50], calibration_data)),
  tar_target(calibrated_24_6, calibrate_devices(splitup_24[51:60], calibration_data)),
  tar_target(calibrated_24_7, calibrate_devices(splitup_24[61:70], calibration_data)),
  tar_target(calibrated_24_8, calibrate_devices(splitup_24[71:length(splitup_24)], calibration_data)),
  
  tar_target(cal_22_1, map(calibrated_22_1, distinct)),
  tar_target(cal_22_2, map(calibrated_22_2, distinct)),
  tar_target(cal_22_3, map(calibrated_22_3, distinct)),
  tar_target(cal_22_4, map(calibrated_22_4, distinct)),
  tar_target(cal_22_5, map(calibrated_22_5, distinct)),
  tar_target(cal_22_6, map(calibrated_22_6, distinct)),
  tar_target(cal_22_7, map(calibrated_22_7, distinct)),
  tar_target(cal_22_8, map(calibrated_22_8, distinct)),
  
  tar_target(cal_23_1, map(calibrated_23_1, distinct)),
  tar_target(cal_23_2, map(calibrated_23_2, distinct)),
  tar_target(cal_23_3, map(calibrated_23_3, distinct)),
  tar_target(cal_23_4, map(calibrated_23_4, distinct)),
  tar_target(cal_23_5, map(calibrated_23_5, distinct)),
  tar_target(cal_23_6, map(calibrated_23_6, distinct)),
  tar_target(cal_23_7, map(calibrated_23_7, distinct)),
  tar_target(cal_23_8, map(calibrated_23_8, distinct)),
  
  tar_target(cal_24_1, map(calibrated_24_1, distinct)),
  tar_target(cal_24_2, map(calibrated_24_2, distinct)),
  tar_target(cal_24_3, map(calibrated_24_3, distinct)),
  tar_target(cal_24_4, map(calibrated_24_4, distinct)),
  tar_target(cal_24_5, map(calibrated_24_5, distinct)),
  tar_target(cal_24_6, map(calibrated_24_6, distinct)),
  tar_target(cal_24_7, map(calibrated_24_7, distinct)),
  tar_target(cal_24_8, map(calibrated_24_8, distinct)),
  
  tar_target(bouts_22_1, map(cal_22_1, get_bouts)),
  tar_target(bouts_22_2, map(cal_22_2, get_bouts)),
  tar_target(bouts_22_3, map(cal_22_3, get_bouts)),
  tar_target(bouts_22_4, map(cal_22_4, get_bouts)),
  tar_target(bouts_22_5, map(cal_22_5, get_bouts)),
  tar_target(bouts_22_6, map(cal_22_6, get_bouts)),
  tar_target(bouts_22_7, map(cal_22_7, get_bouts)),
  tar_target(bouts_22_8, map(cal_22_8, get_bouts)),
  tar_target(bouts_23_1, map(cal_23_1, get_bouts)),
  tar_target(bouts_23_2, map(cal_23_2, get_bouts)),
  tar_target(bouts_23_3, map(cal_23_3, get_bouts)),
  tar_target(bouts_23_4, map(cal_23_4, get_bouts)),
  tar_target(bouts_23_5, map(cal_23_5, get_bouts)),
  tar_target(bouts_23_6, map(cal_23_6, get_bouts)),
  tar_target(bouts_23_7, map(cal_23_7, get_bouts)),
  tar_target(bouts_23_8, map(cal_23_8, get_bouts)),
  tar_target(bouts_24_1, map(cal_24_1, get_bouts)),
  tar_target(bouts_24_2, map(cal_24_2, get_bouts)),
  tar_target(bouts_24_3, map(cal_24_3, get_bouts)),
  tar_target(bouts_24_4, map(cal_24_4, get_bouts)),
  tar_target(bouts_24_5, map(cal_24_5, get_bouts)),
  tar_target(bouts_24_6, map(cal_24_6, get_bouts)),
  tar_target(bouts_24_7, map(cal_24_7, get_bouts)),
  tar_target(bouts_24_8, map(cal_24_8, get_bouts)),
  
  # The predictions didn't work for some reason, even though the same code used to work fine, so I'm going to derive predictions from the scores objects by just taking the highest one
  tar_target(scores_22_1, map(cal_22_1, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_22_2, map(cal_22_2, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_22_3, map(cal_22_3, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_22_4, map(cal_22_4, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_22_5, map(cal_22_5, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_22_6, map(cal_22_6, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_22_7, map(cal_22_7, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_22_8, map(cal_22_8, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_23_1, map(cal_23_1, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_23_2, map(cal_23_2, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_23_3, map(cal_23_3, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_23_4, map(cal_23_4, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_23_5, map(cal_23_5, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_23_6, map(cal_23_6, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_23_7, map(cal_23_7, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_23_8, map(cal_23_8, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_24_1, map(cal_24_1, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_24_2, map(cal_24_2, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_24_3, map(cal_24_3, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_24_4, map(cal_24_4, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_24_5, map(cal_24_5, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_24_6, map(cal_24_6, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_24_7, map(cal_24_7, ~get_scores(.x, mod = classification_model))),
  tar_target(scores_24_8, map(cal_24_8, ~get_scores(.x, mod = classification_model))),
  
  tar_target(preds_22_1, map(scores_22_1, get_preds_from_scores)),
  tar_target(preds_22_2, map(scores_22_2, get_preds_from_scores)),
  tar_target(preds_22_3, map(scores_22_3, get_preds_from_scores)),
  tar_target(preds_22_4, map(scores_22_4, get_preds_from_scores)),
  tar_target(preds_22_5, map(scores_22_5, get_preds_from_scores)),
  tar_target(preds_22_6, map(scores_22_6, get_preds_from_scores)),
  tar_target(preds_22_7, map(scores_22_7, get_preds_from_scores)),
  tar_target(preds_22_8, map(scores_22_8, get_preds_from_scores)),
  tar_target(preds_23_1, map(scores_23_1, get_preds_from_scores)), # 
  tar_target(preds_23_2, map(scores_23_2, get_preds_from_scores)), # 
  tar_target(preds_23_3, map(scores_23_3, get_preds_from_scores)), # 
  tar_target(preds_23_4, map(scores_23_4, get_preds_from_scores)), # 
  tar_target(preds_23_5, map(scores_23_5, get_preds_from_scores)),
  tar_target(preds_23_6, map(scores_23_6, get_preds_from_scores)),
  tar_target(preds_23_7, map(scores_23_7, get_preds_from_scores)), # 
  tar_target(preds_23_8, map(scores_23_8, get_preds_from_scores)), # 
  tar_target(preds_24_1, map(scores_24_1, get_preds_from_scores)), # 
  tar_target(preds_24_2, map(scores_24_2, get_preds_from_scores)), # 
  tar_target(preds_24_3, map(scores_24_3, get_preds_from_scores)), # 
  tar_target(preds_24_4, map(scores_24_4, get_preds_from_scores)), # 
  tar_target(preds_24_5, map(scores_24_5, get_preds_from_scores)), # 
  tar_target(preds_24_6, map(scores_24_6, get_preds_from_scores)), # 
  tar_target(preds_24_7, map(scores_24_7, get_preds_from_scores)), # 
  tar_target(preds_24_8, map(scores_24_8, get_preds_from_scores)), # 
  
  tar_target(bouts_22, c(bouts_22_1, bouts_22_2, bouts_22_3, bouts_22_4, bouts_22_5, bouts_22_6, bouts_22_7, bouts_22_8)),
  tar_target(bouts_23, c(bouts_23_1, bouts_23_2, bouts_23_3, bouts_23_4, bouts_23_5, bouts_23_6, bouts_23_7, bouts_23_8)),
  tar_target(bouts_24, c(bouts_24_1, bouts_24_2, bouts_24_3, bouts_24_4, bouts_24_5, bouts_24_6, bouts_24_7, bouts_24_8)),
  tar_target(scores_22, c(scores_22_1, scores_22_2, scores_22_3, scores_22_4, scores_22_5, scores_22_6, scores_22_7, scores_22_8)),
  tar_target(scores_23, c(scores_23_1, scores_23_2, scores_23_3, scores_23_4, scores_23_5, scores_23_6, scores_23_7, scores_23_8)),
  tar_target(scores_24, c(scores_24_1, scores_24_2, scores_24_3, scores_24_4, scores_24_5, scores_24_6, scores_24_7, scores_24_8)),
  
  ## Get classification model
  tar_target(classification_model, readRDS(here("ACC_algo_Marta_draft/Data/gv_final_model_fit.rda"))),
  
  tar_target(bouts_predictions_22_1, pmap(.l = list(cal_22_1, preds_22_1, scores_22_1, bouts_22_1), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_22_2, pmap(.l = list(cal_22_2, preds_22_2, scores_22_2, bouts_22_2), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_22_3, pmap(.l = list(cal_22_3, preds_22_3, scores_22_3, bouts_22_3), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_22_4, pmap(.l = list(cal_22_4, preds_22_4, scores_22_4, bouts_22_4), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_22_5, pmap(.l = list(cal_22_5, preds_22_5, scores_22_5, bouts_22_5), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_22_6, pmap(.l = list(cal_22_6, preds_22_6, scores_22_6, bouts_22_6), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_22_7, pmap(.l = list(cal_22_7, preds_22_7, scores_22_7, bouts_22_7), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_22_8, pmap(.l = list(cal_22_8, preds_22_8, scores_22_8, bouts_22_8), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_23_1, pmap(.l = list(cal_23_1, preds_23_1, scores_23_1, bouts_23_1), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_23_2, pmap(.l = list(cal_23_2, preds_23_2, scores_23_2, bouts_23_2), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_23_3, pmap(.l = list(cal_23_3, preds_23_3, scores_23_3, bouts_23_3), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_23_4, pmap(.l = list(cal_23_4, preds_23_4, scores_23_4, bouts_23_4), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_23_5, pmap(.l = list(cal_23_5, preds_23_5, scores_23_5, bouts_23_5), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_23_6, pmap(.l = list(cal_23_6, preds_23_6, scores_23_6, bouts_23_6), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_23_7, pmap(.l = list(cal_23_7, preds_23_7, scores_23_7, bouts_23_7), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_23_8, pmap(.l = list(cal_23_8, preds_23_8, scores_23_8, bouts_23_8), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_24_1, pmap(.l = list(cal_24_1, preds_24_1, scores_24_1, bouts_24_1), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_24_2, pmap(.l = list(cal_24_2, preds_24_2, scores_24_2, bouts_24_2), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_24_3, pmap(.l = list(cal_24_3, preds_24_3, scores_24_3, bouts_24_3), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_24_4, pmap(.l = list(cal_24_4, preds_24_4, scores_24_4, bouts_24_4), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_24_5, pmap(.l = list(cal_24_5, preds_24_5, scores_24_5, bouts_24_5), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_24_6, pmap(.l = list(cal_24_6, preds_24_6, scores_24_6, bouts_24_6), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_24_7, pmap(.l = list(cal_24_7, preds_24_7, scores_24_7, bouts_24_7), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bouts_predictions_24_8, pmap(.l = list(cal_24_8, preds_24_8, scores_24_8, bouts_24_8), .f = ~get_bouts_predictions(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  
  tar_target(bouts_predictions_2022, c(bouts_predictions_22_1, bouts_predictions_22_2, bouts_predictions_22_3, bouts_predictions_22_4, bouts_predictions_22_5, bouts_predictions_22_6, bouts_predictions_22_7, bouts_predictions_22_8)),
  tar_target(bouts_predictions_2023, c(bouts_predictions_23_1, bouts_predictions_23_2, bouts_predictions_23_3, bouts_predictions_23_4, bouts_predictions_23_5, bouts_predictions_23_6, bouts_predictions_23_7, bouts_predictions_23_8)),
  tar_target(bouts_predictions_2024, c(bouts_predictions_24_1, bouts_predictions_24_2, bouts_predictions_24_3, bouts_predictions_24_4, bouts_predictions_24_5, bouts_predictions_24_6, bouts_predictions_24_7, bouts_predictions_24_8)),
  # Get the individual IDs so we can match them to gps points
  tar_target(device_ids_2022, purrr::map(bouts_predictions_2022, ~.x$device_id[1])),
  tar_target(device_ids_2023, purrr::map(bouts_predictions_2023, ~.x$device_id[1])),
  tar_target(device_ids_2024, purrr::map(bouts_predictions_2024, ~.x$device_id[1])),
  tar_target(gps_focal_indivs_2022, get_gps_forbouts_indivs(device_ids_2022, gps_2022)),
  tar_target(gps_focal_indivs_2023, get_gps_forbouts_indivs(device_ids_2023, gps_2023)),
  tar_target(gps_focal_indivs_2024, get_gps_forbouts_indivs(device_ids_2024, gps_2024)),
  tar_target(gps_spd, 4),
  tar_target(wg22_1, purrr::map2(bouts_predictions_2022[1:10], gps_focal_indivs_2022[1:10], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_2, purrr::map2(bouts_predictions_2022[11:20], gps_focal_indivs_2022[11:20], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_3, purrr::map2(bouts_predictions_2022[21:30], gps_focal_indivs_2022[21:30], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_4, purrr::map2(bouts_predictions_2022[31:40], gps_focal_indivs_2022[31:40], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_5, purrr::map2(bouts_predictions_2022[41:50], gps_focal_indivs_2022[41:50], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_6, purrr::map2(bouts_predictions_2022[51:60], gps_focal_indivs_2022[51:60], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_7, purrr::map2(bouts_predictions_2022[61:70], gps_focal_indivs_2022[61:70], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_8, purrr::map2(bouts_predictions_2022[71:length(bouts_predictions_2022)], gps_focal_indivs_2022[71:length(gps_focal_indivs_2022)], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_1, purrr::map2(bouts_predictions_2023[1:10], gps_focal_indivs_2023[1:10], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_2, purrr::map2(bouts_predictions_2023[11:20], gps_focal_indivs_2023[11:20], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_3, purrr::map2(bouts_predictions_2023[21:30], gps_focal_indivs_2023[21:30], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_4, purrr::map2(bouts_predictions_2023[31:40], gps_focal_indivs_2023[31:40], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_5, purrr::map2(bouts_predictions_2023[41:50], gps_focal_indivs_2023[41:50], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_6, purrr::map2(bouts_predictions_2023[51:60], gps_focal_indivs_2023[51:60], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_7, purrr::map2(bouts_predictions_2023[61:70], gps_focal_indivs_2023[61:70], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_8, purrr::map2(bouts_predictions_2023[71:length(bouts_predictions_2023)], gps_focal_indivs_2023[71:length(gps_focal_indivs_2023)], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_1, purrr::map2(bouts_predictions_2024[1:10], gps_focal_indivs_2024[1:10], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_2, purrr::map2(bouts_predictions_2024[11:20], gps_focal_indivs_2024[11:20], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_3, purrr::map2(bouts_predictions_2024[21:30], gps_focal_indivs_2024[21:30], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_4, purrr::map2(bouts_predictions_2024[31:40], gps_focal_indivs_2024[31:40], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_5, purrr::map2(bouts_predictions_2024[41:50], gps_focal_indivs_2024[41:50], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_6, purrr::map2(bouts_predictions_2024[51:60], gps_focal_indivs_2024[51:60], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_7, purrr::map2(bouts_predictions_2024[61:70], gps_focal_indivs_2024[61:70], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_8, purrr::map2(bouts_predictions_2024[71:length(bouts_predictions_2024)], gps_focal_indivs_2024[71:length(gps_focal_indivs_2024)], ~get_matches(.x, .y, gps_spd))),
  
  tar_target(with_gps_2022, c(wg22_1, wg22_2, wg22_3, wg22_4, wg22_5, wg22_6, wg22_7, wg22_8)),
  tar_target(with_gps_2023, c(wg23_1, wg23_2, wg23_3, wg23_4, wg23_5, wg23_6, wg23_7, wg23_8)),
  tar_target(with_gps_2024, c(wg24_1, wg24_2, wg24_3, wg24_4, wg24_5, wg24_6, wg24_7, wg24_8)),
  
  ## Attach the gps data back to the bouts and predictions
  tar_target(full_2022, map2(bouts_predictions_2022, with_gps_2022, ~join_gps_bouts(.x, .y))),
  tar_target(full_2023, map2(bouts_predictions_2023, with_gps_2023, ~join_gps_bouts(.x, .y))),
  tar_target(full_2024, map2(bouts_predictions_2024, with_gps_2024, ~join_gps_bouts(.x, .y))),

  # ## GPS data for the focal periods (in case we need it later)
  # tar_target(focal_gps_2023, readRDS(here("data/ACC/2023_hf_period/created/focal_gps_2023.RDS"))),
  # tar_target(focal_gps_2024, readRDS(here("data/ACC/2024_hf_period/created/focal_gps_2024.RDS"))),
  
  ## Feeding bouts (high-frequency periods only)
  tar_target(feeding_bouts_prob_thresh, 0.75),
  tar_target(feeding_bouts_2022, map(full_2022, ~getfeeding(.x, feeding_bouts_prob_thresh))),
  tar_target(feeding_bouts_2023, map(full_2023, ~getfeeding(.x, feeding_bouts_prob_thresh))),
  tar_target(feeding_bouts_2024, map(full_2024, ~getfeeding(.x, feeding_bouts_prob_thresh))),
  
  ## Bind them together to get all feeding bouts
  tar_target(feeding_bouts, mutate(bind_rows(data.table::rbindlist(feeding_bouts_2022), data.table::rbindlist(feeding_bouts_2023), data.table::rbindlist(feeding_bouts_2024)), boutID = paste(device_id, bout_id, sep = "_"))),
  tar_target(feeding_bouts_spatial, st_transform(sf::st_as_sf(feeding_bouts, coords = c("location_long", "location_lat"), crs = "WGS84"), 32636)),
  
  ## Further restrictions on feeding bouts
  ### 1. Must be non-flight--mostly taken care of in the GPS matching, but occasionally we kept something with a higher ground speed. Let's remove those.
  tar_target(feeding_bouts_stationary, dplyr::filter(feeding_bouts_spatial, ground_speed <= gps_spd)),
  ### 2. Must not be on cliffs. For now, I'm going to use a 100m buffer for the linestrings
  # tar_target(cliffs, sf::st_read(here("data/raw/BNTL202203_Cliff/"))),
  # tar_target(cliffs_buffer_m, 100),
  # tar_target(cliffs_buffered, buffer_cliffs(cliffs, cliffs_buffer_m, 32636)),
  # tar_target(feeding_bouts_nocliffs, remove_bouts_on_cliffs(feeding_bouts_stationary, cliffs_buffered)),
  
  ## Using DEM to remove "feeding bouts" that are too much on a slope
  tar_target(filenames, list.files(here("data/raw/DEMs/ASTER/"), pattern = ".tif", full.names = T)),
  tar_target(feeding_bouts_stationary_withslopes, get_slopes(filenames, bbox_south_big, neighbors = 8, feeding_bouts_stationary)),
  tar_target(feeding_bouts_noslope_15, filter(feeding_bouts_stationary_withslopes, slope < 15)),
  tar_target(feeding_bouts_noslope_10, filter(feeding_bouts_stationary_withslopes, slope < 10)),
  tar_target(feeding_bouts_noslope_5, filter(feeding_bouts_stationary_withslopes, slope < 5)),
  
  ## Feeding stations
  ### Created in 00_carcass_data_translation.R
  ### Only spatial, not time-restricted.
  tar_target(stations, readRDS(here("data/created/stations.RDS"))),
  
  ## INPA carcasses
  ### Created in 00_carcass_data_translation.R
  tar_target(carcasses_audited, readRDS(here("data/created/carcasses_audited.RDS"))),
  
  ## Focal carcasses
  ### During the 2022, 2023 and 2024 HF-ACC periods
  tar_target(carcasses_focal, get_focal(carcasses_audited, minmax_dates)),
  
  ## Match bouts to carcasses
  tar_target(dist_bouts_carcasses, 750), # xxx seems maybe too high
  tar_target(hours_after_carcass, 72),
  tar_target(carcass_bouts_15, get_carcass_bouts(bouts = feeding_bouts_noslope_15,
                                              carcasses = carcasses_focal,
                                              dist = dist_bouts_carcasses,
                                              hours_after = hours_after_carcass)),
  tar_target(carcass_bouts_10, get_carcass_bouts(bouts = feeding_bouts_noslope_10,
                                                 carcasses = carcasses_focal,
                                                 dist = dist_bouts_carcasses,
                                                 hours_after = hours_after_carcass)),
  tar_target(carcass_bouts_5, get_carcass_bouts(bouts = feeding_bouts_noslope_5,
                                                 carcasses = carcasses_focal,
                                                 dist = dist_bouts_carcasses,
                                                 hours_after = hours_after_carcass)),
  tar_target(carcass_bouts_df_15, purrr::list_rbind(carcass_bouts_15)), # note: each bout might be affiliated with more than one carcass here!
  tar_target(carcass_bouts_df_10, purrr::list_rbind(carcass_bouts_10)), # note: each bout might be affiliated with more than one carcass here!
  tar_target(carcass_bouts_df_5, purrr::list_rbind(carcass_bouts_5)), # note: each bout might be affiliated with more than one carcass here!
  tar_target(non_carcass_bouts_15, filter(feeding_bouts_noslope_15, !(boutID %in% carcass_bouts_df_15$boutID))),
  tar_target(non_carcass_bouts_10, filter(feeding_bouts_noslope_10, !(boutID %in% carcass_bouts_df_10$boutID))),
  tar_target(non_carcass_bouts_5, filter(feeding_bouts_noslope_5, !(boutID %in% carcass_bouts_df_5$boutID))),
  
  ## Cluster the remaining bouts to detect wild carcasses
  tar_target(dist_bouts_wild_carcass_cluster, 200), 
  tar_target(time_bouts_wild_carcass_cluster, '12 hours'), # note: cannot be more than 24 hours. If we want more than 24 hours, we need to do this grouping a different way.
  tar_target(wild_carcass_bouts_df_15, get_wild_carcass_bouts(non_carcass_bouts_15,
                                                           time = time_bouts_wild_carcass_cluster,
                                                           dist = dist_bouts_wild_carcass_cluster,
                                                           minBouts = 3,
                                                           stations = stations,
                                                           stationDist = 750,
                                                           minIndivs = 3)),
  tar_target(wild_carcass_bouts_df_10, get_wild_carcass_bouts(non_carcass_bouts_10,
                                                           time = time_bouts_wild_carcass_cluster,
                                                           dist = dist_bouts_wild_carcass_cluster,
                                                           minBouts = 3,
                                                           stations = stations,
                                                           stationDist = 750,
                                                           minIndivs = 3)),
  tar_target(wild_carcass_bouts_df_5, get_wild_carcass_bouts(non_carcass_bouts_5,
                                                              time = time_bouts_wild_carcass_cluster,
                                                              dist = dist_bouts_wild_carcass_cluster,
                                                              minBouts = 3,
                                                              stations = stations,
                                                              stationDist = 750,
                                                              minIndivs = 3)),
  tar_target(wild_carcasses_15, get_wild_carcasses(wild_carcass_bouts_df_15) %>%
               mutate(carcType = "wild")),
  tar_target(wild_carcasses_10, get_wild_carcasses(wild_carcass_bouts_df_10) %>%
               mutate(carcType = "wild")),
  tar_target(wild_carcasses_5, get_wild_carcasses(wild_carcass_bouts_df_5) %>%
               mutate(carcType = "wild")),
  tar_target(wild_carcass_bouts_again, assign_time_dist(wild_carcass_bouts_df_10, wild_carcasses_10)),
  tar_target(carcass_bouts_dedup, group_by(carcass_bouts_df_10, boutID) %>%
               arrange(boutID, time_since_carcass) %>%
               slice(1) %>%
               ungroup()),  # Rule: each duplicated bout is assigned to the carcass for which it is closer to the time of carcass placement
  tar_target(all_bouts_assigned, combine_all_bouts(carcass_bouts_dedup, wild_carcass_bouts_again, feeding_bouts)),
  ## Combine carcasses
  tar_target(carcasses_focal_withstats, get_bout_stats(carcasses_focal, carcass_bouts_df_10)),
  tar_target(all_carcasses, bind_rows(carcasses_focal_withstats %>% mutate(carcType = "inpa", year = lubridate::year(date)) %>% dplyr::select(-starts_with("n_")), wild_carcasses_10 %>% dplyr::mutate("date" = lubridate::ymd(dateOnly)) %>% dplyr::select(-dateOnly))),
  
  tar_target(bbox_south_big, sf::st_transform(st_as_sfc(st_set_crs(st_bbox(c("xmin" = 34.205, 
                                               "xmax" = 35.787,
                                               "ymin" = 29.478, 
                                               "ymax" = 31.775)), "WGS84")), 32636)),
  ## Dynamic NBDA testing
  ## 0. Define parameters
  tar_target(days_after, 3),
  tar_target(days_before, 1),
  tar_target(days_before_wild, 3),
  tar_target(seed_distance_flight, 2000),
  tar_target(seed_distance_stationary, 1000),
  tar_target(seed_time_before, lubridate::minutes(30)),
  tar_target(detection_distance_flight, 2000),
  tar_target(detection_distance_stationary, 1000),
  ## 1. Get carcasses and restrict to south
  tar_target(all_carcasses_cropped, sf::st_crop(all_carcasses, bbox_south_big)), # XXX I'm not sure we want to crop these so tightly. Let's rethink this. If we do mapview(all_carcasses)+mapview(all_carcasses_cropped, col.regions = "red"), we see that this cuts off some of the carcasses in the south quite arbitrarily. Will need to change this. We do want to crop it to the south generally and avoid anything super far away, but the bounding box needs to change.
  tar_target(all_bouts_cropped, sf::st_crop(all_bouts_assigned, bbox_south_big)),
  ## 1a. Convert carcasses to Israel time
  ##  XXX FIXME
  ## 2. Separate INPA and wild (the rest of the instructions here are just for INPA)
  tar_target(inpa, filter(all_carcasses_cropped, carcType == "inpa")),
  tar_target(inpa_carcs, group_split(group_by(inpa, carcID))),
  tar_target(wild, filter(all_carcasses_cropped, carcType == "wild")),
  tar_target(wild_carcs, group_split(group_by(wild, carcID))),
  tar_target(gps_2022, data.table::fread("data/ACC/2022_hf_period/created/gps_2022.csv")),
  tar_target(gps_2023, data.table::fread("data/ACC/2023_hf_period/created/gps_2023.csv")),
  tar_target(gps_2024, data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv")),
  tar_target(gps_combined, get_gps_combined(gps_2022, gps_2023, gps_2024, bbox_south_big)),
  ## 4a. Convert gps data to Israel time 
  ## XXX fixme
  ## 4b. Make gps_all
  tar_target(gps_all, get_gps_all(inpa_carcs, gps_combined, days_after, days_before)),
  tar_target(gps_all_inpa, get_gps_all(inpa_carcs, gps_combined, days_after, days_before_wild)), # using the same parameters as for the wild carcasses, for comparison
  tar_target(gps_all_wild, get_gps_all(wild_carcs, gps_combined, days_after, days_before_wild)),
  tar_target(roosts, get_roosts(gps_all)), 
  tar_target(roosts_wild, get_roosts(gps_all_wild)),
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
  tar_target(gps, remove_points_before(gps_all, inpa_carcs, days_after, hours_before = 0)),
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

  # NBDA (overall parameters) -----------------------------------------------
  tar_target(min_sightings, 5),
  
  # NBDA (wild carcasses) ---------------------------------------------------
  ## Define carcasses to run NBDA on. XXX Note: because wild carcIDs are assigned after the carcasses are defined from feeding bouts, which requires choosing a slope cutoff, these numbers are correct only for the 5 degree cutoff. Will need to change if we change the cutoff.
  #tar_target(wild_carcs_for_nbda, c(1, 2, 5, 9, 16, 18, 22, 23, 57)),
  #tar_target(wild_carcs_for_nbda_which, match(wild_carcs_for_nbda, map_dbl(wild_carcs, "carcID"))),
  tar_target(gps_wild, remove_points_before(gps_all_wild, wild_carcs, days_after, hours_before = 24)), # XXX will need to edit this when we have more data in the original gps data. Currently, some carcs will be missing points from more than a few hours before.
  tar_target(see_carcass_wild, get_see_carcass(gps_wild, wild_carcs, detection_distance_flight, detection_distance_stationary)),
  tar_target(firsts_see_wild, get_firsts_see(see_carcass_wild, wild_carcs)),
  tar_target(oa_see_wild, purrr::map(firsts_see_wild, "local_identifier")),
  tar_target(oa_see_indivs_sorted_wild, purrr::map(oa_see_wild, sort)),
  # XXX skipping the flight networks for now. Just going to do the roost networks
  # Everything from here on for the wild carcasses will be subsetted by wild_carcs_for_nbda
  tar_target(roosts_dates_see_wild, get_roost_dates(roosts_wild, 1:length(wild_carcs))),
  tar_target(roosts_bin_see_wild, get_roosts_bin(roosts_dates_see_wild, roost_thresh)),
  tar_target(roosts_bin_fixed_see_wild, fix_nets_list(roosts_bin_see_wild, oa_see_indivs_sorted_wild)),
  tar_target(years_wild, get_years(wild_carcs, oa_see_wild)),
  tar_target(carcIDs_nbda_wild, map_chr(wild_carcs, ~as.character(.x$carcID[1]))),
  ## Need to convert the oas into numeric indices instead of a character vector
  tar_target(oas_nbda_numbers_wild, map2(oa_see_wild, oa_see_indivs_sorted_wild, ~match(.x, .y))),
  tar_target(dates_nbda_wild, map2(wild_carcs, firsts_see_wild, ~mutate(data.frame(dateOnly = seq.Date(from = .x$dateOnly, to = max(.y$dateOnly), by = "day")), day = 1:n()))),
  tar_target(firsts_with_dates_wild, map2(firsts_see_wild, dates_nbda_wild, ~left_join(.x, .y))),
  tar_target(days_vec_nbda_wild, map(firsts_with_dates_wild, "day")),
  tar_target(roost_mats_expanded_wild, expand_roost_mats(roosts_bin_fixed_see_wild, days_vec_nbda_wild, days_vec_nbda_wild)),
  tar_target(n_indivs_wild, map_dbl(roosts_bin_fixed_see_wild, ~nrow(.x[[1]]))),
  tar_target(n_timeperiods_wild, map_dbl(roost_mats_expanded_wild, length)),
  tar_target(N.RD_wild, get_dynamic_nets(n_indivs_wild, n_timeperiods_wild, roost_mats_expanded_wild)),
  tar_target(assMatrixIndices_wild, map(oas_nbda_numbers_wild, ~1:length(.x))),
  tar_target(nbdaData_list_dynamic_roost_wild, get_nbdaData_list_flex(cids = carcIDs_nbda_wild, oas = oas_nbda_numbers_wild, amis = assMatrixIndices_wild, nets1 = N.RD_wild, is_dynamic = T)),
  tar_target(Mods_N.RD_So_wild, mod_trycatch(nbdaData_list_dynamic_roost_wild, type = "social", iterations = 1000)),
  tar_target(Mods_N.RD_Aso_wild, mod_trycatch(nbdaData_list_dynamic_roost_wild, type = "asocial", iterations = 1000)),
  tar_target(sums_RD_wild, get_summaries(Mods_N.RD_So_wild, carcIDs_nbda_wild, "dynamic", "roost")),
  

  
  
  
  
  # NBDA (INPA carcasses) ---------------------------------------------------
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
  tar_target(dates_nbda, map2(carcs_nbda, firsts_nbda, ~mutate(data.frame(dateOnly = seq.Date(from = .x$date, to = max(.y$dateOnly))), day = 1:n()))),
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
