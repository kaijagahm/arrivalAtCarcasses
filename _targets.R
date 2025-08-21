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
  # MOVEBANK CREDENTIALS
  tar_target(pw, "data/movebankCredentials/pw.Rda", format = "file"),
  tar_target(loginObject, get_loginObject(pw)),
  
  # MANUALLY DEFINE HF-ACC WINDOWS (these dates come from the ACC data, but I've manually defined them here so we can exclude the acc part of the pipeline if need be)
  tar_target(mindate_22, "2022-11-11 00:06:18 UTC"),
  tar_target(mindate_23, "2023-03-15 00:54:49 UTC"),
  tar_target(mindate_24, "2024-04-01 00:04:28 UTC"),
  tar_target(maxdate_22, "2022-12-11 11:59:00 UTC"),
  tar_target(maxdate_23, "2023-04-15 11:58:52 UTC"),
  tar_target(maxdate_24, "2024-05-06 20:38:17 UTC"),
  tar_target(minmax_dates, list(mindate_22, maxdate_22, mindate_23, maxdate_23, mindate_24, maxdate_24)),
  tar_target(minmax_buff, list(
    lubridate::ymd_hms(mindate_22) - lubridate::days(31),
    lubridate::ymd_hms(maxdate_22) + lubridate::days(5),
    lubridate::ymd_hms(mindate_23) - lubridate::days(31),
    lubridate::ymd_hms(maxdate_23) + lubridate::days(5),
    lubridate::ymd_hms(mindate_24) - lubridate::days(31),
    lubridate::ymd_hms(maxdate_24) + lubridate::days(5)
  )),
  
  # HIGH-FREQUENCY ACC DATA
  tar_target(data_files_2022, list.files(here("data/ACC/2022_hf_period/raw/"), full.names = T, pattern = ".csv")),
  tar_target(data_files_2023, list.files(here("data/ACC/2023_hf_period/raw/"), full.names = T, pattern = ".csv")),
  tar_target(data_files_2024, list.files(here("data/ACC/2024_hf_period/raw/"), full.names = T, pattern = ".csv")),
  tar_target(unobs_raw_acc_2022, get_acc_data(data_files_2022)),
  tar_target(unobs_raw_acc_2023, get_acc_data(data_files_2023)),
  tar_target(unobs_raw_acc_2024, get_acc_data(data_files_2024)),
  tar_target(acc_2022_flipped, flip_devices(unobs_raw_acc_2022)),
  tar_target(acc_2023_flipped, flip_devices(unobs_raw_acc_2023)),
  tar_target(acc_2024_flipped, flip_devices(unobs_raw_acc_2024)),
  
  ## Calibration
  tar_target(cal_data, read_csv(here("ACC_algo_Marta_draft/Data/example_calibration.csv"))),
  tar_target(splitup_22, group_split(group_by(as.data.frame(acc_2022_flipped)), device_id)),
  tar_target(splitup_23, group_split(group_by(as.data.frame(acc_2023_flipped)), device_id)),
  tar_target(splitup_24, group_split(group_by(as.data.frame(acc_2024_flipped)), device_id)),
  tar_target(calibrated_22_1, caldev(splitup_22[1:10], cal_data)),
  tar_target(calibrated_22_2, caldev(splitup_22[11:20], cal_data)),
  tar_target(calibrated_22_3, caldev(splitup_22[21:30], cal_data)),
  tar_target(calibrated_22_4, caldev(splitup_22[31:40], cal_data)),
  tar_target(calibrated_22_5, caldev(splitup_22[41:50], cal_data)),
  tar_target(calibrated_22_6, caldev(splitup_22[51:60], cal_data)),
  tar_target(calibrated_22_7, caldev(splitup_22[61:70], cal_data)),
  tar_target(calibrated_22_8, caldev(splitup_22[71:length(splitup_22)], cal_data)),
  tar_target(calibrated_23_1, caldev(splitup_22[1:10], cal_data)),
  tar_target(calibrated_23_2, caldev(splitup_23[11:20], cal_data)),
  tar_target(calibrated_23_3, caldev(splitup_23[21:30], cal_data)),
  tar_target(calibrated_23_4, caldev(splitup_23[31:40], cal_data)),
  tar_target(calibrated_23_5, caldev(splitup_23[41:50], cal_data)),
  tar_target(calibrated_23_6, caldev(splitup_23[51:60], cal_data)),
  tar_target(calibrated_23_7, caldev(splitup_23[61:70], cal_data)),
  tar_target(calibrated_23_8, caldev(splitup_23[71:length(splitup_23)], cal_data)),
  tar_target(calibrated_24_1, caldev(splitup_24[1:10], cal_data)),
  tar_target(calibrated_24_2, caldev(splitup_24[11:20], cal_data)),
  tar_target(calibrated_24_3, caldev(splitup_24[21:30], cal_data)),
  tar_target(calibrated_24_4, caldev(splitup_24[31:40], cal_data)),
  tar_target(calibrated_24_5, caldev(splitup_24[41:50], cal_data)),
  tar_target(calibrated_24_6, caldev(splitup_24[51:60], cal_data)),
  tar_target(calibrated_24_7, caldev(splitup_24[61:70], cal_data)),
  tar_target(calibrated_24_8, caldev(splitup_24[71:length(splitup_24)], cal_data)),
  
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
  
  tar_target(bo_22_1, map(cal_22_1, get_bo)),
  tar_target(bo_22_2, map(cal_22_2, get_bo)),
  tar_target(bo_22_3, map(cal_22_3, get_bo)),
  tar_target(bo_22_4, map(cal_22_4, get_bo)),
  tar_target(bo_22_5, map(cal_22_5, get_bo)),
  tar_target(bo_22_6, map(cal_22_6, get_bo)),
  tar_target(bo_22_7, map(cal_22_7, get_bo)),
  tar_target(bo_22_8, map(cal_22_8, get_bo)),
  tar_target(bo_23_1, map(cal_23_1, get_bo)),
  tar_target(bo_23_2, map(cal_23_2, get_bo)),
  tar_target(bo_23_3, map(cal_23_3, get_bo)),
  tar_target(bo_23_4, map(cal_23_4, get_bo)),
  tar_target(bo_23_5, map(cal_23_5, get_bo)),
  tar_target(bo_23_6, map(cal_23_6, get_bo)),
  tar_target(bo_23_7, map(cal_23_7, get_bo)),
  tar_target(bo_23_8, map(cal_23_8, get_bo)),
  tar_target(bo_24_1, map(cal_24_1, get_bo)),
  tar_target(bo_24_2, map(cal_24_2, get_bo)),
  tar_target(bo_24_3, map(cal_24_3, get_bo)),
  tar_target(bo_24_4, map(cal_24_4, get_bo)),
  tar_target(bo_24_5, map(cal_24_5, get_bo)),
  tar_target(bo_24_6, map(cal_24_6, get_bo)),
  tar_target(bo_24_7, map(cal_24_7, get_bo)),
  tar_target(bo_24_8, map(cal_24_8, get_bo)),
  
  # The predictions didn't work for some reason, even though the same code used to work fine, so I'm going to derive predictions from the scores objects by just taking the highest one
  tar_target(sc_22_1, map(cal_22_1, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_2, map(cal_22_2, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_3, map(cal_22_3, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_4, map(cal_22_4, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_5, map(cal_22_5, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_6, map(cal_22_6, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_7, map(cal_22_7, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_8, map(cal_22_8, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_1, map(cal_23_1, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_2, map(cal_23_2, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_3, map(cal_23_3, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_4, map(cal_23_4, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_5, map(cal_23_5, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_6, map(cal_23_6, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_7, map(cal_23_7, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_8, map(cal_23_8, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_1, map(cal_24_1, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_2, map(cal_24_2, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_3, map(cal_24_3, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_4, map(cal_24_4, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_5, map(cal_24_5, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_6, map(cal_24_6, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_7, map(cal_24_7, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_8, map(cal_24_8, ~get_sc(.x, mod = clasmod))),
  
  tar_target(pr_22_1, map(sc_22_1, gpfs)),
  tar_target(pr_22_2, map(sc_22_2, gpfs)),
  tar_target(pr_22_3, map(sc_22_3, gpfs)),
  tar_target(pr_22_4, map(sc_22_4, gpfs)),
  tar_target(pr_22_5, map(sc_22_5, gpfs)),
  tar_target(pr_22_6, map(sc_22_6, gpfs)),
  tar_target(pr_22_7, map(sc_22_7, gpfs)),
  tar_target(pr_22_8, map(sc_22_8, gpfs)),
  tar_target(pr_23_1, map(sc_23_1, gpfs)),  
  tar_target(pr_23_2, map(sc_23_2, gpfs)),  
  tar_target(pr_23_3, map(sc_23_3, gpfs)),  
  tar_target(pr_23_4, map(sc_23_4, gpfs)),  
  tar_target(pr_23_5, map(sc_23_5, gpfs)),
  tar_target(pr_23_6, map(sc_23_6, gpfs)),
  tar_target(pr_23_7, map(sc_23_7, gpfs)),  
  tar_target(pr_23_8, map(sc_23_8, gpfs)),  
  tar_target(pr_24_1, map(sc_24_1, gpfs)),  
  tar_target(pr_24_2, map(sc_24_2, gpfs)),  
  tar_target(pr_24_3, map(sc_24_3, gpfs)),  
  tar_target(pr_24_4, map(sc_24_4, gpfs)),  
  tar_target(pr_24_5, map(sc_24_5, gpfs)),  
  tar_target(pr_24_6, map(sc_24_6, gpfs)),  
  tar_target(pr_24_7, map(sc_24_7, gpfs)),  
  tar_target(pr_24_8, map(sc_24_8, gpfs)),  
  
  tar_target(bo_22, c(bo_22_1, bo_22_2, bo_22_3, bo_22_4, bo_22_5, bo_22_6, bo_22_7, bo_22_8)),
  tar_target(bo_23, c(bo_23_1, bo_23_2, bo_23_3, bo_23_4, bo_23_5, bo_23_6, bo_23_7, bo_23_8)),
  tar_target(bo_24, c(bo_24_1, bo_24_2, bo_24_3, bo_24_4, bo_24_5, bo_24_6, bo_24_7, bo_24_8)),
  tar_target(sc_22, c(sc_22_1, sc_22_2, sc_22_3, sc_22_4, sc_22_5, sc_22_6, sc_22_7, sc_22_8)),
  tar_target(sc_23, c(sc_23_1, sc_23_2, sc_23_3, sc_23_4, sc_23_5, sc_23_6, sc_23_7, sc_23_8)),
  tar_target(sc_24, c(sc_24_1, sc_24_2, sc_24_3, sc_24_4, sc_24_5, sc_24_6, sc_24_7, sc_24_8)),
  
  ## Get classification model
  tar_target(clasmod, readRDS(here("ACC_algo_Marta_draft/Data/gv_final_model_fit.rda"))),
  
  tar_target(bo_pr_22_1, pmap(.l = list(cal_22_1, pr_22_1, sc_22_1, bo_22_1), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_2, pmap(.l = list(cal_22_2, pr_22_2, sc_22_2, bo_22_2), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_3, pmap(.l = list(cal_22_3, pr_22_3, sc_22_3, bo_22_3), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_4, pmap(.l = list(cal_22_4, pr_22_4, sc_22_4, bo_22_4), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_5, pmap(.l = list(cal_22_5, pr_22_5, sc_22_5, bo_22_5), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_6, pmap(.l = list(cal_22_6, pr_22_6, sc_22_6, bo_22_6), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_7, pmap(.l = list(cal_22_7, pr_22_7, sc_22_7, bo_22_7), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_8, pmap(.l = list(cal_22_8, pr_22_8, sc_22_8, bo_22_8), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_1, pmap(.l = list(cal_23_1, pr_23_1, sc_23_1, bo_23_1), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_2, pmap(.l = list(cal_23_2, pr_23_2, sc_23_2, bo_23_2), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_3, pmap(.l = list(cal_23_3, pr_23_3, sc_23_3, bo_23_3), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_4, pmap(.l = list(cal_23_4, pr_23_4, sc_23_4, bo_23_4), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_5, pmap(.l = list(cal_23_5, pr_23_5, sc_23_5, bo_23_5), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_6, pmap(.l = list(cal_23_6, pr_23_6, sc_23_6, bo_23_6), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_7, pmap(.l = list(cal_23_7, pr_23_7, sc_23_7, bo_23_7), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_8, pmap(.l = list(cal_23_8, pr_23_8, sc_23_8, bo_23_8), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_1, pmap(.l = list(cal_24_1, pr_24_1, sc_24_1, bo_24_1), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_2, pmap(.l = list(cal_24_2, pr_24_2, sc_24_2, bo_24_2), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_3, pmap(.l = list(cal_24_3, pr_24_3, sc_24_3, bo_24_3), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_4, pmap(.l = list(cal_24_4, pr_24_4, sc_24_4, bo_24_4), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_5, pmap(.l = list(cal_24_5, pr_24_5, sc_24_5, bo_24_5), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_6, pmap(.l = list(cal_24_6, pr_24_6, sc_24_6, bo_24_6), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_7, pmap(.l = list(cal_24_7, pr_24_7, sc_24_7, bo_24_7), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_8, pmap(.l = list(cal_24_8, pr_24_8, sc_24_8, bo_24_8), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  
  tar_target(bo_pr_2022, purrr::discard(c(bo_pr_22_1, bo_pr_22_2, bo_pr_22_3, bo_pr_22_4, bo_pr_22_5, bo_pr_22_6, bo_pr_22_7, bo_pr_22_8), is.null)),
  tar_target(bo_pr_2023, purrr::discard(c(bo_pr_23_1, bo_pr_23_2, bo_pr_23_3, bo_pr_23_4, bo_pr_23_5, bo_pr_23_6, bo_pr_23_7, bo_pr_23_8), is.null)),
  tar_target(bo_pr_2024, purrr::discard(c(bo_pr_24_1, bo_pr_24_2, bo_pr_24_3, bo_pr_24_4, bo_pr_24_5, bo_pr_24_6, bo_pr_24_7, bo_pr_24_8), is.null)),
  # Get the individual IDs so we can match them to gps points
  tar_target(device_ids_2022, purrr::map(bo_pr_2022, ~.x$device_id[1])),
  tar_target(device_ids_2023, purrr::map(bo_pr_2023, ~.x$device_id[1])),
  tar_target(device_ids_2024, purrr::map(bo_pr_2024, ~.x$device_id[1])),
  tar_target(gps_focal_indivs_2022, get_gps_forbouts_indivs(device_ids_2022, gps_2022)),
  tar_target(gps_focal_indivs_2023, get_gps_forbouts_indivs(device_ids_2023, gps_2023)),
  tar_target(gps_focal_indivs_2024, get_gps_forbouts_indivs(device_ids_2024, gps_2024)),
  tar_target(gps_spd, 4),
  tar_target(wg22_1, purrr::map2(bo_pr_2022[1:10], gps_focal_indivs_2022[1:10], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_2, purrr::map2(bo_pr_2022[11:20], gps_focal_indivs_2022[11:20], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_3, purrr::map2(bo_pr_2022[21:30], gps_focal_indivs_2022[21:30], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_4, purrr::map2(bo_pr_2022[31:40], gps_focal_indivs_2022[31:40], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_5, purrr::map2(bo_pr_2022[41:50], gps_focal_indivs_2022[41:50], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_6, purrr::map2(bo_pr_2022[51:60], gps_focal_indivs_2022[51:60], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_7, purrr::map2(bo_pr_2022[61:70], gps_focal_indivs_2022[61:70], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_8, purrr::map2(bo_pr_2022[71:length(bo_pr_2022)], gps_focal_indivs_2022[71:length(gps_focal_indivs_2022)], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_1, purrr::map2(bo_pr_2023[1:10], gps_focal_indivs_2023[1:10], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_2, purrr::map2(bo_pr_2023[11:20], gps_focal_indivs_2023[11:20], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_3, purrr::map2(bo_pr_2023[21:30], gps_focal_indivs_2023[21:30], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_4, purrr::map2(bo_pr_2023[31:40], gps_focal_indivs_2023[31:40], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_5, purrr::map2(bo_pr_2023[41:50], gps_focal_indivs_2023[41:50], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_6, purrr::map2(bo_pr_2023[51:60], gps_focal_indivs_2023[51:60], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_7, purrr::map2(bo_pr_2023[61:70], gps_focal_indivs_2023[61:70], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_8, purrr::map2(bo_pr_2023[71:length(bo_pr_2023)], gps_focal_indivs_2023[71:length(gps_focal_indivs_2023)], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_1, purrr::map2(bo_pr_2024[1:10], gps_focal_indivs_2024[1:10], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_2, purrr::map2(bo_pr_2024[11:20], gps_focal_indivs_2024[11:20], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_3, purrr::map2(bo_pr_2024[21:30], gps_focal_indivs_2024[21:30], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_4, purrr::map2(bo_pr_2024[31:40], gps_focal_indivs_2024[31:40], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_5, purrr::map2(bo_pr_2024[41:50], gps_focal_indivs_2024[41:50], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_6, purrr::map2(bo_pr_2024[51:60], gps_focal_indivs_2024[51:60], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_7, purrr::map2(bo_pr_2024[61:70], gps_focal_indivs_2024[61:70], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_8, purrr::map2(bo_pr_2024[71:length(bo_pr_2024)], gps_focal_indivs_2024[71:length(gps_focal_indivs_2024)], ~get_matches(.x, .y, gps_spd))),
  
  tar_target(with_gps_2022, c(wg22_1, wg22_2, wg22_3, wg22_4, wg22_5, wg22_6, wg22_7, wg22_8)),
  tar_target(with_gps_2023, c(wg23_1, wg23_2, wg23_3, wg23_4, wg23_5, wg23_6, wg23_7, wg23_8)),
  tar_target(with_gps_2024, c(wg24_1, wg24_2, wg24_3, wg24_4, wg24_5, wg24_6, wg24_7, wg24_8)),
  
  ## Attach the gps data back to the bouts and predictions
  tar_target(full_2022, map2(bo_pr_2022, with_gps_2022, ~join_gps_bouts(.x, .y))),
  tar_target(full_2023, map2(bo_pr_2023, with_gps_2023, ~join_gps_bouts(.x, .y))),
  tar_target(full_2024, map2(bo_pr_2024, with_gps_2024, ~join_gps_bouts(.x, .y))),
  
  # ## GPS data for the focal periods (in case we need it later)
  # tar_target(focal_gps_2023, readRDS(here("data/ACC/2023_hf_period/created/focal_gps_2023.RDS"))),
  # tar_target(focal_gps_2024, readRDS(here("data/ACC/2024_hf_period/created/focal_gps_2024.RDS"))),
  
  ## Feeding bouts (high-frequency periods only)
  tar_target(feeding_bo_prob_thresh, 0.75),
  tar_target(feeding_bo_2022, map(full_2022, ~getfeeding(.x, feeding_bo_prob_thresh))),
  tar_target(feeding_bo_2023, map(full_2023, ~getfeeding(.x, feeding_bo_prob_thresh))),
  tar_target(feeding_bo_2024, map(full_2024, ~getfeeding(.x, feeding_bo_prob_thresh))),
  
  ## Bind them together to get all feeding bouts
  tar_target(feeding_bouts, mutate(bind_rows(data.table::rbindlist(feeding_bo_2022, ignore.attr = T), data.table::rbindlist(feeding_bo_2023, ignore.attr = T), data.table::rbindlist(feeding_bo_2024, ignore.attr = T)), boutID = paste(device_id, bout_id, sep = "_"))),
  tar_target(feeding_bo_spatial, st_transform(sf::st_as_sf(feeding_bouts, coords = c("location_long", "location_lat"), crs = "WGS84"), 32636)),
  
  ## Further restrictions on feeding bouts
  tar_target(feeding_bo_stationary, dplyr::filter(feeding_bo_spatial, as.numeric(ground_speed) <= gps_spd)),
  
  ## Using DEM to remove "feeding bouts" that are too much on a slope
  tar_target(filenames, list.files(here("data/raw/DEMs/ASTER/"), pattern = ".tif", full.names = T)),
  tar_target(feeding_bo_stationary_withslopes, get_slopes(filenames, bbox_south_big, neighbors = 8, feeding_bo_stationary)),
  tar_target(slope_thresh, 15),
  tar_target(feeding_bo_noslope, filter(feeding_bo_stationary_withslopes, slope < slope_thresh)),
  
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
  tar_target(dist_bo_carcasses, 750), # xxx seems maybe too high
  tar_target(hours_after_carcass, 72),
  tar_target(carcass_bo, get_carcass_bouts(bouts = feeding_bo_noslope,
                                           carcasses = carcasses_focal,
                                           dist = dist_bo_carcasses,
                                           hours_after = hours_after_carcass)),
  tar_target(carcass_bo_df, purrr::list_rbind(carcass_bo)), 
  tar_target(non_carcass_bo, filter(feeding_bo_noslope, !(boutID %in% carcass_bo_df$boutID))),
  
  ## Cluster the remaining bouts to detect wild carcasses
  tar_target(wild_dist, 200), 
  tar_target(wild_time, '12 hours'), # note: cannot be more than 24 hours. If we want more than 24 hours, we need to do this grouping a different way.
  tar_target(wild_carcass_bo_df, get_wild_carcass_bouts(non_carcass_bo,
                                                        time = wild_time,
                                                        dist = wild_dist,
                                                        minBouts = 3,
                                                        stations = stations,
                                                        stationDist = 750,
                                                        minIndivs = 3)),
  tar_target(wild_carcasses, get_wild_carcasses(wild_carcass_bo_df) %>%
               mutate(carcType = "wild")),
  tar_target(wild_carcass_bo_again, assign_time_dist(wild_carcass_bo_df, wild_carcasses)),
  tar_target(carcass_bo_dedup, group_by(carcass_bo_df, boutID) %>%
               arrange(boutID, time_since_carcass) %>%
               slice(1) %>%
               ungroup()), # Rule: each duplicated bout is assigned to the carcass for which it is closer to the time of carcass placement
  ## Combine carcasses
  tar_target(carcasses_focal_withstats, get_bout_stats(carcasses_focal, carcass_bo_df)),
  tar_target(all_carcasses, bind_rows(carcasses_focal_withstats %>% mutate(carcType = "inpa", year = lubridate::year(date)) %>% dplyr::select(-starts_with("n_")), wild_carcasses %>% dplyr::mutate("date" = lubridate::ymd(dateOnly)) %>% dplyr::select(-dateOnly))),
  
  tar_target(bbox_south_big, sf::st_transform(
    st_as_sfc(st_set_crs(st_bbox(c("xmin" = 34.205, "xmax" = 35.787,
                                   "ymin" = 29.478, "ymax" = 31.775)), 
                         "WGS84")), 32636)),
  ## Dynamic NBDA testing
  ## 0. Define parameters
  tar_target(days_after, 3),
  tar_target(days_before, 1),
  tar_target(days_before_wild, 3),
  tar_target(seed_time_before, lubridate::minutes(30)),
  tar_target(ddf, 2000),
  tar_target(dds, 1000),
  ## 1. Get carcasses and restrict to south
  tar_target(all_carcasses_cropped, sf::st_crop(all_carcasses, bbox_south_big)), 
  ## 1a. Convert carcasses to Israel time
  ##  XXX FIXME
  ## 2. Separate INPA and wild (the rest of the instructions here are just for INPA)
  tar_target(inpa, filter(all_carcasses_cropped, carcType == "inpa")),
  tar_target(inpa_carcs, group_split(group_by(inpa, carcID))),
  tar_target(wild, filter(all_carcasses_cropped, carcType == "wild")),
  tar_target(wild_carcs, group_split(group_by(wild, carcID))),
  
  # download data to match high frequency period, plus buffer
  tar_target(ornitela_data_2022, readRDS(here("data/ornitela_data_2022.RDS"))),
  tar_target(ornitela_data_2023, readRDS(here("data/ornitela_data_2023.RDS"))),
  tar_target(ornitela_data_2024, readRDS(here("data/ornitela_data_2024.RDS"))),
  tar_target(inpa_data_2022, readRDS(here("data/inpa_data_2022.RDS"))),
  tar_target(inpa_data_2023, readRDS(here("data/inpa_data_2023.RDS"))),
  tar_target(inpa_data_2024, readRDS(here("data/inpa_data_2024.RDS"))),
  
  tar_target(gps_2022_1, sf::st_as_sf(bind_rows(as.data.frame(ornitela_data_2022), as.data.frame(inpa_data_2022)), crs = "WGS84")),
  tar_target(gps_2023_1, sf::st_as_sf(bind_rows(as.data.frame(ornitela_data_2023), as.data.frame(inpa_data_2023)), crs = "WGS84")),
  tar_target(gps_2024_1, sf::st_as_sf(bind_rows(as.data.frame(ornitela_data_2024), as.data.frame(inpa_data_2024)), crs = "WGS84")),
  
  tar_target(gps_2022, dplyr::bind_cols(gps_2022_1, setNames(as.data.frame(sf::st_coordinates(gps_2022_1)), c("location_long", "location_lat")))),
  tar_target(gps_2023, dplyr::bind_cols(gps_2023_1, setNames(as.data.frame(sf::st_coordinates(gps_2023_1)), c("location_long", "location_lat")))),
  tar_target(gps_2024, dplyr::bind_cols(gps_2024_1, setNames(as.data.frame(sf::st_coordinates(gps_2024_1)), c("location_long", "location_lat")))),
  
  tar_target(gps_combined, get_gps_combined(gps_2022, gps_2023, gps_2024, bbox_south_big)),
  ## 4a. Convert gps data to Israel time 
  # XXX decided not to do this yet--because then I'd have to convert everything else and it would be a whole thing.
  ## 4b. Make gps_all
  tar_target(gps_all, get_gps_all(inpa_carcs, gps_combined, days_after, days_before)),
  tar_target(gps_all_inpa, get_gps_all(inpa_carcs, gps_combined, days_after, days_before_wild)), # using the same parameters as for the wild carcasses, for comparison
  tar_target(gps_all_wild, get_gps_all(wild_carcs, gps_combined, days_after, days_before_wild)),
  tar_target(roosts, get_roosts(gps_all, col = "tag_local_identifier")), 
  tar_target(roosts_wild, get_roosts(gps_all_wild, col = "tag_local_identifier"))#,
  # ## 6. Get seeds
  # tar_target(seeds_inpa, get_seeds_gps(gps_all_inpa, inpa_carcs, seed_time_before, ddf, dds)),
  # tar_target(seeds_wild, get_seeds_gps(gps_all_wild, wild_carcs, seed_time_before, ddf, dds)),
  # tar_target(seeds_gps, get_seeds_gps(gps_all, inpa_carcs, seed_time_before, ddf, dds)),
  # tar_target(seed_indivs, map(seeds_gps, ~sort(unique(sf::st_drop_geometry(.x)$local_identifier)))),
  # ## 7. Get distances from roosts to carcasses
  # tar_target(distances, get_distances(roosts, inpa_carcs)),
  # ## 8. Load who's who
  # tar_target(ww, read_csv(here("data/raw/whoswho_vultures_20230920_new.csv"), col_select = 1:40)),
  # ## 9. Get age_group ILV
  # tar_target(www, get_www(ww)),
  # ## 10. Combine age_group ILV with distances to get ILVs data frame
  # tar_target(ilvs, get_ilvs(distances, www))
)