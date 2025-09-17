library(targets)
library(tarchetypes)
library(crew)

# Set target options:
tar_option_set(
  error = "null",
  packages = c("plyr", "vultureUtils", "tidyverse", "here", "NBDA", "sf", "dplyr", "lubridate", "ranger", "tidymodels", "moments", "parsnip", "caret", "zoo", "move", "terra"),
  controller = crew_controller_local(workers = 5)
)

lapply(list.files("R", full.names = TRUE), source) 

list(
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
  tar_target(gps_spd, 4), # This threshold, not 5 m/s, is based on Gideon's ACC analysis, because I want to make sure to match that. We can either use the same threshold or a more conservative one for later definitions of flight.
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
  tar_target(feeding_bouts, mutate(bind_rows(data.table::rbindlist(feeding_bo_2022, ignore.attr = T), data.table::rbindlist(feeding_bo_2023, ignore.attr = T), data.table::rbindlist(feeding_bo_2024, ignore.attr = T)),
                                   year = lubridate::year(start),
                                   boutID = paste(device_id, bout_id, year, sep = "_"))),
  tar_target(feeding_bo_spatial, st_transform(sf::st_as_sf(feeding_bouts, coords = c("location_long", "location_lat"), crs = "WGS84"), 32636)),
  
  ## Further restrictions on feeding bouts
  tar_target(feeding_bo_stationary, dplyr::filter(feeding_bo_spatial, as.numeric(ground_speed) <= gps_spd)),
  
  ## Using DEM to remove "feeding bouts" that are too much on a slope
  tar_target(filenames, list.files(here("data/raw/DEMs/ASTER/"), pattern = ".tif", full.names = T)),
  tar_target(feeding_bo_stationary_withslopes, get_slopes(filenames, bbox_south_big, neighbors = 8, feeding_bo_stationary)),
  tar_target(slope_thresh, 15),
  tar_target(feeding_bo_noslope, filter(feeding_bo_stationary_withslopes, slope < slope_thresh)),
  
  ## SFS
  ### Created in 00_carcass_data_translation.R
  ### Only spatial, not time-restricted.
  tar_target(stations, readRDS(here("data/created/stations.RDS"))),
  
  ## Station carcasses
  ### Created in 00_carcass_data_translation.R
  tar_target(carcasses_audited, readRDS(here("data/created/carcasses_audited.RDS"))),
  
  ## Focal carcasses
  ### During the 2022, 2023 and 2024 HF-ACC periods
  tar_target(carcasses_focal, get_focal(carcasses_audited, minmax_dates)), 
  
  ## Match bouts to carcasses
  tar_target(dist_bo_carcasses, 750), # NNN don't have good justification for this
  tar_target(hours_after_carcass, 72),
  tar_target(carcass_bo, get_carcass_bouts(bouts = feeding_bo_noslope, # NNN look into which stations are 142m apart. # looks like Tzaror_trap and Tzaror_mount, which I think we will end up merging into the same one anyway.
                                           carcasses = carcasses_focal,
                                           dist = dist_bo_carcasses,
                                           hours_after = hours_after_carcass)),
  # as.numeric(st_distance(stations, stations)) -> dists; dists[dists < 1500] # NNN look into this after we've resolved the stations with May and Shaaked
  tar_target(carcass_bo_df, purrr::list_rbind(carcass_bo)), 
  tar_target(non_carcass_bo, filter(feeding_bo_noslope, !(boutID %in% carcass_bo_df$boutID))),
  
  ## Cluster the remaining bouts to detect wild carcasses
  tar_target(wild_dist, 200), # NNN change to average max distance of bouts from known active carcasses, doubled. Should be approx. 800.
  tar_target(wild_time, '12 hours'), # note: cannot be more than 24 hours. If we want more than 24 hours, we need to do this grouping a different way. # NNN figure this out--should it actually be 24? How does spatsoc group things? Is it a sliding window or a consecutive window?
  tar_target(wild_carcass_bo_df, get_wild_carcass_bouts(non_carcass_bo,
                                                        time = wild_time,
                                                        dst = wild_dist,
                                                        minBouts = 3,
                                                        stations = stations,
                                                        stationDist = dist_bo_carcasses,
                                                        minIndivs = 3)), # NNN will need to rerun based on the above wild_dist
  tar_target(wild_carcasses, get_wild_carcasses(wild_carcass_bo_df) %>%
               mutate(carcType = "wild")),
  tar_target(wild_carcass_bo_again, assign_time_dist(wild_carcass_bo_df, wild_carcasses)),
  tar_target(carcass_bo_dedup, group_by(carcass_bo_df, boutID) %>%
               arrange(boutID, time_since_carcass) %>%
               slice(1) %>%
               ungroup()), # Rule: each duplicated bout is assigned to the carcass for which it is closer to the time of carcass placement
  ## Combine carcasses
  tar_target(carcasses_focal_withstats, get_bout_stats(carcasses_focal, carcass_bo_df)), 
  tar_target(all_carcasses, bind_rows(carcasses_focal_withstats %>% mutate(carcType = "stn", year = lubridate::year(date)) %>% dplyr::select(-starts_with("n_")), wild_carcasses %>% dplyr::mutate("date" = lubridate::ymd(dateOnly)) %>% dplyr::select(-dateOnly))), 
  
  tar_target(bbox_south_big, sf::st_transform(
    st_as_sfc(st_set_crs(st_bbox(c("xmin" = 34.205, "xmax" = 35.787,
                                   "ymin" = 29.478, "ymax" = 31.775)), 
                         "WGS84")), 32636)), ### NNN can I just draw a line at Jerusalem and take everything south of that? Does it change anything?
  
  ## Dynamic NBDA testing
  ## 0. Define parameters
  tar_target(days_after, 3), 
  tar_target(days_before, 1),
  tar_target(days_before_wild, 3),
  ## 1. Get carcasses and restrict to south
  tar_target(all_carcasses_cropped, sf::st_crop(all_carcasses, bbox_south_big)), 
  ## 1a. Convert carcasses to Israel time 
  ##  XXX FIXME
  ## 2. Separate stn and wild (the rest of the instructions here are just for stn)
  tar_target(stn, filter(all_carcasses_cropped, carcType == "stn")),
  tar_target(stn_carcs, group_split(group_by(stn, carcID))),
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
  
  tar_target(gps_combined, get_gps_combined(gps_2022, gps_2023, gps_2024, bbox_south_big)), # NNN remove geographic subsetting step here because we want to allow interactions that happen outside the geographic bounds. Make sure in cleaning that I'm only including the steps that came before defining the social network, NOT the additional cleaning steps applied to focal individuals for which we calculated movement stats. For example, do not need to exclude individuals with <30 days/season. 
  # NNN switch back to using the Israel bounding box from the mvmt soc analysis. 
  # NNN use the former latitude cutoff line for deciding which *carcasses* we want, but don't apply a geographic restriction to the *GPS data used for interaction networks*
  
  ## 4a. Convert gps data to Israel time 
  # XXX decided not to do this yet--because then I'd have to convert everything else and it would be a whole thing. # NNN convert at the end.
  # Instead, need to convert carcasses to UTC, and then convert everything back at the end.
  
  ## 4b. Make gps_all
  tar_target(gps_all, get_gps_all(stn_carcs, gps_combined, days_after, days_before)),
  tar_target(gps_all_stn, get_gps_all(stn_carcs, gps_combined, days_after, days_before_wild)), # using the same parameters as for the wild carcasses, for comparison
  tar_target(gps_all_wild, get_gps_all(wild_carcs, gps_combined, days_after, days_before_wild)),
  # tar_target(roosts, get_roosts(gps_all, col = "tag_local_identifier")),
  # tar_target(roosts_wild, get_roosts(gps_all_wild, col = "tag_local_identifier")),
  
  # Preparing data for NBDA -------------------------------------------------
  ## Stn carcasses
  tar_target(stb_mins, 30),
  tar_target(ddf, 2000),
  tar_target(dds, 1000),
  tar_target(dbf, 30), # will need to get gps data 30 days before in order to get longer-term networks
  tar_target(stn_gps_30days, get_gps_all(stn_carcs, gps_combined, days_after, dbf)),
  tar_target(wild_gps_30days, get_gps_all(wild_carcs, gps_combined, days_after, dbf)),
  # tar_target(stn_carcs_tcv, timeconvert(stn_carcs)),
  # tar_target(wild_carcs_tcv, timeconvert(wild_carcs)),
  tar_target(stn_gps_30days_tcv, timeconvert(stn_gps_30days, old_datetime = "timestamp", new_datetime = "timestamp_il")),
  tar_target(wild_gps_30days_tcv, timeconvert(wild_gps_30days, old_datetime = "timestamp", new_datetime = "timestamp_il")),
  tar_target(stmh, 72), # sighting time max hours
  
  # Prepare NBDA data
  ## Prepare NBDA data--stn carcs
  # NNN we were here-- halfway through prepare_nbda_data function review
  
  tar_target(nd1, nb_shortcut(stn_gps_30days_tcv[1:10], ddf, dds, gps_spd, stmh, stb_mins)),
  tar_target(nd2, nb_shortcut(stn_gps_30days_tcv[11:20], ddf, dds, gps_spd, stmh, stb_mins)),
  tar_target(nd3, nb_shortcut(stn_gps_30days_tcv[21:30], ddf, dds, gps_spd, stmh, stb_mins)),
  tar_target(nd4, nb_shortcut(stn_gps_30days_tcv[31:40], ddf, dds, gps_spd, stmh, stb_mins)),
  tar_target(nd5, nb_shortcut(stn_gps_30days_tcv[41:50], ddf, dds, gps_spd, stmh, stb_mins)),
  tar_target(nd6, nb_shortcut(stn_gps_30days_tcv[51:60], ddf, dds, gps_spd, stmh, stb_mins)),
  tar_target(nd7, nb_shortcut(stn_gps_30days_tcv[61:length(stn_gps_30days_tcv)], ddf, dds, gps_spd, stmh, stb_mins)),
  ## Prepare NBDA data--wild carcs
  
  tar_target(nd1_wild, nb_shortcut(wild_gps_30days_tcv[1:10], ddf, dds, gps_spd, stmh, stb_mins)),
  tar_target(nd2_wild, nb_shortcut(wild_gps_30days_tcv[11:20], ddf, dds, gps_spd, stmh, stb_mins)),
  tar_target(nd3_wild, nb_shortcut(wild_gps_30days_tcv[21:30], ddf, dds, gps_spd, stmh, stb_mins)),
  tar_target(nd4_wild, nb_shortcut(wild_gps_30days_tcv[31:40], ddf, dds, gps_spd, stmh, stb_mins)),
  tar_target(nd5_wild, nb_shortcut(wild_gps_30days_tcv[41:length(wild_gps_30days_tcv)], ddf, dds, gps_spd, stmh, stb_mins)),
  
  # NNN implement the Elvira change to the order in the flight/feeding network functions. That should eliminate a lot of the NAs. The remaining ones will be set to 0, resulting in 

  # Flight networks
  ## Flight networks--Cumulative (stn)
  ### Flight networks--Cumulative (stn)-- Binary
  tar_target(fl_bin_cumulative_1_prelim, purrr::map(nd1, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf))})),
  tar_target(fl_bin_cumulative_2_prelim, purrr::map(nd2, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf))})),
  tar_target(fl_bin_cumulative_3_prelim, purrr::map(nd3, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf))})),
  tar_target(fl_bin_cumulative_4_prelim, purrr::map(nd4, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf))})),
  tar_target(fl_bin_cumulative_5_prelim, purrr::map(nd5, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf))})),
  tar_target(fl_bin_cumulative_6_prelim, purrr::map(nd6, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf))})),
  tar_target(fl_bin_cumulative_7_prelim, purrr::map(nd7, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf))})),
  
  tar_target(fl_bin_cumulative_1, purrr::map2(fl_bin_cumulative_1_prelim, nd1, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_bin_cumulative_2, purrr::map2(fl_bin_cumulative_2_prelim, nd2, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_bin_cumulative_3, purrr::map2(fl_bin_cumulative_3_prelim, nd3, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_bin_cumulative_4, purrr::map2(fl_bin_cumulative_4_prelim, nd4, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_bin_cumulative_5, purrr::map2(fl_bin_cumulative_5_prelim, nd5, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_bin_cumulative_6, purrr::map2(fl_bin_cumulative_6_prelim, nd6, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_bin_cumulative_7, purrr::map2(fl_bin_cumulative_7_prelim, nd7, ~fix_nets(.x, .y$all_indivs_sorted))),
  
  ### Flight networks--Cumulative (stn)--Weighted
  tar_target(fl_wt_cumulative_1_prelim, purrr::map(nd1, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_2_prelim, purrr::map(nd2, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_3_prelim, purrr::map(nd3, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_4_prelim, purrr::map(nd4, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_5_prelim, purrr::map(nd5, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_6_prelim, purrr::map(nd6, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_7_prelim, purrr::map(nd7, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  
  tar_target(fl_wt_cumulative_1, purrr::map2(fl_wt_cumulative_1_prelim, nd1, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_2, purrr::map2(fl_wt_cumulative_2_prelim, nd2, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_3, purrr::map2(fl_wt_cumulative_3_prelim, nd3, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_4, purrr::map2(fl_wt_cumulative_4_prelim, nd4, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_5, purrr::map2(fl_wt_cumulative_5_prelim, nd5, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_6, purrr::map2(fl_wt_cumulative_6_prelim, nd6, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_7, purrr::map2(fl_wt_cumulative_7_prelim, nd7, ~fix_nets(.x, .y$all_indivs_sorted))),
  
  ## Flight networks--Cumulative (wild)
  ### Flight networks--Cumulative (wild)--Binary
  tar_target(fl_bin_cumulative_1_wild_prelim, purrr::map(nd1_wild, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf))})),
  tar_target(fl_bin_cumulative_2_wild_prelim, purrr::map(nd2_wild, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf))})),
  tar_target(fl_bin_cumulative_3_wild_prelim, purrr::map(nd3_wild, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf))})),
  tar_target(fl_bin_cumulative_4_wild_prelim, purrr::map(nd4_wild, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf))})),
  tar_target(fl_bin_cumulative_5_wild_prelim, purrr::map(nd5_wild, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf))})),
  
  tar_target(fl_bin_cumulative_1_wild, purrr::map2(fl_bin_cumulative_1_wild_prelim, nd1_wild, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_bin_cumulative_2_wild, purrr::map2(fl_bin_cumulative_2_wild_prelim, nd2_wild, ~fix_nets(.x, .y$all_indivs_sorted))),  
  tar_target(fl_bin_cumulative_3_wild, purrr::map2(fl_bin_cumulative_3_wild_prelim, nd3_wild, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_bin_cumulative_4_wild, purrr::map2(fl_bin_cumulative_4_wild_prelim, nd4_wild, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_bin_cumulative_5_wild, purrr::map2(fl_bin_cumulative_5_wild_prelim, nd5_wild, ~fix_nets(.x, .y$all_indivs_sorted))),
  
  ### Flight networks--Cumulative (wild)--Weighted
  tar_target(fl_wt_cumulative_1_wild_prelim, purrr::map(nd1_wild, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_2_wild_prelim, purrr::map(nd2_wild, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_3_wild_prelim, purrr::map(nd3_wild, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_4_wild_prelim, purrr::map(nd4_wild, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_5_wild_prelim, purrr::map(nd5_wild, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  
  tar_target(fl_wt_cumulative_1_wild, purrr::map2(fl_wt_cumulative_1_wild_prelim, nd1_wild, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_2_wild, purrr::map2(fl_wt_cumulative_2_wild_prelim, nd2_wild, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_3_wild, purrr::map2(fl_wt_cumulative_3_wild_prelim, nd3_wild, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_4_wild, purrr::map2(fl_wt_cumulative_4_wild_prelim, nd4_wild, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_5_wild, purrr::map2(fl_wt_cumulative_5_wild_prelim, nd5_wild, ~fix_nets(.x, .y$all_indivs_sorted))),
  
  ## Flight networks--Static, past 30 days (stn)
  ### Flight networks--Static, past 30 days (stn)--Binary
  tar_target(fl_bin_30days_1_prelim, 
             purrr::map(nd1, ~get_fl_bin(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_bin_30days_2_prelim, 
             purrr::map(nd2, ~get_fl_bin(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_bin_30days_3_prelim, 
             purrr::map(nd3, ~get_fl_bin(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_bin_30days_4_prelim, 
             purrr::map(nd4, ~get_fl_bin(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_bin_30days_5_prelim, 
             purrr::map(nd5, ~get_fl_bin(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_bin_30days_6_prelim, 
             purrr::map(nd6, ~get_fl_bin(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_bin_30days_7_prelim, 
             purrr::map(nd7, ~get_fl_bin(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  
  tar_target(fl_bin_30days_1, purrr::map2(fl_bin_30days_1_prelim, nd1, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_bin_30days_2, purrr::map2(fl_bin_30days_2_prelim, nd2, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_bin_30days_3, purrr::map2(fl_bin_30days_3_prelim, nd3, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_bin_30days_4, purrr::map2(fl_bin_30days_4_prelim, nd4, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_bin_30days_5, purrr::map2(fl_bin_30days_5_prelim, nd5, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_bin_30days_6, purrr::map2(fl_bin_30days_6_prelim, nd6, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_bin_30days_7, purrr::map2(fl_bin_30days_7_prelim, nd7, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  
  ### Flight networks--Static, past 30 days (stn)--Weighted
  tar_target(fl_wt_30days_1_prelim, 
             purrr::map(nd1, ~get_fl_weighted(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_wt_30days_2_prelim, 
             purrr::map(nd2, ~get_fl_weighted(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_wt_30days_3_prelim, 
             purrr::map(nd3, ~get_fl_weighted(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_wt_30days_4_prelim, 
             purrr::map(nd4, ~get_fl_weighted(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_wt_30days_5_prelim, 
             purrr::map(nd5, ~get_fl_weighted(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_wt_30days_6_prelim, 
             purrr::map(nd6, ~get_fl_weighted(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_wt_30days_7_prelim, 
             purrr::map(nd7, ~get_fl_weighted(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  
  tar_target(fl_wt_30days_1, purrr::map2(fl_wt_30days_1_prelim, nd1, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_wt_30days_2, purrr::map2(fl_wt_30days_2_prelim, nd2, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_wt_30days_3, purrr::map2(fl_wt_30days_3_prelim, nd3, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_wt_30days_4, purrr::map2(fl_wt_30days_4_prelim, nd4, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_wt_30days_5, purrr::map2(fl_wt_30days_5_prelim, nd5, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_wt_30days_6, purrr::map2(fl_wt_30days_6_prelim, nd6, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_wt_30days_7, purrr::map2(fl_wt_30days_7_prelim, nd7, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  
  ## Flight networks--Static, past 30 days (wild)
  ### Flight networks--Static, past 30 days (wild)--Binary
  tar_target(fl_bin_30days_1_wild_prelim, 
             purrr::map(nd1_wild, ~get_fl_bin(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_bin_30days_2_wild_prelim, 
             purrr::map(nd2_wild, ~get_fl_bin(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_bin_30days_3_wild_prelim, 
             purrr::map(nd3_wild, ~get_fl_bin(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_bin_30days_4_wild_prelim, 
             purrr::map(nd4_wild, ~get_fl_bin(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_bin_30days_5_wild_prelim, 
             purrr::map(nd5_wild, ~get_fl_bin(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  
  tar_target(fl_bin_30days_1_wild, purrr::map2(fl_bin_30days_1_wild_prelim, nd1_wild, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_bin_30days_2_wild, purrr::map2(fl_bin_30days_2_wild_prelim, nd2_wild, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_bin_30days_3_wild, purrr::map2(fl_bin_30days_3_wild_prelim, nd3_wild, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_bin_30days_4_wild, purrr::map2(fl_bin_30days_4_wild_prelim, nd4_wild, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_bin_30days_5_wild, purrr::map2(fl_bin_30days_5_wild_prelim, nd5_wild, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  
  ### Flight networks--Static, past 30 days (wild)--Weighted
  tar_target(fl_wt_30days_1_wild_prelim, 
             purrr::map(nd1_wild, ~get_fl_weighted(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_wt_30days_2_wild_prelim, 
             purrr::map(nd2_wild, ~get_fl_weighted(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_wt_30days_3_wild_prelim, 
             purrr::map(nd3_wild, ~get_fl_weighted(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_wt_30days_4_wild_prelim, 
             purrr::map(nd4_wild, ~get_fl_weighted(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  tar_target(fl_wt_30days_5_wild_prelim, 
             purrr::map(nd5_wild, ~get_fl_weighted(.x$gps_data_static_hours_n720_n024, dist = ddf))),
  
  tar_target(fl_wt_30days_1_wild, purrr::map2(fl_wt_30days_1_wild_prelim, nd1_wild, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_wt_30days_2_wild, purrr::map2(fl_wt_30days_2_wild_prelim, nd2_wild, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_wt_30days_3_wild, purrr::map2(fl_wt_30days_3_wild_prelim, nd3_wild, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_wt_30days_4_wild, purrr::map2(fl_wt_30days_4_wild_prelim, nd4_wild, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  tar_target(fl_wt_30days_5_wild, purrr::map2(fl_wt_30days_5_wild_prelim, nd5_wild, ~{
    fix_nets(list(.x), .y$all_indivs_sorted)
  })),
  
  # Prepare data for NBDA
  ### Prepare data for NBDA--Cumulative (stn)--Binary--seeds
  tar_target(data_cumul_bin_1, purrr::map2(nd1, fl_bin_cumulative_1, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_bin_2, purrr::map2(nd2, fl_bin_cumulative_2, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_bin_3, purrr::map2(nd3, fl_bin_cumulative_3, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_bin_4, purrr::map2(nd4, fl_bin_cumulative_4, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_bin_5, purrr::map2(nd5, fl_bin_cumulative_5, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_bin_6, purrr::map2(nd6, fl_bin_cumulative_6, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_bin_7, purrr::map2(nd7, fl_bin_cumulative_7, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  
  ### Prepare data for NBDA--Cumulative (wild)--Binary--seeds
  tar_target(data_cumul_bin_1_wild, purrr::map2(nd1_wild, fl_bin_cumulative_1_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_bin_2_wild, purrr::map2(nd2_wild, fl_bin_cumulative_2_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_bin_3_wild, purrr::map2(nd3_wild, fl_bin_cumulative_3_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_bin_4_wild, purrr::map2(nd4_wild, fl_bin_cumulative_4_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_bin_5_wild, purrr::map2(nd5_wild, fl_bin_cumulative_5_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  
  ### Prepare data for NBDA--Cumulative (stn)--Weighted--seeds
  tar_target(data_cumul_wt_1, purrr::map2(nd1, fl_wt_cumulative_1, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_wt_2, purrr::map2(nd2, fl_wt_cumulative_2, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_wt_3, purrr::map2(nd3, fl_wt_cumulative_3, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_wt_4, purrr::map2(nd4, fl_wt_cumulative_4, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_wt_5, purrr::map2(nd5, fl_wt_cumulative_5, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_wt_6, purrr::map2(nd6, fl_wt_cumulative_6, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_wt_7, purrr::map2(nd7, fl_wt_cumulative_7, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  
  ### Prepare data for NBDA--Cumulative (wild)--Weighted--seeds
  tar_target(data_cumul_wt_1_wild, purrr::map2(nd1_wild, fl_wt_cumulative_1_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_wt_2_wild, purrr::map2(nd2_wild, fl_wt_cumulative_2_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_wt_3_wild, purrr::map2(nd3_wild, fl_wt_cumulative_3_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_wt_4_wild, purrr::map2(nd4_wild, fl_wt_cumulative_4_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_cumul_wt_5_wild, purrr::map2(nd5_wild, fl_wt_cumulative_5_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  
  ### Prepare data for NBDA--30 days (stn)--Binary--seeds
  tar_target(data_30days_bin_1, purrr::map2(nd1, fl_bin_30days_1, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_bin_2, purrr::map2(nd2, fl_bin_30days_2, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_bin_3, purrr::map2(nd3, fl_bin_30days_3, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_bin_4, purrr::map2(nd4, fl_bin_30days_4, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_bin_5, purrr::map2(nd5, fl_bin_30days_5, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_bin_6, purrr::map2(nd6, fl_bin_30days_6, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_bin_7, purrr::map2(nd7, fl_bin_30days_7, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  
  ### Prepare data for NBDA--30 days (wild)--Binary--seeds
  tar_target(data_30days_bin_1_wild, purrr::map2(nd1_wild, fl_bin_30days_1_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_bin_2_wild, purrr::map2(nd2_wild, fl_bin_30days_2_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_bin_3_wild, purrr::map2(nd3_wild, fl_bin_30days_3_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_bin_4_wild, purrr::map2(nd4_wild, fl_bin_30days_4_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_bin_5_wild, purrr::map2(nd5_wild, fl_bin_30days_5_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  
  ### Prepare data for NBDA--30 days (stn)--Weighted--seeds
  tar_target(data_30days_wt_1, purrr::map2(nd1, fl_wt_30days_1, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_wt_2, purrr::map2(nd2, fl_wt_30days_2, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_wt_3, purrr::map2(nd3, fl_wt_30days_3, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_wt_4, purrr::map2(nd4, fl_wt_30days_4, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_wt_5, purrr::map2(nd5, fl_wt_30days_5, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_wt_6, purrr::map2(nd6, fl_wt_30days_6, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_wt_7, purrr::map2(nd7, fl_wt_30days_7, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  
  ### Prepare data for NBDA--30 days (wild)--Weighted--seeds
  tar_target(data_30days_wt_1_wild, purrr::map2(nd1_wild, fl_wt_30days_1_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_wt_2_wild, purrr::map2(nd2_wild, fl_wt_30days_2_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_wt_3_wild, purrr::map2(nd3_wild, fl_wt_30days_3_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_wt_4_wild, purrr::map2(nd4_wild, fl_wt_30days_4_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  tar_target(data_30days_wt_5_wild, purrr::map2(nd5_wild, fl_wt_30days_5_wild, ~{if(!is.null(.y) & length(.x$oa_nums) > 1){
    nbdaData(.x$carcID, assMatrix = make_assMatrix(.y), 
             orderAcq = .x$oa_nums, demons = .x$seeds_vec)}else{NULL}})),
  
  ## NBDA models
  ### Cumulative, binary (stn)
  tar_target(mods_cumul_bin, purrr::map(c(data_cumul_bin_1, data_cumul_bin_2, data_cumul_bin_3, data_cumul_bin_4, data_cumul_bin_5, data_cumul_bin_6, data_cumul_bin_7), ~{tryCatch(oadaFit(.x, type = "social"), error = function(e) NULL)})),
  
  ### Cumulative, binary (wild)
  tar_target(mods_cumul_bin_wild, purrr::map(c(data_cumul_bin_1_wild, data_cumul_bin_2_wild, data_cumul_bin_3_wild, data_cumul_bin_4_wild, data_cumul_bin_5_wild), ~{tryCatch(oadaFit(.x, type = "social"), error = function(e) NULL)})), 
  
  ### Cumulative, weighted (stn)
  tar_target(mods_cumul_wt, purrr::map(c(data_cumul_wt_1, data_cumul_wt_2, data_cumul_wt_3, data_cumul_wt_4, data_cumul_wt_5, data_cumul_wt_6, data_cumul_wt_7), ~{tryCatch(oadaFit(.x, type = "social"), error = function(e) NULL)})),
  
  ### Cumulative, weighted (wild)
  tar_target(mods_cumul_wt_wild, purrr::map(c(data_cumul_wt_1_wild, data_cumul_wt_2_wild, data_cumul_wt_3_wild, data_cumul_wt_4_wild, data_cumul_wt_5_wild), ~{tryCatch(oadaFit(.x, type = "social"), error = function(e) NULL)})),
  
  ### 30 days, binary (stn)
  tar_target(mods_30days_bin, purrr::map(c(data_30days_bin_1, data_30days_bin_2, data_30days_bin_3, data_30days_bin_4, data_30days_bin_5, data_30days_bin_6, data_30days_bin_7), ~{tryCatch(oadaFit(.x, type = "social"), error = function(e) NULL)})),
  
  ### 30 days, binary (wild)
  tar_target(mods_30days_bin_wild, purrr::map(c(data_30days_bin_1_wild, data_30days_bin_2_wild, data_30days_bin_3_wild, data_30days_bin_4_wild, data_30days_bin_5_wild), ~{tryCatch(oadaFit(.x, type = "social"), error = function(e) NULL)})),

  ### 30 days, weighted (stn)
  tar_target(mods_30days_wt, purrr::map(c(data_30days_wt_1, data_30days_wt_2, data_30days_wt_3, data_30days_wt_4, data_30days_wt_5, data_30days_wt_6, data_30days_wt_7), ~{tryCatch(oadaFit(.x, type = "social"), error = function(e) NULL)})),
  
  ### 30 days, weighted (wild)
  tar_target(mods_30days_wt_wild, purrr::map(c(data_30days_wt_1_wild, data_30days_wt_2_wild, data_30days_wt_3_wild, data_30days_wt_4_wild, data_30days_wt_5_wild), ~{tryCatch(oadaFit(.x, type = "social"), error = function(e) NULL)})),

  tar_target(stats_cumul_bin, mutate(purrr::list_rbind(map(mods_cumul_bin, getmodstats)), type = "cumul", binwt = "bin", seeds = T, carcID = purrr::map_dbl(stn_carcs, "carcID"))),
  tar_target(stats_cumul_wt, mutate(purrr::list_rbind(map(mods_cumul_wt, getmodstats)), type = "cumul", binwt = "wt", seeds = T, carcID = purrr::map_dbl(stn_carcs, "carcID"))),

  tar_target(stats_cumul_bin_wild, mutate(purrr::list_rbind(map(mods_cumul_bin_wild, getmodstats)), type = "cumul", binwt = "bin", seeds = T, carcID = purrr::map_dbl(wild_carcs, "carcID"))),
  tar_target(stats_cumul_wt_wild, mutate(purrr::list_rbind(map(mods_cumul_wt_wild, getmodstats)), type = "cumul", binwt = "wt", seeds = T, carcID = purrr::map_dbl(wild_carcs, "carcID"))),

  tar_target(stats_30days_bin, mutate(purrr::list_rbind(map(mods_30days_bin, getmodstats)), type = "30days", binwt = "bin", seeds = T, carcID = purrr::map_dbl(stn_carcs, "carcID"))),
  tar_target(stats_30days_wt, mutate(purrr::list_rbind(map(mods_30days_wt, getmodstats)), type = "30days", binwt = "wt", seeds = T, carcID = purrr::map_dbl(stn_carcs, "carcID"))),

  tar_target(stats_30days_bin_wild, mutate(purrr::list_rbind(map(mods_30days_bin_wild, getmodstats)), type = "30days", binwt = "bin", seeds = T, carcID = purrr::map_dbl(wild_carcs, "carcID"))),
  tar_target(stats_30days_wt_wild, mutate(purrr::list_rbind(map(mods_30days_wt_wild, getmodstats)), type = "30days", binwt = "wt", seeds = T, carcID = purrr::map_dbl(wild_carcs, "carcID"))),

  tar_target(stats, purrr::list_rbind(list(stats_cumul_bin, stats_cumul_wt, stats_30days_bin, stats_30days_wt))),
  
  tar_target(stats_wild, purrr::list_rbind(list(stats_cumul_bin_wild, stats_cumul_wt_wild, stats_30days_bin_wild, stats_30days_wt_wild))),
  
  ## Number of individuals involved in each diffusion
  tar_target(ns, purrr::list_rbind(purrr::map(c(nd1, nd2, nd3, nd4, nd5, nd6, nd7), ~{as.data.frame(t(unlist(.x[1:4])))}))),
  
  tar_target(ns_wild, purrr::list_rbind(purrr::map(c(nd1_wild, nd2_wild, nd3_wild, nd4_wild, nd5_wild), ~{as.data.frame(t(unlist(.x[1:4])))})))
)