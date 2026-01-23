library(targets)
library(tarchetypes)
library(crew)

# Set target options:
tar_option_set(
  error = "null",
  packages = c("plyr", "vultureUtils", "tidyverse", "here", "NBDA", "sf", "dplyr", "lubridate", "ranger", "tidymodels", "moments", "parsnip", "caret", "zoo", "move", "terra", "readxl", "data.table", "geosphere"),
  controller = crew_controller_local(workers = 10)
)

lapply(list.files("R", full.names = TRUE), source) 

list(
  # MANUALLY DEFINE HF-ACC WINDOWS (these dates come from the ACC data, but I've manually defined them here so we can exclude the acc part of the pipeline if need be)
  tar_target(mindate_22, "2022-11-11 00:00:00 UTC"),
  tar_target(mindate_23, "2023-03-15 00:00:00 UTC"),
  tar_target(mindate_24, "2024-04-01 00:00:00 UTC"),
  tar_target(maxdate_22, "2022-12-11 00:00:00 UTC"),
  tar_target(maxdate_23, "2023-04-15 00:00:00 UTC"),
  tar_target(maxdate_24, "2024-05-06 00:00:00 UTC"),
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
  tar_target(calibrated_22_1, caldev(splitup_22[1:5], cal_data)),
  tar_target(calibrated_22_2, caldev(splitup_22[6:10], cal_data)),
  tar_target(calibrated_22_3, caldev(splitup_22[11:15], cal_data)),
  tar_target(calibrated_22_4, caldev(splitup_22[16:20], cal_data)),
  tar_target(calibrated_22_5, caldev(splitup_22[21:25], cal_data)),
  tar_target(calibrated_22_6, caldev(splitup_22[26:30], cal_data)),
  tar_target(calibrated_22_7, caldev(splitup_22[31:35], cal_data)),
  tar_target(calibrated_22_8, caldev(splitup_22[36:40], cal_data)),
  tar_target(calibrated_22_9, caldev(splitup_22[41:46], cal_data)),
  tar_target(calibrated_22_10, caldev(splitup_22[46:50], cal_data)),
  tar_target(calibrated_22_11, caldev(splitup_22[51:55], cal_data)),
  tar_target(calibrated_22_12, caldev(splitup_22[56:60], cal_data)),
  tar_target(calibrated_22_13, caldev(splitup_22[61:65], cal_data)),
  tar_target(calibrated_22_14, caldev(splitup_22[66:70], cal_data)),
  tar_target(calibrated_22_15, caldev(splitup_22[71:75], cal_data)),
  tar_target(calibrated_22_16, caldev(splitup_22[76:length(splitup_22)], cal_data)),
  
  tar_target(calibrated_23_1, caldev(splitup_23[1:5], cal_data)),
  tar_target(calibrated_23_2, caldev(splitup_23[6:10], cal_data)),
  tar_target(calibrated_23_3, caldev(splitup_23[11:15], cal_data)),
  tar_target(calibrated_23_4, caldev(splitup_23[16:20], cal_data)),
  tar_target(calibrated_23_5, caldev(splitup_23[21:25], cal_data)),
  tar_target(calibrated_23_6, caldev(splitup_23[26:30], cal_data)),
  tar_target(calibrated_23_7, caldev(splitup_23[31:35], cal_data)),
  tar_target(calibrated_23_8, caldev(splitup_23[36:40], cal_data)),
  tar_target(calibrated_23_9, caldev(splitup_23[41:46], cal_data)),
  tar_target(calibrated_23_10, caldev(splitup_23[46:50], cal_data)),
  tar_target(calibrated_23_11, caldev(splitup_23[51:55], cal_data)),
  tar_target(calibrated_23_12, caldev(splitup_23[56:60], cal_data)),
  tar_target(calibrated_23_13, caldev(splitup_23[61:65], cal_data)),
  tar_target(calibrated_23_14, caldev(splitup_23[66:70], cal_data)),
  tar_target(calibrated_23_15, caldev(splitup_23[71:75], cal_data)),
  tar_target(calibrated_23_16, caldev(splitup_23[76:length(splitup_23)], cal_data)),
  
  tar_target(calibrated_24_1, caldev(splitup_24[1:5], cal_data)),
  tar_target(calibrated_24_2, caldev(splitup_24[6:10], cal_data)),
  tar_target(calibrated_24_3, caldev(splitup_24[11:15], cal_data)),
  tar_target(calibrated_24_4, caldev(splitup_24[16:20], cal_data)),
  tar_target(calibrated_24_5, caldev(splitup_24[21:25], cal_data)),
  tar_target(calibrated_24_6, caldev(splitup_24[26:30], cal_data)),
  tar_target(calibrated_24_7, caldev(splitup_24[31:35], cal_data)),
  tar_target(calibrated_24_8, caldev(splitup_24[36:40], cal_data)),
  tar_target(calibrated_24_9, caldev(splitup_24[41:46], cal_data)),
  tar_target(calibrated_24_10, caldev(splitup_24[46:50], cal_data)),
  tar_target(calibrated_24_11, caldev(splitup_24[51:55], cal_data)),
  tar_target(calibrated_24_12, caldev(splitup_24[56:60], cal_data)),
  tar_target(calibrated_24_13, caldev(splitup_24[61:65], cal_data)),
  tar_target(calibrated_24_14, caldev(splitup_24[66:70], cal_data)),
  tar_target(calibrated_24_15, caldev(splitup_24[71:75], cal_data)),
  tar_target(calibrated_24_16, caldev(splitup_24[76:length(splitup_24)], cal_data)),
  
  tar_target(cal_22_1, map(calibrated_22_1, distinct)),
  tar_target(cal_22_2, map(calibrated_22_2, distinct)),
  tar_target(cal_22_3, map(calibrated_22_3, distinct)),
  tar_target(cal_22_4, map(calibrated_22_4, distinct)),
  tar_target(cal_22_5, map(calibrated_22_5, distinct)),
  tar_target(cal_22_6, map(calibrated_22_6, distinct)),
  tar_target(cal_22_7, map(calibrated_22_7, distinct)),
  tar_target(cal_22_8, map(calibrated_22_8, distinct)),
  tar_target(cal_22_9, map(calibrated_22_9, distinct)),
  tar_target(cal_22_10, map(calibrated_22_10, distinct)),
  tar_target(cal_22_11, map(calibrated_22_11, distinct)),
  tar_target(cal_22_12, map(calibrated_22_12, distinct)),
  tar_target(cal_22_13, map(calibrated_22_13, distinct)),
  tar_target(cal_22_14, map(calibrated_22_14, distinct)),
  tar_target(cal_22_15, map(calibrated_22_15, distinct)),
  tar_target(cal_22_16, map(calibrated_22_16, distinct)),
  
  tar_target(cal_23_1, map(calibrated_23_1, distinct)),
  tar_target(cal_23_2, map(calibrated_23_2, distinct)),
  tar_target(cal_23_3, map(calibrated_23_3, distinct)),
  tar_target(cal_23_4, map(calibrated_23_4, distinct)),
  tar_target(cal_23_5, map(calibrated_23_5, distinct)),
  tar_target(cal_23_6, map(calibrated_23_6, distinct)),
  tar_target(cal_23_7, map(calibrated_23_7, distinct)),
  tar_target(cal_23_8, map(calibrated_23_8, distinct)),
  tar_target(cal_23_9, map(calibrated_23_9, distinct)),
  tar_target(cal_23_10, map(calibrated_23_10, distinct)),
  tar_target(cal_23_11, map(calibrated_23_11, distinct)),
  tar_target(cal_23_12, map(calibrated_23_12, distinct)),
  tar_target(cal_23_13, map(calibrated_23_13, distinct)),
  tar_target(cal_23_14, map(calibrated_23_14, distinct)),
  tar_target(cal_23_15, map(calibrated_23_15, distinct)),
  tar_target(cal_23_16, map(calibrated_23_16, distinct)),
  
  tar_target(cal_24_1, map(calibrated_24_1, distinct)),
  tar_target(cal_24_2, map(calibrated_24_2, distinct)),
  tar_target(cal_24_3, map(calibrated_24_3, distinct)),
  tar_target(cal_24_4, map(calibrated_24_4, distinct)),
  tar_target(cal_24_5, map(calibrated_24_5, distinct)),
  tar_target(cal_24_6, map(calibrated_24_6, distinct)),
  tar_target(cal_24_7, map(calibrated_24_7, distinct)),
  tar_target(cal_24_8, map(calibrated_24_8, distinct)),
  tar_target(cal_24_9, map(calibrated_24_9, distinct)),
  tar_target(cal_24_10, map(calibrated_24_10, distinct)),
  tar_target(cal_24_11, map(calibrated_24_11, distinct)),
  tar_target(cal_24_12, map(calibrated_24_12, distinct)),
  tar_target(cal_24_13, map(calibrated_24_13, distinct)),
  tar_target(cal_24_14, map(calibrated_24_14, distinct)),
  tar_target(cal_24_15, map(calibrated_24_15, distinct)),
  tar_target(cal_24_16, map(calibrated_24_16, distinct)),
  
  tar_target(bo_22_1, map(cal_22_1, get_bo)),
  tar_target(bo_22_2, map(cal_22_2, get_bo)),
  tar_target(bo_22_3, map(cal_22_3, get_bo)),
  tar_target(bo_22_4, map(cal_22_4, get_bo)),
  tar_target(bo_22_5, map(cal_22_5, get_bo)),
  tar_target(bo_22_6, map(cal_22_6, get_bo)),
  tar_target(bo_22_7, map(cal_22_7, get_bo)),
  tar_target(bo_22_8, map(cal_22_8, get_bo)),
  tar_target(bo_22_9, map(cal_22_9, get_bo)),
  tar_target(bo_22_10, map(cal_22_10, get_bo)),
  tar_target(bo_22_11, map(cal_22_11, get_bo)),
  tar_target(bo_22_12, map(cal_22_12, get_bo)),
  tar_target(bo_22_13, map(cal_22_13, get_bo)),
  tar_target(bo_22_14, map(cal_22_14, get_bo)),
  tar_target(bo_22_15, map(cal_22_15, get_bo)),
  tar_target(bo_22_16, map(cal_22_16, get_bo)),
  
  tar_target(bo_23_1, map(cal_23_1, get_bo)),
  tar_target(bo_23_2, map(cal_23_2, get_bo)),
  tar_target(bo_23_3, map(cal_23_3, get_bo)),
  tar_target(bo_23_4, map(cal_23_4, get_bo)),
  tar_target(bo_23_5, map(cal_23_5, get_bo)),
  tar_target(bo_23_6, map(cal_23_6, get_bo)),
  tar_target(bo_23_7, map(cal_23_7, get_bo)),
  tar_target(bo_23_8, map(cal_23_8, get_bo)),
  tar_target(bo_23_9, map(cal_23_9, get_bo)),
  tar_target(bo_23_10, map(cal_23_10, get_bo)),
  tar_target(bo_23_11, map(cal_23_11, get_bo)),
  tar_target(bo_23_12, map(cal_23_12, get_bo)),
  tar_target(bo_23_13, map(cal_23_13, get_bo)),
  tar_target(bo_23_14, map(cal_23_14, get_bo)),
  tar_target(bo_23_15, map(cal_23_15, get_bo)),
  tar_target(bo_23_16, map(cal_23_16, get_bo)),
  
  tar_target(bo_24_1, map(cal_24_1, get_bo)),
  tar_target(bo_24_2, map(cal_24_2, get_bo)),
  tar_target(bo_24_3, map(cal_24_3, get_bo)),
  tar_target(bo_24_4, map(cal_24_4, get_bo)),
  tar_target(bo_24_5, map(cal_24_5, get_bo)),
  tar_target(bo_24_6, map(cal_24_6, get_bo)),
  tar_target(bo_24_7, map(cal_24_7, get_bo)),
  tar_target(bo_24_8, map(cal_24_8, get_bo)),
  tar_target(bo_24_9, map(cal_24_9, get_bo)),
  tar_target(bo_24_10, map(cal_24_10, get_bo)),
  tar_target(bo_24_11, map(cal_24_11, get_bo)),
  tar_target(bo_24_12, map(cal_24_12, get_bo)),
  tar_target(bo_24_13, map(cal_24_13, get_bo)),
  tar_target(bo_24_14, map(cal_24_14, get_bo)),
  tar_target(bo_24_15, map(cal_24_15, get_bo)),
  tar_target(bo_24_16, map(cal_24_16, get_bo)),
  
  # The predictions didn't work for some reason, even though the same code used to work fine, so I'm going to derive predictions from the scores objects by just taking the highest one
  tar_target(sc_22_1, map(cal_22_1, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_2, map(cal_22_2, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_3, map(cal_22_3, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_4, map(cal_22_4, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_5, map(cal_22_5, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_6, map(cal_22_6, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_7, map(cal_22_7, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_8, map(cal_22_8, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_9, map(cal_22_9, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_10, map(cal_22_10, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_11, map(cal_22_11, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_12, map(cal_22_12, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_13, map(cal_22_13, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_14, map(cal_22_14, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_15, map(cal_22_15, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_22_16, map(cal_22_16, ~get_sc(.x, mod = clasmod))),
  
  tar_target(sc_23_1, map(cal_23_1, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_2, map(cal_23_2, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_3, map(cal_23_3, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_4, map(cal_23_4, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_5, map(cal_23_5, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_6, map(cal_23_6, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_7, map(cal_23_7, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_8, map(cal_23_8, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_9, map(cal_23_9, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_10, map(cal_23_10, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_11, map(cal_23_11, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_12, map(cal_23_12, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_13, map(cal_23_13, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_14, map(cal_23_14, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_15, map(cal_23_15, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_23_16, map(cal_23_16, ~get_sc(.x, mod = clasmod))),
  
  tar_target(sc_24_1, map(cal_24_1, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_2, map(cal_24_2, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_3, map(cal_24_3, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_4, map(cal_24_4, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_5, map(cal_24_5, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_6, map(cal_24_6, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_7, map(cal_24_7, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_8, map(cal_24_8, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_9, map(cal_24_9, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_10, map(cal_24_10, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_11, map(cal_24_11, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_12, map(cal_24_12, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_13, map(cal_24_13, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_14, map(cal_24_14, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_15, map(cal_24_15, ~get_sc(.x, mod = clasmod))),
  tar_target(sc_24_16, map(cal_24_16, ~get_sc(.x, mod = clasmod))),
  
  tar_target(pr_22_1, map(sc_22_1, gpfs)),
  tar_target(pr_22_2, map(sc_22_2, gpfs)),
  tar_target(pr_22_3, map(sc_22_3, gpfs)),
  tar_target(pr_22_4, map(sc_22_4, gpfs)),
  tar_target(pr_22_5, map(sc_22_5, gpfs)),
  tar_target(pr_22_6, map(sc_22_6, gpfs)),
  tar_target(pr_22_7, map(sc_22_7, gpfs)),
  tar_target(pr_22_8, map(sc_22_8, gpfs)),
  tar_target(pr_22_9, map(sc_22_9, gpfs)),
  tar_target(pr_22_10, map(sc_22_10, gpfs)),
  tar_target(pr_22_11, map(sc_22_11, gpfs)),
  tar_target(pr_22_12, map(sc_22_12, gpfs)),
  tar_target(pr_22_13, map(sc_22_13, gpfs)),
  tar_target(pr_22_14, map(sc_22_14, gpfs)),
  tar_target(pr_22_15, map(sc_22_15, gpfs)),
  tar_target(pr_22_16, map(sc_22_16, gpfs)),
  
  tar_target(pr_23_1, map(sc_23_1, gpfs)),
  tar_target(pr_23_2, map(sc_23_2, gpfs)),
  tar_target(pr_23_3, map(sc_23_3, gpfs)),
  tar_target(pr_23_4, map(sc_23_4, gpfs)),
  tar_target(pr_23_5, map(sc_23_5, gpfs)),
  tar_target(pr_23_6, map(sc_23_6, gpfs)),
  tar_target(pr_23_7, map(sc_23_7, gpfs)),
  tar_target(pr_23_8, map(sc_23_8, gpfs)),
  tar_target(pr_23_9, map(sc_23_9, gpfs)),
  tar_target(pr_23_10, map(sc_23_10, gpfs)),
  tar_target(pr_23_11, map(sc_23_11, gpfs)),
  tar_target(pr_23_12, map(sc_23_12, gpfs)),
  tar_target(pr_23_13, map(sc_23_13, gpfs)),
  tar_target(pr_23_14, map(sc_23_14, gpfs)),
  tar_target(pr_23_15, map(sc_23_15, gpfs)),
  tar_target(pr_23_16, map(sc_23_16, gpfs)),
  
  tar_target(pr_24_1, map(sc_24_1, gpfs)),
  tar_target(pr_24_2, map(sc_24_2, gpfs)),
  tar_target(pr_24_3, map(sc_24_3, gpfs)),
  tar_target(pr_24_4, map(sc_24_4, gpfs)),
  tar_target(pr_24_5, map(sc_24_5, gpfs)),
  tar_target(pr_24_6, map(sc_24_6, gpfs)),
  tar_target(pr_24_7, map(sc_24_7, gpfs)),
  tar_target(pr_24_8, map(sc_24_8, gpfs)),
  tar_target(pr_24_9, map(sc_24_9, gpfs)),
  tar_target(pr_24_10, map(sc_24_10, gpfs)),
  tar_target(pr_24_11, map(sc_24_11, gpfs)),
  tar_target(pr_24_12, map(sc_24_12, gpfs)),
  tar_target(pr_24_13, map(sc_24_13, gpfs)),
  tar_target(pr_24_14, map(sc_24_14, gpfs)),
  tar_target(pr_24_15, map(sc_24_15, gpfs)),
  tar_target(pr_24_16, map(sc_24_16, gpfs)),
  
  tar_target(bo_22, c(bo_22_1, bo_22_2, bo_22_3, bo_22_4, bo_22_5, bo_22_6, bo_22_7, bo_22_8, bo_22_9, bo_22_10, bo_22_11, bo_22_12, bo_22_13, bo_22_14, bo_22_15, bo_22_16)),
  tar_target(bo_23, c(bo_23_1, bo_23_2, bo_23_3, bo_23_4, bo_23_5, bo_23_6, bo_23_7, bo_23_8, bo_23_9, bo_23_10, bo_23_11, bo_23_12, bo_23_13, bo_23_14, bo_23_15, bo_23_16)),
  tar_target(bo_24, c(bo_24_1, bo_24_2, bo_24_3, bo_24_4, bo_24_5, bo_24_6, bo_24_7, bo_24_8, bo_24_9, bo_24_10, bo_24_11, bo_24_12, bo_24_13, bo_24_14, bo_24_15, bo_24_16)),
  tar_target(sc_22, c(sc_22_1, sc_22_2, sc_22_3, sc_22_4, sc_22_5, sc_22_6, sc_22_7, sc_22_8, sc_22_9, sc_22_10, sc_22_11, sc_22_12, sc_22_13, sc_22_14, sc_22_15, sc_22_16)),
  tar_target(sc_23, c(sc_23_1, sc_23_2, sc_23_3, sc_23_4, sc_23_5, sc_23_6, sc_23_7, sc_23_8, sc_23_9, sc_23_10, sc_23_11, sc_23_12, sc_23_13, sc_23_14, sc_23_15, sc_23_16)),
  tar_target(sc_24, c(sc_24_1, sc_24_2, sc_24_3, sc_24_4, sc_24_5, sc_24_6, sc_24_7, sc_24_8, sc_24_9, sc_24_10, sc_24_11, sc_24_12, sc_24_13, sc_24_14, sc_24_15, sc_24_16)),
  
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
  tar_target(bo_pr_22_9, pmap(.l = list(cal_22_9, pr_22_9, sc_22_9, bo_22_9), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_10, pmap(.l = list(cal_22_10, pr_22_10, sc_22_10, bo_22_10), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_11, pmap(.l = list(cal_22_11, pr_22_11, sc_22_11, bo_22_11), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_12, pmap(.l = list(cal_22_12, pr_22_12, sc_22_12, bo_22_12), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_13, pmap(.l = list(cal_22_13, pr_22_13, sc_22_13, bo_22_13), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_14, pmap(.l = list(cal_22_14, pr_22_14, sc_22_14, bo_22_14), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_15, pmap(.l = list(cal_22_15, pr_22_15, sc_22_15, bo_22_15), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_22_16, pmap(.l = list(cal_22_16, pr_22_16, sc_22_16, bo_22_16), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  
  tar_target(bo_pr_23_1, pmap(.l = list(cal_23_1, pr_23_1, sc_23_1, bo_23_1), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_2, pmap(.l = list(cal_23_2, pr_23_2, sc_23_2, bo_23_2), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_3, pmap(.l = list(cal_23_3, pr_23_3, sc_23_3, bo_23_3), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_4, pmap(.l = list(cal_23_4, pr_23_4, sc_23_4, bo_23_4), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_5, pmap(.l = list(cal_23_5, pr_23_5, sc_23_5, bo_23_5), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_6, pmap(.l = list(cal_23_6, pr_23_6, sc_23_6, bo_23_6), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_7, pmap(.l = list(cal_23_7, pr_23_7, sc_23_7, bo_23_7), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_8, pmap(.l = list(cal_23_8, pr_23_8, sc_23_8, bo_23_8), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_9, pmap(.l = list(cal_23_9, pr_23_9, sc_23_9, bo_23_9), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_10, pmap(.l = list(cal_23_10, pr_23_10, sc_23_10, bo_23_10), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_11, pmap(.l = list(cal_23_11, pr_23_11, sc_23_11, bo_23_11), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_12, pmap(.l = list(cal_23_12, pr_23_12, sc_23_12, bo_23_12), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_13, pmap(.l = list(cal_23_13, pr_23_13, sc_23_13, bo_23_13), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_14, pmap(.l = list(cal_23_14, pr_23_14, sc_23_14, bo_23_14), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_15, pmap(.l = list(cal_23_15, pr_23_15, sc_23_15, bo_23_15), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_23_16, pmap(.l = list(cal_23_16, pr_23_16, sc_23_16, bo_23_16), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  
  tar_target(bo_pr_24_1, pmap(.l = list(cal_24_1, pr_24_1, sc_24_1, bo_24_1), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_2, pmap(.l = list(cal_24_2, pr_24_2, sc_24_2, bo_24_2), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_3, pmap(.l = list(cal_24_3, pr_24_3, sc_24_3, bo_24_3), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_4, pmap(.l = list(cal_24_4, pr_24_4, sc_24_4, bo_24_4), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_5, pmap(.l = list(cal_24_5, pr_24_5, sc_24_5, bo_24_5), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_6, pmap(.l = list(cal_24_6, pr_24_6, sc_24_6, bo_24_6), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_7, pmap(.l = list(cal_24_7, pr_24_7, sc_24_7, bo_24_7), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_8, pmap(.l = list(cal_24_8, pr_24_8, sc_24_8, bo_24_8), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_9, pmap(.l = list(cal_24_9, pr_24_9, sc_24_9, bo_24_9), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_10, pmap(.l = list(cal_24_10, pr_24_10, sc_24_10, bo_24_10), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_11, pmap(.l = list(cal_24_11, pr_24_11, sc_24_11, bo_24_11), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_12, pmap(.l = list(cal_24_12, pr_24_12, sc_24_12, bo_24_12), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_13, pmap(.l = list(cal_24_13, pr_24_13, sc_24_13, bo_24_13), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_14, pmap(.l = list(cal_24_14, pr_24_14, sc_24_14, bo_24_14), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_15, pmap(.l = list(cal_24_15, pr_24_15, sc_24_15, bo_24_15), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  tar_target(bo_pr_24_16, pmap(.l = list(cal_24_16, pr_24_16, sc_24_16, bo_24_16), .f = ~gbp(prepared = ..1, predictions = ..2, scores = ..3, bouts = ..4))),
  
  tar_target(bo_pr_2022, map(purrr::discard(c(bo_pr_22_1, bo_pr_22_2, bo_pr_22_3, bo_pr_22_4, bo_pr_22_5, bo_pr_22_6, bo_pr_22_7, bo_pr_22_8, bo_pr_22_9, bo_pr_22_10, bo_pr_22_11, bo_pr_22_12, bo_pr_22_13, bo_pr_22_14, bo_pr_22_15, bo_pr_22_16), is.null), ~mutate(.x, start = lubridate::ymd_hms(start), end = lubridate::ymd_hms(end)))),
  tar_target(bo_pr_2023, map(purrr::discard(c(bo_pr_23_1, bo_pr_23_2, bo_pr_23_3, bo_pr_23_4, bo_pr_23_5, bo_pr_23_6, bo_pr_23_7, bo_pr_23_8, bo_pr_23_9, bo_pr_23_10, bo_pr_23_11, bo_pr_23_12, bo_pr_23_13, bo_pr_23_14, bo_pr_23_15, bo_pr_23_16), is.null), ~mutate(.x, start = lubridate::ymd_hms(start), end = lubridate::ymd_hms(end)))),
  tar_target(bo_pr_2024, map(purrr::discard(c(bo_pr_24_1, bo_pr_24_2, bo_pr_24_3, bo_pr_24_4, bo_pr_24_5, bo_pr_24_6, bo_pr_24_7, bo_pr_24_8, bo_pr_24_9, bo_pr_24_10, bo_pr_24_11, bo_pr_24_12, bo_pr_24_13, bo_pr_24_14, bo_pr_24_15, bo_pr_24_16), is.null), ~mutate(.x, start = lubridate::ymd_hms(start), end = lubridate::ymd_hms(end)))),
  
  # Get the individual IDs so we can match them to gps points
  tar_target(device_ids_2022, purrr::map(bo_pr_2022, ~.x$device_id[1])),
  tar_target(device_ids_2023, purrr::map(bo_pr_2023, ~.x$device_id[1])),
  tar_target(device_ids_2024, purrr::map(bo_pr_2024, ~.x$device_id[1])),
  tar_target(gps_focal_indivs_2022, map(get_gps_forbouts_indivs(device_ids_2022, gps_2022), ~mutate(.x, ground_speed = as.numeric(ground_speed)))),
  tar_target(gps_focal_indivs_2023, map(get_gps_forbouts_indivs(device_ids_2023, gps_2023), ~mutate(.x, ground_speed = as.numeric(ground_speed)))),
  tar_target(gps_focal_indivs_2024, map(get_gps_forbouts_indivs(device_ids_2024, gps_2024), ~mutate(.x, ground_speed = as.numeric(ground_speed)))),
  tar_target(gps_spd, 4), # Matching Gideon's ACC paper
  tar_target(wg22_1, purrr::map2(bo_pr_2022[1:3], gps_focal_indivs_2022[1:3], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_2, purrr::map2(bo_pr_2022[4:6], gps_focal_indivs_2022[4:6], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_3, purrr::map2(bo_pr_2022[7:9], gps_focal_indivs_2022[7:9], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_4, purrr::map2(bo_pr_2022[10:12], gps_focal_indivs_2022[10:12], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_5, purrr::map2(bo_pr_2022[13:15], gps_focal_indivs_2022[13:15], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_6, purrr::map2(bo_pr_2022[16:18], gps_focal_indivs_2022[16:18], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_7, purrr::map2(bo_pr_2022[19:21], gps_focal_indivs_2022[19:21], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_8, purrr::map2(bo_pr_2022[22:24], gps_focal_indivs_2022[22:24], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_9, purrr::map2(bo_pr_2022[25:27], gps_focal_indivs_2022[25:27], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_10, purrr::map2(bo_pr_2022[28:30], gps_focal_indivs_2022[28:30], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_11, purrr::map2(bo_pr_2022[31:33], gps_focal_indivs_2022[31:33], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_12, purrr::map2(bo_pr_2022[34:36], gps_focal_indivs_2022[34:36], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_13, purrr::map2(bo_pr_2022[37:39], gps_focal_indivs_2022[37:39], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_14, purrr::map2(bo_pr_2022[40:42], gps_focal_indivs_2022[40:42], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_15, purrr::map2(bo_pr_2022[43:45], gps_focal_indivs_2022[43:45], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_16, purrr::map2(bo_pr_2022[46:48], gps_focal_indivs_2022[46:48], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_17, purrr::map2(bo_pr_2022[49:51], gps_focal_indivs_2022[49:51], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_18, purrr::map2(bo_pr_2022[52:54], gps_focal_indivs_2022[52:54], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_19, purrr::map2(bo_pr_2022[55:57], gps_focal_indivs_2022[55:57], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_20, purrr::map2(bo_pr_2022[58:60], gps_focal_indivs_2022[58:60], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_21, purrr::map2(bo_pr_2022[61:63], gps_focal_indivs_2022[61:63], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_22, purrr::map2(bo_pr_2022[64:66], gps_focal_indivs_2022[64:66], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_23, purrr::map2(bo_pr_2022[67:69], gps_focal_indivs_2022[67:69], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_24, purrr::map2(bo_pr_2022[70:72], gps_focal_indivs_2022[70:72], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_25, purrr::map2(bo_pr_2022[73:75], gps_focal_indivs_2022[73:75], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_26, purrr::map2(bo_pr_2022[76:length(bo_pr_2022)], gps_focal_indivs_2022[76:length(gps_focal_indivs_2022)], ~get_matches(.x, .y, gps_spd))),
  
  tar_target(wg23_1, purrr::map2(bo_pr_2023[1:3], gps_focal_indivs_2023[1:3], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_2, purrr::map2(bo_pr_2023[4:6], gps_focal_indivs_2023[4:6], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_3, purrr::map2(bo_pr_2023[7:9], gps_focal_indivs_2023[7:9], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_4, purrr::map2(bo_pr_2023[10:12], gps_focal_indivs_2023[10:12], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_5, purrr::map2(bo_pr_2023[13:15], gps_focal_indivs_2023[13:15], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_6, purrr::map2(bo_pr_2023[16:18], gps_focal_indivs_2023[16:18], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_7, purrr::map2(bo_pr_2023[19:21], gps_focal_indivs_2023[19:21], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_8, purrr::map2(bo_pr_2023[22:24], gps_focal_indivs_2023[22:24], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_9, purrr::map2(bo_pr_2023[25:27], gps_focal_indivs_2023[25:27], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_10, purrr::map2(bo_pr_2023[28:30], gps_focal_indivs_2023[28:30], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_11, purrr::map2(bo_pr_2023[31:33], gps_focal_indivs_2023[31:33], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_12, purrr::map2(bo_pr_2023[34:36], gps_focal_indivs_2023[34:36], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_13, purrr::map2(bo_pr_2023[37:39], gps_focal_indivs_2023[37:39], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_14, purrr::map2(bo_pr_2023[40:42], gps_focal_indivs_2023[40:42], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_15, purrr::map2(bo_pr_2023[43:45], gps_focal_indivs_2023[43:45], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_16, purrr::map2(bo_pr_2023[46:48], gps_focal_indivs_2023[46:48], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_17, purrr::map2(bo_pr_2023[49:51], gps_focal_indivs_2023[49:51], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_18, purrr::map2(bo_pr_2023[52:54], gps_focal_indivs_2023[52:54], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_19, purrr::map2(bo_pr_2023[55:57], gps_focal_indivs_2023[55:57], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_20, purrr::map2(bo_pr_2023[58:60], gps_focal_indivs_2023[58:60], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_21, purrr::map2(bo_pr_2023[61:63], gps_focal_indivs_2023[61:63], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_22, purrr::map2(bo_pr_2023[64:66], gps_focal_indivs_2023[64:66], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_23, purrr::map2(bo_pr_2023[67:69], gps_focal_indivs_2023[67:69], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_24, purrr::map2(bo_pr_2023[70:72], gps_focal_indivs_2023[70:72], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_25, purrr::map2(bo_pr_2023[73:75], gps_focal_indivs_2023[73:75], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_26, purrr::map2(bo_pr_2023[76:length(bo_pr_2023)], gps_focal_indivs_2023[76:length(gps_focal_indivs_2023)], ~get_matches(.x, .y, gps_spd))),
  
  tar_target(wg24_1, purrr::map2(bo_pr_2024[1:3], gps_focal_indivs_2024[1:3], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_2, purrr::map2(bo_pr_2024[4:6], gps_focal_indivs_2024[4:6], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_3, purrr::map2(bo_pr_2024[7:9], gps_focal_indivs_2024[7:9], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_4, purrr::map2(bo_pr_2024[10:12], gps_focal_indivs_2024[10:12], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_5, purrr::map2(bo_pr_2024[13:15], gps_focal_indivs_2024[13:15], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_6, purrr::map2(bo_pr_2024[16:18], gps_focal_indivs_2024[16:18], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_7, purrr::map2(bo_pr_2024[19:21], gps_focal_indivs_2024[19:21], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_8, purrr::map2(bo_pr_2024[22:24], gps_focal_indivs_2024[22:24], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_9, purrr::map2(bo_pr_2024[25:27], gps_focal_indivs_2024[25:27], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_10, purrr::map2(bo_pr_2024[28:30], gps_focal_indivs_2024[28:30], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_11, purrr::map2(bo_pr_2024[31:33], gps_focal_indivs_2024[31:33], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_12, purrr::map2(bo_pr_2024[34:36], gps_focal_indivs_2024[34:36], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_13, purrr::map2(bo_pr_2024[37:39], gps_focal_indivs_2024[37:39], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_14, purrr::map2(bo_pr_2024[40:42], gps_focal_indivs_2024[40:42], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_15, purrr::map2(bo_pr_2024[43:45], gps_focal_indivs_2024[43:45], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_16, purrr::map2(bo_pr_2024[46:48], gps_focal_indivs_2024[46:48], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_17, purrr::map2(bo_pr_2024[49:51], gps_focal_indivs_2024[49:51], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_18, purrr::map2(bo_pr_2024[52:54], gps_focal_indivs_2024[52:54], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_19, purrr::map2(bo_pr_2024[55:57], gps_focal_indivs_2024[55:57], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_20, purrr::map2(bo_pr_2024[58:60], gps_focal_indivs_2024[58:60], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_21, purrr::map2(bo_pr_2024[61:63], gps_focal_indivs_2024[61:63], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_22, purrr::map2(bo_pr_2024[64:66], gps_focal_indivs_2024[64:66], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_23, purrr::map2(bo_pr_2024[67:69], gps_focal_indivs_2024[67:69], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_24, purrr::map2(bo_pr_2024[70:72], gps_focal_indivs_2024[70:72], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_25, purrr::map2(bo_pr_2024[73:75], gps_focal_indivs_2024[73:75], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_26, purrr::map2(bo_pr_2024[76:length(bo_pr_2024)], gps_focal_indivs_2024[76:length(gps_focal_indivs_2024)], ~get_matches(.x, .y, gps_spd))),
  
  tar_target(with_gps_2022, c(wg22_1, wg22_2, wg22_3, wg22_4, wg22_5, wg22_6, wg22_7, wg22_8, wg22_9, wg22_10, wg22_11, wg22_12, wg22_13, wg22_14, wg22_15, wg22_16, wg22_17, wg22_18, wg22_19, wg22_20, wg22_21, wg22_22, wg22_23, wg22_24, wg22_25, wg22_26)),
  tar_target(with_gps_2023, c(wg23_1, wg23_2, wg23_3, wg23_4, wg23_5, wg23_6, wg23_7, wg23_8, wg23_9, wg23_10, wg23_11, wg23_12, wg23_13, wg23_14, wg23_15, wg23_16, wg23_17, wg23_18, wg23_19, wg23_20, wg23_21, wg23_22, wg23_23, wg23_24, wg23_25, wg23_26)),
  tar_target(with_gps_2024, c(wg24_1, wg24_2, wg24_3, wg24_4, wg24_5, wg24_6, wg24_7, wg24_8, wg24_9, wg24_10, wg24_11, wg24_12, wg24_13, wg24_14, wg24_15, wg24_16, wg24_17, wg24_18, wg24_19, wg24_20, wg24_21, wg24_22, wg24_23, wg24_24, wg24_25, wg24_26)),
  
  ## Attach the gps data back to the bouts and predictions
  tar_target(full_2022, map2(bo_pr_2022, with_gps_2022, ~join_gps_bouts(.x, .y))),
  tar_target(full_2023, map2(bo_pr_2023, with_gps_2023, ~join_gps_bouts(.x, .y))),
  tar_target(full_2024, map2(bo_pr_2024, with_gps_2024, ~join_gps_bouts(.x, .y))),
  
  ## Feeding bouts (high-frequency periods only)
  tar_target(feeding_bo_prob_thresh, 0.5),
  tar_target(feeding_bo_2022, map(full_2022, ~getfeeding(.x, feeding_bo_prob_thresh))),
  tar_target(feeding_bo_2023, map(full_2023, ~getfeeding(.x, feeding_bo_prob_thresh))),
  tar_target(feeding_bo_2024, map(full_2024, ~getfeeding(.x, feeding_bo_prob_thresh))),
  
  ## Bind them together to get all feeding bouts
  tar_target(feeding_bouts, mutate(as.data.frame(data.table::rbindlist(c(feeding_bo_2022, feeding_bo_2023, feeding_bo_2024), use.name = T, ignore.attr = T, fill = T)),
                                   year = lubridate::year(start),
                                   boutID = paste(device_id, bout_id, year, sep = "_"))), 
  tar_target(feeding_bo_spatial, st_transform(sf::st_as_sf(feeding_bouts, coords = c("location_long", "location_lat"), crs = "WGS84"), 32636)),
  
  ## Further restrictions on feeding bouts
  tar_target(feeding_bo_stationary, dplyr::filter(feeding_bo_spatial, as.numeric(ground_speed) <= gps_spd)),
  
  ## Use buffered cliffs layer created by Shaked to remove "feeding bouts" that are too much on a slope/cliff
  tar_target(filenames, list.files(here("data/created/DEMs_Shaked/final_25deg_100buff/"), pattern = ".gpkg", full.names = T)),
  tar_target(slopes_layer, st_transform(dplyr::bind_rows(purrr::map(filenames, ~sf::st_read(.x))), 32636)),
  tar_target(feeding_bo_nocliffs, feeding_bo_stationary[is.na(as.numeric(sf::st_intersects(feeding_bo_stationary, slopes_layer))),]),
  
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
  tar_target(dist_bo_stations, 750), # NNN don't have good justification for this
  tar_target(hours_after_carcass, 72),
  tar_target(carcass_bo, get_carcass_bouts(bouts = feeding_bo_nocliffs, # NNN look into which stations are 142m apart. # looks like Tzaror_trap and Tzaror_mount, which I think we will end up merging into the same one anyway.
                                           carcasses = carcasses_focal,
                                           dist = dist_bo_stations,
                                           hours_after = hours_after_carcass)),
  tar_target(station_bo, get_station_bouts(bouts = feeding_bo_nocliffs, stations = stations, dist = dist_bo_stations)),
  tar_target(non_station_bo, filter(feeding_bo_nocliffs, !(boutID %in% station_bo$boutID))),

  ## Cluster the remaining bouts to detect wild carcasses
  tar_target(wild_dist, 200), #chosen in meeting with Orr
  tar_target(wild_time_hrs, 24), # chosen in meeting with Orr
  tar_target(wild_min_pts, 3), # common sense-- we've always used 3. Validated in meeting with Orr.
  tar_target(non_station_bo_prepped, bind_cols(mutate(non_station_bo, 
                                                      timestamp_numeric = 
                                                        as.numeric(difftime(timestamp, 
                                                                            min(timestamp), units = "secs"))), sf::st_coordinates(non_station_bo))),
  tar_target(wild_carcass_bo_df, get_clusters_from_data(non_station_bo_prepped, 
                                                        x = "X", y = "Y", 
                                                        t = "timestamp_numeric",
                                                        eps = wild_dist, 
                                                        eps_t = wild_time_hrs*60*60, 
                                                        minpts = wild_min_pts)),
  tar_target(wild_carcasses, get_wild_carcasses(wild_carcass_bo_df)),
  tar_target(validation, filter(sf::st_read("data/raw/wildCarcassValidation/cluster_centroids_200m_24hr_min3_2022_2023_2022_NOCLIFFS_withnames.kml"), Name != "")),
  tar_target(validation_cleaned, mutate(separate_wider_delim(separate_wider_delim(validation, cols = "Name", delim = "_", names = c("carcIDs", "status")), cols = "carcIDs", delim = "(", names = c("carcID", "carcID_old"), too_few = "align_start"), carcID_old = str_remove_all(carcID_old, "\\)"))),
  tar_target(validation_tojoin, dplyr::mutate(dplyr::filter(dplyr::select(validation_cleaned, carcID, status), status == "valid"), carcID = as.integer(carcID))),
  tar_target(wild_carcasses_validated, left_join(validation_tojoin, wild_carcasses, by = "carcID")),
  # ## Combine wild and stn carcasses
  tar_target(all_carcasses, bind_rows(carcasses_focal %>% mutate(carcType = "stn", year = lubridate::year(date)), wild_carcasses_validated %>% dplyr::mutate("date" = lubridate::ymd(dateOnly)) %>% dplyr::select(-dateOnly))), 
  
  tar_target(bbox_south_big, sf::st_transform(
    st_as_sfc(st_set_crs(st_bbox(c("xmin" = 34.205, "xmax" = 35.787,
                                   "ymin" = 29.478, "ymax" = 31.775)),
                         "WGS84")), 32636)), ### NNN can I just draw a line at Jerusalem and take everything south of that? Does it change anything?
  tar_target(jerusalem_northing_36n, 3514000),

  ## Dynamic NBDA testing
  ## 0. Define parameters
  tar_target(days_after, 3),
  tar_target(days_before, 1),
  tar_target(days_before_wild, 3),
  ## 1. Get carcasses and restrict to south
  tar_target(all_carcasses_south, filter(all_carcasses, Y < jerusalem_northing)),
  ## 2. Separate stn and wild
  tar_target(stn, filter(all_carcasses_south, carcType == "stn")),
  tar_target(stn_carcs, group_split(group_by(stn, carcID))),
  tar_target(wild, filter(all_carcasses_south, carcType == "wild")),
  tar_target(wild_carcs, group_split(group_by(wild, carcID))),

  # download data to match high frequency period, plus buffer
  tar_target(ornitela_data_2022, readRDS(here("data/ornitela_data_2022_version2025-09-21.RDS"))),
  tar_target(ornitela_data_2023, readRDS(here("data/ornitela_data_2023_version2025-09-21.RDS"))),
  tar_target(ornitela_data_2024, readRDS(here("data/ornitela_data_2024_version2025-09-21.RDS"))),
  tar_target(inpa_data_2022, readRDS(here("data/inpa_data_2022_version2025-09-21.RDS"))),
  tar_target(inpa_data_2023, readRDS(here("data/inpa_data_2023_version2025-09-21.RDS"))),
  tar_target(inpa_data_2024, readRDS(here("data/inpa_data_2024_version2025-09-21.RDS"))),

  tar_target(gps_1, purrr::map2(.x = list(ornitela_data_2022, ornitela_data_2023, ornitela_data_2024), .y = list(inpa_data_2022, inpa_data_2023, inpa_data_2024), ~st_as_sf(bind_rows(as.data.frame(.x), as.data.frame(.y)), crs = "WGS84"))),
  
  tar_target(gps, purrr::map(gps_1, ~dplyr::bind_cols(.x, setNames(as.data.frame(sf::st_coordinates(.x)), c("location_long", "location_lat"))))),
  
  tar_target(gps_combined, st_transform(st_as_sf(purrr::list_rbind(gps)), 32636)),
  
  tar_target(fixed_names_ages, fix_names_ages(gps_combined, ww_file)),

  # Data cleaning -----------------------------------------------------------
  tar_target(ww_file, "data/raw/whoswho_vultures_20250422_new.xlsx", format = "file"), # DONE
  tar_target(ww, readxl::read_excel(ww_file, sheet = "all gps tags")),
  ## Remove dates before/after the deploy period
  tar_target(removed_beforeafter_deploy, process_deployments(ww_file,
                                                             fixed_names_ages,
                                                             default_end_date = as.Date("2025-09-21"),
                                                             verbose = TRUE)), # DONE
  ## Remove hospital/invalid periods (# XXX COME BACK TO THIS)
  # tar_target(removed_periods, remove_periods(ww_file, removed_beforeafter_deploy)),
  ## Clean the data with the various steps in the vultureUtils::cleanData function
  tar_target(cleaned, clean_data(removed_beforeafter_deploy)),

  # START HERE
  # ## Mask data with the israel region mask
  # tar_target(mask, "data/raw/CutOffRegion.kml", format = "file"),
  # tar_target(data_masked, mask_data(with_age_sex, mask)),
  ## If any vultures have too *high* a fix rate, downsample it to every 10 minutes so it's easier to work with.
  tar_target(downsampled, mutate(sf::st_transform(sf::st_as_sf(downsample_10min(cleaned), coords = c("location_long", "location_lat"), crs = "WGS84"), 32636), timestamp_il = lubridate::with_tz(timestamp, tzone = "Israel"), date_il = lubridate::date(timestamp_il))),

  # (End data cleaning) -----------------------------------------------------

  # Preparing data for NBDA -------------------------------------------------
  ## Stn carcasses
  tar_target(stb_mins, 30),
  tar_target(ddf, 2000),
  tar_target(dds, 1000),
  tar_target(dbf, 30), # will need to get gps data 30 days before in order to get longer-term networks
  tar_target(stn_gps_30days, get_gps_all(stn_carcs, downsampled, days_after, dbf)),
  tar_target(wild_gps_30days, get_gps_all(wild_carcs, downsampled, days_after, dbf)),

  tar_target(stn_gps_forroosts, map(stn_gps_30days, ~filter(arrange(.x, date_il), date_il %in% tail(unique(date_il), 6)))),
  tar_target(wild_gps_forroosts, map(wild_gps_30days, ~filter(arrange(.x, date_il), date_il %in% tail(unique(date_il), 6)))),

  tar_target(idname, "individual_local_identifier"),
  tar_target(tsname, "timestamp_il"),
  tar_target(tzname, "Israel"),
  tar_target(roosts_stn_1, NEW_get_roosts(stn_gps_forroosts[1:10], id = idname, ts = tsname, tz = tzname)),
  tar_target(roosts_stn_2, NEW_get_roosts(stn_gps_forroosts[11:20], id = idname, ts = tsname, tz = tzname)),
  tar_target(roosts_stn_3, NEW_get_roosts(stn_gps_forroosts[21:30], id = idname, ts = tsname, tz = tzname)),
  tar_target(roosts_stn_4, NEW_get_roosts(stn_gps_forroosts[31:40], id = idname, ts = tsname, tz = tzname)),
  tar_target(roosts_stn_5, NEW_get_roosts(stn_gps_forroosts[41:50], id = idname, ts = tsname, tz = tzname)),
  tar_target(roosts_stn_6, NEW_get_roosts(stn_gps_forroosts[51:length(stn_gps_forroosts)], id = idname, ts = tsname, tz = tzname)),

  tar_target(roosts_wild_1, NEW_get_roosts(wild_gps_forroosts[1:10], id = idname, ts = tsname, tz = tzname)),
  tar_target(roosts_wild_2, NEW_get_roosts(wild_gps_forroosts[11:20], id = idname, ts = tsname, tz = tzname)),
  tar_target(roosts_wild_3, NEW_get_roosts(wild_gps_forroosts[21:30], id = idname, ts = tsname, tz = tzname)),
  tar_target(roosts_wild_4, NEW_get_roosts(wild_gps_forroosts[31:40], id = idname, ts = tsname, tz = tzname)),
  tar_target(roosts_wild_5, NEW_get_roosts(wild_gps_forroosts[41:length(wild_gps_forroosts)], id = idname, ts = tsname, tz = tzname)),

  tar_target(roosts_stn, c(roosts_stn_1, roosts_stn_2, roosts_stn_3, roosts_stn_4, roosts_stn_5, roosts_stn_6)),
  tar_target(roosts_wild, c(roosts_wild_1, roosts_wild_2, roosts_wild_3, roosts_wild_4, roosts_wild_5)),

  # Prepare NBDA data
  ## Prepare NBDA data--stn carcs
  tar_target(nd1, nb_shortcut(stn_gps_30days[1:10], ddf, dds, gps_spd, hours_after_carcass, stb_mins, seeds = T, carcass_data_list = stn_carcs[1:10], age_ilv = T)),
  tar_target(nd2, nb_shortcut(stn_gps_30days[11:20], ddf, dds, gps_spd, hours_after_carcass, stb_mins, seeds = T, carcass_data_list = stn_carcs[11:20], age_ilv = T)),
  tar_target(nd3, nb_shortcut(stn_gps_30days[21:30], ddf, dds, gps_spd, hours_after_carcass, stb_mins, seeds = T, carcass_data_list = stn_carcs[21:30], age_ilv = T)),
  tar_target(nd4, nb_shortcut(stn_gps_30days[31:40], ddf, dds, gps_spd, hours_after_carcass, stb_mins, seeds = T, carcass_data_list = stn_carcs[31:40], age_ilv = T)),
  tar_target(nd5, nb_shortcut(stn_gps_30days[41:50], ddf, dds, gps_spd, hours_after_carcass, stb_mins, seeds = T, carcass_data_list = stn_carcs[41:50], age_ilv = T)),
  tar_target(nd6, nb_shortcut(stn_gps_30days[51:60], ddf, dds, gps_spd, hours_after_carcass, stb_mins, seeds = T, carcass_data_list = stn_carcs[51:60], age_ilv = T)),

  # NNN implement the Elvira change to the order in the flight/feeding network functions. That should eliminate a lot of the NAs. The remaining ones will be set to 0, resulting in

  # Flight networks
  ## Flight networks--Cumulative (stn)
  ### Flight networks--Cumulative (stn)--Weighted
  tar_target(fl_wt_cumulative_1_prelim, purrr::map(nd1, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_2_prelim, purrr::map(nd2, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_3_prelim, purrr::map(nd3, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_4_prelim, purrr::map(nd4, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_5_prelim, purrr::map(nd5, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),
  tar_target(fl_wt_cumulative_6_prelim, purrr::map(nd6, ~{purrr::map(.x$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf))})),

  tar_target(fl_wt_cumulative_1, purrr::map2(fl_wt_cumulative_1_prelim, nd1, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_2, purrr::map2(fl_wt_cumulative_2_prelim, nd2, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_3, purrr::map2(fl_wt_cumulative_3_prelim, nd3, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_4, purrr::map2(fl_wt_cumulative_4_prelim, nd4, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_5, purrr::map2(fl_wt_cumulative_5_prelim, nd5, ~fix_nets(.x, .y$all_indivs_sorted))),
  tar_target(fl_wt_cumulative_6, purrr::map2(fl_wt_cumulative_6_prelim, nd6, ~fix_nets(.x, .y$all_indivs_sorted))),

  # Prepare data for NBDA
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
  ## NBDA models
  ### Cumulative, weighted (stn)
  tar_target(mods_cumul_wt, purrr::map(c(data_cumul_wt_1, data_cumul_wt_2, data_cumul_wt_3, data_cumul_wt_4, data_cumul_wt_5, data_cumul_wt_6), ~{tryCatch(oadaFit(.x, type = "social"), error = function(e) NULL)})),

  tar_target(stats_cumul_wt, mutate(purrr::list_rbind(map(mods_cumul_wt, getmodstats)), type = "cumul", binwt = "wt", seeds = T, carcID = purrr::map_dbl(stn_carcs, "carcID"))),

  tar_target(stats, purrr::list_rbind(list(stats_cumul_wt
  ))),

  ## Number of individuals involved in each diffusion
  tar_target(ns, purrr::list_rbind(purrr::map(c(nd1, nd2, nd3, nd4, nd5, nd6), ~{as.data.frame(t(unlist(.x[1:4])))})))
)