library(targets)
library(tarchetypes)
library(crew)

# Set target options:
tar_option_set(
  error = "null",
  packages = c("plyr", "vultureUtils", "tidyverse", "here", "NBDA", "sf", "dplyr", "lubridate", "ranger", "tidymodels", "moments", "parsnip", "caret", "zoo", "move", "terra", "readxl", "data.table", "geosphere", "tidygraph", "STbayes"),
  controller = crew_controller_local(workers = 10)
)

lapply(list.files("R", full.names = TRUE), source) 

list(
  tar_target(rp, sf::st_read("data/raw/roosts50_kde95_cutOffRegion.kml")),
  tar_target(rp_minus_stations, sf::st_difference(sf::st_transform(rp, 32636), stations_union)),
  
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
  tar_target(device_ids, list(purrr::map_dbl(bo_pr_2022, ~.x$device_id[1]), purrr::map_dbl(bo_pr_2023, ~.x$device_id[1]), purrr::map_dbl(bo_pr_2024, ~.x$device_id[1]))),
  
  tar_target(gps_focal_indivs, map2(.x = device_ids, .y = gps, ~get_gps_forbouts_indivs(.x, .y))),
  tar_target(gps_spd, 4), # Matching Gideon's ACC paper
  tar_target(wg22_1, purrr::map2(bo_pr_2022[1:3], gps_focal_indivs[[1]][1:3], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_2, purrr::map2(bo_pr_2022[4:6], gps_focal_indivs[[1]][4:6], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_3, purrr::map2(bo_pr_2022[7:9], gps_focal_indivs[[1]][7:9], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_4, purrr::map2(bo_pr_2022[10:12], gps_focal_indivs[[1]][10:12], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_5, purrr::map2(bo_pr_2022[13:15], gps_focal_indivs[[1]][13:15], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_6, purrr::map2(bo_pr_2022[16:18], gps_focal_indivs[[1]][16:18], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_7, purrr::map2(bo_pr_2022[19:21], gps_focal_indivs[[1]][19:21], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_8, purrr::map2(bo_pr_2022[22:24], gps_focal_indivs[[1]][22:24], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_9, purrr::map2(bo_pr_2022[25:27], gps_focal_indivs[[1]][25:27], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_10, purrr::map2(bo_pr_2022[28:30], gps_focal_indivs[[1]][28:30], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_11, purrr::map2(bo_pr_2022[31:33], gps_focal_indivs[[1]][31:33], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_12, purrr::map2(bo_pr_2022[34:36], gps_focal_indivs[[1]][34:36], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_13, purrr::map2(bo_pr_2022[37:39], gps_focal_indivs[[1]][37:39], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_14, purrr::map2(bo_pr_2022[40:42], gps_focal_indivs[[1]][40:42], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_15, purrr::map2(bo_pr_2022[43:45], gps_focal_indivs[[1]][43:45], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_16, purrr::map2(bo_pr_2022[46:48], gps_focal_indivs[[1]][46:48], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_17, purrr::map2(bo_pr_2022[49:51], gps_focal_indivs[[1]][49:51], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_18, purrr::map2(bo_pr_2022[52:54], gps_focal_indivs[[1]][52:54], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_19, purrr::map2(bo_pr_2022[55:57], gps_focal_indivs[[1]][55:57], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_20, purrr::map2(bo_pr_2022[58:60], gps_focal_indivs[[1]][58:60], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_21, purrr::map2(bo_pr_2022[61:63], gps_focal_indivs[[1]][61:63], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_22, purrr::map2(bo_pr_2022[64:66], gps_focal_indivs[[1]][64:66], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_23, purrr::map2(bo_pr_2022[67:69], gps_focal_indivs[[1]][67:69], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_24, purrr::map2(bo_pr_2022[70:72], gps_focal_indivs[[1]][70:72], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_25, purrr::map2(bo_pr_2022[73:75], gps_focal_indivs[[1]][73:75], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg22_26, purrr::map2(bo_pr_2022[76:length(bo_pr_2022)], gps_focal_indivs[[1]][76:length(gps_focal_indivs[[1]])], ~get_matches(.x, .y, gps_spd))),
  
  tar_target(wg23_1, purrr::map2(bo_pr_2023[1:3], gps_focal_indivs[[2]][1:3], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_2, purrr::map2(bo_pr_2023[4:6], gps_focal_indivs[[2]][4:6], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_3, purrr::map2(bo_pr_2023[7:9], gps_focal_indivs[[2]][7:9], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_4, purrr::map2(bo_pr_2023[10:12], gps_focal_indivs[[2]][10:12], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_5, purrr::map2(bo_pr_2023[13:15], gps_focal_indivs[[2]][13:15], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_6, purrr::map2(bo_pr_2023[16:18], gps_focal_indivs[[2]][16:18], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_7, purrr::map2(bo_pr_2023[19:21], gps_focal_indivs[[2]][19:21], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_8, purrr::map2(bo_pr_2023[22:24], gps_focal_indivs[[2]][22:24], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_9, purrr::map2(bo_pr_2023[25:27], gps_focal_indivs[[2]][25:27], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_10, purrr::map2(bo_pr_2023[28:30], gps_focal_indivs[[2]][28:30], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_11, purrr::map2(bo_pr_2023[31:33], gps_focal_indivs[[2]][31:33], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_12, purrr::map2(bo_pr_2023[34:36], gps_focal_indivs[[2]][34:36], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_13, purrr::map2(bo_pr_2023[37:39], gps_focal_indivs[[2]][37:39], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_14, purrr::map2(bo_pr_2023[40:42], gps_focal_indivs[[2]][40:42], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_15, purrr::map2(bo_pr_2023[43:45], gps_focal_indivs[[2]][43:45], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_16, purrr::map2(bo_pr_2023[46:48], gps_focal_indivs[[2]][46:48], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_17, purrr::map2(bo_pr_2023[49:51], gps_focal_indivs[[2]][49:51], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_18, purrr::map2(bo_pr_2023[52:54], gps_focal_indivs[[2]][52:54], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_19, purrr::map2(bo_pr_2023[55:57], gps_focal_indivs[[2]][55:57], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_20, purrr::map2(bo_pr_2023[58:60], gps_focal_indivs[[2]][58:60], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_21, purrr::map2(bo_pr_2023[61:63], gps_focal_indivs[[2]][61:63], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_22, purrr::map2(bo_pr_2023[64:66], gps_focal_indivs[[2]][64:66], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_23, purrr::map2(bo_pr_2023[67:69], gps_focal_indivs[[2]][67:69], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_24, purrr::map2(bo_pr_2023[70:72], gps_focal_indivs[[2]][70:72], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_25, purrr::map2(bo_pr_2023[73:75], gps_focal_indivs[[2]][73:75], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg23_26, purrr::map2(bo_pr_2023[76:length(bo_pr_2023)], gps_focal_indivs[[2]][76:length(gps_focal_indivs[[2]])], ~get_matches(.x, .y, gps_spd))),
  
  tar_target(wg24_1, purrr::map2(bo_pr_2024[1:3], gps_focal_indivs[[3]][1:3], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_2, purrr::map2(bo_pr_2024[4:6], gps_focal_indivs[[3]][4:6], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_3, purrr::map2(bo_pr_2024[7:9], gps_focal_indivs[[3]][7:9], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_4, purrr::map2(bo_pr_2024[10:12], gps_focal_indivs[[3]][10:12], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_5, purrr::map2(bo_pr_2024[13:15], gps_focal_indivs[[3]][13:15], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_6, purrr::map2(bo_pr_2024[16:18], gps_focal_indivs[[3]][16:18], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_7, purrr::map2(bo_pr_2024[19:21], gps_focal_indivs[[3]][19:21], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_8, purrr::map2(bo_pr_2024[22:24], gps_focal_indivs[[3]][22:24], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_9, purrr::map2(bo_pr_2024[25:27], gps_focal_indivs[[3]][25:27], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_10, purrr::map2(bo_pr_2024[28:30], gps_focal_indivs[[3]][28:30], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_11, purrr::map2(bo_pr_2024[31:33], gps_focal_indivs[[3]][31:33], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_12, purrr::map2(bo_pr_2024[34:36], gps_focal_indivs[[3]][34:36], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_13, purrr::map2(bo_pr_2024[37:39], gps_focal_indivs[[3]][37:39], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_14, purrr::map2(bo_pr_2024[40:42], gps_focal_indivs[[3]][40:42], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_15, purrr::map2(bo_pr_2024[43:45], gps_focal_indivs[[3]][43:45], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_16, purrr::map2(bo_pr_2024[46:48], gps_focal_indivs[[3]][46:48], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_17, purrr::map2(bo_pr_2024[49:51], gps_focal_indivs[[3]][49:51], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_18, purrr::map2(bo_pr_2024[52:54], gps_focal_indivs[[3]][52:54], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_19, purrr::map2(bo_pr_2024[55:57], gps_focal_indivs[[3]][55:57], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_20, purrr::map2(bo_pr_2024[58:60], gps_focal_indivs[[3]][58:60], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_21, purrr::map2(bo_pr_2024[61:63], gps_focal_indivs[[3]][61:63], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_22, purrr::map2(bo_pr_2024[64:66], gps_focal_indivs[[3]][64:66], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_23, purrr::map2(bo_pr_2024[67:69], gps_focal_indivs[[3]][67:69], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_24, purrr::map2(bo_pr_2024[70:72], gps_focal_indivs[[3]][70:72], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_25, purrr::map2(bo_pr_2024[73:75], gps_focal_indivs[[3]][73:75], ~get_matches(.x, .y, gps_spd))),
  tar_target(wg24_26, purrr::map2(bo_pr_2024[76:length(bo_pr_2024)], gps_focal_indivs[[3]][76:length(gps_focal_indivs[[3]])], ~get_matches(.x, .y, gps_spd))),
  
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
  tar_target(stations_buffered, sf::st_buffer(stations, 1000)),
  tar_target(stations_union, sf::st_union(stations_buffered)),
  
  ## Station carcasses
  ### Created in 00_carcass_data_translation.R
  tar_target(carcasses_audited, readRDS(here("data/created/carcasses_audited.RDS"))),
  
  ## Focal carcasses
  ### During the 2022, 2023 and 2024 HF-ACC periods
  tar_target(carcasses_focal, get_focal(carcasses_audited, minmax_dates)), 
  
  ## Match bouts to carcasses
  tar_target(dist_bo_stations, 750), # NNN don't have good justification for this
  tar_target(hours_after_carcass, 72),
  # tar_target(carcass_bo, get_carcass_bouts(bouts = feeding_bo_nocliffs, # NNN look into which stations are 142m apart. # looks like Tzaror_trap and Tzaror_mount, which I think we will end up merging into the same one anyway.
  #                                          carcasses = carcasses_focal,
  #                                          dist = dist_bo_stations,
  #                                          hours_after = hours_after_carcass)),
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
  tar_target(validation, filter(sf::st_read("data/raw/wildCarcassValidation/cluster_centroids_200m_24hr_min3_2022_2023_2024_NOCLIFFS_withnames.kml"), Name != "")),
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
  # tar_target(days_before, 1),
  # tar_target(days_before_wild, 3),
  ## 1. Get carcasses and restrict to south
  tar_target(all_carcasses_south, filter(all_carcasses, Y < jerusalem_northing_36n)),
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
  
  tar_target(gps_combined, st_transform(st_as_sf(mutate(purrr::list_rbind(gps), ground_speed = as.numeric(ground_speed))), 32636)),
  
  tar_target(fixed_names_ages, fix_names_ages(gps_combined, ww_file)),
  
  # Data cleaning -----------------------------------------------------------
  tar_target(ww_file, "data/raw/whoswho_vultures_20250422_new.xlsx", format = "file"), # DONE
  # tar_target(ww, readxl::read_excel(ww_file, sheet = "all gps tags")),
  ## Remove dates before/after the deploy period
  tar_target(removed_beforeafter_deploy, process_deployments(ww_file,
                                                             fixed_names_ages,
                                                             default_end_date = as.Date("2025-09-21"),
                                                             verbose = TRUE)), # DONE
  
  ## Clean the data with the various steps in the vultureUtils::cleanData function
  tar_target(cleaned, clean_data(removed_beforeafter_deploy)),
  
  # Note: we decided NOT to mask the data to the israel region because we don't need to limit the area in which social interactions could have occurred. We did mask the carcasses, though, taking only the ones south of Jerusalem. In addition, Shaked and I used visual inspection to classify wild carcasses only in the Israel/Jordan area and not farther out. To compare, can look at the original wild_carcasses file and then the validated one, and notice that the ones that didn't have a "status" assigned were outside of the geographic area. There weren't any edge cases.
  ## If any vultures have too *high* a fix rate, downsample it to every 10 minutes so it's easier to work with.
  tar_target(downsampled, mutate(sf::st_transform(sf::st_as_sf(downsample_10min(cleaned), coords = c("location_long", "location_lat"), crs = "WGS84"), 32636), timestamp_il = lubridate::with_tz(timestamp, tzone = "Israel"), date_il = lubridate::date(timestamp_il))),
  
  # Remove hospital/invalid periods
  tar_target(removed_periods, remove_periods(ww_file, downsampled)),
  # (End data cleaning) -----------------------------------------------------
  
  # Preparing data for NBDA -------------------------------------------------
  ## Stn carcasses
  tar_target(stb_mins, 30), # seed time before (mins)
  tar_target(ddf, 2000), # detection distance (flight) (m)
  tar_target(dds, 1000), # detection distance (stationary) (m)
  tar_target(dbf, 30), # days before carcass to get data for longer-term networks
  tar_target(stn_gps_30days, get_gps_all(stn_carcs, downsampled, days_after, dbf)),
  tar_target(wild_gps_30days, get_gps_all(wild_carcs, downsampled, days_after, dbf)),
  
  tar_target(stn_gps_forroosts, map(stn_gps_30days, ~filter(arrange(.x, date_il), date_il %in% tail(unique(date_il), 6)))),
  tar_target(wild_gps_forroosts, map(wild_gps_30days, ~filter(arrange(.x, date_il), date_il %in% tail(unique(date_il), 6)))),
  
  tar_target(idname, "individual_local_identifier"),
  tar_target(tsname, "timestamp_il"),
  tar_target(tzname, "Israel"),
  tar_target(downsampled_forroosts, rename(bind_cols(st_coordinates(st_transform(downsampled, "WGS84")), downsampled), "location_long" = X, "location_lat" = Y)),
  tar_target(roosts_all, sf::st_as_sf(NEW_get_roosts(list(downsampled_forroosts), id = idname, ts = tsname, tz = tzname)[[1]])),
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
  
  # Manual calculation of co-departures from roosts and following
  tar_target(roosts_all_updated, mutate(roosts_all, roostID = as.numeric(st_intersects(sf::st_transform(roosts_all, 32636), rp_minus_stations)))),
  
  tar_target(downsampled_updated, dplyr::mutate(downsampled_forroosts, roostID_gps = as.numeric(sf::st_intersects(sf::st_transform(sf::st_as_sf(downsampled_forroosts), 32636), rp_minus_stations)))),
  
  tar_target(roosts_tojoin, dplyr::rename(sf::st_drop_geometry(dplyr::bind_cols(dplyr::select(roosts_all_updated, individual_local_identifier, roost_date, roostID), sf::st_coordinates(roosts_all_updated))), "roost_X" = X, "roost_Y" = Y)),
  
  tar_target(gps_joined, dplyr::mutate(dplyr::left_join(dplyr::mutate(downsampled_updated, roost_date = date_il-lubridate::days(1)), roosts_tojoin, by = c("individual_local_identifier", "roost_date")), in_a_roost = !is.na(roostID_gps))),
  
  tar_target(gps_joined_knownroost, dplyr::filter(gps_joined, !is.na(roostID))),
  tar_target(indiv_date_list, group_split(group_by(gps_joined_knownroost, date_il, individual_local_identifier), .keep = T)),
  tar_target(leftpoints, purrr::map_dbl(indiv_date_list, ~get_leftroost(.x, threshold = 2))),
  tar_target(data_timeordered, purrr::map2(indiv_date_list, leftpoints, ~{
    .x$left_roost <- FALSE
    if(!is.na(.y)){.x$left_roost[.y] <- TRUE}
    return(.x)})),
  tar_target(data_rejoined, sf::st_as_sf(as.data.frame(data.table::rbindlist(data_timeordered)), crs = 32636)),
  tar_target(leaving_points, dplyr::filter(data_rejoined, left_roost)),
  tar_target(leaving_points_dates, group_split(group_by(leaving_points, date_il), .keep = T)),
  tar_target(dates, purrr::map_chr(leaving_points_dates, ~as.character(.x$date_il[1]))),
  tar_target(roost_mats, setNames(purrr::map(leaving_points_dates, ~{
    mat <- outer(.x$roostID, .x$roostID, FUN = "==") * 1
    rownames(mat) <- .x$individual_local_identifier
    colnames(mat) <- .x$individual_local_identifier
    return(mat)}), dates)),
  tar_target(roost_mats_long, setNames(purrr::map(roost_mats, ~{as.data.frame(.x) %>% rownames_to_column("ID1") %>% pivot_longer(cols = -ID1, names_to = "ID2", values_to = "same_roost")}), dates)),
  tar_target(roost_mats_same_whichroost, filter(left_join(mutate(purrr::list_rbind(roost_mats_long, names_to = "date_il"), date_il = lubridate::ymd(date_il)), leaving_points, by = c("ID1" = "individual_local_identifier", "date_il")), same_roost == 1)),
  tar_target(difftime_mats, setNames(purrr::map(leaving_points_dates, ~{
    mat <- outer(.x$timestamp_il, .x$timestamp_il,
                 function(t1, t2) as.numeric(abs(difftime(t1, t2, units = "mins"))))
    rownames(mat) <- .x$individual_local_identifier
    colnames(mat) <- .x$individual_local_identifier
    return(mat)}), dates)),
  tar_target(difftime_mats_long, setNames(purrr::map(difftime_mats, ~{as.data.frame(.x) %>% rownames_to_column("ID1") %>% pivot_longer(cols = -ID1, names_to = "ID2", values_to = "time_diff_min")}), dates)),
  tar_target(both, setNames(purrr::map2(roost_mats_long, difftime_mats_long, ~dplyr::left_join(.x, .y, by = c("ID1", "ID2"))), dates)),
  tar_target(departure_times, setNames(purrr::map(both, ~{dplyr::filter(.x, same_roost == 1) %>% dplyr::select(-same_roost) %>% filter(ID1 < ID2)}), dates)),
  tar_target(sync_departures, setNames(purrr::map(departure_times, ~filter(.x, time_diff_min <= 10)), dates)),
  tar_target(sync_departures_df, mutate(purrr::list_rbind(sync_departures, names_to = "date_il"), year = lubridate::year(date_il))),
  tar_target(departure_edgelists, purrr::map2(departure_times, leaving_points_dates, ~{.x %>% rename("from" = ID1, "to" = ID2) %>%
      mutate(weight = 1 / (time_diff_min + 1))})),
  tar_target(departure_nets, purrr::map2(departure_edgelists, leaving_points_dates, ~{
    tidygraph::tbl_graph(edges = .x, directed = F) %>%
      tidygraph::activate(nodes) %>%
      dplyr::left_join(dplyr::distinct(dplyr::select(.y, individual_local_identifier, roostID)), by = c("name" = "individual_local_identifier"))})),
  
  tar_target(data_split_years, dplyr::group_split(dplyr::arrange(dplyr::mutate(data_rejoined, year = case_when(date_il < lubridate::ymd("2023-01-01") ~ 2022, date_il > lubridate::ymd("2023-01-01") & date_il < lubridate::ymd("2023-07-01") ~ 2023, date_il > lubridate::ymd("2023-07-01") ~ 2024)), individual_local_identifier), year, .keep = T)),
  
  # Trajectories after departure
  tar_target(mv, purrr::map(data_split_years, ~move2::mt_as_move2(
    .x,
    time = "timestamp_il", track_id = "individual_local_identifier",
    crs = st_crs(data_rejoined)))),
  
  tar_target(interpolated_10min, purrr::map(mv, ~move2::mt_interpolate(
    .x[!sf::st_is_empty(.x), ],
    time = seq(
      as.POSIXct(min(.x$date_il, na.rm = T)),
      as.POSIXct(max(.x$date_il, na.rm = T)+lubridate::days(1)), "10 mins"
    ),
    max_time_lag = units::as_units(1, "hours"),
    omit = TRUE
  ) %>%
    mutate(interp = T) %>%
    bind_rows(mutate(.x[!sf::st_is_empty(.x), ], interp = F)) %>%
    arrange(individual_local_identifier, timestamp_il) %>%
    ungroup())),
  
  tar_target(interpolated_tidied, purrr::map(interpolated_10min, ~{
    .x %>% 
      dplyr::select(individual_local_identifier, date_il, timestamp_il, ground_speed, interp, roost_X, roost_Y, roostID, roostID_gps, in_a_roost, left_roost) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(flight = ground_speed > gps_spd) %>% 
      arrange(individual_local_identifier, timestamp_il) %>% 
      tidyr::fill(date_il) %>% 
      dplyr::group_by(individual_local_identifier, date_il) %>% 
      tidyr::fill(flight) %>% 
      tidyr::fill(roost_X) %>% 
      tidyr::fill(roost_Y) %>% 
      tidyr::fill(roostID) %>% 
      tidyr::fill(left_roost) %>% 
      dplyr::ungroup()})),
  
  tar_target(after_departure, purrr::map(interpolated_tidied, ~{.x %>%
      dplyr::group_by(individual_local_identifier, date_il) %>%
      dplyr::mutate(after = cumsum(left_roost)) %>%
      dplyr::filter(after > 0) %>%
      dplyr::ungroup() %>% dplyr::select(-after)})),
  
  tar_target(after_departure_interp_only, purrr::map(after_departure, ~filter(.x, interp))),
  
  tar_target(sync_departures_list, dplyr::group_split(sync_departures_df, year, .keep = TRUE)),
  
  tar_target(trajectories_sync_list_2022, get_trajectories_sync_pair(sync_departures_list[[1]], after_departure_interp_only[[1]])),
  tar_target(trajectories_sync_list_2023, get_trajectories_sync_pair(sync_departures_list[[2]], after_departure_interp_only[[2]])),
  tar_target(trajectories_sync_list_2024, get_trajectories_sync_pair(sync_departures_list[[3]], after_departure_interp_only[[3]])),
  
  tar_target(trajectories_sync_2022, purrr::list_rbind(trajectories_sync_list_2022)),
  tar_target(trajectories_sync_2023, purrr::list_rbind(trajectories_sync_list_2023)),
  tar_target(trajectories_sync_2024, purrr::list_rbind(trajectories_sync_list_2024)),
  
  tar_target(trajectories_sync, mutate(purrr::list_rbind(setNames(list(trajectories_sync_2022, trajectories_sync_2023, trajectories_sync_2024), c("2022", "2023", "2024")), names_to = "year"), date_il = lubridate::date(timestamp_il))),
  
  # stBayes: dynamic
  ## stn
  tar_target(gps_withdaylight, purrr::map2(stn_gps_30days, stn_carcs, ~get_daylight_hours(.x, .y))),
  
  tar_target(seeds, purrr::map(gps_withdaylight, ~get_seeds(.x, ddf, dds, gps_spd, time_col = "time_since_carcass", stb_mins = stb_mins))),# still using time_since_carcass since it goes back farther than daytime_since_carcass.
  
  tar_target(all_indivs_sorted, purrr::map(gps_withdaylight, ~sort(unique(as.character(.x$individual_local_identifier))))),
  
  tar_target(gps_diffusion, purrr::map(gps_withdaylight, ~dplyr::filter(.x, time_since_carcass >= 0))),
  tar_target(first_sightings, purrr::map2(gps_diffusion, seeds, ~get_first_sightings(.x, hours_after_carcass, gps_spd, ddf, dds, .y))),
  tar_target(event_data, purrr::pmap(list(first_sightings, seeds, all_indivs_sorted, stn_carcs),
                                     ~format_event_data(first_sightings = ..1, seeds = ..2, all_indivs_sorted = ..3, time_col = "daytime_since_carcass", carc = ..4)
  )),
  tar_target(gps_fornetwork, purrr::map2(gps_diffusion, stn_carcs, ~filter(mutate(filter(.x, timestamp_il %in% .y$date:(.y$date+lubridate::hours(hours_after_carcass))), time = as.numeric(daytime_since_carcass)*60*60), time >= 0))),
  
  tar_target(cutpoints, purrr::map(event_data, ~unique(.x$time))),
  tar_target(cutpoints2, purrr::map(cutpoints, ~{if(!(0 %in% .x)){return(c(0, .x))}else{return(.x)}})),
  
  tar_target(gps_fornetwork2, purrr::map2(gps_fornetwork, cutpoints2, ~{
    if(length(.y) == 1 & is.na(.y[1])){return(NULL)}else{
      out <- dplyr::filter(dplyr::mutate(.x, network = cut(time, breaks = .y)), !is.na(network))
      return(out)}
  })),
  tar_target(missing_intervals, purrr::map(gps_fornetwork2, ~{levels(.x$network)[!(levels(.x$network) %in% .x$network)]})),
  tar_target(to_add, purrr::map(missing_intervals, ~data.frame(network = .x))),
  tar_target(gps_fornetwork3, purrr::map2(gps_fornetwork2, to_add, ~{
    if(!is.null(.x) & nrow(.y) > 0 & nrow(.x) > 0){
      return(dplyr::bind_rows(.x, .y))
    }else if(!is.null(.x) & nrow(.y) == 0){
      return(.x)
    }else{NULL}})),
  tar_target(gps_list, purrr::map(gps_fornetwork3, ~{
    if(!is.null(.x)){
      dplyr::group_split(dplyr::arrange(.x, network), network, .keep = T)}else{NULL}
  })),
  tar_target(gps_list_fixed, purrr::map(gps_list, ~purrr::map(.x, ~{
    if(nrow(.x) == 1 & all(is.na(.x$individual_local_identifier))){
      return(.x[0,])}else if(is.null(.x)){
        return(NULL)}else{
          return(.x)}
  }))),
  tar_target(dn1, purrr::map(gps_list_fixed[1:10], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn2, purrr::map(gps_list_fixed[11:20], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn3, purrr::map(gps_list_fixed[21:30], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn4, purrr::map(gps_list_fixed[31:40], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn5, purrr::map(gps_list_fixed[41:50], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn6, purrr::map(gps_list_fixed[51:60], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dynamic_networks, c(dn1, dn2, dn3, dn4, dn5, dn6)),
  
  tar_target(roost_threshold, 500),
  tar_target(nr1, purrr::map2(roosts_stn[1:10], all_indivs_sorted[1:10], ~arrange_roost_nets(.x, .y, roost_threshold))),
  tar_target(nr2, purrr::map2(roosts_stn[11:20], all_indivs_sorted[11:20], ~arrange_roost_nets(.x, .y, roost_threshold))),
  tar_target(nr3, purrr::map2(roosts_stn[21:30], all_indivs_sorted[21:30], ~arrange_roost_nets(.x, .y, roost_threshold))),
  tar_target(nr4, purrr::map2(roosts_stn[31:40], all_indivs_sorted[31:40], ~arrange_roost_nets(.x, .y, roost_threshold))),
  tar_target(nr5, purrr::map2(roosts_stn[41:50], all_indivs_sorted[41:50], ~arrange_roost_nets(.x, .y, roost_threshold))),
  tar_target(nr6, purrr::map2(roosts_stn[51:60], all_indivs_sorted[51:60], ~arrange_roost_nets(.x, .y, roost_threshold))),
  tar_target(networks_long_roost_1, c(nr1, nr2, nr3, nr4, nr5, nr6)),
  
  tar_target(networks_long_roost_2, purrr::map2(networks_long_roost_1, map_dbl(stn_carcs, "carcID"), ~{mutate(.x, "trial" = .y)})),
  
  tar_target(equivalence_tables, purrr::map(first_sightings, ~{
    if(nrow(.x) > 0){
      .x %>% st_drop_geometry() %>% mutate(time_secs = daytime_since_carcass*60*60) %>%
        mutate(time = 1:n(), date = lubridate::date(timestamp_il)) %>% select(date, time) %>% mutate(roost_date = date-lubridate::days(1)) 
    }else{NULL}
  })),
  
  tar_target(equivalence_tables_fixed, purrr::pmap(list("ed" = event_data, "gd" = gps_diffusion, "et" = equivalence_tables), function(ed, gd, et){
    if(!is.null(et)){
      t_last <- min(ed$time[ed$time > ed$t_end])/60/60
      idx <- which.min(abs(difftime(t_last, gd$daytime_since_carcass)))
      final_datetime <- gd$timestamp_il[idx]
      final_date <- lubridate::date(final_datetime)
      final_roost_date <- final_date - lubridate::days(1)
      et_fixed <- et %>% add_row(date = final_date, time = max(et$time)+1, roost_date = final_roost_date)
      return(et_fixed)
    }else{NULL}
  })),
  
  tar_target(equivalence_tables_wild, purrr::map(first_sightings_wild, ~{
    if(nrow(.x) > 0){
      .x %>% st_drop_geometry() %>% mutate(time_secs = daytime_since_carcass*60*60) %>%
        mutate(time = 1:n(), date = lubridate::date(timestamp_il)) %>% select(date, time) %>% mutate(roost_date = date-lubridate::days(1)) 
    }else{NULL}
  })),
  
  tar_target(equivalence_tables_fixed_wild, purrr::pmap(list("ed" = event_data_wild, "gd" = gps_diffusion_wild, "et" = equivalence_tables_wild), function(ed, gd, et){
    if(!is.null(et)){
      t_last <- min(ed$time[ed$time > ed$t_end])/60/60
      idx <- which.min(abs(difftime(t_last, gd$daytime_since_carcass)))
      final_datetime <- gd$timestamp_il[idx]
      final_date <- lubridate::date(final_datetime)
      final_roost_date <- final_date - lubridate::days(1)
      et_fixed <- et %>% add_row(date = final_date, time = max(et$time)+1, roost_date = final_roost_date)
      return(et_fixed)
    }else{NULL}
  })),
  
  
  # Break roost nets into a list by date and set names accordingly.
  tar_target(net_lists_1, purrr::map(networks_long_roost_2, ~group_split(group_by(.x, date)))),
  tar_target(net_lists_1_wild, purrr::map(networks_long_roost_2_wild, ~group_split(group_by(.x, date)))),
  tar_target(net_lists, purrr::map(net_lists_1, ~{
    names(.x) <- map_chr(.x, ~as.character(.x$date[1]))
    return(.x)
  })),
  tar_target(net_lists_wild, purrr::map(net_lists_1_wild, ~{
    names(.x) <- map_chr(.x, ~as.character(.x$date[1]))
    return(.x)
    })),
  tar_target(roostlong, purrr::map2(equivalence_tables_fixed, net_lists, ~{
    if(!is.null(.x) & !is.null(.y)){
      newlist <- vector(mode = "list", length = nrow(.x))
      for(i in 1:nrow(.x)){
        date <- as.character(.x$roost_date[i])
        newlist[[i]] <- .y[[date]]
      }
      out <- purrr::list_rbind(newlist, names_to = "time")
      return(out)
    }else{NULL}
  })),
  tar_target(roostlong_wild, purrr::map2(equivalence_tables_fixed_wild, net_lists_wild, ~{
    if(!is.null(.x) & !is.null(.y)){
      newlist <- vector(mode = "list", length = nrow(.x))
      for(i in 1:nrow(.x)){
        date <- as.character(.x$roost_date[i])
        newlist[[i]] <- .y[[date]]
      }
      out <- purrr::list_rbind(newlist, names_to = "time")
      return(out)
    }else{NULL}
  })),
  
  tar_target(nr1_wild, purrr::map2(roosts_wild[1:18], all_indivs_sorted_wild[1:18], ~arrange_roost_nets(.x, .y, roost_threshold))),
  tar_target(nr2_wild, purrr::map2(roosts_wild[19:37], all_indivs_sorted_wild[19:37], ~arrange_roost_nets(.x, .y, roost_threshold))),
  tar_target(nr3_wild, purrr::map2(roosts_wild[38:56], all_indivs_sorted_wild[38:56], ~arrange_roost_nets(.x, .y, roost_threshold))),
  tar_target(nr4_wild, purrr::map2(roosts_wild[57:75], all_indivs_sorted_wild[57:75], ~arrange_roost_nets(.x, .y, roost_threshold))),
  tar_target(nr5_wild, purrr::map2(roosts_wild[76:94], all_indivs_sorted_wild[76:94], ~arrange_roost_nets(.x, .y, roost_threshold))),
  tar_target(nr6_wild, purrr::map2(roosts_wild[95:112], all_indivs_sorted_wild[95:112], ~arrange_roost_nets(.x, .y, roost_threshold))),
  tar_target(networks_long_roost_1_wild, c(nr1_wild, nr2_wild, nr3_wild, nr4_wild, nr5_wild, nr6_wild)),
  
  tar_target(networks_long_roost_2_wild, purrr::map2(networks_long_roost_1_wild, map_dbl(wild_carcs, "carcID"), ~{mutate(.x, "trial" = .y)})),
  
  tar_target(dynamic_networks_fixed, purrr::map2(dynamic_networks, all_indivs_sorted, ~fix_nets(.x, indivs = .y))),
  
  tar_target(networks_long_dynamic, purrr::map2(dynamic_networks_fixed, stn_carcs, ~mutate(purrr::list_rbind(purrr::map(.x, ~{
    out <- rownames_to_column(.x, var = "focal") %>% pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri")
  }), names_to = "time"), trial = .y$carcID[1]))),
  
  tar_target(networks_long_combined, purrr::map2(networks_long_dynamic, roostlong, ~{
    if(!is.null(.y)){
      left_join(.x, dplyr::select(.y, -date), by = c("time", "focal", "other", "trial")) %>%
        mutate(flight_sri_scaled = flight_sri/max(flight_sri)) %>% # scaling to a 0-1 scale
        select(-flight_sri)
    }else{.x}
  })),
  tar_target(networks_long_combined_wild, purrr::map2(networks_long_dynamic_wild, roostlong_wild, ~{
    if(!is.null(.y)){
      left_join(.x, dplyr::select(.y, -date), by = c("time", "focal", "other", "trial")) %>%
        mutate(flight_sri_scaled = flight_sri/max(flight_sri)) %>%
        select(-flight_sri)
    }else{.x}
  })),
  
  tar_target(data_lists_noILVs, purrr::pmap(list("ev" = event_data, "nld" = networks_long_dynamic), function(ev, nld){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev, 
                               networks = nld, 
                               network_type = "undirected")}else{NULL}})),
  
  tar_target(data_lists_noILVs_2nets, purrr::pmap(list("ev" = event_data, "nld" = networks_long_combined), function(ev, nld){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev, 
                               networks = nld, 
                               network_type = "undirected")}else{NULL}})),
  
  tar_target(data_lists_DistI, purrr::pmap(list("ev" = event_data, "nld" = networks_long_dynamic, "ilvc" = ILV_c, "ilvtv" = ILV_tv), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev, 
                               networks = nld, 
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("mean_dist_to_carcass_norm"))}else{NULL}})),
  
  tar_target(data_lists_DistI_2nets, purrr::pmap(list("ev" = event_data, "nld" = networks_long_combined, "ilvc" = ILV_c, "ilvtv" = ILV_tv), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev, 
                               networks = nld, 
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("mean_dist_to_carcass_norm"))}else{NULL}})),
  
  tar_target(data_lists_DistI_AgeIS, purrr::pmap(list("ev" = event_data, "nld" = networks_long_dynamic, "ilvc" = ILV_c, "ilvtv" = ILV_tv), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev, 
                               networks = nld, 
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("age", "mean_dist_to_carcass_norm"),
                               ILVs = c("age"))}else{NULL}})),
  
  tar_target(data_lists_DistI_AgeIS_2nets, purrr::pmap(list("ev" = event_data, "nld" = networks_long_combined, "ilvc" = ILV_c, "ilvtv" = ILV_tv), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev, 
                               networks = nld, 
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("age", "mean_dist_to_carcass_norm"),
                               ILVs = c("age"))}else{NULL}})),
  
  tar_target(data_lists_DistIS, purrr::pmap(list("ev" = event_data, "nld" = networks_long_dynamic, "ilvc" = ILV_c, "ilvtv" = ILV_tv), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev, 
                               networks = nld, 
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("mean_dist_to_carcass_norm"),
                               ILVs = c("mean_dist_to_carcass_norm"))}else{NULL}})),
  
  tar_target(data_lists_DistIS_2nets, purrr::pmap(list("ev" = event_data, "nld" = networks_long_combined, "ilvc" = ILV_c, "ilvtv" = ILV_tv), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev, 
                               networks = nld, 
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("mean_dist_to_carcass_norm"),
                               ILVs = c("mean_dist_to_carcass_norm"))}else{NULL}})),
  
  tar_target(data_lists_DistIS_AgeIS, purrr::pmap(list("ev" = event_data, "nld" = networks_long_dynamic, "ilvc" = ILV_c, "ilvtv" = ILV_tv), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev, 
                               networks = nld, 
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("mean_dist_to_carcass_norm", "age"),
                               ILVs = c("mean_dist_to_carcass_norm", "age"))}else{NULL}})),
  
  tar_target(data_lists_DistIS_AgeIS_2nets, purrr::pmap(list("ev" = event_data, "nld" = networks_long_combined, "ilvc" = ILV_c, "ilvtv" = ILV_tv), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev, 
                               networks = nld, 
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("mean_dist_to_carcass_norm", "age"),
                               ILVs = c("mean_dist_to_carcass_norm", "age"))}else{NULL}})),
  
  # ILVs
  tar_target(age_ilv, purrr::pmap(list("gd" = gps_diffusion, "sc" = stn_carcs, "ais" = all_indivs_sorted), function(gd, sc, ais){
    yr <- sc$year
    col_to_select <- paste0("age_", yr)
    out <- gd %>% st_drop_geometry() %>%
      dplyr::select(individual_local_identifier, all_of(col_to_select)) %>%
      dplyr::distinct() %>%
      dplyr::rename("age_continuous" = col_to_select) %>%
      dplyr::mutate(age_continuous = case_when(is.na(age_continuous) ~ mean(age_continuous, na.rm = T),
                                               .default = age_continuous)) %>%
      dplyr::mutate(age_categorical = case_when(age_continuous == 0 ~ "juv",
                                                age_continuous > 0 & age_continuous < 5 ~ "sub",
                                                age_continuous >= 5 ~ "adult",
                                                .default = NA))
    missing <- ais[!(ais %in% out$individual_local_identifier)]
    toadd <- data.frame(individual_local_identifier = missing, age_continuous = 6, age_categorical = "adult")
    out <- bind_rows(out, toadd) %>% mutate(age_categorical = factor(age_categorical, levels = c("juv", "sub", "adult")))
    return(out)
  })),
  
  tar_target(dists_dyn, purrr::map2(gps_list_fixed, all_indivs_sorted, ~{ # Added ~ here
    ais <- .y
    if(length(.x) != 0){
      # Inner map
      res <- purrr::map(.x, ~{
        step1 <- .x %>%
          sf::st_drop_geometry() %>%
          dplyr::arrange(individual_local_identifier, time_since_carcass) %>%
          dplyr::group_by(individual_local_identifier) %>%
          dplyr::summarize(mean_dist_to_carcass = mean(dist_to_carcass, na.rm = TRUE))
        
        missing <- ais[!(ais %in% step1$individual_local_identifier)]
        
        # Use NA_real_ to match the numeric type of mean_dist_to_carcass
        missing_df <- data.frame(
          individual_local_identifier = missing, 
          mean_dist_to_carcass = NA_real_ 
        )
        
        step2 <- dplyr::bind_rows(step1, missing_df) %>%
          dplyr::filter(!is.na(individual_local_identifier))
        
        return(step2)
      }) %>% 
        purrr::list_rbind(names_to = "time") %>%
        dplyr::mutate(
          # Use as.vector to strip matrix attributes from scale()
          mean_dist_to_carcass_norm = as.vector(scale(log(mean_dist_to_carcass), center = TRUE, scale = TRUE)),
          mean_dist_to_carcass_norm = tidyr::replace_na(mean_dist_to_carcass_norm, 0) # Added tidyr::
        )
      return(res) # Ensure the result of the mutate chain is returned
      
    }else{
      return(NULL)
    }
  })
  ),
  
  tar_target(ILV_c, purrr::map(age_ilv, ~{
    .x %>% dplyr::rename("id" = individual_local_identifier,
                  "age" = age_categorical) %>%
      dplyr::select(id, age)
  })),
  
  tar_target(ILV_tv, purrr::map2(dists_dyn, purrr::map_dbl(stn_carcs, "carcID"), ~{
    if(!is.null(.x)){
      .x %>% dplyr::select("id" = individual_local_identifier,
                           time, mean_dist_to_carcass_norm) %>%
        dplyr::mutate(trial = .y) %>%
        dplyr::select(trial, id, time, mean_dist_to_carcass_norm) %>%
        mutate(mean_dist_to_carcass_norm = replace_na(mean_dist_to_carcass_norm, 0))
    }else{return(NULL)}
  })),
  
  ## wild
  tar_target(gps_withdaylight_wild_1, purrr::map2(wild_gps_30days[1:14], wild_carcs[1:14], ~get_daylight_hours(.x, .y), .progress = T)),
  tar_target(gps_withdaylight_wild_2, purrr::map2(wild_gps_30days[15:28], wild_carcs[15:28], ~get_daylight_hours(.x, .y), .progress = T)),
  tar_target(gps_withdaylight_wild_3, purrr::map2(wild_gps_30days[29:43], wild_carcs[29:43], ~get_daylight_hours(.x, .y), .progress = T)),
  tar_target(gps_withdaylight_wild_4, purrr::map2(wild_gps_30days[44:58], wild_carcs[44:58], ~get_daylight_hours(.x, .y), .progress = T)),
  tar_target(gps_withdaylight_wild_5, purrr::map2(wild_gps_30days[59:73], wild_carcs[59:73], ~get_daylight_hours(.x, .y), .progress = T)),
  tar_target(gps_withdaylight_wild_6, purrr::map2(wild_gps_30days[74:88], wild_carcs[74:88], ~get_daylight_hours(.x, .y), .progress = T)),
  tar_target(gps_withdaylight_wild_7, purrr::map2(wild_gps_30days[89:103], wild_carcs[89:103], ~get_daylight_hours(.x, .y), .progress = T)),
  tar_target(gps_withdaylight_wild_8, purrr::map2(wild_gps_30days[104:112], wild_carcs[104:112], ~get_daylight_hours(.x, .y), .progress = T)),
  tar_target(seeds_wild_1, purrr::map(gps_withdaylight_wild_1, ~get_seeds(.x, ddf, dds, gps_spd, time_col = "time_since_carcass", stb_mins = stb_mins))),
  tar_target(seeds_wild_2, purrr::map(gps_withdaylight_wild_2, ~get_seeds(.x, ddf, dds, gps_spd, time_col = "time_since_carcass", stb_mins = stb_mins))),
  tar_target(seeds_wild_3, purrr::map(gps_withdaylight_wild_3, ~get_seeds(.x, ddf, dds, gps_spd, time_col = "time_since_carcass", stb_mins = stb_mins))),
  tar_target(seeds_wild_4, purrr::map(gps_withdaylight_wild_4, ~get_seeds(.x, ddf, dds, gps_spd, time_col = "time_since_carcass", stb_mins = stb_mins))),
  tar_target(seeds_wild_5, purrr::map(gps_withdaylight_wild_5, ~get_seeds(.x, ddf, dds, gps_spd, time_col = "time_since_carcass", stb_mins = stb_mins))),
  tar_target(seeds_wild_6, purrr::map(gps_withdaylight_wild_6, ~get_seeds(.x, ddf, dds, gps_spd, time_col = "time_since_carcass", stb_mins = stb_mins))),
  tar_target(seeds_wild_7, purrr::map(gps_withdaylight_wild_7, ~get_seeds(.x, ddf, dds, gps_spd, time_col = "time_since_carcass", stb_mins = stb_mins))),
  tar_target(seeds_wild_8, purrr::map(gps_withdaylight_wild_8, ~get_seeds(.x, ddf, dds, gps_spd, time_col = "time_since_carcass", stb_mins = stb_mins))),
  tar_target(seeds_wild, c(seeds_wild_1, seeds_wild_2, seeds_wild_3, seeds_wild_4, seeds_wild_5, seeds_wild_6, seeds_wild_7, seeds_wild_8)),
  tar_target(all_indivs_sorted_wild_1, purrr::map(gps_withdaylight_wild_1, ~sort(unique(as.character(.x$individual_local_identifier))))),
  tar_target(all_indivs_sorted_wild_2, purrr::map(gps_withdaylight_wild_2,~sort(unique(as.character(.x$individual_local_identifier))))),
  tar_target(all_indivs_sorted_wild_3, purrr::map(gps_withdaylight_wild_3, ~sort(unique(as.character(.x$individual_local_identifier))))),
  tar_target(all_indivs_sorted_wild_4, purrr::map(gps_withdaylight_wild_4, ~sort(unique(as.character(.x$individual_local_identifier))))),
  tar_target(all_indivs_sorted_wild_5, purrr::map(gps_withdaylight_wild_5, ~sort(unique(as.character(.x$individual_local_identifier))))),
  tar_target(all_indivs_sorted_wild_6, purrr::map(gps_withdaylight_wild_6, ~sort(unique(as.character(.x$individual_local_identifier))))),
  tar_target(all_indivs_sorted_wild_7, purrr::map(gps_withdaylight_wild_7, ~sort(unique(as.character(.x$individual_local_identifier))))),
  tar_target(all_indivs_sorted_wild_8, purrr::map(gps_withdaylight_wild_8, ~sort(unique(as.character(.x$individual_local_identifier))))),
  tar_target(all_indivs_sorted_wild, c(all_indivs_sorted_wild_1, all_indivs_sorted_wild_2, all_indivs_sorted_wild_3, all_indivs_sorted_wild_4, all_indivs_sorted_wild_5, all_indivs_sorted_wild_6, all_indivs_sorted_wild_7, all_indivs_sorted_wild_8)),
  tar_target(gps_diffusion_wild_1, purrr::map(gps_withdaylight_wild_1, ~dplyr::filter(.x, time_since_carcass >= 0))),
  tar_target(gps_diffusion_wild_2, purrr::map(gps_withdaylight_wild_2, ~dplyr::filter(.x, time_since_carcass >= 0))),
  tar_target(gps_diffusion_wild_3, purrr::map(gps_withdaylight_wild_3, ~dplyr::filter(.x, time_since_carcass >= 0))),
  tar_target(gps_diffusion_wild_4, purrr::map(gps_withdaylight_wild_4, ~dplyr::filter(.x, time_since_carcass >= 0))),
  tar_target(gps_diffusion_wild_5, purrr::map(gps_withdaylight_wild_5, ~dplyr::filter(.x, time_since_carcass >= 0))),
  tar_target(gps_diffusion_wild_6, purrr::map(gps_withdaylight_wild_6, ~dplyr::filter(.x, time_since_carcass >= 0))),
  tar_target(gps_diffusion_wild_7, purrr::map(gps_withdaylight_wild_7, ~dplyr::filter(.x, time_since_carcass >= 0))),
  tar_target(gps_diffusion_wild_8, purrr::map(gps_withdaylight_wild_8, ~dplyr::filter(.x, time_since_carcass >= 0))),
  tar_target(gps_diffusion_wild, c(gps_diffusion_wild_1, gps_diffusion_wild_2, gps_diffusion_wild_3, gps_diffusion_wild_4, gps_diffusion_wild_5, gps_diffusion_wild_6, gps_diffusion_wild_7, gps_diffusion_wild_8)),
  tar_target(first_sightings_wild_1, purrr::map2(gps_diffusion_wild_1, seeds_wild_1, ~get_first_sightings(.x, hours_after_carcass, gps_spd, ddf, dds, .y))),
  tar_target(first_sightings_wild_2, purrr::map2(gps_diffusion_wild_2, seeds_wild_2, ~get_first_sightings(.x, hours_after_carcass, gps_spd, ddf, dds, .y))),
  tar_target(first_sightings_wild_3, purrr::map2(gps_diffusion_wild_3, seeds_wild_3, ~get_first_sightings(.x, hours_after_carcass, gps_spd, ddf, dds, .y))),
  tar_target(first_sightings_wild_4, purrr::map2(gps_diffusion_wild_4, seeds_wild_4, ~get_first_sightings(.x, hours_after_carcass, gps_spd, ddf, dds, .y))),
  tar_target(first_sightings_wild_5, purrr::map2(gps_diffusion_wild_5, seeds_wild_5, ~get_first_sightings(.x, hours_after_carcass, gps_spd, ddf, dds, .y))),
  tar_target(first_sightings_wild_6, purrr::map2(gps_diffusion_wild_6, seeds_wild_6, ~get_first_sightings(.x, hours_after_carcass, gps_spd, ddf, dds, .y))),
  tar_target(first_sightings_wild_7, purrr::map2(gps_diffusion_wild_7, seeds_wild_7, ~get_first_sightings(.x, hours_after_carcass, gps_spd, ddf, dds, .y))),
  tar_target(first_sightings_wild_8, purrr::map2(gps_diffusion_wild_8, seeds_wild_8, ~get_first_sightings(.x, hours_after_carcass, gps_spd, ddf, dds, .y))),
  tar_target(first_sightings_wild, c(first_sightings_wild_1, first_sightings_wild_2, first_sightings_wild_3, first_sightings_wild_4, first_sightings_wild_5, first_sightings_wild_6, first_sightings_wild_7, first_sightings_wild_8)),
  tar_target(event_data_wild, purrr::pmap(list(first_sightings_wild, seeds_wild, all_indivs_sorted_wild, wild_carcs), ~format_event_data(first_sightings = ..1, seeds = ..2, all_indivs_sorted = ..3, time_col = "daytime_since_carcass", carc = ..4)
  )),
  tar_target(gps_fornetwork_wild, purrr::map2(gps_diffusion_wild, wild_carcs, ~filter(mutate(filter(.x, timestamp_il %in% .y$date:(.y$date+lubridate::hours(hours_after_carcass))), time = as.numeric(daytime_since_carcass)*60*60), time >= 0))),

  tar_target(cutpoints_wild, purrr::map(event_data_wild, ~unique(.x$time))),
  tar_target(cutpoints2_wild, purrr::map(cutpoints_wild, ~{if(!(0 %in% .x)){return(c(0, .x))}else{return(.x)}})),
  tar_target(bins_wild, purrr::map2(gps_fornetwork_wild, cutpoints2_wild, ~{sort(unique(cut(.x$time, breaks = .y)))})),

  tar_target(gps_fornetwork2_wild, purrr::map2(gps_fornetwork_wild, cutpoints2_wild, ~{
    if(length(.y) == 1 & is.na(.y[1])){return(NULL)}else{
      out <- dplyr::filter(dplyr::mutate(.x, network = cut(time, breaks = .y)), !is.na(network))
      return(out)}
  })),
  tar_target(missing_intervals_wild, purrr::map(gps_fornetwork2_wild, ~{levels(.x$network)[!(levels(.x$network) %in% .x$network)]})),
  tar_target(to_add_wild, purrr::map(missing_intervals_wild, ~data.frame(network = .x))),
  tar_target(gps_fornetwork3_wild, purrr::map2(gps_fornetwork2_wild, to_add_wild, ~{
    if(!is.null(.x) & nrow(.y) > 0 & nrow(.x) > 0){
      return(dplyr::bind_rows(.x, .y))
    }else if(!is.null(.x) & nrow(.y) == 0){
      return(.x)
    }else{NULL}})),
  tar_target(gps_list_wild, purrr::map(gps_fornetwork3_wild, ~{
    if(!is.null(.x)){
      dplyr::group_split(dplyr::arrange(.x, network), network, .keep = T)}else{NULL}
  })),
  tar_target(gps_list_fixed_wild, purrr::map(gps_list_wild, ~purrr::map(.x, ~{
    if(nrow(.x) == 1 & all(is.na(.x$individual_local_identifier))){
      return(.x[0,])}else if(is.null(.x)){
        return(NULL)}else{
          return(.x)}
  }))),
  tar_target(dn1_wild, purrr::map(gps_list_fixed_wild[1:10], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn2_wild, purrr::map(gps_list_fixed_wild[11:20], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn3_wild, purrr::map(gps_list_fixed_wild[21:30], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn4_wild, purrr::map(gps_list_fixed_wild[31:40], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn5_wild, purrr::map(gps_list_fixed_wild[41:50], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn6_wild, purrr::map(gps_list_fixed_wild[51:60], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn7_wild, purrr::map(gps_list_fixed_wild[61:70], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn8_wild, purrr::map(gps_list_fixed_wild[71:80], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn9_wild, purrr::map(gps_list_fixed_wild[81:90], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn10_wild, purrr::map(gps_list_fixed_wild[91:100], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn11_wild, purrr::map(gps_list_fixed_wild[101:110], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dn12_wild, purrr::map(gps_list_fixed_wild[111:112], ~purrr::map(.x, ~{
    get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd)}))),
  tar_target(dynamic_networks_wild, c(dn1_wild, dn2_wild, dn3_wild, dn4_wild, dn5_wild, dn6_wild, dn7_wild, dn8_wild, dn9_wild, dn10_wild, dn11_wild, dn12_wild)),

  tar_target(dynamic_networks_fixed_wild, purrr::map2(dynamic_networks_wild, all_indivs_sorted_wild, ~fix_nets(.x, indivs = .y))),

  tar_target(networks_long_dynamic_wild, purrr::map2(dynamic_networks_fixed_wild, wild_carcs, ~mutate(purrr::list_rbind(purrr::map(.x, ~{
    out <- rownames_to_column(.x, var = "focal") %>% pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri")
  }), names_to = "time"), trial = .y$carcID[1]))),

  tar_target(data_lists_noILVs_wild, purrr::pmap(list("ev" = event_data_wild, "nld" = networks_long_dynamic_wild), function(ev, nld){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev,
                               networks = nld,
                               network_type = "undirected")}else{NULL}})),
  
  tar_target(data_lists_noILVs_2nets_wild, purrr::pmap(list("ev" = event_data_wild, "nld" = networks_long_combined_wild), function(ev, nld){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev,
                               networks = nld,
                               network_type = "undirected")}else{NULL}})),

  tar_target(data_lists_DistI_wild, purrr::pmap(list("ev" = event_data_wild, "nld" = networks_long_dynamic_wild, "ilvc" = ILV_c_wild, "ilvtv" = ILV_tv_wild), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev,
                               networks = nld,
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("mean_dist_to_carcass_norm"))}else{NULL}})),
  
  tar_target(data_lists_DistI_2nets_wild, purrr::pmap(list("ev" = event_data_wild, "nld" = networks_long_combined_wild, "ilvc" = ILV_c_wild, "ilvtv" = ILV_tv_wild), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev,
                               networks = nld,
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("mean_dist_to_carcass_norm"))}else{NULL}})),

  tar_target(data_lists_DistI_AgeIS_wild, purrr::pmap(list("ev" = event_data_wild, "nld" = networks_long_dynamic_wild, "ilvc" = ILV_c_wild, "ilvtv" = ILV_tv_wild), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev,
                               networks = nld,
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("age", "mean_dist_to_carcass_norm"),
                               ILVs = c("age"))}else{NULL}})),
  
  tar_target(data_lists_DistI_AgeIS_2nets_wild, purrr::pmap(list("ev" = event_data_wild, "nld" = networks_long_combined_wild, "ilvc" = ILV_c_wild, "ilvtv" = ILV_tv_wild), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev,
                               networks = nld,
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("age", "mean_dist_to_carcass_norm"),
                               ILVs = c("age"))}else{NULL}})),

  tar_target(data_lists_DistIS_wild, purrr::pmap(list("ev" = event_data_wild, "nld" = networks_long_dynamic_wild, "ilvc" = ILV_c_wild, "ilvtv" = ILV_tv_wild), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev,
                               networks = nld,
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("mean_dist_to_carcass_norm"),
                               ILVs = c("mean_dist_to_carcass_norm"))}else{NULL}})),
  
  tar_target(data_lists_DistIS_2nets_wild, purrr::pmap(list("ev" = event_data_wild, "nld" = networks_long_combined_wild, "ilvc" = ILV_c_wild, "ilvtv" = ILV_tv_wild), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev,
                               networks = nld,
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("mean_dist_to_carcass_norm"),
                               ILVs = c("mean_dist_to_carcass_norm"))}else{NULL}})),

  tar_target(data_lists_DistIS_AgeIS_wild, purrr::pmap(list("ev" = event_data_wild, "nld" = networks_long_dynamic_wild, "ilvc" = ILV_c_wild, "ilvtv" = ILV_tv_wild), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev,
                               networks = nld,
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("mean_dist_to_carcass_norm", "age"),
                               ILVs = c("mean_dist_to_carcass_norm", "age"))}else{NULL}})),
  
  tar_target(data_lists_DistIS_AgeIS_2nets_wild, purrr::pmap(list("ev" = event_data_wild, "nld" = networks_long_combined_wild, "ilvc" = ILV_c_wild, "ilvtv" = ILV_tv_wild), function(ev, nld, ilvc, ilvtv){
    if(nrow(nld) > 0){
      STbayes::import_user_STb(event_data = ev,
                               networks = nld,
                               network_type = "undirected",
                               ILV_c = ilvc,
                               ILV_tv = ilvtv,
                               ILVi = c("mean_dist_to_carcass_norm", "age"),
                               ILVs = c("mean_dist_to_carcass_norm", "age"))}else{NULL}})),

  # ILVs
  tar_target(age_ilv_wild, purrr::pmap(list("gd" = gps_diffusion_wild, "sc" = wild_carcs, "ais" = all_indivs_sorted_wild), function(gd, sc, ais){
    yr <- sc$year
    col_to_select <- paste0("age_", yr)
    out <- gd %>% st_drop_geometry() %>%
      dplyr::select(individual_local_identifier, all_of(col_to_select)) %>%
      dplyr::distinct() %>%
      dplyr::rename("age_continuous" = col_to_select) %>%
      dplyr::mutate(age_continuous = case_when(is.na(age_continuous) ~ mean(age_continuous, na.rm = T),
                                               .default = age_continuous)) %>%
      dplyr::mutate(age_categorical = case_when(age_continuous == 0 ~ "juv",
                                                age_continuous > 0 & age_continuous < 5 ~ "sub",
                                                age_continuous >= 5 ~ "adult",
                                                .default = NA))
    missing <- ais[!(ais %in% out$individual_local_identifier)]
    toadd <- data.frame(individual_local_identifier = missing, age_continuous = 6, age_categorical = "adult")
    out <- bind_rows(out, toadd) %>% mutate(age_categorical = factor(age_categorical, levels = c("juv", "sub", "adult")))
    return(out)
  })),

  tar_target(dists_dyn_wild, purrr::map2(gps_list_fixed_wild, all_indivs_sorted_wild, ~{ # Added ~ here
    ais <- .y
    if(length(.x) != 0){
      # Inner map
      res <- purrr::map(.x, ~{
        step1 <- .x %>%
          sf::st_drop_geometry() %>%
          dplyr::arrange(individual_local_identifier, time_since_carcass) %>%
          dplyr::group_by(individual_local_identifier) %>%
          dplyr::summarize(mean_dist_to_carcass = mean(dist_to_carcass, na.rm = TRUE))

        missing <- ais[!(ais %in% step1$individual_local_identifier)]

        # Use NA_real_ to match the numeric type of mean_dist_to_carcass
        missing_df <- data.frame(
          individual_local_identifier = missing,
          mean_dist_to_carcass = NA_real_
        )

        step2 <- dplyr::bind_rows(step1, missing_df) %>%
          dplyr::filter(!is.na(individual_local_identifier))

        return(step2)
      }) %>%
        purrr::list_rbind(names_to = "time") %>%
        dplyr::mutate(
          # Use as.vector to strip matrix attributes from scale()
          mean_dist_to_carcass_norm = as.vector(scale(log(mean_dist_to_carcass), center = TRUE, scale = TRUE)),
          mean_dist_to_carcass_norm = tidyr::replace_na(mean_dist_to_carcass_norm, 0) # Added tidyr::
        )
      return(res) # Ensure the result of the mutate chain is returned

    }else{
      return(NULL)
    }
  })
  ),

  tar_target(ILV_c_wild, purrr::map(age_ilv_wild, ~{
    .x %>% dplyr::rename("id" = individual_local_identifier,
                         "age" = age_categorical) %>%
      dplyr::select(id, age)
  })),

  tar_target(ILV_tv_wild, purrr::map2(dists_dyn_wild, purrr::map_dbl(wild_carcs, "carcID"), ~{
    if(!is.null(.x)){
      .x %>% dplyr::select("id" = individual_local_identifier,
                           time, mean_dist_to_carcass_norm) %>%
        dplyr::mutate(trial = .y) %>%
        dplyr::select(trial, id, time, mean_dist_to_carcass_norm) %>%
        mutate(mean_dist_to_carcass_norm = replace_na(mean_dist_to_carcass_norm, 0))
    }else{return(NULL)}
  }))
)
