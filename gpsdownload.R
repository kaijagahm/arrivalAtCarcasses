library(here)
library(tidyverse)
library(vultureUtils)
library(move)
library(targets)
targets::tar_load(loginObject)

tar_load(minmax_dates)

ornitela_data_2022 <- vultureUtils::downloadVultures(loginObject = loginObject, removeDup = T, dfConvert = T, quiet = T, dateTimeStartUTC = minmax_dates[[1]], dateTimeEndUTC = minmax_dates[[2]])
ornitela_data_2023 <- vultureUtils::downloadVultures(loginObject = loginObject, removeDup = T, dfConvert = T, quiet = T, dateTimeStartUTC = minmax_dates[[3]], dateTimeEndUTC = minmax_dates[[4]])
ornitela_data_2024 <- vultureUtils::downloadVultures(loginObject = loginObject, removeDup = T, dfConvert = T, quiet = T, dateTimeStartUTC = minmax_dates[[5]], dateTimeEndUTC = minmax_dates[[6]])

gps_2022 <- dplyr::select(ornitela_data_2022, local_identifier, tag_id, timestamp, dateOnly, ground_speed, location_lat, location_long, individual_id, tag_local_identifier)
gps_2023 <- dplyr::select(ornitela_data_2023, local_identifier, tag_id, timestamp, dateOnly, ground_speed, location_lat, location_long, individual_id, tag_local_identifier)
gps_2024 <- dplyr::select(ornitela_data_2024, local_identifier, tag_id, timestamp, dateOnly, ground_speed, location_lat, location_long, individual_id, tag_local_identifier)

rm(ornitela_data_2022)
rm(ornitela_data_2023)
rm(ornitela_data_2024)
gc()

data.table::fwrite(gps_2022, file = here("data/ACC/2022_hf_period/created/gps_2022.csv"))
data.table::fwrite(gps_2023, file = here("data/ACC/2023_hf_period/created/gps_2023.csv"))
data.table::fwrite(gps_2024, file = here("data/ACC/2024_hf_period/created/gps_2024.csv"))
gps_2022 <- data.table::fread("data/ACC/2023_hf_period/created/gps_2022.csv")
gps_2023 <- data.table::fread("data/ACC/2023_hf_period/created/gps_2023.csv")
gps_2024 <- data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv")