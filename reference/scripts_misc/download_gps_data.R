# Script to download the GPS data with interactive auth
library(move2)
library(targets)
library(readr)
library(here)
tar_load(minmax_dates)
# don't forget to interactively authenticate!

ornitela_data_2022 <- move2::movebank_download_study(1252551761,
                                          sensor_type_id = "gps",
                                          timestamp_start = minmax_dates[[1]],
                                          timestamp_end = minmax_dates[[2]])
test <- move2::movebank_download_study(1252551761,
                                                     sensor_type_id = "gps",
                                                     timestamp_start = minmax_dates[[1]],
                                                     timestamp_end = minmax_dates[[4]])
ornitela_data_2023 <- move2::movebank_download_study(1252551761,
                                          sensor_type_id = "gps",
                                          timestamp_start = minmax_dates[[3]],
                                          timestamp_end = minmax_dates[[4]])
ornitela_data_2024 <- move2::movebank_download_study(1252551761,
                                          sensor_type_id = "gps",
                                          timestamp_start = minmax_dates[[5]],
                                          timestamp_end = minmax_dates[[6]])
inpa_data_2022 <- move2::movebank_download_study(6071688,
                                          sensor_type_id = "gps",
                                          timestamp_start = minmax_dates[[1]],
                                          timestamp_end = minmax_dates[[2]])
inpa_data_2023 <- move2::movebank_download_study(6071688,
                                          sensor_type_id = "gps",
                                          timestamp_start = minmax_dates[[3]],
                                          timestamp_end = minmax_dates[[4]])
inpa_data_2024 <- move2::movebank_download_study(6071688,
                                          sensor_type_id = "gps",
                                          timestamp_start = minmax_dates[[5]],
                                          timestamp_end = minmax_dates[[6]])

write_rds(ornitela_data_2022, file = here("data/ornitela_data_2022.RDS"))
write_rds(ornitela_data_2023, file = here("data/ornitela_data_2023.RDS"))
write_rds(ornitela_data_2024, file = here("data/ornitela_data_2024.RDS"))
write_rds(inpa_data_2022, file = here("data/inpa_data_2022.RDS"))
write_rds(inpa_data_2023, file = here("data/inpa_data_2023.RDS"))
write_rds(inpa_data_2024, file = here("data/inpa_data_2024.RDS"))
