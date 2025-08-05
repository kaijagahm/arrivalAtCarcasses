# Script to download the GPS data with interactive auth
library(move2)
library(targets)
library(readr)
library(here)
tar_load(minmax_buff)
# don't forget to interactively authenticate!

test_orn_22 <- move2::movebank_download_study(1252551761,
                                              sensor_type_id = "gps",
                                              timestamp_start = minmax_buff[[1]] + days(25),
                                              timestamp_end = minmax_buff[[2]] - days(25))
out <- left_join(test_orn_22, mt_track_data(test_orn_22))

test_inp_22 <- move2::movebank_download_study(6071688,
                                              sensor_type_id = "gps",
                                              timestamp_start = minmax_buff[[1]] + days(25),
                                              timestamp_end = minmax_buff[[2]] - days(25))

test_orn_24 <- move2::movebank_download_study(1252551761,
                                              sensor_type_id = "gps",
                                              timestamp_start = minmax_buff[[5]] + days(25),
                                              timestamp_end = minmax_buff[[6]] - days(25))

test_inp_24 <- move2::movebank_download_study(6071688,
                                              sensor_type_id = "gps",
                                              timestamp_start = minmax_buff[[5]] + days(25),
                                              timestamp_end = minmax_buff[[6]] - days(25))

dim(test_orn_22)
dim(test_inp_22)
dim(test_orn_24)
dim(test_inp_24)

mt_track_id_column(test_orn_22)
mt_track_id_column(test_inp_22)
mt_track_id_column(test_orn_24)
mt_track_id_column(test_inp_24)

ornitela_data_2022 <- move2::movebank_download_study(1252551761,
                                          sensor_type_id = "gps",
                                          timestamp_start = minmax_buff[[1]],
                                          timestamp_end = minmax_buff[[2]])
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
