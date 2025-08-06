# Script to download the GPS data with interactive auth
library(move2)
library(targets)
library(readr)
library(here)
tar_load(minmax_buff)
# don't forget to interactively authenticate!

ornitela_data_2022 <- move2::movebank_download_study(1252551761,
                                          sensor_type_id = "gps",
                                          timestamp_start = minmax_buff[[1]],
                                          timestamp_end = minmax_buff[[2]])
ornitela_data_2023 <- move2::movebank_download_study(1252551761,
                                          sensor_type_id = "gps",
                                          timestamp_start = minmax_buff[[3]],
                                          timestamp_end = minmax_buff[[4]])
ornitela_data_2024 <- move2::movebank_download_study(1252551761,
                                          sensor_type_id = "gps",
                                          timestamp_start = minmax_buff[[5]],
                                          timestamp_end = minmax_buff[[6]])
inpa_data_2022 <- move2::movebank_download_study(6071688,
                                          sensor_type_id = "gps",
                                          timestamp_start = minmax_buff[[1]],
                                          timestamp_end = minmax_buff[[2]])
inpa_data_2023 <- move2::movebank_download_study(6071688,
                                          sensor_type_id = "gps",
                                          timestamp_start = minmax_buff[[3]],
                                          timestamp_end = minmax_buff[[4]])
inpa_data_2024 <- move2::movebank_download_study(6071688,
                                          sensor_type_id = "gps",
                                          timestamp_start = minmax_buff[[5]],
                                          timestamp_end = minmax_buff[[6]])

od22 <- left_join(ornitela_data_2022, dplyr::select(mt_track_data(ornitela_data_2022), individual_local_identifier, deployment_id))
od23 <- left_join(ornitela_data_2023, dplyr::select(mt_track_data(ornitela_data_2023), individual_local_identifier, deployment_id))
od24 <- left_join(ornitela_data_2024, dplyr::select(mt_track_data(ornitela_data_2024), individual_local_identifier, deployment_id))

id22 <- left_join(inpa_data_2022, dplyr::select(mt_track_data(inpa_data_2022), individual_local_identifier, deployment_id))
id23 <- left_join(inpa_data_2023, dplyr::select(mt_track_data(inpa_data_2023), individual_local_identifier, deployment_id))
id24 <- left_join(inpa_data_2024, dplyr::select(mt_track_data(inpa_data_2024), individual_local_identifier, deployment_id))

write_rds(od22, file = here("data/ornitela_data_2022.RDS"))
write_rds(od23, file = here("data/ornitela_data_2023.RDS"))
write_rds(od24, file = here("data/ornitela_data_2024.RDS"))
write_rds(id22, file = here("data/inpa_data_2022.RDS"))
write_rds(id23, file = here("data/inpa_data_2023.RDS"))
write_rds(id24, file = here("data/inpa_data_2024.RDS"))