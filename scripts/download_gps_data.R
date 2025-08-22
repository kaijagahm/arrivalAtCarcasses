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

od22 <- left_join(ornitela_data_2022, mt_track_data(ornitela_data_2022))
od23 <- left_join(ornitela_data_2023, mt_track_data(ornitela_data_2023))
od24 <- left_join(ornitela_data_2024, mt_track_data(ornitela_data_2024))

id22 <- left_join(inpa_data_2022, mt_track_data(inpa_data_2022))
id23 <- left_join(inpa_data_2023, mt_track_data(inpa_data_2023))
id24 <- left_join(inpa_data_2024, mt_track_data(inpa_data_2024))

toremove <- c("sensor_type_id", "acceleration_raw_x", "acceleration_raw_y", "acceleration_raw_z", "barometric_height", "external_temperature", "gps_hdop", "import_marked_outlier", "light_level", "magnetic_field_raw_x", "magnetic_field_raw_y", "tag_voltage", "event_id", "visible", "deployment_comments", "deploy_off_person", "deploy_off_timestamp", "deploy_on_person", "deploy_on_timestamp", "capture_location", "deploy_on_location", "deploy_off_location", "individual_comments", "death_comments", "taxon_canonical_name", "acknowledgements", "citation", "grants_used", "has_quota", "i_am_owner", "is_test", "license_type", "name", "study_number_of_deployments", "number_of_individuals", "number_of_tags", "principal_investigator_name", "study_objective", "study_type", "suspend_license_terms", "i_can_see_data", "there_are_data_which_i_cannot_see", "i_have_download_access", "i_am_collaborator", "study_permission", "number_of_deployed_locations", "timestamp_first_deployed_location", "timestamp_last_deployed_location", "taxon_ids", "contact_person_name", "main_location", "data_decoding_software", "eobs_activity", "eobs_activity_samples", "eobs_battery_voltage", "eobs_fix_battery_voltage", "eobs_horizontal_accuracy_estimate", "eobs_key_bin_checksum", "eobs_speed_accuracy_estimate", "eobs_start_timestamp", "eobs_status", "eobs_temperature", "eobs_type_of_fix", "eobs_used_time_to_get_fixed", "gps_dop", "gps_vdop", "height_above_ellipsoid", "height_raw", "magnetic_field_raw_z", "manually_marked_outlier", "orientation_quaternion_raw_w", "orientation_quaternion_raw_x", "orientation_quaternion_raw_y", "orientation_quaternion_raw_z", "barometric_pressure", "eobs_used_time_to_get_fix", "siblings", "manufacturer_name", "model", "processing_type", "serial_no", "weight")

od22 <- od22 %>% dplyr::select(-any_of(toremove))
od23 <- od23 %>% dplyr::select(-any_of(toremove))
od24 <- od24 %>% dplyr::select(-any_of(toremove))
id22 <- id22 %>% dplyr::select(-any_of(toremove))
id23 <- id23 %>% dplyr::select(-any_of(toremove))
id24 <- id24 %>% dplyr::select(-any_of(toremove))

write_rds(od22, file = here("data/ornitela_data_2022.RDS"))
write_rds(od23, file = here("data/ornitela_data_2023.RDS"))
write_rds(od24, file = here("data/ornitela_data_2024.RDS"))
write_rds(id22, file = here("data/inpa_data_2022.RDS"))
write_rds(id23, file = here("data/inpa_data_2023.RDS"))
write_rds(id24, file = here("data/inpa_data_2024.RDS"))
