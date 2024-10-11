## 2023 period
### Get data
library(tidyverse)
library(data.table)
library(here)
library(ranger)
library(tidymodels)
library(moments)
library(parsnip)
library(caret)
library(zoo)
library(sf)
source(here("R/functions.R"))

# data_files_2023 <- list.files(here("data/ACC/2023_hf_period/raw/"), full.names = T, pattern = ".csv")
# data_files_2024 <- list.files(here("data/ACC/2024_hf_period/raw/"), full.names = T, pattern = ".csv")
# 
# unobs_raw_acc_2023 <- purrr::list_rbind(purrr::map(data_files_2023, ~as.data.frame(data.table::fread(.x, select = c("Latitude", "Longitude", "UTC_datetime", "UTC_date", "UTC_time", "datatype", "device_id", "acc_x", "acc_y", "acc_z")))))
# unobs_raw_acc_2024 <- purrr::list_rbind(purrr::map(data_files_2024, ~as.data.frame(data.table::fread(.x, select = c("Latitude", "Longitude", "UTC_datetime", "UTC_date", "UTC_time", "datatype", "device_id", "acc_x", "acc_y", "acc_z")))))
# 
# readr::write_rds(unobs_raw_acc_2023, here("data/created/unobs_raw_acc_2023.RDS"))
# readr::write_rds(unobs_raw_acc_2024, here("data/created/unobs_raw_acc_2024.RDS"))
# 
# unobs_raw_acc_2023 <- readRDS(here("data/created/unobs_raw_acc_2023.RDS"))
# unobs_raw_acc_2024 <- readRDS(here("data/created/unobs_raw_acc_2024.RDS"))
# 
# mindate_23 <- lubridate::ymd_hms(min(unobs_raw_acc_2023$UTC_datetime))
# maxdate_23 <- lubridate::ymd_hms(max(unobs_raw_acc_2023$UTC_datetime))
# 
# mindate_24 <- lubridate::ymd_hms(min(unobs_raw_acc_2024$UTC_datetime))
# maxdate_24 <- lubridate::ymd_hms(max(unobs_raw_acc_2024$UTC_datetime))
# 
# minmax_dates <- list(mindate_23, maxdate_23, mindate_24, maxdate_24)
# write_rds(minmax_dates, file = here("data/created/minmax_dates.RDS"))

# splitup_23 <- group_split(group_by(as.data.frame(unobs_raw_acc_2023)), device_id)
# splitup_24 <- group_split(group_by(as.data.frame(unobs_raw_acc_2024)), device_id)
# 
# write_rds(splitup_23, here("data/created/splitup_23.RDS"))
# write_rds(splitup_24, here("data/created/splitup_24.RDS"))

# rm(unobs_raw_acc_2023)
# rm(unobs_raw_acc_2024)
# gc()
# 
# splitup_23 <- readRDS(here("data/created/splitup_23.RDS"))
# splitup_24 <- readRDS(here("data/created/splitup_24.RDS"))

# devices_23 <- map_chr(splitup_23, ~as.character(.x$device_id[1]))
# devices_24 <- map_chr(splitup_24, ~as.character(.x$device_id[1]))
# 
# calibration_data <- read_csv(here("ACC_algo_Marta_draft/Data/example_calibration.csv"))

# for(i in 1:length(splitup_23)){
#   name <- paste0("device_", devices_23[i])
#   prepared <- prepare_dataset(splitup_23[[i]], calibration = calibration_data)
#   write_csv(prepared, paste0(here("data/ACC/2023_hf_period/created/prepared/"), "/", name, "_prepared.csv"))
#   cat("done with", i, "\n")
# }

# for(i in 1:length(splitup_24)){
#   name <- paste0("device_", devices_24[i])
#   prepared <- prepare_dataset(splitup_24[[i]], calibration = calibration_data)
#   write_csv(prepared, paste0(here("data/ACC/2024_hf_period/created/prepared/"), "/", name, "_prepared.csv"))
#   cat("done with", i, "\n")
# }

files_23 <- list.files(here("data/ACC/2023_hf_period/created/prepared/"), pattern = ".csv", full.names = T)
files_24 <- list.files(here("data/ACC/2024_hf_period/created/prepared/"), pattern = ".csv", full.names = T)

# bouts_2023 <- vector(mode = "list", length = length(files_23))
# for(i in 1:length(files_23)){
#   file <- as.data.frame(data.table::fread(files_23[i]))
#   bouts_2023[[i]] <- file[,c("bout_id", "device_id", "start_int")] %>%
#     group_by(device_id, bout_id) %>%
#     summarize(start = min(start_int),
#               end = max(start_int),
#               .groups = "drop")
#   cat("Done with bouts for", i, "\n")
# }
# 
# bouts_2024 <- vector(mode = "list", length = length(files_24))
# for(i in 1:length(files_24)){
#   file <- as.data.frame(data.table::fread(files_24[i]))
#   bouts_2024[[i]] <- file[,c("bout_id", "device_id", "start_int")] %>%
#     group_by(device_id, bout_id) %>%
#     summarize(start = min(start_int),
#               end = max(start_int),
#               .groups = "drop")
#   cat("Done with bouts for", i, "\n")
# }
# 
# write_rds(bouts_2023, here("data/ACC/2023_hf_period/created/bouts_2023.RDS"))
# write_rds(bouts_2024, here("data/ACC/2024_hf_period/created/bouts_2024.RDS"))

bouts_2023 <- readRDS(here("data/ACC/2023_hf_period/created/bouts_2023.RDS"))
bouts_2024 <- readRDS(here("data/ACC/2024_hf_period/created/bouts_2024.RDS"))

mod <- readRDS(here("ACC_algo_Marta_draft/Data/gv_final_model_fit.rda"))

# predictions_2023 <- vector(mode = "list", length = length(files_23))
# scores_2023 <- vector(mode = "list", length = length(files_23))
# for(i in 1:length(files_23)){
#   file <- as.data.frame(data.table::fread(files_23[i]))
#   if(nrow(file) > 0){
#     file$start_int <- as.character(file$start_int)
#     predictions_2023[[i]] <- predict(mod, file)
#     scores_2023[[i]] <- predict(mod, file, type = "prob")
#   }
#   cat("Done with predictions and scores for", i, "\n")
# }
# 
# predictions_2024 <- vector(mode = "list", length = length(files_24))
# scores_2024 <- vector(mode = "list", length = length(files_24))
# for(i in 1:length(files_24)){
#   file <- as.data.frame(data.table::fread(files_24[i]))
#   if(nrow(file) > 0){
#     file$start_int <- as.character(file$start_int)
#     predictions_2024[[i]] <- predict(mod, file)
#     scores_2024[[i]] <- predict(mod, file, type = "prob")
#   }
#   cat("Done with predictions and scores for", i, "\n")
# }
# 
# write_rds(predictions_2023, file = here("data/ACC/2023_hf_period/created/predictions_2023.RDS"))
# write_rds(scores_2023, file = here("data/ACC/2023_hf_period/created/scores_2023.RDS"))
# 
# write_rds(predictions_2024, file = here("data/ACC/2024_hf_period/created/predictions_2024.RDS"))
# write_rds(scores_2024, file = here("data/ACC/2024_hf_period/created/scores_2024.RDS"))

predictions_2023 <- readRDS(here("data/ACC/2023_hf_period/created/predictions_2023.RDS"))
bouts_2023 <- readRDS(here("data/ACC/2023_hf_period/created/bouts_2023.RDS"))
scores_2023 <- readRDS(here("data/ACC/2023_hf_period/created/scores_2023.RDS"))

predictions_2024 <- readRDS(here("data/ACC/2024_hf_period/created/predictions_2024.RDS"))
bouts_2024 <- readRDS(here("data/ACC/2024_hf_period/created/bouts_2024.RDS"))
scores_2024 <- readRDS(here("data/ACC/2024_hf_period/created/scores_2024.RDS"))

# prepared_files_23 <- list.files(here("data/ACC/2023_hf_period/created/prepared/"), pattern = ".csv", full.names = T)
# bouts_predictions_2023 <- vector(mode = "list", length = length(predictions_2023))
# for(i in 1:length(prepared_files_23)){
#   filename <- prepared_files_23[i]
#   prepared <- as.data.frame(data.table::fread(filename))
#   if(nrow(prepared) > 0){
#     bouts_predictions_2023[[i]] <- distinct(get_bouts_predictions(prepared, predictions_2023[[i]], scores_2023[[i]], bouts_2023[[i]]))
#   }else{
#     bouts_predictions_2023[[i]] <- NULL
#   }
#   
#   cat("finished", i, "\n")
# }
# 
# prepared_files_24 <- list.files(here("data/ACC/2024_hf_period/created/prepared/"), pattern = ".csv", full.names = T)
# bouts_predictions_2024 <- vector(mode = "list", length = length(predictions_2024))
# for(i in 1:length(prepared_files_24)){
#   filename <- prepared_files_24[i]
#   prepared <- as.data.frame(data.table::fread(filename))
#   if(nrow(prepared) > 0){
#     bouts_predictions_2024[[i]] <- distinct(get_bouts_predictions(prepared, predictions_2024[[i]], scores_2024[[i]], bouts_2024[[i]]))
#   }else{
#     bouts_predictions_2024[[i]] <- NULL
#   }
#   
#   cat("finished", i, "\n")
# }
# 
# write_rds(bouts_predictions_2023, file = here("data/ACC/2023_hf_period/created/bouts_predictions_2023.RDS"))
# write_rds(bouts_predictions_2024, file = here("data/ACC/2024_hf_period/created/bouts_predictions_2024.RDS"))

targets::tar_load(loginObject)
minmax_dates <- readRDS(here("data/created/minmax_dates.RDS"))
# ornitela_data_2023 <- vultureUtils::downloadVultures(loginObject = loginObject,
#                                           removeDup = T, dfConvert = T,
#                                           quiet = T,
#                                           dateTimeStartUTC = minmax_dates[[1]],
#                                           dateTimeEndUTC = minmax_dates[[2]])

# ornitela_data_2024 <- vultureUtils::downloadVultures(loginObject = loginObject,
#                                           removeDup = T, dfConvert = T,
#                                           quiet = T,
#                                           dateTimeStartUTC = minmax_dates[[3]],
#                                           dateTimeEndUTC = minmax_dates[[4]])
# gps_2023 <- dplyr::select(ornitela_data_2023, tag_id, timestamp, dateOnly, ground_speed, location_lat, location_long, individual_id, tag_local_identifier)
# gps_2024 <- dplyr::select(ornitela_data_2024, tag_id, timestamp, dateOnly, ground_speed, location_lat, location_long, individual_id, tag_local_identifier)
# rm(ornitela_data_2023)
# rm(ornitela_data_2024)
# gc()
# 
# data.table::fwrite(gps_2023, file = here("data/ACC/2023_hf_period/created/gps_2023.csv"))
# data.table::fwrite(gps_2024, file = here("data/ACC/2024_hf_period/created/gps_2024.csv"))
# gps_2023 <- data.table::fread(here("data/ACC/2023_hf_period/created/gps_2023.csv"))
# gps_2024 <- data.table::fread(here("data/ACC/2024_hf_period/created/gps_2024.csv"))
# gc()
# 
# bouts_predictions_2023 <- readRDS(here("data/ACC/2023_hf_period/created/bouts_predictions_2023.RDS"))
# bouts_predictions_2024 <- readRDS(here("data/ACC/2024_hf_period/created/bouts_predictions_2024.RDS"))
# 
# device_ids_2023 <- purrr::map(bouts_predictions_2023, ~.x$device_id[1])
# device_ids_2024 <- purrr::map(bouts_predictions_2024, ~.x$device_id[1])
# 
# focal_gps_2023 <- map(device_ids_2023, ~{
#   if(length(.x) > 0){
#     filter(gps_2023, tag_local_identifier == .x)
#   }else{NULL}})
# focal_gps_2024 <- map(device_ids_2024, ~{
#   if(length(.x) > 0){
#     filter(gps_2024, tag_local_identifier == .x)
#   }else{NULL}})
# write_rds(focal_gps_2023, here("data/ACC/2023_hf_period/created/focal_gps_2023.RDS"))
# write_rds(focal_gps_2024, here("data/ACC/2024_hf_period/created/focal_gps_2024.RDS"))

focal_gps_2023 <- readRDS(here("data/ACC/2023_hf_period/created/focal_gps_2023.RDS"))
focal_gps_2024 <- readRDS(here("data/ACC/2024_hf_period/created/focal_gps_2024.RDS"))

# future::plan(future::multisession(workers = 10))
# matches_2023 <- furrr::future_map2(bouts_predictions_2023, focal_gps_2023, ~{
#   if(!is.null(.x)){
#     get_matches(.x, .y)
#   }else{NULL}
# }, .progress = T)
# matches_2024 <- furrr::future_map2(bouts_predictions_2024, focal_gps_2024, ~{
#   if(!is.null(.x)){
#     get_matches(.x, .y)
#   }else{NULL}
# }, .progress = T)
# write_rds(matches_2023, here("data/ACC/2023_hf_period/created/matches_2023.RDS"))
# write_rds(matches_2024, here("data/ACC/2024_hf_period/created/matches_2024.RDS"))

matches_2023 <- readRDS(here("data/ACC/2023_hf_period/created/matches_2023.RDS"))
matches_2024 <- readRDS(here("data/ACC/2024_hf_period/created/matches_2024.RDS"))

# xxx START HERE ERROR
# joined_2023 <- map2(bouts_predictions_2023, matches_2023, ~{
#   if(nrow(.y) > 0){
#     left_join(.x, .y, by = c("device_id" = "tag_local_identifier",
#                              "bout_id"))
#   }else{NULL}
# })
# 
# joined_2024 <- map2(bouts_predictions_2024, matches_2024, ~{
#   if(nrow(.y) > 0){
#     left_join(.x, .y, by = c("device_id" = "tag_local_identifier",
#                              "bout_id"))
#   }else{NULL}
# })
# write_rds(joined_2023, here("data/ACC/2023_hf_period/created/joined_2023.RDS"))
#write_rds(joined_2024, here("data/ACC/2024_hf_period/created/joined_2024.RDS"))

joined_2023 <- readRDS(here("data/ACC/2023_hf_period/created/joined_2023.RDS"))
joined_2024 <- readRDS(here("data/ACC/2024_hf_period/created/joined_2024.RDS"))

### define function to get the feeding bouts
getfeeding <- function(x){
  out <- filter(x, pred == "Eating" & !is.na(location_lat) & .pred_Eating > 0.75)
  out <- sf::st_as_sf(out, coords = c("location_long", "location_lat"),
                      crs = "WGS84", remove = F) %>%
    st_transform(32636)
  return(out)
}

feeding_bouts_certain_2023 <- map(joined_2023, ~{
  if(!is.null(.x)){
    getfeeding(.x)
  }else{NULL}
})
feeding_bouts_certain_2024 <- map(joined_2024, ~{
  if(!is.null(.x)){
    getfeeding(.x)
  }else{NULL}
})

write_rds(feeding_bouts_certain_2023, here("data/ACC/2023_hf_period/created/feeding_bouts_certain_2023.RDS"))
write_rds(feeding_bouts_certain_2024, here("data/ACC/2024_hf_period/created/feeding_bouts_certain_2024.RDS"))

feeding_bouts_certain_2023 <- readRDS(here("data/ACC/2023_hf_period/created/feeding_bouts_certain_2023.RDS"))
feeding_bouts_certain_2024 <- readRDS(here("data/ACC/2024_hf_period/created/feeding_bouts_certain_2024.RDS"))
keep_2023 <- which(!map_lgl(feeding_bouts_certain_2023, is.null))
keep_2024 <- which(!map_lgl(feeding_bouts_certain_2024, is.null))

bouts23 <- purrr::list_rbind(feeding_bouts_certain_2023[keep_2023]) %>%
  mutate(year = 2023,
         start = lubridate::ymd_hms(start),
         end = lubridate::ymd_hms(end),
         dateOnly = lubridate::ymd(dateOnly),
         tag_id = as.numeric(tag_id),
         individual_id = as.numeric(individual_id))
bouts24 <- feeding_bouts_certain_2024[keep_2024] %>%
  map(., ~.x %>% mutate(tag_id = as.numeric(tag_id),
                        individual_id = as.numeric(individual_id))) %>%
  purrr::list_rbind() %>%
  mutate(year = 2024,
         start = lubridate::ymd_hms(start),
         end = lubridate::ymd_hms(end),
         dateOnly = lubridate::ymd(dateOnly))
feeding_bouts <- bind_rows(bouts23, bouts24) %>%
  sf::st_as_sf(crs = 32636)
feeding_bouts <- feeding_bouts %>%
  bind_cols(st_coordinates(.))
write_rds(feeding_bouts, here("data/created/feeding_bouts.RDS"))
