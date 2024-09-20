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
source(here("R/functions.R"))

# data_files_2023 <- list.files(here("data/ACC/2023_hf_period/raw/"), full.names = T)
# 
# unobs_raw_acc_2023 <- data.table::fread(data_files_2023[1], select = c("Latitude", "Longitude", "UTC_datetime", "UTC_date", "UTC_time", "datatype", "device_id", "acc_x", "acc_y", "acc_z"))
# 
# splitup <- group_split(group_by(as.data.frame(unobs_raw_acc_2023)), device_id)
# for(i in 1:length(splitup)){
#   write_csv(splitup[[i]], file = paste0(here("data/ACC/2023_hf_period/created/"), "/device_", i, ".csv"))
#   cat("finished", i)
# }
# devices <- map_chr(splitup, ~.x$device_id[1])
#
# write_rds(unobs_raw_acc_2023, file = here("data/ACC/2023_hf_period/created/unobs_raw_acc_2023.RDS"))
# unobs_raw_acc_2023 <- readRDS(here("data/ACC/2023_hf_period/created/unobs_raw_acc_2023.RDS"))
mindate <- lubridate::ymd_hms(min(unobs_raw_acc_2023$UTC_datetime))
maxdate <- lubridate::ymd_hms(max(unobs_raw_acc_2023$UTC_datetime))


#rm(unobs_raw_acc_2023)
#rm(splitup)

# calibration_data <- read_csv(here("ACC_algo_Marta_draft/Data/example_calibration.csv"))
# 
# files <- list.files(here("data/ACC/2023_hf_period/created/"), pattern = ".csv", full.names = T)
# 
# for(i in 1:length(files)){
#   name <- str_extract(files[i], "device_[0-9]+")
#   file <- as.data.frame(data.table::fread(files[i]))
#   prepared <- prepare_dataset(file, calibration = calibration_data)
#   write_csv(prepared, paste0(here("data/ACC/2023_hf_period/created/prepared/"), "/", name, "_prepared.csv"))
#   rm(file)
#   rm(prepared)
#   cat("done with", i, "\n")
# }
# 
files <- list.files(here("data/ACC/2023_hf_period/created/prepared/"), pattern = ".csv", full.names = T)

# prepared_2023 <- vector(mode = "list", length = length(files))
# for(i in 1:length(files)){
#   prepared_2023[[i]] <- as.data.frame(data.table::fread(files[i]))
# }
#write_rds(prepared_2023, here("data/ACC/2023_hf_period/created/prepared_2023.RDS"))

bouts_2023 <- vector(mode = "list", length = length(files))

for(i in 1:length(files)){
  file <- as.data.frame(data.table::fread(files[i]))
  bouts_2023[[i]] <- file[,c("bout_id", "device_id", "start_int")] %>%
    group_by(device_id, bout_id) %>%
    summarize(start = min(start_int),
              end = max(start_int),
              .groups = "drop")
  cat("Done with bouts for", i, "\n")
}

mod <- readRDS(here("ACC_algo_Marta_draft/Data/gv_final_model_fit.rda"))

predictions_2023 <- vector(mode = "list", length = length(files))
scores_2023 <- vector(mode = "list", length = length(files))
for(i in 25:length(files)){
  file <- as.data.frame(data.table::fread(files[i]))
  if(nrow(file) > 0){
    file$start_int <- as.character(file$start_int)
    predictions_2023[[i]] <- predict(mod, file)
    scores_2023[[i]] <- predict(mod, file, type = "prob")
  }
  cat("Done with predictions and scores for", i, "\n")
}

write_rds(predictions_2023, file = here("data/ACC/2023_hf_period/created/predictions_2023.RDS"))
write_rds(scores_2023, file = here("data/ACC/2023_hf_period/created/scores_2023.RDS"))
write_rds(bouts_2023, file = here("data/ACC/2023_hf_period/created/bouts_2023.RDS"))

predictions_2023 <- readRDS(here("data/ACC/2023_hf_period/created/predictions_2023.RDS"))
bouts_2023 <- readRDS(here("data/ACC/2023_hf_period/created/bouts_2023.RDS"))
scores_2023 <- readRDS(here("data/ACC/2023_hf_period/created/scores_2023.RDS"))

prepared_files <- list.files(here("data/ACC/2023_hf_period/created/prepared/"), pattern = ".csv", full.names = T)
bouts_predictions_2023 <- vector(mode = "list", length = length(predictions_2023))
for(i in 1:length(prepared_files)){
  filename <- prepared_files[i]
  prepared <- as.data.frame(data.table::fread(filename))
  if(nrow(prepared) > 0){
    bouts_predictions_2023[[i]] <- distinct(get_bouts_predictions(prepared, predictions_2023[[i]], scores_2023[[i]], bouts_2023[[i]]))
  }else{
    bouts_predictions_2023[[i]] <- NULL
  }
  
  cat("finished", i, "\n")
}

write_rds(bouts_predictions_2023, file = here("data/ACC/2023_hf_period/created/bouts_predictions_2023.RDS"))

# targets::tar_load(loginObject)
# ornitela_data_2023 <- vultureUtils::downloadVultures(loginObject = loginObject,
#                                           removeDup = T, dfConvert = T,
#                                           quiet = T,
#                                           dateTimeStartUTC = mindate,
#                                           dateTimeEndUTC = maxdate)
# 
# gps_2023 <- dplyr::select(ornitela_data_2023, tag_id, timestamp, dateOnly, ground_speed, location_lat, location_long, individual_id, tag_local_identifier)
# rm(ornitela_data_2023)
# gc()
# 
# data.table::fwrite(gps_2023, file = here("data/ACC/2023_hf_period/created/gps_2023.csv"))
gps_2023 <- data.table::fread(here("data/ACC/2023_hf_period/created/gps_2023.csv"))

bouts_predictions_2023 <- readRDS(here("data/ACC/2023_hf_period/created/bouts_predictions_2023.RDS"))
device_ids_2023 <- purrr::map(bouts_predictions_2023, ~.x$device_id[1])

# focal_gps_2023 <- map(device_ids_2023, ~{
#   if(length(.x) > 0){
#     filter(gps_2023, tag_local_identifier == .x)
#   }else{
#     NULL
#   }})
# write_rds(focal_gps_2023, here("data/ACC/2023_hf_period/created/focal_gps_2023.RDS"))
focal_gps_2023 <- readRDS(here("data/ACC/2023_hf_period/created/focal_gps_2023.RDS"))

matches_2023 <- map2(bouts_predictions_2023, focal_gps_2023, ~{
  if(!is.null(.x)){
    get_matches(.x, .y)
  }else{
    NULL
  }
}, .progress = T)
write_rds(matches_2023, here("data/ACC/2023_hf_period/created/matches_2023.RDS"))
matches_2023 <- readRDS(here("data/ACC/2023_hf_period/created/matches_2023.RDS"))

joined_2023 <- map2(bouts_predictions_2023, matches_2023, ~{
  if(!is.null(.y)){
    left_join(.x, .y, by = c("device_id" = "tag_local_identifier",
                             "bout_id"))
  }else{
    NULL
  }
})
write_rds(joined_2023, here("data/ACC/2023_hf_period/created/joined_2023.RDS"))
joined_2023 <- readRDS(here("data/ACC/2023_hf_period/created/joined_2023.RDS"))

feeding_bouts_certain_2023 <- map(joined_2023, ~{
  if(!is.null(.x)){
    filter(.x, pred == "Eating" & !is.na(location_lat) &
             .pred_Eating > 0.5) %>%
      sf::st_as_sf(coords = c("location_long", "location_lat"),
                   crs = "WGS84")
  }else{
    NULL
  }
})
write_rds(feeding_bouts_certain_2023, here("data/ACC/2023_hf_period/created/feeding_bouts_certain_2023.RDS"))
feeding_bouts_certain_2023 <- readRDS(here("data/ACC/2023_hf_period/created/feeding_bouts_certain_2023.RDS"))

targets::tar_load(fs_union)
library(sf)

feeding_bouts_station_2023 <- map(feeding_bouts_certain_2023, ~{
  if(!is.null(.x)){
    assign_fs(.x, fs_union)
  }else{NULL}
})

write_rds(feeding_bouts_station_2023, here("data/ACC/2023_hf_period/created/feeding_bouts_station_2023.RDS"))
feeding_bouts_station_2023 <- readRDS(here("data/ACC/2023_hf_period/created/feeding_bouts_station_2023.RDS"))
