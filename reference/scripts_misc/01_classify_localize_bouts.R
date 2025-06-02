library(tidyverse)
library(data.table)
library(ranger)
library(tidymodels)
library(moments)
library(parsnip)
library(caret)
library(zoo)
library(sf)
library(ggmap)
library(move)
library(here)
source(here("R/functions.R"))

# Get data files
# data_files_2023 <- list.files(here("data/ACC/2023_hf_period/raw/"), full.names = T, pattern = ".csv")
# data_files_2024 <- list.files(here("data/ACC/2024_hf_period/raw/"), full.names = T, pattern = ".csv")

# unobs_raw_acc_2023 <- purrr::list_rbind(purrr::map(data_files_2023, ~as.data.frame(data.table::fread(.x, select = c("Latitude", "Longitude", "UTC_datetime", "UTC_date", "UTC_time", "datatype", "device_id", "acc_x", "acc_y", "acc_z"))))) %>%
# filter(!is.na(datatype))
# unobs_raw_acc_2024 <- purrr::list_rbind(purrr::map(data_files_2024, ~as.data.frame(data.table::fread(.x, select = c("Latitude", "Longitude", "UTC_datetime", "UTC_date", "UTC_time", "datatype", "device_id", "acc_x", "acc_y", "acc_z"))))) %>%
# filter(!is.na(datatype))
# 
# readr::write_rds(unobs_raw_acc_2023, here("data/created/unobs_raw_acc_2023.RDS"))
# readr::write_rds(unobs_raw_acc_2024, here("data/created/unobs_raw_acc_2024.RDS"))

unobs_raw_acc_2023 <- readRDS(here("data/created/unobs_raw_acc_2023.RDS"))
unobs_raw_acc_2024 <- readRDS(here("data/created/unobs_raw_acc_2024.RDS"))

# Fix INPA tags with backwards acc_y sensor -------------------------------
# # # Look for ones that need to be flipped
# inpa_taglist_full <- readxl::read_excel(here("data/raw/INPA_tag_list_Kaija.xlsx"), sheet = 1)
# inpa_taglist_partial <- readxl::read_excel(here("data/raw/INPA_tag_list_Kaija.xlsx"), sheet = 2)
# 
# inpa_taglist <- inpa_taglist_full %>%
#   mutate(type = "full") %>%
#   bind_rows(inpa_taglist_partial %>%
#               mutate(type = "partial"))
# 
# tags_2023 <- unobs_raw_acc_2023 %>%
#   dplyr::select(device_id) %>%
#   distinct() %>%
#   left_join(inpa_taglist, by = "device_id") %>%
#   mutate(tagtype = case_when(!is.na(movebank_id) ~ "INPA",
#                              .default = "TAU")) # none of the tags included in the attached list seem to match the 2023 data.
# 
# # 2024-10-22 I have realized that this is because Gideon never sent me the INPA tag high-frequency data for 2023, only 2024. So I actually don't have any INPA tags in here, which explains why none of them need to be flipped. Have to wait for Gideon to send me the INPA data for 2023.
# 
# length(unique(unobs_raw_acc_2023$device_id)) # 57 unique individuals
# length(unique(unobs_raw_acc_2024$device_id)) # 80 individuals. This number is so much higher because I also included individuals that weren't set to high frequency.
# 
# unobs_raw_acc_2023 %>%
#   group_by(device_id) %>%
#   summarize(mny = mean(acc_y)) %>%
#   arrange(mny) # no negative means here--no need to flip
# 
# write_csv(unobs_raw_acc_2023 %>%
#             group_by(device_id) %>%
#             summarize(mny = mean(acc_y)) %>%
#             arrange(mny), file = here("data/created/2023_device_ymeans_forGideon_2024-10-22.csv"))

unobs_raw_acc_2023 %>%
  group_by(device_id, UTC_date) %>%
  summarize(mny = mean(acc_y)) %>%
  ungroup() %>%
  ggplot(aes(x = fct_reorder(factor(device_id), mny, .fun = "median"), 
             y = mny))+
  geom_boxplot(outlier.size = 0.5, fill = "lightgray", linewidth = 0.5)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 5))+
  geom_hline(aes(yintercept = 0), lty = 2, col = "red", linewidth = 0.75)+
  labs(y = "Mean daily y acceleration",
       x = "Tag",
       title = "2023 HF period")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

unobs_raw_acc_2024 %>%
  group_by(device_id) %>%
  summarize(mny = mean(acc_y)) %>%
  arrange(mny) # need to flip several

unobs_raw_acc_2024 %>%
  group_by(device_id, UTC_date) %>%
  summarize(mny = mean(acc_y)) %>%
  ungroup() %>%
  ggplot(aes(x = fct_reorder(factor(device_id), mny, .fun = "median"), 
             y = mny))+
  geom_boxplot(outlier.size = 0.5, fill = "lightgray", linewidth = 0.5)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 5))+
  geom_hline(aes(yintercept = 0), lty = 2, col = "red", linewidth = 0.75)+
  labs(y = "Mean daily y acceleration",
       x = "Tag",
       title = "2023 HF period")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) # it's very evident which ones need to be flipped here

toflip_y <- unobs_raw_acc_2024 %>%
  group_by(device_id) %>%
  summarize(mny = mean(acc_y)) %>%
  filter(mny < 0) %>%
  pull(device_id)
unobs_raw_acc_2024 <- unobs_raw_acc_2024 %>%
  mutate(acc_y = case_when(device_id %in% toflip_y ~ -1*acc_y,
                           .default = acc_y))

## check that we did this right
unobs_raw_acc_2024 %>%
  group_by(device_id) %>%
  summarize(mny = mean(acc_y)) %>%
  arrange(mny) # that's better--no negatives!

unobs_raw_acc_2024 %>%
  group_by(device_id, UTC_date) %>%
  summarize(mny = mean(acc_y)) %>%
  ungroup() %>%
  ggplot(aes(x = fct_reorder(factor(device_id), mny, .fun = "median"), 
             y = mny))+
  geom_boxplot(outlier.size = 0.5, fill = "lightgray", linewidth = 0.5)+
  theme_classic()+
  theme(axis.text.x = element_text(size = 5))+
  geom_hline(aes(yintercept = 0), lty = 2, col = "red", linewidth = 0.75)+
  labs(y = "Mean daily y acceleration",
       x = "Tag",
       title = "2023 HF period")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) # much better

# mindate_23 <- lubridate::ymd_hms(min(unobs_raw_acc_2023$UTC_datetime))
# maxdate_23 <- lubridate::ymd_hms(max(unobs_raw_acc_2023$UTC_datetime)) + days(5)
# mindate_24 <- lubridate::ymd_hms(min(unobs_raw_acc_2024$UTC_datetime))
# maxdate_24 <- lubridate::ymd_hms(max(unobs_raw_acc_2024$UTC_datetime)) + days(5)
# 
# minmax_dates <- list(mindate_23, maxdate_23, mindate_24, maxdate_24)
# write_rds(minmax_dates, file = here("data/created/minmax_dates.RDS"))

splitup_23 <- group_split(group_by(as.data.frame(unobs_raw_acc_2023)), device_id)
splitup_24 <- group_split(group_by(as.data.frame(unobs_raw_acc_2024)), device_id)
# 
write_rds(splitup_23, here("data/created/splitup_23.RDS"))
write_rds(splitup_24, here("data/created/splitup_24.RDS"))

# rm(unobs_raw_acc_2023)
# rm(unobs_raw_acc_2024)
# gc()
# 
# splitup_23 <- readRDS(here("data/created/splitup_23.RDS"))
# splitup_24 <- readRDS(here("data/created/splitup_24.RDS"))

# devices_23 <- map_chr(splitup_23, ~as.character(.x$device_id[1]))
devices_24 <- map_chr(splitup_24, ~as.character(.x$device_id[1]))
# 
calibration_data <- read_csv(here("ACC_algo_Marta_draft/Data/example_calibration.csv"))

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

# write_rds(bouts_2023, here("data/ACC/2023_hf_period/created/bouts_2023.RDS"))
# write_rds(bouts_2024, here("data/ACC/2024_hf_period/created/bouts_2024.RDS"))

bouts_2023 <- readRDS(here("data/ACC/2023_hf_period/created/bouts_2023.RDS"))
bouts_2024 <- readRDS(here("data/ACC/2024_hf_period/created/bouts_2024.RDS"))

mod <- readRDS(here("ACC_algo_Marta_draft/Data/gv_final_model_fit.rda"))
# XXX START HERE
gc()

predictions_2023 <- vector(mode = "list", length = length(files_23))
scores_2023 <- vector(mode = "list", length = length(files_23))
for(i in 1:length(files_23)){
  file <- as.data.frame(data.table::fread(files_23[i]))
  if(nrow(file) > 0){
    file$start_int <- as.character(file$start_int)
    predictions_2023[[i]] <- predict(mod, file)
    scores_2023[[i]] <- predict(mod, file, type = "prob")
  }
  cat("Done with predictions and scores for", i, "\n")
}

predictions_2024 <- vector(mode = "list", length = length(files_24))
scores_2024 <- vector(mode = "list", length = length(files_24))
for(i in 1:length(files_24)){
  file <- as.data.frame(data.table::fread(files_24[i]))
  if(nrow(file) > 0){
    file$start_int <- as.character(file$start_int)
    predictions_2024[[i]] <- predict(mod, file)
    scores_2024[[i]] <- predict(mod, file, type = "prob")
  }
  cat("Done with predictions and scores for", i, "\n")
}
# 
# write_rds(predictions_2023, file = here("data/ACC/2023_hf_period/created/predictions_2023.RDS"))
# write_rds(scores_2023, file = here("data/ACC/2023_hf_period/created/scores_2023.RDS"))
# 
write_rds(predictions_2024, file = here("data/ACC/2024_hf_period/created/predictions_2024.RDS"))
write_rds(scores_2024, file = here("data/ACC/2024_hf_period/created/scores_2024.RDS"))

# predictions_2023 <- readRDS(here("data/ACC/2023_hf_period/created/predictions_2023.RDS"))
# bouts_2023 <- readRDS(here("data/ACC/2023_hf_period/created/bouts_2023.RDS"))
# scores_2023 <- readRDS(here("data/ACC/2023_hf_period/created/scores_2023.RDS"))
# 
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
prepared_files_24 <- list.files(here("data/ACC/2024_hf_period/created/prepared/"), pattern = ".csv", full.names = T)
bouts_predictions_2024 <- vector(mode = "list", length = length(predictions_2024))
for(i in 1:length(prepared_files_24)){
  filename <- prepared_files_24[i]
  prepared <- as.data.frame(data.table::fread(filename))
  if(nrow(prepared) > 0){
    bouts_predictions_2024[[i]] <- distinct(get_bouts_predictions(prepared, predictions_2024[[i]], scores_2024[[i]], bouts_2024[[i]]))
  }else{
    bouts_predictions_2024[[i]] <- NULL
  }

  cat("finished", i, "\n")
}

# write_rds(bouts_predictions_2023, file = here("data/ACC/2023_hf_period/created/bouts_predictions_2023.RDS"))
write_rds(bouts_predictions_2024, file = here("data/ACC/2024_hf_period/created/bouts_predictions_2024.RDS"))

targets::tar_load(loginObject)
minmax_dates <- readRDS(here("data/created/minmax_dates.RDS"))

# Matching to GPS data (Gideon code, merged with Kaija code) --------------------------------------
# ornitela_data_2023 <- vultureUtils::downloadVultures(loginObject = loginObject,
#                                           removeDup = T, dfConvert = T,
#                                           quiet = T,
#                                           dateTimeStartUTC = minmax_dates[[1]],
#                                           dateTimeEndUTC = minmax_dates[[2]])
# 
# ornitela_data_2024 <- vultureUtils::downloadVultures(loginObject = loginObject,
#                                           removeDup = T, dfConvert = T,
#                                           quiet = T,
#                                           dateTimeStartUTC = minmax_dates[[3]],
#                                           dateTimeEndUTC = minmax_dates[[4]])
# gps_2023 <- dplyr::select(ornitela_data_2023, local_identifier, tag_id, timestamp, dateOnly, ground_speed, location_lat, location_long, individual_id, tag_local_identifier)
# gps_2024 <- dplyr::select(ornitela_data_2024, local_identifier, tag_id, timestamp, dateOnly, ground_speed, location_lat, location_long, individual_id, tag_local_identifier)
# rm(ornitela_data_2023)
# rm(ornitela_data_2024)
# gc()
# 
# data.table::fwrite(gps_2023, file = here("data/ACC/2023_hf_period/created/gps_2023.csv"))
# data.table::fwrite(gps_2024, file = here("data/ACC/2024_hf_period/created/gps_2024.csv"))
gps_2023 <- data.table::fread("data/ACC/2023_hf_period/created/gps_2023.csv")
gps_2024 <- data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv")
# gc()
# 
bouts_predictions_2023 <- readRDS(here("data/ACC/2023_hf_period/created/bouts_predictions_2023.RDS"))
merged_2023 <- purrr::list_rbind(bouts_predictions_2023)
bouts_predictions_2024 <- readRDS(here("data/ACC/2024_hf_period/created/bouts_predictions_2024.RDS"))
merged_2024 <- purrr::list_rbind(bouts_predictions_2024)

## full.movestack is a movestack file converted to df, with a "study" column ("TAU" or INPA")
## merged is the classification output file (Filtered for Feeding only)
#fix timestamp
gps_2023$timestamp <- as.POSIXct(gps_2023$timestamp, tz = "UTC", format = "%Y-%m-%d %H:%M:%s")
gps_2024$timestamp <- as.POSIXct(gps_2024$timestamp, tz = "UTC", format = "%Y-%m-%d %H:%M:%s")

# keep only relevant 
gps_2023 <- gps_2023 %>% 
  dplyr::select(timestamp, tag_local_identifier, location_long,location_lat, ground_speed)
gps_2024 <- gps_2024 %>% 
  dplyr::select(timestamp, tag_local_identifier, location_long,location_lat, ground_speed)

gps_2023$sensor <- "GPS"
gps_2024$sensor <- "GPS"
merged_2023$sensor <- "ACC"
merged_2024$sensor <- "ACC"

merged_2023 <- merged_2023 %>%
  rename("tag_local_identifier" = device_id,
         "timestamp" = start)
merged_2024 <- merged_2024 %>%
  rename("tag_local_identifier" = device_id,
         "timestamp" = start)#make sure the colnames match time and device_id respectively # KG note: I'm assuming we should use the start, not end, column here, but I'm not sure exactly which column was column 4.

#attach/detach plyr, combine predictions with movestack
full_2023 <- plyr::rbind.fill(gps_2023, merged_2023)
full_2024 <- plyr::rbind.fill(gps_2024, merged_2024)

#check if last chunk worked
nrow(full_2023) == nrow(gps_2023) + nrow(merged_2023)
nrow(full_2024) == nrow(gps_2024) + nrow(merged_2024)

####prepare data for gps crossref#####
full_2023 <- full_2023 %>%
  group_by(tag_local_identifier) %>%
  arrange(timestamp) %>%
  mutate(time_diff = as.numeric(difftime(lead(timestamp), timestamp, units = "secs"))) %>%
  ungroup()

full_2024 <- full_2024 %>%
  group_by(tag_local_identifier) %>%
  arrange(timestamp) %>%
  mutate(time_diff = as.numeric(difftime(lead(timestamp), timestamp, units = "secs"))) %>%
  ungroup()

#####attach GPS point to ACC #####
attach_gps <- function(x){
  out <- x %>% 
    group_by(tag_local_identifier) %>%
    arrange(timestamp) %>%
    mutate(
      location_long_2 = case_when(is.na(location_long) & lag(time_diff) <= 300 & lag(ground_speed <= 4) ~ lag(location_long),
                                  is.na(location_long) & time_diff < 300 & lead(ground_speed <= 4) ~ lead(location_long),
                                  is.na(location_long) & lag(time_diff) <= 700 & lag(ground_speed <= 4) ~ lag(location_long),
                                  is.na(location_long) & time_diff < 700 & lead(ground_speed <= 4) ~ lead(location_long),TRUE ~  location_long),
      location_lat_2 = case_when(is.na(location_lat) & lag(time_diff) <= 300 & lag(ground_speed <= 4) ~ lag(location_lat),
                                 is.na(location_lat) & time_diff < 300 & lead(ground_speed <= 4) ~ lead(location_lat),
                                 is.na(location_lat) & lag(time_diff) <= 700 & lag(ground_speed <= 4) ~ lag(location_lat),
                                 is.na(location_lat) & time_diff < 700 & lead(ground_speed <= 4) ~ lead(location_lat),TRUE ~  location_lat),
      ground_speed_2 = case_when(is.na(location_long) & lag(time_diff) <= 300 & lag(ground_speed <= 4) ~ lag(ground_speed),
                                 is.na(location_long) & time_diff < 300 & lead(ground_speed <= 4) ~ lead(ground_speed),
                                 is.na(location_long) & lag(time_diff) <= 700 & lag(ground_speed <= 4) ~ lag(ground_speed),
                                 is.na(location_long) & time_diff < 700 & lead(ground_speed <= 4) ~ lead(ground_speed),TRUE ~  ground_speed)
    )%>% 
    mutate(
      location_long_3 = case_when(is.na(location_long_2) & lag(time_diff) <= 300 ~ lag(location_long),
                                  is.na(location_long_2) & time_diff < 300 ~ lead(location_long), TRUE ~  location_long_2),
      location_lat_3 = case_when(is.na(location_lat_2) & lag(time_diff) <= 300 ~ lag(location_lat),
                                 is.na(location_lat_2) & time_diff < 300 ~ lead(location_lat), TRUE ~  location_lat_2),
      ground_speed_3 = case_when(is.na(location_long_2) & lag(time_diff) <= 300 ~ lag(ground_speed),
                                 is.na(location_long_2) & time_diff < 300 ~ lead(ground_speed), TRUE ~  ground_speed_2)
    ) %>%
    ungroup()
  return(out)
}
full_2023 <- attach_gps(full_2023)
full_2024 <- attach_gps(full_2024)

# the _2 _1 after the coords were initially kept to compare how many points were caught/missed between different stages, no real reason to keep it that way -GV

# KG: Now I assume that the next thing to do is keep the highest gps pair possible?
full_2023_bouts <- full_2023 %>%
  filter(sensor == "ACC") %>%
  mutate(location_long = case_when(!is.na(location_long_2) ~ location_long_2,
                                   is.na(location_long_2) & !is.na(location_long_3) ~ location_long_3,
                                   .default = location_long),
         location_lat = case_when(!is.na(location_lat_2) ~ location_lat_2,
                                   is.na(location_lat_2) & !is.na(location_lat_3) ~ location_lat_3,
                                   .default = location_lat))

full_2024_bouts <- full_2024 %>%
  filter(sensor == "ACC") %>%
  mutate(location_long = case_when(!is.na(location_long_2) ~ location_long_2,
                                   is.na(location_long_2) & !is.na(location_long_3) ~ location_long_3,
                                   .default = location_long),
         location_lat = case_when(!is.na(location_lat_2) ~ location_lat_2,
                                  is.na(location_lat_2) & !is.na(location_lat_3) ~ location_lat_3,
                                  .default = location_lat))

# End matching to GPS data ------------------------------------------------

# Matching to GPS data (Kaija code) ---------------------------------------

# device_ids_2023 <- purrr::map(bouts_predictions_2023, ~.x$device_id[1])
# device_ids_2024 <- purrr::map(bouts_predictions_2024, ~.x$device_id[1])

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
# 
# focal_gps_2023 <- readRDS(here("data/ACC/2023_hf_period/created/focal_gps_2023.RDS"))
# focal_gps_2024 <- readRDS(here("data/ACC/2024_hf_period/created/focal_gps_2024.RDS"))
#
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
# 
# matches_2023 <- readRDS(here("data/ACC/2023_hf_period/created/matches_2023.RDS"))
# matches_2024 <- readRDS(here("data/ACC/2024_hf_period/created/matches_2024.RDS"))
#
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
# 
# joined_2023 <- readRDS(here("data/ACC/2023_hf_period/created/joined_2023.RDS"))
# joined_2024 <- readRDS(here("data/ACC/2024_hf_period/created/joined_2024.RDS"))

### define function to get the feeding bouts
getfeeding <- function(x){
  out <- filter(x, pred == "Eating" & !is.na(location_lat) & .pred_Eating > 0.75)
  out <- sf::st_as_sf(out, coords = c("location_long", "location_lat"),
                      crs = "WGS84", remove = F) %>%
    st_transform(32636)
  return(out)
}

feeding_bouts_certain_2023 <- getfeeding(full_2023_bouts)
feeding_bouts_certain_2024 <- getfeeding(full_2024_bouts)

write_rds(feeding_bouts_certain_2023, here("data/ACC/2023_hf_period/created/feeding_bouts_certain_2023.RDS"))
write_rds(feeding_bouts_certain_2024, here("data/ACC/2024_hf_period/created/feeding_bouts_certain_2024.RDS"))

feeding_bouts_certain_2023 <- readRDS(here("data/ACC/2023_hf_period/created/feeding_bouts_certain_2023.RDS"))
feeding_bouts_certain_2024 <- readRDS(here("data/ACC/2024_hf_period/created/feeding_bouts_certain_2024.RDS"))

bouts23 <- feeding_bouts_certain_2023 %>%
  rename("start" = timestamp) %>%
  mutate(year = 2023,
         start = lubridate::ymd_hms(start),
         end = lubridate::ymd_hms(end),
         dateOnly = lubridate::date(start),
         tag_local_identifier = as.numeric(tag_local_identifier))
bouts24 <- feeding_bouts_certain_2024 %>%
  rename("start" = timestamp) %>%
  mutate(year = 2024,
         start = lubridate::ymd_hms(start),
         end = lubridate::ymd_hms(end),
         dateOnly = lubridate::date(start),
         tag_local_identifier = as.numeric(tag_local_identifier))
feeding_bouts <- bind_rows(bouts23, bouts24) %>%
  sf::st_as_sf(crs = 32636)
feeding_bouts <- feeding_bouts %>%
  bind_cols(st_coordinates(.)) %>%
  rename("device_id" = "tag_local_identifier")
write_rds(feeding_bouts, here("data/created/feeding_bouts.RDS"))
