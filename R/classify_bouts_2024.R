# Classify bouts for 2024
library(tidyverse)
library(moments)
library(tidymodels)
library(ranger)
library(parsnip)
library(caret)
library(zoo)
library(readxl)
library(here)
library(data.table)
library(future)
library(furrr)

source(here("R/bout_classification_functions.R"))

## 1a. Uploading raw ACC data
unobs_raw_acc <- as.data.frame(data.table::fread(here("data/ACC/2024_hf_period/raw/Multiselect_20240506_205100.csv"), select = c("UTC_datetime", "UTC_date", "UTC_time", "datatype", "device_id", "acc_x", "acc_y", "acc_z")))

## 1b. Split into a list so we can parallelize the rest
unobs_raw_acc_list <- unobs_raw_acc %>%
  group_by(device_id) %>%
  group_split()

## 1c. Set up plan for running things in parallel
future::plan(future::multisession(workers = 10))

unobs_raw_acc_list <- furrr::future_map(unobs_raw_acc_list, transform_datetimes,
  .progress = T) # this is a very slow step. Not entirely sure why.

# If relevant, remove any rows that do not contain ACC data (for example, rows that have only GPS data)
unobs_raw_acc_list <- furrr::future_map(unobs_raw_acc_list, remove_gps_rows, .progress = T)

## 3. Transform raw ACC into acceleration values

# Add calibration file 
calibration <- read.csv(here("ACC_algo_Marta_draft/Data/example_calibration.csv"))

unobs_raw_acc_list <- furrr::future_map(unobs_raw_acc_list, ~calibrate(.x, calibration_data = calibration), .progress = T)

## 4. Identify distinct bouts
# ---- 1) The device identifies the start of the bout -----
unobs_raw_acc_list <- furrr::future_map(unobs_raw_acc_list, add_bout_ids, .progress = T)

## Exclude incomplete bouts
unobs_raw_acc_list <- furrr::future_map(unobs_raw_acc_list, exclude_incomplete_bouts, .progress = T)

# 6. Extract statistical features
stat_feats_list <- furrr::future_map(unobs_raw_acc_list, get_stat_feats, .progress = T)

### This is giving a warning that will turn into an error later when we run the model. Therefore, I'm going to filter out any bouts from the unobs_data that have missing data, so the model will be able to run.

## Prepare dataset before model training sequence
full_unobs_data <- furrr::future_map2(unobs_raw_acc_list, stat_feats_list, 
                                      ~prepare_full_dataset(.x, stat_feats = .y), 
                                      .progress = T) # this is a really slow step too.
gc()

## doing the filtering here: filter out those bad bouts with sd = 0. This will allow us to run the model on this data.
full_unobs_data <- furrr::future_map(full_unobs_data, remove_bad_bouts, .progress = T)

# Behavioral classification of unobserved data XXX start here
unobs_fit <- readRDS(here("ACC_algo_Marta_draft/Data/gv_final_model_fit.rda"))

# Separate - another chunk
full_unobs_data$start_int <- as.character(full_unobs_data$UTC_datetime)

# predictions <-  predict(unobs_fit, full_unobs_data) # this also takes forever!
# save(predictions, file = here("data/ACC/2024_hf_period/created/predictions.Rda"))
load(here("data/ACC/2024_hf_period/created/predictions.Rda")) # okay, this is all well and good, but we also need to attach this to the bouts

bouts_predictions <- full_unobs_data %>%
  dplyr::select(bout_id, device_id) %>%
  bind_cols(predictions) %>%
  rename("pred" = ".pred_class")

write_csv(bouts_predictions, file = here("data/ACC/2024_hf_period/created/bouts_predictions.csv"))

# Separate - another chunk
# Calculate a confidence score for each prediction
scores <- predict(unobs_fit, full_unobs_data, type='prob')
save(scores, file = here("data/ACC/2024_hf_period/created/scores.Rda"))
load(here("data/ACC/2024_hf_period/created/scores.Rda"))

# That was remarkably easy!