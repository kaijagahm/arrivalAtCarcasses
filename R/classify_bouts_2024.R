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
calibration_data <- read.csv(here("ACC_algo_Marta_draft/Data/example_calibration.csv"))
# Behavioral classification of unobserved data XXX start here
unobs_fit <- readRDS(here("ACC_algo_Marta_draft/Data/gv_final_model_fit.rda"))

## 1a. Uploading raw ACC data
# files_to_read <- list.files(here("data/ACC/2024_hf_period/raw/"), pattern = ".csv",
#                             full.names = T)
# unobs_raw_acc <- map(files_to_read, ~as.data.frame(data.table::fread(.x),
#                                                    select = c("UTC_datetime", "UTC_date", "UTC_time", "datatype", "device_id", "acc_x", "acc_y", "acc_z"))) %>%
#   purrr::list_rbind()
# test <- unobs_raw_acc %>% group_by(device_id) %>% group_split()
# 
# ## 1b. Split into a list of devices and write back to files
# for(i in 1:length(test)){
#   dev <- test[[i]]$device_id[1]
#   filename <- paste0("/", dev, ".csv")
#   data.table::fwrite(test[[i]], file = paste0(here("data/ACC/2024_hf_period/created/devices"), filename))
# }

# Now, one at a time, read in a file, classify it, write it out.
files <- list.files(here("data/ACC/2024_hf_period/created/devices/"), pattern = ".csv", full.names = T)

for(i in 1:length(files)){
  cat("Working on file", i, "of", length(files), "######################", "\n")
  file <- data.table::fread(files[i])
  prepared <- prepare_dataset(file, calibration = calibration_data)
  cat("Predicting\n")
  predictions <- stats::predict(unobs_fit, prepared)
  scores <- stats::predict(unobs_fit, prepared, type='prob')
  bouts_predictions <- prepared %>%
    ungroup() %>%
    dplyr::select(bout_id, device_id) %>%
    bind_cols(predictions) %>%
    bind_cols(scores) %>%
    rename("pred" = ".pred_class")
  cat("Writing predictions to file\n")
  filename <- paste0("/", bouts_predictions$device_id[1], ".csv")
  data.table::fwrite(bouts_predictions, file = here("data/ACC/2024_hf_period/created/predictions/", filename))
  cat("Done!\n")
}
# Hooray, now all of the 2024 high-frequency ACC data is classified!
# Examine a test file
testfile <- read_csv(here("data/ACC/2024_hf_period/created/predictions/202364.csv"))

