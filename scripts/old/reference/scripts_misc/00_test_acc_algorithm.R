# Copying over taking chunks from script 2 - using_rf_model.Rmd ---------------
# Using the existing random forest model
library(tidyverse)
library(moments)
library(tidymodels)
library(ranger)
library(parsnip)
library(caret)
library(zoo)

mean_amplitude <- function(x) {
  extreme_points <- which(abs(diff(sign(diff(x)))) == 2) + 1 
  mean(abs(x[extreme_points[-1]] - x[extreme_points[-length(extreme_points)]]))
}

## 1. Uploading raw ACC data
unobs_raw_acc <- list.files(path = "data/raw/ACC/samples/", 
                            pattern = "*.csv", full.names = T) %>%
  map_df(~read.csv(.)) %>%
  mutate(UTC_datetime = as.POSIXct(UTC_datetime, 
                                   format = c("%Y-%m-%d %H:%M:%S"), 
                                   tz = "UTC"))
# If relevant, remove any rows that do not contain ACC data (for example, rows that have only GPS data)
unobs_raw_acc <- subset(unobs_raw_acc, !(datatype %in% c("GPS", "GPSS")))

## 3. Transform raw ACC into acceleration values

# Add calibration file 
calibration <- read.csv("ACC_algo_Marta_draft/Data/example_calibration.csv") 

unobs_raw_acc <- left_join(unobs_raw_acc, calibration, by = "device_id")

# If there are no calibration values of a tag, add the mean value for all tags
unobs_raw_acc <- unobs_raw_acc %>% 
  mutate(slopex = ifelse(is.na(slopex), mean(calibration$slopex), slopex),
         intx = ifelse(is.na(intx), mean(calibration$intx), intx),
         slopey = ifelse(is.na(slopey), mean(calibration$slopey), slopey),
         inty = ifelse(is.na(inty), mean(calibration$inty), inty),
         slopez = ifelse(is.na(slopez), mean(calibration$slopez), slopez),
         intz = ifelse(is.na(intz), mean(calibration$intz), intz))

# Transform raw ACC data into acceleration values
unobs_raw_acc <- unobs_raw_acc %>%
  mutate(acc_x = (acc_x - intx) * slopex,
         acc_y = (acc_y - inty) * slopey,
         acc_z = (acc_z - intz) * slopez) %>%
  dplyr::select(-c(intx:slopez))

## 4. Identify distinct bouts
# ---- 1) The device identifies the start of the bout -----
bout_id <- numeric(nrow(unobs_raw_acc))

j = 0

for(i in 1:nrow(unobs_raw_acc)) {
  if(unobs_raw_acc$datatype[i] == "SEN_ACC_20Hz_START") {
    j = j + 1
  }
  bout_id[i] = j
}

unobs_raw_acc <- unobs_raw_acc %>% 
  add_column(bout_id, .before = 1)

## Exclude incomplete bouts
## calculate bout duration to make sure
durs <- unobs_raw_acc %>%
  dplyr::select(bout_id, device_id, UTC_datetime) %>%
  group_by(device_id, bout_id) %>%
  summarize(beg = min(UTC_datetime),
            end = max(UTC_datetime),
            dur = end-beg,
            hz = n()/as.numeric(dur))
hist(as.numeric(durs$dur)) # looks like there are some 20-second bouts, but it's mostly every 5 seconds
hist(as.numeric(durs$hz)) # hmm, some are less frequent than 20Hz. Need to ask Marta what to do if I'm not sure, or what will happen if it deviates. For now, I'm going to assume that indeed they abide by these numbers.
  
bout_duration <- 5 # add here your relevant bout duration, in seconds
acc_frequency <- 20 # add here the frequency of ACC collection, in Hz

bout_length <- bout_duration * acc_frequency

unobs_raw_acc <- unobs_raw_acc %>%
  add_count(bout_id) %>%
  filter(n == bout_length) %>%
  dplyr::select(-n)

## 6. Extract statistical features
stat_feats <- unobs_raw_acc %>%
  group_by(device_id, bout_id) %>%
  summarise(mean_x = mean(acc_x),
            mean_y = mean(acc_y),
            mean_z = mean(acc_z),
            range_x = max(acc_x)-min(acc_x),
            range_y = max(acc_y)-min(acc_y),
            range_z = max(acc_z)-min(acc_z),
            sd_x = sd(acc_x),
            sd_y = sd(acc_y),
            sd_z = sd(acc_z),
            skewness_x = skewness(acc_x),
            skewness_y = skewness(acc_y),
            skewness_z = skewness(acc_z),
            kurtosis_x = kurtosis(acc_x),
            kurtosis_y = kurtosis(acc_y),
            kurtosis_z = kurtosis(acc_z),
            max_x = max(acc_x),
            max_y = max(acc_y),
            max_z = max(acc_z),
            min_x = min(acc_x),
            min_y = min(acc_y),
            min_z = min(acc_z),
            norm_x = sqrt(sum(acc_x^2)),
            norm_y = sqrt(sum(acc_y^2)),
            norm_z = sqrt(sum(acc_z^2)),
            q25_x = quantile(acc_x, probs = 0.25),
            q25_y = quantile(acc_y, probs = 0.25),
            q25_z = quantile(acc_z, probs = 0.25),
            q50_x = quantile(acc_x, probs = 0.50),
            q50_y = quantile(acc_y, probs = 0.50),
            q50_z = quantile(acc_z, probs = 0.50),
            q75_x = quantile(acc_x, probs = 0.75),
            q75_y = quantile(acc_y, probs = 0.75),
            q75_z = quantile(acc_z, probs = 0.75),
            cov_x_y = cov(acc_x, acc_y),
            cov_x_z = cov(acc_x, acc_z), 
            cov_y_z = cov(acc_y, acc_z),
            cor_x_y = cor(acc_x, acc_y),
            cor_x_z = cor(acc_x, acc_z),
            cor_y_z = cor(acc_y, acc_z),
            mean_diff_x_y = mean(acc_x-acc_y),
            mean_diff_x_z = mean(acc_x-acc_z),
            mean_diff_y_z = mean(acc_y-acc_z),
            sd_diff_x_y = sd(acc_x-acc_y),
            sd_diff_x_z = sd(acc_x-acc_z),
            sd_diff_y_z = sd(acc_y-acc_z),
            mean_amplitude_x = mean_amplitude(acc_x),
            mean_amplitude_y = mean_amplitude(acc_y),
            mean_amplitude_z = mean_amplitude(acc_z)) %>%
  ungroup()

### This is giving a warning that will turn into an error later when we run the model. Therefore, I'm going to filter out any bouts from the unobs_data that have missing data, so the model will be able to run.

## Prepare dataset before model training sequence
full_unobs_data <- unobs_raw_acc %>%
  add_column(idx = rep(1:bout_length, nrow(unobs_raw_acc)/100)) %>%
  dplyr::select(bout_id, idx, device_id, acc_x, acc_y, acc_z) %>%
  pivot_wider(names_from = idx, values_from = c(acc_x, acc_y, acc_z)) 

full_unobs_data <- left_join(full_unobs_data, 
                             unobs_raw_acc[, c("bout_id", "device_id", "UTC_datetime")],
                             by = c("device_id", "bout_id"))

full_unobs_data <- left_join(full_unobs_data, 
                             stat_feats,
                             by = c("device_id", "bout_id"))

full_unobs_data <- full_unobs_data %>%
  filter(!is.na(skewness_x))

# Behavioral classification of unobserved data
unobs_fit <- readRDS("ACC_algo_Marta_draft/Data/gv_final_model_fit.rda")

# Separate - another chunk
full_unobs_data$start_int <- as.character(full_unobs_data$UTC_datetime)

predictions <-  predict(unobs_fit, full_unobs_data)

# Separate - another chunk
# Calculate a confidence score for each prediction
scores <- predict(unobs_fit, full_unobs_data, type='prob')

# That was remarkably easy!

# Test with non-high-frequency ACC ----------------------------------------
# Going to download 5 days for one individual in January 2024 and see how many ACC bouts we can recover.
## 1. Uploading raw ACC data
lf <- read.csv(here("data/raw/ACC/samples/202383_20240828_192300.csv")) %>%
  mutate(UTC_datetime = as.POSIXct(UTC_datetime, 
                                   format = c("%Y-%m-%d %H:%M:%S"), 
                                   tz = "UTC"))
# If relevant, remove any rows that do not contain ACC data (for example, rows that have only GPS data)
lf <- subset(lf, !(datatype %in% c("GPS", "GPSS")))

## 3. Transform raw ACC into acceleration values

# Add calibration file 
calibration <- read.csv("ACC_algo_Marta_draft/Data/example_calibration.csv") 

lf <- left_join(lf, calibration, by = "device_id")

# If there are no calibration values of a tag, add the mean value for all tags
lf <- lf %>% 
  mutate(slopex = ifelse(is.na(slopex), mean(calibration$slopex), slopex),
         intx = ifelse(is.na(intx), mean(calibration$intx), intx),
         slopey = ifelse(is.na(slopey), mean(calibration$slopey), slopey),
         inty = ifelse(is.na(inty), mean(calibration$inty), inty),
         slopez = ifelse(is.na(slopez), mean(calibration$slopez), slopez),
         intz = ifelse(is.na(intz), mean(calibration$intz), intz))

# Transform raw ACC data into acceleration values
lf <- lf %>%
  mutate(acc_x = (acc_x - intx) * slopex,
         acc_y = (acc_y - inty) * slopey,
         acc_z = (acc_z - intz) * slopez) %>%
  dplyr::select(-c(intx:slopez))

## 4. Identify distinct bouts
# ---- 1) The device identifies the start of the bout -----
bout_id <- numeric(nrow(lf))

j = 0

for(i in 1:nrow(lf)) {
  if(lf$datatype[i] == "SEN_ACC_20Hz_START") {
    j = j + 1
  }
  bout_id[i] = j
}

lf <- lf %>% 
  add_column(bout_id, .before = 1)

## Exclude incomplete bouts
## calculate bout duration to make sure
durs <- lf %>%
  dplyr::select(bout_id, device_id, UTC_datetime) %>%
  group_by(device_id, bout_id) %>%
  summarize(beg = min(UTC_datetime),
            end = max(UTC_datetime),
            dur = end-beg,
            hz = n()/as.numeric(dur))
hist(as.numeric(durs$dur)) # looks like there are some 20-second bouts, but it's mostly every 5 seconds
hist(as.numeric(durs$hz)) # hmm, some are less frequent than 20Hz. Need to ask Marta what to do if I'm not sure, or what will happen if it deviates. For now, I'm going to assume that indeed they abide by these numbers.

bout_duration <- 5 # add here your relevant bout duration, in seconds
acc_frequency <- 20 # add here the frequency of ACC collection, in Hz

bout_length <- bout_duration * acc_frequency

lf <- lf %>%
  add_count(bout_id) %>%
  filter(n == bout_length) %>%
  dplyr::select(-n)

## 6. Extract statistical features
stat_feats <- lf %>%
  group_by(device_id, bout_id) %>%
  summarise(mean_x = mean(acc_x),
            mean_y = mean(acc_y),
            mean_z = mean(acc_z),
            range_x = max(acc_x)-min(acc_x),
            range_y = max(acc_y)-min(acc_y),
            range_z = max(acc_z)-min(acc_z),
            sd_x = sd(acc_x),
            sd_y = sd(acc_y),
            sd_z = sd(acc_z),
            skewness_x = skewness(acc_x),
            skewness_y = skewness(acc_y),
            skewness_z = skewness(acc_z),
            kurtosis_x = kurtosis(acc_x),
            kurtosis_y = kurtosis(acc_y),
            kurtosis_z = kurtosis(acc_z),
            max_x = max(acc_x),
            max_y = max(acc_y),
            max_z = max(acc_z),
            min_x = min(acc_x),
            min_y = min(acc_y),
            min_z = min(acc_z),
            norm_x = sqrt(sum(acc_x^2)),
            norm_y = sqrt(sum(acc_y^2)),
            norm_z = sqrt(sum(acc_z^2)),
            q25_x = quantile(acc_x, probs = 0.25),
            q25_y = quantile(acc_y, probs = 0.25),
            q25_z = quantile(acc_z, probs = 0.25),
            q50_x = quantile(acc_x, probs = 0.50),
            q50_y = quantile(acc_y, probs = 0.50),
            q50_z = quantile(acc_z, probs = 0.50),
            q75_x = quantile(acc_x, probs = 0.75),
            q75_y = quantile(acc_y, probs = 0.75),
            q75_z = quantile(acc_z, probs = 0.75),
            cov_x_y = cov(acc_x, acc_y),
            cov_x_z = cov(acc_x, acc_z), 
            cov_y_z = cov(acc_y, acc_z),
            cor_x_y = cor(acc_x, acc_y),
            cor_x_z = cor(acc_x, acc_z),
            cor_y_z = cor(acc_y, acc_z),
            mean_diff_x_y = mean(acc_x-acc_y),
            mean_diff_x_z = mean(acc_x-acc_z),
            mean_diff_y_z = mean(acc_y-acc_z),
            sd_diff_x_y = sd(acc_x-acc_y),
            sd_diff_x_z = sd(acc_x-acc_z),
            sd_diff_y_z = sd(acc_y-acc_z),
            mean_amplitude_x = mean_amplitude(acc_x),
            mean_amplitude_y = mean_amplitude(acc_y),
            mean_amplitude_z = mean_amplitude(acc_z)) %>%
  ungroup()

### This is giving a warning that will turn into an error later when we run the model. Therefore, I'm going to filter out any bouts from the unobs_data that have missing data, so the model will be able to run.

## Prepare dataset before model training sequence
full <- lf %>%
  add_column(idx = rep(1:bout_length, nrow(lf)/100)) %>%
  dplyr::select(bout_id, idx, device_id, acc_x, acc_y, acc_z) %>%
  pivot_wider(names_from = idx, values_from = c(acc_x, acc_y, acc_z)) 

full <- left_join(full, lf[, c("bout_id", "device_id", "UTC_datetime")],
                             by = c("device_id", "bout_id"))

full <- left_join(full, stat_feats,
                             by = c("device_id", "bout_id"))

full <- full %>%
  filter(!is.na(skewness_x))

# Behavioral classification of unobserved data
unobs_fit <- readRDS("ACC_algo_Marta_draft/Data/gv_final_model_fit.rda")

# Separate - another chunk
full$start_int <- as.character(full$UTC_datetime)

predictions <-  predict(unobs_fit, full)

# Separate - another chunk
# Calculate a confidence score for each prediction
scores <- predict(unobs_fit, full, type='prob')

# Okay that was also easy, and we do still see bouts happening during the low-frequency period.
# Time to go through and download and classify all of the data since the beginning of time?