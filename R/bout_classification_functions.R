# Bout classification functions

mean_amplitude <- function(x) {
  extreme_points <- which(abs(diff(sign(diff(x)))) == 2) + 1 
  mean(abs(x[extreme_points[-1]] - x[extreme_points[-length(extreme_points)]]))
}

transform_datetimes <- function(x){
  x %>%
    mutate(UTC_datetime = lubridate::ymd_hms(UTC_datetime,
                                             tz = "UTC"))
  return(x)
}

remove_gps_rows <- function(x){
  out <- subset(x, !(datatype %in% c("GPS", "GPSS")))
  return(out)
}

calibrate <- function(x, calibration_data){
  joined <- left_join(x, calibration_data, by = "device_id")
  
  # If there are no calibration values of a tag, add the mean value for all tags
  joined <- joined %>% 
    mutate(slopex = ifelse(is.na(slopex), mean(calibration$slopex), slopex),
           intx = ifelse(is.na(intx), mean(calibration$intx), intx),
           slopey = ifelse(is.na(slopey), mean(calibration$slopey), slopey),
           inty = ifelse(is.na(inty), mean(calibration$inty), inty),
           slopez = ifelse(is.na(slopez), mean(calibration$slopez), slopez),
           intz = ifelse(is.na(intz), mean(calibration$intz), intz))
  transformed <- joined %>%
    mutate(acc_x = (acc_x - intx) * slopex,
           acc_y = (acc_y - inty) * slopey,
           acc_z = (acc_z - intz) * slopez) %>%
    dplyr::select(-c(intx:slopez))
  return(transformed)
}                    

add_bout_ids <- function(x){
  bout_id <- numeric(nrow(x))
  j = 0
  
  for(i in 1:nrow(x)) {
    if(x$datatype[i] == "SEN_ACC_20Hz_START") {
      j = j + 1
    }
    bout_id[i] = j
  }
  
  x <- x %>% 
    add_column(bout_id, .before = 1)
  return(x)
}

exclude_incomplete_bouts <- function(x, bout_duration = 5, acc_frequency = 20){
  bout_length <- bout_duration * acc_frequency
  
  x <- x %>%
    add_count(bout_id) %>%
    filter(n == bout_length) %>%
    dplyr::select(-n)
  return(x)
}  

get_stat_feats <- function(x){
  out <- x %>%
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
  return(out)
}

prepare_full_dataset <- function(x, stat_feats, bout_duration = 5, acc_frequency = 20){
  bout_length <- bout_duration * acc_frequency
  full <- x %>%
    add_column(idx = rep(1:bout_length, nrow(x)/100)) %>%
    dplyr::select(bout_id, idx, device_id, acc_x, acc_y, acc_z) %>%
    pivot_wider(names_from = idx, values_from = c(acc_x, acc_y, acc_z)) 
  
  full <- left_join(full, 
                    x[, c("bout_id", "device_id", "UTC_datetime")],
                    by = c("device_id", "bout_id"))
  
  full <- left_join(full, 
                    stat_feats,
                    by = c("device_id", "bout_id"))
  return(full)
}

remove_bad_bouts <- function(x){
  filtered <- x %>%
    filter(!is.na(skewness_x))
  return(filtered)
}

## Composite function for preparing the data for classification
prepare_dataset <- function(x, calibration){
  cat("Transforming datetimes\n")
  x <- transform_datetimes(x)
  cat("Removing GPS rows\n")
  x <- remove_gps_rows(x)
  cat("Calibrating\n")
  x <- calibrate(x, calibration_data = calibration)
  cat("Adding bout IDs\n")
  x <- add_bout_ids(x)
  cat("Excluding incomplete bouts\n")
  x <- exclude_incomplete_bouts(x)
  cat("Calculating statistical features\n")
  stat_feats <- get_stat_feats(x)
  cat("Preparing full dataset\n")
  full <- prepare_full_dataset(x, stat_feats = stat_feats)
  cat("Removing bad bouts\n")
  full <- remove_bad_bouts(full)
  rm(x)
  rm(stat_feats)
  return(full)
  rm(full)
}
