get_loginObject <- function(pw){
  load(pw)
  loginObject <- move::movebankLogin(username = "kaijagahm", password = pw)
  rm(pw)
  return(loginObject)
}
# get_inpa <- function(loginObject){
#   inpa <- move::getMovebankData(study = 6071688, 
#                                 login = loginObject, 
#                                 removeDuplicatedTimestamps = TRUE,
#                                 timestamp_start = "2020010100000",
#                                 timestamp_end = "2021021500000")
#   inpa <- methods::as(inpa, "data.frame")
#   inpa <- inpa %>%
#     mutate(dateOnly = lubridate::ymd(substr(timestamp, 1, 10)),
#            year = as.numeric(lubridate::year(timestamp)))
#   return(inpa)
# }

get_ornitela <- function(loginObject){
  minDate <- "2023-06-10 00:00" # two days after 6/8/23, when tags were set to high res
  maxDate <- "2023-09-09 11:59" # two days before 9/11/23, when tags were returned to normal
  ornitela <- vultureUtils::downloadVultures(loginObject = loginObject, 
                                             removeDup = T, dfConvert = T, 
                                             quiet = T, 
                                             dateTimeStartUTC = minDate, 
                                             dateTimeEndUTC = maxDate)
  return(ornitela)
}

fix_names <- function(ornitela, ww_file){
  ww <- read_excel(ww_file, sheet = "all gps tags")
  # pull out just the names columns, nothing else, and remove any duplicates
  ww_tojoin <- ww %>% dplyr::select(Nili_id, Movebank_id) %>% distinct() 
  
  # Prepare for join: are there any individuals in the `local_identifier` column of `joined0` that don't appear in the `Movebank_id` column of `ww_tojoin`?
  problems <- ornitela %>% filter(!(local_identifier %in% ww_tojoin$Movebank_id)) %>% pull(local_identifier) %>% unique()
  problems #let's check these against the who's who and see if we can make some reasonable changes.
  
  ## Fixes:
  # Typo in the Movebank_id column of the who's who:
  ww_tojoin <- ww_tojoin %>% mutate(Movebank_id = case_when(Movebank_id == "A65 Whiite" ~ "A65 White",
                                                            .default = Movebank_id))
  # Fixes to ornitela:
  ornitela <- ornitela %>%
    mutate(local_identifier = case_when(local_identifier == "E86 White" ~ "E86",
                                        local_identifier == "E88 White" ~ "E88w",
                                        .default = local_identifier))
  
  ## Look for any remaining problems:
  problems <- ornitela %>% filter(!(local_identifier %in% ww_tojoin$Movebank_id)) %>% pull(local_identifier) %>% unique()
  problems #going to fix both of these afterward. E66 isn't listed in the Who's who at all, so we'll just call it "E66" in the Nili_id. The other one, Y01>T60 W, I've manually determined is Nili_id "tammy".
  
  # join by movebank ID
  joined <- left_join(ornitela, ww_tojoin, 
                      by = c("local_identifier" = "Movebank_id"))
  joined <- joined %>%
    mutate(Nili_id = case_when(is.na(Nili_id) & local_identifier == "E66 White" ~ "E66",
                               is.na(Nili_id) & local_identifier == "Y01>T60 W" ~ "tammy",
                               .default = Nili_id))
  
  # Are there any remaining NA's for Nili_id?
  nas <- joined %>% filter(is.na(Nili_id)) %>% pull(local_identifier) %>% unique()
  length(nas) # yay, no more!
  return(joined)
}

remove_periods <- function(ww_file, fixed_names){
  periods_to_remove <- read_excel(ww_file, sheet = "periods_to_remove")
  removed_periods <- vultureUtils::removeInvalidPeriods(dataset = fixed_names, periodsToRemove = periods_to_remove)
  return(removed_periods)
}

clean_data <- function(removed_periods){
  cleaned <- vultureUtils::cleanData(dataset = removed_periods,
                                     precise = F,
                                     longCol = "location_long",
                                     latCol = "location_lat",
                                     idCol = "Nili_id",
                                     report = F)
  return(cleaned)
}

remove_captures <- function(capture_sites, carmel, cleaned){
  cs <- read.csv(capture_sites)
  cml <- read.csv(carmel)
  removed_captures <- vultureUtils::removeCaptures(data = cleaned, 
                                                   captureSites = cs, 
                                                   AllCarmelDates = cml, 
                                                   distance = 500, idCol = "Nili_id")
  return(removed_captures)
}

attach_age_sex <- function(removed_captures, ww_file){
  age_sex <- read_excel(ww_file, sheet = "all gps tags")[,1:35] %>%
    dplyr::select(Nili_id, birth_year, sex) %>%
    distinct()
  
  with_age_sex <- removed_captures %>%
    dplyr::select(-c("sex")) %>%
    left_join(age_sex, by = "Nili_id")
  
  return(with_age_sex)
}

get_geofences <- function(){
  ll_top_left_1 <- c(30.928, 34.961)
  ll_bot_right_1 <- c(30.85, 35.073)
  ll_top_left_2 <- c(31.183, 35.229)
  ll_bot_right_2 <- c(31.116, 35.34)
  rect1 <- c(ll_top_left_1, ll_bot_right_1)
  rect2 <- c(ll_top_left_2, ll_bot_right_2)
  return(list(rect1, rect2))
} 

get_sns <- function(tag_sns_file){
  sns_sheet <- read_excel(tag_sns_file)
  sns <- sns_sheet[[1]]
  return(sns)
}

get_hires_tags <- function(with_age_sex, tag_sns){
  hires_tags <- with_age_sex %>%
    filter(tag_local_identifier %in% tag_sns) %>%
    mutate(timestamp_il = as_datetime(timestamp, tz = "Israel"),
           dateOnly_il = lubridate::date(timestamp_il)) 
  return(hires_tags)
}

################################### Bout classification functions

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
    mutate(slopex = replace_na(slopex, mean(calibration_data$slopex)),
           intx = replace_na(intx, mean(calibration_data$intx)),
           slopey = replace_na(slopey, mean(calibration_data$slopey)),
           inty = replace_na(inty, mean(calibration_data$inty)),
           slopez = replace_na(slopez, mean(calibration_data$slopez)),
           intz = replace_na(intz, mean(calibration_data$intz)))
  transformed <- joined %>%
    mutate(acc_x = (acc_x - intx) * slopex,
           acc_y = (acc_y - inty) * slopey,
           acc_z = (acc_z - intz) * slopez) %>%
    dplyr::select(-c(intx:slopez))
  return(transformed)
}                    

add_bout_ids <- function(x){
  bout_id <- numeric(nrow(x))
  if(nrow(x) > 0){
    j = 0
    
    for(i in 1:nrow(x)) {
      if(x$datatype[i] == "SEN_ACC_20Hz_START") {
        j = j + 1
      }
      bout_id[i] = j
    }
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
  full$start_int <- as.character(full$UTC_datetime)
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

prepare_forloop <- function(x, cal){
  out <- vector(mode = "list", length = length(x))
  for(i in 1:length(x)){
    out[[i]] <- prepare_dataset(x[[i]], calibration = cal)
    cat("done with", i, "\n")
  }
  return(out)
}

prepare_parallel <- function(x, cal){
  future::plan(future::multisession(workers = 10))
  out <- furrr::future_map(x, ~suppressWarnings(prepare_dataset(.x, calibration = cal)),
                           .progress = T)
  return(out)
}

get_bouts_predictions <- function(prepared, predictions, 
                                  scores, bouts){
  prepared %>%
    dplyr::ungroup() %>%
    dplyr::select(bout_id, device_id) %>%
    dplyr::bind_cols(predictions) %>%
    dplyr::bind_cols(scores) %>%
    dplyr::rename("pred" = ".pred_class") %>%
    dplyr::left_join(bouts, by = c("device_id", "bout_id"))
}

# get_matches <- function(df, foc){
#   with_middles <- df %>%
#     mutate(start = lubridate::ymd_hms(start),
#            end = lubridate::ymd_hms(end),
#            middle = start + difftime(end, start)/2) %>%
#     group_by(bout_id) %>%
#     group_split()
#   
#   within_5min <- map(with_middles, ~{
#     foc[(.x$start[1] - minutes(5)) <= foc$timestamp & foc$timestamp <= (.x$end[1] + minutes(5)),] 
#   }, .progress = T)
#   
#   within_5min_speed <- map(within_5min, ~.x[.x$ground_speed <= 4,])
#   
#   within_11min_speed <- map(with_middles, ~{
#     foc[(.x$start[1] - minutes(11)) <= foc$timestamp & foc$timestamp <= (.x$end[1] + minutes(11)) & foc$ground_speed < 4,] 
#   }, .progress = T)
#   
#   keep <- purrr::pmap(list(within_5min, within_5min_speed, within_11min_speed, with_middles), ~{
#     if(nrow(..2) > 0){
#       match <- ..2
#     }else if(nrow(..3) > 0){
#       match <- ..3
#     }else if(nrow(..1) > 0){
#       match <- ..1
#     }else{
#       match <- foc[0,]
#     }
#     if(nrow(match) > 1){
#       match <- match[which.min(abs(as.numeric(match$timestamp - ..4$middle[1]))),]
#     }
#     if(nrow(match) > 0){
#       match$bout_id <- ..4$bout_id[1]
#       return(match)
#     }else{
#       return(NULL)
#     }
#   })
#   keep_df <- purrr::list_rbind(keep)
#   return(keep_df)
# }

assign_fs <- function(data, fs){
  data$station <- !is.na(as.numeric(sf::st_intersects(data, fs)))
  return(data)
}

split_data_fun_forloop <- function(data){
  devices <- unique(data$device_id)
  out <- vector(mode = "list", length = length(devices))
  for(i in 1:length(devices)){
    cat("Starting", i, "\n")
    out[[i]] <- data[data$device_id == devices[i],]
  }
  return(out)
}

get_focal <- function(carcasses, times){
  focal <- carcasses %>% 
    filter(datetime >= times[1],
           datetime <= times[2]) %>%
    bind_rows(carcasses %>%
                filter(datetime >= times[3],
                       datetime <= times[4])) %>%
    filter(!cage) %>% # remove carcasses placed in cages 
    dplyr::select(-c("color", "commentsKaija", "investigateKaija", "questionForGideon", "reassign_to", "todo", "interpretation", "flag"))
  return(focal) 
}

get_focal2 <- function(carcasses, times){
  focal <- carcasses %>% 
    filter(dateOnly >= times[1],
           dateOnly <= times[2]) %>%
    bind_rows(carcasses %>%
                filter(dateOnly >= times[3],
                       dateOnly <= times[4]))
  return(focal)
}

get_carcass_bouts <- function(bouts, carcasses, dist, hours_before, hours_after){
  carcass_bouts <- map(1:nrow(carcasses), ~{
    carcass <- carcasses[.x,]
    id <- carcasses$carcID[.x]
    distances <- as.numeric(sf::st_distance(bouts, carcass))
    bouts <- bouts %>%
      mutate(carcID = id,
             dist_to_carcass = distances)
    keep_distance <- bouts %>%
      filter(dist_to_carcass <= dist)
    keep_time <- keep_distance %>%
      filter(start >= (carcass$datetime - hours(hours_before)), 
             end <= (carcass$datetime + hours(hours_after))) %>% 
      mutate(time_since_carcass = difftime(start, carcass$datetime, units = "hours"))
    return(keep_time)
  })
  return(carcass_bouts)
}


# Clustering --------------------------------------------------------------
cluster_carcasses <- function(carcasses, dist){
  buffered <- sf::st_buffer(carcasses, dist)
  parts <- sf::st_cast(st_union(buffered), "POLYGON")
  clust <- unlist(sf::st_intersects(buffered, parts))
  diss <- cbind(buffered, clust)
  
  cluster_centroids <- diss %>%
    group_by(clust) %>%
    summarize(geometry = sf::st_union(geometry)) %>%
    sf::st_centroid() %>%
    ungroup() %>%
    bind_cols(sf::st_coordinates(.))
  return(cluster_centroids)
}

get_wild_carcass_bouts <- function(remaining_bouts, time = '24 hours', dist = 100, minBouts = 3, stations, stationDist = 750){
  # Remove any that are within a certain distance of a known station
  stations_buffered <- st_buffer(stations, stationDist) %>%
    st_union()
  tokeep <- map_dbl(st_intersects(remaining_bouts, stations_buffered), length) == 0 # keep the ones that don't intersect with any feeding station buffer areas
  remaining_bouts <- remaining_bouts[tokeep,]
  
  # Format appropriately for spatsoc
  remaining_bouts$timestamp <- as.POSIXct(remaining_bouts$start)
  remaining_bouts <- data.table::data.table(remaining_bouts)
  
  spatsoc::group_times(remaining_bouts, 
                       datetime = 'timestamp', 
                       threshold = time)
  spatsoc::group_pts(remaining_bouts, threshold = dist, 
                     id ='boutID', coords = c('X', 'Y'), 
                     timegroup = 'timegroup')
  
  # Restrict to groups that have at least 3 bouts and at least 2 individuals
  remaining_bouts <- remaining_bouts %>%
    group_by(group) %>%
    filter(n() >= minBouts,
           length(unique(individualID)) > 1)
  
  # convert back to sf object for mapping
  wild_carcass_bouts_df <- as.data.frame(remaining_bouts) %>%
    rename("carcID" = group) %>%
    sf::st_as_sf(crs = 32636)
  
  return(wild_carcass_bouts_df)
}

get_wild_carcasses <- function(wild_carcass_bouts_df){
  # Get carcasses
  wild_carcasses <- wild_carcass_bouts_df %>%
    group_by(year, carcID) %>%
    summarize(geometry = sf::st_union(geometry),
              dateOnly = dateOnly[1],
              nBouts = n(),
              nIndivs = length(unique(individualID))) %>%
    sf::st_centroid() %>%
    ungroup() %>%
    bind_cols(sf::st_coordinates(.)) 
  return(wild_carcasses)
}
# "Limitations of threshold
# The threshold of group_times is considered only within the scope of 24 hours and this poses limitations on it:
# 
# threshold must evenly divide into 60 minutes or 24 hours
# multi-day blocks are consistent across years and timegroups from these are by year.
# number of minutes cannot exceed 60
# threshold cannot be fractional"


# Shortcuts ---------------------------------------------------------------
dg <- function(x){
  return(sf::st_drop_geometry(x))
}
