get_acc_data <- function(data_files){
  out <- purrr::list_rbind(purrr::map(data_files, ~as.data.frame(data.table::fread(.x, select = c("Latitude", "Longitude", "UTC_datetime", "UTC_date", "UTC_time", "datatype", "device_id", "acc_x", "acc_y", "acc_z"))))) %>% filter(!is.na(datatype))
  return(out)
}

flip_devices <- function(unobs_raw_acc){
  toflip_y <- unobs_raw_acc %>%
    group_by(device_id) %>%
    summarize(mny = mean(acc_y)) %>%
    filter(mny < 0) %>%
    pull(device_id)
  if(length(toflip_y) >0){
    out <- unobs_raw_acc %>%
      mutate(acc_y = case_when(device_id %in% toflip_y ~ -1*acc_y,
                               .default = acc_y))
  }else{
    out <- unobs_raw_acc
  }
  return(out)
}

caldev <- function(splitup, calibration_data){
  prepared <- map(splitup, ~prepare_dataset(.x, calibration = calibration_data))
  return(prepared)
}

get_bo <- function(single_device){
  out <- single_device[,c("bout_id", "device_id", "start_int")] %>%
    group_by(device_id, bout_id) %>%
    summarize(start = min(start_int),
              end = max(start_int),
              .groups = "drop")
  return(out)
}

gpfs <- function(scores){
  if(!is.null(scores)){
    out <- apply(scores, 1, which.max)
    out_vals <- names(scores)[out]
    preds <- str_remove(out_vals, ".pred_")
    return(tibble(".pred_class" = preds))
  }else{
    return(NULL)
  }
}

get_sc <- function(single_device, mod){
  if(nrow(single_device) > 0){
    single_device <- as.data.frame(single_device)
    single_device$start_int <- as.character(single_device$start_int)
    out <- predict(mod, single_device, type = "prob")
  }else{
    out <- NULL
  }  
  return(out)
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
              skewness_x = moments::skewness(acc_x),
              skewness_y = moments::skewness(acc_y),
              skewness_z = moments::skewness(acc_z),
              kurtosis_x = moments::kurtosis(acc_x),
              kurtosis_y = moments::kurtosis(acc_y),
              kurtosis_z = moments::kurtosis(acc_z),
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
  # deduplicate--kg addition 6/2/25
  rm(x)
  rm(stat_feats)
  return(full)
  rm(full)
}

gbp <- function(prepared, predictions, 
                scores, bouts){
  if(nrow(prepared) > 0){
    out <- prepared %>%
      dplyr::ungroup() %>%
      dplyr::select(bout_id, device_id) %>%
      dplyr::bind_cols(predictions) %>%
      dplyr::bind_cols(scores) %>%
      dplyr::rename("pred" = ".pred_class") %>%
      dplyr::left_join(bouts, by = c("device_id", "bout_id")) %>%
      distinct()
  }else{
    out <- NULL
  }
  return(out)
}

get_matches <- function(df, foc, spd){
  if(!is.null(df)){
    with_middles <- df %>%
      dplyr::mutate(middle = start + difftime(end, start)/2,
                    fivebefore = start - lubridate::minutes(5),
                    fiveafter = start + lubridate::minutes(5)#,
                    # elevenbefore = start - lubridate::minutes(11),
                    # elevenafter = start + lubridate::minutes(11)
                    ) %>%
      dplyr::group_split(bout_id)
    
    within_5min <- purrr::map(with_middles, ~{
      foc[.x$fivebefore <= foc$timestamp & foc$timestamp <= .x$fiveafter,]
    })
    
    within_5min_speed <- purrr::map(within_5min, ~return(.x[.x$ground_speed <= spd,]))
    
    # within_11min_speed <- purrr::map(with_middles, ~{
    #   foc[.x$elevenbefore <= foc$timestamp & foc$timestamp <= .x$elevenafter & foc$ground_speed < spd,]
    # })
    
    keep <- vector(mode = "list", length = length(with_middles))
    for(i in 1:length(keep)){
      w5 <- within_5min[[i]]
      w5s <- within_5min_speed[[i]]
      #w11s <- within_11min_speed[[i]]
      wm <- with_middles[[i]]
      if(nrow(w5s) > 0){ # if there are any non-flying points within 5 mins, keep them
        match <- w5s
        }else if(nrow(w5) > 0){ # else, if there are ANY points within 5 mins, keep them.
          match <- w5
        }else{ # if none of those is true, return a 0-row data frame
          match <- foc[0,]
        }
      if(nrow(match) > 1){
        match <- match[which.min(abs(as.numeric(match$timestamp - wm$middle[1]))),] # if more than one match, take the closest to the middle time (either before or after)
      }
      if(nrow(match) > 0){ # for all bouts where we got any gps match at all...
        match$bout_id <- wm$bout_id[1] 
        keep[[i]] <- match
      }else{
        match <- as.data.frame(foc[0,])
        match$bout_id <- numeric(0)
        keep[[i]] <- match}
    }
  }else{
    keep <- data.frame(tag_local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, location_lat = NA, location_long = NA, individual_id = NA, tag_local_identifier = NA, bout_id = NA)
    keep <- list(keep[0,])
  }
  keep_df <- purrr::list_rbind(keep)
  if(nrow(keep_df) == 0){
    keep <- data.frame(tag_local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, location_lat = NA, location_long = NA, individual_id = NA, tag_local_identifier = NA, bout_id = NA)
    keep <- list(keep[0,])
    keep_df <- purrr::list_rbind(keep)
  }
  return(keep_df)
}

get_focal <- function(carcasses, times){
  focal <- carcasses %>% 
    filter(datetime >= times[[1]],
           datetime <= times[[2]]) %>%
    bind_rows(carcasses %>%
                filter(datetime >= times[[3]],
                       datetime <= times[[4]])) %>%
    bind_rows(carcasses %>%
                filter(datetime >= times[[5]],
                       datetime <= times[[6]])) %>%
    filter(!cage) %>% # remove carcasses placed in cages 
    dplyr::select(-c("color", "commentsKaija", "investigateKaija", "questionForGideon", "reassign_to", "todo", "interpretation", "flag"))
  return(focal) 
}

get_carcass_bouts <- function(bouts, carcasses, dist, hours_after){
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
      filter(end <= (carcass$datetime + hours(hours_after))) %>% 
      mutate(time_since_carcass = difftime(start, carcass$datetime, units = "hours"))
    return(keep_time)
  })
  return(carcass_bouts)
}

get_bout_stats <- function(carcasses_focal, carcass_bouts_df){
  stats <- carcass_bouts_df %>%
    dplyr::select(carcID, boutID, individual_id) %>%
    dplyr::group_by(carcID) %>% 
    dplyr::summarize(nBouts = length(unique(boutID)), nIndivs = length(unique(individual_id))) %>%
    dplyr::ungroup()
  out <- dplyr::left_join(carcasses_focal, stats, by = "carcID")
  return(out)
}

# Clustering --------------------------------------------------------------
get_wild_carcass_bouts <- function(non_carcass_bouts, time, dst, minBouts, stations, stationDist, minIndivs){
  # Remove any that are too close to a known station
  stations_buffered <- st_buffer(stations, stationDist)
  ncb <- sf::st_as_sf(non_carcass_bouts, crs = 32636) %>% bind_cols(st_coordinates(.))
  tokeep <- map_dbl(st_intersects(ncb, stations_buffered), length) == 0 # keep the ones that don't intersect with any feeding station buffer areas
  non_carcass_bouts <- non_carcass_bouts[tokeep,]
  
  # Format appropriately for spatsoc
  ncb$timestamp <- as.POSIXct(ncb$start, tz = "UTC")
  ncb <- data.table::data.table(ncb)
  
  spatsoc::group_times(ncb, 
                       datetime = 'timestamp', 
                       threshold = time)
  spatsoc::group_pts(ncb, threshold = dst, 
                     id ='boutID', coords = c('X', 'Y'), 
                     timegroup = 'timegroup')
  
  # Restrict to groups that have at least 3 bouts and at least 3 individuals
  ncb_filtered <- ncb %>%
    group_by(group) %>%
    filter(n() >= minBouts,
           length(unique(individual_local_identifier)) > minIndivs)
  
  # convert back to sf object for mapping
  wild_carcass_bouts_df <- as.data.frame(ncb_filtered) %>%
    rename("carcID" = group) %>%
    sf::st_as_sf(crs = 32636)
  
  return(wild_carcass_bouts_df)
}

get_wild_carcasses <- function(wild_carcass_bo_df){
  # Get carcasses
  wild_carcasses <- wild_carcass_bo_df %>%
    mutate(year = lubridate::year(timestamp)) %>%
    group_by(year, carcID) %>%
    summarize(geometry = sf::st_union(geometry),
              dateOnly = lubridate::date(timestamp)[1],
              nBouts = n(),
              nIndivs = length(unique(individual_id)),
              mintime = min(start),
              maxtime = max(end)) %>%
    sf::st_centroid() %>% # take spatial centroid to define the position of the "carcass"
    ungroup() %>%
    bind_cols(sf::st_coordinates(.)) %>%
    mutate(datetime = mintime,
           datetime = lubridate::ymd_hms(datetime),
           datetime_il = lubridate::with_tz(datetime, tzone = "Israel")) # arbitrarily deciding that the min time of the first bout defines the "carcass time"
  return(wild_carcasses)
}
# "Limitations of threshold
# The threshold of group_times is considered only within the scope of 24 hours and this poses limitations on it:
# 
# threshold must evenly divide into 60 minutes or 24 hours
# multi-day blocks are consistent across years and timegroups from these are by year.
# number of minutes cannot exceed 60
# threshold cannot be fractional"

# prepare_data ------------------------------------------------------------
get_gps_combined <- function(gps_2022, gps_2023, gps_2024, bbox){
  gps_combined <- bind_rows(gps_2022, gps_2023) %>%
    bind_rows(gps_2024) %>%
    #bind_cols(sf::st_coordinates(.)) %>%
    #rename("location_long" = X,
    #       "location_lat" = Y) %>%
    st_transform(32636) %>%
    st_crop(bbox) %>%
    mutate(year = lubridate::year(timestamp))
  return(gps_combined)
}

get_gps_all <- function(carcs, gps_combined, days_after, days_before){
  gps_all <- vector(mode = "list", length = length(carcs))
  for(i in 1:length(carcs)){
    ic <- carcs[[i]]
    cid <- ic$carcID[1]
    carcass_datetime <- ic$datetime[1]
    out <- gps_combined %>%
      filter(timestamp >= (carcass_datetime-days(days_before)) & timestamp <= (carcass_datetime + days(days_after+1))) %>%
      mutate(dist_to_carcass = as.numeric(st_distance(., ic)),
             time_since_carcass = difftime(timestamp, carcass_datetime, units = "hours"),
             carcID = cid)
    out <- sf::st_as_sf(out, crs = 32636) %>% st_transform("WGS84") %>% bind_cols(st_coordinates(.)) %>%
      rename("location_long" = X, "location_lat" = Y)
    gps_all[[i]] <- out
  }
  return(gps_all)
}

get_roosts <- function(gps_all, col){
  r <- map(gps_all, ~{
    if(nrow(.x) > 0){return(get_roosts_df(.x, id = col))}
    else{return(NULL)}
  })
  return(r)
}

# get_seeds_gps <- function(gps_all, stn_carcs, seed_time_before, seed_distance_flight, seed_distance_stationary){
#   seeds_gps <- map2(gps_all, stn_carcs, ~{
#     dttm <- .y$datetime[1]
#     .x %>% filter(timestamp >= dttm-seed_time_before & timestamp <= dttm) %>%
#       filter((ground_speed >= 5 & dist_to_carcass < seed_distance_flight) | (ground_speed < 5 & dist_to_carcass < seed_distance_stationary))
#   })
#   return(seeds_gps)
# }

# get_distances <- function(roosts, stn_carcs){
#   stn_carcs <- map(stn_carcs, ~.x %>% mutate(year = lubridate::year(date)))
#   distances <- map2(roosts, stn_carcs, ~{
#     if(!is.null(.x)){
#       dist <- .x %>%
#         sf::st_as_sf(., coords = c("location_long", "location_lat"), crs = "WGS84") %>%
#         sf::st_transform(32636) %>%
#         mutate(dist = as.numeric(st_distance(., .y))) %>%
#         st_drop_geometry() %>%
#         dplyr::select(tag_local_identifier, roost_date, dist) %>%
#         pivot_wider(id_cols = "tag_local_identifier", names_from = "roost_date", values_from = "dist", names_prefix = "roost_") %>%
#         mutate(year = .y$year[1])
#     }else{
#       dist <- NULL
#     }
#     return(dist)
#   })
#   return(distances)
# }
# 
# get_www <- function(ww){
#   www <- ww %>%
#     dplyr::select(Nili_id, Movebank_id, Nili_id, birth_year, sex) %>%
#     mutate(age_2022 = 2022-birth_year,
#            age_2023 = 2023-birth_year,
#            age_2024 = 2024-birth_year,
#            age_group_2022 = case_when(age_2022 > 5 ~ "02_adult",
#                                       age_2022 <= 5 ~ "01_juv_sub",
#                                       .default = NA),
#            age_group_2023 = case_when(age_2023 > 5 ~ "02_adult",
#                                       age_2023 <= 5 ~ "01_juv_sub",
#                                       .default = NA),
#            age_group_2024 = case_when(age_2024 > 5 ~ "02_adult",
#                                       age_2024 <= 5 ~ "01_juv_sub",
#                                       .default = NA)) %>%
#     dplyr::select("tag_local_identifier" = "Movebank_id", age_group_2022, age_group_2023, age_group_2024) %>%
#     distinct()
#   return(www)
# }
# 
# get_ilvs <- function(distances, www){
#   yrs <- map_dbl(distances, ~.x$year[1])
#   ilvs <- map2(distances, yrs, ~{
#     tojoin <- www %>%
#       dplyr::select(tag_local_identifier, "age_group" = paste0("age_group_", .y))
#     out <- left_join(.x, tojoin, by = "tag_local_identifier")
#     to_rename <- names(out)[grepl("roost_", names(out))]
#     new_names <- paste0("roost_night", 0:(length(to_rename)-1))
#     names(out)[names(out) %in% to_rename] <- new_names
#     return(out)})
#   return(ilvs)
# }

remove_points_before <- function(gps_all, stn_carcs, days_after, hours_before = 0){
  gps <- map2(gps_all, stn_carcs, ~{
    dttm <- .y$datetime[1]
    .x %>%
      filter(timestamp >= lubridate::ymd_hms(dttm)-hours(hours_before) & timestamp <= (lubridate::ymd_hms(dttm) + days(days_after)))
  })
  return(gps)
}

get_at_carcass <- function(gps, stn_carcs, arrival_distance){
  at_carcass <- map2(gps, stn_carcs, ~.x %>%
                       mutate(carcID = .y$carcID) %>%
                       filter(dist_to_carcass < arrival_distance & ground_speed < 5))
  return(at_carcass)
}

get_see_carcass <- function(gps, stn_carcs, detection_distance_flight, detection_distance_stationary){
  see_carcass <- map2(gps, stn_carcs, ~.x %>%
                        mutate(carcID = .y$carcID) %>%
                        filter((ground_speed < 5 & dist_to_carcass < detection_distance_stationary) | (ground_speed >= 5 & dist_to_carcass < detection_distance_flight)))
  return(see_carcass)
}

get_firsts <- function(at_carcass, stn_carcs){
  firsts <- map2(at_carcass, stn_carcs, ~{
    if(nrow(.x) > 1){
      out <- .x %>%
        filter(timestamp >= .y$datetime) %>%
        arrange(timestamp) %>%
        group_by(tag_local_identifier) %>%
        slice(1) %>%
        ungroup() %>%
        arrange(timestamp)
      if(nrow(out) > 0){
        out$rownumber <- 1:nrow(out)
        return(out)
      }else{
        out <- data.frame(local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, individual_id = NA, tag_local_identifier = NA, location_long = NA, location_lat = NA, dist_to_carcass = NA, carcID = .y$carcID, geometry = NA, rownumber=  NA)
        return(out)
      }
    }else{
      out <- data.frame(local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, individual_id = NA, tag_local_identifier = NA, location_long = NA, location_lat = NA, dist_to_carcass = NA, carcID = .y$carcID, geometry = NA, rownumber=  NA)
      return(out)
    }
  })
  return(firsts)
}

get_firsts_see <- function(see_carcass, stn_carcs){
  firsts_see <- map2(see_carcass, stn_carcs, ~{
    if(nrow(.x) > 1){
      out <- .x %>%
        filter(timestamp >= .y$datetime) %>%
        arrange(timestamp) %>%
        group_by(tag_local_identifier) %>%
        slice(1) %>%
        ungroup() %>%
        arrange(timestamp)
      if(nrow(out) > 0){
        out$rownumber <- 1:nrow(out)
        return(out)
      }else{
        out <- data.frame(local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, individual_id = NA, tag_local_identifier = NA, location_long = NA, location_lat = NA, dist_to_carcass = NA, carcID = .y$carcID, geometry = NA, rownumber=  NA)
        return(out)
      }
    }else{
      out <- data.frame(local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, individual_id = NA, tag_local_identifier = NA, location_long = NA, location_lat = NA, dist_to_carcass = NA, carcID = .y$carcID, geometry = NA, rownumber=  NA)
      return(out)
    }
  }) 
  return(firsts_see)
}

get_has_visits <- function(firsts){
  map_dbl(firsts, ~nrow(.x[!is.na(.x$tag_local_identifier),])) > 0
}

get_has_sightings <- function(firsts_see){
  map_dbl(firsts_see, ~nrow(.x[!is.na(.x$tag_local_identifier),])) > 0
}

get_has_enough_sightings <- function(firsts_see, min_sightings){
  map_dbl(firsts_see, ~nrow(.x[!is.na(.x$tag_local_identifier),])) > min_sightings
}

get_flight_allday <- function(gps, subsettor){
  flight_allday <- map(gps[subsettor], ~.x %>%
                         group_by(dateOnly) %>%
                         group_split())
  return(flight_allday)
}

get_gps_flight <- function(gps, subsettor, times_list){
  len <- length(gps[subsettor])
  out <- vector(mode = "list", length = len)
  for(i in 1:len){
    times <- times_list[[i]]
    subsets <- vector(mode = "list", length = length(times))
    for(j in 1:length(times)){
      subsets[[j]] <- gps[subsettor][[i]] %>%
        filter(timestamp <= times[j])
    }
    out[[i]] <- subsets
    #cat("done with", i, "\n")
  }
  return(out)
}

get_gps_flight_hr <- function(gps, subsettor, times_list, hrs){
  len <- length(gps[subsettor])
  out <- vector(mode = "list", length = len)
  for(i in 1:len){
    times <- times_list[[i]][!is.na(times_list[[i]])]
    if(length(times) > 0){
      subsets <- vector(mode = "list", length = length(times))
      for(j in 1:length(times)){
        subsets[[j]] <- gps[subsettor][[i]] %>%
          filter(timestamp >= times[j]-hours(hrs) & timestamp <= times[j])
      }
    }else{
      subsets <- "blank" # assigning this to NULL wasn't working
    }
    out[[i]] <- subsets
  }
  return(out)
}

get_roost_dates <- function(roosts, subsettor){
  out <- map(roosts[subsettor], ~{
    .x %>%
      group_by(roost_date) %>%
      group_split() %>%
      map(., ~st_as_sf(.x, coords = c("location_long", "location_lat"), crs = "WGS84") %>%
            st_transform(32636))
  })
  return(out)
}

get_roosts_bin <- function(dates, roost_thresh){
  map(dates, ~{
    outout <- map(.x, ~{
      ids <- .x$tag_local_identifier
      out <- as.data.frame(st_distance(.x)) %>%
        mutate(across(everything(), as.numeric))
      out[out < roost_thresh] <- 1
      out[out >= roost_thresh] <- 0
      row.names(out) <- ids
      colnames(out) <- ids
      return(out)
    })
    return(outout)
  })
}

get_roosts_weighted <- function(dates){
  map(dates, ~{ # XXX start here
    outout <- map(.x, ~{
      ids <- .x$tag_local_identifier
      out <- as.data.frame(st_distance(.x)) %>%
        mutate(across(everything(), as.numeric))
      out <- 1/sqrt(out)
      row.names(out) <- ids
      colnames(out) <- ids
      return(out)
    })
    return(outout)
  })
}

get_fl_weighted <- function(dat, dist){
  if(is.data.frame(dat)){
    if(nrow(dat) > 0){
      self_edges <- data.frame(ID1 = sort(unique(dat$tag_local_identifier)),
                               ID2 = sort(unique(dat$tag_local_identifier)),
                               sri = 0)
      dat$dateOnly <- lubridate::date(dat$timestamp) # NNN--this will make more sense if everything is converted to Israel time, because otherwise the date delineations won't be correct.
      # NNN check back in previous analysis--do we need to remove the roost sites?
      out1 <- suppressMessages(vultureUtils::getFlightEdges(dat, roostPolygons = NULL,
                                                            consecThreshold = 1,
                                                            idCol = "tag_local_identifier",
                                                            return = "sri",
                                                            distThreshold = dist))
      if(!("sri" %in% names(out1)) & nrow(out1) == 0){ # if the flight edges function returned nothing (if there were no flight interactions)
        out1$sri <- numeric(0) # a numeric vector of length 0 (adding the column so it exists, but the data frame has 0 rows) (bookkeeping)
      }
      # making the matrix square--duplicating the interaction rows in the opposite direction
      out2 <- out1[,c("ID2", "ID1", "sri")]
      names(out2) <- c("ID1", "ID2", "sri")
      out <- bind_rows(out1, out2) # bind together the upper and lower triangle
      out <- out %>%
        mutate(across(c("ID1", "ID2"), as.character)) %>%
        bind_rows(self_edges) %>% # add the diagonal
        mutate(sri = case_when((is.nan(sri)|is.na(sri)) ~ 0, .default = sri)) %>% # XXX forcing all NaNs and NAs to zero because we don't have a choice--can't have missing values in the network. Should be very few NAs because of the previous filtering
        arrange(ID1, ID2) %>% # NNN verify here that the order matches the order vector from the nbda dataset after alphabetizing/removing spaces (need to remove those spaces as early as possible in the process, ideally step 1, so this will all be consistent.)
        pivot_wider(id_cols = "ID1", names_from = "ID2", values_from = "sri") %>%
        mutate(across(everything(), ~replace_na(.x, 0))) %>% # replace any final NAs that resulted from the pivot
        dplyr::select(ID1, all_of(.$ID1)) %>% # get the rows and columns to be in the same order
        as.data.frame() # because apparently we can't set row names on a tibble anymore, ugh
      # NNN check igraph to see if there's a function to convert edgelists to adjacency matrices. Maybe simpler and deals with edge cases.
      row.names(out) <- out$ID1 # doing this because it makes indexing easier later
    }else{
      out <- "blank"
    }
  }else{
    out <- "dat is a list"
  }
  return(out)
}


get_fl_bin <- function(dat, dist){
  if(is.data.frame(dat)){
    if(nrow(dat) > 0){
      self_edges <- data.frame(ID1 = sort(unique(dat$tag_local_identifier)),
                               ID2 = sort(unique(dat$tag_local_identifier)),
                               value = 0)
      dat$dateOnly <- lubridate::date(dat$timestamp) #XXX fix getFlightEdges to not require this!
      out1 <- suppressMessages(vultureUtils::getFlightEdges(dat, roostPolygons = NULL,
                                                            consecThreshold = 1,
                                                            idCol = "tag_local_identifier",
                                                            return = "edges",
                                                            distThreshold = dist)) %>%
        dplyr::select(ID1, ID2) %>%
        distinct() %>%
        mutate(value = 1)
      out2 <- out1[,c("ID2", "ID1", "value")]
      names(out2) <- c("ID1", "ID2", "value")
      out <- bind_rows(out1, out2) %>%
        bind_rows(self_edges) %>%
        arrange(ID1, ID2) %>%
        pivot_wider(id_cols = "ID1", names_from = "ID2", values_fill = 0) %>%
        dplyr::select(ID1, all_of(.$ID1)) %>% # get the rows and columns to be in the same order
        as.data.frame() # because apparently we can't set row names on a tibble anymore, ugh
      row.names(out) <- out$ID1 # doing this because it makes indexing easier later
    }else{
      out <- "blank"
    }
  }else{
    out <- "blank"
  }
  return(out)
}

get_fl_bin_list <- function(gps_list, detection_distance){
  out <- vector(mode = "list", length = length(gps_list))
  for(i in 1:length(out)){
    out[[i]] <- map(gps_list[[i]], ~get_fl_bin(dat = .x, dist = detection_distance))
  }
  return(out)
}

get_fl_wt_list <- function(gps_list, detection_distance){
  out <- vector(mode = "list", length = length(gps_list))
  for(i in 1:length(out)){
    out[[i]] <- map(gps_list[[i]], ~get_fl_weighted(dat = .x, dist = detection_distance))
    cat(i, "\n")
  }
  return(out)
}

fix_nets <- function(nets, indivs){
  indivs <- as.character(indivs[!is.na(indivs)])
  updated <- vector(mode = "list", length = length(nets))
  if(length(nets) > 0){
    for(nt in 1:length(nets)){
      net <- nets[[nt]]
      if(class(net) != "character" & !("ID1" %in% names(net))){ # this is a stupid workaround so the function will work with the co-roost network. Horribly inefficient.
        net$ID1 <- row.names(net)
        net <- net %>%
          relocate(ID1)
      }
      
      # Find any that are missing and add them
      missing <- indivs[!(indivs %in% names(net))]
      if(length(missing) > 0){
        toadd <- data.frame(ID1 = missing, ID2 = missing, value = 0) %>% pivot_wider(id_cols = "ID1", names_from = "ID2", values_from = "value", values_fill = 0)
        if(!any(net == "blank", na.rm = T)){
          net_updated <- as.data.frame(bind_rows(net, toadd %>% mutate(ID1 = as.character(ID1))))
        }else{
          net_updated <- as.data.frame(toadd)
        }
        net_updated[is.na(net_updated)] <- 0
        row.names(net_updated) <- net_updated$ID1
      }else{
        net_updated <- net
      }
      net_updated_2 <- net_updated %>% dplyr::select(-ID1)
      if(any(!(indivs %in% row.names(net_updated_2)))){
        stop(paste(nt, "missing1"))
      }
      if(any(!(indivs %in% names(net_updated_2)))){
        stop(paste(nt, "missing2"))
      }
      updated[[nt]] <- net_updated_2[indivs, indivs]
    }
  }else{
    updated <- NULL
  }
  return(updated)
}

fix_nets_list <- function(list, oa_sorted){
  fixed_list <- vector(mode = "list", length = length(list))
  for(i in 1:length(list)){
    nets <- list[[i]]
    indivs <- oa_sorted[[i]]
    fixed_list[[i]] <- fix_nets(nets, indivs)
  }
  return(fixed_list)
}

get_nets <- function(fixed_list_subset){
  map(fixed_list_subset, ~{
    igraph::graph_from_adjacency_matrix(as.matrix(.x), mode = "undirected", diag = F)
  })
}

get_nets_list <- function(fixed_list){
  map(fixed_list, get_nets)
}

# NBDA --------------------------------------------------------------------
nbdaModSum <- function(model){
  dat <- data.frame(Variable = model@varNames,
                    MLE = model@outputPar,
                    SE = model@se)
  return(dat)
}

get_years <- function(carcs, oas){
  years <- map(carcs, ~st_drop_geometry(.x) %>% 
                 dplyr::select(carcID, datetime, X, Y, stationName, carcassWeight) %>%
                 mutate(year = lubridate::year(datetime), carcID = as.character(carcID)) %>% 
                 dplyr::select(-datetime)) %>% 
    purrr::list_rbind() %>% 
    mutate(n_detections = map_dbl(oas, length))
  return(years)
}

discoveryplot <- function(firsts, carcID){
  firsts %>% 
    ggplot(aes(x = timestamp, y = rownumber))+
    geom_line()+
    geom_point(size = 2, pch = 21, fill = "white")+
    theme_classic()+
    labs(y = "Cumulative number of vultures", 
         x = "Time", 
         title = "Vultures discovering the carcass", 
         caption = "Number of unique vultures that flew within sight (1km)\nof the carcass since placement")+
    ggtitle(carcID)
}

expand_mats <- function(rm, fm, dv){
  expanded <- vector(mode = "list", length = length(fm))
  for(i in 1:length(dv)){
    tryCatch(
      #this is the chunk of code we want to run
      {expanded[[i]] <- rm[[dv[i]]]
      }, error = function(msg){
        expanded[[i]] <- NULL
      })
  }
  return(expanded)
}

expand_roost_mats <- function(roost_mats, fl_mats, days_vec){
  rme <- vector(mode = "list", length = length(roost_mats))
  for(i in 1:length(rme)){
    expanded <- expand_mats(roost_mats[[i]], fl_mats[[i]], days_vec[[i]])
    expanded_as_matrix <- map(expanded, ~{
      if(!is.null(.x)){return(as.matrix(.x))}else{return(.x)}})
    rme[[i]] <- expanded_as_matrix
  }
  return(rme)
}

get_dynamic_nets <- function(ni, nt, matrices){
  n_dynamic <- map2(ni, nt, ~array(NA, dim = c(.x, .x, 1, .y)))
  for(i in 1:length(matrices)){
    for(j in 1:nt[[i]]){
      n_dynamic[[i]][,,1,j] <- array(matrices[[i]][[j]], 
                                     dim = c(ni[[i]], ni[[i]], 1))
    }
  }
  return(n_dynamic)
}

mod_trycatch <- function(datalist, type = "social", iterations = 150){
  mod <- map(datalist, ~{
    tryCatch({oadaFit(.x, type = type, iterations = iterations)}, error = function(msg){"error!"})
  })
  return(mod)
}

getmodstats <- function(mod){
  tryCatch({
    ps <- ifelse(mod@type == "social", unname(nbdaPropSolveByST(model = mod)[1]), NA)
    df <- data.frame(soc = mod@type,
                     loglik = mod@loglik,
                     aic = mod@aic,
                     aicc = mod@aicc,
                     varNames = mod@varNames,
                     outputPar = mod@outputPar, #XXX this will need to change once the model has more params, but for now it's length 1, conveniently.
                     se = mod@se,
                     propsolve = ps)
    return(df)}, 
    error = function(msg){
      df <- data.frame(soc = NA,
                       loglik = NA, 
                       aic = NA, 
                       aicc = NA, 
                       varNames = NA, 
                       outputPar = NA, 
                       se = NA,
                       propsolve = NA)
      return(df)})
  
}

get_summaries <- function(models_list, cids, type, network){
  out <- map(models_list, getmodstats) %>%
    setNames(cids) %>%
    purrr::list_rbind(names_to = "carcID") %>%
    mutate(type = type, network = network)
  return(out)
}

get_model_cis <- function(mods, search){
  for(i in 1:length(mods)){
    if(is.na(search[i,2]) & !is.na(search[i,4])){
      ci <- profLikCI(which = 1, model = mods[[i]],
                      upperRange = search[i,4:5])
    }else if(!is.na(search[i,2]) & !is.na(search[i,4])){
      ci <- profLikCI(which = 1, model = mods[[i]],
                      lowerRange = search[i,2:3],
                      upperRange = search[i,4:5])
    }else{
      ci <- c(NA, NA)
    }
    search[i,6:7] <- ci 
  }
  return(search)
}

get_solveprops_list <- function(cis, datalist, type = "dynamic", bound){
  if(bound == "lower"){
    out <- map2_dbl(cis$ci_lower[cis$type == type], datalist, ~{
      nbdaPropSolveByST(par = .x, nbdadata = .y)[1]
    })
  }else if(bound == "upper"){
    out <- map2_dbl(cis$ci_upper[cis$type == type], datalist, ~{
      nbdaPropSolveByST(par = .x, nbdadata = .y)[1]
    })
  }
  
  return(out)
}

update_cis_dfs <- function(cis_df, lower, upper){
  cis_df$propsolve_lower[cis_df$type == "dynamic"] <- lower
  cis_df$propsolve_upper[cis_df$type == "dynamic"] <- upper
  return(cis_df)
}

bind_cis <- function(x, y){
  out <- bind_rows(x, y) %>% 
    mutate(sig_ci = ifelse(propsolve_lower > 0, T, F), 
           soc = "social")
  return(out)
}

get_ilv_separate <- function(n_indivs, oas, ilvs_lists, ilv){
  out <- map2(n_indivs, oas, ~matrix(NA, nrow = .x, ncol = length(.y)))
  for(i in 1:length(out)){
    for(j in 1:nrow(out[[i]])){
      if(ilv == "age"){
        out[[i]][j,] <- map_chr(ilvs_lists[[i]], ~as.character(.x$age_group[j]))
      }else if(ilv == "dist"){
        out[[i]][j,] <- map_dbl(ilvs_lists[[i]], ~as.numeric(.x$dist_roost[j]))
      }
    }
  }
  return(out)
}

get_ilvs_lists <- function(ilvs_nbda, days_vec_nbda){
  ilvs_lists <- vector(mode = "list", length = length(ilvs_nbda))
  for(i in 1:length(ilvs_lists)){
    ilvs <- ilvs_nbda[[i]]
    nights_vec <- days_vec_nbda[[i]]-1
    ilvs_this_carcass <- map(nights_vec, ~ilvs %>% dplyr::select(tag_local_identifier, paste0("roost_night", .x), age_group) %>% rename("dist_roost" = 2))
    ilvs_lists[[i]] <- ilvs_this_carcass
  }
  return(ilvs_lists)
}

substitute_na_distances <- function(roost_carc_distances){
  map(roost_carc_distances, ~{
    # Replace NA values with column means
    mat_filled <- apply(.x, 2, function(col) {
      col[is.na(col)] <- mean(col, na.rm = TRUE)
      return(col)
    })
    
    # Convert the result back to a matrix (apply returns a matrix here, but to be safe):
    mat_filled <- as.matrix(mat_filled)
    return(mat_filled)
  })
}

std_dists <- function(distances){
  out <- map(distances, ~{
    (.x-mean(.x))/sd(.x)
  })
  return(out)
}

binarize_ages <- function(age_groups){
  out <- map(age_groups, ~{
    .x[.x == "01_juv_sub"] <- 0
    .x[.x == "02_adult"] <- 1
    .x <- apply(.x, 2, as.numeric)
    return(.x)
  })
  return(out)
}

get_constraintsVectMatrix <- function(){
  constraintsVectMatrix<-rbind(
    #s1, s2, asocial_ag, asocial_srcd
    # netcombo 1 0
    #netcombo 1 0
    c(1,0,0,0),
    c(1,0,0,2),
    c(1,0,2,0),
    c(1,0,2,3),
    
    #netcombo 0 1
    c(0,1,0,0),
    c(0,1,0,2),
    c(0,1,2,0),
    c(0,1,2,3),
    
    #netcombo 1 1
    c(1,1,0,0),
    c(1,1,0,2),
    c(1,1,2,0),
    c(1,1,2,3),
    
    #netcombo 1 2
    c(1,2,0,0),
    c(1,2,0,3),
    c(1,2,3,0),
    c(1,2,3,4),
    
    #netcombo 0 0 (doesn't include age effect on social transmission, since there is by definition no social transmission in these models)
    c(0,0,0,0),
    c(0,0,1,0),
    c(0,0,0,1),
    c(0,0,1,2)
  )
  return(constraintsVectMatrix)
}

get_modelset <- function(data, constraints){
  out <- map(data, ~oadaAICtable(.x, constraints))
  return(out)
}

get_maes <- function(modelset_list){
  out <- map(modelset_list, ~{
    rbind( support=variableSupport(.x),
           MAE=modelAverageEstimates(.x),
           USE=unconditionalStdErr(.x))
  })
  return(out)
}

get_lowerlimits <- function(modelset_list, net, conf_level){
  out <- map(modelset_list, ~multiModelLowerLimits(which = net,
                                                   aicTable = .x,
                                                   conf = conf_level))
  return(out)
}

get_nbdaData_list_flex <- function(cids, oas, amis,
                                   nets1, nets2 = NULL,
                                   is_dynamic = FALSE,
                                   dists = NULL, ags = NULL,
                                   seeds = NULL,
                                   n_indivs = NULL, n_timeperiods = NULL) {
  use_ilvs <- !is.null(dists) && !is.null(ags)
  use_two_nets <- !is.null(nets2)
  
  if (use_ilvs) {
    for (i in seq_along(dists)) {
      name1 <- paste0("std_roost_carc_distances_", i)
      name2 <- paste0("age_groups_", i)
      assign(name1, dists[[i]], envir = .GlobalEnv)
      assign(name2, ags[[i]], envir = .GlobalEnv)
    }
  }
  
  # If two networks are used, create combined 4D array list
  if (use_two_nets) {
    stopifnot(is_dynamic, !is.null(n_indivs), !is.null(n_timeperiods))
    twonets_array_list <- vector("list", length(cids))
    for (i in seq_along(cids)) {# for each carcass
      arr <- array(NA, dim = c(n_indivs[i], n_indivs[i], 2, n_timeperiods[i]))
      for (j in seq_len(n_timeperiods[[i]])) {
        arr[,,1,j] <- nets1[[i]][[j]]
        arr[,,2,j] <- nets2[[i]][[j]]
      }
      nets1[[i]] <- arr  # Replace nets1 with merged array (so we still only pass one argument into the ultimate call to nbdaData, I think?)
    }
  }
  
  outlist <- vector("list", length(cids))
  for (i in seq_along(cids)) {
    carcass <- cids[i]
    label <- paste0("Carcass ", carcass)
    
    ilv_args <- list()
    if (use_ilvs) {
      ag_name <- paste0("age_groups_", i)
      srcd_name <- paste0("std_roost_carc_distances_", i)
      ilv_args <- list(
        asoc_ilv = c(ag_name, srcd_name),
        #int_ilv = ag_name,
        asocialTreatment = "timevarying"
      )
    }
    
    if (is_dynamic || use_two_nets) {
      outlist[[i]] <- do.call(nbdaData, c(list(
        label = label,
        demons = seeds[[i]],
        assMatrix = nets1[[i]], # because we replaced nets1 with the merged array before, we can still just pass nets1 in here, because it contains both networks.
        orderAcq = oas[[i]],
        assMatrixIndex = amis[[i]]
      ), ilv_args))
    } else {
      outlist[[i]] <- do.call(nbdaData, c(list(
        label = label,
        demons = seeds[[i]],
        assMatrix = nets1[[i]],
        orderAcq = oas[[i]]
      ), ilv_args))
    }
  }
  
  return(outlist)
}

get_closest_station <- function(all_bouts_assigned, stations){
  bouts_split <- sf::st_as_sf(all_bouts_assigned, coords = c("X", "Y"), crs = 32636, remove = F) %>%
    group_by(boutID) %>%
    group_split()
  stn_min_dists_bouts <- map_dbl(bouts_split, ~min(st_distance(.x, stations)))
  closest_stn_bouts <- purrr::list_rbind(map(bouts_split, ~stations[which.min(st_distance(.x, stations)),]))
  return(closest_stn_bouts)
}

assign_time_dist <- function(wild_carcass_bouts_df, wild_carcasses){
  wc <- wild_carcasses %>% dplyr::select(carcID, datetime, X, Y)
  ids <- unique(wild_carcass_bouts_df$carcID)
  lst <- vector(mode = "list", length = length(ids))
  for(i in 1:length(lst)){
    c <- wc[wc$carcID == ids[i],]
    b <- wild_carcass_bouts_df %>% dplyr::filter(carcID == ids[i])
    dists <- as.numeric(st_distance(b, c))
    b$dist_to_carcass <- dists
    times <- difftime(b$timestamp, c$datetime, units = "hours")
    b$time_since_carcass <- times
    lst[[i]] <- b
  }
  df <- purrr::list_rbind(lst)
  return(df)
}

get_merged <- function(bouts_predictions){
  merged <- purrr::list_rbind(bouts_predictions)
  merged$sensor <- "ACC"
  merged <- merged %>%
    dplyr::rename("tag_local_identifier" = device_id,
                  "timestamp" = start) %>%
    dplyr::mutate(timestamp = case_when(nchar(timestamp) == 10 ~ paste0(timestamp, " 00:00:00"),
                                        .default = timestamp)) %>%
    dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp))
  return(merged)
}

get_full <- function(gps, merged){
  full <- bind_rows(gps, mutate(merged, timestamp = lubridate::ymd_hms(timestamp)))
  full <- full %>%
    group_by(tag_local_identifier) %>%
    arrange(timestamp) %>%
    mutate(time_diff = as.numeric(difftime(lead(timestamp), timestamp, units = "secs"))) %>%
    ungroup()
  return(full)
}

prepare_gps_crossref <- function(full){
  full <- full %>%
    group_by(tag_local_identifier) %>%
    arrange(timestamp) %>%
    mutate(time_diff = as.numeric(difftime(lead(timestamp), timestamp, units = "secs"))) %>%
    ungroup()
  return(full)
}

# attach_gps <- function(x, a = gps_bef, b = gps_aft, spd = gps_spd){
#   out <- x %>% 
#     group_by(tag_local_identifier) %>%
#     arrange(timestamp) %>%
#     rename("llo" = location_long, "lla" = location_lat, "td" = time_diff) %>%
#     mutate(
#       llo2 = case_when(is.na(llo) & lag(td) <= a & lag(ground_speed <= spd) ~ lag(llo),
#                        is.na(llo) & td < a & lead(ground_speed <= spd) ~ lead(llo),
#                        is.na(llo) & lag(td) <= b & lag(ground_speed <= spd) ~ lag(llo),
#                        is.na(llo) & td < b & lead(ground_speed <= spd) ~ lead(llo),
#                        .default = llo),
#       lla2 = case_when(is.na(lla) & lag(td) <= a & lag(ground_speed <= spd) ~ lag(lla),
#                        is.na(lla) & td < a & lead(ground_speed <= spd) ~ lead(lla),
#                        is.na(lla) & lag(td) <= b & lag(ground_speed <= spd) ~ lag(lla),
#                        is.na(lla) & td < b & lead(ground_speed <= spd) ~ lead(lla),
#                        .default =lla),
#       gs2 = case_when(is.na(llo) & lag(td) <= a & lag(ground_speed <= spd) ~ lag(ground_speed),
#                       is.na(llo) & td < a & lead(ground_speed <= spd) ~ lead(ground_speed),
#                       is.na(llo) & lag(td) <= b & lag(ground_speed <= spd) ~ lag(ground_speed),
#                       is.na(llo) & td < b & lead(ground_speed <= spd) ~ lead(ground_speed),
#                       .default = ground_speed)
#     )%>% 
#     # try another method of assigning still-unassigned acc bouts
#     mutate(
#       llo3 = case_when(is.na(llo2) & lag(td) <= a ~ lag(llo),
#                        is.na(llo2) & td < a ~ lead(llo), 
#                        .default = llo2),
#       lla3 = case_when(is.na(lla2) & lag(td) <= a ~ lag(lla2),
#                        is.na(lla2) & td < a ~ lead(lla), 
#                        .default = lla2),
#       gs3 = case_when(is.na(llo2) & lag(td) <= a ~ lag(ground_speed),
#                       is.na(llo2) & td < a ~ lead(ground_speed), 
#                       .default =  gs2)
#     ) %>%
#     ungroup()
#   return(out)
# }
# 
# keep_highest_gps_pair <- function(attached){
#   out <- attached %>%
#     dplyr::filter(sensor == "ACC") %>%
#     dplyr::mutate(location_long = dplyr::case_when(!is.na(llo2) ~ llo2,
#                                                    is.na(llo2) & !is.na(llo3) ~ llo3,
#                                                    .default = location_long),
#                   location_lat = dplyr::case_when(!is.na(lla2) ~ lla2,
#                                                   is.na(lla2) & !is.na(lla3) ~ lla3,
#                                                   .default = location_lat),
#                   ground_speed = dplyr::case_when(!is.na(gs2) ~ gs2,
#                                                   is.na(gs2) & !is.na(gs3) ~ gs3,
#                                                   .default = ground_speed)) %>%
#     dplyr::select(-c("llo2", "lla2", "gs2", "llo3", "lla3", "gs3"))
#   return(out)
# }

getfeeding <- function(x, thresh){
  if(!is.null(x)){
    out <- filter(x, pred == "Eating" & !is.na(location_lat) & .pred_Eating > thresh)
  }else{
    out <- NULL
  }
  return(out)
}

get_gps_forbouts_indivs <- function(device_ids, gps){
  out <- map(device_ids, ~{
    if(length(.x) > 0){
      filter(gps, tag_local_identifier == .x)
    }else{NULL}})
  return(out)
}

buffer_cliffs <- function(cliffs, buffer_m, crs_to_transform = 32636){
  transf <- sf::st_transform(cliffs, crs_to_transform)
  out_polys <- st_buffer(transf, buffer_m)
  out_multipoly <- st_union(out_polys)
  return(out_multipoly)
}

join_gps_bouts <- function(bp, wg){
  if(!is.null(bp) & !is.null(wg)){
    first_gps <- as.data.frame(wg) %>% 
      mutate(tag_local_identifier = as.numeric(as.character(tag_local_identifier))) %>%
      group_by(tag_local_identifier, bout_id) %>%
      arrange(timestamp) %>%
      slice(1) %>%
      ungroup()
    out <- left_join(bp, first_gps, by = c("device_id" = "tag_local_identifier", "bout_id"))
  }else{
    out <- data.frame(bout_id = NA, device_id = NA, pred = NA, .pred_Eating = NA, .pred_Flapping = NA, .pred_Ground = NA, .pred_Lying = NA, .pred_Soaring = NA, .pred_Standing = NA, start = NA, end = NA, tag_local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, location_lat = NA, location_long = NA, individual_id = NA, height_above_msl = NA)
    out <- out[0,]
  }
  return(out)
}

remove_bouts_on_cliffs <- function(bouts, cliffs){
  intersections <- st_intersects(bouts, cliffs)
  lgl <- map_dbl(intersections, length)
  tokeep <- which(lgl == 0)
  keep <- bouts[tokeep,]
  return(keep)
}

# DEM ---------------------------------------------------------------------
get_slopes <- function(filenames, bbox_south_big, neighbors = 8, feeding_bouts_stationary){
  bbox_south_vect <- terra::vect(st_transform(bbox_south_big, "WGS84"))
  demlist <- vector(mode = "list", length = length(filenames))
  for(i in 1:length(demlist)){
    demlist[[i]] <- terra::rast(filenames[i])
  } 
  cropped_list <- map(demlist, function(r) {
    tryCatch({
      crop(r, bbox_south_vect)
    }, error = function(e) {
      message("Error cropping raster: ", e$message)
      return(NULL) # Return NULL if an error occurs
    })
  })
  filtered_list <- cropped_list[!sapply(cropped_list, is.null)]
  merged_raster <- Reduce(f = merge, x =filtered_list)
  terrain <- terra::terrain(merged_raster, v = "slope", unit = "degrees", neighbors = neighbors)
  terrain_proj <- terra::project(terrain, "epsg:32636")
  
  feeding_bouts_vect <- terra::vect(feeding_bouts_stationary)
  slopes <- terra::extract(terrain_proj, feeding_bouts_vect)
  feeding_bouts_stationary_withslopes <- mutate(feeding_bouts_stationary, slope = slopes$slope)
  return(feeding_bouts_stationary_withslopes)
}


# Raster functions --------------------------------------------------------
points_to_raster <- function(
    carcasses_sf,            # sf POINT object
    bbox,                    # bounding box (numeric or object convertible to sf bbox)
    resolution = 10000       # grid cell size in meters (default 10km)
) {
  # Ensure carcasses are in a projected CRS (assume UTM if not set)
  if (is.na(st_crs(carcasses_sf))) {
    stop("Input 'carcasses_sf' must have a defined CRS.")
  }
  if (st_is_longlat(carcasses_sf)) {
    stop("Please project 'carcasses_sf' to a projected CRS (e.g., UTM).")
  }
  
  # Convert bbox to sf polygon if needed
  if (is.numeric(bbox) && length(bbox) == 4) {
    bbox_mat <- matrix(c(bbox[1], bbox[2], bbox[3], bbox[4]), ncol = 2, byrow = TRUE)
    bbox_poly <- st_as_sfc(st_bbox(c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4]), crs = st_crs(carcasses_sf)))
  } else if (inherits(bbox, "sf") || inherits(bbox, "sfc") || inherits(bbox, "SpatVector")) {
    bbox_poly <- st_as_sfc(st_bbox(bbox))
    bbox_poly <- st_transform(bbox_poly, st_crs(carcasses_sf))
  } else {
    stop("Invalid 'bbox' format. Provide a numeric vector of length 4 or an sf/sfc/SpatVector object.")
  }
  
  # Create a regular grid over the bounding box
  grid <- st_make_grid(bbox_poly, cellsize = resolution, square = TRUE)
  grid_sf <- st_sf(grid_id = 1:length(grid), geometry = grid)
  
  # Spatial join: assign carcasses to grid cells
  joined <- st_join(carcasses_sf, grid_sf, join = st_within)
  
  # Count carcasses per grid cell
  counts <- joined |>
    group_by(grid_id) |>
    summarise(carcass_count = n(), .groups = "drop")
  
  # Merge counts back to full grid, fill NAs with 0
  grid_with_counts <- left_join(grid_sf, st_drop_geometry(counts), by = "grid_id") |>
    mutate(carcass_count = ifelse(is.na(carcass_count), 0, carcass_count))
  
  # Convert to SpatVector
  grid_vect <- vect(grid_with_counts)
  
  # Create raster template
  r_template <- rast(grid_vect, resolution = resolution)
  
  # Rasterize
  r <- rasterize(grid_vect, r_template, field = "carcass_count", fun = NULL, background = 0)
  
  return(r)
}
dist_to_carcasses <- function(
    carcasses_sf,
    bbox,
    resolution = 1000,
    start_date = NULL,
    end_date = NULL,
    active_days = 3,         # used only if weight_col=NULL
    weight_col = NULL,       # NULL = unweighted, else name of weight col
    decay_rate = 0,          # ignored if weight_col=NULL
    min_weight = 0,          # ignored if weight_col=NULL
    distance_power = 1,
    visibility_radius = Inf  # Inf means no limit
) {
  # Validate inputs
  if (!"date" %in% names(carcasses_sf)) stop("Missing 'date' column.")
  if (!inherits(carcasses_sf$date, "Date")) {
    carcasses_sf$date <- as.Date(carcasses_sf$date)
  }
  if (st_is_longlat(carcasses_sf)) stop("Please project 'carcasses_sf' to a projected CRS.")
  
  if (!is.null(weight_col) && !(weight_col %in% names(carcasses_sf))) {
    stop(paste("Missing weight column:", weight_col))
  }
  
  # Convert start/end dates
  if (!is.null(start_date)) start_date <- as.Date(start_date)
  if (!is.null(end_date)) end_date <- as.Date(end_date)
  all_dates <- sort(unique(carcasses_sf$date))
  if (is.null(start_date)) start_date <- min(all_dates)
  if (is.null(end_date)) end_date <- max(all_dates)
  date_seq <- seq(start_date, end_date, by = "day")
  
  # Build bbox polygon
  if (is.numeric(bbox) && length(bbox) == 4) {
    bbox_poly <- st_as_sfc(st_bbox(c(xmin = bbox[1], ymin = bbox[2], xmax = bbox[3], ymax = bbox[4]),
                                   crs = st_crs(carcasses_sf)))
  } else {
    bbox_poly <- st_as_sfc(st_bbox(bbox))
    bbox_poly <- st_transform(bbox_poly, st_crs(carcasses_sf))
  }
  
  # Create grid and centroids
  grid <- st_make_grid(bbox_poly, cellsize = resolution)
  grid_sf <- st_sf(grid_id = seq_along(grid), geometry = grid)
  suppressWarnings({
    centroids <- st_centroid(grid_sf)
  })
  grid_vect <- vect(grid_sf)
  r_template <- rast(grid_vect, resolution = resolution)
  
  # If weighted, fix missing weights
  if (!is.null(weight_col)) {
    if (anyNA(carcasses_sf[[weight_col]])) {
      mean_weight <- mean(carcasses_sf[[weight_col]], na.rm = TRUE)
      carcasses_sf[[weight_col]][is.na(carcasses_sf[[weight_col]])] <- mean_weight
    }
  }
  
  dist_stack <- rast()
  empty_days <- c()
  
  for (current_date in date_seq) {
    current_date <- as.Date(current_date)
    
    if (is.null(weight_col)) {
      # Unweighted version: select active carcasses within active_days window
      if (is.null(active_days)) stop("active_days must be set if weight_col=NULL")
      active_window_start <- current_date - (active_days - 1)
      active_carcasses <- carcasses_sf %>%
        filter(date >= active_window_start & date <= current_date)
      
      if (nrow(active_carcasses) == 0) {
        # Assign max diagonal distance
        bbox_coords <- st_bbox(bbox_poly)
        bbox_diagonal <- sqrt((bbox_coords["xmax"] - bbox_coords["xmin"])^2 +
                                (bbox_coords["ymax"] - bbox_coords["ymin"])^2)
        r <- setValues(r_template, bbox_diagonal)
      } else {
        dist_matrix <- st_distance(centroids, active_carcasses)
        dist_matrix_mat <- as.numeric(dist_matrix)
        dist_matrix_mat <- matrix(dist_matrix_mat, nrow = nrow(centroids))
        
        if (is.finite(visibility_radius)) {
          # Mask distances beyond visibility radius
          dist_matrix_mat[dist_matrix_mat > visibility_radius] <- NA
          mean_distances <- apply(dist_matrix_mat, 1, function(x) {
            if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
          })
          # Replace NAs by max at the end
        } else {
          mean_distances <- apply(dist_matrix_mat, 1, mean)
        }
        
        grid_sf$mean_dist <- mean_distances
        grid_vect <- vect(grid_sf)
        r <- rasterize(grid_vect, r_template, field = "mean_dist", fun = mean)
      }
    } else {
      # Weighted version
      decay_df <- carcasses_sf %>%
        mutate(days_elapsed = as.numeric(current_date - .data[["date"]])) %>%
        filter(!is.na(.data[[weight_col]]), days_elapsed >= 0) %>%
        mutate(
          decayed_weight = .data[[weight_col]] * exp(-decay_rate * days_elapsed),
          active = decayed_weight >= min_weight
        ) %>%
        filter(active)
      
      if (nrow(decay_df) == 0) {
        r <- setValues(r_template, NA)
        empty_days <- c(empty_days, current_date)
      } else {
        dist_matrix <- st_distance(centroids, decay_df)
        dist_matrix_mat <- as.numeric(dist_matrix)
        dist_matrix_mat <- matrix(dist_matrix_mat, nrow = nrow(centroids))
        
        weight_matrix <- matrix(rep(decay_df$decayed_weight, each = nrow(centroids)),
                                nrow = nrow(centroids))
        
        if (is.finite(visibility_radius)) {
          in_range <- dist_matrix_mat <= visibility_radius
          weight_matrix[!in_range] <- 0
          dist_matrix_mat[!in_range] <- NA
        }
        
        weighted_dists <- (dist_matrix_mat ^ distance_power) * weight_matrix
        sum_weights <- rowSums(weight_matrix, na.rm = TRUE)
        
        weighted_mean_dist <- rowSums(weighted_dists, na.rm = TRUE) / sum_weights
        weighted_mean_dist[sum_weights == 0] <- NA
        
        grid_sf$mean_dist <- weighted_mean_dist
        grid_vect <- vect(grid_sf)
        r <- rasterize(grid_vect, r_template, field = "mean_dist", fun = NULL)
      }
    }
    
    names(r) <- format(current_date, "%Y-%m-%d")
    dist_stack <- c(dist_stack, r)
  }
  
  # Replace NA pixels with global max distance across all layers
  if (nlyr(dist_stack) == 0) stop("No layers were created.")
  
  global_max <- max(global(dist_stack, fun = "max", na.rm = TRUE)[[1]], na.rm = TRUE)
  dist_stack[is.na(dist_stack)] <- global_max
  
  # Attach metadata
  attr(dist_stack, "empty_days") <- empty_days
  attr(dist_stack, "weight_col") <- weight_col
  attr(dist_stack, "visibility_radius") <- visibility_radius
  
  return(dist_stack)
}

get_pngs <- function(rasterstack){
  dates <- names(rasterstack)
  
  # Compute global min and max for color scale
  global_min <- min(values(rasterstack), na.rm = TRUE)
  global_max <- max(values(rasterstack), na.rm = TRUE)
  
  # Temporary list to store frame file paths
  png_files <- character(nlyr(rasterstack))
  
  # Loop through each raster layer
  for (i in seq_len(nlyr(rasterstack))) {
    r <- rasterstack[[i]]
    date_label <- dates[i]
    
    # Convert raster to data frame for ggplot
    r_df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
    colnames(r_df) <- c("x", "y", "value")
    
    # Create ggplot
    p <- ggplot(r_df) +
      geom_raster(aes(x = x, y = y, fill = value)) +
      coord_equal() +
      scale_fill_viridis_c(
        name = "Avg. Distance (m)",
        limits = c(global_min, global_max),
        na.value = "grey90",
        direction = -1
      ) +
      labs(
        title = paste("Date:", date_label),
        x = NULL,
        y = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
        legend.position = "right"
      )
    
    # Save to PNG
    png_file <- tempfile(fileext = ".png")
    ggsave(png_file, plot = p, width = 6, height = 6, dpi = 150)
    png_files[i] <- png_file
  }
  return(png_files)
}

get_cell_vals_long <- function(stack){
  cell_coords <- map(1:(dim(stack)[1]*dim(stack)[2]), ~xyFromCell(stack, .x))
  pts <- map(cell_coords, ~vect(matrix(.x, ncol = 2), type = "points", crs = crs(stack)))
  ts <- map(pts, ~terra::extract(stack, .x))
  values <-  map(ts, ~as_tibble(as.numeric(.x[1, -1])))
  cell_values_long <- data.table::rbindlist(values, idcol = "cell")
  coords <- data.table::rbindlist(map(cell_coords, as_tibble), idcol = "cell")
  cell_values_long <- left_join(cell_values_long, coords, by = "cell") %>%
    group_by(cell) %>%
    mutate(date = lubridate::ymd(names(stack))) %>%
    ungroup()
  return(cell_values_long)
}

# Functions for preparing NBDA data ---------------------------------------
# How to handle seeds:
# From the NBDA documentation: "demons: an optional binary numeric vector specifying which individuals are trained demonstrators or had otherwise already acquired the target behaviour prior to the start of the diffusion. Length should match the number of rows of assMatrix. e.g. c(0,0,1,0,0,0,1) specifies that individuals 3 and 7 are trained demonstrators."
# So we need to identify the seeds and then match them up to the order of the individuals in the matrix.
prepare_nbda_data <- function(gps,
                              ddf,
                              dds,
                              gps_spd,
                              n_hours_gps_dynamic = list(),
                              n_hours_gps_static = list(),
                              identify_seeds = FALSE,
                              seed_time_before = NULL,
                              sighting_time_max_hours = 72,
                              carcass_data = NULL) {
  
  gps$ground_speed <- as.numeric(gps$ground_speed)
  
  carc_id <- unique(gps$carcID)
  if (length(carc_id) != 1) stop("gps$carcID must have exactly one unique value.")
  
  if (identify_seeds && is.null(seed_time_before)) {
    stop("seed_time_before must be provided when identify_seeds = TRUE.")
  }
  
  # Identify seed individuals if needed
  seeds <- character(0)
  if (identify_seeds) {
    time_window <- seed_time_before / 60
    seeds <- gps %>%
      filter(time_since_carcass >= -time_window,
             time_since_carcass <= 0,
             (ground_speed > gps_spd & dist_to_carcass <= ddf) |
               (ground_speed <= gps_spd & dist_to_carcass <= dds)) %>%
      distinct(tag_local_identifier) %>%
      pull(tag_local_identifier) %>%
      as.character()
  }
  
  # Get first sightings
  gps_after_in_sight <- gps %>%
    filter(time_since_carcass >= 0 & time_since_carcass <= sighting_time_max_hours) %>%
    filter((ground_speed > gps_spd & dist_to_carcass <= ddf) |
             (ground_speed <= gps_spd & dist_to_carcass <= dds))
  
  first_sightings <- gps_after_in_sight %>%
    group_by(tag_local_identifier) %>%
    arrange(time_since_carcass, timestamp) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(time_since_carcass)
  
  if(identify_seeds){
    first_sightings <- first_sightings %>%
      filter(!(tag_local_identifier %in% seeds))
  }
  
  n_found <- length(unique(first_sightings$tag_local_identifier))
  n_gps <- length(unique(gps$tag_local_identifier))
  prop_found <- n_found / n_gps
  
  all_indivs_sorted <- sort(unique(gps$tag_local_identifier)) # NNN fix order here--for some reason, tag_local_identifier is a factor, and some of the levels have spaces on the end, which is causing weird ordering. Needs to be character or numeric.
  
  oa_indivs <- first_sightings %>%
    arrange(time_since_carcass, timestamp) %>%
    pull(tag_local_identifier) %>%
    as.character()
  
  oa_nums <- match(oa_indivs, all_indivs_sorted)
  if (length(oa_nums) != length(oa_indivs)) {
    stop("Length of oa vecs does not match")
  }
  if (length(oa_nums) == length(unique(gps$tag_local_identifier)) && prop_found != 1) {
    stop("All individuals are included in oa_nums, but not all indivs found the carcass. Something's wrong!")
  }
  
  ami <- seq_along(oa_nums) # for the dynamic networks
  
  # Infer carcass placement timestamp from first row
  if(!is.null(carcass_data)){
    cat("Using provided carcass placement time\n")
    carcass_placement_time <- carcass_data$datetime[1]
  }else{
    cat("Inferring carcass placement time from GPS data\n")
    first_row <- gps[1, ]
    carcass_placement_time <- first_row$timestamp - dhours(as.numeric(first_row$time_since_carcass))
    carcass_placement_date <- as.Date(carcass_placement_time)
  } #NNN change this to incorporate the original records of the carcass instead.
  
  gps_data_cumulative <- map(first_sightings$timestamp, function(ts) {
    day_start <- as.POSIXct(paste0(as.Date(ts), " 00:00:00"), tz = tz(ts)) # NNN double check that time zones get treated correctly here--pasting might mess things up. # NNN actually should convert everything to Israel time instead of changing carcasses to UTC, because that way we can use biologically meaningful cutoffs like this one and not have to convert them. 
    gps %>% filter(timestamp >= day_start, timestamp <= ts)
  })
  
  # Dynamic hour-range GPS segments per individual
  gps_data_dynamic_hour_ranges <- list()
  for (range in n_hours_gps_dynamic) { # c(-720, -24)
    if (length(range) != 2 || range[1] > range[2]) {
      stop("Each element of n_hours_gps_dynamic must be a length-2 numeric vector where the first number <= second.")
    }
    start_offset <- range[1]
    end_offset <- range[2]
    start_label <- ifelse(start_offset < 0, paste0("n", sprintf("%03d", abs(start_offset))), sprintf("%03d", start_offset))
    end_label <- ifelse(end_offset < 0, paste0("n", sprintf("%03d", abs(end_offset))), sprintf("%03d", end_offset))
    var_name <- sprintf("gps_data_dynamic_hours_%s_%s", start_label, end_label)

    gps_data_list <- map(first_sightings$timestamp, function(ts) {
      start_time <- ts + lubridate::dhours(as.numeric(start_offset))
      end_time <- ts + lubridate::dhours(as.numeric(end_offset))
      gps %>% filter(timestamp >= start_time, timestamp <= end_time)
    })

    gps_data_dynamic_hour_ranges[[var_name]] <- gps_data_list
  }
  
  # NNN decided to remove the static stuff entirely!
  # # Static hour-range GPS segments (shared across individuals)
  # gps_data_static_hour_ranges <- list()
  # for (range in n_hours_gps_static) {
  #   if (length(range) != 2 || range[1] > range[2]) {
  #     stop("Each element of n_hours_gps_static must be a length-2 numeric vector where the first number <= second.")
  #   }
  #   start_offset <- range[1]
  #   end_offset <- range[2]
  #   start_label <- ifelse(start_offset < 0, paste0("n", sprintf("%03d", abs(start_offset))), sprintf("%03d", start_offset))
  #   end_label <- ifelse(end_offset < 0, paste0("n", sprintf("%03d", abs(end_offset))), sprintf("%03d", end_offset))
  #   var_name <- sprintf("gps_data_static_hours_%s_%s", start_label, end_label)
  #   
  #   start_time <- carcass_placement_time + dhours(as.numeric(start_offset))
  #   end_time <- carcass_placement_time + dhours(as.numeric(end_offset))
  #   gps_data <- gps %>% filter(timestamp >= start_time, timestamp <= end_time)
  #   
  #   gps_data_static_hour_ranges[[var_name]] <- gps_data
  # }
  
  if(identify_seeds){
    seeds_vec <- as.numeric(all_indivs_sorted %in% seeds) # we've already created the seeds vector previously; this is just converting it to 0s and 1s, which is what NBDA needs.
  }else{
    seeds_vec <- NULL
  }
  
  base_list <- list(
    n_found = n_found,
    n_gps = n_gps,
    prop_found = prop_found,
    carcID = carc_id,
    seed_indivs = seeds,
    first_sightings = first_sightings,
    all_indivs_sorted = all_indivs_sorted,
    oa_indivs = oa_indivs,
    oa_nums = oa_nums,
    ami = ami,
    gps_data_cumulative = gps_data_cumulative,
    #gps_data_sameday = gps_data_sameday,
    seeds_vec = seeds_vec
  )
  
  final_list <- c(base_list, 
                  gps_data_dynamic_hour_ranges#, 
                  #gps_data_static_hour_ranges # NNN removing static ranges bc we decided not to use them
                  )
  return(final_list)
}


make_assMatrix <- function(input) {
  # Case 1: Single data frame (static)
  if (is.data.frame(input)) { # (bookkeeping--formatting it exactly as NBDA will expect it)
    mat <- as.matrix(input)
    n_indivs <- nrow(mat)
    assMatrix <- array(
      data = mat,
      dim = c(n_indivs, n_indivs, 1),
      dimnames = list(
        rownames(mat),
        colnames(mat),
        "net1"
      )
    )
    return(assMatrix)
  }
  
  # Case 2: List (dynamic)
  if (!is.list(input)) stop("Input must be a data frame or a list of data frames.")
  if (length(input) == 0) stop("Input list is empty.")
  
  # Validate elements
  dims <- dim(input[[1]])
  rownames_ref <- rownames(input[[1]])
  colnames_ref <- colnames(input[[1]])
  
  for (i in seq_along(input)) { # for each network in the dynamic network list
    df <- input[[i]]
    if (!is.data.frame(df)) stop(paste("Element", i, "is not a data frame."))
    if (!all(dim(df) == dims)) stop(paste("Element", i, "has different dimensions."))
    if (!all(rownames(df) == rownames_ref)) stop(paste("Row names mismatch in element", i))
    if (!all(colnames(df) == colnames_ref)) stop(paste("Column names mismatch in element", i))
  }
  
  mat_list <- lapply(input, as.matrix) # convert each element to a matrix
  n_indivs <- dims[1]
  
  if (length(input) == 1) {
    # Static case: single matrix wrapped in list 
    # edge case--in case you requested a dynamic network but there was only one individual that ever found the carcass, so you end up with a list of length 1. Will be one level down, as opposed to a top-level dataframe not included in a list (static case from above)
    assMatrix <- array(
      data = mat_list[[1]],
      dim = c(n_indivs, n_indivs, 1),
      dimnames = list(
        rownames_ref,
        colnames_ref,
        "net1"
      )
    )
  } else {
    # Dynamic case: time-varying matrix list
    n_time <- length(mat_list)
    assMatrix <- array(
      data = unlist(mat_list),
      dim = c(n_indivs, n_indivs, 1, n_time), # NNN check--why is there a 1 here and what do the dimensions mean?
      dimnames = list(
        rownames_ref,
        colnames_ref,
        "net1",
        paste0("time", seq_len(n_time))
      )
    )
  }
  
  return(assMatrix)
}

timeconvert <- function(carcs_list, old_datetime = "datetime", new_datetime = "datetime_il", new_date = "dateOnly", tz = "Israel"){
  out <- purrr::map(carcs_list, ~{
    .x[[new_datetime]] <- lubridate::with_tz(.x[[old_datetime]], tzone = tz)
    .x[[new_date]] <- lubridate::date(.x[[new_datetime]])
    return(.x)
  })
  return(out)
}

nb_shortcut <- function(list, ddf, dds, gps_spd, stmh, stb, seeds, carcass_data_list){
  out <- purrr::map2(.x = list, .y = carcass_data_list, ~{
    prepare_nbda_data(gps = .x,
                      identify_seeds = seeds,
                      seed_time_before = stb,
                      ddf = ddf, dds = dds, # detection distances
                      gps_spd = gps_spd,
                      n_hours_gps_static = list(c(-720, -24)),
                      sighting_time_max_hours = stmh,
                      carcass_data = .y)
  })
  return(out)
}

# This one is weird because we are working by transmitter ID, but the periods to remove are only by Nili_id, so we need to figure out which transmitter ID applies during the relevant period. Just gonna do this by hand for now.
# XXX START HERE--THIS IS ANNOYING!!
remove_periods <- function(dataset){
  ww <- readxl::read_excel(ww_file, sheet = "all gps tags")
  toremove <- readxl::read_excel(ww_file, sheet = "periods_to_remove") %>%
    filter(remove_end >= min(dataset$timestamp))
  
  ## Elara
  ## Endeavour
  ## Hamsa
  ## Jakarta
  ## Y17T58
  ## Yagur
}

get_roosts <- function(dat, id){
  roosts <- map(dat, ~vultureUtils::get_roosts_df(df = .x, id = id), 
                .progress = T)
  roosts <- roosts %>%
    map(., ~st_as_sf(.x, crs = "WGS84", 
                     coords = c("location_long", "location_lat"), 
                     remove = F), .progress = T)
  return(roosts)
}

get_roosting <- function(roosts, id){
  roosting <- map(roosts, ~{
    vultureUtils::getRoostEdges(.x, mode = "distance", 
                                distThreshold = 500,
                                return = "both", 
                                latCol = "location_lat", 
                                longCol = "location_long", 
                                idCol = id, 
                                dateCol = "roost_date")
  }, .progress = T)
  return(roosting)
}
