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
    
    within_5min_speed <- purrr::map(within_5min, ~return(.x[as.numeric(.x$ground_speed) <= spd,]))
    
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

get_station_bouts <- function(bouts, stations, dist){
  buf <- st_buffer(stations, dist)
  bts <- st_transform(st_as_sf(bouts), 32636)
  which_stn <- which(map_dbl(st_intersects(bts, buf), length) == 1)
  bts_stn <- bts[which_stn,]
  return(bts_stn)
}

# Clustering --------------------------------------------------------------
get_wild_carcasses <- function(df){
  # Get carcasses
  df <- as.data.frame(df)
  df <- st_as_sf(df) # should already have a geometry column
  wild_carcasses <- df %>%
    filter(!is.na(cluster)) %>%
    group_by(year, "carcID" = cluster) %>%
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
           datetime_il = lubridate::with_tz(datetime, tzone = "Israel"),
           carcType = "wild") # arbitrarily deciding that the min time of the first bout defines the "carcass time"
  return(wild_carcasses)
}

# prepare_data ------------------------------------------------------------
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
    cat("done with", i, "\n")
  }
  return(gps_all)
}

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
        group_by(individual_local_identifier) %>%
        slice(1) %>%
        ungroup() %>%
        arrange(timestamp)
      if(nrow(out) > 0){
        out$rownumber <- 1:nrow(out)
        return(out)
      }else{
        out <- data.frame(local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, individual_id = NA, individual_local_identifier = NA, location_long = NA, location_lat = NA, dist_to_carcass = NA, carcID = .y$carcID, geometry = NA, rownumber=  NA)
        return(out)
      }
    }else{
      out <- data.frame(local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, individual_id = NA, individual_local_identifier = NA, location_long = NA, location_lat = NA, dist_to_carcass = NA, carcID = .y$carcID, geometry = NA, rownumber=  NA)
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
        group_by(individual_local_identifier) %>%
        slice(1) %>%
        ungroup() %>%
        arrange(timestamp)
      if(nrow(out) > 0){
        out$rownumber <- 1:nrow(out)
        return(out)
      }else{
        out <- data.frame(local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, individual_id = NA, individual_local_identifier = NA, location_long = NA, location_lat = NA, dist_to_carcass = NA, carcID = .y$carcID, geometry = NA, rownumber=  NA)
        return(out)
      }
    }else{
      out <- data.frame(local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, individual_id = NA, individual_local_identifier = NA, location_long = NA, location_lat = NA, dist_to_carcass = NA, carcID = .y$carcID, geometry = NA, rownumber=  NA)
      return(out)
    }
  }) 
  return(firsts_see)
}

get_has_visits <- function(firsts){
  map_dbl(firsts, ~nrow(.x[!is.na(.x$individual_local_identifier),])) > 0
}

get_has_sightings <- function(firsts_see){
  map_dbl(firsts_see, ~nrow(.x[!is.na(.x$individual_local_identifier),])) > 0
}

get_has_enough_sightings <- function(firsts_see, min_sightings){
  map_dbl(firsts_see, ~nrow(.x[!is.na(.x$individual_local_identifier),])) > min_sightings
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
      ids <- .x$individual_local_identifier
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
      ids <- .x$individual_local_identifier
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

get_fl_weighted <- function(dat, dist, rp, spd){
  if(is.data.frame(dat)){
    if(nrow(dat) > 1){
      self_edges <- data.frame(ID1 = sort(unique(dat$individual_local_identifier)),
                               ID2 = sort(unique(dat$individual_local_identifier)),
                               sri = 0)
      dat$dateOnly_il <- lubridate::date(dat$timestamp_il)
      # NNN check back in previous analysis--do we need to remove the roost sites?
      out1 <- suppressMessages(getEdges_new(dat, roostPolygons = rp,
                                            speedThreshLower = spd,
                                            speedThreshUpper = NULL,
                                            consecThreshold = 1,
                                            idCol = "individual_local_identifier",
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
    }else if(nrow(dat) == 1){
      out <- "one row only"
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
      self_edges <- data.frame(ID1 = sort(unique(dat$individual_local_identifier)),
                               ID2 = sort(unique(dat$individual_local_identifier)),
                               value = 0)
      dat$dateOnly <- lubridate::date(dat$timestamp)
      out1 <- suppressMessages(getEdges_new(dat, roostPolygons = NULL,
                                            speedThreshLower = 4,
                                            speedThreshUpper = NULL,
                                            consecThreshold = 1,
                                            idCol = "individual_local_identifier",
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
        if(!any(net == "blank", na.rm = T) & !any(net == "one row only", na.rm = T)){
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
    ilvs_this_carcass <- map(nights_vec, ~ilvs %>% dplyr::select(individual_local_identifier, paste0("roost_night", .x), age_group) %>% rename("dist_roost" = 2))
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
    dplyr::rename("individual_local_identifier" = device_id,
                  "timestamp" = start) %>%
    dplyr::mutate(timestamp = case_when(nchar(timestamp) == 10 ~ paste0(timestamp, " 00:00:00"),
                                        .default = timestamp)) %>%
    dplyr::mutate(timestamp = lubridate::ymd_hms(timestamp))
  return(merged)
}

get_full <- function(gps, merged){
  full <- bind_rows(gps, mutate(merged, timestamp = lubridate::ymd_hms(timestamp)))
  full <- full %>%
    group_by(individual_local_identifier) %>%
    arrange(timestamp) %>%
    mutate(time_diff = as.numeric(difftime(lead(timestamp), timestamp, units = "secs"))) %>%
    ungroup()
  return(full)
}

prepare_gps_crossref <- function(full){
  full <- full %>%
    group_by(individual_local_identifier) %>%
    arrange(timestamp) %>%
    mutate(time_diff = as.numeric(difftime(lead(timestamp), timestamp, units = "secs"))) %>%
    ungroup()
  return(full)
}

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
                              carcass_data = NULL,
                              age_ilv = T) {
  
  gps$year <- lubridate::year(gps$date_il)
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
      distinct(individual_local_identifier) %>%
      pull(individual_local_identifier) %>%
      as.character()
  }
  
  # Get first sightings
  gps_after_in_sight <- gps %>%
    filter(time_since_carcass >= 0 & time_since_carcass <= sighting_time_max_hours) %>%
    filter((ground_speed > gps_spd & dist_to_carcass <= ddf) |
             (ground_speed <= gps_spd & dist_to_carcass <= dds))
  
  first_sightings <- gps_after_in_sight %>%
    group_by(individual_local_identifier) %>%
    arrange(time_since_carcass, timestamp) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(time_since_carcass)
  
  if(identify_seeds){
    first_sightings <- first_sightings %>%
      filter(!(individual_local_identifier %in% seeds))
  }
  
  n_found <- length(unique(first_sightings$individual_local_identifier))
  n_gps <- length(unique(gps$individual_local_identifier))
  prop_found <- n_found / n_gps
  
  all_indivs_sorted <- sort(unique(as.character(gps$individual_local_identifier)))
  
  if (age_ilv) {
    year = max(gps$year, na.rm = T)
    colname <- paste0("age_", year)
    age <- gps %>%
      sf::st_drop_geometry() %>%
      dplyr::filter(!is.na(individual_local_identifier)) %>%
      dplyr::select(individual_local_identifier, {{colname}}) %>%
      dplyr::distinct() %>%
      dplyr::mutate(individual_local_identifier = as.character(individual_local_identifier))
    
    age_ordered <- age[match(all_indivs_sorted, age$individual_local_identifier),]
    age_ilv_matrix <- cbind(age_ordered[[colname]])
  }
  
  oa_indivs <- first_sightings %>%
    arrange(time_since_carcass, timestamp) %>%
    pull(individual_local_identifier) %>%
    as.character()
  
  oa_nums <- match(oa_indivs, all_indivs_sorted)
  if (length(oa_nums) != length(oa_indivs)) {
    stop("Length of oa vecs does not match")
  }
  if (length(oa_nums) == length(unique(gps$individual_local_identifier)) && prop_found != 1) {
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
    gps %>% dplyr::filter(timestamp >= day_start, timestamp <= ts)
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
    seeds_vec = seeds_vec
  )
  
  if(age_ilv){
    next_list <- c(base_list, 
                   "age_ilv_matrix" = list(age_ilv_matrix))
  }else{next_list <- base_list}
  
  final_list <- c(next_list, 
                  gps_data_dynamic_hour_ranges)
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

nb_shortcut <- function(list, ddf, dds, gps_spd, stmh, stb, seeds, carcass_data_list, age_ilv = T){
  out <- purrr::map2(.x = list, .y = carcass_data_list, ~{
    prepare_nbda_data(gps = .x,
                      identify_seeds = seeds,
                      seed_time_before = stb,
                      ddf = ddf, dds = dds, # detection distances
                      gps_spd = gps_spd,
                      n_hours_gps_static = list(c(-720, -24)),
                      sighting_time_max_hours = stmh,
                      carcass_data = .y,
                      age_ilv = age_ilv)
  })
  return(out)
}

# XXX START HERE--THIS IS ANNOYING!!
remove_periods <- function(ww_file, dataset){
  toremove <- readxl::read_excel(ww_file, sheet = "periods_to_remove") %>%
    dplyr::filter(remove_end >= min(dataset$timestamp))
  if(!(any(toremove$Nili_id %in% dataset$Nili_id))){
    message("No Nili_ids from the periods_to_remove sheet are present in the dataset. No periods to remove.")
    return(dataset)
  }else{
    toremove_long <- toremove %>%
      dplyr::mutate(date_il = purrr::map2(remove_start, remove_end, seq, by = "day")) %>%
      dplyr::select(Nili_id, date_il) %>%
      tidyr::unnest(date_il) %>% dplyr::mutate(remove = T, date_il = as.Date(date_il))
    joined <- dplyr::left_join(dataset, toremove_long, by = c("Nili_id", "date_il"))
    dataset <- joined %>% dplyr::filter(is.na(remove)) %>% dplyr::select(-remove)
    return(dataset)
  }
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

NEW_get_roosts <- function(dat, id, ts = "timestamp_il", tz = "Israel"){
  roosts <- purrr::map(dat, ~NEW_get_roosts_df(df = .x, id = id, timestamp = ts, timestamp_tz = tz))
  roosts <- roosts %>%
    purrr::map(., ~st_as_sf(.x, crs = "WGS84", 
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

NEW_get_roosts_df <- function(df, id = "local_identifier", timestamp = "timestamp", 
                              x = "location_long", y = "location_lat", ground_speed = "ground_speed", 
                              speed_units = "m/s", buffer = 1, twilight = 61, morning_hours = c(0:12), 
                              night_hours = c(13:23), quiet = F,
                              timestamp_tz = "UTC") 
{
  if (!quiet) {
    cat("\nFinding roosts... this may take a while if your dataset is large.\n")
    start <- Sys.time()
  }
  checkmate::assertDataFrame(df)
  checkmate::assertCharacter(id, len = 1)
  checkmate::assertSubset(id, names(df))
  checkmate::assertCharacter(timestamp, len = 1)
  checkmate::assertSubset(timestamp, names(df))
  checkmate::assertCharacter(x, len = 1)
  checkmate::assertSubset(x, names(df))
  checkmate::assertCharacter(y, len = 1)
  checkmate::assertSubset(y, names(df))
  checkmate::assertCharacter(ground_speed, len = 1)
  checkmate::assertSubset(ground_speed, names(df))
  checkmate::assertCharacter(speed_units, len = 1)
  checkmate::assertSubset(speed_units, c("m/s", "km/h"))
  checkmate::assertNumeric(buffer, len = 1)
  checkmate::assertNumeric(twilight, len = 1)
  checkmate::assertNumeric(morning_hours, upper = 24, lower = 0)
  checkmate::assertNumeric(night_hours, upper = 24, lower = 0)
  checkmate::assertNumeric(df[[x]])
  checkmate::assertNumeric(df[[y]])
  checkmate::assertNumeric(df[[ground_speed]])
  if ("sf" %in% class(df)) {
    df <- df %>% sf::st_drop_geometry()
  }
  twilight_secs <- twilight * 60
  if (speed_units == "km/h") {
    df <- df %>% dplyr::mutate(`:=`({
      {
        ground_speed
      }
    }, round(.data[[ground_speed]]/3.6, 3)))
  }
  
  # XXX--this is bad because it assumes a particular format, but whatever.
  df[[timestamp]] <- as.POSIXct(df[[timestamp]], format = "%Y-%m-%d %H:%M:%S", 
                                tz = timestamp_tz)
  
  if (sum(is.na(df[[timestamp]])) > 0) {
    stop("Timestamp needs to be defined as.POSIXct (%Y-%m-%d %H:%M:%S)")
  }
  df$date <- as.Date(df[[timestamp]], tz = timestamp_tz) # this is going to be the date in the specified/local time zone
  indivs <- df %>% dplyr::group_by(.data[[id]]) %>% dplyr::group_split(.keep = T)
  roosts <- purrr::map(indivs, ~{
    temp.id <- unique(.x[[id]]) 
    id.df <- .x %>% dplyr::group_by(date) %>% 
      dplyr::arrange({{timestamp}}) %>%
      dplyr::mutate(row_id = dplyr::case_when(dplyr::row_number() == 
                                                1 ~ "first", dplyr::row_number() == max(dplyr::row_number()) ~ 
                                                "last"), hour = lubridate::hour(.data[[timestamp]])) %>% 
      dplyr::filter(row_id %in% c("first", "last")) %>% # only including the first and last point per date
      dplyr::ungroup() %>% 
      dplyr::mutate(day_diff = round(difftime(dplyr::lead(date), date, units = "days")))
    
    matrix <- as.matrix(id.df[, c(x, y)]) # matrix of locations
    leadMatrix <- as.matrix(cbind(dplyr::lead(id.df[[x]]), 
                                  dplyr::lead(id.df[[y]]))) # matrix of locations for the next point
    distances <- geosphere::distGeo(p1 = matrix, p2 = leadMatrix) * 
      0.001 %>% round(., 2) # getting the distance between the first and last point, which ends up being 0 because the values are really small and we're rounding to 2 digits
    id.df$dist_km <- distances
    id.df$dist_km[id.df$day_diff != 1] <- NA # set to NA if this point is on the same day as the following point or if it's more than 1 day different
    
    data <- id.df %>% dplyr::select(date, "lat" = {{y}}, "lon" = {{x}}) %>% as.data.frame() # the line after this was unnecessarily re-creating the date column so I fixed it.
    # data <- data.frame(date = as.Date(id.df[[timestamp]], tz = timestamp_tz), 
    #                    lat = id.df[[y]], lon = id.df[[x]]) 
    id.df$sunrise <- suncalc::getSunlightTimes(data = data, 
                                               keep = c("sunrise"), tz = timestamp_tz)$sunrise
    id.df$sunset <- suncalc::getSunlightTimes(data = data, 
                                              keep = c("sunset"), tz = timestamp_tz)$sunset
    id.df$sunrise_twilight <- id.df$sunrise + twilight_secs
    id.df$sunset_twilight <- id.df$sunset - twilight_secs
    id.df <- id.df %>% dplyr::mutate(daylight = ifelse(.data[[timestamp]] >= 
                                                         sunrise_twilight & .data[[timestamp]] <= sunset_twilight, 
                                                       "day", "night"))
    id.df <- id.df %>% 
      dplyr::mutate(is_roost = dplyr::case_when(row_id == "last" & daylight == "night" & hour %in% 
                                                  night_hours & ({{ground_speed}} <= 4 | is.na({{ground_speed}})) ~ 1, 
                                                row_id == "first" & daylight == "night" & 
                                                  hour %in% morning_hours & ({{ground_speed}} <= 4 | is.na({{ground_speed}})) ~ 1, 
                                                dist_km <= buffer ~ 1), # because of the NA insertion that we did earlier, this effectively means "if the point is NOT on the same day as the next point, and it's <= 1km from the following morning's point, then call it a roost"
                    roost_date = dplyr::case_when(is_roost == 1 & row_id == "last" ~ date, 
                                                  is_roost == 1 & row_id == "first" ~ date-days(1))#,
                    # roost_date = as.Date(roost_date) # why was this necessary? This should already be a date. And this function is dangerous because it puts it back to UTC.
      )
    temp.id.roosts <- dplyr::filter(id.df, is_roost == 1)
    temp.id.roosts <- temp.id.roosts %>% dplyr::group_by(roost_date) %>% 
      dplyr::arrange({
        {
          timestamp
        }
      }) %>% dplyr::filter(dplyr::row_number() == 1) %>%  # if there are multiple roosts per roost date, take the first one
      dplyr::ungroup() %>% dplyr::select(-c("row_id", 
                                            "hour"))
    temp.id.roosts <- temp.id.roosts %>% dplyr::select({
      {
        id
      }
    }, date, roost_date, sunrise, sunset, sunrise_twilight, 
    sunset_twilight, daylight, is_roost, location_lat, 
    location_long)
    return(temp.id.roosts)
  }) %>% purrr::list_rbind()
  if (!quiet) {
    end <- Sys.time()
    duration <- difftime(end, start, units = "secs")
    cat(paste0("Roost computation completed in ", duration, 
               " seconds."))
  }
  roosts <- roosts %>% dplyr::select({
    {
      id
    }
  }, date, roost_date, sunrise, sunset, sunrise_twilight, sunset_twilight, 
  daylight, is_roost, location_lat, location_long)
  return(roosts)
}

# tar_load(stn_gps_forroosts)
# test <- stn_gps_forroosts[[1]]
# glimpse(test)
# newroosts_test <- NEW_get_roosts_df(test, id = "individual_local_identifier", timestamp = "timestamp_il", x = "location_long", y = "location_lat", timestamp_tz = "Israel")
# oldroosts_test <- get_roosts_df(test, id = "individual_local_identifier")
# dim(newroosts_test)
# dim(oldroosts_test)
# new <- sf::st_as_sf(newroosts_test, coords = c("location_long", "location_lat"), crs = "WGS84")
# old <- sf::st_as_sf(oldroosts_test, coords = c("location_long", "location_lat"), crs = "WGS84")
# mapview(new, col.regions = "blue")+ mapview(old, col.regions = "red")

fix_names_ages <- function(gps_combined, ww_file){
  ww <- read_excel(ww_file, sheet = "all gps tags")
  # pull out just the names columns, nothing else, and remove any duplicates
  ww_tojoin <- ww %>% dplyr::select(Nili_id, Movebank_id, birth_year) %>% distinct() 
  
  # Prepare for join: are there any individuals in the `local_identifier` column of `joined0` that don't appear in the `Movebank_id` column of `ww_tojoin`?
  problems <- gps_combined %>% filter(!(individual_local_identifier %in% ww_tojoin$Movebank_id)) %>% pull(individual_local_identifier) %>% unique()
  
  # problems # check this against the who's who and try to make changes. The only problem is E60w.
  out <- gps_combined %>%
    left_join(ww_tojoin, by = c("individual_local_identifier" = "Movebank_id"))
  
  out <- out %>%
    mutate(Nili_id = case_when(is.na(Nili_id) & individual_local_identifier == "E60w" ~ "gili", .default = Nili_id),
           birth_year = case_when(individual_local_identifier == "E60w" ~ ww$birth_year[ww$Nili_id == "gili"], .default = birth_year))
  
  out <- out %>%
    mutate(age_2022 = 2022-birth_year,
           age_2023 = 2023-birth_year,
           age_2024 = 2024-birth_year)
  
  return(out)
}

get_leftroost <- function(ordered_df, threshold){
  checkmate::assert_data_frame(ordered_df)
  checkmate::assert_subset("in_a_roost", names(ordered_df))
  checkmate::assert_numeric(threshold)
  n <- nrow(ordered_df)
  rle_obj <- rle(as.numeric(ordered_df$in_a_roost))
  first_seq_out <- min(which(rle_obj$lengths >= threshold & rle_obj$values == 0))
  if(!is.infinite(first_seq_out)){
    first_point_out <- sum(rle_obj$lengths[1:(first_seq_out-1)])+1
    if(first_point_out > n){
      first_point_out <- NA
    }
  }else{
    first_point_out <- NA
  }
  return(first_point_out)
}

get_trajectories_sync_pair <- function(sync, trajs){
  
  # Fix type mismatch: sync$date_il is character, trajs$date_il is Date
  sync <- sync %>% dplyr::mutate(date_il = as.Date(date_il))
  
  # 1. Pull out coordinates once, drop geometry
  coords <- sf::st_coordinates(trajs)
  trajs_df <- trajs %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(X = coords[, "X"], Y = coords[, "Y"]) %>%
    dplyr::select(individual_local_identifier, date_il, timestamp_il, flight, X, Y)
  
  # 2. Add a row index to sync
  sync <- sync %>% dplyr::mutate(.pair_id = dplyr::row_number())
  
  # 3. Join trajs to sync twice
  t1 <- trajs_df %>%
    dplyr::rename(ID1 = individual_local_identifier,
                  flight1 = flight, X1 = X, Y1 = Y) %>%
    dplyr::inner_join(
      sync %>% dplyr::select(.pair_id, date_il, ID1),
      by = c("ID1", "date_il")
    )
  
  t2 <- trajs_df %>%
    dplyr::rename(ID2 = individual_local_identifier,
                  flight2 = flight, X2 = X, Y2 = Y) %>%
    dplyr::inner_join(
      sync %>% dplyr::select(.pair_id, date_il, ID2),
      by = c("ID2", "date_il")
    )
  
  # 4. Join the two on .pair_id + timestamp_il
  out <- dplyr::inner_join(
    t1, t2,
    by = c(".pair_id", "date_il", "timestamp_il")
  ) %>%
    dplyr::mutate(
      distance_m = sqrt((X1 - X2)^2 + (Y1 - Y2)^2)
    ) %>%
    dplyr::select(.pair_id, date_il, timestamp_il,
                  id1 = ID1, id2 = ID2, flight1, flight2, distance_m)
  
  # 5. Split back into a list, one element per row of sync
  outs <- vector("list", nrow(sync))
  split_out <- split(out, out$.pair_id)
  outs[as.integer(names(split_out))] <- split_out
  empty <- out[0, ]
  outs[sapply(outs, is.null)] <- list(empty)
  
  return(outs)
}

get_plot_data <- function(event_data){
  out <- event_data %>%
    filter(time > 0, time <= t_end) %>% # exclude demonstrators (time == 0) and censored (time > t_end)
    group_by(trial) %>%
    arrange(time, .by_group = TRUE) %>%
    mutate(
      cum_prop = row_number() / n(),
      type = "observed"
    ) %>%
    select(trial, time, cum_prop, type) %>%
    ungroup()
  
  # add in 0,0 starting point
  plot_data_obs <- bind_rows(
    out,
    out %>%
      distinct(trial) %>%
      mutate(time = 0, cum_prop = 0, type = "observed")
  ) %>%
    arrange(trial, time)
  return(plot_data_obs)
}
get_plot_data_ppc <- function(fit, data_list){
  draws_df <- posterior::as_draws_df(fit$draws(variables = "acquisition_time", inc_warmup = F))
  ppc_long <- draws_df %>%
    dplyr::select(starts_with("acquisition_time[")) %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = c("trial", "ind"),
      names_pattern = "acquisition_time\\[(\\d+),(\\d+)\\]",
      values_to = "time"
    ) %>%
    dplyr::mutate(
      trial = as.integer(trial),
      ind = as.integer(ind),
      draw = rep(1:(nrow(draws_df)), 
                 each = length(unique(.$trial)) * length(unique(.$ind)))
    )
  # thin for plotting
  sample_idx <- sample(c(1:max(ppc_long$draw)), 100)
  ppc_long <- ppc_long %>% dplyr::filter(draw %in% sample_idx)
  
  # build cumulative curves per draw
  plot_data_ppc <- ppc_long %>%
    dplyr::group_by(draw, trial, time) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    dplyr::group_by(draw, trial) %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(cum_prop = cumsum(n) / data_list$Q)
  
  # add in 0,0 starting point
  out <- dplyr::bind_rows(
    plot_data_ppc,
    plot_data_ppc %>%
      dplyr::distinct(trial, draw) %>%
      dplyr::mutate(time = 0, cum_prop = 0, type = "ppc")
  ) %>%
    dplyr::arrange(trial, time)
  return(out)
}

get_seeds <- function(gps, ddf, dds, gps_spd, time_col, stb_mins){
  time_window_hrs <- stb_mins/60
  gps %>%
    filter(.data[[time_col]] >= -time_window_hrs &
             .data[[time_col]] <= 0 & 
             ((ground_speed > gps_spd & dist_to_carcass <= ddf) |
                (ground_speed <= gps_spd & dist_to_carcass <= dds))) %>%
    pull(individual_local_identifier) %>%
    as.character() %>%
    unique()
}

get_first_sightings <- function(gps, stmh, gps_spd, ddf, dds, seeds){
  first_sightings <- gps %>%
    filter(time_since_carcass >= 0 & time_since_carcass <= stmh) %>%
    filter((ground_speed > gps_spd & dist_to_carcass <= ddf) |
             (ground_speed <= gps_spd & dist_to_carcass <= dds)) %>%
    group_by(individual_local_identifier) %>%
    arrange(time_since_carcass, timestamp) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(time_since_carcass) %>%
    filter(!(individual_local_identifier %in% seeds))
  return(first_sightings)
}

format_event_data <- function(first_sightings, seeds, all_indivs_sorted, time_col = "daytime_since_carcass", carc){
  event_data <- first_sightings %>%
    st_drop_geometry() %>%
    dplyr::mutate(trial = carc$carcID[1]) %>%
    dplyr::select(individual_local_identifier, trial, all_of(time_col)) %>%
    mutate(time = as.numeric(.data[[time_col]])*60*60) %>%
    rename("id" = individual_local_identifier) %>%
    mutate(t_end = max(time)) %>% # XXX START HERE WITH CONVERSION BETWEEN HOURS_SINCE_CARCASS AND TIME!! NEED TO SET MAX(TIME) AS SOMETHING ELSE
    select(id, trial, time, t_end)
  
  #time: Integer or float values indicating the time (TADA) or order (OADA) in which the individual was recorded as first informed/knowledgable. If an individual had the event occur prior to the start of the observation period (e.g. pre-trained demonstrator), set as 0. These left censored individuals will not contribute to the likelihood calculation. 
  # add the seeds back in (pre-trained demonstrators)
  if(length(seeds) > 0){
    event_data <- bind_rows(data.frame(id = seeds, trial = carc$carcID[1], t_end = max(event_data$time), time = 0),
                            event_data)
  }
  
  #If an individual never learned during the observation period, set its value tend+1. These will be treated as right-censored individuals in the likelihood calculation.
  # okay so for these, we need to figure out if there are any individuals in the gps dataset that don't appear in the first sightings.
  never_learned <- all_indivs_sorted[!(all_indivs_sorted %in% event_data$id)]
  if(length(never_learned) > 0){
    t_end <- event_data$t_end[1]
    event_data <- bind_rows(event_data,
                            data.frame(id = never_learned, 
                                       trial = carc$carcID[1], 
                                       t_end = t_end, 
                                       time = t_end + 1))
  }
  event_data <- event_data %>%
    arrange(time, id) %>%
    mutate(across(c(time, t_end), as.integer)) # should be INTEGER, not NUMERIC, so the code will work properly
  
  return(event_data)
}

format_event_data_new <- function(first_sightings, seeds, all_indivs_sorted, time_col = "daytime_since_carcass", carc, gps_fornetwork){
  t_end <- max(gps_fornetwork$time)
  
  event_data <- first_sightings %>%
    st_drop_geometry() %>%
    dplyr::mutate(trial = carc$carcID[1]) %>%
    dplyr::select(individual_local_identifier, trial, all_of(time_col)) %>%
    mutate(time = as.numeric(.data[[time_col]])*60*60) %>%
    rename("id" = individual_local_identifier) %>%
    mutate(t_end = t_end) %>% 
    select(id, trial, time, t_end)
  
  #time: Integer or float values indicating the time (TADA) or order (OADA) in which the individual was recorded as first informed/knowledgable. If an individual had the event occur prior to the start of the observation period (e.g. pre-trained demonstrator), set as 0. These left censored individuals will not contribute to the likelihood calculation. 
  # add the seeds back in (pre-trained demonstrators)
  if(length(seeds) > 0){
    event_data <- bind_rows(data.frame(id = seeds, trial = carc$carcID[1], t_end = t_end, time = 0),
                            event_data)
  }
  
  #If an individual never learned during the observation period, set its value tend+1. These will be treated as right-censored individuals in the likelihood calculation.
  # okay so for these, we need to figure out if there are any individuals in the gps dataset that don't appear in the first sightings.
  never_learned <- all_indivs_sorted[!(all_indivs_sorted %in% event_data$id)]
  if(length(never_learned) > 0){
    event_data <- bind_rows(event_data,
                            data.frame(id = never_learned, 
                                       trial = carc$carcID[1], 
                                       t_end = t_end, 
                                       time = t_end + 1))
  }
  event_data <- event_data %>%
    arrange(time, id) %>%
    mutate(across(c(time, t_end), as.integer)) # should be INTEGER, not NUMERIC, so the code will work properly
  
  return(event_data)
}

get_daylight_hours <- function(gps, carc){
  suntimes <- suncalc::getSunlightTimes(date = sort(unique(gps$date_il)), lat = 31.434306, lon = 34.991889, keep = c("sunrise", "sunset"), tz = "Israel") %>% 
    dplyr::select("date_il" = date, sunrise, sunset)
  
  gps <- dplyr::left_join(gps, suntimes, by = "date_il")
  gps <- gps %>% dplyr::filter(timestamp_il >= sunrise & timestamp_il <= sunset)
  
  night_df <- suntimes %>%
    dplyr::filter(date_il >= lubridate::date(carc$date)) %>% # only stuff since the carcass
    dplyr::mutate(prev_sunset = dplyr::lag(sunset),
                  night_hrs = as.numeric(difftime(sunrise, prev_sunset, units = "hours")),
                  cumul_night_hrs = cumsum(ifelse(is.na(night_hrs), 0, night_hrs)) + night_hrs*0,
                  cumul_night_hrs = tidyr::replace_na(cumul_night_hrs, 0))
  
  gps <- gps %>%
    dplyr::left_join(dplyr::select(night_df, date_il, cumul_night_hrs), by = "date_il") %>%
    dplyr::mutate(daytime_since_carcass = dplyr::case_when(time_since_carcass >= 0 ~ as.numeric(time_since_carcass)-cumul_night_hrs,
                                                           .default = NA)) %>%
    dplyr::arrange(timestamp_il)
  return(gps)
}


get_asocial <- function(ed, dl, it = 1000){
  if(!is.null(dl)){
    mod <- STbayes::generate_STb_model(dl, gq = T, est_acqTime = T, model_type = "asocial")
    asocial_fit <- fit_STb(dl, mod, iter = it)
  }else{
    mod <- NULL
    asocial_fit <- NULL
  }
  return(list("mod" = mod, "fit" = asocial_fit))
}

get_summs_curves <- function(ed, dl, it = 1000){
  if(!is.null(dl)){
    # Generate asocial model
    mod <- STbayes::generate_STb_model(dl, gq = T, est_acqTime = T, model_type = "asocial")
    # Fit asocial model
    asocial_fit <- fit_STb(dl, mod, iter = it)
    
    # Generate the model
    mod <- STbayes::generate_STb_model(dl, gq = T, est_acqTime = T)
    # Fit the  model
    fit <- STbayes::fit_STb(dl, mod, iter = it)
    
    # compare social vs. asocial model
    loo_output <- STb_compare(fit, asocial_fit, method="loo-psis")
    comparison_df <- as.data.frame(loo_output$comparison)
    comparison_df$model <- rownames(comparison_df)
    
    pareto_df <- as.data.frame(loo_output$pareto_diagnostics)
    
    
    
    
    
    
    # Get the summary
    summ <- STbayes::STb_summary(fit)
    # Get the ppc curves
    # create cumulative count of events
    ed <- ed %>%
      group_by(trial) %>%
      mutate(n_trial = n())
    
    plot_data_obs <- ed %>%
      filter(time > 0, time <= t_end) %>% # exclude demonstrators (time == 0) and censored (time > t_end)
      group_by(trial) %>%
      arrange(time, .by_group = TRUE) %>%
      mutate(
        cum_prop = row_number() / n_trial, # this denominator needs to be the number of individuals per trial
        type = "observed"
      ) %>%
      select(trial, time, cum_prop, type) %>%
      ungroup()
    
    # add in 0,0 starting point
    plot_data_obs <- bind_rows(
      plot_data_obs,
      plot_data_obs %>%
        distinct(trial) %>%
        mutate(time = 0, cum_prop = 0, type = "observed")
    ) %>%
      arrange(trial, time)
    
    # extract draws of predicted acqtime
    draws_df <- posterior::as_draws_df(fit$draws(variables = "acquisition_time", inc_warmup = FALSE))
    
    # pivot longer
    ppc_long <- draws_df %>%
      select(starts_with("acquisition_time[")) %>%
      pivot_longer(
        cols = everything(),
        names_to = c("trial", "ind"),
        names_pattern = "acquisition_time\\[(\\d+),(\\d+)\\]",
        values_to = "time"
      ) %>%
      mutate(
        trial = as.integer(trial),
        ind = as.integer(ind),
        draw = rep(1:(nrow(draws_df)),
                   each = length(unique(.$trial)) * length(unique(.$ind))
        )
      )
    
    
    # thin sample for plotting
    sample_idx <- sample(c(1:max(ppc_long$draw)), 100)
    ppc_long <- ppc_long %>% filter(draw %in% sample_idx)
    
    # build cumulative curves per draw
    # same as before, we need a way to reference the number of individuals in each trial
    ppc_long <- ppc_long %>%
      group_by(draw, trial) %>%
      mutate(n_trial = n())
    summary(ppc_long)
    # we also need to remove individuals predicted as censored
    ppc_long <- ppc_long %>%
      filter(time > -1)
    # create cumulative curves
    plot_data_ppc <- ppc_long %>%
      group_by(draw, trial, time) %>%
      summarise(n = n(), n_trial = first(n_trial), .groups = "drop") %>%
      group_by(draw, trial) %>%
      arrange(time) %>%
      mutate(cum_prop = cumsum(n) / n_trial)
    
    # add in 0,0 starting point
    plot_data_ppc <- bind_rows(
      plot_data_ppc,
      plot_data_ppc %>%
        distinct(trial, draw) %>%
        mutate(time = 0, cum_prop = 0, type = "ppc")
    ) %>%
      arrange(trial, time)
    return(list("obs" = plot_data_obs, "pred" = plot_data_ppc, "summ" = summ, "asoc_comparison" = comparison_df, "pareto_df" = pareto_df))
  }else{
    return(NULL)
  }
}

arrange_roost_nets <- function(r, ind, rt){
  e <- getRoostEdges(r, mode = "distance", distThreshold = rt, idCol = "individual_local_identifier", return = "edges") %>% select(-distance) %>%
    mutate(roost_together = 1)
  dates <- unique(e$date)
  df <- expand_grid("ID1" = ind, "ID2" = ind)
  toadd <- map(dates, ~mutate(df, date = .x)) %>% purrr::list_rbind() %>% mutate(roost_together = 0)
  all <- bind_rows(e, toadd) %>%
    arrange(date, ID1, ID2, desc(roost_together)) %>%
    group_by(date, ID1, ID2) %>%
    slice(1) %>%
    rename("focal" = ID1, "other" = ID2)
  return(all)
}


# Functions for running STbayes models ------------------------------------

# Helper funs
get_asocial <- function(x){
  if(!is.null(x)){
    mod <- suppressMessages(STbayes::generate_STb_model(x, gq = T, est_acqTime = T, model_type = "asocial"))
    return(mod)
  }else{return(NULL)}}

get_social <- function(x){
  if(!is.null(x)){
    mod <- suppressMessages(STbayes::generate_STb_model(x, gq = T, est_acqTime = T))
    return(mod)
  }else{return(NULL)}
}

fit_model <- function(mod, dl, n_iter = 1000){
  if(!is.null(mod)){
    social_fit <- fit_STb(dl, mod, iter = n_iter)
    return(social_fit)
  }else{return(NULL)}
}

savefit <- function(fit, idx, folder, prefix, type){
  nm <- paste0(folder, "/fit_", prefix, "_", str_pad(as.character(idx), width = 3, side = "left", pad = "0"))
  if(!is.null(fit)){
    STb_save(fit, output_dir = paste0("data/saved_fits/", type, "/"), name = nm)
  }else{
    write_rds(NULL, file = paste0("data/saved_fits/", type, "/", nm, ".rds"))
  }
}

# Get data for plotting ppc curves for models
get_plotdata <- function(event_data, model_fit){
  
  if(!is.null(event_data) & !is.null(model_fit)){
    # create cumulative count of events
    ed <- event_data %>% group_by(trial) %>% mutate(n_trial = n())
    
    plot_data_obs <- ed %>%
      filter(
        #time > 0, # Remove this--we want to include the demonstrators in the obs line, since they're included in the draws!
        time <= t_end) %>% # exclude demonstrators (time == 0) and censored (time > t_end)
      group_by(trial) %>%
      arrange(time, .by_group = TRUE) %>%
      mutate(
        cum_prop = row_number() / n_trial, # this denominator needs to be the number of individuals per trial
        type = "observed"
      ) %>%
      select(trial, time, cum_prop, type) %>%
      ungroup()
    
    # If there's not already a value for 0, add in 0,0 starting point
    if(!(0 %in% plot_data_obs$time)){
      plot_data_obs <- bind_rows(
        plot_data_obs,
        plot_data_obs %>%
          distinct(trial) %>%
          mutate(time = 0, cum_prop = 0, type = "observed")
      ) %>%
        arrange(trial, time)
    }
    
    # extract draws of predicted acqtime
    draws_df <- posterior::as_draws_df(model_fit$draws(variables = "acquisition_time", inc_warmup = FALSE))
    
    # pivot longer
    ppc_long <- draws_df %>%
      select(starts_with("acquisition_time[")) %>%
      pivot_longer(
        cols = everything(),
        names_to = c("trial", "ind"),
        names_pattern = "acquisition_time\\[(\\d+),(\\d+)\\]",
        values_to = "time"
      ) %>%
      mutate(
        trial = as.integer(trial),
        ind = as.integer(ind),
        draw = rep(1:(nrow(draws_df)),
                   each = length(unique(.$trial)) * length(unique(.$ind))
        )
      )
    
    
    # thin sample for plotting
    sample_idx <- sample(c(1:max(ppc_long$draw)), 100)
    ppc_long <- ppc_long %>% filter(draw %in% sample_idx)
    
    # build cumulative curves per draw
    # same as before, we need a way to reference the number of individuals in each trial
    ppc_long <- ppc_long %>%
      group_by(draw, trial) %>%
      mutate(n_trial = n())
    summary(ppc_long)
    # we also need to remove individuals predicted as censored
    ppc_long <- ppc_long %>%
      filter(time > -1)
    # create cumulative curves
    plot_data_ppc <- ppc_long %>%
      group_by(draw, trial, time) %>%
      summarise(n = n(), n_trial = first(n_trial), .groups = "drop") %>%
      group_by(draw, trial) %>%
      arrange(time) %>%
      mutate(cum_prop = cumsum(n) / n_trial)
    
    # add in 0,0 starting point
    plot_data_ppc <- bind_rows(
      plot_data_ppc,
      plot_data_ppc %>%
        distinct(trial, draw) %>%
        mutate(time = 0, cum_prop = 0, type = "ppc")
    ) %>%
      arrange(trial, time)
    
    return(list("obs" = plot_data_obs, "pred" = plot_data_ppc))
    
  }else{NULL}
}

# Make ppc curveplots
get_curveplots <- function(plot_data, cid){
  if(!is.null(plot_data)){
    p <- ggplot(mapping = aes(x = time, y = cum_prop))+
      geom_line(
        data = plot_data$pred, aes(group = interaction(draw, trial)), alpha = 0.1)+
      geom_line(
        data = plot_data$obs, linewidth = 1)+
      labs(x = "Time", y = "Cumulative proportion informed", title = cid)+
      theme_minimal()
    return(p)
  }else{return(NULL)}
}

# Identify individual informed status with respect to each carcass
get_vulture_lines <- function(x){
  if(nrow(x) > 0){
    x %>%
      move2::select_track_data(individual_local_identifier, date_il, day, id) %>%
      move2::mt_set_track_id("id") %>%
      move2::mt_track_lines() %>%
      sf::st_transform(32636)
  }else{
    NULL
  }
}


# Co-departures from roost ------------------------------------------------
### tocombine: 1
join_roosts_gps <- function(gps, roosts, roostPolygons){
  # See which roost polygon each GPS point falls into (if any). (Assumes no overlapping roost polys)
  roostID_gps <- gps %>% sf::st_as_sf() %>%
    sf::st_transform(32636) %>%
    sf::st_intersects(roostPolygons) %>%
    as.numeric()
  gps_updated <- gps %>% mutate(roostID_gps = roostID_gps)
  
  roosts_tojoin <- roosts %>% select(individual_local_identifier, roost_date, roostID) %>%
    bind_cols(sf::st_coordinates(roosts)) %>%
    sf::st_drop_geometry() %>%
    rename("roost_X" = X, "roost_Y" = Y)# roost loc and polygon ID (if any) per vulture per night. Includes non-polygon roosts.
  
  gps_joined <- gps_updated %>%
    mutate(roost_date = date_il - lubridate::days(1)) %>%
    left_join(roosts_tojoin, by = c("individual_local_identifier", "roost_date")) %>%
    mutate(in_a_roost = !is.na(roostID_gps)) # joined roosts to GPS data to prep for determining departures
  
  # CALCULATE DEPARTURES from known roosts only---------------------------------------------
  gps_joined_knownroost <- gps_joined %>%
    filter(!is.na(roostID)) # only roost polygons
  rm(gps_updated) # don't need these anymore; let's clear some memory
  rm(gps_joined)
  
  indiv_date_list <- gps_joined_knownroost %>%
    arrange(date_il, individual_local_identifier, timestamp_il) %>%
    group_by(date_il, individual_local_identifier) %>%
    group_split(.keep = T)
  
  # identify points when they "left" the roost, looking for 2 consecutive points
  leftpoints <- purrr::map_dbl(indiv_date_list, ~get_leftroost(.x, threshold = 2))
  
  data_rejoined <- purrr::map2(indiv_date_list, leftpoints, ~{
    .x$left_roost <- FALSE
    if(!is.na(.y)){.x$left_roost[.y] <- TRUE}
    return(.x)}) %>% data.table::rbindlist() %>%
    as.data.frame() %>%
    sf::st_as_sf(crs = 32636)
  
  return(data_rejoined)
}

get_departures <- function(data_rejoined){
  leaving_points <- data_rejoined %>% filter(left_roost)
  leaving_points_dates <- leaving_points %>%
    group_by(date_il) %>%
    group_split(.keep = T)
  
  dates <- purrr::map_chr(leaving_points_dates, ~as.character(.x$date_il[1]))
  roost_mats <- purrr::map(leaving_points_dates, ~{
    mat <- outer(.x$roostID, .x$roostID, FUN = "==")*1
    rownames(mat) <- .x$individual_local_identifier
    colnames(mat) <- .x$individual_local_identifier
    return(mat)
  }) %>%
    setNames(., dates)
  
  roost_mats_long <- purrr::map(roost_mats, ~{as.data.frame(.x) %>% rownames_to_column("ID1") %>% pivot_longer(cols = -ID1, names_to = "ID2", values_to = "same_roost")}) %>% setNames(., dates)
  
  roost_mats_same_whichroost <- purrr::list_rbind(roost_mats_long, names_to = "date_il") %>%
    mutate(date_il = lubridate::ymd(date_il)) %>%
    left_join(leaving_points, by = c("ID1" = "individual_local_identifier", "date_il")) %>%
    filter(same_roost == 1)
  
  difftime_mats <- purrr::map(leaving_points_dates, ~{
    mat <- outer(.x$timestamp_il, .x$timestamp_il,
                 function(t1, t2) as.numeric(abs(difftime(t1, t2, units = "mins"))))
    rownames(mat) <- .x$individual_local_identifier
    colnames(mat) <- .x$individual_local_identifier
    return(mat)}) %>% setNames(., dates)
  
  difftime_mats_long <- purrr::map(difftime_mats, ~{
    as.data.frame(.x) %>%
      rownames_to_column("ID1") %>% pivot_longer(cols = -ID1, names_to = "ID2", values_to = "time_diff_min")
  }) %>%
    setNames(., dates)
  
  both <- purrr::map2(roost_mats_long, difftime_mats_long, ~dplyr::left_join(.x, .y, by = c("ID1", "ID2"))) %>% setNames(., dates)
  
  departures <- purrr::map(both, ~{
    .x %>% dplyr::filter(same_roost == 1) %>%
      dplyr::select(-same_roost) %>% filter(ID1 < ID2)
  }) %>% setNames(., dates)
  
  departures_df <- purrr::list_rbind(departures, names_to = "date_il") %>%
    mutate(year = lubridate::year(date_il)) %>%
    left_join(st_drop_geometry(data_rejoined) %>% mutate(date_il = as.character(date_il)) %>% select(date_il, "ID1" = "individual_local_identifier", roostID) %>% distinct())
  
  return(departures_df)
}

get_after_departures <- function(data_rejoined, gps_spd, sync_departures_df){
  data_split_years <- data_rejoined %>%
    mutate(year = case_when(date_il < lubridate::ymd("2023-01-01") ~ 2022, date_il > lubridate::ymd("2023-01-01") & date_il < lubridate::ymd("2023-07-01") ~ 2023, date_il > lubridate::ymd("2023-07-01") ~ 2024)) %>%
    arrange(year) %>%
    group_by(year) %>%
    group_split(.keep = T)
  
  mv <- purrr::map(data_split_years, ~{
    out <- move2::mt_as_move2(
      .x, time = "timestamp_il", track_id = "individual_local_identifier",
      crs = st_crs(data_rejoined)
    )
    return(out[order(mt_track_id(out)),])}
  )
  
  interpolated_10min <- purrr::map(mv, ~move2::mt_interpolate(
    .x[!sf::st_is_empty(.x), ],
    time = seq(
      as.POSIXct(min(.x$date_il, na.rm = T)),
      as.POSIXct(max(.x$date_il, na.rm = T)+lubridate::days(1)), "10 mins"
    ),
    max_time_lag = units::as_units(1, "hours"),
    omit = TRUE
  ) %>%
    mutate(interp = T) %>%
    bind_rows(mutate(.x[!sf::st_is_empty(.x),], interp = F)) %>%
    arrange(individual_local_identifier, timestamp_il) %>%
    ungroup())
  
  interpolated_tidied <- purrr::map(interpolated_10min, ~{
    .x %>% 
      dplyr::select(individual_local_identifier, date_il, year, timestamp_il, ground_speed, interp, roost_X, roost_Y, roostID, roostID_gps, in_a_roost, left_roost) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(flight = ground_speed > gps_spd) %>% 
      arrange(individual_local_identifier, timestamp_il) %>% 
      tidyr::fill(date_il) %>% 
      dplyr::group_by(individual_local_identifier, date_il) %>% 
      tidyr::fill(flight) %>% 
      tidyr::fill(roost_X) %>% 
      tidyr::fill(roost_Y) %>% 
      tidyr::fill(roostID) %>% 
      tidyr::fill(left_roost) %>% 
      dplyr::ungroup()})
  
  after_departure <- purrr::map(interpolated_tidied, ~{.x %>%
      dplyr::group_by(individual_local_identifier, date_il) %>%
      dplyr::mutate(after = cumsum(left_roost)) %>%
      dplyr::filter(after > 0) %>%
      dplyr::ungroup() %>% dplyr::select(-after)})
  return(after_departure)
}
  
get_trajectories_sync <- function(after_departure_interp_only, sync_departures_df){
  sync_departures_list <- sync_departures_df %>%
    group_by(year) %>%
    group_split(.keep = T)
  
  trajectories_sync_list_2022 <- get_trajectories_sync_pair(sync_departures_list[[1]], after_departure_interp_only[[1]])
  trajectories_sync_list_2023 <- get_trajectories_sync_pair(sync_departures_list[[2]], after_departure_interp_only[[2]])
  trajectories_sync_list_2024 <- get_trajectories_sync_pair(sync_departures_list[[3]], after_departure_interp_only[[3]]) #  XXXX START HERE 7/4/26
  
  trajectories_sync_2022 <- purrr::list_rbind(trajectories_sync_list_2022)
  trajectories_sync_2023 <- purrr::list_rbind(trajectories_sync_list_2023)
  trajectories_sync_2024 <- purrr::list_rbind(trajectories_sync_list_2024)
  
  trajectories_sync <- purrr::list_rbind(setNames(list(trajectories_sync_2022, trajectories_sync_2023, trajectories_sync_2024), c("2022", "2023", "2024")), names_to = "year") %>%
    mutate(date_il = lubridate::date(timestamp_il))
  return(trajectories_sync)
}

get_first_sightings_per_date <- function(gps_diffusion, gps_diffusion_wild, gps_spd, dds, ddf, trajectories_sync){
  # 1. Get detections of each carcass by each individual on each day
  sightings <- purrr::list_rbind(gps_diffusion) %>% 
    bind_rows(purrr::list_rbind(gps_diffusion_wild)) %>%
    arrange(individual_local_identifier, timestamp_il, carcID) %>%
    filter((ground_speed <= gps_spd & dist_to_carcass <= dds)|(ground_speed > gps_spd & dist_to_carcass <= ddf)) %>%
    rename("id" = individual_local_identifier) %>%
    mutate(date_il = lubridate::date(timestamp_il))
  
  # 2. Find pairs that go to the same carcass on the same day
  # ID1 | ID2 | carcID | date
  trajectories_sync %>% select(id1, id2) %>% distinct() %>% nrow() # 4600 dyads
  all <- unique(c(trajectories_sync$id1, trajectories_sync$id2))
  all_possible_dyads <- expand_grid("id1" = all, "id2" = all) %>% filter(id1 < id2)
  
  all_dates <- unique(sightings$date_il)
  
  sightings_carc_date <- sightings %>%
    group_by(carcID, date_il) %>%
    group_split(.keep = T)
  
  ## Get only the first time that each individual sighted the carcass on the given date
  first_sightings_carc_date <- map(sightings_carc_date, ~{
    out <- .x %>%
      arrange(id, timestamp_il) %>%
      group_by(id) %>%
      slice(1) %>%
      select(date_il, carcID, id, timestamp_il, time_since_carcass, daytime_since_carcass, dist_to_carcass, ground_speed, geometry) %>%
      ungroup()
    return(out)
  })
  return(first_sightings_carc_date) # 577 unique carcasses and dates
}

get_arrival_dyads <- function(first_sightings_carc_date, stn_carcs_modified, wild_carcs, informed){
  # Get diffs for the first sighting per day per carcass for each pair of individuals
  carc_date_pairs <- map(first_sightings_carc_date, ~{
    base_df <- .x %>% st_drop_geometry() %>% mutate(geometry = .x$geometry)
    pairs <- inner_join(base_df, base_df, by = c("carcID", "date_il"), suffix = c("_1", "_2")) %>%
      filter(id_1 < id_2) %>%
      rename("id1" = id_1, "id2" = id_2) %>%
      mutate(
        timestamp_diff          = abs(as.numeric(difftime(timestamp_il_1, timestamp_il_2, units = "secs"))),
        time_since_carcass_diff    = abs(time_since_carcass_1 - time_since_carcass_2),
        daytime_since_carcass_diff = abs(daytime_since_carcass_1 - daytime_since_carcass_2),
        dist_to_carcass_diff       = abs(dist_to_carcass_1 - dist_to_carcass_2),
        ground_speed_diff       = abs(ground_speed_1 - ground_speed_2),
        dist_apart              = st_distance(st_sfc(geometry_1), st_sfc(geometry_2), by_element = TRUE)
      ) %>%
      select(-geometry_1, -geometry_2)
    return(pairs)
  }, .progress = T)

  arrival_dyads <- purrr::list_rbind(carc_date_pairs) %>%
    select(date_il, carcID, id1, id2, timestamp_il_1, timestamp_il_2, timestamp_diff, time_since_carcass_1, time_since_carcass_2, time_since_carcass_diff, daytime_since_carcass_1, daytime_since_carcass_2, daytime_since_carcass_diff, dist_to_carcass_1, dist_to_carcass_2, dist_to_carcass_diff, dist_apart, ground_speed_1, ground_speed_2, ground_speed_diff)
  
  arrival_dyads <- purrr::list_rbind(stn_carcs_modified) %>% bind_rows(purrr::list_rbind(wild_carcs)) %>%
    select(carcID, "carcass_date" = date) %>% right_join(arrival_dyads, by = "carcID") %>%
    mutate(day = as.numeric(difftime(date_il, carcass_date, units = "days")))
  
  arrival_dyads <- arrival_dyads %>%
    mutate(carcID = as.character(carcID)) %>%
    left_join(informed, by = c("carcID", "day", "id1" = "id")) %>%
    rename("id1_informed_prev" = informed_previous) %>%
    left_join(informed, by = c("carcID", "day", "id2" = "id")) %>%
    rename("id2_informed_prev" = informed_previous) %>%
    mutate(dyad_type = case_when(id1_informed_prev & id2_informed_prev ~ "Both informed",
                                 id1_informed_prev & !id2_informed_prev ~ "One informed",
                                 !id1_informed_prev & id2_informed_prev ~ "One informed",
                                 !id1_informed_prev & !id2_informed_prev ~ "Neither informed",
                                 .default = NA))
  
  return(arrival_dyads)
}

get_informed <- function(informed_stn, informed_wild, stn_carcs_modified, wild_carcs){
  names(informed_stn) <- map(stn_carcs_modified, "carcID")
  names(informed_wild) <- map(wild_carcs, "carcID")
  
  islong <- purrr::list_rbind(informed_stn, names_to = "carcID") %>%
    pivot_longer(cols = starts_with("s"), names_to = "day", values_to = "sighted") %>%
    mutate(day = as.numeric(str_remove(day, "s"))) %>%
    arrange(carcID, id, day) %>%
    group_by(carcID, id) %>%
    mutate(informed = cumsum(sighted) > 0) %>%
    select(-sighted) %>%
    mutate(informed_previous = lag(informed)) %>%
    select(-informed) %>%
    ungroup()
  
  iwlong <- purrr::list_rbind(informed_wild, names_to = "carcID") %>%
    pivot_longer(cols = starts_with("s"), names_to = "day", values_to = "sighted") %>%
    mutate(day = as.numeric(str_remove(day, "s"))) %>%
    arrange(carcID, id, day) %>%
    mutate(informed = cumsum(sighted) > 0) %>%
    select(-sighted) %>%
    mutate(informed_previous = lag(informed)) %>%
    select(-informed) %>%
    ungroup()
  
  informed <- bind_rows(islong, iwlong) %>%
    mutate(informed_previous = case_when(is.na(informed_previous) & day == 0 ~ F, .default = informed_previous))
  return(informed)
}

get_max_displs <- function(after_departure_interp_only){
  # Calculate displacements after departures from roost
  adios <- purrr::map(after_departure_interp_only, ~{
    df <- .x
    df %>%
      group_by(individual_local_identifier, date_il) %>%
      mutate(displacement = {
        pts <- st_geometry(df)[cur_group_rows()]
        as.numeric(st_distance(pts, pts[1]))  # [1] not [[1]] to keep geometry
      }) %>%
      ungroup()
  }, .progress = T)
  
  # Determine max displacement per individual per day
  max_displs <- setNames(purrr::map(adios, ~{
    .x %>% st_drop_geometry() %>% group_by(individual_local_identifier, date_il) %>%
      summarize(max_displacement_m = max(displacement, na.rm = T)) %>%
      mutate(max_displacement_km = max_displacement_m/1000)
  }), c("2022", "2023", "2024")) %>% purrr::list_rbind(names_to = "year") %>% mutate(year = factor(year))
  return(max_displs)
}

get_dyad_flight_stats <- function(trajectories_sync, sync_departures_df, ddf){
  joined <- trajectories_sync %>%
    mutate(year = as.numeric(year)) %>%
    left_join(mutate(sync_departures_df, date_il = lubridate::ymd(date_il)), by = c("year", "date_il", "id1" = "ID1", "id2" = "ID2")) %>% rename("departure_time_diff_min" = "time_diff_min") %>%
    mutate(hour = lubridate::hour(timestamp_il),
           distance_km = distance_m/1000)
  
  daylight_hours <- c(7:18)
  
  joined_daylight <- joined %>% filter(hour %in% daylight_hours) %>%
    mutate(flight_status = case_when(flight1 & flight2 ~ "both",
                                     (flight1 & !flight2)|(!flight1 & flight2) ~ "one",
                                     !flight1 & !flight2 ~ "zero"))
  
  # Get flight stats for co-departing dyads
  flight_stats <- joined_daylight %>%
    group_by(year, date_il, id1, id2) %>%
    summarize(n_pts = n(),
              n_with_distance = sum(!is.na(distance_m)),
              prop_with_distance = sum(!is.na(distance_m))/n(),
              n_both_flying = sum(flight_status == "both"),
              prop_both_flying = sum(flight_status == "both")/n(),
              prop_close = sum(distance_km <= ddf)/n(),
              prop_flying_close = sum(flight_status == "both" & (distance_m <= ddf))/sum(flight_status == "both"),
              mean_flight_dist_km = mean(distance_km[flight_status == "both"])) %>%
    ungroup()
  return(flight_stats)
}

beats_asoc <- function(comparison){
  if(!is.null(comparison)){
    comp <- as.data.frame(comparison$comparison) %>% bind_cols(., "mod" = row.names(.))
    beats_asoc <- (comp$elpd_diff[(grepl("asoc", comp$mod))] < comp$elpd_diff[!(grepl("asoc", comp$mod))])& (comp$elpd_diff[(grepl("asoc", comp$mod))] + comp$se_diff[(grepl("asoc", comp$mod))] < 0)
    return(beats_asoc)
  }else{
    return(NA)
  }
}

plot_asoc <- function(comparison){
  if(!is.null(comparison)){
    df <- as.data.frame(comparison$comparison)
    p <- df %>% ggplot(aes(x = row.names(df), y = elpd_diff))+geom_point()+geom_errorbar(aes(ymin = elpd_diff-se_diff, ymax = elpd_diff + se_diff), width = 0.1)+ coord_flip()
    return(p)
  }else{
    return(NULL)
  }
}

pool_coefficient <- function(coef_label, fits, weights, param_map, n_total = 20000) {
  draws_list <- lapply(names(fits), function(nm) {
    n_draw <- round(weights[nm] * n_total)
    if (n_draw == 0) return(NULL)
    
    row <- param_map %>% filter(model == nm, coef_label == !!coef_label)
    
    if (nrow(row) == 0) {
      return(rep(0, n_draw))
    }
    
    d <- as_draws_df(fits[[nm]]$draws(variables = row$param_in_model))[[row$param_in_model]]
    
    # percent_ST is structurally undefined (NaN) in asocial models, since there's
    # no social pathway to compute a percentage of -- same "contributes 0" logic
    # as s being absent from these models entirely
    d[is.nan(d) | is.na(d)] <- 0
    
    sample(d, n_draw, replace = TRUE)
  })
  unlist(draws_list)
}

get_model_averaged_estimates <- function(fits, names){
  if(all(map_lgl(fits, is.null))){
    return(NULL)
  }else{
    suppressMessages(loo_list <- map(fits, ~{
      if(!is.null(.x)){
        .x$loo()}}))
    
    weights_obj <- loo_model_weights(loo_list, method = "stacking")
    weights <- setNames(as.numeric(weights_obj), names)
    names(fits) <- names
    
    # now that I have the weights, have to figure out how to do model averaging.
    # Code from Claude (will need to check all of this!)
    param_names <- lapply(fits, function(f) STb_summary(f)$Parameter)
    names(param_names) <- names(fits)
    param_map <- suppressWarnings(imap_dfr(param_names, function(params, model_name) {
      tibble(model = model_name, param_in_model = params) %>%
        filter(!str_detect(param_in_model, "^log_")) %>%   # drop log-scale duplicates
        mutate(
          coef_label = case_when(
            param_in_model == "lambda_0" ~ "lambda_0",
            str_detect(param_in_model, "^s\\[") ~
              paste0("s_net", str_extract(param_in_model, "(?<=\\[)\\d+")),
            str_detect(param_in_model, "^percent_ST\\[") ~
              paste0("percent_ST_net", str_extract(param_in_model, "(?<=\\[)\\d+")),
            str_detect(param_in_model, "^beta_ILVi_") ~
              paste0(str_remove(param_in_model, "^beta_ILVi_"), "__on_asocial"),
            str_detect(param_in_model, "^beta_ILVs_") ~
              paste0(str_remove(param_in_model, "^beta_ILVs_"), "__on_social"),
            TRUE ~ NA_character_
          )
        )
    }))
    # sanity check: make sure nothing fell through un-labeled
    if(nrow(param_map %>% filter(is.na(coef_label))) > 0){
      stop("Something fell through un-labeled!")
    }
    
    coef_labels <- unique(na.omit(param_map$coef_label))
    
    averaged_results <- lapply(coef_labels, function(cl) {
      pooled <- pool_coefficient(cl, fits, weights, param_map)
      tibble(
        coef_label = cl,
        Median     = median(pooled),
        MAD        = mad(pooled),
        CI_Lower   = unname(quantile(pooled, 0.025)),
        CI_Upper   = unname(quantile(pooled, 0.975))
      )
    }) %>% bind_rows()
    return(averaged_results)
  }
}

get_carc_stats <- function(carcs_list){
  summ <- purrr::list_rbind(carcs_list) %>% group_by(trial) %>% summarize(n_total = n(), n_seeds = sum(time == 0), n_right_censored = sum(time > t_end), n_found = n_total-n_right_censored) %>% mutate(prop_found = n_found/n_total, prop_seeds = n_seeds/n_total, prop_right_censored = n_right_censored/n_total)
  return(summ)
}