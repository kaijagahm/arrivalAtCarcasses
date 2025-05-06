get_loginObject <- function(pw){
  load(pw)
  loginObject <- move::movebankLogin(username = "kaijagahm", password = pw)
  rm(pw)
  return(loginObject)
}

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


# prepare_data ------------------------------------------------------------
get_gps_combined <- function(gps_2023, gps_2024, bbox_south){
  gps_combined <- bind_rows(gps_2023, gps_2024) %>%
    st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>%
    bind_cols(st_coordinates(.)) %>%
    rename("location_long" = X,
           "location_lat" = Y) %>%
    mutate(dateOnly = lubridate::ymd(dateOnly)) %>%
    st_transform(32636) %>%
    st_crop(bbox_south)
  return(gps_combined)
}

get_gps_all <- function(inpa_carcs, gps_combined, days_after){
  gps_all <- vector(mode = "list", length = length(inpa_carcs))
  for(i in 1:length(inpa_carcs)){
    ic <- inpa_carcs[[i]]
    cid <- ic$carcID[1]
    carcass_datetime <- ic$datetime[1]
    out <- gps_combined %>%
      filter(timestamp >= (carcass_datetime-days(1)) & timestamp <= (carcass_datetime + days(days_after+1))) %>%
      mutate(dist_to_carcass = as.numeric(st_distance(., ic)),
             time_since_carcass = difftime(timestamp, carcass_datetime, units = "hours"),
             carcID = cid)
    gps_all[[i]] <- out
  }
  return(gps_all)
}

get_roosts <- function(gps_all){
  r <- map(gps_all, ~{
    if(nrow(.x) > 0){return(get_roosts_df(.x, id = "local_identifier"))}
    else{return(NULL)}
  })
  return(r)
}

get_seeds_gps <- function(gps_all, inpa_carcs, seed_time_before, seed_distance){
  seeds_gps <- map2(gps_all, inpa_carcs, ~{
    dttm <- .y$datetime[1]
    .x %>% filter(timestamp >= dttm-seed_time_before & timestamp <= dttm) %>%
      filter(dist_to_carcass < seed_distance)
  })
  return(seeds_gps)
}

get_distances <- function(roosts, inpa_carcs){
  distances <- map2(roosts, inpa_carcs, ~{
    if(!is.null(.x)){
      dist <- .x %>%
        sf::st_as_sf(., coords = c("location_long", "location_lat"), crs = "WGS84") %>%
        sf::st_transform(32636) %>%
        mutate(dist = as.numeric(st_distance(., .y))) %>%
        st_drop_geometry() %>%
        dplyr::select(local_identifier, roost_date, dist) %>%
        pivot_wider(id_cols = "local_identifier", names_from = "roost_date", values_from = "dist", names_prefix = "roost_") %>%
        mutate(year = .y$year[1])
    }else{
      dist <- NULL
    }
    return(dist)
  })
  return(distances)
}

get_www <- function(ww){
  www <- ww %>%
    dplyr::select(Nili_id, Movebank_id, Nili_id, birth_year, sex) %>%
    mutate(age_2023 = 2023-birth_year,
           age_2024 = 2024-birth_year,
           age_group_2023 = case_when(age_2023 > 5 ~ "02_adult",
                                      age_2023 <= 5 ~ "01_juv_sub",
                                      .default = NA),
           age_group_2024 = case_when(age_2024 > 5 ~ "02_adult",
                                      age_2024 <= 5 ~ "01_juv_sub",
                                      .default = NA)) %>%
    dplyr::select("local_identifier" = "Movebank_id", age_group_2023, age_group_2024) %>%
    distinct()
  return(www)
}

get_ilvs <- function(distances, www){
  yrs <- map_dbl(distances, ~.x$year[1])
  ilvs <- map2(distances, yrs, ~{
    tojoin <- www %>%
      select(local_identifier, "age_group" = paste0("age_group_", .y))
    out <- left_join(.x, tojoin, by = "local_identifier")
    to_rename <- names(out)[grepl("roost_", names(out))]
    new_names <- paste0("roost_night", 0:(length(to_rename)-1))
    names(out)[names(out) %in% to_rename] <- new_names
    return(out)})
  return(ilvs)
}

remove_points_before <- function(gps_all, inpa_carcs, days_after){
  gps <- map2(gps_all, inpa_carcs, ~{
    dttm <- .y$datetime[1]
    .x %>%
      filter(timestamp >= lubridate::ymd_hms(dttm) & timestamp <= (lubridate::ymd_hms(dttm) + days(days_after)))
  })
  return(gps)
}

get_at_carcass <- function(gps, inpa_carcs, arrival_distance){
  at_carcass <- map2(gps, inpa_carcs, ~.x %>%
                       mutate(carcID = .y$carcID) %>%
                       filter(dist_to_carcass < arrival_distance & ground_speed < 5))
  return(at_carcass)
}

get_see_carcass <- function(gps, inpa_carcs, detection_distance){
  see_carcass <- map2(gps, inpa_carcs, ~.x %>%
                        mutate(carcID = .y$carcID) %>%
                        filter(dist_to_carcass < detection_distance))
  return(see_carcass)
}

get_firsts <- function(at_carcass, inpa_carcs){
  firsts <- map2(at_carcass, inpa_carcs, ~{
    if(nrow(.x) > 1){
      out <- .x %>%
        filter(timestamp >= .y$datetime) %>%
        arrange(timestamp) %>%
        group_by(local_identifier) %>%
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

get_firsts_see <- function(see_carcass, inpa_carcs){
  firsts_see <- map2(see_carcass, inpa_carcs, ~{
    if(nrow(.x) > 1){
      out <- .x %>%
        filter(timestamp >= .y$datetime) %>%
        arrange(timestamp) %>%
        group_by(local_identifier) %>%
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
  map_dbl(firsts, ~nrow(.x[!is.na(.x$local_identifier),])) > 0
}

get_has_sightings <- function(firsts_see){
  map_dbl(firsts_see, ~nrow(.x[!is.na(.x$local_identifier),])) > 0
}

check_1 <- function(oa, oa_see, oa_indivs_sorted, oa_see_indivs_sorted, acq_times, see_times){
  if(length(oa) != length(oa_indivs_sorted)){stop("check1: length mismatch 1")}
  if(length(oa_see) != length(oa_see_indivs_sorted)){stop("check1: length mismatch 1")}
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

check_2 <- function(gps_flight_allday, gps_flight_allday_see, gps_flight_cumulative, gps_flight_cumulative_see, gps_flight_3hr, gps_flight_3hr_see){
  if(length(unique(map_dbl(list(gps_flight_3hr, gps_flight_allday, gps_flight_cumulative), length))) != 1){stop("check2: length mismatch 1")}
  if(length(unique(map_dbl(list(gps_flight_3hr_see, gps_flight_allday_see, gps_flight_cumulative_see), length))) != 1){stop("check2: length mismatch 2")}
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
      ids <- .x$local_identifier
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
      ids <- .x$local_identifier
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
      self_edges <- data.frame(ID1 = sort(unique(dat$local_identifier)),
                               ID2 = sort(unique(dat$local_identifier)),
                               sri = 0)
      out <- suppressMessages(vultureUtils::getFlightEdges(dat, roostPolygons = NULL,
                                                           consecThreshold = 1,
                                                           idCol = "local_identifier",
                                                           return = "sri",
                                                           distThreshold = dist)) %>%
        bind_rows(self_edges) %>%
        mutate(sri = case_when(is.nan(sri) ~ 0, .default = sri)) %>% # XXX forcing all NaNs to zero because we don't have a choice--can't have missing values in the network
        arrange(ID1, ID2) %>%
        pivot_wider(id_cols = "ID1", names_from = "ID2", values_from = "sri") %>%
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


get_fl_bin <- function(dat, dist){
  if(is.data.frame(dat)){
    if(nrow(dat) > 0){
      self_edges <- data.frame(ID1 = sort(unique(dat$local_identifier)),
                               ID2 = sort(unique(dat$local_identifier)),
                               value = 0)
      out <- suppressMessages(vultureUtils::getFlightEdges(dat, roostPolygons = NULL,
                                                           consecThreshold = 1,
                                                           idCol = "local_identifier",
                                                           return = "edges",
                                                           distThreshold = dist)) %>%
        dplyr::select(ID1, ID2) %>%
        distinct() %>%
        mutate(value = 1) %>%
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

fix_nets <- function(nets, indivs){
  indivs <- indivs[!is.na(indivs)]
  updated <- vector(mode = "list", length = length(nets))
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
      if(!any(net == "blank")){
        net_updated <- as.data.frame(bind_rows(net, toadd))
      }else{
        net_updated <- as.data.frame(toadd)
      }
      net_updated[is.na(net_updated)] <- 0
      row.names(net_updated) <- net_updated$ID1
    }else{
      net_updated <- net
    }
    net_updated_2 <- net_updated %>% select(-ID1)
    updated[[nt]] <- net_updated_2
    #updated[[nt]] <- net_updated_2[indivs, indivs]
  }
  return(updated)
}

check_3 <- function(fl_allday_bin, fl_allday_bin_see, fl_cumulative_bin, fl_cumulative_bin_see, fl_3hr_bin, fl_3hr_bin_see){
  if(length(unique(map_dbl(list(fl_3hr_bin, fl_allday_bin, fl_cumulative_bin), length))) != 1){stop("check3: length mismatch 1")}
  if(length(unique(map_dbl(list(fl_3hr_bin_see, fl_allday_bin_see, fl_cumulative_bin_see), length))) != 1){stop("check3: length mismatch 2")}
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
  for(i in 1:length(n_dynamic)){
    for(j in 1:length(matrices[[i]])){
      if(!is.null(matrices)){
        n_dynamic[[i]][,,1,j] <- array(matrices[[i]][[j]], dim = c(ni[[i]], ni[[i]], 1))
      }
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
    ilvs_this_carcass <- map(nights_vec, ~ilvs %>% select(local_identifier, paste0("roost_night", .x), age_group) %>% rename("dist_roost" = 2))
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
        assMatrix = nets1[[i]], # because we replaced nets1 with the merged array before, we can still just pass nets1 in here, because it contains both networks.
        orderAcq = oas[[i]],
        assMatrixIndex = amis[[i]]
      ), ilv_args))
    } else {
      outlist[[i]] <- do.call(nbdaData, c(list(
        label = label,
        assMatrix = nets1[[i]],
        orderAcq = oas[[i]]
      ), ilv_args))
    }
  }
  
  return(outlist)
}
