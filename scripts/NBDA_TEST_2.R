# Carcass number 4892923
# INPA
tar_load(inpa_carcs)
tar_load(gps_all_inpa)

library(dplyr)
library(lubridate)
source(here("R/functions.R"))

prepare_nbda_data <- function(gps,
                              detection_distance_flight,
                              detection_distance_stationary,
                              gps_spd,
                              n_days_gps = c(3),
                              remove_seeds = FALSE,
                              seed_time_before = NULL) {
  library(dplyr)
  library(purrr)
  library(lubridate)
  
  carc_id <- unique(gps$carcID)
  if (length(carc_id) != 1) stop("gps$carcID must have exactly one unique value.")
  
  if (remove_seeds && is.null(seed_time_before)) {
    stop("seed_time_before must be provided when remove_seeds = TRUE.")
  }
  
  # Identify seed individuals if needed
  seeds <- character(0)
  if (remove_seeds) {
    time_window <- seed_time_before / 60
    seeds <- gps %>%
      filter(time_since_carcass >= -time_window,
             time_since_carcass <= 0,
             (ground_speed > gps_spd & dist_to_carcass <= detection_distance_flight) |
               (ground_speed <= gps_spd & dist_to_carcass <= detection_distance_stationary)) %>%
      distinct(local_identifier) %>%
      pull(local_identifier) %>%
      as.character()
  }
  
  # Get first sightings
  gps_after <- gps %>%
    filter(time_since_carcass >= 0) %>%
    filter((ground_speed > gps_spd & dist_to_carcass <= detection_distance_flight) |
             (ground_speed <= gps_spd & dist_to_carcass <= detection_distance_stationary))
  
  first_sightings_all <- gps_after %>%
    group_by(local_identifier) %>%
    arrange(time_since_carcass, timestamp) %>%
    slice(1) %>%
    ungroup()
  
  if (remove_seeds) {
    first_sightings <- first_sightings_all %>%
      filter(!(local_identifier %in% seeds))
  } else {
    first_sightings <- first_sightings_all
  }
  
  # Alphabetical order of all individuals
  oas_indivs_sorted <- sort(unique(gps$local_identifier))
  
  # Temporal order of arrivals
  oas_indivs <- first_sightings %>%
    arrange(time_since_carcass, timestamp) %>%
    pull(local_identifier) %>%
    as.character()
  
  oas_num <- match(oas_indivs, oas_indivs_sorted)
  ami <- if (remove_seeds) {
    which(!(first_sightings_all$local_identifier %in% seeds))[match(oas_indivs, first_sightings_all$local_identifier)]
  } else {
    seq_along(oas_num)
  }
  
  n_indivs <- length(oas_indivs)
  
  # Helper to get calendar date
  get_date <- function(dt) as.Date(dt)
  gps_dates_available <- unique(get_date(gps$timestamp))
  
  # Cumulative GPS: all data from midnight until first sighting
  gps_data_cumulative <- map(first_sightings$timestamp, function(ts) {
    day_start <- as.POSIXct(paste0(get_date(ts), " 00:00:00"), tz = tz(ts))
    gps %>% filter(timestamp >= day_start, timestamp <= ts)
  })
  
  # Whole-day GPS: all data from the same calendar day
  gps_data_wholeday <- map(get_date(first_sightings$timestamp), function(date_val) {
    gps %>% filter(get_date(timestamp) == date_val)
  })
  
  # Build dynamic list of n-days-prior GPS segments
  gps_data_ndays_prior_list <- list()
  
  for (n in n_days_gps) {
    var_name <- paste0("gps_data_ndays_prior_", formatC(n, width = 2, flag = "0"))
    
    gps_data_list <- map(get_date(first_sightings$timestamp), function(date_val) {
      start_date <- date_val - n
      date_seq <- seq.Date(start_date, date_val - 1, by = "day")
      if (all(date_seq %in% gps_dates_available)) {
        gps %>% filter(get_date(timestamp) %in% date_seq)
      } else {
        NULL
      }
    })
    
    gps_data_ndays_prior_list[[var_name]] <- gps_data_list
  }
  
  # Combine base return list with dynamic n-days list
  base_list <- list(
    carcID = carc_id,
    seed_indivs = seeds,
    first_sightings = first_sightings,
    oas_indivs_sorted = oas_indivs_sorted,
    oas_indivs = oas_indivs,
    oas_num = oas_num,
    ami = ami,
    n_indivs = n_indivs,
    gps_data_cumulative = gps_data_cumulative,
    gps_data_wholeday = gps_data_wholeday
  )
  
  final_list <- c(base_list, gps_data_ndays_prior_list)
  return(final_list)
}

make_time_varying_assMatrix <- function(df_list) {
  # Error checks
  if (!is.list(df_list)) stop("Input must be a list.")
  if (length(df_list) == 0) stop("Input list is empty.")
  
  # Reference dimensions and names
  dims <- dim(df_list[[1]])
  rownames_ref <- rownames(df_list[[1]])
  colnames_ref <- colnames(df_list[[1]])
  
  for (i in seq_along(df_list)) {
    df <- df_list[[i]]
    if (!is.data.frame(df)) stop(paste("Element", i, "is not a data frame."))
    if (!all(dim(df) == dims)) stop(paste("Element", i, "has different dimensions."))
    if (!all(rownames(df) == rownames_ref)) stop(paste("Row names mismatch in element", i))
    if (!all(colnames(df) == colnames_ref)) stop(paste("Column names mismatch in element", i))
  }
  
  n_indivs <- dims[1]
  n_time <- length(df_list)
  
  # Convert each data frame to matrix and unlist into 4D array
  mat_list <- lapply(df_list, as.matrix)
  
  assMatrix <- array(
    data = unlist(mat_list),
    dim = c(n_indivs, n_indivs, 1, n_time),
    dimnames = list(
      rownames_ref,
      colnames_ref,
      "net1",  # only one network
      paste0("time", seq_len(n_time))
    )
  )
  
  return(assMatrix)
}

seed_time_before <- 30 #mins
tar_load(detection_distance_flight)
tar_load(detection_distance_stationary)
tar_load(gps_spd)
test <- prepare_nbda_data(gps = gps_all_inpa[[which_id]], remove_seeds = FALSE, seed_time_before = NULL, detection_distance_flight = detection_distance_flight, detection_distance_stationary = detection_distance_stationary, gps_spd = gps_spd, n_days_gps = c(2, 30))

# Networks
## Dynamic
### flight, day by day
fl_bin_days <- fix_nets(map(test$gps_data_wholeday, ~get_fl_bin(.x, dist = detection_distance_flight)), test$oas_indivs_sorted)
### flight, cumulative same day
fl_cumulative_sameday <- fix_nets(map(test$gps_data_cumulative, ~get_fl_bin(.x, dist = detection_distance_flight)), test$oas_indivs_sorted)
### flight, since 2 days prior
fl_bin_2daysprior <- fix_nets(map(test$gps_data_ndays_prior_02, ~get_fl_bin(.x, dist = detection_distance_flight)), test$oas_indivs_sorted)

## Dynamic flight networks, cumulative same day up til time of first sighting
data_cumul <- nbdaData(label = test$carcID, 
         assMatrix = make_time_varying_assMatrix(fl_cumulative_sameday), 
         orderAcq = test$oas_num)
mod_cumul <- oadaFit(data_cumul, type = "social")
getmodstats(mod_cumul)

## Dynamic flight networks, entire day of first sighting, including after first sighting
data_wholeday <- nbdaData(label = test$carcID, 
                          assMatrix = make_time_varying_assMatrix(fl_bin_days), 
                          orderAcq = test$oas_num)
mod_wholeday <- oadaFit(data_wholeday, type = "social")
getmodstats(mod_wholeday)

## Dynamic flight networks, 2 days prior to day on which the individual found the carcass (but not including the finding day)
data_2daysprior <- nbdaData(label = test$carcID, 
                          assMatrix = make_time_varying_assMatrix(fl_bin_2daysprior), 
                          orderAcq = test$oas_num)
mod_2daysprior <- oadaFit(data_2daysprior, type = "social")
getmodstats(mod_2daysprior)

### roost, night by night

# Networks
## static
### roost, past month
### roost, past week
### roost, night before carcass
### flight, past 30 days
