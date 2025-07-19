# INPA
source(here("R/functions.R"))
library(dplyr)
library(lubridate)
id <- 4467134
tar_load(inpa_carcs)
tar_load(gps_combined)
tar_load(gps_all_inpa)
dbf <- 30
tar_load(days_after)
which_id <- which(unlist(map(inpa_carcs, "carcID")) == id)

gps_30days <- get_gps_all(inpa_carcs[which_id], gps_combined, days_after, dbf)[[1]]
length(unique(gps_30days$dateOnly))
min(gps_30days$dateOnly) == inpa_carcs[[which_id]]$date - days(dbf)
max(gps_30days$dateOnly) == inpa_carcs[[which_id]]$date + days(days_after) # because the get_gps_all function adds 1 day to allow for calculating roost positions. So this should in fact be different.
max(gps_30days$dateOnly) == inpa_carcs[[which_id]]$date + days(days_after+1) # one day more

prepare_nbda_data <- function(gps,
                              ddf,
                              dds,
                              gps_spd,
                              n_days_gps_dynamic = c(3),
                              n_days_gps_static = list(),
                              remove_seeds = FALSE,
                              seed_time_before = NULL,
                              sighting_time_max_hours = 72) {
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
             (ground_speed > gps_spd & dist_to_carcass <= ddf) |
               (ground_speed <= gps_spd & dist_to_carcass <= dds)) %>%
      distinct(local_identifier) %>%
      pull(local_identifier) %>%
      as.character()
  }
  
  # Get first sightings
  gps_after_in_sight <- gps %>%
    filter(time_since_carcass >= 0 & time_since_carcass <= sighting_time_max_hours) %>%
    filter((ground_speed > gps_spd & dist_to_carcass <= ddf) |
             (ground_speed <= gps_spd & dist_to_carcass <= dds))
  
  first_sightings <- gps_after_in_sight %>%
    group_by(local_identifier) %>%
    arrange(time_since_carcass, timestamp) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(time_since_carcass)
  n_found <- length(unique(first_sightings$local_identifier))
  n_gps <- length(unique(gps$local_identifier))
  prop_found <- n_found/n_gps
  
  if (remove_seeds) {
    first_sightings <- first_sightings %>%
      filter(!(local_identifier %in% seeds))
  } else {
    first_sightings <- first_sightings
  }
  
  # Alphabetical order of all individuals
  all_indivs_sorted <- sort(unique(gps$local_identifier))

  # Temporal order of arrivals
  oa_indivs <- first_sightings %>%
    arrange(time_since_carcass, timestamp) %>%
    pull(local_identifier) %>%
    as.character()
  
  oa_nums <- match(oa_indivs, all_indivs_sorted)
  if(length(oa_nums) != length(oa_indivs)){stop("Length of oa vecs does not match")}
  if(length(oa_nums) == length(unique(gps$local_identifier)) & prop_found != 1){stop("All individuals are included in oa_nums, but not all indivs found the carcass. Something's wrong!")}
  
  # association matrix indices--which association matrix to use for which arrival (in the case of dynamic networks)
  ami <- if (remove_seeds) {
    which(!(first_sightings_all$local_identifier %in% seeds))[match(oa_indivs, first_sightings_all$local_identifier)]
  } else {
    seq_along(oa_nums) # since we're going to expand our gps subsets/dynamic network slices, this can just be a vector of integers.
  }
  
  # Helper to get calendar date
  get_date <- function(dt) as.Date(dt)
  gps_dates_available <- unique(get_date(gps$timestamp))
  
  # Infer carcass placement timestamp from first row
  first_row <- gps[1, ]
  carcass_placement_time <- first_row$timestamp - first_row$time_since_carcass
  carcass_placement_date <- as.Date(carcass_placement_time)
  
  # Cumulative GPS: all data from midnight until that individual's first sighting of the carcass
  gps_data_cumulative <- map(first_sightings$timestamp, function(ts) {
    day_start <- as.POSIXct(paste0(get_date(ts), " 00:00:00"), tz = tz(ts))
    gps %>% filter(timestamp >= day_start, timestamp <= ts)
  })
  
  # same-day GPS: all data from the same calendar day
  gps_data_sameday <- map(get_date(first_sightings$timestamp), function(date_val) {
    gps %>% filter(get_date(timestamp) == date_val)
  })
  
  # Dynamic day-prior GPS segments per individual
  gps_data_ndays_prior_list <- list()
  for (n in n_days_gps_dynamic) {
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
  
  # Static carcass-date-based segments (shared across individuals)
  gps_data_static_ndays_list <- list()
  for (range in n_days_gps_static) {
    if (length(range) != 2 || range[1] > range[2]) {
      stop("Each element of n_days_gps_static must be a length-2 integer vector where the first number <= second.")
    }
    
    start_offset <- range[1]
    end_offset <- range[2]
    start_date <- carcass_placement_date + start_offset
    end_date <- carcass_placement_date + end_offset
    date_seq <- seq.Date(start_date, end_date, by = "day")
    
    var_name <- sprintf("gps_data_static_days_%+03d_%+03d", start_offset, end_offset)
    
    gps_data <- if (all(date_seq %in% gps_dates_available)) {
      gps %>% filter(get_date(timestamp) %in% date_seq)
    } else {
      NULL
    }
    
    gps_data_static_ndays_list[[var_name]] <- gps_data
  }
  
  # Final combined output
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
    gps_data_sameday = gps_data_sameday
  )
  
  final_list <- c(base_list, gps_data_ndays_prior_list, gps_data_static_ndays_list)
  return(final_list)
}

make_assMatrix <- function(input) {
  # Case 1: Single data frame (static)
  if (is.data.frame(input)) {
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
  
  # Case 2: List
  if (!is.list(input)) stop("Input must be a data frame or a list of data frames.")
  if (length(input) == 0) stop("Input list is empty.")
  
  # Validate elements
  dims <- dim(input[[1]])
  rownames_ref <- rownames(input[[1]])
  colnames_ref <- colnames(input[[1]])
  
  for (i in seq_along(input)) {
    df <- input[[i]]
    if (!is.data.frame(df)) stop(paste("Element", i, "is not a data frame."))
    if (!all(dim(df) == dims)) stop(paste("Element", i, "has different dimensions."))
    if (!all(rownames(df) == rownames_ref)) stop(paste("Row names mismatch in element", i))
    if (!all(colnames(df) == colnames_ref)) stop(paste("Column names mismatch in element", i))
  }
  
  mat_list <- lapply(input, as.matrix)
  n_indivs <- dims[1]
  
  if (length(input) == 1) {
    # Static case: single matrix wrapped in list
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
      dim = c(n_indivs, n_indivs, 1, n_time),
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

seed_time_before <- 30 #mins
tar_load(detection_distance_flight)
tar_load(detection_distance_stationary)
tar_load(gps_spd)
test <- prepare_nbda_data(gps = gps_30days, remove_seeds = FALSE, seed_time_before = NULL, ddf = detection_distance_flight, dds = detection_distance_stationary, gps_spd = gps_spd, n_days_gps_dynamic = c(1, 3), n_days_gps_static = list(c(-30, -1), c(-7, -1)))

# Networks
## Dynamic
### flight, day by day
fl_bin_sameday <- fix_nets(map(test$gps_data_sameday, ~get_fl_bin(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
fl_wt_sameday <- fix_nets(map(test$gps_data_sameday, ~get_fl_weighted(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
### flight, cumulative same day
fl_bin_cumulative_sameday <- fix_nets(map(test$gps_data_cumulative, ~get_fl_bin(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
fl_wt_cumulative_sameday <- fix_nets(map(test$gps_data_cumulative, ~get_fl_weighted(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
### flight, since 3 days prior
fl_bin_3daysprior <- fix_nets(map(test$gps_data_ndays_prior_03, ~get_fl_bin(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
fl_wt_3daysprior <- fix_nets(map(test$gps_data_ndays_prior_03, ~get_fl_weighted(.x, dist = detection_distance_flight)), test$all_indivs_sorted)

### flight, since 1 day prior
fl_bin_1daysprior <- fix_nets(map(test$gps_data_ndays_prior_01, ~get_fl_bin(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
fl_wt_1daysprior <- fix_nets(map(test$gps_data_ndays_prior_01, ~get_fl_weighted(.x, dist = detection_distance_flight)), test$all_indivs_sorted)

## Static
### flight, -30 through -1 days
fl_bin_n30n01 <- fix_nets(list(get_fl_bin(test$`gps_data_static_days_-30_-01`, dist = detection_distance_flight)), test$all_indivs_sorted)
fl_wt_n30n01 <- fix_nets(list(get_fl_weighted(test$`gps_data_static_days_-30_-01`, dist = detection_distance_flight)), test$all_indivs_sorted)
### flight, -7 through -1 days
fl_bin_n07n01 <- fix_nets(list(get_fl_bin(test$`gps_data_static_days_-07_-01`, dist = detection_distance_flight)), test$all_indivs_sorted)
fl_wt_n07n01 <- fix_nets(list(get_fl_weighted(test$`gps_data_static_days_-07_-01`, dist = detection_distance_flight)), test$all_indivs_sorted)

## Dynamic flight networks, entire day of first sighting, including after first sighting
data_sameday <- nbdaData(label = test$carcID, 
                         assMatrix = make_assMatrix(fl_bin_sameday), 
                         orderAcq = test$oa_nums)
data_sameday_wt <- nbdaData(label = test$carcID,
                            assMatrix = make_assMatrix(fl_wt_sameday),
                            orderAcq = test$oa_nums)
mod_sameday <- oadaFit(data_sameday, type = "social")
mod_sameday_wt <- oadaFit(data_sameday_wt, type = "social")

## Dynamic flight networks, cumulative same day up til time of first sighting
data_cumul <- nbdaData(label = test$carcID, 
         assMatrix = make_assMatrix(fl_bin_cumulative_sameday), 
         orderAcq = test$oa_nums)
data_cumul_wt <- nbdaData(label = test$carcID, 
         assMatrix = make_assMatrix(fl_wt_cumulative_sameday), 
         orderAcq = test$oa_nums)
mod_cumul <- oadaFit(data_cumul, type = "social")
mod_cumul_wt <- oadaFit(data_cumul_wt, type = "social")

## Dynamic flight networks, 3 days prior to day on which the individual found the carcass (but not including the finding day)
data_3daysprior <- nbdaData(label = test$carcID, 
                          assMatrix = make_assMatrix(fl_bin_3daysprior), 
                          orderAcq = test$oa_nums)
data_3daysprior_wt <- nbdaData(label = test$carcID, 
                            assMatrix = make_assMatrix(fl_wt_3daysprior), 
                            orderAcq = test$oa_nums)
mod_3daysprior <- oadaFit(data_3daysprior, type = "social")
mod_3daysprior_wt <- oadaFit(data_3daysprior_wt, type = "social")

## Static flight networks, days -30 through -1
data_n30n01 <- nbdaData(label = test$carcID, 
                            assMatrix = make_assMatrix(fl_bin_n30n01), 
                            orderAcq = test$oa_nums)
data_n30n01_wt <- nbdaData(label = test$carcID, 
                        assMatrix = make_assMatrix(fl_wt_n30n01), 
                        orderAcq = test$oa_nums)
mod_n30n01 <- oadaFit(data_n30n01, type = "social")
mod_n30n01_wt <- oadaFit(data_n30n01_wt, type = "social")

## Static flight networks, days -7 through -1
data_n07n01 <- nbdaData(label = test$carcID, 
                        assMatrix = make_assMatrix(fl_bin_n07n01), 
                        orderAcq = test$oa_nums)
data_n07n01_wt <- nbdaData(label = test$carcID, 
                        assMatrix = make_assMatrix(fl_wt_n07n01), 
                        orderAcq = test$oa_nums)
mod_n07n01 <- oadaFit(data_n07n01, type = "social")
mod_n07n01_wt <- oadaFit(data_n07n01_wt, type = "social")

mods_list <- list("dynamic_sameday_bin" = mod_sameday, 
                  "dynamic_cumulative_bin" = mod_cumul, 
                  "dynamic_3days_bin" = mod_3daysprior, 
                  "static_3001_bin" = mod_n30n01, 
                  "static_0701_bin" = mod_n07n01,
                  "dynamic_sameday_wt" = mod_sameday_wt, 
                  "dynamic_cumulative_wt" = mod_cumul_wt, 
                  "dynamic_3days_wt" = mod_3daysprior_wt, 
                  "static_3001_wt" = mod_n30n01_wt, 
                  "static_0701_wt" = mod_n07n01_wt)
stats <- purrr::list_rbind(map(mods_list, getmodstats)) # XXX need to modify getmodstats to include the names here
stats$mod <- names(mods_list)
stats <- stats %>%
  mutate(net_type = factor(str_extract(mod, "static|dynamic"), levels = c("static", "dynamic")),
         wt = str_detect(mod, "wt"))

stats %>%
  filter(mod != "static_3001") %>%
  ggplot(aes(x = mod, col = net_type))+
  geom_segment(aes(y = outputPar-se, yend = outputPar + se))+
  geom_point(aes(y = outputPar))+
  theme_classic()+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  labs(y = "S", x = "Model", color = "Network type")+
  coord_flip()

# test removing seeds
# get more gps data for use w any carcass
# turn days into hours for more precise targeting