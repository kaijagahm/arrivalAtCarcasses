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
                              n_hours_gps_dynamic = list(),
                              n_hours_gps_static = list(),
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
  prop_found <- n_found / n_gps
  
  if (remove_seeds) {
    first_sightings <- first_sightings %>%
      filter(!(local_identifier %in% seeds))
  }
  
  all_indivs_sorted <- sort(unique(gps$local_identifier))
  
  oa_indivs <- first_sightings %>%
    arrange(time_since_carcass, timestamp) %>%
    pull(local_identifier) %>%
    as.character()
  
  oa_nums <- match(oa_indivs, all_indivs_sorted)
  if (length(oa_nums) != length(oa_indivs)) {
    stop("Length of oa vecs does not match")
  }
  if (length(oa_nums) == length(unique(gps$local_identifier)) && prop_found != 1) {
    stop("All individuals are included in oa_nums, but not all indivs found the carcass. Something's wrong!")
  }
  
  ami <- seq_along(oa_nums)
  
  # Infer carcass placement timestamp from first row
  first_row <- gps[1, ]
  carcass_placement_time <- first_row$timestamp - dhours(as.numeric(first_row$time_since_carcass))
  carcass_placement_date <- as.Date(carcass_placement_time)
  
  gps_data_cumulative <- map(first_sightings$timestamp, function(ts) {
    day_start <- as.POSIXct(paste0(as.Date(ts), " 00:00:00"), tz = tz(ts))
    gps %>% filter(timestamp >= day_start, timestamp <= ts)
  })
  
  gps_data_sameday <- map(as.Date(first_sightings$timestamp), function(date_val) {
    gps %>% filter(as.Date(timestamp) == date_val)
  })
  
  # Dynamic hour-range GPS segments per individual
  gps_data_dynamic_hour_ranges <- list()
  for (range in n_hours_gps_dynamic) {
    if (length(range) != 2 || range[1] > range[2]) {
      stop("Each element of n_hours_gps_dynamic must be a length-2 numeric vector where the first number <= second.")
    }
    start_offset <- range[1]
    end_offset <- range[2]
    start_label <- ifelse(start_offset < 0, paste0("n", sprintf("%03d", abs(start_offset))), sprintf("%03d", start_offset))
    end_label <- ifelse(end_offset < 0, paste0("n", sprintf("%03d", abs(end_offset))), sprintf("%03d", end_offset))
    var_name <- sprintf("gps_data_dynamic_hours_%s_%s", start_label, end_label)
    
    gps_data_list <- map(first_sightings$timestamp, function(ts) {
      start_time <- ts + dhours(as.numeric(start_offset))
      end_time <- ts + dhours(as.numeric(end_offset))
      gps %>% filter(timestamp >= start_time, timestamp <= end_time)
    })
    
    gps_data_dynamic_hour_ranges[[var_name]] <- gps_data_list
  }
  
  # Static hour-range GPS segments (shared across individuals)
  gps_data_static_hour_ranges <- list()
  for (range in n_hours_gps_static) {
    if (length(range) != 2 || range[1] > range[2]) {
      stop("Each element of n_hours_gps_static must be a length-2 numeric vector where the first number <= second.")
    }
    start_offset <- range[1]
    end_offset <- range[2]
    start_label <- ifelse(start_offset < 0, paste0("n", sprintf("%03d", abs(start_offset))), sprintf("%03d", start_offset))
    end_label <- ifelse(end_offset < 0, paste0("n", sprintf("%03d", abs(end_offset))), sprintf("%03d", end_offset))
    var_name <- sprintf("gps_data_static_hours_%s_%s", start_label, end_label)
    
    start_time <- carcass_placement_time + dhours(as.numeric(start_offset))
    end_time <- carcass_placement_time + dhours(as.numeric(end_offset))
    gps_data <- gps %>% filter(timestamp >= start_time, timestamp <= end_time)
    
    gps_data_static_hour_ranges[[var_name]] <- gps_data
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
    gps_data_sameday = gps_data_sameday
  )
  
  final_list <- c(base_list, gps_data_dynamic_hour_ranges, gps_data_static_hour_ranges)
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

test <- prepare_nbda_data(gps = gps_30days, 
                          remove_seeds = FALSE, 
                          seed_time_before = NULL, 
                          ddf = detection_distance_flight, 
                          dds = detection_distance_stationary, 
                          gps_spd = gps_spd, 
                          n_hours_gps_dynamic = list(c(-24, 0),
                                                     c(-72, 0)),
                          n_hours_gps_static = list(c(-720, -24), 
                                                    c(-168, -24)),
                          sighting_time_max_hours = 72)
# Networks
## Dynamic
### flight, day by day
fl_bin_sameday <- fix_nets(map(test$gps_data_sameday, ~get_fl_bin(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
fl_wt_sameday <- fix_nets(map(test$gps_data_sameday, ~get_fl_weighted(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
### flight, cumulative same day
fl_bin_cumulative_sameday <- fix_nets(map(test$gps_data_cumulative, ~get_fl_bin(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
fl_wt_cumulative_sameday <- fix_nets(map(test$gps_data_cumulative, ~get_fl_weighted(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
### flight, since 3 days prior
fl_bin_3daysprior <- fix_nets(map(test$gps_data_dynamic_hours_n072_000, ~get_fl_bin(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
fl_wt_3daysprior <- fix_nets(map(test$gps_data_dynamic_hours_n072_000, ~get_fl_weighted(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
### flight, since 1 day prior
fl_bin_1daysprior <- fix_nets(map(test$gps_data_dynamic_hours_n024_000, ~get_fl_bin(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
fl_wt_1daysprior <- fix_nets(map(test$gps_data_dynamic_hours_n024_000, ~get_fl_weighted(.x, dist = detection_distance_flight)), test$all_indivs_sorted)


## Static
### flight, -30 through -1 days
fl_bin_n720n024 <- fix_nets(list(get_fl_bin(test$gps_data_static_hours_n720_n024, dist = detection_distance_flight)), test$all_indivs_sorted)
fl_wt_n720n024 <- fix_nets(list(get_fl_weighted(test$gps_data_static_hours_n720_n024, dist = detection_distance_flight)), test$all_indivs_sorted)
### flight, -7 through -1 days
fl_bin_n168n024 <- fix_nets(list(get_fl_bin(test$gps_data_static_hours_n168_n024, dist = detection_distance_flight)), test$all_indivs_sorted)
fl_wt_n168n024 <- fix_nets(list(get_fl_weighted(test$gps_data_static_hours_n168_n024, dist = detection_distance_flight)), test$all_indivs_sorted)

## Dynamic flight networks, entire day of first sighting, including after first sighting
data_sameday <- nbdaData(label = test$carcID, 
                         assMatrix = make_assMatrix(fl_bin_sameday), 
                         orderAcq = test$oa_nums)
data_sameday_wt <- nbdaData(label = test$carcID,
                            assMatrix = make_assMatrix(fl_wt_sameday),
                            orderAcq = test$oa_nums)
mod_sameday <- oadaFit(data_sameday, type = "social")
mod_sameday_wt <- oadaFit(data_sameday_wt, type = "social")

## Dynamic flight networks, cumulative
data_cumul <- nbdaData(label = test$carcID, 
         assMatrix = make_assMatrix(fl_bin_cumulative_sameday), 
         orderAcq = test$oa_nums)
data_cumul_wt <- nbdaData(label = test$carcID, 
         assMatrix = make_assMatrix(fl_wt_cumulative_sameday), 
         orderAcq = test$oa_nums)
mod_cumul <- oadaFit(data_cumul, type = "social")
mod_cumul_wt <- oadaFit(data_cumul_wt, type = "social")

## Dynamic flight networks, -72 hours through sighting
data_3daysprior <- nbdaData(label = test$carcID, 
                          assMatrix = make_assMatrix(fl_bin_3daysprior), 
                          orderAcq = test$oa_nums)
data_3daysprior_wt <- nbdaData(label = test$carcID, 
                            assMatrix = make_assMatrix(fl_wt_3daysprior), 
                            orderAcq = test$oa_nums)
mod_3daysprior <- oadaFit(data_3daysprior, type = "social")
mod_3daysprior_wt <- oadaFit(data_3daysprior_wt, type = "social")

## Dynamic flight networks, -24 hours through sighting
data_1daysprior <- nbdaData(label = test$carcID, 
                            assMatrix = make_assMatrix(fl_bin_1daysprior), 
                            orderAcq = test$oa_nums)
data_1daysprior_wt <- nbdaData(label = test$carcID, 
                               assMatrix = make_assMatrix(fl_wt_1daysprior), 
                               orderAcq = test$oa_nums)
mod_1daysprior <- oadaFit(data_1daysprior, type = "social")
mod_1daysprior_wt <- oadaFit(data_1daysprior_wt, type = "social")

## Static flight networks, -720 hours (30 days prior) through -24 hours (1 day prior)
data_n720n024 <- nbdaData(label = test$carcID, 
                            assMatrix = make_assMatrix(fl_bin_n720n024), 
                            orderAcq = test$oa_nums)
data_n720n024_wt <- nbdaData(label = test$carcID, 
                        assMatrix = make_assMatrix(fl_wt_n720n024), 
                        orderAcq = test$oa_nums)
mod_n720n024 <- oadaFit(data_n720n024, type = "social")
mod_n720n024_wt <- oadaFit(data_n720n024_wt, type = "social")

## Static flight networks, -168 hours (7 days prior) through -24 hours (1 day prior)
data_n168n024 <- nbdaData(label = test$carcID, 
                          assMatrix = make_assMatrix(fl_bin_n168n024), 
                          orderAcq = test$oa_nums)
data_n168n024_wt <- nbdaData(label = test$carcID, 
                             assMatrix = make_assMatrix(fl_wt_n168n024), 
                             orderAcq = test$oa_nums)
mod_n168n024 <- oadaFit(data_n168n024, type = "social")
mod_n168n024_wt <- oadaFit(data_n168n024_wt, type = "social")

mods_list <- list("dynamic_sameday_bin" = mod_sameday, 
                  "dynamic_cumulative_bin" = mod_cumul, 
                  "dynamic_3days_bin" = mod_3daysprior, 
                  "dynamic_1days_bin" = mod_1daysprior,
                  "static_n720_n024_bin" = mod_n720n024, 
                  "static_n168_n024_bin" = mod_n168n024,
                  "dynamic_sameday_wt" = mod_sameday_wt, 
                  "dynamic_cumulative_wt" = mod_cumul_wt, 
                  "dynamic_3days_wt" = mod_3daysprior_wt, 
                  "dynamic_1days_wt" = mod_1daysprior_wt,
                  "static_n720_n024_wt" = mod_n720n024_wt, 
                  "static_n168_n024_wt" = mod_n168n024_wt)
stats <- purrr::list_rbind(map(mods_list, getmodstats))
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
  coord_flip() # niiice

# test removing seeds [TO DO!]
# bring code for carcass arrival plots from wild_carcasses.R into here [TO DO!]
# get more gps data for use w any carcass [DONE--JUST NEED TO FINISH PIPELINE AND LINK TO THIS SCRIPT]
# turn days into hours for more precise targeting [DONE]
