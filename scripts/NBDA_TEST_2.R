# Example of running NBDA on one stn and one wild carcass

# Load libraries and data -------------------------------------------------
source(here("R/functions.R"))
library(dplyr)
library(lubridate)
library(NBDA)
tar_load(stn_carcs)
tar_load(wild_carcs)
tar_load(gps_combined)
dbf <- 30
tar_load(days_after)

hist(lubridate::date(gps_combined$timestamp), breaks = "weeks") # we have GPS data spanning the hf periods

# Get histograms, for reference later
plots_stn <- readRDS(here("data/plots_stn.RDS"))
plots_wild_valid <- readRDS(here("data/plots_wild_valid.RDS"))
names(plots_stn)
length(plots_stn) # all 65 carcasses
names(plots_wild_valid) 
length(plots_wild_valid) # only 16 wild carcasses that we are considering to be valid at this point.

# Define functions --------------------------------------------------------
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
  
  gps$ground_speed <- as.numeric(gps$ground_speed)
  
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
    arrange(time_since_carcass, timestamp_il) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(time_since_carcass)
  
  n_found <- length(unique(first_sightings$tag_local_identifier))
  n_gps <- length(unique(gps$tag_local_identifier))
  prop_found <- n_found / n_gps
  
  if (remove_seeds) {
    first_sightings <- first_sightings %>%
      filter(!(tag_local_identifier %in% seeds))
  }
  
  all_indivs_sorted <- sort(unique(gps$tag_local_identifier))
  
  oa_indivs <- first_sightings %>%
    arrange(time_since_carcass, timestamp_il) %>%
    pull(tag_local_identifier) %>%
    as.character()
  
  oa_nums <- match(oa_indivs, all_indivs_sorted)
  if (length(oa_nums) != length(oa_indivs)) {
    stop("Length of oa vecs does not match")
  }
  if (length(oa_nums) == length(unique(gps$local_identifier)) && prop_found != 1) {
    stop("All individuals are included in oa_nums, but not all indivs found the carcass. Something's wrong!")
  }
  
  ami <- seq_along(oa_nums)
  
  # Infer carcass placement timestamp_il from first row
  first_row <- gps[1, ]
  carcass_placement_time <- first_row$timestamp_il - dhours(as.numeric(first_row$time_since_carcass))
  carcass_placement_date <- as.Date(carcass_placement_time)
  
  gps_data_cumulative <- map(first_sightings$timestamp_il, function(ts) {
    day_start <- as.POSIXct(paste0(as.Date(ts), " 00:00:00"), tz = tz(ts))
    gps %>% filter(timestamp_il >= day_start, timestamp_il <= ts)
  })
  
  gps_data_sameday <- map(as.Date(first_sightings$timestamp_il), function(date_val) {
    gps %>% filter(as.Date(timestamp_il) == date_val)
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
    
    gps_data_list <- map(first_sightings$timestamp_il, function(ts) {
      start_time <- ts + dhours(as.numeric(start_offset))
      end_time <- ts + dhours(as.numeric(end_offset))
      gps %>% filter(timestamp_il >= start_time, timestamp_il <= end_time)
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
    gps_data <- gps %>% filter(timestamp_il >= start_time, timestamp_il <= end_time)
    
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


# Keep only the wild carcasses that we deemed probably "valid" in wild_carcasses.R based on the bounding box
wild_carcs_valid <- wild_carcs[map_lgl(wild_carcs, ~.x$carcID %in% names(plots_wild_valid))]
length(wild_carcs_valid)


# Do NBDA with stn carcass -----------------------------------------------
# Select a stn carcass
id <- 4203377
which_id <- which(unlist(map(stn_carcs, "carcID")) == id)
plots_stn[names(plots_stn) == id][[1]]

stn_carcs <- map(stn_carcs, ~{
  .x %>%
    mutate(datetime_il = with_tz(datetime, tzone = "Israel"),
           dateOnly = lubridate::date(datetime_il))
})

wild_carcs <- map(wild_carcs, ~{
  .x %>%
    mutate(datetime_il = with_tz(datetime, tzone = "Israel"),
           dateOnly = lubridate::date(datetime_il))
})

# Get gps data
# Here is where I think we should convert to Israel time so that the day boundaries make sense.
gps_30days <- get_gps_all(stn_carcs[which_id], gps_combined, days_after, dbf)[[1]]
gps_30days <- gps_30days %>%
  mutate(timestamp_il = with_tz(timestamp, tzone = "Israel"),
         dateOnly = lubridate::date(timestamp_il))
length(unique(gps_30days$dateOnly)) # gps_combined now has 30 days tacked onto the beginning of each of the three month-long hf periods, so i should be able to use the `dbf` 30 days value without worrying about having enough data. Here we're pulling the unique GPS subset for each carcass.
min(gps_30days$dateOnly) == stn_carcs[[which_id]]$dateOnly - days(dbf) # TRUE
max(gps_30days$dateOnly) == stn_carcs[[which_id]]$dateOnly + days(days_after) # FALSE because the get_gps_all function adds 1 day to allow for calculating roost positions. So this should in fact be different.
max(gps_30days$dateOnly) == stn_carcs[[which_id]]$dateOnly + days(days_after+1) # TRUE--one day more

seed_time_before <- 30 #mins
tar_load(ddf)
tar_load(dds)
tar_load(gps_spd)

test <- prepare_nbda_data(gps = gps_30days, 
                          remove_seeds = FALSE, 
                          seed_time_before = NULL, 
                          ddf = ddf, 
                          dds = dds, 
                          gps_spd = gps_spd, 
                          n_hours_gps_dynamic = list(c(-24, 0),
                                                     c(-72, 0)),
                          n_hours_gps_static = list(c(-720, -24), 
                                                    c(-168, -24)),
                          sighting_time_max_hours = 72)
save(test, file = here("data/test.Rda"))

# Networks
## Dynamic
### flight, day by day
# XXX start here--check that the below functions will work with the israel times
fl_bin_sameday <- fix_nets(map(test$gps_data_sameday, ~get_fl_bin(.x, dist = ddf)), test$all_indivs_sorted)
fl_wt_sameday <- fix_nets(map(test$gps_data_sameday, ~get_fl_weighted(.x, dist = ddf)), test$all_indivs_sorted)
### flight, cumulative same day
fl_bin_cumulative_sameday <- fix_nets(map(test$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf)), test$all_indivs_sorted)
fl_wt_cumulative_sameday <- fix_nets(map(test$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf)), test$all_indivs_sorted)
### flight, since 3 days prior
fl_bin_3daysprior <- fix_nets(map(test$gps_data_dynamic_hours_n072_000, ~get_fl_bin(.x, dist = ddf)), test$all_indivs_sorted)
fl_wt_3daysprior <- fix_nets(map(test$gps_data_dynamic_hours_n072_000, ~get_fl_weighted(.x, dist = ddf)), test$all_indivs_sorted)
### flight, since 1 day prior
fl_bin_1daysprior <- fix_nets(map(test$gps_data_dynamic_hours_n024_000, ~get_fl_bin(.x, dist = ddf)), test$all_indivs_sorted)
fl_wt_1daysprior <- fix_nets(map(test$gps_data_dynamic_hours_n024_000, ~get_fl_weighted(.x, dist = ddf)), test$all_indivs_sorted)

## Static
### flight, -30 through -1 days
fl_bin_n720n024 <- fix_nets(list(get_fl_bin(test$gps_data_static_hours_n720_n024, dist = ddf)), test$all_indivs_sorted)
fl_wt_n720n024 <- fix_nets(list(get_fl_weighted(test$gps_data_static_hours_n720_n024, dist = ddf)), test$all_indivs_sorted)
### flight, -7 through -1 days
fl_bin_n168n024 <- fix_nets(list(get_fl_bin(test$gps_data_static_hours_n168_n024, dist = ddf)), test$all_indivs_sorted)
fl_wt_n168n024 <- fix_nets(list(get_fl_weighted(test$gps_data_static_hours_n168_n024, dist = ddf)), test$all_indivs_sorted)

nets_stn <- list("fl_bin_sameday" = fl_bin_sameday,
                  "fl_wt_sameday" = fl_wt_sameday,
                  "fl_bin_cumulative_sameday" = fl_bin_cumulative_sameday,
                  "fl_wt_cumulative_sameday" = fl_wt_cumulative_sameday,
                  "fl_bin_3daysprior" = fl_bin_3daysprior,
                  "fl_wt_3daysprior" = fl_wt_3daysprior,
                  "fl_bin_1daysprior" = fl_bin_1daysprior,
                  "fl_wt_1daysprior" = fl_wt_1daysprior,
                  "fl_bin_n720n024" = fl_bin_n720n024,
                  "fl_wt_n720n024" = fl_wt_n720n024,
                  "fl_bin_n168n024" = fl_bin_n168n024,
                  "fl_wt_n168n024" = fl_wt_n168n024)
save(nets_stn, file = here("data/nets_stn.Rda"))
load(here("data/nets_stn.Rda"))

## Dynamic flight networks, entire day of first sighting, including after first sighting
data_sameday <- nbdaData(label = test$carcID, 
                         assMatrix = make_assMatrix(nets_stn$fl_bin_sameday), 
                         orderAcq = test$oa_nums)
data_sameday_wt <- nbdaData(label = test$carcID,
                            assMatrix = make_assMatrix(nets_stn$fl_wt_sameday),
                            orderAcq = test$oa_nums)
mod_sameday <- oadaFit(data_sameday, type = "social")
mod_sameday_wt <- oadaFit(data_sameday_wt, type = "social")

## Dynamic flight networks, cumulative
data_cumul <- nbdaData(label = test$carcID, 
         assMatrix = make_assMatrix(nets_stn$fl_bin_cumulative_sameday), 
         orderAcq = test$oa_nums)
data_cumul_wt <- nbdaData(label = test$carcID, 
         assMatrix = make_assMatrix(nets_stn$fl_wt_cumulative_sameday), 
         orderAcq = test$oa_nums)
mod_cumul <- oadaFit(data_cumul, type = "social")
mod_cumul_wt <- oadaFit(data_cumul_wt, type = "social")

## Dynamic flight networks, -72 hours through sighting
data_3daysprior <- nbdaData(label = test$carcID, 
                          assMatrix = make_assMatrix(nets_stn$fl_bin_3daysprior), 
                          orderAcq = test$oa_nums)
data_3daysprior_wt <- nbdaData(label = test$carcID, 
                            assMatrix = make_assMatrix(nets_stn$fl_wt_3daysprior), 
                            orderAcq = test$oa_nums)
mod_3daysprior <- oadaFit(data_3daysprior, type = "social")
mod_3daysprior_wt <- oadaFit(data_3daysprior_wt, type = "social")

## Dynamic flight networks, -24 hours through sighting
data_1daysprior <- nbdaData(label = test$carcID, 
                            assMatrix = make_assMatrix(nets_stn$fl_bin_1daysprior), 
                            orderAcq = test$oa_nums)
data_1daysprior_wt <- nbdaData(label = test$carcID, 
                               assMatrix = make_assMatrix(nets_stn$fl_wt_1daysprior), 
                               orderAcq = test$oa_nums)
mod_1daysprior <- oadaFit(data_1daysprior, type = "social")
mod_1daysprior_wt <- oadaFit(data_1daysprior_wt, type = "social")

## Static flight networks, -720 hours (30 days prior) through -24 hours (1 day prior)
data_n720n024 <- nbdaData(label = test$carcID, 
                            assMatrix = make_assMatrix(nets_stn$fl_bin_n720n024), 
                            orderAcq = test$oa_nums)
data_n720n024_wt <- nbdaData(label = test$carcID, 
                        assMatrix = make_assMatrix(nets_stn$fl_wt_n720n024), 
                        orderAcq = test$oa_nums)
mod_n720n024 <- oadaFit(data_n720n024, type = "social")
mod_n720n024_wt <- oadaFit(data_n720n024_wt, type = "social")

## Static flight networks, -168 hours (7 days prior) through -24 hours (1 day prior)
data_n168n024 <- nbdaData(label = test$carcID, 
                          assMatrix = make_assMatrix(nets_stn$fl_bin_n168n024), 
                          orderAcq = test$oa_nums)
data_n168n024_wt <- nbdaData(label = test$carcID, 
                             assMatrix = make_assMatrix(nets_stn$fl_wt_n168n024), 
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
  mutate(sig = case_when(outputPar-se > 0 ~ T, .default = F)) %>%
  filter(mod != "static_3001") %>%
  ggplot(aes(x = mod, col = net_type))+
  geom_segment(aes(y = outputPar-se, yend = outputPar + se, linetype = sig), linewidth = 0.5)+
  geom_point(aes(y = outputPar, shape = sig), size = 3)+
  theme_classic()+
 # theme(legend.position = "none")+
  scale_linetype_manual(values = c(2, 1))+
  scale_shape_manual(values = c(1, 19))+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  labs(y = "S (social transmission strength)",
       x = "Model", color = "Network type",
       shape = "Evidence for\nsocial\ntransmission",
       linetype = "Evidence for\nsocial\ntransmission",
       caption = id)+
  coord_flip() # niiice


# Do NBDA with a wild carcass -----------------------------------------------
# Select a wild carcass
names(plots_wild_valid)[1]
id_wild <- 52
which_id_wild <- which(unlist(map(wild_carcs_valid, "carcID")) == id_wild)
plots_wild_valid[[which_id_wild]]

# Get gps data
gps_30days_wild <- get_gps_all(wild_carcs_valid[which_id_wild], gps_combined, days_after, dbf)[[1]]
length(unique(gps_30days_wild$dateOnly)) # gps_combined now has 30 days tacked onto the beginning of each of the three month-long hf periods, so i should be able to use the `dbf` 30 days value without worrying about having enough data. Here we're pulling the unique GPS subset for each carcass.
min(gps_30days_wild$dateOnly) == wild_carcs_valid[[which_id_wild]]$date - days(dbf)
max(gps_30days_wild$dateOnly) == wild_carcs_valid[[which_id_wild]]$date + days(days_after) # because the get_gps_all function adds 1 day to allow for calculating roost positions. So this should in fact be different.
max(gps_30days_wild$dateOnly) == wild_carcs_valid[[which_id_wild]]$date + days(days_after+1) # one day more

test_wild <- prepare_nbda_data(gps = gps_30days_wild, 
                          remove_seeds = FALSE, 
                          seed_time_before = NULL, 
                          ddf = ddf, 
                          dds = dds, 
                          gps_spd = gps_spd, 
                          n_hours_gps_dynamic = list(c(-24, 0),
                                                     c(-72, 0)),
                          n_hours_gps_static = list(c(-720, -24), 
                                                    c(-168, -24)),
                          sighting_time_max_hours = 72)
save(test_wild, file = here("data/test_wild.Rda"))
# XXX Note: we're going to need to back it up an hour, and I haven't thought through how to do that yet. 

# # Networks
# ## Dynamic
# ### flight, day by day
# fl_bin_sameday_wild <- fix_nets(map(test_wild$gps_data_sameday, ~get_fl_bin(.x, dist = ddf)), test_wild$all_indivs_sorted)
# fl_wt_sameday_wild <- fix_nets(map(test_wild$gps_data_sameday, ~get_fl_weighted(.x, dist = ddf)), test_wild$all_indivs_sorted)
# ### flight, cumulative same day
# fl_bin_cumulative_sameday_wild <- fix_nets(map(test_wild$gps_data_cumulative, ~get_fl_bin(.x, dist = ddf)), test_wild$all_indivs_sorted)
# fl_wt_cumulative_sameday_wild <- fix_nets(map(test_wild$gps_data_cumulative, ~get_fl_weighted(.x, dist = ddf)), test_wild$all_indivs_sorted)
# ### flight, since 3 days prior
# fl_bin_3daysprior_wild <- fix_nets(map(test_wild$gps_data_dynamic_hours_n072_000, ~get_fl_bin(.x, dist = ddf)), test_wild$all_indivs_sorted)
# fl_wt_3daysprior_wild <- fix_nets(map(test_wild$gps_data_dynamic_hours_n072_000, ~get_fl_weighted(.x, dist = ddf)), test_wild$all_indivs_sorted)
# ### flight, since 1 day prior
# fl_bin_1daysprior_wild <- fix_nets(map(test_wild$gps_data_dynamic_hours_n024_000, ~get_fl_bin(.x, dist = ddf)), test_wild$all_indivs_sorted)
# fl_wt_1daysprior_wild <- fix_nets(map(test_wild$gps_data_dynamic_hours_n024_000, ~get_fl_weighted(.x, dist = ddf)), test_wild$all_indivs_sorted)
# 
# ## Static
# ### flight, -30 through -1 days
# fl_bin_n720n024_wild <- fix_nets(list(get_fl_bin(test_wild$gps_data_static_hours_n720_n024, dist = ddf)), test_wild$all_indivs_sorted)
# fl_wt_n720n024_wild <- fix_nets(list(get_fl_weighted(test_wild$gps_data_static_hours_n720_n024, dist = ddf)), test_wild$all_indivs_sorted)
# ### flight, -7 through -1 days
# fl_bin_n168n024_wild <- fix_nets(list(get_fl_bin(test_wild$gps_data_static_hours_n168_n024, dist = ddf)), test_wild$all_indivs_sorted)
# fl_wt_n168n024_wild <- fix_nets(list(get_fl_weighted(test_wild$gps_data_static_hours_n168_n024, dist = ddf)), test_wild$all_indivs_sorted)
# 
# nets_wild <- list("fl_bin_sameday_wild" = fl_bin_sameday_wild, 
#                   "fl_wt_sameday_wild" = fl_wt_sameday_wild,
#                   "fl_bin_cumulative_sameday_wild" = fl_bin_cumulative_sameday_wild, 
#                   "fl_wt_cumulative_sameday_wild" = fl_wt_cumulative_sameday_wild, 
#                   "fl_bin_3daysprior_wild" = fl_bin_3daysprior_wild, 
#                   "fl_wt_3daysprior_wild" = fl_wt_3daysprior_wild, 
#                   "fl_bin_1daysprior_wild" = fl_bin_1daysprior_wild, 
#                   "fl_wt_1daysprior_wild" = fl_wt_1daysprior_wild, 
#                   "fl_bin_n720n024_wild" = fl_bin_n720n024_wild, 
#                   "fl_wt_n720n024_wild" = fl_wt_n720n024_wild, 
#                   "fl_bin_n168n024_wild" = fl_bin_n168n024_wild, 
#                   "fl_wt_n168n024_wild" = fl_wt_n168n024_wild)
# save(nets_wild, file = here("data/nets_wild.Rda"))
load(here("data/nets_wild.Rda"))

## Dynamic flight networks, entire day of first sighting, including after first sighting
data_sameday_wild <- nbdaData(label = test_wild$carcID, 
                         assMatrix = make_assMatrix(nets_wild$fl_bin_sameday_wild), 
                         orderAcq = test_wild$oa_nums)
data_sameday_wt_wild <- nbdaData(label = test_wild$carcID,
                            assMatrix = make_assMatrix(nets_wild$fl_wt_sameday_wild),
                            orderAcq = test_wild$oa_nums)
mod_sameday_wild <- oadaFit(data_sameday_wild, type = "social")
mod_sameday_wt_wild <- oadaFit(data_sameday_wt_wild, type = "social")

## Dynamic flight networks, cumulative
data_cumul_wild <- nbdaData(label = test_wild$carcID, 
                       assMatrix = make_assMatrix(nets_wild$fl_bin_cumulative_sameday_wild), 
                       orderAcq = test_wild$oa_nums)
data_cumul_wt_wild <- nbdaData(label = test_wild$carcID, 
                          assMatrix = make_assMatrix(nets_wild$fl_wt_cumulative_sameday_wild), 
                          orderAcq = test_wild$oa_nums)
mod_cumul_wild <- oadaFit(data_cumul_wild, type = "social")
mod_cumul_wt_wild <- oadaFit(data_cumul_wt_wild, type = "social")

## Dynamic flight networks, -72 hours through sighting
data_3daysprior_wild <- nbdaData(label = test_wild$carcID, 
                            assMatrix = make_assMatrix(nets_wild$fl_bin_3daysprior_wild), 
                            orderAcq = test_wild$oa_nums)
data_3daysprior_wt_wild <- nbdaData(label = test_wild$carcID, 
                               assMatrix = make_assMatrix(nets_wild$fl_wt_3daysprior_wild), 
                               orderAcq = test_wild$oa_nums)
mod_3daysprior_wild <- oadaFit(data_3daysprior_wild, type = "social")
#mod_3daysprior_wt_wild <- oadaFit(data_3daysprior_wt_wild, type = "social")

## Dynamic flight networks, -24 hours through sighting
data_1daysprior_wild <- nbdaData(label = test_wild$carcID, 
                            assMatrix = make_assMatrix(nets_wild$fl_bin_1daysprior_wild), 
                            orderAcq = test_wild$oa_nums)
data_1daysprior_wt_wild <- nbdaData(label = test_wild$carcID, 
                               assMatrix = make_assMatrix(nets_wild$fl_wt_1daysprior_wild), 
                               orderAcq = test_wild$oa_nums)
mod_1daysprior_wild <- oadaFit(data_1daysprior_wild, type = "social")
mod_1daysprior_wt_wild <- oadaFit(data_1daysprior_wt_wild, type = "social")

## Static flight networks, -720 hours (30 days prior) through -24 hours (1 day prior)
data_n720n024_wild <- nbdaData(label = test_wild$carcID, 
                          assMatrix = make_assMatrix(nets_wild$fl_bin_n720n024_wild), 
                          orderAcq = test_wild$oa_nums)
data_n720n024_wt_wild <- nbdaData(label = test_wild$carcID, 
                             assMatrix = make_assMatrix(nets_wild$fl_wt_n720n024_wild), 
                             orderAcq = test_wild$oa_nums)
mod_n720n024_wild <- oadaFit(data_n720n024_wild, type = "social")
mod_n720n024_wt_wild <- oadaFit(data_n720n024_wt_wild, type = "social")

## Static flight networks, -168 hours (7 days prior) through -24 hours (1 day prior)
data_n168n024_wild <- nbdaData(label = test_wild$carcID, 
                          assMatrix = make_assMatrix(nets_wild$fl_bin_n168n024_wild), 
                          orderAcq = test_wild$oa_nums)
data_n168n024_wt_wild <- nbdaData(label = test_wild$carcID, 
                             assMatrix = make_assMatrix(nets_wild$fl_wt_n168n024_wild), 
                             orderAcq = test_wild$oa_nums)
mod_n168n024_wild <- oadaFit(data_n168n024_wild, type = "social")
#mod_n168n024_wt_wild <- oadaFit(data_n168n024_wt_wild, type = "social")

mods_list_wild <- list("dynamic_sameday_bin" = mod_sameday_wild, 
                  "dynamic_cumulative_bin" = mod_cumul_wild, 
                  "dynamic_3days_bin" = mod_3daysprior_wild, 
                  "dynamic_1days_bin" = mod_1daysprior_wild,
                  "static_n720_n024_bin" = mod_n720n024_wild, 
                  "static_n168_n024_bin" = mod_n168n024_wild,
                  "dynamic_sameday_wt" = mod_sameday_wt_wild,
                  "dynamic_cumulative_wt" = mod_cumul_wt_wild,
                  #"dynamic_3days_wt" = mod_3daysprior_wt_wild,
                  "dynamic_1days_wt" = mod_1daysprior_wt_wild,
                  "static_n720_n024_wt" = mod_n720n024_wt_wild#,
                  #"static_n168_n024_wt" = mod_n168n024_wt_wild
)
stats_wild <- purrr::list_rbind(map(mods_list_wild, getmodstats))
stats_wild$mod <- names(mods_list_wild)
stats_wild <- stats_wild %>%
  mutate(net_type = factor(str_extract(mod, "static|dynamic"), levels = c("static", "dynamic")),
         wt = str_detect(mod, "wt"))

stats_wild %>%
  mutate(sig = case_when(outputPar-se > 0 ~ T, .default = F)) %>%
  ggplot(aes(x = mod, col = net_type))+
  geom_segment(aes(y = outputPar-se, yend = outputPar + se, linetype = sig), linewidth = 0.5)+
  geom_point(aes(y = outputPar, shape = sig), size = 3)+
  theme_classic()+
  # theme(legend.position = "none")+
  scale_linetype_manual(values = c(2, 1))+
  scale_shape_manual(values = c(1, 19))+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  labs(y = "S (social transmission strength)",
       x = "Model", color = "Network type",
       shape = "Evidence for\nsocial\ntransmission",
       linetype = "Evidence for\nsocial\ntransmission",
       caption = id)+
  coord_flip()
# test removing seeds [TO DO!]
