# Testing stBayes

library(STbayes)
library(ggplot2)
library(tidyverse)
library(posterior)
library(targets)
library(sf)
lapply(list.files("R", full.names = TRUE), source) 
# using just one carcass as an example
tar_load(gps_spd)
tar_load(hours_after_carcass)
tar_load(data_cumul_wt_3) # this is a list of nbdaData objects
nbda_data <- data_cumul_wt_3[[4]] # carcass 4417687, which is the 4th element of the 3rd list of 10, so element 24 of stn_carcs

tar_load(stn_carcs)
carc <- stn_carcs[[24]]
event_time <- carc$datetime_il
event_date <- lubridate::date(event_time)

# Saving gps data as temp file because it takes too long otherwise.
# tar_load(stn_gps_30days)
# gps <- stn_gps_30days[[24]] # the gps data that we will use for this carcass
# write_rds(gps, "data/created/gps_for_STbayes.RDS")
gps <- readRDS("data/created/gps_for_STbayes.RDS")
gps_day1 <- gps %>% filter(date_il == event_date)
dim(gps_day1)

suntimes <- suncalc::getSunlightTimes(date = sort(unique(gps$date_il)), lat = 31.434306, lon = 34.991889, keep = c("sunrise", "sunset"), tz = "Israel") %>% select("date_il" = date, sunrise, sunset)

gps <- left_join(gps, suntimes)
gps_day1 <- left_join(gps_day1, suntimes)
test_daylight <- gps %>% filter(timestamp_il >= sunrise & timestamp_il <= sunset)
test_daylight_day1 <- gps_day1 %>% filter(timestamp_il >= sunrise & timestamp_il <= sunset)
# table(gps$daylight)
# table(test_daylight$daylight) # sweet, we only have daylight, and it's the same number as before, which means this worked.
gps <- test_daylight
gps_day1 <- test_daylight_day1

night_df <- suntimes %>%
  filter(date_il >= lubridate::date(event_time)) %>% # IMPORTANT!! only stuff since the carcass date; otherwise these numbers will be wrong.
  mutate(prev_sunset = lag(sunset),
         night_hrs = as.numeric(difftime(sunrise, prev_sunset, units = "hours")),
         cumul_night_hrs = cumsum(ifelse(is.na(night_hrs), 0, night_hrs)) + night_hrs*0,
         cumul_night_hrs = replace_na(cumul_night_hrs, 0))

gps <- gps %>%
  left_join(select(night_df, date_il, cumul_night_hrs), by = "date_il") %>%
  mutate(cumul_night_hrs = replace_na(cumul_night_hrs, 0)) %>% # not sure if this will help but maybe
  mutate(daytime_since_carcass = case_when(time_since_carcass >= 0 ~ as.numeric(time_since_carcass)-cumul_night_hrs,
                                           .default = NA)) %>%
  arrange(timestamp_il)
table(gps$date_il, gps$cumul_night_hrs) # should show a stepped pattern beginning with the date of the carcass. Looks good.

gps_day1 <- gps_day1 %>%
  left_join(select(night_df, date_il, cumul_night_hrs), by = "date_il") %>%
  mutate(cumul_night_hrs = replace_na(cumul_night_hrs, 0)) %>% # not sure if this will help but maybe
  mutate(daytime_since_carcass = case_when(time_since_carcass >= 0 ~ as.numeric(time_since_carcass)-cumul_night_hrs,
                                           .default = NA)) %>%
  arrange(timestamp_il)
table(gps_day1$date_il, gps_day1$cumul_night_hrs) # should be only one date, so this is a bit silly

carc_id <- carc$carcID
gps$year <- lubridate::year(gps$date_il)
gps$ground_speed <- as.numeric(gps$ground_speed)
gps$time_since_carcass <- as.numeric(gps$time_since_carcass)

# Identify seed individuals if needed
tar_load(stb_mins) # number of minutes before that is defined as seeds
tar_load(ddf)
tar_load(dds)
stb_mins # 30 mins
seeds <- character(0)
time_window <- stb_mins / 60

seeds <- get_seeds(gps, ddf, dds, gps_spd, time_col = "time_since_carcass", stb_mins = stb_mins) # still using time_since_carcass since it goes back farther than daytime_since_carcass.
seeds # these are the names of the seed individuals

# Get first sightings
all_indivs_sorted <- sort(unique(as.character(gps$individual_local_identifier)))
all_indivs_sorted_day1 <- sort(unique(as.character(gps_day1$individual_local_identifier)))

gps_diffusion <- gps %>% filter(time_since_carcass >= 0)
gps_diffusion_day1 <- gps_day1 %>% filter(time_since_carcass >= 0)
first_sightings <- get_first_sightings(gps_diffusion, hours_after_carcass, gps_spd, ddf, dds, seeds)
first_sightings_day1 <- get_first_sightings(gps_diffusion_day1, hours_after_carcass, gps_spd, ddf, dds, seeds)

gps_fornetwork <- gps_diffusion %>%
  filter(time_since_carcass >= 0 & time_since_carcass <= as.numeric(hours_after_carcass)) %>%
  mutate(time = as.numeric(daytime_since_carcass)*60*60) %>% # this will now correspond to the numeric times in test_event_data.
  filter(time >= 0)

gps_fornetwork_day1 <- gps_diffusion_day1 %>%
  filter(time_since_carcass >= 0 & time_since_carcass <= as.numeric(hours_after_carcass)) %>%
  mutate(time = as.numeric(daytime_since_carcass)*60*60) %>% # this will now correspond to the numeric times in test_event_data.
  filter(time >= 0)

write_rds(gps_fornetwork, file = "data/created/gps_1.RDS")
write_rds(gps_fornetwork_day1, file = "data/created/gps_1_day1.RDS")
write_rds(first_sightings, file = "data/created/first_sightings_1.RDS")
write_rds(first_sightings_day1, file = "data/created/first_sightings_1_day1.RDS")

# Now we have the event data; time to format it the way that STbayes needs.
event_data <- format_event_data_new(first_sightings, seeds, all_indivs_sorted, time_col = "daytime_since_carcass", carc = carc, gps_fornetwork = gps_fornetwork)
event_data_day1 <- format_event_data_new(first_sightings_day1, seeds, all_indivs_sorted_day1, time_col = "daytime_since_carcass", carc = carc, gps_fornetwork = gps_fornetwork_day1)

# "if the user’s observation period included 10 events and the dataset does contain censored individuals, they should supply edge weights from 11 networks in total, where time=1 should contain the network representing the period from [t0,te1), time=2 represents [te1,te2), and time=11 represents from [te10,tend]. NB: If there are censored individuals, the end of the observation period should necessarily be larger than the time of the final event (event_data$t_end > max(event_data$time)."
# So, we have 62 events, which means we should be supplying 63 networks
# Note: it does NOT say what to do if there are seed individuals... I assume I don't provide a network for those, since they're set to 0, so the first one will just be from 0 through te1? (e1 = event 1)
# Also, what they're saying about the end of the observation period seems to contradict how they said to encode the censored individuals (i.e. set them to tend+1)

cutpoints <- unique(event_data$time)
cutpoints_day1 <- unique(event_data_day1$time)
if(!(0 %in% cutpoints)){
  cutpoints <- c(0, cutpoints)}
if(!(0 %in% cutpoints_day1)){
  cutpoints_day1 <- c(0, cutpoints_day1)}

cutpoints[length(cutpoints)] <- event_data$t_end[1]
cutpoints_day1[length(cutpoints_day1)] <- event_data_day1$t_end[1]
length(cutpoints) # need 64 cutpoints so we can have 63 bins so we can define 62 events plus censored indivs.
length(cutpoints_day1) # need 23 cutpoints so we can have 22 bins so we can define 21 events plus censored indivs
bins <- sort(unique(cut(gps_fornetwork$time, breaks = cutpoints)))
bins_day1 <- sort(unique(cut(gps_fornetwork_day1$time, breaks = cutpoints_day1)))
gps_fornetwork$network <- cut(gps_fornetwork$time, breaks = cutpoints)
gps_fornetwork_day1$network <- cut(gps_fornetwork_day1$time, breaks = cutpoints_day1)
lvls <- levels(gps_fornetwork$network)
lvls_day1 <- levels(gps_fornetwork_day1$network)
gps_fornetwork <- gps_fornetwork %>%
  filter(!is.na(network)) # remove NAs (after the diffusion period)
gps_fornetwork_day1 <- gps_fornetwork_day1 %>%
  filter(!is.na(network))
missing_intervals <- levels(gps_fornetwork$network)[!(levels(gps_fornetwork$network) %in% gps_fornetwork$network)]
missing_intervals_day1 <- levels(gps_fornetwork_day1$network)[!(levels(gps_fornetwork_day1$network) %in% gps_fornetwork_day1$network)]

# need to add a date for the missing intervals so the later code will work
missing_intervals_lower <- as.numeric(str_extract(missing_intervals, "(?<=\\()[0-9]+"))
missing_intervals_upper <- as.numeric(str_extract(missing_intervals, "(?<=\\,)[0-9]+(?=\\])"))
missing_intervals_day1_lower <- as.numeric(str_extract(missing_intervals_day1, "(?<=\\()[0-9]+"))
missing_intervals_day1_upper <- as.numeric(str_extract(missing_intervals_day1, "(?<=\\,)[0-9]+(?=\\])"))

dates_before <- as.Date(unlist(purrr::map(missing_intervals_lower, ~{gps_fornetwork %>% filter(time < .x) %>% arrange(timestamp_il) %>% pull(date_il) %>% max()})))
dates_after <- as.Date(unlist(purrr::map(missing_intervals_upper, ~{gps_fornetwork %>% filter(time > .x) %>% arrange(timestamp_il) %>% pull(date_il) %>% min()})))

dates_before_day1 <- as.Date(unlist(purrr::map(missing_intervals_day1_lower, ~{gps_fornetwork_day1 %>% filter(time < .x) %>% arrange(timestamp_il) %>% pull(date_il) %>% max()})))
dates_after_day1 <- as.Date(unlist(purrr::map(missing_intervals_day1_upper, ~{gps_fornetwork_day1 %>% filter(time > .x) %>% arrange(timestamp_il) %>% pull(date_il) %>% min()})))

# I think for now I'm just going to take the date before
to_add <- data.frame(network = missing_intervals, date_il = dates_before) # this is super buggy and i need to return to it!
to_add_day1 <- data.frame(network = missing_intervals_day1, date_il = dates_before_day1)

if(nrow(to_add) > 0){
  gps_fornetwork <- bind_rows(gps_fornetwork, to_add)}
if(nrow(to_add_day1) > 0){
  gps_fornetwork_day1 <- bind_rows(gps_fornetwork_day1, to_add_day1)}

gps_fornetwork <- gps_fornetwork %>%
  mutate(network = factor(network, levels = lvls)) %>%
  arrange(time_since_carcass)
gps_fornetwork_day1 <- gps_fornetwork_day1 %>%
  mutate(network = factor(network, levels = lvls_day1)) %>%
  arrange(time_since_carcass)

# 3/31 addition: let's try the last hour before each timepoint
gps_list_1hr <- map(cutpoints[2:length(cutpoints)], ~{
  onehourbefore <- .x-3600
  dat <- gps_fornetwork %>% filter(time >= onehourbefore & time <= .x)
  return(dat)
})
gps_list_1hr_day1 <- map(cutpoints_day1[2:length(cutpoints_day1)], ~{
  onehourbefore <- .x-3600
  dat <- gps_fornetwork_day1 %>% filter(time >= onehourbefore & time <= .x)
  return(dat)
})

gps_list <- gps_fornetwork %>% arrange(network) %>% group_split(network, .keep = TRUE)
gps_list_day1 <- gps_fornetwork_day1 %>% arrange(network) %>% group_split(network, .keep = TRUE)
length(gps_list) == length(lvls) # yay!
length(gps_list_day1) == length(lvls_day1)

tar_load(rp)
gps_list <- map(gps_list, ~{
  if(nrow(.x) == 1 & all(is.na(.x$individual_local_identifier))){
    return(.x[0,])
  }else{
    return(.x)
  }
})

gps_list_day1 <- map(gps_list_day1, ~{
  if(nrow(.x) == 1 & all(is.na(.x$individual_local_identifier))){
    return(.x[0,])
  }else{
    return(.x)
  }
})

# dynamic_networks <- map(gps_list, ~get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd))
# dynamic_networks_day1 <- map(gps_list_day1, ~get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd))
# dynamic_networks_1hr <- map(gps_list_1hr, ~get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd))
# dynamic_networks_1hr_day1 <- map(gps_list_1hr_day1, ~get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd))
# dynamic_networks_fixed <- fix_nets(nets = dynamic_networks, indivs = all_indivs_sorted)
# dynamic_networks_fixed_day1 <- fix_nets(nets = dynamic_networks_day1, indivs = all_indivs_sorted_day1)
# dynamic_networks_fixed_1hr <- fix_nets(nets = dynamic_networks_1hr, indivs = all_indivs_sorted)
# dynamic_networks_fixed_1hr_day1 <- fix_nets(nets = dynamic_networks_1hr_day1, indivs = all_indivs_sorted_day1)
# map(dynamic_networks_fixed, dim)
# map(dynamic_networks_fixed_day1, dim)
# 
# networks_long <- map(dynamic_networks_fixed, ~{
#   out <- .x %>% rownames_to_column(var = "focal") %>%
#     pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri") %>%
#     mutate(trial = carc_id)
#   return(out)
# })
# networks_long_day1 <- map(dynamic_networks_fixed_day1, ~{
#   out <- .x %>% rownames_to_column(var = "focal") %>%
#     pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri") %>%
#     mutate(trial = carc_id)
#   return(out)
# })
# 
# networks_long_1hr <- map(dynamic_networks_fixed_1hr, ~{
#   out <- .x %>% rownames_to_column(var = "focal") %>%
#     pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri") %>%
#     mutate(trial = carc_id)
#   return(out)
# })
# networks_long_day1_1hr <- map(dynamic_networks_fixed_1hr_day1, ~{
#   out <- .x %>% rownames_to_column(var = "focal") %>%
#     pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri") %>%
#     mutate(trial = carc_id)
#   return(out)
# })
# 
# networks_long_dynamic <- purrr::list_rbind(networks_long, names_to = "time") # this creates a numeric column for "time", which is how stbayes wants it--sequential integer values, not group names.
# networks_long_dynamic_day1 <- purrr::list_rbind(networks_long_day1, names_to = "time")
# networks_long_dynamic_1hr <- purrr::list_rbind(networks_long_1hr, names_to = "time") # this creates a numeric column for "time", which is how stbayes wants it--sequential integer values, not group names.
# networks_long_dynamic_day1_1hr <- purrr::list_rbind(networks_long_day1_1hr, names_to = "time")
# write_rds(networks_long_dynamic, file = "data/created/networks_long_dynamic_fixed.RDS")
# write_rds(networks_long_dynamic_day1, file = "data/created/networks_long_dynamic_fixed_day1.RDS")
# write_rds(networks_long_dynamic_1hr, file = "data/created/networks_long_dynamic_fixed_1hr.RDS")
# write_rds(networks_long_dynamic_day1_1hr, file = "data/created/networks_long_dynamic_fixed_day1_1hr.RDS")

networks_long_dynamic <- readRDS("data/created/networks_long_dynamic_fixed.RDS")
networks_long_dynamic_day1 <- readRDS("data/created/networks_long_dynamic_fixed_day1.RDS")
networks_long_dynamic_1hr <- readRDS("data/created/networks_long_dynamic_fixed_1hr.RDS")
networks_long_dynamic_day1_1hr <- readRDS("data/created/networks_long_dynamic_fixed_day1_1hr.RDS")

# Network must contain all individuals
# "The networks dataframe is used as the reference for all unique IDs, thus each ID must be included at least once in either the focal or other column. If a dyad is absent, their connection is assumed to be zero."
all(sort(unique(c(networks_long_dynamic$focal, networks_long_dynamic$other))) == all_indivs_sorted) #TRUE
all(sort(unique(c(networks_long_dynamic_day1$focal, networks_long_dynamic_day1$other))) == all_indivs_sorted_day1) #TRUE

# ILVs --------------------------------------------------------------------
## AGE
# This carcass is from 2023, and age is not time-varying within this diffusion
age_ilv <- gps %>%
  st_drop_geometry() %>%
  select(individual_local_identifier, age_2023) %>%
  distinct() %>%
  mutate(age_2023_norm = scale(age_2023, center = TRUE, scale = TRUE)[,1]) # both scaling and centering. I'm not 100% sure this is right

age_ilv_day1 <- gps_day1 %>%
  st_drop_geometry() %>%
  select(individual_local_identifier, age_2023) %>%
  distinct() %>%
  mutate(age_2023_norm = scale(age_2023, center = TRUE, scale = TRUE)[,1])

## DISTANCE FROM CARCASS
# "For example, the value of dist_from_resource at time=1 should reflect the average distance of the individual to the resource from the start of the observation period to the first event."
# In the sample, they assume that the data are normally distributed. Mine aren't.
dists_dyn <- map(gps_list, ~{
  step1 <- .x %>% 
    st_drop_geometry() %>%
    arrange(individual_local_identifier, time_since_carcass) %>%
    group_by(individual_local_identifier) %>%
    summarize(mean_dist_to_carcass = mean(dist_to_carcass),
              mean_dist_to_carcass_sqared = mean_dist_to_carcass^2)
  missing <- all_indivs_sorted[!(all_indivs_sorted %in% step1$individual_local_identifier)]
  missing_df <- data.frame(individual_local_identifier = missing, mean_dist_to_carcass = NA, mean_dist_to_carcass_squared = NA) # keeping the values missing so we can scale, but adding the indivs
  step2 <- bind_rows(step1, missing_df) %>%
    filter(!is.na(individual_local_identifier))
  return(step2)
}) %>% purrr::list_rbind(names_to = "time") %>%
  mutate(mean_dist_to_carcass_norm = scale(log(mean_dist_to_carcass), center = T, scale = T)[,1],
         mean_dist_to_carcass_norm = replace_na(mean_dist_to_carcass_norm, 0),
         mean_dist_to_carcass_squared_norm = scale(log(mean_dist_to_carcass_squared), center = T, scale = T)[,1],
         mean_dist_to_carcass_squared_norm = replace_na(mean_dist_to_carcass_squared_norm, 0)) # distance is log-transformed (to make it more normal) AND scaled/centered. Yuck!

dists_dyn_day1 <- map(gps_list_day1, ~{
  step1 <- .x %>% 
    st_drop_geometry() %>%
    arrange(individual_local_identifier, time_since_carcass) %>%
    group_by(individual_local_identifier) %>%
    summarize(mean_dist_to_carcass = mean(dist_to_carcass),
              mean_dist_to_carcass_sqared = mean_dist_to_carcass^2)
  missing <- all_indivs_sorted_day1[!(all_indivs_sorted_day1 %in% step1$individual_local_identifier)]
  missing_df <- data.frame(individual_local_identifier = missing, mean_dist_to_carcass = NA, mean_dist_to_carcass_squared = NA) # keeping the values missing so we can scale, but adding the indivs
  step2 <- bind_rows(step1, missing_df) %>%
    filter(!is.na(individual_local_identifier))
  return(step2)
}) %>% purrr::list_rbind(names_to = "time") %>%
  mutate(mean_dist_to_carcass_norm = scale(log(mean_dist_to_carcass), center = T, scale = T)[,1],
         mean_dist_to_carcass_norm = replace_na(mean_dist_to_carcass_norm, 0),
         mean_dist_to_carcass_squared_norm = scale(log(mean_dist_to_carcass_squared), center = T, scale = T)[,1],
         mean_dist_to_carcass_squared_norm = replace_na(mean_dist_to_carcass_squared_norm, 0)) # distance is log-transformed (to make it more normal) AND scaled/centered. Yuck!

#Time-varying ILVs
#Below we add distance from resource as a time-varying ILV. Similar to dynamic networks, these need to be summarized per inter-event interval. For example, the value of dist_from_resource at time=1 should reflect the average distance of the individual to the resource from the start of the observation period to the first event.

## PROPORTION INFORMED ROOSTMATES
prop_informed <- readRDS("data/created/prop_informed.RDS")
# now, this is going on a per-date schedule. Need to go on a per-time-period schedule. So I need to match the sightings to dates.
# the problem is that some of the time periods contain multiple dates:
dates <- gps_fornetwork %>%
  mutate(network = factor(network, levels = lvls)) %>%
  st_drop_geometry() %>%
  select(network, date_il) %>%
  arrange(network, desc(date_il)) %>%
  distinct() %>%
  group_by(network) %>%
  slice(1) # Using most recent (later) date for now.

informed_list <- map(dates$date_il, ~{
  prop_informed %>% filter(roost_date == .x-lubridate::days(1))
})
informed <- purrr::list_rbind(informed_list, names_to = "time") %>%
  select(time, "id" = ID1, n_roostmates, prop_informed) %>%
  mutate(n_roostmates = replace_na(n_roostmates, 0),
         prop_informed = replace_na(prop_informed, 0)) %>%
  mutate(prop_informed_norm = scale(prop_informed, scale = T, center = T)[,1])

# don't need to create the equivalent of `informed` for the first day only, since by definition nobody roosted with anyone informed the day before the carcass was placed.

## Combine all the ILVs
# This is for the constant ILVs (age). We will also need time-varying ILVs separately
ILV_c <- age_ilv %>%
  rename("id" = individual_local_identifier,
         age = age_2023_norm) %>%
  select(id, age) %>%
  mutate(age = replace_na(age, 0)) # set unknown ages to the mean

ILV_c_day1 <- age_ilv_day1 %>%
  rename("id" = individual_local_identifier,
         age = age_2023_norm) %>%
  select(id, age) %>%
  mutate(age = replace_na(age, 0)) # set unknown ages to the mean

ILV_tv <- dists_dyn %>%
  select("id" = individual_local_identifier,
         time, mean_dist_to_carcass_norm, mean_dist_to_carcass_squared_norm) %>%
  left_join(informed, by = c("id", "time")) %>%
  mutate(trial = carc_id) %>%
  select(trial, id, time, mean_dist_to_carcass_norm, mean_dist_to_carcass_squared_norm, prop_informed_norm) %>%
  mutate(across(c("mean_dist_to_carcass_norm", "mean_dist_to_carcass_squared_norm", "prop_informed_norm"), ~replace_na(.x, 0)))

ILV_tv_day1 <- dists_dyn_day1 %>%
  select("id" = individual_local_identifier,
         time, mean_dist_to_carcass_norm, mean_dist_to_carcass_squared_norm) %>%
  # left_join(informed, by = c("id", "time")) %>% # not relevant for day 1
  mutate(trial = carc_id) %>%
  select(trial, id, time, mean_dist_to_carcass_norm, mean_dist_to_carcass_squared_norm#, 
         #prop_informed_norm
  ) %>%
  mutate(across(c("mean_dist_to_carcass_norm", "mean_dist_to_carcass_squared_norm"#,
                  #"prop_informed_norm"
  ), ~replace_na(.x, 0)))

ILV_tv %>% ggplot(aes(x = prop_informed_norm))+geom_histogram()+facet_wrap(~factor(time)) # the distributions of prop_informed_norm are really weird; I wonder if instead I should have this be categorical (most, some, few) or something...

# Data lists --------------------------------------------------------------
#We need to explicitly tell STbayes which variables are additive (acting independently on intrinsic or social rates) and multiplicative (same effect estimated for intrinsic and social rates). Below, I have specified age as acting independently on the intrinsic and social rate, sex as acting only on the social rate, and weight as a multiplicative effect. Two betas will be estimated for age, and a single beta will be estimated for sex and weight.

data_list <- import_user_STb(event_data = event_data, 
                             networks = networks_long_dynamic,
                             network_type = "undirected",
                             ILV_c = ILV_c,
                             ILV_tv = ILV_tv,
                             ILVi = c("age", "mean_dist_to_carcass_norm", "prop_informed_norm"),
                             ILVs = c("age", "prop_informed_norm")) 
write_rds(data_list, file="data/data_lists/dynamic_daylight_ilvs1_fixed.RDS")

data_list_sq <- import_user_STb(event_data = event_data, 
                                networks = networks_long_dynamic,
                                network_type = "undirected",
                                ILV_c = ILV_c,
                                ILV_tv = ILV_tv,
                                ILVi = c("age", "mean_dist_to_carcass_squared_norm", "prop_informed_norm"),
                                ILVs = c("age", "prop_informed_norm")) 
write_rds(data_list_sq, file="data/data_lists/dynamic_daylight_ilvs1_fixed_sq.RDS")

data_list_day1 <- import_user_STb(event_data = event_data_day1, 
                                  networks = networks_long_dynamic_day1,
                                  network_type = "undirected",
                                  ILV_c = ILV_c_day1,
                                  ILV_tv = ILV_tv_day1,
                                  ILVi = c("age", "mean_dist_to_carcass_squared_norm"),
                                  ILVs = c("age")) 
write_rds(data_list_day1, file="data/data_lists/dynamic_daylight_ilvs1_fixed_day1.RDS")

data_list_1hr <- import_user_STb(event_data = event_data, 
                             networks = networks_long_dynamic_1hr,
                             network_type = "undirected",
                             ILV_c = ILV_c,
                             ILV_tv = ILV_tv,
                             ILVi = c("age", "mean_dist_to_carcass_norm", "prop_informed_norm"),
                             ILVs = c("age", "prop_informed_norm")) 
write_rds(data_list_1hr, file="data/data_lists/dynamic_daylight_ilvs1_fixed_1hr.RDS")

data_list_day1_1hr <- import_user_STb(event_data = event_data_day1, 
                                  networks = networks_long_dynamic_day1_1hr,
                                  network_type = "undirected",
                                  ILV_c = ILV_c_day1,
                                  ILV_tv = ILV_tv_day1,
                                  ILVi = c("age", "mean_dist_to_carcass_squared_norm"),
                                  ILVs = c("age")) 
write_rds(data_list_day1_1hr, file="data/data_lists/dynamic_daylight_ilvs1_fixed_day1_1hr.RDS")

model_full_dynamic <- generate_STb_model(data_list, gq = T, est_acqTime = T)
write(model_full_dynamic, file="data/stan_models/dynamic_daylight_ilvs1_fixed.stan")

model_full_dynamic_sq <- generate_STb_model(data_list_sq, gq = T, est_acqTime = T)
write(model_full_dynamic_sq, file="data/stan_models/dynamic_daylight_ilvs1_fixed_sq.stan")

model_full_dynamic_day1 <- generate_STb_model(data_list_day1, gq = T, est_acqTime = T)
write(model_full_dynamic_day1, file="data/stan_models/dynamic_daylight_ilvs1_fixed_day1.stan")

model_full_dynamic_day1_comp <- generate_STb_model(data_list_day1, gq = T, est_acqTime = T, transmission_func="freqdep_f")
write(model_full_dynamic_day1_comp, file="data/stan_models/dynamic_daylight_ilvs1_fixed_day1_comp.stan")

model_full_dynamic_1hr <- generate_STb_model(data_list_1hr, gq = T, est_acqTime = T)
write(model_full_dynamic_1hr, file="data/stan_models/dynamic_daylight_ilvs1_fixed_1hr.stan")

model_full_dynamic_day1_1hr <- generate_STb_model(data_list_day1_1hr, gq = T, est_acqTime = T)
write(model_full_dynamic_day1_1hr, file="data/stan_models/dynamic_daylight_ilvs1_fixed_day1_1hr.stan")
# fit_dynamic <- fit_STb(data_list,
#                        model_full_dynamic,
#                        parallel_chains = 3,
#                        chains = 3,
#                        cores = 3,
#                        iter = 500,
#                        refresh=50)
# STb_save(fit_dynamic, output_dir = "data/cmdstan_saves", name="dynamic_daylight_ilvs1_fixed")
fit_dynamic <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs1_fixed.rds') 

# fit_dynamic_sq <- fit_STb(data_list_sq,
#                        model_full_dynamic_sq,
#                        parallel_chains = 3,
#                        chains = 3,
#                        cores = 3,
#                        iter = 500,
#                        refresh=50)
# STb_save(fit_dynamic_sq, output_dir = "data/cmdstan_saves", name="dynamic_daylight_ilvs1_fixed_sq")
fit_dynamic_sq <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs1_fixed_sq.rds') 

# fit_dynamic_day1 <- fit_STb(data_list_day1,
#                        model_full_dynamic_day1,
#                        parallel_chains = 3,
#                        chains = 3,
#                        cores = 3,
#                        iter = 500,
#                        refresh=50)
# STb_save(fit_dynamic_day1, output_dir = "data/cmdstan_saves", name="dynamic_daylight_ilvs1_fixed_day1")
fit_dynamic_day1 <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs1_fixed_day1.rds') 

# fit_dynamic_day1_comp <- fit_STb(data_list_day1,
#                             model_full_dynamic_day1_comp,
#                             parallel_chains = 3,
#                             chains = 3,
#                             cores = 3,
#                             iter = 500,
#                             refresh=50)
# STb_save(fit_dynamic_day1_comp, output_dir = "data/cmdstan_saves", name="dynamic_daylight_ilvs1_fixed_day1_comp")
fit_dynamic_day1_comp <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs1_fixed_day1_comp.rds') 

fit_dynamic_1hr <- fit_STb(data_list_1hr,
                       model_full_dynamic_1hr,
                       parallel_chains = 3,
                       chains = 3,
                       cores = 3,
                       iter = 500,
                       refresh=50)
STb_save(fit_dynamic_1hr, output_dir = "data/cmdstan_saves", name="dynamic_daylight_ilvs1_fixed_1hr")
fit_dynamic_1hr <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs1_fixed_1hr.rds') 

fit_dynamic_day1_1hr <- fit_STb(data_list_day1_1hr,
                            model_full_dynamic_day1_1hr,
                            parallel_chains = 3,
                            chains = 3,
                            cores = 3,
                            iter = 500,
                            refresh=50)
STb_save(fit_dynamic_day1_1hr, output_dir = "data/cmdstan_saves", name="dynamic_daylight_ilvs1_fixed_day1_1hr")
fit_dynamic_day1_1hr <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs1_fixed_day1_1hr.rds') 

model_asoc = generate_STb_model(data_list, model_type="asocial", gq = T, est_acqTime = T)
# asocial_fit = fit_STb(data_list,
#                       model_asoc,
#                       parallel_chains =3,
#                       chains =3,
#                       cores = 3,
#                       iter = 500,
#                       refresh=50)
# STb_save(asocial_fit, output_dir = "data/cmdstan_saves", name="dynamic_daylight_ilvs_asoc1_fixed")
asocial_fit <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs_asoc1_fixed.rds') 

# model_asoc_sq = generate_STb_model(data_list_sq, model_type="asocial", gq = T, est_acqTime = T)
# asocial_fit_sq = fit_STb(data_list_sq,
#                       model_asoc_sq,
#                       parallel_chains =3,
#                       chains =3,
#                       cores = 3,
#                       iter = 500,
#                       refresh=50)
# STb_save(asocial_fit_sq, output_dir = "data/cmdstan_saves", name="dynamic_daylight_ilvs_asoc1_fixed_sq")
asocial_fit_sq <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs_asoc1_fixed_sq.rds') 

# model_asoc_day1 = generate_STb_model(data_list_day1, model_type="asocial", gq = T, est_acqTime = T)
# asocial_fit_day1 = fit_STb(data_list_day1,
#                       model_asoc_day1,
#                       parallel_chains =3,
#                       chains =3,
#                       cores = 3,
#                       iter = 500,
#                       refresh=50)
# STb_save(asocial_fit_day1, output_dir = "data/cmdstan_saves", name="dynamic_daylight_ilvs_asoc1_fixed_day1")
asocial_fit_day1 <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs_asoc1_fixed_day1.rds') 

loo_output <- STb_compare(fit_dynamic, asocial_fit, method="loo-psis")
loo_output_sq <- STb_compare(fit_dynamic_sq, asocial_fit_sq, method="loo-psis")
loo_output_day1 <- STb_compare(fit_dynamic_day1, asocial_fit_day1, method="loo-psis")
loo_output_day1_comp <- STb_compare(fit_dynamic_day1_comp, asocial_fit_day1, method="loo-psis")

comparison_df <- as.data.frame(loo_output$comparison)
comparison_df_sq <- as.data.frame(loo_output_sq$comparison)
comparison_df_day1 <- as.data.frame(loo_output_day1$comparison)
comparison_df_day1_comp <- as.data.frame(loo_output_day1_comp$comparison)

comparison_df$model <- rownames(comparison_df)
comparison_df_sq$model <- rownames(comparison_df_sq)
comparison_df_day1$model <- rownames(comparison_df_day1)
comparison_df_day1_comp$model <- rownames(comparison_df_day1_comp)

ggplot(comparison_df, aes(x = reorder(model, elpd_diff), y = elpd_diff)) +
  geom_point(size = 3) + #elpd_diff
  geom_errorbar(aes(ymin = elpd_diff - se_diff, 
                    ymax = elpd_diff + se_diff), width = 0.2) + #SE of elpd diff
  coord_flip() +
  labs(x = "Model", y = "ELPD Difference", title = "Carcass 1 ('fixed')") +
  theme_minimal()

ggplot(comparison_df_sq, aes(x = reorder(model, elpd_diff), y = elpd_diff)) +
  geom_point(size = 3) + #elpd_diff
  geom_errorbar(aes(ymin = elpd_diff - se_diff, 
                    ymax = elpd_diff + se_diff), width = 0.2) + #SE of elpd diff
  coord_flip() +
  labs(x = "Model", y = "ELPD Difference", title = "Carcass 1 ('fixed')") +
  theme_minimal() # looks fine

ggplot(comparison_df_day1, aes(x = reorder(model, elpd_diff), y = elpd_diff)) +
  geom_point(size = 3) + #elpd_diff
  geom_errorbar(aes(ymin = elpd_diff - se_diff, 
                    ymax = elpd_diff + se_diff), width = 0.2) + #SE of elpd diff
  coord_flip() +
  labs(x = "Model", y = "ELPD Difference", title = "Carcass 1 ('fixed')") +
  theme_minimal()

ggplot(comparison_df_day1_comp, aes(x = reorder(model, elpd_diff), y = elpd_diff)) +
  geom_point(size = 3) + #elpd_diff
  geom_errorbar(aes(ymin = elpd_diff - se_diff, 
                    ymax = elpd_diff + se_diff), width = 0.2) + #SE of elpd diff
  coord_flip() +
  labs(x = "Model", y = "ELPD Difference", title = "Carcass 1 ('fixed')") +
  theme_minimal()

# PSIS-LOO is an approximation of LOO, and observations with pareto-k diagnostic values >.7 may indicate that the approximation is unreliable. The function above will warn you if that is the case, and you can visually inspect these diagnostics like so:
pareto_df = as.data.frame(loo_output$pareto_diagnostics)
pareto_df_sq = as.data.frame(loo_output_sq$pareto_diagnostics)
pareto_df_day1 = as.data.frame(loo_output_day1$pareto_diagnostics)
pareto_df_day1_comp = as.data.frame(loo_output_day1_comp$pareto_diagnostics)

ggplot(pareto_df, aes(x=observation, y=pareto_k, color=model))+
  geom_point() +
  scale_color_viridis_d(begin=0.2, end=0.7)+
  geom_hline(yintercept = 0.7, linetype="dashed", color="orange")+
  geom_hline(yintercept = 1, linetype="dashed", color="red")+
  labs(x="Observation", y="Pareto-k value", title="Pareto-k diagnostics")+
  theme_minimal() # a few high--not great. Worse than squared.

ggplot(pareto_df_sq, aes(x=observation, y=pareto_k, color=model))+
  geom_point() +
  scale_color_viridis_d(begin=0.2, end=0.7)+
  geom_hline(yintercept = 0.7, linetype="dashed", color="orange")+
  geom_hline(yintercept = 1, linetype="dashed", color="red")+
  labs(x="Observation", y="Pareto-k value", title="Pareto-k diagnostics")+
  theme_minimal() # a bit not-great but not terrible either.

ggplot(pareto_df_day1, aes(x=observation, y=pareto_k, color=model))+
  geom_point() +
  scale_color_viridis_d(begin=0.2, end=0.7)+
  geom_hline(yintercept = 0.7, linetype="dashed", color="orange")+
  geom_hline(yintercept = 1, linetype="dashed", color="red")+
  labs(x="Observation", y="Pareto-k value", title="Pareto-k diagnostics")+
  theme_minimal() # looks fine

ggplot(pareto_df_day1_comp, aes(x=observation, y=pareto_k, color=model))+
  geom_point() +
  scale_color_viridis_d(begin=0.2, end=0.7)+
  geom_hline(yintercept = 0.7, linetype="dashed", color="orange")+
  geom_hline(yintercept = 1, linetype="dashed", color="red")+
  labs(x="Observation", y="Pareto-k value", title="Pareto-k diagnostics")+
  theme_minimal() # looks fine

# SUMMARIES
summ <- STb_summary(fit_dynamic, digits = 3)
summ_sq <- STb_summary(fit_dynamic_sq, digits = 3)
summ_day1 <- STb_summary(fit_dynamic_day1, digits = 3)
summ_day1_comp <- STb_summary(fit_dynamic_day1_comp, digits = 3)

summ %>% filter(grepl("beta_", Parameter)) %>%
  select(Parameter, Median, CI_Lower, CI_Upper) %>%
  mutate(type = str_extract(Parameter, "ILVs|ILVi"),
         type = case_when(type == "ILVi" ~ "Effect on intrinsic rate",
                          type == "ILVs" ~ "Effect on social rate",
                          .default = type)) %>%
  mutate(param = str_remove(Parameter, "beta_"),
         param = str_remove(param, "ILVi_"),
         param = str_remove(param, "ILVs_"),
         param = str_remove(param, "_norm")) %>%
  ggplot(aes(x = param, y = Median))+
  geom_point()+
  geom_segment(aes(x = param, xend = param, y = CI_Lower, yend = CI_Upper))+
  coord_flip()+
  theme_minimal()+
  facet_wrap(~type, ncol = 1, scale = "free_y")+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  labs(subtitle = "(95% CIs)",
       caption = "Carcass 1 ('fixed')",
       title = "Individual-level variables",
       x = "Parameter") # so, mean dist has a huge impact in this model, but when we square it, its impact goes away almost entirely. Why?

summ_sq %>% filter(grepl("beta_", Parameter)) %>%
  select(Parameter, Median, CI_Lower, CI_Upper) %>%
  mutate(type = str_extract(Parameter, "ILVs|ILVi"),
         type = case_when(type == "ILVi" ~ "Effect on intrinsic rate",
                          type == "ILVs" ~ "Effect on social rate",
                          .default = type)) %>%
  mutate(param = str_remove(Parameter, "beta_"),
         param = str_remove(param, "ILVi_"),
         param = str_remove(param, "ILVs_"),
         param = str_remove(param, "_norm")) %>%
  ggplot(aes(x = param, y = Median))+
  geom_point()+
  geom_segment(aes(x = param, xend = param, y = CI_Lower, yend = CI_Upper))+
  coord_flip()+
  theme_minimal()+
  facet_wrap(~type, ncol = 1, scale = "free_y")+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  labs(subtitle = "(95% CIs)",
       caption = "Carcass 1 ('fixed')",
       title = "Individual-level variables",
       x = "Parameter") # interesting that once we square the distance parameter, it removes the effect entirely

summ_day1 %>% filter(grepl("beta_", Parameter)) %>%
  select(Parameter, Median, CI_Lower, CI_Upper) %>%
  mutate(type = str_extract(Parameter, "ILVs|ILVi"),
         type = case_when(type == "ILVi" ~ "Effect on intrinsic rate",
                          type == "ILVs" ~ "Effect on social rate",
                          .default = type)) %>%
  mutate(param = str_remove(Parameter, "beta_"),
         param = str_remove(param, "ILVi_"),
         param = str_remove(param, "ILVs_"),
         param = str_remove(param, "_norm")) %>%
  ggplot(aes(x = param, y = Median))+
  geom_point()+
  geom_segment(aes(x = param, xend = param, y = CI_Lower, yend = CI_Upper))+
  coord_flip()+
  theme_minimal()+
  facet_wrap(~type, ncol = 1, scale = "free_y")+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  labs(subtitle = "(95% CIs)",
       caption = "Carcass 1 ('fixed')",
       title = "Individual-level variables",
       x = "Parameter")

summ_day1_comp %>% filter(grepl("beta_", Parameter)) %>%
  select(Parameter, Median, CI_Lower, CI_Upper) %>%
  mutate(type = str_extract(Parameter, "ILVs|ILVi"),
         type = case_when(type == "ILVi" ~ "Effect on intrinsic rate",
                          type == "ILVs" ~ "Effect on social rate",
                          .default = type)) %>%
  mutate(param = str_remove(Parameter, "beta_"),
         param = str_remove(param, "ILVi_"),
         param = str_remove(param, "ILVs_"),
         param = str_remove(param, "_norm")) %>%
  ggplot(aes(x = param, y = Median))+
  geom_point()+
  geom_segment(aes(x = param, xend = param, y = CI_Lower, yend = CI_Upper))+
  coord_flip()+
  theme_minimal()+
  facet_wrap(~type, ncol = 1, scale = "free_y")+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  labs(subtitle = "(95% CIs)",
       caption = "Carcass 1 ('fixed')",
       title = "Individual-level variables",
       x = "Parameter")

plot_data_obs <- get_plot_data(event_data)
plot_data_obs_day1 <- get_plot_data(event_data_day1)
plot_data_ppc <- get_plot_data_ppc(fit = fit_dynamic, data_list = data_list)
plot_data_ppc_sq <- get_plot_data_ppc(fit = fit_dynamic_sq, data_list = data_list_sq)
plot_data_ppc_day1 <- get_plot_data_ppc(fit = fit_dynamic_day1, data_list = data_list_day1)
plot_data_ppc_day1_comp <- get_plot_data_ppc(fit = fit_dynamic_day1_comp, data_list = data_list_day1)
plot_data_ppc_day1_1hr <- get_plot_data_ppc(fit = fit_dynamic_day1_1hr, data_list = data_list_day1_1hr)

# plot it
ggplot() +
  geom_line(data = plot_data_ppc, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial",
       title = "Carcass 1 ('fixed')") +
  theme_minimal()

ggplot() +
  geom_line(data = plot_data_ppc_sq, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial",
       title = "Carc 1, dist sq, roostmates ILV fixed") +
  theme_minimal()


ggplot() +
  geom_line(data = plot_data_ppc_day1, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs_day1, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial",
       title = "Carc 1, Day 1, dist sq, no roostmates ILV") +
  theme_minimal()

ggplot() +
  geom_line(data = plot_data_ppc_day1_comp, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs_day1, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial",
       title = "Carc 1, Day 1, dist sq, no roostmates ILV,\ncomplex transmission") +
  theme_minimal()

ggplot() +
  geom_line(data = plot_data_ppc_day1_1hr, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs_day1, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial",
       title = "Day 1, 1hr") +
  theme_minimal()
