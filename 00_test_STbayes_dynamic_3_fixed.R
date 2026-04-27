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
nbda_data <- data_cumul_wt_3[[6]] # carcass 4420641, 

tar_load(stn_carcs)
carc <- stn_carcs[[26]]
event_time <- carc$datetime_il
event_date <- lubridate::date(event_time)

# Saving gps data as temp file because it takes too long otherwise.
# tar_load(stn_gps_30days)
# gps <- stn_gps_30days[[26]] # the gps data that we will use for this carcass
# write_rds(gps, "data/created/gps_for_STbayes_3.RDS")
gps <- readRDS("data/created/gps_for_STbayes_3.RDS")

suntimes <- suncalc::getSunlightTimes(date = sort(unique(gps$date_il)), lat = 31.434306, lon = 34.991889, keep = c("sunrise", "sunset"), tz = "Israel") %>% select("date_il" = date, sunrise, sunset)

gps <- left_join(gps, suntimes)
test_daylight <- gps %>% filter(timestamp_il >= sunrise & timestamp_il <= sunset)
# table(gps$daylight)
# table(test_daylight$daylight) # sweet, we only have daylight, and it's the same number as before, which means this worked.
gps <- test_daylight

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

gps_diffusion <- gps %>% filter(time_since_carcass >= 0)
first_sightings <- get_first_sightings(gps_diffusion, hours_after_carcass, gps_spd, ddf, dds, seeds)

# Get right-censored individuals
rightcens <- all_indivs_sorted[!(all_indivs_sorted %in% c(first_sightings$individual_local_identifier, seeds))]

gps_fornetwork <- gps_diffusion %>%
  filter(time_since_carcass >= 0 & time_since_carcass <= as.numeric(hours_after_carcass)) %>%
  mutate(time = as.numeric(daytime_since_carcass)*60*60) %>% # this will now correspond to the numeric times in test_event_data.
  filter(time >= 0)

write_rds(gps_fornetwork, file = "data/created/gps_3.RDS")
write_rds(first_sightings, file = "data/created/first_sightings_3.RDS")

# Now we have the event data; time to format it the way that STbayes needs.
event_data <- format_event_data_new(first_sightings, seeds, all_indivs_sorted, time_col = "daytime_since_carcass", carc = carc, gps_fornetwork = gps_fornetwork)

gps_fornetwork_norightcens <- gps_fornetwork %>% filter(!(individual_local_identifier %in% rightcens))
all_indivs_sorted_norightcens <- all_indivs_sorted[!(all_indivs_sorted %in% rightcens)]
event_data_nocensored <- format_event_data_new(first_sightings, seeds, all_indivs_sorted_norightcens, time_col = "daytime_since_carcass", carc = carc, gps_fornetwork = gps_fornetwork_norightcens)

cutpoints <- unique(event_data$time)

# For right-censored individuals, need fewer cutpoints. Need to end up with 37 networks for 37 individuals. So I need 38 cutpoints.
cutpoints_norightcens <- unique(event_data_nocensored$time)
if(!(0 %in% cutpoints)){
  cutpoints <- c(0, cutpoints)}
if(!(0 %in% cutpoints_norightcens)){
  cutpoints_norightcens <- c(0, cutpoints_norightcens)}

cutpoints[length(cutpoints)] <- event_data$t_end[1]
length(cutpoints) # need 39 cutpoints so we can have 38 bins so we can define 37 events plus censored indivs.
length(cutpoints_norightcens) # need 38 cutpoints so we can have 38 bins so we can define 37 events plus censored indivs.
bins <- sort(unique(cut(gps_fornetwork$time, breaks = cutpoints)))
bins_norightcens <- sort(unique(cut(gps_fornetwork_norightcens$time, breaks = cutpoints_norightcens)))

gps_fornetwork$network <- cut(gps_fornetwork$time, breaks = cutpoints)
gps_fornetwork_norightcens$network <- cut(gps_fornetwork_norightcens$time, breaks = cutpoints_norightcens)
lvls <- levels(gps_fornetwork$network)
lvls_norightcens <- levels(gps_fornetwork_norightcens$network)
gps_fornetwork <- gps_fornetwork %>%
  filter(!is.na(network)) # remove NAs (after the diffusion period)
gps_fornetwork_norightcens <- gps_fornetwork_norightcens %>%
  filter(!is.na(network)) # remove NAs (after the diffusion period)
missing_intervals <- levels(gps_fornetwork$network)[!(levels(gps_fornetwork$network) %in% gps_fornetwork$network)]
missing_intervals_norightcens <- levels(gps_fornetwork_norightcens$network)[!(levels(gps_fornetwork_norightcens$network) %in% gps_fornetwork_norightcens$network)]

# need to add a date for the missing intervals so the later code will work
missing_intervals_lower <- as.numeric(str_extract(missing_intervals, "(?<=\\()[0-9]+"))
missing_intervals_upper <- as.numeric(str_extract(missing_intervals, "(?<=\\,)[0-9]+(?=\\])"))

dates_before <- as.Date(unlist(purrr::map(missing_intervals_lower, ~{gps_fornetwork %>% filter(time < .x) %>% arrange(timestamp_il) %>% pull(date_il) %>% max()})))
dates_after <- as.Date(unlist(purrr::map(missing_intervals_upper, ~{gps_fornetwork %>% filter(time > .x) %>% arrange(timestamp_il) %>% pull(date_il) %>% min()})))

missing_intervals_lower_norightcens <- as.numeric(str_extract(missing_intervals_norightcens, "(?<=\\()[0-9]+"))
missing_intervals_upper_norightcens <- as.numeric(str_extract(missing_intervals_norightcens, "(?<=\\,)[0-9]+(?=\\])"))

dates_before_norightcens <- as.Date(unlist(purrr::map(missing_intervals_lower_norightcens, ~{gps_fornetwork_norightcens %>% filter(time < .x) %>% arrange(timestamp_il) %>% pull(date_il) %>% max()})))
dates_after_norightcens <- as.Date(unlist(purrr::map(missing_intervals_upper_norightcens, ~{gps_fornetwork_norightcens %>% filter(time > .x) %>% arrange(timestamp_il) %>% pull(date_il) %>% min()})))

# I think for now I'm just going to take the date before
to_add <- data.frame(network = missing_intervals, date_il = dates_before) # this is super buggy and i need to return to it!
to_add_norightcens <- data.frame(network = missing_intervals_norightcens, date_il = dates_before_norightcens) # this is super buggy and i need to return to it!

if(nrow(to_add) > 0){
  gps_fornetwork <- bind_rows(gps_fornetwork, to_add)}
if(nrow(to_add_norightcens) > 0){
  gps_fornetwork_norightcens <- bind_rows(gps_fornetwork_norightcens, to_add_norightcens)}

gps_fornetwork <- gps_fornetwork %>%
  mutate(network = factor(network, levels = lvls)) %>%
  arrange(time_since_carcass)
gps_fornetwork_norightcens <- gps_fornetwork_norightcens %>%
  mutate(network = factor(network, levels = lvls_norightcens)) %>%
  arrange(time_since_carcass)

gps_list <- gps_fornetwork %>% arrange(network) %>% group_split(network, .keep = TRUE)
gps_list_norightcens <- gps_fornetwork_norightcens %>% arrange(network) %>% group_split(network, .keep = TRUE)
length(gps_list) == length(lvls) # yay!
length(gps_list_norightcens) == length(lvls_norightcens)

tar_load(rp)
gps_list <- map(gps_list, ~{
  if(nrow(.x) == 1 & all(is.na(.x$individual_local_identifier))){
    return(.x[0,])
  }else{
    return(.x)
  }
})

gps_list_norightcens <- map(gps_list_norightcens, ~{
  if(nrow(.x) == 1 & all(is.na(.x$individual_local_identifier))){
    return(.x[0,])
  }else{
    return(.x)
  }
})

dynamic_networks <- map(gps_list, ~get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd))
dynamic_networks_norightcens <- map(gps_list_norightcens, ~get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd))
dynamic_networks_fixed <- fix_nets(nets = dynamic_networks, indivs = all_indivs_sorted)
dynamic_networks_fixed_norightcens <- fix_nets(nets = dynamic_networks_norightcens, indivs = all_indivs_sorted_norightcens)
map(dynamic_networks_fixed, dim)
map(dynamic_networks_fixed_norightcens, dim)

networks_long <- map(dynamic_networks_fixed, ~{
  out <- .x %>% rownames_to_column(var = "focal") %>%
    pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri") %>%
    mutate(trial = carc_id)
  return(out)
})
networks_long_norightcens <- map(dynamic_networks_fixed_norightcens, ~{
  out <- .x %>% rownames_to_column(var = "focal") %>%
    pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri") %>%
    mutate(trial = carc_id)
  return(out)
})

networks_long_dynamic <- purrr::list_rbind(networks_long, names_to = "time") # this creates a numeric column for "time", which is how stbayes wants it--sequential integer values, not group names.
networks_long_dynamic_norightcens <- purrr::list_rbind(networks_long_norightcens, names_to = "time")
write_rds(networks_long_dynamic, file = "data/created/networks_long_dynamic_3_fixed.RDS")
write_rds(networks_long_dynamic_norightcens, file = "data/created/networks_long_dynamic_3_fixed_norightcens.RDS")

networks_long_dynamic <- readRDS("data/created/networks_long_dynamic_3_fixed.RDS")
networks_long_dynamic_norightcens <- readRDS("data/created/networks_long_dynamic_3_fixed_norightcens.RDS")

# Network must contain all individuals
# "The networks dataframe is used as the reference for all unique IDs, thus each ID must be included at least once in either the focal or other column. If a dyad is absent, their connection is assumed to be zero."
all(sort(unique(c(networks_long_dynamic$focal, networks_long_dynamic$other))) == all_indivs_sorted) #TRUE
all(sort(unique(c(networks_long_dynamic_norightcens$focal, networks_long_dynamic_norightcens$other))) == all_indivs_sorted_norightcens) #TRUE

# ILVs --------------------------------------------------------------------
## AGE
# This carcass is from 2023, and age is not time-varying within this diffusion
age_ilv <- gps %>%
  st_drop_geometry() %>%
  select(individual_local_identifier, age_2023) %>%
  distinct() %>%
  mutate(age_2023_norm = scale(age_2023, center = TRUE, scale = TRUE)[,1]) # both scaling and centering. I'm not 100% sure this is right

age_ilv_norightcens <- gps %>%
  filter(!(individual_local_identifier %in% rightcens)) %>%
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

dists_dyn_norightcens <- map(gps_list_norightcens, ~{
  step1 <- .x %>% 
    st_drop_geometry() %>%
    arrange(individual_local_identifier, time_since_carcass) %>%
    group_by(individual_local_identifier) %>%
    summarize(mean_dist_to_carcass = mean(dist_to_carcass),
              mean_dist_to_carcass_sqared = mean_dist_to_carcass^2)
  missing <- all_indivs_sorted_norightcens[!(all_indivs_sorted_norightcens %in% step1$individual_local_identifier)]
  if(length(missing) > 0){
    missing_df <- data.frame(individual_local_identifier = missing, mean_dist_to_carcass = NA, mean_dist_to_carcass_squared = NA) # keeping the values missing so we can scale, but adding the indivs
    step2 <- bind_rows(step1, missing_df) %>%
      filter(!is.na(individual_local_identifier))
  }else{
    step2 <- step1 %>% filter(!is.na(individual_local_identifier))
  }
  return(step2)
}) %>% purrr::list_rbind(names_to = "time") %>%
  mutate(mean_dist_to_carcass_norm = scale(log(mean_dist_to_carcass), center = T, scale = T)[,1],
         mean_dist_to_carcass_norm = replace_na(mean_dist_to_carcass_norm, 0),
         mean_dist_to_carcass_squared_norm = scale(log(mean_dist_to_carcass_squared), center = T, scale = T)[,1],
         mean_dist_to_carcass_squared_norm = replace_na(mean_dist_to_carcass_squared_norm, 0))

#Time-varying ILVs
#Below we add distance from resource as a time-varying ILV. Similar to dynamic networks, these need to be summarized per inter-event interval. For example, the value of dist_from_resource at time=1 should reflect the average distance of the individual to the resource from the start of the observation period to the first event.

## PROPORTION INFORMED ROOSTMATES
prop_informed <- readRDS("data/created/prop_informed3.RDS")
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

informed_norightcens <- informed %>%
  filter(!(id %in% rightcens)) # this does include the censored individuals in calculating the roost information ILV, but I think that's appropriate.

# don't need to create the equivalent of `informed` for the first day only, since by definition nobody roosted with anyone informed the day before the carcass was placed.

## Combine all the ILVs
# This is for the constant ILVs (age). We will also need time-varying ILVs separately
ILV_c <- age_ilv %>%
  rename("id" = individual_local_identifier,
         age = age_2023_norm) %>%
  select(id, age) %>%
  mutate(age = replace_na(age, 0)) # set unknown ages to the mean

ILV_c_norightcens <- age_ilv_norightcens %>%
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

ILV_tv_norightcens <- dists_dyn_norightcens %>%
  select("id" = individual_local_identifier,
         time, mean_dist_to_carcass_norm, mean_dist_to_carcass_squared_norm) %>%
  left_join(informed_norightcens, by = c("id", "time")) %>%
  mutate(trial = carc_id) %>%
  select(trial, id, time, mean_dist_to_carcass_norm, mean_dist_to_carcass_squared_norm, prop_informed_norm) %>%
  mutate(across(c("mean_dist_to_carcass_norm", "mean_dist_to_carcass_squared_norm", "prop_informed_norm"), ~replace_na(.x, 0)))

# Data lists --------------------------------------------------------------
#We need to explicitly tell STbayes which variables are additive (acting independently on intrinsic or social rates) and multiplicative (same effect estimated for intrinsic and social rates). Below, I have specified age as acting independently on the intrinsic and social rate, sex as acting only on the social rate, and weight as a multiplicative effect. Two betas will be estimated for age, and a single beta will be estimated for sex and weight.

data_list <- import_user_STb(event_data = event_data, 
                             networks = networks_long_dynamic,
                             network_type = "undirected",
                             ILV_c = ILV_c,
                             ILV_tv = ILV_tv,
                             ILVi = c("age", "mean_dist_to_carcass_norm", "prop_informed_norm"),
                             ILVs = c("age", "prop_informed_norm")) 
write_rds(data_list, file="data/data_lists/datalist_3_fixed.RDS")

data_list_norightcens <- import_user_STb(event_data = event_data_nocensored, 
                                         networks = networks_long_dynamic_norightcens,
                                         network_type = "undirected",
                                         ILV_c = ILV_c_norightcens,
                                         ILV_tv = ILV_tv_norightcens,
                                         ILVi = c("age", "mean_dist_to_carcass_norm", "prop_informed_norm"),
                                         ILVs = c("age", "prop_informed_norm")) 
write_rds(data_list_norightcens, file="data/data_lists/datalist_3_fixed_norightcens.RDS")

# Models
model_full_dynamic <- generate_STb_model(data_list, gq = T, est_acqTime = T)
write(model_full_dynamic, file="data/stan_models/model_3_fixed.stan")

model_full_dynamic_norightcens <- generate_STb_model(data_list_norightcens, gq = T, est_acqTime = T)
write(model_full_dynamic_norightcens, file="data/stan_models/model_3_fixed_norightcens.stan")

# fit_dynamic <- fit_STb(data_list,
#                        model_full_dynamic,
#                        parallel_chains = 3,
#                        chains = 3,
#                        cores = 3,
#                        iter = 500,
#                        refresh=50)
# STb_save(fit_dynamic, output_dir = "data/cmdstan_saves", name="dynamic_daylight_ilvs3_fixed")
fit_dynamic <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs3_fixed.rds') 
# 
# fit_dynamic_norightcens <- fit_STb(data_list_norightcens,
#                                    model_full_dynamic_norightcens,
#                                    parallel_chains = 3,
#                                    chains = 3,
#                                    cores = 3,
#                                    iter = 500,
#                                    refresh=50)
# STb_save(fit_dynamic_norightcens, output_dir = "data/cmdstan_saves", name="dynamic_daylight_ilvs3_fixed_norightcens")
fit_dynamic_norightcens <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs3_fixed_norightcens.rds')

model_asoc = generate_STb_model(data_list, model_type="asocial", gq = T, est_acqTime = T)
# asocial_fit = fit_STb(data_list,
#                       model_asoc,
#                       parallel_chains =3,
#                       chains =3,
#                       cores = 3,
#                       iter = 500,
#                       refresh=50)
# STb_save(asocial_fit, output_dir = "data/cmdstan_saves", name="dynamic_daylight_ilvs_asoc3_fixed")
asocial_fit <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs_asoc3_fixed.rds') 

loo_output <- STb_compare(fit_dynamic, asocial_fit, method="loo-psis")

comparison_df <- as.data.frame(loo_output$comparison)

comparison_df$model <- rownames(comparison_df)

ggplot(comparison_df, aes(x = reorder(model, elpd_diff), y = elpd_diff)) +
  geom_point(size = 3) + #elpd_diff
  geom_errorbar(aes(ymin = elpd_diff - se_diff, 
                    ymax = elpd_diff + se_diff), width = 0.2) + #SE of elpd diff
  coord_flip() +
  labs(x = "Model", y = "ELPD Difference", title = "Carcass 3 (4420641)") +
  theme_minimal()+
  theme(text = element_text(size = 18))

ggplot(comparison_df, aes(x = 1, y = elpd_diff, color = model)) +
  geom_point(size = 4) + #elpd_diff
  geom_errorbar(aes(ymin = elpd_diff - se_diff, 
                    ymax = elpd_diff + se_diff), width = 0.1, linewidth = 2) + #SE of elpd diff
  labs(x = "Model", y = "ELPD Difference", title = "Carcass 3 (4420641)") +
  theme_minimal()+
  theme(text = element_text(size = 18))+
  coord_flip()

# PSIS-LOO is an approximation of LOO, and observations with pareto-k diagnostic values >.7 may indicate that the approximation is unreliable. The function above will warn you if that is the case, and you can visually inspect these diagnostics like so:
pareto_df = as.data.frame(loo_output$pareto_diagnostics)

ggplot(pareto_df, aes(x=observation, y=pareto_k, color=model))+
  geom_point() +
  scale_color_viridis_d(begin=0.2, end=0.7)+
  geom_hline(yintercept = 0.7, linetype="dashed", color="orange")+
  geom_hline(yintercept = 1, linetype="dashed", color="red")+
  labs(x="Observation", y="Pareto-k value", title="Pareto-k diagnostics")+
  theme_minimal() # a few high--not great. Worse than squared.

# SUMMARIES
summ <- STb_summary(fit_dynamic, digits = 3)
summ_norightcens <- STb_summary(fit_dynamic_norightcens, digits = 3)

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
       caption = "Carcass 3 ('fixed')",
       x = "Parameter")+
  theme(text = element_text(size = 18))# so, mean dist has a huge impact in this model, but when we square it, its impact goes away almost entirely. Why?

plot_data_obs <- get_plot_data(event_data)
plot_data_obs_norightcens <- get_plot_data(event_data_nocensored)
plot_data_ppc <- get_plot_data_ppc(fit = fit_dynamic, data_list = data_list)
plot_data_ppc_norightcens <- get_plot_data_ppc(fit = fit_dynamic_norightcens, data_list = data_list_norightcens)

# plot it
ggplot() +
  geom_line(data = plot_data_ppc, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial",
       title = "Original") +
  theme_minimal()

ggplot() +
  geom_line(data = plot_data_ppc_norightcens, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs_norightcens, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial",
       title = "No right-censored indivs") +
  theme_minimal()
