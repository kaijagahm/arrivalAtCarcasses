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
nbda_data <- data_cumul_wt_3[[7]] # carcass 4422323 , which is the 7th element of the 3rd list of 10, so element 27 of stn_carcs

tar_load(stn_carcs)
carc <- stn_carcs[[27]]
event_time <- carc$datetime_il

# Saving gps data as temp file because it takes too long otherwise.
# tar_load(stn_gps_30days)
# gps <- stn_gps_30days[[27]] # the gps data that we will use for this carcass
# write_rds(gps, "data/created/gps_for_STbayes_2.RDS")
gps <- readRDS("data/created/gps_for_STbayes_2.RDS")

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

# gps %>%
#   st_drop_geometry() %>%
#   mutate(across(contains("_since_carcass"), as.numeric)) %>%
#   pivot_longer(cols = c("time_since_carcass", "daytime_since_carcass"), names_to = "type", values_to = "hours") %>%
#   select(timestamp_il, type, hours) %>%
#   mutate(type = str_remove(type, "_since_carcass")) %>%
#   mutate(type = case_when(type == "time" ~ "real time", .default = type)) %>%
#   ggplot(aes(x = timestamp_il, y = hours, color = type))+
#   geom_point()+
#   theme_minimal()+
#   scale_color_manual(name = "Type", values = c("dodgerblue2", "black"))+
#   labs(y = "Hours since carcass", x = "Time", title = "Two ways to measure time since carcass")+
#   NULL

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
first_sightings <- get_first_sightings(gps_diffusion, sighting_time_max_hours, gps_spd, ddf, dds, seeds)

# Now we have the event data; time to format it the way that STbayes needs.
event_data <- format_event_data(first_sightings, seeds, all_indivs_sorted, time_col = "daytime_since_carcass", carc = carc)
tar_load(hours_after_carcass)

gps_fornetwork <- gps_diffusion %>%
  filter(time_since_carcass >= 0 & time_since_carcass <= as.numeric(hours_after_carcass)) %>%
  mutate(time = as.numeric(daytime_since_carcass)*60*60) %>% # this will now correspond to the numeric times in test_event_data.
  filter(time >= 0)

# "if the user’s observation period included 10 events and the dataset does contain censored individuals, they should supply edge weights from 11 networks in total, where time=1 should contain the network representing the period from [t0,te1), time=2 represents [te1,te2), and time=11 represents from [te10,tend]. NB: If there are censored individuals, the end of the observation period should necessarily be larger than the time of the final event (event_data$t_end > max(event_data$time)."
# So, we have 62 events, which means we should be supplying 63 networks
# Note: it does NOT say what to do if there are seed individuals... I assume I don't provide a network for those, since they're set to 0, so the first one will just be from 0 through te1? (e1 = event 1)
# Also, what they're saying about the end of the observation period seems to contradict how they said to encode the censored individuals (i.e. set them to tend+1)

cutpoints <- unique(event_data$time)
cutpoints <- c(0, cutpoints)
length(cutpoints) # need 34 cutpoints so we can have 33 bins so we can define 32 events plus censored indivs.
bins <- sort(unique(cut(gps_fornetwork$time, breaks = cutpoints))) # these look right!
length(bins) # 32. # XXX GO BACK TO FIRST diffusion AND CHECK THIS TOO!!!
gps_fornetwork$network <- cut(gps_fornetwork$time, breaks = cutpoints)
lvls <- levels(gps_fornetwork$network)
length(lvls) # good, there are 33 bins (corresponding to 32 events plus censored individuals)
gps_fornetwork <- gps_fornetwork %>%
  filter(!is.na(network)) # remove NAs (after the diffusion period)
missing_intervals <- levels(gps_fornetwork$network)[!(levels(gps_fornetwork$network) %in% gps_fornetwork$network)]
to_add <- data.frame(network = missing_intervals, date_il = ) # XXX NEED TO FIGURE OUT A WAY TO ADD A DATE SO THAT WHEN WE LATER SPLIT BY THIS WE DON'T LOSE IT. Yikes this is so buggy. #2026-03-02
if(nrow(to_add) > 0){
  gps_fornetwork <- bind_rows(gps_fornetwork, to_add)
}
gps_fornetwork <- gps_fornetwork %>%
  mutate(network = factor(network, levels = lvls)) %>%
  arrange(time_since_carcass)

gps_list <- gps_fornetwork %>% arrange(network) %>% group_split(network, .keep = TRUE)
length(gps_list) == length(lvls) # yay!
# now we need to split cumulative by day
# test <- event_data %>% left_join(select(first_sightings, individual_local_identifier, date_il), by = c("id" = "individual_local_identifier"))
# test$network <- cut(test$time, breaks = cutpoints)
# gps_fornetwork <- left_join(gps_fornetwork, select(st_drop_geometry(test), date_il, network), by = "network")

# XXX CAN'T FIGURE OUT HOW TO DO THE CUMULATIVE ONE CORRECTLY--COMMENTING OUT FOR NOW (2026-02-27)
# gps_list_cumulative <- gps_fornetwork %>%
#   arrange(date_il, network) %>%
#   group_split(date_il) %>%
#   map(function(day_df) {
#     
#     # get unique group_idx in the order they appear within the day
#     groups <- unique(day_df$network)
#     
#     map(seq_along(groups), function(i) {
#       day_df %>%
#         filter(network %in% groups[1:i])
#     })
#     
#   }) %>%
#   flatten()

tar_load(rp)
gps_list <- map(gps_list, ~{
  if(nrow(.x) == 1 & all(is.na(.x$individual_local_identifier))){
    return(.x[0,])
  }else{
    return(.x)
  }
})

dynamic_networks <- map(gps_list, ~get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd))
dynamic_networks_fixed <- fix_nets(nets = dynamic_networks, indivs = all_indivs_sorted)
# 
# dynamic_networks_cumul <- map(gps_list_cumulative, ~get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd))
# dynamic_networks_cumul_fixed <- fix_nets(nets = dynamic_networks_cumul, indivs = all_indivs_sorted)
#
networks_long <- map(dynamic_networks_fixed, ~{
  out <- .x %>% rownames_to_column(var = "focal") %>%
    pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri") %>%
    mutate(trial = carc_id)
  return(out)
})
networks_long_dynamic <- purrr::list_rbind(networks_long, names_to = "time") # this creates a numeric column for "time", which is how stbayes wants it--sequential integer values, not group names.
write_rds(networks_long_dynamic, file = "data/created/networks_long_dynamic_2.RDS")
networks_long_dynamic <- readRDS("data/created/networks_long_dynamic_2.RDS")

# networks_long_cumul <- map(dynamic_networks_cumul_fixed, ~{
#   out <- .x %>% rownames_to_column(var = "focal") %>%
#     pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri") %>%
#     mutate(trial = carc_id)
#   return(out)
# })
# networks_long_dynamic_cumul <- purrr::list_rbind(networks_long_cumul, names_to = "time") # this creates a numeric column for "time", which is how stbayes wants it--sequential integer values, not group names.
# write_rds(networks_long_dynamic_cumul, file = "data/created/networks_long_dynamic_cumul.RDS")
#networks_long_dynamic_cumul <- readRDS("data/created/networks_long_dynamic_cumul.RDS")

# Network must contain all individuals
# "The networks dataframe is used as the reference for all unique IDs, thus each ID must be included at least once in either the focal or other column. If a dyad is absent, their connection is assumed to be zero."
all(sort(unique(c(networks_long_dynamic$focal, networks_long_dynamic$other))) == all_indivs_sorted) #TRUE
# all(sort(unique(c(networks_long_dynamic_cumul$focal, networks_long_dynamic_cumul$other))) == all_indivs_sorted) #TRUE

# ILVs --------------------------------------------------------------------
## AGE
# This carcass is from 2023, and age is not time-varying within this diffusion
age_ilv <- gps %>%
  st_drop_geometry() %>%
  select(individual_local_identifier, age_2023) %>%
  distinct() %>%
  mutate(age_2023_norm = scale(age_2023, center = TRUE, scale = TRUE)[,1]) # both scaling and centering. I'm not 100% sure this is right

## DISTANCE FROM CARCASS
# "For example, the value of dist_from_resource at time=1 should reflect the average distance of the individual to the resource from the start of the observation period to the first event."
# In the sample, they assume that the data are normally distributed. Mine aren't.
dists_dyn <- map(gps_list, ~{
  step1 <- .x %>% 
    st_drop_geometry() %>%
    arrange(individual_local_identifier, time_since_carcass) %>%
    group_by(individual_local_identifier) %>%
    summarize(mean_dist_to_carcass = mean(dist_to_carcass))
  missing <- all_indivs_sorted[!(all_indivs_sorted %in% step1$individual_local_identifier)]
  missing_df <- data.frame(individual_local_identifier = missing, mean_dist_to_carcass = NA) # keeping the values missing so we can scale, but adding the indivs
  step2 <- bind_rows(step1, missing_df) %>%
    filter(!is.na(individual_local_identifier))
  return(step2)
}) %>% purrr::list_rbind(names_to = "time") %>%
  mutate(mean_dist_to_carcass_norm = scale(log(mean_dist_to_carcass), center = T, scale = T)[,1],
         mean_dist_to_carcass_norm = replace_na(mean_dist_to_carcass_norm, 0)) # distance is log-transformed (to make it more normal) AND scaled/centered. Yuck!

# dists_dyn_cumul <- map(gps_list_cumulative, ~{
#   .x %>% 
#     st_drop_geometry() %>%
#     arrange(individual_local_identifier, time_since_carcass) %>%
#     group_by(individual_local_identifier) %>%
#     summarize(mean_dist_to_carcass = mean(dist_to_carcass))
# }) %>% purrr::list_rbind(names_to = "time") %>%
#   mutate(mean_dist_to_carcass_norm = scale(mean_dist_to_carcass, center = TRUE, scale = TRUE)[,1])

#Time-varying ILVs
#Below we add distance from resource as a time-varying ILV. Similar to dynamic networks, these need to be summarized per inter-event interval. For example, the value of dist_from_resource at time=1 should reflect the average distance of the individual to the resource from the start of the observation period to the first event.

## PROPORTION INFORMED ROOSTMATES
prop_informed2 <- readRDS("data/created/prop_informed2.RDS")
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
  prop_informed2 %>% filter(roost_date == .x-lubridate::days(1))
})
informed <- purrr::list_rbind(informed_list, names_to = "time") %>%
  select(time, "id" = ID1, n_roostmates, prop_informed) %>%
  mutate(n_roostmates = replace_na(n_roostmates, 0),
         prop_informed = replace_na(prop_informed, 0)) %>%
  mutate(prop_informed_norm = scale(prop_informed, scale = T, center = T)[,1])

# informed_list_cumul <- map(dates_list_cumul, ~{
#   prop_informed %>% filter(roost_date == .x-lubridate::days(1))})
# informed_cumul <- purrr::list_rbind(informed_list_cumul, names_to = "time") %>%
#   select(time, "id" = ID1, n_roostmates, prop_informed)

## Combine all the ILVs
# This is for the constant ILVs (age). We will also need time-varying ILVs separately
ILV_c <- age_ilv %>%
  rename("id" = individual_local_identifier,
         age = age_2023_norm) %>%
  select(id, age) %>%
  mutate(age = replace_na(age, 0)) # set unknown ages to the mean

ILV_tv <- dists_dyn %>%
  select("id" = individual_local_identifier,
         time, mean_dist_to_carcass_norm) %>%
  left_join(informed, by = c("id", "time")) %>%
  mutate(trial = carc_id) %>%
  select(trial, id, time, mean_dist_to_carcass_norm, prop_informed_norm) %>%
  mutate(across(c("mean_dist_to_carcass_norm", "prop_informed_norm"), ~replace_na(.x, 0)))

ILV_tv %>% ggplot(aes(x = prop_informed_norm))+geom_histogram()+facet_wrap(~factor(time)) # the distributions of prop_informed_norm are really weird; I wonder if instead I should have this be categorical (most, some, few) or something...

# ILV_tv_cumul <- dists_dyn_cumul %>%
#   select("id" = individual_local_identifier,
#          time, mean_dist_to_carcass_norm) %>%
#   left_join(informed_cumul, by = c("id", "time")) %>%
#   mutate(trial = carc_id,
#          across(c("n_roostmates", "prop_informed"), ~replace_na(.x, 0)))

# Data lists --------------------------------------------------------------
#We need to explicitly tell STbayes which variables are additive (acting independently on intrinsic or social rates) and multiplicative (same effect estimated for intrinsic and social rates). Below, I have specified age as acting independently on the intrinsic and social rate, sex as acting only on the social rate, and weight as a multiplicative effect. Two betas will be estimated for age, and a single beta will be estimated for sex and weight.

data_list <- import_user_STb(event_data = event_data, 
                             networks = networks_long_dynamic,
                             network_type = "undirected",
                             ILV_c = ILV_c,
                             ILV_tv = ILV_tv,
                             ILVi = c("age", "mean_dist_to_carcass_norm", "prop_informed_norm"),
                             ILVs = c("age", "prop_informed_norm")) 

# data_list_cumul <- import_user_STb(event_data = event_data, 
#                                    networks = networks_long_dynamic_cumul,
#                                    network_type = "undirected",
#                                    ILV_c = ILV_c,
#                                    ILV_tv = ILV_tv_cumul,
#                                    ILVi = c("age", "mean_dist_to_carcass_norm", "prop_informed"),
#                                    ILVs = c("age", "prop_informed"))

# "If you were making a multi-network model, you could add as many columns as you want."
# good to know for later!

model_full_dynamic <- generate_STb_model(data_list, gq = T, est_acqTime = T)
# model_full_dynamic_cumul <- generate_STb_model(data_list_cumul, gq = T, est_acqTime = T)

fit_dynamic <- fit_STb(data_list,
                       model_full_dynamic,
                       parallel_chains = 3,
                       chains = 3,
                       cores = 3,
                       iter = 500,
                       refresh=50)
STb_save(fit_dynamic, output_dir = "data/cmdstan_saves", name="dynamic_daylight_ilvs2")
fit_dynamic <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs2.rds') 
# 
# fit_dynamic_cumul <- fit_STb(data_list_cumul,
#                              model_full_dynamic_cumul,
#                              parallel_chains = 3,
#                              chains = 3,
#                              cores = 3,
#                              iter = 500,
#                              refresh=50)
# 
# STb_save(fit_dynamic_cumul, output_dir = "data/cmdstan_saves", name="dynamic_cumul_daylight_ilvs")
# fit_dynamic_cumul <- readRDS('data/cmdstan_saves/dynamic_cumul_daylight_ilvs.rds') 

STb_summary(fit_dynamic, digits = 3)
STb_summary(fit_dynamic_cumul, digits = 3)

plot_data_obs <- get_plot_data(event_data)
plot_data_ppc <- get_plot_data_ppc(fit = fit_dynamic, data_list = data_list)
plot_data_ppc_cumul <- get_plot_data_ppc(fit = fit_dynamic_cumul, data_list = data_list_cumul)


# plot it
ggplot() +
  geom_line(data = plot_data_ppc, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial",
       title = "Dynamic network (sequential)") +
  theme_minimal() # maybe a marginally better fit?? still not great, though.

ggplot() +
  geom_line(data = plot_data_ppc_cumul, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial",
       title = "Dynamic network (cumulative within days)") +
  theme_minimal() # this one is the best yet, but it's still pretty far off. We can see that the fits are getting better, and it's definitely better with daylight only vs. night. But the fit starts to get bad basically right after the first day, which really indicates that something is happening at the roosts, I think.

# XXX 2026-02-26 evening 
# Next steps:
# - Find informed status for each individual in the entire dataset over time (I think I already calculated this somewhere?). Figure out where everyone roosted the night before (get roost data). I think I already have this as well. Assign the roost data to roost polygons. Get a data frame with ID, night, informed, and polygon. Calculate, per individuals: number of roostmates, number of informed roostmates, proportion informed roostmates. Optional: Do this again but for a 500m distance instead of polygons.
# - Add distance to roost as a time-varying ILV. I think this needs to change on the same timescale as the dynamic networks (check that this is the case). Take the dataset, cut for each network. Group by individual and take the centroid of its points (does this make sense?) as well as the starting point.
# - Add age ILV to the dynamic models too. The static models are useful for testing, but not much else.
