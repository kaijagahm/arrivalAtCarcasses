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
tar_load(stmh)
sighting_time_max_hours <- 72
tar_load(data_cumul_wt_3) # this is a list of nbdaData objects
nbda_data <- data_cumul_wt_3[[4]] # carcass 4417687, which is the 4th element of the 3rd list of 10, so element 24 of stn_carcs

tar_load(stn_carcs)
carc <- stn_carcs[21:30][[4]]
event_time <- carc$datetime_il

# Saving gps data as temp file because it takes too long otherwise.
# tar_load(stn_gps_30days)
# gps <- stn_gps_30days[21:30][[4]] # the gps data that we will use for this carcass
# write_rds(gps, "data/created/gps_for_STbayes.RDS")
gps <- readRDS("data/created/gps_for_STbayes.RDS")

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
  mutate(daytime_since_carcass = case_when(time_since_carcass >= 0 ~ as.numeric(time_since_carcass)-cumul_night_hrs,
                                           .default = NA)) %>%
  arrange(timestamp_il)

gps %>%
  st_drop_geometry() %>%
  mutate(across(contains("_since_carcass"), as.numeric)) %>%
  pivot_longer(cols = c("time_since_carcass", "daytime_since_carcass"), names_to = "type", values_to = "hours") %>%
  select(timestamp_il, type, hours) %>%
  mutate(type = str_remove(type, "_since_carcass")) %>%
  mutate(type = case_when(type == "time" ~ "real time", .default = type)) %>%
  ggplot(aes(x = timestamp_il, y = hours, color = type))+
  geom_point()+
  theme_minimal()+
  scale_color_manual(name = "Type", values = c("dodgerblue2", "black"))+
  labs(y = "Hours since carcass", x = "Time", title = "Two ways to measure time since carcass")+
  NULL

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

seeds <- get_seeds(gps, ddf, dds, gps_spd, time_col = "time_since_carcass") # still using time_since_carcass since it goes back farther than daytime_since_carcass.
seeds # these are the names of the seed individuals

# Get first sightings
all_indivs_sorted <- sort(unique(as.character(gps$individual_local_identifier)))

gps_diffusion <- gps %>% filter(time_since_carcass >= 0)
first_sightings <- get_first_sightings(gps_diffusion, sighting_time_max_hours, gps_spd, ddf, dds, seeds)

# Now we have the event data; time to format it the way that STbayes needs.
event_data <- format_event_data(first_sightings, seeds, all_indivs_sorted, time_col = "daytime_since_carcass")
tar_load(hours_after_carcass)

gps_fornetwork <- gps_diffusion %>%
  filter(timestamp_il %in% carc$date:(carc$date + lubridate::hours(hours_after_carcass))) %>%
  mutate(time = as.numeric(daytime_since_carcass)*60*60) %>% # this will now correspond to the numeric times in test_event_data.
  filter(time >= 0)

# "if the user’s observation period included 10 events and the dataset does contain censored individuals, they should supply edge weights from 11 networks in total, where time=1 should contain the network representing the period from [t0,te1), time=2 represents [te1,te2), and time=11 represents from [te10,tend]. NB: If there are censored individuals, the end of the observation period should necessarily be larger than the time of the final event (event_data$t_end > max(event_data$time)."
# Note: it does NOT say what to do if there are seed individuals... I assume I don't provide a network for those, since they're set to 0, so the first one will just be from 0 through te1? (e1 = event 1)
# Also, what they're saying about the end of the observation period seems to contradict how they said to encode the censored individuals (i.e. set them to tend+1)

cutpoints <- unique(event_data$time)
sort(unique(cut(gps_fornetwork$time, breaks = cutpoints))) # these look right!
gps_fornetwork$network <- cut(gps_fornetwork$time, breaks = cutpoints)
length(unique(gps_fornetwork$network))
network_intervals <- sort(unique(gps_fornetwork$network))

gps_list <- gps_fornetwork %>% arrange(network) %>% group_split(network, .keep = TRUE)
# now we need to split cumulative by day
# test <- event_data %>% left_join(select(first_sightings, individual_local_identifier, date_il), by = c("id" = "individual_local_identifier"))
# test$network <- cut(test$time, breaks = cutpoints)
# gps_fornetwork <- left_join(gps_fornetwork, select(st_drop_geometry(test), date_il, network), by = "network")
gps_list_cumulative <- gps_fornetwork %>%
  arrange(date_il, network) %>%
  group_split(date_il) %>%
  map(function(day_df) {
    
    # get unique group_idx in the order they appear within the day
    groups <- unique(day_df$network)
    
    map(seq_along(groups), function(i) {
      day_df %>%
        filter(network %in% groups[1:i])
    })
    
  }) %>%
  flatten()

tar_load(rp)
dynamic_networks <- map(gps_list, ~get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd))
dynamic_networks_fixed <- fix_nets(nets = dynamic_networks, indivs = all_indivs_sorted)

dynamic_networks_cumul <- map(gps_list_cumulative, ~get_fl_weighted(dat = .x, dist = ddf, rp = rp, spd = gps_spd))
dynamic_networks_cumul_fixed <- fix_nets(nets = dynamic_networks_cumul, indivs = all_indivs_sorted)

networks_long <- map(dynamic_networks_fixed, ~{
  out <- .x %>% rownames_to_column(var = "focal") %>%
    pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri") %>%
    mutate(trial = carc_id)
  return(out)
})
networks_long_dynamic <- purrr::list_rbind(networks_long, names_to = "time") # this creates a numeric column for "time", which is how stbayes wants it--sequential integer values, not group names.
write_rds(networks_long_dynamic, file = "data/created/networks_long_dynamic.RDS")
networks_long_dynamic <- readRDS("data/created/networks_long_dynamic.RDS")

networks_long_cumul <- map(dynamic_networks_cumul_fixed, ~{
  out <- .x %>% rownames_to_column(var = "focal") %>%
    pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri") %>%
    mutate(trial = carc_id)
  return(out)
})
networks_long_dynamic_cumul <- purrr::list_rbind(networks_long_cumul, names_to = "time") # this creates a numeric column for "time", which is how stbayes wants it--sequential integer values, not group names.
write_rds(networks_long_dynamic_cumul, file = "data/created/networks_long_dynamic_cumul.RDS")
networks_long_dynamic_cumul <- readRDS("data/created/networks_long_dynamic_cumul.RDS")

# Network must contain all individuals
# "The networks dataframe is used as the reference for all unique IDs, thus each ID must be included at least once in either the focal or other column. If a dyad is absent, their connection is assumed to be zero."
all(sort(unique(c(networks_long_dynamic$focal, networks_long_dynamic$other))) == all_indivs_sorted) #TRUE
all(sort(unique(c(networks_long_dynamic_cumul$focal, networks_long_dynamic_cumul$other))) == all_indivs_sorted) #TRUE

data_list <- import_user_STb(event_data = event_data, 
                             networks = networks_long_dynamic,
                             network_type = "undirected") # this provides really helpful confirmatory checks
data_list_cumul <- import_user_STb(event_data = event_data, 
                                   networks = networks_long_dynamic_cumul,
                                   network_type = "undirected")

# "If you were making a multi-network model, you could add as many columns as you want."
# good to know for later!

model_full_dynamic <- generate_STb_model(data_list, gq = T, est_acqTime = T)
model_full_dynamic_cumul <- generate_STb_model(data_list_cumul, gq = T, est_acqTime = T)

fit_dynamic <- fit_STb(data_list,
                    model_full_dynamic,
                    parallel_chains = 3,
                    chains = 3,
                    cores = 3,
                    iter = 250,
                    refresh=50)
STb_save(fit_dynamic, output_dir = "data/cmdstan_saves", name="dynamic_daylight")
fit_dynamic <- readRDS('data/cmdstan_saves/dynamic_daylight.rds') 

fit_dynamic_cumul <- fit_STb(data_list_cumul,
                       model_full_dynamic_cumul,
                       parallel_chains = 3,
                       chains = 3,
                       cores = 3,
                       iter = 250,
                       refresh=50)

STb_save(fit_dynamic_cumul, output_dir = "data/cmdstan_saves", name="dynamic_cumul_daylight")
fit_dynamic_cumul <- readRDS('data/cmdstan_saves/dynamic_cumul_daylight.rds') 

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
  theme_minimal()

ggplot() +
  geom_line(data = plot_data_ppc_cumul, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial",
       title = "Dynamic network (cumulative within days)") +
  theme_minimal()
