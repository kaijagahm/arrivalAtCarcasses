# Testing stBayes

library(STbayes)
library(ggplot2)
library(tidyverse)
library(posterior)
library(targets)
library(future)
lapply(list.files("R", full.names = TRUE), source) 
# using just one carcass as an example
tar_load(data_cumul_wt_3) # this is a list of nbdaData objects
nbda_data <- data_cumul_wt_3[[4]] # carcass 4417687, which is the 4th element of the 3rd list of 10, so element 24 of stn_carcs
tar_load(stn_gps_30days)
gps <- stn_gps_30days[21:30][[4]] # the gps data that we will use for this carcass
tar_load(stn_carcs)
carc <- stn_carcs[21:30][[4]]

carc_id <- carc$carcID
gps$year <- lubridate::year(gps$date_il)
gps$ground_speed <- as.numeric(gps$ground_speed)

# Identify seed individuals if needed
tar_load(stb_mins) # number of minutes before that is defined as seeds
tar_load(ddf)
ddf_lower <- 1000
ddf_higher <- 4000
tar_load(dds)
stb_mins # 30 mins
seeds <- character(0)
time_window <- stb_mins / 60

seeds <- get_seeds(gps, ddf, dds, gps_spd)
seeds_lower <- get_seeds(gps, ddf_lower, dds, gps_spd)
seeds_higher <- get_seeds(gps, ddf_higher, dds, gps_spd)

# Get first sightings
sighting_time_max_hours <- 72
first_sightings <- get_first_sightings(gps, sighting_time_max_hours, gps_spd, ddf, dds, seeds)
first_sightings_lower <- get_first_sightings(gps, sighting_time_max_hours, gps_spd, ddf_lower, dds, seeds_lower)
first_sightings_higher <- get_first_sightings(gps, sighting_time_max_hours, gps_spd, ddf_higher, dds, seeds_higher)

all_indivs_sorted <- sort(unique(as.character(gps$individual_local_identifier)))

# Now we have the event data; time to format it the way that STbayes needs.
event_data <- format_event_data(first_sightings, seeds, all_indivs_sorted)
event_data_lower <- format_event_data(first_sightings_lower, seeds_lower, all_indivs_sorted)
event_data_higher <- format_event_data(first_sightings_higher, seeds_higher, all_indivs_sorted)

#"This edge list gives the network connections of each individual in a long format. You can give a sparse edge list that doesn’t include all dyads, but importantly, all individuals must be accounted for in the edge list. The first three columns must be:
#trial: Character or numeric column indicating which trial the networks belong to.
#focal: Character or numeric column of individual identities.
#other: Character or numeric column of individual identities."

tar_load(hours_after_carcass)
gps_fornetwork <- gps %>%
  filter(timestamp_il %in% carc$date:(carc$date + lubridate::hours(hours_after_carcass))) # limiting to 72 hours after the carcass

tar_load(rp)
network <- get_fl_weighted(dat = gps_fornetwork, dist = ddf, rp = rp, spd = gps_spd)
network_lower <- get_fl_weighted(dat = gps_fornetwork, dist = ddf_lower, rp = rp, spd = gps_spd)
network_higher <- get_fl_weighted(dat = gps_fornetwork, dist = ddf_higher, rp = rp, spd = gps_spd)
networks_fixed <- fix_nets(nets = list(network, network_lower,  network_higher), indivs = all_indivs_sorted)

networks_long <- purrr::map(networks_fixed, ~{.x %>%
  rownames_to_column(var = "focal") %>%
  pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri") %>%
  mutate(trial = carc_id)})
network_long <- networks_long[[1]]
network_long_lower <- networks_long[[2]]
network_long_higher <- networks_long[[3]]

# # Network must contain all individuals
# # "The networks dataframe is used as the reference for all unique IDs, thus each ID must be included at least once in either the focal or other column. If a dyad is absent, their connection is assumed to be zero."
# all(sort(unique(c(network_long$focal, network_long$other))) == all_indivs_sorted) #TRUE

data_list <- import_user_STb(event_data = event_data, 
                             networks = network_long,
                             network_type = "undirected") # this provides really helpful confirmatory checks

data_list_lower <- import_user_STb(event_data = event_data_lower, 
                             networks = network_long_lower,
                             network_type = "undirected")

data_list_higher <- import_user_STb(event_data = event_data_higher, 
                             networks = network_long_higher,
                             network_type = "undirected")

model_full <- generate_STb_model(data_list, gq = T, est_acqTime = T)
cat(model_full) # really long string with the model code
model_full_lower <- generate_STb_model(data_list_lower, gq = T, est_acqTime = T)
model_full_higher <- generate_STb_model(data_list_higher, gq = T, est_acqTime = T)

plan(multisession)
full_fit <- fit_STb(data_list, model_full,
                    parallel_chains = 3, chains = 3,
                    cores = 3, iter = 500, refresh=50) # okay so this takes forever, like FOREVER oh my god
full_fit_lower <- fit_STb(data_list_lower, model_full_lower,
                    parallel_chains = 3, chains = 3,
                    cores = 3, iter = 500, refresh=50)
full_fit_higher <- fit_STb(data_list_higher, model_full_higher,
                    parallel_chains = 3, chains = 3,
                    cores = 3, iter = 500, refresh=50)

STb_save(full_fit, output_dir = "data/cmdstan_saves", name="full_fit_static")
STb_save(full_fit_lower, output_dir = "data/cmdstan_saves", name="full_fit_static_lower")
STb_save(full_fit_higher, output_dir = "data/cmdstan_saves", name="full_fit_static_higher")
full_fit <- readRDS('data/cmdstan_saves/full_fit_static.rds') 
full_fit_lower <- readRDS('data/cmdstan_saves/full_fit_static_lower.rds')
full_fit_higher <- readRDS('data/cmdstan_saves/full_fit_static_higher.rds') 

STb_summary(full_fit, digits = 3)

plot_data_obs_lower <- get_plot_data(event_data_lower)
plot_data_ppc_lower <- get_plot_data_ppc(fit = full_fit_lower, data_list = data_list_lower)

ggplot() +
  geom_line(data = plot_data_ppc_lower, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs_lower, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial", title = "1km threshold") +
  theme_minimal()

plot_data_obs <- get_plot_data(event_data)
plot_data_ppc <- get_plot_data_ppc(fit = full_fit, data_list = data_list)

ggplot() +
  geom_line(data = plot_data_ppc, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial", title = "2km threshold") +
  theme_minimal()

plot_data_obs_higher <- get_plot_data(event_data_higher)
plot_data_ppc_higher <- get_plot_data_ppc(fit = full_fit_higher, data_list = data_list_higher)

ggplot() +
  geom_line(data = plot_data_ppc_higher, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs_higher, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial", title = "4km threshold") +
  theme_minimal()



# XXXX START HERE 2026-02-18

#"The most important output are the intrinsic rate (lambda_0), and the relative strength of social transmission (s), whose interpretations are the same as the NBDA package. The relative strength of social transmission (s = s_prime / lambda_0) is generally what we’re after. %ST for network n is reported as percent_ST[n]. This is a single-network model, thus percent_ST[1] is the estimated percentage of events that occurred through social transmission. The [1] refers to the “assoc” network, as we’ve only given a single network. If you fit a multi-network model, all networks will have an estimate. For a number of reasons, STbayes actually fits lambda_0 and social transmission rate (s_prime) on the log scale. The linear transformation of s_prime itself usually isn’t reported and is excluded from the output, but you could calculate it yourself from the fit."
STb_summary(full_fit, digits = 10) # if we show more digits, then it's not actually 0, it's just really small.
sm <- STb_summary(full_fit, digits = 10)
s_prime <- exp(sm$Median[sm$Parameter == "log_s_prime_mean"])
lambda_0 <- exp(sm$Median[sm$Parameter == "log_lambda_0_mean"]) # we can calculate this one ourselves instead of relying on the estimate
rel_strength_s = s_prime/lambda_0 # 918.8883. This is similar to, but not the same as, the reported s value of 9.279 x 10^2 = 928. Why isn't it the same?

plot_data_obs <- get_plot_data(test_event_data)
plot_data_ppc <- get_plot_data_ppc(fit = full_fit, data_list = data_list)

# plot it
ggplot() +
  geom_line(data = plot_data_ppc, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial") +
  theme_minimal()

# Wow, that's a really bad fit.
