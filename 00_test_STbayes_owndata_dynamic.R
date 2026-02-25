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
tar_load(dds)
stb_mins # 30 mins
seeds <- character(0)
time_window <- stb_mins / 60

seeds <- get_seeds(gps, ddf, dds, gps_spd)
seeds # these are the names of the seed individuals

# Get first sightings
first_sightings <- get_first_sightings(gps, sighting_time_max_hours, gps_spd, ddf, dds, seeds)

all_indivs_sorted <- sort(unique(as.character(gps$individual_local_identifier)))

# Now we have the event data; time to format it the way that STbayes needs.
event_data <- format_event_data(first_sightings, seeds, all_indivs_sorted)
tar_load(hours_after_carcass)

gps_fornetwork <- gps %>%
  filter(timestamp_il %in% carc$date:(carc$date + lubridate::hours(hours_after_carcass))) %>%
  mutate(time = as.numeric(time_since_carcass, units = "secs")) %>% # this will now correspond to the numeric times in test_event_data.
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
networks_long_dynamic <- purrr::list_rbind(networks_long, names_to = "time")

networks_long_cumul <- map(dynamic_networks_cumul_fixed, ~{
  out <- .x %>% rownames_to_column(var = "focal") %>%
    pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri") %>%
    mutate(trial = carc_id)
  return(out)
})
networks_long_dynamic_cumul <- purrr::list_rbind(networks_long_cumul, names_to = "time")

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
                    iter = 500,
                    refresh=50)
STb_save(fit_dynamic, output_dir = "data/cmdstan_saves", name="dynamic")
fit_dynamic <- readRDS('data/cmdstan_saves/dynamic.rds') 

fit_dynamic_cumul <- fit_STb(data_list_cumul,
                       model_full_dynamic_cumul,
                       parallel_chains = 3,
                       chains = 3,
                       cores = 3,
                       iter = 500,
                       refresh=50)

STb_save(fit_dynamic_cumul, output_dir = "data/cmdstan_saves", name="dynamic_cumul")
fit_dynamic_cumul <- readRDS('data/cmdstan_saves/dynamic_cumul.rds') 

STb_summary(fit_dynamic, digits = 3)
STb_summary(fit_dynamic_cumul, digits = 3)

#"The most important output are the intrinsic rate (lambda_0), and the relative strength of social transmission (s), whose interpretations are the same as the NBDA package. The relative strength of social transmission (s = s_prime / lambda_0) is generally what we’re after. %ST for network n is reported as percent_ST[n]. This is a single-network model, thus percent_ST[1] is the estimated percentage of events that occurred through social transmission. The [1] refers to the “assoc” network, as we’ve only given a single network. If you fit a multi-network model, all networks will have an estimate. For a number of reasons, STbayes actually fits lambda_0 and social transmission rate (s_prime) on the log scale. The linear transformation of s_prime itself usually isn’t reported and is excluded from the output, but you could calculate it yourself from the fit.

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
