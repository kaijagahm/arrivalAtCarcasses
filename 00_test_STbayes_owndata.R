# Testing stBayes

library(STbayes)
library(ggplot2)
library(tidyverse)
library(posterior)
library(targets)
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
tar_load(dds)
stb_mins # 30 mins
seeds <- character(0)
time_window <- stb_mins / 60
seeds <- gps %>%
  filter(time_since_carcass >= -time_window,
         time_since_carcass <= 0,
         (ground_speed > gps_spd & dist_to_carcass <= ddf) |
           (ground_speed <= gps_spd & dist_to_carcass <= dds)) %>%
  distinct(individual_local_identifier) %>%
  pull(individual_local_identifier) %>%
  as.character()
seeds # these are the names of the seed individuals

# Get first sightings
sighting_time_max_hours <- 72
first_sightings <- gps %>%
  filter(time_since_carcass >= 0 & time_since_carcass <= sighting_time_max_hours) %>%
  filter((ground_speed > gps_spd & dist_to_carcass <= ddf) |
           (ground_speed <= gps_spd & dist_to_carcass <= dds)) %>%
  group_by(individual_local_identifier) %>%
  arrange(time_since_carcass, timestamp) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(time_since_carcass)

first_sightings <- first_sightings %>%
  filter(!(individual_local_identifier %in% seeds))

all_indivs_sorted <- sort(unique(as.character(gps$individual_local_identifier)))

# Now we have the event data; time to format it the way that STbayes needs.

test_event_data <- first_sightings %>%
  st_drop_geometry() %>%
  dplyr::mutate(trial = carc_id, t_end = max(timestamp_il)) %>%
  dplyr::select(individual_local_identifier, trial, time_since_carcass, t_end) %>%
  mutate(time = as.numeric(time_since_carcass, units = "secs")) %>%
  rename("id" = individual_local_identifier) %>%
  mutate(t_end = max(time)) %>%
  select(id, trial, time, t_end)

#time: Integer or float values indicating the time (TADA) or order (OADA) in which the individual was recorded as first informed/knowledgable. If an individual had the event occur prior to the start of the observation period (e.g. pre-trained demonstrator), set as 0. These left censored individuals will not contribute to the likelihood calculation. 
# add the seeds back in (pre-trained demonstrators)
test_event_data <- bind_rows(data.frame(id = seeds, trial = carc_id, t_end = max(test_event_data$time), time = 0),
                             test_event_data)

#If an individual never learned during the observation period, set its value tend+1. These will be treated as right-censored individuals in the likelihood calculation.
# okay so for these, we need to figure out if there are any individuals in the gps dataset that don't appear in the first sightings.
never_learned <- all_indivs_sorted[!(all_indivs_sorted %in% test_event_data$id)]
t_end <- test_event_data$t_end[1]
test_event_data <- bind_rows(test_event_data,
                             data.frame(id = never_learned, trial = carc_id, t_end = t_end, time = t_end + 1)) %>%
  arrange(time, id) %>%
  mutate(across(c(time, t_end), as.integer)) # should be INTEGER, not NUMERIC, so the code will work properly

# Okay great, that wasn't so bad! Now we have our test event data.
# Now we need our edge_list

#"This edge list gives the network connections of each individual in a long format. You can give a sparse edge list that doesn’t include all dyads, but importantly, all individuals must be accounted for in the edge list. The first three columns must be:
#trial: Character or numeric column indicating which trial the networks belong to.
#focal: Character or numeric column of individual identities.
#other: Character or numeric column of individual identities."

tar_load(hours_after_carcass)
gps_fornetwork <- gps %>%
  filter(timestamp_il %in% carc$date:(carc$date + lubridate::hours(hours_after_carcass))) # limiting to 72 hours after the carcass

tar_load(rp)
network <- get_fl_weighted(dat = gps_fornetwork, dist = ddf, rp = rp, spd = gps_spd)
network_fixed <- fix_nets(nets = list(network), indivs = all_indivs_sorted)[[1]]
network_long <- network_fixed %>%
  rownames_to_column(var = "focal") %>%
  pivot_longer(cols = -focal, names_to = "other", values_to = "flight_sri") %>%
  mutate(trial = carc_id)

# Network must contain all individuals
# "The networks dataframe is used as the reference for all unique IDs, thus each ID must be included at least once in either the focal or other column. If a dyad is absent, their connection is assumed to be zero."
all(sort(unique(c(network_long$focal, network_long$other))) == all_indivs_sorted) #TRUE

data_list <- import_user_STb(event_data = test_event_data, 
                             networks = network_long,
                             network_type = "undirected") # this provides really helpful confirmatory checks

# "If you were making a multi-network model, you could add as many columns as you want."
# good to know for later!

model_full <- generate_STb_model(data_list, gq = T, est_acqTime = T)
cat(model_full) # really long string with the model code
# 
# full_fit <- fit_STb(data_list,
#                     model_full,
#                     parallel_chains = 3,
#                     chains = 3,
#                     cores = 3,
#                     iter = 1000,
#                     refresh=100
# ) # okay so this takes forever, like FOREVER oh my god
# # Total execution time was 33703.5 seconds
# 
# STb_save(full_fit, output_dir = "data/cmdstan_saves", name="my_first_fit")
full_fit <- readRDS('data/cmdstan_saves/my_first_fit.rds') 

#"The most important output are the intrinsic rate (lambda_0), and the relative strength of social transmission (s), whose interpretations are the same as the NBDA package. The relative strength of social transmission (s = s_prime / lambda_0) is generally what we’re after. %ST for network n is reported as percent_ST[n]. This is a single-network model, thus percent_ST[1] is the estimated percentage of events that occurred through social transmission. The [1] refers to the “assoc” network, as we’ve only given a single network. If you fit a multi-network model, all networks will have an estimate. For a number of reasons, STbayes actually fits lambda_0 and social transmission rate (s_prime) on the log scale. The linear transformation of s_prime itself usually isn’t reported and is excluded from the output, but you could calculate it yourself from the fit."
STb_summary(full_fit, digits = 3)

# Okay so for this particular model, we see that lambda_0 is being reported as 0, which makes sense since the network I gave it was over a long period of time. I didn't really expect there to be anything. Need to do a dynamic network. I just wish it didn't take so incredibly long to run!!