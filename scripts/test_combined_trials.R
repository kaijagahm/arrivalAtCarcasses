library(targets)
library(tidyverse)
library(STbayes)

tar_load(event_data)
tar_load(networks_long_combined)
tar_load(ILV_c)
tar_load(ILV_tv)
which_valid <- which(map_dbl(networks_long_combined, nrow) > 0)
event_data <- event_data[which_valid]
# Adding zero dyads to fill out networks that are missing any dyads
all_individuals <- purrr::list_rbind(event_data) %>% pull(id) %>% unique() %>% sort()
all_dyads <- expand_grid("focal" = all_individuals, "other" = all_individuals)

networks_long_combined <- networks_long_combined[which_valid]
ILV_tv <- ILV_tv[which_valid]

event_data_all_test <- purrr::list_rbind(event_data[3:4]) %>% mutate(trial = as.character(trial)) %>% mutate(time = time/1000)
# this did not throw an error for 1:2 or 1:3, but for 1:4 it did. On inspection, 
# map_dbl(event_data[1:4], nrow)
# [1] 130 130 130 129. Different total number of individuals in the diffusions.
# seems like I need to add all individuals to the networks, just set to 0.
networks_long_combined_all_test <- purrr::list_rbind(networks_long_combined[3:4]) %>%
  mutate(trial = as.character(trial)) %>%
  select(-roost_together) # there seems to be a bug in how STbayes handles two networks, so let's just do one for now.
ILV_tv_all_test <- purrr::list_rbind(ILV_tv[3:4]) %>% mutate(trial = as.character(trial))


# ---- 1. Remove self-dyads from both data frames ----
networks_clean <- networks_long_combined_all_test %>%
  filter(focal != other)

all_dyads_clean <- all_dyads %>%
  filter(focal != other)

# ---- 2. Symmetrize the existing network data ----
# For every observed (trial, time, focal, other, values), ensure the
# mirrored (trial, time, other, focal, values) also exists.
mirrored <- networks_clean %>%
  rename(focal = other, other = focal)  # swap columns

networks_symmetric <- bind_rows(networks_clean, mirrored) %>%
  distinct(trial, time, focal, other, .keep_all = TRUE)

# Sanity check: should now be perfectly symmetric
stopifnot(
  networks_symmetric %>%
    mutate(pair_id = paste(pmin(focal, other), pmax(focal, other), time, trial)) %>%
    count(pair_id) %>%
    pull(n) %>%
    { all(. == 2) }
)

# ---- 3. Build the full expected set of trial x time x dyad ----
trial_time_combos <- networks_symmetric %>%
  distinct(trial, time)

expected_full <- trial_time_combos %>%
  cross_join(all_dyads_clean)   # every dyad for every trial-time combo

# ---- 4. Find missing dyads and fill with zeros ----
missing_dyads <- anti_join(
  expected_full, networks_symmetric,
  by = c("trial", "time", "focal", "other")
) %>%
  mutate(
    #roost_together = 0,
    flight_sri_scaled = 0
  )

networks_filled <- bind_rows(networks_symmetric, missing_dyads) %>%
  arrange(trial, time, focal, other)

# ---- Sanity checks ----
n_dyads <- nrow(all_dyads_clean)
n_trial_time <- nrow(trial_time_combos)

nrow(networks_filled) == n_dyads * n_trial_time  # should be TRUE

ed_test <- event_data_all_test
n_test <- networks_filled

data_list_test <- STbayes::import_user_STb(event_data = ed_test,
                                           networks = n_test,
                                           network_type = "undirected")

write_rds(ed_test, file = "data/created/ed_test.RDS")
write_rds(n_test, file = "data/created/n_test.RDS")
write_rds(data_list_test, file = "data/created/data_list_test.RDS")

mod_test <- generate_STb_model(
  data_list_test,
  est_acqTime = FALSE#,
  # veff_params = c("lambda_0", "s"),
  # veff_type = "trial",
)


fit_test <- fit_STb(
  data_list_test, mod_test,
  chains = 1,
  iter = 10,
  refresh = 1,
  max_treedepth = 5,
  seed = 1
)