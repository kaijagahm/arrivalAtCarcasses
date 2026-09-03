# reprex to demonstrate error with STbayes

library(targets)
library(tidyverse)
library(STbayes)

# See below for lines loading in the pre-formatted data!

# tar_load(event_data)
# tar_load(networks_long_combined)
# tar_load(ILV_c)
# tar_load(ILV_tv)
# which_valid <- which(map_dbl(networks_long_combined, nrow) > 0)
# event_data <- event_data[which_valid]
# # Adding zero dyads to fill out networks that are missing any dyads
# all_individuals <- purrr::list_rbind(event_data) %>% pull(id) %>% unique() %>% sort()
# all_dyads <- expand_grid("focal" = all_individuals, "other" = all_individuals)
# 
# networks_long_combined <- networks_long_combined[which_valid]
# ILV_tv <- ILV_tv[which_valid]
# 
# event_data_all_test <- purrr::list_rbind(event_data[3:4]) %>% mutate(trial = as.character(trial)) %>% mutate(time = time/1000)
# # this did not throw an error for 1:2 or 1:3, but for 1:4 it did. On inspection,
# # map_dbl(event_data[1:4], nrow)
# # [1] 130 130 130 129. Different total number of individuals in the diffusions.
# # seems like I need to add all individuals to the networks, just set to 0.
# networks_long_combined_all_test <- purrr::list_rbind(networks_long_combined[3:4]) %>%
#   mutate(trial = as.character(trial)) %>%
#   select(-roost_together) # there seems to be a bug in how STbayes handles two networks, so let's just do one for now.
# ILV_tv_all_test <- purrr::list_rbind(ILV_tv[3:4]) %>% mutate(trial = as.character(trial))
# 
# 
# # ---- 1. Remove self-dyads from both data frames ----
# networks_clean <- networks_long_combined_all_test %>%
#   filter(focal != other)
# 
# all_dyads_clean <- all_dyads %>%
#   filter(focal != other)
# 
# # ---- 2. Symmetrize the existing network data ----
# # For every observed (trial, time, focal, other, values), ensure the
# # mirrored (trial, time, other, focal, values) also exists.
# mirrored <- networks_clean %>%
#   rename(focal = other, other = focal)  # swap columns
# 
# networks_symmetric <- bind_rows(networks_clean, mirrored) %>%
#   distinct(trial, time, focal, other, .keep_all = TRUE)
# 
# # Sanity check: should now be perfectly symmetric
# stopifnot(
#   networks_symmetric %>%
#     mutate(pair_id = paste(pmin(focal, other), pmax(focal, other), time, trial)) %>%
#     count(pair_id) %>%
#     pull(n) %>%
#     { all(. == 2) }
# )
# 
# # ---- 3. Build the full expected set of trial x time x dyad ----
# trial_time_combos <- networks_symmetric %>%
#   distinct(trial, time)
# 
# expected_full <- trial_time_combos %>%
#   cross_join(all_dyads_clean)   # every dyad for every trial-time combo
# 
# # ---- 4. Find missing dyads and fill with zeros ----
# missing_dyads <- anti_join(
#   expected_full, networks_symmetric,
#   by = c("trial", "time", "focal", "other")
# ) %>%
#   mutate(
#     #roost_together = 0,
#     flight_sri_scaled = 0
#   )
# 
# networks_filled <- bind_rows(networks_symmetric, missing_dyads) %>%
#   arrange(trial, time, focal, other)
# 
# # ---- Sanity checks ----
# n_dyads <- nrow(all_dyads_clean)
# n_trial_time <- nrow(trial_time_combos)
# 
# nrow(networks_filled) == n_dyads * n_trial_time  # should be TRUE
# 
# ed_test <- event_data_all_test
# n_test <- networks_filled
# 
# data_list_test <- STbayes::import_user_STb(event_data = ed_test,
#                                            networks = n_test,
#                                            network_type = "undirected")
# 
# write_rds(ed_test, file = "data/created/ed_test.RDS")
# write_rds(n_test, file = "data/created/n_test.RDS")
# write_rds(data_list_test, file = "data/created/data_list_test.RDS")

ed_test <- readRDS("data/created/ed_test.RDS")
n_test <- readRDS("data/created/n_test.RDS")
data_list_test <- readRDS("data/created/data_list_test.RDS")

mod_test_fails <- generate_STb_model(
  data_list_test,
  est_acqTime = TRUE
)

mod_test_works <- generate_STb_model(
  data_list_test,
  est_acqTime = FALSE
)


fit_STb(
  data_list_test, mod_test_fails,
  chains = 1,
  iter = 10,
  refresh = 1,
  max_treedepth = 5,
  seed = 1
)

fit_STb(
  data_list_test, mod_test_works,
  chains = 1,
  iter = 10,
  refresh = 1,
  max_treedepth = 5,
  seed = 1
)

# I asked Claude to look at this and figure out what's going on, and here's what it says. I don't know if this is helpful.
# "I cloned the package source and traced it down — found the exact bug. Here's what's happening:

# The root cause: In generate_STb_model_TADA.R, the ind_id data array (array[K, Q] int<lower=-1> ind_id) stores individual IDs per trial, left-aligned and padded with -1 for any trial that has fewer individuals than the largest trial (Q = max individual count across all trials). This is a legitimate, intentional design — since your 4 trials have slightly different numbers of individuals in event_data (130, 130, 130, 129 — driven by how many demonstrators/events/censored each trial has), Q = 130 and trial 4's ind_id row has exactly one padding slot: ind_id[4, 130] = -1.
# 
# The regular likelihood in the model block handles this correctly — it loops using the trial-specific counts:
# 
# stan
# for (n in 1:N[trial]) {           // <- bounded per-trial, line 832
#     int id = ind_id[trial, n];
#     ...
# if (N_c[trial] > 0) {
#     for (c in 1:N_c[trial]) {     // <- bounded per-trial, line 853
#         int id = ind_id[trial, N[trial] + c];
# 
# But the est_acqTime generated-quantities code (both option = "a", lines 869–909, and option = "b", lines 910–951) loops over the global Q instead:
# 
# stan
# for (n in 1:Q) {                   // <- BUG: should be 1:(N[trial]+N_c[trial])
#     int id = ind_id[trial, n];
#     int learn_time = t[trial, id]; // <- when id == -1 (padding), this throws
# 
# For trial 4 (fewer individuals than Q), when n reaches 130, id = ind_id[4, 130] = -1, and t[4, -1] is exactly the "index -1 out of range; expecting index to be between 1 and 156" error you saw (156 = your total individual count, since t is a [K, P] matrix).
# 
# Why est_acqTime = FALSE works: that code path is never generated/executed at all, so the buggy loop just doesn't exist in that model.
# 
# The fix (in the package source) would be replacing for (n in 1:Q) with for (n in 1:(N[trial] + N_c[trial])) in both est_acqTime_code blocks — mirroring the working pattern from the main model block. This is a genuine bug specifically triggered by trials with unequal numbers of individuals, which is exactly your situation (your trial 4 has 21 demonstrators vs. 1–2 in others, shrinking its event_data row count below the other trials')."