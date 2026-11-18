library(targets)
library(STbayes)
library(posterior)
library(bayestestR)
library(igraph)
library(tidyverse)
library(patchwork)

#idx <- 1:10

tar_load(event_data)
tar_load(event_data_wild)
tar_load(networks_long_combined)
tar_load(networks_long_combined_wild)
tar_load(ILV_c)
tar_load(ILV_c_wild)
tar_load(ILV_tv)
tar_load(ILV_tv_wild)

source("scripts/sparse_helpers.R")

## Need to join age ILV to time-varying ILVs since we're going to be running all of the diffusions together
ILV_tv_new <- purrr::map2(ILV_c, ILV_tv, ~{
  if(!is.null(.y)){
    out <- .y %>%
      left_join(.x, by = "id")
    return(out)
  }else{
    return(NULL)
  }
})

ILV_tv_new_wild <- purrr::map2(ILV_c_wild, ILV_tv_wild, ~{
  if(!is.null(.y)){
    out <- .y %>%
      left_join(.x, by = "id")
    return(out)
  }else{
    return(NULL)
  }
})

rm(ILV_c)
rm(ILV_c_wild)
rm(ILV_tv)
rm(ILV_tv_wild)

write_rds(ILV_tv_new, file = "data/created/ILV_tv_new.RDS")
write_rds(ILV_tv_new_wild, file = "data/created/ILV_tv_new_wild.RDS")
ILV_tv_new <- readRDS("data/created/ILV_tv_new.RDS")
ILV_tv_new_wild <- readRDS("data/created/ILV_tv_new_wild.RDS")

which_valid <- which(map_dbl(networks_long_combined, nrow) > 0)
which_valid_wild <- which(map_dbl(networks_long_combined_wild, nrow) > 0)
event_data <- event_data[which_valid]
event_data_wild <- event_data_wild[which_valid_wild]
# Adding zero dyads to fill out networks that are missing any dyads
# after the which_valid filtering of all lists:
all_individuals <- purrr::list_rbind(event_data) %>% pull(id) %>% unique() %>% sort()
all_individuals_wild <- purrr::list_rbind(event_data_wild) %>% pull(id) %>% unique() %>% sort()
all_dyads <- expand_grid("focal" = all_individuals, "other" = all_individuals)
all_dyads_wild <- expand_grid("focal" = all_individuals_wild, "other" = all_individuals_wild)

networks_long_combined <- networks_long_combined[which_valid]
networks_long_combined_wild <- networks_long_combined_wild[which_valid_wild]
ILV_tv_new <- ILV_tv_new[which_valid]
ILV_tv_new_wild <- ILV_tv_new_wild[which_valid_wild]

event_data_all <- purrr::list_rbind(event_data) %>% mutate(trial = as.character(trial)) %>% mutate(time = time/1000, t_end = t_end/1000)
event_data_all_wild <- purrr::list_rbind(event_data_wild) %>% mutate(trial = as.character(trial)) %>% mutate(time = time/1000, t_end = t_end/1000)

map_dbl(event_data, nrow)
map_dbl(event_data_wild, nrow)
rm(event_data)
rm(event_data_wild)
# [1] 130 130 130 129. Different total number of individuals in the diffusions.
# seems like I need to add all individuals to the networks, just set to 0.
networks_long_combined_all <- purrr::list_rbind(networks_long_combined) %>%
  mutate(trial = as.character(trial))
networks_long_combined_all_wild <- purrr::list_rbind(networks_long_combined_wild) %>%
  mutate(trial = as.character(trial))

ILV_tv_new_all <- purrr::list_rbind(ILV_tv_new) %>% mutate(trial = as.character(trial))
ILV_tv_new_all_wild <- purrr::list_rbind(ILV_tv_new_wild) %>% mutate(trial = as.character(trial))
rm(ILV_tv_new)
rm(ILV_tv_new_wild)
rm(networks_long_combined_wild)
rm(networks_long_combined)
ILV_tv_new_combined <- bind_rows(ILV_tv_new_all, ILV_tv_new_all_wild)
rm(ILV_tv_new_all)
rm(ILV_tv_new_all_wild)

# ---- 1. Remove self-dyads from both data frames ----
networks_clean <- networks_long_combined_all %>%
  filter(focal != other)
networks_clean_wild <- networks_long_combined_all_wild %>%
  filter(focal != other)

rm(networks_long_combined_all)
rm(networks_long_combined_all_wild)

all_dyads_clean <- all_dyads %>%
  filter(focal != other)
all_dyads_clean_wild <- all_dyads_wild %>%
  filter(focal != other)
rm(all_dyads)
rm(all_dyads_wild)

# ---- 2. Symmetrize the existing network data ----
# For every observed (trial, time, focal, other, values), ensure the
# mirrored (trial, time, other, focal, values) also exists.
mirrored <- networks_clean %>%
  rename(focal = other, other = focal)  # swap columns
mirrored_wild <- networks_clean_wild %>%
  rename(focal = other, other = focal)  # swap columns

networks_symmetric <- bind_rows(networks_clean, mirrored) %>%
  distinct(trial, time, focal, other, .keep_all = TRUE)
networks_symmetric_wild <- bind_rows(networks_clean_wild, mirrored_wild) %>%
  distinct(trial, time, focal, other, .keep_all = TRUE)

rm(networks_clean)
rm(networks_clean_wild)
rm(mirrored)
rm(mirrored_wild)

# ---- 3. Build the full expected set of trial x time x dyad ----
trial_time_combos <- networks_symmetric %>%
  distinct(trial, time)
trial_time_combos_wild <- networks_symmetric_wild %>%
  distinct(trial, time)

expected_full <- trial_time_combos %>%
  cross_join(all_dyads_clean)   # every dyad for every trial-time combo
expected_full_wild <- trial_time_combos_wild %>%
  cross_join(all_dyads_clean_wild)

# ---- 4. Find missing dyads and fill with zeros ----
missing_dyads <- anti_join(
  expected_full, networks_symmetric,
  by = c("trial", "time", "focal", "other")
) %>%
  mutate(
    roost_together = 0,
    flight_sri_scaled = 0
  )

missing_dyads_wild <- anti_join(
  expected_full_wild, networks_symmetric_wild,
  by = c("trial", "time", "focal", "other")
) %>%
  mutate(
    roost_together = 0,
    flight_sri_scaled = 0
  )

networks_filled <- bind_rows(networks_symmetric, missing_dyads) %>%
  arrange(trial, time, focal, other)
networks_filled_wild <- bind_rows(networks_symmetric_wild, missing_dyads_wild) %>%
  arrange(trial, time, focal, other)
rm(networks_symmetric)
rm(networks_symmetric_wild)
rm(missing_dyads)
rm(missing_dyads_wild)
rm(expected_full)
rm(expected_full_wild)

# ---- Sanity checks ----
n_dyads <- nrow(all_dyads_clean)
n_dyads_wild <- nrow(all_dyads_clean_wild)
n_trial_time <- nrow(trial_time_combos)
n_trial_time_wild <- nrow(trial_time_combos_wild)
rm(all_dyads_clean)
rm(all_dyads_clean_wild)

nrow(networks_filled) == n_dyads * n_trial_time  # should be TRUE
nrow(networks_filled_wild) == n_dyads_wild * n_trial_time_wild

ed <- event_data_all
ed_wild <- event_data_all_wild
n <- networks_filled %>% select(-roost_date)
n_wild <- networks_filled_wild %>% select(-roost_date)
rm(event_data_all)
rm(event_data_all_wild)
rm(networks_filled)
rm(networks_filled_wild)

ed_all <- bind_rows(ed, ed_wild)
n_all <- bind_rows(n, n_wild)

write_rds(ed_all, file = "data/created/combined_models/ed_all.RDS")
write_rds(n_all, file = "data/created/combined_models/n_all.RDS")
ed_all <- readRDS("data/created/combined_models/ed_all.RDS")
n_all <- readRDS("data/created/combined_models/n_all.RDS")

# Pad out networks with age values for all individuals
global_ids <- sort(unique(c(ed_all$id, n_all$focal, n_all$other)))

age_lookup <- ILV_tv_new_combined %>% # absent individuals get their most common age value across all the trials--shouldn't matter because these won't be used anyway.
  count(id, age) %>%
  group_by(id) %>% slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>% select(id, age)

full_keys <- ILV_tv_new_combined %>%
  distinct(trial, time) %>%
  cross_join(tibble(id = global_ids))

pad_rows <- full_keys %>%
  anti_join(ILV_tv_new_combined, by = c("trial", "time", "id")) %>%
  left_join(age_lookup, by = "id") %>%
  mutate(mean_dist_to_carcass_norm = 0)
rm(age_lookup)

ILV_tv_padded <- bind_rows(ILV_tv_new_combined, pad_rows) %>%
  arrange(trial, time, id)
rm(ILV_tv_new_combined)
rm(pad_rows)

anyNA(ILV_tv_padded)                                # FALSE

anyNA(ILV_tv_padded)                                         # FALSE
anyDuplicated(ILV_tv_padded[c("trial", "id", "time")])       # 0
nrow(ILV_tv_padded) == nrow(full_keys)                        # TRUE
ILV_tv_padded %>% group_by(trial) %>%
  summarise(n_ids = n_distinct(id), n_t = n_distinct(time), .groups = "drop")
rm(full_keys)

# Making the data_list was taking a long time. I talked with Claude and showed it the documentation and it says that I can get away with a sparse network so there aren't so many 0 rows.

# Run once, starting from the full table n_all
all_ids <- sort(unique(c(n_all$focal, n_all$other)))

# 1. Keep only real (nonzero) edges
n_nz <- n_all |> filter(flight_sri_scaled > 0 | roost_together > 0)

# 2. Placeholder rows for slices with no nonzero edges, so slice
#    numbering (rebuilt from the `time` values) matches the full table.
#    Uses two IDs from the trial's event data.
pad_slices <- n_all |>
  distinct(trial, time) |>
  anti_join(n_nz, by = c("trial", "time")) |>
  left_join(
    ed_all |> distinct(trial, id) |> group_by(trial) |>
      summarise(focal = first(id), other = nth(id, 2), .groups = "drop"),
    by = "trial"
  ) |>
  mutate(flight_sri_scaled = 0, roost_together = 0)

# 3. Zero-weight rows so every ID appears in every trial
#    (needed when subsetting trials; harmless for the full import).
#    Skips dyads that already have a real edge in that slice, in either direction.
pad_ids <- n_nz |>
  bind_rows(pad_slices) |>
  group_by(trial) |> summarise(time = min(time), .groups = "drop") |>
  tidyr::crossing(focal = all_ids) |>
  mutate(other = all_ids[match(focal, all_ids) %% length(all_ids) + 1],
         flight_sri_scaled = 0, roost_together = 0) |>
  anti_join(n_nz, by = c("trial", "time", "focal", "other")) |>
  anti_join(n_nz, by = c("trial", "time", "focal" = "other", "other" = "focal"))

n_sparse <- bind_rows(n_nz, pad_slices, pad_ids)
n_sparse_binary <- n_sparse %>% mutate(flight_sri_scaled =case_when(flight_sri_scaled > 0 ~ 1, .default = flight_sri_scaled))
rm(pad_ids)

# 4. Checks
nrow(n_sparse)                                                   # ~220k
n_distinct(n_sparse$trial)                                       # 142
n_distinct(c(n_sparse$focal, n_sparse$other))                    # 161
n_sparse |> group_by(trial) |>
  summarise(k = n_distinct(c(focal, other))) |> count(k)         # "k = 161: num distinct IDs in a trial. n = 142: num trials w/that value. So 142 trials have exactly 161 IDs.”
identical(                                                       # same slices per trial
  n_all    |> distinct(trial, time) |> arrange(trial, time),
  n_sparse |> distinct(trial, time) |> arrange(trial, time))     # TRUE
sum(n_sparse$flight_sri_scaled) == sum(n_all$flight_sri_scaled)  # TRUE (no real flight edges lost)
sum(n_sparse$roost_together) == sum(n_all$roost_together) # TRUE (no real roost edges lost)
saveRDS(n_sparse, "data/created/combined_models/n_sparse.RDS")
saveRDS(n_sparse_binary, "data/created/combined_models/n_sparse_binary.RDS")
n_sparse <- readRDS("data/created/combined_models/n_sparse.RDS")
n_sparse_binary <- readRDS("data/created/combined_models/n_sparse_binary.RDS")

# Array comparison:
f <- function(nets) STbayes::import_user_STb(
  event_data = ed_all |> dplyr::filter(trial %in% keep),
  networks   = nets |> dplyr::filter(trial %in% keep),
  network_type = "undirected",
  ILV_tv = ILV_tv_padded |> dplyr::filter(trial %in% keep),
  ILVi = c("age", "mean_dist_to_carcass_norm"),
  ILVs = "age"
)
edgeless <- setdiff(unique(n_all$trial), unique(n_nz$trial))
keep <- c(edgeless[1], sample(setdiff(unique(ed_all$trial), edgeless), 4))
a <- f(n_all); b <- f(n_sparse)
identical(a$A, b$A) # should be TRUE
rm(a)
rm(b)
rm(n_nz)

# Make the data lists!
# See reference list to keep track of data list versions
dl_01 <- STbayes::import_user_STb(
  event_data = ed_all,
  networks = n_sparse, # instead of n_all
  network_type = "undirected",
  ILV_tv = ILV_tv_padded,
  ILVi = c("age", "mean_dist_to_carcass_norm"),   # ILVs acting on the intrinsic rate
  ILVs = c("age")   # ILVs acting on the social rate
)
stopifnot(dim(dl_01$A)[1] == 2, dl_01$N_networks == 2)
sum_dense <- sum(dl_01$A)
saveRDS(dl_01, "data/created/combined_models/dl_01.RDS")
dl_01_sparse <- sparsify_A(dl_01)
stopifnot(isTRUE(all.equal(sum(dl_01_sparse$A_val), sum_dense)))   # no values lost
dl_01_sparse$A_nnz
saveRDS(dl_01_sparse, "data/created/combined_models/dl_01_sparse.RDS")

dl_01_binary <- STbayes::import_user_STb(
  event_data = ed_all,
  networks = n_sparse_binary, # instead of n_all
  network_type = "undirected",
  ILV_tv = ILV_tv_padded,
  ILVi = c("age", "mean_dist_to_carcass_norm"),   # ILVs acting on the intrinsic rate
  ILVs = c("age")   # ILVs acting on the social rate
)
stopifnot(dim(dl_01_binary$A)[1] == 2, dl_01_binary$N_networks == 2)
sum_dense <- sum(dl_01_binary$A)
saveRDS(dl_01_binary, "data/created/combined_models/dl_01_binary.RDS")
dl_01_sparse_binary <- sparsify_A(dl_01_binary)
stopifnot(isTRUE(all.equal(sum(dl_01_sparse_binary$A_val), sum_dense)))   # no values lost
dl_01_sparse_binary$A_nnz
saveRDS(dl_01_sparse_binary, "data/created/combined_models/dl_01_sparse_binary.RDS")

n_sparse_02 <- dplyr::select(n_sparse, -roost_together)
dl_02 <- STbayes::import_user_STb(
  event_data = ed_all,
  networks = n_sparse_02,
  network_type = "undirected",
  ILV_tv = ILV_tv_padded,
  ILVi = c("age", "mean_dist_to_carcass_norm"),
  ILVs = c("age")
)
stopifnot(dim(dl_02$A)[1] == 1, dl_02$N_networks == 1)
sum_dense <- sum(dl_02$A)
saveRDS(dl_02, "data/created/combined_models/dl_02.RDS")
dl_02_sparse <- sparsify_A(dl_02)
stopifnot(isTRUE(all.equal(sum(dl_02_sparse$A_val), sum_dense)))   # no values lost
dl_02_sparse$A_nnz
saveRDS(dl_02_sparse, "data/created/combined_models/dl_02_sparse.RDS")

n_sparse_02_binary <- dplyr::select(n_sparse_binary, -roost_together)
dl_02 <- STbayes::import_user_STb(
  event_data = ed_all,
  networks = n_sparse_02_binary,
  network_type = "undirected",
  ILV_tv = ILV_tv_padded,
  ILVi = c("age", "mean_dist_to_carcass_norm"),
  ILVs = c("age")
)
stopifnot(dim(dl_02_binary$A)[1] == 1, dl_02_binary$N_networks == 1)
sum_dense <- sum(dl_02_binary$A)
saveRDS(dl_02_binary, "data/created/combined_models/dl_02_binary.RDS")
dl_02_sparse_binary <- sparsify_A(dl_02_binary)
stopifnot(isTRUE(all.equal(sum(dl_02_sparse_binary$A_val), sum_dense)))   # no values lost
dl_02_sparse_binary$A_nnz
saveRDS(dl_02_sparse_binary, "data/created/combined_models/dl_02_sparse_binary.RDS")

n_sparse_03 <- dplyr::select(n_sparse, -flight_sri_scaled)
dl_03 <- STbayes::import_user_STb(
  event_data = ed_all,
  networks = n_sparse_03,
  network_type = "undirected",
  ILV_tv = ILV_tv_padded,
  ILVi = c("age", "mean_dist_to_carcass_norm"),
  ILVs = c("age")
)
stopifnot(dim(dl_03$A)[1] == 1, dl_03$N_networks == 1)
sum_dense <- sum(dl_03$A)
saveRDS(dl_03, "data/created/combined_models/dl_03.RDS")
dl_03_sparse <- sparsify_A(dl_03)
stopifnot(isTRUE(all.equal(sum(dl_03_sparse$A_val), sum_dense)))   # no values lost
dl_03_sparse$A_nnz
saveRDS(dl_03_sparse, "data/created/combined_models/dl_03_sparse.RDS")
# No need for binary on this one since it doesn't have the flight network

dl_04 <- STbayes::import_user_STb(
  event_data = ed_all,
  networks = n_sparse,
  network_type = "undirected",
  ILV_tv = ILV_tv_padded,
  ILVi = c("mean_dist_to_carcass_norm"),
  ILVs = c("age")
)
stopifnot(dim(dl_04$A)[1] == 2, dl_04$N_networks == 2)
sum_dense <- sum(dl_04$A)
saveRDS(dl_04, "data/created/combined_models/dl_04.RDS")
dl_04_sparse <- sparsify_A(dl_04)
stopifnot(isTRUE(all.equal(sum(dl_04_sparse$A_val), sum_dense)))   # no values lost
dl_04_sparse$A_nnz
saveRDS(dl_04_sparse, "data/created/combined_models/dl_04_sparse.RDS")

dl_04_binary <- STbayes::import_user_STb(
  event_data = ed_all,
  networks = n_sparse_binary,
  network_type = "undirected",
  ILV_tv = ILV_tv_padded,
  ILVi = c("mean_dist_to_carcass_norm"),
  ILVs = c("age")
)
stopifnot(dim(dl_04_binary$A)[1] == 2, dl_04_binary$N_networks == 2)
sum_dense <- sum(dl_04_binary$A)
saveRDS(dl_04_binary, "data/created/combined_models/dl_04_binary.RDS")
dl_04_sparse_binary <- sparsify_A(dl_04_binary)
stopifnot(isTRUE(all.equal(sum(dl_04_sparse_binary$A_val), sum_dense)))   # no values lost
dl_04_sparse_binary$A_nnz
saveRDS(dl_04_sparse_binary, "data/created/combined_models/dl_04_sparse_binary.RDS")

dl_05 <- STbayes::import_user_STb(
  event_data = ed_all,
  networks = n_sparse,
  network_type = "undirected",
  ILV_tv = ILV_tv_padded,
  ILVi = c("mean_dist_to_carcass_norm")
)
stopifnot(dim(dl_05$A)[1] == 2, dl_05$N_networks == 2)
sum_dense <- sum(dl_05$A)
saveRDS(dl_05, "data/created/combined_models/dl_05.RDS")
dl_05_sparse <- sparsify_A(dl_05)
stopifnot(isTRUE(all.equal(sum(dl_05_sparse$A_val), sum_dense)))   # no values lost
dl_05_sparse$A_nnz
saveRDS(dl_05_sparse, "data/created/combined_models/dl_05_sparse.RDS")

dl_05_binary <- STbayes::import_user_STb(
  event_data = ed_all,
  networks = n_sparse_binary,
  network_type = "undirected",
  ILV_tv = ILV_tv_padded,
  ILVi = c("mean_dist_to_carcass_norm")
)
stopifnot(dim(dl_05_binary$A)[1] == 2, dl_05_binary$N_networks == 2)
sum_dense <- sum(dl_05_binary$A)
saveRDS(dl_05_binary, "data/created/combined_models/dl_05_binary.RDS")
dl_05_sparse_binary <- sparsify_A(dl_05_binary)
stopifnot(isTRUE(all.equal(sum(dl_05_sparse_binary$A_val), sum_dense)))   # no values lost
dl_05_sparse_binary$A_nnz
saveRDS(dl_05_sparse_binary, "data/created/combined_models/dl_05_sparse_binary.RDS")

#Small data list for test
keep_20 <- sample(unique(ed_all$trial), 20)
ed_20 <- ed_all |> filter(trial %in% keep_20)
miss_20 <- setdiff(unique(c(n_sparse$focal, n_sparse$other)), ed_20$id)
tr1 <- ed_20 |> distinct(trial, t_end) |> slice(1)
ed_20 <- bind_rows(ed_20,
                    tibble(id = miss_20, trial = tr1$trial, t_end = tr1$t_end, time = tr1$t_end + 0.001))
dl_small_20 <- STbayes::import_user_STb(
  event_data = ed_20 |> dplyr::filter(trial %in% keep_20),
  networks   = n_sparse |> dplyr::filter(trial %in% keep_20),
  network_type = "undirected",
  ILV_tv = ILV_tv_padded |> dplyr::filter(trial %in% keep_20),
  ILVi = c("age", "mean_dist_to_carcass_norm"),
  ILVs = "age"
)
stopifnot(dim(dl_small_20$A)[1] == 2, dl_small_20$N_networks == 2)
sum_dense <- sum(dl_small_20$A)
saveRDS(dl_small_20, "data/created/combined_models/dl_small_20.RDS")
dl_small_20_sparse <- sparsify_A(dl_small_20)
stopifnot(isTRUE(all.equal(sum(dl_small_20_sparse$A_val), sum_dense)))   # no values lost
dl_small_20_sparse$A_nnz
saveRDS(dl_small_20_sparse, "data/created/combined_models/dl_small_20_sparse.RDS")