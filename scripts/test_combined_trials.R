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

which_valid <- which(map_dbl(networks_long_combined, nrow) > 0)
which_valid_wild <- which(map_dbl(networks_long_combined_wild, nrow) > 0)
event_data <- event_data[which_valid]
event_data_wild <- event_data_wild[which_valid_wild]
# Adding zero dyads to fill out networks that are missing any dyads
# after the which_valid filtering of all lists:
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
  mutate(trial = as.character(trial)) %>%
  dplyr::select(-roost_together)
networks_long_combined_all_wild <- purrr::list_rbind(networks_long_combined_wild) %>%
  mutate(trial = as.character(trial)) %>%
  dplyr::select(-roost_together)
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
    #roost_together = 0,
    flight_sri_scaled = 0
  )

missing_dyads_wild <- anti_join(
  expected_full_wild, networks_symmetric_wild,
  by = c("trial", "time", "focal", "other")
) %>%
  mutate(
    #roost_together = 0,
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
n <- networks_filled
n_wild <- networks_filled_wild %>% select(-roost_date)
rm(event_data_all)
rm(event_data_all_wild)
rm(networks_filled)
rm(networks_filled_wild)

ed_all <- bind_rows(ed, ed_wild)
n_all <- bind_rows(n, n_wild)

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
n_nz <- n_all |> filter(flight_sri_scaled > 0)

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
  mutate(flight_sri_scaled = 0)

# 3. Zero-weight rows so every ID appears in every trial
#    (needed when subsetting trials; harmless for the full import).
#    Skips dyads that already have a real edge in that slice, in either direction.
pad_ids <- n_nz |>
  bind_rows(pad_slices) |>
  group_by(trial) |> summarise(time = min(time), .groups = "drop") |>
  tidyr::crossing(focal = all_ids) |>
  mutate(other = all_ids[match(focal, all_ids) %% length(all_ids) + 1],
         flight_sri_scaled = 0) |>
  anti_join(n_nz, by = c("trial", "time", "focal", "other")) |>
  anti_join(n_nz, by = c("trial", "time", "focal" = "other", "other" = "focal"))

n_sparse <- bind_rows(n_nz, pad_slices, pad_ids)
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
sum(n_sparse$flight_sri_scaled) == sum(n_all$flight_sri_scaled)  # TRUE (no real edges lost)

saveRDS(n_sparse, "n_sparse.rds")

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

# Make the data list!
data_list <- STbayes::import_user_STb(
  event_data = ed_all,
  networks = n_sparse, # instead of n_all
  network_type = "undirected",
  ILV_tv = ILV_tv_padded,
  ILVi = c("age", "mean_dist_to_carcass_norm"),   # ILVs acting on the intrinsic rate
  ILVs = c("age")   # ILVs acting on the social rate
)

dim(data_list$A)   # expect 1 142 78 161 161
print(object.size(data_list), units = "GB")
saveRDS(data_list, "data/created/data_list.rds")
data_list <- readRDS("data/created/data_list.rds")

# Small data list for test
keep <- sample(unique(ed_all$trial), 20)
ed_sub <- ed_all |> filter(trial %in% keep)
miss <- setdiff(unique(c(n_sparse$focal, n_sparse$other)), ed_sub$id)
tr1 <- ed_sub |> distinct(trial, t_end) |> slice(1)
ed_sub <- bind_rows(ed_sub,
                    tibble(id = miss, trial = tr1$trial, t_end = tr1$t_end, time = tr1$t_end + 0.001))
dl_small <- STbayes::import_user_STb(
  event_data = ed_sub |> dplyr::filter(trial %in% keep),
  networks   = n_sparse |> dplyr::filter(trial %in% keep),
  network_type = "undirected",
  ILV_tv = ILV_tv_padded |> dplyr::filter(trial %in% keep),
  ILVi = c("age", "mean_dist_to_carcass_norm"),
  ILVs = "age"
)
dl_small$T_max
dim(dl_small$ILV_age)[2]   # should equal T_max
dim(dl_small$A)[3]         # should equal T_max

saveRDS(dl_small, "data/created/dl_small.rds")
dl_small <- readRDS("data/created/dl_small.rds")

# mod <- generate_STb_model(
#   data_list,
#   est_acqTime = TRUE,
#   veff_params = c("lambda_0", "s"),
#   veff_type = "trial"
# )

mod_small <- generate_STb_model(
  dl_small,
  est_acqTime = TRUE,
  veff_params = c("lambda_0", "s"),
  veff_type = "trial"
)

# Sys.time()
# fit <- fit_STb(
#   data_list, mod,
#   chains = 1,
#   #parallel_chains = 4,
#   iter = 50,
#   refresh = 5,
#   max_treedepth = 5,
#   seed = 1
# )
# Sys.time()

Sys.time()
fit_small <- fit_STb(
  dl_small, mod_small,
  chains = 1,
  #parallel_chains = 4,
  iter = 50,
  refresh = 5,
  max_treedepth = 5,
  seed = 1
)
Sys.time()

#STb_save(fit, output_dir = "data/saved_fits", name="fit")
sm <- STb_summary(fit, digits = 3) # for some reason, we still only have one percent_ST value, not four. I don't know what's going on with that. Another bug?
sm


# Plot PPCs (from Weibull vignette)
# we need to store num inds per trial to refer to later
event_data <- ed_all %>%
  mutate(trial = as.numeric(factor(trial))) %>%
  group_by(trial) %>%
  mutate(n_trial = n())

# create cumulative proportion dataframe
plot_data_obs <- event_data %>%
  filter(time <= t_end) %>% # exclude censored (time > t_end)
  group_by(trial) %>%
  arrange(time, .by_group = TRUE) %>%
  mutate(
    cum_prop = row_number() / n_trial, # denominator needs to be the number of individuals per trial
    type = "observed"
  ) %>%
  dplyr::select(trial, time, cum_prop, type) %>%
  ungroup()

# add in 0,0 starting point for diffusions w/o demonstrators
starting_points <- plot_data_obs %>%
  dplyr::distinct(trial) %>%
  anti_join(
    plot_data_obs %>% filter(time == 0) %>% distinct(trial),
    by = "trial"
  ) %>%
  mutate(time = 0, cum_prop = 0, type = "observed")
plot_data_obs <- bind_rows(plot_data_obs, starting_points) %>%
  arrange(trial, time)

draws_df <- as_draws_df(fit$draws(variables = "acquisition_time", inc_warmup = FALSE))

# pivot longer
ppc_long <- draws_df %>%
  dplyr::select(starts_with("acquisition_time[")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("trial", "ind"),
    names_pattern = "acquisition_time\\[(\\d+),(\\d+)\\]",
    values_to = "time"
  ) %>%
  mutate(
    trial = as.integer(trial),
    ind = as.integer(ind),
    draw = rep(1:(nrow(draws_df)), 
               each = length(unique(.$trial)) * length(unique(.$ind)))
  )

# thin sample for plotting
sample_idx <- sample(c(1:max(ppc_long$draw)), 100)
ppc_long <- ppc_long %>% filter(draw %in% sample_idx)

# drop individuals not present in a given trial
ppc_long <- ppc_long %>%
  filter(!is.na(time))

# same as before, we need a way to reference the number of individuals in each trial
ppc_long <- ppc_long %>%
  group_by(draw, trial) %>%
  mutate(n_trial = n())

# we also need to remove individuals predicted as censored, which
# have value of -1 in predicted data
ppc_long <- ppc_long %>%
  filter(time > -1)

# build cumulative curves per draw
plot_data_ppc <- ppc_long %>%
  group_by(draw, trial, time) %>%
  summarise(n = n(), n_trial = first(n_trial), .groups = "drop") %>%
  group_by(draw, trial) %>%
  arrange(time) %>%
  mutate(cum_prop = cumsum(n) / n_trial)

# add in 0,0 starting point when no demos, similar to above
starting_points_ppc <- plot_data_ppc %>%
  distinct(trial, draw) %>%
  anti_join(
    plot_data_ppc %>%
      filter(time == 0) %>%
      distinct(trial, draw),
    by = c("trial", "draw")
  ) %>%
  mutate(time = 0, cum_prop = 0, type = "ppc")

plot_data_ppc <- bind_rows(plot_data_ppc, starting_points_ppc) %>%
  arrange(trial, draw, time)

# plot it w diff colored lines for different trials.
ggplot() +
  geom_line(data = plot_data_ppc, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial), color = as.factor(trial)), alpha = .1) +
  geom_line(data = plot_data_obs, aes(x = time, y = cum_prop, 
                                      color = as.factor(trial)), linewidth = 1) +
  scale_color_viridis_d() + 
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial") +
  theme_minimal() # okay, this doesn't look like it fits super well but at least the plot was created correctly!!

# Claude led me through a step by step process to derive per-trial s and %ST values from the model output. I'm saving the entire convo to refer back to, but here's the final function:
# STb_trial_summary()
#
# Per-trial estimates of s (relative strength of social transmission) and %ST
# (share of learning events attributable to social transmission), with 95%
# intervals, from an STbayes fit with trial-level varying effects.
#
# Based on the generated Stan code for:
#   - one network, standard transmission, no ILVs
#   - veff_params = c("lambda_0", "s"), veff_type = "trial"
#     (so the fit contains vectors s_prime[k] and lambda_0[k])
#
# Logic (mirrors the Stan generated quantities block):
#   s_k   = s_prime[k] / lambda_0[k]                  (per draw)
#   Tn    = A[net, trial, step, id, ] . Z[trial, step, ]  at each learner's own
#           learning step (seeds are skipped, as in Stan)
#   %ST_k = mean over trial k's learners of  s_k*Tn / (1 + s_k*Tn)
#           (learners with Tn = 0 count in the denominator and contribute 0)
#
# Checks (warnings, not errors):
#   1. number of learners found == count_ST from the fit
#   2. learner-weighted average of per-trial %ST reproduces Stan's pooled
#      percent_ST[1] draw by draw. If this fails, the assumptions above do
#      not hold for your model (e.g. ILVs, multiple networks), so don't trust
#      the table.
#
# Arguments:
#   fit          CmdStanMCMC object from fit_STb()
#   data_list    the data list used to fit the model
#   prob         interval width (default 0.95)
#   CI_method    "HPDI" (default, as in STb_summary) or "PI" (equal-tailed)
#   digits       rounding for the returned table
#   check        run the consistency checks
#   return_draws if TRUE, return list(table, s_draws, pst_draws)

STb_trial_summary <- function(fit, data_list, prob = 0.95,
                              CI_method = c("HPDI", "PI"),
                              digits = 3, check = TRUE,
                              return_draws = FALSE) {
  CI_method <- match.arg(CI_method)
  if (!inherits(fit, "CmdStanMCMC")) stop("fit must be a CmdStanMCMC object.")
  d <- data_list
  K <- d$K
  
  if (d$N_networks != 1) {
    stop("This function handles single-network models only (N_networks = ", d$N_networks, ").")
  }
  needed <- c("s_prime", "lambda_0", "percent_ST", "count_ST")
  missing_vars <- setdiff(needed, fit$metadata()$stan_variables)
  if (length(missing_vars) > 0) {
    stop("Fit is missing: ", paste(missing_vars, collapse = ", "))
  }
  
  # ---- 1. per-trial s, draw by draw ----
  dr <- posterior::as_draws_df(fit$draws(needed))
  sp_names <- paste0("s_prime[", seq_len(K), "]")
  l0_names <- paste0("lambda_0[", seq_len(K), "]")
  if (!all(c(sp_names, l0_names) %in% names(dr))) {
    stop("Could not find s_prime[1..", K, "] and lambda_0[1..", K, "] in the draws. ",
         "Was the model fit with trial-level varying effects on lambda_0 and s?")
  }
  s_draws <- sapply(seq_len(K), function(k) dr[[sp_names[k]]] / dr[[l0_names[k]]])
  S <- nrow(s_draws)
  
  # ---- 2. network exposure Tn for each learner at its learning step ----
  Tn <- lapply(seq_len(K), function(k) {
    vapply(seq_len(d$N[k]), function(n) {
      id <- d$ind_id[k, n]
      lt <- d$t[k, id]
      if (lt <= 0) return(NA_real_) # seeds are skipped in Stan
      sum(d$A[1, k, lt, id, ] * d$Z[k, lt, ])
    }, numeric(1))
  })
  n_learners <- vapply(Tn, function(x) sum(!is.na(x)), numeric(1))
  n_exposed <- vapply(Tn, function(x) sum(x > 0, na.rm = TRUE), numeric(1))
  
  # ---- 3. per-trial %ST, draw by draw ----
  pst <- vapply(seq_len(K), function(k) {
    tn <- Tn[[k]][!is.na(Tn[[k]])]
    if (length(tn) == 0) return(rep(NA_real_, S))
    x <- outer(s_draws[, k], tn) # S x n_learners
    rowMeans(x / (1 + x))
  }, numeric(S))
  
  # ---- 4. checks ----
  if (check) {
    cnt <- unique(dr[["count_ST"]])
    if (length(cnt) != 1 || cnt != sum(n_learners)) {
      warning("Learner count (", sum(n_learners), ") != count_ST in fit (",
              paste(cnt, collapse = ","), "). Indexing may differ from the Stan model.")
    }
    ok <- n_learners > 0
    pooled <- as.vector(pst[, ok, drop = FALSE] %*% n_learners[ok]) / sum(n_learners)
    chk <- all.equal(pooled, dr[["percent_ST[1]"]], tolerance = 1e-6)
    if (!isTRUE(chk)) {
      warning("Pooled per-trial %ST does not match percent_ST[1] from Stan: ",
              paste(chk, collapse = "; "),
              ". Per-trial values are NOT validated.")
    }
  }
  
  # ---- 5. summarise ----
  summ <- function(x) {
    if (all(is.na(x))) return(c(NA_real_, NA_real_, NA_real_))
    ci <- if (CI_method == "HPDI") {
      h <- bayestestR::hdi(x, ci = prob)
      c(h$CI_low, h$CI_high)
    } else {
      stats::quantile(x, c((1 - prob) / 2, 1 - (1 - prob) / 2), names = FALSE)
    }
    c(stats::median(x), ci)
  }
  s_tab <- t(apply(s_draws, 2, summ))
  st_tab <- t(apply(pst, 2, summ))
  
  out <- data.frame(
    trial = seq_len(K),
    n_learners = n_learners,
    n_exposed = n_exposed,
    s_median = s_tab[, 1], s_lo = s_tab[, 2], s_hi = s_tab[, 3],
    ST_median = st_tab[, 1], ST_lo = st_tab[, 2], ST_hi = st_tab[, 3]
  )
  
  # original trial labels, if available
  ev <- attr(d, "df_event_data")
  if (!is.null(ev) && all(c("trial", "trial_numeric") %in% names(ev))) {
    lab <- unique(as.data.frame(ev)[, c("trial_numeric", "trial")])
    out$trial_label <- lab$trial[match(out$trial, lab$trial_numeric)]
    out <- out[, c("trial", "trial_label", setdiff(names(out), c("trial", "trial_label")))]
  }
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], round, digits = digits)
  rownames(out) <- NULL
  
  if (return_draws) {
    return(list(table = out, s_draws = s_draws, pst_draws = pst))
  }
  out
}

# Example:
# source("STb_trial_summary.R")
#STb_trial_summary(fit, data_list)

trial_summ <- STb_trial_summary(fit, data_list)

## Forest plot: s
forest_s <- trial_summ %>%
  # filter(s_median < 100) %>%
  ggplot(aes(x = trial_label))+
  geom_pointrange(aes(y = s_median, ymax = s_hi, ymin = s_lo))+
  geom_hline(aes(yintercept = 0), linetype = 2, color = "blue")+
  theme_minimal()+
  labs(y = "s", x = "Trial")+
  coord_flip()+
  theme(text = element_text(size = 18)) # finally, here's a results forest plot!
forest_s # this really sucks--s is almost never above 0. is this true? am i interpreting the results correctly?

## Forest plot: %ST
forest_pctst <- trial_summ %>%
  ggplot(aes(x = trial_label))+
  geom_pointrange(aes(y = ST_median, ymax = ST_hi, ymin = ST_lo))+
  geom_hline(aes(yintercept = 0), linetype = 2, color = "blue")+
  theme_minimal()+
  labs(y = "%ST", x = "Trial")+
  coord_flip()+
  theme(text = element_text(size = 18))
forest_pctst
forest_s + forest_pctst # cool! These look like actual results.

