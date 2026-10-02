library(targets)
library(STbayes)
library(posterior)
library(bayestestR)
library(igraph)
library(tidyverse)
library(patchwork)

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

event_data_all_test <- purrr::list_rbind(event_data[1:4]) %>% mutate(trial = as.character(trial)) %>% mutate(time = time/1000, t_end = t_end/1000)
# this did not throw an error for 1:2 or 1:3, but for 1:4 it did. On inspection, 
map_dbl(event_data[1:4], nrow)
# [1] 130 130 130 129. Different total number of individuals in the diffusions.
# seems like I need to add all individuals to the networks, just set to 0.
networks_long_combined_all_test <- purrr::list_rbind(networks_long_combined[1:4]) %>%
  mutate(trial = as.character(trial)) %>%
  select(-roost_together) # there seems to be a bug in how STbayes handles two networks, so let's just do one for now.
ILV_tv_all_test <- purrr::list_rbind(ILV_tv[1:4]) %>% mutate(trial = as.character(trial))


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
  est_acqTime = TRUE,
  veff_params = c("lambda_0", "s"),
  veff_type = "trial"
)


fit_test <- fit_STb(
  data_list_test, mod_test,
  chains = 4,
  parallel_chains = 4,
  iter = 250,
  refresh = 25,
  max_treedepth = 5,
  seed = 1
)

#STb_save(fit_test, output_dir = "data/saved_fits", name="fit_test")
sm <- STb_summary(fit_test, digits = 3) # for some reason, we still only have one percent_ST value, not four. I don't know what's going on with that. Another bug?
sm


# Plot PPCs (from Weibull vignette)
# Something is deeply wrong here...
# we need to store num inds per trial to refer to later
event_data <- ed_test %>%
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
  select(trial, time, cum_prop, type) %>%
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

draws_df <- as_draws_df(fit_test$draws(variables = "acquisition_time", inc_warmup = FALSE))

# pivot longer
ppc_long <- draws_df %>%
  select(starts_with("acquisition_time[")) %>%
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
# STb_trial_summary(fit_test, data_list_test)

trial_summ <- STb_trial_summary(fit_test, data_list_test)

## Forest plot: s
forest_s <- trial_summ %>%
  ggplot(aes(x = trial_label))+
  geom_pointrange(aes(y = s_median, ymax = s_hi, ymin = s_lo))+
  geom_hline(aes(yintercept = 0), linetype = 2, color = "blue")+
  theme_minimal()+
  labs(y = "s", x = "Trial")+
  coord_flip()+
  theme(text = element_text(size = 18)) # finally, here's a results forest plot!

## Forest plot: %ST
forest_pctst <- trial_summ %>%
  ggplot(aes(x = trial_label))+
  geom_pointrange(aes(y = ST_median, ymax = ST_hi, ymin = ST_lo))+
  geom_hline(aes(yintercept = 0), linetype = 2, color = "blue")+
  theme_minimal()+
  labs(y = "%ST", x = "Trial")+
  coord_flip()+
  theme(text = element_text(size = 18))
forest_s + forest_pctst # cool! These look like actual results.

