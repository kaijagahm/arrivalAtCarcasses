# Examine results of combined models
library(tidyverse)
library(targets)
library(STbayes)
library(posterior)

fit_001 <- readRDS("data/saved_fits/hoffman2/fit_001.rds") # tiny sample, both networks, 50 iterations
fit_002 <- readRDS("data/saved_fits/hoffman2/fit_002.rds") 
fit_003 <- readRDS("data/saved_fits/hoffman2/fit_003.rds") # both networks, 1000 iterations
fit_004 <- readRDS("data/saved_fits/hoffman2/fit_004.rds") # both networks, 1000 iterations
fit_005 <- readRDS("data/saved_fits/hoffman2/fit_005.rds") # both networks, 1000 iterations
fit_006 <- readRDS("data/saved_fits/hoffman2/fit_006.rds") 
fit_007 <- readRDS("data/saved_fits/hoffman2/fit_007.rds") 
fit_008 <- readRDS("data/saved_fits/hoffman2/fit_008.rds") 

fit_001_asocial <- readRDS("data/saved_fits/hoffman2/fit_001_asocial.rds") # tiny sample, both networks, 50 iterations
fit_002_asocial <- readRDS("data/saved_fits/hoffman2/fit_002_asocial.rds") 
fit_003_asocial <- readRDS("data/saved_fits/hoffman2/fit_003_asocial.rds") # both networks, 1000 iterations
fit_004_asocial <- readRDS("data/saved_fits/hoffman2/fit_004_asocial.rds") # both networks, 1000 iterations
fit_005_asocial <- readRDS("data/saved_fits/hoffman2/fit_005_asocial.rds") # both networks, 1000 iterations
fit_006_asocial <- readRDS("data/saved_fits/hoffman2/fit_006_asocial.rds") 
fit_007_asocial <- readRDS("data/saved_fits/hoffman2/fit_007_asocial.rds") 
fit_008_asocial <- readRDS("data/saved_fits/hoffman2/fit_008_asocial.rds") 

get_plot_data <- function(ed, fit, samplesize = 100){
  # we need to store num inds per trial to refer to later
  event_data <- ed %>%
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
  sample_idx <- sample(c(1:max(ppc_long$draw)), samplesize)
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
  plot_data <- list("ppc" = plot_data_ppc, "obs" = plot_data_obs)
  
  return(plot_data)
}

ed_all <- readRDS("data/created/combined_models/ed_all.RDS")

plot_data_001 <- get_plot_data(ed_all, fit_001, samplesize = 10) # both
plot_data_002 <- get_plot_data(ed_all, fit_002, samplesize = 10)
plot_data_003 <- get_plot_data(ed_all, fit_003)
plot_data_004 <- get_plot_data(ed_all, fit_004) # flight only
plot_data_005 <- get_plot_data(ed_all, fit_005) # roost only
plot_data_005 <- get_plot_data(ed_all, fit_005)
plot_data_006 <- get_plot_data(ed_all, fit_006)
plot_data_007 <- get_plot_data(ed_all, fit_007)
plot_data_007 <- get_plot_data(ed_all, fit_007, samplesize = 10)

ppc <- map(list(plot_data_001, plot_data_002, plot_data_003, plot_data_004, plot_data_005, plot_data_006, plot_data_007, plot_data_008), "ppc")
obs <- map(list(plot_data_001, plot_data_002, plot_data_003, plot_data_004, plot_data_005, plot_data_006, plot_data_007, plot_data_008), "obs")

plot_ppcs <- function(ppc, obs){
  p <- ggplot() +
    geom_line(data = ppc, 
              aes(x = time, y = cum_prop, 
                  group = interaction(draw, trial), color = as.factor(trial)), alpha = .1) +
    geom_line(data = obs, aes(x = time, y = cum_prop, 
                                        color = as.factor(trial)), linewidth = 1) +
    scale_color_viridis_d() + 
    labs(x = "Time", y = "Cumulative proportion informed", color = "Trial") +
    theme_minimal()+ theme(legend.position = "none")
  return(p)
}

plots <- map2(ppc, obs, ~plot_ppcs(.x, .y))


# Compare social to asocial -----------------------------------------------
get_loo_comparison <- function(fit, fit_asocial){
  loo_output <- STb_compare(fit, fit_asocial, method = "loo-psis")
  comparison_df <- as.data.frame(loo_output$comparison)
  comparison_df$model <- rownames(comparison_df)
  
  p <- ggplot(comparison_df, aes(x = reorder(model, elpd_diff), y = elpd_diff)) +
    geom_point(size = 3) + #elpd_diff
    geom_errorbar(aes(ymin = elpd_diff - 1.96*se_diff, 
                      ymax = elpd_diff + 1.96*se_diff), width = 0.2) + #1.96*SE of elpd diff (95% interval)
    coord_flip() +
    labs(x = "Model", y = "ELPD Difference", title = "Model Comparison") +
    theme_minimal()
  return(p)
}

loo_comparison_001 <- get_loo_comparison(fit_001, fit_001_asocial)
loo_comparison_002 <- get_loo_comparison(fit_002, fit_002_asocial)
loo_comparison_003 <- get_loo_comparison(fit_003, fit_003_asocial)
loo_comparison_004 <- get_loo_comparison(fit_004, fit_004_asocial)
loo_comparison_005 <- get_loo_comparison(fit_005, fit_005_asocial)
loo_comparison_006 <- get_loo_comparison(fit_006, fit_006_asocial)
loo_comparison_007 <- get_loo_comparison(fit_007, fit_007_asocial)
loo_comparison_008 <- get_loo_comparison(fit_008, fit_008_asocial)


# STb_trial_summary()
#
# Per-trial s (relative strength of social transmission, per network) and %ST
# (share of learning events attributable to social transmission, per network
# and total), with intervals, from an STbayes fit with trial-level varying
# effects. Works for any N_networks, with ILVs of type i and s, and with
# sparse (A_val/A_col/A_row_ptr) or dense (A) network data.
#
# Mirrors the generated quantities block:
#   s_{j,k}  = s_prime[j,k] / lambda_0[k]      (baseline, i.e. all ILVs = 0)
#   Tn_j     = A_j[k, step, id, ] . Z[k, step, ] at each learner's own step
#   %ST_j,k  = mean over trial k's learners of
#              s_j*Tn_j*exp(lin_s) / (exp(lin_i) + sum_j s_j*Tn_j*exp(lin_s))
# Seeds (t <= 0) are skipped, as in Stan. ST_total is the sum over networks.
#
# ILV type "m" (if you ever use it) is assumed to enter BOTH the asocial and
# social terms; this is unverified, but the pooled check will flag it if wrong.
#
# Checks (warnings, not errors):
#   1. learners found == count_ST
#   2. learner-weighted pooled per-trial %ST reproduces percent_ST[j] draw by draw
#
# Arguments: fit (CmdStanMCMC), data_list (as fitted), prob, CI_method
#   ("HPDI" or "PI"), digits, check, return_draws (list of table, s_draws,
#   pst_draws; draw arrays are [draw, network, trial]).

STb_trial_summary <- function(fit, data_list, prob = 0.95,
                              CI_method = c("HPDI", "PI"),
                              digits = 3, check = TRUE, return_draws = FALSE) {
  CI_method <- match.arg(CI_method)
  if (!inherits(fit, "CmdStanMCMC")) stop("fit must be a CmdStanMCMC object.")
  d <- data_list
  K <- d$K; P <- d$P; T_max <- d$T_max; nnet <- d$N_networks
  sparse <- !is.null(d$A_row_ptr)
  if (!sparse && is.null(d$A)) stop("data_list has neither A_row_ptr (sparse) nor A (dense).")
  
  vars <- fit$metadata()$stan_variables
  ilv_vars <- grep("^beta_ILV[ism]_", vars, value = TRUE)
  need <- c("s_prime", "lambda_0", "percent_ST", "count_ST")
  if (length(setdiff(need, vars))) {
    stop("Fit is missing: ", paste(setdiff(need, vars), collapse = ", "))
  }
  m <- posterior::as_draws_matrix(fit$draws(c(need, ilv_vars)))
  dm <- matrix(as.numeric(m), nrow(m), ncol(m), dimnames = list(NULL, colnames(m)))
  S <- nrow(dm)
  
  # ---- parameters ----
  nm <- outer(seq_len(nnet), seq_len(K), function(j, k) sprintf("s_prime[%d,%d]", j, k))
  sp <- array(dm[, as.vector(nm)], c(S, nnet, K))             # [draw, network, trial]
  L  <- matrix(dm[, sprintf("lambda_0[%d]", seq_len(K))], S, K)
  s_draws <- sp
  for (j in seq_len(nnet)) s_draws[, j, ] <- matrix(sp[, j, ], S, K) / L
  
  # ---- ILV coefficient draws and data ----
  ilv <- lapply(ilv_vars, function(v) {
    type <- sub("^beta_ILV([ism])_.*$", "\\1", v)
    arr <- d[[paste0("ILV_", sub("^beta_ILV[ism]_", "", v))]]
    if (is.null(arr)) stop("No data entry for ", v, " (expected ILV_<name>).")
    four <- length(dim(arr)) == 4
    nc <- if (four) dim(arr)[4] else 1
    cols <- if (v %in% colnames(dm)) v else sprintf("%s[%d]", v, seq_len(nc))
    list(type = type, arr = arr, four = four, B = dm[, cols, drop = FALSE])
  })
  lin <- function(k, lt, id) {
    li <- rep(0, S); ls <- rep(0, S)
    for (tm in ilv) {
      x <- if (tm$four) tm$arr[k, lt, id, ] else tm$arr[k, lt, id]
      eta <- drop(tm$B %*% x)
      if (tm$type != "s") li <- li + eta   # i, m -> asocial term
      if (tm$type != "i") ls <- ls + eta   # s, m -> social term
    }
    list(i = li, s = ls)
  }
  
  # ---- network exposure Tn for one learner / network (mirrors A_dot) ----
  exposure <- function(j, k, lt, id) {
    if (!sparse) return(sum(d$A[j, k, lt, id, ] * d$Z[k, lt, ]))
    r <- (((j - 1) * K + (k - 1)) * T_max + (lt - 1)) * P + id
    lo <- d$A_row_ptr[r]; hi <- d$A_row_ptr[r + 1] - 1
    if (hi < lo) return(0)
    idx <- lo:hi
    sum(d$A_val[idx] * d$Z[k, lt, d$A_col[idx]])
  }
  
  # ---- loop over learners ----
  n_learn <- numeric(K)
  n_exp <- matrix(0, K, nnet)
  pst <- array(0, c(S, nnet, K))
  for (k in seq_len(K)) {
    for (n in seq_len(d$N[k])) {
      id <- d$ind_id[k, n]
      lt <- d$t[k, id]
      if (lt <= 0) next                                       # seed
      Tn <- vapply(seq_len(nnet), function(j) exposure(j, k, lt, id), numeric(1))
      l <- lin(k, lt, id)
      soc <- sweep(matrix(sp[, , k], S, nnet), 2, Tn, `*`) * exp(l$s)
      den <- L[, k] * exp(l$i) + rowSums(soc)                 # lambda / D
      pst[, , k] <- pst[, , k] + soc / den
      n_learn[k] <- n_learn[k] + 1
      n_exp[k, ] <- n_exp[k, ] + (Tn > 0)
    }
    pst[, , k] <- if (n_learn[k] > 0) pst[, , k] / n_learn[k] else NA_real_
  }
  
  # ---- checks ----
  if (check) {
    ok_all <- TRUE
    cnt <- unique(dm[, "count_ST"])
    if (length(cnt) != 1 || cnt != sum(n_learn)) {
      ok_all <- FALSE
      warning("Learner count (", sum(n_learn), ") != count_ST in fit (",
              paste(cnt, collapse = ","), ").")
    }
    ok <- n_learn > 0
    for (j in seq_len(nnet)) {
      pooled <- as.vector(matrix(pst[, j, ok], S) %*% n_learn[ok]) / sum(n_learn)
      chk <- all.equal(pooled, as.vector(dm[, sprintf("percent_ST[%d]", j)]),
                       tolerance = 1e-6)
      if (!isTRUE(chk)) {
        ok_all <- FALSE
        warning("Pooled per-trial %ST for network ", j, " does not match percent_ST[",
                j, "] from Stan: ", paste(chk, collapse = "; "),
                ". Per-trial values are NOT validated.")
      }
    }
    if (ok_all) message("Checks passed: learner count and pooled %ST match Stan.")
  }
  
  # ---- summarise ----
  summ <- function(x) {
    if (all(is.na(x))) return(rep(NA_real_, 3))
    ci <- if (CI_method == "HPDI") {
      h <- bayestestR::hdi(x, ci = prob); c(h$CI_low, h$CI_high)
    } else stats::quantile(x, c((1 - prob) / 2, 1 - (1 - prob) / 2), names = FALSE)
    c(stats::median(x), ci)
  }
  tab <- function(get) t(vapply(seq_len(K), get, numeric(3)))
  out <- data.frame(trial = seq_len(K), n_learners = n_learn)
  for (j in seq_len(nnet)) {
    out[[paste0("n_exposed_net", j)]] <- n_exp[, j]
    out[paste0("s_net", j, c("_median", "_lo", "_hi"))] <-
      as.data.frame(tab(function(k) summ(s_draws[, j, k])))
  }
  for (j in seq_len(nnet)) {
    out[paste0("ST_net", j, c("_median", "_lo", "_hi"))] <-
      as.data.frame(tab(function(k) summ(pst[, j, k])))
  }
  if (nnet > 1) {
    out[paste0("ST_total", c("_median", "_lo", "_hi"))] <-
      as.data.frame(tab(function(k) summ(rowSums(matrix(pst[, , k], S, nnet)))))
  }
  
  ev <- attr(d, "df_event_data")
  if (!is.null(ev) && all(c("trial", "trial_numeric") %in% names(ev))) {
    lab <- unique(as.data.frame(ev)[, c("trial_numeric", "trial")])
    out$trial_label <- lab$trial[match(out$trial, lab$trial_numeric)]
    out <- out[, c("trial", "trial_label", setdiff(names(out), c("trial", "trial_label")))]
  }
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], round, digits = digits)
  rownames(out) <- NULL
  
  if (return_draws) return(list(table = out, s_draws = s_draws, pst_draws = pst))
  out
}

# Usage: tab <- STb_trial_summary(fit, data_list). If the checks pass you will see “Checks passed”.
# 
# Notes:
#   
#   Interpretation of s with ILVs. s_net* is the baseline ratio s_prime / lambda_0, with all ILVs at zero. The ILV effects on the social and asocial terms are included in %ST, as in Stan, but not in s. If you want an ILV-adjusted s, we’d need to decide what reference individual to use.
# Possible N_veff mismatch. Your data_list shows N_veff = 1, but the Stan code indexes v_trial[, 3] for lambda_0 and v_trial[, 1:2] for the two s_prime vectors. That requires N_veff = 3, and Stan would error otherwise. The list you fitted with probably had 3, and what you printed may be a stale copy. It doesn’t affect this function, which reads only draws, but check it’s the list you intended.
# ILV data. ILV_age is [K, T_max, P, 2] and ILV_mean_dist_to_carcass_norm is [K, T_max, P]. The function handles both shapes and matches them to the beta_ILV* names automatically.
# Speed. It loops over every learner (a few thousand) with vectorised draw math, so it should take seconds to a minute at 4,000 draws.

# XXX start here 2026-11-03
# Example:
# source("STb_trial_summary.R")
data_list <- readRDS("data/created/data_list.RDS")

trial_summ <- STb_trial_summary(fit_hoffman2, data_list)

# ## Forest plot: s
# forest_s <- trial_summ %>%
#   filter(s_hi < 500) %>%
#   ggplot(aes(x = trial_label))+
#   geom_pointrange(aes(y = s_median, ymax = s_hi, ymin = s_lo))+
#   geom_hline(aes(yintercept = 0), linetype = 2, color = "blue")+
#   theme_minimal()+
#   labs(y = "s", x = "Trial")+
#   coord_flip()+
#   theme(text = element_text(size = 18)) # finally, here's a results forest plot!
# forest_s # this really sucks--s is almost never above 0. is this true? am i interpreting the results correctly?

## Forest plot: %ST
forest_pctst <- trial_summ %>%
  mutate(sig = ifelse(ST_lo > 0, T, F)) %>%
  ggplot(aes(x = trial_label))+
  geom_pointrange(aes(y = ST_median, ymax = ST_hi, ymin = ST_lo, color = sig))+
  geom_hline(aes(yintercept = 0), linetype = 2, color = "blue")+
  theme_minimal()+
  labs(y = "%ST", x = "Trial")+
  coord_flip()+
  theme(text = element_text(size = 18))+
  scale_color_manual(values = c("gray", "blue"))
forest_pctst
forest_s + forest_pctst # cool! These look like actual results.

# Okay! We detect social transmission in about half of the cases.

# Need to be able to fit the models for roosts as well, but this is something at least.