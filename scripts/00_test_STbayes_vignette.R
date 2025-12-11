# Testing stBayes

library(STbayes)
library(ggplot2)
library(tidyr)
library(dplyr)
library(posterior)

event_data <- STbayes::event_data
head(event_data)

edge_list <- STbayes::edge_list
head(edge_list)

data_list <- import_user_STb(event_data = event_data, 
                             networks = edge_list,
                             network_type = "undirected") # this provides a "sanity check" message, which is really helpful!

model_full <- generate_STb_model(data_list, gq = T, est_acqTime = T) # displays default priors
#cat(model_full) # to look at the stan code. But I'm not sure we want to do that.

#write(model_full, file="../data/stan_models/my_first_model.stan") # optionally, write the model to a file.

# "Fitting this dataset should be quick, under 10 seconds. If it’s very slow, you might want to try running this on a different computer." - well shoot, it's definitely much longer than 10s. that's really annoying. I don't really have the option to run this on a different computer right now...It seems like compiling the stan program is the part that's taking a while.
full_fit <- fit_STb(data_list,
                    model_full,
                    parallel_chains = 4,
                    chains = 4,
                    cores = 4,
                    iter = 2000,
                    refresh=1000
)

STb_summary(full_fit, digits = 3) # view summary

# The most important output are the intrinsic rate (lambda_0), and the relative strength of social transmission (s), whose interpretations are the same as the NBDA package. The relative strength of social transmission (s = s_prime / lambda_0) is generally what we’re after. %ST for network 

## Here's a resource on which values to use to estimate: https://discourse.mc-stan.org/t/bayesian-models-choosing-between-means-medians-mad-mpe/26987

## In this case, lambda_0 (asocial learning rate) is estimated as 0.001 with spread indicated either by MAD or by the confidence intervals.
## s is 4.613, and percent_ST is 0.819. Not sure if that means 81.9%, or 0.819%.

# n
# is reported as percent_ST[n]. This is a single-network model, thus percent_ST[1] is the estimated percentage of events that occurred through social transmission. The [1] refers to the “assoc” network, as we’ve only given a single network. If you fit a multi-network model, all networks will have an estimate. For a number of reasons, STbayes actually fits lambda_0 and social transmission rate (s_prime) on the log scale. The linear transformation of s_prime itself usually isn’t reported and is excluded from the output, but you could calculate it yourself from the fit.
# 
# At this point, you are just dealing with a cmdstanr fit, so if you have your own pipeline, adios! However, I have provided a few more useful functions for posterior predictive checks and model comparison.


# Cumulative diffusion curves
plot_data_obs <- event_data %>%
  filter(time > 0, time <= t_end) %>% # exclude demonstrators (time == 0) and censored (time > t_end)
  group_by(trial) %>%
  arrange(time, .by_group = TRUE) %>%
  mutate(
    cum_prop = row_number() / n(),
    type = "observed"
  ) %>%
  select(trial, time, cum_prop, type) %>%
  ungroup()

# add in 0,0 starting point
plot_data_obs <- bind_rows(
  plot_data_obs,
  plot_data_obs %>%
    distinct(trial) %>%
    mutate(time = 0, cum_prop = 0, type = "observed")
) %>%
  arrange(trial, time)

draws_df <- as_draws_df(full_fit$draws(variables = "acquisition_time", inc_warmup = FALSE))

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
#> Warning: Dropping 'draws_df' class as required metadata was removed.

# thin sample for plotting
sample_idx <- sample(c(1:max(ppc_long$draw)), 100)
ppc_long <- ppc_long %>% filter(draw %in% sample_idx)

# build cumulative curves per draw
plot_data_ppc <- ppc_long %>%
  group_by(draw, trial, time) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(draw, trial) %>%
  arrange(time) %>%
  mutate(cum_prop = cumsum(n) / data_list$Q)

# add in 0,0 starting point
plot_data_ppc <- bind_rows(
  plot_data_ppc,
  plot_data_ppc %>%
    distinct(trial, draw) %>%
    mutate(time = 0, cum_prop = 0, type = "ppc")
) %>%
  arrange(trial, time)

# plot it
ggplot() +
  geom_line(data = plot_data_ppc, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial") +
  theme_minimal()

#"This looks pretty good, the observed curve falls within the variation of the posterior draws. If the observed data fell outside of the draws, you might want to rethink the model specification, whether any influential variables have been excluded, and whether there might be complex transmission processes."