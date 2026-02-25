# Testing stBayes with some sample data, so we can play around with what would happen with different models

library(STbayes)
library(ggplot2)
library(tidyverse)
library(posterior)
library(targets)
lapply(list.files("R", full.names = TRUE), source) 

event_data_1 <- data.frame(id = c("H", "B", "G", "C", "A", "F", "D", "E", "I"),
                           trial = 1,
                           time = c(0, 1, 14, 15, 18, 24, 26, 30, 30),
                           t_end = 29) # two individuals, E and I, never learned the thing. Individual H started out knowing it.

event_data_2 <- data.frame(id = c("F", "C", "B", "D", "A", "H", "G", "E", "I"),
                           trial = 2,
                           time = c(0, 1, 14, 15, 18, 24, 26, 30, 30),
                           t_end = 29)

edge_list_1 <- data.frame(trial = 1,
                        focal = c("A", "A", "A",
                                  "B", "B", "B",
                                  "C", "C", "C", "C",
                                  "D", "D", "D", "D", "D",
                                  "E", "E",
                                  "F", "F", "F", "F",
                                  "G", "G", "G", "G",
                                  "H", "H", "H", "H",
                                  "I"),
                        other = c("B", "C", "F",
                                  "A", "G", "H",
                                  "A", "D", "F", "G",
                                  "C", "E", "F", "G", "H",
                                  "D", "F",
                                  "A", "C", "D", "E",
                                  "B", "C", "D", "H",
                                  "B", "D", "G", "I",
                                  "H"),
                        weight = c(1, 3, 1,
                                   1, 3, 3,
                                   3, 1, 1, 2,
                                   1, 0.5, 1, 1, 0.5,
                                   0.5, 1, 
                                   1, 1, 1, 1,
                                   3, 2, 1, 1,
                                   3, 0.5, 1, 2,
                                   2))

edge_list_2 <- mutate(edge_list_1, trial = 2)

data_list_1 <- import_user_STb(event_data = event_data_1, 
                               networks = edge_list_1,
                               network_type = "undirected") # this provides really helpful confirmatory checks
data_list_2 <- import_user_STb(event_data = event_data_2, 
                               networks = edge_list_2,
                               network_type = "undirected")

model_1_static <- generate_STb_model(data_list_1, gq = T, est_acqTime = T)
model_2_static <- generate_STb_model(data_list_2, gq = T, est_acqTime = T)

model_1_static_fit <- fit_STb(data_list_1, model_1_static, parallel_chains = 4, chains = 4, cores = 4, iter = 1000, refresh = 100)
model_2_static_fit <- fit_STb(data_list_2, model_2_static, parallel_chains = 4, chains = 4, cores = 4, iter = 1000, refresh = 100)

STb_save(model_1_static_fit, output_dir = "data/cmdstan_saves", name="model_1_static_fit")
STb_save(model_2_static_fit, output_dir = "data/cmdstan_saves", name="model_2_static_fit")

model_1_static_fit <- readRDS('data/cmdstan_saves/model_1_static_fit.rds') 
model_2_static_fit <- readRDS('data/cmdstan_saves/model_2_static_fit.rds') 

# Dynamic
edge_list_dyn_1 <- purrr::list_rbind(list(data.frame(focal = c("B", "C", "D", "F", "G", "H", "H", "I"),
                                   other = c("H", "F", "G", "C", "D", "B", "I", "H"),
                                   weight = c(3, 1, 1, 1, 1, 3, 2, 2)),
                        data.frame(focal = c("B", "B", "D", "E", "G", "H"),
                                   other = c("G", "H", "E", "D", "B", "B"),
                                   weight = c(2, 3, 0.5, 0.5, 2, 3)),
                        data.frame(focal = c("B", "C", "D", "D", "E", "E", "F", "G", "G", "H"),
                                   other = c("G", "G", "E", "H", "D", "F", "E", "C", "B", "D"),
                                   weight = c(1, 1, 0.5, 0.5, 0.5, 1, 1, 1, 1, 0.5)),
                        data.frame(focal = c("A", "A", "C", "F"),
                                   other = c("C", "F", "A", "A"),
                                   weight = c(3, 1, 3, 1)),
                        data.frame(focal = c("A", "C", "D", "F", "G", "H"),
                                   other = c("F", "G", "H", "A", "C", "D"),
                                   weight = c(1, 1, 0.5, 1, 1, 0.5)),
                        data.frame(focal = c("A", "B", "C", "D", "D", "F"),
                                   other = c("B", "A", "D", "C", "F", "D"),
                                   weight = c(1, 1, 1, 1, 1, 1))), 
                        names_to = "time") %>%
  mutate(trial = 1)

edge_list_dyn_2 <- edge_list_dyn_1 %>% mutate(trial = 2)

data_list_dyn_1 <- import_user_STb(event_data = event_data_1, 
                               networks = edge_list_dyn_1,
                               network_type = "undirected") # this provides really helpful confirmatory checks
data_list_dyn_2 <- import_user_STb(event_data = event_data_2, 
                               networks = edge_list_dyn_2,
                               network_type = "undirected")

model_1_dynamic <- generate_STb_model(data_list_dyn_1, gq = T, est_acqTime = T)
model_2_dynamic <- generate_STb_model(data_list_dyn_2, gq = T, est_acqTime = T)

model_1_dynamic_fit <- fit_STb(data_list_dyn_1, model_1_dynamic, parallel_chains = 4, chains = 4, cores = 4, iter = 1000, refresh = 100)
model_2_dynamic_fit <- fit_STb(data_list_dyn_2, model_2_dynamic, parallel_chains = 4, chains = 4, cores = 4, iter = 1000, refresh = 100)

STb_save(model_1_dynamic_fit, output_dir = "data/cmdstan_saves", name="model_1_dynamic_fit")
STb_save(model_2_dynamic_fit, output_dir = "data/cmdstan_saves", name="model_2_dynamic_fit")

model_1_dynamic_fit <- readRDS('data/cmdstan_saves/model_1_dynamic_fit.rds') 
model_2_dynamic_fit <- readRDS('data/cmdstan_saves/model_2_dynamic_fit.rds') 

# Model summaries:
#"The most important output are the intrinsic rate (lambda_0), and the relative strength of social transmission (s), whose interpretations are the same as the NBDA package. The relative strength of social transmission (s = s_prime / lambda_0) is generally what we’re after. %ST for network n is reported as percent_ST[n]. This is a single-network model, thus percent_ST[1] is the estimated percentage of events that occurred through social transmission. The [1] refers to the “assoc” network, as we’ve only given a single network. If you fit a multi-network model, all networks will have an estimate. For a number of reasons, STbayes actually fits lambda_0 and social transmission rate (s_prime) on the log scale. The linear transformation of s_prime itself usually isn’t reported and is excluded from the output, but you could calculate it yourself from the fit."

STb_summary(model_1_static_fit, digits = 3) # expectation model 1: detect social transmission (because I hand-picked the order of diffusion to follow the network almost exactly)
# S value is quite high (7.6), and percent_ST is 0.95. I think maybe this is supposed to be proportion ST? Like 95%? They should correct that.
# Lower bound of the confidence interval is still decently above zero (0.007). This is consistent with what I expected.

STb_summary(model_1_dynamic_fit, digits = 3) # expectation model 1: even stronger evidence for social transmission?
# And indeed that's what we find! Oh wow. 98.3%, with confidence intervals farther from 0.

STb_summary(model_2_static_fit, digits = 3) # expectation model 2: do not detect social transmission, or at least less than model 1 (because I hand-picked the order of diffusion to skip to unconnected individuals when possible, or follow edges with lower weights, while still using the exact same individuals and network structure as model 1 static. I would expect SOME social transmission, but not much.)
# We do in fact see less! s is now only 0.600, and percent ST is now 0.526. In addition, CI_Lower for s is a bit closer to 0 (0.002). I'm pleased!

STb_summary(model_2_dynamic_fit, digits = 3) # expectation model 2: hmm. Stronger evidence for... lack of social transmission? I'm not sure how to see the confidence intervals and evaluate difference from a null model. Not sure about this one.
# Looks like we get even lower %ST, but higher s. What does that mean?

# Let's look at posterior predictive checks for these.
#Cumulative diffusion curves
#We can plot the posterior distribution of cumulative diffusion curves and compare them with the observed curve. First, we create a dataframe for the cumulative number of individuals who experienced an event.

get_plot_data <- function(event_data){
  out <- event_data %>%
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
    out,
    out %>%
      distinct(trial) %>%
      mutate(time = 0, cum_prop = 0, type = "observed")
  ) %>%
    arrange(trial, time)
  return(plot_data_obs)
}

plot_data_obs_1 <- get_plot_data(event_data_1)
plot_data_obs_2 <- get_plot_data(event_data_2)

get_plot_data_ppc <- function(fit, data_list){
  draws_df <- as_draws_df(fit$draws(variables = "acquisition_time", inc_warmup = F))
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
   # thin for plotting
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
  out <- bind_rows(
    plot_data_ppc,
    plot_data_ppc %>%
      distinct(trial, draw) %>%
      mutate(time = 0, cum_prop = 0, type = "ppc")
  ) %>%
    arrange(trial, time)
  return(out)
}

plot_data_ppc_static_1 <- get_plot_data_ppc(fit = model_1_static_fit, data_list = data_list_1)
plot_data_ppc_static_2 <- get_plot_data_ppc(fit = model_2_static_fit, data_list = data_list_2)
plot_data_ppc_dynamic_1 <- get_plot_data_ppc(fit = model_1_dynamic_fit, data_list = data_list_dyn_1)
plot_data_ppc_dynamic_2 <- get_plot_data_ppc(fit = model_2_dynamic_fit, data_list = data_list_dyn_2)

# plot it
ggplot() +
  geom_line(data = plot_data_ppc_static_1, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs_1, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial") +
  theme_minimal()

ggplot() +
  geom_line(data = plot_data_ppc_static_2, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs_2, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial") +
  theme_minimal()

ggplot() +
  geom_line(data = plot_data_ppc_dynamic_1, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs_1, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial") +
  theme_minimal() # looks fairly similar to the static one

ggplot() +
  geom_line(data = plot_data_ppc_dynamic_2, 
            aes(x = time, y = cum_prop, 
                group = interaction(draw, trial)), alpha = .1) +
  geom_line(data = plot_data_obs_2, aes(x = time, y = cum_prop), linewidth = 1) +
  labs(x = "Time", y = "Cumulative proportion informed", color = "Trial") +
  theme_minimal() # also looks similar to the static one.


acqdata_static_1 <- extract_acqTime(model_1_static_fit, data_list_1)
acqdata_static_2 <- extract_acqTime(model_2_static_fit, data_list_2)
acqdata_dynamic_1 <- extract_acqTime(model_1_static_fit, data_list_dyn_1)
acqdata_dynamic_2 <- extract_acqTime(model_2_dynamic_fit, data_list_dyn_2)

ggplot(acqdata_static_1, aes(x = observed_time, y = median_time)) +
  geom_segment(
    aes(x = observed_time, xend = observed_time, 
        y = median_time, yend = observed_time),
    color = "red",
    alpha = 0.3) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(x = "Observed time", y = "Estimated time", title = "Static 1") +
  theme_minimal()

ggplot(acqdata_static_2, aes(x = observed_time, y = median_time)) +
  geom_segment(
    aes(x = observed_time, xend = observed_time, 
        y = median_time, yend = observed_time),
    color = "red",
    alpha = 0.3) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(x = "Observed time", y = "Estimated time", title = "Static 2") +
  theme_minimal()

ggplot(acqdata_dynamic_1, aes(x = observed_time, y = median_time)) +
  geom_segment(
    aes(x = observed_time, xend = observed_time, 
        y = median_time, yend = observed_time),
    color = "red",
    alpha = 0.3) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(x = "Observed time", y = "Estimated time", title = "Dynamic 1") +
  theme_minimal()

ggplot(acqdata_dynamic_2, aes(x = observed_time, y = median_time)) +
  geom_segment(
    aes(x = observed_time, xend = observed_time, 
        y = median_time, yend = observed_time),
    color = "red",
    alpha = 0.3) +
  geom_point(size = 2) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(x = "Observed time", y = "Estimated time", title = "Dynamic 2") +
  theme_minimal()

# I don't really understand how to interpret these graphs.

# Could also compare to a model that doesn't consider social dynamics, following the tutorial.

# Now, what are some things that would be good to test out on these models?
# We already have one seeded demonstrator and two right-censored individuals
# I guess I want to know what happens if we have blank networks