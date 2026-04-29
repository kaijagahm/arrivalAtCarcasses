library(targets)
library(future)
library(furrr)
library(progressr)
plan(multisession, workers = 15)
handlers(global = TRUE)
n_iter <- 1000

# Get data
tar_load(event_data)
tar_load(data_lists)

# Create model objects ----------------------------------------------------
## Asocial
asocial_mods <- purrr::map(data_lists, ~{
  if(!is.null(.x)){
    mod <- STbayes::generate_STb_model(.x, gq = T, est_acqTime = T, model_type = "asocial")
    return(mod)
  }else{return(NULL)}})

## Social
social_mods <- purrr::map(data_lists, ~{
  if(!is.null(.x)){
    mod <- STbayes::generate_STb_model(.x, gq = T, est_acqTime = T)
    return(mod)
  }else{return(NULL)}})

# Fit models --------------------------------------------------------------
## Fit social
# adding NULL first causes the progress bar to initialize, which is helpful
social_fits <- with_progress({furrr::future_map2(c(NULL, social_mods), c(NULL, data_lists), ~{ 
  if(!is.null(.x)){
    social_fit <- fit_STb(.y, .x, iter = n_iter)
    return(social_fit)
  }else{
    return(NULL)
  }
}, .options = furrr_options(seed = TRUE), .progress = T)})

## Save social
map2(social_fits, 1:length(asocial_fits), ~{
  nm <- paste0("social_fit_", str_pad(as.character(.y), width = 3, side = "left", pad = "0"))
  if(!is.null(.x)){
    STb_save(.x, output_dir = "data/saved_fits/station/", name = nm)
  }else{
    write_rds(NULL, file = paste0("data/saved_fits/station/", nm, ".rds"))
  }
})

## Fit asocial
asocial_fits <- with_progress({furrr::future_map2(c(NULL, asocial_mods), c(NULL, data_lists), ~{ 
  if(!is.null(.x)){
    asocial_fit <- fit_STb(.y, .x, iter = n_iter)
    return(asocial_fit)
  }else{
    return(NULL)
  }
}, .options = furrr_options(seed = TRUE), .progress = T)})

## Save asocial
map2(asocial_fits, 1:length(asocial_fits), ~{
  nm <- paste0("asocial_fit_", str_pad(as.character(.y), width = 3, side = "left", pad = "0"))
  if(!is.null(.x)){
    STb_save(.x, output_dir = "data/saved_fits/station/", name = nm)
  }else{
    write_rds(NULL, file = paste0("data/saved_fits/station/", nm, ".rds"))
  }
})

# Test --------------------------------------------------------------------
loo_outputs <- purrr::map2(social_fits, asocial_fits, ~{
  if(!is.null(.x) & !is.null(.y)){
    STb_compare(.x, .y, method = "loo-psis")
  }else{NULL}})

comparison_dfs <- purrr::map(loo_outputs, ~{
  if(!is.null(.x)){
    df <- as.data.frame(.x$comparison)
    df$model <- rownames(df)
    return(df)
  }else{NULL}})

pareto_dfs <- purrr::map(loo_outputs, ~{
  if(!is.null(.x)){
    as.data.frame(.x$pareto_diagnostics)
  }else{NULL}})

summs_social <- purrr::map(social_fits, ~{
  if(!is.null(.x)){
    STb_summary(.x)
  }else{NULL}})

summs_asocial <- purrr::map(asocial_fits, ~{
  if(!is.null(.x)){
    STb_summary(.x)
  }else{NULL}})

plot_data <- purrr::map2(event_data, social_fits, ~{
  if(!is.null(.x) & !is.null(.y)){
    # create cumulative count of events
    ed <- .x %>% group_by(trial) %>% mutate(n_trial = n())
    
    plot_data_obs <- ed %>%
      filter(time > 0, time <= t_end) %>% # exclude demonstrators (time == 0) and censored (time > t_end)
      group_by(trial) %>%
      arrange(time, .by_group = TRUE) %>%
      mutate(
        cum_prop = row_number() / n_trial, # this denominator needs to be the number of individuals per trial
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
    
    # extract draws of predicted acqtime
    draws_df <- posterior::as_draws_df(.y$draws(variables = "acquisition_time", inc_warmup = FALSE))
    
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
                   each = length(unique(.$trial)) * length(unique(.$ind))
        )
      )
    
    
    # thin sample for plotting
    sample_idx <- sample(c(1:max(ppc_long$draw)), 100)
    ppc_long <- ppc_long %>% filter(draw %in% sample_idx)
    
    # build cumulative curves per draw
    # same as before, we need a way to reference the number of individuals in each trial
    ppc_long <- ppc_long %>%
      group_by(draw, trial) %>%
      mutate(n_trial = n())
    summary(ppc_long)
    # we also need to remove individuals predicted as censored
    ppc_long <- ppc_long %>%
      filter(time > -1)
    # create cumulative curves
    plot_data_ppc <- ppc_long %>%
      group_by(draw, trial, time) %>%
      summarise(n = n(), n_trial = first(n_trial), .groups = "drop") %>%
      group_by(draw, trial) %>%
      arrange(time) %>%
      mutate(cum_prop = cumsum(n) / n_trial)
    
    # add in 0,0 starting point
    plot_data_ppc <- bind_rows(
      plot_data_ppc,
      plot_data_ppc %>%
        distinct(trial, draw) %>%
        mutate(time = 0, cum_prop = 0, type = "ppc")
    ) %>%
      arrange(trial, time)
    
    return(list("obs" = plot_data_obs, "pred" = plot_data_ppc))
    
  }else{NULL}
})

plan(sequential) # end with this

