library(targets)
library(future)
library(furrr)
library(progressr)
library(tidyverse)
library(STbayes)
plan(multisession, workers = 15)
handlers(global = TRUE)
n_iter <- 1000

# Get data
tar_load(event_data)
tar_load(data_lists)
tar_load(stn_carcs)

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
# social_fits <- with_progress({furrr::future_map2(c(NULL, social_mods), c(NULL, data_lists), ~{ 
#   if(!is.null(.x)){
#     social_fit <- fit_STb(.y, .x, iter = n_iter)
#     return(social_fit)
#   }else{
#     return(NULL)
#   }
# }, .options = furrr_options(seed = TRUE), .progress = T)})
# 
# ## Save social
# map2(social_fits, 1:length(asocial_fits), ~{
#   nm <- paste0("social_fit_", str_pad(as.character(.y), width = 3, side = "left", pad = "0"))
#   if(!is.null(.x)){
#     STb_save(.x, output_dir = "data/saved_fits/station/", name = nm)
#   }else{
#     write_rds(NULL, file = paste0("data/saved_fits/station/", nm, ".rds"))
#   }
# })
# 
# ## Fit asocial
# asocial_fits <- with_progress({furrr::future_map2(c(NULL, asocial_mods), c(NULL, data_lists), ~{ 
#   if(!is.null(.x)){
#     asocial_fit <- fit_STb(.y, .x, iter = n_iter)
#     return(asocial_fit)
#   }else{
#     return(NULL)
#   }
# }, .options = furrr_options(seed = TRUE), .progress = T)})
# 
# ## Save asocial
# map2(asocial_fits, 1:length(asocial_fits), ~{
#   nm <- paste0("asocial_fit_", str_pad(as.character(.y), width = 3, side = "left", pad = "0"))
#   if(!is.null(.x)){
#     STb_save(.x, output_dir = "data/saved_fits/station/", name = nm)
#   }else{
#     write_rds(NULL, file = paste0("data/saved_fits/station/", nm, ".rds"))
#   }
# })


# Test --------------------------------------------------------------------
soc_filenames <- list.files(path = "data/saved_fits/station/", pattern = "^social")
asoc_filenames <- list.files(path = "data/saved_fits/station/", pattern = "^asocial")
social_fits <- map(soc_filenames, ~readRDS(paste0("data/saved_fits/station/", .x)))
asocial_fits <- map(asoc_filenames, ~readRDS(paste0("data/saved_fits/station/", .x)))

loo_outputs <- purrr::map2(social_fits, asocial_fits, ~{
  if(!is.null(.x) & !is.null(.y)){
    suppressMessages(STb_compare(.x, .y, method = "loo-psis", model_names = c("social", "asocial")))
  }else{NULL}}, .progress = T)

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

# Make asocial comparison plots
asoc_comparison_plots <- purrr::map2(comparison_dfs, map_dbl(stn_carcs, "carcID"), ~{
  if(!is.null(.x)){
    p <- ggplot(.x, aes(x = reorder(model, elpd_diff), y = elpd_diff))+
      geom_point(size = 3)+
      geom_errorbar(aes(ymin = elpd_diff-se_diff,
                        ymax = elpd_diff+se_diff), width = 0.2)+
      coord_flip()+
      labs(x = "Model", y = "ELPD Difference", title = "Model Comparison")+
      theme_minimal()
    return(p)
  }else{return(NULL)}
})

walk2(asoc_comparison_plots, 1:length(asoc_comparison_plots), ~ggsave(plot = .x, file = paste0("data/saved_fits/station/asoc_comparison_plots/asoc_comparison_plot_", str_pad(.y, width = 3, side = "left", pad = "0"), ".png"), width = 6, height = 5))

# Make pareto plots
pareto_plots <- purrr::map2(pareto_dfs, map_dbl(stn_carcs, "carcID"), ~{
  if(!is.null(.x)){
    p <- ggplot(.x, aes(x=observation, y=pareto_k, color=model))+
      geom_point() +
      scale_color_viridis_d(begin=0.2, end=0.7)+
      geom_hline(yintercept = 0.7, linetype="dashed", color="orange")+
      geom_hline(yintercept = 1, linetype="dashed", color="red")+
      labs(x="Observation", y="Pareto-k value", title="Pareto-k diagnostics")+
      theme_minimal()
    return(p)
  }else{return(NULL)}
})

walk2(pareto_plots, 1:length(pareto_plots), ~ggsave(plot = .x, file = paste0("data/saved_fits/station/pareto_plots/pareto_plots_", str_pad(.y, width = 3, side = "left", pad = "0"), ".png"), width = 6, height = 5))

# Make ppc curve plots
ppc_curve_plots <- purrr::map2(plot_data, map_dbl(stn_carcs, "carcID"), ~{
  if(!is.null(.x)){
    p <- ggplot(mapping = aes(x = time, y = cum_prop))+
      geom_line(
        data = .x$pred, aes(group = interaction(draw, trial)), alpha = 0.1)+
      geom_line(
        data = .x$obs, linewidth = 1)+
      labs(x = "Time", y = "Cumulative proportion informed", color = "Trial", title = .y)+
      theme_minimal()
    return(p)
  }else{return(NULL)}})

walk2(ppc_curve_plots, 1:length(ppc_curve_plots), ~ggsave(plot = .x, file = paste0("data/saved_fits/station/curveplots/curveplot_", str_pad(.y, width = 3, side = "left", pad = "0"), ".png"), width = 6, height = 5))

plan(sequential) # end with this