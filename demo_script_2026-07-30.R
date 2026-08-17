# Script to demonstrate/investigate bad curveplots
# Follow-up to Kaija/Michael discussion at ISBE

library(tidyverse)
#library(targets)
library(STbayes)
library(posterior)

# I chose four carcasses (diffusions) that have PPC curveplots of varying levels of badness.
# 004 (bad), 005 (bad), 014 (bad), 019 (decent)
idxs <- c(4, 5, 14, 19)

# Retrieve data from {targets} pipeline and subset it for this demo.
# Let me know if you want access to other data products or data from additional diffusions! Just trying to keep it simple for now and not send you my entire pipeline.

# tar_load(event_data)
# tar_load(data_lists_DistI_2nets)
# tar_load(stn_carcs_modified)
# tar_load(social_mods_DistI_2nets)
# 
# events <- event_data[idxs]
# data_lists <- data_lists_DistI_2nets[idxs]
# carcs <- stn_carcs_modified[idxs]
# soc <- social_mods_DistI_2nets[idxs]
# 
# write_rds(events, "data/forMichael_2026-07-30/events.RDS")
# write_rds(data_lists, "data/forMichael_2026-07-30/data_lists.RDS")
# write_rds(carcs, "data/forMichael_2026-07-30/carcs.RDS")
# write_rds(soc, "data/forMichael_2026-07-30/soc.RDS")

events <- readRDS("data/forMichael_2026-07-30/events.RDS")
data_lists <- readRDS("data/forMichael_2026-07-30/data_lists.RDS")
carcs <- readRDS("data/forMichael_2026-07-30/carcs.RDS")
soc <- readRDS("data/forMichael_2026-07-30/soc.RDS")

# # Fit models (commented this part out because it runs slowly, but feel free to re-run!)
# social_fits <- purrr::map2(soc, data_lists, ~fit_STb(.y, .x, iter = 1000), .progress = T)
# 
# # Save fits
walk2(social_fits, idxs, ~{
  nm <- paste0("fit_social", "_", str_pad(as.character(.y), width = 3, side = "left", pad = "0"))
  STb_save(.x, output_dir = paste0("data/forMichael_2026-07-30/saved_fits/"), name = nm)
})

# Get file names to read fits back in
filenames <- list.files(path = "data/forMichael_2026-07-30/saved_fits/", pattern = "fit_social")

# Read in fits from files
fits <- purrr::map(filenames, ~readRDS(paste0("data/forMichael_2026-07-30/saved_fits/", .x)))

summs <- purrr::map(fits, STb_summary) # Side note: it would be helpful if the arguments in the summary objects could be more informatively named! It would be nice to be able to quickly extract n individuals, n seeded demonstrators, n right-censored individuals, etc. without having to think too hard about it
str(summs[[1]], 1) # example summary structure

# Get data for plotting ppc curves for models
get_plotdata <- function(event_data, model_fit){
  
  if(!is.null(event_data) & !is.null(model_fit)){
    # create cumulative count of events
    ed <- event_data %>% group_by(trial) %>% mutate(n_trial = n())
    
    plot_data_obs <- ed %>%
      filter(
        #time > 0, # Remove this--we want to include the demonstrators in the obs line, since they're included in the draws!
        time <= t_end) %>% # exclude demonstrators (time == 0) and censored (time > t_end)
      group_by(trial) %>%
      arrange(time, .by_group = TRUE) %>%
      mutate(
        cum_prop = row_number() / n_trial, # this denominator needs to be the number of individuals per trial
        type = "observed"
      ) %>%
      select(trial, time, cum_prop, type) %>%
      ungroup()
    
    # If there's not already a value for 0, add in 0,0 starting point
    if(!(0 %in% plot_data_obs$time)){
      plot_data_obs <- bind_rows(
        plot_data_obs,
        plot_data_obs %>%
          distinct(trial) %>%
          mutate(time = 0, cum_prop = 0, type = "observed")
      ) %>%
        arrange(trial, time)
    }
    
    # extract draws of predicted acqtime
    draws_df <- posterior::as_draws_df(model_fit$draws(variables = "acquisition_time", inc_warmup = FALSE))
    
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
}

plotdata <- purrr::map2(fits, events, ~get_plotdata(.y, .x))

# Make ppc curveplots
get_curveplots <- function(plot_data, cid){
  if(!is.null(plot_data)){
    p <- ggplot(mapping = aes(x = time, y = cum_prop))+
      geom_line(
        data = plot_data$pred, aes(group = interaction(draw, trial)), alpha = 0.1)+
      geom_line(
        data = plot_data$obs, linewidth = 1)+
      labs(x = "Time", y = "Cumulative proportion informed", title = cid)+
      theme_minimal()
    return(p)
  }else{return(NULL)}
}

curveplots <- purrr::map2(plotdata, purrr::map_dbl(carcs, "carcID"), ~get_curveplots(.x, .y))

curveplots[[1]]
curveplots[[2]]
curveplots[[3]]
curveplots[[4]]
