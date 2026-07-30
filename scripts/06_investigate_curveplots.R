# Investigate curveplots
library(targets)
library(future)
library(furrr)
library(progressr)
library(tidyverse)
library(STbayes)
library(sf)
library(loo)
library(posterior)
library(patchwork)
plan(multisession, workers = 5)
handlers(global = TRUE)
source("R/functions.R")
nit <- 500
# Get plotdata
tar_load(plotdata_noILVs)
tar_load(plotdata_DistI)
tar_load(plotdata_DistIS)
tar_load(plotdata_DistI_AgeIS)
tar_load(plotdata_DistIS_AgeIS)

tar_load(plotdata_noILVs_wild)
tar_load(plotdata_DistI_wild)
tar_load(plotdata_DistIS_wild)
tar_load(plotdata_DistI_AgeIS_wild)
tar_load(plotdata_DistIS_AgeIS_wild)

# Save curveplots
tar_load(curveplots_noILVs)
tar_load(curveplots_DistI)
tar_load(curveplots_DistIS)
tar_load(curveplots_DistI_AgeIS)
tar_load(curveplots_DistIS_AgeIS)

tar_load(curveplots_noILVs_wild)
tar_load(curveplots_DistI_wild)
tar_load(curveplots_DistIS_wild)
tar_load(curveplots_DistI_AgeIS_wild)
tar_load(curveplots_DistIS_AgeIS_wild)

tar_load(event_data)
tar_load(event_data_wild)

padded <- str_pad(1:length(curveplots_DistI), width = 3, side = "left", pad = "0")
padded_wild <- str_pad(1:length(curveplots_DistI_wild), width = 3, side = "left", pad = "0")

tar_load(stn_carcs_modified)
tar_load(wild_carcs)

walk2(curveplots_noILVs, padded, ~{ggsave(.x, file = paste0("data/saved_fits/station/noILVs_2nets/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistI, padded, ~{ggsave(.x, file = paste0("data/saved_fits/station/DistI_2nets/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistIS, padded, ~{ggsave(.x, file = paste0("data/saved_fits/station/DistIS_2nets/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistI_AgeIS, padded, ~{ggsave(.x, file = paste0("data/saved_fits/station/DistI_AgeIS_2nets/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistIS_AgeIS, padded, ~{ggsave(.x, file = paste0("data/saved_fits/station/DistIS_AgeIS_2nets/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})

walk2(curveplots_noILVs_wild, padded_wild, ~{ggsave(.x, file = paste0("data/saved_fits/wild/noILVs_2nets/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistI_wild, padded_wild, ~{ggsave(.x, file = paste0("data/saved_fits/wild/DistI_2nets/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistIS_wild, padded_wild, ~{ggsave(.x, file = paste0("data/saved_fits/wild/DistIS_2nets/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistI_AgeIS_wild, padded_wild, ~{ggsave(.x, file = paste0("data/saved_fits/wild/DistI_AgeIS_2nets/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistIS_AgeIS_wild, padded_wild, ~{ggsave(.x, file = paste0("data/saved_fits/wild/DistIS_AgeIS_2nets/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})


# Investigate bad curveplots ----------------------------------------------
# For each point, quantify 1) does it fall within the 95% CI of pred? and 2) how far inside/outside is it (z-score)

obs <- map(plotdata_DistI, "obs")
obs_wild <- map(plotdata_DistI_wild, "obs")
pred <- map(plotdata_DistI, "pred")
pred_wild <- map(plotdata_DistI_wild, "pred")

# Step 1: for each observed timepoint, interpolate cum_prop for every draw
obs_times <- map(obs, "time")
obs_times_wild <- map(obs_wild, "time")
interp <- function(x, times){
  if(!is.null(x) & length(times[times != 0]) > 2){
    out <- x %>% group_by(draw) %>%
      group_modify(function(df, grp) {
        tibble(time = times,
               cum_prop = approx(x = df$time, y = df$cum_prop, xout = times, rule = 2)$y)
      }) %>% ungroup()
    return(out)
  }else{return(NULL)}
}
interp_pred <- map2(pred, obs_times, ~interp(.x, .y))
interp_pred_wild <- map2(pred_wild, obs_times_wild, ~interp(.x, .y))

join_fun <- function(obs, interp){
  if(!is.null(interp)){
    return(obs %>% left_join(interp, by = "time", suffix = c("", "_pred")))
  }else{return(NULL)}
}

joined <- map2(obs, interp_pred, ~join_fun(.x, .y))
joined_wild <- map2(obs_wild, interp_pred_wild, ~join_fun(.x, .y))

get_quantiles <- function(joined, carcs){
  if(!is.null(joined)){
    out <- joined %>%
      group_by(time) %>%
      summarize(obs = cum_prop[1],
                lower = quantile(cum_prop_pred, 0.025),
                upper = quantile(cum_prop_pred, 0.975),
                med = quantile(cum_prop_pred, 0.5),
                zscore = (cum_prop[1]-mean(cum_prop_pred))/sd(cum_prop_pred)) %>%
      mutate(inside = case_when(obs >= lower & obs <= upper ~ T, .default = F),
             overunder = case_when(upper <= obs ~ "underpredicted",
                                   lower >= obs ~ "overpredicted",
                                   .default = "valid"),
             idx = 1:n(),
             carcID = carcs$carcID,
             year = carcs$year)
    return(out)
  }else{return(NULL)}
}
quantiles <- map2(joined, stn_carcs_modified, ~get_quantiles(.x, .y))
quantiles_wild <- map2(joined_wild, wild_carcs, ~get_quantiles(.x, .y))
quantiles_df <- purrr::list_rbind(quantiles)
quantiles_df_wild <- purrr::list_rbind(quantiles_wild)
quantiles_df_all <- bind_rows(quantiles_df %>% mutate(type = "stn"),
                              quantiles_df_wild %>% mutate(type = "wild"))

# Rough viz of z-scores
quantiles_df_all %>%
  ggplot(aes(x = time, y = zscore, group = carcID, color = factor(year)))+
  geom_line(alpha = 0.1)+
  facet_grid(rows = vars(type), cols = vars(year))+
  theme_minimal()

# Summarize stats by carc
qt <- quantiles_df_all %>%
  group_by(year, carcID, type) %>%
  summarize(prop_inside = mean(overunder == "valid"),
            prop_under = mean(overunder == "underpredicted"),
            prop_over = mean(overunder == "overpredicted"),
            cumul_zscore = sum(abs(zscore)[!is.infinite(zscore)], na.rm = T),
            max_zscore = max(zscore, na.rm = T),
            min_zscore = min(zscore, na.rm = T),
            med_zscore = median(zscore, na.rm = T)) 

qt_long <- qt %>%
  pivot_longer(cols = c(starts_with("prop"), contains("zscore")), names_to = "category", values_to = "value") 

# Plots
## Over/under, all
qt_long %>%
  filter(str_detect(category, "prop")) %>%
  ggplot(aes(x = factor(carcID), y = value, fill = category))+
  geom_bar(stat = "identity")+
  facet_wrap(~type, scales = "free")+
  theme_minimal()+
  coord_flip()+
  scale_fill_manual(values = c("gray", "firebrick3", "dodgerblue"))+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x = element_text(size = 10))

## Zscore stats by wild/stn
qt_long %>%
  filter(str_detect(category,"zscore")) %>%
  ggplot(aes(x = factor(carcID), y = value))+
  geom_col(stat = "identity")+
  facet_wrap(~type*category, scales = "free_x", ncol = 4, nrow = 2)+
  theme_minimal()+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x = element_blank())

## Add more carcass info
qt_long <- purrr::list_rbind(stn_carcs_modified) %>%
  st_as_sf() %>%
  select(carcID, datetime_il, stationName, carcassWeight, X, Y) %>%
  bind_rows(purrr::list_rbind(wild_carcs) %>%
              st_as_sf() %>%
              select(carcID, datetime_il, stationName, carcassWeight, X, Y)) %>%
  right_join(qt_long, by = "carcID")

quantiles_df_all <- purrr::list_rbind(stn_carcs_modified) %>%
  st_as_sf() %>%
  select(carcID, datetime_il, stationName, carcassWeight, X, Y) %>%
  bind_rows(purrr::list_rbind(wild_carcs) %>%
              st_as_sf() %>%
              select(carcID, datetime_il, stationName, carcassWeight, X, Y)) %>%
  right_join(quantiles_df_all, by = "carcID")

## Over/under by wild/stn and year
qt_long %>%
  filter(str_detect(category, "prop")) %>%
  ggplot(aes(x = factor(carcID), y = value, fill = category))+
  geom_bar(stat = "identity")+
  facet_wrap(~year + type, ncol = 2, scales = "free")+
  theme_minimal()+
  coord_flip()+
  scale_fill_manual(values = c("gray", "firebrick3", "dodgerblue"))
# The station carcasses in 2023 are particularly bad for models overpredicting. I wonder if this has anything to do with the number of tagged individuals?
# The difference between station and wild could also have something to do with the mismatch between starting wild from the first sighting versus starting station from the deposition of the carcass. This isn't comparable--need to go back and fix it. 

# Time to first event
counts <- purrr::list_rbind(event_data) %>%
  bind_rows(purrr::list_rbind(event_data_wild)) %>%
  group_by(trial) %>%
  summarize(n_total = n(),
            n_seeds = sum(time == 0),
            n_censored = sum(time > t_end))

qt_long <- qt_long %>%
  left_join(purrr::list_rbind(event_data) %>%
              group_by(trial) %>%
              filter(time > 0) %>%
              slice(1) %>%
              select(trial, time) %>%
              bind_rows(purrr::list_rbind(event_data_wild) %>%
                          group_by(trial) %>%
                          filter(time > 0) %>%
                          slice(1) %>%
                          select(trial, time)) %>%
              rename("time_to_first_event" = "time",
                     "carcID" = "trial"), by = "carcID") %>%
  mutate(hours_to_first_event = time_to_first_event/60/60) %>%
  left_join(counts, by = c("carcID" = "trial"))

quantiles_df_all <- quantiles_df_all %>%
  left_join(purrr::list_rbind(event_data) %>%
              group_by(trial) %>%
              filter(time > 0) %>%
              slice(1) %>%
              select(trial, time) %>%
              bind_rows(purrr::list_rbind(event_data_wild) %>%
                          group_by(trial) %>%
                          filter(time > 0) %>%
                          slice(1) %>%
                          select(trial, time)) %>%
              rename("time_to_first_event" = "time",
                     "carcID" = "trial"), by = "carcID") %>%
  mutate(hours_to_first_event = time_to_first_event/60/60) %>%
  left_join(counts, by = c("carcID" = "trial")) %>%
  mutate(n_found = n_total-n_seeds-n_censored,
         prop_found = n_found/n_total)

## GOF (prop inside) by hours to first vulture
qt_long %>%
  filter(category == "prop_inside") %>%
  ggplot(aes(x = hours_to_first_event, y = value, color = factor(type)))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(y = "Prop. accurate points", x = "Hours to first sighting", color = "Type")+
  theme_minimal()+
  theme(text = element_text(size = 18))+
  scale_color_manual(values = c("orange", "olivedrab"))

## GOF (prop inside) by hours to first vulture, log-transformed
qt_long %>%
  filter(category == "prop_inside") %>%
  ggplot(aes(x = log(hours_to_first_event), y = value, color = factor(type)))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(y = "Prop. accurate points", x = "Hours to first sighting", color = "Type")+
  theme_minimal()+
  theme(text = element_text(size = 18))+
  scale_color_manual(values = c("orange", "olivedrab"))

## Over/under by hours to first vulture
qt_long %>%
  filter(category %in% c("prop_over", "prop_under")) %>%
  ggplot(aes(x = log(hours_to_first_event), y = value, color = factor(type)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~category, scales = "free")+
  labs(y = "Prop. points", x = "Hours to first sighting", color = "Type")+
  theme_minimal()+
  theme(text = element_text(size = 18))+
  scale_color_manual(values = c("orange", "olivedrab"))

## GOF (z-score) by hours to first vulture, log-transformed
qt_long %>%
  filter(category == "cumul_zscore") %>%
  ggplot(aes(x = log(hours_to_first_event), y = value, color = factor(type)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~category, scales = "free")+
  labs(y = "Deviation from curve\n(cumulative z-score)", x = "Hours to first sighting", color = "Type")+
  theme_minimal()+
  theme(text = element_text(size = 18))+
  scale_color_manual(values = c("orange", "olivedrab"))

## Zscore boxplots, by stn/wild and year
quantiles_df_all %>%
  filter(zscore < 50) %>%
  ggplot(aes(x = factor(carcID), y = zscore, fill = factor(year)))+
  theme_minimal()+
  geom_hline(aes(yintercept = 0), color = "blue")+
  geom_boxplot(outlier.size = 0.5)+
  facet_wrap(~type, scales = "free_x", nrow = 2)+
  theme(axis.text.x = element_blank(),
        text = element_text(size = 16))+
  labs(y = "Z-score", x = "Carcass", fill = "Year")

## Zscore boxplots, by time of day
quantiles_df_all %>%
  mutate(carcID = fct_reorder(factor(carcID), lubridate::hour(datetime_il))) %>%
  filter(zscore < 50) %>%
  ggplot(aes(x = carcID, y = zscore, fill = lubridate::hour(datetime_il)))+
  theme_minimal()+
  geom_hline(aes(yintercept = 0), color = "blue")+
  geom_boxplot(outlier.size = 0.5)+
  facet_wrap(~type, scales = "free_x", nrow = 2)+
  scale_fill_viridis_c()+ # No pattern by time of day!
  theme(axis.text.x = element_blank(),
        text = element_text(size = 16))+
  labs(y = "Z-score", x = "Carcass", fill = "Hour of day")

## Zscore boxplots, by #vultures
quantiles_df_all %>%
  mutate(carcID = fct_reorder(factor(carcID), n_found)) %>%
  filter(zscore < 50) %>%
  ggplot(aes(x = carcID, y = zscore, fill = prop_found))+
  theme_minimal()+
  geom_hline(aes(yintercept = 0), color = "blue")+
  geom_boxplot(outlier.size = 0.5)+
  facet_wrap(~type, scales = "free_x", nrow = 2)+
  scale_fill_viridis_c()+
  theme(axis.text.x = element_blank(),
        text = element_text(size = 16))+
  labs(y = "Z-score", x = "Carcass", fill = "Prop. found")# This definitely does seem to be driving... some sort of pattern? At least a broader range of z-scores when there are more vultures, which I think demonstrates a poorer curve fit in general?

quantiles_df_all %>%
  mutate(carcID = fct_reorder(factor(carcID), n_found)) %>%
  filter(zscore < 50) %>%
  ggplot(aes(x = carcID, y = zscore, color = overunder))+
  theme_minimal()+
  geom_hline(aes(yintercept = 0), color = "black", alpha = 0.5)+
  geom_point(size = 0.7, alpha = 0.7)+  
  facet_wrap(~type, scales = "free_x", nrow = 2)+
  scale_fill_viridis_c()+
  theme(legend.position = "bottom")+
  theme(axis.text.x = element_blank(),
        text = element_text(size = 16))+
  labs(y = "Z-score", x = "Carcass", color = "Prediction")# Y-axis here is still ordered by NUMBER OF VULTURES. Can see many more high and low z-scores (green and red, respectively) when there are more vultures involved.

# Z-scores by carcass predictability?
pred <- readRDS("data/created/predictability_results.RDS")
pred_simple <- pred %>%
  select(carcID, prop_days_covered) %>%
  st_drop_geometry()

qt_long <- left_join(qt_long, pred_simple, by = "carcID")
quantiles_df_all <- left_join(quantiles_df_all, pred_simple, by = "carcID")

## Z-scores by predictability
quantiles_df_all %>%
  mutate(carcID = fct_reorder(factor(carcID), prop_days_covered)) %>%
  filter(zscore < 50) %>%
  ggplot(aes(x = carcID, y = zscore, fill = prop_days_covered))+
  theme_minimal()+
  geom_hline(aes(yintercept = 0), color = "black", alpha = 0.5)+
  geom_boxplot(outlier.size = 0.5)+
  facet_wrap(~type, scales = "free_x", nrow = 2)+
  scale_fill_viridis_c()+
  theme(axis.text.x = element_blank(),
        text = element_text(size = 16))+
  labs(y = "Z-score", x = "Carcass", fill = "Predictability")

## Over/under by predictability
qt_long %>%
  filter(category %in% c("prop_under", "prop_over")) %>%
  ggplot(aes(x = prop_days_covered, y = value, color = category))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_minimal()+
  scale_color_manual(values = c("firebrick3", "dodgerblue"))

# Station name?
## Over/under by station
qt_long %>%
  filter(category %in% c("prop_under", "prop_over"), type == "stn") %>%
  group_by(stationName) %>%
  filter(n() > 2) %>%
  ggplot(aes(x = stationName, y = value, fill = factor(category)))+
  geom_boxplot()+
  #coord_flip()+
  theme_minimal()+
  labs(y = "Prop. points", x = "Station", fill = "Over/\nunder\nprediction")+
  theme(text = element_text(size = 16))+
  scale_fill_manual(values = c("firebrick3", "dodgerblue"))

## Cumul z-score by stn
qt_long %>%
  filter(category == "cumul_zscore", type == "stn") %>%
  group_by(stationName) %>%
  filter(n() > 2) %>%
  ggplot(aes(x = stationName, y = value))+
  geom_boxplot()+
  theme_minimal()+
  labs(y = "Deviation from curve\n(cumulative z-score)", x = "Station", fill = "Over/\nunder\nprediction")+
  theme(text = element_text(size = 16))



# Geography
qt_long %>%
  filter(category %in% c("prop_under", "prop_over")) %>%
  ggplot(aes(x = X, y = Y))+
  geom_point(pch = 1, aes(color = category, size = value))+
  theme_minimal()+
  facet_wrap(~category)+
  theme(text = element_text(size = 16))+
  scale_color_manual(values = c("firebrick3", "dodgerblue"))

qt_long %>%
  filter(category == "cumul_zscore") %>%
  ggplot(aes(x = X, y = Y))+
  geom_point(pch = 1, aes(size = value), alpha = 0.6)+
  theme_minimal()+
  theme(text = element_text(size = 16))

# Prop. seeds
qt_long %>%
  filter(category %in% c("prop_under", "prop_over")) %>%
  ggplot(aes(x = n_seeds/n_total, y = value, color = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~category)+
  theme_minimal()+
  scale_color_manual(values = c("orange", "olivedrab"))+
  labs(y = "Proportion of points", x = "Prop. seeds")

# N seeds
qt_long %>%
  filter(category %in% c("prop_under", "prop_over")) %>%
  ggplot(aes(x = n_seeds, y = value, color = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~category)+
  theme_minimal()+
  scale_color_manual(values = c("orange", "olivedrab"))+
  labs(y = "Proportion of points", x = "# seeds")

# Prop found
qt_long %>%
  filter(category %in% c("prop_under", "prop_over")) %>%
  ggplot(aes(x = (n_total-n_seeds-n_censored)/n_total, y = value, color = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~category)+
  theme_minimal()+
  scale_color_manual(values = c("orange", "olivedrab"))+
  labs(y = "Proportion of points", x = "Prop. found")

# Which points on the curve tend to be bad fits?
quantiles_df_all %>%
  group_by(carcID) %>%
  mutate(prop_inside = mean(overunder == "valid"), n = n()) %>%
  filter(prop_inside < 0.7, n > 10) %>%
  filter(carcID %in% sample(unique(.$carcID), 9)) %>%
  ggplot(aes(x = idx, color = overunder, y = obs))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0)+
  geom_point()+
  theme_minimal()+
  facet_wrap(~carcID, scales = "free")+
  scale_color_manual(values = c("gray", "firebrick3", "dodgerblue"))

quantiles_df_all %>%
  group_by(carcID) %>%
  filter(carcID %in% sample(unique(quantiles_df_all$carcID), 9), !is.infinite(zscore)) %>%
  ggplot(aes(x = time, color = zscore, y = obs))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0)+
  geom_point()+
  theme_minimal()+
  facet_wrap(~carcID, scales = "free")+
  scale_color_viridis_c()

# some common patterns: 
# model underpredicts at the beginning, then overpredicts at the end
# model consistently underpredicts


# Comparing curve shapes: some form of taking the derivative?
#(𝑦𝑖+1−𝑦𝑖−1)/(𝑥𝑖+1−𝑥𝑖−1)

test_obs <- obs[[1]]
test_pred <- interp_pred[[1]]
test_pred <- test_pred %>%
  arrange(draw, time) %>%
  group_by(draw) %>%
  mutate(slope = (lead(cum_prop, 2)-cum_prop)/(lead(time, 2)-time))
test_obs <- test_obs %>% mutate(slope = (lead(cum_prop, 2)-cum_prop)/(lead(time, 2)-time)) 

ggplot(mapping = aes(x = time/60/60, y = slope))+
  geom_line(data = test_pred %>% filter(time <5*60*60), aes(group = draw), alpha = 0.1, color = "purple")+
  geom_line(data = test_obs %>% filter(time <5*60*60), alpha = 1)+
  theme_minimal() +
  labs(y = "Slope", x = "Hours")

# Let's look at correlations instead of 95% confidence intervals, since those aren't sensitive to vertical shifts.
cors_stn<- map2(obs, interp_pred, ~{
  if(is.null(.x)|is.null(.y)){return(NULL)}else{
    a <- .x$cum_prop
    draws <- unique(.y$draw)
    cors <- rep(NA, length = length(draws))
    for(i in 1:length(draws)){
      b <- .y %>% filter(draw == draws[i]) %>% pull(cum_prop)
      cors[i] <- cor(a, b)
    }
    return(cors)
  }
})

cors_wild<- map2(obs_wild, interp_pred_wild, ~{
  if(is.null(.x)|is.null(.y)){return(NULL)}else{
    a <- .x$cum_prop
    draws <- unique(.y$draw)
    cors <- rep(NA, length = length(draws))
    for(i in 1:length(draws)){
      b <- .y %>% filter(draw == draws[i]) %>% pull(cum_prop)
      cors[i] <- cor(a, b)
    }
    return(cors)
  }
})

cors_stn_df <- map2(cors_stn, map_dbl(stn_carcs, "carcID"), ~{
  if(!is.null(.x)){
    return(data.frame(carcID = .y, cor = .x))
  }else{return(NULL)}
}) %>% purrr::list_rbind() %>% mutate(type = "stn")

cors_wild_df <- map2(cors_wild, map_dbl(wild_carcs, "carcID"), ~{
  if(!is.null(.x)){
    return(data.frame(carcID = .y, cor = .x))
  }else{return(NULL)}
}) %>% purrr::list_rbind() %>% mutate(type = "wild")
cors <- bind_rows(cors_stn_df, cors_wild_df) %>% right_join(qt_long %>% pivot_wider(names_from = "category", values_from = "prop"))

cors %>%
  ggplot(aes(x = prop_inside, y = cor, group = carcID, fill = type))+
  geom_boxplot(pch = 1, alpha = 0.5, width = 1, outlier.size = 0.5)+
  theme_minimal()+
  facet_wrap(~type)+
  scale_fill_manual(values = c("orange", "olivedrab"))+
  labs(y = "Obs-pred correlation", x = "Prop. pts inside 95% CI", fill = "Type")+
  theme(text = element_text(size = 16))

cors %>%
  ggplot(aes(x = prop_inside, y = cor, group = carcID, fill = log(hours_to_first_event), color = log(hours_to_first_event)))+
  geom_boxplot(pch = 1, alpha = 0.5, width = 1, outlier.size = 0.5)+
  theme_minimal()+
  facet_wrap(~type)+
  scale_fill_viridis_c()+
  scale_color_viridis_c()+
  labs(y = "Obs-pred correlation", x = "Prop. pts inside 95% CI", fill = "Hrs to first event\n(log-transformed)", color = "Hrs to first event\n(log-transformed)")+
  theme(text = element_text(size = 16))

cors %>%
  ggplot(aes(x = prop_inside, y = cor, group = carcID, fill = prop_under))+
  geom_boxplot(pch = 1, alpha = 0.9, width = 1, outlier.size = 0.5)+
  theme_minimal()+
  facet_wrap(~type)+
  scale_fill_gradient2(low = "white", high = "firebrick3")+
  labs(y = "Obs-pred correlation", x = "Prop. pts inside 95% CI", fill = "Prop. overpredicted")+
  theme(text = element_text(size = 16))

cors %>%
  ggplot(aes(x = prop_inside, y = cor, group = carcID, fill = prop_under))+
  geom_boxplot(pch = 1, alpha = 0.9, width = 1, outlier.size = 0.5)+
  theme_minimal()+
  facet_wrap(~type)+
  scale_fill_gradient2(low = "white", high = "dodgerblue3")+
  labs(y = "Obs-pred correlation", x = "Prop. pts inside 95% CI", fill = "Prop. underpredicted")+
  theme(text = element_text(size = 16))

