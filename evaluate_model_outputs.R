# Making models
library(tidyverse)
library(targets)
library(STbayes)
library(posterior)
library(sf)

source("R/functions.R")
tar_load(oo)
tar_load(ooo)
tar_load(oooo)
tar_load(stn_carcs)
carcIDs <- map_dbl(stn_carcs, "carcID")
all <- c(oo, ooo, oooo)

plots <- purrr::map(all, ~{
  if(!is.null(.x)){
    p <- ggplot() +
      geom_line(
        data = .x$pred,
        aes(
          x = time, y = cum_prop,
          group = interaction(draw, trial)
        ), alpha = .1
      ) +
      geom_line(data = .x$obs, aes(x = time, y = cum_prop), linewidth = 1) +
      labs(x = "Time", y = "Cumulative proportion informed", color = "Trial") +
      theme_minimal()
  }else{
    p <- NULL
  }
  return(p)
})

pct_st <- purrr::map_dbl(all, ~{
  if(!is.null(.x)){
    summ <- .x$summ
    out <- summ$Median[summ$Parameter == "percent_ST[1]"]
  }else{
    out <- NA
  }
})

results <- data.frame(carcID = carcIDs, pct_st = pct_st)
carcs_focal <- readRDS("data/created/carcs_focal.RDS")
cf <- carcs_focal %>%
  select(carcID, year, stn_days_last6mos)

results <- left_join(results, cf, by = "carcID")

results %>% filter(pct_st > 0) %>% ggplot(aes(x = stn_days_last6mos, y = pct_st, color = factor(year)))+geom_point(pch = 1, size = 2)+theme_minimal()+
  geom_smooth(method = "lm")
