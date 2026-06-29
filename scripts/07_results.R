# %ST vs predictability
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
handlers(global = TRUE)
source("R/functions.R")

tar_load(social_fits_DistI_2nets)
tar_load(social_fits_DistI_wild_2nets)
summs <- map(social_fits_DistI_2nets, ~{
  if(!is.null(.x)){
    STb_summary(.x)
  }else{
    NULL
  }})

summs_wild <- map(social_fits_DistI_wild_2nets, ~{
  if(!is.null(.x)){
    STb_summary(.x)
  }else{
    NULL
  }})
tar_load(stn_carcs_modified)
tar_load(wild_carcs)
names(summs) <- map_dbl(stn_carcs_modified, "carcID")
names(summs_wild) <- map_dbl(wild_carcs, "carcID")
summs_df <- purrr::list_rbind(summs, names_to = "carcID") %>% mutate(carcType = "stn")
summs_df_wild <- purrr::list_rbind(summs_wild, names_to = "carcID") %>% mutate(carcType = "wild")

summs_all <- bind_rows(summs_df, summs_df_wild) %>%
  mutate(carcID = as.numeric(carcID))

pred <- readRDS("data/created/predictability_results.RDS")

summs_all <- left_join(summs_all, pred, by = c("carcID", "carcType"))

# Moment of truth ---------------------------------------------------------
# Median %ST by carcass type
# Have not yet excluded the ones where it wasn't valid
pctst <- summs_all %>%
  filter(grepl("percent_ST", Parameter)) %>%
  mutate(param = case_when(Parameter == "percent_ST[1]" ~ "Flight",
                           Parameter == "percent_ST[2]" ~ "Roost"))
pctst %>%
  ggplot(aes(x = factor(year), y = Median, fill = carcType))+
  geom_boxplot(outlier.size = 0.5)+
  facet_wrap(~param)+
  theme_minimal()+
  theme(text = element_text(size = 16),
        legend.position = "bottom")+
  labs(y = "%ST (Median)",
       x = "Year",
       fill = "Carcass type")+
  scale_fill_manual(values = c("darkorange", "olivedrab"))

pctst %>%
  ggplot(aes(x = prop_days_covered, y = Median, color = carcType))+
  geom_point(size = 2, pch = 1, alpha = 0.8)+
  theme_minimal()+
  geom_smooth(method = "lm")+
  theme(text = element_text(size = 16),
        legend.position = "bottom")+
  labs(y = "%ST (Median)",
       x = "Predictability",
       color = "Carcass type")+
  scale_color_manual(values = c("darkorange", "olivedrab"))

pctst %>%
  filter(CI_Lower > 0) %>%
  ggplot(aes(x = prop_days_covered, y = Median, color = carcType))+
  geom_point(size = 2, pch = 1, alpha = 0.8)+
  theme_minimal()+
  geom_smooth(method = "lm")+
  theme(text = element_text(size = 16),
        legend.position = "bottom")+
  labs(y = "%ST (Median)",
       x = "Predictability",
       color = "Carcass type")+
  scale_color_manual(values = c("darkorange", "olivedrab"))

pctst %>%
  filter(CI_Lower > 0) %>%
  ggplot(aes(x = prop_days_covered, y = Median, color = carcType))+
  facet_wrap(~year)+
  geom_point(size = 2, pch = 1, alpha = 0.8)+
  theme_minimal()+
  geom_smooth(method = "lm")+
  theme(text = element_text(size = 16),
        legend.position = "bottom")+
  labs(y = "%ST (Median)",
       x = "Predictability",
       color = "Carcass type")+
  scale_color_manual(values = c("darkorange", "olivedrab"))


