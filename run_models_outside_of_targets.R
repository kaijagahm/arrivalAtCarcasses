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

# Get event data
tar_load(event_data)
tar_load(event_data_wild)

# Get data lists (stn)
tar_load(data_lists_noILVs_2nets)
tar_load(data_lists_DistI_2nets)
tar_load(data_lists_DistIS_2nets)
tar_load(data_lists_DistI_AgeIS_2nets)
tar_load(data_lists_DistIS_AgeIS_2nets)

# Get data lists (wild)
tar_load(data_lists_noILVs_2nets_wild)
tar_load(data_lists_DistI_2nets_wild)
tar_load(data_lists_DistIS_2nets_wild)
tar_load(data_lists_DistI_AgeIS_2nets_wild)
tar_load(data_lists_DistIS_AgeIS_2nets_wild)

# Get carcass data (stn and wild)
tar_load(stn_carcs)
tar_load(wild_carcs)

# Create model objects ----------------------------------------------------
# Asocial stn, 2nets
asocial_mods_noILVs_2nets <- purrr::map(data_lists_noILVs_2nets, get_asocial)
asocial_mods_DistI_2nets <- purrr::map(data_lists_DistI_2nets, get_asocial)
asocial_mods_DistIS_2nets <- purrr::map(data_lists_DistIS_2nets, get_asocial)
asocial_mods_DistI_AgeIS_2nets <- purrr::map(data_lists_DistI_AgeIS_2nets, get_asocial)
asocial_mods_DistIS_AgeIS_2nets <- purrr::map(data_lists_DistIS_AgeIS_2nets, get_asocial)

# Asocial wild, 2nets
asocial_mods_noILVs_2nets_wild <- purrr::map(data_lists_noILVs_2nets_wild, get_asocial)
asocial_mods_DistI_2nets_wild <- purrr::map(data_lists_DistI_2nets_wild, get_asocial)
asocial_mods_DistIS_2nets_wild <- purrr::map(data_lists_DistIS_2nets_wild, get_asocial)
asocial_mods_DistI_AgeIS_2nets_wild <- purrr::map(data_lists_DistI_AgeIS_2nets_wild, get_asocial)
asocial_mods_DistIS_AgeIS_2nets_wild <- purrr::map(data_lists_DistIS_AgeIS_2nets_wild, get_asocial)

# Social stn, 2nets
social_mods_noILVs_2nets <- purrr::map(data_lists_noILVs_2nets, get_social)
social_mods_DistI_2nets <- purrr::map(data_lists_DistI_2nets, get_social)
social_mods_DistIS_2nets <- purrr::map(data_lists_DistIS_2nets, get_social)
social_mods_DistI_AgeIS_2nets <- purrr::map(data_lists_DistI_AgeIS_2nets, get_social)
social_mods_DistIS_AgeIS_2nets <- purrr::map(data_lists_DistIS_AgeIS_2nets, get_social)

# Social wild, 2nets
social_mods_noILVs_2nets_wild <- purrr::map(data_lists_noILVs_2nets_wild, get_social)
social_mods_DistI_2nets_wild <- purrr::map(data_lists_DistI_2nets_wild, get_social)
social_mods_DistIS_2nets_wild <- purrr::map(data_lists_DistIS_2nets_wild, get_social)
social_mods_DistI_AgeIS_2nets_wild <- purrr::map(data_lists_DistI_AgeIS_2nets_wild, get_social)
social_mods_DistIS_AgeIS_2nets_wild <- purrr::map(data_lists_DistIS_AgeIS_2nets_wild, get_social)

# Fit models --------------------------------------------------------------
# Fit and save social (stn)
social_fits_noILVs_2nets <- with_progress(furrr::future_map2(social_mods_noILVs_2nets, data_lists_noILVs_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(social_fits_noILVs_2nets, ~{savefit(.x, .y, folder = "NoILVs_2nets", prefix = "social", type = "station")}) # noILVs

social_fits_DistI_2nets <- with_progress(furrr::future_map2(social_mods_DistI_2nets, data_lists_DistI_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(social_fits_DistI_2nets, ~{savefit(.x, .y, folder = "DistI_2nets", prefix = "social", type = "station")}) # DistI

social_fits_DistIS_2nets <- with_progress(furrr::future_map2(social_mods_DistIS_2nets, data_lists_DistIS_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(social_fits_DistIS_2nets, ~{savefit(.x, .y, folder = "DistIS_2nets", prefix = "social", type = "station")}) # DistIS

social_fits_DistI_AgeIS_2nets <- with_progress(furrr::future_map2(social_mods_DistI_AgeIS_2nets, data_lists_DistI_AgeIS_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(social_fits_DistI_AgeIS_2nets, ~{savefit(.x, .y, folder = "DistI_AgeIS_2nets", prefix = "social", type = "station")}) # DistI_AgeIS

social_fits_DistIS_AgeIS_2nets <- with_progress(furrr::future_map2(social_mods_DistIS_AgeIS_2nets, data_lists_DistIS_AgeIS_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(social_fits_DistIS_AgeIS_2nets, ~{savefit(.x, .y, folder = "DistIS_AgeIS_2nets", prefix = "social", type = "station")}) # DistIS_AgeIS

# Fit and save asocial (stn)
asocial_fits_noILVs_2nets <- with_progress(furrr::future_map2(asocial_mods_noILVs_2nets, data_lists_noILVs_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(asocial_fits_noILVs_2nets, ~{savefit(.x, .y, folder = "NoILVs_2nets", prefix = "asocial", type = "station")}) # noILVs

asocial_fits_DistI_2nets <- with_progress(furrr::future_map2(asocial_mods_DistI_2nets, data_lists_DistI_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(asocial_fits_DistI_2nets, ~{savefit(.x, .y, folder = "DistI_2nets", prefix = "asocial", type = "station")}) # DistI

asocial_fits_DistIS_2nets <- with_progress(furrr::future_map2(asocial_mods_DistIS_2nets, data_lists_DistIS_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(asocial_fits_DistIS_2nets, ~{savefit(.x, .y, folder = "DistIS_2nets", prefix = "asocial", type = "station")}) # DistIS

asocial_fits_DistI_AgeIS_2nets <- with_progress(furrr::future_map2(asocial_mods_DistI_AgeIS_2nets, data_lists_DistI_AgeIS_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(asocial_fits_DistI_AgeIS_2nets, ~{savefit(.x, .y, folder = "DistI_AgeIS_2nets", prefix = "asocial", type = "station")}) # DistI_AgeIS

asocial_fits_DistIS_AgeIS_2nets <- with_progress(furrr::future_map2(asocial_mods_DistIS_AgeIS_2nets, data_lists_DistIS_AgeIS_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(asocial_fits_DistIS_AgeIS_2nets, ~{savefit(.x, .y, folder = "DistIS_AgeIS_2nets", prefix = "asocial", type = "station")}) # DistIS_AgeIS

# Fit and save social (wild)
social_fits_noILVs_2nets_wild <- with_progress(furrr::future_map2(social_mods_noILVs_2nets_wild, data_lists_noILVs_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(social_fits_noILVs_2nets_wild, ~{savefit(.x, .y, folder = "NoILVs_2nets", prefix = "social", type = "wild")}) # noILVs

social_fits_DistI_2nets_wild <- with_progress(furrr::future_map2(social_mods_DistI_2nets_wild, data_lists_DistI_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(social_fits_DistI_2nets_wild, ~{savefit(.x, .y, folder = "DistI_2nets", prefix = "social", type = "wild")}) # DistI

social_fits_DistIS_2nets_wild <- with_progress(furrr::future_map2(social_mods_DistIS_2nets_wild, data_lists_DistIS_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(social_fits_DistIS_2nets_wild, ~{savefit(.x, .y, folder = "DistIS_2nets", prefix = "social", type = "wild")}) # DistIS

social_fits_DistI_AgeIS_2nets_wild <- with_progress(furrr::future_map2(social_mods_DistI_AgeIS_2nets_wild, data_lists_DistI_AgeIS_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(social_fits_DistI_AgeIS_2nets_wild, ~{savefit(.x, .y, folder = "DistI_AgeIS_2nets", prefix = "social", type = "wild")}) # DistI_AgeIS

social_fits_DistIS_AgeIS_2nets_wild <- with_progress(furrr::future_map2(social_mods_DistIS_AgeIS_2nets_wild, data_lists_DistIS_AgeIS_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(social_fits_DistIS_AgeIS_2nets_wild, ~{savefit(.x, .y, folder = "DistIS_AgeIS_2nets", prefix = "social", type = "wild")}) # DistIS_AgeIS

# Fit and save asocial (wild)
asocial_fits_noILVs_2nets_wild <- with_progress(furrr::future_map2(asocial_mods_noILVs_2nets_wild, data_lists_noILVs_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(asocial_fits_noILVs_2nets_wild, ~{savefit(.x, .y, folder = "NoILVs_2nets", prefix = "asocial", type = "wild")}) # noILVs

asocial_fits_DistI_2nets_wild <- with_progress(furrr::future_map2(asocial_mods_DistI_2nets_wild, data_lists_DistI_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(asocial_fits_DistI_2nets_wild, ~{savefit(.x, .y, folder = "DistI_2nets", prefix = "asocial", type = "wild")}) # DistI

asocial_fits_DistIS_2nets_wild <- with_progress(furrr::future_map2(asocial_mods_DistIS_2nets_wild, data_lists_DistIS_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(asocial_fits_DistIS_2nets_wild, ~{savefit(.x, .y, folder = "DistIS_2nets", prefix = "asocial", type = "wild")}) # DistIS

asocial_fits_DistI_AgeIS_2nets_wild <- with_progress(furrr::future_map2(asocial_mods_DistI_AgeIS_2nets_wild, data_lists_DistI_AgeIS_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(asocial_fits_DistI_AgeIS_2nets_wild, ~{savefit(.x, .y, folder = "DistI_AgeIS_2nets", prefix = "asocial", type = "wild")}) # DistI_AgeIS

asocial_fits_DistIS_AgeIS_2nets_wild <- with_progress(furrr::future_map2(asocial_mods_DistIS_AgeIS_2nets_wild, data_lists_DistIS_AgeIS_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(asocial_fits_DistIS_AgeIS_2nets_wild, ~{savefit(.x, .y, folder = "DistIS_AgeIS_2nets", prefix = "asocial", type = "wild")}) # DistIS_AgeIS


#Get filenames
## Station social
soc_filenames_noILVs_2nets <- list.files(path = "data/saved_fits/station/NoILVs_2nets/", pattern = "fit_social")
soc_filenames_DistI_2nets <- list.files(path = "data/saved_fits/station/DistI_2nets/", pattern = "fit_social")
soc_filenames_DistIS_2nets <- list.files(path = "data/saved_fits/station/DistIS_2nets/", pattern = "fit_social")
soc_filenames_DistI_AgeIS_2nets <- list.files(path = "data/saved_fits/station/DistI_AgeIS_2nets/", pattern = "fit_social")
soc_filenames_DistIS_AgeIS_2nets <- list.files(path = "data/saved_fits/station/DistIS_AgeIS_2nets/", pattern = "fit_social")

## Station asocial
asoc_filenames_noILVs_2nets <- list.files(path = "data/saved_fits/station/NoILVs_2nets/", pattern = "fit_asocial")
asoc_filenames_DistI_2nets <- list.files(path = "data/saved_fits/station/DistI_2nets/", pattern = "fit_asocial")
asoc_filenames_DistIS_2nets <- list.files(path = "data/saved_fits/station/DistIS_2nets/", pattern = "fit_asocial")
asoc_filenames_DistI_AgeIS_2nets <- list.files(path = "data/saved_fits/station/DistI_AgeIS_2nets/", pattern = "fit_asocial")
asoc_filenames_DistIS_AgeIS_2nets <- list.files(path = "data/saved_fits/station/DistIS_AgeIS_2nets/", pattern = "fit_asocial")

## Wild social
soc_filenames_noILVs_wild_2nets <- list.files(path = "data/saved_fits/wild/NoILVs_2nets/", pattern = "fit_social")
soc_filenames_DistI_wild_2nets <- list.files(path = "data/saved_fits/wild/DistI_2nets/", pattern = "fit_social")
soc_filenames_DistIS_wild_2nets <- list.files(path = "data/saved_fits/wild/DistIS_2nets/", pattern = "fit_social")
soc_filenames_DistI_AgeIS_wild_2nets <- list.files(path = "data/saved_fits/wild/DistI_AgeIS_2nets/", pattern = "fit_social")
soc_filenames_DistIS_AgeIS_wild_2nets <- list.files(path = "data/saved_fits/wild/DistIS_AgeIS_2nets/", pattern = "fit_social")

## Wild asocial
asoc_filenames_noILVs_wild_2nets <- list.files(path = "data/saved_fits/wild/NoILVs_2nets/", pattern = "fit_asocial")
asoc_filenames_DistI_wild_2nets <- list.files(path = "data/saved_fits/wild/DistI_2nets/", pattern = "fit_asocial")
asoc_filenames_DistIS_wild_2nets <- list.files(path = "data/saved_fits/wild/DistIS_2nets/", pattern = "fit_asocial")
asoc_filenames_DistI_AgeIS_wild_2nets <- list.files(path = "data/saved_fits/wild/DistI_AgeIS_2nets/", pattern = "fit_asocial")
asoc_filenames_DistIS_AgeIS_wild_2nets <- list.files(path = "data/saved_fits/wild/DistIS_AgeIS_2nets/", pattern = "fit_asocial")

# Read in fits
## Station social
social_fits_noILVs_2nets <- map(soc_filenames_noILVs_2nets, ~readRDS(paste0("data/saved_fits/station/NoILVs_2nets/", .x)))
social_fits_DistI_2nets <- map(soc_filenames_DistI_2nets, ~readRDS(paste0("data/saved_fits/station/DistI_2nets/", .x)))
social_fits_DistIS_2nets <- map(soc_filenames_DistIS_2nets, ~readRDS(paste0("data/saved_fits/station/DistIS_2nets/", .x)))
social_fits_DistI_AgeIS_2nets <- map(soc_filenames_DistI_AgeIS_2nets, ~readRDS(paste0("data/saved_fits/station/DistI_AgeIS_2nets/", .x)))
social_fits_DistIS_AgeIS_2nets <- map(soc_filenames_DistIS_AgeIS_2nets, ~readRDS(paste0("data/saved_fits/station/DistIS_AgeIS_2nets/", .x)))

## Station asocial
asocial_fits_noILVs_2nets <- map(asoc_filenames_noILVs_2nets, ~readRDS(paste0("data/saved_fits/station/NoILVs_2nets/", .x)))
asocial_fits_DistI_2nets <- map(asoc_filenames_DistI_2nets, ~readRDS(paste0("data/saved_fits/station/DistI_2nets/", .x)))
asocial_fits_DistIS_2nets <- map(asoc_filenames_DistIS_2nets, ~readRDS(paste0("data/saved_fits/station/DistIS_2nets/", .x)))
asocial_fits_DistI_AgeIS_2nets <- map(asoc_filenames_DistI_AgeIS_2nets, ~readRDS(paste0("data/saved_fits/station/DistI_AgeIS_2nets/", .x)))
asocial_fits_DistIS_AgeIS_2nets <- map(asoc_filenames_DistIS_AgeIS_2nets, ~readRDS(paste0("data/saved_fits/station/DistIS_AgeIS_2nets/", .x)))

## Wild social
social_fits_noILVs_wild_2nets <- map(soc_filenames_noILVs_wild_2nets, ~readRDS(paste0("data/saved_fits/wild/NoILVs_2nets/", .x)))
social_fits_DistI_wild_2nets <- map(soc_filenames_DistI_wild_2nets, ~readRDS(paste0("data/saved_fits/wild/DistI_2nets/", .x)))
social_fits_DistIS_wild_2nets <- map(soc_filenames_DistIS_wild_2nets, ~readRDS(paste0("data/saved_fits/wild/DistIS_2nets/", .x)))
social_fits_DistI_AgeIS_wild_2nets <- map(soc_filenames_DistI_AgeIS_wild_2nets, ~readRDS(paste0("data/saved_fits/wild/DistI_AgeIS_2nets/", .x)))
social_fits_DistIS_AgeIS_wild_2nets <- map(soc_filenames_DistIS_AgeIS_wild_2nets, ~readRDS(paste0("data/saved_fits/wild/DistIS_AgeIS_2nets/", .x)))

## Wild asocial
asocial_fits_noILVs_wild_2nets <- map(asoc_filenames_noILVs_wild_2nets, ~readRDS(paste0("data/saved_fits/wild/NoILVs_2nets/", .x)))
asocial_fits_DistI_wild_2nets <- map(asoc_filenames_DistI_wild_2nets, ~readRDS(paste0("data/saved_fits/wild/DistI_2nets/", .x)))
asocial_fits_DistIS_wild_2nets <- map(asoc_filenames_DistIS_wild_2nets, ~readRDS(paste0("data/saved_fits/wild/DistIS_2nets/", .x)))
asocial_fits_DistI_AgeIS_wild_2nets <- map(asoc_filenames_DistI_AgeIS_wild_2nets, ~readRDS(paste0("data/saved_fits/wild/DistI_AgeIS_2nets/", .x)))
asocial_fits_DistIS_AgeIS_wild_2nets <- map(asoc_filenames_DistIS_AgeIS_wild_2nets, ~readRDS(paste0("data/saved_fits/wild/DistIS_AgeIS_2nets/", .x)))

# Get model summaries (stn)
summs_noILVs_2nets <- map(social_fits_noILVs_2nets, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistI_2nets <- map(social_fits_DistI_2nets, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistIS_2nets <- map(social_fits_DistIS_2nets, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistI_AgeIS_2nets <- map(social_fits_DistI_AgeIS_2nets, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistIS_AgeIS_2nets <- map(social_fits_DistIS_AgeIS_2nets, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")

summs_2nets <- purrr::list_rbind(list("noILVs" = summs_noILVs_2nets, "DistI" = summs_DistI_2nets, "DistIS" = summs_DistIS_2nets, "DistI_AgeIS" = summs_DistI_AgeIS_2nets, "DistIS_AgeIS" = summs_DistIS_AgeIS_2nets), names_to = "model") %>% mutate(type = "stn")

# Get model summaries (wild)
summs_noILVs_wild_2nets <- map(social_fits_noILVs_wild_2nets, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistI_wild_2nets <- map(social_fits_DistI_wild_2nets, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistIS_wild_2nets <- map(social_fits_DistIS_wild_2nets, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistI_AgeIS_wild_2nets <- map(social_fits_DistI_AgeIS_wild_2nets, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistIS_AgeIS_wild_2nets <- map(social_fits_DistIS_AgeIS_wild_2nets, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")

summs_wild_2nets <- purrr::list_rbind(list("noILVs" = summs_noILVs_wild_2nets, "DistI" = summs_DistI_wild_2nets, "DistIS" = summs_DistIS_wild_2nets, "DistI_AgeIS" = summs_DistI_AgeIS_wild_2nets, "DistIS_AgeIS" = summs_DistIS_AgeIS_wild_2nets), names_to = "model") %>% mutate(type = "wild")

hist(summs_2nets$Rhat) # this is fine
hist(summs_wild_2nets$Rhat)
# this is also fine

# Are the Rhat values different for different parameters?
summs_2nets %>%
  ggplot(aes(x = Rhat))+
  geom_density(aes(color = Parameter))+
  theme_minimal()+
  theme(legend.position = "bottom")

summs_wild_2nets %>%
  ggplot(aes(x = Rhat))+
  geom_density(aes(color = Parameter))+
  theme_minimal()+
  theme(legend.position = "bottom")

# What about for different models?
summs_2nets %>%
  mutate(model = factor(model, levels = c("noILVs", "DistI", "DistIS", "DistI_AgeIS", "DistIS_AgeIS"))) %>%
  ggplot(aes(x = Rhat))+
  geom_density(aes(color = Parameter))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  facet_wrap(~model)

# Inter-model comparisons
# Model comparisons
## Compare 
## 5-way comparisons
comps <- pmap(list(a = social_fits_noILVs_2nets, b = social_fits_DistI_2nets, c = social_fits_DistIS_2nets, d = social_fits_DistI_AgeIS_2nets, e = social_fits_DistIS_AgeIS_2nets), function(a, b, c, d, e){if(!is.null(a)){STb_compare(a, b, c, d, e, model_names = c("noILVs", "DistI", "DistIS", "DistI_AgeIS", "DistIS_AgeIS"), method = "loo-psis")}else{NULL}})

comps_wild <- pmap(list(a = social_fits_noILVs_wild_2nets, b = social_fits_DistI_wild_2nets, c = social_fits_DistIS_wild_2nets, d = social_fits_DistI_AgeIS_wild_2nets, e = social_fits_DistIS_AgeIS_wild_2nets), function(a, b, c, d, e){if(!is.null(a)){STb_compare(a, b, c, d, e, model_names = c("noILVs", "DistI", "DistIS", "DistI_AgeIS", "DistIS_AgeIS"), method = "loo-psis")}else{NULL}})

comps_dfs <- map(comps, ~as.data.frame(.x$comparison))
comps_dfs_wild <- map(comps_wild, ~as.data.frame(.x$comparison))
comps_dfs <- map(comps_dfs, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_wild <- map(comps_dfs_wild, ~{.x$model <- rownames(.x);return(.x)})

# Examining which are the most common orders
names(comps_dfs) <- map_dbl(stn_carcs, "carcID")
names(comps_dfs_wild) <- map_dbl(wild_carcs, "carcID")
comps_dfs_df <- purrr::list_rbind(comps_dfs, names_to = "carcID")
comps_dfs_wild_df <- purrr::list_rbind(comps_dfs_wild, names_to = "carcID")
rownames(comps_dfs_df) <- NULL
rownames(comps_dfs_wild_df) <- NULL

comps_dfs_df <- comps_dfs_df %>%
  arrange(carcID, desc(elpd_diff)) %>%
  group_by(carcID) %>%
  mutate(idx = 1:n())

comps_dfs_wild_df <- comps_dfs_wild_df %>%
  arrange(carcID, desc(elpd_diff)) %>%
  group_by(carcID) %>%
  mutate(idx = 1:n())

comps_dfs_df %>%
  ggplot(aes(x = carcID, y = log(abs(elpd_diff)), col = factor(model)))+
  geom_point()+
  coord_flip()+ # noILVs is consistently the worst, but there isn't much/any pattern in terms of which one is the best.
  theme_minimal()

comps_dfs_wild_df %>%
  ggplot(aes(x = carcID, y = log(abs(elpd_diff)), col = factor(model)))+
  geom_point()+
  coord_flip()+ # similar story with wild. noILVs is almost always the worst, but at the top there's not a single consistent pattern.
  theme_minimal()

# Note that this doesn't take into account which ones are actually different from each other. Let's see if we can do that.

topmods_stn <- comps_dfs_df %>%
  filter(elpd_diff + se_diff >= 0)
topmods_wild <- comps_dfs_wild_df %>%
  filter(elpd_diff + se_diff >= 0)

topmods_stn %>%
  group_by(model) %>%
  summarize(in_top_mods.prop_carcs = length(unique(carcID))/length(stn_carcs)) %>%
  arrange(desc(in_top_mods.prop_carcs)) %>%
  ggplot(aes(x = factor(model, levels = model), y = in_top_mods.prop_carcs, fill = factor(model)))+
  geom_col()+
  theme_minimal()+
  theme(text = element_text(size = 18),
        legend.position = "none")+
  labs(y = "In top mods? (Prop. carcs)",
       x = "Model",
       title = "Stn")+
  scale_y_continuous(limits = c(0, 1))

topmods_wild %>%
  group_by(model) %>%
  summarize(in_top_mods.prop_carcs = length(unique(carcID))/length(wild_carcs)) %>%
  arrange(desc(in_top_mods.prop_carcs)) %>%
  ggplot(aes(x = factor(model, levels = model), y = in_top_mods.prop_carcs, fill = factor(model)))+
  geom_col()+
  theme_minimal()+
  theme(text = element_text(size = 18),
        legend.position = "none")+
  labs(y = "In top mods? (Prop. carcs)",
       x = "Model",
       title = "Wild")+
  scale_y_continuous(limits = c(0, 1))

# In neither case is there one model formulation that is consistently one of the top models.

# Making model comparison plots
comps_plots <- map(comps_dfs, ~{
  if(nrow(.x) > 0){
    p <- ggplot(.x, aes(x = reorder(model, elpd_diff), y = elpd_diff)) +
      geom_point(size = 3) + #elpd_diff
      geom_errorbar(aes(ymin = elpd_diff - se_diff, 
                        ymax = elpd_diff + se_diff), width = 0.2) + #SE of elpd diff
      coord_flip() +
      labs(x = "Model", y = "ELPD Difference", title = "Model Comparison") +
      theme_minimal()
    return(p)
  }else{
    return(NULL)
  }
})

comps_plots_wild <- map(comps_dfs_wild, ~{
  if(nrow(.x) > 0){
    p <- ggplot(.x, aes(x = reorder(model, elpd_diff), y = elpd_diff)) +
      geom_point(size = 3) + #elpd_diff
      geom_errorbar(aes(ymin = elpd_diff - se_diff,
                        ymax = elpd_diff + se_diff), width = 0.2) + #SE of elpd diff
      coord_flip() +
      labs(x = "Model", y = "ELPD Difference", title = "Model Comparison") +
      theme_minimal()
    return(p)
  }else{
    return(NULL)
  }
})

# See the comparison plots
comps_plots[[1]]
comps_plots_wild[[1]]

# Are the coefficients similar across different models?
coefs_withinfo_stn <- summs_2nets %>%
  left_join(data.frame(carcID = map_dbl(stn_carcs, "carcID"), idx = 1:length(stn_carcs)), by = "idx") %>%
  mutate(carcID = as.character(carcID)) %>%
  left_join(select(topmods_stn, carcID, model) %>%
              mutate(in_topmods = T), by = c("model", "carcID")) %>%
  mutate(in_topmods = replace_na(in_topmods, F))

coefs_withinfo_wild <- summs_wild_2nets %>%
  left_join(data.frame(carcID = map_dbl(wild_carcs, "carcID"), idx = 1:length(wild_carcs)), by = "idx") %>%
  mutate(carcID = as.character(carcID)) %>%
  left_join(select(topmods_wild, carcID, model) %>%
              mutate(in_topmods = T), by = c("model", "carcID")) %>%
  mutate(in_topmods = replace_na(in_topmods, F))

# What about percent_ST only?
coefs_withinfo_stn %>%
  filter(idx == 1, Parameter %in% c("percent_ST[1]", "percent_ST[2]")) %>%
  ggplot(aes(x = Parameter, color = factor(model), y = Median))+
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, linewidth = in_topmods), position = position_dodge(width = 0.5), width = 0, alpha = 0.8)+
  geom_point(aes(size = in_topmods), position = position_dodge(width = 0.5), pch = 1)+
  theme_minimal()+
  coord_flip()+
  labs(y = "Estimate", x = "Parameter", color = "Model",
       size = "Top model?", linewidth = "Top model?",
       title = map_dbl(stn_carcs, "carcID")[1])+ 
  scale_linewidth_manual(values = c(0.5, 1.5))+
  scale_size_manual(values = c(1, 3)) # okay yeah the estimates of %ST don't change.

# Let's do this for the rest of them.
model_levels <- c("DistI", "DistI_AgeIS", "DistIS", "DistIS_AgeIS", "noILVs")
model_colors <- c(
  "DistI"        = "#E41A1C",
  "DistI_AgeIS"  = "#377EB8",
  "DistIS"       = "#4DAF4A",
  "DistIS_AgeIS" = "#FF7F00",
  "noILVs"       = "#984EA3"
)

shared_scales <- list(
  scale_color_manual(values = model_colors, limits = model_levels),
  scale_linewidth_manual(values = c(0.5, 1.5), limits = c(FALSE, TRUE)),
  scale_size_manual(values = c(1, 3), limits = c(FALSE, TRUE))
)

plot_betas <- function(df, i, carcs){
  df %>%
    mutate(model = factor(model, levels = model_levels)) %>%
    filter(idx == i, grepl("beta", Parameter)) %>%
    ggplot(aes(x = Parameter, color = model, y = Median))+
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, linewidth = factor(in_topmods)),
                  position = position_dodge(width = 0.5), width = 0, alpha = 0.8)+
    geom_point(aes(size = factor(in_topmods)),
               position = position_dodge(width = 0.5), pch = 1)+
    theme_minimal()+
    scale_x_discrete(labels = function(x) {
      gsub(" ", "_", str_wrap(gsub("_", " ", x), width = 15))
    })+       
    coord_flip()+
    labs(y = "Estimate", x = "Parameter", color = "Model",
         size = "Top model?", linewidth = "Top model?",
         title = map_dbl(carcs, "carcID")[i])+
    shared_scales
}

betas_plots_stn <- map(1:length(stn_carcs), ~{
  plot_betas(coefs_withinfo_stn, .x, carcs = stn_carcs) +
    theme(legend.position = "none")
})

betas_plots_wild <- map(1:length(wild_carcs), ~{
  plot_betas(coefs_withinfo_wild, .x, carcs = wild_carcs) +
    theme(legend.position = "none")
})

plot_pctST <- function(df, i, carcs){
  plt <- df %>%
    mutate(model = factor(model, levels = c("DistI", "DistI_AgeIS", "DistIS", "DistIS_AgeIS", "noILVs"))) %>%
    filter(idx == i, grepl("percent_ST", Parameter)) %>%
    ggplot(aes(x = Parameter, color = factor(model), y = Median))+
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, linewidth = factor(in_topmods)), position = position_dodge(width = 0.5), width = 0, alpha = 0.8)+
    geom_point(aes(size = factor(in_topmods)), position = position_dodge(width = 0.5), pch = 1)+
    theme_minimal()+
    geom_hline(aes(yintercept = 0))+
    coord_flip()+
    labs(y = "Estimate", x = "Parameter", color = "Model",
         size = "Top model?", linewidth = "Top model?",
         title = map_dbl(carcs, "carcID")[i])+ 
    shared_scales
  return(plt)
}

pctST_plots_stn <- map(1:length(stn_carcs), ~{
  plot_pctST(coefs_withinfo_stn, .x, carcs = stn_carcs)
})

pctST_plots_wild <- map(1:length(wild_carcs), ~{
  plot_pctST(coefs_withinfo_wild, .x, carcs = wild_carcs)
})

patchworks_stn <- map2(betas_plots_stn, pctST_plots_stn, ~{.x + .y})
patchworks_wild <- map2(betas_plots_wild, pctST_plots_wild, ~{.x + .y})

patchworks_stn[[1]]
patchworks_stn[[2]]
patchworks_stn[[3]]
patchworks_wild[[1]]
patchworks_wild[[2]]
patchworks_wild[[3]]

# Check Pareto values
pareto_dfs <- map(comps, ~as.data.frame(.x$pareto_diagnostics))
pareto_dfs_wild <- map(comps_wild, ~as.data.frame(.x$pareto_diagnostics))

pareto_plots <- map2(pareto_dfs, map_dbl(stn_carcs, "carcID"), ~{
  if(nrow(.x) > 0){
    p <- ggplot(.x, aes(x=observation, y=pareto_k, color=model))+
      geom_point() +
      scale_color_viridis_d(begin=0.2)+
      geom_hline(yintercept = 0.7, linetype="dashed", color="orange")+
      geom_hline(yintercept = 1, linetype="dashed", color="red")+
      labs(x="Observation", y="Pareto-k value", title=.y)+
      theme_minimal()
    return(p)
  }else{NULL}
})

pareto_plots_wild <- map2(pareto_dfs_wild, map_dbl(wild_carcs, "carcID"), ~{
  if(nrow(.x) > 0){
    p <- ggplot(.x, aes(x=observation, y=pareto_k, color=model))+
      geom_point() +
      scale_color_viridis_d(begin=0.2)+
      geom_hline(yintercept = 0.7, linetype="dashed", color="orange")+
      geom_hline(yintercept = 1, linetype="dashed", color="red")+
      labs(x="Observation", y="Pareto-k value", title=.y)+
      theme_minimal()
    return(p)
  }else{NULL}
})

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

plotdata_noILVs <- map2(social_fits_noILVs_2nets, event_data, ~get_plotdata(.y, .x))
plotdata_DistI <- map2(social_fits_DistI_2nets, event_data, ~get_plotdata(.y, .x))
plotdata_DistIS <- map2(social_fits_DistIS_2nets, event_data, ~get_plotdata(.y, .x))
plotdata_DistI_AgeIS <- map2(social_fits_DistI_AgeIS_2nets, event_data, ~get_plotdata(.y, .x))
plotdata_DistIS_AgeIS <- map2(social_fits_DistIS_AgeIS_2nets, event_data, ~get_plotdata(.y, .x))

plotdata_noILVs_wild <- map2(social_fits_noILVs_wild_2nets, event_data_wild, ~get_plotdata(.y, .x))
plotdata_DistI_wild <- map2(social_fits_DistI_wild_2nets, event_data_wild, ~get_plotdata(.y, .x))
plotdata_DistIS_wild <- map2(social_fits_DistIS_wild_2nets, event_data_wild, ~get_plotdata(.y, .x))
plotdata_DistI_AgeIS_wild <- map2(social_fits_DistI_AgeIS_wild_2nets, event_data_wild, ~get_plotdata(.y, .x))
plotdata_DistIS_AgeIS_wild <- map2(social_fits_DistIS_AgeIS_wild_2nets, event_data_wild, ~get_plotdata(.y, .x))

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

# Make ppc curve plots
curveplots_noILVs <- map2(plotdata_noILVs, map_dbl(stn_carcs, "carcID"), ~get_curveplots(.x, .y))
curveplots_DistI <- map2(plotdata_DistI, map_dbl(stn_carcs, "carcID"), ~get_curveplots(.x, .y))
curveplots_DistIS <- map2(plotdata_DistIS, map_dbl(stn_carcs, "carcID"), ~get_curveplots(.x, .y))
curveplots_DistI_AgeIS <- map2(plotdata_DistI_AgeIS, map_dbl(stn_carcs, "carcID"), ~get_curveplots(.x, .y))
curveplots_DistIS_AgeIS <- map2(plotdata_DistIS_AgeIS, map_dbl(stn_carcs, "carcID"), ~get_curveplots(.x, .y))

curveplots_noILVs_wild <- map2(plotdata_noILVs_wild, map_dbl(wild_carcs, "carcID"), ~get_curveplots(.x, .y))
curveplots_DistI_wild <- map2(plotdata_DistI_wild, map_dbl(wild_carcs, "carcID"), ~get_curveplots(.x, .y))
curveplots_DistIS_wild <- map2(plotdata_DistIS_wild, map_dbl(wild_carcs, "carcID"), ~get_curveplots(.x, .y))
curveplots_DistI_AgeIS_wild <- map2(plotdata_DistI_AgeIS_wild, map_dbl(wild_carcs, "carcID"), ~get_curveplots(.x, .y))
curveplots_DistIS_AgeIS_wild <- map2(plotdata_DistIS_AgeIS_wild, map_dbl(wild_carcs, "carcID"), ~get_curveplots(.x, .y))

padded <- str_pad(1:length(curveplots_noILVs), width = 3, side = "left", pad = "0")
padded_wild <- str_pad(1:length(curveplots_noILVs_wild), width = 3, side = "left", pad = "0")

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


# Investigating curveplots--quantifying, for each point on the X axis, whether it falls within the 95% CI for the models.

obs <- map(plotdata_DistI, "obs")
obs_wild <- map(plotdata_DistI_wild, "obs")
pred <- map(plotdata_DistI, "pred")
pred_wild <- map(plotdata_DistI_wild, "pred")

# Step 1: for each observed timepoint, interpolate cum_prop for every draw
obs_times <- map(obs, "time")
obs_times_wild <- map(obs_wild, "time")
interp_pred <- map2(pred, obs_times, ~{
  if(!is.null(.x) & length(.y[.y != 0]) > 2){
    .x %>% group_by(draw) %>%
      group_modify(function(df, grp) {
        tibble(time = .y,
               cum_prop = approx(x = df$time, y = df$cum_prop, xout = .y, rule = 2)$y)
      }) %>% ungroup()
  }else{ NULL}})

interp_pred_wild <- map2(pred_wild, obs_times_wild, ~{
  if(!is.null(.x) & length(.y[.y != 0]) > 2){
    .x %>% group_by(draw) %>%
      group_modify(function(df, grp) {
        tibble(time = .y,
               cum_prop = approx(x = df$time, y = df$cum_prop, xout = .y, rule = 2)$y)
      }) %>% ungroup()
  }else{ NULL}})

joined <- map2(obs, interp_pred, ~{
  if(!is.null(.y)){
    .x %>% left_join(.y, by = "time", suffix = c("", "_pred"))
  }else{NULL}})

joined_wild <- map2(obs_wild, interp_pred_wild, ~{
  if(!is.null(.y)){
    .x %>% left_join(.y, by = "time", suffix = c("", "_pred"))
  }else{NULL}})

quantiles <- map2(joined, stn_carcs, ~{
  if(!is.null(.x)){
    .x %>%
      group_by(time) %>%
      summarize(obs = cum_prop[1],
                lower = quantile(cum_prop_pred, 0.025),
                upper = quantile(cum_prop_pred, 0.975),
                med = quantile(cum_prop_pred, 0.5)) %>%
      mutate(inside = case_when(obs >= lower & obs <= upper ~ T, .default = F),
             overunder = case_when(upper <= obs ~ "underpredicted",
                                   lower >= obs ~ "overpredicted",
                                   .default = "valid"),
             idx = 1:n(),
             carcID = .y$carcID,
             year = .y$year)
  }else{
    NULL
  }
})

quantiles_wild <- map2(joined_wild, wild_carcs, ~{
  if(!is.null(.x)){
    .x %>%
      group_by(time) %>%
      summarize(obs = cum_prop[1],
                lower = quantile(cum_prop_pred, 0.025),
                upper = quantile(cum_prop_pred, 0.975),
                med = quantile(cum_prop_pred, 0.5)) %>%
      mutate(inside = case_when(obs >= lower & obs <= upper ~ T, .default = F),
             overunder = case_when(upper <= obs ~ "underpredicted",
                                   lower >= obs ~ "overpredicted",
                                   .default = "valid"),
             idx = 1:n(),
             carcID = .y$carcID,
             year = .y$year)
  }else{
    NULL
  }
})

quantiles_df <- purrr::list_rbind(quantiles)
quantiles_df_wild <- purrr::list_rbind(quantiles_wild)

quantiles_df_all <- bind_rows(quantiles_df %>% mutate(type = "stn"),
                              quantiles_df_wild %>% mutate(type = "wild"))

qt <- quantiles_df_all %>%
  group_by(year, carcID, type) %>%
  summarize(prop_inside = mean(overunder == "valid"),
            prop_under = mean(overunder == "underpredicted"),
            prop_over = mean(overunder == "overpredicted")) 

qt_long <- qt %>%
  pivot_longer(cols = starts_with("prop"), names_to = "category", values_to = "prop") 

qt_long %>%
  ggplot(aes(x = factor(carcID), y = prop, fill = category))+
  geom_bar(stat = "identity")+
  facet_wrap(~type, scales = "free")+
  theme_minimal()+
  coord_flip()+
  scale_fill_manual(values = c("gray", "firebrick3", "dodgerblue"))+
  labs(x = NULL, y = NULL)+
  theme(axis.text.x = element_text(size = 10))

# Some observations:
# Station carcasses are more likely to be generally overpredicted. Wild carcasses are more likely to be generally underpredicted. This is the opposite of what I would have guessed!
# More carcasses than I had expected have significant problems with their PPC curves.

qt_long <- purrr::list_rbind(stn_carcs) %>%
  st_as_sf() %>%
  select(carcID, datetime_il, stationName, carcassWeight, X, Y) %>%
  bind_rows(purrr::list_rbind(wild_carcs) %>%
              st_as_sf() %>%
              select(carcID, datetime_il, stationName, carcassWeight, X, Y)) %>%
  right_join(qt_long, by = "carcID")

qt_long %>%
  ggplot(aes(x = factor(carcID), y = prop, fill = category))+
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

qt_long %>%
  filter(category == "prop_inside") %>%
  ggplot(aes(x = hours_to_first_event, y = prop, color = factor(type)))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(y = "Prop. accurate points", x = "Hours to first sighting", color = "Type")+
  theme_minimal()+
  theme(text = element_text(size = 18))+
  scale_color_manual(values = c("orange", "olivedrab"))

qt_long %>%
  filter(category == "prop_inside") %>%
  ggplot(aes(x = log(hours_to_first_event), y = prop, color = factor(type)))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(y = "Prop. accurate points", x = "Hours to first sighting", color = "Type")+
  theme_minimal()+
  theme(text = element_text(size = 18))+
  scale_color_manual(values = c("orange", "olivedrab"))

qt_long %>%
  filter(category != "prop_inside") %>%
  ggplot(aes(x = log(hours_to_first_event), y = prop, color = factor(type)))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~category)+
  labs(y = "Prop. points", x = "Hours to first sighting", color = "Type")+
  theme_minimal()+
  theme(text = element_text(size = 18))+
  scale_color_manual(values = c("orange", "olivedrab"))

# Station name?
qt_long %>%
  filter(category != "prop_inside", type == "stn") %>%
  group_by(stationName) %>%
  filter(n() > 2) %>%
  ggplot(aes(x = stationName, y = prop, fill = factor(category)))+
  geom_boxplot()+
  #coord_flip()+
  theme_minimal()+
  labs(y = "Prop. points", x = "Station", fill = "Over/\nunder\nprediction")+
  theme(text = element_text(size = 16))+
  scale_fill_manual(values = c("firebrick3", "dodgerblue"))

# Geography
qt_long %>%
  filter(category != "prop_inside") %>%
  ggplot(aes(x = X, y = Y))+
  geom_point(pch = 1, aes(color = category, size = prop))+
  theme_minimal()+
  facet_wrap(~category)+
  theme(text = element_text(size = 16))+
  scale_color_manual(values = c("firebrick3", "dodgerblue"))

# Prop. seeds
qt_long %>%
  filter(category != "prop_inside") %>%
  ggplot(aes(x = n_seeds/n_total, y = prop, color = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~category)+
  theme_minimal()+
  scale_color_manual(values = c("orange", "olivedrab"))+
  labs(y = "Proportion of points", x = "Prop. seeds")

# N seeds
qt_long %>%
  filter(category != "prop_inside") %>%
  ggplot(aes(x = n_seeds, y = prop, color = type))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~category)+
  theme_minimal()+
  scale_color_manual(values = c("orange", "olivedrab"))+
  labs(y = "Proportion of points", x = "# seeds")

# Prop censored
qt_long %>%
  filter(category != "prop_inside") %>%
  ggplot(aes(x = (n_total - n_censored - n_seeds)/n_total, y = prop, color = type))+
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

# Model output evaluation and inter-model comparisons ---------------------
# Comparison between each model and its asocial equivalent
comps_noILVs <- map2(social_fits_noILVs, asocial_fits_noILVs, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("noILVs", "noILVs_asoc"), method = "loo-psis")}else{NULL}})

comps_DistI <- map2(social_fits_DistI, asocial_fits_DistI, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistI", "DistI_asoc"), method = "loo-psis")}else{NULL}})

comps_DistIS <- map2(social_fits_DistIS, asocial_fits_DistIS, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistIS", "DistIS_asoc"), method = "loo-psis")}else{NULL}})

comps_DistI_AgeIS <- map2(social_fits_DistI_AgeIS, asocial_fits_DistI_AgeIS, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistI_AgeIS", "DistI_AgeIS_asoc"), method = "loo-psis")}else{NULL}})

comps_DistIS_AgeIS <- map2(social_fits_DistIS_AgeIS, asocial_fits_DistIS_AgeIS, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistIS_AgeIS", "DistIS_AgeIS_asoc"), method = "loo-psis")}else{NULL}})

comps_noILVs_wild <- map2(social_fits_noILVs_wild, asocial_fits_noILVs_wild, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("noILVs", "noILVs_asoc"), method = "loo-psis")}else{NULL}})

comps_DistI_wild <- map2(social_fits_DistI_wild, asocial_fits_DistI_wild, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistI", "DistI_asoc"), method = "loo-psis")}else{NULL}})

comps_DistIS_wild <- map2(social_fits_DistIS_wild, asocial_fits_DistIS_wild, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistIS", "DistIS_asoc"), method = "loo-psis")}else{NULL}})

comps_DistI_AgeIS_wild <- map2(social_fits_DistI_AgeIS_wild, asocial_fits_DistI_AgeIS_wild, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistI_AgeIS", "DistI_AgeIS_asoc"), method = "loo-psis")}else{NULL}})

comps_DistIS_AgeIS_wild <- map2(social_fits_DistIS_AgeIS_wild, asocial_fits_DistIS_AgeIS_wild, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistIS_AgeIS", "DistIS_AgeIS_asoc"), method = "loo-psis")}else{NULL}})

comps_dfs_noILVs <- map(comps_noILVs, ~as.data.frame(.x$comparison))
comps_dfs_DistI <- map(comps_DistI, ~as.data.frame(.x$comparison))
comps_dfs_DistIS <- map(comps_DistIS, ~as.data.frame(.x$comparison))
comps_dfs_DistI_AgeIS <- map(comps_DistI_AgeIS, ~as.data.frame(.x$comparison))
comps_dfs_DistIS_AgeIS <- map(comps_DistIS_AgeIS, ~as.data.frame(.x$comparison))

comps_dfs_noILVs_wild <- map(comps_noILVs_wild, ~as.data.frame(.x$comparison))
comps_dfs_DistI_wild <- map(comps_DistI_wild, ~as.data.frame(.x$comparison))
comps_dfs_DistIS_wild <- map(comps_DistIS_wild, ~as.data.frame(.x$comparison))
comps_dfs_DistI_AgeIS_wild <- map(comps_DistI_AgeIS_wild, ~as.data.frame(.x$comparison))
comps_dfs_DistIS_AgeIS_wild <- map(comps_DistIS_AgeIS_wild, ~as.data.frame(.x$comparison))

comps_dfs_noILVs <- map(comps_dfs_noILVs, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistI <- map(comps_dfs_DistI, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistIS <- map(comps_dfs_DistIS, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistI_AgeIS <- map(comps_dfs_DistI_AgeIS, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistIS_AgeIS <- map(comps_dfs_DistIS_AgeIS, ~{.x$model <- rownames(.x);return(.x)})

comps_dfs_noILVs_wild <- map(comps_dfs_noILVs_wild, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistI_wild <- map(comps_dfs_DistI_wild, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistIS_wild <- map(comps_dfs_DistIS_wild, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistI_AgeIS_wild <- map(comps_dfs_DistI_AgeIS_wild, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistIS_AgeIS_wild <- map(comps_dfs_DistIS_AgeIS_wild, ~{.x$model <- rownames(.x);return(.x)})

get_diff_from_asoc <- function(comparison_df){
  if(!is.null(comparison_df)){
    interval <- c((comparison_df$elpd_diff[2]-comparison_df$se_diff[2]), (comparison_df$elpd_diff[2]+comparison_df$se_diff[2]))
    sig <- comparison_df$elpd_diff[1] >= max(interval) | comparison_df$elpd_diff[1] <= min(interval)
    return(sig)
  }else{return(NULL)}
}

dfa_noILVs <- map(comps_dfs_noILVs, get_diff_from_asoc) %>% setNames(., map_dbl(stn_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")
dfa_DistI <- map(comps_dfs_DistI, get_diff_from_asoc) %>% setNames(., map_dbl(stn_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")
dfa_DistIS <- map(comps_dfs_DistIS, get_diff_from_asoc) %>% setNames(., map_dbl(stn_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID") 
dfa_DistI_AgeIS <- map(comps_dfs_DistI_AgeIS, get_diff_from_asoc) %>% setNames(., map_dbl(stn_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")
dfa_DistIS_AgeIS <- map(comps_dfs_DistIS_AgeIS, get_diff_from_asoc) %>% setNames(., map_dbl(stn_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")

dfa <- purrr::list_rbind(list("noILVs" = dfa_noILVs, "DistI" = dfa_DistI, "DistIS" = dfa_DistIS, "DistI_AgeIS" = dfa_DistI_AgeIS, "DistIS_AgeIS" = dfa_DistIS_AgeIS), names_to = "model")
names(dfa)[3] <- "sig"

dfa_noILVs_wild <- map(comps_dfs_noILVs_wild, get_diff_from_asoc) %>% setNames(., map_dbl(wild_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")
dfa_DistI_wild <- map(comps_dfs_DistI_wild, get_diff_from_asoc) %>% setNames(., map_dbl(wild_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")
dfa_DistIS_wild <- map(comps_dfs_DistIS_wild, get_diff_from_asoc) %>% setNames(., map_dbl(wild_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID") 
dfa_DistI_AgeIS_wild <- map(comps_dfs_DistI_AgeIS_wild, get_diff_from_asoc) %>% setNames(., map_dbl(wild_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")
dfa_DistIS_AgeIS_wild <- map(comps_dfs_DistIS_AgeIS_wild, get_diff_from_asoc) %>% setNames(., map_dbl(wild_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")

dfa_wild <- purrr::list_rbind(list("noILVs" = dfa_noILVs_wild, "DistI" = dfa_DistI_wild, "DistIS" = dfa_DistIS_wild, "DistI_AgeIS" = dfa_DistI_AgeIS_wild, "DistIS_AgeIS" = dfa_DistIS_AgeIS_wild), names_to = "model")
names(dfa_wild)[3] <- "sig"

dfa %>%
  mutate(model = factor(model, levels = c("noILVs", "DistI", "DistIS", "DistI_AgeIS", "DistIS_AgeIS"))) %>%
  ggplot(aes(x = model, y = factor(carcID), fill = sig))+
  geom_tile()+
  scale_fill_manual(values = c("red", "skyblue"))+
  labs(y = NULL, x = "ILVs", fill = "Diff from\nasocial?")+
  theme_minimal() # interesting! some of the significance disappears, some appears

dfa_wild %>%
  mutate(model = factor(model, levels = c("noILVs", "DistI", "DistIS", "DistI_AgeIS", "DistIS_AgeIS"))) %>%
  ggplot(aes(x = model, y = factor(carcID), fill = sig))+
  geom_tile()+
  scale_fill_manual(values = c("red", "skyblue"))+
  labs(y = NULL, x = "ILVs", fill = "Diff from\nasocial?")+
  theme_minimal() # interesting! some of the significance disappears, some appears

ns <- map(data_lists, ~{
  found <- .x$N
  tot <- .x$P
  propfound <- found/tot
  return(data.frame(found = found, tot = tot, propfound = propfound))
})
names(ns) <- map_dbl(stn_carcs, "carcID")
nsdf <- purrr::list_rbind(ns, names_to = "carcID")

results <- data.frame(carcID = map_dbl(stn_carcs, "carcID"), pct_st = pct_st) %>%
  mutate(carcID = as.character(carcID)) %>%
  left_join(dfa, by = "carcID") %>%
  left_join(nsdf, by = "carcID") %>%
  rename("diff_from_asoc" = `.x[[i]]`) %>%
  mutate(carcID = as.numeric(carcID)) %>%
  left_join(purrr::list_rbind(stn_carcs), by = "carcID")

# sig and %ST by prop found
results %>%
  filter(diff_from_asoc) %>%
  ggplot(aes(x = propfound, y = pct_st))+
  geom_point(pch = 1, color = "blue")+
  geom_smooth(method = "lm", color = "blue")+
  theme_minimal()+
  labs(y = "%ST",
       x = "Prop. tagged vultures that found the carcass")+
  theme(text = element_text(size = 18))

results %>%
  filter(!is.na(diff_from_asoc)) %>%
  ggplot(aes(x = propfound, y = diff_from_asoc, color = diff_from_asoc))+
  geom_point(pch = 1)+
  theme_minimal()+
  labs(y = "ST detected?",
       x = "Prop. tagged vultures that found the carcass")+
  theme(text = element_text(size = 18),
        legend.position = "none")+
  scale_color_manual(values = c("red", "blue"))

# sig and %ST by n found
results %>%
  filter(diff_from_asoc) %>%
  ggplot(aes(x = found, y = pct_st))+
  geom_point(pch = 1, color = "blue")+
  geom_smooth(method = "lm", color = "blue")+
  theme_minimal()+
  labs(y = "%ST",
       x = "N tagged vultures that found the carcass")+
  theme(text = element_text(size = 18))

results %>%
  filter(!is.na(diff_from_asoc)) %>%
  ggplot(aes(x = found, y = diff_from_asoc, color = diff_from_asoc))+
  geom_point(pch = 1)+
  theme_minimal()+
  labs(y = "ST detected?",
       x = "N tagged vultures that found the carcass")+
  theme(text = element_text(size = 18),
        legend.position = "none")+
  scale_color_manual(values = c("red", "blue"))

# No relationship between number of vultures and social transmission probability. Yay!

results %>%
  filter(diff_from_asoc) %>%
  ggplot(aes(x = carcassWeight, y = pct_st))+
  geom_point(pch = 1, color = "blue")+
  theme_minimal()+
  geom_smooth(method = "lm", color = "blue")+
  labs(y = "%ST",
       x = "Carcass weight")+ # No relationship between carcass weight and %ST
  theme(text = element_text(size = 18))

# %ST by station (stations with >= 3 carcs)
results %>%
  group_by(stationName, diff_from_asoc) %>%
  summarize(n = n()) %>%
  ggplot(aes(fill=diff_from_asoc, y=n, x=stationName)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(labels = c("FALSE", "TRUE", "No Data"), values = c("red", "blue", "gray"), name = "Diff from asoc?")+
  theme_minimal()+
  coord_flip()+
  labs(y = "Carcasses", x = NULL)

# Flight vs. roost generally
sms <- map(social_fits_DistI_2nets, ~{
  if(!is.null(.x)){
    return(STb_summary(.x))
  }else{NULL}})
sms_df <- purrr::list_rbind(sms, names_to = "idx")

sms_df %>%
  filter(grepl("percent_ST", Parameter)) %>%
  mutate(Parameter = case_when(Parameter == "percent_ST[1]" ~ "%ST_roost",
                               Parameter == "percent_ST[2]" ~ "%ST_flight")) %>%
  ggplot(aes(x = idx, y = Median, color = Parameter))+
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, x = idx), width = 0, linewidth = 1, alpha = 0.5)+
  geom_point(pch = 21, fill = "white", size = 2)+
  theme_minimal()+
  coord_flip()+
  scale_color_manual(values = c("skyblue2", "darkgreen"))+
  labs(x = "Carcass (idx)",
       title = "Flight vs. roost, station carcs",
       y = "%ST estimate",
       color = "Network")+
  theme(text = element_text(size = 18))

# Are flight and roost networks correlated? It's hard to draw conclusions from the multi-network NBDA if they are.
library(vegan)
library(dplyr)
library(tidyr)
tar_load(networks_long_combined) # get the network data

make_matrix <- function(data, value_col) {
  mat <- data %>%
    select(focal, other, value = all_of(value_col)) %>%
    pivot_wider(names_from = other, values_from = value, values_fill = 0) %>%
    column_to_rownames("focal") %>%
    as.matrix()
  
  # Ensure square and symmetric with all individuals
  mat_full <- matrix(0, nrow = length(inds), ncol = length(inds),
                     dimnames = list(inds, inds))
  mat_full[rownames(mat), colnames(mat)] <- mat
  mat_sym <- pmax(mat_full, t(mat_full))  # symmetrise
  
  return(mat_sym)
}

mantel_by_time <- function(df, time_val) {
  
  d <- df %>% filter(time == time_val)
  
  # Get all unique individuals
  inds <- sort(unique(c(d$focal, d$other)))
  
  # Pivot each network variable to a matrix
  m1 <- make_matrix(d, "roost_together")
  m2 <- make_matrix(d, "flight_sri_scaled")
  
  # Check there's variance in both matrices -- Mantel fails if one is all zeros
  if (var(m1[lower.tri(m1)]) == 0 || var(m2[lower.tri(m2)]) == 0) {
    return(tibble(time = time_val, statistic = NA, p_value = NA, 
                  note = "no variance in one or both matrices"))
  }
  
  result <- mantel(m1, m2, method = "spearman", permutations = 999)
  
  tibble(
    time     = time_val,
    statistic = result$statistic,
    p_value   = result$signif,
    note      = "ok"
  )
}

your_data <- networks_long_combined[[1]]

# Run across all time steps
time_steps <- sort(unique(your_data$time))

results <- purrr::map_dfr(time_steps, ~ mantel_by_time(your_data, .x))
results %>%
  mutate(sig_corr = case_when(p_value >= 0.05 ~ F,
                              p_value < 0.05 ~ T, 
                              .default = NA)) %>%
  ggplot(aes(x = time, y = statistic, color = sig_corr))+
  geom_point()

# "The multi-network NBDA will work most effectively when the networks are independent. When they are highly dependent (e.g. correlated), it will require a lot of data to distinguish the effects of each network. This will be reflected in wide confidence intervals (CIs) for each s parameter, and for the estimated difference between them." Farine et al 2015

# Model averaging
inputs_stn <- list(
  m1 = social_fits_noILVs,
  m2 = social_fits_DistI,
  m3 = social_fits_DistI_AgeIS,
  m4 = social_fits_DistIS,
  m5 = social_fits_DistIS_AgeIS
  #,
  # ex1 = exclude_struct1,
  # ex2 = exclude_struct2,
  # ex3 = exclude_struct3,
  # ex4 = exclude_struct4
)

all_beta_names <- c("beta_ILVi_mean_dist_to_carcass_norm", 
                    "beta_ILVs_mean_dist_to_carcass_norm", 
                    "beta_ILVi_age[1]",
                    "beta_ILVs_age[1]",
                    "beta_ILVi_age[2]",
                    "beta_ILVs_age[2]")

model_averaged_params_stn <- pmap(inputs_stn, function(m1, m2, m3, m4, m5#, 
                                                       #ex1, ex2, ex3, ex4
) {
  
  all_models <- list(m1, m2, m3, m4, m5)
  #exclude <- c(ex1, ex2, ex3, ex4)
  models <- all_models#[!exclude]
  
  # return NULL if all models are NULL
  if (all(map_lgl(all_models, is.null))) return(NULL)
  
  # get beta names available in surviving models
  available_draws <- models[[1]]$draws() %>% as_draws_matrix()
  available_betas <- models %>%
    map(~colnames(.x$draws() %>% as_draws_matrix())) %>%
    reduce(union) %>%
    intersect(all_beta_names) # XXX START HERE 6/5
  
  weighted_average <- function(var) {
    draws <- map(models, ~.x$draws(var) %>% as_draws_matrix())
    reduce(
      seq_along(models),
      function(acc, j) acc + weights[j] * draws[[j]],
      .init = matrix(0, nrow = nrow(draws[[1]]), ncol = ncol(draws[[1]]))
    )
  }
  
  if (length(models) == 1) {
    weights <- NULL  # not needed but keeps structure consistent
    result <- map(all_beta_names, ~{
      if (.x %in% available_betas) {
        models[[1]]$draws(.x) %>% as_draws_matrix()
      } else {
        NA
      }
    }) %>% setNames(all_beta_names)
    return(c(list(percent_ST = models[[1]]$draws("percent_ST") %>% as_draws_matrix()), result))
  }
  
  loos <- map(models, ~suppressWarnings(.x$loo()))
  weights <- loo_model_weights(loos, method = "stacking")
  
  result <- map(all_beta_names, ~{
    if (.x %in% available_betas) {
      weighted_average(.x)
    } else {
      NA
    }
  }) %>% setNames(all_beta_names)
  
  c(list(percent_ST = weighted_average("percent_ST")), result)
})
