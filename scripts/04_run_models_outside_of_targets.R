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
plan(multisession, workers = 10)
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

# # Get data lists (wild)
# tar_load(data_lists_noILVs_2nets_wild)
# tar_load(data_lists_DistI_2nets_wild)
# tar_load(data_lists_DistIS_2nets_wild)
# tar_load(data_lists_DistI_AgeIS_2nets_wild)
# tar_load(data_lists_DistIS_AgeIS_2nets_wild)

# Get carcass data (stn and wild)
tar_load(stn_carcs_modified)
#tar_load(wild_carcs)

# # Load model objects ----------------------------------------------------
# Asocial stn, 2nets
tar_load(asocial_mods_noILVs_2nets)
tar_load(asocial_mods_DistI_2nets)
tar_load(asocial_mods_DistIS_2nets)
tar_load(asocial_mods_DistI_AgeIS_2nets)
tar_load(asocial_mods_DistIS_AgeIS_2nets)
# 
# # Asocial wild, 2nets
# tar_load(asocial_mods_noILVs_2nets_wild)
# tar_load(asocial_mods_DistI_2nets_wild)
# tar_load(asocial_mods_DistIS_2nets_wild)
# tar_load(asocial_mods_DistI_AgeIS_2nets_wild)
# tar_load(asocial_mods_DistIS_AgeIS_2nets_wild)
# 
# Social stn, 2nets
tar_load(social_mods_noILVs_2nets)
tar_load(social_mods_DistI_2nets)
tar_load(social_mods_DistIS_2nets)
tar_load(social_mods_DistI_AgeIS_2nets)
tar_load(social_mods_DistIS_AgeIS_2nets)
# 
# # Social wild, 2nets
# tar_load(social_mods_noILVs_2nets_wild)
# tar_load(social_mods_DistI_2nets_wild)
# tar_load(social_mods_DistIS_2nets_wild)
# tar_load(social_mods_DistI_AgeIS_2nets_wild)
# tar_load(social_mods_DistIS_AgeIS_2nets_wild)
#
# # Fit models --------------------------------------------------------------
# # Fit and save social (stn)
# social_fits_noILVs_2nets <- with_progress(furrr::future_map2(social_mods_noILVs_2nets, data_lists_noILVs_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(social_fits_noILVs_2nets, ~{savefit(.x, .y, folder = "NoILVs_2nets", prefix = "social", type = "station")}) # noILVs
# 
# social_fits_DistI_2nets <- with_progress(furrr::future_map2(social_mods_DistI_2nets, data_lists_DistI_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(social_fits_DistI_2nets, ~{savefit(.x, .y, folder = "DistI_2nets", prefix = "social", type = "station")}) # DistI
# 
# social_fits_DistIS_2nets <- with_progress(furrr::future_map2(social_mods_DistIS_2nets, data_lists_DistIS_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(social_fits_DistIS_2nets, ~{savefit(.x, .y, folder = "DistIS_2nets", prefix = "social", type = "station")}) # DistIS

social_fits_DistI_AgeIS_2nets <- with_progress(furrr::future_map2(social_mods_DistI_AgeIS_2nets, data_lists_DistI_AgeIS_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
iwalk(social_fits_DistI_AgeIS_2nets, ~{savefit(.x, .y, folder = "DistI_AgeIS_2nets", prefix = "social", type = "station")}) # DistI_AgeIS

# social_fits_DistIS_AgeIS_2nets <- with_progress(furrr::future_map2(social_mods_DistIS_AgeIS_2nets, data_lists_DistIS_AgeIS_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(social_fits_DistIS_AgeIS_2nets, ~{savefit(.x, .y, folder = "DistIS_AgeIS_2nets", prefix = "social", type = "station")}) # DistIS_AgeIS
# 
# # Fit and save asocial (stn)
# asocial_fits_noILVs_2nets <- with_progress(furrr::future_map2(asocial_mods_noILVs_2nets, data_lists_noILVs_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(asocial_fits_noILVs_2nets, ~{savefit(.x, .y, folder = "NoILVs_2nets", prefix = "asocial", type = "station")}) # noILVs
# 
# asocial_fits_DistI_2nets <- with_progress(furrr::future_map2(asocial_mods_DistI_2nets, data_lists_DistI_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(asocial_fits_DistI_2nets, ~{savefit(.x, .y, folder = "DistI_2nets", prefix = "asocial", type = "station")}) # DistI
# 
# asocial_fits_DistIS_2nets <- with_progress(furrr::future_map2(asocial_mods_DistIS_2nets, data_lists_DistIS_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(asocial_fits_DistIS_2nets, ~{savefit(.x, .y, folder = "DistIS_2nets", prefix = "asocial", type = "station")}) # DistIS
# 
# asocial_fits_DistI_AgeIS_2nets <- with_progress(furrr::future_map2(asocial_mods_DistI_AgeIS_2nets, data_lists_DistI_AgeIS_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(asocial_fits_DistI_AgeIS_2nets, ~{savefit(.x, .y, folder = "DistI_AgeIS_2nets", prefix = "asocial", type = "station")}) # DistI_AgeIS
# 
# asocial_fits_DistIS_AgeIS_2nets <- with_progress(furrr::future_map2(asocial_mods_DistIS_AgeIS_2nets, data_lists_DistIS_AgeIS_2nets, ~fit_model(.x, .y, nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(asocial_fits_DistIS_AgeIS_2nets, ~{savefit(.x, .y, folder = "DistIS_AgeIS_2nets", prefix = "asocial", type = "station")}) # DistIS_AgeIS

# # Fit and save social (wild)
# social_fits_noILVs_2nets_wild <- with_progress(furrr::future_map2(social_mods_noILVs_2nets_wild, data_lists_noILVs_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(social_fits_noILVs_2nets_wild, ~{savefit(.x, .y, folder = "NoILVs_2nets", prefix = "social", type = "wild")}) # noILVs
# 
# social_fits_DistI_2nets_wild <- with_progress(furrr::future_map2(social_mods_DistI_2nets_wild, data_lists_DistI_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(social_fits_DistI_2nets_wild, ~{savefit(.x, .y, folder = "DistI_2nets", prefix = "social", type = "wild")}) # DistI
# 
# social_fits_DistIS_2nets_wild <- with_progress(furrr::future_map2(social_mods_DistIS_2nets_wild, data_lists_DistIS_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(social_fits_DistIS_2nets_wild, ~{savefit(.x, .y, folder = "DistIS_2nets", prefix = "social", type = "wild")}) # DistIS
# 
# social_fits_DistI_AgeIS_2nets_wild <- with_progress(furrr::future_map2(social_mods_DistI_AgeIS_2nets_wild, data_lists_DistI_AgeIS_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(social_fits_DistI_AgeIS_2nets_wild, ~{savefit(.x, .y, folder = "DistI_AgeIS_2nets", prefix = "social", type = "wild")}) # DistI_AgeIS
# 
# social_fits_DistIS_AgeIS_2nets_wild <- with_progress(furrr::future_map2(social_mods_DistIS_AgeIS_2nets_wild, data_lists_DistIS_AgeIS_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(social_fits_DistIS_AgeIS_2nets_wild, ~{savefit(.x, .y, folder = "DistIS_AgeIS_2nets", prefix = "social", type = "wild")}) # DistIS_AgeIS
# 
# # Fit and save asocial (wild)
# asocial_fits_noILVs_2nets_wild <- with_progress(furrr::future_map2(asocial_mods_noILVs_2nets_wild, data_lists_noILVs_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(asocial_fits_noILVs_2nets_wild, ~{savefit(.x, .y, folder = "NoILVs_2nets", prefix = "asocial", type = "wild")}) # noILVs
# 
# asocial_fits_DistI_2nets_wild <- with_progress(furrr::future_map2(asocial_mods_DistI_2nets_wild, data_lists_DistI_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(asocial_fits_DistI_2nets_wild, ~{savefit(.x, .y, folder = "DistI_2nets", prefix = "asocial", type = "wild")}) # DistI
# 
# asocial_fits_DistIS_2nets_wild <- with_progress(furrr::future_map2(asocial_mods_DistIS_2nets_wild, data_lists_DistIS_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(asocial_fits_DistIS_2nets_wild, ~{savefit(.x, .y, folder = "DistIS_2nets", prefix = "asocial", type = "wild")}) # DistIS
# 
# asocial_fits_DistI_AgeIS_2nets_wild <- with_progress(furrr::future_map2(asocial_mods_DistI_AgeIS_2nets_wild, data_lists_DistI_AgeIS_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(asocial_fits_DistI_AgeIS_2nets_wild, ~{savefit(.x, .y, folder = "DistI_AgeIS_2nets", prefix = "asocial", type = "wild")}) # DistI_AgeIS
# 
# asocial_fits_DistIS_AgeIS_2nets_wild <- with_progress(furrr::future_map2(asocial_mods_DistIS_AgeIS_2nets_wild, data_lists_DistIS_AgeIS_2nets_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
# iwalk(asocial_fits_DistIS_AgeIS_2nets_wild, ~{savefit(.x, .y, folder = "DistIS_AgeIS_2nets", prefix = "asocial", type = "wild")}) # DistIS_AgeIS