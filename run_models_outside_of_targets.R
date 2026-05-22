library(targets)
library(future)
library(furrr)
library(progressr)
library(tidyverse)
library(STbayes)
library(sf)
plan(multisession, workers = 30)
handlers(global = TRUE)
nit <- 500

# Get data
tar_load(event_data)
tar_load(event_data_wild)
tar_load(data_lists_noILVs)
tar_load(data_lists_DistI)
tar_load(data_lists_DistIS)
tar_load(data_lists_DistI_AgeIS)
tar_load(data_lists_DistIS_AgeIS)

tar_load(data_lists_noILVs_2nets)
tar_load(data_lists_DistI_2nets)
tar_load(data_lists_DistIS_2nets)
tar_load(data_lists_DistI_AgeIS_2nets)
tar_load(data_lists_DistIS_AgeIS_2nets)

tar_load(data_lists_noILVs_wild)
tar_load(data_lists_DistI_wild)
tar_load(data_lists_DistIS_wild)
tar_load(data_lists_DistI_AgeIS_wild)
tar_load(data_lists_DistIS_AgeIS_wild)

# tar_load(data_lists_noILVs_2nets_wild)
# tar_load(data_lists_DistI_2nets_wild)
# tar_load(data_lists_DistIS_2nets_wild)
# tar_load(data_lists_DistI_AgeIS_2nets_wild)
# tar_load(data_lists_DistIS_AgeIS_2nets_wild)

tar_load(stn_carcs)
tar_load(wild_carcs)

# Helper funs
get_asocial <- function(x){
  if(!is.null(x)){
    mod <- suppressMessages(STbayes::generate_STb_model(x, gq = T, est_acqTime = T, model_type = "asocial"))
    return(mod)
  }else{return(NULL)}}

get_social <- function(x){
  if(!is.null(x)){
    mod <- suppressMessages(STbayes::generate_STb_model(x, gq = T, est_acqTime = T))
    return(mod)
  }else{return(NULL)}
}

fit_model <- function(mod, dl, n_iter = 1000){
  if(!is.null(mod)){
    social_fit <- fit_STb(dl, mod, iter = n_iter)
    return(social_fit)
  }else{return(NULL)}
}

savefit <- function(fit, idx, folder, prefix, type){
  nm <- paste0(folder, "/fit_", prefix, "_", str_pad(as.character(idx), width = 3, side = "left", pad = "0"))
  if(!is.null(fit)){
    STb_save(fit, output_dir = paste0("data/saved_fits/", type, "/"), name = nm)
  }else{
    write_rds(NULL, file = paste0("data/saved_fits/", type, "/", nm, ".rds"))
  }
}

# Create model objects ----------------------------------------------------
# Asocial station, flight only
asocial_mods_noILVs <- purrr::map(data_lists_noILVs, get_asocial)
asocial_mods_DistI <- purrr::map(data_lists_DistI, get_asocial)
asocial_mods_DistIS <- purrr::map(data_lists_DistIS, get_asocial)
asocial_mods_DistI_AgeIS <- purrr::map(data_lists_DistI_AgeIS, get_asocial)
asocial_mods_DistIS_AgeIS <- purrr::map(data_lists_DistIS_AgeIS, get_asocial)

# Asocial wild, flight only
asocial_mods_noILVs_wild <- purrr::map(data_lists_noILVs_wild, get_asocial)
asocial_mods_DistI_wild <- purrr::map(data_lists_DistI_wild, get_asocial)
asocial_mods_DistIS_wild <- purrr::map(data_lists_DistIS_wild, get_asocial)
asocial_mods_DistI_AgeIS_wild <- purrr::map(data_lists_DistI_AgeIS_wild, get_asocial)
asocial_mods_DistIS_AgeIS_wild <- purrr::map(data_lists_DistIS_AgeIS_wild, get_asocial)

# Social station, flight only
social_mods_noILVs <- purrr::map(data_lists_noILVs, get_social)
social_mods_DistI <- purrr::map(data_lists_DistI, get_social)
## test complex transmission for a few
social_mods_DistI_complex_test <- purrr::map(data_lists_DistI[1:3], ~STbayes::generate_STb_model(.x, gq = T, est_acqTime = T, transmission_func = "freqdep_f"))
social_mods_DistIS <- purrr::map(data_lists_DistIS, get_social)
social_mods_DistI_AgeIS <- purrr::map(data_lists_DistI_AgeIS, get_social)
social_mods_DistIS_AgeIS <- purrr::map(data_lists_DistIS_AgeIS, get_social)

# Social wild, flight only
social_mods_noILVs_wild <- purrr::map(data_lists_noILVs_wild, get_social)
social_mods_DistI_wild <- purrr::map(data_lists_DistI_wild, get_social)
social_mods_DistIS_wild <- purrr::map(data_lists_DistIS_wild, get_social)
social_mods_DistI_AgeIS_wild <- purrr::map(data_lists_DistI_AgeIS_wild, get_social)
social_mods_DistIS_AgeIS_wild <- purrr::map(data_lists_DistIS_AgeIS_wild, get_social)

# Asocial station, 2nets
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

# Social station, 2nets
social_mods_noILVs_2nets <- purrr::map(data_lists_noILVs_2nets, get_social)
social_mods_DistI_2nets <- purrr::map(data_lists_DistI_2nets, get_social)
## test complex transmission for a few
social_mods_DistI_complex_test_2nets <- purrr::map(data_lists_DistI_2nets[1:3], ~STbayes::generate_STb_model(.x, gq = T, est_acqTime = T, transmission_func = "freqdep_f"))
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
## Station
# Fit and save social
social_fits_noILVs <- with_progress(furrr::future_map2(social_mods_noILVs, data_lists_noILVs, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_noILVs, 1:length(social_fits_noILVs), ~{savefit(.x, .y, folder = "NoILVs", prefix = "social", type = "station")})

social_fits_noILVs_2nets <- with_progress(furrr::future_map2(social_mods_noILVs_2nets, data_lists_noILVs_2nets, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_noILVs_2nets, 1:length(social_fits_noILVs_2nets), ~{savefit(.x, .y, folder = "NoILVs_2nets", prefix = "social", type = "station")})

social_fits_DistI <- with_progress(furrr::future_map2(social_mods_DistI, data_lists_DistI, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_DistI, 1:length(social_fits_DistI), ~{savefit(.x, .y, folder = "DistI", prefix = "social", type = "station")})

social_fits_DistI_2nets <- with_progress(furrr::future_map2(social_mods_DistI_2nets, data_lists_DistI_2nets, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_DistI_2nets, 1:length(social_fits_DistI_2nets), ~{savefit(.x, .y, folder = "DistI_2nets", prefix = "social", type = "station")})

social_fits_DistI_compl <- with_progress(furrr::future_map2(social_mods_DistI_complex_test, data_lists_DistI[1:3], ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_DistI_compl, 1:length(social_fits_DistI_compl), ~{savefit(.x, .y, folder = "DistI", prefix = "social_complex_", type = "station")})

social_fits_DistIS <- with_progress(furrr::future_map2(social_mods_DistIS, data_lists_DistIS, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_DistIS, 1:length(social_fits_DistIS), ~{savefit(.x, .y, folder = "DistIS", prefix = "social", type = "station")})

social_fits_DistIS_2nets <- with_progress(furrr::future_map2(social_mods_DistIS_2nets, data_lists_DistIS_2nets, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_DistIS_2nets, 1:length(social_fits_DistIS_2nets), ~{savefit(.x, .y, folder = "DistIS_2nets", prefix = "social", type = "station")})

social_fits_DistI_AgeIS <- with_progress(furrr::future_map2(social_mods_DistI_AgeIS, data_lists_DistI_AgeIS, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_DistI_AgeIS, 1:length(social_fits_DistI_AgeIS), ~{savefit(.x, .y, folder = "DistI_AgeIS", prefix = "social", type = "station")})

social_fits_DistI_AgeIS_2nets <- with_progress(furrr::future_map2(social_mods_DistI_AgeIS_2nets, data_lists_DistI_AgeIS_2nets, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_DistI_AgeIS_2nets, 1:length(social_fits_DistI_AgeIS_2nets), ~{savefit(.x, .y, folder = "DistI_AgeIS_2nets", prefix = "social", type = "station")})

social_fits_DistIS_AgeIS <- with_progress(furrr::future_map2(social_mods_DistIS_AgeIS, data_lists_DistIS_AgeIS, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_DistIS_AgeIS, 1:length(social_fits_DistIS_AgeIS), ~{savefit(.x, .y, folder = "DistIS_AgeIS", prefix = "social", type = "station")})

social_fits_DistIS_AgeIS_2nets <- with_progress(furrr::future_map2(social_mods_DistIS_AgeIS_2nets, data_lists_DistIS_AgeIS_2nets, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_DistIS_AgeIS_2nets, 1:length(social_fits_DistIS_AgeIS_2nets), ~{savefit(.x, .y, folder = "DistIS_AgeIS_2nets", prefix = "social", type = "station")})

# Fit and save asocial
asocial_fits_noILVs <- with_progress(furrr::future_map2(asocial_mods_noILVs, data_lists_noILVs, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(asocial_fits_noILVs, 1:length(asocial_fits_noILVs), ~{savefit(.x, .y, folder = "NoILVs", prefix = "asocial", type = "station")})

asocial_fits_DistI <- with_progress(furrr::future_map2(asocial_mods_DistI, data_lists_DistI, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(asocial_fits_DistI, 1:length(asocial_fits_DistI), ~{savefit(.x, .y, folder = "DistI", prefix = "asocial", type = "station")})

asocial_fits_DistIS <- with_progress(furrr::future_map2(asocial_mods_DistIS, data_lists_DistIS, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(asocial_fits_DistIS, 1:length(asocial_fits_DistIS), ~{savefit(.x, .y, folder = "DistIS", prefix = "asocial", type = "station")})

asocial_fits_DistI_AgeIS <- with_progress(furrr::future_map2(asocial_mods_DistI_AgeIS, data_lists_DistI_AgeIS, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(asocial_fits_DistI_AgeIS, 1:length(asocial_fits_DistI_AgeIS), ~{savefit(.x, .y, folder = "DistI_AgeIS", prefix = "asocial", type = "station")})

asocial_fits_DistIS_AgeIS <- with_progress(furrr::future_map2(asocial_mods_DistIS_AgeIS, data_lists_DistIS_AgeIS, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(asocial_fits_DistIS_AgeIS, 1:length(asocial_fits_DistIS_AgeIS), ~{savefit(.x, .y, folder = "DistIS_AgeIS", prefix = "asocial", type = "station")})

## Wild
# Fit and save social
social_fits_noILVs_wild <- with_progress(furrr::future_map2(social_mods_noILVs_wild, data_lists_noILVs_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_noILVs_wild, 1:length(social_fits_noILVs_wild), ~{savefit(.x, .y, folder = "NoILVs", prefix = "social", type = "wild")})

social_fits_DistI_wild <- with_progress(furrr::future_map2(social_mods_DistI_wild, data_lists_DistI_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_DistI_wild, 1:length(social_fits_DistI_wild), ~{savefit(.x, .y, folder = "DistI", prefix = "social", type = "wild")})

social_fits_DistIS_wild <- with_progress(furrr::future_map2(social_mods_DistIS_wild, data_lists_DistIS_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_DistIS_wild, 1:length(social_fits_DistIS_wild), ~{savefit(.x, .y, folder = "DistIS", prefix = "social", type = "wild")})

social_fits_DistI_AgeIS_wild <- with_progress(furrr::future_map2(social_mods_DistI_AgeIS_wild, data_lists_DistI_AgeIS_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_DistI_AgeIS_wild, 1:length(social_fits_DistI_AgeIS_wild), ~{savefit(.x, .y, folder = "DistI_AgeIS", prefix = "social", type = "wild")})

social_fits_DistIS_AgeIS_wild <- with_progress(furrr::future_map2(social_mods_DistIS_AgeIS_wild, data_lists_DistIS_AgeIS_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(social_fits_DistIS_AgeIS_wild, 1:length(social_fits_DistIS_AgeIS_wild), ~{savefit(.x, .y, folder = "DistIS_AgeIS", prefix = "social", type = "wild")})

# Fit and save asocial
asocial_fits_noILVs_wild <- with_progress(furrr::future_map2(asocial_mods_noILVs_wild, data_lists_noILVs_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(asocial_fits_noILVs_wild, 1:length(asocial_fits_noILVs_wild), ~{savefit(.x, .y, folder = "NoILVs", prefix = "asocial", type = "wild")})

asocial_fits_DistI_wild <- with_progress(furrr::future_map2(asocial_mods_DistI_wild, data_lists_DistI_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(asocial_fits_DistI_wild, 1:length(asocial_fits_DistI_wild), ~{savefit(.x, .y, folder = "DistI", prefix = "asocial", type = "wild")})

asocial_fits_DistIS_wild <- with_progress(furrr::future_map2(asocial_mods_DistIS_wild, data_lists_DistIS_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(asocial_fits_DistIS_wild, 1:length(asocial_fits_DistIS_wild), ~{savefit(.x, .y, folder = "DistIS", prefix = "asocial", type = "wild")})

asocial_fits_DistI_AgeIS_wild <- with_progress(furrr::future_map2(asocial_mods_DistI_AgeIS_wild, data_lists_DistI_AgeIS_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(asocial_fits_DistI_AgeIS_wild, 1:length(asocial_fits_DistI_AgeIS_wild), ~{savefit(.x, .y, folder = "DistI_AgeIS", prefix = "asocial", type = "wild")})

asocial_fits_DistIS_AgeIS_wild <- with_progress(furrr::future_map2(asocial_mods_DistIS_AgeIS_wild, data_lists_DistIS_AgeIS_wild, ~fit_model(.x, .y, n_iter = nit), .options = furrr_options(seed = TRUE), .progress = T))
walk2(asocial_fits_DistIS_AgeIS_wild, 1:length(asocial_fits_DistIS_AgeIS_wild), ~{savefit(.x, .y, folder = "DistIS_AgeIS", prefix = "asocial", type = "wild")})

#Get filenames
## Station social
soc_filenames_noILVs <- list.files(path = "data/saved_fits/station/NoILVs/", pattern = "fit_social")
soc_filenames_noILVs_2nets <- list.files(path = "data/saved_fits/station/NoILVs_2nets/", pattern = "fit_social")
soc_filenames_DistI <- list.files(path = "data/saved_fits/station/DistI/", pattern = "fit_social")
soc_filenames_DistI_2nets <- list.files(path = "data/saved_fits/station/DistI_2nets/", pattern = "fit_social")
soc_filenames_DistI_compl <- list.files(path = "data/saved_fits/station/DistI/", pattern = "fit_social_complex_")
soc_filenames_DistIS <- list.files(path = "data/saved_fits/station/DistIS/", pattern = "fit_social")
soc_filenames_DistIS_2nets <- list.files(path = "data/saved_fits/station/DistIS_2nets/", pattern = "fit_social")
soc_filenames_DistI_AgeIS <- list.files(path = "data/saved_fits/station/DistI_AgeIS/", pattern = "fit_social")
soc_filenames_DistI_AgeIS_2nets <- list.files(path = "data/saved_fits/station/DistI_AgeIS_2nets/", pattern = "fit_social")
soc_filenames_DistIS_AgeIS <- list.files(path = "data/saved_fits/station/DistIS_AgeIS/", pattern = "fit_social")
soc_filenames_DistIS_AgeIS_2nets <- list.files(path = "data/saved_fits/station/DistIS_AgeIS_2nets/", pattern = "fit_social")

## Station asocial
asoc_filenames_noILVs <- list.files(path = "data/saved_fits/station/NoILVs/", pattern = "fit_asocial")
asoc_filenames_DistI <- list.files(path = "data/saved_fits/station/DistI/", pattern = "fit_asocial")
asoc_filenames_DistIS <- list.files(path = "data/saved_fits/station/DistIS/", pattern = "fit_asocial")
asoc_filenames_DistI_AgeIS <- list.files(path = "data/saved_fits/station/DistI_AgeIS/", pattern = "fit_asocial")
asoc_filenames_DistIS_AgeIS <- list.files(path = "data/saved_fits/station/DistIS_AgeIS/", pattern = "fit_asocial")

## Wild social
soc_filenames_noILVs_wild <- list.files(path = "data/saved_fits/wild/NoILVs/", pattern = "fit_social")
soc_filenames_DistI_wild <- list.files(path = "data/saved_fits/wild/DistI/", pattern = "fit_social")
soc_filenames_DistIS_wild <- list.files(path = "data/saved_fits/wild/DistIS/", pattern = "fit_social")
soc_filenames_DistI_AgeIS_wild <- list.files(path = "data/saved_fits/wild/DistI_AgeIS/", pattern = "fit_social")
soc_filenames_DistIS_AgeIS_wild <- list.files(path = "data/saved_fits/wild/DistIS_AgeIS/", pattern = "fit_social")

## Wild asocial
asoc_filenames_noILVs_wild <- list.files(path = "data/saved_fits/wild/NoILVs/", pattern = "fit_asocial")
asoc_filenames_DistI_wild <- list.files(path = "data/saved_fits/wild/DistI/", pattern = "fit_asocial")
asoc_filenames_DistIS_wild <- list.files(path = "data/saved_fits/wild/DistIS/", pattern = "fit_asocial")
asoc_filenames_DistI_AgeIS_wild <- list.files(path = "data/saved_fits/wild/DistI_AgeIS/", pattern = "fit_asocial")
asoc_filenames_DistIS_AgeIS_wild <- list.files(path = "data/saved_fits/wild/DistIS_AgeIS/", pattern = "fit_asocial")

# Read in fits
## Station social
social_fits_noILVs <- map(soc_filenames_noILVs, ~readRDS(paste0("data/saved_fits/station/NoILVs/", .x)))
social_fits_noILVs_2nets <- map(soc_filenames_noILVs_2nets, ~readRDS(paste0("data/saved_fits/station/NoILVs_2nets/", .x)))
social_fits_DistI <- map(soc_filenames_DistI, ~readRDS(paste0("data/saved_fits/station/DistI/", .x)))
social_fits_DistI_2nets <- map(soc_filenames_DistI_2nets, ~readRDS(paste0("data/saved_fits/station/DistI_2nets/", .x)))
social_fits_DistIS <- map(soc_filenames_DistIS, ~readRDS(paste0("data/saved_fits/station/DistIS/", .x)))
social_fits_DistIS_2nets <- map(soc_filenames_DistIS_2nets, ~readRDS(paste0("data/saved_fits/station/DistIS_2nets/", .x)))
social_fits_DistI_AgeIS <- map(soc_filenames_DistI_AgeIS, ~readRDS(paste0("data/saved_fits/station/DistI_AgeIS/", .x)))
social_fits_DistI_AgeIS_2nets <- map(soc_filenames_DistI_AgeIS_2nets, ~readRDS(paste0("data/saved_fits/station/DistI_AgeIS_2nets/", .x)))
social_fits_DistIS_AgeIS <- map(soc_filenames_DistIS_AgeIS, ~readRDS(paste0("data/saved_fits/station/DistIS_AgeIS/", .x)))
social_fits_DistIS_AgeIS_2nets <- map(soc_filenames_DistIS_AgeIS_2nets, ~readRDS(paste0("data/saved_fits/station/DistIS_AgeIS_2nets/", .x)))

## Station asocial
asocial_fits_noILVs <- map(asoc_filenames_noILVs, ~readRDS(paste0("data/saved_fits/station/NoILVs/", .x)))
asocial_fits_DistI <- map(asoc_filenames_DistI, ~readRDS(paste0("data/saved_fits/station/DistI/", .x)))
asocial_fits_DistIS <- map(asoc_filenames_DistIS, ~readRDS(paste0("data/saved_fits/station/DistIS/", .x)))
asocial_fits_DistI_AgeIS <- map(asoc_filenames_DistI_AgeIS, ~readRDS(paste0("data/saved_fits/station/DistI_AgeIS/", .x)))
asocial_fits_DistIS_AgeIS <- map(asoc_filenames_DistIS_AgeIS, ~readRDS(paste0("data/saved_fits/station/DistIS_AgeIS/", .x)))

social_fits_noILVs_wild <- map(soc_filenames_noILVs_wild, ~readRDS(paste0("data/saved_fits/wild/NoILVs/", .x)))
social_fits_DistI_wild <- map(soc_filenames_DistI_wild, ~readRDS(paste0("data/saved_fits/wild/DistI/", .x)))
social_fits_DistIS_wild <- map(soc_filenames_DistIS_wild, ~readRDS(paste0("data/saved_fits/wild/DistIS/", .x)))
social_fits_DistI_AgeIS_wild <- map(soc_filenames_DistI_AgeIS_wild, ~readRDS(paste0("data/saved_fits/wild/DistI_AgeIS/", .x)))
social_fits_DistIS_AgeIS_wild <- map(soc_filenames_DistIS_AgeIS_wild, ~readRDS(paste0("data/saved_fits/wild/DistIS_AgeIS/", .x)))

asocial_fits_noILVs_wild <- map(asoc_filenames_noILVs_wild, ~readRDS(paste0("data/saved_fits/wild/NoILVs/", .x)))
asocial_fits_DistI_wild <- map(asoc_filenames_DistI_wild, ~readRDS(paste0("data/saved_fits/wild/DistI/", .x)))
asocial_fits_DistIS_wild <- map(asoc_filenames_DistIS_wild, ~readRDS(paste0("data/saved_fits/wild/DistIS/", .x)))
asocial_fits_DistI_AgeIS_wild <- map(asoc_filenames_DistI_AgeIS_wild, ~readRDS(paste0("data/saved_fits/wild/DistI_AgeIS/", .x)))
asocial_fits_DistIS_AgeIS_wild <- map(asoc_filenames_DistIS_AgeIS_wild, ~readRDS(paste0("data/saved_fits/wild/DistIS_AgeIS/", .x)))

# Inspect Rhat values (wild)
summs_noILVs_wild <- map(social_fits_noILVs_wild, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistI_wild <- map(social_fits_DistI_wild, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistIS_wild <- map(social_fits_DistIS_wild, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistI_AgeIS_wild <- map(social_fits_DistI_AgeIS_wild, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistIS_AgeIS_wild <- map(social_fits_DistIS_AgeIS_wild, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")

summs_wild <- purrr::list_rbind(list("noILVs" = summs_noILVs_wild, "DistI" = summs_DistI_wild, "DistIS" = summs_DistIS_wild, "DistI_AgeIS" = summs_DistI_AgeIS_wild, "DistIS_AgeIS" = summs_DistIS_AgeIS_wild), names_to = "model") %>% mutate(type = "wild")

summs_noILVs <- map(social_fits_noILVs, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistI <- map(social_fits_DistI, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistIS <- map(social_fits_DistIS, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistI_AgeIS <- map(social_fits_DistI_AgeIS, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")
summs_DistIS_AgeIS <- map(social_fits_DistIS_AgeIS, ~{if(!is.null(.x)){STb_summary(.x)}else{NULL}}) %>% purrr::list_rbind(names_to = "idx")

summs <- purrr::list_rbind(list("noILVs" = summs_noILVs, "DistI" = summs_DistI, "DistIS" = summs_DistIS, "DistI_AgeIS" = summs_DistI_AgeIS, "DistIS_AgeIS" = summs_DistIS_AgeIS), names_to = "model") %>% mutate(type = "stn")

hist(summs_wild$Rhat) # seems like all the rhat values are totally fine here. I don't see anything much below 1 or over 1.1. I am also planning to run more chains.
# Maybe some of them over 1.01 are too high. Let's look at the ppc curves and see if they match the bad rhat values.
hist(summs$Rhat)

# Are the Rhat values different for different parameters?
summs %>%
  ggplot(aes(x = Rhat))+
  geom_density(aes(color = Parameter))+
  theme_minimal()+
  theme(legend.position = "bottom")

summs_wild %>%
  ggplot(aes(x = Rhat))+
  geom_density(aes(color = Parameter))+
  theme_minimal()+
  theme(legend.position = "bottom") # looks like they are all fairly similar. If anything, the rhats tend to be slightly higher for some of the age betas.

# What about for different models?
summs %>%
  mutate(model = factor(model, levels = c("noILVs", "DistI", "DistIS", "DistI_AgeIS", "DistIS_AgeIS"))) %>%
  ggplot(aes(x = Rhat))+
  geom_density(aes(color = Parameter))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  facet_wrap(~model)

summs_wild %>%
  mutate(model = factor(model, levels = c("noILVs", "DistI", "DistIS", "DistI_AgeIS", "DistIS_AgeIS"))) %>%
  ggplot(aes(x = Rhat))+
  geom_density(aes(color = Parameter))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  facet_wrap(~model) # I guess it gets a little wonky in the more complicated models, but overall this just doesn't look too bad.

# Are some of the bad model fits related to number of individuals, number of seeds, etc.?
stn_categories <- c(1, 1, 2, 0, 1, 2, NA, NA, 1, 2, 0, NA, NA, 1, 0, NA, NA, 0, 0, 0, 2, 2, 0, 1, 1, 1, 0, 0, 2, 1, 1, NA, 1, 2, 0, 2, 1, 2, 1, 2, 2, 1, 0, 0, 1, 1, 2, 0, 1, 2, 2, 0, 1, 1, 1, 2, 2, 1, 0, 2)
wild_categories <- c(0, 1, 0, 0, 2, 2, 0, NA, 2, 0, 1, 0, NA, NA, 0, 0, NA, 0, 0, 1, 0, 2, 2, 0, 1, 0, 2, 0, 1, 1, 2, 1, 1, NA, 1, 0, 2, 0, 1, 0, 1, NA, 1, 1, 0, 1, 2, 0, 2, 1, 1, 1, 1, 2, 1, 1, 0, 1, 2, 2, NA, 2, 2, 1, 1, 1, 1, 1, 0, 0, 0, 2, 2, 2, 1, 1, 1, 1, 2, 1, NA, 0, NA, 1, 0, 2, 1, 2, 2, 1, 1, 1, 0, 1, 0, 2, 0, 1, 0, 0, 0, 0, NA, NA, 0, 1, 0, 1, 1, 2, NA, 1)

# Compare to characteristics of the carcasses
stn_categories_nona <- stn_categories[!is.na(stn_categories)]
wild_categories_nona <- wild_categories[!is.na(wild_categories)]
dls_stn_nonull <- data_lists_DistI[-which(map_lgl(data_lists_DistI, is.null))]
dls_wild_nonull <- data_lists_DistI_wild[-which(map_lgl(data_lists_DistI_wild, is.null))]
tar_load(seeds)
tar_load(seeds_wild)
seeds_stn_nonull <- seeds[!is.na(stn_categories)] %>% map_dbl(., length)
seeds_wild_nonull <- seeds_wild[!is.na(wild_categories)] %>% map_dbl(., length)
stats <- data.frame(type = c(rep("stn", length(stn_categories_nona)), rep("wild", length(wild_categories_nona))),
                    ppc_quality = c(stn_categories_nona, wild_categories_nona),
                    tot_innetwork = c(map_dbl(dls_stn_nonull, "P"), map_dbl(dls_wild_nonull, "P")),
                    tot_rightcensored = c(map_dbl(dls_stn_nonull, "N_c"), map_dbl(dls_wild_nonull, "N_c")),
                    tot_seeds = c(seeds_stn_nonull, seeds_wild_nonull)) %>%
  mutate(tot_found = c(map_dbl(dls_stn_nonull, "N"), map_dbl(dls_wild_nonull, "N"))-tot_seeds) %>%
  mutate(ppc_quality = factor(ppc_quality, levels = c(0, 1, 2)))

## N in network
stats %>%
  ggplot(aes(x = ppc_quality, y = tot_innetwork, color = type))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(alpha = 0.4, position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.15), pch = 1, size = 2)+
  theme_minimal()+
  labs(y = "N in network", x = "PPC plot quality")+
  coord_flip()+
  scale_color_manual(values = c("darkorange", "olivedrab3"), name = "Type")+
  facet_wrap(~type, ncol = 1)
# Maybe a bit of a positive relationship between high number of points in the network and bad plot quality, but no difference between 1 and 2.

## Prop found
stats %>%
  ggplot(aes(x = ppc_quality, y = tot_found/tot_innetwork, color = type))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(alpha = 0.4, position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.15), pch = 1, size = 2)+
  theme_minimal()+
  labs(y = "Prop found", x = "PPC plot quality")+
  coord_flip()+
  scale_color_manual(values = c("darkorange", "olivedrab3"), name = "Type")+
  facet_wrap(~type, ncol = 1)
# Carcasses where proportionally fewer individuals found the carcass are not more likely to have bad ppc plots, which is what I would have expected/feared. If anything, a lot of the bad plots are the opposite (higher proportion found), especially for station. But the relationship is not clear and is probably not a significant difference. No relationship found for wild. 

## N seeds
stats %>%
  ggplot(aes(x = ppc_quality, y = tot_seeds, color = type))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(alpha = 0.4, position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.15), pch = 1, size = 2)+
  theme_minimal()+
  labs(y = "N seeds", x = "PPC plot quality")+
  coord_flip()+
  scale_color_manual(values = c("darkorange", "olivedrab3"), name = "Type")+
  facet_wrap(~type, ncol = 1) # yes, the ones with more seeds do seem to have a higher likelihood of having bad ppc plots. This would align with the problems I found earlier with Michael.

## Prop. finders that were seeds (out of total found)
stats %>%
  ggplot(aes(x = ppc_quality, y = tot_seeds/(tot_seeds+tot_found), color = type))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(alpha = 0.4, position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.15), pch = 1, size = 2)+
  theme_minimal()+
  labs(y = "Prop finders that were seeds", x = "PPC plot quality")+
  coord_flip()+
  scale_color_manual(values = c("darkorange", "olivedrab3"), name = "Type")+
  facet_wrap(~type, ncol = 1)
# Even clearer relationship for the stn carcs--having a higher proportion of seeds does seem to be correlated with having really bad looking ppc plots.

## N right-censored individuals
stats %>%
  ggplot(aes(x = ppc_quality, y = tot_rightcensored, color = type))+
  geom_boxplot(outlier.shape = NA)+
  geom_point(alpha = 0.4, position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.15), pch = 1, size = 2)+
  theme_minimal()+
  labs(y = "N right-censored", x = "PPC plot quality")+
  coord_flip()+
  scale_color_manual(values = c("darkorange", "olivedrab3"), name = "Type")+
  facet_wrap(~type, ncol = 1) # bad plots don't appear to be driven by having more right-censored individuals. If anything, for stn, there maybe is a negative relationship? Unclear if significant.


# Inter-model comparisons
# Model comparisons
## Compare 
## 5-way comparisons
comps <- pmap(list(a = social_fits_noILVs, b = social_fits_DistI, c = social_fits_DistIS, d = social_fits_DistI_AgeIS, e = social_fits_DistIS_AgeIS), function(a, b, c, d, e){if(!is.null(a)){STb_compare(a, b, c, d, e, model_names = c("noILVs", "DistI", "DistIS", "DistI_AgeIS", "DistIS_AgeIS"), method = "loo-psis")}else{NULL}})

comps_wild <- pmap(list(a = social_fits_noILVs_wild, b = social_fits_DistI_wild, c = social_fits_DistIS_wild, d = social_fits_DistI_AgeIS_wild, e = social_fits_DistIS_AgeIS_wild), function(a, b, c, d, e){if(!is.null(a)){STb_compare(a, b, c, d, e, model_names = c("noILVs", "DistI", "DistIS", "DistI_AgeIS", "DistIS_AgeIS"), method = "loo-psis")}else{NULL}})

comps_dfs <- map(comps, ~as.data.frame(.x$comparison))
comps_dfs_wild <- map(comps_wild, ~as.data.frame(.x$comparison))
comps_dfs <- map(comps_dfs, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_wild <- map(comps_dfs_wild, ~{.x$model <- rownames(.x);return(.x)})
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

comps_plots
comps_plots_wild

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
    
    # add in 0,0 starting point
    plot_data_obs <- bind_rows(
      plot_data_obs,
      plot_data_obs %>%
        distinct(trial) %>%
        mutate(time = 0, cum_prop = 0, type = "observed")
    ) %>%
      arrange(trial, time)
    
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

plotdata_noILVs <- map2(social_fits_noILVs, event_data, ~get_plotdata(.y, .x))
plotdata_DistI <- map2(social_fits_DistI, event_data, ~get_plotdata(.y, .x))
plotdata_DistIS <- map2(social_fits_DistIS, event_data, ~get_plotdata(.y, .x))
plotdata_DistI_AgeIS <- map2(social_fits_DistI_AgeIS, event_data, ~get_plotdata(.y, .x))
plotdata_DistIS_AgeIS <- map2(social_fits_DistIS_AgeIS, event_data, ~get_plotdata(.y, .x))

plotdata_noILVs_wild <- map2(social_fits_noILVs_wild, event_data_wild, ~get_plotdata(.y, .x))
plotdata_DistI_wild <- map2(social_fits_DistI_wild, event_data_wild, ~get_plotdata(.y, .x))
plotdata_DistIS_wild <- map2(social_fits_DistIS_wild, event_data_wild, ~get_plotdata(.y, .x))
plotdata_DistI_AgeIS_wild <- map2(social_fits_DistI_AgeIS_wild, event_data_wild, ~get_plotdata(.y, .x))
plotdata_DistIS_AgeIS_wild <- map2(social_fits_DistIS_AgeIS_wild, event_data_wild, ~get_plotdata(.y, .x))

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

walk2(curveplots_noILVs, padded, ~{ggsave(.x, file = paste0("data/saved_fits/station/noILVs/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistI, padded, ~{ggsave(.x, file = paste0("data/saved_fits/station/DistI/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistIS, padded, ~{ggsave(.x, file = paste0("data/saved_fits/station/DistIS/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistI_AgeIS, padded, ~{ggsave(.x, file = paste0("data/saved_fits/station/DistI_AgeIS/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistIS_AgeIS, padded, ~{ggsave(.x, file = paste0("data/saved_fits/station/DistIS_AgeIS/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})

walk2(curveplots_noILVs_wild, padded_wild, ~{ggsave(.x, file = paste0("data/saved_fits/wild/noILVs/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistI_wild, padded_wild, ~{ggsave(.x, file = paste0("data/saved_fits/wild/DistI/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistIS_wild, padded_wild, ~{ggsave(.x, file = paste0("data/saved_fits/wild/DistIS/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistI_AgeIS_wild, padded_wild, ~{ggsave(.x, file = paste0("data/saved_fits/wild/DistI_AgeIS/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})
walk2(curveplots_DistIS_AgeIS_wild, padded_wild, ~{ggsave(.x, file = paste0("data/saved_fits/wild/DistIS_AgeIS/curveplots/curveplot_", .y, ".png"), width = 6, height = 5)})



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


