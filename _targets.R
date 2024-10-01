# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.
# tar_config_set(
#   seconds_meta_append = 15,
#   seconds_reporter = 0.5
# )

# Set target options:
tar_option_set(
  memory = "transient",
  garbage_collection = TRUE,
  controller = crew::crew_controller_local(workers = 10, seconds_timeout = 120),
  format = "qs",
  error = "null",
  packages = c("vultureUtils", "sf", "tidyverse", "move2", "feather", "readxl", "elevatr", "here", "furrr", "future", "purrr", "igraph", "mapview", "parallel",   "ggplot2", "ggraph", "tidygraph", "moments", "tidymodels", "ranger", "parsnip", "caret", "zoo", "readxl", "data.table", "readr") # Packages that your targets need for their tasks.
  
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
)

# Run the R scripts in the R/ folder with your custom functions:
lapply(list.files("R", full.names = TRUE), source) # source all scripts in the R directory
# tar_source("other_functions.R") # Source other scripts as needed.

list(
  # Prepare data (mining period, geofenced)
  # tar_target(geofences, get_geofences()),
  # tar_target(tag_sns_file, "data/geofence_tags_6Aug23_45.xlsx", format = "file"),
  # tar_target(tag_sns, get_sns(tag_sns_file)),
  tar_target(pw, "movebankCredentials/pw.Rda", format = "file"),
  tar_target(loginObject, get_loginObject(pw)),
  tar_target(ornitela, get_ornitela(loginObject)),
  tar_target(ww_file, "data/whoswho_vultures_20230920_new.xlsx", format = "file"),
  # tar_target(fixed_names, fix_names(ornitela, ww_file)),
  # tar_target(removed_periods, remove_periods(ww_file, fixed_names)),
  # tar_target(cleaned, clean_data(removed_periods)),
  # tar_target(capture_sites, "data/capture_sites.csv", format = "file"),
  # tar_target(carmel, "data/all_captures_carmel_2010-2021.csv", format = "file"),
  # tar_target(removed_captures, remove_captures(capture_sites, carmel, cleaned)),
  # tar_target(with_age_sex, attach_age_sex(removed_captures,t ww_file)),
  # tar_target(hires_tags, get_hires_tags(with_age_sex, tag_sns)),
  
  # High-frequency ACC
  ## 2024 period
  ### Get data
  tar_target(calibration_data, read_csv(here("ACC_algo_Marta_draft/Data/example_calibration.csv"))),
  tar_target(gv_model, readRDS(here("ACC_algo_Marta_draft/Data/gv_final_model_fit.rda"))),
  tar_target(data_files_2024, list.files(here("data/ACC/2024_hf_period/raw/"), 
                                         pattern = ".csv", full.names = T)),
  tar_target(data_files_2023, list.files(here("data/ACC/2023_hf_period/raw/"), 
                                         pattern = ".csv", full.names = T)),
  tar_target(feeding_bouts_certain_2023, readRDS(here("data/ACC/2023_hf_period/created/feeding_bouts_certain_2023.RDS"))),
  tar_target(feeding_bouts_certain_2024, readRDS(here("data/ACC/2024_hf_period/created/feeding_bouts_certain_2024.RDS"))) # XXXX ARRRRRGGHHH WHY WON'T THIS WORK?????
  # ### Classify feeding station vs. not
  # tar_target(stations, readRDS(here("data/created/stations.RDS"))),
  # tar_target(stations_buffered, sf::st_buffer(stations, dist = 100)),
  # tar_target(stations_union, sf::st_union(stations_buffered)),
  # tar_target(feeding_bouts_station, assign_fs(feeding_bouts, stations_union),
  #            pattern = map(feeding_bouts),
  #            iteration = "list"),
  # 
  # # in case we wanted to assign station vs. non-station based on carcass placement instead of station coordinates
  # tar_target(carcasses_inpa, readRDS(here("data/created/carcasses_inpa.RDS"))),
  # tar_target(carcasses_buffered, sf::st_buffer(carcasses_inpa, dist = 100)),
  # tar_target(carcasses_union, sf::st_union(carcasses_buffered))
  )


