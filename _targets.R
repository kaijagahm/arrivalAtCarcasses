# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("vultureUtils", "sf", "tidyverse", "move", "feather", "readxl", "elevatr", "here", "furrr", "future", "purrr", "igraph", "mapview", "parallel",   "ggplot2", "ggraph", "tidygraph") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
lapply(list.files("R", full.names = TRUE), source) # source all scripts in the R directory
# tar_source("other_functions.R") # Source other scripts as needed.

list(
  # Prepare data
  tar_target(geofences, get_geofences()),
  tar_target(tag_sns_file, "data/geofence_tags_6Aug23_45.xlsx", format = "file"),
  tar_target(tag_sns, get_sns(tag_sns_file)),
  tar_target(pw, "movebankCredentials/pw.Rda", format = "file"),
  tar_target(loginObject, get_loginObject(pw)),
  tar_target(ornitela, get_ornitela(loginObject)),
  tar_target(ww_file, "data/whoswho_vultures_20230920_new.xlsx", format = "file"),
  tar_target(fixed_names, fix_names(ornitela, ww_file)),
  tar_target(removed_periods, remove_periods(ww_file, fixed_names)),
  tar_target(cleaned, clean_data(removed_periods)),
  tar_target(capture_sites, "data/capture_sites.csv", format = "file"),
  tar_target(carmel, "data/all_captures_carmel_2010-2021.csv", format = "file"),
  tar_target(removed_captures, remove_captures(capture_sites, carmel, cleaned)),
  tar_target(with_age_sex, attach_age_sex(removed_captures, ww_file)),
  tar_target(hires_tags, get_hires_tags(with_age_sex, tag_sns))
)



