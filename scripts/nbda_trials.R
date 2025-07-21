# NBDA, all the different permutations
library(tidyverse)
library(NBDA)
library(here)
library(targets)
source(here("R/functions.R"))
# Look at the INPA carcass plots (in wild_carcasses.R) and pick a few that have a lot of individuals and have the carcass placed relatively early in the day.
carcasses_to_use_inpa <- data.frame(carcID = c(4892923, 4890051, 4467134, 4407966, 4233130, 4224995), datetime = lubridate::ymd_hms(c("2024-05-08 12:50:56", "2024-05-06 13:42:35", "2023-04-16 13:15:33", "2023-03-15 15:57:09", "2022-12-06 10:16:37", "2022-11-30 11:54:06")), year = c(2024, 2024, 2023, 2023, 2022, 2022), carcType = "inpa") # chosen based on looking at plots. two for each year

# Look at the wild carcasses and pick 1-3 that look particularly robust
carcasses_to_use_wild <- data.frame(carcID = c(65, 53), datetime = lubridate::ymd_hms(c("2023-03-27 11:01:34", "2023-03-31 09:21:02")), year = c(2023, 2023), carcType = "wild")

# This is too complicated, so I'm going to just focus on one carcass.

touse <- bind_rows(carcasses_to_use_inpa, carcasses_to_use_wild)

# Get the GPS data for each one (including day before)
tar_load(gps_all_inpa)
tar_load(gps_all_wild)
stn <- purrr::list_rbind(gps_all_inpa) %>% sf::st_drop_geometry() %>% mutate(type = "inpa")
wild <- purrr::list_rbind(gps_all_wild) %>% sf::st_drop_geometry() %>% mutate(type = "wild")

gps <- bind_rows(stn, wild) %>%
  filter(carcID %in% touse$carcID)
length(unique(gps$carcID)) # got 8 carcasses (2 inpa from each year, and 2 wild from 2023)

# Get seeded demonstrators for each one
# tar_load(detection_distance_flight) # 2km
# tar_load(detection_distance_stationary) # 1km
# tar_load(seed_time_before) # 30 min
tar_load(seeds_inpa)
tar_load(seeds_wild)

keep <- unlist(map(seeds_inpa, ~all(.x$carcID %in% touse$carcID) & nrow(.x) > 0))
seeds_inpa_keep <- seeds_inpa[keep]
length(seeds_inpa_keep) # 5 out of the 6 inpa carcasses have seed individuals
keep <- unlist(map(seeds_wild, ~all(.x$carcID %in% touse$carcID) & nrow(.x) > 0))
seeds_wild_keep <- seeds_wild[keep]
seeds <- c(seeds_inpa_keep, seeds_wild_keep)
names(seeds) <- unlist(map(seeds, ~.x$carcID[1]))

which_inpa <- which(map_dbl(inpa_carcs, "carcID") %in% touse$carcID[touse$carcType == "inpa"])
which_wild <- which(map_dbl(wild_carcs, "carcID") %in% touse$carcID[touse$carcType == "wild"])
tar_load(roosts)
tar_load(roosts_wild)
tar_load(inpa_carcs)
tar_load(wild_carcs)
distances_inpa <- get_distances(roosts[which_inpa], inpa_carcs[which_inpa])
distances_wild <- get_distances(roosts_wild[which_wild], wild_carcs[which_wild])
tar_load(ilvs) # only have inpa so far--need to either exclude entirely or add later for wild
tar_load(gps_all_inpa)
tar_load(gps_all_wild)
tar_load(days_after)
gps_inpa_since <- remove_points_before(gps_all_inpa[which_inpa], inpa_carcs[which_inpa], days_after, hours_before = 0)
gps_wild_since <- remove_points_before(gps_all_wild[which_wild], wild_carcs[which_wild], days_after, hours_before = 0)
tar_load(firsts_see)
firsts_see_inpa <- firsts_see[which_inpa]
tar_load(firsts_see_wild)
firsts_see_wild <- firsts_see_wild[which_wild]

# For each carcass, we will need the following
# carcass ID
# order of arrival
# association matrix indices (if dynamic)
# nets 1
# nets 2
# ilvs (if using)
# seeds
# n individuals
# n time periods
get_nbdaData_list_flex <- function(cids, oas, amis,
                                   nets1, nets2 = NULL,
                                   is_dynamic = FALSE,
                                   dists = NULL, ags = NULL,
                                   seeds = NULL,
                                   n_indivs = NULL, n_timeperiods = NULL)
