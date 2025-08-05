# Example of running NBDA on one INPA and one wild carcass

# Load libraries and data -------------------------------------------------
source(here("R/functions.R"))
library(dplyr)
library(lubridate)
library(NBDA)
tar_load(inpa_carcs)
tar_load(wild_carcs)
tar_load(gps_combined)
tar_load(gps_all_inpa)
dbf <- 30
tar_load(days_after)

hist(gps_combined$dateOnly, breaks = "weeks") # we have GPS data spanning the hf periods

# Get histograms, for reference later
plots_inpa <- readRDS(here("data/plots_inpa.RDS"))
plots_wild_valid <- readRDS(here("data/plots_wild_valid.RDS"))
names(plots_inpa)
length(plots_inpa) # all 81 carcasses
names(plots_wild_valid) 
length(plots_wild_valid) # only 14 wild carcasses that we are considering to be valid at this point.

# Keep only the wild carcasses that we deemed probably "valid" in wild_carcasses.R based on the bounding box
wild_carcs_valid <- wild_carcs[map_lgl(wild_carcs, ~.x$carcID %in% names(plots_wild_valid))]
length(wild_carcs_valid)


# Do NBDA with INPA carcass -----------------------------------------------
# Select an INPA carcass
id <- 4892923
which_id <- which(unlist(map(inpa_carcs, "carcID")) == id)
plots_inpa[names(plots_inpa) == id][[1]]

# Get gps data
gps_30days <- get_gps_all(inpa_carcs[which_id], gps_combined, days_after, dbf)[[1]]
length(unique(gps_30days$dateOnly)) # gps_combined now has 30 days tacked onto the beginning of each of the three month-long hf periods, so i should be able to use the `dbf` 30 days value without worrying about having enough data. Here we're pulling the unique GPS subset for each carcass.
min(gps_30days$dateOnly) == inpa_carcs[[which_id]]$date - days(dbf)
max(gps_30days$dateOnly) == inpa_carcs[[which_id]]$date + days(days_after) # because the get_gps_all function adds 1 day to allow for calculating roost positions. So this should in fact be different.
max(gps_30days$dateOnly) == inpa_carcs[[which_id]]$date + days(days_after+1) # one day more

seed_time_before <- 30 #mins
tar_load(detection_distance_flight)
tar_load(detection_distance_stationary)
tar_load(gps_spd)

test <- prepare_nbda_data(gps = gps_30days, 
                          remove_seeds = FALSE, 
                          seed_time_before = NULL, 
                          ddf = detection_distance_flight, 
                          dds = detection_distance_stationary, 
                          gps_spd = gps_spd, 
                          n_hours_gps_dynamic = list(c(-24, 0),
                                                     c(-72, 0)),
                          n_hours_gps_static = list(c(-720, -24), 
                                                    c(-168, -24)),
                          sighting_time_max_hours = 72)
save(test, file = here("data/test.Rda"))

# # Networks
# ## Dynamic
# ### flight, day by day
# fl_bin_sameday <- fix_nets(map(test$gps_data_sameday, ~get_fl_bin(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
# fl_wt_sameday <- fix_nets(map(test$gps_data_sameday, ~get_fl_weighted(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
# ### flight, cumulative same day
# fl_bin_cumulative_sameday <- fix_nets(map(test$gps_data_cumulative, ~get_fl_bin(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
# fl_wt_cumulative_sameday <- fix_nets(map(test$gps_data_cumulative, ~get_fl_weighted(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
# ### flight, since 3 days prior
# fl_bin_3daysprior <- fix_nets(map(test$gps_data_dynamic_hours_n072_000, ~get_fl_bin(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
# fl_wt_3daysprior <- fix_nets(map(test$gps_data_dynamic_hours_n072_000, ~get_fl_weighted(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
# ### flight, since 1 day prior
# fl_bin_1daysprior <- fix_nets(map(test$gps_data_dynamic_hours_n024_000, ~get_fl_bin(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
# fl_wt_1daysprior <- fix_nets(map(test$gps_data_dynamic_hours_n024_000, ~get_fl_weighted(.x, dist = detection_distance_flight)), test$all_indivs_sorted)
# 
# ## Static
# ### flight, -30 through -1 days
# fl_bin_n720n024 <- fix_nets(list(get_fl_bin(test$gps_data_static_hours_n720_n024, dist = detection_distance_flight)), test$all_indivs_sorted)
# fl_wt_n720n024 <- fix_nets(list(get_fl_weighted(test$gps_data_static_hours_n720_n024, dist = detection_distance_flight)), test$all_indivs_sorted)
# ### flight, -7 through -1 days
# fl_bin_n168n024 <- fix_nets(list(get_fl_bin(test$gps_data_static_hours_n168_n024, dist = detection_distance_flight)), test$all_indivs_sorted)
# fl_wt_n168n024 <- fix_nets(list(get_fl_weighted(test$gps_data_static_hours_n168_n024, dist = detection_distance_flight)), test$all_indivs_sorted)
# 
# nets_inpa <- list("fl_bin_sameday" = fl_bin_sameday, 
#                   "fl_wt_sameday" = fl_wt_sameday,
#                   "fl_bin_cumulative_sameday" = fl_bin_cumulative_sameday, 
#                   "fl_wt_cumulative_sameday" = fl_wt_cumulative_sameday, 
#                   "fl_bin_3daysprior" = fl_bin_3daysprior, 
#                   "fl_wt_3daysprior" = fl_wt_3daysprior, 
#                   "fl_bin_1daysprior" = fl_bin_1daysprior, 
#                   "fl_wt_1daysprior" = fl_wt_1daysprior, 
#                   "fl_bin_n720n024" = fl_bin_n720n024, 
#                   "fl_wt_n720n024" = fl_wt_n720n024, 
#                   "fl_bin_n168n024" = fl_bin_n168n024, 
#                   "fl_wt_n168n024" = fl_wt_n168n024)
# save(nets_inpa, file = here("data/nets_inpa.Rda"))
load(here("data/nets_inpa.Rda"))

## Dynamic flight networks, entire day of first sighting, including after first sighting
data_sameday <- nbdaData(label = test$carcID, 
                         assMatrix = make_assMatrix(nets_inpa$fl_bin_sameday), 
                         orderAcq = test$oa_nums)
data_sameday_wt <- nbdaData(label = test$carcID,
                            assMatrix = make_assMatrix(nets_inpa$fl_wt_sameday),
                            orderAcq = test$oa_nums)
mod_sameday <- oadaFit(data_sameday, type = "social")
mod_sameday_wt <- oadaFit(data_sameday_wt, type = "social")

## Dynamic flight networks, cumulative
data_cumul <- nbdaData(label = test$carcID, 
         assMatrix = make_assMatrix(nets_inpa$fl_bin_cumulative_sameday), 
         orderAcq = test$oa_nums)
data_cumul_wt <- nbdaData(label = test$carcID, 
         assMatrix = make_assMatrix(nets_inpa$fl_wt_cumulative_sameday), 
         orderAcq = test$oa_nums)
mod_cumul <- oadaFit(data_cumul, type = "social")
mod_cumul_wt <- oadaFit(data_cumul_wt, type = "social")

## Dynamic flight networks, -72 hours through sighting
data_3daysprior <- nbdaData(label = test$carcID, 
                          assMatrix = make_assMatrix(nets_inpa$fl_bin_3daysprior), 
                          orderAcq = test$oa_nums)
data_3daysprior_wt <- nbdaData(label = test$carcID, 
                            assMatrix = make_assMatrix(nets_inpa$fl_wt_3daysprior), 
                            orderAcq = test$oa_nums)
mod_3daysprior <- oadaFit(data_3daysprior, type = "social")
mod_3daysprior_wt <- oadaFit(data_3daysprior_wt, type = "social")

## Dynamic flight networks, -24 hours through sighting
data_1daysprior <- nbdaData(label = test$carcID, 
                            assMatrix = make_assMatrix(nets_inpa$fl_bin_1daysprior), 
                            orderAcq = test$oa_nums)
data_1daysprior_wt <- nbdaData(label = test$carcID, 
                               assMatrix = make_assMatrix(nets_inpa$fl_wt_1daysprior), 
                               orderAcq = test$oa_nums)
mod_1daysprior <- oadaFit(data_1daysprior, type = "social")
mod_1daysprior_wt <- oadaFit(data_1daysprior_wt, type = "social")

## Static flight networks, -720 hours (30 days prior) through -24 hours (1 day prior)
data_n720n024 <- nbdaData(label = test$carcID, 
                            assMatrix = make_assMatrix(nets_inpa$fl_bin_n720n024), 
                            orderAcq = test$oa_nums)
data_n720n024_wt <- nbdaData(label = test$carcID, 
                        assMatrix = make_assMatrix(nets_inpa$fl_wt_n720n024), 
                        orderAcq = test$oa_nums)
mod_n720n024 <- oadaFit(data_n720n024, type = "social")
mod_n720n024_wt <- oadaFit(data_n720n024_wt, type = "social")

## Static flight networks, -168 hours (7 days prior) through -24 hours (1 day prior)
data_n168n024 <- nbdaData(label = test$carcID, 
                          assMatrix = make_assMatrix(nets_inpa$fl_bin_n168n024), 
                          orderAcq = test$oa_nums)
data_n168n024_wt <- nbdaData(label = test$carcID, 
                             assMatrix = make_assMatrix(nets_inpa$fl_wt_n168n024), 
                             orderAcq = test$oa_nums)
mod_n168n024 <- oadaFit(data_n168n024, type = "social")
mod_n168n024_wt <- oadaFit(data_n168n024_wt, type = "social")

mods_list <- list("dynamic_sameday_bin" = mod_sameday, 
                  "dynamic_cumulative_bin" = mod_cumul, 
                  "dynamic_3days_bin" = mod_3daysprior, 
                  "dynamic_1days_bin" = mod_1daysprior,
                  "static_n720_n024_bin" = mod_n720n024, 
                  "static_n168_n024_bin" = mod_n168n024,
                  "dynamic_sameday_wt" = mod_sameday_wt, 
                  "dynamic_cumulative_wt" = mod_cumul_wt, 
                  "dynamic_3days_wt" = mod_3daysprior_wt, 
                  "dynamic_1days_wt" = mod_1daysprior_wt,
                  "static_n720_n024_wt" = mod_n720n024_wt, 
                  "static_n168_n024_wt" = mod_n168n024_wt)
stats <- purrr::list_rbind(map(mods_list, getmodstats))
stats$mod <- names(mods_list)
stats <- stats %>%
  mutate(net_type = factor(str_extract(mod, "static|dynamic"), levels = c("static", "dynamic")),
         wt = str_detect(mod, "wt"))

stats %>%
  mutate(sig = case_when(outputPar-se > 0 ~ T, .default = F)) %>%
  filter(mod != "static_3001") %>%
  ggplot(aes(x = mod, col = net_type))+
  geom_segment(aes(y = outputPar-se, yend = outputPar + se, linetype = sig), linewidth = 0.5)+
  geom_point(aes(y = outputPar, shape = sig), size = 3)+
  theme_classic()+
 # theme(legend.position = "none")+
  scale_linetype_manual(values = c(2, 1))+
  scale_shape_manual(values = c(1, 19))+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  labs(y = "S (social transmission strength)",
       x = "Model", color = "Network type",
       shape = "Evidence for\nsocial\ntransmission",
       linetype = "Evidence for\nsocial\ntransmission",
       caption = id)+
  coord_flip() # niiice


# Do NBDA with a wild carcass -----------------------------------------------
# Select a wild carcass
names(plots_wild_valid)[1]
id_wild <- 52
which_id_wild <- which(unlist(map(wild_carcs_valid, "carcID")) == id_wild)
plots_wild_valid[[which_id_wild]]

# Get gps data
gps_30days_wild <- get_gps_all(wild_carcs_valid[which_id_wild], gps_combined, days_after, dbf)[[1]]
length(unique(gps_30days_wild$dateOnly)) # gps_combined now has 30 days tacked onto the beginning of each of the three month-long hf periods, so i should be able to use the `dbf` 30 days value without worrying about having enough data. Here we're pulling the unique GPS subset for each carcass.
min(gps_30days_wild$dateOnly) == wild_carcs_valid[[which_id_wild]]$date - days(dbf)
max(gps_30days_wild$dateOnly) == wild_carcs_valid[[which_id_wild]]$date + days(days_after) # because the get_gps_all function adds 1 day to allow for calculating roost positions. So this should in fact be different.
max(gps_30days_wild$dateOnly) == wild_carcs_valid[[which_id_wild]]$date + days(days_after+1) # one day more

test_wild <- prepare_nbda_data(gps = gps_30days_wild, 
                          remove_seeds = FALSE, 
                          seed_time_before = NULL, 
                          ddf = detection_distance_flight, 
                          dds = detection_distance_stationary, 
                          gps_spd = gps_spd, 
                          n_hours_gps_dynamic = list(c(-24, 0),
                                                     c(-72, 0)),
                          n_hours_gps_static = list(c(-720, -24), 
                                                    c(-168, -24)),
                          sighting_time_max_hours = 72)
save(test_wild, file = here("data/test_wild.Rda"))
# XXX Note: we're going to need to back it up an hour, and I haven't thought through how to do that yet. 

# # Networks
# ## Dynamic
# ### flight, day by day
# fl_bin_sameday_wild <- fix_nets(map(test_wild$gps_data_sameday, ~get_fl_bin(.x, dist = detection_distance_flight)), test_wild$all_indivs_sorted)
# fl_wt_sameday_wild <- fix_nets(map(test_wild$gps_data_sameday, ~get_fl_weighted(.x, dist = detection_distance_flight)), test_wild$all_indivs_sorted)
# ### flight, cumulative same day
# fl_bin_cumulative_sameday_wild <- fix_nets(map(test_wild$gps_data_cumulative, ~get_fl_bin(.x, dist = detection_distance_flight)), test_wild$all_indivs_sorted)
# fl_wt_cumulative_sameday_wild <- fix_nets(map(test_wild$gps_data_cumulative, ~get_fl_weighted(.x, dist = detection_distance_flight)), test_wild$all_indivs_sorted)
# ### flight, since 3 days prior
# fl_bin_3daysprior_wild <- fix_nets(map(test_wild$gps_data_dynamic_hours_n072_000, ~get_fl_bin(.x, dist = detection_distance_flight)), test_wild$all_indivs_sorted)
# fl_wt_3daysprior_wild <- fix_nets(map(test_wild$gps_data_dynamic_hours_n072_000, ~get_fl_weighted(.x, dist = detection_distance_flight)), test_wild$all_indivs_sorted)
# ### flight, since 1 day prior
# fl_bin_1daysprior_wild <- fix_nets(map(test_wild$gps_data_dynamic_hours_n024_000, ~get_fl_bin(.x, dist = detection_distance_flight)), test_wild$all_indivs_sorted)
# fl_wt_1daysprior_wild <- fix_nets(map(test_wild$gps_data_dynamic_hours_n024_000, ~get_fl_weighted(.x, dist = detection_distance_flight)), test_wild$all_indivs_sorted)
# 
# ## Static
# ### flight, -30 through -1 days
# fl_bin_n720n024_wild <- fix_nets(list(get_fl_bin(test_wild$gps_data_static_hours_n720_n024, dist = detection_distance_flight)), test_wild$all_indivs_sorted)
# fl_wt_n720n024_wild <- fix_nets(list(get_fl_weighted(test_wild$gps_data_static_hours_n720_n024, dist = detection_distance_flight)), test_wild$all_indivs_sorted)
# ### flight, -7 through -1 days
# fl_bin_n168n024_wild <- fix_nets(list(get_fl_bin(test_wild$gps_data_static_hours_n168_n024, dist = detection_distance_flight)), test_wild$all_indivs_sorted)
# fl_wt_n168n024_wild <- fix_nets(list(get_fl_weighted(test_wild$gps_data_static_hours_n168_n024, dist = detection_distance_flight)), test_wild$all_indivs_sorted)
# 
# nets_wild <- list("fl_bin_sameday_wild" = fl_bin_sameday_wild, 
#                   "fl_wt_sameday_wild" = fl_wt_sameday_wild,
#                   "fl_bin_cumulative_sameday_wild" = fl_bin_cumulative_sameday_wild, 
#                   "fl_wt_cumulative_sameday_wild" = fl_wt_cumulative_sameday_wild, 
#                   "fl_bin_3daysprior_wild" = fl_bin_3daysprior_wild, 
#                   "fl_wt_3daysprior_wild" = fl_wt_3daysprior_wild, 
#                   "fl_bin_1daysprior_wild" = fl_bin_1daysprior_wild, 
#                   "fl_wt_1daysprior_wild" = fl_wt_1daysprior_wild, 
#                   "fl_bin_n720n024_wild" = fl_bin_n720n024_wild, 
#                   "fl_wt_n720n024_wild" = fl_wt_n720n024_wild, 
#                   "fl_bin_n168n024_wild" = fl_bin_n168n024_wild, 
#                   "fl_wt_n168n024_wild" = fl_wt_n168n024_wild)
# save(nets_wild, file = here("data/nets_wild.Rda"))
load(here("data/nets_wild.Rda"))

## Dynamic flight networks, entire day of first sighting, including after first sighting
data_sameday_wild <- nbdaData(label = test_wild$carcID, 
                         assMatrix = make_assMatrix(nets_wild$fl_bin_sameday_wild), 
                         orderAcq = test_wild$oa_nums)
data_sameday_wt_wild <- nbdaData(label = test_wild$carcID,
                            assMatrix = make_assMatrix(nets_wild$fl_wt_sameday_wild),
                            orderAcq = test_wild$oa_nums)
mod_sameday_wild <- oadaFit(data_sameday_wild, type = "social")
mod_sameday_wt_wild <- oadaFit(data_sameday_wt_wild, type = "social")

## Dynamic flight networks, cumulative
data_cumul_wild <- nbdaData(label = test_wild$carcID, 
                       assMatrix = make_assMatrix(nets_wild$fl_bin_cumulative_sameday_wild), 
                       orderAcq = test_wild$oa_nums)
data_cumul_wt_wild <- nbdaData(label = test_wild$carcID, 
                          assMatrix = make_assMatrix(nets_wild$fl_wt_cumulative_sameday_wild), 
                          orderAcq = test_wild$oa_nums)
mod_cumul_wild <- oadaFit(data_cumul_wild, type = "social")
mod_cumul_wt_wild <- oadaFit(data_cumul_wt_wild, type = "social")

## Dynamic flight networks, -72 hours through sighting
data_3daysprior_wild <- nbdaData(label = test_wild$carcID, 
                            assMatrix = make_assMatrix(nets_wild$fl_bin_3daysprior_wild), 
                            orderAcq = test_wild$oa_nums)
data_3daysprior_wt_wild <- nbdaData(label = test_wild$carcID, 
                               assMatrix = make_assMatrix(nets_wild$fl_wt_3daysprior_wild), 
                               orderAcq = test_wild$oa_nums)
mod_3daysprior_wild <- oadaFit(data_3daysprior_wild, type = "social")
#mod_3daysprior_wt_wild <- oadaFit(data_3daysprior_wt_wild, type = "social")

## Dynamic flight networks, -24 hours through sighting
data_1daysprior_wild <- nbdaData(label = test_wild$carcID, 
                            assMatrix = make_assMatrix(nets_wild$fl_bin_1daysprior_wild), 
                            orderAcq = test_wild$oa_nums)
data_1daysprior_wt_wild <- nbdaData(label = test_wild$carcID, 
                               assMatrix = make_assMatrix(nets_wild$fl_wt_1daysprior_wild), 
                               orderAcq = test_wild$oa_nums)
mod_1daysprior_wild <- oadaFit(data_1daysprior_wild, type = "social")
mod_1daysprior_wt_wild <- oadaFit(data_1daysprior_wt_wild, type = "social")

## Static flight networks, -720 hours (30 days prior) through -24 hours (1 day prior)
data_n720n024_wild <- nbdaData(label = test_wild$carcID, 
                          assMatrix = make_assMatrix(nets_wild$fl_bin_n720n024_wild), 
                          orderAcq = test_wild$oa_nums)
data_n720n024_wt_wild <- nbdaData(label = test_wild$carcID, 
                             assMatrix = make_assMatrix(nets_wild$fl_wt_n720n024_wild), 
                             orderAcq = test_wild$oa_nums)
mod_n720n024_wild <- oadaFit(data_n720n024_wild, type = "social")
mod_n720n024_wt_wild <- oadaFit(data_n720n024_wt_wild, type = "social")

## Static flight networks, -168 hours (7 days prior) through -24 hours (1 day prior)
data_n168n024_wild <- nbdaData(label = test_wild$carcID, 
                          assMatrix = make_assMatrix(nets_wild$fl_bin_n168n024_wild), 
                          orderAcq = test_wild$oa_nums)
data_n168n024_wt_wild <- nbdaData(label = test_wild$carcID, 
                             assMatrix = make_assMatrix(nets_wild$fl_wt_n168n024_wild), 
                             orderAcq = test_wild$oa_nums)
mod_n168n024_wild <- oadaFit(data_n168n024_wild, type = "social")
#mod_n168n024_wt_wild <- oadaFit(data_n168n024_wt_wild, type = "social")

mods_list_wild <- list("dynamic_sameday_bin" = mod_sameday_wild, 
                  "dynamic_cumulative_bin" = mod_cumul_wild, 
                  "dynamic_3days_bin" = mod_3daysprior_wild, 
                  "dynamic_1days_bin" = mod_1daysprior_wild,
                  "static_n720_n024_bin" = mod_n720n024_wild, 
                  "static_n168_n024_bin" = mod_n168n024_wild,
                  "dynamic_sameday_wt" = mod_sameday_wt_wild,
                  "dynamic_cumulative_wt" = mod_cumul_wt_wild,
                  #"dynamic_3days_wt" = mod_3daysprior_wt_wild,
                  "dynamic_1days_wt" = mod_1daysprior_wt_wild,
                  "static_n720_n024_wt" = mod_n720n024_wt_wild#,
                  #"static_n168_n024_wt" = mod_n168n024_wt_wild
)
stats_wild <- purrr::list_rbind(map(mods_list_wild, getmodstats))
stats_wild$mod <- names(mods_list_wild)
stats_wild <- stats_wild %>%
  mutate(net_type = factor(str_extract(mod, "static|dynamic"), levels = c("static", "dynamic")),
         wt = str_detect(mod, "wt"))

stats_wild %>%
  mutate(sig = case_when(outputPar-se > 0 ~ T, .default = F)) %>%
  ggplot(aes(x = mod, col = net_type))+
  geom_segment(aes(y = outputPar-se, yend = outputPar + se, linetype = sig), linewidth = 0.5)+
  geom_point(aes(y = outputPar, shape = sig), size = 3)+
  theme_classic()+
  # theme(legend.position = "none")+
  scale_linetype_manual(values = c(2, 1))+
  scale_shape_manual(values = c(1, 19))+
  geom_hline(aes(yintercept = 0), linetype = 2)+
  labs(y = "S (social transmission strength)",
       x = "Model", color = "Network type",
       shape = "Evidence for\nsocial\ntransmission",
       linetype = "Evidence for\nsocial\ntransmission",
       caption = id)+
  coord_flip()
# test removing seeds [TO DO!]
