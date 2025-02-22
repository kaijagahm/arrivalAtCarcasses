# PACKAGES
## Workflow
library(here)
library(targets)

## Data wrangling
library(tidyverse) # for data wrangling
library(vultureUtils)
library(data.table)

## Networks
library(igraph) # for network viz
library(tidygraph)
library(ggraph)
#library(asnipe) # XXX remove?

## Spatial
library(sf)
library(mapview)
# library(sp) # XXX remove?
# library(geosphere) # XXX remove?
# devtools::install_github("John-R-Wallace-NOAA/Imap")
# library(Imap) # XXX remove?

## Modeling and stats
# install_github("whoppitt/NBDA")
library(NBDA)
library(vegan)

## Visualization
library(ggplot2)
library(gridExtra)

## PSEUDOCODE
## 0. Define parameters
## 1. Get carcasses and restrict to south
## 1a. Convert carcasses to Israel time # xxx FIXME!
## 2. Separate INPA and wild (the rest of the instructions here are just for INPA)
## 3. Get datetimes
## 4. Get all gps data
## 4a. Convert gps data to Israel time # xxx FIXME!
## 4b. Make gps_all
## 5. Get roosts
## 6. Get seeds
## 7. Get distances from roosts to carcasses
## 8. Load who's who
## 9. Get age_group ILV
## 10. Combine age_group ILV with distances to get ILVs data frame
## 11. Make gps
## 12. Get arrivals
## 13. Get firsts
## 14. Get oa
## 15. Get GPS subsets for flight (four different intervals)
## 16. Get roost nets
## 17. Get flight nets (whole days)
## 18. Get flight nets (four different intervals)

## 0. Define parameters
days_after <- 3
seed_distance <- 1000 # 1000m to be within sight of the carcass
seed_time_before <- hours(1)

## 1. Get carcasses and restrict to south
tar_load(all_carcasses_annotated)
tar_load(bbox_south)
aca <- all_carcasses_annotated %>% st_crop(bbox_south)

## 1a. Convert carcasses to Israel time
# XXX FIXME!

## 2. Separate INPA and wild (the rest of the instructions here are just for INPA)
inpa <- aca %>% filter(carcType == "inpa")
inpa_carcs <- inpa %>% group_by(carcID) %>% group_split() # split into a list so we can use this later with map, e.g. to calculate distances
save(inpa_carcs, file = here("test_dynamic_nbda/data/inpa_carcs.Rda"))

wild <- aca %>% filter(carcType == "wild") # the rest of the code will pertain to inpa only; this is a stub for now
wild_carcs <- wild %>% group_by(carcID) %>% group_split()

## 3. Get datetimes
datetimes <- inpa$datetime

## 4. Get all gps data
gps_2023 <- data.table::fread("data/ACC/2023_hf_period/created/gps_2023.csv")
gps_2024 <- data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv") 
gps <- bind_rows(gps_2023, gps_2024) %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>%
  bind_cols(st_coordinates(.)) %>%
  rename("location_long" = X,
         "location_lat" = Y) %>%
  mutate(dateOnly = lubridate::ymd(dateOnly)) %>%
  st_transform(32636) %>%
  st_crop(bbox_south)

## 4a. Convert gps data to Israel time
# XXX FIXME!

## 4b. Make gps_all
gps_all <- map2(datetimes, inpa_carcs, ~{
  gps %>%
    filter(dateOnly >= lubridate::date(.x)-days(1) & dateOnly <= lubridate::date(.x) + days(days_after+1)) %>%
    mutate(dist_to_carcass = as.numeric(st_distance(., .y)))
}) #XXX check that we are getting the roost location for the day before the carcass was placed

## 5. Get roosts
# roosts <- map(gps_all, ~{
#   if(nrow(.x) > 0){
#     return(get_roosts_df(.x, id = "local_identifier"))
#   }
#     else{return(NULL)}
#   },
#     .progress = T)
# write_rds(roosts, file = here("test_dynamic_nbda/data/roosts.RDS"))
roosts <- readRDS(here("test_dynamic_nbda/data/roosts.RDS"))

## 6. Get seeds
seed_distance
seeds_gps <- map2(gps_all, datetimes, ~{
  .x %>% filter(timestamp >= .y-seed_time_before & timestamp <= .y) %>%
    filter(dist_to_carcass < seed_distance)
})

## 7. Get distances from roosts to carcasses
distances <- map2(roosts, inpa_carcs, ~{
  if(!is.null(.x)){
    dist <- .x %>%
      sf::st_as_sf(., coords = c("location_long", "location_lat"), crs = "WGS84") %>%
      sf::st_transform(32636) %>%
      mutate(dist = as.numeric(st_distance(., .y))) %>%
      st_drop_geometry() %>%
      select(local_identifier, roost_date, dist) %>%
      pivot_wider(id_cols = "local_identifier", names_from = "roost_date", values_from = "dist", names_prefix = "roost_")
  }else{
    dist <- NULL
  }
  return(dist)
})

## 8. Load who's who
# ww <- read_csv(here("data/raw/whoswho_vultures_20230920_new.csv"),
#                col_select = 1:40)
# write_rds(ww, file = here("data/created/ww.RDS"))
ww <- readRDS(here("data/created/ww.RDS"))
glimpse(ww)

## 9. Get age_group ILV
www <- ww %>%
  dplyr::select(Nili_id, Movebank_id, Nili_id, birth_year, sex) %>%
  mutate(age_2023 = 2023-birth_year,
         age_2024 = 2024-birth_year,
         age_group_2023 = case_when(age_2023 > 5 ~ "02_adult",
                                    age_2023 <= 5 ~ "01_juv_sub",
                                    .default = NA),
         age_group_2024 = case_when(age_2024 > 5 ~ "02_adult",
                                    age_2024 <= 5 ~ "01_juv_sub",
                                    .default = NA)) %>%
  select("local_identifier" = "Movebank_id", age_group_2023, age_group_2024) %>%
  distinct()

## 10. Combine age_group ILV with distances to get ILVs data frame
# XXX start here
ilvs <- map(distances, ~{
  .x %>%
    left_join(www, by = "local_identifier")
}) # let's reduce this down to just one age group column, depending on the carcass

all(map_dbl(ilvs, nrow) == map_dbl(distances, nrow)) # should be TRUE--we shouldn't have added any rows.

for(i in 1:length(ilvs)){
  if(lubridate::year(inpa_carcs[[i]]$dateOnly) == 2023){
    ilvs[[i]] <- ilvs[[i]] %>% select(-age_group_2024) %>%
      rename("age_group" = age_group_2023)
  }else{
    ilvs[[i]] <- ilvs[[i]] %>% select(-age_group_2023) %>%
      rename("age_group" = age_group_2024)
  }
}
save(ilvs, file = here("test_dynamic_nbda/data/ilvs.Rda"))

## 11. Make gps (i.e. remove days before the carcass)
gps <- map2(gps_all, datetimes, ~{
  .x %>%
    filter(dateOnly >= lubridate::date(.y) & dateOnly <= lubridate::date(.y) + days_after)
})
rows_removed <- map_dbl(gps_all, nrow) - map_dbl(gps, nrow)
pct_removed <- 100*(rows_removed/(map_dbl(gps_all, nrow)))# just checking that this looks reasonable. We removed a couple days of data, so values around 30% make sense.
save(gps, file = here("test_dynamic_nbda/data/gps.Rda"))

## 12. Get arrivals/sightings of the carcass
# at_carcass <- map2(gps, inpa_carcs, ~.x %>%
#                      mutate(carcID = .y$carcID) %>%
#                      filter(dist_to_carcass < 400 & ground_speed < 5))

see_carcass <- map2(gps, inpa_carcs, ~.x %>%
                      mutate(carcID = .y$carcID) %>%
                      filter(dist_to_carcass < 1000))

## 13. Get firsts
# Get first arrival of each vulture to the carcass
# firsts <- map2(at_carcass, inpa_carcs, ~{
#   if(nrow(.x) > 1){
#     out <- .x %>%
#       filter(timestamp >= .y$datetime) %>%
#       arrange(timestamp) %>%
#       group_by(local_identifier) %>%
#       slice(1) %>%
#       ungroup() %>%
#       arrange(timestamp)
#     if(nrow(out) > 0){
#       out$rownumber <- 1:nrow(out)
#       return(out)
#     }else{
#       out <- data.frame(local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, individual_id = NA, tag_local_identifier = NA, location_long = NA, location_lat = NA, dist_to_carcass = NA, carcID = .y$carcID, geometry = NA, rownumber=  NA)
#       return(out)
#     }
#   }else{
#     out <- data.frame(local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, individual_id = NA, tag_local_identifier = NA, location_long = NA, location_lat = NA, dist_to_carcass = NA, carcID = .y$carcID, geometry = NA, rownumber=  NA)
#     return(out)
#   }
# }) 
# map_dbl(firsts, nrow)
# map_dbl(firsts, ~.x %>% filter(!is.na(local_identifier)) %>% nrow(.)) # needed to do this because not all instances of 1 row are actually zeroes.
# # XXX why do we have so many carcasses provisioned that have nobody arriving? That seems weird!!!
# save(firsts, file = here("test_dynamic_nbda/data/firsts.Rda"))

# Get first time each vulture saw the carcass
firsts_see <- map2(see_carcass, inpa_carcs, ~{
  if(nrow(.x) > 1){
    out <- .x %>%
      filter(timestamp >= .y$datetime) %>%
      arrange(timestamp) %>%
      group_by(local_identifier) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(timestamp)
    if(nrow(out) > 0){
      out$rownumber <- 1:nrow(out)
      return(out)
    }else{
      out <- data.frame(local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, individual_id = NA, tag_local_identifier = NA, location_long = NA, location_lat = NA, dist_to_carcass = NA, carcID = .y$carcID, geometry = NA, rownumber=  NA)
      return(out)
    }
  }else{
    out <- data.frame(local_identifier = NA, tag_id = NA, timestamp = NA, dateOnly = NA, ground_speed = NA, individual_id = NA, tag_local_identifier = NA, location_long = NA, location_lat = NA, dist_to_carcass = NA, carcID = .y$carcID, geometry = NA, rownumber=  NA)
    return(out)
  }
}) 
save(firsts_see, file = here("test_dynamic_nbda/data/firsts_see.Rda"))

# has_visits <- map_dbl(firsts, ~nrow(.x[!is.na(.x$local_identifier),])) > 0
# save(has_visits, file = here("test_dynamic_nbda/data/has_visits.Rda"))

has_sightings <- map_dbl(firsts_see, ~nrow(.x[!is.na(.x$local_identifier),])) > 0
save(has_sightings, file = here("test_dynamic_nbda/data/has_sightings.Rda"))

# Everything after this will be subsetted by has_visits or has_sightings; won't be calculated otherwise.
## 14. Get oa
# oa <- map(firsts[has_visits], "local_identifier")
oa_see <- map(firsts_see[has_sightings], "local_identifier")
# save(oa, file = here("test_dynamic_nbda/data/oa.Rda"))
save(oa_see, file = here("test_dynamic_nbda/data/oa_see.Rda"))
# oa_num <- map(oa, order)
oa_see_num <- map(oa_see, order)
# save(oa_num, file = here("test_dynamic_nbda/data/oa_num.Rda"))
save(oa_see_num, file = here("test_dynamic_nbda/data/oa_see_num.Rda"))
# oa_indivs_sorted <- map(oa, sort)
oa_see_indivs_sorted <- map(oa_see, sort)

## 14.5 Get acquisition times
# acq_times <- map(firsts[has_visits], ~.x$timestamp)
see_times <- map(firsts_see[has_sightings], ~.x$timestamp)

## 15. Get GPS subsets for flight (four different intervals)
# gps_flight_allday <- map(gps[has_visits], ~.x %>%
#                            group_by(dateOnly) %>%
#                            group_split())
gps_flight_allday_see <- map(gps[has_sightings], ~.x %>%
                               group_by(dateOnly) %>%
                               group_split())

# gps_flight_cumulative <- vector(mode = "list", length = length(gps[has_visits]))
# for(i in 1:length(gps[has_visits])){
#   times <- acq_times[[i]]
#   subsets <- vector(mode = "list", length = length(times))
#   for(j in 1:length(times)){
#     subsets[[j]] <- gps[has_visits][[i]] %>%
#       filter(timestamp <= times[j])
#   }
#   gps_flight_cumulative[[i]] <- subsets
# }

gps_flight_cumulative_see <- vector(mode = "list", length = length(gps[has_sightings]))
for(i in 1:length(gps[has_sightings])){
  times <- see_times[[i]]
  subsets <- vector(mode = "list", length = length(times))
  for(j in 1:length(times)){
    subsets[[j]] <- gps[has_sightings][[i]] %>%
      filter(timestamp <= times[j])
  }
  gps_flight_cumulative_see[[i]] <- subsets
}

# gps_flight_1hr <- vector(mode = "list", length = length(gps[has_visits]))
# for(i in 1:length(gps[has_visits])){
#   times <- acq_times[[i]][!is.na(acq_times[[i]])]
#   if(length(times) > 0){
#     subsets <- vector(mode = "list", length = length(times))
#     for(j in 1:length(times)){
#       subsets[[j]] <- gps[has_visits][[i]] %>%
#         filter(timestamp >= times[j]-hours(1) & timestamp <= times[j])
#     }
#   }else{
#     subsets <- "blank" # assigning this to NULL wasn't working
#   }
#   gps_flight_1hr[[i]] <- subsets
# }
# length(gps_flight_1hr)

gps_flight_1hr_see <- vector(mode = "list", length = length(gps[has_sightings]))
for(i in 1:length(gps[has_sightings])){
  times <- see_times[[i]][!is.na(see_times[[i]])]
  if(length(times) > 0){
    subsets <- vector(mode = "list", length = length(times))
    for(j in 1:length(times)){
      subsets[[j]] <- gps[has_sightings][[i]] %>%
        filter(timestamp >= times[j]-hours(1) & timestamp <= times[j])
    }
  }else{
    subsets <- "blank" # assigning this to NULL wasn't working
  }
  gps_flight_1hr_see[[i]] <- subsets
}
length(gps_flight_1hr_see)

# gps_flight_3hr <- vector(mode = "list", length = length(gps[has_visits]))
# for(i in 1:length(gps[has_visits])){
#   times <- acq_times[[i]][!is.na(acq_times[[i]])]
#   if(length(times) > 0){
#     subsets <- vector(mode = "list", length = length(times))
#     for(j in 1:length(times)){
#       subsets[[j]] <- gps[has_visits][[i]] %>%
#         filter(timestamp >= times[j]-hours(3) & timestamp <= times[j])
#     }
#   }else{
#     subsets <- "blank" # assigning this to NULL wasn't working
#   }
#   gps_flight_3hr[[i]] <- subsets
# }
# length(gps_flight_3hr)

gps_flight_3hr_see <- vector(mode = "list", length = length(gps[has_sightings]))
for(i in 1:length(gps[has_sightings])){
  times <- see_times[[i]][!is.na(see_times[[i]])]
  if(length(times) > 0){
    subsets <- vector(mode = "list", length = length(times))
    for(j in 1:length(times)){
      subsets[[j]] <- gps[has_sightings][[i]] %>%
        filter(timestamp >= times[j]-hours(3) & timestamp <= times[j])
    }
  }else{
    subsets <- "blank" # assigning this to NULL wasn't working
  }
  gps_flight_3hr_see[[i]] <- subsets
}
length(gps_flight_3hr_see)


## 16. Get roost nets
# Have to make sure that all individuals in oa are included in the roost network, and no others.

# map(roosts[has_visits], ~unique(.x$roost_date)) # XXX why are there different numbers of roosts? Is it boundaries of the month?

# roosts_dates <- map(roosts[has_visits], ~{
#   .x %>%
#     group_by(roost_date) %>%
#     group_split() %>%
#     map(., ~st_as_sf(.x, coords = c("location_long", "location_lat"), crs = "WGS84") %>%
#           st_transform(32636))
# })

roosts_dates_see <- map(roosts[has_sightings], ~{
  .x %>%
    group_by(roost_date) %>%
    group_split() %>%
    map(., ~st_as_sf(.x, coords = c("location_long", "location_lat"), crs = "WGS84") %>%
          st_transform(32636))
})

# roosts_pairwise_distances <- map(roosts_dates, ~{
#   outout <- map(.x, ~{
#     ids <- .x$local_identifier
#     out <- as.data.frame(st_distance(.x)) %>%
#       mutate(across(everything(), as.numeric))
#     row.names(out) <- ids
#     colnames(out) <- ids
#     return(out)
#   })
#   return(outout)
# })

roosts_pairwise_distances_see <- map(roosts_dates_see, ~{
  outout <- map(.x, ~{
    ids <- .x$local_identifier
    out <- as.data.frame(st_distance(.x)) %>%
      mutate(across(everything(), as.numeric))
    row.names(out) <- ids
    colnames(out) <- ids
    return(out)
  })
  return(outout)
})

thresh <- 500 # 500m threshold for roosting together. should check Orr's paper to see if I can find a better threshold.
# roosts_bin <- map(roosts_dates, ~{
#   outout <- map(.x, ~{
#     ids <- .x$local_identifier
#     out <- as.data.frame(st_distance(.x)) %>%
#       mutate(across(everything(), as.numeric))
#     out[out < 500] <- 1
#     out[out >= 500] <- 0
#     row.names(out) <- ids
#     colnames(out) <- ids
#     return(out)
#   })
#   return(outout)
# })
roosts_bin_see <- map(roosts_dates_see, ~{
  outout <- map(.x, ~{
    ids <- .x$local_identifier
    out <- as.data.frame(st_distance(.x)) %>%
      mutate(across(everything(), as.numeric))
    out[out < 500] <- 1
    out[out >= 500] <- 0
    row.names(out) <- ids
    colnames(out) <- ids
    return(out)
  })
  return(outout)
})
save(roosts_bin_see, file = here("test_dynamic_nbda/data/roosts_bin_see.Rda"))

## 17. Get flight nets (whole days)
get_fl_bin <- function(dat){
  if(is.data.frame(dat)){
    if(nrow(dat) > 0){
      self_edges <- data.frame(ID1 = sort(unique(dat$local_identifier)),
                               ID2 = sort(unique(dat$local_identifier)),
                               value = 0)
      out <- suppressMessages(vultureUtils::getFlightEdges(dat, roostPolygons = NULL,
                                                           consecThreshold = 1,
                                                           idCol = "local_identifier",
                                                           return = "edges")) %>%
        select(ID1, ID2) %>%
        distinct() %>%
        mutate(value = 1) %>%
        bind_rows(self_edges) %>%
        arrange(ID1, ID2) %>%
        pivot_wider(id_cols = "ID1", names_from = "ID2", values_fill = 0) %>%
        select(ID1, all_of(.$ID1)) %>% # get the rows and columns to be in the same order
        as.data.frame() # because apparently we can't set row names on a tibble anymore, ugh
      row.names(out) <- out$ID1 # doing this because it makes indexing easier later
    }else{
      out <- "blank"
    }
  }else{
    out <- "blank"
  }
  return(out)
}

# fl_allday_bin <- map(gps_flight_allday, ~{
#   map(.x, ~get_fl_bin(.x))
# }, .progress = T)
# length(fl_allday_bin)
# save(fl_allday_bin, file = here("test_dynamic_nbda/data/fl_allday_bin.Rda"))
# load(here("test_dynamic_nbda/data/fl_allday_bin.Rda"))

fl_allday_bin_see <- map(gps_flight_allday_see, ~{
  map(.x, ~get_fl_bin(.x))
}, .progress = T)
save(fl_allday_bin_see, file = here("test_dynamic_nbda/data/fl_allday_bin_see.Rda"))
load(here("test_dynamic_nbda/data/fl_allday_bin_see.Rda"))

## 18. Get flight nets (four different intervals)
# fl_cumulative_bin <- map(gps_flight_cumulative, ~{
#   map(.x, ~get_fl_bin(.x))
# }, .progress = T)
# length(fl_cumulative_bin)
# save(fl_cumulative_bin, file = here("test_dynamic_nbda/data/fl_cumulative_bin.Rda"))
# load(here("test_dynamic_nbda/data/fl_cumulative_bin.Rda"))

fl_cumulative_bin_see <- map(gps_flight_cumulative_see, ~{
  map(.x, ~get_fl_bin(.x))
}, .progress = T)
save(fl_cumulative_bin_see, file = here("test_dynamic_nbda/data/fl_cumulative_bin_see.Rda"))
load(here("test_dynamic_nbda/data/fl_cumulative_bin_see.Rda"))

# fl_1hr_bin <- map(gps_flight_1hr, ~{
#   map(.x, ~get_fl_bin(.x))
# }, .progress = T)
# length(fl_1hr_bin)
# save(fl_1hr_bin, file = here("test_dynamic_nbda/data/fl_1hr_bin.Rda"))
# load(here("test_dynamic_nbda/data/fl_1hr_bin.Rda"))

fl_1hr_bin_see <- map(gps_flight_1hr_see, ~{
  map(.x, ~get_fl_bin(.x))
}, .progress = T)
save(fl_1hr_bin_see, file = here("test_dynamic_nbda/data/fl_1hr_bin_see.Rda"))
load(here("test_dynamic_nbda/data/fl_1hr_bin_see.Rda"))

# fl_3hr_bin <- map(gps_flight_3hr, ~{
#   map(.x, ~get_fl_bin(.x))
# }, .progress = T)
# length(fl_3hr_bin)
# save(fl_3hr_bin, file = here("test_dynamic_nbda/data/fl_3hr_bin.Rda"))
# load(here("test_dynamic_nbda/data/fl_3hr_bin.Rda"))

fl_3hr_bin_see <- map(gps_flight_3hr_see, ~{
  map(.x, ~get_fl_bin(.x))
}, .progress = T)
save(fl_3hr_bin_see, file = here("test_dynamic_nbda/data/fl_3hr_bin_see.Rda"))
load(here("test_dynamic_nbda/data/fl_3hr_bin_see.Rda"))

# Now we need to edit these networks to make sure 1) they include all individuals that eventually arrived at the carcass, even if just with zeroes, and 2) they don't include any individuals except the ones that arrived at the carcass (since this seems to be a requirement for NBDA, although to be honest I feel kind of uncomfortable with this, so I might revisit it later...)
fix_nets <- function(nets, indivs){
  indivs <- indivs[!is.na(indivs)]
  updated <- vector(mode = "list", length = length(nets))
  for(nt in 1:length(nets)){
    net <- nets[[nt]]
    if(!("ID1" %in% names(net))){ # this is a stupid workaround so the function will work with the co-roost network. Horribly inefficient.
      net$ID1 <- row.names(net)
      net <- net %>%
        relocate(ID1)
    }
    
    # Find any that are missing and add them
    missing <- indivs[!(indivs %in% names(net))]
    if(length(missing) > 0){
      toadd <- data.frame(ID1 = missing, ID2 = missing, value = 0) %>% pivot_wider(id_cols = "ID1", names_from = "ID2", values_from = "value", values_fill = 0)
      if(!any(net == "blank")){ # XXX THE PROBLEM IS HERE!!! dealing with missing data. ugh.
        net_updated <- as.data.frame(bind_rows(net, toadd))
      }else{
        net_updated <- as.data.frame(toadd)
      }
      net_updated[is.na(net_updated)] <- 0
      row.names(net_updated) <- net_updated$ID1
    }else{
      net_updated <- net
    }
    net_updated_2 <- net_updated[,-1]
    updated[[nt]] <- net_updated_2[indivs, indivs]

  }
  return(updated)
}

# fl_allday_bin_fixed <- vector(mode = "list", length = length(fl_allday_bin))
# for(i in 1:length(fl_allday_bin)){
#   nets <- fl_allday_bin[[i]]
#   indivs <- oa_indivs_sorted[[i]]
#   fl_allday_bin_fixed[[i]] <- fix_nets(nets, indivs)
# }
# save(fl_allday_bin_fixed, file = here("test_dynamic_nbda/data/fl_allday_bin_fixed.Rda"))
# load(here("test_dynamic_nbda/data/fl_allday_bin_fixed.Rda"))

fl_allday_bin_fixed_see <- vector(mode = "list", length = length(fl_allday_bin_see))
for(i in 1:length(fl_allday_bin_see)){
  nets <- fl_allday_bin_see[[i]]
  indivs <- sort(unique(gps[has_sightings][[i]]$local_identifier))
  fl_allday_bin_fixed_see[[i]] <- fix_nets(nets, indivs)
}
save(fl_allday_bin_fixed_see, file = here("test_dynamic_nbda/data/fl_allday_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/fl_allday_bin_fixed_see.Rda"))

# fl_cumulative_bin_fixed <- vector(mode = "list", length = length(fl_cumulative_bin))
# for(i in 1:length(fl_cumulative_bin)){
#   nets <- fl_cumulative_bin[[i]]
#   indivs <- oa_indivs_sorted[[i]]
#   fl_cumulative_bin_fixed[[i]] <- fix_nets(nets, indivs)
# }
# save(fl_cumulative_bin_fixed, file = here("test_dynamic_nbda/data/fl_cumulative_bin_fixed.Rda"))
# load(here("test_dynamic_nbda/data/fl_cumulative_bin_fixed.Rda"))

fl_cumulative_bin_fixed_see <- vector(mode = "list", length = length(fl_cumulative_bin_see))
for(i in 1:length(fl_cumulative_bin_see)){
  nets <- fl_cumulative_bin_see[[i]]
  indivs <- sort(unique(gps[has_sightings][[i]]$local_identifier))
  fl_cumulative_bin_fixed_see[[i]] <- fix_nets(nets, indivs)
}
save(fl_cumulative_bin_fixed_see, file = here("test_dynamic_nbda/data/fl_cumulative_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/fl_cumulative_bin_fixed_see.Rda"))

# fl_1hr_bin_fixed <- vector(mode = "list", length = length(fl_1hr_bin))
# for(i in 1:length(fl_1hr_bin)){
#   nets <- fl_1hr_bin[[i]]
#   indivs <- oa_indivs_sorted[[i]]
#   fl_1hr_bin_fixed[[i]] <- fix_nets(nets, indivs)
# }
# save(fl_1hr_bin_fixed, file = here("test_dynamic_nbda/data/fl_1hr_bin_fixed.Rda"))
# load(here("test_dynamic_nbda/data/fl_1hr_bin_fixed.Rda"))

fl_1hr_bin_fixed_see <- vector(mode = "list", length = length(fl_1hr_bin_see))
for(i in 1:length(fl_1hr_bin_see)){
  nets <- fl_1hr_bin_see[[i]]
  indivs <- sort(unique(gps[has_sightings][[i]]$local_identifier))
  fl_1hr_bin_fixed_see[[i]] <- fix_nets(nets, indivs)
}
save(fl_1hr_bin_fixed_see, file = here("test_dynamic_nbda/data/fl_1hr_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/fl_1hr_bin_fixed_see.Rda"))

# fl_3hr_bin_fixed <- vector(mode = "list", length = length(fl_3hr_bin))
# for(i in 1:length(fl_3hr_bin)){
#   nets <- fl_3hr_bin[[i]]
#   indivs <- oa_indivs_sorted[[i]]
#   fl_3hr_bin_fixed[[i]] <- fix_nets(nets, indivs)
# }
# save(fl_3hr_bin_fixed, file = here("test_dynamic_nbda/data/fl_3hr_bin_fixed.Rda"))
# load(here("test_dynamic_nbda/data/fl_3hr_bin_fixed.Rda"))

fl_3hr_bin_fixed_see <- vector(mode = "list", length = length(fl_3hr_bin_see))
for(i in 1:length(fl_3hr_bin_see)){
  nets <- fl_3hr_bin_see[[i]]
  indivs <- sort(unique(gps[has_sightings][[i]]$local_identifier))
  fl_3hr_bin_fixed_see[[i]] <- fix_nets(nets, indivs)
}
save(fl_3hr_bin_fixed_see, file = here("test_dynamic_nbda/data/fl_3hr_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/fl_3hr_bin_fixed_see.Rda"))

# Another check: fl_allday_bin_fixed should have one network per day, while the others should have one network per acquisition event.
# map_dbl(fl_allday_bin_fixed, length) 
# all(map_dbl(fl_cumulative_bin_fixed, length) == map(oa, length)) # TRUE
# all(map_dbl(fl_1hr_bin_fixed, length) == map(oa, length)) # TRUE
# all(map_dbl(fl_3hr_bin_fixed, length) == map(oa, length)) # TRUE

# okay good! The flight network data looks okay. Now we need to subset the roost networks similarly.
# length(roosts_bin) # should be same as number of valid diffusions--good.
# map_dbl(roosts_bin, length) # this is an odder result. What's going on here? XXX
# XXX it does look like the same one that only had one day of data for the flight network also only has one day of data for the roost network. That's normal. But I don't understand where the 6 comes from, and why one of them only has 4 while some others have 5.
# roosts_bin_fixed <- vector(mode = "list", length = length(roosts_bin))
# for(i in 1:length(roosts_bin)){
#   nets <- roosts_bin[[i]]
#   indivs <- oa_indivs_sorted[[i]]
#   roosts_bin_fixed[[i]] <- fix_nets(nets, indivs)
# }
# save(roosts_bin_fixed, file = here("test_dynamic_nbda/data/roosts_bin_fixed.Rda"))

roosts_bin_fixed_see <- vector(mode = "list", length = length(roosts_bin_see))
for(i in 1:length(roosts_bin_see)){
  nets <- roosts_bin_see[[i]]
  indivs <- sort(unique(gps[has_sightings][[i]]$local_identifier))
  roosts_bin_fixed_see[[i]] <- fix_nets(nets, indivs)
}
save(roosts_bin_fixed_see, file = here("test_dynamic_nbda/data/roosts_bin_fixed_see.Rda"))

# Make networks -----------------------------------------------------------
# roosts_bin_nets <- map(roosts_bin_fixed, ~{map(.x, ~{igraph::graph_from_adjacency_matrix(as.matrix(.x), mode = "undirected", diag = F)})})
roosts_bin_nets_see <- map(roosts_bin_fixed_see, ~{map(.x, ~{igraph::graph_from_adjacency_matrix(as.matrix(.x), mode = "undirected", diag = F)})})

# fl_allday_bin_nets <- map(fl_allday_bin_fixed, ~{map(.x, ~{igraph::graph_from_adjacency_matrix(as.matrix(.x), mode = "undirected", diag = F)})})
fl_allday_bin_nets_see <- map(fl_allday_bin_fixed_see, ~{map(.x, ~{igraph::graph_from_adjacency_matrix(as.matrix(.x), mode = "undirected", diag = F)})})

# fl_cumulative_bin_nets <- map(fl_cumulative_bin_fixed, ~{map(.x, ~{igraph::graph_from_adjacency_matrix(as.matrix(.x), mode = "undirected", diag = F)})})
fl_cumulative_bin_nets_see <- map(fl_cumulative_bin_fixed_see, ~{map(.x, ~{igraph::graph_from_adjacency_matrix(as.matrix(.x), mode = "undirected", diag = F)})})

# fl_1h_bin_nets <- map(fl_1hr_bin_fixed, ~{map(.x, ~{igraph::graph_from_adjacency_matrix(as.matrix(.x), mode = "undirected", diag = F)})})
fl_1h_bin_nets_see <- map(fl_1hr_bin_fixed_see, ~{map(.x, ~{igraph::graph_from_adjacency_matrix(as.matrix(.x), mode = "undirected", diag = F)})})

# fl_3h_bin_nets <- map(fl_3hr_bin_fixed, ~{map(.x, ~{igraph::graph_from_adjacency_matrix(as.matrix(.x), mode = "undirected", diag = F)})})
fl_3h_bin_nets_see <- map(fl_3hr_bin_fixed_see, ~{map(.x, ~{igraph::graph_from_adjacency_matrix(as.matrix(.x), mode = "undirected", diag = F)})})

# save(roosts_bin_nets, file = here("test_dynamic_nbda/data/roosts_bin_nets.Rda"))
save(roosts_bin_nets_see, file = here("test_dynamic_nbda/data/roosts_bin_nets_see.Rda"))

# save(fl_allday_bin_nets, file = here("test_dynamic_nbda/data/fl_allday_bin_nets.Rda"))
save(fl_allday_bin_nets_see, file = here("test_dynamic_nbda/data/fl_allday_bin_nets_see.Rda"))

# save(fl_cumulative_bin_nets, file = here("test_dynamic_nbda/data/fl_cumulative_bin_nets.Rda"))
save(fl_cumulative_bin_nets_see, file = here("test_dynamic_nbda/data/fl_cumulative_bin_nets_see.Rda"))

# save(fl_1h_bin_nets, file = here("test_dynamic_nbda/data/fl_1h_bin_nets.Rda"))
save(fl_1h_bin_nets_see, file = here("test_dynamic_nbda/data/fl_1h_bin_nets_see.Rda"))

# save(fl_3h_bin_nets, file = here("test_dynamic_nbda/data/fl_3h_bin_nets.Rda"))
save(fl_3h_bin_nets_see, file = here("test_dynamic_nbda/data/fl_3h_bin_nets_see.Rda"))

