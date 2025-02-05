# Script to test out several diffusions
# Created 2025-01-28 in advance of the January vulture meeting
# Parameters/specs
# - 3 diffusions
# - Dynamic roost networks (each day)
# OADA for now
# Compare social and asocial
# Seeded demonstrators = anyone who was within 1km of the carcass when it was placed
# Time-constant ILVs: age group, initial distance to carcass
# Just roost network for now; we will add in the co-flight network next

# Load packages -----------------------------------------------------------
library(here)
library(targets)
library(NBDA)
library(tidyverse)
library(sf)


# Distances of each point to the carcass site
gps_mycarcs <- map2(1:nrow(mycarcs), gps_mycarcs, ~{
  dist <- as.numeric(st_distance(st_transform(.y, 32636), mycarcs[.x,]))
  out <- .y %>%
    mutate(dist_to_carc = dist)
  return(out)
})

at_carcass <- map2(gps_mycarcs, mycarcs$carcID, ~.x %>%
                     mutate(carcID = .y) %>%
                     filter(dist_to_carc < 250 & ground_speed < 5))

firsts <- map2(at_carcass, mycarcs$datetime, ~{
  if(nrow(.x) > 1){
    out <- .x %>%
      filter(timestamp >= .y) %>%
      arrange(timestamp) %>%
      group_by(local_identifier) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(timestamp) %>%
      mutate(rownumber = 1:nrow(.))
  }else{
    out <- .x
  }
}) 

n_indivs <- map_dbl(firsts, nrow) # huh, many of these diffusions don't actually have many arrivals. Let's focus in on the ones that do.
all_indivs <- map(firsts, ~sort(unique(.x$local_identifier)))
oas <- map(firsts, ~.x$local_identifier)

touse <- n_indivs > 1

# ILVs
## Distance from carcass on the day before
# Activity areas: day before
gps_daybefore <- map(date_before, ~{
  bef <- gps %>%
    filter(dateOnly == .x) %>%
    sf::st_transform("WGS84") %>%
    bind_cols(st_coordinates(.)) %>%
    rename("location_long" = X,
           "location_lat" = Y) %>%
    mutate(dateOnly = lubridate::ymd(dateOnly))
  return(out)
})

activity_daybefore <- map2(gps_daybefore, 1:nrow(mycarcs), ~{
  centers <- .x %>%
    group_by(local_identifier, dateOnly) %>%
    summarize(activity_center = st_union(geometry), 
              .groups = "drop") %>%
    st_centroid() %>%
    st_transform(32636)
  
  dists_to_carc <- as.numeric(st_distance(centers, mycarcs[.y,]))
  
  centers$dist_to_carc_m <- dists_to_carc
  return(centers)
})

## Age group
ww <- readRDS(here("data/created/ww.RDS"))
glimpse(ww)
www <- ww %>%
  dplyr::select(Nili_id, Movebank_id, birth_year, sex) %>%
  mutate(age_2023 = 2023-birth_year,
         age_2024 = 2024-birth_year, 
         age_group_2023 = case_when(age_2023 >5 ~ "02_adult",
                                    age_2023 <= 5 ~ "01_juv_sub",
                                    .default = NA),
         age_group_2024 = case_when(age_2024 > 5 ~ "02_adult",
                                    age_2023 <= 5 ~ "01_juv_sub",
                                    .default = NA))

# Get ILVs specific to each carcass
touse
ilvs_list <- map2(all_indivs, activity_daybefore, ~{
  ilvs <- www[www$Movebank_id %in% .x, c("Movebank_id", "age_group_2023", "age_group_2024")]
  ilvs <- ilvs[order(ilvs$Movebank_id, )]
  ilvs$dist_before <- 
  return(ilvs)
})
