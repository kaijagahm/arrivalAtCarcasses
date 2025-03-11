# Higher-order interactions
# Focusing on one carcass: Carcass 13
library(data.table)
library(tidyverse)
library(sf)
library(igraph)
library(targets)
library(here)

load(here("test_dynamic_nbda/data/inpa_carcs.Rda"))
load(here("test_dynamic_nbda/data/has_visits.Rda"))
tar_load(bbox_south)
carcs <- inpa_carcs[has_visits] # get the carcasses corresponding to the networks, in case we need them

focal <- inpa_carcs[has_visits][[13]]
gps_2023 <- data.table::fread("data/ACC/2023_hf_period/created/gps_2023.csv")
gps_2024 <- data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv") 
gps <- bind_rows(gps_2023, gps_2024) %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>%
  bind_cols(st_coordinates(.)) %>%
  rename("location_long" = X,
         "location_lat" = Y) %>%
  mutate(dateOnly = lubridate::ymd(dateOnly)) %>%
  st_transform(32636)

head(gps)

gps_focal <- gps %>%
  filter(timestamp >= (focal$datetime - days(1)) & timestamp <= (focal$datetime + days(3))) %>%
  mutate(period = cut.POSIXt(timestamp, breaks = "20 min"),
         dist_to_carcass = as.numeric(st_distance(., focal)))
dim(gps_focal)

flight_only <- gps_focal %>%
  filter(ground_speed >= 5)
dim(flight_only)
periods <- data.frame(period = 1:length(unique(flight_only$period)),
                      period_start = sort(unique(flight_only$period)))

pts_list <- flight_only %>% group_by(period) %>% group_split()

get_simplices <- function(pts){
  indivs <- pts$local_identifier
  within_dist <- st_is_within_distance(pts, dist = 1000, remove_self = TRUE)
  adjacency_list <- map(1:length(within_dist), ~within_dist[[.x]])
  g <- graph_from_adj_list(adjacency_list, mode = "all")
  # Find maximal cliques (highest-order simplices)
  maximal_cliques <- max_cliques(g, min = 2)
  # Convert maximal cliques to a list of point indices
  maximal_simplices <- map(maximal_cliques, as.integer)
  maximal_simplices_ids <- map(maximal_simplices, ~indivs[.x])
  return(maximal_simplices_ids)
}

periods_simplices_list <- map(pts_list, get_simplices)

fn <- function(x){
  out <- map(x, ~data.frame(ID = .x)) %>%
    rbindlist(idcol = "simplex_id") %>%
    as.data.frame()
  return(out)
}
  
periods_simplices_df <- map(periods_simplices, fn) %>% 
  rbindlist(idcol = "period") %>%
  as.data.frame() %>%
  left_join(periods) %>%
  group_by(period, simplex_id) %>%
  mutate(id = paste0("ID_", 1:n())) %>%
  pivot_wider(id_cols = c("period", "period_start", "simplex_id"),
              values_from = "ID",
              names_from = "id") # okay, so this includes all the simplices

# Now we also need to get the times at which the individuals were first informed (within 1km of carcass) and the times at which they first arrived (within 250m of carcass, on the ground)
first_informed <- gps_focal %>%
  filter(timestamp >= focal$datetime) %>%
  filter(dist_to_carcass < 1000) %>%
  arrange(timestamp) %>%
  group_by(local_identifier) %>%
  slice(1) %>%
  ungroup() %>%
  select(local_identifier, timestamp, ground_speed, location_long, location_lat, period, dist_to_carcass)
nrow(first_informed) == length(unique(gps_focal$local_identifier)) # maybe some individuals never went there?
nrow(first_informed) < length(unique(gps_focal$local_identifier)) # yep, fewer

# How many individuals eventually arrived at the carcass
nrow(first_informed)/length(unique(gps_focal$local_identifier)) # 57% of the individuals eventually detected the carcass

first_arrival <- gps_focal %>%
  filter(timestamp >= focal$datetime) %>%
  filter(ground_speed < 5 & dist_to_carcass < 250) %>%
  arrange(timestamp) %>%
  group_by(local_identifier) %>%
  slice(1) %>%
  ungroup() %>%
  select(local_identifier, timestamp, ground_speed, location_long, location_lat, period, dist_to_carcass)

nrow(first_arrival)/length(unique(gps_focal$local_identifier)) # 47% of the individuals eventually arrived at the carcass

length(unique(first_arrival$local_identifier))/length(unique(first_informed$local_identifier)) # 83% of the birds that flew within 1km of the carcass eventually landed there (answering Matt's question about whether being informed is a good proxy for eventually arriving there)

save(focal, file = here("forNina/focal.Rda"))
save(gps_focal, file = here("forNina/gps_focal.Rda"))
save(first_informed, file = here("forNina/first_informed.Rda"))
save(first_arrival, file = here("forNina/first_arrival.Rda"))
save(periods, file = here("forNina/periods.Rda"))
save(periods_simplices_df, file = here("forNina/periods_simplices_df.Rda"))

write_csv(focal, file = here("forNina/focal.csv"))
write_csv(gps_focal, file = here("forNina/gps_focal.csv"))
write_csv(first_informed, file = here("forNina/first_informed.csv"))
write_csv(first_arrival, file = here("forNina/first_arrival.csv"))
write_csv(periods, file = here("forNina/periods.csv"))
write_csv(periods_simplices_df, file = here("forNina/periods_simplices_df.csv"))
