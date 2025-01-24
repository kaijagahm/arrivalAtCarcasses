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

# Load data ---------------------------------------
# Note: XXX will need to go back to the bout calculation and change the time zone to israel time
tar_load(all_carcasses_annotated)
tar_load(all_bouts_annotated)
tar_load(bbox_south)
aca <- all_carcasses_annotated %>% filter(year == "2024") %>% st_crop(bbox_south)
all_bouts_annotated %>%
  filter(carcID %in% aca$carcID) %>%
  group_by(carcID) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) # let's use one with a lot of bouts: 4874955

mycarc <- aca %>%
  filter(carcID == "4874955")
date_placed <- lubridate::date(mycarc$datetime)
date_before <- date_placed - days(1)
plusfour <- date_placed + days(4)

# Downloaded code for tits finding colored wool (Vistalli et al. 2023)
# Determined I need the following:

# *For one single carcass, at first*
#   - dataset of GPS points in the south, from one day before placement day to +4 days
gps <- data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv") %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>%
  st_transform(32636) %>%
  st_crop(bbox_south)
gps_mycarc <- gps %>%
  filter(dateOnly >= date_before & dateOnly <= plusfour) %>%
  st_transform("WGS84") %>%
  bind_cols(st_coordinates(.)) %>%
  rename("location_long" = X,
         "location_lat" = Y) %>%
  mutate(dateOnly = lubridate::ymd(dateOnly))
dim(gps_mycarc) # a lot of gps points in the south between one day before day of placement and 4 days after placement
length(unique(gps_mycarc$local_identifier)) # 59 individuals detected

# - for calculating activity areas on the day before: get just that one day of data
gps_mycarc_daybefore <- gps_mycarc %>%
  filter(dateOnly == date_before)
dim(gps_mycarc_daybefore)

gps_mycarc_fromplacement <- gps_mycarc %>%
  filter(dateOnly > date_before)
dim(gps_mycarc_fromplacement)
all_individuals <- sort(unique(gps_mycarc_fromplacement$local_identifier))
length(all_individuals)

# ACQUISITION DATA ----------------------------------------------
## Time of acquisition ----------------------------------------------
# - time of first arrivals to carcass
distances <- as.numeric(st_distance(st_transform(gps_mycarc_fromplacement, 32636), mycarc))
gps_mycarc_fromplacement$dist_to_carc <- distances

at_carcass <- gps_mycarc_fromplacement %>%
  mutate(carcID = mycarc$carcID) %>%
  filter(dist_to_carc < 250 & ground_speed < 5) # near carcass and not flying

firsts <- at_carcass %>%
  filter(timestamp >= mycarc$datetime) %>%
  arrange(timestamp) %>%
  group_by(local_identifier) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(timestamp) %>%
  mutate(rownumber = 1:n())
length(unique(firsts$local_identifier)) # 33 individuals arrived at the carcass during the 4 days since placement
nrow(firsts)

# quick viz
firsts %>% 
  ggplot(aes(x = timestamp, y = rownumber))+
  geom_point()+
  geom_path()+
  labs(y = "Number of unique individuals",
       x = "Time")
# XXX as a side note, this makes it pretty clear that the appropriate scale on which to vary the dynamic networks is daily!

## 4) get arrival data to the carcass -----------------------------------------
# (ref: Load data from wool dispensers)
dim(at_carcass)
head(at_carcass)
glimpse(at_carcass)

# How many different birds have visited
length(unique(at_carcass$local_identifier))
# 33 (out of a network of 58-59 depending on the day)
n_indivs <- length(unique(at_carcass$local_identifier))
indivs <- unique(at_carcass$local_identifier)

write_csv(at_carcass, file = here("test_dynamic_nbda/data/at_carcass.csv"))
write_csv(firsts, file = here("test_dynamic_nbda/data/firsts.csv"))

# NETWORKS ---------------------------------------
## Co-flight (foraging) ---------------------------------------
### --UPDATE DYNAMICALLY EVENTUALLY, BUT FOR NOW STATIC OVER ENTIRE PERIOD
# - co-flight network, from beginning of placement day to +4 days. consecThreshold = 1.
rp <- sf::st_read(here("data/raw/roosts50_kde95_cutOffRegion.kml"))
coflight <- getFlightEdges(gps_mycarc_fromplacement, roostPolygons = rp, roostBuffer = 50,
                           consecThreshold = 1, distThreshold = 1000,
                           speedThreshUpper = NULL, speedThreshLower = 5,
                           timeThreshold = "10 minutes",
                           idCol = "local_identifier",
                           return = "sri")
g <- igraph::graph_from_data_frame(coflight, directed = FALSE, vertices = all_individuals)
t_g <- tidygraph::as_tbl_graph(g) %>% activate(edges) %>%
  filter(!is.na(sri) & sri > 0)
ggraph(t_g) +
  geom_edge_link(aes(width = sri), alpha = 0.5)+
  geom_node_point(size = 4, color = "dodgerblue")+
  scale_edge_width(range = c(0, 1))+
  theme_classic()
coflight_adj <- as_adjacency_matrix(t_g, attr=  "sri", sparse = F)

## Co-roosting (following) ---------------------------------------
### --UPDATE DYNAMICALLY EACH DAY
# - roosts for each vulture on each night, beginning the night before the carcass was placed. In order to get this, we need to add two extra days of data (since both morning and night are necessary for roost computation). Need date_before through plusfour + days(1)
gps_mycarc_forroosts <- gps %>%
  # need to include the previous day
  filter(dateOnly >= date_before & dateOnly <= (plusfour + days(1))) %>%
  st_transform("WGS84") %>%
  bind_cols(st_coordinates(.)) %>%
  rename("location_long" = X,
         "location_lat" = Y) %>%
  mutate(dateOnly = lubridate::ymd(dateOnly))
r <- get_roosts_df(gps_mycarc_forroosts, id = "local_identifier")
length(unique(r$roost_date)) # we have roosts for nights including the night before the carcass was placed.


# create dummy data frame for adding any missing indivs
dummy <- data.frame(local_identifier = all_individuals, 
                    location_lat = NA, 
                    location_long = NA)

unique(r$roost_date)
table(r$roost_date)
r_list <- r %>%
  group_by(roost_date) %>%
  group_split() %>%
  map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"), remove = F, crs = "WGS84") %>% 
        st_transform(32636))

missing <- map(r_list, ~all_individuals[!(all_individuals %in% .x$local_identifier)])

fill_in <- map(missing, ~dummy %>% filter(local_identifier %in% .x))

r_list <- map2(r_list, fill_in, ~{
  if(nrow(.y) > 0){
    out <- bind_rows(.x, .y) %>%
      arrange(local_identifier)
  }else{out <- .x %>%
    arrange(local_identifier)}
  return(out)})

# - matrices of pairwise distances between them (co-roost network; changes each day)
roost_pairwise_distances <- map(r_list, ~as.data.frame(st_distance(.x))) %>%
  map(., ~{
    row.names(.x) <- all_individuals
    colnames(.x) <- all_individuals
    return(.x)
  }) %>%
  map(., ~.x %>% mutate(across(everything(), as.numeric)))

# Get inverted square root of all of these:
rpd_invsq <- map(roost_pairwise_distances, ~.x %>% 
                   mutate(across(everything(), 
                                 ~1/sqrt(.x))))
rpd_invsq <- map(rpd_invsq, ~{
  .x[.x==Inf] <- 0
  return(.x)
})
rpd_invsq <- map(rpd_invsq, as.matrix)
# Subset to the individuals involved in the diffusion
rpd_invsq_subset <- map(rpd_invsq, ~{
  out <- .x[indivs, indivs]
  return(out)
})
map(rpd_invsq_subset, dim) # should all be 33x33
# use the inverted square root of distances so that locations closer together have higher values

roost_dates <- lubridate::ymd(map_chr(r_list, ~as.character(max(.x$roost_date, na.rm = T))))
acquisition_dates <- sort(unique(firsts$dateOnly)) # these are the dates on which acquisition events occurred. So we only need to keep the roost networks from the night before the first date to the night before the last one
roost_dates_tokeep <- which(roost_dates %in% roost_dates[roost_dates >= min(acquisition_dates-days(1)) & roost_dates < max(acquisition_dates)]) # don't need to include the roost network for the night *after* the last acquisition day

# Save each of the roost networks as a matrix so we can use it more easily in the analysis
for(i in roost_dates_tokeep){
  filename <- paste0("test_dynamic_nbda/data/roost_networks/r", i, ".csv")
  write.csv(rpd_invsq_subset[[i]], file = here(filename))
}

n_roost_networks <- length(roost_dates_tokeep)
write_rds(roost_dates[roost_dates_tokeep], file = here("test_dynamic_nbda/data/roost_networks/roost_dates.RDS"))

# INDIVIDUAL-LEVEL VARIABLES ----------------------------------------------
## Distance to carcass (day before) ----------------------------------------------
### activity centers
activity_centers <- gps_mycarc_daybefore %>%
  group_by(local_identifier) %>%
  summarize(st_union(geometry)) %>%
  st_centroid() %>%
  st_transform(32636) # XXX maybe change this to only include daylight points?

## distance between those activity centers and the carcass location (to use as an ILV)
dists_to_carc <- as.numeric(st_distance(activity_centers, mycarc))

## create ilvs data frame
ilvs <- st_drop_geometry(activity_centers) %>%
  bind_cols("dist_to_carc_daybefore" = dists_to_carc)

## Age ----------------------------------------------
# ww <- read_csv(here("data/raw/whoswho_vultures_20230920_new.csv"),
#                col_select = 1:40)
# write_rds(ww, file = here("data/created/ww.RDS"))
ww <- readRDS(here("data/created/ww.RDS"))
glimpse(ww)
www <- ww %>%
  dplyr::select(Nili_id, Movebank_id, Nili_id, birth_year, sex)

www_tojoin <- www %>%
  filter(Movebank_id %in% ilvs$local_identifier) %>%
  distinct()
dim(www_tojoin) #59 rows, yay

## check before joining:
all(www_tojoin$Movebank_id %in% ilvs$local_identifier) # TRUE
all(ilvs$local_identifier %in% www_tojoin$Movebank_id) # TRUE

ilvs <- ilvs %>%
  left_join(www_tojoin, by = c("local_identifier" = "Movebank_id"))

table(ilvs$sex, exclude = NULL) # we have sex info for almost all of them

## Calculate age in 2024
ilvs <- ilvs %>%
  mutate(age_in_2024 = 2024-birth_year)

## Assign age groups
ilvs <- ilvs %>%
  mutate(age_group = case_when(age_in_2024 >5 ~ "02_adult",
                               age_in_2024 <= 5 ~ "01_juv_sub",
                               .default = NA),
         age_group = factor(age_group),
         sex = factor(sex))
dim(ilvs) # 59 individuals still

# Modeling ----------------------------------------------

## 2) Create foraging network ------------------------------------------------
# AKA co-flight network
glimpse(coflight)
t_g
t_g %>% activate(nodes) %>% pull(name) %>% length() # 59 vultures included in the co-flight network.

# Subset co-flight network to only include the individuals involved in the diffusion
t_g_subset <- t_g %>%
  activate(nodes) %>%
  filter(name %in% indivs)
t_g_subset %>% activate(nodes) %>% pull(name) %>% length() # 33 vultures

# Let's figure out the order of acquisition
indivs # these are in alphabetical order
ids_order <- firsts$local_identifier
indivs_lookup <- 1:length(indivs)
names(indivs_lookup) <- indivs
oa <- indivs_lookup[ids_order]
write_rds(oa, file = here("test_dynamic_nbda/data/oa.RDS"))
