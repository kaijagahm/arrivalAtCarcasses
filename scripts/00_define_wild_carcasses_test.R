# Testing out code to define wild carcasses based on dbscan clustering
library(tidyverse)
library(targets)
# library(stdbscanr) # no longer loading this--using patched code
library(sf)
library(data.table)
library(mapview)
tar_load(non_station_bo)
source("R/stdbscanr_source_patched.R")
source("R/functions.R")

# add xy cols
test <- sf::st_as_sf(non_station_bo) %>%
  bind_cols(st_coordinates(.)) %>% # get X and Y cols
  ungroup() %>%
  arrange(timestamp)

test24 <- test %>% # focusing just on 2024 so we can compare with the other data
  filter(year == 2024) %>%
  mutate(timestamp_numeric = as.numeric(difftime(timestamp, min(timestamp), units = "secs"))) %>% arrange(timestamp_numeric)

# # I dug into the `find_temporally_connected_points` function and found that it relies on simple subtraction of timestamps without units specified. 
# # If I do test22$timestamp[2]-test22$timestamp[1], I get the answer in units of minutes for some reason
# test22$timestamp[2]-test22$timestamp[1] # 7.38 mins
# # but if I convert these to numeric first, then it works in seconds:
# as.numeric(test22$timestamp)[2]-as.numeric(test22$timestamp)[1] # 443
# 443/60 #7.38  minutes again
# # okay so I'm going to run the function on a version of the timestamps converted to numeric

set.seed(3)

# Sensitivity analysis
param_hrs <- c(12, 24, 36, 48, 72)
param_dists <- c(50, 100, 200) # distance (meters)
param_minpts <- c(3, 5, 10) 
params <- expand_grid(param_hrs, param_dists, param_minpts) # these are all the possibilities

outs_24 <- vector(mode = "list", length = nrow(params))

for(i in 1:length(outs_24)){
  h <- params$param_hrs[i]
  s <- h*60*60
  d <- params$param_dists[i]
  m <- params$param_minpts[i]
  
  outs_24[[i]] <- test24 %>%
    get_clusters_from_data(., x = "X", y = "Y", t = "timestamp_numeric",
                           eps = d, eps_t = s, minpts = m) %>%
    mutate(param_hrs = h,
           param_dists = d,
           param_minpts = m)
}

wild_carcasses_params <- map(outs_24, ~get_wild_carcasses(.x))
for(i in 1:length(wild_carcasses_params)){
  row <- params[i,]
  wild_carcasses_params[[i]] <- bind_cols(wild_carcasses_params[[i]], row) # add back the parameters
}
params$n_carcs <- map_dbl(wild_carcasses_params, nrow)

all <- purrr::list_rbind(outs_24) %>%
  mutate(id = paste("cl", cluster, param_hrs, param_dists, param_minpts, sep = "_")) %>%
  left_join(params, by = c("param_hrs", "param_dists", "param_minpts"))

all_summ <- all %>%
  mutate(cluster = factor(cluster)) %>%
  filter(!is.na(cluster)) %>%
  group_by(year, param_hrs, param_dists, param_minpts, cluster, n_carcs) %>%
  summarize(id = id[1],
            dur = difftime(max(timestamp), min(timestamp), units = "hours"),
            n = n(),
            n_indivs = length(unique(individual_local_identifier))) %>%
  ungroup()

all_summ %>%
  select(year, param_hrs, param_dists, param_minpts, n_carcs) %>%
  distinct() %>%
  ggplot(aes(x = factor(param_hrs, levels = c("12", "24", "36", "48", "72")), y = factor(param_dists, levels = c("50", "100", "200")), fill = n_carcs))+
  geom_tile()+
  facet_wrap(~param_minpts)+
  scale_fill_viridis_c()+
  labs(y = "Dist", x = "Hours", fill = "Carcasses")
# no matter the parameters, we find only 20-some carcasses when we insist on 10 points per cluster. Makes sense--the feeding bouts are not that common.
# We could remove 10 as an option for minpts, but what if that's actually the truth? We do have some underlying truth here.

# We have info about landings that were confirmed as carcasses. We do not have info about landings that were not confirmed as carcasses (might be able to get it, but don't have it as of now). So we'll be able to detect false negatives and true positives, but not not true negatives or false positives.

all_summ %>%
  ggplot(aes(x = jitter(param_hrs), y = dur, col = factor(param_dists)))+
  geom_point(alpha = 0.7)+
  geom_smooth(alpha = 0.2)+
  facet_wrap(~param_minpts)+
  theme_bw()+
  labs(y = "Carcass duration (hrs)",
       x = "eps_t (hrs)",
       color = "eps (m)",
       title = "Carcass duration by params",
       caption = "Facets are min pts")

all_summ %>%
  ggplot(aes(x = jitter(param_hrs), y = n, col = factor(param_minpts)))+
  geom_point(alpha = 0.7)+
  geom_smooth(alpha = 0.2)+
  facet_wrap(~param_dists)+
  theme_bw()+
  labs(y = "Carcass nbouts",
       x = "eps_t (hrs)",
       color = "Min pts",
       title = "Carcass nbouts by params",
       caption = "Facets are dist")

all_summ %>%
  ggplot(aes(x = jitter(param_hrs), y = n_indivs, col = factor(param_minpts)))+
  geom_point(alpha = 0.7)+
  geom_smooth(alpha = 0.2)+
  facet_wrap(~param_dists)+
  theme_bw()+
  labs(y = "Carcass nvultures",
       x = "eps_t (hrs)",
       color = "Min pts",
       title = "Carcass nvultures by params",
       caption = "Facets are dist")

all_summ %>%
  ggplot(aes(x = jitter(param_hrs), y = dur, col = factor(param_minpts)))+
  geom_point(alpha = 0.7)+
  geom_smooth(alpha = 0.2)+
  facet_wrap(~param_dists)+
  theme_bw()+
  labs(y = "Carcass duration (hrs)",
       x = "eps_t (hrs)",
       color = "Min pts",
       title = "Carcass duration by params",
       caption = "Facets are dist") # duration is the one affected most strongly by eps_t, which makes sense of course. If we similarly had a parameter for geographic spread of the feeding points, I'm sure we would see it scaling with eps; that's only natural.

# Time to do some ground-truthing. How can we compare what we found to the real carcass locations?

# Add in some verification data--verified wild carcasses from the INPA
wildVerified <- readxl::read_excel("data/raw/INPA_alertSystem_wildCarcassData.xlsx")
names(wildVerified) <- c("eventID", "alertDate", "alertTime", "locDate", "locTime", "event", "individual", "species", "deviceSN", "areas", "alertStatus", "transmissionSince", "movedSince", "long", "lat", "nIndivs", "reasonClosed", "notifications", "time", "worker", "checkType", "animalLocated", "animalAlive", "carcassLocated", "carcassMedications", "carcassTreatment", "carcassSpecies", "comment")

wildVerified <- wildVerified %>%
  select(eventID, alertDate, alertTime, locDate, locTime, event, individual, deviceSN, long, lat, nIndivs, reasonClosed, animalLocated, animalAlive, carcassLocated, carcassMedications, carcassTreatment, carcassSpecies)

wvsf <- sf::st_as_sf(wildVerified, coords = c("long", "lat"), crs = "WGS84", remove = F) %>%
  sf::st_transform(32636) %>%
  bind_cols(., st_coordinates(.)) %>%
  mutate(across(c("animalLocated", "animalAlive", "carcassLocated", "carcassMedications"), as.factor)) %>%
  mutate(animalLocated = ifelse(animalLocated == "yes", T, F),
         animalAlive = ifelse(animalAlive == "yes", T, F),
         carcassLocated = ifelse(carcassLocated == "yes", T, F),
         carcassMedications = ifelse(carcassMedications == "yes", T, F))
# note that this contains only instances when the carcass *was* located--Shaked seems to have filtered it down for me.
table(wvsf$carcassLocated, exclude = NULL) # no NAs either.

# How do these fall on a timeline?
wvsf %>%
  ggplot(aes(x = alertDate))+
  geom_histogram()

# Restricting just to our focal time period
tar_load(minmax_dates)
wvsf_focal <- wvsf %>%
  filter(alertDate >= minmax_dates[[5]] & alertDate <= minmax_dates[[6]])
dim(wvsf_focal)
mapview(wvsf_focal)
wvsf_focal %>%
  ggplot(aes(x = alertDate))+
  geom_histogram() # at least superficially, this space/time distribution looks kinda similar...

# mapview(t24, col.regions = "black", cex = 2, layer.name = "Non-SFS feeding bouts")+ 
mapview(wild_carcasses_params[[1]], col.regions = "dodgerblue3", layer.name = "DBSCAN1")+
  mapview(wild_carcasses_params[[2]], col.regions = "darkblue", layer.name = "DBSCAN2")+
  mapview(wvsf_focal, col.regions = "yellow", layer.name = "Confirmed carcasses")

hrs1 <- 12
dist1 <- 50
minpts1 <- 3
idx1 <- which(params$param_hrs == hrs1 & params$param_dists == dist1 & params$param_minpts == minpts1)
layername1 = paste0(hrs1, "hrs_", dist1, "m_", minpts1)

hrs2 <- 24
dist2 <- 200
minpts2 <- 3
idx2 <- which(params$param_hrs == hrs2 & params$param_dists == dist2 & params$param_minpts == minpts2)
layername2 = paste0(hrs2, "hrs_", dist2, "m_", minpts2)

mapview(t24, col.regions = "black", cex = 2, layer.name = "Non-SFS feeding bouts")+
  mapview(wvsf_focal, col.regions = "yellow", layer.name = "Confirmed carcasses", homebutton = F)+
  # mapview(wild_carcasses_params[[idx1]], col.regions = "darkblue", layer.name = layername1)+
  mapview(wild_carcasses_params[[idx2]], col.regions = "dodgerblue2", layer.name = layername2)

# 50 meters is definitely too small
# 12 hours is too short--at least 24
# Some of the places where there is no confirmed carcass could be because the rangers know that they don't have to check--either bc it's in a nature reserve or because the cluster is on a cliff/obviously a mistake

# Let's re-do the clustering with the parameters that Orr told me to use in the 2025-11-19 meeting
# 200 meters, 24 hours, at least 3 points per cluster.
layername2 = paste0(hrs2, "hrs_", dist2, "m_", minpts2)
hrs24_m200_min3 <- wild_carcasses_params[[which(params$param_hrs == 24 & params$param_dists == 200 & params$param_minpts == 3)]]

r <- sf::st_read("data/raw/roosts50_kde95_cutOffRegion.kml")
mapview(t24, col.regions = "black", cex = 2, layer.name = "Non-SFS feeding bouts")+
  mapview(wvsf_focal, col.regions = "yellow", layer.name = "Confirmed carcasses", homebutton = F)+
  mapview(wild_carcasses_params[[idx2]], col.regions = "dodgerblue2", layer.name = layername2)+
  mapview(r, col.regions = "magenta")

tar_load(wild_carcasses)
wc <- wild_carcasses %>%
  filter(year == 2024)

r <- sf::st_read("data/raw/roosts50_kde95_cutOffRegion.kml")
mapview(t24, col.regions = "black", cex = 2, layer.name = "Non-SFS feeding bouts")+
  mapview(wvsf_focal, col.regions = "yellow", layer.name = "Confirmed carcasses", homebutton = F)+
  mapview(wild_carcasses_params[[idx2]], col.regions = "dodgerblue2", layer.name = layername2)+
  mapview(r, col.regions = "magenta")+
  mapview(wc, col.regions = "red")

tar_load(non_station_bo_prepped)
st_write(t24, "data/created/non_sfs_feeding_bouts_2024.kml")
st_write(non_station_bo_prepped, "data/created/non_sfs_feeding_bouts_2022_2023_2024.kml")
st_write(wc, "data/created/cluster_centroids_200m_24hr_min3_2024.kml")
st_write(wild_carcasses, "data/created/cluster_centroids_200m_24hr_min3_2022_2023_2024.kml")
