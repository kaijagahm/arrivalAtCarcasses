# Testing out code to define wild carcasses based on dbscan clustering
library(tidyverse)
library(targets)
# library(stdbscanr) # no longer loading this--using patched code
library(sf)
library(data.table)
library(mapview)
tar_load(non_carcass_bo)
dim(non_carcass_bo)
glimpse(non_carcass_bo)
source("R/stdbscanr_source_patched.R")

# subset down
test <- sf::st_as_sf(non_carcass_bo) %>%
  select(individual_local_identifier, timestamp, year) %>%
  bind_cols(st_coordinates(.))
test22 <- test %>%
  ungroup() %>%
  arrange(timestamp) %>%
  filter(year == 2022) %>%
  mutate(timestamp_numeric = as.numeric(difftime(timestamp, min(timestamp), units = "secs"))) %>%
  arrange(timestamp_numeric)

test23 <- test %>%
  ungroup() %>%
  arrange(timestamp) %>%
  filter(year == 2023) %>%
  mutate(timestamp_numeric = as.numeric(difftime(timestamp, min(timestamp), units = "secs"))) %>%
  arrange(timestamp_numeric)

test24 <- test %>%
  ungroup() %>%
  arrange(timestamp) %>%
  filter(year == 2024) %>%
  mutate(timestamp_numeric = as.numeric(difftime(timestamp, min(timestamp), units = "secs"))) %>%
  arrange(timestamp_numeric)

# I dug into the `find_temporally_connected_points` function and found that it relies on simple subtraction of timestamps without units specified. 
# If I do test22$timestamp[2]-test22$timestamp[1], I get the answer in units of minutes for some reason
test22$timestamp[2]-test22$timestamp[1] # 7.38 mins
# but if I convert these to numeric first, then it works in seconds:
as.numeric(test22$timestamp)[2]-as.numeric(test22$timestamp)[1] # 443
443/60 #7.38  minutes again
# okay so I'm going to run the function on a version of the timestamps converted to numeric

set.seed(3)

# Sensitivity analysis
hours <- c(0.5, 1, 2, 6, 12, 24, 36, 48, 72)
secs <- hours*60*60
outs_22 <- vector(mode = "list", length = length(secs))
outs_23 <- outs_22
outs_24 <- outs_22
eps <- 50
minpts <- 3
for(i in 1:length(secs)){
  outs_22[[i]] <- test22 %>%
    get_clusters_from_data(., x = "X", y = "Y", t = "timestamp_numeric",
                           eps = eps, 
                           eps_t = secs[i], 
                           minpts = minpts) %>% mutate(eps = hours[i])
  
  outs_23[[i]] <- test23 %>%
    get_clusters_from_data(., x = "X", y = "Y", t = "timestamp_numeric",
                           eps = eps, 
                           eps_t = secs[i], 
                           minpts = minpts) %>% mutate(eps = hours[i])
  
  outs_24[[i]] <- test24 %>%
    get_clusters_from_data(., x = "X", y = "Y", t = "timestamp_numeric",
                           eps = eps, 
                           eps_t = secs[i], 
                           minpts = minpts) %>% mutate(eps = hours[i])
}

all <- purrr::list_rbind(outs_22) %>% bind_rows(purrr::list_rbind(outs_23)) %>% bind_rows(purrr::list_rbind(outs_24)) %>%
  mutate(uq = paste(cluster, eps, year, sep = "_"))

all_summ <- all %>%
  mutate(cluster = factor(cluster)) %>%
  filter(!is.na(cluster)) %>%
  group_by(year, eps, cluster) %>%
  summarize(uq = uq[1],
            diff_hrs = difftime(max(timestamp), min(timestamp), units = "hours"),
            n = n(),
            n_indivs = length(unique(individual_local_identifier)))

all_summ %>%
  ggplot(aes(x = as.numeric(eps), y = diff_hrs))+
  geom_abline(aes(slope = 1, intercept = 0), alpha = 0.5)+
  geom_jitter(alpha = 0.5, size = 2, aes(color = factor(year)), width = 1.5)+ 
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(y = "Hours between earliest and latest bout",
       x = "EPS parameter (hours)",
       color = "Year") # okay so a few of these thresholds, through chaining, actually extend the points included in a single cluster. Others don't. It's not obvious from this exactly what we should pick, but this helps us get a sense for what's going on. Also, it appears that all the years behave about the same.

# Regardless, if we pick the wrong threshold it will probably just result in some clusters having some points not included, not in the clusters being removed altogether.

# How many of the clusters have at least three individuals?
all_summ %>%
  ggplot(aes(x = n, y = n_indivs, color = factor(year)))+
  geom_hline(aes(yintercept = 2), alpha = 0.5)+
  geom_point(size = 2, alpha = 0.5) +
  theme_minimal()

all_summ %>%
  filter(n_indivs >= 3) %>%
  group_by(year, eps) %>% summarize(n_wild_carcs = length(unique(uq))) 

# Let's arbitrarily choose 24 hours for this.

# We really do need some ground-truthing data to see if these are even reasonable...
all_touse <- all %>%
  filter(eps == 24) %>%
  sf::st_as_sf() %>%
  mutate(carcID = paste(str_pad(cluster, width = 2, side = "left", pad = "0"), year, sep = "_"))
all22 <- all_touse %>% filter(year == 2022)
all23 <- all_touse %>% filter(year == 2023)
all24 <- all_touse %>% filter(year == 2024)

mapview(all22, zcol = "carcID")
mapview(all23, zcol = "cluster")
mapview(all24, zcol = "cluster")

# Get locations
wild_carcasses_dbscan <- all_touse %>%
  group_by(year, carcID) %>%
  summarize(geometry = sf::st_union(geometry),
            dateOnly = lubridate::date(timestamp)[1],
            nBouts = n(),
            nIndivs = length(unique(individual_local_identifier)),
            mintime = min(timestamp),
            maxtime = max(timestamp)) %>%
  sf::st_centroid() %>%
  ungroup() %>%
  bind_cols(sf::st_coordinates(.)) %>%
  mutate(datetime = mintime,
         datetime = lubridate::ymd_hms(datetime),
         datetime_il = lubridate::with_tz(datetime, tzone = "Israel"))

mapview(wild_carcasses_dbscan, zcol = "year")

mapview(wild_carcasses_dbscan, col.regions = "dodgerblue3")
ggplot(wild_carcasses_dbscan, aes(X, Y))+
  geom_point(color = "dodgerblue3", alpha = 0.75, position = position_nudge(x = 500))+
  theme_minimal()+
  facet_wrap(~year)+
  theme(panel.grid.minor = element_blank())

writeRDS(wild_carcasses_dbscan)

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

wcdb <- wild_carcasses_dbscan %>% filter(datetime >= minmax_dates[[5]] & datetime <= minmax_dates[[6]])
t24 <- st_as_sf(test24)

mapview(t24, col.regions = "black", cex = 2, layer.name = "Non-SFS feeding bouts")+ 
  mapview(wcdb, col.regions = "dodgerblue3", layer.name = "DBSCAN")+
  mapview(wvsf_focal, col.regions = "yellow", layer.name = "Confirmed carcasses")

