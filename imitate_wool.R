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
coflight_adj <- as_adjacency_matrix(t_g, attr=  "sri")

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

# ACQUISITION DATA ----------------------------------------------
## Time of acquisition ----------------------------------------------
# - time of first arrivals to carcass
distances <- as.numeric(st_distance(st_transform(gps_mycarc_fromplacement, 32636), mycarc))
gps_mycarc_fromplacement$dist_to_carc <- distances

at_carcass <- gps_mycarc_fromplacement %>%
  mutate(carcID = mycarc$carcID) %>%
  filter(dist_to_carc < 250 & ground_speed < 5) # near carcass and not flying

first_at_carcass <- at_carcass %>%
  arrange(timestamp) %>%
  group_by(local_identifier) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(timestamp) %>%
  mutate(rownumber = 1:n())
length(unique(first_at_carcass$local_identifier)) # 33 individuals arrived at the carcass during the 4 days since placement
nrow(first_at_carcass)

# quick viz
first_at_carcass %>% 
  ggplot(aes(x = timestamp, y = rownumber))+
  geom_point()+
  geom_path()+
  labs(y = "Number of unique individuals",
       x = "Time")

# Modeling ----------------------------------------------

## 2) Create foraging network ------------------------------------------------
# AKA co-flight network
glimpse(coflight)
t_g
t_g %>% activate(nodes) %>% pull(name) %>% length() # 59 vultures included in the co-flight network.

## 3) Create distance matrix ----------------------------------------------------

### 3.5. Calculate the neighbour matrix -------------------------------------
# AKA nighttime neighbour matrix = pairwise distances between roosts
map(roost_pairwise_distances, dim)

# Get inverted square root of all of these:
rpd_invsq <- map(roost_pairwise_distances, ~.x %>% 
                   mutate(across(everything(), 
                                 ~1/sqrt(.x))))
rpd_invsq <- map(rpd_invsq, ~{
  .x[.x==Inf] <- 0
  return(.x)
})
# use the inverted square root of distances so that locations closer together have higher values

## 4) get arrival data to the carcass -----------------------------------------
# (ref: Load data from wool dispensers)
dim(at_carcass)
head(at_carcass)
glimpse(at_carcass)

# How many different birds have visited
length(unique(at_carcass$local_identifier))
# 33 (out of a network of 58-59 depending on the day)

## 5) Load individual-level variables (ILVs) ---------------------------------------------------------
glimpse(ilvs)
load(here("data/ILVs.combined.RDA"))
str(ILVs.combined) # just a simple data frame
glimpse(ilvs)
# columns:
# - local_identifier: the individual ID 
# - dist_to_carc_daybefore: how far away the individual's center of activity was from the carcass site on the day before the carcass was placed
# - Nili_id: redundant with local_identifier
# - sex: m, f, or NA for unknown
# - birth_year: year hatched
# - age_in_2024: numerical age in 2024
# - age_group: >5 is adult; <= 5 is juvenile/subadult

### 5.1) Double-check stability of foraging network -------------------------
# AKA: investigate stability of co-flight network (e.g. over days 1, 2, 3)
rows_day1 <- gps_mycarc %>%
  filter(dateOnly == date_placed)
rows_day2 <- gps_mycarc %>%
  filter(dateOnly == date_placed + days(1))
rows_day3 <- gps_mycarc %>%
  filter(dateOnly == date_placed + days(2))

indivs <- unique(gps_mycarc$local_identifier)
getadj <- function(rows){
  cf <- getFlightEdges(rows, roostPolygons = rp, consecThreshold = 1, distThreshold = 1000, idCol = "local_identifier", return = "sri")
  g <- igraph::graph_from_data_frame(cf, directed = FALSE, vertices = indivs)
  tg <- tidygraph::as_tbl_graph(g) %>%
    activate(edges) %>%
    filter(!is.na(sri) & sri > 0)
  adj <- as_adjacency_matrix(tg, attr = "sri")
  return(adj)
}

coflight_day1 <- getadj(rows_day1)
coflight_day2 <- getadj(rows_day2)
coflight_day3 <- getadj(rows_day3)

mantel(coflight_day1, coflight_day2) # a significant mantel test would suggest that the matrices are significantly correlated. This is not significant: 0.04, with a mantel statistic of 0.05 (extremely low correlation!)

# I would expect the same between coflight_day2 and coflight_day3
mantel(coflight_day2, coflight_day3) # slightly higher correlation: 0.19, and this time it is significant (0.001). Interesting! So the matrices are significantly correlated. This is probably because they are missing so many values... should really use the new co-flight code instead I think.

# Anyway, though, this is just one carcass, and I suspect that in general we are going to need to use dynamic networks. There's no way that the co-flight network will be consistently correlated over time.
#XXX to do--read papers on dynamic networks for NBDA
# CONFIRMS WE NEED A DYNAMIC FORAGING NETWORK! But sticking with static for now for simplicity.

# 6) NBDA - social information to use to find lining material -----------------------------------------------------------------
# 6.2. Check for correlation between foraging and roosting matrices ----------------------------------------------
mantel(coflight_adj, roost_pairwise_distances[[1]], permutations = 9999)
# Mantel statistic based on Pearson's product-moment correlation 
# 
# Call:
# mantel(xdis = coflight_adj, ydis = roost_pairwise_distances[[1]],      permutations = 9999) 
# 
# Mantel statistic r: -0.1754 
#       Significance: 1 
# 
# Upper quantiles of permutations (null model):
#    90%    95%  97.5%    99% 
# 0.0421 0.0530 0.0623 0.0724 
# Permutation: free
# Number of permutations: 9999

mantel(coflight_adj, roost_pairwise_distances[[2]], permutations = 9999) # n.s
# XXX it will be more complicated to test the correlations once we have dynamic networks for both foraging and roosting, but that's okay for now. Also keep in mind that some of these have NAs, which is going to interfere.

# non-significant correlation between foraging and neighbour network, which means we can include them both in the NBDA analysis at the same time
# XXX for now, just going to use the first roost neighbor network so we don't have to deal with anything being dynamic.
nn_fornow <- roost_pairwise_distances[[1]]

# 6.3. Prepare NBDA data ------------------------------------------------------------------
prepare.NBDA.data <- function(at_carcass, include.all, ILVs.include){
  at_carcass <- at_carcass[order(at_carcass$timestamp),] # ensure it is sorted according to date/time
  location <- unique(at_carcass$carcID)
  
  # order data in ascending order (according to PIT tag)
  ilvs_ordered <- ilvs[order(ilvs$local_identifier),] # order ascending according to local identifier

  # create an array with the two matrices
  # XXX START HERE--something is wrong with the format of assMatrix.nbda. It needs to exactly match the GBI matrix produced in the tit project, which means I need to actually rerun that code without assuming a particular format.
  assMatrix.nbda <- array(data = c(coflight_adj, nn_fornow), 
                          dim = c(nrow(coflight_adj), ncol(coflight_adj), 2))
  
  # create objects in the global environment for each ILV
  age.nbda <- as.matrix(ilvs$age_group) 
  age.nbda[age.nbda == "01_juv_sub"] <- -0.5 # -0.5 for juveniles/subadults
  age.nbda[age.nbda == "02_adult"] <- 0.5 # 0.5 for adult birds
  age.nbda <- as.matrix(as.numeric(age.nbda)) # ILVs need to be defined as matrices
  
  sex.nbda <- as.matrix(ilvs$sex) 
  sex.nbda[sex.nbda == "m"] <- -0.5 # -0.5 for males
  sex.nbda[sex.nbda == "f"] <- 0.5 # 0.5 for females
  sex.nbda[is.na(sex.nbda)] <- 0 # 0 when sex is unknown
  sex.nbda <- as.matrix(as.numeric(sex.nbda)) # ILVs need to be defined as matrices
  
  distance.nbda <- as.matrix(ilvs[,"dist_to_carc_daybefore"]) # distance to the current carcass
  distance.nbda[is.na(distance.nbda)] <- mean(distance.nbda[!is.na(distance.nbda)]) # for any where we don't know the activity area for the previous day, set the distance to the mean
  # we take the square root and standardize the distance for better model fitting - using the standard deviation and mean across all distances calculated above
  distance.nbda <- (sqrt(distance.nbda)-sqrt(mean(distance.nbda)))/sqrt(sd(distance.nbda)) # XXX NEED TO CHECK ON STANDARDIZATION--WHAT'S GOING ON WITH MEAN THING HERE?
  
  # we need to assign these ILVs as objects to the global environment
  assign(paste("age", location, sep="_"), age.nbda, envir = .GlobalEnv)
  assign(paste("sex", location, sep="_"), sex.nbda, envir = .GlobalEnv)
  assign(paste("distance", location, sep="_"), distance.nbda, envir = .GlobalEnv)
  
  ILVs <- paste(ILVs.include, location, sep="_")
  assign(paste("ILVs", location, sep="_"), ILVs, envir = .GlobalEnv)
  
  # extract the order of finding the carcass from the carcass data
  t.first <- at_carcass[match(unique(at_carcass$local_identifier), at_carcass$local_identifier),]
  order <- NULL
  time <- NULL
  num.visits <- NULL
  for(i in t.first$local_identifier){
    order[which(t.first$local_identifier==i)] <- which(rownames(coflight_adj)==i)
    time[which(t.first$local_identifier==i)] <- lubridate::ymd_hms(as.character(subset(t.first$timestamp, t.first$local_identifier==i)))
  }
  
  object <- NULL
  object$forage.net <- coflight_adj
  object$roost.net <- nn_fornow
  object$ILVs.full <- ilvs
  object$assMatrix <- assMatrix.nbda
  
  object$OAc <- order
  object$TAc <- time
  
  
  # finally, we create the NBDA data object, which is returned from running the function
  object2 <- nbdaData(label=location,                        
                      assMatrix=object$assMatrix,          
                      asoc_ilv=get(paste("ILVs", location, sep="_")),            
                      int_ilv=get(paste("ILVs", location, sep="_")),            
                      multi_ilv="ILVabsent",        
                      orderAcq=object$OAc,          
                      timeAcq=object$TAc
  )
  
  return(object2)
}


