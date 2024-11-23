library(tidyverse)
library(igraph)
library(NBDA)
library(vultureUtils)
library(targets)
library(sf)
library(here)
library(tidygraph)
library(ggraph)

# Now time to look at the NBDA code ---------------------------------------
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
plusfour <- date_placed + days(4)

# Downloaded code for tits finding colored wool (Vistalli et al. 2023)
# Determined I need the following:

# *For one single carcass, at first*
#   - dataset of GPS points in the south, from beginning of placement day to +4 days
gps <- data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv") %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>%
  st_transform(32636) %>%
  st_crop(bbox_south)
gps_mycarc <- gps %>%
  filter(dateOnly >= date_placed & dateOnly <= plusfour) %>%
  st_transform("WGS84") %>%
  bind_cols(st_coordinates(.)) %>%
  rename("location_long" = X,
         "location_lat" = Y) %>%
  mutate(dateOnly = lubridate::ymd(dateOnly))
dim(gps_mycarc) # a lot of gps points in the south between day of placement and 4 days later
length(unique(gps_mycarc$individual_id)) # 59 individuals detected in the south between day of placement and 4 days later.

# - co-flight network, from beginning of placement day to +4 days. consecThreshold = 1.
rp <- sf::st_read(here("data/raw/roosts50_kde95_cutOffRegion.kml"))
coflight <- getFlightEdges(gps_mycarc, roostPolygons = rp, roostBuffer = 50,
               consecThreshold = 1, distThreshold = 1000,
               speedThreshUpper = NULL, speedThreshLower = 5,
               timeThreshold = "10 minutes",
               idCol = "individual_id",
               return = "sri")
g <- igraph::graph_from_data_frame(coflight, directed = FALSE)
t_g <- tidygraph::as_tbl_graph(g) %>% activate(edges) %>%
  filter(!is.na(sri) & sri > 0)
ggraph(t_g) +
  geom_edge_link(aes(width = sri), alpha = 0.5)+
  geom_node_point(size = 4, color = "dodgerblue")+
  scale_edge_width(range = c(0, 1))+
  theme_classic()
coflight_adj <- as_adjacency_matrix(t_g, attr=  "sri")

# - roosts for each vulture on each night, beginning the night before the carcass was placed. In order to get this, we need to add two extra days of data (since both morning and night are necessary for roost computation). Need date_placed-days(1) through plusfour + days(1)
gps_mycarc_forroosts <- gps %>%
  # need to include the previous day
  filter(dateOnly >= (date_placed-days(1)) & dateOnly <= (plusfour + days(1))) %>%
  st_transform("WGS84") %>%
  bind_cols(st_coordinates(.)) %>%
  rename("location_long" = X,
         "location_lat" = Y) %>%
  mutate(dateOnly = lubridate::ymd(dateOnly))
r <- get_roosts_df(gps_mycarc_forroosts, id = "individual_id")
length(unique(r$roost_date)) # we have roosts for nights including the night before the carcass was placed.
unique(r$roost_date)
table(r$roost_date)
r_list <- r %>%
  group_by(roost_date) %>%
  group_split() %>%
  map(., ~sf::st_as_sf(.x, coords = c("location_long", "location_lat"), remove = F, crs = "WGS84") %>% st_transform(32636))

# - matrices of pairwise distances between them
indivs <- map(r_list, ~.x$individual_id)
roost_pairwise_distances <- map(r_list, ~as.data.frame(st_distance(.x))) %>%
  map2(., indivs, ~{
    out <- .x
    names(out) <- .y
    row.names(out) <- .y
    return(out)
    }) %>%
  map(., ~.x %>% mutate(across(everything(), as.numeric)))

# - daily centers of activity for each vulture, excluding roosts
activity_list <- 
  gps_mycarc %>%
  group_by(dateOnly) %>%
  group_split()

activity_centers <- map(activity_list, ~.x %>%
                          group_by(individual_id) %>%
                          summarize(st_union(geometry)) %>%
                          st_centroid() %>%
                          st_transform(32636))
indivs_activity <- map(activity_centers, ~.x$individual_id)

# - matrix of pairwise distances between them
activity_centers_pairwise_distances <- map(activity_centers, ~as.data.frame(st_distance(.x))) %>%
  map2(., indivs_activity, ~{
    out <- .x
    names(out) <- .y
    row.names(out) <- .y
    return(out)
  }) %>%
  map(., ~.x %>% mutate(across(everything(), as.numeric)))

# - overall center of activity for each vulture from carcass placement day to +4 days
overall_activity_centers <- gps_mycarc %>%
  group_by(individual_id) %>%
  summarize(st_union(geometry)) %>%
  st_centroid() %>%
  st_transform(32636)

# - matrix of pairwise distances between them
indivs_overall <- overall_activity_centers$individual_id
overall_activity_centers_pairwise_distances <- as.data.frame(st_distance(overall_activity_centers)) %>%
  mutate(across(everything(), as.numeric))
names(overall_activity_centers_pairwise_distances) <- indivs_overall
row.names(overall_activity_centers_pairwise_distances) <- indivs_overall

# - time of first arrivals to carcass
distances <- as.numeric(st_distance(st_transform(gps_mycarc, 32636), mycarc))
gps_mycarc$dist_to_carc <- distances

at_carcass <- gps_mycarc %>%
  filter(ground_speed < 5) %>%
  filter(dist_to_carc < 250)

first_at_carcass <- at_carcass %>%
  arrange(timestamp) %>%
  group_by(individual_id) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(timestamp) %>%
  mutate(rownumber = 1:n())

# quick viz
first_at_carcass %>% 
  ggplot(aes(x = timestamp, y = rownumber))+
  geom_point()+
  geom_path()+
  labs(y = "Number of unique individuals",
       x = "Time")

# - age and sex for each individual
# it won't be trivial to join this information onto the gps data, since I didn't do a good job maintaining the IDs. Let's just proceed with no ILVs for now and add them later.

# Got the data, now time to try modeling ----------------------------------
# Bringing code over from imitate_wool.R whenever possible
library(asnipe)
library(sp)
library(geosphere)
library(Imap) # XXX had to install devtools version; this is no loner on CRAN
library(data.table)
library(ggplot2)
library(gridExtra)

# 2) Create foraging network ------------------------------------------------
# AKA co-flight network
glimpse(coflight)
t_g
t_g %>% activate(nodes) %>% pull(name) %>% length() # 59 vultures included in the co-flight network.

# 3) Create distance matrix ----------------------------------------------------
# 3.3. calculate distances between nest boxes and dispensers --------------
# AKA Distance between roosts on each night and the carcass site
st_crs(r_list[[1]]) == st_crs(mycarc)
r_list <- map(r_list, ~.x %>% mutate(dist_to_carc = as.numeric(st_distance(.x, mycarc))))

# 3.4. Extract which boxes are within 200m of dispensers ------------------
# KG: can skip this; already restricted to south

# 3.5. Calculate the neighbour matrix -------------------------------------
# AKA nighttime neighbour matrix = pairwise distances between roosts
map(roost_pairwise_distances, dim)
# AKA daytime neighbour matrix = pairwise distances between daily activity centers
map(activity_centers_pairwise_distances, dim)
# AKA daytime neighbour matrix (aggregate) = pairwise distances between overall activity centers
dim(overall_activity_centers_pairwise_distances)

# Get inverted square root of all of these:
rpd_invsq <- map(roost_pairwise_distances, ~.x %>% 
                   mutate(across(everything(), 
                                 ~1/sqrt(.x))))
rpd_invsq <- map(rpd_invsq, ~{
  .x[.x==Inf] <- 0
  return(.x)
})

acpd_invsq <- map(activity_centers_pairwise_distances, ~.x %>%
                    mutate(across(everything(),
                                  ~1/sqrt(.x))))
acpd_invsq <- map(acpd_invsq, ~{
  .x[.x==Inf] <- 0
  return(.x)
})

oacpd_invsq <- overall_activity_centers_pairwise_distances %>%
  mutate(across(everything(),
                ~1/sqrt(.x)))
oacpd_invsq[oacpd_invsq == Inf] <- 0
hist(as.matrix(oacpd_invsq))
# use the inverted square root of distances 
# so that locations closer together have higher values

# 4) Load data from wool dispensers -----------------------------------------
# AKA: get arrival data to the carcass
dim(at_carcass)
head(at_carcass)
glimpse(at_carcass)

# How many different birds have visited
length(unique(at_carcass$individual_id))
# 33 (out of a network of 58-59 depending on the day)

# # 5) Load individual-level variables (ILVs) ---------------------------------------------------------
# XXX SKIPPING THIS FOR NOW
# XXX couldn't make the NBDA data without ILVs, so maybe we can put in some dummy ones?
load(here("data/ILVs.combined.RDA"))
str(ILVs.combined) # just a simple data frame
fakeILVs <- data.frame(individual_id = unique(gps_mycarc$individual_id),
                       age = 1,
                       sex = 1)
# columns:
# - Age: 
# - Sex:
# - First.visits: Date (yymmddHHMMSS) of first visit to a dispenser XXX any dispenser, or the target one?
# - D1-D5: distance (in m) to each dispenser # XXX change to distance to roosts/etc?
# - D1.visited-D5.visited: 0 if not visited dispenser, 1 if registered on the respective dispenser #XXX ?
# - closest.dispenser: distance (in m) to the closest dispenser # XXX ?

# 5.1) Double-check stability of foraging network -------------------------
# AKA: investigate stability of co-flight network (e.g. over days 1, 2, 3)
rows_day1 <- gps_mycarc %>%
  filter(dateOnly == date_placed)
rows_day2 <- gps_mycarc %>%
  filter(dateOnly == date_placed + days(1))
rows_day3 <- gps_mycarc %>%
  filter(dateOnly == date_placed + days(2))

indivs <- unique(gps_mycarc$individual_id)
getadj <- function(rows){
  cf <- getFlightEdges(rows, roostPolygons = rp, consecThreshold = 1, distThreshold = 1000, idCol = "individual_id", return = "sri")
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

library(vegan)

mantel(coflight_day1, coflight_day2) # a significant mantel test would suggest that the matrices are significantly correlated. This is not significant: 0.04, with a mantel statistic of 0.05 (extremely low correlation!)

# I would expect the same between coflight_day2 and coflight_day3
mantel(coflight_day2, coflight_day3) # slightly higher correlation: 0.19, and this time it is significant (0.001). Interesting! So the matrices are significantly correlated. This is probably because they are missing so many values... should really use the new co-flight code instead I think.

# Anyway, though, this is just one carcass, and I suspect that in general we are going to need to use dynamic networks. There's no way that the co-flight network will be consistently correlated over time.
#XXX to do--read papers on dynamic networks for NBDA

# 6.1. prepare matrices -------------------------------------------------

# extract how many points each vulture had recorded at the carcass
at_carcass %>%
  st_drop_geometry() %>%
  group_by(individual_id) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) # okay but this isn't the same as the number of independent visits (i.e. separated by at least one point that's not at the carcass)

gps_mycarc <- gps_mycarc %>%
  mutate(at_carcass = case_when(ground_speed < 5 & dist_to_carc < 250 ~ T,
                                .default = F)) %>%
  arrange(individual_id, timestamp) %>%
  mutate(visit = NA)

current_indiv <- gps_mycarc$individual_id[1]
current_visit <- 0
for(i in 1:nrow(gps_mycarc)){
  if(gps_mycarc$at_carcass[i] == FALSE){ # if not at carcass, no visit
    gps_mycarc$visit[i] <- NA
  }else{ # if at carcass
    if(gps_mycarc$individual_id[i] != gps_mycarc$individual_id[i-1]){ # if new indiv
      current_visit <- current_visit + 1 # next visit number
      gps_mycarc$visit[i] <- current_visit
    }else{ # if same indiv
      if(gps_mycarc$at_carcass[i-1] == TRUE){ # if previous row was also at carcass, same visit 
        gps_mycarc$visit[i] <- current_visit
      }else{ # if previous row was not at carcass, then new visit
        current_visit <- current_visit + 1 #next visit number
        gps_mycarc$visit[i] <- current_visit
      }
    }
  }
}

at_carcass <- gps_mycarc %>%
  filter(at_carcass == T)

at_carcass %>%
  group_by(individual_id) %>%
  st_drop_geometry() %>%
  summarize(npoints = n(),
            nvisits = length(unique(visit))) %>%
  arrange(desc(npoints), desc(nvisits))

# Average and max number of visits by a single individual to the carcass over the 4-day period
at_carcass %>%
  group_by(individual_id) %>%
  st_drop_geometry() %>%
  summarize(npoints = n(),
            nvisits = length(unique(visit))) %>%
  arrange(desc(npoints), desc(nvisits)) %>%
  summarize(mnpoints = mean(npoints),
            maxpoints = max(npoints),
            mnvisits = mean(nvisits),
            maxvisits = max(nvisits))

# 6.2. Check for correlation between foraging and neighbour matrix ----------------------------------------------
# AKA: check for correlation between co-flight network and the oacpd_invsq (overall activity centers pairwise distances inverse square) matrix (or whatever spatial matrix I ultimately decide to use) 
dim(coflight_adj)
dim(oacpd_invsq)

library(vegan)
mantel(coflight_adj, oacpd_invsq, permutations = 9999)

# Mantel statistic based on Pearson's product-moment correlation 
# 
# Call:
# mantel(xdis = coflight_adj, ydis = oacpd_invsq, permutations = 9999) 
# 
# Mantel statistic r: 0.02873 
#       Significance: 0.1781 
# 
# Upper quantiles of permutations (null model):
#    90%    95%  97.5%    99% 
# 0.0424 0.0565 0.0721 0.0883 
# Permutation: free
# Number of permutations: 9999

# non-significant correlation between co-flight and spatial network, which means we can include them both in the NBDA analysis at the same time


# 6.3. Prepare NBDA data ------------------------------------------------------------------
prepare.NBDA.data <- function(at_carcass, include.all, ILVs.include){
  # dispenser.data <- dispenser.data[order(dispenser.data$date.time),] # ensure it is sorted according to date
  at_carcass <- at_carcass %>%
    ungroup() %>%
    arrange(timestamp)
  
  # location <- unique(dispenser.data$Location)
  location <- rep(mycarc$carcID, nrow(at_carcass)) # all the same carcass (for now)
  
  # # We extract which females were breeding in boxes within 200m of the respective dispenser
  # IDs.included <- subset(ILVs.combined$PIT_f, ILVs.combined[,location]<=200 | ILVs.combined[,paste(location, ".visited", sep="")]==1 ) 
  # IDs.included <- subset(IDs.included, IDs.included %in% IDs.to.include.in.NBDA)
  # # we remove boxes D04, R06, G33 (females breeding in two boxes - we retained the ones closer to the dispenser)
  # ILVs.sub.disp <- subset(ILVs.combined, ILVs.combined$PIT_f %in% IDs.included & !(ILVs.combined$Box %in% c("D04", "R06", "G33")))
  # # subset the dispenser data to only those females
  # dispenser.data <- subset(dispenser.data, dispenser.data$PIT %in%  IDs.included)
  # XXX not doing this because we're including all the individuals and the ILV matrix already includes all of them.
  
  # order data in ascending order (according to PIT tag)
  # ILVs.sub.disp <- ILVs.sub.disp[order(ILVs.sub.disp$PIT_f),] # order ascending according to PIT tag
  fakeILVs <- fakeILVs[order(fakeILVs$individual_id),]
  
  # # subset the two networks to those IDs
  # forage.net <- foraging.network.NBDA[rownames(foraging.network.NBDA) %in% IDs.included, colnames(foraging.network.NBDA) %in% IDs.included]
  # neighbour.net <- neighbour_matrix.NBDA.new[rownames(neighbour_matrix.NBDA.new) %in% IDs.included, colnames(neighbour_matrix.NBDA.new) %in% IDs.included]
  # XXX not doing this because again, we're using all the individuals
  
  # create an array with the two matrices
  # assMatrix.nbda <- array(data=c(forage.net, neighbour.net), dim=c(nrow(forage.net), ncol(forage.net), 2))
  assMatrix.nbda <- array(data=c(coflight_adj, oacpd_invsq), 
                          dim = c(nrow(coflight_adj), ncol(coflight_adj), 2))
  
  # # create objects in the global environment for each ILV
  # species.nbda <- as.matrix(ILVs.sub.disp$Species) 
  # species.nbda[species.nbda!="GRETI"] <- -0.5 # we assign -0.5 for non great tits
  # species.nbda[species.nbda=="GRETI"] <- 0.5 # we assign 0.5 for great tits
  # species.nbda <- as.matrix(as.numeric(species.nbda)) # ILVs need to be defined as matrices
  # age.nbda <- as.matrix(ILVs.sub.disp$Age) 
  # age.nbda[age.nbda=="first.year"] <- -0.5 # -0.5 for juveniles
  # age.nbda[age.nbda=="adult"] <- 0.5 # 0.5 for adult birds
  # age.nbda <- as.matrix(as.numeric(age.nbda))
  # distance.nbda <- as.matrix(as.numeric(ILVs.sub.disp[,location])) # we extract the distance of the box to the respective dispenser
  # distance.nbda[is.na(distance.nbda)] <- mean(distance.nbda[!is.na(distance.nbda)]) # for those not breeding in boxes, add the average distance to the respective dispenser
  # # we take the square root and standardize the distance for better model fitting - using the standard deviation and mean across all distances calculated above
  # distance.nbda <- (sqrt(distance.nbda)-sqrt(mean(dist.vec)))/sqrt(sd(dist.vec))
  # XXX are they going to have to also reorder the networks in order by ID?
  sex.nbda <- as.matrix(fakeILVs$sex) # XXX note that for any future ILVs that are not numeric, need to make them numeric.
  age.nbda <- as.matrix(fakeILVs$age)
  
  # we need to assign these ILVs as objects to the global environment
  # assign(paste("species", location, sep="_"), species.nbda, envir = .GlobalEnv)
  # assign(paste("age", location, sep="_"), age.nbda, envir = .GlobalEnv)
  # assign(paste("distance", location, sep="_"), distance.nbda, envir = .GlobalEnv)
  assign(paste("")) # XXX again, how are we doing this if we're not sure that the order is the same?
  # XXX START HERE
    
}
prepare.NBDA.data <- function(dispenser.data, include.all, ILVs.include){
  

  
  
  ILVs <- paste(ILVs.include, location, sep="_")
  assign(paste("ILVs", location, sep="_"), ILVs, envir = .GlobalEnv)
  
  # extract the order of finding the dispenser from the dispenser data
  t.first <- dispenser.data[match(unique(dispenser.data$PIT), dispenser.data$PIT),]
  order <- NULL
  time <- NULL
  num.visits <- NULL
  for( i in t.first$PIT){
    order[which(t.first$PIT==i)] <- which(rownames(forage.net)==i)
    time[which(t.first$PIT==i)] <- as.POSIXct(as.character(subset(t.first$date.time, t.first$PIT==i)), format="%y%m%d%H%M%S", origin="1970-01-01")-as.POSIXct("21032612300000", format="%y%m%d%H%M%S") # difference in days
    # 26.03.21 12:30 CEST was the start date of the experiment
  }
  
  object <- NULL
  object$forage.net <- forage.net
  object$neighbour.net <- neighbour.net
  object$ILVs.full <- ILVs.sub.disp
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
                      timeAcq=object$TAc,           
                      endTime=41
  )
  
  return(object2)
}