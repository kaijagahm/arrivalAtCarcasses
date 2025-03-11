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
# XXX as a side note, this makes it pretty clear that the appropriate scale on which to vary the dynamic networks is daily!

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
# mantel(coflight_adj, rpd_invsq[[1]], permutations = 9999)
# Mantel statistic based on Pearson's product-moment correlation 
# 
# Call:
# mantel(xdis = coflight_adj, ydis = rpd_invsq[[1]], permutations = 9999) 
# 
# Mantel statistic r: 0.1953 
#       Significance: 1e-04 
# 
# Upper quantiles of permutations (null model):
#    90%    95%  97.5%    99% 
# 0.0384 0.0518 0.0627 0.0774 
# Permutation: free
# Number of permutations: 9999

# XXX this is significantly correlated, so I guess it's bad to use them both in the same model. Uh oh. What's the cutoff for that?

mantel(coflight_adj, rpd_invsq[[2]], permutations = 9999) # also significantly correlated. Uh oh! Will need to figure out what to do.

# significant correlation between foraging and neighbour network, which means we cannot include them both in the NBDA analysis at the same time (XXX but I'm going to do it anyway for the test run.)
# XXX for now, just going to use the first roost neighbor network so we don't have to deal with anything being dynamic.
nn_fornow <- rpd_invsq[[1]]

# 6.3. Prepare NBDA data ------------------------------------------------------------------
prepare.NBDA.data <- function(at_carcass, include.all, ILVs.include){
  at_carcass <- at_carcass[order(at_carcass$timestamp),] # ensure it is sorted according to date/time
  location <- unique(at_carcass$carcID)
  
  # order data in ascending order (according to PIT tag)
  ilvs_ordered <- ilvs[order(ilvs$local_identifier),] # order ascending according to local identifier

  # create an array with the two matrices
  # XXX START HERE--something is wrong with the format of assMatrix.nbda. It needs to exactly match the GBI matrix produced in the tit project, which means I need to actually rerun that code without assuming a particular format.
  assMatrix.nbda <- array(data = unlist(c(coflight_adj, nn_fornow)), 
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

# 6.4. Including all vultures ----------------------------------------------
nbdaData.all <- prepare.NBDA.data(at_carcass = at_carcass, include.all = TRUE, ILVs.include = c("age", "sex", "distance")) # XXX do this for each carcass

# extract the number of birds in each carcass area and the number of learners
# test carcass
dim(nbdaData.all@assMatrix[,,2,1])
length(nbdaData.all@orderAcq)
# 59 birds, 33 learners

# 6.5. Create constraints Vector Matrix ---------------------------------

# here, we provide a function to generate the constraints vector matrix
# which defines the parameter combinations of all NBDA models that are to be run

create.constraints.Vect.Matrix <- function(NBDA_data_object, num_networks, num_ILVs){
  suppressWarnings(
    if(length(NBDA_data_object@asoc_ilv) == 1){ # KG addition
      if(NBDA_data_object@asoc_ilv=="ILVabsent"){
        num.ILV.asoc <- 0
      }else {num.ILV.asoc <- length(NBDA_data_object@asoc_ilv)}
    }else {num.ILV.asoc <- length(NBDA_data_object@asoc_ilv)}) 
  
  
  suppressWarnings(
    if(length(NBDA_data_object@int_ilv) == 1){# KG addition
      if(NBDA_data_object@int_ilv=="ILVabsent"){
        num.ILV.int<- 0
      } else {num.ILV.int<- length(NBDA_data_object@int_ilv)}
    }else {num.ILV.int<- length(NBDA_data_object@int_ilv)})
  
  suppressWarnings(
    if(length(NBDA_data_object@multi_ilv) == 1){# KG addition
      if(NBDA_data_object@multi_ilv=="ILVabsent"){
        num.ILV.multi <- 0
      } else {num.ILV.multi <- length(NBDA_data_object@multi_ilv)}
    }else {num.ILV.multi <- length(NBDA_data_object@multi_ilv)})
  
  vector <- seq(1:(num_networks+num.ILV.asoc+num.ILV.int+num.ILV.multi))
  
  count <- 0 # create an object 'count', which starts on 0
  
  constraintsVect <- matrix(nrow = 10000000, ncol=(num_networks+num.ILV.asoc+num.ILV.int+num.ILV.multi)) # create a matrix to save the combination of parameters in
  constraintsVect[1,] <- vector # the first row gets filled with a sequence from 1:8 (all parameters will be estimated, none are set to 0)
  
  for (i in 1:(length(vector)-1)){ # a loop for each number of parameters to be estimated
    array <- combn(vector, i, FUN = NULL, simplify = TRUE) # for each number of paramters to be estiamted (e.g. 2) create all possible combinations of numbers between 1:12 (e.g. 2&8, 1&5 etc)
    
    for (j in 1:length(array[1,])){ # for each of those combinations
      vector2 <- seq(1:((num_networks+(num.ILV.asoc+num.ILV.int+num.ILV.multi))-i)) # create a second vector with 11-i free spaces
      position <- array[,j] # for each created combination
      count <- count+1 # add +1 to the count
      
      for (k in position){ # at each possible position
        vector2 <- append(vector2, 0, after=k-1) # add a 0 (e.g. 1 0 2 3 ...; 1 2 0 3 4 5 ...; 1 2 3 0 4 5 ....)
      }
      constraintsVect[count+1,] <- vector2 # and save the resulting order in a matrix
    }
  }
  
  
  constraintsVect <- na.omit(constraintsVect) # remove all NAs from the matrix
  
  # extract which columns are networks
  col.networks <- c(1:num_networks)
  
  col.names <- NULL
  
  if(num.ILV.asoc!=0){
    col.names <- rep("asoc", num.ILV.asoc)
  }
  
  if(num.ILV.int!=0){
    col.names <- c(col.names, rep("int", num.ILV.int))
  }
  
  if(num.ILV.multi!=0){
    col.names <- c(col.names, rep("multi", num.ILV.multi))
  }
  
  colnames(constraintsVect) <- c(rep("network", num_networks), col.names)
  
  constraintsVect <- as.matrix(as.data.frame(constraintsVect))
  
  # extract the models containing any social network
  
  social.models <- rep(NA, length(constraintsVect[,1]))
  
  for (k in 1:length(constraintsVect[,1])){
    sum <- sum(constraintsVect[k,1:num_networks])
    if(sum!=0){
      social.models[k] <- k
    }
  }
  social.models <- as.vector(na.omit(social.models))
  
  social.models.matrix <- constraintsVect[social.models,]
  
  # if multiplicative models are fit, we need to adjust the matrix
  # if the multiplicative slots are filled, it automatically fits the parameter for asoc and social (just constrained to be the same)
  # meaning that we can remove it from the asoc and int slot
  
  if(num.ILV.multi!=0){
    social.models.retain <- rep(NA, length(social.model.matrix[,1]))
    multi.models <- rep(NA, length(social.models.matrix[,1]))
    for (k in 1:length(social.models.matrix[,1])){
      sum <- sum(social.models.matrix[k,which(colnames(social.models.matrix)=="multi")])
      sum2 <- sum(social.models.matrix[k, c(which(colnames(social.models.matrix)=="asoc"),which(colnames(social.models.matrix)=="int"))])
      if(sum!=0 & sum2==0){ # if multi models are fit and int and asoc are set to 0
        multi.models[k] <- k # then retain the model
      } else if (sum==0){
        social.models.retain[k] <- k
      }
    }
    
    multi.models <- as.vector(na.omit(multi.models))
    social.models.retain <- as.vector(na.omit(social.models.retain))
    
    models.to.retain <- c(multi.models, social.models.retain)
    
    # these models are retained
    retain.matrix.soc <- social.models.matrix[models.to.retain,]
    
    social.models.matrix <- retain.matrix.soc
  }
  
  # extract the models containing no social network
  
  asocial.models <- rep(NA, length(constraintsVect[,1]))
  
  for (k in 1:length(constraintsVect[,1])){
    sum <- sum(constraintsVect[k,1:num_networks])
    if(sum==0){
      asocial.models[k] <- k
    }
  }
  asocial.models <- as.vector(na.omit(asocial.models))
  
  asocial.models.matrix <- constraintsVect[asocial.models,]
  
  cols.asoc <- which(colnames(constraintsVect)=="asoc")
  
  asocial.retain <- rep(NA, length(asocial.models))
  for (k in 1:length(asocial.models)){
    sum <- sum(asocial.models.matrix[k,which(colnames(constraintsVect)!="asoc")])
    if(sum==0){
      asocial.retain[k] <- k
    }
  }
  
  
  asocial.retain <- as.vector(na.omit(asocial.retain))
  
  asocial.models.to.retain <- asocial.models.matrix[asocial.retain, ]
  asocial.models.to.retain.matrix <- as.matrix(asocial.models.to.retain)
  constraintsVectMatrix <- rbind(social.models.matrix,asocial.models.to.retain)
  
  # add the Null model (without social learning, and no ILVs)
  constraintsVectMatrix <- rbind(constraintsVectMatrix, rep(0, length(constraintsVectMatrix[1,])))
  
  row.names(constraintsVectMatrix) <- NULL
  return(constraintsVectMatrix)
}


constraintsVectMatrix <- create.constraints.Vect.Matrix(NBDA_data_object = nbdaData.all, num_networks = 2, num_ILVs = 3)
colnames(constraintsVectMatrix) <- c("foraging network", "roost network", "asoc_age",
                                     "asoc_sex", "asoc_distance", "soc_age", "soc_sex",
                                     "soc_distance") # XXX KG: it's not clear to me how they determined which order to put species/age/distance in these column names, and I don't love that it's hard-coded. I went with the order they were in in my nbdaData object, but I need to double check on theirs.
# XXX it looks like the order doesn't matter--each of the columns has the same number of rows where it's included and where it's not
table(constraintsVectMatrix[,"asoc_age"]) # 100 0s and the others add up to 100
table(constraintsVectMatrix[,"asoc_sex"])
table(constraintsVectMatrix[,"asoc_distance"])
table(constraintsVectMatrix[,"soc_age"]) # 104 0s and the others add up to 96
table(constraintsVectMatrix[,"soc_sex"])
table(constraintsVectMatrix[,"soc_distance"])

# we have a look at the ouput
head(constraintsVectMatrix)

# each row represents a model, each column a parameters
# the first two columns refer to the two networks
# columns 3-5 to the ILVs influencing asocial learning
# columns 6-8 to the ILVs influencing social learning
# if a parameter is set to 0, it is not estimated in that model
# if it is a number >0, then the parameter is estimated 
# we could in theory constrain model parameters to be the same 
# by setting equal numbers (e.g. foraging netowrk=1, neighbour network =1)
# but here, we want to estimate all parameters independently (hence, consecutive numbers for each additional parameter)
# XXX KG: do the consecutive numbers actually mean anything, or are they just arbitrary placeholders? E.g. would setting two parameters to both be 1 be different than setting them both to 2?

# 6.6. Run TADA on all vultures  --------------------------------------------------------------------

# we run TADA with multiple diffusions

# Now we can run TADA
TADA.finding.all <-
  tadaAICtable(
    nbdadata = list(
      nbdaData.all),
    constraintsVectMatrix = constraintsVectMatrix, 
    writeProgressFile = F,
    cores=1
  )

# we have a look at the resulting AICc table
# each row corresponds to a model
# Akaike weights are given in the last column
# we can see that the top model is model 190 with an Akaike weight of 0.982
head(print(TADA.finding.all@printTable))

write.csv(TADA.finding.all@printTable, "AIC.table.csv")

constraintsVectMatrix[190,]
# we can extract network support via summed Akaike weights
networksSupport(TADA.finding.all)

# support numberOfModels
# 0:0 1.282362e-90              8
# 0:1 9.279020e-28             64
# 1:0 7.398404e-36             64
# 1:2 1.000000e+00             64

# most evidential support for transmission along the foraging and roosting networks (1.00), 
# followed by the roosting network alone (9.28x10^-28)
# followed by transmission through the foraging newtork alone (7.40x10^-36)
# followed by asocial (1.28x10^-90)

# XXX KG This either means that they are DEFINITELY transmitting through both for this one, or that something's off with the scaling.

variableSupport(TADA.finding.all)
#          s1 s2 ASOC:age_4874955 ASOC:sex_4874955 ASOC:distance_4874955 SOCIAL:age_4874955 SOCIAL:sex_4874955
# support  1  1      0.004794253       0.01789115          5.762637e-36       4.055563e-10         0.01789115
#                SOCIAL:distance_4874955
# support            6.762121e-26

#                s1        s2 ASOC:age_D1 ASOC:species_D1 ASOC:distance_D1 SOCIAL:age_D1 SOCIAL:species_D1 SOCIAL:distance_D1
# support 0.7598735 0.1930601   0.1751455       0.1916325        0.6019383     0.2535429         0.1806516          0.1496506

# for an ILV to influence the social or asocial learning rate, we'd need the weight to be >0.5.
# none of the ILVs influence social or asocial learning rate (weights are all < 0.5)

# extracting effect sizes: model averaged estimates
mle <- modelAverageEstimates(TADA.finding.all , averageType = "median")
mle

# s1                      s2                      ASOCIALage_4874955      ASOCIALsex_4874955 
# 8.379196431             0.004781336             0.000000000             0.000000000 
# ASOCIALdistance_4874955 SOCIALage_4874955       SOCIALsex_4874955       SOCIALdistance_4874955 
# 0.000000000             0.000000000             0.000000000             0.000000000 

# we can see that all of the values for the ILVs are 0, which means that the social and asocial learning rates are unaffected by the ILVs in this example.

# 6.7. Extract effect sizes ------------------------------------------------

# we extract effect sizes conditional on the best model
# the best model is model 190 (top model in AIC table)
constraintsVectMatrix[190,]
# foraging network    roost network         asoc_age         asoc_sex    asoc_distance          soc_age 
# 1                   2                     0                0           0                      0 
# soc_sex     soc_distance 
# 0           0 
# it contains the foraging network and the roost network influencing the learning rate

# we create constrained NBDA Data Objects for that specific model
bestModelData <- constrainedNBDAdata(nbdadata=nbdaData.all,constraintsVect =constraintsVectMatrix[190,])

# and run TADA on the best model
model.best.social <-
  tadaFit(
    list(bestModelData)
  )

cbind.data.frame(model.best.social@varNames, model.best.social@outputPar)

# model.best.social@varNames model.best.social@outputPar
# 1            Scale (1/rate):                1.713873e+09
# 2    1 Social transmission 1                8.379196e+00
# 3    2 Social transmission 2                4.781336e-03

# extract the % of events occurring through social learning
prop.solve.social.byevent <-
  oadaPropSolveByST.byevent(
    nbdadata = list(
      bestModelData
    ),
    model = model.best.social
  )
prop.solve.social.byevent 
# this gives an estimate of the likelihood of each event occurring through social learning

# this extracts the overall percentage that have learned socially
prop.solve.social <-
  oadaPropSolveByST(
    nbdadata = list(
      bestModelData
    ),
    model = model.best.social
  )
prop.solve.social # P(Network 1, forage): 0.06
# P(Network 2, roost): 0.92 # XXX noticing that a lot of them are learning through the co-roosting network. definitely need to adjust which networks we're using here, in order for this one to make sense.

# this means that 92+6% of birds have found the carcass through social learning 
# the remaining 2% have done so through asocial learning

# extract profile likelihood. which=1 extracts the first parameter 
# (in this case s for the foraging network)
plotProfLik(which=1,model=model.best.social,range=c(0,10000), resolution=10) 
# we check where the profile likelihood crosses the dotted line to get the
# range for the lower and upper interval - set the ranges accordingly
CIs <- profLikCI(which=1,model=model.best.social, lowerRange = c(1500,2200), upperRange = c(8000,10000)) # extract confidence intervals
CIs
# Lower CI Upper CI 
# 1606.417 9384.638 # XXX I'm not clear on what is meant by such an enormous confidence interval and such a huge value of s. Maybe just that we're really really certain it's social transmission? But it does definitely seem like the scaling is off.

# Now the same thing for the co-roosting network
plotProfLik(which=2,model=model.best.social,range=c(0,10000), resolution=10) 
# XXX what does it mean that this one is just a line? can't extract a CI for this.

# we also want to extract what this means in %

#To get the estimates for the lower bound 
# we have to compute the corresponding values of the other parameters for that model
# if s is constrained to the value of the lower bound 
bestModelDataS1LowerBound <- constrainedNBDAdata(
  nbdadata =
    nbdaData.all,
  constraintsVect = constraintsVectMatrix[190, ],
  offset = c(CIs[1] , rep(0, 7))
)

#Now, when we fit an "asocial" model it constrains the value of s1=0, but then the value of s at the lower bound is added to s as an offset
bestModelS1LowerBound <-
  tadaFit(
    list(
      bestModelDataS1LowerBound
    ) ,
    type = "asocial"
  )
bestModelS1LowerBound@outputPar
# [1] 1713872785

#Now we plug this into the prop solve function to get %
prop.solve.social.lower <-
  oadaPropSolveByST(
    model = bestModelS1LowerBound,
    nbdadata = list(
      bestModelDataS1LowerBound
    )
  )
prop.solve.social.lower
# P(S offset) 
# 0.87353 
# lower bound for % of birds having learned the dial task through social learning is 87.4%

# We repeat it for the upper bound
bestModelDataS1upperBound <- constrainedNBDAdata(
  nbdadata =
    nbdaData.all,
  constraintsVect = constraintsVectMatrix[190, ],
  offset = c(CIs[2] , rep(0, 7))
)

# we again the fit the 'asocial' model with the offset to constrain s to the value of the upper bound
bestModelS1upperBound <-
  tadaFit(
    list(
      bestModelDataS1upperBound
    ) ,
    type = "asocial"
  )
bestModelS1upperBound@outputPar
# [1] 1713872785

#Now plug into the prop solve function
prop.solve.social.upper <-
  oadaPropSolveByST(
    model = bestModelS1upperBound,
    nbdadata = list(
      bestModelDataS1upperBound
    )
  )
prop.solve.social.upper
# P(S offset) 
# 0.87475

# upper bound for % of birds having learned about the location of the carcass through social learning is 87.5% # XXX that's REALLY similar to the lower bound, wow.

# XXX here is where we would extract the effect size for ILVs influencing social learning if we had evidence that any of them did, but we don't.
