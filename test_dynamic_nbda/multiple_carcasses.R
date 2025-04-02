# Multiple carcasses
# - at least 3 vultures detecting
# - at stations
# - during the target time periods
# - order of detection, not arrival
# - roost nets (dynamic)
# - flight nets (dynamic; 4km detection radius; cumulative)
# - ILVs: age (static); distance from roost to carcass (time-varying)

# Load packages -----------------------------------------------------------
library(here)
library(NBDA)
library(tidyverse)
library(sf)
library(lme4)

# Load data
load(here("test_dynamic_nbda/data/has_sightings.Rda"))
load(here("test_dynamic_nbda/data/inpa_carcs.Rda"))
load(here("test_dynamic_nbda/data/oa_see.Rda"))
load(here("test_dynamic_nbda/data/firsts_see.Rda"))

load(here("test_dynamic_nbda/data/fl_cumulative_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/roosts_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/roosts_bin_nets_see.Rda"))
load(here("test_dynamic_nbda/data/fl_cumulative_bin_nets_see.Rda"))

load(here("test_dynamic_nbda/data/gps.Rda"))
load(here("test_dynamic_nbda/data/ilvs.Rda"))

nbdaModSum <- function(model){
  dat <- data.frame(Variable = model@varNames,
                    MLE = model@outputPar,
                    SE = model@se)
  return(dat)
}

# Get the networks --------------------------------------------------------
which(has_sightings) %>% length() # we have 42 carcasses that have any sightings at all, but we also need to restrict it to carcasses with at least 3 vultures sighting the carcass
has_3_sightings <- which(map_dbl(firsts_see[has_sightings], nrow) >= 3)
length(has_3_sightings) # we will have 35 carcasses to work with that have at least 3 sightings.
carcs <- inpa_carcs[has_sightings][has_3_sightings]
oas <- oa_see[has_3_sightings]
firsts <- firsts_see[has_sightings][has_3_sightings]
all(map_dbl(oas, length) == map_dbl(firsts, nrow)) #TRUE (check for correspondence)
# check lengths--should all be 35
length(firsts)
length(oas)
length(carcs)
years <- map(carcs, ~st_drop_geometry(.x) %>% dplyr::select(carcID, datetime, X, Y, stationName, carcassWeight) %>% mutate(year = lubridate::year(datetime), carcID = as.character(carcID)) %>% dplyr::select(-datetime)) %>% purrr::list_rbind() %>% mutate(n_detections = map_dbl(oas, length))

# Make plots of carcass discoveries over time
carcIDs <- map_chr(carcs, ~as.character(.x$carcID[1]))

discoveryplot <- function(firsts, carcID){
  firsts %>% 
    ggplot(aes(x = timestamp, y = rownumber))+
    geom_line()+
    geom_point(size = 2, pch = 21, fill = "white")+
    theme_classic()+
    labs(y = "Cumulative number of vultures", 
         x = "Time", 
         title = "Vultures discovering the carcass", 
         caption = "Number of unique vultures that flew within sight (1km)\nof the carcass since placement")+
    ggtitle(carcID)
}

map2(firsts, carcIDs, ~discoveryplot(.x, .y))

fl_mats <- fl_cumulative_bin_fixed_see[has_3_sightings]
roost_mats <- roosts_bin_fixed_see[has_3_sightings]
fl_nets <- fl_cumulative_bin_nets_see[has_3_sightings]
roost_nets <- roosts_bin_nets_see[has_3_sightings]

# Need to convert the oas into numeric indices instead of a character vector
matrix_orders <- map(roost_mats, ~row.names(.x[[1]]))
oas <- map2(oas, matrix_orders, ~match(.x, .y))

# Need to change the roost networks to have the same number of slices as the flight networks
length(fl_mats[[1]])
length(fl_nets[[1]])
length(roost_mats[[1]])
length(roost_nets[[1]])
# This means we need to figure out which acquisition events happened on which days
dates <- map2(carcs, firsts, ~data.frame(dateOnly = seq.Date(from = .x$dateOnly, to = max(.y$dateOnly), by = "day")) %>% mutate(day = 1:n()))
firsts <- map2(firsts, dates, ~.x %>%
                 left_join(.y))
days_vec <- map(firsts, ~.x$day)

expand <- function(roost_mats, fl_mats, days_vec){
  expanded <- vector(mode = "list", length = length(fl_mats))
  for(i in 1:length(days_vec)){
    tryCatch(
      #this is the chunk of code we want to run
      {expanded[[i]] <- roost_mats[[days_vec[i]]]
      }, error = function(msg){
        expanded[[i]] <- NULL
      })
  }
  return(expanded)
}

roost_mats_expanded <- map(1:length(roost_mats), ~{
  map(expand(roost_mats[[.x]], fl_mats[[.x]], days_vec[[.x]]), ~{
    if(!is.null(.x)){
      as.matrix(.x)
    }}
  )
})

fl_mats <- map(fl_mats, ~map(.x, as.matrix))

r_static_mns <- map(unique(roost_mats_expanded), ~as.matrix(Reduce("+", .x)/length(.x)))

roost_nets_expanded <- map(1:length(roost_mats), ~{
  expand(roost_nets[[.x]], fl_mats[[.x]], days_vec[[.x]])
})

# Okay, so now we have the roost and flight networks, in matrix format, that we're going to need to put into the model. Now let's grab the ilvs
load(here("test_dynamic_nbda/data/ilvs.Rda"))
rename_roost_dates <- function(df){
  to_rename <- names(df)[grepl("roost_", names(df))]
  new_names <- paste0("roost_night", 0:(length(to_rename)-1))
  names(df)[names(df) %in% to_rename] <- new_names
  return(df)
}

my_ilvs <- map(ilvs[has_sightings][has_3_sightings], rename_roost_dates)

ilvs_lists <- map(1:length(my_ilvs), ~{
  ilvs_list <- vector(mode = "list", length = length(roost_mats_expanded[[.x]]))
  for(i in 1:length(ilvs_list)){
    col <- paste0("roost_night", days_vec[[.x]][i]-1)
    if(col %in% names(my_ilvs[[.x]])){
      ilvs_list[[i]] <- my_ilvs[[.x]] %>%
        dplyr::select(local_identifier, age_group, all_of(col))
    }else{
      ilvs_list[[i]] <- "missing roost column"
    }
  }
  return(ilvs_list)
}, .progress = T)

length(ilvs_lists)

# First step: NBDA for all carcasses using dynamic roost network ----------
# No ILVs or flight networks yet--just testing this
n_indivs <- map_dbl(roost_mats, ~nrow(.x[[1]]))

n_timeperiods <- map_dbl(roost_mats_expanded, length)

# Create static roost nets
N.RS <- map2(r_static_mns, n_indivs, ~array(.x, dim = c(.y, .y, 1)))

#Create the empty arrays and slot in the network for each time period
N.RD <- map2(n_indivs, n_timeperiods, ~array(NA, dim = c(.x, .x, 1, .y)))
for(i in 1:length(N.RD)){
  for(j in 1:length(roost_mats_expanded[[i]])){
    if(!is.null(roost_mats_expanded[[i]][[1]])){
      N.RD[[i]][,,1,j] <- array(roost_mats_expanded[[i]][[j]], dim = c(n_indivs[[i]], n_indivs[[i]], 1))
    }
  }
}

N.FD <- map2(n_indivs, n_timeperiods, ~array(NA, dim = c(.x, .x, 1, .y)))
for(i in 1:length(N.FD)){
  for(j in 1:length(fl_mats[[i]])){
    if(!is.null(fl_mats[[i]][[1]])){
      N.FD[[i]][,,1,j] <- array(fl_mats[[i]][[j]], dim = c(n_indivs[[i]], n_indivs[[i]], 1))
    }
  }
}

# Now we need a vector specifying which time period corresponds to which detection event. Since we already did the work of expanding the matrices (oops), this vector will just be 1 through the number of detection events.
assMatrixIndices <- map(oas, ~1:length(.x))

#Now we enter the 4 dimensional network and assMatrixIndex as follows
nbdaData_list_dynamic <- vector(mode = "list", length = length(N.RD))
for(i in 1:length(nbdaData_list_dynamic)){
  carcass <- carcIDs[i]
  nbdaData_list_dynamic[[i]] <- nbdaData(label = paste0("Carcass ", carcass),
                                         assMatrix = N.RD[[i]],
                                         orderAcq = oas[[i]],
                                         assMatrixIndex = assMatrixIndices[[i]])
  cat("done with", i, "\n")
}

nbdaData_list_static <- vector(mode = "list", length = length(N.RS))
for(i in 1:length(nbdaData_list_static)){
  carcass <- carcIDs[i]
  nbdaData_list_static[[i]] <- nbdaData(label = paste0("Carcass ", carcass),
                                        assMatrix = N.RS[[i]],
                                        orderAcq = oas[[i]])
  cat("done with", i, "\n")
}

nbdaData_list_dynamic_flight <- vector(mode = "list", length = length(N.FD))
for(i in 1:length(nbdaData_list_dynamic_flight)){
  carcass <- carcIDs[i]
  nbdaData_list_dynamic_flight[[i]] <- nbdaData(label = paste0("Carcass ", carcass),
                                                assMatrix = N.FD[[i]],
                                                orderAcq = oas[[i]],
                                                assMatrixIndex = assMatrixIndices[[i]])
  cat("done with", i, "\n")
}

Mods_N.RS_So <- map(nbdaData_list_static, ~{
  tryCatch({oadaFit(.x)}, error = function(msg){NULL})
})

Mods_N.RD_So <- map(nbdaData_list_dynamic, ~{
  tryCatch({oadaFit(.x)}, error = function(msg){NULL})
})

Mods_N.RS_Aso <- map(nbdaData_list_static, ~{
  tryCatch({oadaFit(.x, type = "asocial")}, error = function(msg){NULL})
})

Mods_N.RD_Aso <- map(nbdaData_list_dynamic, ~{
  tryCatch({oadaFit(.x, type = "asocial")}, error = function(msg){NULL})
})

Mods_N.FD_So <- map(nbdaData_list_dynamic_flight, ~{
  tryCatch({oadaFit(.x)}, error = function(msg){NULL})
})

Mods_N.FD_Aso <- map(nbdaData_list_dynamic_flight, ~{
  tryCatch({oadaFit(.x, type = "asocial")}, error = function(msg){NULL})
})

# Things to get from the models:
getmodstats <- function(mod){
  tryCatch({
    ps <- ifelse(mod@type == "social", unname(nbdaPropSolveByST(model = mod)[1]), NA)
    df <- data.frame(soc = mod@type,
                     loglik = mod@loglik,
                     aic = mod@aic,
                     aicc = mod@aicc,
                     varNames = mod@varNames,
                     outputPar = mod@outputPar, #XXX this will need to change once the model has more params, but for now it's length 1, conveniently.
                     se = mod@se,
                     propsolve = ps)
    return(df)}, 
    error = function(msg){
      df <- data.frame(soc = NA,
                       loglik = NA, 
                       aic = NA, 
                       aicc = NA, 
                       varNames = NA, 
                       outputPar = NA, 
                       se = NA,
                       propsolve = NA)
      return(df)})
  
}
summaries_static <- list_rbind(map(Mods_N.RS_So, getmodstats) %>% setNames(carcIDs), names_to = "carcID") %>% mutate(type = "static", network = "roost")
summaries_dynamic <- list_rbind(map(Mods_N.RD_So, getmodstats) %>% setNames(carcIDs), names_to = "carcID") %>% mutate(type = "dynamic", network = "roost")

summaries_static_aso <- list_rbind(map(Mods_N.RS_Aso, getmodstats) %>% setNames(carcIDs), names_to = "carcID") %>% mutate(type = "static", network = "roost")
summaries_dynamic_aso <- list_rbind(map(Mods_N.RD_Aso, getmodstats) %>% setNames(carcIDs), names_to = "carcID") %>% mutate(type = "dynamic", network = "roost")

summaries_dynamic_flight <- list_rbind(map(Mods_N.FD_So, getmodstats) %>% setNames(carcIDs), names_to = "carcID") %>% mutate(type = "dynamic", network = "flight")
summaries_dynamic_flight_aso <- list_rbind(map(Mods_N.FD_Aso, getmodstats) %>% setNames(carcIDs), names_to = "carcID") %>% mutate(type = "dynamic", network = "flight")

summaries <- bind_rows(summaries_static, summaries_dynamic, summaries_static_aso, summaries_dynamic_aso, summaries_dynamic_flight, summaries_dynamic_flight_aso)
# Models with a dynamic network can be compared to static network models if they are fitted to the same order of acquisition, which these are.

# Get confidence intervals (only for the social models)
## This is the tricky part--in order to find the confidence intervals, we have to manually visualize the plots. Luckily, there aren't too many of them.
search_roost <- data.frame(type = c(rep("dynamic", length(Mods_N.RD_So)),
                                    rep("static", length(Mods_N.RS_So))),
                           lower_min = NA,
                           lower_max = NA,
                           upper_min = NA,
                           upper_max = NA,
                           ci_lower = NA,
                           ci_upper = NA,
                           carcID = c(carcIDs, carcIDs),
                           network = "roost")

search_flight <- data.frame(type = rep("dynamic", length(Mods_N.RD_So)),
                            lower_min = NA,
                            lower_max = NA,
                            upper_min = NA,
                            upper_max = NA,
                            ci_lower = NA,
                            ci_upper = NA,
                            carcID = carcIDs,
                            network = "flight")

## Dynamic models (roosting)
plotProfLik(which = 1, model = Mods_N.RD_So[[1]], range = c(0,1.5))
search_roost[1,2:5] <- c(0, 0.2, 0.4, 0.6)
profLikCI(which = 1, model = Mods_N.RD_So[[1]],
          lowerRange = c(0, 0.2),
          upperRange = c(0.5, 1)) # XXX this is an example of a thing i need to watch out for--make sure the lower and upper bounds are on the correct side of the minimum, otherwise it won't find them correctly.
plotProfLik(which = 1, model = Mods_N.RD_So[[2]], range = c(0,2.5))
search_roost[2,2:5] <- c(0, 0.2, 0.5, 1)
plotProfLik(which = 1, model = Mods_N.RD_So[[3]], range = c(20,30))
search_roost[3,2:5] <- c(NA, NA, 24, 28)
#plotProfLik(which = 1, model = Mods_N.RD_So[[4]], range = c(0, 1))
search_roost[4,2:5] <- c(NA, NA, 0, 0.2)
#plotProfLik(which = 1, model = Mods_N.RD_So[[5]], range = c(0, 2.5))
search_roost[5,2:5] <- c(NA, NA, 0.25, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[6]], range = c(0, 2.5))
search_roost[6,2:5] <- c(0, 0.2, 0.4, 0.6)
#plotProfLik(which = 1, model = Mods_N.RD_So[[7]], range = c(0, 2.5))
search_roost[7,2:5] <- c(NA, NA, 0.5, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[8]], range = c(0, 1))
search_roost[8,2:5] <- c(NA, NA, 0, 0.2)
#plotProfLik(which = 1, model = Mods_N.RD_So[[9]], range = c(0, 1))
search_roost[9,2:5] <- c(0, 0.1, 0.4, 0.6)
#plotProfLik(which = 1, model = Mods_N.RD_So[[10]], range = c(0, 1))
search_roost[10,2:5] <- c(NA, NA, 0.4, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[11]], range = c(0, 2))
search_roost[11,2:5] <- c(0, 0.25, 1, 1.6)
#plotProfLik(which = 1, model = Mods_N.RD_So[[12]], range = c(0, 2))
search_roost[12,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[12]], range = c(0, 2))
search_roost[12,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[13]], range = c(0, 2))
search_roost[13,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[14]], range = c(0, 10))
search_roost[14,2:5] <- c(NA, NA, 4, 6)
#plotProfLik(which = 1, model = Mods_N.RD_So[[15]], range = c(0, 2.5))
search_roost[15,2:5] <- c(0, 0.5, 1.5, 2.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[16]], range = c(0, 2))
search_roost[16,2:5] <- c(NA, NA, 0.5, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[17]], range = c(0, 50))
search_roost[17,2:5] <- c(NA, NA, 30, 40)
#plotProfLik(which = 1, model = Mods_N.RD_So[[18]], range = c(0, 5))
search_roost[18,2:5] <- c(NA, NA, 2, 3)
#plotProfLik(which = 1, model = Mods_N.RD_So[[19]], range = c(0, 30))
search_roost[19,2:5] <- c(NA, NA, 25, 30)
#plotProfLik(which = 1, model = Mods_N.RD_So[[20]], range = c(0, 5))
search_roost[20,2:5] <- c(NA, NA, 0, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[21]], range = c(0, 2))
search_roost[21,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[22]], range = c(0, 2))
search_roost[22,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[23]], range = c(0, 2.5))
search_roost[23,2:5] <- c(0, 0.5, 2, 2.5)
plotProfLik(which = 1, model = Mods_N.RD_So[[24]], range = c(0, 5))
search_roost[24,2:5] <- c(NA, NA, 2, 4)
#plotProfLik(which = 1, model = Mods_N.RD_So[[25]], range = c(0, 2))
search_roost[25,2:5] <- c(0, 0.25, 0.5, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[26]], range = c(0, 2))
search_roost[26,2:5] <- c(NA, NA, 1.5, 2)
plotProfLik(which = 1, model = Mods_N.RD_So[[27]], range = c(0, 2))
#search_roost[27,2:5] <- c(0, 0.25, 0.5, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[28]], range = c(0, 2))
search_roost[28,2:5] <- c(NA, NA, 0.5, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[29]], range = c(0, 2))
search_roost[29,2:5] <- c(0, 0.5, 1, 1.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[30]], range = c(0, 10))
search_roost[30,2:5] <- c(NA, NA, 6, 8)
#plotProfLik(which = 1, model = Mods_N.RD_So[[31]], range = c(0, 2.5))
search_roost[31,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[32]], range = c(0, 3))
search_roost[32,2:5] <- c(0, 0.5, 2, 3)
#plotProfLik(which = 1, model = Mods_N.RD_So[[33]], range = c(0, 10))
search_roost[33,2:5] <- c(0, 2, 6, 8)
#plotProfLik(which = 1, model = Mods_N.RD_So[[34]], range = c(0, 10))
search_roost[34,2:5] <- c(NA, NA, 8, 9)
#plotProfLik(which = 1, model = Mods_N.RD_So[[35]], range = c(0, 5))
search_roost[35,2:5] <- c(NA, NA, 4, 5)

## Dynamic models (flight)
#plotProfLik(which = 1, model = Mods_N.FD_So[[1]], range = c(5,300))
search_flight[1,2:5] <- c(0, 50, 225, 250)
#plotProfLik(which = 1, model = Mods_N.FD_So[[2]], range = c(0,5))
search_flight[2,2:5] <- c(0, 1, 2, 3)
#plotProfLik(which = 1, model = Mods_N.FD_So[[3]], range = c(0, 10))
search_flight[3,2:5] <- c(NA, NA, 2, 4)
#plotProfLik(which = 1, model = Mods_N.FD_So[[4]], range = c(0, 3))
search_flight[4,2:5] <- c(0, 0.5, 1, 1.5)
#plotProfLik(which = 1, model = Mods_N.FD_So[[5]], range = c(5, 40))
search_flight[5,2:5] <- c(5, 10, 30, 35)
#plotProfLik(which = 1, model = Mods_N.FD_So[[6]], range = c(0, 2.5))
search_flight[6,2:5] <- c(0, 0.1, 0.5, 1)
#plotProfLik(which = 1, model = Mods_N.FD_So[[7]], range = c(0, 10))
search_flight[7,2:5] <- c(0, 1, 5, 7)
# plotProfLik(which = 1, model = Mods_N.FD_So[[8]], range = c(0, 10))
search_flight[8,2:5] <- c(NA, NA, 2, 4)
#plotProfLik(which = 1, model = Mods_N.FD_So[[9]], range = c(0, 10))
search_flight[9,2:5] <- c(0, 2, 7, 9)
#plotProfLik(which = 1, model = Mods_N.FD_So[[10]], range = c(0, 1))
search_flight[10,2:5] <- c(0, 0.2, 0.5, 0.7)
#plotProfLik(which = 1, model = Mods_N.FD_So[[11]], range = c(0, 3))
search_flight[11,2:5] <- c(0, 0.25, 2, 2.5)
#plotProfLik(which = 1, model = Mods_N.FD_So[[12]], range = c(0, 3))
search_flight[12,2:5] <- c(0, 0.25, 2, 2.5)
#plotProfLik(which = 1, model = Mods_N.FD_So[[12]], range = c(0, 3))
search_flight[12,2:5] <- c(0, 0.5, 1.5, 2.5)
#plotProfLik(which = 1, model = Mods_N.FD_So[[13]], range = c(0, 2))
search_flight[13,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.FD_So[[14]], range = c(0, 1000))
search_flight[14,2:5] <- c(NA, NA, 400, 600)
#plotProfLik(which = 1, model = Mods_N.FD_So[[15]], range = c(3, 30))
search_flight[15,2:5] <- c(2, 7, 20, 25)
#plotProfLik(which = 1, model = Mods_N.FD_So[[16]], range = c(0, 2))
search_flight[16,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.FD_So[[17]], range = c(0, 2000))
search_flight[17,2:5] <- c(0, 250, 1250, 1600)
#plotProfLik(which = 1, model = Mods_N.FD_So[[18]], range = c(0, 5))
search_flight[18,2:5] <- c(NA, NA, 1, 2)
#plotProfLik(which = 1, model = Mods_N.FD_So[[19]], range = c(0, 200))
search_flight[19,2:5] <- c(NA, NA, 150, 200)
#plotProfLik(which = 1, model = Mods_N.FD_So[[20]], range = c(0, 5))
search_flight[20,2:5] <- c(0, 1, 2.5, 3.5)
#plotProfLik(which = 1, model = Mods_N.FD_So[[21]], range = c(0, 5))
search_flight[21,2:5] <- c(0, 1, 3.5, 4.5)
#plotProfLik(which = 1, model = Mods_N.FD_So[[22]], range = c(0, 2))
search_flight[22,2:5] <- c(NA, NA, 0.25, 0.50)
#plotProfLik(which = 1, model = Mods_N.FD_So[[23]], range = c(0, 30))
search_flight[23,2:5] <- c(0, 5, 22, 26)
#plotProfLik(which = 1, model = Mods_N.FD_So[[24]], range = c(0, 5))
search_flight[24,2:5] <- c(NA, NA, 3, 4)
#plotProfLik(which = 1, model = Mods_N.FD_So[[25]], range = c(0, 6))
search_flight[25,2:5] <- c(0, 1, 4.5, 5.5)
#plotProfLik(which = 1, model = Mods_N.FD_So[[26]], range = c(0, 2))
search_flight[26,2:5] <- c(NA, NA, 1.5, 2)
#plotProfLik(which = 1, model = Mods_N.FD_So[[27]], range = c(0, 10))
search_flight[27,2:5] <- c(0, 2, 5, 6)
#plotProfLik(which = 1, model = Mods_N.FD_So[[28]], range = c(0, 2))
search_flight[28,2:5] <- c(NA, NA, 1, 2)
#plotProfLik(which = 1, model = Mods_N.FD_So[[29]], range = c(0, 30))
search_flight[29,2:5] <- c(2, 4, 20, 25)
#plotProfLik(which = 1, model = Mods_N.FD_So[[30]], range = c(0, 20))
search_flight[30,2:5] <- c(NA, NA, 15, 20)
#plotProfLik(which = 1, model = Mods_N.FD_So[[31]], range = c(0, 2.5))
search_flight[31,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.FD_So[[32]], range = c(0, 7))
search_flight[32,2:5] <- c(0, 1, 4, 6)
#plotProfLik(which = 1, model = Mods_N.FD_So[[33]], range = c(0, 10))
search_flight[33,2:5] <- c(NA, NA, 0, 2)
#plotProfLik(which = 1, model = Mods_N.FD_So[[34]], range = c(0, 65))
search_flight[34,2:5] <- c(NA, NA, 50, 60)
#plotProfLik(which = 1, model = Mods_N.FD_So[[35]], range = c(0, 20))
search_flight[35,2:5] <- c(NA, NA, 15, 20)

for(i in 1:length(Mods_N.RD_So)){
  if(is.na(search_roost[i,2]) & !is.na(search_roost[i,4])){
    ci <- profLikCI(which = 1, model = Mods_N.RD_So[[i]],
                    upperRange = search_roost[i,4:5])
  }else if(!is.na(search_roost[i,2]) & !is.na(search_roost[i,4])){
    ci <- profLikCI(which = 1, model = Mods_N.RD_So[[i]],
                    lowerRange = search_roost[i,2:3],
                    upperRange = search_roost[i,4:5])
  }else{
    ci <- c(NA, NA)
  }
  search_roost[i,6:7] <- ci 
  cat("done with ", i, "\n")
}

for(i in 1:length(Mods_N.FD_So)){
  if(is.na(search_flight[i,2]) & !is.na(search_flight[i,4])){
    ci <- profLikCI(which = 1, model = Mods_N.FD_So[[i]],
                    upperRange = search_flight[i,4:5])
  }else if(!is.na(search_flight[i,2]) & !is.na(search_flight[i,4])){
    ci <- profLikCI(which = 1, model = Mods_N.FD_So[[i]],
                    lowerRange = search_flight[i,2:3],
                    upperRange = search_flight[i,4:5])
  }else{
    ci <- c(NA, NA)
  }
  search_flight[i,6:7] <- ci 
  cat("done with ", i, "\n")
}

solveprops_dynamic_lower <- map2_dbl(search_roost$ci_lower[search_roost$type == "dynamic"], nbdaData_list_dynamic, ~{
  nbdaPropSolveByST(par = .x, nbdadata = .y)[1]
})

solveprops_dynamic_upper <- map2_dbl(search_roost$ci_upper[search_roost$type == "dynamic"], nbdaData_list_dynamic, ~{
  nbdaPropSolveByST(par = .x, nbdadata = .y)[1]
})

solveprops_dynamic_flight_lower <- map2_dbl(search_flight$ci_lower[search_flight$type == "dynamic"], nbdaData_list_dynamic_flight, ~{
  nbdaPropSolveByST(par = .x, nbdadata = .y)[1]
})

solveprops_dynamic_flight_upper <- map2_dbl(search_flight$ci_upper[search_flight$type == "dynamic"], nbdaData_list_dynamic_flight, ~{
  nbdaPropSolveByST(par = .x, nbdadata = .y)[1]
})

search_roost$propsolve_lower[search_roost$type == "dynamic"] <- solveprops_dynamic_lower
search_roost$propsolve_upper[search_roost$type == "dynamic"] <- solveprops_dynamic_upper
search_flight$propsolve_lower[search_flight$type == "dynamic"] <- solveprops_dynamic_flight_lower
search_flight$propsolve_upper[search_flight$type == "dynamic"] <- solveprops_dynamic_flight_upper

search <- bind_rows(search_roost, search_flight)
search <- search %>%
  mutate(sig_ci = ifelse(propsolve_lower > 0, TRUE, FALSE),
         soc = "social")

# join all these calculated conf int params to the overall model summary table
summaries <- left_join(summaries, search, by = c("carcID", "soc", "type", "network")) %>%
  left_join(years) # this includes info not just on years but also on carcass location, station, and weight, so we can analyze social transmission by carcass characteristics.

# Plotting ----------------------------------------------------------------
summaries %>%
  filter(type == "dynamic", soc == "social", !is.na(sig_ci)) %>%
  ggplot(aes(y = carcID, color = network))+
  geom_segment(aes(x = propsolve_lower, xend = propsolve_upper, linetype = sig_ci), position = position_dodge(width = 0.5))+
  geom_point(aes(x = propsolve, pch = sig_ci), size = 2, position = position_dodge(width = 0.5))+
  scale_shape_manual(values = c(21, 19))+
  scale_color_manual(values = c("dodgerblue2", "olivedrab4"))+
  scale_linetype_manual(values = c(3, 1))+
  facet_wrap(~year, ncol = 1, scales = "free_y")+
  theme_minimal()+
  labs(y = "Carcass",
       x = "Proportion of detections by social transmission",
       title = "Dynamic roost networks")

# Okay, this is great, we have info on both flight and roosting for the same carcasses.
# Now we could ask whether there's a trend for number of detection events, weight of carcass, or location of carcass. And then we can incorporate ILVs and compete these against each other.

test <- summaries %>%
  filter(type == "dynamic", soc == "social", !is.na(sig_ci)) 

mylogit <- glm(sig_ci ~ network + n_detections + carcassWeight, data = test, family = "binomial") # XXX should probably standardize detections and carcassWeight
summary(mylogit)

newdata <- as.data.frame(expand.grid("n_detections" = seq(from = 3, to = 69, by = 11), "carcassWeight" = seq(from = 30, to = 550, by = 10), "network" = c("flight", "roost")))

newdata$p <- predict(mylogit, newdata = newdata, type = "response")

newdata %>%
  ggplot(aes(x = carcassWeight, y = p, col = network, group = interaction(factor(n_detections), network)))+
  geom_line(aes(size = factor(n_detections)))+
  scale_size_manual(values = seq(from = 0.2, to = 1.5, length.out = 7))+
  scale_color_manual(values =c("dodgerblue2", "olivedrab4"))+
  theme_minimal()+
  labs(y = "P(social transmission)",
       x = "Carcass weight",
       color = "Network")+
  geom_point(data = test %>% mutate(p = ifelse(sig_ci, 1, 0)), 
             aes(x = carcassWeight, y = p, pch = network), size = 3)+
  scale_shape_manual(name = "Network", values = c(21, 8))

# So, heavier carcasses are less likely to show a signal of social transmission; roost network is less likely to show social transmission; more detections is more likely to show a signal of social transmission. 

# Okay, now, within the models that show significant evidence of social transmission, what relationships do we see?
test %>%
  filter(sig_ci) %>%
  ggplot(aes(x = carcassWeight, col = network, shape = network))+
  geom_segment(aes(x = carcassWeight, y = propsolve_lower, yend = propsolve_upper))+
  geom_point(size = 2, aes(y = propsolve))+
  scale_color_manual(name = "Network", values = c("dodgerblue2", "olivedrab4"))+
  theme_minimal()+
  scale_shape_manual(name = "Network", values = c(19, 8))+
  geom_smooth(method = "lm", aes(y = propsolve, fill = network), alpha = 0.2, linetype = 2)+
  scale_fill_manual(name = "Network", values = c("dodgerblue2", "olivedrab4"))+
  labs(y = "Proportion of detections by social transmission",
       x = "Carcass weight (kg)")

mod1 <- lm(propsolve ~ carcassWeight + network, data = test[test$sig_ci,])
summary(mod1)

test %>%
  filter(sig_ci) %>%
  ggplot(aes(x = n_detections, col = network, shape = network))+
  geom_segment(aes(x = n_detections, y = propsolve_lower, yend = propsolve_upper))+
  geom_point(size = 2, aes(y = propsolve))+
  scale_color_manual(name = "Network", values = c("dodgerblue2", "olivedrab4"))+
  scale_fill_manual(name = "Network", values = c("dodgerblue2", "olivedrab4"))+
  theme_minimal()+
  scale_shape_manual(name = "Network", values = c(19, 8))+
  geom_smooth(method = "lm", aes(y = propsolve, fill = network), alpha = 0.2, linetype = 2)+
  labs(y = "Proportion of detections by social transmission",
       x = "Number of detections") # we see that number of detections affected how likely we were to detect a social effect, but not the magnitude of the social effect once detected.

mod2 <- lm(propsolve ~ n_detections + network, data = test[test$sig_ci,])
summary(mod2)

# What about station?
test %>%
  mutate(stationName = str_replace_all(stationName, "_", " ")) %>%
  ggplot(aes(x = stationName, y = propsolve, fill = network, color = network))+
  geom_boxplot(alpha = 0.5)+
  scale_fill_manual(name = "Network", values = c("dodgerblue2", "olivedrab4"))+
  scale_color_manual(name = "Network", values = c("dodgerblue2", "olivedrab4"))+
  theme_minimal()+
  labs(y = "Proportion of detections by social transmission",
       x = NULL)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  facet_wrap(~year, scales = "free_x", nrow = 2, strip.position="right")

# Adding ILVs -------------------------------------------------------------
length(ilvs)
my_ilvs <- ilvs[has_sightings][has_3_sightings]
my_ilvs <- map(my_ilvs, ~{
  vec <- 0:((.x %>% dplyr::select(contains("roost_")) %>% length())-1)
  new_names <- paste0("dist_roost_night_", vec)
  names(.x)[2:(length(vec)+1)] <- new_names
  return(.x)
})
ilvs_lists <- map2(my_ilvs, days_vec, ~{
  lst <- vector(mode = "list", length = length(.y))
  for(i in 1:length(lst)){
    col <- paste0("dist_roost_night_", .y[i]-1)
    lst[[i]] <- .x %>%
      dplyr::select(local_identifier, age_group, "dist_roost" = any_of(col))
  }
  return(lst)
})

# Need to make a matrix where each row is an individual and each column is an acquisition event. Then we fill it with the values.
roost_carc_distances <- map2(n_indivs, oas, 
                             ~matrix(NA, nrow = .x, ncol = length(.y)))
age_groups <- map2(n_indivs, oas,
                   ~matrix(NA, nrow = .x, ncol = length(.y)))
for(i in 1:length(age_groups)){ # loop on carcasses
  ag <- age_groups[[i]]
  rcd <- roost_carc_distances[[i]]
  for(j in 1:nrow(ag)){ # loop on individuals
    ag[j,] <- tryCatch({map_chr(ilvs_lists[[i]], ~as.character(.x$age_group[j]))}, error = function(msg){NA})
    rcd[j,] <- tryCatch({map_dbl(ilvs_lists[[i]], ~as.numeric(.x$dist_roost[j]))}, error = function(msg){NA})
  }
  roost_carc_distances[[i]] <- rcd
  age_groups[[i]] <- ag
  cat("done with carcass ", i, "\n")
}

# Check for NA values
map_dbl(roost_carc_distances, ~round(sum(is.na(.x))/length(.x), 2)) %>%  hist() # a very small proportion of values are NA, presumably when we didn't have distances to the carcass available.
# Where we have NAs, let's set them to the average value for the rest of the matrix.

mns_non_NAs <- map_dbl(roost_carc_distances, ~mean(.x[!is.na(.x)]))
roost_carc_distances <- map2(roost_carc_distances, mns_non_NAs, ~{
  .x[is.na(.x)] <- .y
  return(.x)
})
map_lgl(roost_carc_distances, anyNA) # 24 still has NAs because it's all NA, but that's ok.

std_roost_carc_distances <- map(roost_carc_distances, ~{
  (.x-mean(.x))/sd(.x)
})

# Change the age groups to 0s and 1s
age_groups <- map(age_groups, ~{
  .x[.x == "01_juv_sub"] <- 0
  .x[.x == "02_adult"] <- 1
  .x <- apply(.x, 2, as.numeric)
  return(.x)
})

hist(age_groups[[1]]) 
#Since std_roost_carc_distances is centered on 0, and juveniles/subadults = 0, adults=1, the baseline asocial rate is a juvenile/subadult of mean distance from the roost to the carcass
#Therefore s is estimated relative to this baseline

age_groups_reversed <- map(age_groups, ~{ # in case we need a reversed one
  +(!.x)
})

# THIS IS SO DUMB! Because of the way the code is written for the nbdaData function, I need to create global environment variable that I can then refer to by name for the ILVs. I can't pass an *object* into the function for the ILV matrices. grrrrrrr
for(i in 1:length(std_roost_carc_distances)){
  name1 <- quo_name(paste0("std_roost_carc_distances_", i))
  name2 <- quo_name(paste0("age_groups_", i))
  assign(name1, std_roost_carc_distances[[i]], envir = .GlobalEnv)
  assign(name2, age_groups[[i]], envir = .GlobalEnv)
}

nbdaData_list_fd_ilvs <- vector(mode = "list", length = length(N.FD))
for(i in 1:length(nbdaData_list_fd_ilvs)){
  ag_name <- paste0("age_groups_", i)
  srcd_name <- paste0("std_roost_carc_distances_", i)
  ilvs_to_use <- c(ag_name, srcd_name)
  carcass <- carcIDs[i]
  nbdaData_list_fd_ilvs[[i]] <- nbdaData(label = paste0("Carcass ", carcass),
                                         assMatrix = N.FD[[i]],
                                         orderAcq = oas[[i]],
                                         assMatrixIndex = assMatrixIndices[[i]],
                                         asoc_ilv = ilvs_to_use,
                                         asocialTreatment = "timevarying")
  cat("done with", i, "\n")
}

# Make the models
Mods_N.FD_addI.TC_I.TV_So <- map(nbdaData_list_fd_ilvs, ~{
  tryCatch({oadaFit(.x)}, error = function(msg){NULL})
})

summaries_with_ilvs <- list_rbind(map(Mods_N.FD_addI.TC_I.TV_So, getmodstats) %>% setNames(carcIDs), names_to = "carcID") %>% mutate(type = "dynamic", network = "flight")

search_with_ilvs <- data.frame(type = rep("dynamic", length(Mods_N.FD_addI.TC_I.TV_So)),
                            lower_min = NA,
                            lower_max = NA,
                            upper_min = NA,
                            upper_max = NA,
                            ci_lower = NA,
                            ci_upper = NA,
                            carcID = carcIDs,
                            network = "flight")

plotProfLik(which = 1, model = Mods_N.FD_addI.TC_I.TV_So[[12]], range = c(0, 2))
search_with_ilvs[35,2:5] <- c(NA, NA, 20, 40)
search_with_ilvs[34,2:5] <- c(NA, NA, 100, 200)
search_with_ilvs[33,2:5] <- c(NA, NA, 1, 2)
search_with_ilvs[32,2:5] <- c(NA, NA, 0, 1)
search_with_ilvs[31,2:5] <- c(NA, NA, 0, 1)
search_with_ilvs[30,2:5] <- c(NA, NA, 100, 200)
search_with_ilvs[29,2:5] <- c(0, 5, 15, 20)
search_with_ilvs[28,2:5] <- c(NA, NA, 1, 2)
search_with_ilvs[27,2:5] <- c(0, 1, 6, 8)
search_with_ilvs[26,2:5] <- c(NA, NA, 3, 5)
search_with_ilvs[25,2:5] <- NA
search_with_ilvs[24,2:5] <- c(NA, NA, 8,12)
search_with_ilvs[23,2:5] <- c(0, 5, 20, 30)
search_with_ilvs[22,2:5] <- c(NA, NA, 0, 1)
search_with_ilvs[21,2:5] <- c(0, 2, 4, 6)
search_with_ilvs[20,2:5] <- NA
search_with_ilvs[19,2:5] <- c(NA, NA, 700, 800)
search_with_ilvs[18,2:5] <- NA
search_with_ilvs[17,2:5] <- c(0, 20, 9000, 12000)
search_with_ilvs[16,2:5] <- c(NA, NA, 0.1, 0.2)
search_with_ilvs[15,2:5] <- c(0, 10, 60, 80)
search_with_ilvs[14,2:5] <- c(NA, NA, 400, 600)
search_with_ilvs[13,2:5] <- c(NA, NA, 0.1, 0.3)
search_with_ilvs[12,2:5] <- c(NA, NA, 1, 2)
search_with_ilvs[11,2:5] <- c(0, 0.1, 1, 3)
search_with_ilvs[10,2:5] <- c(0, 0.1, 0.4, 0.6)
search_with_ilvs[9,2:5] <- c(0, 1, 5, 10)
search_with_ilvs[8,2:5] <- c(NA, NA, 3, 5)
search_with_ilvs[7,2:5] <- c(0, 1, NA, NA)
search_with_ilvs[6,2:5] <- NA
search_with_ilvs[5,2:5] <- NA
search_with_ilvs[4,2:5] <- NA
search_with_ilvs[3,2:5] <- c(NA, NA, 2, 3)
search_with_ilvs[2,2:5] <- c(0, 1, 1.5, 3)
search_with_ilvs[1,2:5] <- c(0, 100, 350, 450)

for(i in 1:length(Mods_N.FD_addI.TC_I.TV_So)){
  if(is.na(search_with_ilvs[i,2]) & !is.na(search_with_ilvs[i,4])){
    ci <- profLikCI(which = 1, model = Mods_N.FD_addI.TC_I.TV_So[[i]],
                    upperRange = search_with_ilvs[i,4:5])
  }else if(!is.na(search_with_ilvs[i,2]) & !is.na(search_with_ilvs[i,4])){
    ci <- profLikCI(which = 1, model = Mods_N.FD_addI.TC_I.TV_So[[i]],
                    lowerRange = search_with_ilvs[i,2:3],
                    upperRange = search_with_ilvs[i,4:5])
  }else{
    ci <- c(NA, NA)
  }
  search_with_ilvs[i,6:7] <- ci 
  cat("done with ", i, "\n")
}

solveprops_ilvs_lower <- map2_dbl(search_with_ilvs$ci_lower, Mods_N.FD_addI.TC_I.TV_So, ~{
  tryCatch({nbdaPropSolveByST(par = c(.x, .y@outputPar[2],
                            .y@outputPar[3]), 
                    nbdadata = .y@nbdadata)[1]}, error = function(msg){NA})
})

solveprops_ilvs_upper <- map2_dbl(search_with_ilvs$ci_upper, Mods_N.FD_addI.TC_I.TV_So, ~{
  tryCatch({nbdaPropSolveByST(par = c(.x, .y@outputPar[2],
                                      .y@outputPar[3]), 
                              nbdadata = .y@nbdadata)[1]}, error = function(msg){NA})
})

search_with_ilvs$propsolve_lower <- solveprops_ilvs_lower
search_with_ilvs$propsolve_upper <- solveprops_ilvs_upper

search_with_ilvs <- search_with_ilvs %>%
  mutate(sig_ci = ifelse(propsolve_lower > 0, TRUE, FALSE),
         soc = "social")

# join all these calculated conf int params to the overall model summary table
sm <- summaries_with_ilvs %>%
  filter(varNames == "1 Social transmission 1") %>%
  left_join(search_with_ilvs, by = c("carcID", "soc", "type", "network")) %>%
  left_join(years) # this includes info not just on years but also on carcass location, station, and weight, so we can analyze social transmission by carcass characteristics.

sm %>%
  filter(!is.na(sig_ci)) %>%
  ggplot(aes(y = carcID))+
  geom_segment(aes(x = propsolve_lower, xend = propsolve_upper, linetype = sig_ci))+
  geom_point(aes(x = propsolve, pch = sig_ci), size = 2)+
  scale_shape_manual(values = c(21, 19))+
  scale_linetype_manual(values = c(3, 1))+
  facet_wrap(~year, ncol = 1, scales = "free_y")+
  theme_minimal()+
  labs(y = "Carcass",
       x = "Proportion of detections by social transmission",
       title = "Dynamic flight networks (and ILVs)")


# For just one carcass, let's make all the possible models and see how it goes.
models_to_make <- expand.grid(sd = c("static", "dynamic"), network = c("roost", "flight"), ilvs = c("age", "distance", "both", "neither"), social = c("social", "asocial")) # 32 models for the one carcass.

# semi-randomly selected carcass 4877850 from among carcasses with at least 30 detections and a significant effect for at least one of the network models.
datasets_to_make <- expand.grid(sd = c("static", "dynamic"), network = c("roost", "flight"), ilvs = c("age", "distance", "both", "neither")) # need 12 datasets, and then we'll use those to make a social and an asocial model for each

# Before I start making these decisions, let's see if we have any evidence of age or distance actually making a difference to when individuals discover the carcass. No point in correcting for it if we don't think it'll affect things. We could also preemptively decide to use dynamic NBDA since that would describe what's happening better--it doesn't make sense to use static.
fs <- map2(firsts_see[has_sightings][has_3_sightings], carcIDs, ~.x %>% mutate(carcID = .y))
mi <- map2(my_ilvs, carcIDs, ~.x %>% mutate(carcID = .y))
joined <- map2(mi, fs, left_join)
all(map_dbl(joined, nrow) == map_dbl(my_ilvs, nrow)) # same rows as number of individuals in the ILVs, not the number of individuals that eventually detected the carcass, since not everyone did eventually detect the carcass.
all(map_dbl(joined, nrow) == map_dbl(fs, nrow))
joined_df <- purrr::list_rbind(joined)
joined_df <- joined_df %>%
  mutate(found_carcass = ifelse(!is.na(timestamp), TRUE, FALSE)) %>%
  mutate(year = lubridate::year(dateOnly))

# Are adults or juveniles more likely to find the carcass?
## quick and dirty viz: proportion of adults that found the carcass vs. proportion of juveniles that found the carcass, for different carcasses
joined_df %>%
  group_by(carcID, age_group) %>%
  summarize(prop_found_carcass = sum(found_carcass)/n()) %>%
  ggplot(aes(x = age_group, y = prop_found_carcass, group = carcID))+
  geom_point()+
  geom_line() # no obvious pattern to what proportion of juveniles vs. adults found the carcass

mod3 <- glmer(found_carcass ~ age_group + (1|carcID) 
              #+ (1|local_identifier)
              , data = joined_df, family = binomial)
summary(mod3) # adults slightly less likely to find the carcass than juveniles. Effect basically disappears when we include a random effect of individual ID, but if anything it still trends negative. 

# Do adults and juveniles differ in their time of arrival to the carcass?
## This one is easier to visualize because time of arrival is continuous.
joined_df %>%
  filter(!is.na(year)) %>%
  ggplot(aes(x = carcID, y = time_since_carcass, fill = age_group))+
  geom_boxplot()+
  theme_classic()+
  facet_wrap(~year, scales = "free_x") # not clear from this viz whether juveniles or adults find the carcass earlier. Let's do a model

mod4 <- lmer(as.numeric(time_since_carcass) ~ age_group + (1|carcID), data = joined_df)
summary(mod4) # adults seem to find the carcass more quickly.

# This is pointing to the need to include an effect of age in the models. Would it be better to use a continuous rather than categorical predictor? Which is better/fewer degrees of freedom?

# Does distance from the carcass the night before affect how quickly you find the carcass?
joined_df %>%
  filter(!is.na(year)) %>%
  ggplot(aes(x = dist_roost_night_0, y = time_since_carcass, col = carcID))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.1)+
  facet_wrap(~year)+
  theme_classic()+
  theme(legend.position = "none") # there does seem to be a positive trend--roosting farther away means you find the carcass later.
# what about night 1?

joined_df %>%
  filter(!is.na(year)) %>%
  ggplot(aes(x = dist_roost_night_1, y = time_since_carcass, col = carcID))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.1)+
  facet_wrap(~year)+
  theme_classic()+
  theme(legend.position = "none") # ooh, even more of a positive trend! Interesting. The causality might start going the other way at some point.

# What about night 2?
joined_df %>%
  filter(!is.na(year)) %>%
  ggplot(aes(x = dist_roost_night_2, y = time_since_carcass, col = carcID))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.1)+
  facet_wrap(~year)+
  theme_classic()+
  theme(legend.position = "none") # breaks down a bit.

# but regardless, I also think it is important to include distance in the calculation here.

# Okay, it doesn't look like we can get rid of the ILVs. Maybe we can just use dynamic networks instead of static, since static doesn't make that much sense?

# For just one carcass, let's make all the possible models and see how it goes.
models_to_make <- expand.grid(sd = "dynamic", network = c("roost", "flight"), ilvs = c("age", "distance", "both", "neither"), social = c("social", "asocial")) # 16 models for the one carcass.

datasets_to_make <- expand.grid(sd = "dynamic", network = c("roost", "flight"), ilvs = c("age", "distance", "both", "neither")) # need 8 datasets, and then we'll use those to make a social and an asocial model for each

## Define function for making the datasets
make_nbda_data <- function(idx, lbl, network, ilvs = "neither"){
  # Define flight or roosting network
  if(network == "flight"){
    am <- N.FD[[idx]]
  }else{
    am <- N.RD[[idx]]
  }
  
  # Define ILVs to use
  ag_name <- paste0("age_groups_", idx)
  srcd_name <- paste0("std_roost_carc_distances_", idx)

  # Make data object with the chosen parts
  if(ilvs %in% c("age", "distance", "both")){
    if(ilvs == "age"){
      ilvs_to_use <- ag_name
    }else if(ilvs == "distance"){
      ilvs_to_use <- srcd_name
    }else{
      ilvs_to_use <- c(ag_name, srcd_name)
    }
    dat <- nbdaData(label = lbl,
             assMatrix = am,
             orderAcq = oas[[idx]],
             assMatrixIndex = assMatrixIndices[[idx]],
             asoc_ilv = ilvs_to_use,
             asocialTreatment = "timevarying"
    )
  }else if (ilvs == "neither"){
    dat <- nbdaData(label = lbl,
             assMatrix = am,
             orderAcq = oas[[idx]],
             assMatrixIndex = assMatrixIndices[[idx]],)
  }
  return(dat)
}

idx <- 29
lbl <- paste0("Carcass ", idx)

datasets <- vector(mode = "list", length = nrow(datasets_to_make))
for(i in 1:length(datasets)){
 datasets[[i]] <- make_nbda_data(idx = idx, lbl = lbl, network = datasets_to_make$network[i], ilvs = datasets_to_make$ilvs[i])
}
map(datasets, str)

## Make models
mods_social <- map(datasets, ~{tryCatch({oadaFit(.x)}, error = function(msg){NULL})})
mods_asocial <- map(datasets, ~{tryCatch({oadaFit(.x, type = "asocial")}, error = function(msg){NULL})})

## Compare social and asocial
map_dbl(mods_asocial, ~.x@aicc) - map_dbl(mods_social, ~.x@aicc) # for this particular carcass, all models show that the social model has more support than the asocial model, which is not surprising since this is one of the carcasses where we detected a social effect!

## Look at the model summaries
summaries_social <- list_rbind(map2(mods_social, datasets_to_make$network, ~getmodstats(.x) %>% mutate(network = .y))) %>%
  mutate(carcID = carcIDs[[idx]],
         sd = "dynamic")
summaries_social # none of the output parameters are equal to 0, so we can keep all these models (this will not necessarily be true for the other carcasses)

summaries_asocial <- list_rbind(map2(mods_asocial, datasets_to_make$network, ~getmodstats(.x) %>% mutate(network = .y))) %>%
  mutate(carcID = carcIDs[[idx]],
         sd = "dynamic")
summaries_asocial # none of these are zero either, but I don't understand why there are no variables in the last one. Surely there should still be able to be an asocial model on a network without any ilvs? Oh I guess it's because by definition there is no social transmission. So that model still exists and has an aicc that can be compared, but it doesn't have a social parameter or any other variables. Fair enough!


# Testing multimodel inference, per tutorial 7 ----------------------------
## To follow the tutorial, let's use one carcass (one diffusion) with one static network (static roost network) and two ILVs (age; distance on the first day from the carcass).

## Get our standardized ILVs (static, not dynamic)
age <- cbind(age_groups_29[,1])
dist <- cbind(std_roost_carc_distances_29[,1])
asoc <- c("age", "dist")
## Create an object for the "unconstrained" model
nd_unconstrained <- nbdaData(label = "test", assMatrix = N.RS[[29]], orderAcq = oas[[29]], asoc_ilv = asoc, int_ilv = asoc)

## Fit and display the model
mod_unc <- oadaFit(nd_unconstrained)
data.frame(Variable=mod_unc@varNames,MLE=mod_unc@outputPar,SE=mod_unc@se)

# Time to use multi-model inference to determine different possibilities
# Define a set of models by constraining the unconstrained data. 0 means omitted from the model; same non-zero number means constrained to be the same. Can only constrain the same type of parameter to be the same.

#e.g. here constraintsVect=c(1,2,0,0,3), would be a social model with an effect of age group on asocial learning and an effect of distance on social learning.

#To specify a set of models, we construct a constraintsVectMatrix, with each row giving a constraintsVect and specifying a different model in the set.

constraintsVectMatrix<-rbind(
  #The full model
  c(1,2,3,4,5),
  #Social models with only ILVs affecting asocial learning
  c(1,2,3,0,0),
  c(1,2,0,0,0),
  c(1,0,2,0,0),
  #Social models with only ILVs affecting social learning
  c(1,0,0,2,3),
  c(1,0,0,2,0),
  c(1,0,0,0,2),
  #Social models with ILVs affecting both asocial and social learning
  c(1,2,0,3,0),
  c(1,2,0,0,3),
  c(1,2,0,3,4),
  c(1,0,2,3,0),
  c(1,0,2,0,3),
  c(1,0,2,3,4),
  c(1,2,3,4,0),
  c(1,2,3,0,4),
  #Social model with no ILVs
  c(1,0,0,0,0),
  #Asocial models with all combinations of ILVs
  c(0,0,0,0,0),
  c(0,1,0,0,0),
  c(0,0,1,0,0),
  c(0,1,2,0,0)
  #Remember asocial models cannot have an effect of ILVs on social learning.
)

#To fit the set of models we use
modelSet_uc<-oadaAICtable(nbdadata = nd_unconstrained, constraintsVectMatrix =constraintsVectMatrix)

#We can print out the set of models ordered by AICc as follows:
print(modelSet_uc)
#This shows us the model number (row in the original constraintsVectMatrix)
#Model type- asocial; unconstrained if ILVs are affecting social learning;  additive if ILVs are affecting only asocial learning, NA if there are no ILVs. netCombo we will come to below. baseline is NA because this is an OADA. The next columns beginning with CONS are the constraints vector defining that model. The next columns beginning with OFF are the offset vector used to define the model (here not used). convergence says whether the optimisation algorithm "thinks" it has converged. loglik the -log-likelihood for the model. next are the parameter estimates; these are set to zero if the parameter is constrained to zero. next are the SEs, then AIC and AICc; AICc is used by default. deltaAICc is the difference in AICc from the best model, the relative support for the model compared to the best model, and the Akaike weight

#Here we can see we do not have a clearly favoured best model, so it makes sense to use multi-model inferencing

#The first thing we can do is get support for the network
networksSupport(modelSet_uc)
#which tells us that network 1 has 99.9% support compared to 0.001% support for no network (asocial)
#But we can also see that the number of models is not equal (see main text) so this is not a good way to compare support for social and asocial models
typeSupport(modelSet_uc)
#We can also break down support by model type, but again we have unequal model numbers so this is not so useful here

#More useful is the total support for each variable:
variableSupport(modelSet_uc)
#Showing some support (36.8%) for an effect of age on asocial learning, 98.8% support for an effect of distance on asocial learning (yay, this makes sense!!!), 64.9% support for an effect of age on social learning (innnnnteresting. Which way?), and 36.9% support for an effect of distance on social learning (also innnnnteresting. it's not obvious to me why that would be.)
#s1 also has high support but is present in more models than it is absent as we saw above, so this could be misleading.
# XXX WHY ARE WE ABLE TO DO THIS WITH THE OTHER VARIABLES? IS THE NUMBER OF MODELS EQUAL FOR THEM? Ah, yes, I think it is.

#We can get model averaged estimated as follows:
modelAverageEstimates(modelSet_uc)

#If we prefer, we can get model weighted medians instead (see main text):
modelAverageEstimates(modelSet_uc,averageType = "median") # XXX return to main text to see if we need this.

#We can get unconditional SEs:
unconditionalStdErr(modelSet_uc)

#A neat way to tie this up together is:

rbind(
  support=variableSupport(modelSet_uc),
  MAE=modelAverageEstimates(modelSet_uc),
  USE=unconditionalStdErr(modelSet_uc))

# s1  ASOC:age  ASOC:dist SOCIAL:age SOCIAL:dist
# support  0.9988585 0.3683924  0.9883787  0.6492749   0.3692828
# MAE      2.4415161 0.4948954 -3.2083844 -0.7867349  -0.3147896
# USE     50.7331454 1.2617150  1.5659963  0.8029706   1.0467285

#We could use the USE to generate Wald CIs, but we saw this can be misleading in an NBDA where profile likelihoods are asymmetrical
#We already know that the profile likelihoods are asymmetrical for the 2 parameters with support here from a previous tutorial.

#One option would be to obtain the profile likelihood 95% CI for every model in which an s parameter is present and check how robust these are to modelnselection uncertainty. However, we have seen in previous tutorials that finding the 95% CIs is an interactive process, with the user finding the points at which the profile -log-likelihood crosses the maximum -log-likelihood +1.92 cutoff line.

#That said, it is quite easy to find the lower limit for an s parameter, since we know it must be between 0 and the MLE for s in that model. So we can use an automated procedure to find the lower limit for s in each model and the estimate of propST (%ST) corresponding to this. It is the lower limit for s that is likely to be of most interest, since this tells us the strength of evidence for social transmission.

#We can obtain the lower limit of an s parameter (determined by `which` if there are more than 1) for a given confidence level (conf) in all models containing that parameter as follows:

lowerLimitsByModel<-multiModelLowerLimits(which=1,aicTable = modelSet_uc,conf=0.95)
lowerLimitsByModel

# model netCombo      lowerCI  propST  deltaAICc akaikeWeight  adjAkWeight cumulAdjAkWeight
# 1     11        1 1.590442e-01 0.27769  0.0000000 2.748233e-01 2.751374e-01        0.2751374
# 2      4        1 8.487458e-02 0.23063  0.6755992 1.960420e-01 1.962660e-01        0.4714035
# 3      1        1 6.030908e-01 0.33746  0.7783762 1.862222e-01 1.864350e-01        0.6578385
# 4     14        1 1.589741e-01 0.26634  2.1519890 9.370336e-02 9.381044e-02        0.7516489
# 5     13        1 7.263118e-05 0.27315  2.2957512 8.720424e-02 8.730390e-02        0.8389528
# 6      2        1 8.232349e-02 0.21967  2.8833158 6.500542e-02 6.507971e-02        0.9040325
# 7     12        1 7.514043e-05 0.27088  2.9115081 6.409552e-02 6.416877e-02        0.9682013
# 8     15        1 7.872984e-05 0.26996  5.2267296 2.014112e-02 2.016414e-02        0.9883654
# 9      5        1 1.001183e-01 0.58465  7.8260512 5.490959e-03 5.497234e-03        0.9938627
# 10     7        1 1.872343e-01 0.59568  8.8968514 3.214600e-03 3.218273e-03        0.9970809
# 11    10        1 1.090177e-01 0.58416 10.0240456 1.829615e-03 1.831706e-03        0.9989126
# 12     9        1 1.962738e-01 0.59612 11.0699473 1.084539e-03 1.085778e-03        0.9999984
# 15     6        1 2.016263e+00 0.67403 25.9885352 6.247624e-07 6.254764e-07        0.9999991
# 16     8        1 2.929456e+00 0.71003 26.1227440 5.842138e-07 5.848815e-07        0.9999996
# 17    16        1 1.252522e+00 0.63504 27.7936391 2.533621e-07 2.536516e-07        0.9999999
# 18     3        1 1.266250e+00 0.66246 29.4548561 1.104111e-07 1.105373e-07        1.0000000

#This function also returns the propST corresponding to each lower limit.
#deltaAICc is the difference in AICc from the best model in the full set provided to the function (XXX ?)
#akaikeWeight is the corresponding Akaike weight from the full model set.
#adjAkWeight is the adjusted Akaike weight conditional on the parameter being in the model (so they sum to one in this reduced set)
#cumulAdjAkWeight is the cumulative adjusted Akaike weight.

#We can see that in the top few models, the lower limit for propST is consistently above 0.25, and the effect gets larger as we move down the table. (XXX does that mean the worse-fitting models estimate stronger social transmission, or am I interpreting this wrong?)
#Across all models, the lower 95% CI for s goes as low as 7.872984e-05 and propST as low as 0.21967 (21.9%).
#But seeing this at least adds to our confidence that we have good evidence for social transmission, since 0 is outside the 95% CI in all models.

#The lowest values for lowerCI and propST are in moderately plausible models, so these are somewhat conservative lower limits for the effect of social transmission (then again, there are many similar values, e.g. in the mid-20s).

#We can obtain a model averaged lower-limit for propST as follows:

sum(lowerLimitsByModel$propST*lowerLimitsByModel$adjAkWeight)
#[1] 0.2773856 # yeah, high 20s. Not a ton of social transmission.

#############################################################################
# TUTORIAL 7.2
# MULTI-MODEL INFERENCE IN AN OADA: ADDITIVE VERSUS MULTIPLICATIVE VERSUS ASOCIAL MODELS
# 1 diffusion
# 1 static network
# 2 time constant ILVs
#############################################################################

#An alternative is to consider only additive versus multiplicative versus asocial models (See main text and the additional guidance in the supporting information for dicussion of pros and cons).
#To do this we need to build a different starting nbdaData object:

nbdaData2_addVmulti<-nbdaData(label="test",assMatrix=N.RS[[idx]],orderAcq = oas[[idx]],asoc_ilv = asoc,multi_ilv=asoc)
#Here providing asoc as the argument to asoc_ilv and multi_ilv (not int_ilv)

#A full additive model is therefore given by constraintsVect= c(1,2,3,0,0), whereas a full multiplicative model is given by c(1,0,0,2,3)
#We can specify constrainstVectMatrix to give the 3 required model subsets as follows:

constraintsVectMatrix<-rbind(
  #no ILVs
  c(1,0,0,0,0),
  #additive models
  c(1,2,0,0,0),
  c(1,2,3,0,0),
  c(1,0,2,0,0),
  #multipicative models
  c(1,0,0,2,0),
  c(1,0,0,2,3),
  c(1,0,0,0,2),
  #asocial models
  c(0,0,0,0,0),
  c(0,1,0,0,0),
  c(0,1,2,0,0),
  c(0,0,1,0,0))


modelSet_addVmulti<-oadaAICtable(nbdadata = nbdaData2_addVmulti,constraintsVectMatrix =constraintsVectMatrix)

#We can print out the set of models ordered by AICc as follows:
print(modelSet_addVmulti)

networksSupport(modelSet_addVmulti)
#which tells us that network 1 has 99.6% support compared to 0.35% support for no network (asocial)
#But we can also see that the number of models is not equal (see main text)

(ts <- typeSupport(modelSet_addVmulti))
#We can also break down support by model type. This is much more useful in this case, since we can do a fair three-way model comparison.
#The noILVs model can be added to both additive and multiplicative sets, since they reduce to the same model when there are no ILVs
#Giving us a fair 4 model comparison (XXX KG I think they meant a three-way model comparison with four models for each)
#additive
ts$support[1] + ts$support[4] # 80.6% support
#multiplicative
ts$support[3] + ts$support[4] # 19% support
#asocial
ts$support[2] # 0.35% support
# XXX these should (approx.) sum to 100%

#Both are much better supported than asocial learning, with additive being favoured highly over multiplicative.

#We can get the total support for each variable
variableSupport(modelSet_addVmulti)

#or we can condition on a particular model type:
variableSupport(modelSet_addVmulti,typeFilter = "additive")
#                s1  ASOC:age ASOC:dist A&S:age A&S:dist
# support 0.9956461 0.2494804 0.9999996       0        0

#This gives the support across all models consistent with the additive assumption--i.e. additive, noILVs and asocial models.
#This has equal numbers of models with each parameter present and absent (assuming we included all combinations above), so it is a fair measure of support for each

#We can get model averaged estimates as follows. Here it probably also makes sense to condition on model type
modelAverageEstimates(modelSet_addVmulti,typeFilter = "additive")
# ASOCIALage ASOCIALdist   SOCIALage  SOCIALdist 
# 0.35245012  0.02539066 -3.50970936  0.00000000  0.00000000 

#We can get unconditional SEs:
unconditionalStdErr(modelSet_addVmulti,typeFilter = "additive")
# SEasocialage SEasocialdist   SEsocialage  SEsocialdist 
# 0.05177897    0.04976491    1.38143298    0.00000000    0.00000000 

#A neat way to tie this up together is:

rbind(
  support=variableSupport(modelSet_addVmulti,typeFilter = "additive"),
  MAE=modelAverageEstimates(modelSet_addVmulti,typeFilter = "additive"),
  USE=unconditionalStdErr(modelSet_addVmulti,typeFilter = "additive"))

#                 s1   ASOC:age  ASOC:dist A&S:age A&S:dist
# support 0.99564609 0.24948043  0.9999996       0        0
# MAE     0.35245012 0.02539066 -3.5097094       0        0
# USE     0.05177897 0.04976491  1.3814330       0        0


#If the multiplicative model had been favoured we would have obtained an output like this:
rbind(
  support=variableSupport(modelSet_addVmulti,typeFilter = "multiplicative"),
  MAE=modelAverageEstimates(modelSet_addVmulti,typeFilter = "multiplicative"),
  USE=unconditionalStdErr(modelSet_addVmulti,typeFilter = "multiplicative"))
# 
#                s1     ASOC:age   ASOC:dist     A&S:age   A&S:dist
# support 0.9818112  0.006461266  0.01818878  0.26843388  0.9818078
# MAE     0.9300756 -0.047135590 -1.40885532 -0.04909154 -1.3808103
# USE     0.5150562  0.036800619  0.08336943  0.03540059  0.1173653

#The MAEs and USEs in the ASOC (asocial) columns includes the asocial models whereas those in the A&S (asocial and social) columns do not.
#So if we want MAEs including the asocial models we use the ASOC columns, whereas if we want MAEs not including the asocial models we use the A&S columns

#The support in the ASOC columns is for models containing ONLY an effect of the ILV on asocial learning
#whereas the support in the A&S columns is for models containing the same effect on both social and asocial learning
#The total support for the variable, conditional on the multiplicative model can be obtained by adding these two columns together.


#We can examine the plausible lower limit for s across models as before

lowerLimitsByModel_addVmulti<-multiModelLowerLimits(which=1,aicTable = modelSet_addVmulti,conf=0.95)
lowerLimitsByModel_addVmulti

# model netCombo    lowerCI  propST deltaAICc akaikeWeight  adjAkWeight cumulAdjAkWeight
# 1     4        1 0.08487458 0.23063  0.000000 6.054249e-01 6.075668e-01        0.6075668
# 2     3        1 0.08232349 0.21967  2.207717 2.007524e-01 2.014626e-01        0.8090295
# 3     7        1 0.16907247 0.27086  2.953475 1.382679e-01 1.387571e-01        0.9477865
# 4     6        1 0.17882551 0.28022  4.908312 5.202760e-02 5.221167e-02        0.9999982
# 7     1        1 1.25252168 0.63504 27.118040 7.824431e-07 7.852113e-07        0.9999990
# 8     5        1 1.45897477 0.65939 27.433429 6.682930e-07 6.706573e-07        0.9999997
# 9     2        1 1.26624975 0.66246 28.779257 3.409762e-07 3.421825e-07        1.0000000

#We can obtain a model averaged lower-limit for propST as follows:

sum(lowerLimitsByModel_addVmulti$propST*lowerLimitsByModel_addVmulti$adjAkWeight)
# [1] 0.2365941

# XXX START HERE
#############################################################################
# TUTORIAL 7.4
# MULTI-MODEL INFERENCE IN AN OADA WITH MULTIPLE NETWORKS
# 1 diffusion
# 2 static networks
# 2 time constant ILVs
#############################################################################


#Read in the 2 social networks and order of acquisition vector as shown in Tutorial 3
socNet1<-as.matrix(read.csv(file="jane13307-sup-0001-supinfo/jane_13307_exampleStaticSocNet.csv"))
socNet2<-as.matrix(read.csv(file="jane13307-sup-0001-supinfo/jane_13307_exampleStaticSocNet2.csv"))
oa1<-c(26,29,30,8,19,21,22,3,14,12,11,1,17,28,5,9,15,7,6,25,4,13,27,18,20,24,23,16,2,10)

#Load and process the ILVs as shown in Tutorial 2
ILVdata<-read.csv(file="jane13307-sup-0001-supinfo/jane_13307_exampleTimeConstantILVs.csv")
female<-cbind(ILVdata$female)
male<-1-female
age<-cbind(ILVdata$age)
stAge<-(age-mean(age))/sd(age)
asoc<-c("male","stAge")

#In a multi-network NBDA, we need to combine our networks into an array
#If all of our networks are static (do not change over time) this is a three-dimensional array of size
#no. individuals x no.individuals x no.networks
socNets<-array(NA,dim=c(30,30,2))
#Then slot the networks into the array:
socNets[,,1]<-socNet1
socNets[,,2]<-socNet2

#Then we go on to create our nbdaData object as before. Let us assume we are interested in the unconstrained model:
nbdaData_multiNet<-nbdaData(label="ExampleDiffusion2",assMatrix=socNets,orderAcq = oa1,asoc_ilv = asoc,int_ilv = asoc )
#Then fit the model:
model1_multiNet<-oadaFit(nbdaData_multiNet)
#And get the output:
data.frame(Variable=model1_multiNet@varNames,MLE=model1_multiNet@outputPar,SE=model1_multiNet@se)

#Let us assume we have 4 competing hypotheses about social transmission.
#1. Transmission through network 1 only (s2=0)
#2. Transmission through network 2 only (s1=0)
#3. Transmission through network 1 and network 2 at equal rates per unit connection (s1=s2)
#4. Transmission through network 1 and network 2 at different rate (no constraint)

#Each of these can be represented by different constraints between the s parameters, which we refer to as a network combination or netcombo:
#1. 1 0
#2. 0 1
#3. 1 1
#4. 1 2

#We can fit models with each netcombo and compare the fit. However, we are unsure which ILVs to include, so we want to use multi-model inference
#We need to set up a constraintsVectMatrix that considers each netcombo but also every combination of ILVs

constraintsVectMatrix<-rbind(
  #netcombo 1 0
  c(1,0,0,0,0,0),
  c(1,0,0,0,0,2),
  c(1,0,0,0,2,0),
  c(1,0,0,0,2,3),
  c(1,0,0,2,0,0),
  c(1,0,0,2,0,3),
  c(1,0,0,2,3,0),
  c(1,0,0,2,3,4),
  c(1,0,2,0,0,0),
  c(1,0,2,0,0,3),
  c(1,0,2,0,3,0),
  c(1,0,2,0,3,4),
  c(1,0,2,3,0,0),
  c(1,0,2,3,0,4),
  c(1,0,2,3,4,0),
  c(1,0,2,3,4,5),
  
  #netcombo 0 1
  c(0,1,0,0,0,0),
  c(0,1,0,0,0,2),
  c(0,1,0,0,2,0),
  c(0,1,0,0,2,3),
  c(0,1,0,2,0,0),
  c(0,1,0,2,0,3),
  c(0,1,0,2,3,0),
  c(0,1,0,2,3,4),
  c(0,1,2,0,0,0),
  c(0,1,2,0,0,3),
  c(0,1,2,0,3,0),
  c(0,1,2,0,3,4),
  c(0,1,2,3,0,0),
  c(0,1,2,3,0,4),
  c(0,1,2,3,4,0),
  c(0,1,2,3,4,5),
  
  #netcombo 1 1
  c(1,1,0,0,0,0),
  c(1,1,0,0,0,2),
  c(1,1,0,0,2,0),
  c(1,1,0,0,2,3),
  c(1,1,0,2,0,0),
  c(1,1,0,2,0,3),
  c(1,1,0,2,3,0),
  c(1,1,0,2,3,4),
  c(1,1,2,0,0,0),
  c(1,1,2,0,0,3),
  c(1,1,2,0,3,0),
  c(1,1,2,0,3,4),
  c(1,1,2,3,0,0),
  c(1,1,2,3,0,4),
  c(1,1,2,3,4,0),
  c(1,1,2,3,4,5),
  
  #netcombo 1 2
  c(1,2,0,0,0,0),
  c(1,2,0,0,0,3),
  c(1,2,0,0,3,0),
  c(1,2,0,0,3,4),
  c(1,2,0,3,0,0),
  c(1,2,0,3,0,4),
  c(1,2,0,3,4,0),
  c(1,2,0,3,4,5),
  c(1,2,3,0,0,0),
  c(1,2,3,0,0,4),
  c(1,2,3,0,4,0),
  c(1,2,3,0,4,5),
  c(1,2,3,4,0,0),
  c(1,2,3,4,0,5),
  c(1,2,3,4,5,0),
  c(1,2,3,4,5,6),
  
  #netcombo 0 0 
  c(0,0,0,0,0,0),
  c(0,0,1,0,0,0),
  c(0,0,0,1,0,0),
  c(0,0,1,2,0,0)
)

modelSet_multiNet<-oadaAICtable(nbdaData_multiNet,constraintsVectMatrix = constraintsVectMatrix)

networksSupport(modelSet_multiNet)

#support numberOfModels
#0:0 0.01065492              4
#0:1 0.00818532             16
#1:0 0.67992442             16
#1:1 0.11457221             16
#1:2 0.18666313             16

#Note that we have equal numbers of models for each network combo save for asocial learning (0 0), so we can compare the support for different models
#of social transmission. However, we recommend examining the CIs for s parameters to judge the evidence for social transmission versus asocial learning (see main text).
#We can see strongest support (68%) for social transmission only through network 1, and second most (18.7%) for transmission of different strengths through each network

#We can also get MAEs etc as before:

rbind(
  support=variableSupport(modelSet_multiNet),
  MAE=modelAverageEstimates(modelSet_multiNet),
  USE=unconditionalStdErr(modelSet_multiNet))

#              s1        s2   ASOC:male ASOC:stAge SOCIAL:male SOCIAL:stAge
#support 0.9811598 0.3094207   0.5935977  0.3502696  0.22537376    0.3445204
#MAE     3.6470815 0.1433855 -11.2021988 -0.3394773 -0.02000315    0.1031443
#USE           NaN       NaN         NaN  0.5557081         NaN          NaN

#The missing USEs are due to some models in which the SEs could not be derived. If we look at
print(modelSet_multiNet)
#We can see that the SEs are present in all the models with high weight, so it seems reasonable to get an approximate USE by replacing NAs and NaNs with the weighted mean across
#all other models:

rbind(
  support=variableSupport(modelSet_multiNet),
  MAE=modelAverageEstimates(modelSet_multiNet),
  USE=unconditionalStdErr(modelSet_multiNet,nanReplace = T))

#              s1        s2     ASOC:male    ASOC:stAge SOCIAL:male SOCIAL:stAge
#support  0.9811598 0.3094207  5.935977e-01  0.3502696  0.22537376    0.3445204
#MAE      3.6470815 0.1433855 -1.120220e+01 -0.3394773 -0.02000315    0.1031443
#USE     36.0285332 1.0059692  7.310227e+07  0.5557081  0.14394848    0.1284004

#Averaged across models we can see that s1 is estimated to be considerably greater than s2.
#We would recommend presenting the 95% CI for s1 and s2 in the best model in which they are present
#and the 95% CI for s1-s2 in the best model in which they are both present and unconstrained.

#We can check the sensitivity of the 95% CIs for s1 to model selection uncertainty as before:

lowerLimitsByModel<-multiModelLowerLimits(which=1,aicTable = modelSet_multiNet,conf=0.95)
lowerLimitsByModel

#   model netCombo      lowerCI  propST deltaAICc akaikeWeight  adjAkWeight cumulAdjAkWeight
#1      9      1:0 3.899414e-01 0.53571 0.0000000 0.1315365430 0.1340623086        0.1340623
#2      1      1:0 3.007326e-01 0.47783 0.8673898 0.0852501433 0.0868871171        0.2209494
#3     13      1:0 4.020928e-01 0.54412 1.1352833 0.0745628988 0.0759946561        0.2969441
#4     10      1:0 4.086640e-01 0.54993 1.2875510 0.0690968545 0.0704236527        0.3673677
#5      2      1:0 2.690855e-01 0.45299 1.9136852 0.0505236774 0.0514938334        0.4188616
#6     14      1:0 1.452914e-01 0.38811 2.1230682 0.0455017414 0.0463754662        0.4652370
#7     11      1:0 9.396559e-02 0.30242 2.4186965 0.0392494122 0.0400030796        0.5052401
#8     57      1:2 3.743582e-01 0.73650 2.4786325 0.0380906356 0.0388220522        0.5440622
#9      5      1:0 4.037247e-01 0.54338 2.5009361 0.0376682150 0.0383915203        0.5824537
#10     3      1:0 3.522655e-01 0.51316 2.8398628 0.0317964081 0.0324069629        0.6148606
#11     6      1:0 5.757081e-02 0.23110 3.0352408 0.0288371449 0.0293908759        0.6442515
#12    49      1:2 3.007326e-01 0.47783 3.1689771 0.0269719152 0.0274898301        0.6717414
#13    41      1:1 1.573432e-01 0.41185 3.5213567 0.0226148409 0.0230490912        0.6947904
#14    15      1:0 1.242905e-01 0.34397 3.7513244 0.0201584287 0.0205455110        0.7153360
#15    61      1:2 3.639542e-01 0.74839 3.8122064 0.0195540321 0.0199295088        0.7352655
#16    12      1:0 4.061835e-02 0.26161 3.9587446 0.0181725536 0.0185215031        0.7537870
#17    58      1:2 4.066797e-01 0.74767 3.9644741 0.0181205685 0.0184685197        0.7722555
#18    45      1:1 1.856705e-01 0.50556 4.0029245 0.0177755243 0.0181168501        0.7903723
#19    33      1:1 1.274416e-02 0.04379 4.1942219 0.0161540980 0.0164642891        0.8068366
#20     4      1:0 2.374518e-01 0.43650 4.2344673 0.0158322823 0.0161362939        0.8229729
#21    50      1:2 2.690855e-01 0.46200 4.3923177 0.0146307554 0.0149116953        0.8378846
#22     7      1:0 4.763377e-01 0.58550 4.6267354 0.0130125853 0.0132624530        0.8511471
#23    53      1:2 4.031556e-01 0.54471 4.9795686 0.0109080429 0.0111174992        0.8622646
#24    16      1:0 1.909823e-02 0.24849 5.0137907 0.0107229829 0.0109288857        0.8731935
#25    62      1:2 1.452915e-01 0.45126 5.0230682 0.0106733566 0.0108783065        0.8840718
#26    59      1:2 9.396390e-02 0.61701 5.0956195 0.0102931120 0.0104907604        0.8945625
#27    51      1:2 3.522655e-01 0.52724 5.3184952 0.0092076724 0.0093844782        0.9039470
#28    42      1:1 1.565209e-01 0.41558 5.5865516 0.0080527119 0.0082073401        0.9121543
#29     8      1:0 1.176647e-02 0.19174 5.5990505 0.0080025438 0.0081562087        0.9203105
#30    54      1:2 5.757082e-02 0.29719 5.7121639 0.0075625072 0.0077077225        0.9280183
#31    37      1:1 5.059767e-02 0.14921 5.7482011 0.0074274614 0.0075700836        0.9355884
#32    46      1:1 7.603493e-02 0.37244 5.9165026 0.0068280110 0.0069591225        0.9425475
#33    43      1:1 2.559226e-03 0.52095 5.9343480 0.0067673575 0.0068973043        0.9494448
#34    34      1:1 1.081990e-02 0.15035 6.1186879 0.0061714929 0.0062899979        0.9557348
#36    35      1:1 9.875455e-03 0.15061 6.4670343 0.0051849882 0.0052845504        0.9610193
#37    47      1:1 2.528128e-02 0.55149 6.5904721 0.0048746518 0.0049682549        0.9659876
#38    63      1:2 1.242834e-01 0.64277 6.6513244 0.0047285684 0.0048193664        0.9708070
#39    60      1:2 4.062878e-02 0.57001 6.8586465 0.0042629502 0.0043448074        0.9751518
#40    52      1:2 2.374518e-01 0.48312 6.9113904 0.0041519973 0.0042317240        0.9793835
#41    38      1:1 4.534872e-05 0.11161 7.2380830 0.0035262790 0.0035939906        0.9829775
#42    55      1:2 4.753915e-01 0.58807 7.3036585 0.0034125351 0.0034780626        0.9864555
#43    44      1:1 6.649519e-05 0.51734 8.0635604 0.0023338155 0.0023786295        0.9888342
#44    64      1:2 1.909815e-02 0.33128 8.1659646 0.0022173270 0.0022599041        0.9910941
#45    39      1:1 4.755244e-02 0.22975 8.1991606 0.0021808276 0.0022227038        0.9933168
#49    48      1:1 6.307494e-05 0.28074 8.4117525 0.0019609096 0.0019985630        0.9953153
#50    56      1:2 1.176656e-02 0.22252 8.4990505 0.0018771590 0.0019132042        0.9972285
#51    36      1:1 3.834547e-02 0.25710 8.5969567 0.0017874792 0.0018218024        0.9990503
#53    40      1:1 7.916526e-05 0.16341 9.8999189 0.0009317649 0.0009496567        1.0000000

#All 95% CI are above zero, adding to our confidence that we have evidence for social transmission through network 1


#We can obtain a model averaged lower-limit for propST as follows:

sum(lowerLimitsByModel$propST*lowerLimitsByModel$adjAkWeight)
#[1] 0.5473627

#However, note that this includes models with netCombo= 1 1- i.e. one in which the value of s1 is constrained to be = s2. 
#Our preferred approach is to refit the model set including only those models in which the value of s1 is unconstrained, and re-run the exercise:


constraintsVectMatrixs1<-rbind(
  #netcombo 1 0
  c(1,0,0,0,0,0),
  c(1,0,0,0,0,2),
  c(1,0,0,0,2,0),
  c(1,0,0,0,2,3),
  c(1,0,0,2,0,0),
  c(1,0,0,2,0,3),
  c(1,0,0,2,3,0),
  c(1,0,0,2,3,4),
  c(1,0,2,0,0,0),
  c(1,0,2,0,0,3),
  c(1,0,2,0,3,0),
  c(1,0,2,0,3,4),
  c(1,0,2,3,0,0),
  c(1,0,2,3,0,4),
  c(1,0,2,3,4,0),
  c(1,0,2,3,4,5),
  
  #netcombo 1 2
  c(1,2,0,0,0,0),
  c(1,2,0,0,0,3),
  c(1,2,0,0,3,0),
  c(1,2,0,0,3,4),
  c(1,2,0,3,0,0),
  c(1,2,0,3,0,4),
  c(1,2,0,3,4,0),
  c(1,2,0,3,4,5),
  c(1,2,3,0,0,0),
  c(1,2,3,0,0,4),
  c(1,2,3,0,4,0),
  c(1,2,3,0,4,5),
  c(1,2,3,4,0,0),
  c(1,2,3,4,0,5),
  c(1,2,3,4,5,0),
  c(1,2,3,4,5,6))

modelSet_s1<-oadaAICtable(nbdadata = nbdaData_multiNet,constraintsVectMatrix =constraintsVectMatrixs1)
lowerLimitsByModel_s1<-multiModelLowerLimits(which=1,aicTable = modelSet_s1,conf=0.95)
lowerLimitsByModel_s1

sum(lowerLimitsByModel_s1$propST*lowerLimitsByModel_s1$adjAkWeight)
#[1] 0.5857381

#Giving a model-averaged lower limit of 58.6% of events occurring as a result of social transmission via network 1


#We can now do the same for s2:

constraintsVectMatrixs2<-rbind(
  #netcombo 0 1
  c(0,1,0,0,0,0),
  c(0,1,0,0,0,2),
  c(0,1,0,0,2,0),
  c(0,1,0,0,2,3),
  c(0,1,0,2,0,0),
  c(0,1,0,2,0,3),
  c(0,1,0,2,3,0),
  c(0,1,0,2,3,4),
  c(0,1,2,0,0,0),
  c(0,1,2,0,0,3),
  c(0,1,2,0,3,0),
  c(0,1,2,0,3,4),
  c(0,1,2,3,0,0),
  c(0,1,2,3,0,4),
  c(0,1,2,3,4,0),
  c(0,1,2,3,4,5),
  
  #netcombo 1 2
  c(1,2,0,0,0,0),
  c(1,2,0,0,0,3),
  c(1,2,0,0,3,0),
  c(1,2,0,0,3,4),
  c(1,2,0,3,0,0),
  c(1,2,0,3,0,4),
  c(1,2,0,3,4,0),
  c(1,2,0,3,4,5),
  c(1,2,3,0,0,0),
  c(1,2,3,0,0,4),
  c(1,2,3,0,4,0),
  c(1,2,3,0,4,5),
  c(1,2,3,4,0,0),
  c(1,2,3,4,0,5),
  c(1,2,3,4,5,0),
  c(1,2,3,4,5,6))

modelSet_s2<-oadaAICtable(nbdadata = nbdaData_multiNet,constraintsVectMatrix =constraintsVectMatrixs2)

lowerLimitsByModel_s2<-multiModelLowerLimits(which=2,aicTable = modelSet_s2,conf=0.95)
lowerLimitsByModel_s2

#model netCombo    lowerCI  propST  deltaAICc akaikeWeight  adjAkWeight cumulAdjAkWeight
#1     25      1:2 0.00000000 0.00000  0.0000000 0.1954885179 0.1954885179        0.1954885
#2     17      1:2 0.00000000 0.00000  0.6903446 0.1384250918 0.1384250918        0.3339136
#3     29      1:2 0.00000000 0.00000  1.3335739 0.1003550795 0.1003550795        0.4342687
#4     26      1:2 0.00000000 0.00000  1.4858416 0.0929982664 0.0929982664        0.5272670
#5     18      1:2 0.00000000 0.00000  1.9136852 0.0750878699 0.0750878699        0.6023548
#6     21      1:2 0.00000000 0.00000  2.5009361 0.0559821884 0.0559821884        0.6583370
#7     30      1:2 0.00000000 0.00000  2.5444357 0.0547777329 0.0547777329        0.7131147
#8     27      1:2 0.00000000 0.00000  2.6169871 0.0528262440 0.0528262440        0.7659410
#9     19      1:2 0.00000000 0.00000  2.8398627 0.0472555577 0.0472555577        0.8131965
#10    22      1:2 0.00000000 0.00000  3.2335314 0.0388122512 0.0388122512        0.8520088
#11    31      1:2 0.00000000 0.00000  4.1726919 0.0242679289 0.0242679289        0.8762767
#12    28      1:2 0.00000000 0.00000  4.3800140 0.0218782859 0.0218782859        0.8981550
#13    20      1:2 0.00000000 0.00000  4.4327579 0.0213088542 0.0213088542        0.9194639
#14    23      1:2 0.00000000 0.00000  4.8250260 0.0175137910 0.0175137910        0.9369777
#15    32      1:2 0.00000000 0.00000  5.6873321 0.0113797515 0.0113797515        0.9483574
#16     1      0:1 0.00000000 0.00000  5.8518172 0.0104813034 0.0104813034        0.9588387
#17    24      1:2 0.00000000 0.00000  6.0204180 0.0096339435 0.0096339435        0.9684727
#18     2      0:1 0.00000000 0.00000  7.3398351 0.0049807681 0.0049807681        0.9734534
#19    13      0:1 0.00000000 0.00000  7.9931050 0.0035928621 0.0035928621        0.9770463
#20     9      0:1 0.00000000 0.00000  8.0761826 0.0034466762 0.0034466762        0.9804930
#21     5      0:1 0.00000000 0.00000  8.1326620 0.0033507045 0.0033507045        0.9838437
#22     3      0:1 0.00000000 0.00000  8.1453457 0.0033295221 0.0033295221        0.9871732
#23     4      0:1 0.00000000 0.00000  8.9206772 0.0022595440 0.0022595440        0.9894327
#24     6      0:1 0.00000000 0.00000  9.3040168 0.0018654331 0.0018654331        0.9912982
#25    10      0:1 0.00000000 0.00000  9.6072278 0.0016030174 0.0016030174        0.9929012
#26    11      0:1 0.00000000 0.00000  9.8584312 0.0014138070 0.0014138070        0.9943150
#27    15      0:1 0.00000000 0.00000  9.9529192 0.0013485663 0.0013485663        0.9956636
#28    14      0:1 0.00000000 0.00000 10.3194716 0.0011227329 0.0011227329        0.9967863
#29     7      0:1 0.00000000 0.00000 10.6018689 0.0009748879 0.0009748879        0.9977612
#30     8      0:1 0.00000000 0.00000 10.7016820 0.0009274287 0.0009274287        0.9986886
#31    12      0:1 0.00000000 0.00000 11.0238742 0.0007894368 0.0007894368        0.9994780
#32    16      0:1 0.07159997 0.44518 11.8513525 0.0005219546 0.0005219546        1.0000000

#In this case, for all models except the worst one the 95% CI for s2 contain 0, so we can be sure we do not have good evidence for social
#transmission via network 2.

sum(lowerLimitsByModel_s2$propST*lowerLimitsByModel_s2$adjAkWeight)
#[1] 0.0002323638

#############################################################################
# TUTORIAL 7.5
# MULTI-MODEL INFERENCE IN A cTADA: INCLUDING A HOMOGENEOUS NETWORK
# 1 diffusion
# 1 static network
# 2 time constant ILVs
#############################################################################

# We can include multiple networks in a TADA in the same way as described for an OADA in tutorial 7.4.
# Furthermore, in a TADA, even when we only have one social network of interest, we may wish to include
# a homogeneous network in our multi-model procedure (see tutorial 4.3).
# If we have multiple diffusions and use a cTADA we may also want to include a group network (see tutorial
# 6.5)
# In this tutorial we show how to include a homogeneous network in a multi-model inference procedure for
# a cTADA of a single diffusion. A group network could be included in a cTADA/ stratified OADA of multiple
# diffusions in the same way.


#We use the same data as for tutoral 7.3:
#########################################
#Read in the social network and order of acquisition vector as shown in Tutorials 1 and 2
socNet1<-as.matrix(read.csv(file="jane13307-sup-0001-supinfo/jane_13307_exampleStaticSocNet.csv"))
socNet1<-array(socNet1,dim=c(30,30,1))
oa1<-c(26,29,30,8,19,21,22,3,14,12,11,1,17,28,5,9,15,7,6,25,4,13,27,18,20,24,23,16,2,10)

ILVdata<-read.csv(file="jane13307-sup-0001-supinfo/jane_13307_exampleTimeConstantILVs.csv")
#Then extract each ILV
female<-cbind(ILVdata$female)
male<-1-female
#females=0 males=1
age<-cbind(ILVdata$age)
#age in years
#I am going to standardize age
stAge<-(age-mean(age))/sd(age)

asoc<-c("male","stAge")

#Now in addition, we need a vector giving the times at which each individual acquired the behaviour:
ta1<-c(234,252,262,266,273,296,298,310,313,326,332,334,334,337,338,340,343,367,374,376,377,393,402,405,407,407,435,472,499,567)
#######################################

#Now we add in the homogeneous network as a second network:
dim(socNet1)
socNets<-array(NA,dim=c(30,30,2))
socNets[,,1]<-socNet1
socNets[,,2]<-1


#Create an object for the "unconstrained" model
nbdaData_tada<-nbdaData(label="ExampleDiffusion2",assMatrix=socNets,orderAcq = oa1,asoc_ilv = asoc,int_ilv=asoc,timeAcq = ta1,endTime = 568)

#Now we have an object with two networks- the first is the social network, the second the homogeneous network

#Now for the model set, we consider all possible sets of ILVs in an unconstrained model for both social and
#homogeneous networks and an asocial model too:

constraintsVectMatrix<-rbind(
  #social network models (1 in first slot, 0 in second)
  c(1,0,0,0,0,0),
  c(1,0,0,0,0,2),
  c(1,0,0,0,2,0),
  c(1,0,0,0,2,3),
  c(1,0,0,2,0,0),
  c(1,0,0,2,0,3),
  c(1,0,0,2,3,0),
  c(1,0,0,2,3,4),
  c(1,0,2,0,0,0),
  c(1,0,2,0,0,3),
  c(1,0,2,0,3,0),
  c(1,0,2,0,3,4),
  c(1,0,2,3,0,0),
  c(1,0,2,3,0,4),
  c(1,0,2,3,4,0),
  c(1,0,2,3,4,5),
  #homogeneous network models (0 in first slot, 1 in second)
  c(0,1,0,0,0,0),
  c(0,1,0,0,0,2),
  c(0,1,0,0,2,0),
  c(0,1,0,0,2,3),
  c(0,1,0,2,0,0),
  c(0,1,0,2,0,3),
  c(0,1,0,2,3,0),
  c(0,1,0,2,3,4),
  c(0,1,2,0,0,0),
  c(0,1,2,0,0,3),
  c(0,1,2,0,3,0),
  c(0,1,2,0,3,4),
  c(0,1,2,3,0,0),
  c(0,1,2,3,0,4),
  c(0,1,2,3,4,0),
  c(0,1,2,3,4,5),
  #asocial models
  c(0,0,0,0,0,0),
  c(0,0,1,0,0,0),
  c(0,0,1,2,0,0),
  c(0,0,0,1,0,0))

#Again, in a TADA we likely want to consider different baseline functions too. 
#To speed things up we will just consider the constant and weibull baseline functions here,
#so we want to replicate the constraintsVectMartix above 2 times:
constraintsVectMatrixAll<-rbind(constraintsVectMatrix,constraintsVectMatrix)

#And we also need to provide a vector saying what baseline function each model will have.


baselineVect<-rep(c("constant","weibull"),each=dim(constraintsVectMatrix)[1])

#Check:
cbind(baselineVect,constraintsVectMatrixAll)

#To fit the set of models, we use
modelSet_tada<-tadaAICtable(nbdadata = nbdaData_tada,constraintsVectMatrix =constraintsVectMatrixAll,baselineVect = baselineVect)


#We can then get support for each combination of networks across all models
networksSupport(modelSet_tada)
#       support numberOfModels
#0:0 0.02347081              8
#0:1 0.03745207             32
#1:0 0.93907712             32

#Here we can see that the social network (1:0= 93.9%) has far more support than the homogeneous network (0:1= 3.7%) and the number of
#models is the same making this a fair comparison. The ratio of support is
0.93907712/0.03745207 
#One could report this result as something like:
# "25.1x more support for the social network than the homogeneous network, providing evidence that the diffusion follows the
# social network."

#One could then proceed to make inferences as in tutorial 7.3 above. Given the different scales of the networks, it would be reasonable
#to refit the model set to just include the social network before calculating model-averaged estimates. However, if the support for
#the homogeneous network is tiny (e.g. <0.1%) then it is unlikely to affect the model-averaged estimates at all.



