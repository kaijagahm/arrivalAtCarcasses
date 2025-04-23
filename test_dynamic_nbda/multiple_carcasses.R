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
library(targets)

# Load data
tar_load(has_sightings)
tar_load(inpa_carcs)
tar_load(oa_see)
tar_load(firsts_see)

tar_load(fl_cumulative_bin_fixed_see)
tar_load(roosts_bin_fixed_see)

tar_load(gps)
tar_load(ilvs)

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

# Need to convert the oas into numeric indices instead of a character vector
matrix_orders <- map(roost_mats, ~row.names(.x[[1]]))
oas <- map2(oas, matrix_orders, ~match(.x, .y))

# Need to change the roost networks to have the same number of slices as the flight networks
length(fl_mats[[1]])
length(roost_mats[[1]])
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
# (Post-hoc analysis)
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

# Before I start making these decisions, let's see if we have any evidence of age or distance actually making a difference to when individuals discover the carcass. No point in correcting for it if we don't think it'll affect things. We could also preemptively decide to use dynamic NBDA since that would describe what's happening better--it doesn't make sense to use static.
fs <- map2(firsts_see[has_sightings][has_3_sightings], carcIDs, ~.x %>% mutate(carcID = .y))
mi <- map2(my_ilvs, carcIDs, ~.x %>% mutate(carcID = .y))
joined <- map2(mi, fs, left_join)
all(map_dbl(joined, nrow) == map_dbl(my_ilvs, nrow)) # same rows as number of individuals in the ILVs, not the number of individuals that eventually detected the carcass, since not everyone did eventually detect the carcass.
all(map_dbl(joined, nrow) == map_dbl(fs, nrow)) # false

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

# Next: move to model averaging, following the tutorial. See model_averaging.R.

# Save the objects I'll need for this
write_rds(age_groups_29, file = here("test_dynamic_nbda/data/age_groups_29.RDS"))
write_rds(std_roost_carc_distances_29, file = here("test_dynamic_nbda/data/std_roost_carc_distances_29.RDS"))
write_rds(N.RS, file = here("test_dynamic_nbda/data/N.RS.RDS"))
write_rds(N.RD, file = here("test_dynamic_nbda/data/N.RD.RDS"))
write_rds(N.FD, file = here("test_dynamic_nbda/data/N.FD.RDS"))
write_rds(oas, file = here("test_dynamic_nbda/data/oas.RDS"))
write_rds(roost_mats_expanded, file = here("test_dynamic_nbda/data/roost_mats_expanded.RDS"))
write_rds(fl_mats, file = here("test_dynamic_nbda/data/fl_mats.RDS"))
