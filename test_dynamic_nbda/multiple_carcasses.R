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

# Need to convert the oas into numeric indices instead of a character vector
matrix_orders <- map(roost_mats, ~row.names(.x[[1]]))
oas <- map2(oas, matrix_orders, ~match(.x, .y))

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
    geom_vline(aes(xintercept = carc$datetime), col = "red", lty = 2)+
    ggtitle(carcID)
}

map2(firsts, carcIDs, ~discoveryplot(.x, .y))

fl_mats <- fl_cumulative_bin_fixed_see[has_3_sightings]
roost_mats <- roosts_bin_fixed_see[has_3_sightings]
fl_nets <- fl_cumulative_bin_nets_see[has_3_sightings]
roost_nets <- roosts_bin_nets_see[has_3_sightings]

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

 #XXX start here
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
        select(local_identifier, age_group, all_of(col))
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

Mods_N.RS_So <- map(nbdaData_list_static, ~{
  tryCatch({oadaFit(.x)}, error = function(msg){NULL})
})

Mods_N.RD_So <- map(nbdaData_list_dynamic, ~{
  tryCatch({oadaFit(.x)}, error = function(msg){NULL})
})

Mods_N.RD_Aso <- map(nbdaData_list_dynamic, ~{
  tryCatch({oadaFit(.x, type = "asocial")}, error = function(msg){NULL})
})

summaries_static <- map(Mods_N.RS_So, ~{
  tryCatch({bind_cols(nbdaModSum(.x), "aicc" = .x@aicc)}, error = function(msg){data.frame(Variable = NA, MLE = NA, SE = NA)})
}) %>% setNames(carcIDs) %>% list_rbind(names_to = "carcID")

summaries_dynamic <- map(Mods_N.RD_So, ~{
  tryCatch({bind_cols(nbdaModSum(.x), "aicc" = .x@aicc)}, error = function(msg){data.frame(Variable = NA, MLE = NA, SE = NA, aicc = NA)})
}) %>% setNames(carcIDs) %>% list_rbind(names_to = "carcID")

summaries <- left_join(summaries_static, summaries_dynamic, by = c("carcID", "Variable"), suffix = c("_static", "_dynamic"))
# Models with a dynamic network can be compared to static network models if they are fitted to the same order of acquisition, which these are.

aiccs_dynamic_asocial <- map_dbl(Mods_N.RD_Aso, ~.x@aicc)
summaries$aicc_dynamic_asocial <- aiccs_dynamic_asocial

# AICC differences: dynamic vs static and dynamic social vs. dynamic asocial
summaries <- summaries %>%
  mutate(diff_S.D = aicc_static - aicc_dynamic,
         diff_D.Aso.DSo = aicc_dynamic_asocial - aicc_dynamic,
         static_favored = exp(0.5*diff_S.D),
         asocial_favored = exp(0.5*diff_D.Aso.DSo))

# XXX should re-do these--if the other model is supported, this shows as 0, not negative, which is misleading. Should show the difference as a histogram, and then show how much more supported the model is for each one.
summaries %>%
  ggplot(aes(x = static_favored))+
  geom_histogram(fill = "lightgray", color = "darkgray")+
  theme_minimal()+
  labs(y = "Count",
       x = "Times more support for static model")+
  geom_vline(aes(xintercept = 0), col = "red", linetype = 2)

summaries %>%
  ggplot(aes(x = exp(0.5*-1*diff_D.Aso.DSo)))+
  geom_histogram(fill = "lightgray", color = "darkgray")+
  theme_minimal()+
  labs(y = "Count",
       x = "Times more support for social model")+
  geom_vline(aes(xintercept = 0), col = "red", linetype = 2)

# XXX start here--p-values
#There is 1 parameter in Mod_N.RS_So, and 0 in Mod_N.RS_Aso, so we have 1 d.f.
pchisq(2*(Mod_N.RD_Aso@loglik-Mod_N.RD_So@loglik),df=1,lower.tail=F)
#[1] 0.6152954
#p= 0.6152954; no evidence of an effect consistent with social transmission

plotProfLik(which = 1, model = Mod_N.RD_So, range = c(0,100), resolution = 20)
(p <- profLikCI(which = 1, model = Mod_N.RD_So, 
                lowerRange = c(0,2))) # same deal here; we can't find an upper bound.

nbdaPropSolveByST(model = Mod_N.RD_So)
#7.7% by social transmission

nbdaPropSolveByST(par = p[1], nbdadata = nbdaData2)
# P(Network 1)  P(S offset) 
# 0.3351       0.0000 # XXX this doesn't make sense! What's going on with this model? Why is the lower bound higher than the model itself?

nbdaPropSolveByST(par = p[2], nbdadata = nbdaData2)
# P(Network 1)  P(S offset) 
# NA           NA 

