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

Mods_N.RS_Aso <- map(nbdaData_list_static, ~{
  tryCatch({oadaFit(.x, type = "asocial")}, error = function(msg){NULL})
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
aiccs_static_asocial <- map_dbl(Mods_N.RS_Aso, ~.x@aicc)
summaries$aicc_dynamic_asocial <- aiccs_dynamic_asocial
summaries$aicc_static_asocial <- aiccs_static_asocial

# AICC differences: dynamic vs static and social vs. asocial
summaries <- summaries %>%
  mutate(diff_S.D = aicc_static - aicc_dynamic,
         diff_DAso.DSo = aicc_dynamic_asocial - aicc_dynamic,
         diff_SAso.SSo = aicc_static_asocial - aicc_static,
         dynamic_favored = exp(0.5*(aicc_static - aicc_dynamic_asocial)),
         social_favored_dynamic = exp(0.5*(aicc_dynamic_asocial - aicc_dynamic)),
         social_favored_static = exp(0.5*(aicc_static_asocial - aicc_static)))

summaries %>%
  ggplot(aes(x = diff_S.D))+
  geom_histogram(fill = "lightgray", color = "darkgray")+
  theme_minimal()+
  labs(y = "Count",
       x = "Static-Dynamic",
       caption = "Positive values indicate more support for the dynamic model")+
  geom_vline(aes(xintercept = 0), col = "red", linetype = 2)

summaries %>%
  ggplot(aes(x = diff_DAso.DSo))+
  geom_histogram(fill = "lightgray", color = "darkgray")+
  theme_minimal()+
  labs(y = "Count",
       x = "Asocial-Social (dynamic models)",
       caption = "Positive values indicate more support for the social model")+
  geom_vline(aes(xintercept = 0), col = "red", linetype = 2)

summaries %>%
  ggplot(aes(x = diff_SAso.SSo))+
  geom_histogram(fill = "lightgray", color = "darkgray")+
  theme_minimal()+
  labs(y = "Count",
       x = "Asocial-Social (static models)",
       caption = "Positive values indicate more support for the social model")+
  geom_vline(aes(xintercept = 0), col = "red", linetype = 2)

# P-values: do we see social transmission?
summaries$p_DAso.DSo <- map2_dbl(Mods_N.RD_Aso, Mods_N.RD_So, ~{
  tryCatch({pchisq(2*(.x@loglik - .y@loglik), df = 1, lower.tail = F)}, error = function(msg){NA})
})

summaries$p_SAso.SSo <- map2_dbl(Mods_N.RS_Aso, Mods_N.RS_So, ~{
  tryCatch({pchisq(2*(.x@loglik - .y@loglik), df = 1, lower.tail = F)}, error = function(msg){NA})
})

summaries$p_S.D <- map2_dbl(Mods_N.RD_Aso, Mods_N.RD_So, ~{
  tryCatch({pchisq(2*(.x@loglik - .y@loglik), df = 1, lower.tail = F)}, error = function(msg){NA})
})

summaries %>%
  mutate(sig = ifelse(p_DAso.DSo <= 0.05, T, F)) %>%
  filter(!is.na(sig)) %>%
  ggplot(aes(x = diff_DAso.DSo, y = carcID))+
  geom_point(aes(shape = sig), size = 2)+
  scale_shape_manual(name = "Evidence for\nsoc.transmission?\n(alpha = 0.05)", 
                     values = c(1, 19))+
  theme_minimal()+
  labs(y = "Carcass",
       x = "AICC difference (asocial - social)",
       title = "Social transmission",
       subtitle = "Dynamic roost networks")+
  theme(legend.position = "bottom")+
  geom_vline(aes(xintercept = 0), color = "red", linetype = 2)
# It's a bit odd that we're not seeing more evidence for social transmission! What does the evidence for social transmission look like over the static roost network?

summaries %>%
  mutate(sig = ifelse(p_SAso.SSo <= 0.05, T, F)) %>%
  filter(!is.na(sig)) %>%
  ggplot(aes(x = diff_SAso.SSo, y = carcID))+
  geom_point(aes(shape = sig), size = 2)+
  scale_shape_manual(name = "Evidence for\nsoc.transmission?\n(alpha = 0.05)", 
                     values = c(1, 19))+
  theme_minimal()+
  labs(y = "Carcass",
       x = "AICC difference (asocial - social)",
       title = "Social transmission",
       subtitle = "Static roost networks")+
  theme(legend.position = "bottom")+
  geom_vline(aes(xintercept = 0), color = "red", linetype = 2) # similar to the dynamic ones.

# Get the proportion of events estimated to be solved by social transmission
solveprops_dynamic <- map(Mods_N.RD_So, ~as.data.frame(t(nbdaPropSolveByST(model = .x)))) %>% setNames(carcIDs) %>% list_rbind(names_to = "carcID") %>%
  select(-V1)
solveprops_static <- map(Mods_N.RS_So, ~as.data.frame(t(nbdaPropSolveByST(model = .x)))) %>% setNames(carcIDs) %>% list_rbind(names_to = "carcID") %>%
  select(-V1)

# Get confidence intervals
## This is the tricky part--in order to find the confidence intervals, we have to manually visualize the plots. Luckily, there aren't too many of them.

search <- data.frame(type = c(rep("dynamic", length(Mods_N.RD_So)),
                              rep("static", length(Mods_N.RS_So))),
                     lower_min = NA,
                     lower_max = NA,
                     upper_min = NA,
                     upper_max = NA,
                     ci_lower = NA,
                     ci_upper = NA,
                     carcID = carcIDs)
## Dynamic models
#plotProfLik(which = 1, model = Mods_N.RD_So[[1]], range = c(0,2.5))
search[1,2:5] <- c(0, 0.2, 0.4, 0.6)
#plotProfLik(which = 1, model = Mods_N.RD_So[[2]], range = c(0,2.5))
search[2,2:5] <- c(0, 0.2, 0.5, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[3]], range = c(20,30))
search[3,2:5] <- c(NA, NA, 24, 28)
#plotProfLik(which = 1, model = Mods_N.RD_So[[4]], range = c(0, 1))
search[4,2:5] <- c(NA, NA, 0, 0.2)
#plotProfLik(which = 1, model = Mods_N.RD_So[[5]], range = c(0, 2.5))
search[5,2:5] <- c(NA, NA, 0.25, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[6]], range = c(0, 2.5))
search[6,2:5] <- c(0, 0.2, 0.4, 0.6)
#plotProfLik(which = 1, model = Mods_N.RD_So[[7]], range = c(0, 2.5))
search[7,2:5] <- c(NA, NA, 0.5, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[8]], range = c(0, 1))
search[8,2:5] <- c(NA, NA, 0, 0.2)
#plotProfLik(which = 1, model = Mods_N.RD_So[[9]], range = c(0, 1))
search[9,2:5] <- c(0, 0.1, 0.4, 0.6)
#plotProfLik(which = 1, model = Mods_N.RD_So[[10]], range = c(0, 1))
search[10,2:5] <- c(NA, NA, 0.4, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[11]], range = c(0, 2))
search[11,2:5] <- c(0, 0.25, 1, 1.6)
#plotProfLik(which = 1, model = Mods_N.RD_So[[12]], range = c(0, 2))
search[12,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[12]], range = c(0, 2))
search[12,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[13]], range = c(0, 2))
search[13,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[14]], range = c(0, 10))
search[14,2:5] <- c(NA, NA, 4, 6)
#plotProfLik(which = 1, model = Mods_N.RD_So[[15]], range = c(0, 2.5))
search[15,2:5] <- c(0, 0.5, 1.5, 2.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[16]], range = c(0, 2))
search[16,2:5] <- c(NA, NA, 0.5, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[17]], range = c(0, 50))
search[17,2:5] <- c(NA, NA, 30, 40)
#plotProfLik(which = 1, model = Mods_N.RD_So[[18]], range = c(0, 5))
search[18,2:5] <- c(NA, NA, 2, 3)
#plotProfLik(which = 1, model = Mods_N.RD_So[[19]], range = c(0, 30))
search[19,2:5] <- c(NA, NA, 25, 30)
#plotProfLik(which = 1, model = Mods_N.RD_So[[20]], range = c(0, 5))
search[20,2:5] <- c(NA, NA, 0, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[21]], range = c(0, 2))
search[21,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[22]], range = c(0, 2))
search[22,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[23]], range = c(0, 2.5))
search[23,2:5] <- c(0, 0.5, 2, 2.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[24]], range = c(0, 2.5))
#plotProfLik(which = 1, model = Mods_N.RD_So[[25]], range = c(0, 2))
search[25,2:5] <- c(0, 0.25, 0.5, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[26]], range = c(0, 2))
search[26,2:5] <- c(NA, NA, 1.5, 2)
plotProfLik(which = 1, model = Mods_N.RD_So[[27]], range = c(0, 2))
#search[27,2:5] <- c(0, 0.25, 0.5, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[28]], range = c(0, 2))
search[28,2:5] <- c(NA, NA, 0.5, 1)
#plotProfLik(which = 1, model = Mods_N.RD_So[[29]], range = c(0, 2))
search[29,2:5] <- c(0, 0.5, 1, 1.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[30]], range = c(0, 10))
search[30,2:5] <- c(NA, NA, 6, 8)
#plotProfLik(which = 1, model = Mods_N.RD_So[[31]], range = c(0, 2.5))
search[31,2:5] <- c(NA, NA, 0, 0.5)
#plotProfLik(which = 1, model = Mods_N.RD_So[[32]], range = c(0, 3))
search[32,2:5] <- c(0, 0.5, 2, 3)
#plotProfLik(which = 1, model = Mods_N.RD_So[[33]], range = c(0, 10))
search[33,2:5] <- c(0, 2, 6, 8)
#plotProfLik(which = 1, model = Mods_N.RD_So[[34]], range = c(0, 10))
search[34,2:5] <- c(NA, NA, 8, 9)
#plotProfLik(which = 1, model = Mods_N.RD_So[[35]], range = c(0, 5))
search[35,2:5] <- c(NA, NA, 4, 5)

for(i in 1:length(Mods_N.RD_So)){
  if(is.na(search[i,2]) & !is.na(search[i,4])){
    ci <- profLikCI(which = 1, model = Mods_N.RD_So[[i]],
                    upperRange = search[i,4:5])
  }else if(!is.na(search[i,2]) & !is.na(search[i,4])){
    ci <- profLikCI(which = 1, model = Mods_N.RD_So[[i]],
                    lowerRange = search[i,2:3],
                    upperRange = search[i,4:5])
  }else{
    ci <- c(NA, NA)
  }
  search[i,6:7] <- ci 
  cat("done with ", i, "\n")
}

solveprops_dynamic_lower <- map2_dbl(search$ci_lower[search$type == "dynamic"], nbdaData_list_dynamic, ~{
  nbdaPropSolveByST(par = .x, nbdadata = .y)[1]
  })

solveprops_dynamic_upper <- map2_dbl(search$ci_upper[search$type == "dynamic"], nbdaData_list_dynamic, ~{
  nbdaPropSolveByST(par = .x, nbdadata = .y)[1]
})

search$propsolve <- c(solveprops_dynamic[,2], solveprops_static[,2])
search$propsolve_lower[search$type == "dynamic"] <- solveprops_dynamic_lower
search$propsolve_upper[search$type == "dynamic"] <- solveprops_dynamic_upper

search <- search %>%
  mutate(sig = ifelse(propsolve_lower > 0, TRUE, FALSE))

search %>%
  filter(type == "dynamic") %>%
  ggplot(aes(y = carcID, color = sig))+
  geom_segment(aes(x = propsolve_lower, xend = propsolve_upper))+
  geom_point(aes(x = propsolve, pch = sig), size = 2)+
  scale_color_manual(values = c("gray60", "black"))+
  scale_shape_manual(values = c(21, 19))+
  theme_minimal()+
  labs(y = "Carcass",
       x = "Proportion of detections by social transmission",
       title = "Dynamic roost networks")

# Next step: do the same thing for the static networks
# Then do it with dynamic flight networks