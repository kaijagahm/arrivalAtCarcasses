# Script to test out a diffusion with different types of models
# Created 2025-01-28 in advance of the January vulture meeting
# Updated 2025-03-21 with 4km detection threshold and longer co-flight networks (cumulative)
# Parameters/specs
# - 1 diffusion
# - Dynamic roost networks (each day)
# - Dynamic flight networks (cumulative)
# - "acquisition" will be when each individual first detects the carcass (i.e. is within 4km of it)
# - will include all individuals, not just individuals that eventually detect the carcass, in the diffusion, per MJH advice in meeting on Tues 2/11
# OADA for now b/c I haven't really evaluated whether TADA is appropriate, and OADA is a bit simpler.
# Compare social and asocial
# Seeded demonstrators = anyone who was within 1km of the carcass when it was placed
# Time-constant ILVs: age group, initial distance to carcass

# Load packages -----------------------------------------------------------
library(here)
library(targets)
library(NBDA)
library(tidyverse)
library(sf)
library(devtools)

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
which(has_sightings)
whch <- 17
carc <- inpa_carcs[has_sightings][[whch]]
oa <- oa_see[[whch]]
firsts <- firsts_see[has_sightings][[whch]]
all(oa %in% firsts$local_identifier)
all(firsts$local_identifier %in% oa)

# Make a plot of them discovering the carcass
firsts %>% 
  ggplot(aes(x = timestamp, y = rownumber))+
  geom_line()+
  geom_point(size = 2, pch = 21, fill = "white")+
  theme_classic()+
  labs(y = "Cumulative number of vultures", 
       x = "Time", 
       title = "Vultures discovering the carcass", 
       caption = "Number of unique vultures that flew within sight (1km)\nof the carcass since placement")+
  geom_vline(aes(xintercept = carc$datetime), col = "red", lty = 2) # this one has three distinct days of carcass discovery. Let's see how that goes.

fl_mats <- fl_cumulative_bin_fixed_see[[whch]]
roost_mats <- roosts_bin_fixed_see[[whch]]
fl_nets <- fl_cumulative_bin_nets_see[[whch]]
roost_nets <- roosts_bin_nets_see[[whch]]

# map2(roost_nets, 1:length(roost_nets), ~{
#   ggraph(.x)+
#     geom_edge_link(color = "darkgreen")+
#     geom_node_label(aes(label = name), size = 3)+
#     theme_graph()+
#     ggtitle(paste0("Roost network, night ", .y))
# })

# map2(fl_nets, 1:length(fl_nets), ~{
#   ggraph(.x)+
#     geom_edge_link(color = "dodgerblue3")+
#     geom_node_label(aes(label = name), size = 3)+
#     theme_graph()+
#     ggtitle(paste0("Cumulative flight network, vulture #", .y))
# })

indivs <- row.names(roost_mats[[1]])

# Need to change the roost networks to have the same number of slices as the flight networks
length(fl_mats)
length(fl_nets)
length(roost_mats)
length(roost_nets)
# This means we need to figure out which acquisition events happened on which days
firsts %>%
  st_drop_geometry() %>%
  select(local_identifier, dateOnly, timestamp) 

carc$dateOnly
min(firsts$dateOnly)
max(firsts$dateOnly)
as.numeric(difftime(max(firsts$dateOnly), min(firsts$dateOnly)))
dates <- data.frame(dateOnly = seq.Date(from = carc$dateOnly, to = max(firsts$dateOnly), by = "day")) %>%
  mutate(day = 1:n())
firsts <- firsts %>%
  left_join(dates)
days_vec <- firsts$day

roost_mats_expanded <- vector(mode = "list", length = length(fl_mats))
for(i in 1:length(days_vec)){
  # First element of the roost networks should be the night before the carcass was placed. So I think we're actually not going to end up using the last element of the roost networks at this stage.
  roost_mats_expanded[[i]] <- roost_mats[[days_vec[i]]]
}

roost_nets_expanded <- vector(mode = "list", length = length(fl_mats))
for(i in 1:length(days_vec)){
  roost_nets_expanded[[i]] <- roost_nets[[days_vec[i]]]
}

# Okay, now we have the roost matrices and flight matrices, and they should be the same length (also have the networks for each, but I don't think I'm going to end up using those)
length(roost_mats_expanded) == length(fl_mats) # yay!

# One other thing to check: are we including all individuals, or only the ones that ended up seeing the carcass?
dim(roost_mats_expanded[[1]]) # equal dimensions
length(unique(gps[has_sightings][[whch]]$local_identifier)) # equal to each dimension of the previous one
dim(fl_mats[[1]]) # equal dimensions

dim(roost_mats_expanded[[20]])
dim(fl_mats[[20]]) # this seems good!

# Okay, so now we have the roost and flight networks, in matrix format, that we're going to need to put into the model. Now let's grab the ilvs
load(here("test_dynamic_nbda/data/ilvs.Rda"))
my_ilvs <- ilvs[has_sightings][[whch]] %>%
  rename("roost_night0" = "roost_2023-04-09",
         "roost_night1" = "roost_2023-04-10",
         "roost_night2" = "roost_2023-04-11",
         "roost_night3" = "roost_2023-04-12",
         "roost_night5" = "roost_2023-04-13")

# Let's make this into an extended data frame as well
days_vec
ilvs_list <- vector(mode = "list", length = length(roost_mats_expanded))
for(i in 1:length(ilvs_list)){
  col <- paste0("roost_night", days_vec[i]-1)
  ilvs_list[[i]] <- my_ilvs %>%
    select(local_identifier, age_group, all_of(col))
}
length(ilvs_list) # great, now we have a list of ILVs that we can make be dynamic.

# I think I have everything I need here. Let's check.

# Working the data into the JANE workflow ---------------------------------
# Imitate NBDA tutorial 1
# JANE 13307
# PACKAGES
## Workflow

# Some parameters
# How many individuals are involved in the diffusion?
n_indivs <- nrow(roost_mats[[1]])

# Read in the roost networks for each day
roost_mats[1:4] # ignoring day 5, since that's not relevant to this diffusion

## Create a static network for comparison by taking the mean of the roost network values
r_static_mn <- as.matrix(Reduce("+",roost_mats[1:4])/length(roost_mats[1:4])) # get the mean of the roost values from each day's network so we can use a static network

# Mod_N.RS: Static roost net, no ILVs -------------------------------------------

############################################################################# F
# TUTORIAL 1.1
# FITTING A BASIC OADA MODEL
# 1 diffusion
# 1 static network
# 0 ILVs
############################################################################# F
r_static_mn # static (mean) roost network
class(r_static_mn) # check that it's a matrix--it is!

#Convert the matrix to a three dimensional array- this is because the NBDA package
#is designed to work with multiple networks as well as a single network
N.RS <- array(r_static_mn, dim = c(n_indivs, n_indivs, 1))
#The network needs to be arranged such that row N contains the incoming connections for
#individual N.

#Enter a vector giving the order in which individuals learned the target behaviour
#This corresponds to the individuals' positions in the social network matrix
# Define the order of how the individuals appear in the matrix
matrix_order <- row.names(r_static_mn)
# Define the order of when the individuals sight the carcass
sighting_order <- firsts$local_identifier
# Use match to find the positions of the sighting_order in the matrix_order
oa <- match(sighting_order, matrix_order)

#e.g. the first individual to learn here is in the 15th row and column of N.RS, followed by the individual in the 18th row and column, etc.

# Create an nbdaData object containing the data we need to fit an OADA model
# The label is simply a string of text you can use to remind yourself what data is stored in this object
# assMatrix stands for association matrix, since these are most commonly used, but this can be any type of social network 
nbdaData1 <- nbdaData(label = paste0("Diffusion_", whch), 
                      assMatrix = N.RS, orderAcq = oa)

#We can now fit an OADA model using oadaFit, here we store it in an object named Mod_N.RS_So
Mod_N.RS_So <- oadaFit(nbdaData1)

#The maximum likelihood estimates (MLEs) for the parameter(s) is stored in the @outputPar slot
Mod_N.RS_So@outputPar
#The standard errors for the parameter(s) is stored in the @se slot
Mod_N.RS_So@se
#But we can get a neat printout of the model fit as follows
nbdaModSum(Mod_N.RS_So) # function defined at the top

#giving us:

# Variable      MLE       SE
# 1 1 Social transmission 1 1.387364 0.888224
# So we can see that s has been estimated at 1.387364, but the SE is kind of large in comparison, at 0.888224. Does this mean there is not good evidence for social transmission? (In the example, it didn't mean that necessarily, but we'll see...)

#The value estimated for s might be difficult to interpret, depending on the network
#used. In such cases, we can obtain an estimate of the % of events that occured by social
#transmission as opposed to asocial learning (%ST) as follows:

nbdaPropSolveByST(model = Mod_N.RS_So)
# P(Network 1)  P(S offset) 
# 0.11279      0.00000 

#This tells us that the estimated value for s corresponds to 11% (the function returns a 
#proportion so multiply by 100 to get %ST)
#P(Network 1) stands for the proportion of events that were a result of transmission through 
#network 1 (we only have one network)
#P(S offset) can be ignored for now
# XXX but note: this is based on the aggregate roost network, so it is likely not very accurate!!

#By default the calculation of %ST excludes the first learning event (innovation) for an unseeded diffusion, since we know this had to be asocial learning. If we want to include them we can do so:

nbdaPropSolveByST(model = Mod_N.RS_So, exclude.innovations = F)
# P(Network 1)  P(S offset) 
# 0.11108      0.00000 

#Let us fit an asocial model (with s=0), by specifying type="asocial"
Mod_N.RS_Aso <- oadaFit(nbdaData1, type = "asocial")

#And then compare the social and asocial models using 

Mod_N.RS_So@aicc
Mod_N.RS_Aso@aicc
Mod_N.RS_Aso@aicc-Mod_N.RS_So@aicc

#So the asocial model is favoured by 1.79 AICc units. This means:
exp(0.5*(Mod_N.RS_So@aicc-Mod_N.RS_Aso@aicc))
#[1] 2.454841
#the asocial model is 2.454841x more likely to be the best K-L model, out of the two. (interesting--did not expect that)
#Or we can say the asocial model has 2.454841x more support than the social model.

#We can also conduct a likelihood ratio test (LRT) for social transmission
#The @loglik slot contains the -log-likelihood- i.e. minus the log-likelihood
#So we can get the test statistic as double the difference in -log-likelihood as follows:
2*(Mod_N.RS_Aso@loglik-Mod_N.RS_So@loglik)
#[1]0.2663759

#There is 1 parameter in Mod_N.RS_So, and 0 in Mod_N.RS_Aso, so we have 1 d.f.
pchisq(2*(Mod_N.RS_Aso@loglik-Mod_N.RS_So@loglik),df=1,lower.tail=F)
#[1] 0.6057733
#p= 0.60577337; no evidence of an effect consistent with social transmission

#We can get 95% confidence intervals (C.I.s) for the parameter by first plotting the
#profile log-likelihood function. This is the -log-likelihood for a specified
#value of the parameter, when all other parameters in the model have been optimized.
#In this case there are no other parameters, so the profile log-likelihood is the
#same as the -log-likelihood.
#We specify which=1 because we are interested in the first parameter in the model
#as listed in the model output above. We specify the name of the model, and also the range
#we want to plot over, and resolution determines how many points will be plotted
plotProfLik(which = 1, model = Mod_N.RS_So, range = c(0,10), resolution = 20)

#Any values of s for which the profile log-likelihood is above the dotted line would be
#REJECTED in a likelihood ratio test (LRT), and therefore are OUTSIDE the 95% C.I.
#Therefore we can get the 95% C.I. by finding the crossing points
#Here we can see one crossing point between 0 and 2, but there seems to be no other crossing point.

#Before we move on to find these points, take a moment to note the asymmetry in the
#profile log-likelihood. We have quite a lot of certainty about the lower limit of
#s, but little certainty about the upper value.
#It is this uncertainty about the upper value that led to a high SE above, and thus
#the SE failed to quantify the strength of evidence against the null hypothesis, s=0.

#So we need to find the cross over points to get the 95% C.I. We use the profLikCI
#function, specifying the lowerRange to search in.

(p <- profLikCI(which = 1, model = Mod_N.RS_So, 
                lowerRange = c(0,2)))
# Lower CI Upper CI 
# 0.435197       NA  

#s=0 is not included in the 95% C.I. so there is at least reasonable evidence for social transmission or at least a statistical effect consistent with social transmission.
#Note we can obtain C.I.s for a different level of confidence by setting, e.g. conf=0.99 in the plotProfLik and profLikCI functions.

#We can get an estimate of %ST corresponding to the upper and lower limits of the
#95% C.I. as follows.
#Instead of specifying the model, we specify the parameter values and the name of the nbdaData object

nbdaPropSolveByST(par = p[1], nbdadata = nbdaData1)
# P(Network 1)  P(S offset) 
# 0.50051      0.00000 
nbdaPropSolveByST(par = p[2], nbdadata = nbdaData1)
#NA

#So between 50% and NA% of events are estimated to have occurred by social transmission.


# Mod_N.RD: Dynamic roost net, no ILVs ------------------------------------
#############################################################################F
# TUTORIAL 1.4
# USING A DYNAMIC NETWORK IN OADA
# 1 diffusion
# 1 dynamic network
#############################################################################F
#Imagine we believe the social network changed at various times during the diffusion
#We already loaded in several co-roosting networks above, one for each night.

length(unique(roost_mats_expanded)) # 4 nights of roosting networks, starting from the night before the carcass was placed
length(roost_mats_expanded) # these are each replicated as many times as there are sighting events on the day following them.
roost_mats_expanded <- map(roost_mats_expanded, as.matrix)

#We need to combine these in a 4 dimensional array, with the 4th dimension for time periods
(n_timeperiods <- length(roost_mats_expanded)) # 66 first detection events, and therefore 66 roost networks (many of which are repeats of each other)

#Create the empty array
N.RD <- array(NA, dim = c(n_indivs, n_indivs, 1, n_timeperiods))
#Slot in the network for each time period # XXX
for(i in 1:length(roost_mats_expanded)){
  N.RD[,,1,i] <- array(roost_mats_expanded[[i]], dim = c(n_indivs, n_indivs, 1))
}

# Now we need a vector specifying which time period corresponds to which acquisition event. I already created that above: days_vec.
#assMatrixIndex <- days_vec # XXX KG: actually, I think this is assuming I am using the un-expanded version of the roost matrices. But I already did the work of expanding them (oops), so maybe what I really need to do is just specify assMatrixIndex as 1:41.
assMatrixIndex <- 1:length(roost_mats_expanded)

#Now we enter the 4 dimensional network and assMatrixIndex as follows
nbdaData2 <- nbdaData(label = paste0("Diffusion_", whch),
                              assMatrix = N.RD, 
                              orderAcq = oa,
                              assMatrixIndex = assMatrixIndex)

Mod_N.RD_So <- oadaFit(nbdaData2)
nbdaModSum(Mod_N.RD_So)
# Variable        MLE         SE
# 1 1 Social transmission 1 0.02388205 0.05345923

# Compare to static network:
nbdaModSum(Mod_N.RS_So)
# Variable        MLE         SE
# 1 1 Social transmission 1 0.03955125 0.09005417

# Models with a dynamic network can be compared to static network models if they are fitted to the same order of acquisition
Mod_N.RD_So@aicc
#[1] 456.3319
Mod_N.RS_So@aicc 
#[1] 456.3181

exp(0.5*(Mod_N.RS_So@aicc-Mod_N.RD_So@aicc))
#Compare to the asocial model
Mod_N.RD_Aso <- oadaFit(nbdaData2, type = "asocial")
Mod_N.RD_Aso@aicc-Mod_N.RD_So@aicc
# -1.809965 # actually, looks like the asocial model is better here too.

#So the asocial model is favoured by 1.81 AICc units. This means:
exp(0.5*(Mod_N.RD_Aso@aicc-Mod_N.RD_So@aicc))
#[1] 0.404549
#the asocial model is 0.4x more likely to be the best K-L model, out of the two. (surprising!)

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

# Mod_N.RS_I.TC: Static roost net, 1 time-constant ILV ----------------------------------------------------------

############################################################################
# TUTORIAL 2.1
# ADDING TIME-CONSTANT ILVs TO AN OADA MODEL
# 1 diffusion
# 1 static network
# 1 Time-constant ILV
#############################################################################F

#Read in the social network and order of acquisition vector as shown in tutorial 1.
dim(N.RS) # we're still just using the array containing the static social network.
oa

#Get a CSV of the time-constant ILVs.
dim(my_ilvs)
all(my_ilvs$local_identifier == indivs) # should be TRUE to make sure they're in the same order

age_group <- cbind(my_ilvs$age_group) # age group (categorical)
age_group[age_group == "01_juv_sub"] <- 0
age_group[age_group == "02_adult"] <- 1
age_group <- cbind(as.numeric(age_group))

#Each ILV needs to be stored as a column matrix; this is to allow extension to time-varying ILVs (see below)

#Since age and distance are centered on 0, the baseline asocial rate is an individual of mean age and that starts the mean distance away from the carcass (if we use continuous age). Or if we use categorical age (which is what we're doing here, with just one age group variable), the baseline asocial rate is a juvenile that starts the mean distance away from the carcass.
#Therefore s is estimated relative to this baseline.

ilvs_to_use <- c("age_group")
#We create a vector of the names of the variables to be included in the analysis

#Now we create an nbdaData object as before, but specifying the ILVs to be included
#asoc_ilv indicates the ILVs assumed to affect asocial learning rate
#int_ilv indicates the ILVs assumed to affect social learning rate
#multi_ilv indicates the ILVs assumed to affect asocial and social learning rate the same amount (multiplicative model)

#Therefore, if we wanted to fit an "additive" model we would specify:
nbdaData3 <- nbdaData(label = paste0("Diffusion_", whch), 
                          assMatrix = N.RS,
                          orderAcq = oa,
                          asoc_ilv = ilvs_to_use)
#Then fit the model:
Mod_N.RS_addI.TC_So <- oadaFit(nbdaData3)
nbdaPropSolveByST(model = Mod_N.RS_addI.TC_So)
# P(Network 1)  P(S offset) 
# 0.18629      0.00000 

#And get the output:
nbdaModSum(Mod_N.RS_addI.TC_So)

# Variable        MLE        SE
# 1 1 Social transmission 1 0.09029961 0.1536384
# 2    2 Asocial: age_group 0.37745036 0.3569567

#If we wanted to fit a "multiplicative" model we would specify:
nbdaData4 <- nbdaData(label = paste0("Diffusion_", whch),
                            assMatrix = N.RS,
                            orderAcq = oa,
                            multi_ilv = ilvs_to_use)
Mod_N.RS_multiI.TC_So <- oadaFit(nbdaData4)
nbdaModSum(Mod_N.RS_multiI.TC_So)

# Variable        MLE        SE
# 1      1 Social transmission 1 0.08664349 0.1267771
# 2 2 Social= asocial: age_group 0.33195961 0.2644327
nbdaPropSolveByST(model = Mod_N.RS_multiI.TC_So)
# P(Network 1)  P(S offset) 
# 0.69927      0.00000

#Or the "unconstrained" model
nbdaData5 <- nbdaData(label = paste0("Diffusion_", whch),
                         assMatrix = N.RS,
                         orderAcq = oa,
                         asoc_ilv = ilvs_to_use,
                         int_ilv = ilvs_to_use)
Mod_N.RS_uncI.TC_So <- oadaFit(nbdaData5)
nbdaModSum(Mod_N.RS_uncI.TC_So)

# Variable        MLE        SE
# 1 1 Social transmission 1 0.05701736 0.1406007
# 2    2 Asocial: age_group 0.21463432 0.4796139
# 3     3 Social: age_group 0.79863628 1.9086581

#Note that one does not need to specify the same set of ILVs in asoc_ilv int_ilv and multi_ilv
#So a model can be fitted in which some variables affect only asocial learning, some only social learning, some affect both the same amount, and some affect social and asocial learning differently.

#In practise, there will be little reason to decide, a priori, which ILVs to put in which slot.
#Our preferred approach is to include in multi_ilv only ILVs for which there is a logical reason to believe it will affect asocial and social learning the same amount, put all other ILVs in both the asoc_ilv and int_ilv slots, and then perform multi-model inferencing (see Tutorial 7)

#For simplicity in the tutorial, we will take the approach of choosing the best of the 3 models based on AICc

Mod_N.RS_addI.TC_So@aicc # this one has the lowest aicc, so it's preferred. (Thank goodness, because I didn't want to deal with the multiplicative model)
Mod_N.RS_multiI.TC_So@aicc
Mod_N.RS_uncI.TC_So@aicc

#the multiplicative model is favored
nbdaModSum(Mod_N.RS_multiI.TC_So)

# Variable        MLE        SE
# 1      1 Social transmission 1 0.08664349 0.1267771
# 2 2 Social= asocial: age_group 0.33195961 0.2644327

# And we can compare with an asocial model containing the same ILVs:
Mod_N.RS_addI.TC_Aso <- oadaFit(nbdaData3, type = "asocial")
Mod_N.RS_addI.TC_So@aicc
Mod_N.RS_addI.TC_Aso@aicc # slightly smaller, so the asocial model fits a bit better than the social one.

exp(0.5*(Mod_N.RS_addI.TC_So@aicc-Mod_N.RS_addI.TC_Aso@aicc))
#[1] 2.052045

# Not going to find the social bounds because the asocial model is actually favored

# (XXX KG: this comment is a holdover from the tutorial; i don't know if it applies here or not) However, this may well be an artifact of the asocial baseline chosen- we can see that adults are estimated to be faster than juveniles at asocial learning (coefficient of about 9.6 for the MLE estimate for age_group), and juveniles/subadults are set as the baseline. This means s is being estimated relative to a very small baseline rate of asocial learning.
# We can reparameterise the model so that adults (of mean initial distance away from the carcass) are the baseline.]

age_group_rev <- 1-age_group
ilvs_to_use_2 <- c("age_group_rev")
#Now juveniles/subadults = 1 and adults = 0. Adults is the base level, and the coefficient will refer to the difference from adults

nbdaData3_rev <- nbdaData(label = paste0("Diffusion_", whch),
                          assMatrix = N.RS,
                          orderAcq = oa,
                          asoc_ilv = ilvs_to_use_2)
#Then fit the model:
Mod_N.RS_addI.TCrev_So <- oadaFit(nbdaData3_rev)
#And get the output:
nbdaModSum(Mod_N.RS_addI.TCrev_So)

# Variable         MLE         SE
# 1  1 Social transmission 1  0.06191003 0.09398279
# 2 2 Asocial: age_group_rev -0.37745000 0.35695659
# Now we get a negative coefficient for juveniles/subadults, and a much smaller s value that's easier to interpret.

# The AIC value may be different if one of the models just did a better job at finding the optimum (not in this case)
#Note that the two models specified are the same, just parameterized differently- so you may even see the same AICc here. # KG yep that's what happened here

#You may also think we have somehow magically changed the importance of social transmission, given the very different estimation of s! But this is not the case, s is merely estimated relative to a much different baseline rate of asocial learning. This can be seen by comparing %ST for the two models:

nbdaPropSolveByST(model = Mod_N.RS_addI.TC_So)
# P(Network 1)  P(S offset) 
# 0.18629      0.00000
nbdaPropSolveByST(model = Mod_N.RS_addI.TCrev_So)
# P(Network 1)  P(S offset) 
# 0.18629      0.00000 

#Now we also get a profile log-likelihood for s we can work with:
plotProfLik(which = 1, model = Mod_N.RS_addI.TCrev_So, range = c(0,10), resolution = 20)

#We can see the lower limit is between 0 and 2, and the upper is not findable

profLikCI(which = 1, model = Mod_N.RS_addI.TCrev_So, 
          lowerRange = c(0, 2))
# Lower CI  Upper CI 
# 0.4616716        NA 

#We can again get the %ST corresponding to the upper and lower points of the 95% CI. However, before we do so, we need to look at the constrainedNBDAdata function- so we will come back to this after we have looked at the estimated ILV effects and 95% C.I.s for those.

#What about confidence intervals for the coefficients for the ILVs?
#We could get Wald 95% C.I.s by taking MLE +/- 1.96x SE

data.frame(Variable = Mod_N.RS_addI.TCrev_So@varNames,
           MLE = Mod_N.RS_addI.TCrev_So@outputPar, SE = Mod_N.RS_addI.TCrev_So@se,
           WaldLower = Mod_N.RS_addI.TCrev_So@outputPar-1.96*Mod_N.RS_addI.TCrev_So@se,
           WaldUpper = Mod_N.RS_addI.TCrev_So@outputPar+1.96*Mod_N.RS_addI.TCrev_So@se)

# Variable         MLE         SE  WaldLower WaldUpper
# 1  1 Social transmission 1  0.06191003 0.09398279 -0.1222962 0.2461163
# 2 2 Asocial: age_group_rev -0.37745000 0.35695659 -1.0770849 0.3221849

#We have already seen why the Wald C.I.s are misleading for s (XXX KG: we have?)
#For the juvenile effect it looks highly suspect, suggesting that a very large difference in either direction is plausible
#The reason for this is that asymmetrical profile log-likelihoods can also arise for ILVs.
#But we can use the same approach to getting profile likelihood C.I.s for the other parameters in the model

#Since age_group is the second parameter in the model output, we set which = 2
plotProfLik(which = 2, model = Mod_N.RS_addI.TCrev_So, range = c(-500,5), resolution = 20)

#We can see a lot of asymmetry here, suggesting the Wald 95% C.I.s might not be good. Let us get the profile likelihood intervals. We can see that the upper limit is around 0 and the lower limit is nowhere to be found.

profLikCI(which = 2, model = Mod_N.RS_addI.TCrev_So,
          lowerRange = c(-1000000,1), upperRange = c(-10, 10))

# Error in nlminb(start = startValue, objective = oadaLikelihood, gradient = gradient_fn,  : 
#                   NA/NaN gradient evaluation

#Let us examine the back-transformed effect and C.I.
#The MLE for distance is Mod_N.RS_addI.TCrev_So@outputPar[2]
exp(Mod_N.RS_addI.TCrev_So@outputPar[2])
#[1] 0.6856075
#So the rate of asocial learning decreases [increases?] by an estimated factor of x0.685 for juveniles as opposed to adults, with back transformed 95% C.I. of
exp(Mod_N.RS_addI.TCrev_So@outputPar[2]-1.96*Mod_N.RS_addI.TCrev_So@se[2])
exp(Mod_N.RS_addI.TCrev_So@outputPar[2]+1.96*Mod_N.RS_addI.TCrev_So@se[2])
# 0.34 to 1.38 [does this mean no significant effect because in this case, 1 would be no change? or is even a decimal number an increase here?]

# XXX QQQ KG: this does not seem to be meaningful. What to do?

# XXX KG: need to figure out how to back-transform the CIs. Something isn't lining up when I try to do anything other than hard-coding them.

# AGE GROUP

#since age group (reversed) is the second parameter in the model output, we set which=2
plotProfLik(which = 2, model = Mod_N.RS_addI.TCrev_So, range = c(-2,2), resolution = 20)

profLikCI(which = 2, model = Mod_N.RS_addI.TCrev_So, lowerRange = c(-2,-1), upperRange = c(0,1))
# Lower CI   Upper CI 
# -1.4993108  0.2180992 
# contains 0

#So the 95% CI includes zero, and there is not huge evidence that juveniles/subadults are slower at asocial learning than adults. We back-transform the effect as follows:

exp(Mod_N.RS_addI.TCrev_So@outputPar[2])
# [1]  0.6856075

#This gives us the upper limit of the ratio (subadult or juvenile asocial learning rate)/(adult asocial learning rate) (XXX this answers my question above: if the ratio is 1, then that's no change. So our back-transformed CI before, which went from a number below 1 to a number above 1, would mean that the confidence interval includes 0, as confirmed by that CI that we just calculated.)
#You may find it easier to report by reversing the sign so we are reporting the faster/slower category:
exp(Mod_N.RS_addI.TCrev_So@outputPar[2]*-1) 
# [1] 1.458561

# Mod_N.RS_I.TC_I.TV: Static roost net, 1 time-constant ILV, 1 time-varying ILV ----------------------------------------------------
#############################################################################f
# TUTORIAL 2.2
# ADDING TIME-VARYING ILVs TO AN OADA MODEL
# 1 diffusion
# 1 static network
# 1 Time constant ILV (age group)
# 1 Time varying ILV (distance of roost from carcass)
###########################################################################

#Time-varying ILVs are easily added to an OADA
#It might take a bit of work to set up the nbdaData object, but once this is done the analysis proceeds as above.

#If one or more of our ILVs is time-varying, we need to set up an array for ALL of our ILVs specifying their values for every individual for every acquistion event.

#For example, let us assume that individuals' age groups don't vary over time, but their roosts' distance from the carcass does--we're going to incorporate the distance of each roost site to the carcass on each day.
length(ilvs_list) 

#Let us set up a matrix with rows = number of individuals (59), columns = number of acquisition events (41)
n_acq_events <- length(oa)
n_acq_events # 66
n_indivs # 70

roost_carc_distance <- matrix(NA, nrow = n_indivs, ncol = n_acq_events)
for(i in 1:length(indivs)){
  # each row is an individual; each column is an acquisition event
  ind <- indivs[[i]]
  roost_carc_distance[i,] <- map_dbl(ilvs_list, ~as.numeric(.x[.x$local_identifier == ind,3])) # 3 because the third column is the one that contains the distance from the roost to the carcass on that day.
}

dim(roost_carc_distance)
any(is.na(roost_carc_distance)) # we have some NA values
sum(is.na(roost_carc_distance))/length(roost_carc_distance) # a very small percentage of them are NA
# I'm going to address this by setting any NA values to the mean value of the non-NAs
mn_non_NAs <- mean(roost_carc_distance[!is.na(roost_carc_distance)])
roost_carc_distance[is.na(roost_carc_distance)] <- mn_non_NAs
any(is.na(roost_carc_distance)) # now we have no more

# I'm now realizing that we need to standardize these values to avoid throwing the model totally out of whack.
std_roost_carc_distance <-(roost_carc_distance-mean(roost_carc_distance))/sd(roost_carc_distance) # XXX not sure if I standardized this correctly, since the values are repeated, but we're going to try.
hist(std_roost_carc_distance) # this is a super weird distribution. I wonder if it's going to be a problem.
#Since std_roost_carc_distance is centered on 0, and juveniles/subadults = 0, adults=1, the baseline asocial rate is a juvenile/subadult of mean distance from the roost to the carcass
#Therefore s is estimated relative to this baseline

#let us also assume we want to include the age_group variable from above
age_group # 0 is juvenile/subadult and 1 is adult
#We have to input this as a time-varying ILV as well, even though it does not change
#However it is easy to create this using the byrow=F argument:
age_group_TV <- matrix(age_group, nrow = n_indivs, ncol = n_acq_events, byrow = F)
age_group_TV # each row is an individual (so, we'll have the same value all the way across) and each column is an acquisition event (each column will be identical to the previous column)
age_group_TV_rev <- +(!age_group_TV) # weird trick I learned to reverse the 0s and 1s in a matrix. Making this vector in case I need to use it reversed.

ilvs_to_use_tv <- c("std_roost_carc_distance","age_group_TV")
ilvs_to_use_tv_2 <- c("std_roost_carc_distance", "age_group_TV_rev")

#We can then create the nbdaData object for the unconstrained model, specifying asocialTreatment="timevarying"

nbdaData6 <- nbdaData(label = paste0("Diffusion_", whch),
                            assMatrix = N.RS, # single static network
                            orderAcq = oa,  # single order of acquisition
                            asoc_ilv = ilvs_to_use_tv, # two time-varying 
                            asocialTreatment = "timevarying")

nbdaData6_rev <- nbdaData(label = paste0("Diffusion_", whch),
                            assMatrix = N.RS, # single static network
                            orderAcq = oa,  # single order of acquisition
                            asoc_ilv = ilvs_to_use_tv_2, # two time-varying 
                            asocialTreatment = "timevarying")
#Fit the model
Mod_N.RS_addI.TC_I.TV_So <- oadaFit(nbdaData6)
Mod_N.RS_addI.TCrev_I.TV_So <- oadaFit(nbdaData6_rev) 
# Warning message:
#   In nlminb(start = startValue, objective = oadaLikelihood, gradient = gradient_fn,  :
#               NA/NaN function evaluation # XXX how do I debug this? # XXX update: this warning went away after I standardized the roost_carc_distance variable, so maybe it was due to very large values of that.

#Display the output
nbdaModSum(Mod_N.RS_addI.TC_I.TV_So)

# Variable         MLE        SE
# 1            1 Social transmission 1  0.01758634 0.1182444
# 2 2 Asocial: std_roost_carc_distance -0.12576058 0.1576255
# 3            3 Asocial: age_group_TV  0.34646346 0.3052808
# Oh interesting, one thing that we see here is that the SE is actually larger than the estimate for social transmission. Maybe that's why I can't get a confidence interval properly?

nbdaModSum(Mod_N.RS_addI.TCrev_I.TV_So)

#                             Variable        MLE        SE
# 1            1 Social transmission 1  0.2292159 0.2199268
# 2 2 Asocial: std_roost_carc_distance -1.4427892 0.4808184
# 3        3 Asocial: age_group_TV_rev -1.1657155 0.8254652
# Reversing them doesn't change the values for std_roost_carc_distance (good, as expected). Does change the direction of the coefficient for age_group_TV_rev (as expected) and also changes the values for social transmission because it's now being calculated relative to a different baseline level.

# (The tutorial stops here, but let me see if I can interpret the results of these models by myself by using the code demonstrated above.)

nbdaPropSolveByST(model = Mod_N.RS_addI.TC_I.TV_So)
# P(Network 1)  P(S offset) 
# 0.04614      0.00000   # interesting! the probability of solving by social transmission has gone way down with the inclusion of the time-varying ILV. I wonder why.

nbdaPropSolveByST(model = Mod_N.RS_addI.TCrev_I.TV_So)
# P(Network 1)  P(S offset) 
# 0.04614      0.00000  # as expected, this is exactly the same.

#Fit the asocial model for comparison:
Mod_N.RS_addI.TCrev_I.TV_Aso <- oadaFit(nbdaData6_rev, type = "asocial")

#And then compare the social and asocial models using 

Mod_N.RS_addI.TCrev_I.TV_So@aicc
Mod_N.RS_addI.TCrev_I.TV_Aso@aicc
Mod_N.RS_addI.TCrev_I.TV_Aso@aicc-Mod_N.RS_addI.TCrev_I.TV_So@aicc # asocial is slightly lower than social, which means asocial is favored.

#So the asocial model is favoured by 2.170678 AICc units. This means:
exp(0.5*(Mod_N.RS_addI.TCrev_I.TV_So@aicc-Mod_N.RS_addI.TCrev_I.TV_Aso@aicc))
#[1] 2.960443
#the asocial model is 2.960443x more likely to be the best K-L model, out of the two. 
#Or we can say the asocial model has 2.960443x more support than the social model.

#Let's get a 95% confidence interval for the social transmission parameter
plotProfLik(which = 1, model = Mod_N.RS_addI.TC_I.TV_So, range = c(0,10), resolution = 20) # oh this is a super weird shape! I don't think it will be any different with the reversed age group variable, but let's see.
plotProfLik(which = 1, model = Mod_N.RS_addI.TCrev_I.TV_So, range = c(0,5), resolution = 20) # oho! this is actually different and it gives us parameters we can work with. Let's continue forward with the reversed model.

(p <- profLikCI(which = 1, model = Mod_N.RS_addI.TCrev_I.TV_So, lowerRange = c(0,1)))
# XXX note: I initially got a very mysterious error when I provided ever so slightly wider ranges for the lower and upper ranges. I had a long discussion with DeepSeek about it, verified that there was nothing wrong with my model object or my data (e.g. no NAs etc.) and narrowed it down to there being something wrong with the profLikCI function itself. Turns out that I just needed to zoom the plot in even farther and narrow the starting ranges.
# Lower CI  Upper CI 
# 0.4532501        NA 


#We can get an estimate of %ST corresponding to the upper and lower limits of the
#95% C.I. as follows.
#Instead of specifying the model, we specify the parameter values and the name of the nbdaData object

nbdaPropSolveByST(par = c(p[1], Mod_N.RS_addI.TCrev_I.TV_So@outputPar[2], Mod_N.RS_addI.TCrev_I.TV_So@outputPar[3]), nbdadata = nbdaData6_rev) # have to pass in the vector of values instead of just one.
# P(Network 1)  P(S offset) 
# 0.53635      0.00000 

nbdaPropSolveByST(par = c(p[2], Mod_N.RS_addI.TCrev_I.TV_So@outputPar[2], Mod_N.RS_addI.TCrev_I.TV_So@outputPar[3]), nbdadata = nbdaData6_rev)
# P(Network 1)  P(S offset) 
# NA           NA 

nbdaPropSolveByST(model = Mod_N.RS_addI.TCrev_I.TV_So)
# P(Network 1)  P(S offset) 
# 0.04614      0.00000  # I don't understand why I keep getting a confidence interval that does not contain the actual estimate. How can our estimate be 4.6% while the lower bound is 53%?

# So 4.6% of events are expected to have occurred due to social transmission, with a 95% confidence lower bound of 53%. (doesn't make sense)

# Now let's look at confidence intervals for the other parameters.
# Parameter 2 is distance between roost and carcass. The estimate was -1.4427889, which makes sense--greater distance would lead to a lower likelihood of learning. I'm encouraged that at least the direction is reasonable.
plotProfLik(which = 2, model = Mod_N.RS_addI.TCrev_I.TV_So, range = c(-10,20), resolution = 20) # doesn't cross the x axis at all. What does that mean? Also don't know what the red dots mean.
# can't get a confidence interval because of this
# oh, the console output has a column called "converged" and I see that the value for 16.8 is "No", which corresponds to where the red dot is. So I guess that the red dots appear when the model didn't converge at those values.

#Nonetheless, let us examine the back-transformed effect and C.I.
exp(Mod_N.RS_addI.TCrev_I.TV_So@outputPar[2])
#[1] 0.881826
#So the rate of asocial learning decreases by an estimated factor of x0.88 for an increase of 1 S.D. in distance from the carcass, with back transformed 95% C.I. of
exp(p[1])
exp(p[2])
#1.57x - NAx (this doesn't mean much...)

#What if we preferred to interpret effect sizes per meter, rather than per SD? We simply divide the coefficient by the SD for distance (original variable, not the standardized version)

exp(Mod_N.RS_addI.TCrev_I.TV_So@outputPar[2]/sd(roost_carc_distance))
#[1] 0.9999
#So the rate of asocial learning decreases by an estimated factor of x0.99 for an increase of 1 meter of age (XXX QQQ this doesn't really make sense--no way that 1 meter has that much of an effect, and anyway, shouldn't it be much smaller? I'm also not sure how this works with the dynamic variables.)

# Mod_N.RD_I.TC_I.TV: Dynamic roost net, 1 time-constant ILV, 1 time-varying ILV ----------------------------------------------------
# We already created a dynamic network at the very beginning: the roost network changes over the course of days. N.RD was created from roost_mats_expanded, which has the roost networks replicated for the corresponding diffusion events that occurred on the days they pertain to.
dim(N.RD)

#Now we need to create a vector specifying which time period corresponds to which acquisition event
assMatrixIndex # 1:41 because we already expanded the networks. If we had not already expanded them, this is where we could specify which events corresponded to which networks.

nbdaData7_rev <- nbdaData(label = paste0("Diffusion_", whch),
                      assMatrix = N.RD, # dynamic roost networks
                      orderAcq = oa,  # same order of acquisition as before
                      asoc_ilv = ilvs_to_use_tv_2, # two time-varying ILVs
                      asocialTreatment = "timevarying",
                      assMatrixIndex = assMatrixIndex)
Mod_N.RD_addI.TCrev_I.TV_So <- oadaFit(nbdaData7_rev)


nbdaModSum(Mod_N.RD_addI.TCrev_I.TV_So)
#                             Variable        MLE        SE
# 1            1 Social transmission 1  0.3586783 0.2351649
# 2 2 Asocial: std_roost_carc_distance -1.1211520 0.3917824
# 3        3 Asocial: age_group_TV_rev -1.2081944 0.6588063

#Models with a dynamic network can be compared to static network models if they are fitted to the same order of acquisition
Mod_N.RD_addI.TCrev_I.TV_So@aicc
# [1] 253.6264 # this one is slightly lower, so the dynamic networks fit ever so slightly better (yay! that's what we wanted!)
Mod_N.RS_addI.TCrev_I.TV_So@aicc
# [1] 257.2567
exp(0.5*(Mod_N.RS_addI.TCrev_I.TV_So@aicc-Mod_N.RD_addI.TCrev_I.TV_So@aicc))
#1.02x more support for the dynamic model


# Now time to analyze the results of this model
nbdaPropSolveByST(model = Mod_N.RD_addI.TCrev_I.TV_So)
# P(Network 1)  P(S offset) 
# 0.04158      0.00000

# Compare to the one with the static network:
nbdaPropSolveByST(model = Mod_N.RS_addI.TCrev_I.TV_So)
# P(Network 1)  P(S offset) 
# 0.04614      0.00000 

#Fit the asocial model for comparison:
Mod_N.RD_addI.TCrev_I.TV_Aso <- oadaFit(nbdaData7_rev, type = "asocial")

#And then compare the social and asocial models using 
Mod_N.RD_addI.TCrev_I.TV_So@aicc
Mod_N.RD_addI.TCrev_I.TV_Aso@aicc # lower--once again, the asocial model is a better fit.
Mod_N.RD_addI.TCrev_I.TV_Aso@aicc-Mod_N.RD_addI.TCrev_I.TV_So@aicc # asocial is slightly lower than social, which means it's better
exp(0.5*(Mod_N.RD_addI.TCrev_I.TV_Aso@aicc-Mod_N.RD_addI.TCrev_I.TV_So@aicc))
# 0.34x more support for the asocial model vs. social


# 95% confidence interval for the social transmission parameter:
plotProfLik(which = 1, model = Mod_N.RD_addI.TCrev_I.TV_So, range = c(0,20), resolution = 20) # once again, we don't have an upper bound

(p <- profLikCI(which = 1, model = Mod_N.RD_addI.TCrev_I.TV_So, lowerRange = c(0,1)))
# Lower CI  Upper CI 
# 0.1564609        NA 

#We can get an estimate of %ST corresponding to the upper and lower limits of the
#95% C.I. as follows.
#Instead of specifying the model, we specify the parameter values and the name of the nbdaData object

nbdaPropSolveByST(par = c(p[1], Mod_N.RD_addI.TCrev_I.TV_So@outputPar[2], Mod_N.RD_addI.TCrev_I.TV_So@outputPar[3]), nbdadata = nbdaData7_rev) # have to pass in the vector of values instead of just one.
# P(Network 1)  P(S offset)  # once again, we're getting a "lower" estimate that's higher than the estimate from the model alone. What's going on?
# 0.31615      0.00000

nbdaPropSolveByST(par = c(p[2], Mod_N.RD_addI.TCrev_I.TV_So@outputPar[2], Mod_N.RD_addI.TCrev_I.TV_So@outputPar[3]), nbdadata = nbdaData7_rev)
# P(Network 1)  P(S offset) 
# NA           NA 

nbdaPropSolveByST(model = Mod_N.RD_addI.TCrev_I.TV_So)
# P(Network 1)  P(S offset) 
# 0.04158      0.00000 

# So 4% of events are expected to have occurred due to social transmission, with a 95% lower confidence bound of 31.6%. 

# Mod_N.RD_N.FD_I.TC_I.TV: Dynamic roost and flight networks, 1 time-constant and 1 time-varying ILV --------
# Time to introduce the co-flight networks. Going to go straight into using them as a dynamic network.
length(fl_mats) # should be same as length of roost_mats_expanded
fl_mats <- map(fl_mats, as.matrix) # had to do this before with roost_mats_expanded
length(roost_mats_expanded)
length(oa)

#In a multi-network NBDA, we need to combine our networks into an array
#If we have dynamic (time-varying) networks we need to create a four dimensional array of size no. individuals x no.individuals x no.networks x number of time periods and provide an assMatrixIndex vector as shown in Tutorial 1.
n_timeperiods

#Create the empty array
N.RD_N.FD <- array(NA, dim = c(n_indivs, n_indivs, 2, n_timeperiods))
#Slot in the network for each time period # XXX
for(i in 1:length(roost_mats_expanded)){
  N.RD_N.FD[,,1,i] <- array(roost_mats_expanded[[i]], dim = c(n_indivs, n_indivs, 1))
  N.RD_N.FD[,,2,i] <- array(fl_mats[[i]], dim = c(n_indivs, n_indivs, 1))
}

assMatrixIndex # already expanded the networks

nbdaData8_rev <- nbdaData(label = paste0("Diffusion_", whch),
                          assMatrix = N.RD_N.FD, # dynamic roost networks and dynamic flight networks
                          orderAcq = oa,  
                          asoc_ilv = ilvs_to_use_tv_2, # two time-varying ILVs
                          asocialTreatment = "timevarying",
                          assMatrixIndex = assMatrixIndex)
Mod_N.RD_N.FD_addI.TCrev_I.TV_So <- oadaFit(nbdaData8_rev)

nbdaModSum(Mod_N.RD_N.FD_addI.TCrev_I.TV_So)
# Variable         MLE         SE
# 1            1 Social transmission 1  0.00371263 0.04661483
# 2            2 Social transmission 2  0.01931091 0.02985088
# 3 3 Asocial: std_roost_carc_distance -0.11315891 0.15108500
# 4        4 Asocial: age_group_TV_rev -0.36540841 0.29876618

#You will notice that we have a second s parameter in the model. The s parameters correspond to the order the networks
#are entered into the socNets array. So the first s parameter,s1, labelled "1 Social transmission 1"  corresponds to 
#the network in socNets[,,1] and the second s parameter,s2, labelled "2 Social transmission 2" to the network in 
#socNets[,,2]. # KG: in our case, 1 = co-roosting and 2 = co-flight

Mod_N.RD_N.FD_addI.TCrev_I.TV_So@outputPar

#Inference regarding the ILVs can proceed as it did in Tutorial 2, so here we will focus on inference about the s parameters.

#It looks like we have a very small amount of evidence for social transmission on each of the networks; perhaps slightly more on the flight network?
#Let us fit some constrained models to test some null hypotheses.

#First let us test the hypothesis s1=0.
#We first create the nbdaData object with the constraint s1=0:
N.RD_zero <- constrainedNBDAdata(nbdaData8_rev, constraintsVect = c(0,1,2,3)) # KG: I had to guess at how long this needed to be and I'm still not sure how to get that info
#Here the first parameter, s1, is constrained to be 0 and all other parameters are unconstrained
#Then fit the model:
Mod_N.RDzero_N.FD_addI.TCrev_I.TV_So <- oadaFit(N.RD_zero)

#We can then compare AICcs
Mod_N.RD_N.FD_addI.TCrev_I.TV_So@aicc
#[1] 460.2391
Mod_N.RDzero_N.FD_addI.TCrev_I.TV_So@aicc
#[1] 457.977
#The model with s1>0 is not favored over the model where it is constrained to 0. By how much is the zero model favored?
exp(0.5*(Mod_N.RDzero_N.FD_addI.TCrev_I.TV_So@aicc-Mod_N.RD_N.FD_addI.TCrev_I.TV_So@aicc))
#[1] 0.3226998
#0.3226998x more support for a model in which there is no social transmission following network 1 (vs. one where there is allowed to be social transmission following network 1)

#We can also conduct a likelihood ratio test (LRT)
#Test statistic
teststat <- 2*(Mod_N.RDzero_N.FD_addI.TCrev_I.TV_So@loglik-Mod_N.RD_N.FD_addI.TCrev_I.TV_So@loglik)
#The difference in number of parameters is 1, so df=1
pchisq(teststat,df=1,lower.tail = F)
#[1] 0.9353718
#No evidence for social transmission following network 1 (co-roosting network) # consistent with our findings before that the social model did not outcompete the asocial model

#Now we can do the same for the hypothesis s2=0
N.FD_zero <- constrainedNBDAdata(nbdaData8_rev, constraintsVect = c(1,0,2,3))
Mod_N.RD_N.FDzero_addI.TCrev_I.TV_So <- oadaFit(N.FD_zero)
Mod_N.RD_N.FD_addI.TCrev_I.TV_So@aicc
#[1] 460.2391
Mod_N.RD_N.FDzero_addI.TCrev_I.TV_So@aicc
#[1] 458.5986
#The model with s2=0 is favored. By how much?
exp(0.5*(Mod_N.RD_N.FD_addI.TCrev_I.TV_So@aicc-Mod_N.RD_N.FDzero_addI.TCrev_I.TV_So@aicc))
#[1] 2.271122x more support for the model where social transmission is 0

#Now the LRT
#Test statistic
teststat <- 2*(Mod_N.RD_N.FDzero_addI.TCrev_I.TV_So@loglik-Mod_N.RD_N.FD_addI.TCrev_I.TV_So@loglik)
#The difference in number of parameters is 1, so df=1
pchisq(teststat,df=1,lower.tail = F)
#[1] 0.4280557
#No evidence for social transmission following network 2 (co-flight network)


#So overall we have:
#1. no evidence for social transmission following network 1 (roosting). 
#2. no evidence for social transmission following network 2 (flight).

#In general terms: it is often tempting, when we find strong evidence of effect A, and no evidence of effect B to conclude that we have strong evidence that effect A > effect B.
#But this is a logical error--remember that "no evidence of an effect" does not equate to "strong evidence of no effect".

#We would strongly advise users of NBDA to present confidence intervals (C.I.s) for effect sizes. These can be obtained for s1 and s2 in the same manner as in Tutorial 2:

#for s1, which=1
plotProfLik(which=1,model=Mod_N.RD_N.FD_addI.TCrev_I.TV_So, range=c(0,3),resolution=20)
#lower limit between 0 and 0.5
# no obvious upper limit
# not going to bother calculating the conf ints here because it's going to be the same deal as before

#for s2, which=2
plotProfLik(which=2,model=Mod_N.RD_N.FD_addI.TCrev_I.TV_So, range=c(0,10),resolution=20)
# can't find an upper limit

nbdaPropSolveByST(model=Mod_N.RD_N.FD_addI.TCrev_I.TV_So)
# P(Network 1) P(Network 2)  P(S offset) 
# 0.44577      0.00000      0.00000 

#If we want to get %ST corresponding to the upper and lower limits of C.I.s we can do so using the same procedure as in previous tutorials:
