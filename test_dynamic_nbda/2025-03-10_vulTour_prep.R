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
# XXX START HERE---- below this line the annotations still match the previous carcass, not this one.
nbdaPropSolveByST(par = p[2], nbdadata = nbdaData2)
# P(Network 1)  P(S offset) 
# 0.70119      0.00000 

# Mod_N.RS_I.TC: Static roost net, 1 time-constant ILV ----------------------------------------------------------

#############################################################################F
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
# 0.72641      0.00000

#And get the output:
nbdaModSum(Mod_N.RS_addI.TC_So)

#                  Variable          MLE          SE
# 1 1 Social transmission 1 12703.992827 530926.3792
# 2    2 Asocial: age_group     9.638279     41.7922

#If we wanted to fit a "multiplicative" model we would specify:
nbdaData4 <- nbdaData(label = paste0("Diffusion_", whch),
                            assMatrix = N.RS,
                            orderAcq = oa,
                            multi_ilv = ilvs_to_use)
Mod_N.RS_multiI.TC_So <- oadaFit(nbdaData4)
nbdaModSum(Mod_N.RS_multiI.TC_So)

#                       Variable      MLE        SE
# 1      1 Social transmission 1 1.476400 0.9432359
# 2 2 Social= asocial: age_group 0.599431 0.3233459
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

#                 Variable          MLE        SE
# 1 1 Social transmission 1 1.071600e+04       NaN
# 2    2 Asocial: age_group 9.460513e+00       NaN
# 3     3 Social: age_group 4.375611e-02 0.4521125

#Note that one does not need to specify the same set of ILVs in asoc_ilv int_ilv and multi_ilv
#So a model can be fitted in which some variables affect only asocial learning, some only social learning, some affect both the same amount, and some affect social and asocial learning differently.

#In practise, there will be little reason to decide, a priori, which ILVs to put in which slot.
#Our preferred approach is to include in multi_ilv only ILVs for which there is a logical reason to believe it will affect asocial and social learning the same amount, put all other ILVs in both the asoc_ilv and int_ilv slots, and then perform multi-model inferencing (see Tutorial 7)

#For simplicity in the tutorial, we will take the approach of choosing the best of the 3 models based on AICc

Mod_N.RS_addI.TC_So@aicc # this one has the lowest aicc, so it's preferred. (Thank goodness, because I didn't want to deal with the multiplicative model)
Mod_N.RS_multiI.TC_So@aicc
Mod_N.RS_uncI.TC_So@aicc

#We see the additive model is favored. 
nbdaModSum(Mod_N.RS_addI.TC_So)

#                  Variable          MLE          SE
# 1 1 Social transmission 1 12703.992827 530926.3792
# 2    2 Asocial: age_group     9.638279     41.7922

# And we can compare with an asocial model containing the same ILVs:
Mod_N.RS_addI.TC_Aso <- oadaFit(nbdaData3, type = "asocial")
Mod_N.RS_addI.TC_So@aicc
# [1] 267.4296
Mod_N.RS_addI.TC_Aso@aicc
# [1] 296.0008 # slightly larger, so the social model fits a bit better than the asocial one.

exp(0.5*(Mod_N.RS_addI.TC_Aso@aicc-Mod_N.RS_addI.TC_So@aicc))
#[1] 1600142

#From looking at the MLEs for the parameters we can see that s is estimated to be very large.
#Indeed if we look at the profile log-likelihood plot for s:
plotProfLik(which=1,model=Mod_N.RS_addI.TC_So,range=c(0,50),resolution=20)
plotProfLik(which=1,model=Mod_N.RS_addI.TC_So,range=c(0,200),resolution=20)
plotProfLik(which=1,model=Mod_N.RS_addI.TC_So,range=c(0,1000),resolution=20)
#We can see it appears to level out as s tends to infinity

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

# Variable                          MLE           SE
# 1  1 Social transmission 1   0.8282869    0.4635687
# 2 2 Asocial: age_group_rev -18.1289926 7452.1016535
# Now we get a large negative coefficient for juveniles/subadults, and a much smaller s value that's easier to interpret.

# We can also see that the AICc is fractionally better for model 3, showing that the optimum has been found more precisely. (KG: in their tutorial there was more of a difference. Here it doesn't seem to matter at all.)
Mod_N.RS_addI.TC_So@aicc
Mod_N.RS_addI.TCrev_So@aicc # KG oh wow, this is barely different at all but it is sliiiiightly better. 

#Note that the two models specified are the same, just parameterized differently- so you may even see the same AICc here. # KG yep that's what happened here

#You may also think we have somehow magically changed the importance of social transmission, given the very different estimation of s! But this is not the case, s is merely estimated relative to a much different baseline rate of asocial learning. This can be seen by comparing %ST for the two models:

nbdaPropSolveByST(model = Mod_N.RS_addI.TC_So)
# P(Network 1)  P(S offset) 
# 0.72641      0.00000
nbdaPropSolveByST(model = Mod_N.RS_addI.TCrev_So)
# P(Network 1)  P(S offset) 
# 0.72644      0.00000 
#About the same in each case (and the minor discrepancy is just due to Mod_N.RS_addI.TC_So not quite finding the optimum)

#Now we also get a profile log-likelihood for s we can work with:
plotProfLik(which = 1, model = Mod_N.RS_addI.TCrev_So, range = c(0,10), resolution = 20)

#We can see the lower limit is between 0 and 2, and the upper is between 2 and 4.

profLikCI(which = 1, model = Mod_N.RS_addI.TCrev_So, 
          upperRange = c(2, 4),
          lowerRange = c(0, 2))
# Lower CI Upper CI 
# 1.999959 2.910370

#We can again get the %ST corresponding to the upper and lower points of the 95% CI. However, before we do so, we need to look at the constrainedNBDAdata function- so we will come back to this after we have looked at the estimated ILV effects and 95% C.I.s for those.

#What about confidence intervals for the coefficients for the ILVs?
#We could get Wald 95% C.I.s by taking MLE +/- 1.96x SE

data.frame(Variable = Mod_N.RS_addI.TCrev_So@varNames,
           MLE = Mod_N.RS_addI.TCrev_So@outputPar, SE = Mod_N.RS_addI.TCrev_So@se,
           WaldLower = Mod_N.RS_addI.TCrev_So@outputPar-1.96*Mod_N.RS_addI.TCrev_So@se,
           WaldUpper = Mod_N.RS_addI.TCrev_So@outputPar+1.96*Mod_N.RS_addI.TCrev_So@se)

#                   Variable         MLE           SE     WaldLower    WaldUpper
# 1  1 Social transmission 1   0.8282869    0.4635687 -8.030783e-02     1.736882
# 2 2 Asocial: age_group_rev -18.1289926 7452.1016535 -1.462425e+04 14587.990248

#We have already seen why the Wald C.I.s are misleading for s (XXX KG: we have?)
#For the juvenile effect it looks highly suspect, suggesting that a very large difference in either direction is plausible
#The reason for this is that asymmetrical profile log-likelihoods can also arise for ILVs.
#But we can use the same approach to getting profile likelihood C.I.s for the other parameters in the model

#Since age_group is the second parameter in the model output, we set which = 2
plotProfLik(which = 2, model = Mod_N.RS_addI.TCrev_So, range = c(-500,5), resolution = 20)

#We can see a lot of asymmetry here, suggesting the Wald 95% C.I.s might not be good. Let us get the profile likelihood intervals. We can see that the upper limit is around 0 and the lower limit is nowhere to be found.

profLikCI(which = 2, model = Mod_N.RS_addI.TCrev_So,
          lowerRange = c(-1000000,1), upperRange = c(-10, 10))

# Lower CI     Upper CI 
# -450.2623133   -0.5297181
# XXX KG: this came with a lot of warnings, and frankly I find it suspect since I can't even see the lower limit anywhere. It is certainly not around 450.
# XXX QQQ: One model produces reasonable estimates for s and unreasonable estimates for age_group, and the other is reversed. Can I make them both and look at the CIs for each variable from the model that more easily shows it, or does that not work?

#So this asymmetry has made a big difference when compared to the Wald intervals. (KG: the wald interval for this one was (-14624.25, 14587.990248), which is quite different, especially on the upper end!)

#The first thing to note is that 0 is not within the 95% C.I. so there is some evidence that age affects asocial learning rate.

#Let us examine the back-transformed effect and C.I.
#The MLE for distance is Mod_N.RS_addI.TCrev_So@outputPar[2]
exp(Mod_N.RS_addI.TCrev_So@outputPar[2])
#[1] 1.338685e-08
#So the rate of asocial learning increases by an estimated factor of x1.34*10^-8 for juveniles as opposed to adults, with back transformed 95% C.I. of
exp(Mod_N.RS_addI.TCrev_So@outputPar[2]-1.96*Mod_N.RS_addI.TCrev_So@se[2])
exp(Mod_N.RS_addI.TCrev_So@outputPar[2]+1.96*Mod_N.RS_addI.TCrev_So@se[2])
#0 - Inf

# XXX QQQ KG: this does not seem to be meaningful. What to do?

#(KG: no longer relevant for this categorical varabiable!) What if we prefered to interpret effect sizes per km away from the carcass, rather than per SD? We simply divide the coefficient by the SD for distance (original variable, not the standardized version)

# XXX KG: need to figure out how to back-transform the CIs. Something isn't lining up when I try to do anything other than hard-coding them.

# AGE GROUP

#since age group (reversed) is the second parameter in the model output, we set which=2
plotProfLik(which = 2, model = Mod_N.RS_addI.TCrev_So, range = c(-20,5), resolution = 20)

#We can see the profile log-likelihood is indeed highly asymmetrical here too, explaining the large SE.
#We can also see there is something a bit odd going on at the left side of the plot, but first let us zoom in and find the upper limit

plotProfLik(which = 2, model = Mod_N.RS_addI.TCrev_So, range = c(-2,2), resolution = 20)
#We can see the upper limit is somewhere around 0, between -2 and 2. What about the lower (most negative) limit?

plotProfLik(which = 2, model = Mod_N.RS_addI.TCrev_So, range = c(-30,-10), resolution = 20) # whoa, what the heck is this?

# (XXX there's a long discussion in the tutorial of an invalid thing that doesn't actually happen in my data. Can return to this if needed.)

profLikCI(which = 2, model = Mod_N.RS_addI.TCrev_So, upperRange = c(-2,2))
# Lower CI   Upper CI 
# NA -0.5296837  

#So the 95% CI includes zero, and there is not huge evidence that juveniles/subadults are slower at asocial learning than adults. We back-transform the effect as follows:

exp(Mod_N.RS_addI.TCrev_So@outputPar[2])
# [1] 1.338685e-08

#This gives us the upper limit of the ratio (subadult or juvenile asocial learning rate)/(adult asocial learning rate)
#You may find it easier to report by reversing the sign so we are reporting the faster/slower category:
exp(Mod_N.RS_addI.TCrev_So@outputPar[2]*-1) # XXX KG I have no idea what's going on here
# [1] 74700147

# Mod_N.RS_I.TC_I.TV: Static roost net, 1 time-constant ILV, 1 time-varying ILV ----------------------------------------------------
#############################################################################f
# TUTORIAL 2.2
# ADDING TIME-VARYING ILVs TO AN OADA MODEL
# 1 diffusion
# 1 static network
# 1 Time constant ILV (age group)
# 1 Time varying ILV (distance of roost from carcass)
#############################################################################f

#Time-varying ILVs are easily added to an OADA
#It might take a bit of work to set up the nbdaData object, but once this is done the analysis proceeds as above.

#If one or more of our ILVs is time-varying, we need to set up an array for ALL of our ILVs specifying their values for every individual for every acquistion event.

#For example, let us assume that individuals' age groups don't vary over time, but their roosts' distance from the carcass does--we're going to incorporate the distance of each roost site to the carcass on each day.
length(ilvs_list) 

#Let us set up a matrix with rows = number of individuals (59), columns = number of acquisition events (41)
n_acq_events <- length(oa)
n_acq_events # 41
n_indivs # 59

roost_carc_distance <- matrix(NA, nrow = n_indivs, ncol = n_acq_events)
for(i in 1:length(indivs)){
  # each row is an individual; each column is an acquisition event
  ind <- indivs[[i]]
  roost_carc_distance[i,] <- map_dbl(ilvs_list, ~as.numeric(.x[.x$local_identifier == ind,3])) # 3 because the third column is the one that contains the distance from the roost to the carcass on that day.
}

dim(roost_carc_distance)
# I'm now realizing that we need to standardize these values to avoid throwing the model totally out of whack.
std_roost_carc_distance <-(roost_carc_distance-mean(roost_carc_distance))/sd(roost_carc_distance) # XXX not sure if I standardized this correctly, since the values are repeated, but we're going to try.
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

#                             Variable        MLE        SE
# 1            1 Social transmission 1  0.7353657 1.1351871
# 2 2 Asocial: std_roost_carc_distance -1.4427889 0.4808177
# 3            3 Asocial: age_group_TV  1.1657085 0.8254575
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
# 0.37249      0.00000  # interesting! the probability of solving by social transmission has gone way down with the inclusion of the time-varying ILV. I wonder why.

nbdaPropSolveByST(model = Mod_N.RS_addI.TCrev_I.TV_So)
# P(Network 1)  P(S offset) 
# 0.37249      0.00000  # as expected, this is exactly the same.

#Fit the asocial model for comparison:
Mod_N.RS_addI.TCrev_I.TV_Aso <- oadaFit(nbdaData6_rev, type = "asocial")

#And then compare the social and asocial models using 

Mod_N.RS_addI.TCrev_I.TV_So@aicc
Mod_N.RS_addI.TCrev_I.TV_Aso@aicc
Mod_N.RS_addI.TCrev_I.TV_Aso@aicc-Mod_N.RS_addI.TCrev_I.TV_So@aicc # asocial is slightly higher than social, which means social is favored.

#So the social model is favoured by 2.232307 AICc units. This means:
exp(0.5*(Mod_N.RS_addI.TCrev_I.TV_Aso@aicc-Mod_N.RS_addI.TCrev_I.TV_So@aicc))
#[1] 3.053088
#the social model is 3.053088x more likely to be the best K-L model, out of the two. (okay cool, so even though way fewer of the events are estimated to have occurred by social transmission, it's still a lot.)
#Or we can say the social model has 3.053088x more support than the asocial model.

#There are 3 parameters in Mod_N.RS_addI.TC_I.TV_So, and 2 in Mod_N.RS_addI.TC_I.TV_Aso, so we have 1 d.f. (XXX KG: I think?)
pchisq(2*(Mod_N.RS_addI.TC_I.TV_Aso@loglik-Mod_N.RS_addI.TC_I.TV_So@loglik),df=1,lower.tail=F)
#[1] 0.03262851
#p= 0.03262851; some evidence of an effect consistent with social transmission (KG note: if I'm wrong about the DF and it is in fact 2 or 3, then this effect would not be statistically significant; the p value would go up considerably.) # XXX I don't understand this part entirely.

#Let's get a 95% confidence interval for the social transmission parameter
plotProfLik(which = 1, model = Mod_N.RS_addI.TC_I.TV_So, range = c(0,10), resolution = 20) # oh this is a super weird shape! I don't think it will be any different with the reversed age group variable, but let's see.
plotProfLik(which = 1, model = Mod_N.RS_addI.TCrev_I.TV_So, range = c(0,5), resolution = 20) # oho! this is actually different and it gives us parameters we can work with. Let's continue forward with the reversed model.

(p <- profLikCI(which = 1, model = Mod_N.RS_addI.TCrev_I.TV_So, upperRange = c(1,2),lowerRange = c(0,0.229)))
# XXX note: I initially got a very mysterious error when I provided ever so slightly wider ranges for the lower and upper ranges. I had a long discussion with DeepSeek about it, verified that there was nothing wrong with my model object or my data (e.g. no NAs etc.) and narrowed it down to there being something wrong with the profLikCI function itself. Turns out that I just needed to zoom the plot in even farther and narrow the starting ranges.
# Lower CI    Upper CI 
# 0.007611012 1.295365844 


#We can get an estimate of %ST corresponding to the upper and lower limits of the
#95% C.I. as follows.
#Instead of specifying the model, we specify the parameter values and the name of the nbdaData object

nbdaPropSolveByST(par = c(p[1], Mod_N.RS_addI.TCrev_I.TV_So@outputPar[2], Mod_N.RS_addI.TCrev_I.TV_So@outputPar[3]), nbdadata = nbdaData6_rev) # have to pass in the vector of values instead of just one.
# P(Network 1)  P(S offset) 
# 0.04528      0.00000 

nbdaPropSolveByST(par = c(p[2], Mod_N.RS_addI.TCrev_I.TV_So@outputPar[2], Mod_N.RS_addI.TCrev_I.TV_So@outputPar[3]), nbdadata = nbdaData6_rev)
# P(Network 1)  P(S offset)
# 0.63105      0.00000  

nbdaPropSolveByST(model = Mod_N.RS_addI.TCrev_I.TV_So)
# P(Network 1)  P(S offset) 
# 0.37249      0.00000 

# So 37% of events are expected to have occurred due to social transmission, with a 95% confidence interval between 4.5% and 63%. Interesting! That's a pretty wide range and it's lower than I expected. Granted, this is still a static network, so things might change when we incorporate the dynamic roost network to go with the dynamic ILVs.

# Now let's look at confidence intervals for the other parameters.
# Parameter 2 is distance between roost and carcass. The estimate was -1.4427889, which makes sense--greater distance would lead to a lower likelihood of learning. I'm encouraged that at least the direction is reasonable.
plotProfLik(which = 2, model = Mod_N.RS_addI.TCrev_I.TV_So, range = c(-10,20), resolution = 20) # oh this is weird! I don't understand what the red dots are, but at least we have a crossing point?
plotProfLik(which = 2, model = Mod_N.RS_addI.TCrev_I.TV_So, range = c(-5,5), resolution = 20) 
(p <- profLikCI(which = 2, model = Mod_N.RS_addI.TCrev_I.TV_So, upperRange = c(-1,0),lowerRange = c(-4,-2))) # Hmm okay so this does give us a confidence interval! I still don't really know what the red dots are but whatever...
# Lower CI   Upper CI 
# -2.7478365 -0.6266401 
# Let's see, how do we interpret this, especially with a standardized variable?

# (back to copying/modifying from the tutorial...)
#The first thing to note is that 0 is NOT within the 95% C.I., so there is evidence that distance from the roost to the carcass affects social learning rate (since the coefficient is negative, that means greater distances learn slower, which is what we expected. Yay!)

#Nonetheless, let us examine the back-transformed effect and C.I.
#The MLE for distance is Mod_N.RS_addI.TCrev_I.TV_So@outputPar[2] =  -1.442789
exp(Mod_N.RS_addI.TCrev_I.TV_So@outputPar[2])
#[1] 0.2362678
#So the rate of asocial learning decreases by an estimated factor of x0.24 for an increase of 1 S.D. in distance from the carcass, with back transformed 95% C.I. of
exp(p[1])
exp(p[2])
#0.064x - 0.53x

#What if we preferred to interpret effect sizes per meter, rather than per SD? We simply divide the coefficient by the SD for distance (original variable, not the standardized version)

exp(Mod_N.RS_addI.TCrev_I.TV_So@outputPar[2]/sd(roost_carc_distance))
#[1] 0.99
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
# [1] 253.6264 # this one is slightly lower, so the dynamic networks fit slightly better (yay! that's what we wanted!)
Mod_N.RS_addI.TCrev_I.TV_So@aicc
# [1] 257.2567
exp(0.5*(Mod_N.RS_addI.TCrev_I.TV_So@aicc-Mod_N.RD_addI.TCrev_I.TV_So@aicc))
#6.14x more support for the dynamic model


# Now time to analyze the results of this model
nbdaPropSolveByST(model = Mod_N.RD_addI.TCrev_I.TV_So)
6# P(Network 1)  P(S offset) 
# 0.44577      0.00000 

# Compare to the one with the static network:
nbdaPropSolveByST(model = Mod_N.RS_addI.TCrev_I.TV_So)
# P(Network 1)  P(S offset) 
# 0.37249      0.00000  # as expected, fewer transmission events in the static network version (good!)

#Fit the asocial model for comparison:
Mod_N.RD_addI.TCrev_I.TV_Aso <- oadaFit(nbdaData7_rev, type = "asocial")

#And then compare the social and asocial models using 
Mod_N.RD_addI.TCrev_I.TV_So@aicc
Mod_N.RD_addI.TCrev_I.TV_Aso@aicc # higher, as expected!
Mod_N.RD_addI.TCrev_I.TV_Aso@aicc-Mod_N.RD_addI.TCrev_I.TV_So@aicc # asocial is slightly higher than social, which means it's worse
exp(0.5*(Mod_N.RD_addI.TCrev_I.TV_Aso@aicc-Mod_N.RD_addI.TCrev_I.TV_So@aicc))
# -18.75224x more support for the social model vs. asocial


# 95% confidence interval for the social transmission parameter:
plotProfLik(which = 1, model = Mod_N.RD_addI.TCrev_I.TV_So, range = c(0,5), resolution = 20) #  nice, should be easy to identify the crossing points here

(p <- profLikCI(which = 1, model = Mod_N.RD_addI.TCrev_I.TV_So, upperRange = c(1,2),lowerRange = c(0,0.3)))
# Lower CI   Upper CI 
# 0.07116753 1.26517200 

#We can get an estimate of %ST corresponding to the upper and lower limits of the
#95% C.I. as follows.
#Instead of specifying the model, we specify the parameter values and the name of the nbdaData object

nbdaPropSolveByST(par = c(p[1], Mod_N.RD_addI.TCrev_I.TV_So@outputPar[2], Mod_N.RD_addI.TCrev_I.TV_So@outputPar[3]), nbdadata = nbdaData7_rev) # have to pass in the vector of values instead of just one.
# P(Network 1)  P(S offset) 
# 0.21996      0.00000

nbdaPropSolveByST(par = c(p[2], Mod_N.RD_addI.TCrev_I.TV_So@outputPar[2], Mod_N.RD_addI.TCrev_I.TV_So@outputPar[3]), nbdadata = nbdaData7_rev)
# P(Network 1)  P(S offset) 
# 0.60094      0.00000 

nbdaPropSolveByST(model = Mod_N.RD_addI.TCrev_I.TV_So)
# P(Network 1)  P(S offset) 
# 0.44577      0.00000  

# So 44% of events are expected to have occurred due to social transmission, with a 95% confidence interval between 21.9% and 60.1%. Interesting! That's a pretty wide range and it's lower than I expected. I had expected the percentage to go up a little with the dynamic network, and it did a little bit (with a narrow confidence range, too), but not by much. There's other stuff going on--which maybe unsurprising given that we have not introduced the co-flight networks!!!

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
#                             Variable        MLE        SE
# 1            1 Social transmission 1  0.3586772       NaN
# 2            2 Social transmission 2  0.0000000       NaN
# 3 3 Asocial: std_roost_carc_distance -1.1211523 0.3765711
# 4        4 Asocial: age_group_TV_rev -1.2082015 0.6563662

#You will notice that we have a second s parameter in the model. The s parameters correspond to the order the networks
#are entered into the socNets array. So the first s parameter,s1, labelled "1 Social transmission 1"  corresponds to 
#the network in socNets[,,1] and the second s parameter,s2, labelled "2 Social transmission 2" to the network in 
#socNets[,,2]. # KG: in our case, 1 = co-roosting and 2 = co-flight

# XXX why are the standard errors NaN?
# Thinking through possibilities from DeepSeek:
# One problem could be highly correlated predictors.
# I don't really know how to assess the correlation between two dynamic matrices. Let me see
correlations <- map2_dbl(roost_mats_expanded, fl_mats, ~{
  cor(c(.x), c(.y))
})

plot(correlations)
hist(correlations) # these are very low correlations. They are patterned through time, which makes sense because they could both be responding to environmental variables, but they are overall so low that I don't think this is the problem.
Mod_N.RD_N.FD_addI.TCrev_I.TV_So@outputPar

# One other potential problem is that the co-flight networks are quite sparse
round(map_dbl(roost_mats_expanded, sum)/(59*59),2)
round(map_dbl(fl_mats, sum)/(59*59),2) # much sparser than the co-roosting networks

# This in itself could explain why we are detecting more social transmission over the denser networks.

#Inference regarding the ILVs can proceed as it did in Tutorial 2, so here we will focus on inference about the s parameters.

#It looks like we have some evidence of social transmission following network 1 but not network 2, at first sight.
#Let us fit some constrained models to test some null hypotheses.

#First let us test the hypothesis s1=0.
#We first create the nbdaData object with the constraint s1=0:
N.RD_zero <- constrainedNBDAdata(nbdaData8_rev, constraintsVect = c(0,1,2,3)) # KG: I had to guess at how long this needed to be and I'm still not sure how to get that info
#Here the first parameter, s1, is constrained to be 0 and all other parameters are unconstrained
#Then fit the model:
Mod_N.RDzero_N.FD_addI.TCrev_I.TV_So <- oadaFit(N.RD_zero)

#We can then compare AICcs
Mod_N.RD_N.FD_addI.TCrev_I.TV_So@aicc
#[1] 256.0888
Mod_N.RDzero_N.FD_addI.TCrev_I.TV_So@aicc
#[1] 261.8219
#The model with s1>0 is favoured but by how much?
exp(0.5*(Mod_N.RDzero_N.FD_addI.TCrev_I.TV_So@aicc-Mod_N.RD_N.FD_addI.TCrev_I.TV_So@aicc))
#[1] 17.5756
#17.6x more support for a model in which there is social transmission following network 1

#We can also conduct a likelihood ratio test (LRT)
#Test statistic
teststat <- 2*(Mod_N.RDzero_N.FD_addI.TCrev_I.TV_So@loglik-Mod_N.RD_N.FD_addI.TCrev_I.TV_So@loglik)
#The difference in number of parameters is 1, so df=1
pchisq(teststat,df=1,lower.tail = F)
#[1] 0.004199476
#Strong evidence for social transmission following network 1 (co-roosting network) # KG: this is consistent with the model including only the roost network

#Now we can do the same for the hypothesis s2=0
N.FD_zero <- constrainedNBDAdata(nbdaData8_rev, constraintsVect = c(1,0,2,3))
Mod_N.RD_N.FDzero_addI.TCrev_I.TV_So <- oadaFit(N.FD_zero)
Mod_N.RD_N.FD_addI.TCrev_I.TV_So@aicc
#[1] 256.0888
Mod_N.RD_N.FDzero_addI.TCrev_I.TV_So@aicc
#[1] 253.6264
#The model with s2=0 is favoured but by how much?
exp(0.5*(Mod_N.RD_N.FD_addI.TCrev_I.TV_So@aicc-Mod_N.RD_N.FDzero_addI.TCrev_I.TV_So@aicc))
#[1] 3.425444

#Now the LRT
#Test statistic
teststat <- 2*(Mod_N.RD_N.FDzero_addI.TCrev_I.TV_So@loglik-Mod_N.RD_N.FD_addI.TCrev_I.TV_So@loglik)
#The difference in number of parameters is 1, so df=1
pchisq(teststat,df=1,lower.tail = F)
#[1] 1
#No evidence for social transmission following network 2 (co-flight network)

#This does not necessarily mean that we have strong evidence for s1>s2. It could be that s2 has very wide confidence intervals. We need to test the hypothesis s1 = s2 separately, as follows
N.RD_equals_N.FD <- constrainedNBDAdata(nbdaData8_rev, constraintsVect = c(1,1,2,3))
#In the constraintsVect, parameter 1 and 2 have the same number, meaning they are constrained to have the same value, i.e. s1=s2 as required
#Fit the model:
Mod_N.RD_equals_N.FD_addI.TCrev_I.TV_So <- oadaFit(N.RD_equals_N.FD)
#compare AICcs:
Mod_N.RD_N.FD_addI.TCrev_I.TV_So@aicc
Mod_N.RD_equals_N.FD_addI.TCrev_I.TV_So@aicc #oop, this one is lower!
exp(0.5*(Mod_N.RD_equals_N.FD_addI.TCrev_I.TV_So@aicc-Mod_N.RD_N.FD_addI.TCrev_I.TV_So@aicc))
#[1] 0.4611768

#LRT:
#Test statistic
teststat <- 2*(Mod_N.RD_equals_N.FD_addI.TCrev_I.TV_So@loglik-Mod_N.RD_N.FD_addI.TCrev_I.TV_So@loglik)
#The difference in number of parameters is 1, so df=1
pchisq(teststat,df=1,lower.tail = F)
#[1] 0.3389193
#No evidence that s1>s2.

#So overall we have:
#1. strong evidence for social transmission following network 1. 
#2. no evidence for social transmission following network 2.
#3. no evidence that the social transmission following network 2, if it exists, is weaker than the social transmission following network 1

#In general terms: it is often tempting, when we find strong evidence of effect A, and no evidence of effect B to conclude that we have strong evidence that effect A > effect B.
#But this is a logical error--remember that "no evidence of an effect" does not equate to "strong evidence of no effect".

#We would strongly advise users of NBDA to present confidence intervals (C.I.s) for effect sizes. These can be obtained for s1 and s2 in the same manner as in Tutorial 2:

#for s1, which=1
plotProfLik(which=1,model=Mod_N.RD_N.FD_addI.TCrev_I.TV_So, range=c(0,3),resolution=20)
#lower limit between 0 and 0.5
#upper limit between 1 and 2
profLikCI(which=1 ,model=Mod_N.RD_N.FD_addI.TCrev_I.TV_So,
          lowerRange = c(0,0.5),upperRange = c(1,2))
# Lower CI   Upper CI 
# 0.07116999 1.26517200

#for s2, which=2
plotProfLik(which=2,model=Mod_N.RD_N.FD_addI.TCrev_I.TV_So, range=c(1,2),resolution=20)
# lower limit between 1.5 and 2
plotProfLik(which=2,model=Mod_N.RD_N.FD_addI.TCrev_I.TV_So, range=c(100,1000),resolution=20)
# can't find an upper limit
profLikCI(which=2,model=Mod_N.RD_N.FD_addI.TCrev_I.TV_So, lowerRange = c(1.5, 2))
# Lower CI Upper CI 
# 1.768372       NA  # XXX if no upper limit exists and the lower limit is above 0, then why do we not have any evidence of social transmission via this network?

#So we have a clearer picture already as to why we saw the pattern of significance in the LRTs
#There is quite a lot of overlap in the 95% C.I.s. (#KG: in our example there's not; I'm keeping this here for explanation only)
#However, do not fall into the trap of thinking that because the C.I.s for two parameters overlap there is NOT evidence for a difference between them!
#Instead one should aim to get the confidence interval for the difference, in this case for s1-s2.
# KG: I removed the code for calculating this because it doesn't seem relevant for our non-overlapping CIs. Can go back to the tutorial if I need to for another diffusion.

#We can also get %ST for the proportion of events that ocurred via social transmission for each network:

nbdaPropSolveByST(model=Mod_N.RD_N.FD_addI.TCrev_I.TV_So)
# P(Network 1) P(Network 2)  P(S offset) 
# 0.44577      0.00000      0.00000 

#If we want to get %ST corresponding to the upper and lower limits of C.I.s we can do so using the same procedure as in previous tutorials:

#s1 
(p <- profLikCI(which=1 ,model=Mod_N.RD_N.FD_addI.TCrev_I.TV_So,
                lowerRange = c(0,0.5),upperRange = c(1,2)))
# Lower CI   Upper CI 
# 0.07116753 1.26517200 

nbdaPropSolveByST(par = c(p[1], Mod_N.RD_N.FD_addI.TCrev_I.TV_So@outputPar[2], Mod_N.RD_N.FD_addI.TCrev_I.TV_So@outputPar[3], Mod_N.RD_N.FD_addI.TCrev_I.TV_So@outputPar[4]), nbdadata = nbdaData8_rev) # have to pass in the vector of values instead of just one.
# P(Network 1) P(Network 2)  P(S offset) 
# 0.21997      0.00000      0.00000 

nbdaPropSolveByST(par = c(p[2], Mod_N.RD_N.FD_addI.TCrev_I.TV_So@outputPar[2], Mod_N.RD_N.FD_addI.TCrev_I.TV_So@outputPar[3], Mod_N.RD_N.FD_addI.TCrev_I.TV_So@outputPar[4]), nbdadata = nbdaData8_rev)
# P(Network 1) P(Network 2)  P(S offset) 
# 0.60095      0.00000      0.00000 

#So a plausible range for the % of events occurring by social transmission through network 1 is 22% - 60%

#s2 
(p <- profLikCI(which=2 ,model=Mod_N.RD_N.FD_addI.TCrev_I.TV_So,
                lowerRange = c(0,3)))
# Lower CI Upper CI 
# 1.768386       NA 

nbdaPropSolveByST(par = c(Mod_N.RD_N.FD_addI.TCrev_I.TV_So@outputPar[2], p[1], Mod_N.RD_N.FD_addI.TCrev_I.TV_So@outputPar[3], Mod_N.RD_N.FD_addI.TCrev_I.TV_So@outputPar[4]), nbdadata = nbdaData8_rev) # have to pass in the vector of values instead of just one.
# P(Network 1) P(Network 2)  P(S offset) 
# 0.00000      0.07538      0.00000

nbdaPropSolveByST(model=Mod_N.RD_N.FD_addI.TCrev_I.TV_So)
#So a plausible range for the % of events occurring by social transmission through network 1 is 22% - 60%
