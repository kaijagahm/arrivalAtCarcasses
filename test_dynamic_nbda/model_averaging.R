# Model averaging/multimodel inference with NBDA
# Created 2025-04-15
# This will be important since we have a few possible ILVs to include and a few possible network models to try (flight, roosting, etc)

# Going to start with just one carcass so we don't get overwhelmed, so I've moved this code out of the multiple_carcasses.R script.
library(tidyverse)
library(NBDA)
library(here)

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

# Reading in the objects exported from multiple_carcasses.R
age_groups_29 <- readRDS(here("test_dynamic_nbda/data/age_groups_29.RDS"))
std_roost_carc_distances_29 <- readRDS(here("test_dynamic_nbda/data/std_roost_carc_distances_29.RDS"))
N.RD <- readRDS(here("test_dynamic_nbda/data/N.RD.RDS"))
N.FD <- readRDS(here("test_dynamic_nbda/data/N.FD.RDS"))
N.RS <- readRDS(here("test_dynamic_nbda/data/N.RS.RDS"))
oas <- readRDS(here("test_dynamic_nbda/data/oas.RDS"))
roost_mats_expanded <- readRDS(here("test_dynamic_nbda/data/roost_mats_expanded.RDS"))
fl_mats <- readRDS(here("test_dynamic_nbda/data/fl_mats.RDS"))
idx <- 29

# Testing multimodel inference, per tutorial 7 ----------------------------
## To follow the tutorial, let's use one carcass (one diffusion) with one static network (static roost network) and two ILVs (age; distance on the first day from the carcass).

## Get our standardized ILVs (static, not dynamic)
age <- cbind(age_groups_29[,1])
dist <- cbind(std_roost_carc_distances_29[,1])
asoc <- c("age", "dist")
## Create an object for the "unconstrained" model
nd_unconstrained <- nbdaData(label = "test", assMatrix = N.RS[[idx]], orderAcq = oas[[idx]], asoc_ilv = asoc, int_ilv = asoc)

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

#############################################################################
# TUTORIAL 7.4
# MULTI-MODEL INFERENCE IN AN OADA WITH MULTIPLE NETWORKS
# 1 diffusion
# 2 dynamic networks
# 2 time constant ILVs
#############################################################################
#In a multi-network NBDA, we need to combine our networks into an array
#If we have dynamic (time-varying) networks we need to create a four dimensional array of size no. individuals x no.individuals x no.networks x number of time periods and provide an assMatrixIndex vector as shown in Tutorial 1.

n_timeperiods <- dim(N.FD[[idx]])[4]
n_indivs <- dim(N.FD[[idx]])[1]
rme <- roost_mats_expanded[[idx]]
fm <- fl_mats[[idx]]

#Create the empty array
N.RD_N.FD <- array(NA, dim = c(n_indivs, n_indivs, 2, n_timeperiods))
#Slot in the network for each time period # XXX
for(i in 1:length(rme)){
  N.RD_N.FD[,,1,i] <- array(rme[[i]], dim = c(n_indivs, n_indivs, 1)) # network 1 is roosting
  N.RD_N.FD[,,2,i] <- array(fm[[i]], dim = c(n_indivs, n_indivs, 1)) # network 2 is flight
}

assMatrixIndex <- 1:length(oas[[idx]]) # already expanded the matrices

nbdaData_multiNet <- nbdaData(label = paste0("Diffusion_", idx),
                              assMatrix = N.RD_N.FD,
                              orderAcq = oas[[idx]],  
                              asoc_ilv = asoc,
                              int_ilv = asoc,
                              asocialTreatment = "timevarying", # I don't understand why this is "asocialTreatment" instead of "ilvsTreatment", since it seems to also apply to the multiplicative/interacting ILVs, but oh well. We need to specify this as time-varying because the default, constant, would assume that the ILVs do not change through time.
                              assMatrixIndex = assMatrixIndex)

#Then we go on to create our nbdaData object as before. Let us assume we are interested in the unconstrained model:
#Then fit the model:
model1_multiNet<-oadaFit(nbdaData_multiNet)
#And get the output:
data.frame(Variable=model1_multiNet@varNames,MLE=model1_multiNet@outputPar,SE=model1_multiNet@se) # XXX why are there NaN values in these models? I wonder if it has to do with the error things not working very well?

#Let us assume we have 4 competing hypotheses about social transmission.
#1. Transmission through network 1 only (s2=0) (1 = roosting)
#2. Transmission through network 2 only (s1=0) (2 = flight)
#3. Transmission through network 1 and network 2 at equal rates per unit connection (s1=s2) (roosting = flight)
#4. Transmission through network 1 and network 2 at different rate (no constraint) (roosting != flight)

#Each of these can be represented by different constraints between the s parameters, which we refer to as a network combination or netcombo:
#1. 1 0 (roosting but not flight)
#2. 0 1 (flight but not roosting)
#3. 1 1 (roosting == flight)
#4. 1 2 (roosting != flight)

#We can fit models with each netcombo and compare the fit. However, we are unsure which ILVs to include, so we want to use multi-model inference (KG: yes, this is exactly my situation!)
#We need to set up a constraintsVectMatrix that considers each netcombo but also every combination of ILVs
# We need six columns because length(model1_multiNet@outputPar is 6, because we're using two possible ILVs and allowing each one to be absent, additive, or multiplicative, I think.)

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

# support numberOfModels
# 0:0 1.521450e-07              4
# 0:1 6.465641e-01             16
# 1:0 1.854665e-06             16
# 1:1 1.721005e-03             16
# 1:2 3.517129e-01             16

# Translating that out of scientific notation
# 0:0 0.0000152145 %            4
# 0:1 64.65641 %               16
# 1:0 0.0001854665 %           16
# 1:1 0.1721005 %              16
# 1:2 35.17129 %               16

#Note that we have equal numbers of models for each network combo save for asocial learning (0 0), so we can compare the support for different models
#of social transmission. However, we recommend examining the CIs for s parameters to judge the evidence for social transmission versus asocial learning (see main text).
#We can see strongest support (64.65%) for social transmission only through network 2 (flight), and second most (35.17%) for transmission of different strengths through each network (roosting and flight).

#We can also get MAEs etc as before:
rbind(
  support=variableSupport(modelSet_multiNet),
  MAE=modelAverageEstimates(modelSet_multiNet),
  USE=unconditionalStdErr(modelSet_multiNet))

#                 s1       s2   ASOC:age  ASOC:dist SOCIAL:age SOCIAL:dist
# support 0.35343576 0.999998  0.3879940  1.0000000  0.4982325   0.3175748
# MAE     0.02975252 4.018881 -0.2005121 -2.0613338  0.4040631  -0.1262081
# USE            NaN      NaN  0.1336602  0.3290568        NaN         NaN

#The missing USEs are due to some models in which the SEs could not be derived. If we look at
print(modelSet_multiNet)
#We can see that the SEs are present in all the models with high weight, so it seems reasonable to get an approximate USE by replacing NAs and NaNs with the weighted mean across all other models:

rbind(
  support=variableSupport(modelSet_multiNet),
  MAE=modelAverageEstimates(modelSet_multiNet),
  USE=unconditionalStdErr(modelSet_multiNet,nanReplace = T))

#                  s1        s2   ASOC:age  ASOC:dist SOCIAL:age SOCIAL:dist
# support 0.353435757  0.999998  0.3879940  1.0000000  0.4982325  0.31757481
# MAE     0.029752517  4.018881 -0.2005121 -2.0613338  0.4040631 -0.12620806
# USE     0.008229083 12.204649  0.1336602  0.3290568 16.0207377  0.09694897

#Averaged across models we can see that s2 (flight) is estimated to be considerably greater than s1 (roosting). (KG: but the SE is really high for s2, so I don't know...)

#We would recommend presenting the 95% CI for s1 and s2 in the best model in which they are present
#and the 95% CI for s1-s2 in the best model in which they are both present and unconstrained.

#We can check the sensitivity of the 95% CIs for s2 to model selection uncertainty as before:

lowerLimitsByModel<-multiModelLowerLimits(which=2,aicTable = modelSet_multiNet,conf=0.95) # yeesh, this takes forever!!
lowerLimitsByModel 
# Make a plot (it will only show the scenarios where s2 is not constrained to 0, so 0:1, 1:1, and 1:2)
lowerLimitsByModel %>%
  ggplot(aes(x = factor(model), col = netCombo))+
  geom_point(aes(y = propST))+
  geom_segment(aes(y = lowerCI, yend = propST))+
  theme_minimal()+
  labs(title = "Social transmission on the roost network",
       subtitle = "Lower limits by model",
       y = "Proportion social transmission",
       x = "Model")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) # Uhhh why is this showing nonsensical results? Why are the lower limits of the CIs *higher* than the estimates? What am I missing?

# XXX were their CI "lower limits" also above the estimates? Maybe I'm misinterpreting what these values represent? Going to look at the tutorial output to check.
# I'm not misinterpreting--the tutorial output clearly shows that the CI minima are lower than the estimates. So what is going on in our example here? Data bad? Something up with the scaling? Or is this simply an indication that there is not support for social transmission?
# Some of the CI's point down the way they should, but that's only in the 1:1 scenario, and we've already seen that we have very low support for that scenario.
# Maybe this would be solved by just first testing it against the asocial model, and then moving on to this only if there is support for social transmission at all. I don't really understand how to figure that out, though.

#We can obtain a model averaged lower-limit for propST as follows:

sum(lowerLimitsByModel$propST*lowerLimitsByModel$adjAkWeight)
#[1] 0.3988139 # yeah, so this is once again quite a bit higher than the vast majority of our actual estimates.

#However, note that this includes models with netCombo= 1 1- i.e. one in which the value of s2 is constrained to be = s1. 
#Our preferred approach is to refit the model set including only those models in which the value of s2 is unconstrained, and re-run the exercise:


constraintsVectMatrixs1<-rbind(
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

modelSet_s2<-oadaAICtable(nbdadata = nbdaData_multiNet,constraintsVectMatrix =constraintsVectMatrixs1)
lowerLimitsByModel_s2<-multiModelLowerLimits(which=2,aicTable = modelSet_s2,conf=0.95)
lowerLimitsByModel_s2 # yeah, so again these look unreasonable.

sum(lowerLimitsByModel_s2$propST*lowerLimitsByModel_s2$adjAkWeight)
#[1] 0.3992269, but I don't know if we can trust this based on the insane results on the plot. 

#Giving a model-averaged lower limit of 39.9% of events occurring as a result of social transmission via network 1

# We could do the same for s1, but I'm hesitant to do so because of the disastrous results here
# I wonder whether this has anything to do with the fact that the networks are dynamic? There's no tutorial on model averaging for dynamic networks, but I don't really see why the approach should be different.

# This is especially weird because if I go back to the non-model-averaged estimates that I did in multiple_carcasses.R and look up this particular carcass, #29 (4877850), search_with_ilvs gives us a CI of (0.32146, 0.49672) for propsolve (which is consistent with the model-averaged estimate of 0.39), but/and tells us it's significantly different from 0 i.e. there is evidence for social transmission on the co-flight network. Apparently there's also evidence of social transmission on the co-roost network, though I didn't run that one with ILVs.

# all in all I'm just kinda confused about where these weird standard errors are coming from. 

# Went back to the paper and oh--they say that the standard error estimates can often be really wacky if things are asymmetrical, which I bet these are. So I'm supposed to not trust those and instead just figure out the best model and then find the CI for that model, which makes a bit more sense to me.
# They do then follow it up with a discussion of how you're supposed to confirm that the lower limit of the CI is above 0 in order to validate that there's support for that model. Which is what I did that led me to finding the weird CI values.
# So I'm left with the question: if I'm supposed to disregard the CIs, but I'm also supposed to use the CIs to validate whether the model is legit, can I do both at the same time? How do I figure out which to trust?
# These seem like questions I should ask Matt or Sonja or someone else, but I think I'll leave it here for now.