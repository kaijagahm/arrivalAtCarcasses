# Imitate NBDA tutorial 1
# JANE 13307
# PACKAGES
## Workflow
library(here)
library(devtools)
library(targets)
#Then download and install my NBDA package from GitHub:
#devtools::install_github("whoppitt/NBDA")
#And load it as follows
library(NBDA)

targets::tar_load(all_carcasses_annotated)
aca <- all_carcasses_annotated
mycarc <- aca %>%
  filter(carcID == "4874955")
at_carcass <- read_csv(here("test_dynamic_nbda/data/at_carcass.csv"))
firsts <- read_csv(here("test_dynamic_nbda/data/firsts.csv"))
dim(firsts) # This shows the order of acquisition and the time of acquisition too

# Some parameters
# How many individuals are involved in the diffusion?
n_indivs <- length(unique(firsts$local_identifier))

# Read in the roost networks for each day
r1 <- as.matrix(sapply(read.csv(here("test_dynamic_nbda/data/roost_networks/r1.csv")), as.numeric))[,-1]
r2 <- as.matrix(sapply(read.csv(here("test_dynamic_nbda/data/roost_networks/r2.csv")), as.numeric))[,-1] 
r3 <- as.matrix(sapply(read.csv(here("test_dynamic_nbda/data/roost_networks/r3.csv")), as.numeric))[,-1]  
r4 <- as.matrix(sapply(read.csv(here("test_dynamic_nbda/data/roost_networks/r4.csv")), as.numeric))[,-1]  

## Create a static network for comparison by taking the mean of the roost network values
ls <- list(r1, r2, r3, r4)
r_static_mn <- Reduce("+",ls)/length(ls) # get the mean of the roost values from each day's network so we can use a static network

#############################################################################
# TUTORIAL 1.1
# FITTING A BASIC OADA MODEL
# 1 diffusion
# 1 static network
# 0 ILVs
#############################################################################
#Read in the csv file containing the social network, converting it to a matrix
r_static_mn # static (mean) roost network
class(r_static_mn) # check that it's a matrix

#Convert the matrix to a three dimensional array- this is because the NBDA package
#is designed to work with multiple networks as well as a single network
socNet1 <- array(r_static_mn, dim = c(n_indivs, n_indivs, 1))
#The network needs to be arranged such that row N contains the incoming connections for
#individual N.

#Enter a vector giving the order in which individuals learned the target behaviour
#This corresponds to the individuals' positions in the social network matrix
oa <- readRDS(here("test_dynamic_nbda/data/oa.RDS"))
oa
#e.g. the first individual to learn here is in the 14th row and column of socNet1

# Create an nbdaData object containing the data we need to fit an OADA model
# The label is simply a string of text you can use to remind yourself what data is stored in this object
# assMatrix stands for association matrix, since these are most commonly used, but this can be any type of social network # QQQ wait so could I use an edge list???
nbdaData1 <- nbdaData(label="ExampleDiffusion1", assMatrix = socNet1, orderAcq = oa)

#We can now fit an OADA model using oadaFit, here we store it in an object named model_social
model_social <- oadaFit(nbdaData1)

#The maximum likelihood estimates (MLEs) for the parameter(s) is stored in the @outputPar slot
model_social@outputPar
#The standard errors for the parameter(s) is stored in the @se slot
model_social@se
#But we can get a neat printout of the model fit as follows

data.frame(Variable = model_social@varNames,
           MLE = model_social@outputPar,
           SE = model_social@se)

#giving us:

#                  Variable       MLE          SE
# 1 1 Social transmission 1 318182328 1.26958e+12

# XXX KG note: I think "MLE" means "maximum likelihood estimate"
# So we can see that s has been estimated at 318182328, but the SE looks large in comparison at
# 1.26958e+12. Does this mean there is not good evidence for social transmission? (In the example, it didn't mean that necessarily, but we'll see...)

#The value estimated for s might be difficult to interpret, depending on the network
#used. In such cases, we can obtain an estimate of the % of events that occured by social
#transmission as opposed to asocial learning (%ST) as follows:

nbdaPropSolveByST(model = model_social)
# P(Network 1)  P(S offset) 
# 1            0 

#This tells us that the estimated value for s corresponds to 100% (the function returns a 
#proportion so multiply by 100 to get %ST)
#P(Network 1) stands for the proportion of events that were a result of transmission through 
#network 1 (we only have one network)
#P(S offset) can be ignored for now

#By default the calculation of %ST excludes the first learning event (innovation) for an unseeded diffusion, since we know this had to be asocial learning. If we want to include them we can do so:

nbdaPropSolveByST(model = model_social, exclude.innovations = F)
# P(Network 1)  P(S offset) 
# 0.9697       0.0000 

#Let us fit an asocial model (with s=0), by specifying type="asocial"
model_asocial <- oadaFit(nbdaData1, type = "asocial")

#And then compare the social and asocial models using 

model_social@aicc
model_asocial@aicc
model_asocial@aicc-model_social@aicc

#So the social model is favoured by 12.10372 AICc units. This means:
exp(0.5*(model_asocial@aicc-model_social@aicc))
#[1] 424.9022
#the social model is 424.9x more likely to be the best K-L model, out of the two.
#Or we can say the social model has 424.9x more support than the asocial model.

#We can also conduct a likelihood ratio test (LRT) for social transmission
#The @loglik slot contains the -log-likelihood- i.e. minus the log-likelihood
#So we can get the test statistic as double the difference in -log-likelihood as follows:
2*(model_asocial@loglik-model_social@loglik)
#[1] 14.23275

#There is 1 parameter in model_social, and 0 in model_asocial, so we have 1 d.f.
pchisq(2*(model_asocial@loglik-model_social@loglik),df=1,lower.tail=F)
#[1] 0.0001615346
#p= 0.00016, strong evidence of an effect consistent with social transmission

#We can get 95% confidence intervals (C.I.s) for the parameter by first plotting the
#profile log-likelihood function. This is the -log-likelihood for a specified
#value of the parameter, when all other parameters in the model have been optimized.
#In this case there are no other parameters, so the profile log-likelihood is the
#same as the -log-likelihood.
#We specify which=1 because we are interested in the first parameter in the model
#as listed in the model output above. We specify the name of the model, and also the range
#we want to plot over, and resolution determines how many points will be plotted
plotProfLik(which = 1, model = model_social, range = c(0,10), resolution = 20)

#Any values of s for which the profile log-likelihood is above the dotted line would be
#REJECTED in a likelihood ratio test (LRT), and therefore are OUTSIDE the 95% C.I.
#Therefore we can get the 95% C.I. by finding the crossing points

#Here we can see that there is one crossing point between 0 and 20, but it looks like we
#have to zoom out a bit to see the upper limit. We can make the range wider to do this
plotProfLik(which = 1, model = model_social, range = c(0,100000), resolution = 20) # looks like there is no upper limit, presumably because the result was 100%

#There is no visible upper limit.

#Before we move on to find these points, take a moment to note the asymmetry in the
#profile log-likelihood. We have quite a lot of certainty about the lower limit of
#s, but little certainty about the upper value.
#It is this uncertainty about the upper value that led to a high SE above, and thus
#the SE failed to quantify the strength of evidence against the null hypothesis, s=0.

#So we need to find the cross over points to get the 95% C.I. We use the profLikCI
#function, specifying the upperRange and lowerRange to search in.

profLikCI(which = 1, model = model_social, upperRange = c(10000,1000000),lowerRange = c(0,20))
# Lower CI     Upper CI 
# 7.581885 10000.000200 

#s=0 is not included in the 95% C.I. so there is at least reasonable evidence for social transmission
#or at least a statistical effect consistent with social transmission.
#Note we can obtain C.I.s for a different level of confidence by setting, e.g. conf=0.99 in the
#plotProfLik and profLikCI functions

#We can get an estimate of %ST corresponding to the upper and lower limits of the
#95% C.I. as follows.
#Instead of specifying the model, we specify the parameter values and the name of the nbdaData object

nbdaPropSolveByST(par = 7.581885, nbdadata = nbdaData1)
# P(Network 1)  P(S offset) 
# 0.70011      0.00000 
nbdaPropSolveByST(par = 10000.000200, nbdadata = nbdaData1)
# P(Network 1)  P(S offset) 
# 0.99954      0.00000 

#So between 70.0 - 99.9% of events are estimated to have occurred by social transmission.

#############################################################################
# TUTORIAL 1.2
# ADDING SEEDED DEMONSTRATORS TO AN OADA MODEL
# 1 diffusion
# 1 static network
# Seeded demonstrators
#############################################################################

#Adding seeded demonstrators to a diffusion is straightfoward: simply create a vector showing who started
#the diffusion informed (1) or naive, and input it to the demons argument in nbdaData

#Let's say that the individuals that were already present at the carcass site in the 2 hours before it was deposited were the demonstrators.
head(at_carcass)

carcass_time <- mycarc$datetime
carcass_time_minus2 <- carcass_time-hours(2)
demonstrators <- at_carcass %>%
  filter(timestamp >= carcass_time_minus2 & timestamp < carcass_time) %>%
  pull(local_identifier) %>%
  unique() # this gives us five demonstrators

oa

#So let create a demons vector full of zeroes
demons<-rep(0,length(oa))
#and slot in 1s for 26,29 and 30
demons[oa[demonstrators]] <- 1
demons

#we can then remove them from the order of acquistion:
oa2 <- oa[!(names(oa) %in% demonstrators)]
length(oa2)
length(oa)

#create the nbdaData object
nbdaData1_seeded <- nbdaData(label = "ExampleDiffusion1", 
                             assMatrix = socNet1,
                             orderAcq = oa2, 
                             demons = demons)
#and fit the model
model_seeded <- oadaFit(nbdaData1_seeded)

#Note that models with seeded demonstrators cannot be compared to models with those same individuals included in the order of acqusition using AICc or LRTs, since they are being fitted to different data (different orders of acquisition)

# XXX KG addition: going to calculate the values anyway--not a direct comparison, but we do get a different conclusion depending on how we define the acquisition here. 
nbdaPropSolveByST(model = model_seeded) # with this order of acquisition, only 90% of the events are estimated to have occurred by 

#############################################################################
# TUTORIAL 1.3
# ADDING TRANSMISSION WEIGHTS TO AN OADA MODEL
# 1 diffusion
# 1 static network
# Transmission weights
#############################################################################

# XXX KG addition: could theoretically add transmission weights according to e.g. time that each individual spent at the carcass, or whether they actually ate there versus just visiting. Not going to do this right now because it would be a bunch more calculations.

#############################################################################
# TUTORIAL 1.4
# USING A DYNAMIC NETWORK IN OADA
# 1 diffusion
# 1 dynamic network
#############################################################################
#Imagine we believe the social network changed at various times during the diffusion
#We already loaded in several co-roosting networks above, one for each night.

ls # this is a list of r1, r2, r3, r4, etc.

#We need to combine these in a 4 dimensional array, with the 4th dimension for time periods
n_timeperiods <- length(ls) # 4 nights of roost networks

#Create the empty array
socNet <- array(NA, dim = c(n_indivs, n_indivs, 1, n_timeperiods))
#Slot in the network for each time period
socNet[,,,1]<-r1
socNet[,,,2]<-r2
socNet[,,,3]<-r3
socNet[,,,4]<-r4

## (For comparison: static network)
socNet_static <- array(NA, dim = c(n_indivs, n_indivs, 1, 1)) # just 1 time period
socNet_static[,,,1]<-r_static_mn # created this network as the mean of the others, up at the top above.

# Now we need to create a vector specifying which time period corresponds to which acquisition event
# Let's figure out which diffusion events correspond to which day
roost_dates <- readRDS(here("test_dynamic_nbda/data/roost_networks/roost_dates.RDS"))
lookup <- data.frame(timeperiod = 1:length(roost_dates),
                     roost_date = roost_dates)
acq_roost_dates <- firsts %>%
  mutate(roost_date = dateOnly-days(1)) %>%
  left_join(lookup, by = "roost_date")

# Get order of acquisition based on the positions in the roost network

assMatrixIndex<-acq_roost_dates$timeperiod
#Now we enter the 4 dimensional network and assMatrixIndex as follows
nbdaData1_weights <- nbdaData(label = "ExampleDiffusionDynNet",
                              assMatrix = socNet, 
                              orderAcq = oa,
                              assMatrixIndex = assMatrixIndex)

model_dynamic <- oadaFit(nbdaData1_weights)
data.frame(Variable = model_dynamic@varNames,
           MLE = model_dynamic@outputPar,
           SE = model_dynamic@se)
# Variable       MLE       SE
# 1 1 Social transmission 1 0.5777962 1.436463

# (Static network comparison)
nbdaData1 <- nbdaData(label = "ExampleDiffusion1",
                      assMatrix = socNet_static, 
                      orderAcq = oa)

#We can now fit an OADA model using oadaFit, here we store it in an object named model_social
model_social <- oadaFit(nbdaData1)


# Models with a dynamic network can be compared to static network models if they are fitted to the same order of acquisition
model_dynamic@aicc
#[1] 172.0084
model_social@aicc 
# [1] 158.0052

# here the static model actually fits significantly better than the dynamic model (lower AICC)


#############################################################################
# TUTORIAL 2.1
# ADDING TIME-CONSTANT ILVs TO AN OADA MODEL
# 1 diffusion
# 1 static network
# 2 Time constant ILVs
#############################################################################

#Read in the social network and order of acquisition vector as shown in tutorial 1.
socNet1
dim(socNet1) # we're still just using the array containing the static social network.
oa

#Get a CSV of the time-constant ILVs.
ilvs <- read_csv(here("test_dynamic_nbda/data/ilvs.csv"))
ilvs <- ilvs %>%
  filter(local_identifier %in% indivs)
ilvs$local_identifier == indivs # should be TRUE to make sure they're in the same order

dist_before <- cbind(ilvs$dist_to_carc_daybefore)
dist_before_std <- (dist_before-mean(dist_before))/sd(dist_before) 

age_group <- cbind(ilvs$age_group) # age group (categorical)
age_group[age_group == "01_juv_sub"] <- 0
age_group[age_group == "02_adult"] <- 1
age_group <- cbind(as.numeric(age_group))

age <- cbind(ilvs$age_in_2024) #age in years (continuous)
age_std <- (age-mean(age))/sd(age)

#Each ILV needs to be stored as a column matrix; this is to allow extension to time-varying ILVs (see below)

#Since age and distance are centered on 0, the baseline asocial rate is an individual of mean age and that starts the mean distance away from the carcass (if we use continuous age). Or if we use categorical age, the baseline asocial rate is a juvenile that starts the mean distance away from the carcass.
#Therefore s is estimated relative to this baseline.

asoc <- c("age_group", "dist_before_std") # XXX I don't understand why this is called "asoc"--it's very confusing! I would call it "ilvs_to_use" or something
#We create a vector of the names of the variables to be included in the analysis

#Now we create an nbdaData object as before, but specifying the ILVs to be included
#asoc_ilv indicates the ILVs assumed to affect asocial learning rate
#int_ilv indicates the ILVs assumed to affect social learning rate
#multi_ilv indicates the ILVs assumed to affect asocial and social learning rate the same amount (multiplicative model)

#Therefore, if we wanted to fit an "additive" model we would specify:
nbdaData2_add <- nbdaData(label = "ExampleDiffusion2", 
                          assMatrix = socNet1,
                          orderAcq = oa,
                          asoc_ilv = asoc)
#Then fit the model:
model2_add <- oadaFit(nbdaData2_add)
nbdaPropSolveByST(model = model2_add)

#And get the output:
data.frame(Variable = model2_add@varNames,
           MLE = model2_add@outputPar,
           SE = model2_add@se)

#                    Variable          MLE  SE
# 1    1 Social transmission 1 914461.62843 NaN
# 2       2 Asocial: age_group -97461.53309 NaN
# 3 3 Asocial: dist_before_std      4.97328 NaN

#If we wanted to fit a "multiplicative" model we would specify:
nbdaData2_multi <- nbdaData(label = "ExampleDiffusion2",
                            assMatrix = socNet1,
                            orderAcq = oa,
                            multi_ilv = asoc)
model2_multi <- oadaFit(nbdaData2_multi)
data.frame(Variable = model2_multi@varNames,
           MLE = model2_multi@outputPar,
           SE = model2_multi@se)
nbdaPropSolveByST(model = model2_multi)

#                             Variable           MLE  SE
# 1            1 Social transmission 1  6.282767e+08 NaN
# 2       2 Social= asocial: age_group  7.020256e-01 NaN
# 3 3 Social= asocial: dist_before_std -2.127998e-01 NaN

#Or the "unconstrained" model
nbdaData2_uc <- nbdaData(label = "ExampleDiffusion2",
                         assMatrix = socNet1,
                         orderAcq = oa,
                         asoc_ilv = asoc,
                         int_ilv = asoc)
model2_uc <- oadaFit(nbdaData2_uc)
data.frame(Variable = model2_uc@varNames,
           MLE = model2_uc@outputPar,
           SE = model2_uc@se)

#                     Variable         MLE           SE
# 1    1 Social transmission 1  79.4522060 2.260913e+02
# 2       2 Asocial: age_group -25.6941287 1.154700e+04
# 3 3 Asocial: dist_before_std   3.4922057 2.480081e+00
# 4        4 Social: age_group   1.0250513 4.694902e-01
# 5  5 Social: dist_before_std  -0.4365975 2.620383e-01

#Note that one does not need to specify the same set of ILVs in asoc_ilv int_ilv and multi_ilv
#So a model can be fitted in which some variables affect only asocial learning, some only social learning, some affect both the same amount, and some affect social and asocial learning differently.

#In practise, there will be little reason to decide, a priori, which ILVs to put in which slot.
#Our preferred approach is to include in multi_ilv only ILVs for which there is a logical reason to believe it will affect asocial and social learning the same amount, put all other ILVs in both the asoc_ilv and int_ilv slots, and then perform multi-model inferencing (see Tutorial 7)

#For simplicity in the tutorial, we will take the approach of choosing the best of the 3 models based on AICc

model2_add@aicc
model2_multi@aicc
model2_uc@aicc # this one is lowest, but I want to pick a simpler one so let's look at the additive model

#We see the unconstrained model is favored. I'm not sure what it means that the standard error estimates are NA in the other two--might just be because the probability of social transmission is very high and therefore SE can't be estimated? For consistency with the tutorial, I'm going to stick with the additive one and hope I don't run into problems with the SE.

data.frame(Variable = model2_add@varNames,
           MLE = model2_add@outputPar,
           SE = model2_add@se)

# Variable          MLE  SE
# 1    1 Social transmission 1 914461.62843 NaN
# 2       2 Asocial: age_group -97461.53309 NaN
# 3 3 Asocial: dist_before_std      4.97328 NaN

# And we can compare with an asocial model containing the same ILVs:
model2_asocial <- oadaFit(nbdaData2_add, type="asocial")
model2_add@aicc
# [1] 155.7121
model2_asocial@aicc
#[1] 173.1955 # KG: this is larger, so as expected, the additive model is favored over the asocial model.

#From looking at the MLEs for the parameters we can see that s is estimated to be very large.
#Indeed if we look at the profile log-likelihood plot for s:
plotProfLik(which=1,model=model2_add,range=c(0,50),resolution=20)
plotProfLik(which=1,model=model2_add,range=c(0,200),resolution=20)
plotProfLik(which=1,model=model2_add,range=c(0,1000),resolution=20)
#We can see it appears to level out as s tends to infinity

#[(XXX KG: this comment is a holdover from the tutorial and doesn't apply to this model, I think...) However, this may well be an artifact of the asocial baseline chosen- we can see that adults are estimated to be faster than juveniles at asocial learning (coefficient of about [] for the MLE estimate for age_group), and juveniles/subadults are set as the baseline. This means s is being estimated relative to a very small baseline rate of asocial learning.
# We can reparameterise the model so that adults (of mean initial distance away from the carcass) are the baseline.]

age_group_rev <- 1-age_group
asoc2 <- c("age_group_rev","dist_before_std")
#Now juveniles/subadults = 1 and adults = 0. Adults is the base level, and the coefficient will refer to the difference from adults

nbdaData3_add <- nbdaData(label = "ExampleDiffusion2_reparam",
                          assMatrix = socNet1,
                          orderAcq = oa,
                          asoc_ilv = asoc2)
#Then fit the model:
model3_add <- oadaFit(nbdaData3_add)
#And get the output:
data.frame(Variable = model3_add@varNames,
           MLE = model3_add@outputPar,
           SE = model3_add@se)

# Variable          MLE  SE
# 1    1 Social transmission 1 6.713641e+17 NaN
# 2   2 Asocial: age_group_rev 1.691814e+01 NaN
# 3 3 Asocial: dist_before_std 8.088980e+00 NaN

# Now we get a small positive coefficient for juveniles/subadults, but a very large s value that's hard to interpret still (XXX KG note: this deviates substantially from the tutorial! May be due to the NAs?)

# We can also see that the AICc is fractionally better for model 3, showing that the optimum has been found more precisely. (KG: in their tutorial there was more of a difference. Here it doesn't seem to matter at all.)
model2_add@aicc
model3_add@aicc # KG oh wow, this is barely different at all  

#Note that the two models specified are the same, just parameterized differently- so you may even
#see the same AICc here. # KG yep that's what happened here

#You may also think we have somehow magically changed the importance of social transmission, given the very different estimation of s! But this is not the case, s is merely estimated relative to a much different baseline rate of asocial learning. This can be seen by comparing %ST for the two models:

nbdaPropSolveByST(model = model2_add)
# P(Network 1)  P(S offset) 
# 0.81045      0.00000 
nbdaPropSolveByST(model = model3_add)
# P(Network 1)  P(S offset) 
# 0.81095      0.00000 

#About the same in each case (and the minor discrepancy is just due to model2_add not quite finding the optimum)

#Now we also get a profile log-likelihood for s we can work with:
plotProfLik(which = 1, model = model3_add, range = c(0,10), resolution = 20)
plotProfLik(which = 1, model = model3_add, range = c(0,1000), resolution = 10)

#We can see the lower limit is between 100 and 300, and the upper from [] (XXX KG: actually, there is no upper limit evident at all.)

profLikCI(which = 1, model = model3_add, 
          upperRange = c(100, 10000),
          lowerRange = c(0,2))
# Lower CI   Upper CI 
# 1.999959 217.463418  # XXX QQQ I don't understand why this is able to calculate a confidence interval when the upper limit isn't even visible on the graph.

#We can again get the %ST corresponding to the upper and lower points of the 95% CI. However, before we do so, we need to look at the constrainedNBDAdata function- so we will come back to this after we have looked at the estimated ILV effects and 95% C.I.s for those.

#What about confidence intervals for the coefficients for the ILVs?
#We could get Wald 95% C.I.s by taking MLE +/- 1.96x SE

data.frame(Variable = model3_add@varNames,
           MLE = model3_add@outputPar, SE = model3_add@se,
           WaldLower = model3_add@outputPar-1.96*model3_add@se,
           WaldUpper = model3_add@outputPar+1.96*model3_add@se)

# Variable          MLE  SE WaldLower WaldUpper
# 1    1 Social transmission 1 6.713641e+17 NaN       NaN       NaN
# 2   2 Asocial: age_group_rev 1.691814e+01 NaN       NaN       NaN
# 3 3 Asocial: dist_before_std 8.088980e+00 NaN       NaN       NaN
#(XXX KG: can't continue this part of the tutorial because of all the NAs in this model. The comments from here on out are from before I fixed the ILV data)
#++++++++++++++++++++++++++++++

#We have already seen why the Wald C.I.s are misleading for s (XXX KG: we have?)
#For dist_before_std the Wald 95% C.I.s look like they *could* be reasonable (XXX KG: I think?)
#But for the juvenile effect it looks highly suspect, suggesting that a very large difference in either direction is plausible
#The reason for this is that asymmetrical profile log-likelihoods can also arise for ILVs.
#But we can use the same approach to getting profile likelihood C.I.s for the other parameters in the model

# INITIAL DISTANCE FROM CARCASS 
# (continuous variable) (analog to "age" in the tutorial, which is also continuous)

#We will start with age since that is easier to interpret in this case
#Since age is the third parameter in the model output, we set which = 3
plotProfLik(which = 3, model = model3_add, range = c(-5,5), resolution=20)

#We can see a little asymmetry here but it is not too bad, suggesting the Wald 95% C.I.s might be good in this case, but let us get the profile likelihood intervals anyway. We can see the lower limit is around -1 to 1 and the upper limit is... somewhere greater than 4 (KG)

profLikCI(which = 3, model = model3_add,
          lowerRange = c(-1,1), upperRange = c(0, 10))

# Lower CI   Upper CI 
# 0.04977953 6.76047449

#So even this mild asymmetry has made a bit of difference when compared to the Wald intervals. (KG: the wald interval for this one was (0.03231697, 1.239792), which is quite different, especially on the upper end!)

#The first thing to note is that 0 is not within the 95% C.I. so there is some evidence that initial distance from the carcass affects asocial learning rate (KG: I think this makes sense... how far they start from the carcass should affect their rate of finding it on their own. But is it in the right direction? I don't remember what we did about distance). (KG: Thinking about it more, we just took distance and standardized it. So we would expect greater distance to lead to a lower asocial rate of finding the carcass. We actually see the opposite here--greater initial distance (the day before, I think?) is associated with a slight increase in learning rate. That doesn't make a lot of sense, but I'm also not putting a huge amount of stock in it because it's a very small coefficient and also the confidence interval isn't very far away from 0, and also I'm not sure that this is even the right way of measuring space yet... just trying things out!)

#Let us examine the back-transformed effect and C.I.
#The MLE for distance is model3_add@outputPar[3]
exp(model3_add@outputPar[3])
#[1] 1.889013
#So the rate of asocial learning increases by an estimated factor of x1.89 for an increase of 1 S.D. in initial distance from the carcass, with back transformed 95% C.I. of
exp(model3_add@outputPar[3]-1.96*model3_add@se[3])
exp(model3_add@outputPar[3]+1.96*model3_add@se[3])
#1.032845 - 3.454895x

#So regardless, there is a positive effect of initial distance on rate of asocial learning

#What if we prefered to interpret effect sizes per km away from the carcass, rather than per SD? We simply divide the coefficient by the SD for distance (original variable, not the standardized version)

exp(model3_add@outputPar[3]/sd(dist_before))
#[1] 1.000066
#So the rate of asocial learning increases by an estimated factor of x1 for an increase of 1 m from the carcass (XXX KG: this absolutely can't be right. What gives?)
#back transformed 95% C.I. of

# XXX KG: need to figure out how to back-transform the CIs. Something isn't lining up when I try to do anything other than hard-coding them.

# AGE GROUP

#since age group (reversed) is the second parameter in the model output, we set which=2
plotProfLik(which = 2, model = model3_add, range = c(-20,5), resolution = 20)

#We can see the profile log-likelihood is indeed highly asymmetrical here too, explaining the large SE.
#We can also see there is something a bit odd going on at the left side of the plot, but first let us zoom in and find the upper limit

plotProfLik(which = 2, model = model3_add, range = c(-5,10), resolution = 20)
#We can see the upper limit is somewhere around 8, between 5 and 10. What about the lower (most negative) limit? (XXX KG: what's with the red dots?)

plotProfLik(which = 2, model = model3_add, range = c(-30,-10), resolution = 20)

# (XXX there's a long discussion in the tutorial of an invalid thing that doesn't actually happen in my data. Can return to this if needed.)

profLikCI(which = 2, model = model3_add, upperRange = c(-5,10))
# Lower CI Upper CI 
# 0.000000 8.113189 

#So the 95% CI includes zero, and there is not huge evidence that juveniles/subadults are slower at asocial learning than adults We back-transform the effect as follows:

exp(model3_add@outputPar[2])
# [1] 3.341119e-09

#This gives us the upper limit of the ratio (subadult or juvenile asocial learning rate)/(adult asocial learning rate)
#You may find it easier to report by reversing the sign so we are reporting the faster/slower category:
exp(model3_add@outputPar[2]*-1) # XXX KG I have no idea what's going on here

# XXX next to do: constraining models
#++++++++++++++++++++++++++++++

#############################################################################
# TUTORIAL 2.2
# ADDING TIME-VARYING ILVs TO AN OADA MODEL
# 1 diffusion
# 1 static network
# 1 Time constant ILV
# 1 Time varying ILV
#############################################################################

#Time-varying ILVs are easily added to an OADA
#It might take a bit of work to set up the nbdaData object, but once this is done the analysis proceeds as above.

#If one or more of our ILVs is time-varying, we need to set up an array for ALL of our ILVs specifying their values for every individual for every acquistion event.

#For example, let us assume that individuals' age groups don't vary over time, but their distance from the carcass does--we're going to incorporate the distance of each roost site to the carcass on each day.
activity_centers <- readRDS(here("test_dynamic_nbda/data/activity_centers.RDS"))

#Let us set up a matrix with rows = number of individuals, columns = number of acquisition event (in this case, 30 for both)
# XXX KG note: unclear whether the time-varying ILVs can only be categorical, or whether continuous/quantitative ones are accepted too. Going to try with continuous and see how it goes.
n_acq_events <- length(oa)
n_acq_events # 33
n_indivs # 33

ilvs <- ilvs %>%
  left_join((activity_centers %>%
  st_drop_geometry() %>%
  pivot_wider(id_cols = "local_identifier", names_from = "dateOnly", values_from = "dist_to_carc_m", names_prefix = "dist_")), by = "local_identifier") 
day1 <- ilvs$`dist_2024-04-21`
day2 <- ilvs$`dist_2024-04-22`
day3 <- ilvs$`dist_2024-04-23`
day4 <- ilvs$`dist_2024-04-24`
day5 <- ilvs$`dist_2024-04-25`
day6 <- ilvs$`dist_2024-04-26`

which_dates <- acq_roost_dates$timeperiod
n_acq_firstday <- sum(which_dates == 1)
n_acq_secondday <- sum(which_dates == 2)
n_acq_thirdday <- sum(which_dates == 3)
n_acq_fourthday <- sum(which_dates == 4)
  
#Let us set up a matrix with rows = number of individuals, columns = number of acquistion event (in this case, 33 for both)
treatment <- matrix(NA, nrow = n_indivs, ncol = n_acq_events)
for(i in 1:length(indivs)){
  # each row is an individual; each column is an acquisition event
  treatment[i,] <- c(rep(day1[i], n_acq_firstday), 
                     rep(day2[i], n_acq_secondday),
                     rep(day3[i], n_acq_thirdday),
                     rep(day4[i], n_acq_fourthday))
}

#let us also assume we want to include the age_group_rev variable from above
age_group_rev # reversed, so 0 is adult and 1 is juvenile/subadult
#We have to input this as a time varying ILV as well, even though it does not change
#However it is easy to create this using the byrow=F argument:
age_group_rev_TV <- matrix(age_group_rev, nrow = n_indivs, ncol = n_acq_events, byrow = F)
age_group_rev_TV # each row is an individual (so, we'll have the same value all the way across) and each column is an acquisition event (each column will be identical to the previous column)

asocTV <- c("treatment","age_group_rev_TV")

#We can then create the nbdaData object for the unconstrained model, specifying asocialTreatment="timevarying"

nbdaData3_TVadd <- nbdaData(label = "ExampleDiffusion2",
                            assMatrix = socNet1,
                            orderAcq = oa, 
                            asoc_ilv = asocTV,
                            asocialTreatment = "timevarying")
#Fit the model
model_socialTV <- oadaFit(nbdaData3_TVadd)
#Display the output
data.frame(Variable = model_socialTV@varNames,
           MLE = model_socialTV@outputPar,
           SE = model_socialTV@se) # why are the SE values NA here?




