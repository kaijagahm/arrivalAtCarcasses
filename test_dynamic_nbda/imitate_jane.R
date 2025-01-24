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

tar_load(aca)
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


#We need to combine these in a 4 dimensional array, with the 4th dimension for time periods
n_timeperiods <- 4

#Create the empty array
socNet <- array(NA, dim = c(n_indivs, n_indivs, 1, n_timeperiods))
#Slot in the network for each time period
socNet[,,,1]<-r1
socNet[,,,2]<-r2
socNet[,,,3]<-r3
socNet[,,,4]<-r4

## (For comparison: static network)
socNet_static <- array(NA, dim = c(n_indivs, n_indivs, 1, 1)) # just 1 time period
socNet_static[,,,1]<-r_static_mn

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
model_social@aicc # XXX need to do this one with the static network for comparison
# [1] 158.0052

# here the static model actually fits significantly better than the dynamic model (lower AICC)