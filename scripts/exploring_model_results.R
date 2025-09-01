library(tidyverse)
library(targets)

tar_load(stats)
tar_load(stn)
tar_load(ns)

ns <- data.frame(n = ns, carcID = stn$carcID)

test <- stats %>% 
  mutate(lower = outputPar - se, upper = outputPar + se, sig = ifelse(lower > 0 & !is.na(lower), T, F)) %>%
  left_join(ns) %>%
  left_join(stn) %>%
  mutate(year = lubridate::year(date))

dim(test)
nrow(test) == 8*nrow(stn)

test %>%
  filter(!is.na(outputPar)) %>%
  mutate(lower = case_when(!sig ~ NA, .default = lower),
         upper = case_when(!sig ~ NA, .default = upper)) %>%
  filter(outputPar < 50) %>%
  ggplot(aes(x = factor(carcID), y = outputPar, col = interaction(type, binwt)))+
  geom_hline(aes(yintercept = 0), linetype = 3, color = "black")+
  geom_segment(aes(y = lower, yend = upper))+
  geom_point(aes(pch = sig))+
  scale_shape_manual(values = c(1, 19))+
  #scale_linetype_manual(values = c(2, 1))+
  theme_classic()+
  coord_flip()+
  facet_grid(rows = vars(year), cols = vars(seeds), scales = "free_y")+
  scale_color_manual(values = c("firebrick1", "skyblue", "firebrick4", "dodgerblue4")) # not actually missing data for 2022, it's just that the estimates/SEs are all really high so they don't show up.

# What about the relationship to the number of individuals in the diffusion?
# Does number of individuals predict significant social transmission?
test %>%
  mutate(sig_num = ifelse(sig, 1, 0)) %>%
  ggplot(aes(x = n, y = sig_num, color = interaction(type, binwt))) +
  geom_point(position = position_jitter(height = 0.02), alpha = 0.2, size = 2) + 
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)+
  theme_minimal()+
  facet_grid(rows = vars(binwt), cols = vars(type))+
  scale_color_manual(values = c("firebrick1", "skyblue", "firebrick4", "dodgerblue4"))+
  theme(legend.position = "bottom") # There doesn't seem to be a relationship between the number of individuals in the diffusion and the likelihood of detecting a significant effect, for any of the network types.
  
# Now, is there a relationship between the number of individuals and the strength of social effect, for the ones where an effect was detected?
test %>%
  filter(sig) %>%
  ggplot(aes(x = n, y = outputPar, col = interaction(type, binwt)))+
  geom_point()+
  theme_minimal()+
  geom_smooth(method = "lm") # there appears to be a slight, but not significant, effect of n on outputPar for the cumulative networks. I suspect that if we get rid of the high outlier (one carcass, where the outputPar was above 40), that we will not see any effect anymore.

test %>%
  filter(outputPar < 40, sig) %>%
  ggplot(aes(x = n, y = outputPar, col = interaction(type, binwt)))+
  geom_point()+
  theme_minimal()+
  geom_smooth(method = "lm") # huh, there is still a hint of the effect. Not sure it's significant.

# Exploring two particular carcasses --------------------------------------
plots_stn <- readRDS(here("data/plots_stn.RDS"))
plots_stn[[1]] #carcID 4202095 # seem to be many vultures nearby for e.g. 2 hours before. Expect many seeds.
# 2022-11-14 12:46:56
plots_stn[[2]] #carcID 4203377 # seem to be very few vultures nearby. Expect few seeds.
# 2022-11-15 11:01:40

# these two carcasses are only a day apart.

tar_load(nd1) # should be the first and second of these
tar_load(stn_carcs_tcv)
map_dbl(nstn_carcs_tcvmap_dbl(nd1[1:2], "carcID")) # sure enough!

# Carcass intro -----------------------------------------------------------
mycarcs <- map_dbl(nd1[1:2], "carcID")
## Carcass 1--Hever
mapview(stn_carcs_tcv[[1]])
str(nd1[[1]], 1) # 94 total; 44 found (46.8% of the tracked population). 31% of the population were seeds. 
mean(nd1[[1]]$oa_indivs %in% nd1[[1]]$seed_indivs) # 68.2% of the individuals that found the carc were seeds...
sum(!(nd1[[1]]$oa_indivs %in% nd1[[1]]$seed_indivs)) #... leaving only 14 individuals that found the carcass without having been near it in the 30 minutes before it was placed.
mapview()
# check that the seeds are correct
nd1[[1]]$seed_indivs
nd1[[1]]$all_indivs_sorted
as.logical(nd1[[1]]$seeds_vec)
seed_indivs_derived <- as.character(nd1[[1]]$all_indivs_sorted[as.logical(nd1[[1]]$seeds_vec)])
class(nd1[[1]]$seed_indivs)
class(seed_indivs_derived)
all(seed_indivs_derived %in% nd1[[1]]$seed_indivs)
all(nd1[[1]]$seed_indivs %in% seed_indivs_derived) # okay yeah, they match, so I did this correctly

## Carcass 2--Daroch
mapview(stn_carcs_tcv[[2]])
str(nd1[[2]], 1) # 94 total; 36 found (38.3% of the tracked population). 31% of the population were seeds. 
mean(nd1[[2]]$oa_indivs %in% nd1[[2]]$seed_indivs) # 13.8% of the individuals that found the carc were seeds...
sum(!(nd1[[2]]$oa_indivs %in% nd1[[2]]$seed_indivs)) #... leaving 31 individuals that found the carcass without having been near it in the 30 minutes before it was placed. That seems like a very reasonable number of individuals! So I'm not sure why this didn't work well with the seeds.

# Models with/without seeds -----------------------------------------------------------
test %>%
  filter(carcID %in% mycarcs) %>%
  ggplot(aes(x = carcID, color = interaction(type, binwt)))+
  theme_classic()+
  geom_point(aes(y = outputPar))+
  scale_color_manual(values = c("firebrick1", "skyblue", "firebrick4", "dodgerblue4"))
# so the other ones don't seem to be missing just because they were non-significant--the model outputs seem to have been NA in the first place. Why is this? There are many errors that could have produced a null result, so let's look at what they are.

# Gonna dig into both of these, but especially the second one, since it should have had 31 individuals left, which should be plenty to do the diffusion.
# Started digging into this and it looks fine and lined up. But then I went back to the tutorial and realized I'm supposed to remove those individuals from the order of acquisition!


# XXX also to look into--I'm not sure the ns lined up correctly. 9 indivs sounds wrong.

