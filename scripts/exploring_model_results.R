library(tidyverse)
library(targets)

tar_load(stats)
tar_load(stats_wild)
tar_load(stn)
tar_load(wild)
tar_load(ns)
tar_load(ns_wild)

stats <- stats %>% mutate(stn_wild = "stn")
stats_wild <- stats_wild %>% mutate(stn_wild = "wild")

ns <- data.frame(n = ns, carcID = stn$carcID)
ns_wild <- data.frame(n = ns_wild, carcID = wild$carcID)

stats <- left_join(stats, ns) %>% left_join(stn)
stats_wild <- left_join(stats_wild, ns_wild) %>% left_join(wild)

stats_all <- bind_rows(stats, stats_wild)

test <- stats_all %>% 
  mutate(lower = outputPar - se, upper = outputPar + se, sig = ifelse(lower > 0 & !is.na(lower), T, F)) %>%
  mutate(year = lubridate::year(date))

test %>%
  filter(!is.na(outputPar), stn_wild == "stn") %>%
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
  scale_color_manual(values = c("firebrick1", "skyblue", "firebrick4", "dodgerblue4"))+
  labs(title = "SFS carcasses") # not actually missing data for 2022, it's just that the estimates/SEs are all really high so they don't show up.

test %>%
  filter(!is.na(outputPar), stn_wild == "wild") %>%
  mutate(lower = case_when(!sig ~ NA, .default = lower),
         upper = case_when(!sig ~ NA, .default = upper)) %>%
  filter(outputPar < 30) %>%
  ggplot(aes(x = factor(carcID), y = outputPar, col = interaction(type, binwt)))+
  geom_hline(aes(yintercept = 0), linetype = 3, color = "black")+
  geom_segment(aes(y = lower, yend = upper))+
  geom_point(aes(pch = sig))+
  scale_shape_manual(values = c(1, 19))+
  #scale_linetype_manual(values = c(2, 1))+
  theme_classic()+
  coord_flip()+
  facet_grid(rows = vars(year), cols = vars(seeds), scales = "free_y")+
  scale_color_manual(values = c("firebrick1", "skyblue", "firebrick4", "dodgerblue4"))+
  labs(title = "Wild carcasses") # not actually missing data for 2022, it's just that the estimates/SEs are all really high so they don't show up.

# What about the relationship to the number of individuals in the diffusion?
# Does number of individuals predict significant social transmission?
test %>%
  filter(!seeds) %>%
  mutate(sig_num = ifelse(sig, 1, 0)) %>%
  ggplot(aes(x = n, y = sig_num, color = interaction(type, binwt), 
             linetype = stn_wild)) +
  geom_point(position = position_jitter(height = 0.02), alpha = 0.2, size = 2) + 
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)+
  theme_minimal()+
  facet_grid(rows = vars(binwt), cols = vars(type))+
  scale_color_manual(values = c("firebrick1", "skyblue", "firebrick4", "dodgerblue4"))+
  theme(legend.position = "bottom") + 
  labs(title = "No seeds")# For the station carcasses, there seems to be no relationship between number of individuals involved in the diffusion and likelihood of detecting social transmission. But for the wild carcasses, more individuals in the diffusion significantly predicts us detecting social transmission.
# Now of course, the direction of causality could be the other way around. Maybe having social transmission causes more individuals to arrive; that would be totally plausible. Still supports a different mechanism.
# Framing for intro--conservation--it has been claimed that carcasses at smaller, less frequently provisioned stations will be more likely to mimic natural conditions.

test %>%
  filter(seeds) %>%
  mutate(sig_num = ifelse(sig, 1, 0)) %>%
  ggplot(aes(x = n, y = sig_num, color = interaction(type, binwt), 
             linetype = stn_wild)) +
  geom_point(position = position_jitter(height = 0.02), alpha = 0.2, size = 2) + 
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE)+
  theme_minimal()+
  facet_grid(rows = vars(binwt), cols = vars(type))+
  scale_color_manual(values = c("firebrick1", "skyblue", "firebrick4", "dodgerblue4"))+
  theme(legend.position = "bottom") + 
  labs(title = "Seeds") # similar results once we account for seeds
  
# Now, is there a relationship between the number of individuals and the strength of social effect, for the ones where an effect was detected?
test %>%
  filter(sig) %>%
  ggplot(aes(x = n, y = outputPar, col = interaction(type, binwt)))+
  geom_point()+
  theme_minimal()+
  facet_wrap(~seeds)+
  scale_color_manual(values = c("firebrick1", "skyblue", "firebrick4", "dodgerblue4"))+
  geom_smooth(method = "lm") # seems to be a negative relationship between the number of individuals and the output parameter.
# Note that we "can't compare the results" from with/without seeds since they are fitted to different orders of arrival. I'm not sure what that means exactly... like does this mean we can't pairwise compare the output parameters, or we can't compare the percent social transmission estimates, or we can't do regressions, or what? Will need to go back and re-read the Hasenjager paper.

test %>%
  filter(outputPar < 40, sig) %>%
  ggplot(aes(x = n, y = outputPar, col = interaction(type, binwt)))+
  geom_point()+
  theme_minimal()+
  facet_wrap(~seeds)+
  scale_color_manual(values = c("firebrick1", "skyblue", "firebrick4", "dodgerblue4"))+
  geom_smooth(method = "lm") # still a negative effect after removing the high ones

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
  filter(!is.na(outputPar), carcID %in% mycarcs) %>%
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
  facet_wrap(~seeds, scales = "free_y")+
  theme(legend.position = "bottom")+
  scale_color_manual(values = c("skyblue", "dodgerblue4")) # interesting! so for carcass 1, the result is approx the same. For carcass 2, we detect social transmission when we consider the seeds, but we don't detect it otherwise.
# Is this exactly the kind of comparison that I'm not supposed to be doing??

# XXX also to look into--I'm not sure the ns lined up correctly. 9 indivs sounds wrong.

# Okay, time to run the same thing for the wild carcasses.

# Why are so many station carcasses invalid for NBDA? --------
## Hypothesis: because after removing the seeds, they don't have enough individuals



