library(tidyverse)
library(targets)

tar_load(stats)
tar_load(stats_wild)
tar_load(stn)
tar_load(wild)
tar_load(stn_carcs)
tar_load(wild_carcs)
tar_load(ns)
tar_load(ns_noseeds)
tar_load(ns_wild)
tar_load(ns_wild_noseeds)

stats <- stats %>% mutate(stn_wild = "stn")
stats_wild <- stats_wild %>% mutate(stn_wild = "wild")

ns_df <- data.frame(n = c(ns,  ns_noseeds), carcID = rep(map_dbl(stn_carcs, "carcID"), 2),
                 seeds = c(rep(TRUE, length(ns)), rep(FALSE, length(ns_noseeds))))

ns_df_wild <- data.frame(n = c(ns_wild,  ns_wild_noseeds), carcID = rep(map_dbl(wild_carcs, "carcID"), 2),
                    seeds = c(rep(TRUE, length(ns_wild)), rep(FALSE, length(ns_wild_noseeds))))

stats <- left_join(stats, ns_df, by = c("carcID", "seeds")) %>% 
  left_join(stn, by = "carcID")
stats_wild <- left_join(stats_wild, ns_df_wild, by = c("carcID", "seeds")) %>% 
  left_join(wild, by = "carcID")

stats_all <- bind_rows(stats, stats_wild) %>%
  mutate(lower = outputPar - se, upper = outputPar + se, sig = ifelse(lower > 0 & !is.na(lower), T, F)) %>%
  mutate(year = lubridate::year(date))

stats_all %>%
  filter(!is.na(outputPar), stn_wild == "stn", binwt == "wt") %>%
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
  scale_color_manual(values = c("firebrick4", "dodgerblue4"))+
  labs(title = "SFS carcasses") # not actually missing data for 2022, it's just that the estimates/SEs are all really high so they don't show up.

stats_all %>%
  filter(!is.na(outputPar), stn_wild == "wild", binwt == "wt", seeds == T) %>%
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
  scale_color_manual(values = c("firebrick4", "dodgerblue4"))+
  labs(title = "Wild carcasses") # not actually missing data for 2022, it's just that the estimates/SEs are all really high so they don't show up.

# What about the relationship to the number of individuals in the diffusion?
# Does number of individuals predict significant social transmission?
stats_all %>%
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
  labs(title = "No seeds", y = "Social transmission detected")+
  scale_y_continuous(breaks = c(0, 1))
# There seems to be a relationship between the number of individuals and the probability of detecting social transmission

stats_all %>%
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
  labs(title = "Seeds", y = "Social transmission detected")+ # similar results once we account for seeds
  scale_y_continuous(breaks = c(0, 1))
  
# Now, is there a relationship between the number of individuals and the strength of social effect, for the ones where an effect was detected?
stats_all %>%
  filter(sig) %>%
  ggplot(aes(x = n, y = outputPar, col = interaction(type, binwt)))+
  geom_point()+
  theme_minimal()+
  facet_wrap(~seeds)+
  scale_color_manual(values = c("firebrick1", "skyblue", "firebrick4", "dodgerblue4"))+
  geom_smooth(method = "lm") # seems to be a negative relationship between the number of individuals and the output parameter.
# Note that we "can't compare the results" from with/without seeds since they are fitted to different orders of arrival. I'm not sure what that means exactly... like does this mean we can't pairwise compare the output parameters, or we can't compare the percent social transmission estimates, or we can't do regressions, or what? Will need to go back and re-read the Hasenjager paper.

# Exploring two particular carcasses --------------------------------------
plots_stn <- readRDS(here("data/plots_stn.RDS"))
plots_stn[[1]] #carcID 4202095 # seem to be many vultures nearby for e.g. 2 hours before. Expect many seeds.
# 2022-11-14 12:46:56
plots_stn[[2]] #carcID 4203377 # seem to be very few vultures nearby. Expect few seeds.
# 2022-11-15 11:01:40

# these two carcasses are only a day apart.

tar_load(nd1) # should be the first and second of these
tar_load(stn_carcs_tcv)
map_dbl(stn_carcs_tcv[1:2], "carcID") # sure enough!

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
stats_all %>%
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
# First of all, to clarify, results may be NA or NULL for many reasons, including model didn't run for whatever reason, or not enough individuals, or whatever else.
## How many are attributable to having too few individuals?
propnull <- stats_all %>%
  #filter(is.na(outputPar) & is.na(soc)) %>%
  group_by(year, seeds, stn_wild, type, binwt) %>%
  summarize(n = n(),
            prop_null = mean(is.na(propsolve)))
propnull

propnull %>%
  filter(stn_wild == "stn") %>%
  ggplot(aes(x = type, y = prop_null, fill = interaction(type, binwt)))+
  scale_fill_manual(values = c("firebrick1", "skyblue", "firebrick4", "dodgerblue4"))+
  geom_col(position = position_dodge())+
  facet_grid(rows = vars(year), cols = vars(seeds))+
  labs(title = "SFS carcasses") # no difference between the different model types at all, so the reason the models are NULL probably has to do with the underlying dataset rather than the modeling method/the particular networks used. Also notice that there are fewer NULL results in 2023 than the other years--could be due to number of individuals tracked?

propnull %>%
  filter(stn_wild == "wild") %>%
  ggplot(aes(x = type, y = prop_null, fill = interaction(type, binwt)))+
  scale_fill_manual(values = c("firebrick1", "skyblue", "firebrick4", "dodgerblue4"))+
  geom_col(position = position_dodge())+
  facet_grid(rows = vars(year), cols = vars(seeds))+
  labs(title = "Wild carcasses") # most of these ran fine! Interesting that the failures were mostly with the station carcasses. Maybe due to lat/long reassignment?

# Solutions:
## 1. Some of the carcasses could be in the wrong place still, so we would not be detecting arrivals. Go work on the placement of the station carcasses again.
## 2. Data cleaning could be a mess. Add data cleaning to the targets pipeline

## 3. Check whether the ones that are NULL are all/mostly the ones with few individuals (restricting to SFS cumul for simplicity)
stats_all %>%
  filter(stn_wild == "stn", is.na(propsolve), type == "cumul") %>%
  select(propsolve, type, binwt, seeds, carcID, stn_wild, n, year, stationName) %>%
  arrange(carcID, year, binwt, seeds) %>%
  View() # Yes!! All the ones that failed here had 0 or 1 individuals.

# Let's do the same check, this time expanding to all model types and both stn/wild.
stats_all %>%
  filter(is.na(propsolve)) %>%
  select(propsolve, type, binwt, seeds, carcID, stn_wild, n, year, stationName) %>%
  arrange(carcID, year, binwt, seeds) %>%
  View() # Yep! These all still failed because they had 0 or 1 individual.

# problem solved! As long as we interpret the NAs to 0s caveat correctly, these models should be legit.