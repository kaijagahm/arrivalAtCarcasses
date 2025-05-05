# Interpret results of targets pipeline
#2025-05-02 
library(tidyverse)
library(targets)
library(here)

tar_load(summaries_ilvs)
test <- summaries_ilvs %>% mutate(varNames = str_remove(varNames, "^[0-9]\\s"),
                                  varNames = str_remove(varNames, "_[0-9]+$")) %>%
  mutate(varNames = case_when(varNames == "Social transmission 1" ~ "S",
                              varNames == "Asocial: age_groups" ~ "Asoc: age",
                              varNames == "Asocial: std_roost_carc_distances" ~ "Asoc: dist",
                              varNames == "Social: age_groups" ~ "Soc: age",
                              .default = varNames),
         sig = case_when(0>(outputPar-se) & 0<(outputPar+se) ~ F, .default = T))

# Noticing that there are a lot of NaN's in the SE column. Are there any patterns?
test <- test %>%
  mutate(nan_se = ifelse(is.nan(se), T, F))

test %>%
  ggplot(aes(x = outputPar, y = nan_se, col = varNames))+
  geom_point() # No clear pattern with outputPar

# Asking chatGPT why there are NaNs for SE:
# Hessian may not have converged
tar_load(Mods_N.RD_So_ilvs)
summary(Mods_N.RD_So_ilvs[[1]])
str(Mods_N.RD_So_ilvs[[1]])
Mods_N.RD_So_ilvs[[1]]@hessian # aha, it looks like there are some NaN values in the hessian.
Mods_N.RD_So_ilvs[[4]]@hessian # this one had NaNs for one of the coefficients only, not all of them. And here the hessian looks totally fine.
# Maybe in this case there's something weird about the ages?
table(Mods_N.RD_So_ilvs[[4]]@nbdadata[[1]]@asocILVdata[,1]) # ages--looks diverse enough
hist(Mods_N.RD_So_ilvs[[4]]@nbdadata[[1]]@asocILVdata[,2]) # distances--also looks fine
# And this one has 36 individuals participating in the acquisition, so low sample size doesn't seem like the problem either. The coefficient that was the problem was the social effect of age.

Mods_N.RD_So_ilvs[[23]]@hessian # this one also has all the SEs as NA, but its hessian looks fine. What's going on?

# Hypothesis: if all coefs are NaN, the hessian will have pro

# I wonder if this happens when the network is all 1s or all 0s, or close to it?
str(Mods_N.RD_So_ilvs[[1]]@nbdadata,3)
sum(Mods_N.RD_So_ilvs[[1]]@nbdadata[[1]]@assMatrix)/length(Mods_N.RD_So_ilvs[[1]]@nbdadata[[1]]@assMatrix) # 23% matrix density. That doesn't seem to be the problem.

table(Mods_N.RD_So_ilvs[[1]]@nbdadata[[1]]@asocILVdata[,1]) # ages
hist(Mods_N.RD_So_ilvs[[1]]@nbdadata[[1]]@asocILVdata[,2]) # distances
# neither of these look bad a priori...
tar_load(oas_nbda_updated)
length(oas_nbda_updated[[1]]) # 69 individuals; so this isn't just due to few individuals arriving at this carcass.
# XXX start here with trying to narrow down what's going on here.

test %>%
  ggplot(aes(x = outputPar, y = carcID, col = network))+
  geom_segment(aes(x = outputPar-se, xend = outputPar + se, linetype = sig), position = position_dodge(width = 0.5))+
  geom_vline(aes(xintercept = 0), alpha = 0.2, linetype = 2)+
  geom_point(aes(pch = sig), position = position_dodge(width = 0.5))+
  scale_shape_manual(values = c(1, 19))+
  scale_linetype_manual(values = c(2, 1))+
  facet_wrap(~varNames)+
  scale_x_continuous(limits = c(-4, 4))+
  theme_classic() # okay, so we see hardly any social effect, but we do see some carcasses with significant age and/or distance effects on social transmission. We see almost no carcasses with significant age effects on social transmission.

# Only S
test %>%
  filter(varNames == "S") %>%
  ggplot(aes(x = outputPar, y = carcID, col = network))+
  geom_segment(aes(x = outputPar-se, xend = outputPar + se, linetype = sig), position = position_dodge(width = 0.5))+
  geom_vline(aes(xintercept = 0), alpha = 0.2, linetype = 2)+
  geom_point(aes(pch = sig), position = position_dodge(width = 0.5))+
  scale_shape_manual(values = c(1, 19))+
  scale_linetype_manual(values = c(2, 1))+
  scale_x_continuous(limits = c(-0.6, 4))+
  theme_classic()

# Now let's examine the same thing but with models that actually include both roost and flight networks, instead of doing them in separate models like we did here.

tar_load(summary_2nets)
test <- summary_2nets %>% 
  mutate(varNames = str_remove(varNames, "^[0-9]\\s"),
                                  varNames = str_remove(varNames, "_[0-9]+$")) %>%
  mutate(varNames = case_when(varNames == "Social transmission 1" ~ "S1",
                              varNames == "Social transmission 2" ~ "S2",
                              
                              varNames == "Asocial: age_groups" ~ "Asoc: age",
                              varNames == "Asocial: std_roost_carc_distances" ~ "Asoc: dist",
                              varNames == "Social: age_groups" ~ "Soc: age",
                              .default = varNames),
         sig = case_when(0>(outputPar-se) & 0<(outputPar+se) ~ F, .default = T)) # but in this case, se is always NaN. Why?

test %>%
  ggplot(aes(x = outputPar, y = carcID, col = network))+
  #geom_segment(aes(x = outputPar-se, xend = outputPar + se, linetype = sig), position = position_dodge(width = 0.5))+
  geom_vline(aes(xintercept = 0), alpha = 0.2, linetype = 2)+
  geom_point(#aes(pch = sig), 
             position = position_dodge(width = 0.5),
             pch = 4)+ # using x's because we don't have significance information at this point.
  scale_shape_manual(values = c(1, 19))+
  #scale_linetype_manual(values = c(2, 1))+
  facet_wrap(~varNames)+
  scale_x_continuous(limits = c(-4, 4))+
  theme_classic() # It's really suspicious to me that in the model with both networks, the s parameter drops to 0. What is going on here? Especially in S2 (flight), literally for every carcass it's 0, whereas in the  models with individual networks, we had at least a few where it was significant.


# They both come from N.FD and N.RD, so the networks themselves aren't the problem. There could possibly be a problem with how we're putting the networks into the nbdaData object.
