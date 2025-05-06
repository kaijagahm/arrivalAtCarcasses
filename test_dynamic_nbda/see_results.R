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
         sig = case_when(0>(outputPar-se) & 0<(outputPar+se) ~ F,
                         outputPar == 0 ~ F,
                         .default = T))

# Noticing that there are a lot of NaN's in the SE column. Are there any patterns?
test <- test %>%
  mutate(nan_se = ifelse(is.nan(se), T, F))

test %>%
  ggplot(aes(x = outputPar, y = nan_se, col = varNames))+
  geom_point() # The majority of the NaN values for SE are when the parameter is estimated at exactly 0.

test %>%
  filter(outputPar < 20) %>%
  ggplot(aes(x = outputPar, y = carcID, col = network))+
  geom_segment(aes(x = outputPar-se, xend = outputPar + se, linetype = sig), position = position_dodge(width = 0.5))+
  geom_vline(aes(xintercept = 0), alpha = 0.2, linetype = 2)+
  geom_point(aes(pch = sig), position = position_dodge(width = 0.5))+
  scale_shape_manual(values = c(1, 19))+
  scale_linetype_manual(values = c(2, 1))+
  facet_wrap(~varNames, scales = "free_x")+
  #scale_x_continuous(limits = c(-4, 4))+
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
