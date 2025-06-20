# Interpret results of targets pipeline
#2025-05-02 
library(tidyverse)
library(targets)
library(here)
library(mapview)

tar_load(summaries)
tar_load(summaries_ilvs)
s <- summaries %>% mutate(varNames = str_remove(varNames, "^[0-9]\\s"),
                          varNames = str_remove(varNames, "_[0-9]+$")) %>%
     mutate(varNames = case_when(varNames == "Social transmission 1" ~ "S",
                                 .default = varNames),
            sig = case_when(0>(outputPar-se) & 0<(outputPar+se) ~ F,
                            outputPar == 0 ~ F,
                            .default = T))

si <- summaries_ilvs %>% mutate(varNames = str_remove(varNames, "^[0-9]\\s"),
                                  varNames = str_remove(varNames, "_[0-9]+$")) %>%
  mutate(varNames = case_when(varNames == "Social transmission 1" ~ "S",
                              varNames == "Asocial: age_groups" ~ "Asoc: age",
                              varNames == "Asocial: std_roost_carc_distances" ~ "Asoc: dist",
                              varNames == "Social: age_groups" ~ "Soc: age",
                              .default = varNames),
         sig = case_when(0>(outputPar-se) & 0<(outputPar+se) ~ F,
                         outputPar == 0 ~ F,
                         .default = T))

si %>%
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
si %>%
  filter(varNames == "S") %>%
  ggplot(aes(x = outputPar, y = carcID, col = network))+
  geom_segment(aes(x = outputPar-se, xend = outputPar + se, linetype = sig), position = position_dodge(width = 0.5))+
  geom_vline(aes(xintercept = 0), alpha = 0.2, linetype = 2)+
  geom_point(aes(pch = sig), position = position_dodge(width = 0.5))+
  scale_shape_manual(values = c(1, 19))+
  scale_linetype_manual(values = c(2, 1))+
  scale_x_continuous(limits = c(-0.6, 4))+
  theme_classic()+
  ggtitle("Social effect (with ILVs)")

s %>%
  filter(varNames == "S") %>%
  ggplot(aes(x = outputPar, y = carcID, col = network))+
  geom_segment(aes(x = outputPar-se, xend = outputPar + se, linetype = sig), position = position_dodge(width = 0.5))+
  geom_vline(aes(xintercept = 0), alpha = 0.2, linetype = 2)+
  geom_point(aes(pch = sig), position = position_dodge(width = 0.5))+
  scale_shape_manual(values = c(1, 19))+
  scale_linetype_manual(values = c(2, 1))+
  scale_x_continuous(limits = c(-0.6, 4))+
  theme_classic()+
  ggtitle("Social effect (without ILVs)")

# Now let's examine the same thing but with models that actually include both roost and flight networks, instead of doing them in separate models like we did here.

tar_load(summary_2nets)
tar_load(summary_2nets_ilvs)
s2 <- summary_2nets %>% 
  mutate(varNames = str_remove(varNames, "^[0-9]\\s"),
         varNames = str_remove(varNames, "_[0-9]+$")) %>%
  mutate(varNames = case_when(varNames == "Social transmission 1" ~ "S1",
                              varNames == "Social transmission 2" ~ "S2",
                              .default = varNames),
         sig = case_when(0>(outputPar-se) & 0<(outputPar+se) ~ F, .default = T))

s2i <- summary_2nets_ilvs %>% 
  mutate(varNames = str_remove(varNames, "^[0-9]\\s"),
                                  varNames = str_remove(varNames, "_[0-9]+$")) %>%
  mutate(varNames = case_when(varNames == "Social transmission 1" ~ "S1",
                              varNames == "Social transmission 2" ~ "S2",
                              
                              varNames == "Asocial: age_groups" ~ "Asoc: age",
                              varNames == "Asocial: std_roost_carc_distances" ~ "Asoc: dist",
                              varNames == "Social: age_groups" ~ "Soc: age",
                              .default = varNames),
         sig = case_when(0>(outputPar-se) & 0<(outputPar+se) ~ F, .default = T)) # but in this case, se is always NaN. Why?

s2 %>%
  ggplot(aes(x = outputPar, y = carcID, col = network))+
  geom_vline(aes(xintercept = 0), alpha = 0.2, linetype = 2)+
  geom_point(#aes(pch = sig), 
             position = position_dodge(width = 0.5),
             pch = 4)+ # using x's because we don't have significance information at this point.
  scale_shape_manual(values = c(1, 19))+
  facet_wrap(~varNames)+
  scale_x_continuous(limits = c(-4, 4))+
  theme_classic()+ # why do we have absolutely no social transmission effect here?
  ggtitle("Two networks; no ILVs")

s2i %>%
  ggplot(aes(x = outputPar, y = carcID, col = network))+
  geom_vline(aes(xintercept = 0), alpha = 0.2, linetype = 2)+
  geom_point(#aes(pch = sig), 
    position = position_dodge(width = 0.5),
    pch = 4)+ # using x's because we don't have significance information at this point.
  scale_shape_manual(values = c(1, 19))+
  facet_wrap(~varNames)+
  scale_x_continuous(limits = c(-4, 4))+
  theme_classic() +
  ggtitle("Two networks; asocial ILVs")

# Wild carcasses ----------------------------------------------------------
tar_load(sums_RD_wild)
tar_load(wild_carcasses_5)
mapview(wild_carcasses_5)
# The carcasses we're going to focus on now are 1, 2, 5, 9, 16, 18, 22, 23, 57, based on looking at the map because we have reason to believe that these are real.

sums_RD_wild %>% filter(carcID %in% c(1, 2, 5, 9, 16, 18, 22, 23, 57)) %>%
  ggplot(aes(x = carcID, y = outputPar))+
  geom_point()+
  theme_minimal()+
  geom_segment(aes(y = outputPar - se, yend = outputPar + se))

# none of these show any social effect (well, one of them had a teeny tiny effect, but the SE overlaps 0). The largest social effect was 0.05, and that was for carcass 47, which wasn't one of our focal carcasses (not sure where it is).

# How many individuals were involved in each of these diffusions?
tar_load(n_indivs_wild) # this is a good number of individuals!! None of these seem to be suffering from a too-small sample of individuals. 
