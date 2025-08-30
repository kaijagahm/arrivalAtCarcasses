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
nrow(test) == 4*nrow(stn)

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
  facet_wrap(~year, scales = "free_y", nrow = 3)+
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

# It's worth asking what explains these very high values of social transmission, and why they seem more common, generally, for the weighted network rather than the unweighted network.

# But in order to be sure, we need to work on our seeded demonstrators first!