# Script for exploring the data created in prepare_data.R
# This is data for the INPA carcasses from the focal months in 2023 and 2024
# Packages
library(mapview)
library(sf)
library(tidyverse)

# Load data
load(here("test_dynamic_nbda/data/fl_allday_bin_fixed.Rda"))
load(here("test_dynamic_nbda/data/fl_cumulative_bin_fixed.Rda"))
load(here("test_dynamic_nbda/data/fl_1hr_bin_fixed.Rda"))
load(here("test_dynamic_nbda/data/fl_3hr_bin_fixed.Rda"))
load(here("test_dynamic_nbda/data/roosts_bin_fixed.Rda"))
load(here("test_dynamic_nbda/data/oa.Rda"))
load(here("test_dynamic_nbda/data/oa_num.Rda"))
load(here("test_dynamic_nbda/data/firsts.Rda"))
load(here("test_dynamic_nbda/data/inpa_carcs.Rda"))
load(here("test_dynamic_nbda/data/roosts_bin_nets.Rda"))
load(here("test_dynamic_nbda/data/fl_allday_bin_nets.Rda"))
load(here("test_dynamic_nbda/data/fl_cumulative_bin_nets.Rda"))
load(here("test_dynamic_nbda/data/fl_1h_bin_nets.Rda"))
load(here("test_dynamic_nbda/data/fl_3h_bin_nets.Rda"))

# 1. I noticed that a lot of carcasses don't have any tagged birds that ever go there. Why? What's going on?
## Maybe this is explained by the hour of the day when the carcass is placed?
df <- bind_rows(inpa_carcs) %>%
  mutate(time_of_day = lubridate::hour(datetime),
         year = lubridate::year(datetime)) %>%
  mutate(number_of_firsts = map_dbl(firsts, ~.x %>% filter(!is.na(local_identifier)) %>% nrow(.)))
df %>%
  ggplot(aes(x = time_of_day, y = number_of_firsts))+
  geom_point(aes(size = carcassWeight), alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~year)+
  theme_minimal() # doesn't seem to be related to hour of day

## maybe it's related to the size of the carcass
df %>%
  ggplot(aes(x = carcassWeight, y = number_of_firsts))+
  geom_point(aes(size = time_of_day), alpha = 0.5)+
  geom_smooth(method = "lm")+
  facet_wrap(~year)+
  theme_minimal() # this also doesn't produce a clear relationship.

## Let's see a map of the carcasses, colored by the number of individuals that visit over the course of 4-ish days
mapview(df %>% filter(year == 2023), zcol = "number_of_firsts")
mapview(df %>% filter(year == 2024), zcol = "number_of_firsts")
## There definitely seems to be some relationship between the centrality of the carcass and the number of visits, but I'm surprised at how stark the numbers are, given how many carcasses are placed.

# For the networks, we are already only dealing with the carcasses that have visits from vultures
load(here("test_dynamic_nbda/data/has_visits.Rda"))
carcs <- inpa_carcs[has_visits] # get the carcasses corresponding to the networks, in case we need them

# select a random carcass
rand <- 12
rand_carc <- carcs[[rand]]
rand_oa <- oa[[rand]]

# get roosting and flight networks
rand_r <- roosts_bin_fixed[[rand]]
length(rand_r)

rand_f_a <- fl_allday_bin_fixed[[rand]]
rand_f_c <- fl_cumulative_bin_fixed[[rand]]
rand_f_1h <- fl_1hr_bin_fixed[[rand]]
rand_f_3h <- fl_3hr_bin_fixed[[rand]]

# Now let's figure out which of the flight networks will be on which dates
idx <- firsts[has_visits][[rand]] %>%
  group_by(dateOnly) %>%
  summarize(n = n()) %>%
  pull(n) # okay so the first 15 are on the first day, and then the next 6, and then the next 1, and then the next 13.
idx

# Put everything in long format
tolong <- function(df){
  df %>%
    mutate(ID1 = row.names(.)) %>%
    pivot_longer(cols = -ID1, names_to = "ID2", values_to = "inter")
}
r_long <- map(rand_r, tolong) %>% data.table::rbindlist(idcol = "day") %>% mutate(type = "roost")
f_a_long <- map(rand_f_a, tolong) %>% data.table::rbindlist(idcol = "day") %>% mutate(type = "fl_a")
f_c_long <- map(rand_f_c, tolong) %>% map2(., rep(1:4, times = idx), ~{.x %>% mutate(day = .y)}) %>% rbindlist(idcol = "acq") %>% mutate(type = "fl_c")
f_1h_long <- map(rand_f_1h, tolong) %>% map2(., rep(1:4, times = idx), ~{.x %>% mutate(day = .y)}) %>% rbindlist(idcol = "acq") %>% mutate(type = "fl_1h")
f_3h_long <- map(rand_f_3h, tolong) %>% map2(., rep(1:4, times = idx), ~{.x %>% mutate(day = .y)}) %>% rbindlist(idcol = "acq") %>% mutate(type = "fl_3h")

inters_daily <- bind_rows(f_a_long, r_long)
inters_daily_wide <- pivot_wider(inters_daily, id_cols = c("ID1", "ID2", "day"), names_from = "type", values_from = "inter")

inters <- bind_rows(f_c_long, f_1h_long, f_3h_long)
inters_wide <- inters %>%
  pivot_wider(id_cols = c("day", "ID1", "ID2", "acq"), names_from = "type", values_from = "inter")

# Correlations?
# Jamie wanted to know: does who you roost with predict who you fly with?
# This is just for one carcass, but let's see what we find
inters_daily_wide %>%
  filter(day < 5, ID1 < ID2) %>% # remove self edges and keep only one way
  ggplot(aes(x = roost, y = fl_a)) + 
  geom_jitter(alpha = 0.7, pch = 1, size = 2)+
  theme_minimal()+
  facet_wrap(~day) # this is closer. I think we need to do a chi-squared test to see if these are significantly related. I asked DeepSeek for help on this, since I don't remember the syntax.

# Create a contingency table
formodel <- inters_daily_wide %>%
  mutate(dyad_id = paste(ID1, ID2, sept = "_")) %>%
  filter(ID1 < ID2)
contingency_table <- table(formodel$roost, formodel$fl_a)

# Print the contingency table to check
print(contingency_table)

# Perform the chi-squared test
chi_squared_test <- chisq.test(contingency_table)

# View the results
print(chi_squared_test) # the p-value is very very very small; we can conclude that there is a significant association between co-roosting and co-flying (for all days together)

# But for all days together isn't quite right.
# DeepSeek suggests running a GLMM to account for the repeated-measures structure of the data
library(lme4)

# Fit a GLMM with co_fly as the response, co_roost as the predictor,
# and a random intercept for each dyad
glmm_roost_fly <- glmer(fl_a ~ roost + (1 | dyad_id), 
                    data = formodel, 
                    family = binomial(link = "logit"))

# Summarize the model
summary(glmm_roost_fly)
# DS: The fixed effect of co_roost will tell you whether co-roosting significantly predicts co-flying, while accounting for the repeated measures within dyads. Roosting does significantly predict co-flying--it increases the likelihood of co-flying. 0.96 is the effect of roosting on the log odds of co-flying
# DS: The random intercept for dyad_id captures the variability between dyads. Variance of the random intercept is 0.99; st. dev is 0.995. We have 595 dyads in the datasets. Random intercepts capture variability between dyads in their baseline probability of co-flying.

# Now I'd like to add an interaction term with day to figure out whether the extent to which co-roosting predicts co-flying varies as time goes on (i.e. from the day the carcass is placed to several days after the carcass is placed.)
glmm_roost_fly_day <- glmer(fl_a ~ roost*day + (1 | dyad_id), 
                    data = formodel, 
                    family = binomial(link = "logit"))

# Summarize the model
summary(glmm_roost_fly_day) # interesting! So, day does affect the probability of co-flying--dyads are less likely to co-fly on later days than on earlier days (this could be due to any number of reasons, and note that this model isn't fully accurate because it doesn't account for the inclusion of each individual in several dyads; we would have to use ERGMs or something similar to deal with that). But day does *not* change the effect of co-roosting on co-flight (for this particular carcass). Roosting together slightly increases the likelihood of flying together on the subsequent day, and that effect remains constant from day 1 through day 4 for this carcass.

# Combining multiple carcasses --------------------------------------------
# This has been great to determine the effect of co-roosting on co-flying for one carcass, but it would be great to get a sense of this over all the carcasses. To do that, I'm going to have to create inters_daily_wide and inters_wide for all of the different carcasses and then bind them together. Let's come back to this.
# XXX START HERE.
