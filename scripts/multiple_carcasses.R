# Multiple carcasses
# - at least 3 vultures detecting
# - at stations
# - during the target time periods
# - order of detection, not arrival
# - roost nets (dynamic)
# - flight nets (dynamic; 4km detection radius; cumulative)
# - ILVs: age (static); distance from roost to carcass (time-varying)

# Load packages -----------------------------------------------------------
library(here)
library(NBDA)
library(tidyverse)
library(sf)
library(lme4)
library(targets)

# Load data
tar_load(Mods_N.RD_So)
tar_load(Mods_N.RS_So)

search_roost <- data.frame(type = c(rep("dynamic", length(Mods_N.RD_So)),
                                    rep("static", length(Mods_N.RS_So))),
                           lower_min = NA,
                           lower_max = NA,
                           upper_min = NA,
                           upper_max = NA,
                           ci_lower = NA,
                           ci_upper = NA,
                           carcID = c(carcIDs_nbda, carcIDs_nbda),
                           network = "roost")

search_flight <- data.frame(type = rep("dynamic", length(Mods_N.RD_So)),
                            lower_min = NA,
                            lower_max = NA,
                            upper_min = NA,
                            upper_max = NA,
                            ci_lower = NA,
                            ci_upper = NA,
                            carcID = carcIDs_nbda,
                            network = "flight")

## Dynamic models (roosting)
plotProfLik(which = 1, model = Mods_N.RD_So[[1]], range = c(0,1.5))
search_roost[1,2:5] <- c(NA, NA, 0.1, 0.3)
search_roost[2,2:5] <- c(NA, NA, 0.05, 0.1)
search_roost[3,2:5] <- c(NA, NA, 10, 15)
search_roost[4,2:5] <- c(NA, NA, 0.5, 1)
search_roost[5,2:5] <- c(NA, NA, 0.25, 0.4)
search_roost[6,2:5] <- c(NA, NA, 0.6, 0.8)
search_roost[7,2:5] <- c(NA, NA, 0.6, 0.8)
search_roost[8,2:5] <- c(NA, NA, 0.3, 0.4)
search_roost[9,2:5] <- c(0, 0.5, 1, 1.5)
search_roost[10,2:5] <- c(NA, NA, 0.1, 0.5)
search_roost[11,2:5] <- c(NA, NA, 0.6, 1.0)
search_roost[12,2:5] <- c(NA, NA, 0.4, 0.6)
search_roost[13,2:5] <- c(NA, NA, 0.2, 0.4)
search_roost[14,2:5] <- c(NA, NA, 10, 15)
search_roost[15,2:5] <- c(0, 0.2, 0.8, 1)
search_roost[16,2:5] <- c(NA, NA, 1, 2)
search_roost[17,2:5] <- c(NA, NA, NA, NA)
search_roost[18,2:5] <- c(0, 0.5, 1, 1.5)
search_roost[19,2:5] <- c(NA, NA, 2, 3)
search_roost[20,2:5] <- c(0, 1, 60, 80)
search_roost[21,2:5] <- c(NA, NA, 0.1, 0.4)
search_roost[22,2:5] <- c(NA, NA, 0.2, 0.4)
search_roost[23,2:5] <- c(NA, NA, NA, NA)
search_roost[24,2:5] <- c(NA, NA, 0.2, 0.4)
search_roost[25,2:5] <- c(NA, NA, 0.6, 1)
search_roost[26,2:5] <- c(NA, NA, 0.05, 0.1)
search_roost[27,2:5] <- c(0, 0.25, 3, 4)
search_roost[28,2:5] <- c(NA, NA, 0.8, 1)
search_roost[29,2:5] <- c(NA, NA, 0.2, 0.4)
search_roost[30,2:5] <- c(NA, NA, 4, 6)
search_roost[31,2:5] <- c(NA, NA, 0.1, 0.2)
search_roost[32,2:5] <- c(NA, NA, 0.4, 0.6)
search_roost[33,2:5] <- c(0, 0.2, 0.6, 0.8)
search_roost[34,2:5] <- c(NA, NA, NA, NA)
search_roost[35,2:5] <- c(NA, NA, 0.05, 0.15)
search_roost[36,2:5] <- c(0, 0.15, 0.6, 0.8)
search_roost[37,2:5] <- c(0, 0.15, 0.6, 1)
search_roost[38,2:5] <- c(NA, NA, NA, NA)
search_roost[39,2:5] <- c(NA, NA, 0.01, 0.1)
search_roost[40,2:5] <- c(0, 0.25, 0.5, 1)
search_roost[41,2:5] <- c(0, 0.1, 1, 1.5)

## Dynamic models (flight)
search_flight[1,2:5] <- c(0, 25, 150, 200)
search_flight[2,2:5] <- c(0, 0.5, 1.5, 2)
search_flight[3,2:5] <- c(NA, NA, 1, 1.5)
search_flight[4,2:5] <- c(NA, NA, 1, 1.5)
search_flight[5,2:5] <- c(5, 10, 20, 25)
search_flight[6,2:5] <- c(NA, NA, 0.4, 0.6)
search_flight[7,2:5] <- c(0, 0.5, 10, 15)
search_flight[8,2:5] <- c(0, 0.5, 4, 8)
search_flight[9,2:5] <- c(0, 1, 6, 8)
search_flight[10,2:5] <- c(NA, NA, 0.2, 0.4)
search_flight[11,2:5] <- c(0, 1, 3, 4)
search_flight[12,2:5] <- c(NA, NA, 0.5, 1)
search_flight[13,2:5] <- c(NA, NA, 0.1, 0.3)
search_flight[14,2:5] <- c(NA, NA, 10, 30)
search_flight[15,2:5] <- c(2.5, 5, 20, 25)
search_flight[16,2:5] <- c(0, 0.1, 0.5, 1)
search_flight[17,2:5] <- c(NA, NA, 30, 50)
search_flight[18,2:5] <- c(0, 2, 10, 15)
search_flight[19,2:5] <- c(NA, NA, 1.5, 2)
search_flight[20,2:5] <- c(0, 0.25, 1.5, 2)
search_flight[21,2:5] <- c(NA, NA, 2, 2.5)
search_flight[22,2:5] <- c(NA, NA, 0.4, 0.6)
search_flight[23,2:5] <- c(NA, NA, NA, NA)
search_flight[24,2:5] <- c(NA, NA, 1.5, 2.5)
search_flight[25,2:5] <- c(0, 1, 2.5, 3)
search_flight[26,2:5] <- c(NA, NA, 0.2, 0.4)
search_flight[27,2:5] <- c(5, 10, 30, 40)
search_flight[28,2:5] <- c(NA, NA, 1, 2)
search_flight[29,2:5] <- c(NA, NA, 1.5, 2)
search_flight[30,2:5] <- c(NA, NA, 3, 4)
search_flight[31,2:5] <- c(0.5, 1.5, 6, 7)
search_flight[32,2:5] <- c(0, 1, 6, 8)
search_flight[33,2:5] <- c(NA, NA, 0, 2)
search_flight[34,2:5] <- c(0, 5, 15, 20)
search_flight[35,2:5] <- c(NA, NA, 0.2, 1)
search_flight[36,2:5] <- c(0, 1, 4, 6)
search_flight[37,2:5] <- c(0, 0.5, 1.5, 2.5)
search_flight[38,2:5] <- c(NA, NA, 10, 20)
search_flight[39,2:5] <- c(0, 0.25, 1.5, 2)
search_flight[40,2:5] <- c(0, 1, 6, 8)
search_flight[41,2:5] <- c(0, 2, 18, 20)

write_rds(search_flight, file = here("data/created/search_flight.RDS"))
write_rds(search_roost, file = here("data/created/search_roost.RDS"))

## back to the targets pipeline now.

# Plotting ----------------------------------------------------------------
# (Post-hoc analysis)
tar_load(summaries_updated)

summaries_updated %>%
  filter(type == "dynamic", soc == "social", !is.na(sig_ci)) %>%
  ggplot(aes(y = carcID, color = network))+
  geom_segment(aes(x = propsolve_lower, xend = propsolve_upper, linetype = sig_ci), position = position_dodge(width = 0.5))+
  geom_point(aes(x = propsolve, pch = sig_ci), size = 2, position = position_dodge(width = 0.5))+
  scale_shape_manual(values = c(21, 19))+
  scale_color_manual(values = c("dodgerblue2", "olivedrab4"))+
  scale_linetype_manual(values = c(3, 1))+
  facet_wrap(~year, ncol = 1, scales = "free_y")+
  theme_minimal()+
  labs(y = "Carcass",
       x = "Proportion of detections by social transmission",
       title = "Dynamic roost networks")

# Okay, this is great, we have info on both flight and roosting for the same carcasses.
# Now we could ask whether there's a trend for number of detection events, weight of carcass, or location of carcass. And then we can incorporate ILVs and compete these against each other.

test <- summaries_updated %>%
  filter(type == "dynamic", soc == "social", !is.na(sig_ci)) 

mylogit <- glm(sig_ci ~ network + n_detections + carcassWeight, data = test, family = "binomial") # XXX should probably standardize detections and carcassWeight
summary(mylogit)

newdata <- as.data.frame(expand.grid("n_detections" = seq(from = 3, to = 69, by = 11), "carcassWeight" = seq(from = 30, to = 550, by = 10), "network" = c("flight", "roost")))

newdata$p <- predict(mylogit, newdata = newdata, type = "response")

newdata %>%
  ggplot(aes(x = carcassWeight, y = p, col = network, group = interaction(factor(n_detections), network)))+
  geom_line(aes(size = factor(n_detections)))+
  scale_size_manual(values = seq(from = 0.2, to = 1.5, length.out = 7))+
  scale_color_manual(values =c("dodgerblue2", "olivedrab4"))+
  theme_minimal()+
  labs(y = "P(social transmission)",
       x = "Carcass weight",
       color = "Network")+
  geom_point(data = test %>% mutate(p = ifelse(sig_ci, 1, 0)), 
             aes(x = carcassWeight, y = p, pch = network), size = 3)+
  scale_shape_manual(name = "Network", values = c(21, 8))

# So, heavier carcasses are very slightly less likely to show a signal of social transmission; roost network is less likely to show social transmission; more detections is more likely to show a signal of social transmission. 

# Okay, now, within the models that show significant evidence of social transmission, what relationships do we see?
mod1 <- lm(propsolve ~ carcassWeight + n_detections + network, data = test[test$sig_ci,])
summary(mod1) 
test %>%
  filter(sig_ci) %>%
  ggplot(aes(x = carcassWeight, col = network, shape = network))+
  geom_segment(aes(x = carcassWeight, y = propsolve_lower, yend = propsolve_upper))+
  geom_point(size = 2, aes(y = propsolve))+
  scale_color_manual(name = "Network", values = c("dodgerblue2", "olivedrab4"))+
  theme_minimal()+
  scale_shape_manual(name = "Network", values = c(19, 8))+
  geom_smooth(method = "lm", aes(y = propsolve, fill = network), alpha = 0.2)+
  scale_fill_manual(name = "Network", values = c("dodgerblue2", "olivedrab4"))+
  labs(y = "Proportion of detections by social transmission",
       x = "Carcass weight (kg)")

test %>%
  filter(sig_ci) %>%
  ggplot(aes(x = n_detections, col = network, shape = network))+
  geom_segment(aes(x = n_detections, y = propsolve_lower, yend = propsolve_upper))+
  geom_point(size = 2, aes(y = propsolve))+
  scale_color_manual(name = "Network", values = c("dodgerblue2", "olivedrab4"))+
  scale_fill_manual(name = "Network", values = c("dodgerblue2", "olivedrab4"))+
  theme_minimal()+
  scale_shape_manual(name = "Network", values = c(19, 8))+
  geom_smooth(method = "lm", aes(y = propsolve, fill = network), alpha = 0.2)+
  labs(y = "Proportion of detections by social transmission",
       x = "Number of detections") # we see that number of detections affected how likely we were to detect a social effect, but not the magnitude of the social effect once detected.

mod2 <- lm(propsolve ~ n_detections + network, data = test[test$sig_ci,])
summary(mod2)

# What about station?
test %>%
  mutate(stationName = str_replace_all(stationName, "_", " ")) %>%
  ggplot(aes(x = stationName, y = propsolve, fill = network, color = network))+
  geom_boxplot(alpha = 0.5)+
  scale_fill_manual(name = "Network", values = c("dodgerblue2", "olivedrab4"))+
  scale_color_manual(name = "Network", values = c("dodgerblue2", "olivedrab4"))+
  theme_minimal()+
  labs(y = "Proportion of detections by social transmission",
       x = NULL)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  facet_wrap(~year, scales = "free_x", nrow = 2, strip.position="right")

# # Adding ILVs -------------------------------------------------------------
# Make the models
search_with_ilvs <- data.frame(type = rep("dynamic", length(Mods_N.FD_addI.TC_I.TV_So)),
                            lower_min = NA,
                            lower_max = NA,
                            upper_min = NA,
                            upper_max = NA,
                            ci_lower = NA,
                            ci_upper = NA,
                            carcID = carcIDs,
                            network = "flight")

plotProfLik(which = 1, model = Mods_N.FD_addI.TC_I.TV_So[[12]], range = c(0, 2))
search_with_ilvs[35,2:5] <- c(NA, NA, 20, 40)
search_with_ilvs[34,2:5] <- c(NA, NA, 100, 200)
search_with_ilvs[33,2:5] <- c(NA, NA, 1, 2)
search_with_ilvs[32,2:5] <- c(NA, NA, 0, 1)
search_with_ilvs[31,2:5] <- c(NA, NA, 0, 1)
search_with_ilvs[30,2:5] <- c(NA, NA, 100, 200)
search_with_ilvs[29,2:5] <- c(0, 5, 15, 20)
search_with_ilvs[28,2:5] <- c(NA, NA, 1, 2)
search_with_ilvs[27,2:5] <- c(0, 1, 6, 8)
search_with_ilvs[26,2:5] <- c(NA, NA, 3, 5)
search_with_ilvs[25,2:5] <- NA
search_with_ilvs[24,2:5] <- c(NA, NA, 8,12)
search_with_ilvs[23,2:5] <- c(0, 5, 20, 30)
search_with_ilvs[22,2:5] <- c(NA, NA, 0, 1)
search_with_ilvs[21,2:5] <- c(0, 2, 4, 6)
search_with_ilvs[20,2:5] <- NA
search_with_ilvs[19,2:5] <- c(NA, NA, 700, 800)
search_with_ilvs[18,2:5] <- NA
search_with_ilvs[17,2:5] <- c(0, 20, 9000, 12000)
search_with_ilvs[16,2:5] <- c(NA, NA, 0.1, 0.2)
search_with_ilvs[15,2:5] <- c(0, 10, 60, 80)
search_with_ilvs[14,2:5] <- c(NA, NA, 400, 600)
search_with_ilvs[13,2:5] <- c(NA, NA, 0.1, 0.3)
search_with_ilvs[12,2:5] <- c(NA, NA, 1, 2)
search_with_ilvs[11,2:5] <- c(0, 0.1, 1, 3)
search_with_ilvs[10,2:5] <- c(0, 0.1, 0.4, 0.6)
search_with_ilvs[9,2:5] <- c(0, 1, 5, 10)
search_with_ilvs[8,2:5] <- c(NA, NA, 3, 5)
search_with_ilvs[7,2:5] <- c(0, 1, NA, NA)
search_with_ilvs[6,2:5] <- NA
search_with_ilvs[5,2:5] <- NA
search_with_ilvs[4,2:5] <- NA
search_with_ilvs[3,2:5] <- c(NA, NA, 2, 3)
search_with_ilvs[2,2:5] <- c(0, 1, 1.5, 3)
search_with_ilvs[1,2:5] <- c(0, 100, 350, 450)

for(i in 1:length(Mods_N.FD_addI.TC_I.TV_So)){
  if(is.na(search_with_ilvs[i,2]) & !is.na(search_with_ilvs[i,4])){
    ci <- profLikCI(which = 1, model = Mods_N.FD_addI.TC_I.TV_So[[i]],
                    upperRange = search_with_ilvs[i,4:5])
  }else if(!is.na(search_with_ilvs[i,2]) & !is.na(search_with_ilvs[i,4])){
    ci <- profLikCI(which = 1, model = Mods_N.FD_addI.TC_I.TV_So[[i]],
                    lowerRange = search_with_ilvs[i,2:3],
                    upperRange = search_with_ilvs[i,4:5])
  }else{
    ci <- c(NA, NA)
  }
  search_with_ilvs[i,6:7] <- ci 
  cat("done with ", i, "\n")
}

solveprops_ilvs_lower <- map2_dbl(search_with_ilvs$ci_lower, Mods_N.FD_addI.TC_I.TV_So, ~{
  tryCatch({nbdaPropSolveByST(par = c(.x, .y@outputPar[2],
                            .y@outputPar[3]), 
                    nbdadata = .y@nbdadata)[1]}, error = function(msg){NA})
})

solveprops_ilvs_upper <- map2_dbl(search_with_ilvs$ci_upper, Mods_N.FD_addI.TC_I.TV_So, ~{
  tryCatch({nbdaPropSolveByST(par = c(.x, .y@outputPar[2],
                                      .y@outputPar[3]), 
                              nbdadata = .y@nbdadata)[1]}, error = function(msg){NA})
})

search_with_ilvs$propsolve_lower <- solveprops_ilvs_lower
search_with_ilvs$propsolve_upper <- solveprops_ilvs_upper

search_with_ilvs <- search_with_ilvs %>%
  mutate(sig_ci = ifelse(propsolve_lower > 0, TRUE, FALSE),
         soc = "social")

# join all these calculated conf int params to the overall model summary table
sm <- summaries_with_ilvs %>%
  filter(varNames == "1 Social transmission 1") %>%
  left_join(search_with_ilvs, by = c("carcID", "soc", "type", "network")) %>%
  left_join(years) # this includes info not just on years but also on carcass location, station, and weight, so we can analyze social transmission by carcass characteristics.

sm %>%
  filter(!is.na(sig_ci)) %>%
  ggplot(aes(y = carcID))+
  geom_segment(aes(x = propsolve_lower, xend = propsolve_upper, linetype = sig_ci))+
  geom_point(aes(x = propsolve, pch = sig_ci), size = 2)+
  scale_shape_manual(values = c(21, 19))+
  scale_linetype_manual(values = c(3, 1))+
  facet_wrap(~year, ncol = 1, scales = "free_y")+
  theme_minimal()+
  labs(y = "Carcass",
       x = "Proportion of detections by social transmission",
       title = "Dynamic flight networks (and ILVs)")

# Before I start making these decisions, let's see if we have any evidence of age or distance actually making a difference to when individuals discover the carcass. No point in correcting for it if we don't think it'll affect things. We could also preemptively decide to use dynamic NBDA since that would describe what's happening better--it doesn't make sense to use static.
fs <- map2(firsts_see[has_sightings][has_3_sightings], carcIDs, ~.x %>% mutate(carcID = .y))
mi <- map2(my_ilvs, carcIDs, ~.x %>% mutate(carcID = .y))
joined <- map2(mi, fs, left_join)
all(map_dbl(joined, nrow) == map_dbl(my_ilvs, nrow)) # same rows as number of individuals in the ILVs, not the number of individuals that eventually detected the carcass, since not everyone did eventually detect the carcass.
all(map_dbl(joined, nrow) == map_dbl(fs, nrow)) # false

joined_df <- purrr::list_rbind(joined)
joined_df <- joined_df %>%
  mutate(found_carcass = ifelse(!is.na(timestamp), TRUE, FALSE)) %>%
  mutate(year = lubridate::year(dateOnly))

# Are adults or juveniles more likely to find the carcass?
## quick and dirty viz: proportion of adults that found the carcass vs. proportion of juveniles that found the carcass, for different carcasses
joined_df %>%
  group_by(carcID, age_group) %>%
  summarize(prop_found_carcass = sum(found_carcass)/n()) %>%
  ggplot(aes(x = age_group, y = prop_found_carcass, group = carcID))+
  geom_point()+
  geom_line() # no obvious pattern to what proportion of juveniles vs. adults found the carcass

mod3 <- glmer(found_carcass ~ age_group + (1|carcID) 
              #+ (1|local_identifier)
              , data = joined_df, family = binomial)
summary(mod3) # adults slightly less likely to find the carcass than juveniles. Effect basically disappears when we include a random effect of individual ID, but if anything it still trends negative. 

# Do adults and juveniles differ in their time of arrival to the carcass?
## This one is easier to visualize because time of arrival is continuous.
joined_df %>%
  filter(!is.na(year)) %>%
  ggplot(aes(x = carcID, y = time_since_carcass, fill = age_group))+
  geom_boxplot()+
  theme_classic()+
  facet_wrap(~year, scales = "free_x") # not clear from this viz whether juveniles or adults find the carcass earlier. Let's do a model

mod4 <- lmer(as.numeric(time_since_carcass) ~ age_group + (1|carcID), data = joined_df)
summary(mod4) # adults seem to find the carcass more quickly.

# This is pointing to the need to include an effect of age in the models. Would it be better to use a continuous rather than categorical predictor? Which is better/fewer degrees of freedom?

# Does distance from the carcass the night before affect how quickly you find the carcass?
joined_df %>%
  filter(!is.na(year)) %>%
  ggplot(aes(x = dist_roost_night_0, y = time_since_carcass, col = carcID))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.1)+
  facet_wrap(~year)+
  theme_classic()+
  theme(legend.position = "none") # there does seem to be a positive trend--roosting farther away means you find the carcass later.
# what about night 1?

joined_df %>%
  filter(!is.na(year)) %>%
  ggplot(aes(x = dist_roost_night_1, y = time_since_carcass, col = carcID))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.1)+
  facet_wrap(~year)+
  theme_classic()+
  theme(legend.position = "none") # ooh, even more of a positive trend! Interesting. The causality might start going the other way at some point.

# What about night 2?
joined_df %>%
  filter(!is.na(year)) %>%
  ggplot(aes(x = dist_roost_night_2, y = time_since_carcass, col = carcID))+
  geom_point()+
  geom_smooth(method = "lm", alpha = 0.1)+
  facet_wrap(~year)+
  theme_classic()+
  theme(legend.position = "none") # breaks down a bit.

# but regardless, I also think it is important to include distance in the calculation here.

# Okay, it doesn't look like we can get rid of the ILVs. Maybe we can just use dynamic networks instead of static, since static doesn't make that much sense?

# Next: move to model averaging, following the tutorial. See model_averaging.R.

# Save the objects I'll need for this
write_rds(age_groups_29, file = here("test_dynamic_nbda/data/age_groups_29.RDS"))
write_rds(std_roost_carc_distances_29, file = here("test_dynamic_nbda/data/std_roost_carc_distances_29.RDS"))
write_rds(N.RS, file = here("test_dynamic_nbda/data/N.RS.RDS"))
write_rds(N.RD, file = here("test_dynamic_nbda/data/N.RD.RDS"))
write_rds(N.FD, file = here("test_dynamic_nbda/data/N.FD.RDS"))
write_rds(oas, file = here("test_dynamic_nbda/data/oas.RDS"))
write_rds(roost_mats_expanded, file = here("test_dynamic_nbda/data/roost_mats_expanded.RDS"))
write_rds(fl_mats, file = here("test_dynamic_nbda/data/fl_mats.RDS"))
