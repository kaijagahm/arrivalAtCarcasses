# Investigate distance and age ilvs
library(targets)
library(tidyverse)
library(lme4)
library(lmerTest)

tar_load(oas_nbda)
tar_load(ilvs_nbda)
tar_load(carcIDs_nbda)
tar_load(days_vec_nbda)

oas_dfs <- map2(oas_nbda, days_vec_nbda, ~data.frame(local_identifier = .x,
                                                     see_order = 1:length(.x),
                                                     day = .y))

analyze <- map2(ilvs_nbda, oas_dfs, ~left_join(.x, .y, by = "local_identifier"))
names(analyze) <- carcIDs_nbda
out <- purrr::list_rbind(analyze, names_to = "carcID")
dim(out)

out <- out %>%
  group_by(carcID) %>%
  mutate(see_order_rel = see_order/max(see_order, na.rm = T)) %>%
  arrange(carcID, day, see_order) %>%
  group_by(carcID, day) %>%
  mutate(see_order_day = 1:n())

out <- out %>%
  mutate(night_before_dist = case_when(day == 1 ~ roost_night0,
                                       day == 2 ~ roost_night1,
                                       day == 3 ~ roost_night2,
                                       .default = NA)) %>%
  ungroup()

# Visualize arrivals ------------------------------------------------------
out %>%
  ggplot(aes(x = age_group, y = see_order_rel))+
  geom_boxplot() # No overall differences between adults and juveniles in their arrival order to the carcass

out %>%
  ggplot(aes(x = carcID, y = see_order_rel, fill = age_group))+
  geom_boxplot() # I don't see a consistent age difference in relative sighting order across the carcasses

# But really, we should be measuring them separately and applying multi-model corrections. Let's do a bunch of t tests, I guess?
pvals <- rep(NA, length(carcIDs_nbda))
for(i in 1:length(unique(carcIDs_nbda))){
  dat <- out %>%
    filter(carcID == carcIDs_nbda[i]) %>%
    select(age_group, see_order) %>%
    filter(!is.na(see_order) & !is.na(age_group))
  result <- tryCatch({t.test(see_order ~ age_group, data = dat)},
           error = function(msg){NULL})
  if(!is.null(result)){
    pvals[i] <- result$p.value
  }else{
    pvals[i] <- NA
  }
}
new_sig_level <- 0.05/sum(!is.na(pvals))
pvals_still_sig <- pvals < new_sig_level # adults and juveniles do not consistently arrive earlier/later, after correcting for the number of carcasses.

# Visualize arrivals on the days they arrived
out %>%
  group_by(day, carcID) %>%
  filter(day %in% 1:3) %>%
  mutate(facet_titles = paste0("Day ", day)) %>%
  ggplot(aes(x = night_before_dist/1000, y = see_order_rel, col = age_group, fill = age_group))+
  geom_point(pch = 1, alpha = 0.5) +
  geom_smooth(method = "lm", alpha = 0.2)+
  stat_smooth(geom = "line", method = "lm", se = F, linewidth = 0.5, alpha = 0.5,
              aes(group = interaction(carcID, age_group)))+
  facet_wrap(~facet_titles)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(x = "Roost-carc distance (km), night before",
       y = "Relative sighting order") # there does not seem to be a consistent age difference in how distance affects arrival order. This would point to no age effect on asocial learning, though it does not directly measure it.

# So based on this, I don't see a strong requirement to include age as an ILV, although it is still possible for it to have an effect on the rate of social learning, or the rate of asocial learning, without showing up in the overall order of arrivals directly.

# Distance, on the other hand, definitely seems important. Let's look at distance without age.
out %>%
  filter(day %in% 1:3) %>%
  mutate(facet_titles = paste0("Day ", day)) %>%
  ggplot(aes(x = night_before_dist/1000, y = see_order_rel))+
  geom_point(pch = 1, alpha = 0.3) +
  geom_smooth(method = "lm", alpha = 0.2, color = "black")+
  stat_smooth(geom = "line", method = "lm", se = F, linewidth = 0.5, alpha = 0.3,
              aes(group = carcID))+
  facet_wrap(~facet_titles)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(x = "Roost-carc distance (km), night before",
       y = "Relative sighting order")

# Distance from roost night 0 to the carcass versus arrival order
out %>%
  filter(day %in% 1:3) %>%
  ggplot(aes(x = roost_night0/1000, y = see_order))+
  geom_point(pch = 1, alpha = 0.3) +
  geom_smooth(method = "lm", alpha = 0.2, color = "black")+
  stat_smooth(geom = "line", method = "lm", se = F, linewidth = 0.5, alpha = 0.3,
              aes(group = carcID))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(x = "Roost-carc distance (km), first night",
       y = "Relative sighting order")
  
# So yes, there's a clear positive effect of roost distance on order of arrival. In addition, for most carcasses, there appears to be a positive effect of roost distance on order of arrival, although it is not strictly the case for all carcasses.

mod <- lmer(see_order_day ~ night_before_dist + (1|carcID), data = out)
summary(mod) # not significant, interestingly

# But again, what we're really interested in is whether it's significant for any of them, right?
mod2 <- lm(see_order_day ~ night_before_dist + carcID, data = out)
summary(mod2) # okay, this would argue for definitely including distance in the models, so I can figure out whether the distance effect varies with aspects of the carcass.
