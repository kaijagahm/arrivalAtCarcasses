library(tidyverse)
library(targets)
library(sf)
library(NBDA)
tar_load(mods_cumul_bin)
tar_load(mods_cumul_wt)
tar_load(mods_cumul_bin_wild)
tar_load(mods_cumul_wt_wild)
tar_load(stats)
tar_load(stats_wild)
tar_load(stn)
tar_load(wild)
tar_load(stn_carcs)
tar_load(wild_carcs)
tar_load(ns)
tar_load(ns_wild)

# Saving today's versions
# write_rds(mods_cumul_bin, file = "data/created/exploring_model_results/2025-10-01_mods_cumul_bin.RDS")
# write_rds(mods_cumul_wt, file = "data/created/exploring_model_results/2025-10-01_mods_cumul_wt.RDS")
# write_rds(mods_cumul_bin_wild, file = "data/created/exploring_model_results/2025-10-01_mods_cumul_bin_wild.RDS")
# write_rds(mods_cumul_wt_wild, file = "data/created/exploring_model_results/2025-10-01_mods_cumul_wt_wild.RDS")
# write_rds(stats, file = "data/created/exploring_model_results/2025-10-01_stats.RDS")
# write_rds(stats, file = "data/created/exploring_model_results/2025-10-01_stats_wild.RDS")
# test <- readRDS("data/created/exploring_model_results/2025-10-01_mods_cumul_bin.RDS")

stats <- stats %>% mutate(stn_wild = "stn")
stats_wild <- stats_wild %>% mutate(stn_wild = "wild")

stats <- left_join(stats, ns, by = "carcID") %>% 
  left_join(stn, by = "carcID")
stats_wild <- left_join(stats_wild, ns_wild, by = "carcID") %>% 
  left_join(wild, by = "carcID")

stats_all <- bind_rows(stats, stats_wild) %>%
  mutate(lower = outputPar - se, upper = outputPar + se, sig_se = ifelse(lower > 0 & !is.na(lower), T, F)) %>%
  mutate(year = lubridate::year(date))

# stats_all %>%
#   filter(!is.na(outputPar), stn_wild == "stn", binwt == "wt") %>%
#   mutate(lower = case_when(!sig_se ~ NA, .default = lower),
#          upper = case_when(!sig_se ~ NA, .default = upper)) %>%
#   filter(outputPar < 50) %>%
#   ggplot(aes(x = factor(carcID), y = outputPar, col = interaction(type, binwt)))+
#   geom_hline(aes(yintercept = 0), linetype = 3, color = "black")+
#   geom_segment(aes(y = lower, yend = upper))+
#   geom_point(aes(pch = sig_se))+
#   scale_shape_manual(values = c(1, 19))+
#   theme_classic()+
#   coord_flip()+
#   facet_wrap(~year, scales = "free_y", ncol = 1)+
#   scale_color_manual(values = c("firebrick4", "dodgerblue4"))+
#   labs(title = "SFS carcasses", y = "Social transmission", x = "Carcass")

# stats_all %>%
#   filter(!is.na(outputPar), stn_wild == "wild", binwt == "wt") %>%
#   mutate(lower = case_when(!sig_se ~ NA, .default = lower),
#          upper = case_when(!sig_se ~ NA, .default = upper)) %>%
#   filter(outputPar < 30) %>%
#   ggplot(aes(x = factor(carcID), y = outputPar, col = interaction(type, binwt)))+
#   geom_hline(aes(yintercept = 0), linetype = 3, color = "black")+
#   geom_segment(aes(y = lower, yend = upper))+
#   geom_point(aes(pch = sig_se))+
#   scale_shape_manual(values = c(1, 19))+
#   theme_classic()+
#   coord_flip()+
#   facet_wrap(~year, scales = "free_y", ncol = 1)+
#   scale_color_manual(values = c("firebrick4", "dodgerblue4"))+
#   labs(title = "Wild carcasses", y = "Social transmission", x = "Carcass")

# What about the relationship to the number of individuals in the diffusion?
# Does number of individuals predict significant social transmission?
stats_all %>%
  mutate(stn_wild = case_when(stn_wild == "stn" ~ "SFS",
                              stn_wild == "wild" ~ "Non-SFS",
                              .default = NA)) %>%
  filter(seeds, binwt == "wt", type == "cumul") %>%
  mutate(sig_num = ifelse(sig_se, 1, 0)) %>%
  ggplot(aes(x = n_found, y = sig_num)) +
  geom_point(position = position_jitter(height = 0.02), alpha = 0.2, size = 2, color = "darkblue") + 
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "darkblue")+
  theme_minimal()+
  facet_wrap(~stn_wild)+
  theme(legend.position = "bottom", text = element_text(size = 14)) + 
  labs(y = "Social transmission detected", x = "Vultures in diffusion")+
  scale_y_continuous(breaks = c(0, 1))

stats_all %>%
  mutate(stn_wild = case_when(stn_wild == "stn" ~ "SFS",
                              stn_wild == "wild" ~ "Non-SFS",
                              .default = NA)) %>%
  filter(seeds, binwt == "wt", type == "cumul") %>%
  mutate(sig_num = ifelse(sig_se, 1, 0)) %>%
  ggplot(aes(x = prop_found, y = sig_num)) +
  geom_point(position = position_jitter(height = 0.02), alpha = 0.2, size = 2, color = "darkblue") + 
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "darkblue")+
  theme_minimal()+
  facet_wrap(~stn_wild)+
  theme(legend.position = "bottom", text = element_text(size = 14)) + 
  labs(y = "Social transmission detected", x = "Proportion of vultures in diffusion")+
  scale_y_continuous(breaks = c(0, 1))
  
# Now, is there a relationship between the number of individuals and the strength of social effect, for the ones where an effect was detected?
stats_all %>%
  filter(sig_se) %>%
  mutate(stn_wild = case_when(stn_wild == "stn" ~ "SFS",
                              stn_wild == "wild" ~ "Non-SFS",
                              .default = NA)) %>%
  filter(seeds, binwt == "wt", type == "cumul") %>%
  ggplot(aes(x = n_found, y = outputPar)) +
  geom_point(size = 2, alpha = 0.75, color = "darkblue")+
  geom_smooth(method = "lm", color = "darkblue")+
  theme_minimal()+
  facet_wrap(~stn_wild)+
  theme(legend.position = "bottom", text = element_text(size = 14)) + 
  labs(y = "Social transmission strength", x = "Vultures in diffusion")

stats_all %>%
  filter(sig_se) %>%
  mutate(stn_wild = case_when(stn_wild == "stn" ~ "SFS",
                              stn_wild == "wild" ~ "Non-SFS",
                              .default = NA)) %>%
  filter(seeds, binwt == "wt", type == "cumul") %>%
  ggplot(aes(x = prop_found, y = outputPar)) +
  geom_point(size = 2, alpha = 0.75, color = "darkblue")+
  geom_smooth(method = "lm", color = "darkblue")+
  theme_minimal()+
  facet_wrap(~stn_wild)+
  theme(legend.position = "bottom", text = element_text(size = 14)) + 
  labs(y = "Social transmission strength", x = "Proportion of vultures in diffusion")

stats_all %>%
  filter(sig_se) %>%
  mutate(stn_wild = case_when(stn_wild == "stn" ~ "SFS",
                              stn_wild == "wild" ~ "Non-SFS",
                              .default = NA)) %>%
  filter(seeds, binwt == "wt", type == "cumul") %>%
  ggplot(aes(x = prop_found, y = outputPar, color = factor(year))) +
  geom_point(size = 2, alpha = 0.75)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  facet_wrap(~stn_wild)+
  theme(legend.position = "bottom", text = element_text(size = 14)) + 
  labs(y = "Social transmission strength", x = "Proportion of vultures in diffusion")

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
  filter(stn_wild == "stn", binwt == "wt", type == "cumul", seeds == TRUE) %>%
  ggplot(aes(x = year, y = prop_null, fill = factor(year)))+
  geom_col(position = position_dodge())+
  labs(title = "SFS carcasses", x = "Year", y = "Prop. carcasses with null results", fill = "Year")+
  theme_minimal()+
  theme(text = element_text(size = 14))+
  scale_y_continuous(limits = c(0, 0.5))

propnull %>%
  filter(stn_wild == "wild", binwt == "wt", type == "cumul", seeds == TRUE) %>%
  ggplot(aes(x = year, y = prop_null, fill = factor(year)))+
  geom_col(position = position_dodge())+
  labs(title = "Non-SFS carcasses", x = "Year", y = "Prop. carcasses with null results", fill = "Year")+
  theme_minimal()+
  theme(text = element_text(size = 14))+
  scale_y_continuous(limits = c(0, 0.5))

# Solutions:
## 1. Some of the carcasses could be in the wrong place still, so we would not be detecting arrivals. Go work on the placement of the station carcasses again.
## 2. Data cleaning could be a mess. Add data cleaning to the targets pipeline

## 3. Check whether the ones that are NULL are all/mostly the ones with few individuals (restricting to SFS cumul for simplicity)
stats_all %>%
  filter(stn_wild == "stn", is.na(propsolve), type == "cumul") %>%
  select(propsolve, type, binwt, carcID, stn_wild, n_found, prop_found, year, stationName) %>%
  arrange(carcID, year, binwt) %>%
  View() # Yes!! All the ones that failed here had 0 or 1 individuals.

# Let's do the same check, this time expanding to all model types and both stn/wild.
stats_all %>%
  filter(is.na(propsolve)) %>%
  select(propsolve, type, binwt, carcID, stn_wild, n_found, prop_found, year, stationName) %>%
  arrange(carcID, year, binwt) %>%
  View() # Yep! These all still failed because they had 0 or 1 individual.

# problem solved! As long as we interpret the NAs to 0s caveat correctly, these models should be legit.

# Attempting confidence intervals -----------------------------------------

# Load data
tar_load(stn_carcs)
tar_load(wild_carcs)
tar_load(data_cumul_wt_1)
tar_load(data_cumul_wt_2)
tar_load(data_cumul_wt_3)
tar_load(data_cumul_wt_4)
tar_load(data_cumul_wt_5)
tar_load(data_cumul_wt_6)
tar_load(data_cumul_wt_7)
data_stn <- c(data_cumul_wt_1, data_cumul_wt_2, data_cumul_wt_3, data_cumul_wt_4, data_cumul_wt_5, data_cumul_wt_6, data_cumul_wt_7)
tar_load(data_cumul_wt_1_wild)
tar_load(data_cumul_wt_2_wild)
tar_load(data_cumul_wt_3_wild)
tar_load(data_cumul_wt_4_wild)
tar_load(data_cumul_wt_5_wild)
data_wild <- c(data_cumul_wt_1_wild, data_cumul_wt_2_wild, data_cumul_wt_3_wild, data_cumul_wt_4_wild, data_cumul_wt_5_wild)
tar_load(mods_cumul_wt) # with seeds
tar_load(mods_cumul_wt_wild)

search_sfs <- data.frame(type = rep("cumul", length = length(mods_cumul_wt)),
                         lower_min = NA, lower_max = NA, upper_min = NA, upper_max = NA, ci_lower = NA, ci_upper = NA, carcID = map_dbl(stn_carcs, "carcID"))

search_wild <- data.frame(type = rep("cumul", length = length(mods_cumul_wt_wild)),
                         lower_min = NA, lower_max = NA, upper_min = NA, upper_max = NA, ci_lower = NA, ci_upper = NA, carcID = map_dbl(wild_carcs, "carcID"))

search_sfs[1,2:5] <- c(0,2, 5,15) # fixed/
search_sfs[2,2:5] <- c(0, 20, 100, 120) # fixed/
search_sfs[3,2:5] <- c(0,2, 6,10) # fixed/
search_sfs[4,2:5] <- c(0,1, 4,6) # fixed
search_sfs[5,2:5] <- c(0, 5, 12, 20) # fixed
search_sfs[6,2:5] <- c(NA, NA, 2,4) # fixed
search_sfs[9,2:5] <- c(0,1, 2,4) # fixed
search_sfs[10,2:5] <- c(0, 10, 80, 120) # fixed
search_sfs[14,2:5] <- c(0, 1, 3, 4) # fixed
search_sfs[15,2:5] <- c(0, 20, 210, 220) # fixed
search_sfs[18,2:5] <- c(NA, NA, 400, 500) # fixed 
search_sfs[20,2:5] <- c(NA, NA, 0, 0.2) # fixed
search_sfs[21,2:5] <- c(NA, NA, 3, 4) # fixed
search_sfs[22,2:5] <- c(0, 20, 1300, 1500) # fixed
search_sfs[23,2:5] <- c(NA, NA, 0.5, 1) # fixed
search_sfs[24,2:5] <- c(0, 0.2, 0.6, 0.8) #fixed
search_sfs[27,2:5] <- c(0, 0.5, 2, 4) # fixed
search_sfs[28,2:5] <- c(NA, NA, 0.5, 1) # fixed
search_sfs[29,2:5] <- c(NA, NA, 1, 1.5) # fixed
search_sfs[32,2:5] <- c(0, 0.05, 0.35, 0.5) # fixed
search_sfs[34,2:5] <- c(NA, NA, 0.5, 1) # fixed
search_sfs[35,2:5] <- c(NA, NA, 6, 9) # fixed
search_sfs[37,2:5] <- c(0, 0.5, 1, 2)#fixed
search_sfs[38,2:5] <- c(0, 0.5, 1, 2) # fixed
search_sfs[39,2:5] <- c(140, 200, NA, NA) # fixed
search_sfs[40,2:5] <- c(NA, NA, 1.5, 2) # fixed
search_sfs[44,2:5] <- c(NA, NA, 0.5, 1) # fixed
search_sfs[47,2:5] <- c(20, 40, NA, NA) # fixed
search_sfs[48,2:5] <- c(NA, NA, 100, 200) # fixed
search_sfs[49,2:5] <- c(0, 0.2, 0.7, 1) # fixed
search_sfs[50,2:5] <- c(NA, NA, 0.75, 1.5)# fixed
search_sfs[51,2:5] <- c(NA, NA, 80, 100) # fixed
search_sfs[52,2:5] <- c(NA, NA, 1, 1.5) # fixed
search_sfs[54,2:5] <- c(NA, NA, 30, 40) # fixed
search_sfs[55,2:5] <- c(0, 0.2, 0.75, 1.5) # fixed
search_sfs[56,2:5] <- c(0.5, 1, 4, 6) # fixed
search_sfs[58,2:5] <- c(0, 1, 2, 4) # fixed
search_sfs[59,2:5] <- c(NA, NA, 20, 40) # fixed
search_sfs[60,2:5] <- c(NA, NA, 0.5, 1) # fixed
search_sfs[61,2:5] <- c(0, 0.1, 4, 6) # fixed
search_sfs[62,2:5] <- c(0, 0.1, 4, 6) # fixed
search_sfs[63,2:5] <- c(10, 20, NA, NA) # fixed
# plotProfLik(which = 1, model = mods_cumul_wt[[65]], range = c(0, 2))

search_wild[1,2:5] <- c(0, .5, 1.5, 2.5) # fixed
search_wild[2,2:5] <- c(0, 1, 4, 5) # fixed
search_wild[3,2:5] <- c(0, 1, 20, 30) # fixed
search_wild[5,2:5] <- c(0, 1, 3, 4) # fixed
search_wild[6,2:5] <- c(NA, NA, 1, 2) # fixed
# spot checked the rest and they look fine
search_wild[7,2:5] <- c(0, 1, 4, 6)
search_wild[8,2:5] <- c(0, 2, 15, 17)
search_wild[9,2:5] <- c(0, 1, 4, 6)
search_wild[10,2:5] <- c(0, 1, 40, 50)
search_wild[11,2:5] <- c(0, 2, 10, 15)
search_wild[12,2:5] <- c(0, 2, 35, 45)
search_wild[14,2:5] <- c(0, 1, 8, 10)
search_wild[15,2:5] <- c(0, 1, 2, 4)
search_wild[16,2:5] <- c(0, 1, 4, 5)
search_wild[17,2:5] <- c(NA, NA, 6, 8)
search_wild[18,2:5] <- c(1, 3, 25, 35)
search_wild[19,2:5] <- c(NA, NA, 12, 16)
search_wild[20,2:5] <- c(NA, NA, 0.6, 0.8)
search_wild[21,2:5] <- c(4, 6, 50, 60)
search_wild[22,2:5] <- c(0, 2, 20, 25)
search_wild[23,2:5] <- c(NA, NA, 3, 4)
search_wild[24,2:5] <- c(NA, NA, 1, 2)
search_wild[25,2:5] <- c(0, 2, 9, 12)
search_wild[26,2:5] <- c(NA, NA, 150, 200)
search_wild[27,2:5] <- c(NA, NA, 3, 4)
search_wild[28,2:5] <- c(0, 0.1, 0.5, 1)
search_wild[29,2:5] <- c(0, 0.5, 1.5, 2.5)
search_wild[30,2:5] <- c(0, 0.5, 4, 5)
search_wild[31,2:5] <- c(0, 0.5, 1.5, 2)
search_wild[32,2:5] <- c(NA, NA, 3, 5)
search_wild[33,2:5] <- c(NA, NA, 1, 2)
search_wild[34,2:5] <- c(NA, NA, 3, 5)
search_wild[35,2:5] <- c(NA, NA, 2, 3)
search_wild[36,2:5] <- c(NA, NA, 2, 4)
search_wild[37,2:5] <- c(NA, NA, 1, 2)
search_wild[38,2:5] <- c(NA, NA, 10, 15)
search_wild[39,2:5] <- c(NA, NA, 0.5, 10)
search_wild[40,2:5] <- c(NA, NA, 2, 3)
search_wild[41,2:5] <- c(0, 1, 4, 5)
search_wild[42,2:5] <- c(NA, NA, 0.5, 1)
search_wild[43,2:5] <- c(0, 0.5, 3, 5)
search_wild[44,2:5] <- c(NA, NA, 1, 2)
search_wild[45,2:5] <- c(NA, NA, 10, 20)
search_wild[46,2:5] <- c(NA, NA, 2, 3)
plotProfLik(which = 1, model = mods_cumul_wt_wild[[46]], range = c(0, 4))

out <- vector(mode = "list", length = nrow(search_sfs))
for(i in 1:nrow(search_sfs)){
  tryCatch(
    expr = {
      cis <- as.data.frame(t(profLikCI(which = 1, model = mods_cumul_wt[[i]], 
                            lowerRange = search_sfs[i, 2:3], 
                            upperRange = search_sfs[i, 4:5])))
      ps_lower <- nbdaPropSolveByST(par=cis$`Lower CI`,nbdadata=data_stn[[i]])
      ps_upper <- nbdaPropSolveByST(par=cis$`Upper CI`,nbdadata=data_stn[[i]])
      cis$ps_lower <- ps_lower[1]
      cis$ps_upper <- ps_upper[1]
      out[[i]] <- cis
    },
    error = function(e){ 
      out[[i]] <- NULL
    }
  )
}
names(out) <- map_dbl(stn_carcs, "carcID")

out_wild <- vector(mode = "list", length = nrow(search_wild))
for(i in 1:nrow(search_wild)){
  tryCatch(
    expr = {
      cis <- as.data.frame(t(profLikCI(which = 1, model = mods_cumul_wt_wild[[i]], 
                                       lowerRange = search_wild[i, 2:3], 
                                       upperRange = search_wild[i, 4:5])))
      ps_lower <- nbdaPropSolveByST(par=cis$`Lower CI`,nbdadata=data_wild[[i]])
      ps_upper <- nbdaPropSolveByST(par=cis$`Upper CI`,nbdadata=data_wild[[i]])
      cis$ps_lower <- ps_lower[1]
      cis$ps_upper <- ps_upper[1]
      out_wild[[i]] <- cis
    },
    error = function(e){ 
      out_wild[[i]] <- NULL
    }
  )
}
names(out_wild) <- map_dbl(wild_carcs, "carcID")

cis <- purrr::list_rbind(out, names_to = "carcID") %>%
  mutate(carcID = as.numeric(carcID))
cis_wild <- purrr::list_rbind(out_wild, names_to = "carcID") %>%
  mutate(carcID = as.numeric(carcID))

cis_all <- bind_rows(cis, cis_wild)

stats_cumul_wt <- stats_all %>%
  filter(binwt == "wt", type == "cumul") %>%
  left_join(cis_all, by = "carcID") %>%
  mutate(sig_ci = case_when(`Lower CI` > 0 & !is.na(`Lower CI`) ~ T, .default = F))

# XXX THIS ONE!
stats_cumul_wt %>%
  ggplot(aes(x = factor(carcID), y = propsolve, color = factor(year)))+
  geom_hline(aes(yintercept = 0), linetype = 3, color = "black")+
  geom_segment(aes(y = ps_lower, yend = ps_upper))+
  geom_point(aes(pch = sig_ci))+
  scale_shape_manual(values = c(1, 19))+
  theme_classic()+
  coord_flip()+
  ggh4x::facet_grid2(rows = vars(year), cols = vars(stn_wild), 
                     scales = "free_y", independent = "y")+
  labs(y = "Prop. solved by social transmission", x = "Carcass")+
  theme(legend.position = "none")

# CIs for S instead of propsolve
stats_cumul_wt %>%
  filter(outputPar < 20) %>% # have to remove the high outliers
  ggplot(aes(x = factor(carcID), y = outputPar, color = factor(year)))+
  geom_hline(aes(yintercept = 0), linetype = 3, color = "black")+
  geom_segment(aes(y = `Lower CI`, yend = `Upper CI`))+
  geom_point()+
  scale_shape_manual(values = c(1, 19))+
  theme_classic()+
  coord_flip()+
  ggh4x::facet_grid2(rows = vars(year), cols = vars(stn_wild), 
                     scales = "free", independent = "all")+
  labs(y = "S", x = "Carcass", caption = "Note varying scales; also, these aren't marked sig/non-sig.")+
  theme(legend.position = "none")
# why are some missing CIs? I'm not sure-- some I think I couldn't get from the profLik graphs, others idk.
# XXX if time: check whether the ones that are missing these line up with the ones that have NAs for the manual plots above.

stats_cumul_wt %>%
  mutate(sig_num = case_when(ps_lower > 0 & !is.na(ps_lower) ~ 1, .default = 0)) %>%
  ggplot(aes(x = prop_found, y = sig_num, color = factor(year))) +
  geom_point(position = position_jitter(height = 0.02), alpha = 0.5, size = 2) + 
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, alpha = 0.2)+
  theme_minimal()+
  facet_wrap(~stn_wild)+
  theme(legend.position = "bottom", text = element_text(size = 14)) + 
  labs(y = "Social transmission detected", x = "Prop. vultures in diffusion")+
  scale_y_continuous(breaks = c(0, 1))

# S detected?
stats_cumul_wt %>%
  mutate(sig_num = case_when(ps_lower > 0 & !is.na(ps_lower) ~ 1, .default = 0)) %>%
  ggplot(aes(x = prop_found, y = sig_num)) +
  geom_point(position = position_jitter(height = 0.02), alpha = 0.5, size = 2) + 
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, alpha = 0.2)+
  theme_minimal()+
  facet_wrap(~stn_wild)+
  theme(legend.position = "bottom", text = element_text(size = 14)) + 
  labs(y = "Social transmission detected", x = "Prop. vultures in diffusion")+
  scale_y_continuous(breaks = c(0, 1))

# S strength
stats_cumul_wt %>%
  filter(sig_ci) %>%
  mutate(stn_wild = case_when(stn_wild == "stn" ~ "SFS",
                              stn_wild == "wild" ~ "Non-SFS",
                              .default = NA)) %>%
  filter(seeds, binwt == "wt", type == "cumul") %>%
  ggplot(aes(x = prop_found, y = outputPar)) +
  geom_point(size = 2, alpha = 0.75)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  facet_wrap(~stn_wild)+
  theme(legend.position = "bottom", text = element_text(size = 14)) + 
  labs(y = "Social transmission strength", x = "Prop. vultures in diffusion")

# S strength by year
stats_cumul_wt %>%
  filter(sig_ci) %>%
  mutate(stn_wild = case_when(stn_wild == "stn" ~ "SFS",
                              stn_wild == "wild" ~ "Non-SFS",
                              .default = NA)) %>%
  filter(seeds, binwt == "wt", type == "cumul") %>%
  ggplot(aes(x = prop_found, y = outputPar, color = factor(year))) +
  geom_point(size = 2, alpha = 0.75)+
  geom_smooth(method = "lm", alpha = 0.2)+
  theme_minimal()+
  facet_wrap(~stn_wild)+
  theme(legend.position = "bottom", text = element_text(size = 14)) + 
  labs(y = "Social transmission strength", x = "Prop. vultures in diffusion", color = "Year")

