# Script for exploring the data created in prepare_data.R
# This is data for the INPA carcasses from the focal months in 2023 and 2024
# Packages
library(mapview)
library(sf)
library(tidyverse)

# Load data
load(here("test_dynamic_nbda/data/fl_allday_bin_fixed.Rda"))
load(here("test_dynamic_nbda/data/fl_allday_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/fl_cumulative_bin_fixed.Rda"))
load(here("test_dynamic_nbda/data/fl_cumulative_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/fl_1hr_bin_fixed.Rda"))
load(here("test_dynamic_nbda/data/fl_1hr_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/fl_3hr_bin_fixed.Rda"))
load(here("test_dynamic_nbda/data/fl_3hr_bin_fixed_see.Rda"))
load(here("test_dynamic_nbda/data/roosts_bin_fixed.Rda"))
load(here("test_dynamic_nbda/data/roosts_bin_fixed_see.Rda"))

load(here("test_dynamic_nbda/data/inpa_carcs.Rda"))

load(here("test_dynamic_nbda/data/oa.Rda"))
load(here("test_dynamic_nbda/data/oa_see.Rda"))
load(here("test_dynamic_nbda/data/oa_num.Rda"))
load(here("test_dynamic_nbda/data/oa_see_num.Rda"))
load(here("test_dynamic_nbda/data/firsts.Rda"))
load(here("test_dynamic_nbda/data/firsts_see.Rda"))
load(here("test_dynamic_nbda/data/roosts_bin_nets.Rda"))
load(here("test_dynamic_nbda/data/roosts_bin_nets_see.Rda"))
load(here("test_dynamic_nbda/data/fl_allday_bin_nets.Rda"))
load(here("test_dynamic_nbda/data/fl_allday_bin_nets_see.Rda"))
load(here("test_dynamic_nbda/data/fl_cumulative_bin_nets.Rda"))
load(here("test_dynamic_nbda/data/fl_cumulative_bin_nets_see.Rda"))
load(here("test_dynamic_nbda/data/fl_1h_bin_nets.Rda"))
load(here("test_dynamic_nbda/data/fl_1h_bin_nets_see.Rda"))
load(here("test_dynamic_nbda/data/fl_3h_bin_nets.Rda"))
load(here("test_dynamic_nbda/data/fl_3h_bin_nets_see.Rda"))

# 1. I noticed that a lot of carcasses don't have any tagged birds that ever go there. Why? What's going on?
## Maybe this is explained by the hour of the day when the carcass is placed?
df <- bind_rows(inpa_carcs) %>%
  mutate(time_of_day = lubridate::hour(datetime),
         year = lubridate::year(datetime)) %>%
  mutate(number_of_firsts = map_dbl(firsts, ~.x %>% filter(!is.na(local_identifier)) %>% nrow(.)),
         number_of_seen = map_dbl(firsts_see, ~.x %>% filter(!is.na(local_identifier)) %>% nrow(.))) %>%
  pivot_longer(cols = contains("number_of"), names_to = "Measure", values_to = "number") %>%
  mutate(Measure = case_when(Measure == "number_of_firsts" ~ "Arrivals",
                             Measure == "number_of_seen" ~ "Detections"))

df %>%
  ggplot(aes(x = time_of_day, y = number, col = Measure, fill = Measure))+
  geom_point(aes(size = carcassWeight), alpha = 0.5)+
  geom_smooth(method = "lm", alpha = 0.1)+
  facet_wrap(~year)+
  theme_minimal()+ # doesn't seem to be related to hour of day
  labs(y = "Number of vultures",
       x = "Hour of carcass placement",
       size = "Carcass\nweight",
       caption = "Arrivals and detections within 4 days of carcass placement.\nArrival: vulture on ground (<5m/s) within 400m of carcass.\nDetection: vulture within 1000m of carcass.")


## maybe it's related to the size of the carcass
df %>%
  ggplot(aes(x = carcassWeight, y = number, col = Measure, fill = Measure))+
  geom_point(aes(size = time_of_day), alpha = 0.5)+
  geom_smooth(method = "lm", alpha = 0.1)+
  facet_wrap(~year)+
  theme_minimal()+ # this also doesn't produce a clear relationship.
  labs(y = "Number of vultures",
       x = "Carcass weight",
       size = "Hour of carcass placement",
       caption = "Arrivals and detections within 4 days of carcass placement.\nArrival: vulture on ground (<5m/s) within 400m of carcass.\nDetection: vulture within 1000m of carcass.")

## Let's see a map of the carcasses, colored by the number of individuals that visit over the course of 4-ish days
mapview(df %>% filter(year == 2023), zcol = "number_of_firsts")
mapview(df %>% filter(year == 2024), zcol = "number_of_firsts")
## There definitely seems to be some relationship between the centrality of the carcass and the number of visits, but I'm surprised at how stark the numbers are, given how many carcasses are placed.

# For the networks, we are already only dealing with the carcasses that have visits from vultures
load(here("test_dynamic_nbda/data/has_visits.Rda"))
carcs <- inpa_carcs[has_visits] # get the carcasses corresponding to the networks, in case we need them

tolong <- function(list, id, tp){
  df <- map(list, ~.x %>%
              mutate(ID1 = row.names(.)) %>%
              pivot_longer(cols = -ID1, names_to = "ID2", values_to = "inter"))
  out <- rbindlist(df, idcol = id) %>% mutate(type = tp)
  return(out)
}

# here, "carc" is the numerical index of which carcass we're using, after already filtering by "has_visits"
compile_networks_long <- function(carc){
  # Get acquisition event dates
  idx <- firsts[has_visits][[carc]] %>%
    group_by(dateOnly) %>% summarize(n = n()) %>%
    pull(n)
  idx_day <- data.frame(acq = 1:sum(idx), day = rep(1:length(idx), times = idx))
  
  # Get long-format data for each type of network
  r_long <- tolong(roosts_bin_fixed[[carc]], id = "day", tp = "roost")
  f_a_long <- tolong(fl_allday_bin_fixed[[carc]], id = "day", tp = "fl_a")
  f_c_long <- tolong(fl_cumulative_bin_fixed[[carc]], id = "acq", tp = "fl_c")
  f_1h_long <- tolong(fl_1hr_bin_fixed[[carc]], id = "acq", tp = "fl_1h")
  f_3h_long <- tolong(fl_3hr_bin_fixed[[carc]], id = "acq", tp = "fl_3h")
  
  # Join the networks and convert to wide (separately for the daily networks and the per-acquisition networks)
  ## daily networks (roost and daily flight)
  inters_daily <- bind_rows(f_a_long, r_long) %>%
    mutate(dyad_id = paste(ID1, ID2, sep = "_"))
  inters_daily_wide <- pivot_wider(inters_daily, id_cols = c("dyad_id", "ID1", "ID2", "day"), names_from = "type", values_from = "inter")
  
  ## per-acquisition networks (cumulative, 1 hour, 3 hour)
  inters <- bind_rows(f_c_long, f_1h_long, f_3h_long) %>%
    left_join(idx_day) %>%
    mutate(dyad_id = paste(ID1, ID2, sep = "_"))
  inters_wide <- inters %>%
    pivot_wider(id_cols = c("dyad_id", "ID1", "ID2", "acq", "day"), names_from = "type", values_from = "inter")
  head(inters_wide)
  
  # Combine into a single wide-format data frame containing all networks
  inters_all_wide <- left_join(inters_wide, inters_daily_wide) # should include all the different networks at different scales in the same data frame.
  return(inters_all_wide)
}
networks_long <- map(1:length(carcs), compile_networks_long) 
names(networks_long) <- map_dbl(carcs, "carcID")
networks_long <- as.data.frame(data.table::rbindlist(networks_long, idcol = "carcID"))

# Visualizations ----------------------------------------------------------
plt <- function(g, title_slug, i){
  title <- paste(title_slug, i, sep = " ")
  ggraph(g)+
    geom_edge_link()+
    geom_node_label(aes(label = name))+
    theme_graph()+
    ggtitle(title)
}
carc <- inpa_carcs[[13]] # carcass information
carc # 2023-03-30 12:54:29, Hahalak_mount
oa[[13]] # order of arrivals to this carcass
mapview(carc)
tr <- roosts_bin_nets[[13]]
tr_g <- map2(tr, 1:length(tr), ~plt(.x, "Roosts, night", .y))

tfa <- fl_allday_bin_nets[[13]]
tfa_g <- map2(tfa, 1:length(tfa), ~plt(.x, "Flight, day", .y))

tfc <- fl_cumulative_bin_nets[[13]]
tfc_g <- map2(tfc, 1:length(tfc), ~plt(.x, "Flight (cumulative),\nacquisition event", .y))

tf1 <- fl_1h_bin_nets[[13]]
tf1_g <- map2(tf1, 1:length(tf1), ~plt(.x, "Flight (1 hour before),\nacquisition event", .y))

tf3 <- fl_3h_bin_nets[[13]]
tf3_g <- map2(tf3, 1:length(tf3), ~plt(.x, "Flight (3 hours before),\nacquisition event", .y))

# Correlations?
# Jamie wanted to know: does who you roost with predict who you fly with?
# DeepSeek suggests running a GLMM to account for the repeated-measures structure of the data
library(lme4)

# Fit a GLMM with nested random effects for period_id within carcID
test <- networks_long %>%
  filter(carcID %in% unique(networks_long$carcID)[1:2])
test_model <- glmer(fl_a ~ roost * day + (1 | dyad_id) + (1 | carcID/day), 
                    data = test, 
                    family = binomial(link = "logit")) # even this is taking a really long time to run... maybe as a proxy I could do a bunch of chi-squared tests and do a multiple testing correction? # XXX this doesn't work--runs forever even with just two carcasses

# Summarize the model
summary(test_model) 
