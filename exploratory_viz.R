# Script for exploring the data created in prepare_data.R
# This is data for the INPA carcasses from the focal months in 2023 and 2024
# Packages
library(mapview)
library(sf)
library(tidyverse)

# Load data
load(here("test_dynamic_nbda/data/fl_allday_bin.Rda"))
load(here("test_dynamic_nbda/data/fl_cumulative_bin.Rda"))
load(here("test_dynamic_nbda/data/fl_1hr_bin.Rda"))
load(here("test_dynamic_nbda/data/fl_3hr_bin.Rda"))
load(here("test_dynamic_nbda/data/roosts_bin.Rda"))
load(here("test_dynamic_nbda/data/oa.Rda"))
load(here("test_dynamic_nbda/data/oa_num.Rda"))
load(here("test_dynamic_nbda/data/firsts.Rda"))
load(here("test_dynamic_nbda/data/inpa_carcs.Rda"))

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

# Separate out the ones that have visits and the ones that don't ----------
have_visits <- map_dbl(oa, ~sum(!is.na(.x))) > 0

test <- roosts_bin[have_visits][[1]]
length(test)
nodes <- names(test)
graph_from_adjacency_matrix(test[[1]])
