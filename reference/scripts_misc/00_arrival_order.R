library(tidyverse)
library(here)

# Get rank ordering of arrivals -------------------------------------------
hires_carcasses <- read_csv(here("data/hires_carcasses.csv")) %>% group_by(ID) %>% group_split()
load(here("data/carcass_data.Rda"))

# carc_data <- map2(carcass_data, hires_carcasses, ~.x %>%
#                     select(timestamp_il, dateOnly_il, Nili_id, location_long, 
#                            location_lat, dist_m, state, state_description, time_relative) %>% 
#                     bind_cols(st_drop_geometry(.y %>% select(ID, date, time, datetime, long, lat))) %>%
#                     rename("carc_ID" = ID,
#                            "carc_date" = date,
#                            "carc_time" = time,
#                            "carc_datetime" = datetime,
#                            "carc_long" = long,
#                            "carc_lat" = lat) %>%
#                     mutate())
# save(carc_data, file = here("data/carc_data.Rda"))
load(here("data/carc_data.Rda"))
  
# Let's calculate detection order and then arrival order at the carcass (light blue and red).
# I don't think we should go past 5 days post-carcass, since at that point there could be a new carcass present. # XXX side question: how long between carcasses per feeding station?
carc <- purrr::list_rbind(carc_data)
carc_after <- carc %>%
  filter(time_relative > 0, time_relative < 120)
firsts <- carc_after %>%
  filter(state %in% c("vis_flying", "at_carcass")) %>%
  select(carc_ID, Nili_id, state, state_description, time_relative) %>%
  arrange(carc_ID, time_relative) %>%
  group_by(carc_ID, state, Nili_id) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(carc_ID, state) %>%
  arrange(time_relative, .by_group = T) %>%
  mutate(rank_order = 1:n())

# Now let's visualize it by individual to see how much variability we have
firsts %>%
  ggplot(aes(x = Nili_id, y = rank_order))+
  geom_boxplot()+
  facet_wrap(~state_description, ncol = 1)+
  theme_classic() # okay, each individual varies a lot. There are maybe some that tend to be earlier or later, but not by a lot...

by_indiv <- firsts %>%
  ungroup() %>%
  group_by(Nili_id, state) %>%
  summarize(mn_rank = mean(rank_order),
            min_rank = min(rank_order),
            mn_latency = mean(time_relative),
            min_latency = min(time_relative))

# How does mean rank relate to mean latency?
by_indiv %>%
  ggplot(aes(x = mn_latency, y = mn_rank, col = state))+
  geom_point()+
  theme_classic()+
  scale_color_manual(values = c("skyblue", "red"))+
  geom_smooth(method = "lm")+
  ylab("Mean rank order")+
  xlab("Mean latency (hours)") # ok, it's kind of interesting that these display opposite trends. 
# Some thoughts: first of all, those are really high mean latencies. I suspect that the distribution of latencies for a given vulture might be pretty right-skewed; I wonder if mean is a good measure or not. 
# Second, I'm wondering how the latency distributions will be affected by the timing of nights...
# Third, it's interesting that for detection, latency is positively related to rank, but that's not true for arrival at the carcass--the ones that tend to arrive sooner are not necessarily the ones that tend to arrive first; in fact it's the opposite. That's probably because the ones with lower latencies overall are probably the carcasses at the center of everything?

# What about literally the distributions of latencies by individual?
firsts %>%
  ggplot(aes(x = time_relative, fill = state_description, 
             col = state_description, group = Nili_id))+
  geom_histogram()+
  facet_wrap(~state_description)+
  scale_color_manual(values = c("skyblue", "red"))+
  scale_fill_manual(values = c("skyblue", "red"))+
  theme_classic() # Interesting. definitely very right-skewed. And keep in mind these are *first* visits to the carcass, for each individual. So this suggests that we should analyze the data separately--one way would be for carcasses where this individual arrived on the first day (not as simple as "first 24 hours"--would have to be more like "before the first night"), and another would be for carcasses where it arrived on the second day or later. Presumably different dynamics for whether there are following-from-the-roost options or not.

# A random individual
set.seed(3)
rand <- sample(unique(firsts$Nili_id), 1)
firsts %>%
  filter(Nili_id == rand) %>%
  ggplot(aes(x = ))