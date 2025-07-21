# Investigate carcass sightings w/r/t seeds
library(tidyverse)
library(targets)

# It seems suspicious that there are so many seed individuals
tar_load(seeds_see)
length(seeds_see)
tar_load(oa_see)
length(oa_see) # 53--number of carcasses with sightings
tar_load(has_sightings)
length(has_sightings) # 57--T or F for each carcass
tar_load(firsts_see)
length(firsts_see) # all carcasses--first sightings by each individual (4km)
tar_load(gps_all) # includes points before carcass
length(gps_all) # 57--includes all carcasses
gps_has_sightings <- gps_all[has_sightings]

# What proportion of the individuals are currently shown as seeds?
df <- data.frame(n = map_dbl(oa_see, length),
                 n_seed = map_dbl(seeds_see, length)) %>%
  mutate(prop = n_seed/n)

df %>%
  ggplot(aes(x = prop))+
  geom_histogram()

df %>%
  ggplot(aes(x = n, y = prop))+
  geom_point()+
  geom_smooth(method = "lm") # when more individuals find the carcass, a greater proportion of them are seeds. Alternatively, when a greater proportion of individuals are seeds, more individuals find the carcass overall

# given this, and given the fact that we don't necessarily trust the times when INPA placed the carcasses, I'm not sure I buy this. I wonder if these "seeds" are actually individuals that are landing at the carcass early, and the time of carcass deposition was recorded too late.

# Let's investigate this with some visualizations
df[2,] #69 individuals overall, 21 "seed" individuals, 30% of individuals are seeds. This seems like a decent one to start with.

dat <- gps_has_sightings[[2]]

plots <- vector(mode = "list", length = length(gps_has_sightings))
for(i in 1:length(gps_has_sightings)){
  cid <- gps_has_sightings[[i]]$carcID[1]
  p <- gps_has_sightings[[i]] %>%
    filter(local_identifier %in% seeds_see[[i]]) %>%
    filter(time_since_carcass < 5 & time_since_carcass > -5, dist_to_carcass < 25000) %>%
    ggplot(aes(x = as.numeric(time_since_carcass), y = dist_to_carcass))+
    annotate("rect", xmin=-0.5, xmax=0, ymin=0, ymax=4000, alpha=0.1, fill="black")+
    geom_vline(aes(xintercept = 0), alpha = 0.75, linetype = 2, linewidth = 0.5)+
    geom_hline(aes(yintercept = 4000), alpha = 0.75, linetype = 2, linewidth = 0.5)+
    geom_point(alpha = 0.5, aes(col = ground_speed))+
    scale_color_viridis_c()+
    theme_classic()+
    theme(legend.position = "none")+
    ggtitle(cid)
  plots[[i]] <- p
}

plots[[2]]
plots[[10]]
plots[[11]]

# Based on looking at these, it seems like most of these demonstrators can be accounted for by nearby roosts.
# I might want to change the criteria for having seen a carcass to 1km (if stationary) or 4km (if in flight). Is that reasonable?

# To ask vulture people:

