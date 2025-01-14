# Load packages and gps data
library(tidyverse)
library(here)
library(targets)
source(here("params.R"))
#devtools::install_github("whoppitt/NBDA")
library(NBDA)

gps <- data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv") %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>%
  st_transform(32636) # this data originally comes from the script 01_classify_localize_bouts.R, which just pulled it directly from Movebank. It should contain all tagged individuals in the population, not merely the ones that had high-frequency ACC tags.

tar_load(all_carcasses_annotated)
tar_load(all_bouts_annotated)
tar_load(bbox_south)
# Restrict bouts and carcasses to south in 2024
aca <- all_carcasses_annotated %>% filter(year == "2024") %>% st_crop(bbox_south)
aba <- all_bouts_annotated %>% st_as_sf() %>% filter(year == "2024") %>% st_crop(bbox_south)

# Get data within the right date range ------------------------------------------
carcass_gps_data <- vector(mode = "list", length = nrow(aca))
for(i in 1:length(carcass_gps_data)){
  carcassid <- aca$carcID[i]
  date <- aca$dateOnly[i]
  date_plus <- date + days(carcass_dur_days)
  
  # Get gps data in the right date range
  in_date_range <- gps %>%
    filter(timestamp >= date & timestamp <= date_plus)
  
  # Attach the carcass ID
  in_date_range <- in_date_range %>%
    mutate(carcID = carcassid)
  
  # Calculate distance to carcass
  dists <- as.numeric(st_distance(in_date_range, aca[i,]))
  in_date_range$dist_m <- dists
  
  carcass_gps_data[[i]] <- in_date_range
  cat("done with iteration", i, "\n")
}
map_dbl(carcass_gps_data, nrow) # all of these carcasses have associated GPS data, which makes sense since I didn't give any distance restriction, only time.

sample_gps <- purrr::list_rbind(carcass_gps_data) %>%
  sf::st_as_sf() %>%
  bind_cols(., st_coordinates(.))

# Classify informed vs. not based on distance (all days) -----------------------------
# XXX may need to treat wild and provisioned carcasses differently, but let's do a first pass...
sample_gps <- sample_gps %>%
  group_by(carcID, individual_id) %>%
  arrange(dateOnly, timestamp, .by_group = T) %>%
  mutate(informed = case_when(dist_m <= 1000 ~ T,
                              .default = NA)) %>%
  fill(informed, .direction = "down") %>%
  mutate(informed = case_when(is.na(informed) ~ F,
                              .default = informed))
table(sample_gps$informed)

# Some stats
informed_stats <- sample_gps %>%
  st_drop_geometry() %>%
  left_join(aca %>% select(carcID, carcType) %>% distinct()) %>%
  ungroup() %>%
  select(carcType, carcID, individual_id, informed) %>%
  distinct() %>%
  ungroup() %>%
  group_by(carcType, carcID, individual_id) %>%
  arrange(desc(informed), .by_group = T) %>%
  ungroup() %>%
  group_by(carcType, carcID) %>%
  summarize(ever_informed = sum(informed),
            total = length(unique(individual_id)),
            never_informed = total-ever_informed,
            prop_ever_informed = ever_informed/total) %>%
  arrange(ever_informed)%>%
  left_join(aca %>%
              dplyr::select(carcID, carcType, X, Y))# okay this is more like what I thought! So now I'm wondering why I wasn't getting these results earlier. Hmm...

informed_stats %>%
  ggplot(aes(x = carcType, y = prop_ever_informed, fill = carcType, col = carcType))+
  geom_violin(alpha = 0.2)+
  theme_classic()+
  geom_jitter(width = 0.1, pch = 1, size = 2)+
  theme(legend.position = "none")+
  labs(y = "Proportion ever informed",
       x = "Carcass type",
       title = "2024 carcasses",
       subtitle = paste0("Informed = within 1000m\nCarcass duration = ", 
                         carcass_dur_days, " days"))
# These different distributions might be due to the different way we're measuring time of deposition (we only have date for the wild carcasses, not also time). But it also could be due to real biological differences between what's happening in the INPA vs. wild situations.
# Maybe also a geographic difference-- wild carcasses are disproportionately located nearer to the center. Does this relate to geography?

mapview(aca, zcol = "carcType") # wild carcasses are generally more central, but not always, and some of them are way over in Jordan!

# Does latitude affect the proportion of the population that visits within 5 days?
informed_stats %>%
  ggplot(aes(x = Y, y = prop_ever_informed, col = carcType))+
  geom_point(pch = 1, size = 2)+
  geom_smooth(method = "lm")+
  theme_classic()+
  labs(y = "Proportion ever informed", x = "Carcass latitude") # I don't think this is a linear relationship. Let's try the same thing with a loess?

informed_stats %>%
  ggplot(aes(x = Y, y = prop_ever_informed, col = carcType))+
  geom_point(pch = 1, size = 2)+
  geom_smooth()+
  theme_classic()+
  labs(y = "Proportion ever informed", x = "Carcass latitude") # this makes much more sense. Mid latitudes are going to have many more individuals.

# I assume we'd find the same thing if we looked at longitude
informed_stats %>%
  ggplot(aes(x = X, y = prop_ever_informed, col = carcType))+
  geom_point(pch = 1, size = 2)+
  geom_smooth()+
  theme_classic()+
  labs(y = "Proportion ever informed", x = "Carcass longitude") # huh, a less straightforward pattern, but still definitely nonlinear.

# Need to probably directly calculate centrality in order to assess the effect of geography. But anyway, we can tell that the spatial placement of the carcass is very important to its dynamics! This aligns with what I would expect.

# I don't know if we can put these into a model... does that even work?
geo_mod <- lm(prop_ever_informed ~ X*Y + carcType, data = informed_stats)
summary(geo_mod) # yeah, no significant effect. Could be not enough data; could be this isn't the right way to look at centrality.

# Could do a density raster... but anyway, that's for later.

# Well, whatever! Let's grab some sample carcasses. -----------------------
# Going to choose some from the mid range proportions.
informed_stats %>%
  ggplot(aes(x = prop_ever_informed, col = carcType))+
  geom_density()

carcs <- informed_stats %>%
  filter(prop_ever_informed > 0.25 & prop_ever_informed < 0.6) %>%
  group_by(carcType) %>%
  sample_n(3) %>%
  pull(carcID)

dataset <- sample_gps %>%
  filter(carcID %in% carcs)

# Tangent to visualize the spread of info for a single carcass ------------
# Let's pick one and visualize the spread of information through time
one <- "4874955"
order <- dataset %>%
  filter(carcID == one) %>%
  group_by(individual_id) %>%
  arrange(desc(informed), timestamp, .by_group = T) %>%
  slice(1) %>%
  arrange(desc(timestamp)) %>%
  pull(individual_id)

dataset %>%
  filter(carcID == one) %>%
  ggplot(aes(x = timestamp, y = factor(individual_id, levels = order), 
             col = informed))+
  geom_point(pch = 1, alpha = 0.2)+
  theme_minimal()+
  theme(axis.text.y = element_blank())+
  labs(title = "Spread of information for one carcass",
       subtitle = one,
       y = "Individual",
       x = "Timestamp",
       caption = "Informed = have been within 1000m of carcass since it was placed")+
  scale_color_manual(name = "Informed",
                     values = c("lightgray", "firebrick3"))+
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))
# This would also be interesting to visualize if we instead did "have been within 1000m of carcass within the past 24 hours" or something

dataset %>%
  filter(carcID == one) %>%
  mutate(close = case_when(dist_m <= 1000 ~ T,
                           .default = F)) %>%
  ggplot(aes(x = timestamp, y = factor(individual_id, levels = order), 
             col = close))+
  geom_point(pch = 1, alpha = 0.2)+
  theme_minimal()+
  theme(axis.text.y = element_blank())+
  labs(title = "Instances of direct information\nacquisition for one carcass",
       subtitle = one,
       y = "Individual",
       x = "Timestamp",
       caption = "Close = currently within 1000m of carcass")+
  scale_color_manual(name = "Close",
                     values = c("lightgray", "dodgerblue3"))+
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 2)))




