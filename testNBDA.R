# Load packages and gps data
library(tidyverse)
library(here)
library(targets)
source(here("params.R"))

gps <- data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv") %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>%
  st_transform(32636) # this data originally comes from the script 01_classify_localize_bouts.R, which just pulled it directly from Movebank. It should contain all tagged individuals in the population, not merely the ones that had high-frequency ACC tags.
# So this leaves the question of why there seem to be so many deposited carcasses that don't have any individuals coming within 1000m of them! It's very odd.
# Let's run this through with all of the 2024 carcasses in the south and see how frequent it is to have few individuals arriving.

tar_load(all_carcasses_annotated)
tar_load(all_bouts_annotated)
tar_load(bbox_south)
# Restrict bouts and carcasses to south in 2024
aca <- all_carcasses_annotated %>% filter(year == "2024") %>% st_crop(bbox_south)
aba <- all_bouts_annotated %>% st_as_sf() %>% filter(year == "2024") %>% st_crop(bbox_south)

# Take a subset of this data to test. 2 wild and 2 placed carcasses -------
# Hahalak_mount 2023-03-20
# carcs <- aca %>%
#   group_by(carcType) %>%
#   slice_sample(n = 3) %>%
#   pull(carcID)
# carcs
# test <- "4872568"

# one of them is 4872568, which is at Hava_cliff
# sample_carcs <- aca %>%
#   filter(carcID %in% test)

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
sample_gps %>%
  st_drop_geometry() %>%
  ungroup() %>%
  select(carcID, individual_id, informed) %>%
  distinct() %>%
  ungroup() %>%
  group_by(carcID, individual_id) %>%
  arrange(desc(informed), .by_group = T) %>%
  ungroup() %>%
  group_by(carcID) %>%
  summarize(ever_informed = sum(informed),
            total = length(unique(individual_id)),
            never_informed = total-ever_informed) %>%
  arrange(ever_informed) %>%
  View() # okay this is more like what I thought! So now I'm wondering why I wasn't getting these results earlier. Hmm...

# When they're first informed
a <- sample_gps %>%
  filter(informed == T) %>%
  group_by(carcID, individual_id) %>%
  slice_head() %>%
  arrange(timestamp)

a %>%
  ggplot(aes(x = timestamp, y = fct_reorder(factor(individual_id), timestamp)))+
  geom_point() # okay, this looks reasonable! I don't love the plot, but that's okay, we'll come back to it. Let's get some more carcasses for now.
