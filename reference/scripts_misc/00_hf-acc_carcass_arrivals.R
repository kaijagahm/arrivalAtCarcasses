# Load packages and gps data
library(tidyverse)
library(here)
library(targets)
source(here("params.R"))

gps_2023 <- data.table::fread("data/ACC/2023_hf_period/created/gps_2023.csv")
gps_2024 <- data.table::fread("data/ACC/2024_hf_period/created/gps_2024.csv")
gps <- bind_rows(gps_2023, gps_2024) %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = "WGS84") %>%
  st_transform(32636)
tar_load(all_carcasses_annotated)
tar_load(all_bouts_annotated)
aca <- all_carcasses_annotated
aba <- all_bouts_annotated

# Get data within 1km of carcass ------------------------------------------
carcass_gps_data <- vector(mode = "list", length = nrow(aca))
for(i in 1:length(carcass_gps_data)){
  carcassid <- aca$carcID[i]
  date <- aca$dateOnly[i]
  date_plus <- date + days(carcass_dur_days)
  
  # Get gps data in the right date range
  in_date_range <- gps %>%
    filter(timestamp >= date & timestamp <= date_plus)
  
  # Get gps data within distance threshold
  in_date_range <- in_date_range %>%
    mutate(dist_to_carcass = as.numeric(st_distance(in_date_range, aca[i,])))
  close_enough <- in_date_range %>%
    filter(dist_to_carcass <= dist_gps_consider)
  
  # Attach the carcass ID
  close_enough <- close_enough %>%
    mutate(carcID = carcassid) %>%
    mutate(time_since_carcass = timestamp-aca$datetime[i])
  
  carcass_gps_data[[i]] <- close_enough
  cat("done with iteration", i, "\n")
}
map_dbl(carcass_gps_data, nrow) # okay good, reasonable number of rows for most of the carcasses!

# Categorize that data ----------------------------------------------------
## Ground vs. flying
### Ground: far, near, at
### Flying: detect, approach
categorize <- function(data){
  # Ground vs. flying
  data <- data %>%
    mutate(behav = case_when(ground_speed >= 5 ~ "flying",
                             .default = "ground"))
  
  # Categorize
  categorized <- data %>%
    mutate(dist_category = case_when(behav == "ground" & 
                                       dist_to_carcass <= dist_at_ground ~ "at",
                                     behav == "ground" &
                                       dist_to_carcass > dist_at_ground &
                                       dist_to_carcass <= dist_near_ground ~ "near",
                                     behav == "ground" &
                                       dist_to_carcass > dist_near_ground ~ "far",
                                     behav == "flying" &
                                       dist_to_carcass <= dist_near_flying ~ "near",
                                     behav == "flying" &
                                       dist_to_carcass > dist_near_flying &
                                       dist_to_carcass <= dist_viz_flying ~ "viz",
                                     .default = NA)) %>%
    mutate(category = paste(behav, dist_category, sep = "_"))
  
  return(categorized)
}

categorized <- map(carcass_gps_data, ~categorize(.x))
carcass_approach_data <- purrr::list_rbind(categorized)
carcass_approach_data <- carcass_approach_data %>%
  mutate(category = factor(category, levels = c("ground_at", "ground_near", "ground_far", "flying_near", "flying_viz")))

write_rds(carcass_approach_data, file = here("data/created/carcass_approach_data.RDS"))

# Now some visualizations -------------------------------------------------
approach <- readRDS(here("data/created/carcass_approach_data.RDS"))
levels(approach$category)

pal <- c("red", "orange", "black", "skyblue1", "dodgerblue4")

# Group by carcass and 30 min interval
approach <- approach %>%
  left_join(st_drop_geometry(aca) %>%
              dplyr::select(carcID, stationName, "carc_datetime" = datetime, 
                            "carc_date" = dateOnly,
                            carcType)) %>%
  mutate(time_since_carcass = case_when(carcType == "inpa" ~ 
                                          difftime(timestamp, carc_datetime, units = "mins"),
                                        carcType == "wild" ~ 
                                          difftime(dateOnly, carc_date, units = "mins")))

int <- approach %>%
  mutate(interval = round(as.numeric(time_since_carcass)/60)) %>%
  group_by(carcID, carcType, carc_date, stationName, behav, dist_category, 
           category, interval) %>%
  summarize(points = n(),
            indivs = length(unique(individual_id)), 
            .groups = "drop")

wild <- int %>%
  filter(carcType == "wild")
inpa <- int %>%
  filter(carcType == "inpa")

rand_inpa <- sample(unique(inpa$carcID), 1)
int %>%
  filter(carcID == rand_inpa) %>%
  {ggplot(., aes(x = interval, y = indivs, fill = category, group = interval))+
  geom_col()+
  theme_classic()+
  scale_fill_manual(name = "Behavior",
                    values = pal)+
  labs(x = "Time since carcass",
       y = "# vultures",
       title = paste(.$stationName[1], " (", rand_inpa, ")", sep = ""),
       subtitle = paste(.$carc_date[1]))+
  geom_vline(aes(xintercept = 0), col = "black", linetype = 2)+
  theme(legend.position = "bottom", nrow = 2)}



