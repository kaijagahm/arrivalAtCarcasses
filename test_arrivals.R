library(here)
library(tidyverse)
library(readxl)
library(sf)
library(mapview)
library(moveVis)
library(future)
library(targets)

# allow moveVis to use multiple cores for faster processing
use_multicore(n_cores = 10, verbose = TRUE)

tar_load(hires_tags)
hires_tags <- hires_tags %>%
  mutate(behavior = case_when(ground_speed > 5 ~ "flying",
                              ground_speed <= 5 ~ "ground",
                              .default = NA))
carcasses <- read_excel(here("data/FeedingData from 2018_2024_Translated.xlsx"))
carcasses_simple <- carcasses %>% select(ID, `Date Event`, `Event time`, `WGS84 - LONG`, `WGS84 - LAT`, `Inspection area - RTG`, `Merhav - RTG`, `RTG district`)
names(carcasses_simple) <- c("ID", "date", "time", "long", "lat", "inspectionAreaRTG", "merhavRTG", "RTGDistrict")
carcasses_simple <- carcasses_simple %>%
  mutate(across(c("date", "time"), as.character)) %>%
  mutate(datetime = paste0(substr(date, 1, 10), " ", substr(time, 12, 19)),
         datetime = lubridate::ymd_hms(datetime, tz = "Israel"),
         date = lubridate::date(datetime)) %>%
  mutate(time = substr(time, 12, 19))
carcasses_sf <- sf::st_as_sf(carcasses_simple, coords = c("long", "lat"), crs = "WGS84", remove = F)
mapview(carcasses_sf %>% filter(datetime > lubridate::ymd("2024-01-01")), zcol = "date", legend = F)
save(carcasses_sf, file = here("data/carcasses_sf.Rda"))

# Grab just the carcasses that fell during the period of hi-res tagging (the first one, not this year's yet)
mindate <- min(hires_tags$dateOnly_il)
maxdate <- max(hires_tags$dateOnly_il)
hires_carcasses <- carcasses_sf %>%
  filter(datetime >= mindate & datetime <= maxdate) %>%
  mutate(lab = paste0("(", round(long, 5), ", ", 
                round(lat, 5), ")\n",
                inspectionAreaRTG, "\n",
                merhavRTG, "\n",
                RTGDistrict))
dim(hires_carcasses) # 70 carcasses were placed during this time. That's going to be our maximum sample size.

# for each carcass, get the data to go with it. Let's go to the beginning of the day when the carcass was placed, since we know 1) the times reported may not be exact and 2) the vultures sometimes circle when they can see the car coming.

# fn <- function(idx){
#   carcass <- hires_carcasses[idx,]
#   carcass_date <- carcass$date
#   carcass_label <- carcass$lab
#   dat <-  hires_tags %>%
#     filter(dateOnly_il %in% 
#              seq.Date(from = carcass_date-1, to = carcass_date+10, by = "day")) %>%
#     mutate(time_relative = difftime(timestamp_il, carcass$datetime, units = "hours")) %>%
#     mutate(lab = carcass_label)
#   distances_m <- as.numeric(sf::st_distance(dat, carcass))
#   if(length(distances_m) != nrow(dat)){
#     stop("length doesn't match")
#   }else{
#     dat$dist_m <- distances_m
#   }
#   dat <- dat %>%
#     mutate(vis_flying = ifelse(dist_m <= 1000 & behavior == "flying", TRUE, FALSE),
#            approach_flying = ifelse(dist_m <= 500 & behavior == "flying", TRUE, FALSE),
#            approach_ground = ifelse(dist_m <= 150 & behavior == "ground", TRUE, FALSE),
#            near_ground = ifelse(dist_m <= 50 & behavior == "ground", TRUE, FALSE))
#   return(dat)
# }
# 
# carcass_data <- map(1:nrow(hires_carcasses), fn, .progress = T)
# 
# save(carcass_data, file = here("data/carcass_data.Rda"))
load(here("data/carcass_data.Rda"))


# Add behavior states -----------------------------------------------------
carcass_data <- map(carcass_data, ~.x %>% 
      mutate(state = case_when(vis_flying & !approach_flying ~ "vis_flying",
                               approach_flying ~ "approach_flying",
                               approach_ground & !near_ground ~ "approach_ground",
                               near_ground ~ "near_ground",
                               behavior == "flying" & !vis_flying & 
                                 !approach_flying ~ "far_flying",
                               behavior == "ground" & !approach_ground & 
                                 !near_ground ~ "far_ground",
                               .default = NA)))

test <- carcass_data[[10]]

test_zoomed <- test %>%
  filter(location_long > 35.02, location_long < 35.1, 
         location_lat < 31.0, location_lat > 30.8) %>%
  ggplot(aes(x = location_long, y = location_lat))+
  geom_point(size = 3, aes(col = state, group = Nili_id), alpha = 0.5)+
  theme_classic()+
  scale_color_manual(values = c("skyblue", "orange", "navy", "black", "red", "blue"))+
  geom_point(data = hires_carcasses[10,], aes(x = long, y = lat), size = 1, col = "magenta")+
  guides(color = guide_legend(override.aes = list(size = 3)))+
  theme(legend.position = "bottom")
ggsave(test_zoomed, filename = here("behavior_around_carcass_10.png"), width = 5, height = 5)

# What about time series for individuals?
test %>%
  ggplot(aes(x = timestamp, y = dist_m, col = state))+
  geom_line(aes(group = Nili_id))+
  geom_vline(data = hires_carcasses[10,], aes(xintercept = datetime), col = "magenta", linetype = 2)+
  scale_color_manual(values = c("skyblue", "orange", "navy", "black", "red", "blue"))+
  theme_classic()+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(override.aes = list(linewidth = 3))) # this is much more informative, and very similar to the plots I made before. 
# This plot clearly demonstrates that 3 days is an appropriate length of time for this carcass--after the third day they stopped visiting the site. Or I suppose it's possible that they moved the carcass around, but I think we can consider it to have been exhausted.

# Now let's do the same thing but restrict it to just individuals within 10km (10000 m) 
test %>%
  mutate(dist_m = case_when(dist_m > 10000 ~ NA, .default = dist_m)) %>% # have to do this weird hack instead of filtering to avoid lines getting connected across areas where they shouldn't be connected.
  ggplot(aes(x = timestamp, y = dist_m, col = state))+
  geom_line(aes(group = Nili_id))+
  geom_vline(data = hires_carcasses[10,], aes(xintercept = datetime), col = "magenta", linetype = 2)+
  scale_color_manual(values = c("skyblue", "orange", "navy", "black", "red", "blue"))+
  theme_classic()+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(override.aes = list(linewidth = 3))) # this is much better. Note the red lines going up--this is because the segments are getting colored according to their first point, so if a bird starts nearby on the ground but then changes to e.g. far_flying at the next timepoint, the line between the two timepoints will be red, not navy.

# Zoom in even more, to within 5km
# Now let's do the same thing but restrict it to just individuals within 10km (10000 m) 
test %>%
  mutate(dist_m = case_when(dist_m > 5000 ~ NA, .default = dist_m)) %>% # have to do this weird hack instead of filtering to avoid lines getting connected across areas where they shouldn't be connected.
  ggplot(aes(x = timestamp, y = dist_m, col = state))+
  geom_line(aes(group = Nili_id))+
  geom_vline(data = hires_carcasses[10,], aes(xintercept = datetime), col = "magenta", linetype = 2)+
  scale_color_manual(values = c("skyblue", "orange", "navy", "black", "red", "blue"))+
  theme_classic()+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(override.aes = list(linewidth = 3))) 

# Now another zoom, to just the day when the carcass was deposited. Let's look at the arrival dynamics on that day.
test %>%
  filter(timestamp >= hires_carcasses[10,]$datetime & timestamp < "2023-06-19 18:00:00") %>%
  mutate(dist_m = case_when(dist_m > 5000 ~ NA, .default = dist_m)) %>% # have to do this weird hack instead of filtering to avoid lines getting connected across areas where they shouldn't be connected.
  ggplot(aes(x = timestamp, y = dist_m, col = state))+
  geom_line(aes(group = Nili_id))+
  geom_vline(data = hires_carcasses[10,], aes(xintercept = datetime), col = "magenta", linetype = 2)+
  scale_color_manual(values = c("skyblue", "orange", "navy", "black", "red", "blue"))+
  theme_classic()+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(override.aes = list(linewidth = 3)))+
  scale_x_continuous(limits = c(lubridate::ymd_hms("2023-06-19 11:00:00"), lubridate::ymd_hms("2023-06-19 18:00:00")))
# this is interesting, and it reveals that the dynamics of who *leaves* the carcass when might be of interest as well as the arrival dynamics.

# Carcass duration --------------------------------------------------------
# Another question I have is, how long would we expect the carcass to last? Can we see the arrival dynamics diminishing over days?
# This plot (which I made before) pretty clearly shows a 3-day period of carcass persistence:
test %>%
  ggplot(aes(x = timestamp, y = dist_m, col = state))+
  geom_line(aes(group = Nili_id))+
  geom_vline(data = hires_carcasses[10,], aes(xintercept = datetime), col = "magenta", linetype = 2)+
  scale_color_manual(values = c("skyblue", "orange", "navy", "black", "red", "blue"))+
  theme_classic()+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(override.aes = list(linewidth = 3)))

# Let's look at a few more random ones
timeplot <- function(idx){
  dat <- carcass_data[[idx]]
  carc <- hires_carcasses[idx,]
  p <- dat %>%
    mutate(dist_m = case_when(dist_m > 10000 ~ NA, .default = dist_m)) %>% # have to do this weird hack instead of filtering to avoid lines getting connected across areas where they shouldn't be connected.
    ggplot(aes(x = timestamp, y = dist_m, col = state))+
    geom_line(aes(group = Nili_id))+
    geom_vline(data = carc, aes(xintercept = datetime), col = "magenta", linetype = 2)+
    scale_color_manual(values = c("skyblue", "orange", "navy", "black", "red", "blue"))+
    theme_classic()+
    theme(legend.position = "bottom")+
    labs(subtitle = carc$lab)+
    guides(color = guide_legend(override.aes = list(linewidth = 3)))
  return(p)
}
timeplots <- map(1:nrow(hires_carcasses), timeplot)
hires_carcasses <- hires_carcasses %>%
  mutate(filename = paste0("fig/carcass_timelines/",
                           date, "_", time, "_", 
                           str_remove_all(RTGDistrict, " "), 
                           ".png"))

# for(i in 1:length(timeplots)){
#   ggsave(timeplots[[i]], filename = here(hires_carcasses$filename[i]), create.dir = T)
# }
# For some reason the plots won't save; I keep getting warnings and the files don't write. So let's just view them here I guess.

walk(timeplots, print)

# Some observations of the carcass timelines: 

# Hmm, I actually want to look at full plots for every single carcass now.
# Observations, looking at the plots: In some cases, no hires-tagged vultures ever approached. In other cases, lots of them came and they visited day after day.

# Let's make this much simpler and look at a plot of the total number of unique vultures in each category in a given hour
time_summaries <- map(carcass_data, ~.x %>%
                        st_drop_geometry() %>%
                        mutate(tenmin = lubridate::floor_date(timestamp, unit = "10 minutes")) %>%
                        group_by(tenmin, state, lab) %>%
                        summarize(n_vultures = length(unique(Nili_id))))

time_summaries[[4]] %>%
  ggplot(aes(x = tenmin, y = n_vultures, fill = state))+
  geom_bar(position = "stack", stat = "identity")+
  scale_fill_manual(values = c("skyblue", "orange", "navy", "black", "red", "blue"))+
  theme_minimal()+
  labs(subtitle = time_summaries[[4]]$lab[1])

walk(time_summaries, ~.x %>%
       ggplot(aes(x = tenmin, y = n_vultures, fill = state))+
       geom_bar(position = "stack", stat = "identity")+
       scale_fill_manual(values = c("skyblue", "orange", "navy", "black", "red", "blue"))+
       theme_minimal())

# Now let's just pay attention to the near categories
for(i in 1:length(time_summaries)){
  lab <- time_summaries[[i]]$lab[1]
  datetime <- str_replace_all(str_replace(hires_carcasses$datetime[i], "\\s", "_"), ":", ".")
  filename <- paste0("fig/carcass_arrivals/", datetime, "_", 
                     str_remove_all(hires_carcasses$RTGDistrict[i], "\\s"),
                     ".png")
  fl <- "fig/carcass_arrivals/test.png"
  ts <- time_summaries[[i]] %>%
    filter(state %in% c("approach_flying",
                        "approach_ground",
                        "near_ground", 
                        "vis_flying")) %>%
    mutate(state_description = case_when(state == "approach_flying" ~ "Approaching\n(fl. <= 500m)",
                                         state == "vis_flying" ~ "In sight\n(fl. 500-1000m)",
                                         state == "approach_ground" ~ "Nearby\n(gr. 50-150m)",
                                         state == "near_ground" ~ "At carcass\n(gr. < 50m)")) %>%
    mutate(state_description = factor(state_description, levels = c("In sight\n(fl. 500-1000m)", "Approaching\n(fl. <= 500m)", "Nearby\n(gr. 50-150m)", "At carcass\n(gr. < 50m)")))
  if(nrow(ts) > 0){
    p <- ts %>%
      ggplot(aes(x = tenmin, y = n_vultures, fill = state_description))+
      geom_bar(position = "stack", stat = "identity")+
      scale_fill_manual(name = "Behavior",
                        values = c("skyblue", "blue", "orange", "red"))+
      theme_minimal()+
      theme(legend.position = "bottom")+
      ylab("Vultures")+
      xlab("Time (10-min bins)")+
      labs(subtitle = lab)
    ggsave(filename = here::here(filename), plot = p, width = 7, height = 6)
  }
}
