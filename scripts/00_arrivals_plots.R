library(here)
library(tidyverse)
library(readxl)
library(sf)
library(mapview)
#devtools::install_github("16EAGLE/moveVis") 
library(moveVis)
library(future)
library(targets)
library(patchwork)
source(here("params.R"))

# allow moveVis to use multiple cores for faster processing
use_multicore(n_cores = ncores, verbose = TRUE)

targets::tar_load(hires_tags)
hires_tags <- hires_tags %>%
  mutate(behavior = case_when(ground_speed > flight_speed ~ "flying",
                              ground_speed <= flight_speed ~ "ground",
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
write_csv(hires_carcasses, file = here("data/hires_carcasses.csv"))

# for each carcass, get the data to go with it. Let's go to the beginning of the day when the carcass was placed, since we know 1) the times reported may not be exact and 2) the vultures sometimes circle when they can see the car coming.
# 
fn <- function(idx){
  carcass <- hires_carcasses[idx,]
  carcass_date <- carcass$date
  carcass_label <- carcass$lab
  dat <-  hires_tags %>%
    filter(dateOnly_il %in%
             seq.Date(from = carcass_date-1, to = carcass_date+10, by = "day")) %>%
    mutate(time_relative = difftime(timestamp_il, carcass$datetime, units = "hours")) %>%
    mutate(lab = carcass_label)
  distances_m <- as.numeric(sf::st_distance(dat, carcass))
  if(length(distances_m) != nrow(dat)){
    stop("length doesn't match")
  }else{
    dat$dist_m <- distances_m
  }
  dat <- dat %>%
    mutate(state = case_when(dist_m > 1000 & behavior == "flying" ~ "far_flying",
                             dist_m <= 1000 & dist_m > 500 & behavior == "flying" ~ "vis_flying",
                             dist_m <= 500 & behavior == "flying" ~ "approach_flying",
                             dist_m > 1000 & behavior == "ground" ~ "far_ground",
                             dist_m <= 1000 & dist_m > 300 & behavior == "ground" ~ "vis_ground",
                             dist_m <= 300 & dist_m > 50 & behavior == "ground" ~ "near_ground",
                             dist_m <= 50 & behavior == "ground" ~ "at_carcass"),
           state_description = case_when(state == "far_flying"~ "Out of sight flying\n(fl. >1000m)",
                                         state == "vis_flying"~ "In sight flying\n(fl. 500-1000m)",
                                         state == "approach_flying" ~ "Approaching\n(fl. <=500m)",
                                         state == "far_ground" ~ "Out of sight ground\n(gr. >1000m)",
                                         state == "vis_ground" ~ "In sight ground\n(gr. 300-1000m)",
                                         state == "near_ground" ~ "Near ground\n(gr. 50-300m)",
                                         state == "at_carcass" ~ "At carcass\n(gr. <=50m)"),
           state = factor(state, levels = c("far_flying", "vis_flying", "approach_flying", "far_ground", "vis_ground", "near_ground", "at_carcass")),
           state_description = factor(state_description, levels = c("Out of sight flying\n(fl. >1000m)", "In sight flying\n(fl. 500-1000m)", "Approaching\n(fl. <=500m)", "Out of sight ground\n(gr. >1000m)", "In sight ground\n(gr. 300-1000m)", "Near ground\n(gr. 50-300m)", "At carcass\n(gr. <=50m)")))
  return(dat)
}

carcass_data <- map(1:nrow(hires_carcasses), fn, .progress = T)

save(carcass_data, file = here("data/carcass_data.Rda"))
load(here("data/carcass_data.Rda"))


# Add behavior states -----------------------------------------------------
test <- carcass_data[[10]]
statecolors <- c("navy", "skyblue", "blue", "black", "darkorange4", "orange", "red")

# What about time series for individuals?
test %>%
  ggplot(aes(x = timestamp, y = dist_m, col = state))+
  geom_line(aes(group = Nili_id))+
  geom_vline(data = hires_carcasses[10,], aes(xintercept = datetime), col = "magenta", linetype = 2)+
  scale_color_manual(values = statecolors)+
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
  scale_color_manual(values = statecolors)+
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
  scale_color_manual(values = statecolors)+
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
  scale_color_manual(values = statecolors)+
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
  scale_color_manual(values = statecolors)+
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
    scale_color_manual(values = statecolors)+
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
  scale_fill_manual(values = statecolors)+
  theme_minimal()+
  labs(subtitle = time_summaries[[4]]$lab[1])

walk(time_summaries, ~.x %>%
       ggplot(aes(x = tenmin, y = n_vultures, fill = state))+
       geom_bar(position = "stack", stat = "identity")+
       scale_fill_manual(values = statecolors)+
       theme_minimal())
save(time_summaries, file = here("data/time_summaries.Rda"))

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
                        "vis_flying"))
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

# Cumulative counts for each carcass over time
test <- carcass_data[[4]]

acc_fun <- function(dat){
  firsts <- dat %>%
    st_drop_geometry() %>%
    filter(time_relative > 0) %>%
    select(Nili_id, state, state_description, time_relative) %>%
    arrange(time_relative) %>%
    group_by(Nili_id, state) %>%
    slice(1)
  
  firsts_summ <- firsts %>%
    group_by(state) %>%
    arrange(time_relative, .by_group = T) %>%
    mutate(rn = 1:n())
  return(firsts_summ)
}

summs <- imap(carcass_data, ~acc_fun(.x) %>% mutate(carcass = .y)) %>% purrr::list_rbind()

# restrict to 5 days, and calculate proportions
# 5 days = 5*24 = 120 hours
summs <- summs %>%
  filter(time_relative <= 120) %>%
  group_by(carcass, state_description) %>%
  mutate(prop = rn/max(rn))

abs <- summs %>%
  filter(!(state %in% c("far_flying", "far_ground"))) %>%
  ggplot(aes(x = time_relative, y = rn, group = carcass, 
             col = state_description))+
  facet_wrap(~state_description)+
  geom_line(alpha = 0.3)+
  theme_classic()+
  scale_color_manual(name = "", 
                     values = statecolors[c(2, 3, 5, 6, 7)])+
  ggtitle("Absolute numbers of vultures")+
  ylab("Number of vultures")+
  xlab("Time since carcass deposition")+
  guides(color = guide_legend(position = "inside",
                              override.aes = list(alpha = 1, linewidth = 1.5)))+
  theme(legend.position.inside = c(0.85, 0.26))

rel <- summs %>%
  filter(!(state %in% c("far_flying", "far_ground"))) %>%
  ggplot(aes(x = time_relative, y = prop, group = carcass, 
             col = state_description))+
  facet_wrap(~state_description)+
  geom_line(alpha = 0.3)+
  theme_classic()+
  scale_color_manual(name = "", 
                     values = statecolors[c(2, 3, 5, 6, 7)])+
  ggtitle("Relative numbers of vultures")+
  ylab("Proportion of total")+
  xlab("Time since carcass deposition")+
  guides(color = guide_legend(position = "inside", 
                              override.aes = list(alpha = 1, linewidth = 1.5)))+
  theme(legend.position.inside = c(0.85, 0.26))

abs_rel <- (abs+theme(legend.position = "none")) + rel + plot_layout(ncol = 2)
ggsave(abs_rel, filename = here("fig/accumulation_curves_abs_rel.png"), width = 12, height = 7)

# Examining one specific carcass ------------------------------------------
hires_carcasses
target_carcass <- hires_carcasses[4,] # placed at 15:05, so afternoon.
target_carcass_data <- carcass_data[[4]]
glimpse(target_carcass_data)

# Need to figure out where everyone roosted on the night of 6/12-6/13, 6/13-6/14, and 6/14-6/15.
roosts <- target_carcass_data %>%
  filter(dateOnly_il %in% c("2023-06-12", "2023-06-13", "2023-06-14", "2023-06-15"),
         lubridate::hour(timestamp_il) %in% c(21:23, 0:7)) %>%
  select(Nili_id, dateOnly_il, timestamp_il, location_lat, location_long) %>%
  mutate(roost_date = case_when(lubridate::hour(timestamp_il) %in% 0:7 ~ dateOnly_il-1,
                                .default = dateOnly_il)) %>%
  filter(roost_date %in% c("2023-06-12", "2023-06-13", "2023-06-14", "2023-06-15")) %>%
  group_by(Nili_id, roost_date) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_centroid() %>%
  ungroup() %>%
  group_by(roost_date) %>%
  group_split() %>%
  map(., ~.x %>% arrange(Nili_id))

vultures <- map(roosts, ~.x$Nili_id)
dists <- roosts %>%
  map(., ~as.numeric(st_distance(.x))) %>%
  map(., ~{
    close <- which(.x <= 500)
    far <- which(.x > 500)
    test[close] <- "close"
    test[far] <- "far"
    return(test)
  }) %>%
  map(., ~matrix(.x, nrow = sqrt(length(.x)), ncol = sqrt(length(.x)))) %>%
  map2(., vultures, ~{setNames(as.data.frame(.x), .y) %>%
      mutate(vulture = .y)}) %>%
  map(., ~{
    out <- pivot_longer(.x, cols = -"vulture", names_to = "vulture2", values_to = "value")
    out$value <- unlist(out$value)
    return(out)})
length(dists)
names(dists) <- c("2023-06-12", "2023-06-13", "2023-06-14", "2023-06-15")

# Nobody went to this carcass after it was placed on the first afternoon (6/13), so let's consider 6/14 to be the first day.
states <- target_carcass_data %>%
  group_by(dateOnly_il) %>%
  select(Nili_id, dateOnly_il, dist_m, state, timestamp_il) %>%
  filter(timestamp_il > lubridate::ymd_hms("2023-06-13 15:00:00")) %>%
  arrange(timestamp_il) %>%
  group_by(dateOnly_il, Nili_id, state) %>%
  slice(1)

allvultures <- sort(unique(target_carcass_data$Nili_id))

firstday_informed <- states %>%
  filter(dateOnly_il == "2023-06-14" & state %in% c("approach_flying", "near_ground", "at_carcass", "vis_flying", "vis_ground")) %>%
  pull(Nili_id) %>%
  unique() %>%
  sort()

firstday_arrived <- states %>%
  filter(dateOnly_il == "2023-06-14" & state %in% c("at_carcass")) %>%
  pull(Nili_id) %>%
  unique() %>%
  sort()

secondday_arrived <- states %>%
  filter(dateOnly_il == "2023-06-15" & state %in% c("at_carcass")) %>%
  pull(Nili_id) %>%
  unique() %>%
  sort()
secondday_arrived_informed <- secondday_arrived[secondday_arrived %in% firstday_informed]
secondday_arrived_uninformed <- secondday_arrived[!(secondday_arrived %in% firstday_informed)]

secondday_informed <-  states %>%
  filter(dateOnly_il %in% c("2023-06-14", "2023-06-15") & state %in% c("approach_flying", "near_ground", "at_carcass", "vis_flying", "vis_ground")) %>%
  pull(Nili_id) %>%
  unique() %>%
  sort()

thirdday_arrived <- states %>%
  filter(dateOnly_il == "2023-06-16" & state %in% c("at_carcass")) %>%
  pull(Nili_id) %>%
  unique() %>%
  sort() # nobody arrived on the third day, so no need to parse it out by informed vs. not.

secondday_prev_roosts <- dists[["2023-06-14"]] %>%
  filter(value == "close",
         vulture != vulture2)
roostbuddies <- map(secondday_arrived_uninformed, ~{
  dat <- secondday_prev_roosts %>%
    filter(vulture == .x | vulture2 == .x)
  all <- unique(c(dat$vulture, dat$vulture2))
  buddies <- all[all != .x]
  return(buddies)
})
prop_buddies_informed <- map_dbl(roostbuddies, ~{
  sum(.x %in% firstday_informed)/length(.x)
}) # proportion of roost buddies that were informed

# Ok now can we do the same thing for the vultures that did not go to the carcass on either the first or second day? Did *anyone* not go to the carcass on the first or second day?
allvultures <- unique(carcass_data[[4]]$Nili_id)
allinformed <- sort(unique(c(firstday_informed, secondday_informed)))
length(allvultures)
length(allinformed) # okay so not all of them are informed
uninformed <- allvultures[!(allvultures %in% allinformed)]

uninformed_roostbuddies <- map(uninformed, ~{
  dat <- secondday_prev_roosts %>%
    filter(vulture == .x | vulture2 == .x)
  all <- unique(c(dat$vulture, dat$vulture2))
  buddies <- all[all != .x]
  return(buddies)
})
uninformed_prop_buddies_informed <- map_dbl(uninformed_roostbuddies, ~{
  sum(.x %in% firstday_informed)/length(.x)
}) # hmm so just on first glance there does not appear to be a difference in the proportion of informed roost buddies between individuals that did not go to the carcass vs. individuals that arrived for the first time on the second day. Obviously tiny sample size, just one carcass, need to look more carefully.


