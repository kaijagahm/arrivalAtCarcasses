# Defining predictability of feeding stations
# See "Predictability of food resources affects carcass finding" in Obsidian
# How do we define predictability?
# From Riotte-Lambert & Matthiopoulous 2020 TREE: "the value of an environmental variable (e.g., the abundance of a resource) is increasingly predictable at a given spatiotemporal scale if it is characterised by lower variability or higher correlation with itself or another environmental variable, measured at the given spatiotemporal scale."
library(tidyverse)
library(targets)
library(here)
library(sf)
library(mapview)

# Load in the carcass data
tar_load(carcasses_audited) # will need to go back to the original data to see the names of the management regions--apparently they are all supposed to be provisioned approximately equally, but aren't, according to Reznikov et al.
tar_load(bbox_south_big)
carcasses_south <- st_crop(carcasses_audited, bbox_south_big)

# Simplify--keeping only dates, not datetimes, because sometimes there are multiple carcasses placed very close to each other in time
carcs <- carcasses_south %>%
  select(carcID, date, time, datetime, datetime_il, long, lat, stationName, carcassWeight, geometry, X, Y)

carcs_simple <- carcasses_south %>%
  select(date, stationName) %>%
  st_drop_geometry() %>%
  distinct()
dim(carcs)
dim(carcs_simple)
nrow(carcs)-nrow(carcs_simple) # 95 instances of two carcasses being placed at the same station on the same day

stn <- carcs_simple %>%
  filter(!is.na(stationName))
dim(stn)

# Look at a plot of the carcass frequencies over time
stn %>%
  ggplot(aes(x = date, y = stationName))+
  geom_point(alpha = 0.5)+
  theme_minimal()+
  labs(title = "SFS provisioned carcasses, 2018-2024",
       y = "Supplementary Feeding Station",
       x = "Date")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

tar_load(minmax_dates)
stn %>%
  filter((date >= minmax_dates[[1]] & date <= minmax_dates[[2]]) | (date >= minmax_dates[[3]] & date <= minmax_dates[[4]]) | (date >= minmax_dates[[5]] & date <= minmax_dates[[6]])) %>%
  mutate(year = lubridate::year(date)) %>%
  ggplot(aes(x = date, y = stationName, color = stationName))+
  geom_point(size = 3, alpha = 0.75)+
  theme_bw()+
  facet_wrap(~year, scales = "free_x")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")+
  labs(y = "Feeding station", x = "Date")

# Calculations for the whole period
# Intervals
stn <- stn %>%
  arrange(stationName, date) %>%
  group_by(stationName) %>%
  mutate(interval = as.numeric(difftime(date, lag(date), units = "days")))

stn <- stn %>%
  filter(!is.na(interval))

stn %>%
  group_by(stationName) %>%
  filter(n() > 4) %>%
  ggplot(aes(x = stationName, y = interval))+
  geom_boxplot(outlier.size = 0.2)+
  coord_flip()+
  labs(x = "SFS",
       y = "Interval between carcasses (days)",
       caption = "For stations with at least 4 carcasses")+
  theme_minimal()

# A simpler measure--how likely is there to be food there? Considering 6-month period before each carcass, how many of the dates in that period were active at that station?
tar_load(carcasses_audited)
tar_load(stats)
focal_carcs <- carcasses_audited %>%
  filter(carcID %in% stats$carcID)

stn_days_last6mos <- rep(NA, nrow(focal_carcs))
for(i in 1:nrow(focal_carcs)){
  current_date <- focal_carcs$date[i]
  current_stn <- focal_carcs$stationName[i]
  prev_6mos <- carcasses_audited %>%
    filter(date >= (current_date-months(6)) & date <= current_date,
           stationName == current_stn)
  all_dates <- seq.Date(from = current_date-months(6), to = current_date)
  active_dates <- sort(unique(c(prev_6mos$date, prev_6mos$date + days(1), prev_6mos$date + days(2))))
  stn_days_last6mos[i] <- length(active_dates)/length(all_dates)
}

focal_carcs$stn_days_last6mos <- stn_days_last6mos

i <- 3
current_date <- focal_carcs$date[i]
current_stn <- focal_carcs$stationName[i]
prev_6mos <- carcasses_audited %>%
  filter(date >= (current_date-months(6)) & date <= current_date,
         stationName == current_stn)
all_dates <- seq.Date(from = current_date-months(6), to = current_date)
active_dates <- sort(unique(c(prev_6mos$date, prev_6mos$date + days(1), prev_6mos$date + days(2))))

testdf <- data.frame(date = all_dates) %>%
  mutate(active = ifelse(lubridate::date(date) %in% lubridate::date(active_dates), T, F))
testdf %>%
  ggplot(aes(x = date, y = active, col = active))+
  geom_point(size = 2, alpha = 0.75)+
  theme_minimal()+
  labs(y = "Active carcass?", x = "Date")+
  theme(legend.position = "none")

# What about carcasses in the area? Not just at the same station but within let's say 4km?
focal_carcs_buffered <- st_buffer(focal_carcs, 4000)
#mapview(focal_carcs_buffered)

area_days_last6mos <- rep(NA, nrow(focal_carcs))
for(i in 1:nrow(focal_carcs)){
  current_date <- focal_carcs$date[i]

  prev_6mos <- carcasses_audited %>%
    filter(date >= (current_date-months(6)) & date <= current_date)
  keep <- prev_6mos[st_intersects(prev_6mos, focal_carcs_buffered[i,], sparse = F)[,1],]
  
  
  all_dates <- seq.Date(from = current_date-months(6), to = current_date)
  active_dates <- sort(unique(c(keep$date, keep$date + days(1), keep$date + days(2))))
  area_days_last6mos[i] <- length(active_dates)/length(all_dates)
}
i <- 3
current_date <- focal_carcs$date[i]

prev_6mos <- carcasses_audited %>%
  filter(date >= (current_date-months(6)) & date <= current_date)
keep <- prev_6mos[st_intersects(prev_6mos, focal_carcs_buffered[i,], sparse = F)[,1],]


all_dates <- seq.Date(from = current_date-months(6), to = current_date)
active_dates <- sort(unique(c(keep$date, keep$date + days(1), keep$date + days(2))))

focal_carcs$area_days_last6mos <- area_days_last6mos

testdf <- data.frame(date = all_dates) %>%
  mutate(active = ifelse(lubridate::date(date) %in% lubridate::date(active_dates), T, F))
testdf %>%
  ggplot(aes(x = date, y = active, col = active))+
  geom_point(size = 2, alpha = 0.75)+
  theme_minimal()+
  labs(y = "Active carcass?", x = "Date")+
  theme(legend.position = "none")

focal_carcs %>%
  ggplot(aes(x = stn_days_last6mos, y = area_days_last6mos, color = stationName))+
  geom_point(size = 2, alpha = 0.75)+
  theme_minimal()+
  labs(y = "Prop. days with nearby active carcass, last 6 mos",
       x = "Prop. days with active carcass at station, last 6 mos",
       color = "Station")+
  coord_equal()

focal_carcs %>%
  ggplot(aes(x = stn_days_last6mos, fill = stationName))+
  geom_histogram()+
  theme_minimal()+
  labs(y = "Count", x = "Predictability (same station, last 6 months)", fill = "Station")

focal_carcs %>%
  ggplot(aes(x = area_days_last6mos, fill = stationName))+
  geom_histogram()+
  theme_minimal()+
  labs(y = "Count", x = "Predictability (4km radius, last 6 months)", fill = "Station")

# Grab 6 month chunks of carcass dates for each station, starting every 1 month after the beginning of the carcass data.
dates_beginning <- seq.Date(from = min(stn$date), to = max(stn$date)+days(90), by = "1 month")
dates_end <- dates_beginning + days(180) # 6 month windows, starting each month

dates <- data.frame(beg = dates_beginning, end = dates_end)
stns <- sort(unique(stn$stationName))
dates_exp <- expand_grid(stns, dates)

carcasses_moving_window <- vector(mode = "list", length = nrow(dates_exp))
for(i in 1:nrow(dates_exp)){
  carcasses_moving_window[[i]] <- stn %>%
    filter(stationName == dates_exp$stns[i], date >= dates_exp$beg[i], date < dates_exp$end[i]) %>% bind_cols(dates_exp[i,])
}

cmw <- purrr::list_rbind(carcasses_moving_window)
dim(cmw)
dim(stn)

set.seed(5)
random_stations <- sample(unique(cmw$stationName), 4)
cmw %>%
  filter(stationName %in% random_stations) %>%
  ggplot(aes(x = end, y = interval))+
  geom_point(alpha = 0.1)+
  geom_smooth()+
  facet_wrap(~stationName)+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank())+
  labs(y = "Time between carcasses (days)",
       x = "End of 6-month period")

cmw %>%
  filter(stationName %in% random_stations) %>%
  ggplot(aes(x = end, y = 1/interval))+
  geom_point(alpha = 0.1)+
  geom_smooth()+
  facet_wrap(~stationName)+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank())+
  labs(y = "Carcass placement rate (1/days)",
       x = "End of 6-month period")

predi <- cmw %>%
  group_by(stationName, end) %>%
  summarize(mn_int = mean(interval, na.rm = T),
            var_int = var(interval, na.rm = T),
            sd_int = sd(interval, na.rm = T)) %>%
  ungroup()

predi %>%
  filter(end < lubridate::date("2024-01-01")) %>%
  ggplot(aes(x = end, y = sd_int))+
  geom_point()+
  labs(y = "SD days between carcasses", x = "End of 6 month period")+
  theme_minimal()

predi %>%
  filter(end < lubridate::date("2024-01-01")) %>%
  ggplot(aes(x = end, y = var_int))+
  geom_point()+
  labs(y = "Variance in days between carcasses", x = "End of 6 month period")+
  theme_minimal()

predi %>%
  filter(end < lubridate::date("2024-01-01")) %>%
  ggplot(aes(x = end, y = var_int))+
  geom_point()+
  labs(y = "Variance in days between carcasses", x = "End of 6 month period")+
  theme_minimal()

# Okay, but ideally we would want to look at the distribution of station variabilities per 6-month period.
predi %>%
  filter(end < lubridate::date("2024-01-01")) %>% # because things seem to get weird over here--probably need to exclude the north for starters
  ggplot(aes(x = var_int))+
  geom_density(aes(group = factor(end), color = end), linewidth = 0.1, alpha = 0.2)+
  theme_minimal()+ # okay, this is super duper right-skewed
  labs(x = "Variance in carcass interval",
       color = "6-month window")+
  scale_color_viridis_c()

# What happens if we log-transform the variances?
predi %>%
  filter(end < lubridate::date("2024-01-01")) %>% 
  ggplot(aes(x = log(var_int), color = end))+
  geom_density(aes(group = factor(end)), linewidth = 0.1)+
  theme_minimal()+ # this is so beautiful that it basically has me convinced that this is how we should divide the stations up.
  labs(x = "Variance in carcass interval (log-transformed)")+
  scale_color_viridis_c()

# But now of course we're going to have a question about the relationship between number of carcasses and variance, and I expect them to be highly correlated:
predi %>%
  ggplot(aes(x = log(var_int), y = log(mn_int)))+
  geom_point(alpha = 0.2)+
  theme_minimal()+
  labs(y = "Mean carcass interval, days (log-transformed)",
       x = "Carcass interval variance, days (log-transformed)")

predi %>%
  ggplot(aes(x = log(var_int), y = log(mn_int)))+
  geom_point(alpha = 0.2)+
  theme_minimal()+
  labs(y = "Mean carcass interval, days (log-transformed)",
       x = "Carcass interval variance, days (log-transformed)")+
  geom_vline(aes(xintercept = log(median(var_int, na.rm = T))), color = "red")+
  geom_hline(aes(yintercept = log(median(mn_int, na.rm = T))), color = "red")+
  annotate(geom = "text", x = 2, y = 0.5, label = "Regular and frequent", size = 2.5)+
  annotate(geom = "text", x = 10, y = 0.5, label = "Irregular and frequent", size = 2.5)+
  annotate(geom = "text", x = 2, y = 6, label = "Regular and infrequent", size = 2.5)+
  annotate(geom = "text", x = 10, y = 6, label = "Irregular and infrequent", size = 2.5)+
  annotate(geom = "text", x = 13.3, y = 3.6, label = "Irregular", size = 3, color = "red")+
  annotate(geom = "text", x = -0.5, y = 3.6, label = "Regular", size = 3, color = "red")+
  annotate(geom = "text", x = 6.5, y = 7.5, label = "Infrequent", size = 3, color = "red")+
  annotate(geom = "text", x = 6.5, y = 0.5, label = "Frequent", size = 3, color = "red")

# In which cases was the mean interval less than 10 days (relevant hunger period for vultures)?
predi %>%
  mutate(col = ifelse(mn_int < 10, T, F)) %>%
  ggplot(aes(x = log(var_int), y = log(mn_int), col = col))+
  geom_point(alpha = 0.2)+
  theme_minimal()+
  labs(y = "Mean carcass interval, days (log-transformed)",
       x = "Carcass interval variance, days (log-transformed)",
       color = "Mean interval\n< 10 days")

# Flip the axes and look at residuals of variance relative to the mean-variance line
predi %>%
  ggplot(aes(x = log(mn_int), y = log(var_int)))+
  geom_point(alpha = 0.2)+
  theme_minimal()+
  labs(x = "Mean carcass interval, days (log-transformed)",
       y = "Carcass interval variance, days (log-transformed)")+
  geom_smooth(method = "lm", alpha = 0.2)

# Longer times come with more variance
formod <- predi %>%
  filter(!is.na(mn_int), !is.na(var_int))
mod <- lm(log(var_int) ~ log(mn_int), data = formod)

formod$resid <- mod$residuals

formod %>%
  ggplot(aes(x = log(mn_int), y = log(var_int), col = resid))+
  geom_point()+
  scale_color_gradient2(
    low = 'red', mid = 'white', high = 'blue',
    midpoint = 0, guide = 'colourbar', aesthetics = 'color'
  )+
  theme_minimal()+
  labs(y = "Carcass interval variance, days (log-transformed)", x = "Mean carcass interval, days (log-transformed)",
       color = "Residual")+
  annotate(x = 4.5, y = -1, label = "Unusually regular", color= "red", geom = "text")+
  annotate(x = 2.5, y = 10, label = "Unusually irregular", color= "blue", geom = "text")

# Compare against Reznikov ------------------------------------------------
# "Data include the years 2019–2021, from April until mid-September: 2019"
rezn <- carcasses_south %>%
  filter((date >= lubridate::date("2019-04-01") & date <= lubridate::date("2019-09-15")) |
         (date >= lubridate::date("2020-04-01") & date <= lubridate::date("2020-09-15")) |
         (date >= lubridate::date("2021-04-01") & date <= lubridate::date("2021-09-15")))
dim(rezn)
mapview(rezn)
# It's not clear how they classified carcasses to the different groups (they probably used the INPA management regions, which I didn't preserve; would have to go back to the data for that.)
# But looking at their map figure, it seems they did not include anything north of En Gedi or south of... I don't know the name, but anyway the one way down at the bottom shouldn't be included.
# It looks like we should be able to make subdivisions by latitude.
rezn %>% ggplot(aes(x = lat_orig)) + geom_histogram()
# okay so blue is gonna be 30.25 through 30.75, orange will be 30.75 through 31.1, and pink will be 31.1 through 31.5. There's one in the middle, at 31.12, and I determined that it belongs with the pink ones based on their map.

rezn <- rezn %>%
  mutate(rezn_group = case_when(lat_orig > 30.25 & lat_orig <= 30.75 ~ "skyblue",
                                lat_orig > 30.75 & lat_orig <= 31.1 ~ "orange1",
                                lat_orig > 31.1 & lat_orig < 31.6 ~ "pink"))
table(rezn$rezn_group, exclude = NULL)

mapview(rezn, zcol = "rezn_group") # the colors won't be true, but at least it will be plotted. Nice, so we've recreated the groups as best as possible.

rezn <- rezn %>%
  select(-starts_with("n_")) %>%
  select(-c("flagGideon", "flag", "interpretation", "todo", "reassign_to", "questionForGideon", "investigateKaija", "commentsKaija", "color", "edited_coords", "explanation"))

# Now, how many feeding stations do we have?
table(rezn$stationName) # 24, which is more than the 19 they mention. Can any of them be grouped?
rezn %>%
  filter(rezn_group == "skyblue") %>%
  ggplot(aes(x = long, y = lat))+
  geom_point(alpha = 0.75, pch = 1, size = 5, aes(col = stationName))+
  theme_minimal() # Hava cliff and Kaolin should maybe be grouped
rezn %>% filter(stationName %in% c("Hava_cliff", "Kaolin")) %>% pull(stationName) %>% table()
# hmm, but there are a decent number at each one. Maybe these are really separate.
# all the others in this region look fine.

rezn %>%
  filter(rezn_group == "pink") %>%
  ggplot(aes(x = long, y = lat))+
  geom_point(alpha = 0.75, size = 5, aes(col = stationName))+
  theme_minimal() # wait, is there a gorni_hill carcass incorrectly placed at Ben_Yair_view?

rezn %>%
  filter(stationName %in% c("Ben_Yair_view", "Gorni_hill")) %>%
  ggplot(aes(x = long, y = lat))+
  geom_point(alpha = 0.75, size = 5, aes(col = stationName))+
  theme_minimal() # oh no! okay, we need to reclassify those carcasses as belonging to Ben_Yair_view I think.

rezn <- rezn %>%
  mutate(stationName = case_when(stationName == "Gorni_hill" & long > 35.31 ~ "Ben_Yair_view", .default = stationName))
rezn %>%
  filter(stationName %in% c("Ben_Yair_view", "Gorni_hill")) %>%
  ggplot(aes(x = long, y = lat))+
  geom_point(alpha = 0.75, size = 5, aes(col = stationName))+
  theme_minimal() # that looks better. Need to also make that change in the feeding station classification script.
table(rezn$stationName)

rezn %>%
  filter(rezn_group == "orange1") %>%
  ggplot(aes(x = long, y = lat))+
  geom_point(alpha = 0.75, size = 5, aes(col = stationName))+
  theme_minimal() # this is hard to look at--let's reduce the number of colors by removing the ones that are definitely different.

rezn %>%
  filter(!(stationName %in% c("Antenas", "Golhan", "Hahalak_mount", "Small_crater_view", "Camus_south", "Hatzera_drill", "Other", "Lashabia_450", "Rosh Maale Hazra (inactive)", "Halukim_ridge"))) %>%
  filter(rezn_group == "orange1") %>%
  ggplot(aes(x = long, y = lat))+
  geom_point(alpha = 0.75, size = 5, aes(col = stationName))+
  theme_minimal() # hmm, once we've eliminated these, we end up with three stations left, and they are in fact very close to each other but they are still distinctly separate and group-able.

# I'm actually more concerned about the "other" ones:
other <- rezn %>%
  filter(stationName == "Other")
mapview(other) # okay, in order to look at the predictabilities properly, these need to be classified separately. And we need to incorporate that into the carcass classifications earlier in the process.
other %>%
  ggplot(aes(x = itmLong_orig, y = itmLat_orig))+
  geom_point()

rezn <- rezn %>%
  mutate(stationName = case_when(stationName == "Other" & itmLong_orig < 200000 & itmLat_orig > 535000 ~ "Other1",
                                 stationName == "Other" & itmLong_orig < 200000 & itmLat_orig < 535000 ~ "Other4",
                                 stationName == "Other" & itmLong_orig > 200000 & itmLat_orig > 535000 ~ "Other2",
                                 stationName == "Other" & itmLong_orig > 200000 & itmLat_orig < 535000 ~ "Other3", .default = stationName))
table(rezn$stationName)

# Okay, now let's get rid of any stations with only 1 carcass.
rezn_ltd <- rezn %>%
  group_by(stationName) %>%
  filter(n() > 1)
table(rezn_ltd$stationName)

rezn_ltd$year <- lubridate::year(rezn_ltd$date)
table(rezn_ltd$year) # okay, different numbers of carcasses but not too wildly different.

# How close are our station numbers to theirs?
rezn_ltd %>%
  st_drop_geometry() %>%
  select(stationName, rezn_group) %>%
  distinct() %>%
  group_by(rezn_group) %>%
  summarize(n = length(unique(stationName)))

# Okay so they had 4 (pink), 11 (orange), 5 (blue).
# We have 4 (pink), 13 (orange), 5 (blue). 
# Very close!! To the point where I'm not worried. The translation from Hebrew is going to make this hard no matter what. I'm happy enough with that to move forward and see if we replicate their findings.

# Okay, now we can finally do the calculation that they said to do.
# "First, we computed the mean food supply frequency in each feeding station."
# I don't understand how they handled the calculation for the different years, but I guess I'm just going to divide it by year for the actual "days since" calculation (since we're skipping a major portion of the year) but then include all of the valid days_since numbers when I actually calculate the mean?
rezn_ltd <- rezn_ltd %>%
  arrange(datetime) %>%
  group_by(stationName, year) %>%
  mutate(days_since_last = as.numeric(difftime(date, lag(date), units = "days"))) %>%
  ungroup()
  
rezn_stats_stn <- rezn_ltd %>%
  st_drop_geometry() %>%
  group_by(stationName, rezn_group, year) %>%
  summarize(mn_days_since_stn = mean(days_since_last, na.rm = T))

rezn_stats_area <- rezn_stats_stn %>%
  group_by(rezn_group, year) %>%
  summarize(mn_freq_area = mean(mn_days_since_stn, na.rm = T),
            sd_freq_area = sd(mn_days_since_stn, na.rm = T))
#"Second, we calculated the mean and standard deviation (SD) of the food supply frequency in each area (FC area = 4 feeding stations, FD area = 11 feeding stations, NFD area = 5 feeding stations; Figure 1) using the mean food supply frequency of all feeding stations in that area. Therefore, high SD reflects low homogeneity and low SD reflects high homogeneity (Figure 2)." 

rezn_stats <- left_join(rezn_stats_stn, rezn_stats_area)

rezn_stats %>%
  mutate(rezn_group = factor(rezn_group, levels = c("pink", "orange1", "skyblue"))) %>%
  mutate(rezn_group_name = fct_recode(rezn_group, "FC" = "pink", "FD" = "orange1", "NFD" = "skyblue")) %>%
  ggplot(aes(x = rezn_group_name, fill = factor(year), y = mn_days_since_stn))+
  geom_boxplot(position = position_dodge(width = 0.85))+
  labs(y ="Carcass interval (days)--station-year means",
       x = "Reznikov group (recreated)")+
  theme_bw()+
  scale_fill_manual(name = "Year", values = c("gray90", "gray60", "gray30"))+
  geom_point(aes(y = sd_freq_area, x = rezn_group_name, color = rezn_group_name),
             position = position_dodge(width = 0.85), size = 3)+
  scale_color_manual(values = c("pink", "orange", "skyblue"), guide = "none")

# Okay, so this is my attempt to recreate the plot, but I wasn't able to.
# Possible sources of difference:
# - Maybe they were excluding some carcasses that I included, or vice versa? I don't remember removing a bunch but maybe I did and am forgetting.
# - Maybe the fixing/reclassifications that I did with Gideon drastically changed the results.
# - Maybe I've misunderstood what they are showing on their plot, or the order in which they did their calculations? The methods were not very clear and the supplementary material did not clarify sufficiently.

# Based on the plot that I'm able to generate, I am  not convinced that the areas sort so neatly into feeding schemes. But I do like the scheme/categorical idea, and I like the way that they combined space and frequencies. Maybe I could take their idea and
# - plot the values continuously and then cluster, instead of using predetermined clusters
# - include year - round data
# - verify the station classifications
# - look at spatial heterogeneity in a continuous way, e.g. using all stations within 100km or something? I don't know how to do this.

# Content moved over from old script exploratory_viz.R  -------------------------------
# Script for creating a predictability raster
## How often are carcasses present in various grid squares?

# Load packages
library(sf)
library(terra)
library(dplyr)
library(lubridate)
library(purrr)
library(magick)
library(patchwork)
library(tidyverse)
library(targets)
library(here)
source(here("R/functions.R"))

# Load carcass data
tar_load(all_carcasses)
tar_load(carcasses_audited)
# get all stn carcasses
carcasses_audited <- carcasses_audited %>%
  select(carcID, date, long, lat, geometry, X, Y, carcassWeight) %>%
  mutate(carcType = "stn")
# get all carcasses for the three hf windows, including both wild and stn
all_carcasses <- all_carcasses %>%
  select(carcID, date, long, lat, geometry, X, Y, carcType, nBouts, nIndivs, carcassWeight)
# add on all the carcasses from the other times besides the hf windows
all_carcasses <- bind_rows(all_carcasses %>% mutate(source = "ac"), carcasses_audited %>% mutate(source = "ca")) 
# deduplicate, defaulting to all_carcasses
all_carcasses <- all_carcasses %>%
  arrange(carcID, source) %>%
  group_by(carcID) %>%
  slice(1)
tar_load(bbox_south_big)
all_carcasses <- st_crop(all_carcasses, bbox_south_big)

all_carcasses %>% 
  mutate(year = lubridate::year(date)) %>%
  group_by(year, date, carcType) %>%
  filter(year >= 2022) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = date, y = n, fill = carcType))+
  geom_col()+
  facet_wrap(~year, scales = "free_x", nrow = 1)+
  theme_classic()

rast_all_5km <- points_to_raster(carcasses_sf = all_carcasses, bbox = bbox_south_big, resolution = 5000)
# rast_all_1km <- points_to_raster(carcasses_sf = all_carcasses, bbox = bbox_south_big, resolution = 1000)
# plot(rast_all_1km)

# Now divide the data into years and run this on all the years
years_list <- all_carcasses %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  group_split()

# rasts_1km <- map(years_list, ~points_to_raster(.x, bbox_south_big, 1000))
rasts_5km <- map(years_list, ~points_to_raster(.x, bbox_south_big, 5000))
# Convert each raster to a data frame and tag with year
rasts_df_5km <- map2_dfr(
  rasts_5km,
  map_dbl(years_list, ~.x$year[1]),
  ~as.data.frame(.x, xy = TRUE) %>% 
    rename(value = 3) %>%
    mutate(year = .y)
)

# rasts_df_1km <- map2_dfr(
#   rasts_1km,
#   map_dbl(years_list, ~.x$year[1]),
#   ~as.data.frame(.x, xy = TRUE) %>% 
#     rename(value = 3) %>%
#     mutate(year = .y)
# )

# Inspect global value range (for setting color scale)
range_vals_5km <- range(rasts_df_5km[rasts_df_5km$year <= 2020,]$value, na.rm = TRUE)
# range_vals_1km <- range(rasts_df_1km[rasts_df_1km$year <= 2020,]$value, na.rm = TRUE)

# Plot using ggplot2
rasts_df_5km %>%
  filter(year >= 2020) %>%
  ggplot(aes(x = x, y = y, fill = value)) +
  geom_raster() +
  facet_wrap(~year, nrow = 1) +
  scale_fill_viridis_c(limits = range_vals_5km, na.value = "transparent") +
  coord_fixed() +
  theme_minimal() +
  labs(title = "Number of carcasses per year",
       fill = "Carcasses", y = "", x = "")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom")

# rasts_df_1km %>%
#   filter(year >= 2020) %>%
#   ggplot(aes(x = x, y = y, fill = value)) +
#   geom_raster() +
#   facet_wrap(~year, nrow = 1) +
#   scale_fill_viridis_c(limits = range_vals_1km, na.value = "transparent") +
#   coord_fixed() +
#   theme_minimal() +
#   labs(title = "Number of carcasses per year",
#        fill = "Carcasses", y = "", x = "")+
#   theme(axis.text.y = element_blank(),
#         axis.text.x = element_blank(),
#         legend.position = "bottom")

startDate <- "2023-03-15"
endDate <- "2023-04-15"
test <- dist_to_carcasses(all_carcasses, bbox_south_big, resolution = 1000, start_date = startDate,
                          end_date = endDate, active_days = 3, visibility_radius = 10000)

png_files <- get_pngs(test)
imgs <- image_read(png_files)
#animation <- image_animate(imgs, fps = 2) 
#animation
#image_write(animation, path = "fig/month_1000_act3_vis10km_decay0.gif") # XXX something's wrong with the coloring here, but we can fix that later.

# Visualizing different decay rates over time
initial_weight <- 500
days <- 1:10
decay_rates <- seq(0.1, 2, by = 0.05)

# Create data frame
decay_df <- expand.grid(day = days, decay_rate = decay_rates)
decay_df$weight <- initial_weight * exp(-decay_df$decay_rate * (decay_df$day - 1))

decay_df %>%
  ggplot(aes(x = day, y = weight, col = decay_rate, group = factor(decay_rate)))+
  geom_line()+
  theme_classic()+
  scale_color_viridis_c()+
  geom_hline(aes(yintercept = 5), col = "red")

# Now with weighted distances to carcasses, assuming decay rate of -2
test_wt <- dist_to_carcasses(all_carcasses, bbox_south_big, resolution = 1000,
                             start_date = startDate, end_date = endDate, 
                             weight_col = "carcassWeight",
                             visibility_radius = 10000, decay_rate = 2, 
                             distance_power = 2, min_weight = 10)

png_files <- get_pngs(test_wt)
imgs <- image_read(png_files)
#animation <- image_animate(imgs, fps = 2) 
#animation
#image_write(animation, path = "fig/month_1000_dist2_vis10km_decay2.gif") # XXX something's wrong with the coloring here, but we can fix that later.

cell_values_long <- as.data.frame(test_wt, cells = T, wide = F)

# cell_values_long %>%
#   ggplot(aes(x = layer, y = values/1000, group = cell))+
#   geom_line(alpha = 0.05)+
#   theme_classic()+
#   labs(y = "Weighted distance (km)",
#        x = "Days",
#        title = "Distance to active carcasses",
#        caption = "Weighted distance takes into account the distance to all carcasses\nwith remaining weight <= 5kg. Carcass weight declines after placement.")

mean_raster <- terra::mean(test_wt, na.rm = TRUE)
var_raster <- terra::app(test_wt, fun = function(x) var(x, na.rm = TRUE))

# Compute min, max, and normalize as before
mean_min <- global(mean_raster, "min", na.rm = TRUE)[[1]]
mean_max <- global(mean_raster, "max", na.rm = TRUE)[[1]]
var_min  <- global(var_raster,  "min", na.rm = TRUE)[[1]]
var_max  <- global(var_raster,  "max", na.rm = TRUE)[[1]]

mean_range <- mean_max - mean_min
var_range  <- var_max  - var_min

mean_norm <- if (mean_range > 0) (mean_raster - mean_min) / mean_range else mean_raster * 0
var_norm  <- if (var_range  > 0) (var_raster  - var_min)  / var_range  else var_raster  * 0

# Convert to data frame
df <- as.data.frame(c(mean_norm, var_norm), xy = TRUE, na.rm = FALSE)
colnames(df)[3:4] <- c("mean", "variance")

# Mean legend plot (grayscale to red)
mean_plot <- ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = mean)) +
  scale_fill_viridis_c(direction = -1) +
  coord_equal() +
  labs(title = "Mean weighted distance (normalized)") +
  theme_minimal()
mean_plot

# Variance legend plot (grayscale to blue)
var_plot <- ggplot(df) +
  geom_tile(aes(x = x, y = y, fill = variance)) +
  scale_fill_viridis_c() +
  coord_equal() +
  labs(title = "Variance in weighted distance (normalized)") +
  theme_minimal()
var_plot

## Let's look at these patterns over a longer timescale--all of 2023
test_wt_year <- dist_to_carcasses(all_carcasses, bbox_south_big, resolution = 5000,
                                  start_date = "2023-01-01", end_date = "2023-12-31", 
                                  weight_col = "carcassWeight",
                                  visibility_radius = 10000, decay_rate = 2, 
                                  distance_power = 2, min_weight = 10)

png_files <- get_pngs(test_wt_year)
imgs <- image_read(png_files)
# animation <- image_animate(imgs, fps = 2) 
# animation
# image_write(animation, path = "fig/year_5000_dist2_vis10km_decay2.gif")

# Carcass availability on the entire landscape over time
carcs_2023 <- all_carcasses %>%
  select(date, carcassWeight, X, Y) %>%
  filter(date >= lubridate::ymd("2023-01-01"), date <= lubridate::ymd("2023-12-31")) %>%
  mutate(date = as.Date(date))

cell_values_long_2023 <- as.data.frame(test_wt_year, cells = T, wide = F) %>%
  mutate(layer = lubridate::ymd(layer))
dim(cell_values_long_2023)
cell_values_long_2023 %>%
  group_by(layer) %>%
  summarize(mn = mean(values/1000)) %>%
  filter(mn < 99936) %>% # restrict to only the cells that sometimes have less than the max distance to a carcass
  ggplot(aes(x = layer))+
  #geom_vline(data = carcs_2023, aes(xintercept = date), alpha = 0.1)+
  geom_line(aes(y = mn), col = "black")+
  theme_classic()+
  labs(title = "Weighted distance to carcasses, 2023",
       subtitle = "Southern region",
       y = "Weighted distance to active carcasses (km)",
       x = "Date",
       caption = "Carcass weights decline exponentially, rate = -2; Distance power = 2 (inverse square)\nBlack line = region-wide mean")

# How far do vultures tend to be from the carcass, compared with the average of pixels? (habitat selection question)
# I don't have full data pulled for any of the years, so let's focus on the high-frequency period in 2023
# Need to extract, for each vulture, the average weighted distance to active carcasses for each day.
tar_load(gps_2023)
start <- min(gps_2023$dateOnly)
end <- max(gps_2023$dateOnly)
layer_dates <- as.Date(names(test_wt_year))
r_subset <- test_wt_year[[layer_dates >= start & layer_dates <= end]]
gps_2023$dateOnly <- as.Date(gps_2023$dateOnly)
layer_index <- match(gps_2023$dateOnly, layer_dates)
gps_2023_sf <- sf::st_as_sf(gps_2023, coords = c("location_long", "location_lat"), crs = "WGS84", remove = F) %>% sf::st_transform(32636)
gps_sf_days <- gps_2023_sf %>% group_by(dateOnly) %>% group_split()
gps_vect_days <- map(gps_sf_days, vect)
dates <- map_chr(gps_vect_days, ~as.character(.x$dateOnly[[1]]))
gps_sf_out <- vector(mode = "list", length = length(gps_vect_days))
for(i in 1:length(dates)){
  rast <- r_subset[[dates[i]]]
  out <- setNames(terra::extract(rast, gps_vect_days[[i]], cells = T, ID = F), c("value", "cell"))
  gps_sf_out[[i]] <- cbind(gps_vect_days[[i]], out)
}
out <- as.data.frame(do.call(rbind, gps_sf_out))

raster_day_means <- as.data.frame(global(r_subset, fun = "mean", na.rm = TRUE)) %>%
  mutate(dateOnly = as.Date(row.names(.))) %>%
  rename("region_mean" = mean)
vulture_day_means <- out %>%
  group_by(local_identifier, dateOnly) %>%
  summarize(mn = mean(value), .groups = "drop") %>%
  left_join(raster_day_means, by = "dateOnly")

# How far are the vultures
vulture_day_means %>%
  ggplot(aes(x = dateOnly, y = sqrt(mn)/1000, group = local_identifier))+
  geom_line(aes(y = sqrt(region_mean)/1000), col = "blue")+
  geom_line(alpha = 0.1)+
  theme_minimal()+ # trivial result--vultures stay much closer to carcasses than the average pixel. In order to really quantify what's going on, we would need to do habitat selection analyses. This also of course doesn't take into account that you can be really close to one carcass and really far from another.
  labs(y = "Mean distance (km)",
       x = "Date (2023)",
       title = "Mean distance of vultures from carcasses over time",
       subtitle = "Blue line = average of pixels in region; black lines = vultures")

# Now get the carcass weights over time
test <- all_carcasses %>% filter(date >= start & date <= end) %>%
  select(carcID, date, X, Y, carcType, carcassWeight) %>%
  mutate(carcassWeight = case_when(is.na(carcassWeight) ~ mean(carcassWeight, na.rm = T), .default = carcassWeight)) %>%
  mutate(date = lubridate::date(date))
dates <- seq.Date(start, end)
carcs <- sort(unique(test$carcID))
dec <- 1.5
fill_in_exp <- function(prev, new, decay = dec) {
  if_else(!is.na(new), new, prev * exp(-1*decay))
}
df <- expand_grid("date" = dates, "carcID" = carcs) %>%
  left_join(test, by = c("date", "carcID")) %>%
  arrange(carcID, date) %>% # fill downward and add exponential decline here
  group_by(carcID) %>%
  fill(c("X", "Y", carcType), .direction = "downup") %>%
  mutate(carcassWeight = accumulate(carcassWeight, fill_in_exp)) %>%
  mutate(carcassWeight = case_when(carcassWeight < 10 ~ NA, .default = carcassWeight))

# Carcass decay over time
df %>%
  ggplot(aes(x = date, y = carcassWeight, group = carcID, col = carcType))+
  geom_line()+
  theme_minimal()+
  labs(y = "Carcass weight (kg)",
       x = "Date",
       col = "Carcass type",
       caption = paste0("Exponential decay parameter = -", dec, "\n", "(Wild carcasses set to mean weight of stn carcasses)"))+
  theme(legend.position = "bottom")+
  scale_color_viridis_d()

# Now, amount of meat on the landscape at a time
meat_on_landscape <- df %>%
  group_by(date) %>%
  summarize(all = sum(carcassWeight, na.rm = T),
            `wild (est)` = sum(carcassWeight[carcType == "wild"], na.rm = T),
            stn = sum(carcassWeight[carcType == "stn"], na.rm = T)) %>%
  pivot_longer(cols = c("all", "wild (est)", "stn"), names_to = "type", values_to = "kg")

meat_on_landscape %>%
  filter(type != "all") %>%
  ggplot(aes(x = date, y = kg, fill = type))+
  geom_area()+
  theme_minimal()+
  labs(y = "Meat on landscape (kg)",
       x = "Date",
       col = "Type of carcass",
       title = "Carcass weight, south, Mar-Apr 2023",
       caption = paste0("Exponential decay parameter = -", dec, "\n", "(Wild carcasses set to mean weight of stn carcasses)"))+
  scale_fill_manual(values = c("firebrick1", "skyblue"))

