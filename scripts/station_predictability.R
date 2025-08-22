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
  select(carcID, date, time, datetime, long, lat, stationName, carcassWeight, geometry, X, Y)

carcs_simple <- carcasses_south %>%
  select(date, stationName) %>%
  st_drop_geometry() %>%
  distinct()
dim(carcs)
dim(carcs_simple) # 95 instances of multiple carcasses being placed on the same day

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
       y = "Interval (days)")+
  theme_minimal()

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
  geom_density(aes(group = factor(end)), linewidth = 0.1)+
  theme_minimal() # okay, this is super duper right-skewed

# What happens if we log-transform the variances?
predi %>%
  filter(end < lubridate::date("2024-01-01")) %>% 
  ggplot(aes(x = log(var_int)))+
  geom_density(aes(group = factor(end)), linewidth = 0.1)+
  theme_minimal() # this is so beautiful that it basically has me convinced that this is how we should divide the carcasses up.

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