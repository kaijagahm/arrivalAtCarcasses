# Defining predictability of feeding stations
# See "Predictability of food resources affects carcass finding" in Obsidian
# How do we define predictability?
# From Riotte-Lambert & Matthiopoulous 2020 TREE: "the value of an environmental variable (e.g., the abundance of a resource) is increasingly predictable at a given spatiotemporal scale if it is characterised by lower variability or higher correlation with itself or another environmental variable, measured at the given spatiotemporal scale."
library(tidyverse)
library(targets)
library(here)
library(sf)

# Load in the carcass data
tar_load(carcasses_audited)
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

# Looking at how a given station changes over time

