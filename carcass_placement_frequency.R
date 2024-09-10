# Understanding the frequency with which carcasses are placed
library(tidyverse)
library(sf)
library(mapview)
library(here)
library(readxl)

feed <- read_excel(here("data/FeedingData from 2018_2024_Translated.xlsx")) %>% 
  select(contains("RTG"), contains("WGS84"), "date" = `Date Event`, "time" = `Event time`) %>%
  rename("inspectionArea" = `Inspection area - RTG`,
         "merhavRTG" = `Merhav - RTG`,
         "district" = `RTG district`,
         "long" = `WGS84 - LONG`,
         "lat" = `WGS84 - LAT`) %>%
  mutate(datetime = lubridate::ymd_hms(paste0(as.character(date), " ", substr(as.character(time), 12, 19)))) %>%
  mutate(year = lubridate::year(datetime))

feed_sf <- feed %>%
  st_as_sf(coords = c("long", "lat"), crs = "WGS84") %>%
  st_transform(32636)

fs_buffered <- st_buffer(feed_sf, 300)
parts <- st_cast(st_union(fs_buffered),"POLYGON")
clust <- unlist(st_intersects(fs_buffered, parts))
diss <- cbind(fs_buffered, clust) %>%
  group_by(clust)

ggplot(diss)+
  geom_sf(aes(col = factor(clust), fill = factor(clust)))

mapview(feed_sf, zcol = "inspectionArea")
mapview(feed_sf, zcol = "merhavRTG")
mapview(feed_sf, zcol = "district")

ggplot(diss, aes(x = datetime, y = clust, col = factor(clust)))+
  geom_point()+
  theme_bw()+
  ylab("Feeding Station")+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  labs(title = "Carcass placement at ~feeding stations",
       caption = "Feeding stations created by buffering and merging nearby coordinates.")+
  scale_x_datetime(date_breaks = "6 months",
                   date_labels = "%m/%y")

# Zooming in year by year
diss %>%
  group_by(year, clust) %>%
  filter(n() > 4) %>%
  ggplot(aes(x = datetime, y = factor(clust), 
             col = factor(clust)))+
  geom_point()+
  facet_wrap(~year, scales = "free") +
  theme_bw()+
  ylab("Feeding Station")+
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  labs(title = "Carcass placement at ~feeding stations, by year",
       caption = "Feeding stations created by buffering and merging nearby coordinates.")+
  scale_x_datetime(date_breaks = "2 months",
                   date_labels = "%b")

# now let's get some stats for this
stats <- diss %>%
  arrange(clust, datetime) %>%
  group_by(clust) %>%
  mutate(interval = difftime(datetime, lag(datetime), units = "days"))

stats %>%
  group_by(clust) %>%
  filter(n() > 4) %>%
  filter(!is.na(interval)) %>%
  ggplot(aes(x = fct_reorder(factor(clust), interval, .fun = median), 
             y = interval))+
  geom_boxplot(outlier.shape = NA)+
  theme_minimal()+
  geom_jitter(alpha = 0.3, aes(col = factor(clust)), width = 0.2)+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  ylab("Interval (days)")+
  xlab("Feeding station")

stats %>%
  group_by(clust) %>%
  filter(n() > 4) %>%
  filter(!is.na(interval)) %>%
  ggplot(aes(x = fct_reorder(factor(clust), interval, .fun = median), 
             y = interval))+
  geom_boxplot(outlier.shape = NA)+
  theme_minimal()+
  geom_jitter(alpha = 0.3, aes(col = factor(clust)), width = 0.2)+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  ylab("Interval (days)")+
  xlab("Feeding station")+
  facet_wrap(~year, scales = "free_y")

# Now picking the top 5
stats %>%
  filter(!is.na(interval)) %>%
  group_by(clust) %>%
  mutate(size = n()) %>%
  ungroup() %>%
  filter(size >= sort(unique(size), TRUE)[5])  %>%
  arrange(desc(size)) %>%
  ggplot(aes(x = fct_reorder(factor(clust), interval, .fun = median), 
             y = interval))+
  geom_boxplot(outlier.shape = NA)+
  theme_minimal()+
  geom_jitter(alpha = 0.3, aes(col = factor(clust)), width = 0.2)+
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  ylab("Interval (days)")+
  xlab("Feeding station")+
  facet_wrap(~year, scales = "free_y")

lines <- stats %>%
  st_drop_geometry() %>%
  filter(!is.na(interval)) %>%
  group_by(clust) %>%
  mutate(size = n()) %>%
  ungroup() %>%
  filter(size >= sort(unique(size), TRUE)[5]) %>%
  group_by(clust, year) %>%
  summarize(mean = mean(interval))

stats %>%
  filter(!is.na(interval)) %>%
  group_by(clust) %>%
  mutate(size = n()) %>%
  ungroup() %>%
  filter(size >= sort(unique(size), TRUE)[5])  %>%
  arrange(desc(size)) %>%
  ggplot(aes(x = interval, group = clust, col = factor(clust)))+
  geom_density(linewidth = 1)+
  theme_minimal()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.y = element_blank())+
  xlab("Interval (days)")+
  facet_wrap(~year, scales = "free")+
  geom_vline(data = lines, aes(xintercept = mean, col = factor(clust)), linetype = 2, linewidth = 0.5)+
  labs(title = "Frequency at 5 overall most provisioned feeding stations, by year")+
  ylab("")
