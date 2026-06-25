library(tidyverse)
library(targets)
library(ggspatial)

tar_load(stn_carcs)
tar_load(bbox_south_big)
carc1 <- stn_carcs[[24]]
carc2 <- stn_carcs[[27]]
carc3 <- stn_carcs[[26]]
carcs <- list(carc1, carc2, carc3)

gps1 <- readRDS("data/created/gps_for_STbayes.RDS") %>%
  filter(as.numeric(time_since_carcass) >= 0, as.numeric(time_since_carcass) <= 72) %>% st_transform(32636)
gps2 <- readRDS("data/created/gps_for_STbayes_2.RDS") %>%
  filter(as.numeric(time_since_carcass) >= 0, as.numeric(time_since_carcass) <= 72) %>% st_transform(32636)
gps3 <- readRDS("data/created/gps_for_STbayes_3.RDS") %>%
  filter(as.numeric(time_since_carcass) >= 0, as.numeric(time_since_carcass) <= 72) %>% st_transform(32636)

st_crop(gps1, bbox_south_big) %>%
  filter(date_il %in% c("2023-03-22", "2023-03-23", "2023-03-24")) %>%
  ggplot()+
  annotation_map_tile(zoom = 9, type = "cartolight")+
  geom_sf(aes(col = factor(individual_id)), pch = 1, alpha = 0.7)+
  theme(legend.position = "none")+
  facet_wrap(~date_il)+
  geom_sf(data = carc1, color = "black", size = 2)

st_crop(gps2, bbox_south_big) %>%
  filter(date_il %in% c("2023-03-24", "2023-03-25", "2023-03-26")) %>%
  ggplot()+
  annotation_map_tile(zoom = 9, type = "cartolight")+
  geom_sf(aes(col = factor(individual_id)), pch = 1, alpha = 0.7)+
  theme(legend.position = "none")+
  facet_wrap(~date_il)+
  geom_sf(data = carc2, color = "black", size = 2)

st_crop(gps3, bbox_south_big) %>%
  filter(date_il %in% c("2023-03-23", "2023-03-24", "2023-03-25")) %>%
  ggplot()+
  annotation_map_tile(zoom = 9, type = "cartolight")+
  geom_sf(aes(col = factor(individual_id)), pch = 1, alpha = 0.7)+
  theme(legend.position = "none")+
  facet_wrap(~date_il)+
  geom_sf(data = carc3, color = "black", size = 2)

carcs_focal %>%
  st_crop(bbox_south_big) %>%
  select(stationName) %>%
  group_by(stationName) %>%
  summarise(st_union(geometry)) %>%
  st_centroid() %>%
  ggplot()+
  annotation_map_tile(zoom = 9, type = "cartolight")+
  geom_sf(aes(fill = stationName), size = 4, pch = 21)+
  labs(fill = "SFS Name")

tar_load(all_carcasses)
st_crop(all_carcasses, bbox_south_big) %>%
  mutate(carcType = case_when(carcType == "stn" ~ "SFS",
                              carcType == "wild" ~ "Non-sfs")) %>%
  ggplot()+
  annotation_map_tile(zoom = 9, type = "cartolight")+
  geom_sf(aes(fill = carcType), pch = 21, size = 4, alpha = 0.5)+
  labs(fill = "")+
  scale_fill_manual(values = c("olivedrab3", "darkorange3"))+
  theme_minimal()+
  theme(text = element_text(size = 18),
        legend.position = "bottom")
