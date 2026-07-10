library(tidyverse)
library(targets)
library(ggspatial)
library(sf)
library(ggraph)
library(tidygraph)

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


# Map of all carcasses ----------------------------------------------------
tar_load(all_carcasses)
carcasscolors <- c("#DE9C0D", "#16697A")
map_allcarcasses <- st_crop(all_carcasses, bbox_south_big) %>%
  mutate(carcType = case_when(carcType == "stn" ~ "SFS",
                              carcType == "wild" ~ "Non-SFS")) %>%
  ggplot()+
  annotation_map_tile(zoom = 9, type = "cartolight")+
  geom_sf(aes(fill = carcType), pch = 21, size = 4, alpha = 0.5)+
  labs(fill = "")+
  scale_fill_manual(values = carcasscolors)+
  theme_minimal()+
  theme(text = element_text(size = 18),
        legend.position = "bottom")
ggsave(map_allcarcasses, file = "fig/ISBEplots/map_allcarcasses.png", width = 6, height = 8)

# Dynamic network ---------------------------------------------------------
tar_load(networks_long_combined)
net <- networks_long_combined[[2]]
length(net)
tar_load(event_data)
ed <- event_data[[2]]
ed %>% filter(time < t_end) %>% nrow() == length(unique(net$time))
ed <- ed %>% filter(time < t_end) %>% select(id, time) %>%
  mutate(time = as.numeric(factor(time)))
indivs <- ed$id

net <- net %>% select(time, focal, other, flight_sri_scaled) %>%
  filter(focal %in% indivs & other %in% indivs)

# ---- 1. Get the full set of node names ----
all_nodes <- sort(unique(c(net$focal, net$other)))

# ---- 2. Build one "master" graph (all edges, any time) just to fix a layout ----
# Using all nonzero edges across all times gives a sensible overall layout;
# nodes with no edges anywhere will still be placed (just off on their own).
master_edges <- net %>%
  filter(flight_sri_scaled > 0, focal != other) %>%
  distinct(focal, other) %>%
  rename(from = focal, to = other)

master_graph <- tbl_graph(
  nodes    = tibble(name = all_nodes),
  edges    = master_edges,
  directed = FALSE
)

set.seed(42)  # fixes the FR algorithm's randomness so layout is reproducible
master_layout <- create_layout(master_graph, layout = "kk")

node_coords <- master_layout %>%
  as_tibble() %>%
  select(name, x, y)

# ---- 3. Cumulative "red" node sets per time value ----
times <- sort(unique(net$time))

red_by_time <- set_names(
  map(times, ~ ed %>% filter(time <= .x) %>% pull(id) %>% unique()),
  as.character(times)
)

# ---- Global edge weight range (based on edges that will actually be drawn) ----
weight_range <- net %>%
  filter(flight_sri_scaled > 0) %>%
  summarise(min_w = min(flight_sri_scaled), max_w = max(flight_sri_scaled))

global_min <- weight_range$min_w
global_max <- weight_range$max_w

# ---- Updated plotting function ----
plot_network_time <- function(t) {
  
  edges_t <- net %>%
    filter(time == t, flight_sri_scaled > 0, focal != other) %>%
    transmute(from = focal, to = other, weight = flight_sri_scaled)
  
  nodes_t <- tibble(name = all_nodes) %>%
    mutate(status = if_else(name %in% red_by_time[[as.character(t)]], "red", "black"))
  
  g <- tbl_graph(nodes = nodes_t, edges = edges_t, directed = FALSE)
  
  layout_t <- create_layout(
    g, layout = "manual",
    x = node_coords$x[match(nodes_t$name, node_coords$name)],
    y = node_coords$y[match(nodes_t$name, node_coords$name)]
  )
  
  ggraph(layout_t) +
    geom_edge_link(aes(width = weight), alpha = 0.8, colour = "grey50") +
    scale_edge_width(
      range  = c(0.1, 1),
      limits = c(global_min, global_max),  # <-- fixes the width scale across ALL plots
      guide  = "none"
    ) +
    geom_node_point(aes(fill = status), size = 15, alpha = 0.9, pch = 21) +
    scale_fill_manual(values = c(black = "lightgray", red = "black"), guide = "none") +
    theme_void() +
    labs(title = paste("Network at time", t))
}

all_plots <- set_names(map(times, plot_network_time), as.character(times))

# View one
all_plots[[4]]

# ---- 6. Save all of them ----
walk2(all_plots[1:6], names(all_plots[1:6]), function(p, t) {
  ggsave(filename = paste0("fig/ISBEplots/networks/network_time_", t, ".png"),
         plot = p, width = 7, height = 7, dpi = 300)
})


# Acc plot ----------------------------------------------------------------
tar_load(cal_24_2)
testacc <- cal_24_2[[1]]
tar_load(bo_pr_24_2)
testscores <- bo_pr_24_2[[1]]
whichfeeding <- which(testscores$pred == "Eating")
testacc_summ <- testacc %>% select(contains("acc_"), bout_id) %>%
  pivot_longer(cols = -bout_id, names_to = "name", values_to = "value") %>%
  mutate(dimension = str_remove_all(str_extract(name, "_x_|_y_|_z_"), "_"),
         time = as.numeric(str_extract(name, "[0-9]+")))

testacc_list <- testacc_summ %>% group_by(bout_id) %>% group_split()
testacc_list_feeding <- testacc_list[whichfeeding]
bout_plots <- map(testacc_list_feeding, ~{
  plt <- .x %>%
    ggplot(aes(x = time, y = value, color = dimension))+
    geom_line(linewidth = 2, alpha = 0.9)+
    theme_classic()+
    theme(text = element_text(size = 16),
          axis.text = element_blank(),
          legend.position = "none")+
    scale_color_manual(values = c("#E38FFA", "#C10BF4", "#590570"))+
    labs(y = "ACC value",
         x = "Time")
  return(plt)
})
set.seed(3)
randombouts <- bout_plots[sample(1:length(bout_plots), 3, replace = F)]
ggsave(randombouts[[1]], file = "fig/ISBEplots/accplot_1.png", width = 4, height = 4)
ggsave(randombouts[[2]], file = "fig/ISBEplots/accplot_2.png", width = 4, height = 4)
ggsave(randombouts[[3]], file = "fig/ISBEplots/accplot_3.png", width = 4, height = 4)

# Carcass predictability plot ---------------------------------------------
pred <- readRDS("data/created/predictability_results.RDS")

predplot <- pred %>%
  mutate(carctype = case_when(carcType == "stn" ~ "SFS",
                              carcType == "wild" ~ "Non-SFS")) %>%
  ggplot(aes(y = prop_days_covered, x = carctype, fill = carctype, color = carctype))+
  geom_violin(linewidth = 1, alpha = 0.4)+
  geom_jitter(width = 0.07, size = 5, pch = 1, alpha = 0.9, aes(color = carctype))+
  theme_classic()+
  theme(text = element_text(size = 16),
        legend.position = "none")+
  scale_fill_manual(values = carcasscolors)+
  scale_color_manual(values = carcasscolors)+
  labs(y = "Predictability", x = "Carcass type")
predplot
ggsave(predplot, file = "fig/ISBEplots/predplot.png", width = 7, height = 6)

source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R")
predplot_2 <- pred %>%
  mutate(carctype = case_when(carcType == "stn" ~ "SFS",
                              carcType == "wild" ~ "Non-SFS")) %>%
  ggplot(aes(y = prop_days_covered, x = carctype, fill = carctype, color = carctype)) +
  geom_flat_violin(alpha = 0.8, size = 0.5) +
  #geom_jitter(width = 0.1, size = 4, pch = 1, alpha = 0.9)+
  geom_boxplot(aes(color = carctype), fill = NA, width = 0.2, position = position_nudge(x = -0.15))+
  theme(legend.position="none")+
  theme_classic()+
  theme(text = element_text(size = 16),
        legend.position = "none")+
  labs(y = "Predictability", x = "Carcass type")+
  scale_fill_manual(values = carcasscolors)+
  scale_color_manual(values = carcasscolors)

ggsave(predplot_2, file = "fig/ISBEplots/predplot_2.png", width = 5, height = 7)


