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

net_flight <- net %>% select(time, focal, other, flight_sri_scaled) %>%
  filter(focal %in% indivs & other %in% indivs)
net_roost <- net %>% select(time, focal, other, roost_together) %>%
  filter(focal %in% indivs & other %in% indivs)

# ---- 1. Get the full set of node names ----
all_nodes_flight <- sort(unique(c(net_flight$focal, net_flight$other)))
all_nodes_roost <- sort(unique(c(net_roost$focal, net_roost$other)))

# ---- 2. Build one "master" graph (all edges, any time) just to fix a layout ----
# Using all nonzero edges across all times gives a sensible overall layout;
# nodes with no edges anywhere will still be placed (just off on their own).
master_edges_flight <- net_flight %>%
  filter(flight_sri_scaled > 0, focal != other) %>%
  distinct(focal, other) %>%
  rename(from = focal, to = other)

master_edges_roost <- net_roost %>%
  filter(roost_together > 0, focal != other) %>%
  distinct(focal, other) %>%
  rename(from = focal, to = other)

master_graph_flight <- tbl_graph(
  nodes    = tibble(name = all_nodes_flight),
  edges    = master_edges_flight,
  directed = FALSE
)

master_graph_roost <- tbl_graph(
  nodes    = tibble(name = all_nodes_roost),
  edges    = master_edges_roost,
  directed = FALSE
)

set.seed(42)  # fixes the FR algorithm's randomness so layout is reproducible
master_layout_flight <- create_layout(master_graph_flight, layout = "kk")
master_layout_roost <- create_layout(master_graph_roost, layout = "graphopt")

node_coords_flight <- master_layout_flight %>%
  as_tibble() %>%
  select(name, x, y)

node_coords_roost <- master_layout_roost %>%
  as_tibble() %>%
  select(name, x, y)

# ---- 3. Cumulative "red" node sets per time value ----
times_flight <- sort(unique(net_flight$time))
times_roost <- sort(unique(net_roost$time))

informed_by_time_flight <- set_names(
  map(times_flight, ~ ed %>% filter(time <= .x) %>% pull(id) %>% unique()),
  as.character(times_flight)
)

informed_by_time_roost <- set_names(
  map(times_roost, ~ ed %>% filter(time <= .x) %>% pull(id) %>% unique()),
  as.character(times_roost)
)

# ---- Global edge weight range (based on edges that will actually be drawn) ----
weight_range_flight <- net_flight %>%
  filter(flight_sri_scaled > 0) %>%
  summarise(min_w = min(flight_sri_scaled), max_w = max(flight_sri_scaled))

global_min <- weight_range_flight$min_w
global_max <- weight_range_flight$max_w

# ---- Updated plotting function ----
plot_network_time_flight <- function(t) {
  
  edges_t <- net_flight %>%
    filter(time == t, flight_sri_scaled > 0, focal != other) %>%
    transmute(from = focal, to = other, weight = flight_sri_scaled)
  
  nodes_t <- tibble(name = all_nodes_flight) %>%
    mutate(status = if_else(name %in% informed_by_time_flight[[as.character(t)]], "informed", "uninformed"))
  
  g <- tbl_graph(nodes = nodes_t, edges = edges_t, directed = FALSE)
  
  layout_t <- create_layout(
    g, layout = "manual",
    x = node_coords_flight$x[match(nodes_t$name, node_coords_flight$name)],
    y = node_coords_flight$y[match(nodes_t$name, node_coords_flight$name)]
  )
  
  ggraph(layout_t) +
    geom_edge_link(aes(width = weight), alpha = 0.8, colour = "grey50") +
    scale_edge_width(
      range  = c(0.1, 1),
      limits = c(global_min, global_max),  # <-- fixes the width scale across ALL plots
      guide  = "none"
    ) +
    geom_node_point(aes(fill = status), size = 15, alpha = 0.9, pch = 21) +
    scale_fill_manual(values = c(uninformed = "lightgray", informed = "black"), guide = "none") +
    theme_void() +
    labs(title = paste("Network at time", t))
}

all_plots_flight <- set_names(map(times_flight, plot_network_time_flight), as.character(times_flight))

plot_network_time_roost <- function(t) {
  
  edges_t <- net_roost %>%
    filter(time == t, roost_together > 0, focal != other) %>%
    transmute(from = focal, to = other, weight = roost_together)
  
  if(nrow(edges_t) > 0){
    nodes_t <- tibble(name = all_nodes_roost) %>%
      mutate(status = if_else(name %in% red_by_time_roost[[as.character(t)]], "informed", "uninformed"))
    
    g <- tbl_graph(nodes = nodes_t, edges = edges_t, directed = FALSE)
    
    layout_t <- create_layout(
      g, layout = "manual",
      x = node_coords_roost$x[match(nodes_t$name, node_coords_roost$name)],
      y = node_coords_roost$y[match(nodes_t$name, node_coords_roost$name)]
    )
    
    ggraph(layout_t) +
      geom_edge_link(width = 1, alpha = 0.8, colour = "grey50") +
      geom_node_point(aes(fill = status), size = 15, alpha = 0.9, pch = 23) +
      scale_fill_manual(values = c(uninformed = "lightgray", informed = "black"), guide = "none") +
      theme_void()
  }else{
    NULL
  }
}

all_plots_roost <- set_names(map(times_roost, plot_network_time_roost), as.character(times_roost))
all_plots_roost[[1]]
all_plots_roost[[30]]
all_plots_roost[[33]]

# ---- 6. Save all of them ----
walk2(all_plots_flight[1:6], names(all_plots_flight[1:6]), function(p, t) {
  ggsave(filename = paste0("fig/ISBEplots/networks/flight_network_time_", t, ".png"),
         plot = p, width = 7, height = 7, dpi = 300)
})

walk2(all_plots_roost[c(1, 30, 33)], names(all_plots_roost[c(1, 30, 33)]), function(p, t) {
  ggsave(filename = paste0("fig/ISBEplots/networks/roost_network_time_", t, ".png"),
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


