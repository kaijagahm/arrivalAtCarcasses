library(tidyverse)
library(targets)
library(tidygraph)
library(ggraph)

tar_load(fl_wt_cumulative_1)
graphs <- map(fl_wt_cumulative_1[[2]], ~tidygraph::as_tbl_graph(as.matrix(.x)))

windowsFonts(Arial = windowsFont("Arial"))
theme_set(
  theme_graph(base_family = "Arial") +
    theme(text = element_text(family = "Arial"))
)

get_layout <- function(graphs) {
  
  # ---- 1. Collect all nodes ----
  all_nodes <- graphs %>%
    map(~ .x %>% activate(nodes) %>% as_tibble()) %>%
    list_rbind() %>%
    distinct(name)
  
  # ---- 2. Collect all edges, converting indices to names ----
  edges_union <- graphs %>%
    map(function(g) {
      nodes <- g %>% activate(nodes) %>% as_tibble()
      
      g %>%
        activate(edges) %>%
        as_tibble() %>%
        mutate(
          ID1 = nodes$name[from],
          ID2 = nodes$name[to]
        ) %>%
        select(ID1, ID2, weight)
    }) %>%
    list_rbind() %>%
    filter(weight > 0, !is.na(weight)) %>%
    group_by(ID1, ID2) %>%
    summarise(weight = mean(weight), .groups = "drop")
  
  # ---- 3. Build union graph ----
  g_union <- tbl_graph(
    nodes = all_nodes,
    edges = edges_union,
    directed = FALSE
  )
  
  # ---- 4. Compute layout ----
  g_layout <- g_union %>%
    activate(edges) %>%
    mutate(layout_weight = weight)
  
  layout_tbl <- create_layout(
    g_layout,
    layout  = "fr",
    weights = layout_weight
  )
  
  return(layout_tbl)
}

layout <- get_layout(graphs)


plot_network_fixed <- function(graph, layout_tbl, title = NULL) {
  
  # ---- 1. Nodes present in THIS graph ----
  present_nodes <- graph %>%
    activate(nodes) %>%
    as_tibble() %>%
    pull(name)
  
  # ---- 2. Extract edges with node names ----
  nodes_tbl <- graph %>%
    activate(nodes) %>%
    as_tibble()
  
  edges_tbl <- graph %>%
    activate(edges) %>%
    as_tibble() %>%
    mutate(
      ID1 = nodes_tbl$name[from],
      ID2 = nodes_tbl$name[to],
      sri = weight
    ) %>%
    select(ID1, ID2, sri) %>%
    filter(sri > 0, !is.na(sri))
  
  # ---- 3. Rebuild graph on FULL layout node set ----
  g <- tbl_graph(
    nodes = layout_tbl %>% select(name),
    edges = edges_tbl,
    directed = FALSE
  ) %>%
    activate(nodes) %>%
    left_join(layout_tbl, by = "name") %>%
    mutate(present = name %in% present_nodes)
  
  # ---- 4. Plot ----
  plot <- ggraph(g, layout = "manual", x = x, y = y) +
    geom_edge_link(aes(alpha = sri)) +
    
    ## hide absent nodes
    geom_node_point(
      aes(alpha = present),
      size = 2,
      show.legend = FALSE
    ) +
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) +
    
    ## hide labels for absent nodes
    geom_node_text(
      family = "Arial",
      aes(label = if_else(present, name, "")),
      repel = TRUE,
      size = 3,
      color = "blue"
    ) +
    ggtitle(title)
  
  return(plot)
}

plots <- map2(graphs, 1:length(graphs), ~plot_network_fixed(.x, layout_tbl = layout, title = .y))
