# 2026-03-31 Demo Script
library(STbayes)
library(tidyverse)
library(targets)
library(mapview)
library(ggraph)
library(tidygraph)
library(posterior)
lapply(list.files("R", full.names = TRUE), source) 

# 1. Read in GPS and other data --------------------------------------------------------
tar_load(stn_carcs)
carc <- stn_carcs[[24]]
tar_load(rp) # roost polygons
event_time <- carc$datetime_il
event_date <- lubridate::date(event_time)
mapview(carc) # Carcass placed in the morning, 11:13 am Israel time, 2023-03-22.

gps_fornetwork <- readRDS("data/created/gps_1.RDS")
gps_fornetwork_day1 <- readRDS("data/created/gps_1_day1.RDS")
first_sightings <- readRDS("data/created/first_sightings_1.RDS")
first_sightings_day1 <- readRDS("data/created/first_sightings_1_day1.RDS")
event_data <- readRDS("data/created/event_data_1.RDS")
event_data_day1 <- readRDS("data/created/event_data_1_day1.RDS")

# Make cutpoints based on the event data
cutpoints <- unique(event_data$time)
cutpoints_day1 <- unique(event_data_day1$time)
if(!(0 %in% cutpoints)){
  cutpoints <- c(0, cutpoints)}
if(!(0 %in% cutpoints_day1)){
  cutpoints_day1 <- c(0, cutpoints_day1)}

cutpoints[length(cutpoints)] <- event_data$t_end[1]
cutpoints_day1[length(cutpoints_day1)] <- event_data_day1$t_end[1]
gps_fornetwork$network <- cut(gps_fornetwork$time, breaks = cutpoints)
gps_fornetwork_day1$network <- cut(gps_fornetwork_day1$time, breaks = cutpoints_day1)

length(cutpoints) # need 64 cutpoints so we can have 63 bins so we can define 62 events plus censored indivs.
length(cutpoints_day1) # need 23 cutpoints so we can have 22 bins so we can define 21 events plus censored indivs

# 2. Make co-flight networks --------------------------------------------------------
gps_list <- gps_fornetwork %>% arrange(network) %>% group_split(network, .keep = TRUE)
gps_list_day1 <- gps_fornetwork_day1 %>% arrange(network) %>% group_split(network, .keep = TRUE)

gps_list <- map(gps_list, ~{
  if(nrow(.x) == 1 & all(is.na(.x$individual_local_identifier))){
    return(.x[0,])}else{return(.x)}})

gps_list_day1 <- map(gps_list_day1, ~{
  if(nrow(.x) == 1 & all(is.na(.x$individual_local_identifier))){
    return(.x[0,])}else{return(.x)}})

networks_long_dynamic <- readRDS("data/created/networks_long_dynamic_fixed.RDS")
networks_long_dynamic_day1 <- readRDS("data/created/networks_long_dynamic_fixed_day1.RDS")

# 4.1. Make and save network plots --------------------------------------------
get_plots <- function(networksdf){
  global_graph <- networks_long_dynamic %>% as_tbl_graph(directed = FALSE)
  global_layout <- create_layout(global_graph, layout = "eigen")
  node_positions <- global_layout %>% as_tibble() %>% select(name, x, y)
  
  nets <- networks_long_dynamic %>% group_split(time, .keep = F) %>% purrr::map(., ~as_tbl_graph(.x, directed = FALSE)) %>%
    purrr::map(., ~.x %>% activate(edges) %>% filter(flight_sri > 0) %>% activate(nodes) %>% left_join(node_positions, by = "name"))
  plots <- map2(nets, 1:length(nets), ~{
    ggraph(.x, layout = "manual", x = x, y = y) +
      geom_edge_link(aes(width = flight_sri), alpha = 0.6) +
      geom_node_point(size = 3, color = "steelblue", alpha = 0.75) +
      scale_edge_width(range = c(0.05, 1)) +
      theme_void()+
      ggtitle(paste0("Time = ", .y))
  })
  return(plots)
}
plots_all <- get_plots(networks_long_dynamic)
plots_day1 <- get_plots(networks_long_dynamic_day1)
# walk2(plots_all, seq_along(plots_all), ~{
#   ggsave(filename = paste0("fig/dynamic_flight_networks/all/plot_", .y, ".png"),
#          plot = .x, width = 4, height = 4, dpi = 300)})
# walk2(plots_day1, seq_along(plots_all), ~{
#   ggsave(filename = paste0("fig/dynamic_flight_networks/day1/plot_", .y, ".png"),
#          plot = .x, width = 4, height = 4, dpi = 300)})

# 3. Read in ILVs ---------------------------------------------------------
ILV_c <- readRDS("data/created/ILV_c_1.RDS")
ILV_c_day1 <- readRDS("data/created/ILV_c_1_day1.RDS")
ILV_tv <- readRDS("data/created/ILV_tv_1.RDS")
ILV_tv_day1 <- readRDS("data/created/ILV_tv_1_day1.RDS")

# 4. Define data lists and models -----------------------------------------
## Data lists
datalist_orig <- readRDS("data/data_lists/dynamic_daylight_ilvs1_fixed.RDS")
datalist_distsq <- readRDS("data/data_lists/dynamic_daylight_ilvs1_fixed_sq.RDS")
datalist_day1 <- readRDS("data/data_lists/dynamic_daylight_ilvs1_fixed_day1.RDS")

## Models
mod_orig <- generate_STb_model(datalist_orig)
mod_distsq <- generate_STb_model(datalist_distsq)
mod_day1 <- generate_STb_model(datalist_day1)
mod_day1_comp <- generate_STb_model(datalist_day1, transmission_func="freqdep_f")

# 5. Examine model results -----------------------------------------
## Models
fit_orig <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs1_fixed.rds') 
fit_distsq <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs1_fixed_sq.rds') 
fit_day1 <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs1_fixed_day1.rds') 
fit_day1_comp <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs1_fixed_day1_comp.rds')

## Asocial models
asoc_orig <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs_asoc1_fixed.rds') 
# asoc_distsq <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs_asoc1_fixed_sq.rds') 
# asoc_day1 <- readRDS('data/cmdstan_saves/dynamic_daylight_ilvs_asoc1_fixed_day1.rds') 

## Summaries
summ <- STb_summary(fit_orig, digits = 3)
summ_sq <- STb_summary(fit_distsq, digits = 3)
summ_day1 <- STb_summary(fit_day1, digits = 3)
summ_day1_comp <- STb_summary(fit_day1_comp, digits = 3)

get_ilv_results <- function(summ, title = ""){
  plt <- summ %>% filter(grepl("beta_", Parameter)) %>%
    select(Parameter, Median, CI_Lower, CI_Upper) %>%
    mutate(type = str_extract(Parameter, "ILVs|ILVi"),
           type = case_when(type == "ILVi" ~ "Effect on intrinsic rate",
                            type == "ILVs" ~ "Effect on social rate",
                            .default = type)) %>%
    mutate(param = str_remove(Parameter, "beta_"),
           param = str_remove(param, "ILVi_"),
           param = str_remove(param, "ILVs_"),
           param = str_remove(param, "_norm")) %>%
    ggplot(aes(x = param, y = Median))+
    geom_point()+
    geom_segment(aes(x = param, xend = param, y = CI_Lower, yend = CI_Upper))+
    coord_flip()+
    theme_minimal()+
    facet_wrap(~type, ncol = 1, scale = "free_y")+
    geom_hline(aes(yintercept = 0), linetype = 2)+
    labs(subtitle = "(95% CIs)",
         title = title,
         x = "Parameter")
  return(plt)
} ## utility function

get_ilv_results(summ, "Orig")
get_ilv_results(summ_sq, "Distsq")
get_ilv_results(summ_day1, "Day1")
get_ilv_results(summ_day1_comp, "Day1_comp")

plot_data_obs <- get_plot_data(event_data)
plot_data_obs_day1 <- get_plot_data(event_data_day1)

ppc_orig <- get_plot_data_ppc(fit = fit_orig, data_list = datalist_orig)
ppc_distsq <- get_plot_data_ppc(fit = fit_distsq, data_list = datalist_distsq)
ppc_day1 <- get_plot_data_ppc(fit = fit_day1, data_list = datalist_day1)
ppc_day1_comp <- get_plot_data_ppc(fit = fit_day1_comp, data_list = datalist_day1)

ppc_asoc <- get_plot_data_ppc(fit = asoc_orig, data_list = datalist_orig)

plotppc <- function(ppc, obs, title){
  plt <- ggplot() +
    geom_line(data = ppc, 
              aes(x = time, y = cum_prop, 
                  group = interaction(draw, trial)), alpha = .1) +
    geom_line(data = obs, aes(x = time, y = cum_prop), linewidth = 1) +
    labs(x = "Time", y = "Cumulative proportion informed",
         title = title) +
    theme_minimal()
  return(plt)
} ## utility function

plotppc(ppc_asoc, plot_data_obs, "Asoc")
plotppc(ppc_orig, plot_data_obs, "Orig")
plotppc(ppc_distsq, plot_data_obs, "Distsq")
plotppc(ppc_day1, plot_data_obs_day1, "Day1")
plotppc(ppc_day1_comp, plot_data_obs_day1, "Day1_comp")
