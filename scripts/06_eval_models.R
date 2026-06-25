library(targets)
library(future)
library(furrr)
library(progressr)
library(tidyverse)
library(STbayes)
library(sf)
library(loo)
library(posterior)
library(patchwork)
plan(multisession, workers = 5)
handlers(global = TRUE)
source("R/functions.R")
nit <- 500

# # Read in fits
# ## Social stn
# tar_load(social_fits_noILVs_2nets)
# tar_load(social_fits_DistI_2nets)
# tar_load(social_fits_DistIS_2nets)
# tar_load(social_fits_DistI_AgeIS_2nets)
# tar_load(social_fits_DistIS_AgeIS_2nets)
# 
# ## Social wild
# tar_load(social_fits_noILVs_wild_2nets)
# tar_load(social_fits_DistI_wild_2nets)
# tar_load(social_fits_DistIS_wild_2nets)
# tar_load(social_fits_DistI_AgeIS_wild_2nets)
# tar_load(social_fits_DistIS_AgeIS_wild_2nets)
# 
# ## Asocial stn
# tar_load(asocial_fits_noILVs_2nets)
# tar_load(asocial_fits_DistI_2nets)
# tar_load(asocial_fits_DistIS_2nets)
# tar_load(asocial_fits_DistI_AgeIS_2nets)
# tar_load(asocial_fits_DistIS_AgeIS_2nets)
# 
# ## Asocial wild
# tar_load(asocial_fits_noILVs_wild_2nets)
# tar_load(asocial_fits_DistI_wild_2nets)
# tar_load(asocial_fits_DistIS_wild_2nets)
# tar_load(asocial_fits_DistI_AgeIS_wild_2nets)
# tar_load(asocial_fits_DistIS_AgeIS_wild_2nets)

# Load model summaries
tar_load(summs_2nets)
tar_load(summs_wild_2nets)

# Get carcass data (stn and wild)
tar_load(stn_carcs)
tar_load(wild_carcs)

# Model convergence checks ------------------------------------------------
## Rhat check
hist(summs_2nets$Rhat) # this is fine
hist(summs_wild_2nets$Rhat)
# this is also fine

### Rhat by parameter
summs_2nets %>%
  ggplot(aes(x = Rhat))+ geom_density(aes(color = Parameter))+
  theme_minimal()+ theme(legend.position = "bottom")

summs_wild_2nets %>%
  ggplot(aes(x = Rhat))+ geom_density(aes(color = Parameter))+
  theme_minimal()+ theme(legend.position = "bottom") # these look fine

### Rhat by model
summs_2nets %>%
  mutate(model = factor(model, levels = c("noILVs", "DistI", "DistIS", 
                                          "DistI_AgeIS", "DistIS_AgeIS"))) %>%
  ggplot(aes(x = Rhat))+ geom_density(aes(color = Parameter))+
  theme_minimal()+ theme(legend.position = "bottom")+ facet_wrap(~model)

# Model comparison ------------------------------------------------
tar_load(comps)
tar_load(comps_wild)
tar_load(comps_dfs)
tar_load(comps_dfs_wild)

# Examining which are the most common orders
names(comps_dfs) <- map_dbl(stn_carcs, "carcID")
names(comps_dfs_wild) <- map_dbl(wild_carcs, "carcID")
comps_dfs_df <- purrr::list_rbind(comps_dfs, names_to = "carcID")
comps_dfs_wild_df <- purrr::list_rbind(comps_dfs_wild, names_to = "carcID")
rownames(comps_dfs_df) <- NULL
rownames(comps_dfs_wild_df) <- NULL

comps_dfs_df <- comps_dfs_df %>%
  arrange(carcID, desc(elpd_diff)) %>%
  group_by(carcID) %>%
  mutate(idx = 1:n())

comps_dfs_wild_df <- comps_dfs_wild_df %>%
  arrange(carcID, desc(elpd_diff)) %>%
  group_by(carcID) %>%
  mutate(idx = 1:n())

comps_dfs_df %>%
  ggplot(aes(x = carcID, y = log(abs(elpd_diff)), col = factor(model)))+
  geom_point()+
  coord_flip()+ # noILVs is consistently the worst, but there isn't much/any pattern in terms of which one is the best.
  theme_minimal()

comps_dfs_wild_df %>%
  ggplot(aes(x = carcID, y = log(abs(elpd_diff)), col = factor(model)))+
  geom_point()+
  coord_flip()+ # similar story with wild. noILVs is almost always the worst, but at the top there's not a single consistent pattern.
  theme_minimal()

# Note that this doesn't take into account which ones are actually different from each other. Let's see if we can do that.

topmods_stn <- comps_dfs_df %>%
  filter(elpd_diff + se_diff >= 0)
topmods_wild <- comps_dfs_wild_df %>%
  filter(elpd_diff + se_diff >= 0)

topmods_stn %>%
  group_by(model) %>%
  summarize(in_top_mods.prop_carcs = length(unique(carcID))/length(stn_carcs)) %>%
  arrange(desc(in_top_mods.prop_carcs)) %>%
  ggplot(aes(x = factor(model, levels = model), y = in_top_mods.prop_carcs, fill = factor(model)))+
  geom_col()+
  theme_minimal()+
  theme(text = element_text(size = 18),
        legend.position = "none")+
  labs(y = "In top mods? (Prop. carcs)",
       x = "Model",
       title = "Stn")+
  scale_y_continuous(limits = c(0, 1))

topmods_wild %>%
  group_by(model) %>%
  summarize(in_top_mods.prop_carcs = length(unique(carcID))/length(wild_carcs)) %>%
  arrange(desc(in_top_mods.prop_carcs)) %>%
  ggplot(aes(x = factor(model, levels = model), y = in_top_mods.prop_carcs, fill = factor(model)))+
  geom_col()+
  theme_minimal()+
  theme(text = element_text(size = 18),
        legend.position = "none")+
  labs(y = "In top mods? (Prop. carcs)",
       x = "Model",
       title = "Wild")+
  scale_y_continuous(limits = c(0, 1))

# In neither case is there one model formulation that is consistently one of the top models.

# Model comparison plots
get_comps_plots <- function(x){
  if(nrow(x) > 0){
    p <- ggplot(x, aes(x = reorder(model, elpd_diff), y = elpd_diff)) +
      geom_point(size = 3) + #elpd_diff
      geom_errorbar(aes(ymin = elpd_diff - se_diff, 
                        ymax = elpd_diff + se_diff), width = 0.2) + #SE of elpd diff
      coord_flip() +
      labs(x = "Model", y = "ELPD Difference", title = "Model Comparison") +
      theme_minimal()
    return(p)
  }else{
    return(NULL)
  }
}

comps_plots <- map(comps_dfs, get_comps_plots)
comps_plots_wild <- map(comps_dfs_wild, get_comps_plots)

# See the comparison plots
comps_plots[[1]]
comps_plots_wild[[1]]

# Are the coefficients similar across different models?
coefs_withinfo_stn <- summs_2nets %>%
  left_join(data.frame(carcID = map_dbl(stn_carcs, "carcID"), idx = 1:length(stn_carcs)), by = "idx") %>%
  mutate(carcID = as.character(carcID)) %>%
  left_join(select(topmods_stn, carcID, model) %>%
              mutate(in_topmods = T), by = c("model", "carcID")) %>%
  mutate(in_topmods = replace_na(in_topmods, F))

coefs_withinfo_wild <- summs_wild_2nets %>%
  left_join(data.frame(carcID = map_dbl(wild_carcs, "carcID"), idx = 1:length(wild_carcs)), by = "idx") %>%
  mutate(carcID = as.character(carcID)) %>%
  left_join(select(topmods_wild, carcID, model) %>%
              mutate(in_topmods = T), by = c("model", "carcID")) %>%
  mutate(in_topmods = replace_na(in_topmods, F))

# What about percent_ST only?
coefs_withinfo_stn %>%
  filter(idx == 1, Parameter %in% c("percent_ST[1]", "percent_ST[2]")) %>%
  ggplot(aes(x = Parameter, color = factor(model), y = Median))+
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, linewidth = in_topmods), position = position_dodge(width = 0.5), width = 0, alpha = 0.8)+
  geom_point(aes(size = in_topmods), position = position_dodge(width = 0.5), pch = 1)+
  theme_minimal()+
  coord_flip()+
  labs(y = "Estimate", x = "Parameter", color = "Model",
       size = "Top model?", linewidth = "Top model?",
       title = map_dbl(stn_carcs, "carcID")[1])+ 
  scale_linewidth_manual(values = c(0.5, 1.5))+
  scale_size_manual(values = c(1, 3)) # okay yeah the estimates of %ST don't change.

# Let's do this for the rest of them.
model_levels <- c("DistI", "DistI_AgeIS", "DistIS", "DistIS_AgeIS", "noILVs")
model_colors <- c(
  "DistI"        = "#E41A1C",
  "DistI_AgeIS"  = "#377EB8",
  "DistIS"       = "#4DAF4A",
  "DistIS_AgeIS" = "#FF7F00",
  "noILVs"       = "#984EA3"
)

shared_scales <- list(
  scale_color_manual(values = model_colors, limits = model_levels),
  scale_linewidth_manual(values = c(0.5, 1.5), limits = c(FALSE, TRUE)),
  scale_size_manual(values = c(1, 3), limits = c(FALSE, TRUE))
)

plot_betas <- function(df, i, carcs){
  df %>%
    mutate(model = factor(model, levels = model_levels)) %>%
    filter(idx == i, grepl("beta", Parameter)) %>%
    ggplot(aes(x = Parameter, color = model, y = Median))+
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, linewidth = factor(in_topmods)),
                  position = position_dodge(width = 0.5), width = 0, alpha = 0.8)+
    geom_point(aes(size = factor(in_topmods)),
               position = position_dodge(width = 0.5), pch = 1)+
    theme_minimal()+
    scale_x_discrete(labels = function(x) {
      gsub(" ", "_", str_wrap(gsub("_", " ", x), width = 15))
    })+       
    coord_flip()+
    labs(y = "Estimate", x = "Parameter", color = "Model",
         size = "Top model?", linewidth = "Top model?",
         title = map_dbl(carcs, "carcID")[i])+
    shared_scales
}

betas_plots_stn <- map(1:length(stn_carcs), ~{
  plot_betas(coefs_withinfo_stn, .x, carcs = stn_carcs) +
    theme(legend.position = "none")
})

betas_plots_wild <- map(1:length(wild_carcs), ~{
  plot_betas(coefs_withinfo_wild, .x, carcs = wild_carcs) +
    theme(legend.position = "none")
})

plot_pctST <- function(df, i, carcs){
  plt <- df %>%
    mutate(model = factor(model, levels = c("DistI", "DistI_AgeIS", "DistIS", "DistIS_AgeIS", "noILVs"))) %>%
    filter(idx == i, grepl("percent_ST", Parameter)) %>%
    ggplot(aes(x = Parameter, color = factor(model), y = Median))+
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, linewidth = factor(in_topmods)), position = position_dodge(width = 0.5), width = 0, alpha = 0.8)+
    geom_point(aes(size = factor(in_topmods)), position = position_dodge(width = 0.5), pch = 1)+
    theme_minimal()+
    geom_hline(aes(yintercept = 0))+
    coord_flip()+
    labs(y = "Estimate", x = "Parameter", color = "Model",
         size = "Top model?", linewidth = "Top model?",
         title = map_dbl(carcs, "carcID")[i])+ 
    shared_scales
  return(plt)
}

pctST_plots_stn <- map(1:length(stn_carcs), ~{
  plot_pctST(coefs_withinfo_stn, .x, carcs = stn_carcs)
})

pctST_plots_wild <- map(1:length(wild_carcs), ~{
  plot_pctST(coefs_withinfo_wild, .x, carcs = wild_carcs)
})

patchworks_stn <- map2(betas_plots_stn, pctST_plots_stn, ~{.x + .y})
patchworks_wild <- map2(betas_plots_wild, pctST_plots_wild, ~{.x + .y})

patchworks_stn[[1]]
patchworks_stn[[2]]
patchworks_stn[[3]]
patchworks_wild[[1]]
patchworks_wild[[2]]
patchworks_wild[[3]]

# Model output evaluation and inter-model comparisons ---------------------
# Comparison between each model and its asocial equivalent
comps_noILVs <- map2(social_fits_noILVs, asocial_fits_noILVs, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("noILVs", "noILVs_asoc"), method = "loo-psis")}else{NULL}})

comps_DistI <- map2(social_fits_DistI, asocial_fits_DistI, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistI", "DistI_asoc"), method = "loo-psis")}else{NULL}})

comps_DistIS <- map2(social_fits_DistIS, asocial_fits_DistIS, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistIS", "DistIS_asoc"), method = "loo-psis")}else{NULL}})

comps_DistI_AgeIS <- map2(social_fits_DistI_AgeIS, asocial_fits_DistI_AgeIS, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistI_AgeIS", "DistI_AgeIS_asoc"), method = "loo-psis")}else{NULL}})

comps_DistIS_AgeIS <- map2(social_fits_DistIS_AgeIS, asocial_fits_DistIS_AgeIS, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistIS_AgeIS", "DistIS_AgeIS_asoc"), method = "loo-psis")}else{NULL}})

comps_noILVs_wild <- map2(social_fits_noILVs_wild, asocial_fits_noILVs_wild, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("noILVs", "noILVs_asoc"), method = "loo-psis")}else{NULL}})

comps_DistI_wild <- map2(social_fits_DistI_wild, asocial_fits_DistI_wild, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistI", "DistI_asoc"), method = "loo-psis")}else{NULL}})

comps_DistIS_wild <- map2(social_fits_DistIS_wild, asocial_fits_DistIS_wild, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistIS", "DistIS_asoc"), method = "loo-psis")}else{NULL}})

comps_DistI_AgeIS_wild <- map2(social_fits_DistI_AgeIS_wild, asocial_fits_DistI_AgeIS_wild, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistI_AgeIS", "DistI_AgeIS_asoc"), method = "loo-psis")}else{NULL}})

comps_DistIS_AgeIS_wild <- map2(social_fits_DistIS_AgeIS_wild, asocial_fits_DistIS_AgeIS_wild, ~{if(!is.null(.x)){STb_compare(.x, .y, model_names = c("DistIS_AgeIS", "DistIS_AgeIS_asoc"), method = "loo-psis")}else{NULL}})

comps_dfs_noILVs <- map(comps_noILVs, ~as.data.frame(.x$comparison))
comps_dfs_DistI <- map(comps_DistI, ~as.data.frame(.x$comparison))
comps_dfs_DistIS <- map(comps_DistIS, ~as.data.frame(.x$comparison))
comps_dfs_DistI_AgeIS <- map(comps_DistI_AgeIS, ~as.data.frame(.x$comparison))
comps_dfs_DistIS_AgeIS <- map(comps_DistIS_AgeIS, ~as.data.frame(.x$comparison))

comps_dfs_noILVs_wild <- map(comps_noILVs_wild, ~as.data.frame(.x$comparison))
comps_dfs_DistI_wild <- map(comps_DistI_wild, ~as.data.frame(.x$comparison))
comps_dfs_DistIS_wild <- map(comps_DistIS_wild, ~as.data.frame(.x$comparison))
comps_dfs_DistI_AgeIS_wild <- map(comps_DistI_AgeIS_wild, ~as.data.frame(.x$comparison))
comps_dfs_DistIS_AgeIS_wild <- map(comps_DistIS_AgeIS_wild, ~as.data.frame(.x$comparison))

comps_dfs_noILVs <- map(comps_dfs_noILVs, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistI <- map(comps_dfs_DistI, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistIS <- map(comps_dfs_DistIS, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistI_AgeIS <- map(comps_dfs_DistI_AgeIS, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistIS_AgeIS <- map(comps_dfs_DistIS_AgeIS, ~{.x$model <- rownames(.x);return(.x)})

comps_dfs_noILVs_wild <- map(comps_dfs_noILVs_wild, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistI_wild <- map(comps_dfs_DistI_wild, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistIS_wild <- map(comps_dfs_DistIS_wild, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistI_AgeIS_wild <- map(comps_dfs_DistI_AgeIS_wild, ~{.x$model <- rownames(.x);return(.x)})
comps_dfs_DistIS_AgeIS_wild <- map(comps_dfs_DistIS_AgeIS_wild, ~{.x$model <- rownames(.x);return(.x)})

get_diff_from_asoc <- function(comparison_df){
  if(!is.null(comparison_df)){
    interval <- c((comparison_df$elpd_diff[2]-comparison_df$se_diff[2]), (comparison_df$elpd_diff[2]+comparison_df$se_diff[2]))
    sig <- comparison_df$elpd_diff[1] >= max(interval) | comparison_df$elpd_diff[1] <= min(interval)
    return(sig)
  }else{return(NULL)}
}

dfa_noILVs <- map(comps_dfs_noILVs, get_diff_from_asoc) %>% setNames(., map_dbl(stn_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")
dfa_DistI <- map(comps_dfs_DistI, get_diff_from_asoc) %>% setNames(., map_dbl(stn_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")
dfa_DistIS <- map(comps_dfs_DistIS, get_diff_from_asoc) %>% setNames(., map_dbl(stn_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID") 
dfa_DistI_AgeIS <- map(comps_dfs_DistI_AgeIS, get_diff_from_asoc) %>% setNames(., map_dbl(stn_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")
dfa_DistIS_AgeIS <- map(comps_dfs_DistIS_AgeIS, get_diff_from_asoc) %>% setNames(., map_dbl(stn_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")

dfa <- purrr::list_rbind(list("noILVs" = dfa_noILVs, "DistI" = dfa_DistI, "DistIS" = dfa_DistIS, "DistI_AgeIS" = dfa_DistI_AgeIS, "DistIS_AgeIS" = dfa_DistIS_AgeIS), names_to = "model")
names(dfa)[3] <- "sig"

dfa_noILVs_wild <- map(comps_dfs_noILVs_wild, get_diff_from_asoc) %>% setNames(., map_dbl(wild_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")
dfa_DistI_wild <- map(comps_dfs_DistI_wild, get_diff_from_asoc) %>% setNames(., map_dbl(wild_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")
dfa_DistIS_wild <- map(comps_dfs_DistIS_wild, get_diff_from_asoc) %>% setNames(., map_dbl(wild_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID") 
dfa_DistI_AgeIS_wild <- map(comps_dfs_DistI_AgeIS_wild, get_diff_from_asoc) %>% setNames(., map_dbl(wild_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")
dfa_DistIS_AgeIS_wild <- map(comps_dfs_DistIS_AgeIS_wild, get_diff_from_asoc) %>% setNames(., map_dbl(wild_carcs, "carcID")) %>% map(., as.data.frame) %>% purrr::list_rbind(., names_to = "carcID")

dfa_wild <- purrr::list_rbind(list("noILVs" = dfa_noILVs_wild, "DistI" = dfa_DistI_wild, "DistIS" = dfa_DistIS_wild, "DistI_AgeIS" = dfa_DistI_AgeIS_wild, "DistIS_AgeIS" = dfa_DistIS_AgeIS_wild), names_to = "model")
names(dfa_wild)[3] <- "sig"

dfa %>%
  mutate(model = factor(model, levels = c("noILVs", "DistI", "DistIS", "DistI_AgeIS", "DistIS_AgeIS"))) %>%
  ggplot(aes(x = model, y = factor(carcID), fill = sig))+
  geom_tile()+
  scale_fill_manual(values = c("red", "skyblue"))+
  labs(y = NULL, x = "ILVs", fill = "Diff from\nasocial?")+
  theme_minimal() # interesting! some of the significance disappears, some appears

dfa_wild %>%
  mutate(model = factor(model, levels = c("noILVs", "DistI", "DistIS", "DistI_AgeIS", "DistIS_AgeIS"))) %>%
  ggplot(aes(x = model, y = factor(carcID), fill = sig))+
  geom_tile()+
  scale_fill_manual(values = c("red", "skyblue"))+
  labs(y = NULL, x = "ILVs", fill = "Diff from\nasocial?")+
  theme_minimal() # interesting! some of the significance disappears, some appears

ns <- map(data_lists, ~{
  found <- .x$N
  tot <- .x$P
  propfound <- found/tot
  return(data.frame(found = found, tot = tot, propfound = propfound))
})
names(ns) <- map_dbl(stn_carcs, "carcID")
nsdf <- purrr::list_rbind(ns, names_to = "carcID")

results <- data.frame(carcID = map_dbl(stn_carcs, "carcID"), pct_st = pct_st) %>%
  mutate(carcID = as.character(carcID)) %>%
  left_join(dfa, by = "carcID") %>%
  left_join(nsdf, by = "carcID") %>%
  rename("diff_from_asoc" = `.x[[i]]`) %>%
  mutate(carcID = as.numeric(carcID)) %>%
  left_join(purrr::list_rbind(stn_carcs), by = "carcID")

# sig and %ST by prop found
results %>%
  filter(diff_from_asoc) %>%
  ggplot(aes(x = propfound, y = pct_st))+
  geom_point(pch = 1, color = "blue")+
  geom_smooth(method = "lm", color = "blue")+
  theme_minimal()+
  labs(y = "%ST",
       x = "Prop. tagged vultures that found the carcass")+
  theme(text = element_text(size = 18))

results %>%
  filter(!is.na(diff_from_asoc)) %>%
  ggplot(aes(x = propfound, y = diff_from_asoc, color = diff_from_asoc))+
  geom_point(pch = 1)+
  theme_minimal()+
  labs(y = "ST detected?",
       x = "Prop. tagged vultures that found the carcass")+
  theme(text = element_text(size = 18),
        legend.position = "none")+
  scale_color_manual(values = c("red", "blue"))

# sig and %ST by n found
results %>%
  filter(diff_from_asoc) %>%
  ggplot(aes(x = found, y = pct_st))+
  geom_point(pch = 1, color = "blue")+
  geom_smooth(method = "lm", color = "blue")+
  theme_minimal()+
  labs(y = "%ST",
       x = "N tagged vultures that found the carcass")+
  theme(text = element_text(size = 18))

results %>%
  filter(!is.na(diff_from_asoc)) %>%
  ggplot(aes(x = found, y = diff_from_asoc, color = diff_from_asoc))+
  geom_point(pch = 1)+
  theme_minimal()+
  labs(y = "ST detected?",
       x = "N tagged vultures that found the carcass")+
  theme(text = element_text(size = 18),
        legend.position = "none")+
  scale_color_manual(values = c("red", "blue"))

# No relationship between number of vultures and social transmission probability. Yay!

results %>%
  filter(diff_from_asoc) %>%
  ggplot(aes(x = carcassWeight, y = pct_st))+
  geom_point(pch = 1, color = "blue")+
  theme_minimal()+
  geom_smooth(method = "lm", color = "blue")+
  labs(y = "%ST",
       x = "Carcass weight")+ # No relationship between carcass weight and %ST
  theme(text = element_text(size = 18))

# %ST by station (stations with >= 3 carcs)
results %>%
  group_by(stationName, diff_from_asoc) %>%
  summarize(n = n()) %>%
  ggplot(aes(fill=diff_from_asoc, y=n, x=stationName)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_manual(labels = c("FALSE", "TRUE", "No Data"), values = c("red", "blue", "gray"), name = "Diff from asoc?")+
  theme_minimal()+
  coord_flip()+
  labs(y = "Carcasses", x = NULL)

# Flight vs. roost generally
sms <- map(social_fits_DistI_2nets, ~{
  if(!is.null(.x)){
    return(STb_summary(.x))
  }else{NULL}})
sms_df <- purrr::list_rbind(sms, names_to = "idx")

sms_df %>%
  filter(grepl("percent_ST", Parameter)) %>%
  mutate(Parameter = case_when(Parameter == "percent_ST[1]" ~ "%ST_roost",
                               Parameter == "percent_ST[2]" ~ "%ST_flight")) %>%
  ggplot(aes(x = idx, y = Median, color = Parameter))+
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper, x = idx), width = 0, linewidth = 1, alpha = 0.5)+
  geom_point(pch = 21, fill = "white", size = 2)+
  theme_minimal()+
  coord_flip()+
  scale_color_manual(values = c("skyblue2", "darkgreen"))+
  labs(x = "Carcass (idx)",
       title = "Flight vs. roost, station carcs",
       y = "%ST estimate",
       color = "Network")+
  theme(text = element_text(size = 18))

# Are flight and roost networks correlated? It's hard to draw conclusions from the multi-network NBDA if they are.
library(vegan)
library(dplyr)
library(tidyr)
tar_load(networks_long_combined) # get the network data

make_matrix <- function(data, value_col) {
  mat <- data %>%
    select(focal, other, value = all_of(value_col)) %>%
    pivot_wider(names_from = other, values_from = value, values_fill = 0) %>%
    column_to_rownames("focal") %>%
    as.matrix()
  
  # Ensure square and symmetric with all individuals
  mat_full <- matrix(0, nrow = length(inds), ncol = length(inds),
                     dimnames = list(inds, inds))
  mat_full[rownames(mat), colnames(mat)] <- mat
  mat_sym <- pmax(mat_full, t(mat_full))  # symmetrise
  
  return(mat_sym)
}

mantel_by_time <- function(df, time_val) {
  
  d <- df %>% filter(time == time_val)
  
  # Get all unique individuals
  inds <- sort(unique(c(d$focal, d$other)))
  
  # Pivot each network variable to a matrix
  m1 <- make_matrix(d, "roost_together")
  m2 <- make_matrix(d, "flight_sri_scaled")
  
  # Check there's variance in both matrices -- Mantel fails if one is all zeros
  if (var(m1[lower.tri(m1)]) == 0 || var(m2[lower.tri(m2)]) == 0) {
    return(tibble(time = time_val, statistic = NA, p_value = NA, 
                  note = "no variance in one or both matrices"))
  }
  
  result <- mantel(m1, m2, method = "spearman", permutations = 999)
  
  tibble(
    time     = time_val,
    statistic = result$statistic,
    p_value   = result$signif,
    note      = "ok"
  )
}

your_data <- networks_long_combined[[1]]

# Run across all time steps
time_steps <- sort(unique(your_data$time))

results <- purrr::map_dfr(time_steps, ~ mantel_by_time(your_data, .x))
results %>%
  mutate(sig_corr = case_when(p_value >= 0.05 ~ F,
                              p_value < 0.05 ~ T, 
                              .default = NA)) %>%
  ggplot(aes(x = time, y = statistic, color = sig_corr))+
  geom_point()

# "The multi-network NBDA will work most effectively when the networks are independent. When they are highly dependent (e.g. correlated), it will require a lot of data to distinguish the effects of each network. This will be reflected in wide confidence intervals (CIs) for each s parameter, and for the estimated difference between them." Farine et al 2015

# Model averaging
inputs_stn <- list(
  m1 = social_fits_noILVs,
  m2 = social_fits_DistI,
  m3 = social_fits_DistI_AgeIS,
  m4 = social_fits_DistIS,
  m5 = social_fits_DistIS_AgeIS
  #,
  # ex1 = exclude_struct1,
  # ex2 = exclude_struct2,
  # ex3 = exclude_struct3,
  # ex4 = exclude_struct4
)

all_beta_names <- c("beta_ILVi_mean_dist_to_carcass_norm", 
                    "beta_ILVs_mean_dist_to_carcass_norm", 
                    "beta_ILVi_age[1]",
                    "beta_ILVs_age[1]",
                    "beta_ILVi_age[2]",
                    "beta_ILVs_age[2]")

model_averaged_params_stn <- pmap(inputs_stn, function(m1, m2, m3, m4, m5#, 
                                                       #ex1, ex2, ex3, ex4
) {
  
  all_models <- list(m1, m2, m3, m4, m5)
  #exclude <- c(ex1, ex2, ex3, ex4)
  models <- all_models#[!exclude]
  
  # return NULL if all models are NULL
  if (all(map_lgl(all_models, is.null))) return(NULL)
  
  # get beta names available in surviving models
  available_draws <- models[[1]]$draws() %>% as_draws_matrix()
  available_betas <- models %>%
    map(~colnames(.x$draws() %>% as_draws_matrix())) %>%
    reduce(union) %>%
    intersect(all_beta_names) # XXX START HERE 6/5
  
  weighted_average <- function(var) {
    draws <- map(models, ~.x$draws(var) %>% as_draws_matrix())
    reduce(
      seq_along(models),
      function(acc, j) acc + weights[j] * draws[[j]],
      .init = matrix(0, nrow = nrow(draws[[1]]), ncol = ncol(draws[[1]]))
    )
  }
  
  if (length(models) == 1) {
    weights <- NULL  # not needed but keeps structure consistent
    result <- map(all_beta_names, ~{
      if (.x %in% available_betas) {
        models[[1]]$draws(.x) %>% as_draws_matrix()
      } else {
        NA
      }
    }) %>% setNames(all_beta_names)
    return(c(list(percent_ST = models[[1]]$draws("percent_ST") %>% as_draws_matrix()), result))
  }
  
  loos <- map(models, ~suppressWarnings(.x$loo()))
  weights <- loo_model_weights(loos, method = "stacking")
  
  result <- map(all_beta_names, ~{
    if (.x %in% available_betas) {
      weighted_average(.x)
    } else {
      NA
    }
  }) %>% setNames(all_beta_names)
  
  c(list(percent_ST = weighted_average("percent_ST")), result)
})
