# Defining predictability of feeding stations
# See "Predictability of food resources affects carcass finding" in Obsidian
# How do we define predictability?
# From Riotte-Lambert & Matthiopoulous 2020 TREE: "the value of an environmental variable (e.g., the abundance of a resource) is increasingly predictable at a given spatiotemporal scale if it is characterised by lower variability or higher correlation with itself or another environmental variable, measured at the given spatiotemporal scale."
library(tidyverse)
library(targets)
library(here)
library(sf)
library(mapview)
library(ggspatial)
library(gstat)
library(sp)

# Load in the carcass data
tar_load(all_carcasses) # These are the FOCAL carcasses that fall within the study period
tar_load(all_carcasses_forpred) # these are the carcass records, not all of which are included in the study, which we're using for predictability calculations.
all_carcasses_forpred <- all_carcasses_forpred %>%
  mutate(focal = carcID %in% all_carcasses$carcID)
# XXX start here
tar_load(bbox_south_big)
carcasses_south <- st_crop(all_carcasses, bbox_south_big)

# Simplify--keeping only dates, not datetimes, because sometimes there are multiple carcasses placed very close to each other in time
carcs <- carcasses_south %>%
  select(carcID, carcType, date, time, datetime, datetime_il, long, lat, stationName, carcassWeight, geometry, X, Y)

carcs_simple <- carcasses_south %>%
  select(carcID, carcType, date, stationName, year, carcassWeight) %>%
  distinct()
dim(carcs)
dim(carcs_simple)
table(carcs_simple$carcType) # 60 stn and 112 wild

stn <- carcs_simple %>%
  filter(!is.na(stationName))
dim(stn)

tar_load(minmax_dates)
stn %>%
  filter((date >= minmax_dates[[1]] & date <= minmax_dates[[2]]) | (date >= minmax_dates[[3]] & date <= minmax_dates[[4]]) | (date >= minmax_dates[[5]] & date <= minmax_dates[[6]])) %>%
  ggplot(aes(x = date, y = stationName, color = stationName))+
  geom_point(size = 3, alpha = 0.75)+
  theme_bw()+
  facet_wrap(~year, scales = "free_x")+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        text = element_text(size = 18),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12))+
  labs(y = "Feeding station", x = "Date")

# Wild carcasses
wild <- carcs_simple %>%
  filter(carcType == "wild")

mapview(wild, zcol = "year")
yearlist <- purrr::map(group_split(wild, year), sf::st_as_sf)

# Intervals
stn <- stn %>%
  arrange(stationName, date) %>%
  group_by(stationName) %>%
  mutate(interval = as.numeric(difftime(date, lag(date), units = "days")))

stn <- stn %>%
  filter(!is.na(interval))

# Probably what's relevant is only the 6-month period before each carcass.
carcs_buffered <-  st_buffer(carcs_simple, 8000) # 8km radius

# Carcasses last for 3 days
carcs_buffered <- carcs_buffered %>%
  mutate(end_date = date+lubridate::days(3)) %>%
  glimpse()
write_rds(carcs_buffered, file = "data/created/carcs_buffered.RDS")

all_carcasses_forpred <- all_carcasses_forpred %>%
  mutate(end_date = date + lubridate::days(3))

predictability_results <- carcs_buffered %>%
  mutate(
    window_start = date - lubridate::days(180),
    window_end   = date - lubridate::days(1) # up to the day before focal date
  ) %>%
  rowwise() %>%
  mutate(
    stats = list({
      # Spatial neighbors (shared by both calculations below)
      neighbor_indices <- sf::st_intersects(geometry, all_carcasses_forpred)[[1]]
      neighbors <- all_carcasses_forpred[neighbor_indices, ]
      
      ## --- 1. Proportion of days covered (unchanged logic) ---
      overlapping_neighbors <- neighbors %>%
        filter(date <= window_end & end_date >= window_start)
      
      if (nrow(overlapping_neighbors) == 0) {
        prop_days_covered <- 0
      } else {
        covered_days <- overlapping_neighbors %>%
          mutate(
            clipped_start = pmax(date, window_start),
            clipped_end   = pmin(end_date, window_end)
          ) %>%
          mutate(days_seq = map2(clipped_start, clipped_end, ~seq(.x, .y, by = "day"))) %>%
          pull(days_seq) %>%
          flatten() %>%
          unique()
        
        total_window_days <- as.numeric(window_end - window_start) + 1
        prop_days_covered <- length(covered_days) / total_window_days
      }
      
      ## --- 2. Inter-carcass intervals within the buffer/window ---
      # Use the raw deposition dates (not the 3-day end_date span) of every
      # carcass event within 4km, in the 180 days before the focal carcass.
      carcass_dates <- neighbors %>%
        filter(date >= window_start & date <= window_end) %>%
        pull(date) %>%
        sort()
      
      n_events <- length(carcass_dates)
      
      if (n_events < 2) {
        sd_interval_days  <- NA_real_
        var_interval_days <- NA_real_
      } else {
        intervals <- as.numeric(diff(carcass_dates)) # consecutive gaps in days
        sd_interval_days  <- sd(intervals)
        var_interval_days <- var(intervals)
      }
      
      tibble::tibble(
        prop_days_covered     = prop_days_covered,
        n_carcasses_in_window = n_events,
        sd_interval_days      = sd_interval_days,
        var_interval_days     = var_interval_days
      )
    })
  ) %>%
  ungroup() %>%
  tidyr::unnest(stats)

predictability_results %>%
  ggplot(aes(x = prop_days_covered, y = sd_interval_days))+
  geom_point(aes(color = carcType))+
  theme_minimal() # this is interesting! Super different from before, which is appropriate.

# 9/12/26 To come back to: let's investigate whether there are problems caused by only having records of station carcasses from before the study periods, not wild carcasses. Might have to define both in terms of station carcasses only for predictability.

# Calculating temporal autocorrelation ------------------------------------
neighbor_list_4km <- sf::st_is_within_distance(all_carcasses, all_carcasses_forpred, dist = 4000)

neighbor_list_8km <- sf::st_is_within_distance(all_carcasses, all_carcasses_forpred, dist = 8000)

neighbor_list_16km <- sf::st_is_within_distance(all_carcasses, all_carcasses_forpred, dist = 16000)

# This is outdated but I'm leaving it here because it seems that we might have some wild carcasses that are too close together.
# # check_neighbors <- function(i, all_carcasses, dist = 4000) {
# #   focal_date   <- all_carcasses$date[i]
# #   window_start <- focal_date - lubridate::days(180)
# #   window_end   <- focal_date - lubridate::days(1)
# #   
# #   idx <- sf::st_is_within_distance(all_carcasses[i, ], all_carcasses, dist = dist)[[1]]
# #   neighbors <- all_carcasses[idx, ] %>%
# #     sf::st_drop_geometry() %>%
# #     dplyr::filter(date + lubridate::days(2) >= window_start, date <= window_end) %>%
# #     dplyr::arrange(date)
# #   
# #   neighbors
# # } # check synchronized carcasses at the landscape-scale
# # check_neighbors(193, all_carcasses, dist = 4000) # shows that there actually are multiple carcasses at the same time
# test <- check_neighbors(192, all_carcasses, dist = 4000)
# st_as_sf(test, coords = c("X", "Y"), crs = 32636) %>% mapview()
# # ugh, these are too close together! Maybe need to revisit wild carcass definitions again. That's super frustrating.

# Anyway, let's carry on for now.
get_activity_vectors <- function(neighbor_list){
  out <- purrr::imap(neighbor_list, function(idx, i) {
    focal_date   <- as.Date(all_carcasses$date[i])
    window_start <- focal_date - lubridate::days(180)
    window_end   <- focal_date - lubridate::days(1)
    window_len   <- as.integer(window_end - window_start) + 1L  # 180
    
    neighbors <- all_carcasses_forpred[idx, ]
    
    act_start <- as.Date(neighbors$date)
    act_end   <- act_start + lubridate::days(2)  # active for 3 days: date, date+1, date+2
    
    # keep only carcasses whose active window overlaps the 180-day lookback
    keep <- act_start <= window_end & act_end >= window_start
    act_start <- act_start[keep]
    act_end   <- act_end[keep]
    
    if (length(act_start) == 0) return(rep(0L, window_len))
    
    # clip active intervals to the lookback window, convert to day-offsets
    clipped_start <- pmax(act_start, window_start)
    clipped_end   <- pmin(act_end, window_end)
    start_idx <- as.integer(clipped_start - window_start) + 1L
    end_idx   <- as.integer(clipped_end   - window_start) + 1L
    
    # diff-array: +1 at each interval start, -1 just after each interval end,
    # then cumsum gives the running count — no per-day loops, no seq()
    delta <- tabulate(start_idx, nbins = window_len + 1L) -
      tabulate(end_idx + 1L, nbins = window_len + 1L)
    
    as.integer(cumsum(delta[1:window_len]))
  })
  return(out)
}
vectors_4km <- get_activity_vectors(neighbor_list_4km)
all(map_lgl(vectors_4km, ~all(.x>=0))) # TRUE
vectors_8km <- get_activity_vectors(neighbor_list_8km)
all(map_lgl(vectors_8km, ~all(.x>=0))) # TRUE
vectors_16km <- get_activity_vectors(neighbor_list_16km)
all(map_lgl(vectors_16km, ~all(.x>=0))) # TRUE
glimpse(acf(vectors_4km[[1]]))

acfs_lag1_4km <- purrr::map_dbl(vectors_4km, ~{if(sum(.x)>0){acf(.x)$acf[2]}else{NA}})
acfs_lag1_8km <- purrr::map_dbl(vectors_8km, ~{if(sum(.x)>0){acf(.x)$acf[2]}else{NA}})
acfs_lag1_16km <- purrr::map_dbl(vectors_16km, ~{if(sum(.x)>0){acf(.x)$acf[2]}else{NA}})

# Okay, so these produce reasonable outputs. Would need to decide which lag to use if I wanted this to be useful at all.

# Can't use Colwell's P because we don't have multiple cycles over the same types of periods. Could measure just constancy, but I don't think that's what we're interested in--doesn't distinguish ordering.

# New approach, after talking to Will--semivariograms. -------------------------------------
# Use gstat package to calculate semivariograms, using time instead of space

dfs8_plain <- map(vectors_8km, ~data.frame(day = 1:length(.x),
                                           dummy = 0,
                                           n = .x))

dfs_8 <- dfs8_plain

for(i in 1:length(dfs_8)){
  coordinates(dfs_8[[i]]) <- ~day + dummy
}
str(dfs_8[[1]])

vgm_emps <- map(dfs_8, ~variogram(n ~ 1, data = .x)) # Empirical semivariogram: semivariance vs. time lag (in days)

plot(vgm_emps[[1]])

vgm_fits <- map(vgm_emps, ~fit.variogram(.x, vgm("Exp")))

plot(vgm_emps[[1]], vgm_fits[[1]])
plot(vgm_emps[[8]], vgm_fits[[8]])
plot(vgm_emps[[66]], vgm_fits[[66]])
plot(vgm_emps[[100]], vgm_fits[[100]])

sills <- map_dbl(vgm_fits, ~.x$psill[2])
ranges <- map_dbl(vgm_fits, ~.x$range[2])
errs <- map_dbl(vgm_fits, ~attr(.x, "SSErr"))

ac <- all_carcasses %>%
  mutate(sill = sills, range = ranges, err = errs)

ac %>%
  #filter(range < 2000, sill < 2) %>%
  ggplot(aes(x = range, y = sill, color = carcType))+
  geom_point(pch = 1, size = 1.5, alpha = 0.9)+
  theme_minimal()

# This is fascinating! So, range = temporal persistence, and sill = magnitude of temporal variation. We have some with very high temporal persistence and also high temporal variation, but the vast majority of them have low temporal persistence and low variation.

# Let's investigate those high outliers some more.
which(ranges > 100)
plot(vgm_emps[[42]], vgm_fits[[42]])
plot(vgm_emps[[43]], vgm_fits[[43]])
plot(vgm_emps[[46]], vgm_fits[[46]])
plot(vgm_emps[[47]], vgm_fits[[47]])

plot(vgm_emps[[56]], vgm_fits[[56]])
plot(vgm_emps[[57]], vgm_fits[[57]])
plot(vgm_emps[[58]], vgm_fits[[58]])
plot(vgm_emps[[59]], vgm_fits[[59]])
plot(vgm_emps[[60]], vgm_fits[[60]])
plot(vgm_emps[[61]], vgm_fits[[61]])

plot(vgm_emps[[76]], vgm_fits[[76]])
plot(vgm_emps[[77]], vgm_fits[[77]])
plot(vgm_emps[[78]], vgm_fits[[78]])
plot(vgm_emps[[79]], vgm_fits[[79]])
plot(vgm_emps[[80]], vgm_fits[[80]])
plot(vgm_emps[[81]], vgm_fits[[81]])
plot(vgm_emps[[82]], vgm_fits[[82]])

plot(vgm_emps[[99]], vgm_fits[[99]])

plot(vgm_emps[[114]], vgm_fits[[114]])
plot(vgm_emps[[115]], vgm_fits[[115]])
plot(vgm_emps[[161]], vgm_fits[[161]])
plot(vgm_emps[[172]], vgm_fits[[172]]) # these probably reflect change over time.

# Tried taking the residuals and it didn't really help.
# Could it be due to a similar pattern--a bunch of zeroes and then a few carcasses?
vectors_8km[[56]] # yeah, nothing and then some stuff toward the end. A new station becoming active, perhaps?
vectors_8km[[57]] # same pattern
vectors_8km[[114]] # almost identical.

# Okay, so I'm not sure what to do about these. 
# For now, let's look more at the non-outlier values
ac %>%
  filter(range < 100, sill < 10) %>%
  ggplot(aes(x = range, y = sill, color = carcType))+
  geom_point(pch = 1, size = 1.5, alpha = 0.9)+
  theme_minimal() # after removing the outliers, things look a bit more reasonable!

# Is there a way to categorize the big outliers numerically?
ac %>%
  ggplot(aes(x = range, y = sill, color = err))+
  geom_point(pch = 1, size = 1.5, alpha = 0.9)+
  theme_minimal()+
  scale_color_viridis_c() # welp, error doesn't help

# So I guess for now I'm just going to exclude those carcasses? Not sure what else to do.

# There's a lot of variation along both sill and range, so I will probably need/want to use both.

# Let me look at which stations these are and see if it makes sense.

ac %>%
  filter(range < 100, sill < 10) %>%
  ggplot(aes(x = range, y = sill, color = stationName))+
  geom_point(pch = 1, size = 1.5, alpha = 0.9)+
  theme_minimal()+
  facet_wrap(~carcType) # we definitely have a bunch of points from Hever clustering weirdly, but a lot of other stuff looks pretty continuous. And interestingly, the wild carcasses don't have a super different distribution than the station ones!

ac %>%
  filter(range < 100, sill < 10) %>%
  ggplot(aes(x = factor(year), y = range, fill = carcType, color = carcType))+
  geom_violin(alpha = 0.5)+
  theme_minimal()+
  labs(y = "Temporal persistence (variogram range)", x = "Year", color = "Type", fill = "Type") # roughly equivalent persistence between station and wild carcasses

ac %>%
  filter(range < 100, sill < 10) %>%
  ggplot(aes(x = factor(year), y = sill, fill = carcType, color = carcType))+
  geom_violin(alpha = 0.5)+
  theme_minimal()+
  labs(y = "Temporal variation (variogram sill)", x = "Year", color = "Type", fill = "Type") # interesting! The station carcasses are actually more variable in general than the wild carcasses.

### START HERE 9/12/26

ac %>%
  filter(!is.na(stationName)) %>%
  #filter(stationName %in% c("Hever", "Kachal", "Tzaror_mount")) %>%
  filter(range < 2000, sill < 2) %>%
  ggplot(aes(x = range, y = sill, color = stationName))+
  geom_point(pch = 1, size = 3, alpha = 0.9)+
  theme_minimal() # after we filter out Gamla, it looks like that reduces us down to some that appear to be Hever, Kachal, and Tzaror_mount or something similar.

# What's interesting, though, is that these stations sometimes fall in the less persistent category. Let me look at this by time instead.

ac %>%
  filter(!is.na(stationName)) %>%
  filter(range < 2000, sill < 2) %>%
  ggplot(aes(x = range, y = sill, color = factor(year)))+
  geom_point(pch = 1, size = 3, alpha = 0.9)+
  theme_minimal()  # all the really persistent ones are from 2024. Did they just place more carcasses then?

which(ranges > 200 & ranges < 2000)
plot(vgm_emps[[42]], vgm_fits[[42]]) # looks like this one doesn't really level off, which would indicate that the pattern isn't stationary--varying over time. Maybe need to take variogram of the residuals? But maybe not, since we do care about what's been happening over time.

# Examining predictability results ----------------------------------------
predictability_results %>%
  mutate(carcType = case_when(carcType == "stn" ~ "SFS",
                              carcType == "wild" ~ "Non-SFS",
                              .default = NA)) %>%
  ggplot(aes(x = factor(year), y = prop_days_covered, fill = carcType))+
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.75), alpha = 0.2)+
  geom_point(aes(color = carcType, x = factor(year)), position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.2), pch = 1, alpha = 0.7, size = 3)+
  labs(y = "Predictability",
       x = "Year", fill = "Carcass type", color = "Carcass type",
       caption = "Predictability: % days in last 6mos with at least 1 active carcass within 4km.\nActive carcass: within 3 days of placement/discovery")+
  theme_minimal()+
  scale_fill_manual(values = c("darkorange3", "olivedrab3"))+
  scale_color_manual(values = c("darkorange3", "olivedrab3"))+
  theme(text = element_text(size = 18),
        plot.caption = element_text(size = 14))

predictability_results %>%
  mutate(carcType = case_when(carcType == "stn" ~ "SFS",
                              carcType == "wild" ~ "Non-SFS",
                              .default = NA)) %>%
  ggplot(aes(x = prop_days_covered, fill = carcType, color = carcType))+
  geom_density(alpha = 0.2)+
  labs(x = "Predictability", y = "Density",
       fill = "Carcass type", color = "Carcass type",
       caption = "Predictability: % days in last 6mos with at least 1 active carcass within 4km.\nActive carcass: within 3 days of placement/discovery")+
  theme_minimal()+
  scale_fill_manual(values = c("darkorange3", "olivedrab3"))+
  scale_color_manual(values = c("darkorange3", "olivedrab3"))+
  theme(text = element_text(size = 18),
        plot.caption = element_text(size = 14))

predictability_results %>%
  mutate("Predictability" = prop_days_covered) %>%
  mutate(carcType = case_when(carcType == "stn" ~ "SFS",
                              carcType == "wild" ~ "Non-SFS",
                              .default = NA)) %>%
  ggplot()+
  annotation_map_tile(zoom = 9, type = "cartolight")+
  geom_sf(aes(fill = Predictability, color = Predictability), alpha = 0.4)+
  scale_fill_viridis_c()+
  scale_color_viridis_c()+
  theme_minimal()+
  theme(text = element_text(size = 18))

saveRDS(predictability_results, file = "data/created/predictability_results.RDS")
