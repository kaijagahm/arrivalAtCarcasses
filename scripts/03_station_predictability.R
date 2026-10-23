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

tar_load(bbox_south_big)
carcasses_south <- st_crop(all_carcasses, bbox_south_big)

# Simplify--keeping only dates, not datetimes, because sometimes there are multiple carcasses placed very close to each other in time
carcs <- carcasses_south %>%
  select(carcID, carcType, date, time, datetime, datetime_il, long, lat, stationName, carcassWeight, geometry, X, Y)

carcs_simple <- carcasses_south %>%
  select(carcID, carcType, date, stationName, year, carcassWeight) %>%
  distinct()
table(carcs_simple$carcType) # 60 stn and 110 wild

stn <- carcs_simple %>%
  filter(!is.na(stationName))

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
  labs(y = "Feeding station", x = "Date") # we can see the variation in predictability here

# Wild carcasses
wild <- carcs_simple %>%
  filter(carcType == "wild")

mapview(wild, zcol = "year")
yearlist <- purrr::map(group_split(wild, year), sf::st_as_sf)

# Carcasses last for 3 days
carcs_simple <- carcs_simple %>%
  mutate(end_date = date+lubridate::days(3)) %>%
  glimpse()
write_rds(carcs_simple, file = "data/created/carcs_simple.RDS")

all_carcasses_forpred <- all_carcasses_forpred %>%
  mutate(end_date = date + lubridate::days(3))

# 9/12/26 To come back to: let's investigate whether there are problems caused by only having records of station carcasses from before the study periods, not wild carcasses. Might have to define both in terms of station carcasses only for predictability.

# Calculating temporal autocorrelation ------------------------------------
## Station carcasses only:
neighbor_list_4km <- sf::st_is_within_distance(filter(carcs_simple, carcType == "stn"), filter(all_carcasses_forpred, carcType == "stn"), dist = 4000)

# Anyway, let's carry on for now.
get_activity_vectors <- function(neighbor_list, days_back = 180, days_active = 3, start_date = NULL, end_date = NULL){
  out <- purrr::imap(neighbor_list, function(idx, i) {
    focal_date   <- as.Date(all_carcasses$date[i])
    if(!is.null(start_date)){
      window_start <- lubridate::ymd(start_date)
    }else{window_start <- focal_date - lubridate::days(days_back)}
    if(!is.null(end_date)){
      window_end <- lubridate::ymd(end_date)
    }else{window_end<- focal_date - lubridate::days(1)}
    
    window_len   <- as.integer(window_end - window_start) + 1L  # 180
    
    neighbors <- all_carcasses_forpred[idx, ]
    
    act_start <- as.Date(neighbors$date)
    act_end   <- act_start + lubridate::days(days_active-1)  # active for 3 days: date, date+1, date+2
    
    # keep only carcasses whose active window overlaps th lookback
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

yrs <- carcs_simple$year[carcs_simple$carcType == "stn"]
ids <- carcs_simple$carcID[carcs_simple$carcType == "stn"]
vectors_4km_6mos <- get_activity_vectors(neighbor_list_4km) %>% setNames(., nm = ids)
vectors_4km_hfonly_2022 <- get_activity_vectors(neighbor_list_4km[yrs == 2022], start_date = lubridate::date(minmax_dates[[1]]), end_date = lubridate::date(minmax_dates[[2]])) %>% setNames(., nm = ids[yrs == 2022])
vectors_4km_hfonly_2023 <- get_activity_vectors(neighbor_list_4km[yrs == 2023], start_date = lubridate::date(minmax_dates[[3]]), end_date = lubridate::date(minmax_dates[[4]])) %>% setNames(., nm = ids[yrs == 2023])
vectors_4km_hfonly_2024 <- get_activity_vectors(neighbor_list_4km[yrs == 2024], start_date = lubridate::date(minmax_dates[[5]]), end_date = lubridate::date(minmax_dates[[6]])) %>% setNames(., nm = ids[yrs == 2024])
vectors_4km_hfonly <- c(vectors_4km_hfonly_2022, vectors_4km_hfonly_2023, vectors_4km_hfonly_2024)
all(names(vectors_4km_6mos) == names(vectors_4km_hfonly)) # TRUE


# Can't use Colwell's P because we don't have multiple cycles over the same types of periods. Could measure just constancy, but I don't think that's what we're interested in--doesn't distinguish ordering.

# New approach, after talking to Will--semivariograms. -------------------------------------
# Use gstat package to calculate semivariograms, using time instead of space

dfs4_6mos <- map(vectors_4km_6mos, ~{
  out <- data.frame(day = 1:length(.x),
             dummy = 0,
             n = .x)
  coordinates(out) <- ~day+dummy
  return(out)
})

dfs4_hfonly <- map(vectors_4km_hfonly, ~{
  out <- data.frame(day = 1:length(.x),
                    dummy = 0,
                    n = .x)
  coordinates(out) <- ~day+dummy
  return(out)
})

vgm_emps_6mos <- map(dfs4_6mos, ~variogram(n ~ 1, data = .x)) # Empirical semivariogram: semivariance vs. time lag (in days)
vgm_emps_hfonly <- map(dfs4_hfonly, ~variogram(n ~ 1, data = .x))

vgm_fits_6mos <- map(vgm_emps_6mos, ~fit.variogram(.x, vgm("Exp")))
vgm_fits_hfonly <- map(vgm_emps_hfonly, ~fit.variogram(.x, vgm("Exp")))

plot(vgm_emps_6mos[[1]], vgm_fits_6mos[[1]])
plot(vgm_emps_6mos[[8]], vgm_fits_6mos[[8]])
plot(vgm_emps_6mos[[56]], vgm_fits_6mos[[56]]) # some of them are bad

sills_6mos <- map_dbl(vgm_fits_6mos, ~.x$psill[2])
ranges_6mos <- map_dbl(vgm_fits_6mos, ~.x$range[2])
errs_6mos <- map_dbl(vgm_fits_6mos, ~attr(.x, "SSErr"))
sills_hfonly <- map_dbl(vgm_fits_hfonly, ~.x$psill[2])
ranges_hfonly <- map_dbl(vgm_fits_hfonly, ~.x$range[2])
errs_hfonly <- map_dbl(vgm_fits_hfonly, ~attr(.x, "SSErr"))

ac <- filter(carcs_simple, carcType == "stn") %>%
  mutate(sill_6mos = sills_6mos, range_6mos = ranges_6mos, err_6mos = errs_6mos,
         sill_hfonly = sills_hfonly, range_hfonly = ranges_hfonly, err_hfonly = errs_hfonly)

ac %>%
  #filter(range < 1000, sill < 2) %>%
  ggplot(aes(x = range_6mos, y = sill_6mos))+
  geom_point(pch = 1, size = 1.5, alpha = 0.9)+
  theme_minimal()  # just a few outliers!

ac %>%
  filter(range_6mos < 600) %>%
  ggplot(aes(x = sill_6mos))+
  geom_histogram(fill = "skyblue", color = "black", breaks = seq(from = 0, to = 1.4, by = 0.2)) + theme_bw()+
  labs(x = "Sill: Temporal variation (units ??)", y = "Count", title = "6mo back, stn only")+
  theme(text = element_text(size = 18))+
  facet_wrap(~year)

ac %>%
  filter(range_6mos < 600) %>%
  ggplot(aes(x = sill_hfonly))+
  geom_histogram(fill = "skyblue", color = "black", breaks = seq(from = 0, to = 1.4, by = 0.2)) + theme_bw()+
  labs(x = "Sill: Temporal variation (units ??)", y = "Count", title = "HF period, stn only")+
  theme(text = element_text(size = 18))+
  facet_wrap(~year)

ac %>%
  filter(range_6mos < 20) %>%
  ggplot(aes(x = range_6mos))+
  geom_histogram(fill = "brown", color = "black", breaks = seq(from = 1, to = 10, by = 0.5)) + theme_bw()+
  labs(x = "Range: Temporal persistence (days)", y = "Count", title = "6mo back, stn only")+
  theme(text = element_text(size = 18))+
  facet_wrap(~factor(year))

ac %>%
  filter(range_6mos < 600) %>%
  ggplot(aes(x = range_hfonly))+
  geom_histogram(fill = "brown", color = "black", breaks = seq(from = 1, to = 10, by = 0.5)) + theme_bw()+
  labs(x = "Range: Temporal persistence (days)", y = "Count", title = "HF period, stn only")+
  theme(text = element_text(size = 18))+
  facet_wrap(~factor(year))+
  scale_y_continuous(breaks = 1:9)

# Range = temporal persistence, and sill = magnitude of temporal variation. We have some with very high temporal persistence and also high temporal variation, but the vast majority of them have low temporal persistence. The main axis of variability seems to be temporal variation (sill)

# Is there a way to categorize the big outliers numerically?
ac %>%
  ggplot(aes(x = range_6mos, y = sill_6mos, color = err_6mos))+
  geom_point(size = 3, alpha = 0.9)+
  theme_minimal()+
  scale_color_viridis_c() # the outliers do have much higher error than the others! Why?

ac %>%
  ggplot(aes(x = range_hfonly, y = sill_hfonly, color = err_hfonly))+
  geom_point(size = 3, alpha = 0.9)+
  theme_minimal()+
  scale_color_viridis_c()

# Now let's compare the 1-month periods to the 6-month periods and see how good of a proxy the 1-month periods would be.

ac %>%
  arrange(date) %>%
  group_by(year) %>%
  mutate(idx = 1:n()) %>%
  filter(range_6mos < 500) %>% 
  ggplot(aes(x = sill_hfonly, y = sill_6mos, color = idx))+
  geom_smooth(method = "lm", color = "black", alpha = 0.2, lwd = 0.5)+
  geom_point(size = 3, alpha = 0.7)+
  theme_minimal()+
  scale_color_viridis_c()+
  facet_wrap(~factor(year), scales = "free_x")+
  labs(y = "Sill (variation), 6mo back, stn only",
       x = "Sill (variation), HF period, stn only", color = "Index\nwithin\nperiod")+
  theme_bw()+
  theme(text = element_text(size = 18))
# This is a decent proxy, actually (positive slope, fairly tight standard error)
# No clear relationship between position in the time period and goodness of the proxy
# Somewhat different year to year but not drastically

# What about stationName?
ac %>%
  arrange(date) %>%
  group_by(year) %>%
  mutate(idx = 1:n()) %>%
  filter(range_6mos < 500) %>% 
  ggplot(aes(x = sill_hfonly, y = sill_6mos, color = stationName))+
  geom_smooth(method = "lm", color = "black", alpha = 0.2, lwd = 0.5)+
  geom_point(size = 3, alpha = 0.7)+
  theme_minimal()+
  scale_color_viridis_d(option = "B")+
  facet_wrap(~factor(year), scales = "free_x")+
  labs(y = "Sill (variation), 6mo back, stn only",
       x = "Sill (variation), HF period, stn only", color = "Station")+
  theme_bw()+
  theme(text = element_text(size = 18))

outliers_removed <- ac %>% filter(range_6mos < 500)
mod_sill <- lm(sill_6mos ~ sill_hfonly, data = outliers_removed)
summary(mod_sill) # R^2 is only 0.38, but I guess that's not the worst I've ever seen
cor_sill <- cor(outliers_removed$sill_6mos, outliers_removed$sill_hfonly) # cor = 0.63

ac %>%
  arrange(date) %>%
  group_by(year) %>%
  mutate(idx = 1:n()) %>%
  filter(range_6mos < 500) %>% 
  ggplot(aes(x = range_hfonly, y = range_6mos, color = stationName))+
  geom_smooth(method = "lm", color = "black", alpha = 0.2, lwd = 0.5)+
  geom_point(size = 3, alpha = 0.7)+
  theme_minimal()+
  scale_color_viridis_d(option = "B")+
  facet_wrap(~factor(year), scales = "free_x")+
  labs(y = "Range (persistence), 6mo back, stn only",
       x = "Range (persistence), HF period, stn only", color = "Station")+
  theme_bw()+
  theme(text = element_text(size = 18)) # really bad proxy for this
mod_range <- lm(range_6mos ~ range_hfonly, data = outliers_removed)
summary(mod_range) # R^2 is basically 0
cor_range <- cor(outliers_removed$range_6mos, outliers_removed$range_hfonly) 
cor_range # cor = -0.11. Absolutely cannot use hf range as a proxy for 6mo range.
