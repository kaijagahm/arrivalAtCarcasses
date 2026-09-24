# Compare feeding bouts between high-frequency period and lower-frequency period
library(tidyverse)
library(sf)
library(mapview)
library(targets)

tar_load(feeding_bo_2023)
tar_load(feeding_bo_2023_test)

length(feeding_bo_2023)
length(feeding_bo_2023_test) # makes sense that these are the same length since they're still lists by individual

floor(map_dbl(feeding_bo_2023, nrow)/2) # dividing by two since this is a month of data vs. two weeks
map_dbl(feeding_bo_2023_test, nrow) # there we go!! some, just way fewer, which is to be expected.

tar_load(full_2023)
tar_load(full_2023_test)

bouts_2023 <- purrr::list_rbind(full_2023) %>%
  pivot_longer(cols = starts_with(".pred_"),
               names_to = "which_prob",
               values_to = "prob") %>%
  mutate(which_prob = str_remove(which_prob, ".pred_"))
bouts_2023_test <- purrr::list_rbind(full_2023_test) %>%
  pivot_longer(cols = starts_with(".pred_"),
               names_to = "which_prob",
               values_to = "prob") %>%
  mutate(which_prob = str_remove(which_prob, ".pred_"))

all <- bind_rows(bouts_2023 %>% mutate(period = "2023"),
                 bouts_2023_test %>% mutate(period = "2023_test"))

all %>%
  filter(which_prob == pred) %>%
  ggplot(aes(x = prob, color = pred, fill = pred))+
  geom_density()+
  facet_wrap(~period) # luckily, I don't see any systemic differences in the classification probabilities between these two datasets.

all %>%
  filter(which_prob == pred, pred == "Eating") %>%
  ggplot(aes(x = prob, color = pred, fill = pred))+
  geom_density()+
  facet_wrap(~period)+ 
  geom_vline(aes(xintercept = 0.5), color = "blue", linetype = 2) # okay, so with an 0.5 threshold, we should still be getting quite a few bouts, which lines up with what we see above.

# Okay, so we know that this method can successfully identify and localize feeding bouts, just fewer of them. That's good! Some options:

# 1. Downsample the high-frequency bouts to get a comparable rate. What would the rate even be?
bouts_2023 <- map(feeding_bo_2023, ~select(.x, device_id, .pred_Eating, start)) %>% purrr::list_rbind() %>% mutate(start_date = lubridate::date(start)) %>% select(-start) %>% mutate(when = "2023")

bouts_2023_test <- map(feeding_bo_2023_test, ~select(.x, device_id, .pred_Eating, start)) %>% purrr::list_rbind() %>% mutate(start_date = lubridate::date(start)) %>% select(-start) %>% mutate(when = "2023_test")

both <- bind_rows(bouts_2023, bouts_2023_test)

summ_by_individual <- both %>%
  group_by(when, device_id) %>%
  summarize(n = n()) %>%
  pivot_wider(id_cols = "device_id", names_from = "when", values_from = "n") %>%
  rename("hf" = `2023`,
         "lf" = `2023_test`) %>%
  mutate(floor_half_hf = floor(hf/2),
         ratio_lf_half = lf/floor_half_hf)

# What about spatial density?
# 1. Make three two-week rasters to look at the density of bouts over time. Normalize them. How similar? (I already suspect this is not going to work because the numbers are just so much lower, but who knows)

# XXX start here with this--some issues with joining
# I wonder if I could simply scale the number of bouts by the ACC frequency? Is it maybe that simple?
