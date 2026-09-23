# Compare feeding bouts between high-frequency period and lower-frequency period
library(tidyverse)
library(sf)
library(mapview)
library(targets)

tar_load(feeding_bo_2023)
tar_load(feeding_bo_2023_test)

length(feeding_bo_2023)
length(feeding_bo_2023_test) # makes sense that these are the same length since they're still lists by individual

map_dbl(feeding_bo_2023, nrow)
map_dbl(feeding_bo_2023_test, nrow) # ah, we're not getting ANY feeding bouts here. What's going on?

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
  facet_wrap(~period)

all %>%
  filter(which_prob == pred, pred == "Eating") %>%
  ggplot(aes(x = prob, color = pred, fill = pred))+
  geom_density()+
  facet_wrap(~period) # huh, odd. With a 0.5 threshold, we still should have some eating ones. What's going on?

# Ah, the problem is that these bouts aren't localized.
getfeeding <- function(x, thresh){
  if(!is.null(x)){
    out <- filter(x, pred == "Eating" & !is.na(location_lat) & .pred_Eating > thresh)
  }else{
    out <- NULL
  }
  return(out)
}

getfeeding_nolocs <- function(x, thresh){
  if(!is.null(x)){
    out <- filter(x, pred == "Eating" & .pred_Eating > thresh)
  }else{
    out <- NULL
  }
  return(out)
}

test <- purrr::map(full_2023_test, ~getfeeding(.x, thresh = 0.5))
test_nolocs <- purrr::map(full_2023_test, ~getfeeding_nolocs(.x, thresh = 0.5))
map_dbl(test, nrow)
map_dbl(test_nolocs, nrow) # yeah, okay, so the problem is that these bouts are not getting matched with GPS points, not that there are no bouts at all.

# What proportion of them are getting matched in the original data?
prop_localized_2023 <- map(full_2023, ~{.x %>%
  group_by(device_id, pred) %>%
  summarize(prop_localized = mean(!is.na(location_lat)), .groups = "drop")},) %>%
  purrr::list_rbind()

prop_localized_2023_test <- map(full_2023_test, ~{.x %>%
    group_by(device_id, pred) %>%
    summarize(prop_localized = mean(!is.na(location_lat)), .groups = "drop")},) %>%
  purrr::list_rbind()
hist(prop_localized_2023_test$prop_localized) # yeah so none of this data is getting localized at all. what did I do wrong?

