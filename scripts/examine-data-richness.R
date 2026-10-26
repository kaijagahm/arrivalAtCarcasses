# Examining individuals over time to figure out which time periods are the most data-rich so we can run models on more station carcasses (issue #71)
library(targets)
library(tidyverse)
library(sf)

tar_load(downsampled_lf)
tar_load(bbox_south_big)
dslf <- st_crop(downsampled_lf, bbox_south_big)
hist(dslf$date_il, breaks = "days") # pretty decent coverage

dim(dslf)
dim(distinct(dslf, timestamp_il, individual_local_identifier)) # same

dslf %>%
  st_drop_geometry() %>%
  group_by(date_il) %>%
  summarize(n_indivs = length(unique(individual_local_identifier))) %>%
  ggplot(aes(x = date_il, y = n_indivs))+
  geom_line()+
  scale_x_date(breaks = "months") # number of tagged individuals varies but always stays above 60, which is pretty nice. Best times would be late 2022 or maybe february 2023 onward.

# Would probably also want to check the number of events for each carcass.
