library(tidyverse)
library(targets)
library(mapview)
library(sf)

tar_load(trajectories_sync)
tar_load(arrival_dyads)

# For now, let's work on just one carcass on one date
table(arrival_dyads$carcID) # 4885990
arrival_dyads_test <- arrival_dyads %>% filter(carcID == 4885990,
                                               timestamp_diff < 30*60) # arriving less than 30 minutes apart
trajectories_test <- trajectories_sync %>% filter(paste(id1, id2, date_il) %in% paste(arrival_dyads_test$id1, arrival_dyads_test$id2, arrival_dyads_test$date_il)) # test an example: 2024-05-02, A29w and A57w
#arrival_dyads_test %>% filter(id1 == "A29w", id2 == "A57w", date_il == "2024-05-02") # this comes up--good.

# arrival_dyads_test has each arrival dyad; trajectories_test has the flight paths for those dyads on those days.

joined <- left_join(trajectories_test, select(arrival_dyads_test, -c("carcass_date.x", "carcass_date.y", "date.x", "date.y", "carcass_date")), by = c("date_il", "id1", "id2"))

# THIS DATA STRUCTURE IS A HUGE PAIN IN THE ASS!!!!
