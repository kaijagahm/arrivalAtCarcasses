library(tidyverse)
library(sf)
library(targets)
r <- st_read("data/raw/roosts50_kde95_cutOffRegion.kml")
tar_load(stn_gps_forroosts)
tar_load(roosts_stn)
tar_load(stn_carcs)

# Based on Harel et al. paper
# depart within 2 min of each other (will need to amend to longer time window for this gps frequency)
# departure time = first GPS fix outside roost area
# long flights: daily displacement > 15km. Only considered these individuals to exclude the effects of local enhancement and direct sight
# exclude or ignore groups of larger size; focus only on dyads
# measured a few things: prop time spent within detection range of each other, mean distance during flight (how?? since gps points aren't synchronous), and whether or not the informed individual was leading the uninformed individual in space.

# how do we define departure?
# first flying point after sunrise?
# first flying point outside the roost polygon?
# something else?
# how did they do it? is their code available?

# Pick a carcass to work with
gps <- stn_gps_forroosts[[10]]
roosts <- roosts_stn[[10]]
carc <- stn_carcs[[10]]
mapview(carc, col.regions = "red") + mapview(gps, cex = 0.2, col.regions = "black")+mapview(r, col.regions = "purple")
carc$date
sort(unique(roosts$date))

roosts_day1_2 <- roosts %>% filter(roost_date == carc$date)
gps_day2 <- gps %>% filter(date_il == (carc$date + days(1)))
mapview(carc, col.regions = "red") + mapview(gps_day2, cex = 0.2, col.regions = "black")+mapview(r, col.regions = "purple") + mapview(roosts_day1_2, cex = 0.5, col.regions = "black")

# define when they've left the roost
indivs <- sort(unique(roosts_day1_2$individual_local_identifier))
# for(i in 1:length(indivs))){
  i <- 2
  indiv_roost <- roosts_day1_2 %>% filter(individual_local_identifier == indivs[i])
  indiv_gps <- gps %>% filter(individual_local_identifier == indivs[i]) %>% arrange(timestamp_il)
  dist_from_roost <- as.numeric(st_distance(indiv_gps, indiv_roost)) # this doesn't make sense.
  plot(dist_from_roost)
# }