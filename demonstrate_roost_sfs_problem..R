# Demonstration of potential problem with roost sites vs. feeding stations
library(tidyverse)
library(targets)
library(sf)
library(mapview)

tar_load(rp) # roost polygons
tar_load(nd1) # pull NBDA data object so we can access the carcass data for this individual
tar_load(stn_carcs) # going to focus on carcass 1, which is carcID 4202095
cumul <- nd1[[1]]$gps_data_cumulative # pulling the GPS data for that one carcass

# from looking at the saved files: network 36 looks reasonable. 37-42 are empty. 43 is normal again.
mapview(cumul[[36]], col.regions = "blue") + mapview(stn_carcs[[1]], col.regions = "red") + mapview(rp, col.regions = "green")

mapview(cumul[[37]], col.regions = "blue") + mapview(stn_carcs[[1]], col.regions = "red") + mapview(rp, col.regions = "green")

mapview(cumul[[38]], col.regions = "blue") + mapview(stn_carcs[[1]], col.regions = "red") + mapview(rp, col.regions = "green")

mapview(cumul[[39]], col.regions = "blue") + mapview(stn_carcs[[1]], col.regions = "red") + mapview(rp, col.regions = "green")

# We can see that a bunch of points are falling in the roost polygon. They are, correctly, being filtered out and therefore not registering as flight interactions. But this raises the question of whether it's a problem that the carcass itself falls into the roost polygon, since all instances of individuals arriving at the carcass will then take place inside the roost polygon.