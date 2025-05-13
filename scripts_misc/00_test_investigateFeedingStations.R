# Investigate correspondence between different feeding station definitions
library(tidyverse)
library(targets)
library(here)

tar_load(stations)
tar_load(stations_inferred)
tar_load(carcasses_inpa)

mapview(stations, col.regions = "black", label = "stationName", layer.name = "Feeding stations")+
  mapview(carcasses_inpa, col.regions = "blue", label = "stationName", layer.name = "INPA carcasses (2018-2024)")

# Examples of confusion
# smooth mountain (halak) is right next to south camus--correct?
# Large crater and Yemin_hatira stations have no carcasses at all
# Hever cage right next to Hever, but then there's another Hever point a little farther away
# Many of the blue carcasses have no nearby black point at all. Should I totally ignore the black points?
# Which of these are the same and which are not the same?
sort(unique(carcasses_inpa$stationName))
