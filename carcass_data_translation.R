# Script to read in and aggregate the carcass data
library(tidyverse)
library(here)
library(readxl)
old <- read_excel(here("data/raw/translated/FeedingData from 2018_2024_Translated_9_25_2024 (1).xlsx")) %>%
  select("carcass_id" = ID, 
         "date" = `Date Event`, 
         "time" = `Event time`, 
         "itmLong" = `ITM - LONG`, 
         "itmLat" = `ITM - LAT`, 
         "long" = `WGS84 - LONG`, 
         "lat" = `WGS84 - LAT`, 
         "accuracy_m" = `Accuracy in meters (automatic)`, 
         "accuracy" = `Accuracy of reporting location`, 
         "reportTiming" = `The timing of the report`, 
         "waterFilled" = `Water filling`, 
         "stationName" = `name of raptor feeding station`, 
         "carcassType" = `Carcass type`,
         "carcassWeight" = `The weight of the food (kg)`,
         contains("Number of"),
         "newFood" = `new food`) %>%
  rename("nCows" = "number of  cow carcasses",
         "nDonkeys" = "Number of donkey carcasses",
         "nLambsKids" = "Number of baby sheep and goat carcasses",
         "nChickens" = "number of chicken carcasses",
         "nCalves" = "Number of calf carcasses",
         "nCamels" = "Number of camel carcasses",
         "nHorses" = "number of  horse carcasses",
         "nFish" = "Number of fish carcasses",
         "nTurkeys" = "Number of turkey carcasses",
         "nPigs" = "Number of pig carcasses",
         "nSheep" = "number of sheep carcasses") %>%
  select(-"number of warnings")
new <- read_excel(here("data/raw/translated/Feeding 2024 update - translated (1).xlsx")) %>%
  select("carcass_id" = Identifier, 
         date, time, 
         "itmLong" = `ITM - LONG`, 
         "itmLat" = `ITM - LAT`, 
         "long" = `WGS84 - LONG`, 
         "lat" = `WGS84 - LAT`, 
         "accuracy_m" = `Accuracy - automatic (meters)`, 
         "accuracy" = `accuracy`, 
         "reportTiming" = `timing of report`, 
         "waterFilled" = `was water filled?`, 
         "stationName" = `feeding station name - translated andshortened`, 
         "carcassType" = `type of carcass`, 
         "carcassWeight" = `Weight of carcass (Kg)`, contains("number of"), 
         "newFood" = `New food - translated`) %>%
  rename("nDonkeys" = "number of donkeys",
         "nLambsKids" = "number of lambs and kids", 
         "nChickens" = "number of chicken", 
         "nCalves" = "number of calves", 
         "nCamels" = "number of camels", 
         "nHorses" = "number of horses", 
         "nFish" = "number of fish", 
         "nTurkeys" = "number of turkeys",
         "nPigs" = "number of pigs", 
         "nSheep" = "number of sheep", 
         "nDeer" = "number of follow deer", 
         "nWildDonkeys" = "number of wild donkeys", 
         "nGazelles" = "number of gazels", 
         "nCows" = "number of cow carcasses") %>%
  select(-"number of carcases(automatic)")

names(new[!(names(new) %in% names(old))])
names(old[!(names(old) %in% names(new))])

carcasses_inpa <- bind_rows(old, new)

carcasses_inpa <- carcasses_inpa %>%
  mutate(datetime = lubridate::ymd_hms(paste0(as.character(lubridate::ymd(date)), substr(time, 12, 19))))

sort(unique(carcasses_inpa$stationName))

carcasses_inpa <- carcasses_inpa %>%
  mutate(cage = case_when(str_detect(stationName, "cage") ~ T,
                          .default = F))

carcasses_inpa_sf <- st_as_sf(carcasses_inpa, coords = c("long", "lat"), crs = "WGS84", remove = F) %>%
  st_transform(32636)

write_rds(carcasses_inpa, here("data/created/carcasses_inpa.RDS"))

# Now the feeding stations ------------------------------------------------
north <- readxl::read_excel(here("data/raw/feeding_stations_north.xlsx"), sheet = 2) %>%
  setNames(c("stationName", "region", "itmLong", "itmLat", "lat", "long"))
south <- readxl::read_excel(here("data/raw/feeding_station_south_coordinates.xlsx")) %>%
  setNames(c("stationName", "active", "itmLong", "itmLat", "lat", "long"))

stations <- bind_rows(north, south) 
stations$lat <- str_remove_all(stations$lat, "\\s")
stations$long <- str_remove_all(stations$long, "\\s")
stations <- stations %>%
  mutate(across(c("long", "lat"), as.numeric))

stations <- stations %>%
  st_as_sf(coords = c("long", "lat"), remove = F, crs = "WGS84") %>%
  st_transform(32636)

write_rds(stations, here("data/created/stations.RDS"))

fs <- sort(unique(stations$stationName))

mapview(carcasses_inpa_sf, label = "stationName", color = "blue", col.regions = "blue") +
  mapview(stations, label = "stationName", color = "red", col.regions = "red")

# Looking at this map shows us that 1) not all INPA carcasses are deposited at feeding stations (what's up with that? is this not a complete list of feeding stations?) # XXX add this to the report

# Correspondence in names based on overlapping dots
# 1. Ben_Yair_view = observation ben-Yair
# 2. Gorni_hill = Gorni hill
# 3. North_Golan = north golan
# 4. Hai_Bar_Carmel = Carmel - cage or Hai-bar feeding station
# Hever doesn't seem to be included on the total list of feeding stations
# 5. Amiaz = Other (one of the "Other"s)
# this is going to take forever--I need to show this to them in the report.

