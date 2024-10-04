# Script to read in and aggregate the carcass data
library(tidyverse)
library(here)
library(readxl)

old <- read_excel(here("data/raw/translated/FeedingData from 2018_2024_Translated_9_25_2024 (1).xlsx")) %>%
  select("carcID" = ID, 
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
  rename("n_Cows" = "number of  cow carcasses",
         "n_Donkeys" = "Number of donkey carcasses",
         "n_LambsKids" = "Number of baby sheep and goat carcasses",
         "n_Chickens" = "number of chicken carcasses",
         "n_Calves" = "Number of calf carcasses",
         "n_Camels" = "Number of camel carcasses",
         "n_Horses" = "number of  horse carcasses",
         "n_Fish" = "Number of fish carcasses",
         "n_Turkeys" = "Number of turkey carcasses",
         "n_Pigs" = "Number of pig carcasses",
         "n_Sheep" = "number of sheep carcasses") %>%
  select(-"number of warnings")
new <- read_excel(here("data/raw/translated/Feeding 2024 update - translated (1).xlsx")) %>%
  select("carcID" = Identifier, 
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
  rename("n_Donkeys" = "number of donkeys",
         "n_LambsKids" = "number of lambs and kids", 
         "n_Chickens" = "number of chicken", 
         "n_Calves" = "number of calves", 
         "n_Camels" = "number of camels", 
         "n_Horses" = "number of horses", 
         "n_Fish" = "number of fish", 
         "n_Turkeys" = "number of turkeys",
         "n_Pigs" = "number of pigs", 
         "n_Sheep" = "number of sheep", 
         "n_Deer" = "number of follow deer", 
         "n_WildDonkeys" = "number of wild donkeys", 
         "n_Gazelles" = "number of gazels", 
         "n_Cows" = "number of cow carcasses") %>%
  select(-"number of carcases(automatic)")

names(new[!(names(new) %in% names(old))])
names(old[!(names(old) %in% names(new))])

# Check for duplicates
any(old$carcID %in% new$carcID)
which(old$carcID %in% new$carcID)
old <- old[-which(old$carcID %in% new$carcID),]

carcasses_inpa <- bind_rows(old, new)

carcasses_inpa <- carcasses_inpa %>%
  mutate(datetime = lubridate::ymd_hms(paste0(as.character(lubridate::ymd(date)), substr(time, 12, 19))))

sort(unique(carcasses_inpa$stationName))

carcasses_inpa <- carcasses_inpa %>%
  mutate(cage = case_when(str_detect(stationName, "cage") ~ T,
                          .default = F))

carcasses_inpa <- st_as_sf(carcasses_inpa, coords = c("long", "lat"), crs = "WGS84", remove = F) %>%
  st_transform(32636)

carcasses_inpa %>% group_by(carcID) %>% filter(n() > 1) # 0 rows, good.

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

mapview(carcasses_inpa, label = "stationName", color = "blue", col.regions = "blue") +
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


# TEMPORARY: BECAUSE TARGETS PIPELINE WON'T RUN ---------------------------
# tar_load(data_files_2024)
# tar_load(data_files_2023)
# 
# unobs_raw_acc_2024 <- purrr::list_rbind(purrr::map(data_files_2024, ~as.data.frame(data.table::fread(.x, select = c("Latitude", "Longitude", "UTC_datetime", "UTC_date", "UTC_time", "datatype", "device_id", "acc_x", "acc_y", "acc_z")))))
# 
# unobs_raw_acc_2023 <- purrr::list_rbind(purrr::map(data_files_2023, ~as.data.frame(data.table::fread(.x, select = c("Latitude", "Longitude", "UTC_datetime", "UTC_date", "UTC_time", "datatype", "device_id", "acc_x", "acc_y", "acc_z")))))
# 
# readr::write_rds(unobs_raw_acc_2024, here("data/created/unobs_raw_acc_2024.RDS"))
# readr::write_rds(unobs_raw_acc_2023, here("data/created/unobs_raw_acc_2023.RDS"))

