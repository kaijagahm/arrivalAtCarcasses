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

write_csv(carcasses_inpa, here("data/created/carcasses_inpa.csv"))
