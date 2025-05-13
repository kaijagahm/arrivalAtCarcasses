# Script to read in and aggregate the carcass data
library(here)
library(readxl)
library(tidyverse)
library(sf)
library(mapview)
source(here("R/functions.R"))

old <- read_excel(here("data/raw/translated/FeedingData from 2018_2024_Translated_9_25_2024 (1).xlsx")) %>%
  dplyr::select("carcID" = ID, 
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
  dplyr::select(-"number of warnings")
new <- read_excel(here("data/raw/translated/Feeding 2024 update - translated (1).xlsx")) %>%
  dplyr::select("carcID" = Identifier, 
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
  dplyr::select(-"number of carcases(automatic)")

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
new <- readxl::read_excel(here("data/raw/FeedingStations2work.xlsx"), sheet = 2) %>% mutate(type = "new") %>%
  rename("itmLong" = X, "itmLat" = Y, "long" = lon, "stationName" = name, "region" = area)

stations <- bind_rows(north, south) 
stations$lat <- str_remove_all(stations$lat, "\\s")
stations$long <- str_remove_all(stations$long, "\\s")
stations <- stations %>%
  mutate(across(c("long", "lat"), as.numeric)) %>%
  bind_rows(new)

stations <- stations %>%
  st_as_sf(coords = c("long", "lat"), remove = F, crs = "WGS84") %>%
  st_transform(32636)

## Deduplicate and keep the new ones
stations <- stations %>%
  arrange(stationName, type) %>%
  group_by(stationName) %>%
  slice(1)

View(stations) # remove the two doubled up HaMakhtesh ones, since they are already accounted for with Hatzera_drill and Ashmedai (and no points are actually assigned to these stations right now)
stations <- stations %>%
  filter(!grepl("HaMakhtesh", stationName))

write_rds(stations, here("data/created/stations.RDS"))

fs <- sort(unique(stations$stationName))

mapview(carcasses_inpa, label = "stationName", color = "blue", col.regions = "blue") +
  mapview(stations, label = "stationName", color = "black", col.regions = "black")

# Looking at this map shows us that 1) not all INPA carcasses are deposited at feeding stations (what's up with that? is this not a complete list of feeding stations?) # XXX add this to the report

# Audit and fix the data --------------------------------------------------
carcassAudit <- read_excel(here("data/raw/carcassAudit.xlsx")) %>%
  mutate(carcID = as.numeric(carcID),
         flag = as.logical(flag),
         flagGideon = as.logical(flagGideon))
ci_fixed <- carcasses_inpa #initialized a new version of the carcasses, fixed.

# Tasks
# 1. Standardize names
sort(unique(fs))
sort(unique(carcasses_inpa$stationName))

ci_fixed <- ci_fixed %>%
  mutate(stationName = case_when(stationName == "Antennas" ~ "Antenas",
                                 stationName == "Hai-bar feeding station" ~ "Hai_Bar_Carmel",
                                 stationName == "south camus" ~ "Camus_south",
                                 stationName %in% c("Mount Gazem", "Mt Gezem") ~ "Gezem_mount",
                                 stationName == "Galchan" ~ "Golhan",
                                 stationName == "smooth mountain (halak)" ~ "Hahalak_mount",
                                 stationName == "Hatzra Drilling" ~ "Hatzera_drill",
                                 stationName == "Hispia meddow" ~ "Hispiya_meadow",
                                 stationName == "kaolin" ~ "Kaolin",
                                 stationName == "Lashvia 450" ~ "Lashabia_450",
                                 stationName == "north golan" ~ "North_Golan",
                                 stationName == "small crater observation" ~ "Small_crater_view",
                                 stationName == "Mt Zaror" ~ "Tzaror_mount",
                                 stationName == "Zaror cage" ~ "Tzaror_trap",
                                 stationName == "Gorni hill" ~ "Gorni_hill",
                                 stationName == "Nahal Nakrot" ~ "Nekarot_valley",
                                 stationName == "north cliff - Hava stream" ~ "Hava_cliff",
                                 stationName == "observation ben-Yair" ~ "Ben_Yair_view",
                                 stationName == "other" ~ "Other",
                                 .default = stationName))
sort(unique(ci_fixed$stationName))

unique(ci_fixed$stationName)[!(unique(ci_fixed$stationName) %in% fs)]

#carcassAudit %>% filter(!(carcID %in% ci_fixed$carcID)) %>% View() # all of these are as they should be--I didn't mistype any carcIDs. The remaining NAs are where I had "CLUSTER" or similar.

audited <- ci_fixed %>%
  left_join(carcassAudit)

needhelp <- audited %>%
  filter(flagGideon == T)
nrow(needhelp)

canfix <- audited %>%
  filter(!flagGideon | is.na(flagGideon))

# new map for gideon:
# mapview(stations, label = "stationName", color = "black", col.regions = "black") + 
#   mapview(canfix, label = "stationName", color = "gray", col.regions = "gray")+
#   mapview(needhelp %>% filter(color == "yellow"), label = "stationName", color = "yellow", col.regions = "yellow", legend = F)+
#   mapview(needhelp %>% filter(color == "pink"), label = "stationName", color = "pink", col.regions = "pink", legend = F)+
#   mapview(needhelp %>% filter(color == "white"), label = "stationName", color = "white", col.regions = "white", legend = F)+
#   mapview(needhelp %>% filter(color == "green"), label = "stationName", color = "green", col.regions = "green", legend = F)+
#   mapview(needhelp %>% filter(color == "blue"), label = "stationName", color = "blue", col.regions = "blue", legend = F)+
#   mapview(needhelp %>% filter(color == "red"), label = "stationName", color = "red", col.regions = "red", legend = F)+
#   mapview(needhelp %>% filter(color == "tan"), label = "stationName", color = "tan", col.regions = "tan", legend = F)+
#   mapview(needhelp %>% filter(color == "purple"), label = "stationName", color = "purple", col.regions = "purple", legend = F)+
#   mapview(needhelp %>% filter(color == "darkred"), label = "stationName", color = "darkred", col.regions = "darkred", legend = F)+
#   mapview(needhelp %>% filter(color == "darkorange"), label = "stationName", color = "darkorange4", col.regions = "darkorange4", legend = F)+
#   mapview(needhelp %>% filter(color == "orange"), label = "stationName", color = "orange", col.regions = "orange", legend = F)+
#   mapview(needhelp %>% filter(color == "darkgreen"), label = "stationName", color = "darkgreen", col.regions = "darkgreen", legend = F)
# XXX on 2024-10-18, sent this map to Gideon along with the carcassAudit spreadsheet so he can help me figure out how to fix the confusing points.

# 2. create a column for "edited"
audited$edited_coords <- NA
audited$explanation <- NA
audited <- dg(audited) # drop geometry

# 3. Reassign points to the correct feeding stations and mark as reassigned
toreassign <- audited %>%
  filter(todo == "reassign" & !is.na(carcID))

reassign_coords <- stations %>%
  dg() %>%
  dplyr::select(stationName, lat, long) %>%
  rename("lat_fixed" = lat, "long_fixed" = long)

all(toreassign$reassign_to %in% reassign_coords$stationName)
toreassign[which(!(toreassign$reassign_to %in% reassign_coords$stationName)),] %>%
  pull(reassign_to) %>%
  unique() # okay, there are a bunch that I need to reassign that I don't yet have station coords for. 

# XXX is Gamla the same as "Gamla - cage"?
# XXX Need a coordinate point for "Carmel - cage"
# XXX Need a coordinate point for Kachal
# XXX Need a coordinate point for Hever, Hever cage, and Ashmedai
audited$long_orig <- audited$long
audited$lat_orig <- audited$lat
audited <- audited %>%
  rename("itmLong_orig" = "itmLong",
         "itmLat_orig" = "itmLat")

set.seed(3)
for(i in 1:nrow(audited)){
  if(audited$todo[i] == "reassign" & !is.na(audited$todo[i])){
    if(audited$reassign_to[i] %in% reassign_coords$stationName){
      
      # edit lat and long with a little jitter
      audited$lat[i] <- reassign_coords$lat_fixed[reassign_coords$stationName == audited$reassign_to[i]] + runif(1, min = -0.0001, max = 0.0001)
      audited$long[i] <- reassign_coords$long_fixed[reassign_coords$stationName == audited$reassign_to[i]] + runif(1, min = -0.0001, max = 0.0001)
      
      # add the correct station name
      audited$stationName[i] <- audited$reassign_to[i]
      
      audited$edited_coords[i] <- T
      audited$explanation[i] <- "reassigned to station with jitter"
    }
  }
}

## turn it back into a spatial object
audited <- st_as_sf(audited, coords = c("long", "lat"), crs = "WGS84", remove = F) %>% st_transform(32636)

# View progress so far--should have many fewer points out of place
# XXX start here
mapview(stations, label = "stationName", color = "black", col.regions = "black", layer.name = "stations")+
  mapview(audited %>% filter(is.na(edited_coords) & !flag), label = "stationName", color = "gray", col.regions = "gray", layer.name = "ok")+
  mapview(audited %>% filter(edited_coords), label = "stationName", color = "blue", col.regions = "blue", layer.name = "fixed_coords")+
  mapview(audited %>% filter(is.na(edited_coords) & flag), label = "stationName",
          color = "red", col.regions = "red", layer.name = "todo")

# Awesome, this looks great!

# 4. Look for points exactly stacked
## IDs to look for: 5006137, 3059636, 3055792
checkfor <- c(5006137, 3059636, 3055792)
dups <- audited %>% group_by(lat, long) %>%
  filter(n() > 1) %>%
  arrange(lat, long)
dup_ids <- dups$carcID
all(checkfor %in% dups$carcID) # yay, caught them all

# For any that are exactly duplicated, let's jitter ever so slightly
set.seed(3)
for(i in 1:nrow(audited)){
  if(audited$carcID[i] %in% dup_ids){
    audited$lat[i] <- audited$lat[i] + runif(1, min = -0.0001, max = 0.0001)
    audited$long[i] <- audited$long[i] + runif(1, min = -0.0001, max = 0.0001)
    audited$edited_coords[i] <- T
    audited$explanation[i] <- "Jittered to deduplicate coords"
  }
}

## turn it back into a spatial object
audited <- st_as_sf(dg(audited), coords = c("long", "lat"), crs = "WGS84", remove = F) %>% st_transform(32636)

## double check that there are no duplicate coords remaining
audited %>% group_by(lat, long) %>% filter(n() > 1) # 0 rows, yay

# 5. check IDs in clusters
# Look at 750m circles around the feeding stations
stations_buffered <- st_buffer(stations, 750)
stations_buffered_list <- stations_buffered %>%
  group_by(stationName) %>%
  group_split()
intersections <- map(stations_buffered_list, ~st_intersection(audited, .x) %>%
                       dplyr::select(carcID, date, time, long, lat, stationName))
map_dbl(intersections, nrow)
names <- map_chr(stations_buffered_list, ~.x$stationName[1])
names_intersections <- map(intersections, ~sort(.x$stationName))
names(names_intersections) <- names
names(intersections) <- names

# XXX assign all dots close to Amiaz as Amiaz (formerly Other) [DONE, BELOW]
# XXX we have one Tzaror_mount near Antenas. Should that be assigned to Antenas, or have coords changed to Tzaror_mount?
# XXX three Gorni_hills next to Ben_Yair_view
# XXX one North_Golan next to Kachal
# XXX one Lashabia_450 next to Camus_south
# XXX assign all dots close to Chail_hills as Chail_hills (formerly Other) [DONE, BELOW]
# XXX one Tzaror_mount next to Daroch
# XXX many instances of "cage" next to "Gamla". Are there any instances of "cage" elsewhere?
# XXX For Hai_Bar_Carmel, we have both "Carmel - cage" and "Hai_Bar_Carmel". Either need to merge these or get a new point for "Carmel - cage".
# XXX one Gezem_mount next to Hava_cliff
# XXX one Gezem_mount next to Tzaror_mount (also within striking distance of Tzaror_trap)
# XXX assign all dots close to Tzvira_plateau to Tzvira_plateau (formerly Other) [DONE, BELOW]

# Can take action on assigning the ones to Amiaz, Chail_hills, and Tzvira_plateau. Need help for the others.

## Amiaz
mapview(audited, label = "stationName", color = "gray", col.regions = "gray") +
mapview(stations, label = "stationName", color = "black", col.regions = "black")+
mapview(intersections$Amiaz, label = "stationName", color = "blue", col.regions = "blue") # we are safe here, nothing else around Amiaz

audited$stationName[audited$carcID %in% intersections$Amiaz$carcID] <- "Amiaz"

## Chail_hills
mapview(audited, label = "stationName", color = "gray", col.regions = "gray") +
  mapview(stations, label = "stationName", color = "black", col.regions = "black")+
  mapview(intersections$Chail_hills, label = "stationName", color = "blue", col.regions = "blue") # likewise, no risk of accidentally picking up other points here

audited$stationName[audited$carcID %in% intersections$Chail_hills$carcID] <- "Chail_hills"

## Tzvira_plateau
mapview(audited, label = "stationName", color = "gray", col.regions = "gray") +
  mapview(stations, label = "stationName", color = "black", col.regions = "black")+
  mapview(intersections$Tzvira_plateau, label = "stationName", color = "blue", col.regions = "blue") # safe here too 

audited$stationName[audited$carcID %in% intersections$Tzvira_plateau$carcID] <- "Chail_hills"

# Remaining to-dos to ask Gideon about
# XXX we have one Tzaror_mount near Antenas. Should that be assigned to Antenas, or have coords changed to Tzaror_mount?
# XXX three Gorni_hills next to Ben_Yair_view
# XXX one North_Golan next to Kachal
# XXX one Lashabia_450 next to Camus_south
# XXX one Tzaror_mount next to Daroch
# XXX many instances of "cage" next to "Gamla". Are there any instances of "cage" elsewhere?
# XXX For Hai_Bar_Carmel, we have both "Carmel - cage" and "Hai_Bar_Carmel". Either need to merge these or get a new point for "Carmel - cage".
# XXX one Gezem_mount next to Hava_cliff
# XXX one Gezem_mount next to Tzaror_mount (also within striking distance of Tzaror_trap)

# Write out the audited carcass data --------------------------------------
carcasses_audited <- audited %>%
  bind_cols(st_coordinates(.))
write_rds(carcasses_audited, file = here("data/created/carcasses_audited.RDS"))