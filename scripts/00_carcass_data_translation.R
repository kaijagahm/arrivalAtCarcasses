# Script to read in and aggregate the carcass data
library(here)
library(readxl)
library(tidyverse)
library(RColorBrewer)
library(paletteer)
library(sf)
library(mapview)
library(writexl)
source(here("R/functions.R"))

# We have two spreadsheets containing carcass data. One from 2018 through 2024, and one updated [I don't remember the exact specs of the updated one]. Need to bring these into R, combine them, check for duplicates, and clean everything up.

# Read in the old and new data
old <- read_excel(here("data/raw/translated/FeedingData from 2018_2024_Translated_9_25_2024 (1).xlsx")) # there will be a bunch of warnings due to the Hebrew characters. Ignore for now--I don't think they affect most important columns.
new <- read_excel(here("data/raw/translated/Feeding 2024 update - translated (1).xlsx"))

# Clean up the column names (no spaces, etc.)
old <- old %>%
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

new <- new %>%
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

# Figure out which names don't match
names(new[!(names(new) %in% names(old))]) # names present only in `new`--some specific animal columns
names(old[!(names(old) %in% names(new))]) # names present only in `old`--none

# Check for duplicate carcIDs
any(old$carcID %in% new$carcID) # some of the `old` ones also show up in the `new` dataset
which(old$carcID %in% new$carcID) # the indices of which ones are in both
old <- old[-which(old$carcID %in% new$carcID),] # remove those carcasses from `old`

# Join the old and new datasets
## bind_rows will handle the three columns that are present only in `new`--will just fill with NAs for `old`
carcasses_inpa <- bind_rows(old, new)
original_summary <- carcasses_inpa %>% group_by(year = lubridate::year(date), stationName) %>% summarize(n = n()) %>% pivot_wider(names_from = "year", values_from = "n")
write_csv(original_summary, file = "data/created/original_summary_2026-07-09.csv")

# Add a properly-formatted datetime column
carcasses_inpa <- carcasses_inpa %>%
  mutate(datetime_il = lubridate::ymd_hms(paste0(as.character(lubridate::ymd(date)), 
                                                 substr(time, 12, 19)), tz = "Israel"),
         datetime = lubridate::with_tz(datetime_il, tzone = "UTC"))

# Now it's time to look at the station names. These names matter because we will want to group together carcasses that are at the same station. Due to different translations, we may have some mismatches.
carcasses_inpa <- carcasses_inpa %>% mutate(stationName = factor(stationName))
sort(unique(carcasses_inpa$stationName)) # there are differences in capitalization and a bunch of different names that may or may not be the same.
# I also notice that some of the station names are "___ cage". I want to know when a carcass is provided in a cage or not, because that will make a difference to how the vultures behave around it. So let's add a "cage" column that takes TRUE or FALSE depending on whether the carcass was provided in a cage.

# Add "cage" variable
carcasses_inpa <- carcasses_inpa %>%
  mutate(cage = case_when(str_detect(stationName, "cage") ~ T,
                          .default = F))

# Transform the carcass data to a spatial object so we can view the points on a map
carcasses_inpa <- st_as_sf(carcasses_inpa, coords = c("long", "lat"), 
                           crs = "WGS84", remove = F) %>%
  st_transform(32636) # 32636 is the UTM projection for the area around Israel

# Check if any duplicate carcasses are left (they should have all been removed)
carcasses_inpa %>% group_by(carcID) %>% filter(n() > 1) # 0 rows, good.

# Read in data about feeding station locations to help us out. 
# ------------------------------------------------
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

# Transform to a spatial object
stations <- stations %>%
  st_as_sf(coords = c("long", "lat"), remove = F, crs = "WGS84") %>%
  st_transform(32636)

# Deduplicate the stations by name and keep the new ones
stations <- stations %>%
  arrange(stationName, type) %>%
  group_by(stationName) %>%
  slice(1)

# Begin manually fixing stations, in consultation with Gideon
## Remove HaMakhtesh HaKatan and HaMakhtesh HaKatan reserve, since they are already accounted for with Hatzera_drill and Ashmedai (and no points are actually assigned to these stations right now) (Gideon told me to make this change in his email on 2024-10-22).
stations <- stations %>%
  filter(!grepl("HaMakhtesh", stationName))

fs <- sort(unique(stations$stationName))

# Compare feeding station names from the `stations` data frame and from the `carcasses_inpa` data. We are likely to have similar names but different spellings/capitalizations etc.
mapview(carcasses_inpa, label = "stationName", color = "blue", col.regions = "blue") +
  mapview(stations, label = "stationName", color = "black", col.regions = "black")

# Looking at this map shows us that not all INPA carcasses are recorded as being deposited at feeding stations, likely due to the errors mentioned above (GPS throwing, points on roads, etc.)

# Audit and fix the data --------------------------------------------------
# I did this by hand and the notes are in this spreadsheet:
carcassAudit <- read_excel(here("data/raw/carcassAudit.xlsx")) %>%
  mutate(carcID = as.numeric(carcID),
         flag = as.logical(flag),
         flagGideon = as.logical(flagGideon)) %>%
  filter(!is.na(carcID))
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

audited <- ci_fixed %>%
  left_join(carcassAudit)
audited$edited_coords <- NA
audited$edited_station <- NA
audited$explanation <- NA

# 3. Move points to the correct feeding stations and mark as moved
## Grab the ones that need to be moved
tomove <- audited %>%
  filter(todo == "reassign" & !is.na(carcID))
dim(tomove) # 102 carcasses need to be moved!! That's a lot.

## Get station coords to use for moving the points--we can match by station name
move_coords <- stations %>%
  sf::st_drop_geometry() %>%
  dplyr::select(stationName, lat, long) %>%
  rename("lat_fixed" = lat, "long_fixed" = long)

## Check whether we have station coords to use for moving the carcasses for all of them
all(tomove$reassign_to %in% move_coords$stationName) # nope, I do not :(
tomove[which(!(tomove$reassign_to %in% move_coords$stationName)),] %>%
  pull(reassign_to) %>%
  unique() # these are the ones that we need coordinates for.

## Save the original lat and long coords to new columns for future reference
audited$long_orig <- audited$long
audited$lat_orig <- audited$lat
audited <- audited %>%
  rename("itmLong_orig" = "itmLong",
         "itmLat_orig" = "itmLat")

for(i in 1:nrow(audited)){
  cat("i = ", i, "\n")
  if(audited$todo[i] == "reassign" & !is.na(audited$todo[i])){ # if needs reassignment...
    if(audited$reassign_to[i] %in% move_coords$stationName){ # and if we have the coords...
      
      # ...then reassign the lat/long to the station coordinates
      audited$lat[i] <- move_coords$lat_fixed[move_coords$stationName == audited$reassign_to[i]]
      audited$long[i] <- move_coords$long_fixed[move_coords$stationName == audited$reassign_to[i]]
      
      # ...and add the correct station name.
      audited$stationName[i] <- audited$reassign_to[i]
      
      # to keep track, make a note that we edited these coords and add an explanation.
      audited$edited_coords[i] <- T
      audited$explanation[i] <- "reassigned to station coords"
      if(audited$stationNameChange[i] == T & !is.na(audited$stationNameChange[i])){
        audited$edited_station[i] <- T
      }
    }
  }
}

# Double check: are there any that aren't labeled as having the coords edited that have mismatches in the old and new coords?
audited %>% filter(is.na(edited_coords), lat_orig != lat) # none, good
audited %>% filter(is.na(edited_coords), long_orig != long)

# The other way: are there any that are labeled as edited_coords but where the coords didn't change?
audited %>% filter(edited_coords, lat_orig == lat) # one (Maybe it just wasn't different enough?)
audited %>% filter(edited_coords, long_orig == long) # none. 

## turn it back into a spatial object
audited <- st_as_sf(audited, coords = c("long", "lat"), crs = "WGS84", remove = F) %>% st_transform(32636)

# 5. check IDs in clusters
## Look at 750m circles around the feeding stations
stations_buffered <- st_buffer(stations, 750)
stations_buffered_list <- stations_buffered %>%
  group_by(stationName) %>%
  group_split()
intersections <- map(stations_buffered_list, ~st_intersection(audited, .x) %>%
                       dplyr::select(carcID, date, time, long, lat, stationName))
map_dbl(intersections, nrow) # do the carcasses intersect with the buffer zone of a known feeding station?
names <- map_chr(stations_buffered_list, ~.x$stationName[1])
names_intersections <- map(intersections, ~sort(.x$stationName))
names(names_intersections) <- names
names(intersections) <- names

# Can take action on assigning the ones to Amiaz and Tzvira_plateau. Need help for the others.

# Proceeding with caution, we can use these intersections to guide assignment of carcasses to stations. This is assuming that the stationName was incorrectly chosen, rather than the coordinates being wrong. Let's look at the stations one at a time and make sure the assignment makes sense.
## Amiaz
mapview(audited, label = "stationName", color = "gray", col.regions = "gray") +
  mapview(stations, label = "stationName", color = "black", col.regions = "black")+
  mapview(intersections$Amiaz, label = "stationName", color = "blue", col.regions = "blue") # everything blue should be reassigned to Amiaz. We have a bunch currently labeled "Other" there. Looks safe to do this--no other station is in the immediate vicinity.

audited$stationName[audited$carcID %in% intersections$Amiaz$carcID] <- "Amiaz"
audited$edited_station[audited$carcID %in% intersections$Amiaz$carcID] <- TRUE

## Tzvira_plateau
mapview(audited, label = "stationName", color = "gray", col.regions = "gray") +
  mapview(stations, label = "stationName", color = "black", col.regions = "black")+
  mapview(intersections$Tzvira_plateau, label = "stationName", color = "blue", col.regions = "blue") # safe here too 

audited$stationName[audited$carcID %in% intersections$Tzvira_plateau$carcID] <- "Tzvira_plateau"
audited$edited_station[audited$carcID %in% intersections$Tzvira_plateau$carcID] <- TRUE

# Another audit with Shaaked and May --------------------------------------
# Fixes after new meeting with Shaaked ------------------------------------
# Hai-Bar Carmel = the whole complex
# 499018: can set this roughly as the cage location for Carmel 
carmel_cage_loc <- audited[audited$carcID == "4990018",c("long", "lat")]
# move Hai Bar Carmel points to the feeding station.
idx1 <- which(audited$carcID %in% c(4931791, 4921125, 4904674, 4925905))
lng1 <- stations$long[stations$stationName == "Hai_Bar_Carmel"]
lat1 <- stations$lat[stations$stationName == "Hai_Bar_Carmel"]
audited$long[idx1] <- lng1
audited$lat[idx1] <- lat1
audited$explanation[idx1] <- "Kaija_Shaaked moved to station"
audited$edited_coords[idx1] <- TRUE

# move Carmel cage points to the cage location
idx2 <- which(audited$carcID %in% c(4909227, 5014984, 4974526, 4981075, 4976681, 4976659, 4987102, 4987103, 4987104, 4987105, 4961809, 4962473, 4962475, 4962476, 4962477, 4962479, 4962480, 4962482))
lng2 <- carmel_cage_loc$long
lat2 <- carmel_cage_loc$lat
audited$long[idx2] <- lng2
audited$lat[idx2] <- lat2
audited$explanation[idx2] <- "Kaija_Shaaked moved to Carmel cage"
audited$edited_coords[idx2] <- TRUE

# 4906144--probably wrong name. Check if scroll down.
# KG decision: rename to Kachal
audited$stationName[audited$carcID == 4906144] <- "Kachal"
audited$explanation[audited$carcID == 4906144] <- "KG renamed to Kachal based on proximity. Assuming the original station name (North_Golan) was wrong."
audited$edited_station[audited$carcID == 4906144] <- TRUE 

# Gamla stn point is wrong--should be near e.g. 5006712 pts
long3 <- audited$long[audited$carcID == 5006712]
lat3 <- audited$lat[audited$carcID == 5006712]
stations$long[stations$stationName == "Gamla"] <- long3
stations$lat[stations$stationName == "Gamla"] <- lat3
audited$explanation[stations$stationName == "Gamla"] <- "Kaija_Shaaked fixed incorrect Gamla station coords"

# Move slightly misaligned Gamla points to the new station coords
idx4 <- which(audited$carcID %in% c(4895890, 4908755, 4992013))
lng4 <- stations$long[stations$stationName == "Gamla"]
lat4 <- stations$lat[stations$stationName == "Gamla"]
audited$long[idx4] <- lng4
audited$lat[idx4] <- lat4
audited$edited_coords[idx4] <- TRUE
audited$explanation[idx4] <- "Kaija_Shaaked moved to Gamla station"

# 4924215 visitor building loc. all points to move elsewhere.
# The problem is, I don't know where these should go because there are three possible cage locations. Just going to leave these be for now because ultimately I don't care about the cage points anyway.

# Fix single point in North Golan: 4927271
audited$long[audited$carcID == 4927271] <- stations$long[stations$stationName == "North_Golan"]
audited$lat[audited$carcID == 4927271] <- stations$lat[stations$stationName == "North_Golan"]
audited$explanation[audited$carcID == 4927271] <- "Kaija_Shaaked moved to North_Golan station"
audited$edited_coords[audited$carcID == 4927271] <- TRUE

# Fix points a little ways away from Cachal
idx5 <- which(audited$carcID %in% c(4995231, 5016991, 5011708, 5011716, 4846275, 4846276, 4985947))
lng5 <- stations$long[stations$stationName == "Cachal"]
lat5 <- stations$lat[stations$stationName == "Cachal"]
audited$long[idx5] <- lng5
audited$lat[idx5] <- lat5
audited$explanation[idx5] <- "Kaija_Shaaked moved to Cachal station"
audited$edited_coords[idx5] <- TRUE

# Fix Hever/Hever cage points
idx6 <- which(audited$carcID %in% c(4774827, 4046471, 4481174, 3055795, 3055790, 3055799, 3114393, 1873329, 1873331, 1920556, 1920559, 4713589, 4723154, 4239726))
lng6 <- stations$long[stations$stationName == "Hever"]
lat6 <- stations$lat[stations$stationName == "Hever"]
audited$long[idx6] <- lng6
audited$lat[idx6] <- lat6
audited$explanation[idx6] <- "Kaija_Shaaked moved to Hever station"
audited$edited_coords[idx6] <- TRUE

# Fix Gorni Hill / Ben_Yair_view points
# Ambiguous ones: there are a few carcasses at Ben_Yair_view that are labeled Gorni_Hill. The two stations are close and it's anyone's guess whether they're mislabeled or mis-placed. I'm going to assume for now that they are mislabeled, so I'll just correct the label but not the coordinates, but that assumption isn't really based on strong evidence either way. # May confirms these were mislabeled, not mis-located.
idx7 <- which(audited$carcID %in% c(3126041, 1966034, 3479246))
audited$stationName[idx7] <- "Ben_Yair_view"
audited$edited_station[idx7] <- TRUE
audited$explanation[idx7] <- "Kaija_Shaaked fixed station name (Gorni Hill > Ben Yair view)"

# Relocate 4850735 to Antenas, although this does raise the question of whether the time is correct # May confirms this is right
idx8 <- which(audited$carcID == 4850735)
lng8 <- stations$long[stations$stationName == "Antenas"]
lat8 <- stations$lat[stations$stationName == "Antenas"]
audited$long[idx8] <- lng8
audited$lat[idx8] <- lat8
audited$explanation[idx8] <- "Kaija_Shaaked moved to Antenas station. Note: original location was very far away."
audited$edited_coords[idx8] <- TRUE

# Relabel all the "cage" points as "Gamla - cage"
idxx <- which(audited$stationName == "cage")
audited$stationName[idxx] <- "Gamla - cage"
audited$edited_station[idxx] <- TRUE
audited$explanation[idxx] <- "Renamed to Gamla - cage for consistency"

# Relabel 2953181 as Camus_south because it's with all the other Camus_south points
audited$stationName[audited$carcID == 2953181] <- "Camus_south"
audited$explanation[audited$carcID == 2953181] <- "Relabeled to Camus_south to match the points around it (KG)"
audited$edited_station[audited$carcID == 2953181] <- TRUE

# Move 1974887 and 1929902 to Camus_south coords because they're too far away
idx9 <- which(audited$carcID %in% c(1974887, 1929902))
lng9 <- stations$long[stations$stationName == "Camus_south"]
lat9 <- stations$lat[stations$stationName == "Camus_south"]
audited$long[idx9] <- lng9
audited$lat[idx9] <- lat9
audited$explanation[idx9] <- "Kaija_Shaaked moved to Camus_south station"
audited$edited_coords[idx9] <- TRUE

# Move points to Small_crater_view because they're on the road
idx10 <- which(audited$carcID %in% c(4882796, 4781817, 4790816, 4724712, 4724711, 4747265, 4850746, 4768278, 4737538, 4731983))
lng10 <- stations$long[stations$stationName == "Small_crater_view"]
lat10 <- stations$lat[stations$stationName == "Small_crater_view"]
audited$long[idx10] <- lng10
audited$lat[idx10] <- lat10
audited$explanation[idx10] <- "Kaija_Shaaked moved to Small_crater_view station"
audited$edited_coords[idx10] <- TRUE

# Fix stray Tzaror points
idx11 <- which(audited$carcID %in% c(4195626, 2448354, 4420641, 1985750))
lng11 <- stations$long[stations$stationName == "Tzaror_mount"]
lat11 <- stations$lat[stations$stationName == "Tzaror_mount"]
audited$long[idx11] <- lng11
audited$lat[idx11] <- lat11
audited$explanation[idx11] <- "Kaija_Shaaked moved to Tzaror_mount station"
audited$edited_coords[idx11] <- TRUE

# Relabel a lot of the "Other" points in one particular cluster as Ashmedai. Also for one that's currently labeled Rosh Maale Hatzera
idx12 <- which(audited$carcID %in% c(1768916, 3758525, 2296768, 2862521, 2026369, 2886595, 4035988, 4808631, 4498675, 2070171, 3076137, 1748428))
audited$stationName[idx12] <- "Ashmedai"
audited$edited_station[idx12] <- TRUE
audited$explanation[idx12] <- "Relabeled to Ashmedai to match the points around it (KG)"

# 1885366 Gezem_mount--relabel to Tzaror_mount
audited$stationName[audited$carcID == 1885366] <- "Tzaror_mount"
audited$edited_station[audited$carcID == 1885366] <- TRUE
audited$explanation[audited$carcID == 1885366] <- "Relabeled to Tzaror_mount to match points around it (KG)"

# Relabel the second location of Golhan to "Golhan 2" to distinguish it from the first
idx13 <- which(audited$carcID %in% c(4892923, 4935384, 4962861, 4874955, 4847006, 4811483, 4776380, 4762512))
audited$stationName[idx13] <- "Golhan2"
audited$edited_station[idx13] <- TRUE
audited$explanation[idx13] <- "Relabeled to Golhan2 to distinguish from other Golhan location (KG)"

# Name the different "Other" places
audited$stationName[audited$carcID %in% c(4253362, 4181242)] <- "Random_Feeding_JD"
audited$edited_station[audited$carcID %in% c(4253362, 4181242)] <- TRUE
audited$explanation[audited$carcID %in% c(4253362, 4181242)] <- "Random feeding experiments conducted by rangers"

audited$stationName[audited$carcID %in% c(1848638, 1950783, 3965678, 2031190)] <- "Nahal_Saif_Barrels"
audited$explanation[audited$carcID %in% c(1848638, 1950783, 3965678, 2031190)] <- "May: Experiment by Arie"
audited$edited_station[audited$carcID %in% c(1848638, 1950783, 3965678, 2031190)] <- TRUE

audited$edited_station[audited$carcID == 3059634] <- TRUE
audited$stationName[audited$carcID == 3059634] <- "Random_Feeding_SA"
audited$explanation[audited$carcID == 3059634] <- "Gazelle roadkill; ranger placed it at location experimentally."
audited <- audited %>%
  filter(!(carcID %in% c(3059636, 3059637))) # remove the other two and retain 3059634 as gazelle point.

idx15 <- which(audited$stationName == "Other" & audited$long < 34.72570)
audited$edited_station[idx15] <- TRUE
audited$stationName[idx15] <- "Chail_hills"
audited$explanation[idx15] <- "This is the real location of the Chail_hills station"
stations$long[stations$stationName == "Chail_hills"] <- audited$long[audited$carcID == 1730414]
stations$lat[stations$stationName == "Chail_hills"] <- audited$lat[audited$carcID == 1730414]

# May: 4564184 and 1450235--relabel as Chail_hills, and move coords to Chail hills stn coords
idx16 <- which(audited$carcID %in% c(4564184, 1450235))
lng16 <- stations$long[stations$stationName == "Chail_hills"]
lat16 <- stations$lat[stations$stationName == "Chail_hills"]
audited$long[idx16] <- lng16
audited$lat[idx16] <- lat16
audited$explanation[idx16] <- "May relabeled and moved to Chail_hills"
audited$edited_coords[idx16] <- TRUE

# Remove some fake carcasses that were inserted for testing purposes.
audited <- audited %>%
  filter(!(carcID %in% c(4868888, 4988920, 4850788, 4850711)))

# May: Assign 4871929 to Gamla cage using May's estimated coords.
audited$lat[audited$carcID == 4871929] <- 32.541615
audited$long[audited$carcID == 4871929] <- 35.454660
audited$edited_coords[audited$carcID == 4871929] <- TRUE
audited$explanation[audited$carcID == 4871929] <- "Moved to Gamla cage--GPS jamming"

# May: 1762465--relabel as "Upper Ein Avdat"
audited$stationName[audited$carcID == 1762465] <- "Upper_Ein_Avdat"
audited$edited_station[audited$carcID == 1762465] <- TRUE
audited$explanation[idx16] <- "May: this was a one-time feeding for vulture day activities"

# May: 4315893--move coords to Hever stn, keep label as is
audited$lat[audited$carcID == 4315893] <- stations$lat[stations$stationName == "Hever"]
audited$long[audited$carcID == 4315893] <- stations$long[stations$stationName == "Hever"]
audited$edited_coords[audited$carcID == 4315893] <- TRUE
audited$explanation[audited$carcID == 4315893] <- "May: retrospective report; coords were inaccurate."

# Fix the NA stations
audited$stationName[audited$carcID == 5016662] <- "Hai_Bar_Carmel"
audited$edited_station[audited$carcID == 5016662] <- TRUE
audited$explanation[audited$carcID == 5016662] <- "Relabeled to Hai_Bar_Carmel to match points around it (KG)"

audited$stationName[audited$carcID == 5011580] <- "Small_crater_view"
audited$edited_station[audited$carcID == 5011580] <- TRUE
audited$explanation[audited$carcID == 5011580] <- "Relabeled to Small_Crater_view to match points around it (KG)"

# test <- audited %>% filter(is.na(stationName))
audited <- st_as_sf(st_drop_geometry(audited), coords = c("long", "lat"), crs = "WGS84", remove = F) %>% st_transform(32636)

# Label 4850012 as Nahal Daliyot
audited$stationName[audited$carcID == 4850012] <- "Nahal_Daliyot"
audited$edited_station[audited$carcID == 4850012] <- TRUE
audited$explanation[audited$carcID == 4850012] <- "May: Experiment for a potential new feeding station"

# Label 4924206 and 5006137 as Kachal_cage, and move to Cachal. These are already labeled as "cage".
audited$stationName[audited$carcID %in% c(4924206, 5006137)] <- "Kachal_cage"
audited$long[audited$carcID %in% c(4924206, 5006137)] <- stations$long[stations$stationName == "Cachal"]
audited$lat[audited$carcID %in% c(4924206, 5006137)] <- stations$lat[stations$stationName == "Cachal"]
audited$edited_station[audited$carcID %in% c(4924206, 5006137)] <- TRUE
audited$edited_coords[audited$carcID %in% c(4924206, 5006137)] <- TRUE
audited$explanation[audited$carcID %in% c(4924206, 5006137)] <- "May: Move to Kachal station and label as Kachal_cage"

mapview(stations, col.regions = "black")+
  mapview(audited, label = "stationName", zcol = "stationName", col.regions = paletteer_c("grDevices::rainbow", 42))+ mapview(stations, col.regions = "black")

# Summary of edits made
orig <- bind_rows(old, new) %>%
  select(carcID, "stationName_orig" = stationName, "long_orig" = long, "lat_orig" = lat) %>%
  distinct()

new <- audited %>%
  select(carcID, stationName, long, lat, edited_coords, edited_station, explanation) %>%
  distinct()

both <- left_join(orig, new, by = "carcID") %>%
  filter(!is.na(carcID), !is.na(long), !is.na(lat)) %>%
  select(carcID, stationName_orig, stationName, long_orig, lat_orig, long, lat, edited_coords, edited_station, explanation) %>%
  sf::st_as_sf(coords = c("long", "lat"), remove = F, crs = "WGS84")

both %>%
  filter(is.na(edited_coords) & ((long_orig != long)|(lat_orig != lat)))# 0 rows--good, so this column is accurate

changed <- both %>%
  filter(edited_coords | edited_station)

moved <- both %>%
  filter(edited_coords)

moved_original_locs <- st_drop_geometry(moved) %>%
  sf::st_as_sf(coords = c("long_orig", "lat_orig"), remove = F, crs = "WGS84")

relabeled <- both %>%
  filter(edited_station)

summary_bystation <- audited %>%
  st_drop_geometry() %>%
  group_by(stationName) %>%
  summarize(n = n(),
            n_relabeled = sum(edited_station, na.rm = T),
            n_moved = sum(edited_coords, na.rm = T),
            prop_relabeled = round(n_relabeled/n, 2),
            prop_moved = round(n_moved/n, 2)) %>%
  arrange(desc(prop_moved), desc(prop_relabeled))

summary_byyear <- audited %>%
  st_drop_geometry() %>%
  group_by("year" = lubridate::year(date)) %>%
  summarize(n = n(),
            n_relabeled = sum(edited_station, na.rm = T),
            n_moved = sum(edited_coords, na.rm = T),
            prop_relabeled = round(n_relabeled/n, 2),
            prop_moved = round(n_moved/n, 2)) %>%
  arrange(desc(year))

dataset_list <- list(
  "all_points" = st_drop_geometry(audited),
  "carcasses_changed" = st_drop_geometry(changed),
  "carcasses_moved"   = st_drop_geometry(moved),
  "carcasses_moved_originallocs" = st_drop_geometry(moved_original_locs),
  "carcasses_relabeled" = st_drop_geometry(relabeled),
  "summary_by_station" = summary_bystation,
  "summary_by_year" = summary_byyear
)

writexl::write_xlsx(dataset_list, path = "data/created/carcass_auditing/2026-07-09_carcassData.xlsx")

st_write(changed, "data/created/carcass_auditing/changed.kml", driver = "KML", delete_layer = TRUE)
st_write(moved, "data/created/carcass_auditing/moved.kml", driver = "KML", delete_layer = TRUE)
st_write(moved_original_locs, "data/created/carcass_auditing/moved_original_locs.kml", driver = "KML", delete_layer = TRUE)
st_write(relabeled, "data/created/carcass_auditing/relabeled.kml", driver = "KML", delete_layer = TRUE)
st_write(both, "data/created/carcass_auditing/all_points.kml", driver = "KML", delete_layer = TRUE)
write_csv(audited, "data/created/carcass_auditing/all_points.csv")

# # Write out the audited carcass data --------------------------------------
carcasses_audited <- audited %>%
  bind_cols(st_coordinates(.))
write_rds(carcasses_audited, file = here("data/created/carcass_auditing/carcasses_audited.RDS"))

# Write out the fixed station data, since I made at least a few edits to station positions
write_rds(stations, file = here("data/created/carcass_auditing/stations.RDS"))
