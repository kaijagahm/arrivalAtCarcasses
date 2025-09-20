# Script to read in and aggregate the carcass data
library(here)
library(readxl)
library(tidyverse)
library(RColorBrewer)
library(sf)
library(mapview)
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

write_rds(carcasses_inpa, file = here("data/created/carcasses_inpa.RDS"))

# Look at the stations on mapview
custom_colors <- hcl.colors(n = length(levels(carcasses_inpa$stationName)), palette = "Spectral", rev = TRUE)
mapview(carcasses_inpa, zcol = "stationName", col.regions = custom_colors)

# What do we notice?
## Some GPS points are thrown into the sea and will probably need to be reassigned to their stations.
## Some points seem far away from their feeding station--for example, there are some in North Golan that are far from the others (4927271, 4942740). Need to see if those are legitimately elsewhere or whether they need to be reassigned to the lat/long coords of the feeding station. Some are quite obviously wrong, such as 4942740 (which is in a town), but some, like 4927271, seem at least plausible to my naive eyes.
## There are also some that seem to have been assigned the wrong station name. For example, there's a North Golan point (4906144) among the Kachal points. While it's possible that the ranger entered data for a legitimate North Golan point while physically located at Kachal and accidentally used the Kachal location, it seems more likely that they intedended to categorize this carcass as Kachal but entered the wrong station name. Someone should confirm this.
## There are fairly frequently points located on roads. Data was probably entered while they were driving? Should change lat/long to the lat/long of the station. I guess this raises the question of whether we also need to change the timestamp of the carcass, too? But I'm not sure how we would be able to figure that out. Example: (4961228)
## There are other irregularities. Let's go through them one by one.

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

mapview(stations, zcol = "stationName", col.regions = custom_colors)
write_rds(stations, here("data/created/stations.RDS"))

# Begin manually fixing stations, in consultation with Gideon
## Remove HaMakhtesh HaKatan and HaMakhtesh HaKatan reserve, since they are already accounted for with Hatzera_drill and Ashmedai (and no points are actually assigned to these stations right now) (Gideon told me to make this change in his email on 2024-10-22).
View(stations)
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

unique(ci_fixed$stationName)[!(unique(ci_fixed$stationName) %in% fs)]

mapview(ci_fixed, zcol = "stationName", col.regions = custom_colors)

audited <- ci_fixed %>%
  left_join(carcassAudit)

needhelp <- audited %>%
  filter(flagGideon == T)
nrow(needhelp) # 37

canfix <- audited %>%
  filter(!flagGideon | is.na(flagGideon))

# new map for gideon:
y <- needhelp %>% filter(color == "yellow")
p <- needhelp %>% filter(color == "pink")
w <- needhelp %>% filter(color == "white")
g <- needhelp %>% filter(color == "green")
b <- needhelp %>% filter(color == "blue")
r <- needhelp %>% filter(color == "red")
t <- needhelp %>% filter(color == "tan")
pp <- needhelp %>% filter(color == "purple")
dr <- needhelp %>% filter(color == "darkred")
do <- needhelp %>% filter(color == "darkorange")
o <- needhelp %>% filter(color == "orange")
dg <- needhelp %>% filter(color == "darkgreen")

mapview(stations, label = "stationName", color = "black", col.regions = "black") +
  mapview(canfix, label = "stationName", color = "gray", col.regions = "gray")+
  mapview(y, label = "stationName", color = "yellow", col.regions = "yellow", legend = F)+
  mapview(p, label = "stationName", color = "pink", col.regions = "pink", legend = F)+
  mapview(w, label = "stationName", color = "white", col.regions = "white", legend = F)+
  mapview(g, label = "stationName", color = "green", col.regions = "green", legend = F)+
  mapview(b, label = "stationName", color = "blue", col.regions = "blue", legend = F)+
  mapview(r, label = "stationName", color = "red", col.regions = "red", legend = F)+
  mapview(t, label = "stationName", color = "tan", col.regions = "tan", legend = F)+
  mapview(pp, label = "stationName", color = "purple", col.regions = "purple", legend = F)+
  mapview(dr, label = "stationName", color = "darkred", col.regions = "darkred", legend = F)+
  mapview(do, label = "stationName", color = "darkorange4", col.regions = "darkorange4", legend = F)+
  mapview(o, label = "stationName", color = "orange", col.regions = "orange", legend = F)+
  mapview(dg, label = "stationName", color = "darkgreen", col.regions = "darkgreen", legend = F)
# XXX on 2024-10-18, sent this map to Gideon along with the carcassAudit spreadsheet so he can help me figure out how to fix the confusing points.

# 2. We're going to need to keep track of which coordinates have been edited. Let's create a column for that.
audited$edited_coords <- NA
audited$explanation <- NA
audited <- sf::st_drop_geometry(audited)

# 3. Reassign points to the correct feeding stations and mark as reassigned
## Grab the ones that need to be reassigned
toreassign <- audited %>%
  filter(todo == "reassign" & !is.na(carcID))
dim(toreassign) # 102 carcasses need to be reassigned!! That's a lot.

## Get station coords to use for reassignment--we can match by station name
reassign_coords <- stations %>%
  sf::st_drop_geometry() %>%
  dplyr::select(stationName, lat, long) %>%
  rename("lat_fixed" = lat, "long_fixed" = long)

## Check whether we have station coords to use for reassignment for all of them
all(toreassign$reassign_to %in% reassign_coords$stationName) # nope, I do not :(
toreassign[which(!(toreassign$reassign_to %in% reassign_coords$stationName)),] %>%
  pull(reassign_to) %>%
  unique() # these are the ones that we need coordinates for.

# XXX is Gamla the same as "Gamla - cage"?
# XXX is Carmel - cage the same as "Hai_Bar_Carmel"?
# XXX Need a coordinate point for Kachal
# XXX is Hever cage the same as Hever?

## Save the original lat and long coords to new columns for future reference
audited$long_orig <- audited$long
audited$lat_orig <- audited$lat
audited <- audited %>%
  rename("itmLong_orig" = "itmLong",
         "itmLat_orig" = "itmLat")

for(i in 1:nrow(audited)){
  if(audited$todo[i] == "reassign" & !is.na(audited$todo[i])){ # if needs reassignment...
    if(audited$reassign_to[i] %in% reassign_coords$stationName){ # and if we have the coords...
      
      # ...then reassign the lat/long to the station coordinates
      audited$lat[i] <- reassign_coords$lat_fixed[reassign_coords$stationName == audited$reassign_to[i]]
      audited$long[i] <- reassign_coords$long_fixed[reassign_coords$stationName == audited$reassign_to[i]]
      
      # ...and add the correct station name.
      audited$stationName[i] <- audited$reassign_to[i]
      
      # to keep track, make a note that we edited these coords and add an explanation.
      audited$edited_coords[i] <- T
      audited$explanation[i] <- "reassigned to station coords"
    }
  }
}

## turn it back into a spatial object
audited <- st_as_sf(audited, coords = c("long", "lat"), crs = "WGS84", remove = F) %>% st_transform(32636)
mapview(audited, zcol = "stationName", col.regions = custom_colors)

# View progress so far--should have many fewer points out of place
# XXX start here
mapview(stations, label = "stationName", color = "black", col.regions = "black", layer.name = "stations")+
  mapview(audited %>% filter(is.na(edited_coords) & !flag), label = "stationName", color = "gray", col.regions = "gray", layer.name = "ok")+
  mapview(audited %>% filter(edited_coords), label = "stationName", color = "blue", col.regions = "blue", layer.name = "fixed_coords")+
  mapview(audited %>% filter(is.na(edited_coords) & flag), label = "stationName",
          color = "red", col.regions = "red", layer.name = "todo") # the ones we still need to fix

# XXX 2025-09-04--I think we don't need this part.
# # Awesome, this looks great!
# # For any that are exactly duplicated, let's jitter ever so slightly
# set.seed(3)
# for(i in 1:nrow(audited)){
#   if(audited$carcID[i] %in% dup_ids){
#     audited$lat[i] <- audited$lat[i] + runif(1, min = -0.0001, max = 0.0001)
#     audited$long[i] <- audited$long[i] + runif(1, min = -0.0001, max = 0.0001)
#     audited$edited_coords[i] <- T
#     audited$explanation[i] <- "Jittered to deduplicate coords"
#   }
# }
# 
# ## turn it back into a spatial object
# audited <- st_as_sf(sf::st_drop_geometry(audited), coords = c("long", "lat"), crs = "WGS84", remove = F) %>% st_transform(32636)
# XXX returning to this on 2025-09-04--I don't know why I did this--maybe to make the visualization easier? I actually think we don't need to do it for the actual data. It's fine if they're exactly on top of each other. Also got rid of the checks for duplicate coords, because it's fine.

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

# Can take action on assigning the ones to Amiaz, Chail_hills, and Tzvira_plateau. Need help for the others.

# Proceeding with caution, we can use these intersections to guide assignment of carcasses to stations. This is assuming that the stationName was incorrectly chosen, rather than the coordinates being wrong. Let's look at the stations one at a time and make sure the assignment makes sense.
## Amiaz
mapview(audited, label = "stationName", color = "gray", col.regions = "gray") +
mapview(stations, label = "stationName", color = "black", col.regions = "black")+
mapview(intersections$Amiaz, label = "stationName", color = "blue", col.regions = "blue") # everything blue should be reassigned to Amiaz. We have a bunch currently labeled "Other" there. Looks safe to do this--no other station is in the immediate vicinity.

audited$stationName[audited$carcID %in% intersections$Amiaz$carcID] <- "Amiaz"

## Chail_hills
mapview(audited, label = "stationName", color = "gray", col.regions = "gray") +
  mapview(stations, label = "stationName", color = "black", col.regions = "black")+
  mapview(intersections$Chail_hills, label = "stationName", color = "blue", col.regions = "blue") # looks okay! Note that the carcass is overlapping the station, so it's hard to see, but there is a black point below it. Can reassign to Chail_hills.

audited$stationName[audited$carcID %in% intersections$Chail_hills$carcID] <- "Chail_hills"

## Tzvira_plateau
mapview(audited, label = "stationName", color = "gray", col.regions = "gray") +
  mapview(stations, label = "stationName", color = "black", col.regions = "black")+
  mapview(intersections$Tzvira_plateau, label = "stationName", color = "blue", col.regions = "blue") # safe here too 

audited$stationName[audited$carcID %in% intersections$Tzvira_plateau$carcID] <- "Tzvira_plateau"

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

mapview(stations, zcol = "stationName", col.regions = custom_colors)

# Write out the audited carcass data --------------------------------------
carcasses_audited <- audited %>%
  bind_cols(st_coordinates(.))
write_rds(carcasses_audited, file = here("data/created/carcasses_audited.RDS"))
