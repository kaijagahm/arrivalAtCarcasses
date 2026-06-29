library(tidyverse)
library(targets)

tar_load(roosts_stn)
tar_load(roost_wild)
tar_load(roosts_all)
tar_load(rp_minus_stations)
tar_load(downsampled_forroosts)
gps <- downsampled_forroosts %>%
  sf::st_as_sf(., crs = 32636) %>%
  bind_cols(st_coordinates(.))

# Manual calculation of co-departures from roosts and following
roosts_all_updated <- mutate(roosts_all, roostID = as.numeric(st_intersects(sf::st_transform(roosts_all, 32636), rp_minus_stations)))

roosts_tojoin <- roosts_all_updated %>%
  select(individual_local_identifier, roost_date, roostID) %>%
  sf::st_transform(32636) %>%
  bind_cols(., sf::st_coordinates(.)) %>%
  rename("roost_X" = X, "roost_Y" = Y)

test <- gps %>%
  arrange(individual_local_identifier, date_il) %>%
  mutate(roost_date = date_il-lubridate::days(1)) %>%
  st_drop_geometry() %>%
  left_join(st_drop_geometry(roosts_tojoin), by = c("individual_local_identifier", "roost_date")) %>%
  filter(!is.na(roost_X))
glimpse(test)

test <- test %>%
  mutate(dist_from_roost = sqrt((roost_Y-Y)^2/(roost_X-X)^2)) #huh? why isn't this coming out as m?
test %>%
  filter(date_il == unique(test$date_il)[5], individual_local_identifier == unique(test$individual_local_identifier)[11]) %>%
  ggplot(aes(x = timestamp_il, y = dist_from_roost))+
  geom_point()+
  geom_line() # units are weird here.




gps_joined <- dplyr::mutate(dplyr::left_join(dplyr::mutate(downsampled_updated, roost_date = date_il-lubridate::days(1)), roosts_tojoin, by = c("individual_local_identifier", "roost_date")), in_a_roost = !is.na(roostID_gps)) # joined roosts to GPS data to prep for determining departures

# tar_target(gps_joined_knownroost, dplyr::filter(gps_joined, !is.na(roostID))), # only roost polygons
# tar_target(indiv_date_list, group_split(group_by(gps_joined_knownroost, date_il, individual_local_identifier), .keep = T)),
# tar_target(leftpoints, purrr::map_dbl(indiv_date_list, ~get_leftroost(.x, threshold = 2))),
# tar_target(data_timeordered, purrr::map2(indiv_date_list, leftpoints, ~{
#   .x$left_roost <- FALSE
#   if(!is.na(.y)){.x$left_roost[.y] <- TRUE}
#   return(.x)})),
# tar_target(data_rejoined, sf::st_as_sf(as.data.frame(data.table::rbindlist(data_timeordered)), crs = 32636)),
# tar_target(leaving_points, dplyr::filter(data_rejoined, left_roost)),
# tar_target(leaving_points_dates, group_split(group_by(leaving_points, date_il), .keep = T)),
# tar_target(dates, purrr::map_chr(leaving_points_dates, ~as.character(.x$date_il[1]))),
# tar_target(roost_mats, setNames(purrr::map(leaving_points_dates, ~{
#   mat <- outer(.x$roostID, .x$roostID, FUN = "==") * 1
#   rownames(mat) <- .x$individual_local_identifier
#   colnames(mat) <- .x$individual_local_identifier
#   return(mat)}), dates)),
# tar_target(roost_mats_long, setNames(purrr::map(roost_mats, ~{as.data.frame(.x) %>% rownames_to_column("ID1") %>% pivot_longer(cols = -ID1, names_to = "ID2", values_to = "same_roost")}), dates)),
# tar_target(roost_mats_same_whichroost, filter(left_join(mutate(purrr::list_rbind(roost_mats_long, names_to = "date_il"), date_il = lubridate::ymd(date_il)), leaving_points, by = c("ID1" = "individual_local_identifier", "date_il")), same_roost == 1)),
# tar_target(difftime_mats, setNames(purrr::map(leaving_points_dates, ~{
#   mat <- outer(.x$timestamp_il, .x$timestamp_il,
#                function(t1, t2) as.numeric(abs(difftime(t1, t2, units = "mins"))))
#   rownames(mat) <- .x$individual_local_identifier
#   colnames(mat) <- .x$individual_local_identifier
#   return(mat)}), dates)),
# tar_target(difftime_mats_long, setNames(purrr::map(difftime_mats, ~{as.data.frame(.x) %>% rownames_to_column("ID1") %>% pivot_longer(cols = -ID1, names_to = "ID2", values_to = "time_diff_min")}), dates)),
# tar_target(both, setNames(purrr::map2(roost_mats_long, difftime_mats_long, ~dplyr::left_join(.x, .y, by = c("ID1", "ID2"))), dates)),
# tar_target(departure_times, setNames(purrr::map(both, ~{dplyr::filter(.x, same_roost == 1) %>% dplyr::select(-same_roost) %>% filter(ID1 < ID2)}), dates)),
# tar_target(sync_departures, setNames(purrr::map(departure_times, ~filter(.x, time_diff_min <= 10)), dates)),
