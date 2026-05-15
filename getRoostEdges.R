function (dataset, mode = "distance", roostPolygons = NULL, 
          distThreshold = 500, latCol = "location_lat", longCol = "location_long", 
          idCol = "Nili_id", dateCol = "date", roostCol = "roostID", 
          crsToSet = "WGS84", crsToTransform = 32636, return = "edges", 
          getLocs = FALSE) 
{
  checkmate::assertDataFrame(dataset)
  checkmate::assertSubset(mode, c("distance", "polygon"), 
                          empty.ok = F)
  if (!missing(roostPolygons) & !is.null(roostPolygons)) {
    checkmate::assertSubset("sf", class(roostPolygons))
    if (is.na(sf::st_crs(roostPolygons))) {
      stop("roostPolygons object must have a valid crs.")
    }
  }
  checkmate::assertNumeric(distThreshold, lower = 0, null.ok = FALSE)
  checkmate::assertCharacter(latCol, len = 1)
  checkmate::assertCharacter(longCol, len = 1)
  checkmate::assertCharacter(idCol, len = 1)
  checkmate::assertCharacter(dateCol, len = 1)
  checkmate::assertCharacter(roostCol, null.ok = T, len = 1)
  checkmate::assertSubset(c(latCol, longCol, idCol, dateCol), 
                          names(dataset))
  checkmate::assertSubset(return, c("edges", "sri", 
                                    "both"))
  checkmate::assertLogical(getLocs, len = 1)
  if (getLocs & return == "sri") {
    warning("Cannot return interaction locations when return = 'sri'. If you want interaction locations, use return = 'edges' or return = 'both'.")
  }
  if (mode == "distance") {
    if ("sf" %in% class(dataset)) {
      if (is.na(sf::st_crs(dataset))) {
        message(paste0("`dataset` is already an sf object but has no CRS. Setting CRS to ", 
                       crsToSet, "."))
        dataset <- sf::st_set_crs(dataset, crsToSet)
      }
    }
    else if (is.data.frame(dataset)) {
      if (nrow(dataset) == 0) {
        stop("Dataset passed to vultureUtils::getRoostEdges has 0 rows. Cannot proceed with grouping.")
      }
      dataset <- dataset %>% sf::st_as_sf(coords = c(longCol, 
                                                     latCol), remove = FALSE) %>% sf::st_set_crs(crsToSet)
    }
    else {
      stop("`dataset` must be a data frame or an sf object.")
    }
    dataset <- dataset %>% sf::st_transform(crsToTransform)
    dataset <- dataset %>% dplyr::mutate(utmE = unlist(purrr::map(dataset$geometry, 
                                                                  1)), utmN = unlist(purrr::map(dataset$geometry, 2))) %>% 
      sf::st_drop_geometry()
    data.table::setDT(dataset)
    edges <- spatsoc::edge_dist(DT = dataset, threshold = distThreshold, 
                                id = idCol, coords = c("utmE", "utmN"), 
                                splitBy = dateCol, timegroup = NULL, fillNA = FALSE, 
                                returnDist = TRUE)
    locs <- dataset %>% tibble::as_tibble() %>% dplyr::select(tidyselect::all_of(c(idCol, 
                                                                                   dateCol, latCol, longCol))) %>% dplyr::distinct() %>% 
      dplyr::mutate(across(tidyselect::all_of(c(latCol, 
                                                longCol)), as.numeric))
    meanLocs <- locs %>% dplyr::group_by(across(all_of(c(idCol, 
                                                         dateCol)))) %>% dplyr::summarize(mnLat = mean(.data[[latCol]], 
                                                                                                       na.rm = T), mnLong = mean(.data[[longCol]], na.rm = T))
    ef <- edges %>% dplyr::left_join(meanLocs, by = c(ID1 = idCol, 
                                                      dateCol)) %>% dplyr::rename(latID1 = mnLat, longID1 = mnLong) %>% 
      dplyr::left_join(meanLocs, by = c(ID2 = idCol, dateCol)) %>% 
      dplyr::rename(latID2 = mnLat, longID2 = mnLong) %>% 
      dplyr::mutate(interactionLat = (latID1 + latID2)/2, 
                    interactionLong = (longID1 + longID2)/2)
    if (!(nrow(edges) == nrow(ef))) {
      stop("Wrong number of rows!")
    }
    edges <- ef
  }
  else {
    if (!(roostCol %in% names(dataset)) & !is.null(roostPolygons)) {
      if ("sf" %in% class(dataset)) {
        if (is.na(sf::st_crs(dataset))) {
          message(paste0("`dataset` is already an sf object but has no CRS. Setting CRS to ", 
                         crsToSet, "."))
          dataset <- sf::st_set_crs(dataset, crsToSet)
        }
      }
      else if (is.data.frame(dataset)) {
        checkmate::assertChoice(latCol, names(dataset))
        checkmate::assertChoice(longCol, names(dataset))
        if (nrow(dataset) == 0) {
          stop("Dataset passed to vultureUtils::getRoostEdges has 0 rows. Cannot proceed with grouping.")
        }
        dataset <- dataset %>% sf::st_as_sf(coords = c(longCol, 
                                                       latCol), remove = FALSE) %>% sf::st_set_crs(crsToSet)
      }
      else {
        stop("`dataset` must be a data frame or an sf object.")
      }
      if (sf::st_crs(dataset) != sf::st_crs(roostPolygons)) {
        dataset <- sf::st_transform(dataset, crs = sf::st_crs(roostPolygons))
      }
      roostPolygons$roostPolygonID <- 1:nrow(roostPolygons)
      roostPolygons <- roostPolygons[, "roostPolygonID"]
      polys <- sf::st_join(dataset, roostPolygons) %>% 
        sf::st_drop_geometry() %>% dplyr::rename(`:=`({
          {
            roostCol
          }
        }, roostPolygonID))
    }
    else if (roostCol %in% names(dataset)) {
      polys <- dataset
    }
    else if (!(roostCol %in% names(dataset)) & is.null(roostPolygons)) {
      stop(paste0("Column `", roostCol, "` not found in dataset, and no roost polygons provided. Must provide either the name of a column containing roost assignments or a valid set of roost polygons."))
    }
    edges <- polys %>% dplyr::filter(!is.na(.data[[roostCol]])) %>% 
      dplyr::group_by(.data[[dateCol]], .data[[roostCol]]) %>% 
      dplyr::group_split(.keep = T) %>% purrr::map_dfr(~{
        tidyr::expand_grid(ID1 = .x[[idCol]], .x)
      }) %>% dplyr::rename(ID2 = {
        {
          idCol
        }
      }) %>% dplyr::filter(ID1 < ID2) %>% dplyr::select(-c("sunrise", 
                                                           "sunset", "sunrise_twilight", "sunset_twilight", 
                                                           "daylight", "is_roost"))
  }
  locsColNames <- c("latID1", "longID1", "latID2", 
                    "longID2", "interactionLat", "interactionLong")
  if (!getLocs & !is.null(edges) & mode == "polygon") {
    edges[[latCol]] <- NULL
    edges[[longCol]] <- NULL
    edges[[roostCol]] <- NULL
  }
  else if (!getLocs & !is.null(edges) & mode != "polygon") {
    edges <- edges %>% dplyr::select(-any_of(locsColNames))
  }
  if (return %in% c("both", "sri")) {
    dfSRI <- calcSRI(dataset = dataset, edges = edges, idCol = idCol, 
                     timegroupCol = dateCol)
    if (return == "sri") {
      return(dfSRI)
    }
    else if (return == "both") {
      return(list(edges = edges, sri = dfSRI))
    }
  }
  else {
    return(edges)
  }
}
<bytecode: 0x00000228644fb940>
  <environment: namespace:vultureUtils>