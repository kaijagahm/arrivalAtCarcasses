getEdges <- function(dataset,
                     roostPolygons = NULL,
                     roostBuffer,
                     consecThreshold,
                     distThreshold,
                     speedThreshUpper,
                     speedThreshLower,
                     timeThreshold = "10 minutes",
                     idCol = "Nili_id",
                     quiet = T,
                     includeAllVertices = F,
                     daytimeOnly = T,
                     return = "edges",
                     getLocs = FALSE,
                     speedCol = "ground_speed",
                     timestampCol = "timestamp",
                     filterFirst = FALSE){
  
  # ARGUMENT CHECKS and housekeeping
  if(TRUE){
    checkmate::assertDataFrame(dataset)
    checkmate::assertSubset("sf", class(dataset))
    checkmate::assertClass(roostPolygons, "sf", null.ok = TRUE)
    checkmate::assertNumeric(roostBuffer, len = 1, null.ok = TRUE)
    checkmate::assertNumeric(consecThreshold, len = 1)
    checkmate::assertNumeric(distThreshold, len = 1)
    checkmate::assertNumeric(speedThreshUpper, len = 1, null.ok = TRUE)
    checkmate::assertNumeric(speedThreshLower, len = 1, null.ok = TRUE)
    checkmate::assertCharacter(timeThreshold, len = 1)
    checkmate::assertLogical(daytimeOnly, len = 1)
    checkmate::assertSubset(return, choices = c("edges", "sri", "both"),
                            empty.ok = FALSE)
    checkmate::assertSubset("timestamp", names(dataset)) # for sunrise/sunset calculations.
    checkmate::assertSubset("dateOnly", names(dataset)) # for sunrise/sunset calculations
    checkmate::assertSubset("location_lat", names(dataset)) # passed to spaceTimeGroups. XXX fix with GH#58
    checkmate::assertSubset("location_long", names(dataset)) # passed to spaceTimeGroups. XXX fix with GH#58
    checkmate::assertSubset(idCol, names(dataset)) # passed to spaceTimeGroups.
    checkmate::assertLogical(getLocs, len = 1)
    # Only require ground_speed column when filtering by speed
    if(!is.null(c(speedThreshLower, speedThreshUpper))){
      checkmate::assertSubset(speedCol, names(dataset)) # necessary for filterLocs.
    }
    
    # Message about getLocs and sri
    if(getLocs & return == "sri"){
      warning("Cannot return interaction locations when return = 'sri'. If you want interaction locations, use return = 'edges' or return = 'both'.")
    }
    
    # Get all unique individuals before applying any filtering
    if(includeAllVertices){
      uniqueIndivs <- unique(dataset[[idCol]])
    }
  }
  
  dim## SPEED FILTER
  # Restrict interactions based on ground speed
  filteredData <- vultureUtils::filterLocs(df = dataset,
                                           speedThreshUpper = speedThreshUpper,
                                           speedThreshLower = speedThreshLower, speedCol = speedCol)
  
  # ROOST FILTER
  # If roost polygons were provided, use them to filter out data
  if(!is.null(roostPolygons)){
    # Buffer the roost polygons
    
    if(!is.null(roostBuffer)){
      roostPolygons <- convertAndBuffer(roostPolygons, dist = roostBuffer)
    }
    # Exclude any points that fall within a (buffered) roost polygon
    points <- filteredData[lengths(sf::st_intersects(filteredData, roostPolygons)) == 0,]
  }else{
    message("No roost polygons provided; points will not be filtered by spatial intersection.")
    points <- filteredData
  }
  
  # DAYLIGHT FILTER
  # Restrict based on daylight
  if(daytimeOnly){
    times <- suncalc::getSunlightTimes(date = unique(lubridate::date(points$timestamp)), lat = 31.434306, lon = 34.991889,
                                       keep = c("sunrise", "sunset")) %>%
      dplyr::select(date, sunrise, sunset) # XXX the coordinates I'm using here are from the centroid of Israel calculated here: https://rona.sh/centroid. This is just a placeholder until we decide on a more accurate way of doing this.
    points <- points %>%
      # remove leftover sunrise/sunset cols just in case
      {if("sunrise" %in% names(.)) dplyr::select(., -sunrise) else .}%>%
      {if("sunset" %in% names(.)) dplyr::select(., -sunset) else .}%>%
      dplyr::left_join(times, by = c("dateOnly" = "date")) %>%
      dplyr::mutate(daytime = dplyr::case_when(timestamp > .data[["sunrise"]] &
                                                 timestamp < .data[["sunset"]] ~ T,
                                               TRUE ~ F))
    
    # Filter out nighttimes
    nNightPoints <- nrow(points[points$daytime == F,])
    points <- points %>%
      dplyr::filter(daytime == T)
    nDayPoints <- nrow(points)
    if(quiet == F){
      cat(paste0("Removed ", nNightPoints, " nighttime points, leaving ",
                 nDayPoints, " points.\n"))
    }
  }
  
  # EMPTY POINTS HOUSEKEEPING
  # If there are no rows left after filtering, create an empty data frame with the appropriate format.
  if(nrow(points) == 0){
    # DUMMY EDGELIST, NO SRI TO COMPUTE
    out <- data.frame(timegroup = as.integer(),
                      ID1 = as.character(),
                      ID2 = as.character(),
                      distance = as.numeric(),
                      minTimestamp = as.POSIXct(character()),
                      maxTimestamp = as.POSIXct(character()))
    warning("After filtering, the dataset had 0 rows.")
  }
  
  ## GET EDGELIST (optionally compute SRI)
  if(nrow(points) != 0){
    ## Do we need to compute SRI?
    if(return == "edges"){ # if SRI is not needed, we can save time by not computing it.
      if(quiet){
        ### EDGES ONLY, QUIET
        out <- suppressMessages(suppressWarnings(vultureUtils::spaceTimeGroups(dataset = points,
                                                                               distThreshold = distThreshold,
                                                                               consecThreshold = consecThreshold,
                                                                               timeThreshold = timeThreshold,
                                                                               sri = FALSE,
                                                                               idCol = idCol)))
      }else{
        ### EDGES ONLY, WARNINGS
        # compute edges without suppressing warnings
        out <- vultureUtils::spaceTimeGroups(dataset = points,
                                             distThreshold = distThreshold,
                                             consecThreshold = consecThreshold,
                                             timeThreshold = timeThreshold,
                                             sri = FALSE,
                                             idCol = idCol)
      }
      
    }else if(return %in% c("sri", "both")){ # otherwise we need to compute SRI.
      if(quiet){
        ### EDGES AND SRI, QUIET
        # suppress warnings while computing edges and SRI, returning a list of edges+sri
        out <- suppressMessages(suppressWarnings(vultureUtils::spaceTimeGroups(dataset = points,
                                                                               distThreshold = distThreshold,
                                                                               consecThreshold = consecThreshold,
                                                                               timeThreshold = timeThreshold,
                                                                               sri = TRUE,
                                                                               idCol = idCol)))
        if(return == "sri"){
          out <- out["sri"]
        }
      }else{
        ### EDGES AND SRI, WARNINGS
        # compute edges and SRI without suppressing warnings, returning a list of edges+sri
        out <- vultureUtils::spaceTimeGroups(dataset = points,
                                             distThreshold = distThreshold,
                                             consecThreshold = consecThreshold,
                                             timeThreshold = timeThreshold,
                                             sri = TRUE,
                                             idCol = idCol)
        if(return == "sri"){
          out <- out["sri"]
        }
      }
    }
  }
  
  # HOUSEKEEPING--RETURNS
  locsColNames <- c("latID1", "longID1", "latID2", "longID2", "interactionLat", "interactionLong")
  if(!getLocs & return %in% c("edges", "both")){
    if(!is.list(out)){
      out <- out %>%
        dplyr::select(-any_of(locsColNames))
    }else{
      if("edges" %in% names(out)){
        out$edges <- out$edges %>%
          dplyr::select(-any_of(locsColNames))
      }else{
        out <- out %>%
          dplyr::select(-any_of(locsColNames))
      }
    }
  }
  
  ## APPEND VERTICES
  if(includeAllVertices){
    toReturn <- append(out, list(as.character(uniqueIndivs)))
  }else{
    toReturn <- out
  }
  
  # If the list only has one object, unlist it one level down.
  if(length(toReturn) == 1){
    toReturn <- toReturn[[1]]
  }
  
  ## RETURN LIST
  return(toReturn)
}

getEdges_EDB <- function(dataset,
                         roostPolygons = NULL,
                         roostBuffer,
                         consecThreshold,
                         distThreshold,
                         speedThreshUpper,
                         speedThreshLower,
                         timeThreshold = "10 minutes",
                         idCol = "Nili_id",
                         quiet = T,
                         includeAllVertices = F,
                         daytimeOnly = T,
                         return = "edges",
                         getLocs = FALSE,
                         speedCol = "ground_speed",
                         timestampCol = "timestamp",
                         filterFirst = FALSE,
                         ids_all = NULL){
  
  # ARGUMENT CHECKS and housekeeping
  if(TRUE){
    checkmate::assertDataFrame(dataset)
    # checkmate::assertSubset("sf", class(dataset))
    checkmate::assertClass(roostPolygons, "sf", null.ok = TRUE)
    checkmate::assertNumeric(roostBuffer, len = 1, null.ok = TRUE)
    checkmate::assertNumeric(consecThreshold, len = 1)
    checkmate::assertNumeric(distThreshold, len = 1)
    checkmate::assertNumeric(speedThreshUpper, len = 1, null.ok = TRUE)
    checkmate::assertNumeric(speedThreshLower, len = 1, null.ok = TRUE)
    checkmate::assertCharacter(timeThreshold, len = 1)
    checkmate::assertLogical(daytimeOnly, len = 1)
    checkmate::assertSubset(return, choices = c("edges", "sri", "both"),
                            empty.ok = FALSE)
    checkmate::assertSubset("timestamp", names(dataset)) # for sunrise/sunset calculations.
    checkmate::assertSubset("dateOnly", names(dataset)) # for sunrise/sunset calculations
    checkmate::assertSubset("location_lat", names(dataset)) # passed to spaceTimeGroups. XXX fix with GH#58
    checkmate::assertSubset("location_long", names(dataset)) # passed to spaceTimeGroups. XXX fix with GH#58
    checkmate::assertSubset(idCol, names(dataset)) # passed to spaceTimeGroups.
    checkmate::assertLogical(getLocs, len = 1)
    # Only require ground_speed column when filtering by speed
    if(!is.null(c(speedThreshLower, speedThreshUpper))){
      checkmate::assertSubset(speedCol, names(dataset)) # necessary for filterLocs.
    }
    
    # Message about getLocs and sri
    if(getLocs & return == "sri"){
      warning("Cannot return interaction locations when return = 'sri'. If you want interaction locations, use return = 'edges' or return = 'both'.")
    }
    
    # Get all unique individuals before applying any filtering
    if(includeAllVertices){
      uniqueIndivs <- unique(dataset[[idCol]])
    }
    
    #------------------------------------------------------------
    #SAVE RAW DATASET COPY
    #------------------------------------------------------------
    dataset_rawdata <- dataset  #backup raw dataset
    
    #------------------------------------------------------------
    #CONVERT timestampCol to POSIXct datetime
    #------------------------------------------------------------
    dataset <- dataset %>%
      dplyr::mutate({{ timestampCol }} := as.POSIXct(.data[[timestampCol]], format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
    
    #------------------------------------------------------------
    #Convert dataset to data.table (needed for spatsoc functions)
    #------------------------------------------------------------
    data.table::setDT(dataset)
  }
  
  #------------------------------------------------------------
  #GROUP POINTS INTO TIMEGROUPS (using spatsoc)
  #------------------------------------------------------------
  # XXX KG: I don't understand why we are doing this now if we're going to also pass it through spaceTimeGroups later.
  if(TRUE){
    dataset <- spatsoc::group_times(dataset, datetime = timestampCol, threshold = timeThreshold)
    
    dataset_denominator <- dataset  #save a copy for SRI denominator
    
    # record start/end time per timegroup
    timegroupData <- dataset %>%
      dplyr::select(tidyselect::all_of(timestampCol), timegroup) %>%
      dplyr::group_by(timegroup) %>%
      dplyr::summarize(minTimestamp = min(.data[[timestampCol]], na.rm = TRUE),
                       maxTimestamp = max(.data[[timestampCol]], na.rm = TRUE))
  }
  
  #------------------------------------------------------------
  #CONVERT TO sf OBJECT WITH LAT/LONG GEOMETRY
  #(needed for spatial filtering later)
  #------------------------------------------------------------
  if(TRUE){ # this is purely for grouping related code into a chunk
    dataset_sf <- sf::st_as_sf(dataset, coords = c("location_long", "location_lat"), crs = "WGS84", remove = FALSE)
    
    # validate CRS/convert to sf if needed
    if ("sf" %in% class(dataset_sf)) {
      if (is.na(sf::st_crs(dataset_sf))) {
        message(paste0("`dataset_sf` has no CRS. Setting CRS to WGS84."))
        dataset_sf <- sf::st_set_crs(dataset_sf, "WGS84")
      }
    } else if (is.data.frame(dataset_sf)) {
      checkmate::assertChoice("location_lat", names(dataset_sf))
      checkmate::assertChoice("location_long", names(dataset_sf))
      if (nrow(dataset_sf) == 0) stop("dataset_sf has 0 rows.")
      dataset_sf <- dataset_sf %>%
        sf::st_as_sf(coords = c(.data[["location_long"]], .data[["location_lat"]]), remove = FALSE) %>%
        sf::st_set_crs("WGS84")
    } else {
      stop("`dataset_sf` must be a data frame or sf object.")
    }
  }
  
  #------------------------------------------------------------
  #FILTER OUT POINTS INSIDE ROOST POLYGONS
  #------------------------------------------------------------
  if(TRUE){ # removed conversion to sf object, since we should have handled that already in the previous part.
    if (!is.null(roostPolygons)) {
      if (!is.null(roostBuffer)) {
        roostPolygons <- convertAndBuffer(roostPolygons, dist = roostBuffer)  #buffer polygons
      }
      removedRoosts_dataset <- dataset_sf[lengths(sf::st_intersects(dataset_sf, roostPolygons)) == 0, ]
    } else {
      message("No roost polygons provided; skipping spatial exclusion.")
      removedRoosts_dataset <- dataset_sf
    }
  }
  
  #------------------------------------------------------------
  #FILTER FOR DAYTIME ONLY (if requested)
  #------------------------------------------------------------
  if (daytimeOnly) {
    times <- suncalc::getSunlightTimes(date = unique(lubridate::date(removedRoosts_dataset$timestamp)),
                                       lat = 31.434306, lon = 34.991889,
                                       keep = c("sunrise", "sunset")) %>%
      dplyr::select(date, sunrise, sunset)
    
    removedRoosts_dataset <- removedRoosts_dataset %>%
      #remove leftover sunrise/sunset cols just in case
      {if("sunrise" %in% names(.)) dplyr::select(., -sunrise) else .}%>%
      {if("sunset" %in% names(.)) dplyr::select(., -sunset) else .}%>%
      dplyr::left_join(times, by = c("dateOnly" = "date")) %>%
      dplyr::mutate(daytime = dplyr::case_when(timestamp > .data$sunrise &
                                                 timestamp < .data$sunset ~ TRUE,
                                               TRUE ~ FALSE))
    
    #Filter out nighttimes
    nNightPoints <- nrow(removedRoosts_dataset[removedRoosts_dataset$daytime == F,])
    dataset_dayonly <- removedRoosts_dataset %>%
      dplyr::filter(daytime == TRUE)
    nDayPoints <- nrow(dataset_dayonly)
    if(quiet == FALSE){
      cat(paste0("Removed ", nNightPoints, " nighttime points, leaving ",
                 nDayPoints, " points.\n"))
    }
  }
  
  #------------------------------------------------------------
  #FILTER BASED ON SPEED (if thresholds set)
  #------------------------------------------------------------
  filteredData <- filterLocs(df = dataset_dayonly,
                             speedThreshUpper = speedThreshUpper,
                             speedThreshLower = speedThreshLower,
                             speedCol = speedCol)
  
  #------------------------------------------------------------
  #BUILD ALL POSSIBLE PAIRS (DYADS) FOR THE DATASET
  #------------------------------------------------------------
  # In order to calculate accurate SRI values, we need to know which individuals are missing.
  # First, we assemble a complete list of all dyads based on the individuals in the full dataset (after doing all the preliminary filters, such as daylight)
  ids_dataset <- as.character(unique(dataset[[idCol]]))
  pairs_dataset <- expand.grid(ID1 = ids_dataset, ID2 = ids_dataset, stringsAsFactors = FALSE) %>%
    dplyr::mutate(pair = paste(ID1, ID2, sep = "_")) %>%
    dplyr::filter(ID1 != ID2)
  
  # Next, we do the same with all the individuals we want to include in the analysis (e.g. all tagged, all in the whole season, etc). These are passed in as a separate vector, since they might be coming from a different dataset. If not specified, this part will be skipped.
  if(!is.null(ids_all)){
    pairs_all <- expand.grid(ID1 = ids_all, ID2 = ids_all, stringsAsFactors = FALSE) %>%
      dplyr::mutate(pair = paste(ID1, ID2, sep = "_")) %>%
      dplyr::filter(ID1 != ID2) %>%
      mutate(sri = NA)
  }
  
  #------------------------------------------------------------
  #CALL spaceTimeGroups() FUNCTION
  #- return either edges or SRI
  #------------------------------------------------------------
  if(nrow(filteredData) != 0){
    if(return == "edges"){ #if SRI is not needed, don't compute it (saves time)
      if(quiet){
        ###EDGES ONLY, QUIET
        out <- suppressMessages(suppressWarnings(
          spaceTimeGroups_EDB(dataset = filteredData, # XXX have to make this function now
                              sriDenominatorDataset = dataset_denominator,
                              distThreshold = distThreshold,
                              pairs_all = pairs_all,
                              pairs_dataset = pairs_dataset,
                              consecThreshold = consecThreshold,
                              timeThreshold = timeThreshold,
                              sri = FALSE,
                              idCol = idCol,
                              timegroupData = timegroupData)))
      }else{
        ###EDGES ONLY, WARNINGS
        #compute edges without suppressing warnings
        out <- spaceTimeGroups_EDB(dataset = filteredData, # XXX have to make this function now
                                   sriDenominatorDataset = dataset_denominator,
                                   distThreshold = distThreshold,
                                   pairs_all = pairs_all,
                                   pairs_dataset = pairs_dataset,
                                   consecThreshold = consecThreshold,
                                   timeThreshold = timeThreshold,
                                   sri = FALSE,
                                   idCol = idCol,
                                   timegroupData = timegroupData)
      }
      
    }else if(return %in% c("sri", "both")){
      if(quiet){
        ###EDGES AND SRI, QUIET
        #suppress warnings while computing edges and SRI, returning a list of edges+sri
        out <- suppressMessages(suppressWarnings(
          spaceTimeGroups_EDB(dataset = filteredData,
                              sriDenominatorDataset = dataset_denominator,
                              distThreshold = distThreshold,
                              pairs_all = pairs_all,
                              pairs_dataset = pairs_dataset,
                              consecThreshold = consecThreshold,
                              timeThreshold = timeThreshold,
                              sri = TRUE,
                              idCol = idCol,
                              timegroupData = timegroupData)))
        if(return == "sri"){
          out <- out["sri"]
        }
      }else{
        ###EDGES AND SRI, WARNINGS
        #compute edges and SRI without suppressing warnings, returning a list of edges+sri
        out <- spaceTimeGroups_EDB(dataset = filteredData,
                                   sriDenominatorDataset = dataset_denominator, #XXX added this
                                   distThreshold = distThreshold,
                                   pairs_all = pairs_all,
                                   pairs_dataset = pairs_dataset,
                                   consecThreshold = consecThreshold,
                                   timeThreshold = timeThreshold,
                                   sri = TRUE,
                                   idCol = idCol,
                                   timegroupData = timegroupData)
        if(return == "sri"){
          out <- out["sri"]
        }
      }
    }
  } #close the if(nrow(filteredData) != 0)
  
  #------------------------------------------------------------
  #HANDLE EMPTY DATA AFTER FILTERING # JJJ need to test this--not sure it actually works
  #------------------------------------------------------------
  if (nrow(filteredData) == 0) {
    warning("After filtering, no data remains.")
    all_ids_day_filteredData <- unique(dataset_rawdata[[idCol]])
    allPairs_day_filteredData <- expand.grid(ID1 = all_ids_day_filteredData, ID2 = all_ids_day_filteredData, stringsAsFactors = FALSE) %>%
      dplyr::mutate(pair_filteredData = paste(ID1, ID2, sep = "_"), sri = NA) %>%
      dplyr::filter(ID1 != ID2)
    
    if (nrow(allPairs_day_filteredData) == 0) {
      allPairs_day_filteredData <- allPairs_entire_season_output %>% dplyr::select(ID1, ID2, sri)
    } else {
      matching_indices <- allPairs_day_filteredData$pair_filteredData %in% allPairs_entire_season_output$pair
      allPairs_day_filteredData$sri[matching_indices] <- 0
    }
    
    return(data.frame(ID1 = allPairs_day_filteredData$ID1,
                      ID2 = allPairs_day_filteredData$ID2,
                      sri = allPairs_day_filteredData$sri))
  }
  
  #------------------------------------------------------------
  #REMOVE LOCATION COLUMNS IF getLocs = FALSE
  #------------------------------------------------------------
  locsColNames <- c("latID1", "longID1", "latID2", "longID2", "interactionLat", "interactionLong")
  if (!getLocs & return %in% c("edges", "both")) {
    if (!is.list(out)) {
      out <- dplyr::select(out, -any_of(locsColNames))
    } else if ("edges" %in% names(out)) {
      out$edges <- dplyr::select(out$edges, -any_of(locsColNames))
    }
  }
  
  #------------------------------------------------------------
  #OPTIONALLY APPEND VERTEX LIST
  #------------------------------------------------------------
  if (includeAllVertices) {
    toReturn <- append(out, list(as.character(uniqueIndivs)))
  } else {
    toReturn <- out
  }
  
  #------------------------------------------------------------
  #RETURN FINAL OUTPUT
  #------------------------------------------------------------
  if (length(toReturn) == 1) toReturn <- toReturn[[1]]
  return(toReturn)
}