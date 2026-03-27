library(vultureUtils)
#vultureUtils::cleanData

# The spikyspeedsfilter is where the daylight column seems to be getting introduced in the cleaning. Daylight is being calculated with suncalc using the Jerusalem lat/long values:  lat = 31.434306, lon = 34.991889
spikySpeedsFilter <- function (dataset, idCol, longCol, latCol) 
{
  df <- vultureUtils::calcSpeeds(dataset, grpCol = idCol, longCol = longCol, 
                                 latCol = latCol)
  df2 <- df %>% dplyr::filter(lead_speed_m_s <= 50 & abs(lag_speed_m_s) <= 
                                50) %>% dplyr::select(-c("lead_hour_diff_sec", 
                                                         "lead_dist_m", "lead_speed_m_s", "lag_hour_diff_sec", 
                                                         "lag_dist_m", "lag_speed_m_s"))
  df2 <- vultureUtils::calcSpeeds(df2, grpCol = idCol, longCol = longCol, 
                                  latCol = latCol)
  df3 <- df2 %>% dplyr::filter(lead_speed_m_s <= 50)
  spikySpeeds <- getStats(df3, idCol)
  times <- suncalc::getSunlightTimes(date = unique(lubridate::date(df3$timestamp)), 
                                     lat = 31.434306, lon = 34.991889, keep = c("sunrise", 
                                                                                "sunset")) %>% dplyr::select(dateOnly = date, 
                                                                                                             sunrise, sunset)
  df4 <- df3 %>% dplyr::mutate(dateOnly = lubridate::ymd(dateOnly)) %>% 
    dplyr::left_join(times, by = "dateOnly") %>% dplyr::mutate(daylight = ifelse(timestamp >= 
                                                                                   sunrise & timestamp <= sunset, "day", "night")) %>% 
    dplyr::select(-c(sunrise, sunset))
  df4 <- vultureUtils::calcSpeeds(df4, grpCol = idCol, longCol = longCol, 
                                  latCol = latCol)
  df5 <- df4 %>% dplyr::mutate(day_diff = as.numeric(difftime(dplyr::lead(lubridate::date(timestamp)), 
                                                              lubridate::date(timestamp), units = "days")), night_outlier = ifelse(daylight == 
                                                                                                                                     "night" & day_diff %in% c(0, 1) & dplyr::lead(daylight) == 
                                                                                                                                     "night" & lead_dist_m > 10000, T, F)) %>% dplyr::filter(!night_outlier) %>% 
    dplyr::select(-c("lead_hour_diff_sec", "lag_hour_diff_sec", 
                     "lead_dist_m", "lag_dist_m", "lead_speed_m_s", 
                     "lag_speed_m_s"))
  dataset <- df5
  list(dataset = dataset, spikySpeeds = spikySpeeds)
}

  
function (dataset, gpsMaxTime = -1, precise = F, longCol = "location_long.1", 
          latCol = "location_lat.1", idCol = "Nili_id", 
          removeVars = T, report = T, ...) 
{
  checkmate::assertDataFrame(dataset)
  checkmate::assertCharacter(longCol, len = 1)
  checkmate::assertCharacter(latCol, len = 1)
  checkmate::assertChoice("gps_time_to_fix", names(dataset))
  checkmate::assertChoice("heading", names(dataset))
  checkmate::assertChoice("gps_satellite_count", names(dataset))
  checkmate::assertChoice("ground_speed", names(dataset))
  checkmate::assertChoice("external_temperature", names(dataset))
  checkmate::assertChoice("barometric_height", names(dataset))
  checkmate::assertClass(dataset$timestamp, "POSIXct")
  filterNames <- c("Input data")
  reportData <- data.frame()
  init <- getStats(dataset, idCol)
  reportData <- dplyr::bind_rows(reportData, init)
  dataset <- vultureUtils::tempHeightSpeedFilter(dataset)
  outliers <- getStats(dataset, idCol)
  reportData <- dplyr::bind_rows(reportData, outliers)
  filterNames <- append(filterNames, "Removed outliers with zeroes in three columns")
  if (gpsMaxTime > 0) {
    dataset <- vultureUtils::gpsTimeFilter(dataset, maxTime = gpsMaxTime)
    badTimeToFix <- getStats(dataset, idCol)
    reportData <- dplyr::bind_rows(reportData, badTimeToFix)
    filterNames <- append(filterNames, "Removed points that took too long to get GPS fix")
  }
  dataset <- vultureUtils::headingFilter(dataset)
  badHeading <- getStats(dataset, idCol)
  reportData <- dplyr::bind_rows(reportData, badHeading)
  filterNames <- append(filterNames, "Removed points with invalid heading data")
  dataset <- vultureUtils::satelliteFilter(dataset, minSatellites = 3)
  badSatellites <- getStats(dataset, idCol)
  reportData <- dplyr::bind_rows(reportData, badSatellites)
  filterNames <- append(filterNames, "Removed points with too few satellites")
  if (precise) {
    dataset <- vultureUtils::preciseFilter(dataset)
    lowQualityPoints <- getStats(dataset, idCol)
    reportData <- dplyr::bind_rows(reportData, lowQualityPoints)
    filterNames <- append(filterNames, "Removed points with < 4 satellites and hdop > 5")
  }
  values <- vultureUtils::spikySpeedsFilter(dataset, idCol = idCol, 
                                            longCol = longCol, latCol = latCol)
  dataset <- values$dataset
  spikySpeeds <- values$spikySpeeds
  nightDistance <- getStats(dataset, idCol)
  reportData <- dplyr::bind_rows(reportData, spikySpeeds)
  reportData <- dplyr::bind_rows(reportData, nightDistance)
  filterNames <- append(filterNames, "Removed spiky speeds")
  filterNames <- append(filterNames, "Removed points that moved too far at night")
  values <- vultureUtils::spikyAltitudesFilter(dataset, idCol = idCol)
  dataset <- values$dataset
  nAltitudesToNA <- values$nAltitudesToNA
  if (removeVars == T) {
    varsRemoved <- vultureUtils::removeUnnecessaryVars(dataset)
    nColsRemoved <- ncol(dataset) - ncol(varsRemoved)
    dataset <- varsRemoved
  }
  final <- getStats(dataset, idCol)
  reportData <- dplyr::bind_rows(reportData, final)
  filterNames <- append(filterNames, "Final")
  if (report) {
    steps <- filterNames
    df <- reportData %>% dplyr::mutate(step = steps) %>% 
      dplyr::relocate(step) %>% dplyr::mutate(rowsLost = dplyr::lag(rows) - 
                                                rows, propRowsLost = round(rowsLost/dplyr::lag(rows), 
                                                                           3))
    print(df)
  }
  out <- dataset
  return(out %>% dplyr::ungroup())
}
<bytecode: 0x000002694d97d0a8>
  <environment: namespace:vultureUtils>