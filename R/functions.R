get_loginObject <- function(pw){
  load(pw)
  loginObject <- move::movebankLogin(username = "kaijagahm", password = pw)
  rm(pw)
  return(loginObject)
}
# get_inpa <- function(loginObject){
#   inpa <- move::getMovebankData(study = 6071688, 
#                                 login = loginObject, 
#                                 removeDuplicatedTimestamps = TRUE,
#                                 timestamp_start = "2020010100000",
#                                 timestamp_end = "2021021500000")
#   inpa <- methods::as(inpa, "data.frame")
#   inpa <- inpa %>%
#     mutate(dateOnly = lubridate::ymd(substr(timestamp, 1, 10)),
#            year = as.numeric(lubridate::year(timestamp)))
#   return(inpa)
# }

get_ornitela <- function(loginObject){
  minDate <- "2023-06-10 00:00" # two days after 6/8/23, when tags were set to high res
  maxDate <- "2023-09-09 11:59" # two days before 9/11/23, when tags were returned to normal
  ornitela <- vultureUtils::downloadVultures(loginObject = loginObject, 
                                             removeDup = T, dfConvert = T, 
                                             quiet = T, 
                                             dateTimeStartUTC = minDate, 
                                             dateTimeEndUTC = maxDate)
  return(ornitela)
}

fix_names <- function(ornitela, ww_file){
  ww <- read_excel(ww_file, sheet = "all gps tags")
  # pull out just the names columns, nothing else, and remove any duplicates
  ww_tojoin <- ww %>% dplyr::select(Nili_id, Movebank_id) %>% distinct() 
  
  # Prepare for join: are there any individuals in the `local_identifier` column of `joined0` that don't appear in the `Movebank_id` column of `ww_tojoin`?
  problems <- ornitela %>% filter(!(local_identifier %in% ww_tojoin$Movebank_id)) %>% pull(local_identifier) %>% unique()
  problems #let's check these against the who's who and see if we can make some reasonable changes.
  
  ## Fixes:
  # Typo in the Movebank_id column of the who's who:
  ww_tojoin <- ww_tojoin %>% mutate(Movebank_id = case_when(Movebank_id == "A65 Whiite" ~ "A65 White",
                                                            .default = Movebank_id))
  # Fixes to ornitela:
  ornitela <- ornitela %>%
    mutate(local_identifier = case_when(local_identifier == "E86 White" ~ "E86",
                                        local_identifier == "E88 White" ~ "E88w",
                                        .default = local_identifier))
  
  ## Look for any remaining problems:
  problems <- ornitela %>% filter(!(local_identifier %in% ww_tojoin$Movebank_id)) %>% pull(local_identifier) %>% unique()
  problems #going to fix both of these afterward. E66 isn't listed in the Who's who at all, so we'll just call it "E66" in the Nili_id. The other one, Y01>T60 W, I've manually determined is Nili_id "tammy".
  
  # join by movebank ID
  joined <- left_join(ornitela, ww_tojoin, 
                      by = c("local_identifier" = "Movebank_id"))
  joined <- joined %>%
    mutate(Nili_id = case_when(is.na(Nili_id) & local_identifier == "E66 White" ~ "E66",
                               is.na(Nili_id) & local_identifier == "Y01>T60 W" ~ "tammy",
                               .default = Nili_id))
  
  # Are there any remaining NA's for Nili_id?
  nas <- joined %>% filter(is.na(Nili_id)) %>% pull(local_identifier) %>% unique()
  length(nas) # yay, no more!
  return(joined)
}

remove_periods <- function(ww_file, fixed_names){
  periods_to_remove <- read_excel(ww_file, sheet = "periods_to_remove")
  removed_periods <- vultureUtils::removeInvalidPeriods(dataset = fixed_names, periodsToRemove = periods_to_remove)
  return(removed_periods)
}

clean_data <- function(removed_periods){
  cleaned <- vultureUtils::cleanData(dataset = removed_periods,
                                     precise = F,
                                     longCol = "location_long",
                                     latCol = "location_lat",
                                     idCol = "Nili_id",
                                     report = F)
  return(cleaned)
}

remove_captures <- function(capture_sites, carmel, cleaned){
  cs <- read.csv(capture_sites)
  cml <- read.csv(carmel)
  removed_captures <- vultureUtils::removeCaptures(data = cleaned, 
                                     captureSites = cs, 
                                     AllCarmelDates = cml, 
                                     distance = 500, idCol = "Nili_id")
  return(removed_captures)
}

attach_age_sex <- function(removed_captures, ww_file){
  age_sex <- read_excel(ww_file, sheet = "all gps tags")[,1:35] %>%
    dplyr::select(Nili_id, birth_year, sex) %>%
    distinct()
  
  with_age_sex <- removed_captures %>%
    dplyr::select(-c("sex")) %>%
    left_join(age_sex, by = "Nili_id")
  
  return(with_age_sex)
}

get_geofences <- function(){
  ll_top_left_1 <- c(30.928, 34.961)
  ll_bot_right_1 <- c(30.85, 35.073)
  ll_top_left_2 <- c(31.183, 35.229)
  ll_bot_right_2 <- c(31.116, 35.34)
  rect1 <- c(ll_top_left_1, ll_bot_right_1)
  rect2 <- c(ll_top_left_2, ll_bot_right_2)
  return(list(rect1, rect2))
} 

get_sns <- function(tag_sns_file){
  sns_sheet <- read_excel(tag_sns_file)
  sns <- sns_sheet[[1]]
  return(sns)
}

get_hires_tags <- function(with_age_sex, tag_sns){
  hires_tags <- with_age_sex %>%
    filter(tag_local_identifier %in% tag_sns) %>%
    mutate(timestamp_il = as_datetime(timestamp, tz = "Israel"),
           dateOnly_il = lubridate::date(timestamp_il)) 
  return(hires_tags)
}
