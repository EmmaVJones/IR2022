
# function to pull all stations on file

loadData <- function(outputDir) {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  # Concatenate all data together into one data.frame
  if(str_detect(outputDir, 'WQSlookupTable')){#outputDir == 'WQSlookupTable' | outputDir == './WQSlookupTable'){
    data <- lapply(files, read_csv) 
    data <- do.call(rbind, data) %>%
      distinct(StationID, WQS_ID, .keep_all = T)
  } else {
    data <- lapply(files, read_csv) # read_csv produces parsing errors
    data <- do.call(rbind, data) %>%
      distinct(FDT_STA_ID, ID305B_1,  .keep_all = T)
  }
  
  data
}

# function to find sites with +1 segment
snapCheck <- function(successDataFrame){
  successDataFrame %>%
    group_by(StationID) %>%
    filter(n()>1)
}

# Bring in data from server
WQStableExisting <- read_csv('data/WQSlookupTable/20210401_0001_WQSlookup.csv') #read_csv('data/WQSlookupTable/20201207_092616_WQSlookup.csv') 
WQStableNew <- loadData("C:/HardDriveBackup/R/GitHub/IR2022/1.preprocessData/WQSlookupTable") %>% 
  # this is only necessary if you are allow assessors to continue to work with application after a cleanup phase
  filter(!StationID %in% WQStableExisting$StationID)
WQStableCombined <- bind_rows(WQStableExisting, WQStableNew) #%>% 
  # make sure no duplicates
  #group_by(StationID) %>% #mutate(n())
# write it out to do edits manually
write.csv(WQStableCombined, 'data/WQSlookupTable/20210428_0000_WQSlookup.csv', row.names = F)

# use this info below and comment field to make edits to spreadsheet saved above

# don't waste time on stations that don't need to be assessed
hitList <- read_csv('data/stationHitList.csv')


# what sites still need data
snap_input <- readRDS('C:/HardDriveBackup/R/GitHub/IR2022/1.preprocessData/data/WQStable02032021.RDS')
# Weblink to internal DEQ app constants
# slightly different format so it will work in excel, dropped href
webLinkpart1 <- "https://gis.deq.virginia.gov/GISStaffApplication/?query=WQM%20Stations%20(All%20stations%20with%20full%20attributes),STATION_ID,"
webLinkpart2 <- "&showLayers=DEQInternalDataViewer_1723;WATER%20LAYERS;WQM%20Stations%20(All%20stations%20with%20full%20attributes);"
webLinkpart3 <- "&level=14" 

# make a dataset for last assessor to review
stillMissing <- filter(snap_input, ! StationID %in% WQStableCombined$StationID) %>% 
  mutate('Final WQS_ID' = NA,
         GISlink = case_when(str_extract(WQS_ID, "^.{2}") == 'EP' ~ "Estuaries%20WQS;Estuarine%20waters;Tidal%20flow%20paths",
                             str_extract(WQS_ID, "^.{2}") == 'EL' ~ "Estuaries%20WQS;Estuarine%20waters;Tidal%20flow%20paths",
                             str_extract(WQS_ID, "^.{2}") == 'RL' ~ "Streams/Rivers%20WQS;Public%20water%20supply;Trout;All%20other%20streams/rivers",
                             str_extract(WQS_ID, "^.{2}") == 'LP' ~ "Lakes/Reservoirs%20WQS;Public%20Water%20Supply;Trout;All%20other%20lakes/reservoirs",
                             TRUE ~ as.character(NA))) %>% 
  rename('Suggested WQS_ID' = 'WQS_ID') %>% 
  mutate('Comments' = NA,
         `DEQ GIS Web App Link` =  paste0(webLinkpart1, StationID, webLinkpart2, GISlink, webLinkpart3)) %>% 
  dplyr::select(StationID, `Suggested WQS_ID`, `Buffer Distance`, `Final WQS_ID`, Comments, `DEQ GIS Web App Link`)
write.csv(stillMissing, 'WQSforKristieReview.csv', row.names = F)

# QA time
# Find any stations missing WQS information
missing_WQStableNew <- filter(WQStableNew, is.na(WQS_ID)) %>% 
  filter(!( StationID %in% hitList$`StationID to Remove`)) %>% 
  arrange(StationID) %>% 
  left_join(dplyr::select(stationsTable2022begin, STATION_ID, LATITUDE, LONGITUDE), by = c('StationID' = 'STATION_ID'))

suggestedWQSs <- readRDS('C:/HardDriveBackup/R/GitHub/IR2022/1.preprocessData/data/WQStable02032021.RDS')  

missing_suggestedWQSs <- suggestedWQSs %>% 
  filter(StationID %in% missing_WQStableNew$StationID) %>% 
  filter(!( StationID %in% hitList$`StationID to Remove`)) %>% 
  arrange(StationID) %>% 
  left_join(dplyr::select(stationsTable2022begin, STATION_ID, LATITUDE, LONGITUDE), by = c('StationID' = 'STATION_ID'))

# Find any stations have been associated with more than one WQS
tooMany_WQStableNew <- WQStableNew %>% 
  group_by(StationID) %>%
  filter(n()>1) %>% 
  arrange(StationID)%>% 
  left_join(dplyr::select(stationsTable2022begin, STATION_ID, LATITUDE, LONGITUDE), by = c('StationID' = 'STATION_ID'))

# double check no duplicates in whole thing
tooMany_WQStable <- read.csv('data/WQSlookupTable/20210401_0000_WQSlookup.csv') %>% 
  group_by(StationID) %>%
  filter(n()>1) %>% 
  arrange(StationID)%>% 
  left_join(dplyr::select(stationsTable2022begin, STATION_ID, LATITUDE, LONGITUDE), by = c('StationID' = 'STATION_ID'))


# make sure missing WQS_IDs in whole thing are expected
WQStableCombined<- read_csv('data/WQSlookupTable/20210401_0000_WQSlookup.csv')
missing_WQS <- filter(WQStableCombined, is.na(WQS_ID) & Comments == 'Manual Accept |') # drop where user entered info
missingSuggestions <- filter(suggestedWQSs, StationID %in% missing_WQS$StationID) %>% 
  group_by(StationID) %>% 
  mutate(n = n())

# Assume user was right for manual accept with n = 1
missingSuggestionsFix <- filter(missingSuggestions, n ==1)
WQStableCombinedFix <- filter(WQStableCombined, StationID %in% missingSuggestionsFix$StationID) %>% 
  left_join(dplyr::select(missingSuggestionsFix, StationID, fixedWQS= WQS_ID), by = 'StationID') %>% 
  mutate(WQS_ID = fixedWQS) %>% 
  dplyr::select(-fixedWQS)


WQStableCombined1 <- bind_rows(filter(WQStableCombined, ! StationID %in% WQStableCombinedFix$StationID),
                               WQStableCombinedFix) 

# Manually decide these for assessor to move things along
superFixes <- filter(missingSuggestions, n > 1) %>% 
  arrange(StationID) %>% 
  left_join(dplyr::select(stationsTable2022begin, STATION_ID, LATITUDE, LONGITUDE), by = c('StationID' = 'STATION_ID'))

write.csv(WQStableCombined1, 'data/WQSlookupTable/20210401_0001_WQSlookup.csv', row.names = F)



# so now data/WQSlookupTable/20210401_0001_WQSlookup.csv is the most up to date version of WQS information for IR2022


filter(WQSlookup, ! StationID %in% WQSlookup_withStandards$StationID)
