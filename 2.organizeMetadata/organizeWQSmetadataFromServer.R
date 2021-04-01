
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
WQStableExisting <- read_csv('data/WQSlookupTable/20201207_092616_WQSlookup.csv') 
WQStableNew <- loadData("C:/HardDriveBackup/R/GitHub/IR2022/1.preprocessData/WQSlookupTable")   
WQStableCombined <- bind_rows(WQStableExisting, WQStableNew)
# write it out to do edits manually
write.csv(WQStableCombined, 'data/WQSlookupTable/20210401_0000_WQSlookup.csv', row.names = F)

# use this info below and comment field to make edits to spreadsheet saved above

# don't waste time on stations that don't need to be assessed
hitList <- read_csv('data/stationHitList.csv')


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
