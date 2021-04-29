# find kristie's work


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

#WQStableNew <- loadData("WQSlookupTable") 
kristie <- read_csv('WQSlookupTable/kristie.csv')

#what got assessed?
#z <- filter(stationTableResults, STATION_ID %in% WQStableNew$StationID)
z1 <- filter(stationTableResults, STATION_ID %in% kristie$StationID)
z2 <- filter(conventionals, FDT_STA_ID %in% kristie$StationID) %>% distinct(FDT_STA_ID)
z3 <- filter(beginNew, STATION_ID %in% kristie$StationID)

# old wqs answers
stationTableOld <- read_csv('C:/HardDriveBackup/R/GitHub/IR2022/4.RiverineApplication/userDataToUpload/processedStationData/stationTableResults.csv')
oldWQS <- filter(stationTableOld, STATION_ID %in% kristie$StationID)
