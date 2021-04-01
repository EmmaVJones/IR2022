


# function to pull all stations on file

loadData <- function(outputDir) {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  # Concatenate all data together into one data.frame
  if(outputDir == 'WQSlookupTable' | outputDir == './WQSlookupTable'){
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
AUtableExisting <- read_csv('data/AUlookupTable/20201013_130553_AUlookup.csv') %>% 
  dplyr::select(FDT_STA_ID, ID305B_1, ID305B_2, n:Comments)
AUtableNew <- loadData("C:/HardDriveBackup/R/GitHub/IR2022/1.preprocessData/AUlookupTable") %>%  # run in console
  dplyr::select(FDT_STA_ID, ID305B_1, ID305B_2, n:Comments)
AUtableCombined <- bind_rows(AUtableExisting, AUtableNew)
# write it out to do edits manually
write.csv(AUtableCombined, 'data/AUlookupTable/20210401_0000_AUlookup.csv', row.names = F)

# use this info below and comment field to make edits to spreadsheet saved above
# QA time
# Find any stations missing AU information
missing_AUtableNew <- filter(AUtableNew, is.na(ID305B_1))

suggestedAUs <- read.csv('C:/HardDriveBackup/R/GitHub/IR2022/1.preprocessData/data/preAnalyzedAUdata.csv') %>% 
  filter(FDT_STA_ID %in% missing_AUtableNew$FDT_STA_ID) %>% 
  dplyr::select(FDT_STA_ID, ID305B_1)

# Find any stations have been associated with more than one AU
tooMany_AUtableNew <- AUtableNew %>% 
  group_by(FDT_STA_ID) %>%
  filter(n()>1) %>% 
  arrange(FDT_STA_ID)

# so now data/AUlookupTable/20210401_0000_AUlookup.csv is the most up to date version of AU information for IR2022

