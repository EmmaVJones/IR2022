



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

WQStableExisting <- loadData("./WQSlookupTable") # run in console



saveData <- function(data,outputDir) {
  #data <- t(data)
  # Create a unique file name
  if(outputDir == 'WQSlookupTable' | outputDir == './WQSlookupTable'){
    fileName <- sprintf("%s_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"), 'WQSlookup')
  } else {
    fileName <- sprintf("%s_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"), 'AUlookup')
  }
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
}

#saveData(WQStableExisting, './WQSlookupTable')


#If just pulling from previous version.

WQStableExisting <- loadData("./WQSlookupTable") # run in console
# quick snap check previous WQS attributed data from assessors bc some issues have been noticed
tooMany_WQStableExisting <- snapCheck(WQStableExisting)

# manually correct and dumb user errors. If more than one WQS_ID remains after this correction then we will send that back to the user for review after we tack on any new stations to be looked at
