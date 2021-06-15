stationTableResults <- stationsTemplate 

for(i in 1:nrow(stationTable)){
  print(paste('Assessing station', i, 'of', nrow(stationTable), sep=' '))
  
stationData <- filter(conventionals, FDT_STA_ID %in% stationTable$STATION_ID[i]) %>%
  left_join(stationTable, by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
  # special lake steps
  {if(stationTable$STATION_ID[i] %in% lakeStations$STATION_ID)
    suppressWarnings(suppressMessages(
      mutate(., lakeStation = TRUE) %>%
        thermoclineDepth())) # adds thermocline information and SampleDate
    else mutate(., lakeStation = FALSE) }



results <- cbind(
  StationTableStartingData(stationData),
  bacteriaAssessmentDecisionClass(stationData) )

stationTableResults <- bind_rows(stationTableResults,results)
}
 