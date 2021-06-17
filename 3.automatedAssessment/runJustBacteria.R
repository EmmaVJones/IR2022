stationTableResults <- stationsTemplate %>% 
  dplyr::select(STATION_ID:VAHU6, ECOLI_EXC:ENTER_STAT)

# for just TRO, PRO, NRO
stationTable <- filter(stationTable, REGION %in% c('TRO', 'PRO', 'NRO'))

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

if(nrow(stationData) > 0){
  results <- #cbind(
    #StationTableStartingData(stationData),
    #bacteriaAssessmentDecisionClass(stationData) )
    StationTableStartingData(stationData) %>% 
    
    left_join(
      left_join(
        bacteriaAssessmentDecision(stationData, 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126) %>% 
          dplyr::select(-associatedDecisionData) %>% 
          rename('ECOLI_BACTERIADECISION' = 'BACTERIADECISION', 'ECOLI_BACTERIASTATS' = 'BACTERIASTATS'),
        bacteriaAssessmentDecision(stationData, 'ENTEROCOCCI', 'LEVEL_ENTEROCOCCI', 10, 130, 35) %>% 
          dplyr::select(-associatedDecisionData)%>% 
          rename('ENTER_BACTERIADECISION' = 'BACTERIADECISION', 'ENTER_BACTERIASTATS' = 'BACTERIASTATS'), 
        by = 'StationID'), by = c("STATION_ID" = 'StationID'))
  
  stationTableResults <- bind_rows(stationTableResults,results)
}


}
write_csv(stationTableResults,'dataForAssessors/BacteriaBothWays_TROPRONRO.csv', na = "") # dont write out a character NA in csv



 