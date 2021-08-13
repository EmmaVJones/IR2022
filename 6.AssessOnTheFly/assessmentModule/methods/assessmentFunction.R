

# Automated Assessment Function
automatedAssessmentFunction <- function(stationTable, conventionals, lakeStations, lacustrineDesignation, VSCIresults){
  stationTableResults <- tibble()
  # save ammonia results (based on default assessment information) for use in app to speed rendering
  ammoniaAnalysis <- tibble()
  
  for(i in 1:nrow(stationTable)){
    #i = 1
    print(paste('Assessing station', i, 'of', nrow(stationTable), sep=' '))
    
    # pull one station data
    stationData <- filter(conventionals, FDT_STA_ID %in% stationTable$STATION_ID[i]) %>%
      left_join(stationTable, by = c('FDT_STA_ID' = 'STATION_ID')) %>%
      pHSpecialStandardsCorrection() %>% # correct pH to special standards where necessary
      # special lake steps
      {if(stationTable$STATION_ID[i] %in% lakeStations$STATION_ID)
        suppressWarnings(suppressMessages(
          mutate(., lakeStation = TRUE) %>%
            thermoclineDepth())) # adds thermocline information and SampleDate
        else mutate(., lakeStation = FALSE) } %>% 
      # manually add lacustrine designation per user input
      mutate(LACUSTRINE = ifelse(stationTable$STATION_ID[i] %in% lacustrineDesignation$STATION_ID, TRUE, FALSE))
    
    
    # If data exists for station, run it
    if(nrow(stationData) > 0){
      # Date last sampled
      dateLastSampled <- as.character(max(stationData$FDT_DATE_TIME)) } else {dateLastSampled <- 'No data in conventionals data pull'}
    
    
    # Ammonia special section
    ammoniaAnalysisStation <- freshwaterNH3limit(stationData, trout = ifelse(unique(stationData$CLASS) %in% c('V','VI'), TRUE, FALSE),
                                                 mussels = TRUE, earlyLife = TRUE) 
    # https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section155/ states the assumption is that
    #  waters are to be assessed with the assumption that mussels and early life stages of fish should be present
    # trout presence is determined by WQS class, this can be changed in the app but is forced to be what the station
    # is attributed to in the automated assessment scripts
    
    
    if(nrow(stationData) > 0){
      # PWS stuff
      if(is.na(unique(stationData$PWS))  ){
        PWSconcat <- tibble(#STATION_ID = unique(stationData$FDT_STA_ID),
          PWS= NA)
      } else {
        PWSconcat <- cbind(#tibble(STATION_ID = unique(stationData$FDT_STA_ID)),
          assessPWS(stationData, NITRATE_mg_L, LEVEL_NITRATE, 10, 'PWS_Nitrate'),
          assessPWS(stationData, CHLORIDE_mg_L, LEVEL_CHLORIDE, 250, 'PWS_Chloride'),
          assessPWS(stationData, SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL, 250, 'PWS_Total_Sulfate')) %>%
          dplyr::select(-ends_with('exceedanceRate')) }
      
      # chloride assessment if data exists
      if(nrow(filter(stationData, !is.na(CHLORIDE_mg_L))) > 0){
        chlorideFreshwater <- chlorideFreshwaterSummary(suppressMessages(chlorideFreshwaterAnalysis(stationData)))
      } else {chlorideFreshwater <- tibble(CHL_EXC = NA, CHL_STAT= NA)}
      
      # Nutrients based on station type
      # Nutrient: TP (lakes have real standards; riverine no longer uses 0.2 mg/L as an observed effect for Aquatic life use 
      if(unique(stationData$lakeStation) == TRUE){
        TP <- TP_Assessment(stationData) 
      } else {
        TP <- countNutrients(stationData, PHOSPHORUS_mg_L, LEVEL_PHOSPHORUS, NA) %>% quickStats('NUT_TP') %>% 
          mutate(NUT_TP_STAT = ifelse(NUT_TP_STAT == 'IM', "Review", NA)) } # flag OE but don't show a real assessment decision
      
      # Nutrients: Chl a (lakes)
      if(unique(stationData$lakeStation) == TRUE){
        chla <- chlA_Assessment(stationData)
        #tibble(NUT_CHLA_EXC = NA, NUT_CHLA_SAMP = NA, NUT_CHLA_STAT = NA) # placeholder for now
      } else {
        chla <- countNutrients(stationData, CHLOROPHYLL_A_ug_L, LEVEL_CHLOROPHYLL_A, NA) %>% quickStats('NUT_CHLA') %>%
          mutate(NUT_CHLA_STAT = NA) } # don't show a real assessment decision
      
      # run DO daily average status for riverine and tuck results into comments later
      if(unique(stationData$lakeStation) == TRUE){
        DO_Daily_Avg_STAT <- ''
      } else {
        DO_Daily_Avg_STAT <- paste0('DO_Daily_Avg_STAT: ', 
                                    DO_Assessment_DailyAvg(stationData) %>% 
                                      quickStats('DO_Daily_Avg') %>% 
                                      dplyr::select(DO_Daily_Avg_STAT) %>% pull())}
      
      
      results <- cbind(
        dplyr::select(stationData, STATION_ID = FDT_STA_ID, Sta_Desc:BASIN_CODE, Basin_Code= Basin_Code.y, CountyCityName:Location) %>% 
          distinct(STATION_ID, .keep_all = T),
        tempExceedances(stationData) %>% quickStats('TEMP'),
        DOExceedances_Min(stationData) %>% quickStats('DO'), 
        # this will be removed for lake stations later since it does not apply
        pHExceedances(stationData) %>% quickStats('PH'),
        bacteriaAssessmentDecisionClass(stationData),
        
        # old bacteria methods 
        bacteriaExceedances_OLD(bacteria_Assessment_OLD(stationData, 'ECOLI', 126, 235),'E.COLI') %>%
          dplyr::select(-contains("exceedanceRate")) %>% 
          rename_all(function(x){paste0("OLD_", x)}), #for dplyr>1.0.4 rename_with(everything(), function(x){paste0("OLD_", x)})
        bacteriaExceedances_OLD(bacteria_Assessment_OLD(stationData, 'ENTEROCOCCI', 35, 104),'ENTER') %>%
          dplyr::select(-contains("exceedanceRate")) %>% 
          rename_all(function(x){paste0("OLD_", x)}), #for dplyr>1.0.4 rename_with(everything(), function(x){paste0("OLD_", x)})
        
        
        ammoniaDecision(list(acute = freshwaterNH3Assessment(ammoniaAnalysisStation, 'acute'),
                             chronic = freshwaterNH3Assessment(ammoniaAnalysisStation, 'chronic'),
                             fourDay = freshwaterNH3Assessment(ammoniaAnalysisStation, 'four-day'))), 
        
        # Will add in metals assessment at a later date
        
        # PCB and fish info can only be incorporated when data migrated into CEDS
        
        # Benthics, just a flag that benthic data exists
        benthicAssessment(stationData, VSCIresults) %>% 
          mutate(BENTHIC_STAT = case_when(BENTHIC_STAT == 'Review' ~ paste0("<b><a href='https://rconnect.deq.virginia.gov/CEDSBenthicDataQueryTool/?StationID=",
                                                                            unique(stationData$FDT_STA_ID),"'",
                                                                            " target= '_blank'> See results in CEDS Benthic Data Query Tool</a></b>"),
                                          TRUE ~ NA_character_)),
        
        # Nutrient Assessment done above by waterbody type
        TP,
        chla) %>%
        # COMMENTS
        mutate(COMMENTS = paste0(DO_Daily_Avg_STAT, 
                                 ' | AMMONIA Comment: ', `Assessment Decision`,
                                 BACTERIA_COMMENTS = paste0(' | E.coli Comment: ', ECOLI_STATECOLI_VERBOSE,
                                                            ' | Enterococci Comment: ', ENTER_STATENTER_VERBOSE)) ) %>%
        dplyr::select(-ends_with(c('exceedanceRate', 'VERBOSE', 'Assessment Decision', 'StationID'))) %>%  # to match Bulk Upload template but helpful to keep visible til now for testing
        mutate(`Date Last Sampled` = dateLastSampled) 
    } else {# pull what you can from last cycle and flag as carry over
      results <- filter(stationTable, STATION_ID == stationTable$STATION_ID[i]) %>%
        dplyr::select(STATION_ID , Sta_Desc:BASIN_CODE, Basin_Code= Basin_Code.y, CountyCityName:Location) %>% 
        distinct(STATION_ID, .keep_all = T) %>% 
        mutate(COMMENTS = 'This station has no data in current window.') %>%
        mutate(`Date Last Sampled` = dateLastSampled)
    }
    stationTableResults <- bind_rows(stationTableResults, results) %>% 
      relocate(c( BACTERIADECISION, BACTERIASTATS), .after = `Date Last Sampled`)
    ammoniaAnalysis <- bind_rows(ammoniaAnalysis, tibble(StationID = unique(stationData$FDT_STA_ID), AmmoniaAnalysis = list(ammoniaAnalysisStation)))
  }
  
  return(list(stationTableResults = stationTableResults, ammoniaAnalysis = ammoniaAnalysis))
}
