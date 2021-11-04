#source('global.R')

assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326))

shinyServer(function(input, output, session) {
  
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
 # Subset statewideResults by chosen Region
  
  observeEvent(input$runData, {
    reactive_objects$regionResults <- statewideResults[[input$regionChoice]]    })
  
  observe({req(reactive_objects$regionResults)
    reactive_objects$stationTableResultsTwoYear <- left_join(reactive_objects$regionResults$`Assessment Results`$stationTableResults,
                                                      dplyr::select(reactive_objects$regionResults$stationGIS_View,
                                                                    STATION_ID = Station_Id, LATITUDE = Latitude, LONGITUDE = Longitude),
                                                      by = 'STATION_ID')
    reactive_objects$stationTableResultsYTD <- left_join(reactive_objects$regionResults$`Assessment Results YTD`$stationTableResults,
                                                      dplyr::select(reactive_objects$regionResults$stationGIS_ViewYTD,
                                                                    STATION_ID = Station_Id, LATITUDE = Latitude, LONGITUDE = Longitude),
                                                      by = 'STATION_ID')
    reactive_objects$runSummaryTwoYear <- summarizeRuns(reactive_objects$regionResults$stationFieldData)
    reactive_objects$runSummaryYTD <- summarizeRuns(reactive_objects$regionResults$stationFieldDataYTD)
    
    reactive_objects$assessmentSummaryTwoYear <- stationSummary(reactive_objects$stationTableResultsTwoYear, parameterEXCcrosswalk) 
    reactive_objects$assessmentSummaryYTD <- stationSummary(reactive_objects$stationTableResultsYTD, parameterEXCcrosswalk)  })

  
 
  callModule(regionalReviewMap,'regionalReviewMapYTD', reactive(reactive_objects$assessmentSummaryYTD), reactive(input$parameterChoiceYTD),
             reactive(reactive_objects$stationTableResultsYTD))
  
  callModule(regionalReviewMap,'regionalReviewMapTwoYear', reactive(reactive_objects$assessmentSummaryTwoYear), reactive(input$parameterChoiceTwoYear),
             reactive(reactive_objects$stationTableResultsTwoYear))
  
  callModule(regionalReviewStationStatus,'regionalReviewStationStatusYTD', reactive(reactive_objects$stationTableResultsYTD),
             reactive(reactive_objects$regionResults$ConventionalsYTD))
  callModule(regionalReviewStationStatus,'regionalReviewStationStatusTwoYear', reactive(reactive_objects$stationTableResultsTwoYear),
             reactive(reactive_objects$regionResults$Conventionals))
  
  callModule(regionalReviewMonitoringSummary, 'regionalReviewMonitoringSummaryYTD', reactive(reactive_objects$runSummaryYTD), 
             reactive(reactive_objects$regionResults$stationFieldDataYTD),  
             reactive(reactive_objects$regionResults$stationGIS_ViewYTD), reactive(assessmentLayer))
  callModule(regionalReviewMonitoringSummary, 'regionalReviewMonitoringSummaryTwoYear', reactive(reactive_objects$runSummaryTwoYear), 
             reactive(reactive_objects$regionResults$stationFieldData),
             reactive(reactive_objects$regionResults$stationGIS_View), reactive(assessmentLayer))
  
})
  
