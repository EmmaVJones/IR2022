#source('global.R')

# Pull data from server
#conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
#  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
#vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
#WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# placeholder for now, shouldn't be a spatial file
#historicalStationsTable <- st_read('data/GIS/2020_wqms.shp') %>%
#  st_drop_geometry()#read_csv('data/stationsTable2022begin.csv') # last cycle stations table (forced into new station table format)
#WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")

# Bring in local data (for now)
#ammoniaAnalysis <- readRDS('userDataToUpload/processedStationData/ammoniaAnalysis.RDS')



shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  # Reactive Value to store all site data
  siteData <- reactiveValues()
  
  
  ################################ Data Upload Tab ################################################# 
  
  # Download data template
  output$downloadTemplate <- downloadHandler(filename=function(){'stationTableStatewideExample.csv'},
                                             content=function(file){write_csv(template,file,na = "")})
  
  output$templateLastUpdated_ <- renderUI({
    helpText(paste0('Template last updated: ', lastUpdated))  })
  
  
  # real
  stationTable <- reactive({
    req(input$stationsTable)
    inFile <- input$stationsTable
    stationTable <- read_csv(inFile$datapath,
                             col_types = cols(COMMENTS = col_character())) %>% # force to character bc parsing can incorrectly guess logical based on top 1000 rows
      #fix periods in column names from excel
      as_tibble()
    # Remove stations that don't apply to application
    lakeStations <- filter_at(stationTable, vars(starts_with('TYPE')), any_vars(. == 'L'))
    stationTable <- filter(stationTable, !STATION_ID %in% lakeStations$STATION_ID) %>%
      # add WQS information to stations
      left_join(WQSlookup, by = c('STATION_ID'='StationID')) %>%
      mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
      mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
      # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
      left_join(WQSvalues, by = 'CLASS_BASIN') %>%
      dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
      rename('CLASS' = 'CLASS.x') %>%
      left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME) %>%
                  distinct(WQM_STA_ID, .keep_all = TRUE), by = c('STATION_ID' = 'WQM_STA_ID'))
    # last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary
    
    return(stationTable)
  })
  
  
  
  
  ################################ Watershed Selection Tab ########################################
  
  output$DEQregionSelectionUI <- renderUI({req(vahu6)
    selectInput("DEQregionSelection", "Select DEQ Assessment Region", choices = unique(vahu6$ASSESS_REG))})
  
  
  # Pull AU data from server
  regionalAUs <- reactive({ 
    req(input$pullAUs)
    withProgress(message = 'Reading in Large Spatial File',
                 st_zm(st_as_sf(pin_get('AUreservoir_EVJ', board = 'rsconnect')) )) })     # change to final
  
  
  
  
  
})