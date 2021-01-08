#source('global.R')

# Pull data from server
#conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
#  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
####vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
#WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# placeholder for now, shouldn't be a spatial file
#historicalStationsTable <- st_read('data/GIS/2020_wqms.shp') %>%
#  st_drop_geometry()#read_csv('data/stationsTable2022begin.csv') # last cycle stations table (forced into new station table format)
#WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")
#regions <- st_read('data/GIS/AssessmentRegions_simple.shp')

# Bring in local data (for now)
#ammoniaAnalysis <- readRDS('userDataToUpload/processedStationData/ammoniaAnalysis.RDS')
#lakeNutStandards <- read_csv('data/9VAC25-260-187lakeNutrientStandards.csv')



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
  
  
 
  #######################################
  # for testing
  stationTable <- reactive({
    read_csv('userDataToUpload/processedStationData/stationTableResults.csv',
             col_types = cols(COMMENTS = col_character())) %>%# force to character bc parsing can incorrectly guess logical based on top 1000 rows
      filter_at(vars(starts_with('TYPE')), any_vars(. == 'L')) %>% # keep only lake stations
      # add WQS information to stations
      left_join(WQSlookup, by = c('STATION_ID'='StationID')) %>%
      mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
      mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
      # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
      left_join(WQSvalues, by = 'CLASS_BASIN') %>%
      dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
      rename('CLASS' = 'CLASS.x') %>%
      left_join(dplyr::select(WQMstationFull, WQM_STA_ID, EPA_ECO_US_L3CODE, EPA_ECO_US_L3NAME) %>%
                  distinct(WQM_STA_ID, .keep_all = TRUE), by = c('STATION_ID' = 'WQM_STA_ID')) %>% # last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary
      lakeNameStandardization() %>% # standardize lake names
      left_join(lakeNutStandards, by = c('Lake_Name'))
  }) #for testing
  #######################################
  
  
  
  
  ################################ Watershed Selection Tab ########################################
  
  output$DEQregionSelectionUI <- renderUI({selectInput("DEQregionSelection", "Select DEQ Assessment Region", choices = c('BRRO', 'PRO', 'TRO', 'SWRO', 'NRO', 'VRO'))})
  
  # Pull AU data from server
  regionalAUs <- reactive({ req(input$pullAUs)
    withProgress(message = 'Reading in Large Spatial File',
                 st_zm(st_as_sf(pin_get('AUreservoir_EVJ', board = 'rsconnect')) ) %>%
                   lakeNameStandardization()) })  
  
  
  
  # Query lakes in region By Selectize arguments
  output$lakeSelection_ <- renderUI({req(regionalAUs(), input$DEQregionSelection)
    z <- filter(regionalAUs(), ASSESS_REG %in% input$DEQregionSelection & ASSESS_REG %in% input$DEQregionSelection)
    selectInput('lakeSelection', 'Select Lake', choices = sort(unique(z$Lake_Name)))})
  
  AUs <- reactive({req(input$lakeSelection, input$DEQregionSelection, regionalAUs())
    filter(regionalAUs(), Lake_Name %in% input$lakeSelection & ASSESS_REG %in% input$DEQregionSelection)})
  lake_filter <- reactive({req(AUs(), stationTable())
    filter_at(stationTable(), vars(starts_with('ID305B')), any_vars(. %in% AUs()$ID305B)) })
  
  lakeStations <- reactive({req(lake_filter()) 
    lake_filter() %>%
      st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326) }) # add projection, needs to be geographic for now bc entering lat/lng

  
  # Lake Map
  output$VAmap <- renderLeaflet({ req(AUs()) #, lake_filter()) 
    z <- suppressWarnings(st_coordinates(sf::st_centroid(AUs() %>% group_by(Lake_Name) %>% summarise())))
    
    CreateWebMap(maps = c("Topo","Imagery"), collapsed = TRUE) %>%
      {if(nrow(AUs())>1)
        setView(., z[1], z[2], zoom = 10) 
        else setView(., z[1], z[2], zoom = 12) } %>%
      addPolygons(data= AUs(), group = 'Selected Lake',
                  popup=leafpop::popupTable(AUs(), zcol=c('Lake_Name',"ID305B","ASSESS_REG"))) %>%
      {if(nrow(lakeStations()) > 0)
        addCircleMarkers(., data = lakeStations(), color='black', fillColor='yellow', radius = 4,
                         fillOpacity = 0.5,opacity=0.8,weight = 1,stroke=T, group="Monitored Stations",
                         label = ~STATION_ID, layerId = ~STATION_ID) 
        else . } %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Monitored Stations', 'Selected Lake'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')       })
  
  # Table of AUs within Selected Lake
  output$AUSummary <-  DT::renderDataTable({ req(AUs())
    DT::datatable(AUs() %>% st_drop_geometry(), rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(AUs()), scrollY = "300px", dom='Bti'),
                  selection = 'none')   })
  
  # Table of Stations within Selected Lake
  stationSummary <- reactive({req(lake_filter())
    filter(conventionals, FDT_STA_ID %in% lake_filter()$STATION_ID) %>%
      distinct(FDT_STA_ID, .keep_all = TRUE)  %>% 
      dplyr::select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:Data_Source, Latitude, Longitude) %>% 
      dplyr::select(-FDT_DATE_TIME) }) # drop date time bc confusing to users 
  
  output$stationSummary <- DT::renderDataTable({req(stationSummary())
    DT::datatable(stationSummary(), rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(stationSummary()), scrollY = "300px", dom='Bti'),
                  selection = 'none') })
  
  
  
  output$test <- renderPrint({paste(min(lakeStations()$LONGITUDE), min(lakeStations()$LATITUDE), max(lakeStations()$LONGITUDE), max(lakeStations()$LATITUDE))})
  
  
  
  #the_data <- reactive({req(regionalAUs(), input$DEQregionSelection)
  #  filter(regionalAUs(), ASSESS_REG %in% input$DEQregionSelection) })
  #lake_AUs <- shiny::callModule(dynamicSelect, "lakeSelection", the_data, "Lake_Name" )
  
})