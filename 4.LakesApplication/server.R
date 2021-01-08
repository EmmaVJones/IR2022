#source('global.R')

# Pull data from server
#conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
#  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
####vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
#WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# placeholder for now, shouldn't be a spatial file
#historicalStationsTable <-  st_read('data/GIS/va20ir_wqms.shp') %>%
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
                         label = ~STATION_ID, layerId = ~STATION_ID,
                         popup=leafpop::popupTable(lakeStations(), zcol=c('STATION_ID',"ID305B_1","ID305B_2","ID305B_3"))) 
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
  
  
  
  
  ################################ Assessment Unit Review Tab ########################################
  
  # Show selected Lake 
  output$selectedLake <- DT::renderDataTable({
    z <- dplyr::select(lake_filter(), Lake_Name, VAHU6, Lakes_187B) %>%
      group_by(Lake_Name) %>%
      summarise(VAHU6 = toString(sort(unique(VAHU6))),
                `Section 187` = toString(sort(unique(Lakes_187B))))
    datatable(z, rownames = FALSE, options= list(pageLength = 1, scrollY = "35px", dom='t'), selection = 'none')})
  
  conventionalsLake <- reactive({ req(lake_filter())
    filter(conventionals, FDT_STA_ID %in% lake_filter1$STATION_ID) %>%
    left_join(dplyr::select(stationTable1, STATION_ID:VAHU6,
                            WQS_ID:`Total Phosphorus (ug/L)`),
              #WQS_ID:`Max Temperature (C)`), 
              by = c('FDT_STA_ID' = 'STATION_ID')) %>%
    filter(!is.na(ID305B_1)) %>%
    pHSpecialStandardsCorrection() }) #correct pH to special standards where necessary
  
  
  output$AUselection_ <- renderUI({req(lake_filter())
    AUselectionOptions <- unique(dplyr::select(lake_filter(), ID305B_1:ID305B_10) %>% 
                                    mutate_at(vars(starts_with("ID305B")), as.character) %>%
                                    pivot_longer(ID305B_1:ID305B_10, names_to = 'ID305B', values_to = 'keep') %>%
                                    pull(keep) )
    AUselectionOptions <- AUselectionOptions[!is.na(AUselectionOptions) & !(AUselectionOptions %in% c("NA", "character(0)", "logical(0)"))]
    selectInput('AUselection', 'Assessment Unit Selection', choices = AUselectionOptions)})
  
  output$selectedAU <- DT::renderDataTable({req(input$AUselection)
    z <- filter(regionalAUs(), ID305B %in% input$AUselection) %>% st_set_geometry(NULL) %>% as.data.frame()
    datatable(z, rownames = FALSE, 
              options= list(pageLength = nrow(z),scrollX = TRUE, scrollY = "300px", dom='t'),
              selection = 'none')})
  
  output$stationSelection_ <- renderUI({ req(conventionalsLake(), input$AUselection)
    stationSelectionOptions <- filter_at(lake_filter(), vars(starts_with("ID305B")), any_vars(. %in% input$AUselection)) %>%
      distinct(STATION_ID) %>% arrange(STATION_ID) %>%  pull()
    fluidRow(selectInput('stationSelection', 'Station Selection', choices = stationSelectionOptions),
             helpText("The stations available in the drop down are limited to stations with an ID305B_1:ID305B_10 designation equal 
                      to the selected AU. All AU's associated with the selected station can be viewed in the map below."))})
  
  AUData <- reactive({req(input$AUselection)
    filter_at(conventionalsLake(), vars(starts_with("ID305B")), any_vars(. %in% input$AUselection) ) }) 
  
  stationData <- reactive({req(input$stationSelection)
    filter(AUData(), FDT_STA_ID %in% input$stationSelection) })
  
  stationInfo <- reactive({req(input$stationSelection, AUData())
    filter(stationTable(), STATION_ID == input$stationSelection) %>% 
      select(STATION_ID:VAHU6, WQS_ID:`Total Phosphorus (ug/L)`)})
  
  
  output$stationInfo <- DT::renderDataTable({ req(stationInfo())
    z <- stationInfo() %>%
      t() %>% as.data.frame() %>% rename(`Station and WQS Information` = 1)
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')  })
  
  output$stationMap <- renderLeaflet({req(nrow(stationInfo()) >0) # to prevent having no lat/lng data for that half second app is catching up after station change
    point <- dplyr::select(stationInfo(),  STATION_ID, starts_with('ID305B'), LATITUDE, LONGITUDE ) %>%
      st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326) # add projection, needs to be geographic for now bc entering lat/lng
    segmentChoices <- dplyr::select(point, starts_with('ID305B')) %>% st_drop_geometry() %>% as.character()  
    segment <- filter(regionalAUs(), ID305B %in% segmentChoices)
    map1 <- mapview(segment,zcol = 'ID305B', label= segment$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
                    popup= leafpop::popupTable(segment, zcol=c("ID305B","Acres","CYCLE","WATER_NAME")), legend= FALSE) + 
      mapview(point, color = 'yellow', lwd = 5, label= point$STATION_ID, layer.name = c('Selected Station'),
              popup=NULL, legend= FALSE)
    map1@map %>% setView(point$LONGITUDE, point$LATITUDE, zoom = 12) })
  
  # Historical Station Information need last cycle stations table final
  output$stationHistoricalInfo1 <- DT::renderDataTable({ req(nrow(stationInfo()) >0)
    z <- suppressWarnings(filter(historicalStationsTable, STATION_ID %in% input$stationSelection) %>% 
                            select(STATION_ID:COMMENTS) %>%
                            t() %>% as.data.frame() %>% rename(`Station Information From 2020 Cycle` = 'V1')) # need to update each rebuild
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')  })
  
  output$stationHistoricalInfo2 <- DT::renderDataTable({ req(nrow(stationInfo()) >0)
    z <- suppressWarnings(filter(historicalStationsTable2, STATION_ID %in% input$stationSelection) %>% 
                            select(STATION_ID:COMMENTS) %>%
                            t() %>% as.data.frame() %>% rename(`Station Information From 2018 Cycle` = 'V1')) # need to update each rebuild
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')  })
  
  
  
  output$test <- renderPrint({paste(min(lakeStations()$LONGITUDE), min(lakeStations()$LATITUDE), max(lakeStations()$LONGITUDE), max(lakeStations()$LATITUDE))})
  
  
  
  
})