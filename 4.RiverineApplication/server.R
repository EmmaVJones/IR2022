source('global.R')

# Pinned to server, do for each region
#regionalAUs <- st_read('userDataToUpload/AU_working/va_2020_aus_riverine_DRAFT_BRRO.shp') %>%
#  st_transform(4326)   # transform to WQS84 for spatial intersection 
#pin(regionalAUs, name = 'BRROworkingAUriverine', description = "BRRO working AU riverine", board = "rsconnect")



# Pull data from server
conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")



shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  # Reactive Value to store all site data
  siteData <- reactiveValues()
  
  
  ################################ Data Upload Tab ################################################# 
  
  stationTable <- reactive({
    req(input$stationsTable)
    inFile <- input$stationsTable
    stationTable <- read_csv(inFile$datapath) %>%
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
      rename('CLASS' = 'CLASS.x') 
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
                 st_zm(st_as_sf(pin_get(paste0(input$DEQregionSelection, 'workingAUriverine'), board = 'rsconnect')) )) })
  
  # Query VAHUC6's By Selectize arguments
  the_data <- reactive({
    req(regionalAUs(), input$DEQregionSelection)
    filter(vahu6, ASSESS_REG %in% input$DEQregionSelection) %>%
      left_join(dplyr::select(subbasinToVAHU6, VAHU6, Basin, BASIN_CODE, Basin_Code))})
  basin_filter <- shiny::callModule(dynamicSelect, "basinSelection", the_data, "Basin_Code" )
  huc6_filter <- shiny::callModule(dynamicSelect, "HUC6Selection", basin_filter, "VAHU6" )
  
  # Spatially intersect chosen VAHU6 with regionalAUs
  AUs <- reactive({req(huc6_filter(), regionalAUs())
    suppressWarnings(st_intersection(regionalAUs(),  huc6_filter())) }) #filter(vahu6, VAHU6 %in% huc6_filter()$VAHU6)))})
  

  # Watershed Map
  output$VAmap <- renderLeaflet({
    req(basin_filter(), huc6_filter(), regionalAUs())
    m <- mapview(basin_filter(),label= basin_filter()$VAHU6, layer.name = 'Basin Chosen',
                 popup= leafpop::popupTable(basin_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")), legend= FALSE) + 
      mapview(huc6_filter(), color = 'yellow',lwd= 5, label= huc6_filter()$VAHU6, layer.name = c('Selected HUC6'),
              popup= leafpop::popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")), legend= FALSE)
    m@map %>% setView(st_bbox(huc6_filter())$xmax[[1]],st_bbox(huc6_filter())$ymax[[1]],zoom = 9) })
  
  # Table of AUs within Selected VAHU6
  output$AUSummary <-  DT::renderDataTable({ req(AUs())
    DT::datatable(AUs() %>% st_drop_geometry(), rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(AUs()), scrollY = "300px", dom='Bti'))   })

  # Table of Stations within Selected VAHU6
  output$stationSummary <- DT::renderDataTable({
    req(huc6_filter())
    z <- filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      distinct(FDT_STA_ID, .keep_all = TRUE) %>%
      select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:STA_CBP_NAME) %>% 
      mutate(#`In Stations Table` = ifelse(FDT_STA_ID %in% unique(stationTable()$STATION_ID), 'yes','no'),
             #`In Selected Region` = ifelse(FDT_STA_ID %in% filter(stationTable(), REGION %in% input$DEQregionSelection)$FDT_STA_ID, 'yes','no'),
             `Analyzed By App` = #ifelse(`In Stations Table` == 'yes'# && `In Selected Region` == 'yes', 'yes','no'))
               ifelse(FDT_STA_ID %in% unique(stationTable()$STATION_ID), 'yes','no'))
    
    
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "300px", dom='Bti')) %>%
      DT::formatStyle('Analyzed By App', target = 'row', backgroundColor = styleEqual(c('yes','no'), c('lightgray', 'yellow'))) })
  
  # Button to visualize modal map of AUs in selected VAHU6
  observeEvent(input$reviewAUs,{
    showModal(modalDialog(
      title="Spatially Preview Assessment Units for Selected VAHU6",
      leafletOutput('AUmap'),
      easyClose = TRUE))  })
  
  # modal map
  output$AUmap <- renderLeaflet({
    req(AUs(), huc6_filter())
    
    z <- AUs()
    z$ID305B <- factor(z$ID305B) # drop extra factor levels so colors come out right
    
    m <- mapview(huc6_filter(), color = 'yellow',lwd= 5, label= NULL, layer.name = c('Selected HUC6'),
                 popup= leafpop::popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")), legend= FALSE) + 
      mapview(z, label= z$ID305B, layer.name = c('AUs in Selected HUC6'), zcol = "ID305B", legend=FALSE,
              popup= leafpop::popupTable(z, zcol=c("ID305B","MILES","CYCLE","WATER_NAME","LOCATION" )), legend= FALSE)
    m@map })
  
  
  
  
  ################################ Assessment Unit Review Tab ########################################
  
  #  output$test <- renderPrint({#req(regionalAUs)
  #    glimpse(AUs())})
  
  
  # Show selected VAHU6
  output$selectedVAHU6 <- DT::renderDataTable({
    datatable(huc6_filter() %>% st_set_geometry(NULL) %>% select(VAHU6, VaName, Basin),
              rownames = FALSE, options= list(pageLength = 1, scrollY = "35px", dom='t'))})
  
  
  ## Don't let user click pull data button if no conventionals data for VAHU6
  #observe({
  #  shinyjs::toggleState('pullHUCdata', nrow(AUs())!=0 )
  #})
  
  
  # Pull Conventionals data for selected AU on click
  conventionals_HUC <- reactive({#eventReactive( input$pullHUCdata, {
    filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      left_join(dplyr::select(stationTable(), STATION_ID:VAHU6,
                              WQS_ID:CLASS_DESCRIPTION),
                              #WQS_ID:`Max Temperature (C)`), 
                by = c('FDT_STA_ID' = 'STATION_ID')) %>%
      filter(!is.na(ID305B_1)) %>%
      pHSpecialStandardsCorrection() }) #correct pH to special standards where necessary
  
  output$AUselection_ <- renderUI({ req(conventionals_HUC())
    selectInput('AUselection', 'Assessment Unit Selection', choices = unique(conventionals_HUC()$ID305B_1))  })
  
  output$selectedAU <- DT::renderDataTable({req(conventionals_HUC(),input$AUselection)
    z <- filter(regionalAUs(), ID305B %in% input$AUselection) %>% st_set_geometry(NULL) %>% as.data.frame()
    datatable(z, rownames = FALSE, 
              options= list(pageLength = nrow(z),scrollX = TRUE, scrollY = "300px", dom='t'))})
  
  output$stationSelection_ <- renderUI({ req(conventionals_HUC(), input$AUselection)
    z <- filter(conventionals_HUC(), ID305B_1 %in% input$AUselection | ID305B_2 %in% input$AUselection | 
                  ID305B_3 %in% input$AUselection | ID305B_4 %in% input$AUselection | ID305B_5 %in% input$AUselection | 
                  ID305B_6 %in% input$AUselection | ID305B_7 %in% input$AUselection | ID305B_8 %in% input$AUselection | 
                  ID305B_9 %in% input$AUselection | ID305B_10 %in% input$AUselection) %>%
      distinct(FDT_STA_ID)
    fluidRow(selectInput('stationSelection', 'Station Selection', choices = unique(z$FDT_STA_ID)),
             helpText("The stations available in the drop down are limited to stations with an ID305B_1:ID305B_10 designation equal 
                      to the selected AU. All AU's associated with the selected station can be viewed in the map below."))})
  
  AUData <- eventReactive( input$AUselection, {
    filter_at(conventionals_HUC(), vars(starts_with("ID305B")), any_vars(. %in% input$AUselection) ) }) 
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData(), FDT_STA_ID %in% input$stationSelection) })
  
  output$stationInfo <- DT::renderDataTable({ req(stationData())
    z <- filter(stationTable(), STATION_ID == input$stationSelection) %>% 
      select(STATION_ID:VAHU6, WQS_ID:Trout) %>% 
      t() %>% as.data.frame() %>% rename(`Station and WQS Information` = 1)
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'))  })


  output$stationMap <- renderLeaflet({req(nrow(stationData()) >0) # to prevent having no lat/lng data for that half second app is catching up after station change
    point <- dplyr::select(stationData()[1,],  FDT_STA_ID, starts_with('ID305B'), Latitude, Longitude ) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), 
               remove = F, # don't remove these lat/lon cols from df
               crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
    segmentChoices <- dplyr::select(point, starts_with('ID305B')) %>% st_drop_geometry() %>% as.character()  
    segment <- filter(regionalAUs(), ID305B %in% segmentChoices)
    map1 <- mapview(segment,zcol = 'ID305B', label= segment$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
                    popup= leafpop::popupTable(segment, zcol=c("ID305B","MILES","CYCLE","WATER_NAME")), legend= FALSE) + 
      mapview(point, color = 'yellow', lwd = 5, label= point$FDT_STA_ID, layer.name = c('Selected Station'),
              popup=NULL, legend= FALSE)
    map1@map %>% setView(point$Longitude, point$Latitude, zoom = 11) })
  
  
  # Historical Station Information need last cycle stations table final
  output$stationHistoricalInfo <- DT::renderDataTable({ req(nrow(stationData()) >0)
    z <- suppressWarnings(filter(historicalStationsTable, STATION_ID %in% input$stationSelection) %>% 
      select(STATION_ID:COMMENTS) %>%
      t() %>% as.data.frame() %>% rename(`Station Information From Last Cycle` = 'V1'))
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'))  })
  
  
  ## Station Table View Section
  
  observe({#run longer analyses first
    siteData$ecoli <- bacteriaAssessmentDecision(stationData(), 'E.COLI', 'ECOLI_RMK', 10, 410, 126) %>%
      dplyr::select(ECOLI_EXC:ECOLI_STAT)
    siteData$enter <- bacteriaAssessmentDecision(stationData(), 'ENTEROCOCCI', 'RMK_31649', 10, 130, 35) %>%
      dplyr::select(ENTER_EXC:ENTER_STAT)})
  
  observe({
    req(nrow(siteData$ecoli)>0, nrow(siteData$enter)>0) # need to tell the app to wait for data to exist in these objects before smashing data together or will bomb out when switching between VAHU6's on the Watershed Selection Page
    siteData$stationTableOutput <- cbind(StationTableStartingData(stationData()),
                                         tempExceedances(stationData()) %>% quickStats('TEMP'),
                                         DOExceedances_Min(stationData()) %>% quickStats('DO'), 
                                         pHExceedances(stationData()) %>% quickStats('PH'),
                                         siteData$ecoli,
                                         siteData$enter) %>%
      mutate(COMMENTS = NA) %>%
      dplyr::select(-ends_with('exceedanceRate'))
  })

  output$stationTableDataSummary <- DT::renderDataTable({
    req(stationData(),siteData$stationTableOutput)
    datatable(siteData$stationTableOutput, extensions = 'Buttons', escape=F, rownames = F, editable = TRUE,
              options= list(scrollX = TRUE, pageLength = nrow(siteData$stationTableOutput),
                            # hide certain columns
                            #columnDefs = list(list(targets = 6, visible = FALSE)),
                            dom='Bt', buttons=list('copy',
                                                   list(extend='csv',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                   list(extend='excel',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep=''))))) %>% 
      formatStyle(c('TEMP_EXC','TEMP_SAMP','TEMP_STAT'), 'TEMP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('DO_EXC','DO_SAMP','DO_STAT'), 'DO_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('PH_EXC','PH_SAMP','PH_STAT'), 'PH_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('ECOLI_EXC','ECOLI_SAMP','ECOLI_GM_EXC','ECOLI_GM_SAMP','ECOLI_STAT'), 'ECOLI_STAT', backgroundColor = styleEqual(c('IM'), c('red'))) %>%
      formatStyle(c('ENTER_SAMP','ENTER_EXC',"ENTER_GM_EXC","ENTER_GM_SAMP",'ENTER_STAT'), 'ENTER_STAT', backgroundColor = styleEqual(c('IM'), c('red')))
  })
  
  
  ## PWS table
  
  
  
  #### Data Sub Tab ####---------------------------------------------------------------------------------------------------
  
  # Display Data 
  output$AURawData <- DT::renderDataTable({ req(AUData())
    DT::datatable(AUData(), extensions = 'Buttons', escape=F, rownames = F, 
                  options= list(scrollX = TRUE, pageLength = nrow(AUData()), scrollY = "300px", 
                                dom='Btf', buttons=list('copy',
                                                        list(extend='csv',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                        list(extend='excel',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')))))})
  # Summarize data
  output$stationDataTableRecords <- renderText({req(AUData())
    paste(nrow(AUData()), 'records were retrieved for',as.character(input$AUselection),sep=' ')})
  output$uniqueStationDataTableRecords <- renderTable({req(AUData())
    plyr::count(AUData(), vars = c("FDT_STA_ID")) %>% dplyr::rename('Number of Records'='freq')})
  output$stationDataTableAssessmentWindow <- renderText({req(AUData())
    withinAssessmentPeriod(AUData())})
  
  
  # Need this as a reactive to regenerate below modules when user changes station 
  stationSelected <- reactive({input$stationSelection})
  
  
  ## Temperature Sub Tab ##------------------------------------------------------------------------------------------------------
  
  callModule(temperaturePlotlySingleStation,'temperature', AUData, stationSelected)
  
  ## pH Sub Tab ##------------------------------------------------------------------------------------------------------
  
  callModule(pHPlotlySingleStation,'pH', AUData, stationSelected)

 })