source('global.R')

# Pinned to server, done for each region in C:\HardDriveBackup\R\GitHub\IR2022\1.preprocessData\preprocessingWorkflow\ReworkingDataPreprocessingMethod.Rmd
#regionalAUs <- st_read('userDataToUpload/AU_working/va_2020_aus_riverine_DRAFT_BRRO.shp') %>%
#  st_transform(4326)   # transform to WQS84 for spatial intersection 
#pin(regionalAUs, name = 'BRROworkingAUriverine', description = "BRRO working AU riverine", board = "rsconnect")



# Pull data from server
conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# placeholder for now, shouldn't be a spatial file
historicalStationsTable <- st_read('data/GIS/va20ir_wqms.shp') %>%
  st_drop_geometry()#read_csv('data/stationsTable2022begin.csv') # last cycle stations table (forced into new station table format)
WCmetals <- pin_get("WCmetals-2020IRfinal",  board = "rsconnect")
Smetals <- pin_get("Smetals-2020IRfinal",  board = "rsconnect")
WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")
VSCIresults <- pin_get("VSCIresults", board = "rsconnect") %>%
  filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) )
VCPMI63results <- pin_get("VCPMI63results", board = "rsconnect") %>%
  filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) )
VCPMI65results <- pin_get("VCPMI65results", board = "rsconnect") %>%
  filter( between(`Collection Date`, assessmentPeriod[1], assessmentPeriod[2]) ) 

# Bring in local data (for now)
ammoniaAnalysis <- readRDS('userDataToUpload/processedStationData/ammoniaAnalysis.RDS')




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
                  distinct(WQM_STA_ID, .keep_all = TRUE), by = c('STATION_ID' = 'WQM_STA_ID')) %>%
    # last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary
      mutate(lakeStation = FALSE)
    
    return(stationTable)
  })
  
  
  

  ################################ Watershed Selection Tab ########################################
  
  output$DEQregionSelectionUI <- renderUI({req(vahu6)
    selectInput("DEQregionSelection", "Select DEQ Assessment Region", choices = unique(vahu6$ASSESS_REG))})
  

  # Pull AU data from server
  regionalAUs <- reactive({ 
    req(input$pullAUs)
    withProgress(message = 'Reading in Large Spatial File',
                 st_zm(st_as_sf(pin_get(paste0(input$DEQregionSelection, 'workingAUriverine'), board = 'rsconnect')) )) })     # change to final
  
  
  
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
                  options= list(scrollX = TRUE, pageLength = nrow(AUs()), scrollY = "300px", dom='Bti'),
                  selection = 'none')   })

  # Table of Stations within Selected VAHU6
  stationSummary <- reactive({req(huc6_filter())
    filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      distinct(FDT_STA_ID, .keep_all = TRUE)  %>% 
      dplyr::select(FDT_STA_ID:FDT_SPG_CODE, STA_LV2_CODE:Data_Source, Latitude, Longitude) %>% 
      dplyr::select(-FDT_DATE_TIME) %>% # drop date time bc confusing to users
      mutate(#`In Stations Table` = ifelse(FDT_STA_ID %in% unique(stationTable$STATION_ID), 'yes','no'),
        #`In Selected Region` = ifelse(FDT_STA_ID %in% filter(stationTable, REGION %in% DEQregionSelection)$STATION_ID, 'yes','no'),
        `Analyzed By App` = #ifelse(`In Stations Table` == 'yes'# && `In Selected Region` == 'yes', 'yes','no'))
          ifelse(FDT_STA_ID %in% unique(stationTable()$STATION_ID), 'yes','no')) })
  
  output$stationSummary <- DT::renderDataTable({req(stationSummary())
    DT::datatable(stationSummary(), rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(stationSummary()), 
                                                                    scrollY = "300px", dom='Bti'),
                  selection = 'none') %>%
      DT::formatStyle('Analyzed By App', target = 'row', backgroundColor = styleEqual(c('yes','no'), c('lightgray', 'yellow'))) })
     
  
  # Table of stations that were carried over from last cycle that have no data in current window
  carryoverStations <- reactive({req(huc6_filter(), stationTable())
    filter(stationTable(), VAHU6 %in% huc6_filter()$VAHU6 & str_detect(COMMENTS, "This station has no data")) })
  
  output$carryoverStationSummary <- DT::renderDataTable({
    req(carryoverStations())
    z <- carryoverStations() %>%  dplyr::select(STATION_ID:VAHU6, COMMENTS)

    DT::datatable(z, rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "300px", dom='Bti',
                                autoWidth = TRUE, columnDefs = list(list(width = '400px', targets = c(29)))),
                  selection = 'none') })
  
  ## Review AU Modal
  
  # Button to visualize modal map of AUs in selected VAHU6
  observeEvent(input$reviewAUs,{
    showModal(modalDialog(
      title="Spatially Preview Assessment Units for Selected VAHU6",
      leafletOutput('AUmap'),
      easyClose = TRUE))  })
  
  # AU modal map
  output$AUmap <- renderLeaflet({
    req(AUs(), huc6_filter())
    stations <- dplyr::select(stationSummary(), STATION_ID = FDT_STA_ID, LATITUDE = Latitude, LONGITUDE = Longitude, `Analyzed By App`) %>%
      bind_rows(filter(stationTable(), VAHU6 %in% huc6_filter()$VAHU6 & !STATION_ID %in% stationSummary()$FDT_STA_ID) %>%
                  dplyr::select(STATION_ID, LATITUDE, LONGITUDE) %>%
                  mutate(`Analyzed By App` = 'IM carryover with no data in window')) %>%
      st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
               remove = F, # don't remove these lat/lon cols from df
               crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
    
    z <- AUs()
    z$ID305B <- factor(z$ID305B) # drop extra factor levels so colors come out right
    
    m <- mapview(huc6_filter(), color = 'yellow',lwd= 5, label= NULL, layer.name = c('Selected HUC6'),
                 popup= leafpop::popupTable(huc6_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")), legend= FALSE) + 
      mapview(z, label= z$ID305B, layer.name = c('AUs in Selected HUC6'), zcol = "ID305B", 
              popup= leafpop::popupTable(z, zcol=c("ID305B","MILES","CYCLE","WATER_NAME","LOCATION" )), legend= FALSE) +
      mapview(stations, label = stations$STATION_ID, layer.name = c('Stations in Selected HUC6'), zcol = "Analyzed By App", 
              popup= leafpop::popupTable(stations, zcol=c("STATION_ID", "Analyzed By App")), legend= FALSE) 
      #mapview(stations, label = stations$STATION_ID, layer.name = c('Stations in Selected HUC6'), zcol = "Station Details", 
      #        popup= leafpop::popupTable(stations, zcol=c("STATION_ID", "Station Details")), legend= TRUE) 
    m@map })
  
  
  ## Status Overview Modal
  
  
  observeEvent(input$statusOverview,{
    showModal(modalDialog(
      title="Spatially Preview Station Statuses",
      helpText('This map allows users to quickly preview station status results. The map presents an overall station overview where 
               the station is colored based on the most harmful status category (e.g. if a station has 8 Supporting parameter statuses, 
               2 Insufficient parameter statuses, and 1 Impaired status, the station will be colored red to reflect the Impaired status). 
               The number of statuses in the most harmful category is reported for each station in the popup window (accessed by clicking
               the station in the map).'),
      helpText("Additionally, users may use the selection option below to investigate individual parameter statuses across the watershed."),
      helpText(strong('All statuses are based on the station table dataset uploaded to the application.')),
      fluidRow(column(4), # make sure drop down doesn't cover up map navigation features
               column(8,selectInput('chooseStatusParameter','Choose parameter to report station status.', 
                                     choices = c('Overall Status', unique(parameterSTATcrosswalk$Parameter))))),
      #verbatimTextOutput('testStationStatus'),
      leafletOutput('statusMap'),
      size = 'l',
      easyClose = TRUE))  })
  
  # VAHU6 station status 
  stationStatus <- reactive({req(input$statusOverview, huc6_filter(), stationTable())
    VAHU6stationSummary(stationTable(), huc6_filter(), parameterSTATcrosswalk) })
  
  # Station Status modal map
  #output$testStationStatus <- renderPrint({ stationStatus()})
  output$statusMap <- renderLeaflet({ req(input$chooseStatusParameter, nrow(stationStatus()) >0)
    indStatusMap(input$chooseStatusParameter, stationStatus())  })
  
  
  ################################ Assessment Unit Review Tab ########################################
  
  #  output$test <- renderPrint({#req(regionalAUs)
  #    glimpse(AUs())})
  
  
  # Show selected VAHU6
  output$selectedVAHU6 <- DT::renderDataTable({
    datatable(huc6_filter() %>% st_set_geometry(NULL) %>% select(VAHU6, VaName, Basin),
              rownames = FALSE, options= list(pageLength = 1, scrollY = "35px", dom='t'),
              selection = 'none')})
  
  
  ## Don't let user click pull data button if no conventionals data for VAHU6
  #observe({
  #  shinyjs::toggleState('pullHUCdata', nrow(AUs())!=0 )
  #})
  
  
  # Pull Conventionals data for selected AU on click
  conventionals_HUC <- reactive({#eventReactive( input$pullHUCdata, {
    filter(conventionals, Huc6_Vahu6 %in% huc6_filter()$VAHU6) %>%
      left_join(dplyr::select(stationTable(), STATION_ID:VAHU6,
                              WQS_ID:EPA_ECO_US_L3NAME),
                              #WQS_ID:`Max Temperature (C)`), 
                by = c('FDT_STA_ID' = 'STATION_ID')) %>%
      filter(!is.na(ID305B_1)) %>%
      pHSpecialStandardsCorrection() }) #correct pH to special standards where necessary
  
  output$AUselection_ <- renderUI({ req(conventionals_HUC())
    # AUs from data in conventionals and carryoverStations
    AUselectionOptions <- unique(c(conventionals_HUC()$ID305B_1, 
                            #dplyr::select(carryoverStations(), ID305B_1:ID305B_10) %>% as.character()))
                            # this method allows for multiple carryover stations to be concatinated correctly
                            dplyr::select(carryoverStations(), ID305B_1:ID305B_10) %>% 
                              mutate_at(vars(starts_with("ID305B")), as.character) %>%
                              pivot_longer(ID305B_1:ID305B_10, names_to = 'ID305B', values_to = 'keep') %>%
                              pull(keep) ))
    AUselectionOptions <- AUselectionOptions[!is.na(AUselectionOptions) & !(AUselectionOptions %in% c("NA", "character(0)", "logical(0)"))]
    selectInput('AUselection', 'Assessment Unit Selection', choices = AUselectionOptions)  })
  
  output$selectedAU <- DT::renderDataTable({req(conventionals_HUC(),input$AUselection)
    z <- filter(regionalAUs(), ID305B %in% input$AUselection) %>% st_set_geometry(NULL) %>% as.data.frame()
    datatable(z, rownames = FALSE, 
              options= list(pageLength = nrow(z),scrollX = TRUE, scrollY = "300px", dom='t'),
              selection = 'none')})
  
  output$stationSelection_ <- renderUI({ req(conventionals_HUC(), input$AUselection)
    z <- filter(conventionals_HUC(), ID305B_1 %in% input$AUselection | ID305B_2 %in% input$AUselection | 
                  ID305B_3 %in% input$AUselection | ID305B_4 %in% input$AUselection | ID305B_5 %in% input$AUselection | 
                  ID305B_6 %in% input$AUselection | ID305B_7 %in% input$AUselection | ID305B_8 %in% input$AUselection | 
                  ID305B_9 %in% input$AUselection | ID305B_10 %in% input$AUselection) %>%
      distinct(FDT_STA_ID) %>%
      pull()
    # add in carryover stations
    if(nrow(carryoverStations()) > 0){
      carryoverStationsInAU <- filter(carryoverStations(),  ID305B_1 %in% input$AUselection | ID305B_2 %in% input$AUselection | 
                                        ID305B_3 %in% input$AUselection | ID305B_4 %in% input$AUselection | ID305B_5 %in% input$AUselection | 
                                        ID305B_6 %in% input$AUselection | ID305B_7 %in% input$AUselection | ID305B_8 %in% input$AUselection | 
                                        ID305B_9 %in% input$AUselection | ID305B_10 %in% input$AUselection) %>%
        distinct(STATION_ID) %>%
        pull()
      if(length(carryoverStationsInAU) > 0){
        z <- c(z, carryoverStationsInAU)  } }
    
    fluidRow(selectInput('stationSelection', 'Station Selection', choices = sort(unique(z))),
             helpText("The stations available in the drop down are limited to stations with an ID305B_1:ID305B_10 designation equal 
                      to the selected AU. All AU's associated with the selected station can be viewed in the map below."))})
  
  AUData <- reactive({req(input$AUselection)#eventReactive( input$AUselection, {
    filter_at(conventionals_HUC(), vars(starts_with("ID305B")), any_vars(. %in% input$AUselection) ) }) 
  
  stationData <- reactive({req(input$stationSelection)#eventReactive( input$stationSelection, {
    filter(AUData(), FDT_STA_ID %in% input$stationSelection) })
  
  stationInfo <- reactive({req(input$stationSelection, AUData())
    filter(stationTable(), STATION_ID == input$stationSelection) %>% 
      select(STATION_ID:VAHU6, WQS_ID:Trout)})
  
  output$stationInfo <- DT::renderDataTable({ req(stationInfo())#req(stationData())
    z <- stationInfo() %>%
      #filter(stationTable(), STATION_ID == input$stationSelection) %>% 
      #select(STATION_ID:VAHU6, WQS_ID:Trout) %>% 
      t() %>% as.data.frame() %>% rename(`Station and WQS Information` = 1)
    DT::datatable(z, options= list(pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')  })
  
  
  output$stationMap <- renderLeaflet({req(nrow(stationInfo()) >0) # to prevent having no lat/lng data for that half second app is catching up after station change
    point <- dplyr::select(stationInfo(),  STATION_ID, starts_with('ID305B'), LATITUDE, LONGITUDE ) %>%
      st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
               remove = F, # don't remove these lat/lon cols from df
               crs = 4269) # add projection, needs to be geographic for now bc entering lat/lng
    segmentChoices <- dplyr::select(point, starts_with('ID305B')) %>% st_drop_geometry() %>% as.character()  
    segment <- filter(regionalAUs(), ID305B %in% segmentChoices)
    map1 <- mapview(segment,zcol = 'ID305B', label= segment$ID305B, layer.name = 'Assessment Unit (ID305B_1)',
                    popup= leafpop::popupTable(segment, zcol=c("ID305B","MILES","CYCLE","WATER_NAME")), legend= FALSE) + 
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
  
  
  
  
  ## Station Table View Section
  
  # Run longer analyses first
  ecoli <- reactive({req(stationData())
    bacteriaAssessmentDecision(stationData(), 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126)})
  enter <- reactive({req(stationData())
    bacteriaAssessmentDecision(stationData(), 'ENTEROCOCCI', 'LEVEL_ENTEROCOCCI', 10, 130, 35)})
  ammoniaAnalysisStation <- reactive({req(stationData())
    z <- filter(ammoniaAnalysis, StationID %in% unique(stationData()$FDT_STA_ID)) %>%
      map(1) 
    z$AmmoniaAnalysis })
  
  observe({
    req(nrow(ecoli()) > 0, nrow(enter()) > 0)# need to tell the app to wait for data to exist in these objects before smashing data together or will bomb out when switching between VAHU6's on the Watershed Selection Page
    siteData$stationTableOutput <- cbind(StationTableStartingData(stationData()),
                                         tempExceedances(stationData()) %>% quickStats('TEMP'),
                                         DOExceedances_Min(stationData()) %>% quickStats('DO'), 
                                         pHExceedances(stationData()) %>% quickStats('PH'),
                                         ecoli() %>% dplyr::select(ECOLI_EXC:ECOLI_STAT),#siteData$ecoli %>% dplyr::select(ECOLI_EXC:ECOLI_STAT),
                                         enter() %>% dplyr::select(ENTER_EXC:ENTER_STAT), #siteData$enter %>% dplyr::select(ENTER_EXC:ENTER_STAT)) %>%
                                         
                                         ammoniaDecision(list(acute = freshwaterNH3Assessment(ammoniaAnalysisStation(), 'acute'),
                                                              chronic = freshwaterNH3Assessment(ammoniaAnalysisStation(), 'chronic'),
                                                              fourDay = freshwaterNH3Assessment(ammoniaAnalysisStation(), 'four-day'))), 
                                         
                                         metalsExceedances(filter(WCmetals, FDT_STA_ID %in% stationData()$FDT_STA_ID) %>% 
                                                             dplyr::select(`ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`), 'WAT_MET'),
                                         metalsExceedances(filter(Smetals, FDT_STA_ID %in% stationData()$FDT_STA_ID) %>% 
                                                             dplyr::select(ARSENIC:ZINC), 'SED_MET'),
                                         benthicAssessment(stationData(), VSCIresults),
                                         countNutrients(stationData(), PHOSPHORUS_mg_L, LEVEL_PHOSPHORUS, 0.2) %>% quickStats('NUT_TP') %>% 
                                           mutate(NUT_TP_STAT = ifelse(NUT_TP_STAT != "S", "Review", NA)), # flag OE but don't show a real assessment decision
                                         countNutrients(stationData(), CHLOROPHYLL_A_ug_L, LEVEL_CHLOROPHYLL_A, NA) %>% quickStats('NUT_CHLA') %>%
                                           mutate(NUT_CHLA_STAT = NA)) %>% # don't show a real assessment decision) %>%
      mutate(COMMENTS = NA) %>%
      dplyr::select(-ends_with(c('exceedanceRate','Assessment Decision')))
  })
  
  #output$testOutside <- renderPrint({ecoli()})#siteData$ecoli})
  
  output$stationTableDataSummary <- DT::renderDataTable({
    req(stationData()$FDT_STA_ID == input$stationSelection, siteData$stationTableOutput)
    datatable(siteData$stationTableOutput, extensions = 'Buttons', escape=F, rownames = F, editable = TRUE,
              options= list(scrollX = TRUE, pageLength = nrow(siteData$stationTableOutput),
                            # hide certain columns
                            dom='Bt', buttons=list('copy',
                                                   list(extend='csv',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                   list(extend='excel',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep='')))),
              selection = 'none') %>% 
      formatStyle(c('TEMP_EXC','TEMP_SAMP','TEMP_STAT'), 'TEMP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('DO_EXC','DO_SAMP','DO_STAT'), 'DO_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('PH_EXC','PH_SAMP','PH_STAT'), 'PH_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('ECOLI_EXC','ECOLI_SAMP','ECOLI_GM_EXC','ECOLI_GM_SAMP','ECOLI_STAT'), 'ECOLI_STAT', backgroundColor = styleEqual(c('IM'), c('red'))) %>%
      formatStyle(c('ENTER_SAMP','ENTER_EXC',"ENTER_GM_EXC","ENTER_GM_SAMP",'ENTER_STAT'), 'ENTER_STAT', backgroundColor = styleEqual(c('IM'), c('red'))) %>%
      formatStyle(c('AMMONIA_EXC','AMMONIA_STAT'), 'AMMONIA_STAT', backgroundColor = styleEqual(c('Review', 'IM'), c('yellow','red'))) %>%
      formatStyle(c('WAT_MET_EXC','WAT_MET_STAT'), 'WAT_MET_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('SED_MET_EXC','SED_MET_STAT'), 'SED_MET_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('BENTHIC_STAT'), 'BENTHIC_STAT', backgroundColor = styleEqual(c('Review'), c('yellow'))) %>%
      formatStyle(c('NUT_TP_EXC','NUT_TP_SAMP'), 'NUT_TP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) 
    
      
  })
  
  
  ## PWS table
  output$PWStable <- DT::renderDataTable({
    req(stationData())
    if(is.na(unique(stationData()$PWS))){
      PWSconcat <- tibble(STATION_ID = unique(stationData()$FDT_STA_ID),
                          PWS= 'PWS Standards Do Not Apply To Station')
      DT::datatable(PWSconcat, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(PWSconcat), dom='t'),
                    selection = 'none')
      
    } else {
      PWSconcat <- cbind(tibble(STATION_ID = unique(stationData()$FDT_STA_ID)),
                         assessPWS(stationData(), NITRATE, LEVEL_NITRATE, 10, 'PWS_Nitrate'),
                         assessPWS(stationData(), CHLORIDE, LEVEL_CHLORIDE, 250, 'PWS_Chloride'),
                         assessPWS(stationData(), SULFATE_TOTAL, LEVEL_SULFATE_TOTAL, 250, 'PWS_Total_Sulfate')) %>%
        dplyr::select(-ends_with('exceedanceRate')) 
      
      DT::datatable(PWSconcat, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(PWSconcat), dom='t'),
                    selection = 'none') %>% 
        formatStyle(c("PWS_Nitrate_EXC","PWS_Nitrate_SAMP","PWS_Nitrate_STAT"), "PWS_Nitrate_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
        formatStyle(c("PWS_Chloride_EXC","PWS_Chloride_SAMP","PWS_Chloride_STAT"), "PWS_Chloride_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) %>%
        formatStyle(c("PWS_Total_Sulfate_EXC","PWS_Total_Sulfate_SAMP","PWS_Total_Sulfate_STAT"), "PWS_Total_Sulfate_STAT", backgroundColor = styleEqual(c('Review'), c('red'))) } })
  
  
  #### Data Sub Tab ####---------------------------------------------------------------------------------------------------
  
  # Display Data 
  output$AURawData <- DT::renderDataTable({ req(AUData())
    DT::datatable(AUData(), extensions = 'Buttons', escape=F, rownames = F, 
                  options= list(scrollX = TRUE, pageLength = nrow(AUData()), scrollY = "300px", 
                                dom='Btf', buttons=list('copy',
                                                        list(extend='csv',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                        list(extend='excel',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')))),
                  selection = 'none')})
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
  
  ## DO Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(DOPlotlySingleStation,'DO', AUData, stationSelected)
  
  ## Specific Conductivity Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(SpCondPlotlySingleStation,'SpCond', AUData, stationSelected)
  
  ## Salinity Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(salinityPlotlySingleStation,'salinity', AUData, stationSelected)
  
  ## Total Nitrogen Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(TNPlotlySingleStation,'TN', AUData, stationSelected)
  
  ## Ammonia Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(AmmoniaPlotlySingleStation,'Ammonia', AUData, stationSelected, ammoniaAnalysis)
  
  ## Total Phosphorus Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(TPPlotlySingleStation,'TP', AUData, stationSelected)
  
  ## Fecal Coliform Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(fecalPlotlySingleStation,'fecal', AUData, stationSelected)
  
  ## ECOLI Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(EcoliPlotlySingleStation,'Ecoli', AUData, stationSelected, ecoli)#siteData$ecoli)
  
  ## Enteroccoci Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(EnteroPlotlySingleStation,'Entero', AUData, stationSelected, enter)#siteData$enter)
  
  ## Chlorophyll a Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(chlAPlotlySingleStation,'chlA', AUData, stationSelected)
 
  ## Suspended Sediments Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(SSCPlotlySingleStation,'SSC', AUData, stationSelected)
  
  ## Nitrate Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(NitratePlotlySingleStation,'Nitrate', AUData, stationSelected)
  
  ## Chloride Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(ClPlotlySingleStation,'Cl', AUData, stationSelected)
  
  ## Sulfate Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(DSulfatePlotlySingleStation,'DSulfate', AUData, stationSelected)
  
  
  
  # Other Data Sources
  
  #### Benthics Sub Tab ####---------------------------------------------------------------------------------------------------
  callModule(BenthicsPlotlySingleStation,'Benthics', AUData, stationSelected, VSCIresults, VCPMI63results, VCPMI65results)
  
  #### Metals Sub Tab ####---------------------------------------------------------------------------------------------------
  callModule(metalsTableSingleStation,'metals', AUData, WCmetals ,Smetals, stationSelected)
  
})