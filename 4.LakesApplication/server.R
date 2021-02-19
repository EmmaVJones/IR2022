#source('global.R')

# Pull data from server
conventionals <- pin_get('conventionals2022IRdraftWithSecchi', board = "rsconnect") %>%
  
  
  
  #pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
####vahu6 <- st_as_sf(pin_get("vahu6", board = "rsconnect")) # bring in as sf object
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
# placeholder for now, shouldn't be a spatial file
historicalStationsTable <-  st_read('data/GIS/va20ir_wqms.shp') %>%
  st_drop_geometry()#read_csv('data/stationsTable2022begin.csv') # last cycle stations table (forced into new station table format)
WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")
regions <- st_read('data/GIS/AssessmentRegions_simple.shp')
WCmetals <- pin_get("WCmetals-2020IRfinal",  board = "rsconnect")
Smetals <- pin_get("Smetals-2020IRfinal",  board = "rsconnect")

# Bring in local data (for now)
ammoniaAnalysis <- readRDS('userDataToUpload/processedStationData/ammoniaAnalysis.RDS')
lakeNutStandards <- read_csv('data/9VAC25-260-187lakeNutrientStandards.csv')



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
  
  
   stationTable <- reactive({
    req(input$stationsTable)
    inFile <- input$stationsTable
    stationTable <- read_csv(inFile$datapath,
                             col_types = cols(COMMENTS = col_character(),
                                              LACUSTRINE = col_character())) %>% # force to character bc parsing can incorrectly guess logical based on top 1000 rows
      #fix periods in column names from excel
      as_tibble() %>%
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
      
      # extra special step
      mutate(Lake_Name = case_when(STATION_ID %in% c('2-TRH000.40') ~ 'Thrashers Creek Reservoir',
                                   TRUE ~ as.character(Lake_Name))) %>%
      
      
      left_join(lakeNutStandards, by = c('Lake_Name')) %>%
      ## lake drummond special standards
      mutate(`Chlorophyll a (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 35,
                                                TRUE ~ as.numeric(`Chlorophyll a (ug/L)`)),
             `Total Phosphorus (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 40,
                                                   TRUE ~ as.numeric(`Total Phosphorus (ug/L)`))) %>%
      mutate(lakeStation = TRUE)
  })
  
  
  
  
  
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
    filter(conventionals, FDT_STA_ID %in% lake_filter()$STATION_ID) %>%
      left_join(dplyr::select(stationTable(), STATION_ID:VAHU6, lakeStation,
                              WQS_ID:`Total Phosphorus (ug/L)`),
                #WQS_ID:`Max Temperature (C)`), 
                by = c('FDT_STA_ID' = 'STATION_ID')) %>%
      filter(!is.na(ID305B_1)) %>%
      pHSpecialStandardsCorrection() %>% #correct pH to special standards where necessary
      thermoclineDepth() }) # adds thermocline information and SampleDate
  
  
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
  
  
  ## Station Table View Section
  
  # Run longer analyses first
  ecoli <- reactive({req(stationData())
    bacteriaAssessmentDecision(stationData(), 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126)})
  # save individual ecoli results for later
  AUmedians <- reactive({ req(AUData(), input$AUselection)
    AUData() %>%
      filter(ID305B_1 %in% input$AUselection) %>%# run ecoli by only 1 AU at a time
      group_by(SampleDate) %>%
      filter(!is.na(ECOLI)) %>%
      mutate(EcoliDailyMedian = median(ECOLI, na.rm = TRUE)) %>%
      dplyr::select(ID305B_1, FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, SampleDate, EcoliDailyMedian, ECOLI, RMK_ECOLI, LEVEL_ECOLI) %>%
      arrange(SampleDate) %>% ungroup() })
  
  # need to run analysis on only one point per day
  AUmediansForAnalysis <- reactive({req(AUmedians())
    AUmedians() %>% 
      filter(! LEVEL_ECOLI %in% c('Level I', 'Level II')) %>%
      mutate(ECOLI_Station = ECOLI,
             ECOLI = EcoliDailyMedian,
             FDT_STA_ID = unique(ID305B_1),
             FDT_DATE_TIME = SampleDate) %>%
      dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, SampleDate, ECOLI, ECOLI_Station, RMK_ECOLI, LEVEL_ECOLI) %>%
      distinct(SampleDate, .keep_all = T) })
  ecoliAU <- reactive({req(AUmediansForAnalysis())
    bacteriaAssessmentDecision(AUmediansForAnalysis(), 'ECOLI', 'LEVEL_ECOLI', 10, 410, 126)})
  ammoniaAnalysisStation <- reactive({req(stationData())
    z <- filter(ammoniaAnalysis, StationID %in% unique(stationData()$FDT_STA_ID)) %>%
      map(1) 
    z$AmmoniaAnalysis })
  
  observe({
    req(nrow(ecoli()) > 0)# need to tell the app to wait for data to exist in these objects before smashing data together or will bomb out when switching between VAHU6's on the Watershed Selection Page
    siteData$stationTableOutput <- cbind(StationTableStartingData(stationData()),
                                         tempExceedances(stationData()) %>% quickStats('TEMP'),
                                         DOExceedances_Min(stationData()) %>% quickStats('DO'), 
                                         pHExceedances(stationData()) %>% quickStats('PH'),
                                         ecoli() %>% dplyr::select(ECOLI_EXC:ECOLI_STAT),
                                         tibble(ENTER_EXC = NA, ENTER_SAMP = NA, ENTER_GM_EXC = NA, ENTER_GM_SAMP = NA, ENTER_STAT = NA),
                                         ammoniaDecision(list(acute = freshwaterNH3Assessment(ammoniaAnalysisStation(), 'acute'),
                                                              chronic = freshwaterNH3Assessment(ammoniaAnalysisStation(), 'chronic'),
                                                              fourDay = freshwaterNH3Assessment(ammoniaAnalysisStation(), 'four-day'))),
                                         # Roger's water column metals analysis, transcribed
                                         metalsExceedances(filter(WCmetals, FDT_STA_ID %in% stationData()$FDT_STA_ID) %>% 
                                                             dplyr::select(`ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`), 'WAT_MET'),
                                         # Mark's water column PCB results, flagged
                                         PCBmetalsDataExists(filter(markPCB, str_detect(SampleMedia, 'Water')) %>%
                                                               filter(StationID %in%  stationData()$FDT_STA_ID), 'WAT_TOX'),
                                         # Roger's sediment metals analysis, transcribed
                                         metalsExceedances(filter(Smetals, FDT_STA_ID %in% stationData()$FDT_STA_ID) %>% 
                                                             dplyr::select(ARSENIC:ZINC), 'SED_MET'),
                                         # Mark's sediment PCB results, flagged
                                         PCBmetalsDataExists(filter(markPCB, str_detect(SampleMedia, 'Sediment')) %>%
                                                               filter(StationID %in%  stationData()$FDT_STA_ID), 'SED_TOX'),
                                         # Gabe's fish metals results, flagged
                                         PCBmetalsDataExists(filter(fishMetals, Station_ID %in% stationData()$FDT_STA_ID), 'FISH_MET'),
                                         # Gabe's fish PCB results, flagged
                                         PCBmetalsDataExists(filter(fishPCB, `DEQ rivermile` %in%  stationData()$FDT_STA_ID), 'FISH_TOX'),
                                         tibble(BENTHIC_STAT = NA, BENTHIC_WOE_CAT= NA, BIBI_SCORE = NA),
                                         TP_Assessment(stationData()),
                                         chlA_Assessment(stationData()) ) %>%
      mutate(COMMENTS = NA) %>%
      dplyr::select(-ends_with(c('exceedanceRate','Assessment Decision')))
  })
  
  #output$test <- renderPrint({TP_Assessment(stationData())})
  
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
      formatStyle(c('AMMONIA_EXC','AMMONIA_STAT'), 'AMMONIA_STAT', backgroundColor = styleEqual(c('Review', 'IM'), c('yellow','red'))) %>%
      formatStyle(c('WAT_MET_EXC','WAT_MET_STAT'), 'WAT_MET_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('WAT_TOX_EXC','WAT_TOX_STAT'), 'WAT_TOX_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('SED_MET_EXC','SED_MET_STAT'), 'SED_MET_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%   
      formatStyle(c('SED_TOX_EXC','SED_TOX_STAT'), 'SED_TOX_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('FISH_MET_EXC','FISH_MET_STAT'), 'FISH_MET_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%   
      formatStyle(c('FISH_TOX_EXC','FISH_TOX_STAT'), 'FISH_TOX_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('BENTHIC_STAT'), 'BENTHIC_STAT', backgroundColor = styleEqual(c('Review'), c('yellow'))) %>%
      formatStyle(c('NUT_TP_EXC','NUT_TP_SAMP'), 'NUT_TP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>% 
      formatStyle(c('NUT_CHLA_EXC','NUT_CHLA_SAMP'), 'NUT_CHLA_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red')))  })
  
  
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
                         assessPWS(stationData(), NITRATE_mg_L, LEVEL_NITRATE, 10, 'PWS_Nitrate'),
                         assessPWS(stationData(), CHLORIDE_mg_L, LEVEL_CHLORIDE, 250, 'PWS_Chloride'),
                         assessPWS(stationData(), SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL, 250, 'PWS_Total_Sulfate')) %>%
        dplyr::select(-ends_with('exceedanceRate')) 
      
      DT::datatable(PWSconcat, escape=F, rownames = F, options= list(scrollX = TRUE, pageLength = nrow(PWSconcat), dom='t'),
                    selection = 'none') %>% 
        formatStyle(c("PWS_Nitrate_EXC","PWS_Nitrate_SAMP","PWS_Nitrate_STAT"), "PWS_Nitrate_STAT", backgroundColor = styleEqual(c('Review'), c('red')))  } })
  
  
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
  
  
  ## Thermocline Sub Tab  ##------------------------------------------------------------------------------------------------------
  callModule(thermoclinePlotlySingleStation,'thermocline', AUData, stationSelected)
  
  ## Temperature Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(temperaturePlotlySingleStation,'temperature', AUData, stationSelected)
  
  ## Dissolved Oxygen Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(DOPlotlySingleStation,'DO', AUData, stationSelected)
  
  ## pH Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(pHPlotlySingleStation,'pH', AUData, stationSelected)
  
  ## E. coli Sub Tab ##------------------------------------------------------------------------------------------------------
  # single station tab
  callModule(EcoliPlotlySingleStation,'Ecoli', AUData, stationSelected, ecoli)
  # AU tab
  callModule(EcoliPlotlyAU,'EcoliAU', AUData, AUmedians, AUmediansForAnalysis, ecoliAU)
  
  ## For Nutrients
  AUselection <- reactive({as.character(input$AUselection)})
  
  ## Chlorophyll a Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(chlAPlotlySingleStation,'chlA', AUData, stationSelected, AUselection)
  
  ## Total Phosphorus Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(TPPlotlySingleStation,'TP', AUData, stationSelected, AUselection)
  
  ## Trophic State Index Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(TSIPlotlySingleStation,'TSI', AUData, stationSelected, AUselection)  
  
  ## Ammonia Sub Tab ##------------------------------------------------------------------------------------------------------
  callModule(AmmoniaPlotlySingleStation,'Ammonia', AUData, stationSelected, ammoniaAnalysis)
  
  ## Nitrate Sub Tab ##-----------------------------------------------------------------------------------------------------
  callModule(NitratePlotlySingleStation,'Nitrate', AUData, stationSelected)
  
  ## Chloride Sub Tab ##-----------------------------------------------------------------------------------------------------
  callModule(ClPlotlySingleStation,'Cl', AUData, stationSelected)
  
  ## Sulfate Sub Tab ##-----------------------------------------------------------------------------------------------------
  callModule(DSulfatePlotlySingleStation,'DSulfate', AUData, stationSelected)
  
})