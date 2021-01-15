#source('appTestingData.R')



TSIPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(3,uiOutput(ns('oneStationSelectionUI'))),
               column(3, selectInput(ns('TSIparameter'),'TSI Parameter To Plot', choices = c('Secchi Depth', 'Chlorophyll a', 'Total Phosphorus'))),
               column(3, helpText('Users may change the parameter plotted below with the TSI Parameter To Plot utility.')),
               column(3,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application.'),
      plotlyOutput(ns('plotly')),
      br(),hr(),br(),
      #fluidRow(
        #column(7, 
      h5('Trophic State Index calculations for the ',span(strong('selected site')),' are highlighted below.'),
      dataTableOutput(ns('rangeTableSingleSite')), #),
      #  column(1),
      #  column(4, h5('Individual TSI statistics for the ',span(strong('selected site')),' are highlighted below.'),
      #         dataTableOutput(ns("stationExceedanceRate")))),
      br(),
      wellPanel(
        h4(strong('AU Assessment')),
        helpText("The 2022 IR Guidance states: 'If multiple stations are sampled on the lake/reservoir or within a 
                 lake/reservoir assessment unit, the individual TSI equations should be calculated at each station 
                 and then averaged (using a median or arithmetic mean) to determine the values for the waterbody.'"),
        helpText('This application uses mean as the central tendency statistic for TSI.'),
        fluidRow(
          column(7, h5('Trophic State Index calculations for the ',span(strong('Assessment Unit')),' are highlighted below.'),
                 dataTableOutput(ns('rangeTableAU'))),
          column(1),
          column(4, h5('TSI statistics for the ',span(strong('AU')),' are highlighted below.'),
                 dataTableOutput(ns("AUExceedanceRate"))) ) )
      
    )
  )
}

TSIPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove, AUselectionFromOutsideModal){
  ns <- session$ns
  
  # Select One station for individual review
  output$oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({    req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) })
  
  # Button to visualize modal table of available parameter data
  observeEvent(input$reviewData,{
    showModal(modalDialog(
      title="Review Raw Data for Selected Station and Parameter",
      helpText('This table subsets the conventionals raw data by station selected in Single Station Visualization Section drop down and
               parameter currently reviewing. Scroll right to see the raw parameter values and any data collection comments. Data analyzed
               by app is highlighted in gray (all DEQ data and non agency/citizen monitoring Level III), data counted by app and noted in
               comment fields is highlighed in yellow (non agency/citizen monitoring Level II), and data NOT CONSIDERED in app is noted in
               orange (non agency/citizen monitoring Level I).'),
      DT::dataTableOutput(ns('parameterData')),
      easyClose = TRUE))  })
  
  # modal parameter data
  output$parameterData <- DT::renderDataTable({
    req(oneStation())
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID:FDT_COMMENT, SECCHI_DEPTH_M, RMK_SECCHI_DEPTH, LEVEL_SECCHI_DEPTH, 
                                     CHLOROPHYLL_A_ug_L, RMK_CHLOROPHYLL_A, LEVEL_CHLOROPHYLL_A,
                                     PHOSPHORUS_mg_L, RMK_PHOSPHORUS, LEVEL_PHOSPHORUS, LakeStratification)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t'),
                  selection = 'none') %>%
      formatStyle(c('SECCHI_DEPTH_M','RMK_SECCHI_DEPTH', 'LEVEL_SECCHI_DEPTH'), 'LEVEL_SECCHI_DEPTH', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray')) %>%
      formatStyle(c('CHLOROPHYLL_A_ug_L','RMK_CHLOROPHYLL_A', 'LEVEL_CHLOROPHYLL_A'), 'LEVEL_CHLOROPHYLL_A', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray')) %>%
      formatStyle(c('PHOSPHORUS_mg_L','RMK_PHOSPHORUS', 'LEVEL_PHOSPHORUS'), 'LEVEL_PHOSPHORUS', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))  })
  
  
  TSIoneStation <- reactive({req(oneStation())
    TSIcalculation(oneStation())  })
  
  output$plotly <- renderPlotly({req(TSIoneStation(), input$TSIparameter)
    datOG <- TSIoneStation()
    dat <- TSIoneStation() %>%
      dplyr::select( associatedData) %>%
      unnest(cols = c(associatedData)) %>%
      mutate(`Overall TSI SD` = datOG$TSI_SD,
             `Overall TSI Chl a` = datOG$TSI_chla,
             `Overall TSI TP` = datOG$TSI_TP)
    
    TSIplotly(dat, input$TSIparameter)  })
    
  output$rangeTableSingleSite <- renderDataTable({    req(TSIoneStation())
    z <- TSIoneStation() %>% dplyr::select(-associatedData) %>%
      rename('Mean Secchi Depth' = 'meanSD',
             'TSI Secchi Depth' = 'TSI_SD',
             'Mean Chlorophyll a' = 'meanchla',
             'TSI Chlorophyll a' = 'TSI_chla',
             'Mean Total Phosphorus' = 'meanTP',
             'TSI Total Phosphrus' = 'TSI_TP')
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "70px", dom='t'),
              selection = 'none') %>%
      formatRound(columns=c('Mean Secchi Depth', 'TSI Secchi Depth','Mean Chlorophyll a','TSI Chlorophyll a','Mean Total Phosphorus','TSI Total Phosphrus'), digits=3)})
  
  
  # output$stationExceedanceRate <- renderDataTable({    req(ns(input$oneStationSelection), oneStation())
  #   z <- TSIassessment(oneStation())
  #   datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "60px", dom='t'),
  #             selection = 'none') })
  # 
  
  output$rangeTableAU <- renderDataTable({    req(TSIoneStation())
    z <- TSIcalculation(filter(AUdata(), ID305B_1 %in% AUselectionFromOutsideModal())) %>% dplyr::select(-associatedData)%>%
      rename('Mean Secchi Depth' = 'meanSD',
             'TSI Secchi Depth' = 'TSI_SD',
             'Mean Chlorophyll a' = 'meanchla',
             'TSI Chlorophyll a' = 'TSI_chla',
             'Mean Total Phosphorus' = 'meanTP',
             'TSI Total Phosphrus' = 'TSI_TP')
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "70px", dom='t'),
              selection = 'none') %>%
      formatRound(columns=c('Mean Secchi Depth', 'TSI Secchi Depth','Mean Chlorophyll a','TSI Chlorophyll a','Mean Total Phosphorus','TSI Total Phosphrus'), digits=3)})
  
  
  output$AUExceedanceRate <- renderDataTable({    req(TSIoneStation())
    z <- TSIassessment(filter(AUdata(), ID305B_1 %in% AUselectionFromOutsideModal())) 
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "70px", dom='t'),
              selection = 'none') })
}




ui <- fluidPage(
  uiOutput('AUselection_'),
  h5(strong('AU information from last cycle')),
  #  DT::dataTableOutput('selectedAU'),br(),
  uiOutput('stationSelection_'),
  TSIPlotlySingleStationUI('TSI') )

server <- function(input,output,session){
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
      left_join(lakeNutStandards, by = c('Lake_Name')) %>%
      mutate(lakeStation = TRUE)
  })  #for testing
  
  # Pull AU data from server
  # for testing
  # Pull AU data from server
  regionalAUs <- reactive({ 
    withProgress(message = 'Reading in Large Spatial File',
                 st_zm(st_as_sf(pin_get('AUreservoir_EVJ', board = 'rsconnect')) ) %>%
                   lakeNameStandardization())  }) # for testing
  
  conventionalsLake <- reactive({#eventReactive( input$pullHUCdata, {
    filter(conventionals, FDT_STA_ID %in% stationSelectionOptions1) %>% #lake_filter1$STATION_ID) %>%
      left_join(dplyr::select(stationTable(), STATION_ID:VAHU6, lakeStation,
                              WQS_ID:`Total Phosphorus (ug/L)`),
                #WQS_ID:`Max Temperature (C)`), 
                by = c('FDT_STA_ID' = 'STATION_ID')) %>%
      filter(!is.na(ID305B_1)) %>%
      pHSpecialStandardsCorrection() %>% #correct pH to special standards where necessary
      thermoclineDepth() }) # adds thermocline information and SampleDate
  
  output$AUselection_ <- renderUI({ req(conventionalsLake())
    selectInput('AUselection', 'Assessment Unit Selection', choices = unique(conventionalsLake()$ID305B_1))  })
  
  output$selectedAU <- DT::renderDataTable({req(conventionalsLake(),input$AUselection)
    z <- filter(regionalAUs(), ID305B %in% input$AUselection) %>% st_set_geometry(NULL) %>% as.data.frame()
    datatable(z, rownames = FALSE, 
              options= list(pageLength = nrow(z),scrollX = TRUE, scrollY = "300px", dom='t'))})
  
  output$stationSelection_ <- renderUI({ req(conventionalsLake(), input$AUselection)
    z <- filter(conventionalsLake(), ID305B_1 %in% input$AUselection | ID305B_2 %in% input$AUselection | 
                  ID305B_3 %in% input$AUselection | ID305B_4 %in% input$AUselection | ID305B_5 %in% input$AUselection | 
                  ID305B_6 %in% input$AUselection | ID305B_7 %in% input$AUselection | ID305B_8 %in% input$AUselection | 
                  ID305B_9 %in% input$AUselection | ID305B_10 %in% input$AUselection) %>%
      distinct(FDT_STA_ID)
    fluidRow(selectInput('stationSelection', 'Station Selection', choices = unique(z$FDT_STA_ID)),
             helpText("The stations available in the drop down are limited to stations with an ID305B_1:ID305B_10 designation equal 
                      to the selected AU. All AU's associated with the selected station can be viewed in the map below."))})
  
  AUData <- eventReactive( input$AUselection, {
    filter_at(conventionalsLake(), vars(starts_with("ID305B")), any_vars(. %in% input$AUselection) ) }) 
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData(), FDT_STA_ID %in% input$stationSelection) })
  
  stationSelected <- reactive({input$stationSelection})
  
  
  
  # stationData <- eventReactive( input$stationSelection, {
  #   filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  # stationSelected <- reactive({input$stationSelection})
  # 
  # AUselection <- reactive({as.character(selectedAU1) }) #input$AUselection)})
  # 
  # AUData <- reactive({filter_at(conventionalsLake1, vars(starts_with("ID305B")), any_vars(. %in% selectedAU1) ) }) #"VAW-L07L_ROA03A10"))})#
  
  AUselection <- reactive({as.character(input$AUselection)})
  
  callModule(TSIPlotlySingleStation,'TSI', AUData, stationSelected, AUselection)
  
}

shinyApp(ui,server)

  