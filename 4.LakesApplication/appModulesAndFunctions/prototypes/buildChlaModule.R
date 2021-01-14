#source('appTestingData.R')


chlAPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(3,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               column(2,uiOutput(ns('changeLacustrineUI'))),
               column(2,helpText('The default Lacustrine Zone selection is what is designated in the user uploaded stations table.')),
               column(1),
               column(3,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application.'),
      plotlyOutput(ns('plotly')),
      br(),hr(),br(),
      fluidRow(
        column(7, h5('Annual Chlorophyll a 90th percentiles for the ',span(strong('selected site')),' are displayed below.'),
               helpText('Note: Assessment is based on the two most recent monitoring years that data are available within the 
                        assessment window. A third sampling year may be needed if the two previous years result in differing 
                        assessment statuses. '),
               dataTableOutput(ns("annual90TableSingleSite"))),
        column(1),
        column(4, 
               h5('Chlorophyll a exceedance statistics for the ',span(strong('selected site')),' are highlighted below.
                  These are the results reflected in the stations table above.'),
               dataTableOutput(ns("stationExceedanceRate")))),
      br(),
      wellPanel(
        h4(strong('AU Assessment')),
        helpText("The 2022 IR Guidance states: For lake or reservoir assessment units with multiple stations, 
        chlorophyll-a and total phosphorous data should be pooled by calculating the median of same-month 
        observations from April to October of any given year in a homogenous unit. The 90th percentile of monthly 
        chlorophyll-a medians is the value used to compare to the chlorophyll-a criterion for a particular 
        lake/reservoir.  The median of monthly TP medians should be used to assess against the applicable total 
        phosphorous criterion. '"),
        #helpText(strong('This application provides an AU level assessment, but it is up to the assessor to determine 
        #                if algaecides have been used in any part of the lake.')),
        fluidRow(
          column(7, h5('Annual Chlorophyll a 90th percentiles for only stations that have an ID305B_1 matching
                       the ',span(strong('selected Assessment Unit')),' are displayed below.'),
                 dataTableOutput(ns('annual90TableAU'))),
          column(1),
          column(4, h5('Chlorophyll a exceedance statistics for the ',span(strong('Assessment Unit')),' are highlighted below.'),
                 dataTableOutput(ns("AUExceedanceRate"))))
          )
    )
  )
}


chlAPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove, AUselectionFromOutsideModal){
  ns <- session$ns
  
  # Select One station for individual review
  output$oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation_original <- reactive({
    req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(CHLOROPHYLL_A_ug_L))})
  
  # Option to change lacustrine designation and force function to calculate resutls
  output$changeLacustrineUI <- renderUI({
    req(oneStation_original())
    selectInput(ns('changeLacustrine'),strong('Lacustrine Zone Designation For Analysis'),
                choices= c('NA', 'YES'),
                width='400px', selected = unique(oneStation_original()$LACUSTRINE)) })
  
  # change WQS for rest of module if user chooses to do so
  oneStation <- reactive({req(oneStation_original(), input$changeLacustrine)
    mutate(oneStation_original(), LACUSTRINE = input$changeLacustrine) })
  
  
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
    req(oneStation_original())
    parameterFilter <- dplyr::select(oneStation_original(), FDT_STA_ID:FDT_COMMENT, CHLOROPHYLL_A_ug_L, RMK_CHLOROPHYLL_A, LEVEL_CHLOROPHYLL_A, LACUSTRINE)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t'),
                  selection = 'none') %>%
      formatStyle(c('CHLOROPHYLL_A_ug_L','RMK_CHLOROPHYLL_A','LEVEL_CHLOROPHYLL_A'), 'LEVEL_CHLOROPHYLL_A', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray')) })
  
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation())
    dat <- mutate(oneStation(), top = `Chlorophyll a (ug/L)`,
                 LakeStratification = replace_na(LakeStratification,"NA")) %>%
      mutate(LakeStratification = factor(LakeStratification,levels=c("Epilimnion",'NA',"Hypolimnion")))#,ordered=T)
    plot_ly(data=dat) %>%
      add_lines(data=dat, x=~SampleDate,y=~top, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text="Chlorophyll a Limit", name="Chlorophyll a Limit") %>%
      add_markers(x= ~SampleDate, y= ~CHLOROPHYLL_A_ug_L,mode = 'scatter', name="Chlorophyll a (ug/L)",
                  color=~LakeStratification, #marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Chlorophyll a: ",CHLOROPHYLL_A_ug_L,"ug/L"),
                                               paste("LakeStratification: ",LakeStratification)))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Chlorophyll a (ug/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
  
  
  output$annual90TableSingleSite <- renderDataTable({req(oneStation())
    z <- chlA_analysis(oneStation()) %>%
      rename('Chlorophyll a 90th Percentile' = 'pct90',
             'Chlorophyll a Limit' = 'Chlorophyll a (ug/L)',
             'Chlorophyll a Exceedance' = 'chlA_Exceedance')
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none') })
  
  output$stationExceedanceRate <- renderDataTable({req(oneStation())
    z <- chlA_Assessment(oneStation())
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "70px", dom='t'),
              selection = 'none') })
  

  # AU Assessment Section
   justAUID305B_1 <- reactive({
     filter(AUdata(), ID305B_1 %in% AUselectionFromOutsideModal()) %>%
       filter(!is.na(CHLOROPHYLL_A_ug_L))})
  
   output$annual90TableAU <- renderDataTable({req(justAUID305B_1())
     z <- chlA_analysis(justAUID305B_1()) %>%
       rename('Chlorophyll a 90th Percentile' = 'pct90',
              'Chlorophyll a Limit' = 'Chlorophyll a (ug/L)',
              'Chlorophyll a Exceedance' = 'chlA_Exceedance')
     datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
               selection = 'none') })
   
   output$AUExceedanceRate <- renderDataTable({req(justAUID305B_1())
     z <- chlA_Assessment(justAUID305B_1())
     datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "70px", dom='t'),
               selection = 'none') })
}


ui <- fluidPage(
  uiOutput('AUselection_'),
  h5(strong('AU information from last cycle')),
  #  DT::dataTableOutput('selectedAU'),br(),
  uiOutput('stationSelection_'),
  chlAPlotlySingleStationUI('chlA'))

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
  
  callModule(chlAPlotlySingleStation,'chlA', AUData, stationSelected, AUselection)
  
}

shinyApp(ui,server)

