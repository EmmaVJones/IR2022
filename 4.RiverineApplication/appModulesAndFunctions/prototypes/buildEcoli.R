source('appTestingData.R')

EcoliPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6, helpText('Station used for this module is the station selected above to expedite app rendering.')),#uiOutput(ns('oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      #verbatimTextOutput(ns('test')),
      plotlyOutput(ns('plotly')),
      br(),hr(),br(),
      fluidRow(
        h4(strong('New Standard (STV= 410 CFU / 100 mL, geomean = 126 CFU / 100 mL with additional sampling requirements)')), 
        column(6, h5('All E. coli records that are ',span(strong('above the criteria')),' for the ',
                     span(strong('selected site')),' are highlighted below.',
                     span(strong('If no data are reflected in below tables then no data exceeded the respective criteria.'))),
               helpText('The below table highlights all analyzed windows that have either STV violations OR geomean violations. Note
                        the number of samples in the window, STV Assessment, and Geomean Assessment columns for context. These violations
                        are important to understand the dataset, but the verbose assessment decision in the table to the right is where one should look
                        for assistance choosing which of the potential violations are driving the decision. Explore the dataset in 
                        90 day windows in the interactive graph below and the full dataset with assessment decisions paired with each window
                        in the bottom-most table.'),
               DT::dataTableOutput(ns('exceedancesNEWStdTableSingleSite')),
               br()),
        column(6, br(), br(),br(), br(),br(), br(),br(), br(),
               h5('Individual E. coli exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               DT::dataTableOutput(ns("newStdTableSingleSite")),
               h4(strong('See below section for detailed analysis with new recreation standard.')),
               br())),
      br(),hr(),
        fluidRow(
          h4(strong('Old Standard (Single Sample Maximum = 235 CFU / 100 mL, Monthly Geomean = 126 CFU / 100 mL)')),
          column(6,
                 h5('All E. coli records that are ',span(strong('above the criteria')),' for the ',
                    span(strong('selected site')),' are highlighted below.',
                    span(strong('If no data are reflected in below tables then no data exceeded the respective criteria.'))),
                 h4(strong('Single Sample Maximum = 235 CFU / 100 mL')),
                 DT::dataTableOutput(ns('exceedancesOldStdTableSingleSiteSTV')), 
                 h4(strong('Monthly Geomean = 126 CFU / 100 mL')),
                 DT::dataTableOutput(ns('exceedancesOldStdTableSingleSitegeomean')),
                 br()),
          column(6,
                 br(),br(),br(),
                 h5('Individual E. coli exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
                 DT::dataTableOutput(ns("oldStdTableSingleSite")))),
        hr(),br(),
      h4(strong('New Recreation Standard In Depth Analysis')),
      helpText('Review the 90 day windows (identified by each sample date) for STV and geomean exceedances.
               Comments are specific to each row of data. To view the dataset within each 90 day window, use
               the drop down box to select the start of the window in question.'),
      fluidRow(
        column(6, helpText('Below is the raw data associated with the ',span('selected site'),'.'), 
               h5(strong('Raw Data')),DT::dataTableOutput(ns('rawData'))),
        column(6, uiOutput(ns('windowChoice')),
               plotlyOutput(ns('plotlyZoom')))),
      br(), br(),
      h5(strong('Analyzed Data (Each window with an individual STV and geomean assessment decisions)')),
      DT::dataTableOutput(ns('analysisTable')))
  )
}


EcoliPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove, analyzedData){
  ns <- session$ns
  
  # Doesn't work bc analyzedData could be from a different station  
  # Select One station for individual review
  #  output$oneStationSelectionUI <- renderUI({
  #    req(stationSelectedAbove)
  #    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
  #                width='300px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({
    filter(AUdata(), FDT_STA_ID %in% as.character(stationSelectedAbove())) %>% #input$oneStationSelection) %>%
      filter(!is.na(E.COLI))})
  
  # Bring in pre analyzed data to expedite process
  oneStationAnalysis <- reactive({analyzedData()})# bc not updating in full app unless this is reactive
  oneStationDecisionData <- reactive({oneStationAnalysis()[['associatedDecisionData']][[1]]}) # bc not updating in full app unless this is reactive
  
  #output$test <- renderPrint({analyzedData()})
  
  
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
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID:FDT_COMMENT, E.COLI, ECOLI_RMK)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('E.COLI','ECOLI_RMK'), 'ECOLI_RMK', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$plotly <- renderPlotly({
    req(oneStation())
    dat <- oneStation() %>%
      mutate(newSTV = 410, geomean = 126, oldSTV = 235)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
    plot_ly(data=dat) %>%
      add_markers(x= ~SampleDate, y= ~E.COLI,mode = 'scatter', name="E. coli (CFU / 100 mL)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("E. coli: ",E.COLI,"CFU / 100 mL")))%>%
      add_lines(data=dat, x=~SampleDate,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~oldSTV, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text= "Old SSM: 235 CFU / 100 mL", name="Old SSM: 235 CFU / 100 mL") %>%
      add_lines(data=dat, x=~SampleDate,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
                hoverinfo = "text", text= "Geomean: 126 CFU / 100 mL", name="Geomean: 126 CFU / 100 mL") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="E. coli (CFU / 100 mL)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
  })
  
  
  ### New standard ----------------------------------------------------------------------------------

  output$exceedancesNEWStdTableSingleSite <- DT::renderDataTable({
    req(oneStation(),!is.na(oneStationDecisionData()))
    z <- oneStationDecisionData() %>% 
      filter(`STV Exceedances In Window` > 0 | `Geomean In Window` > 126) %>%
      dplyr::select(-associatedData) %>% # remove embedded tibble to make table work
      mutate(`Date Window Starts` = as.Date(`Date Window Starts`),
             `Date Window Ends` = as.Date(`Date Window Ends`))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t')) 
  })
  
  
  output$newStdTableSingleSite <- DT::renderDataTable({
    req(oneStation(),oneStationAnalysis())
    z <- oneStationAnalysis() %>% 
      dplyr::select(ECOLI_EXC:ECOLI_GM_SAMP, 'Verbose Assessment Decision' = ECOLI_STATECOLI_VERBOSE) #only grab decision
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'))  })
  
  
  #### Old Standard ---------------------------------------------------------------------------------
  output$exceedancesOldStdTableSingleSiteSTV <- DT::renderDataTable({req(oneStation())
    z <- bacteria_ExceedancesSTV_OLD(oneStation() %>%
                                       dplyr::select(FDT_DATE_TIME, E.COLI)%>% # Just get relevant columns, 
                                       filter(!is.na(E.COLI)) #get rid of NA's
                                     , 235 ) %>%
      filter(exceeds == T) %>%
      mutate(FDT_DATE_TIME = as.Date(FDT_DATE_TIME), E.COLI = parameter) %>%
      dplyr::select(FDT_DATE_TIME, E.COLI, limit, exceeds)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "150px", dom='ti')) })
  
  output$exceedancesOldStdTableSingleSitegeomean <- DT::renderDataTable({    req(oneStation())
    z <- bacteria_ExceedancesGeomeanOLD(oneStation() %>% 
                                          dplyr::select(FDT_DATE_TIME,E.COLI) %>% # Just get relavent columns, 
                                          filter(!is.na(E.COLI)), #get rid of NA's
                                        'E.COLI', 126) 
    if(!is.null(z)){
      z <- z %>%
        dplyr::select(FDT_DATE_TIME, E.COLI, sampleMonthYear, geoMeanCalendarMonth, limit, samplesPerMonth) %>%
        filter(samplesPerMonth > 4, geoMeanCalendarMonth > limit) # minimum sampling rule for geomean to apply
    } else {z <- tibble(FDT_DATE_TIME = NA, `E.COLI` = NA, sampleMonthYear= NA, geoMeanCalendarMonth= NA, limit= NA, samplesPerMonth= NA)}
      
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "100px", dom='t')) })
  
  output$oldStdTableSingleSite <- DT::renderDataTable({req(oneStation())
    #get rid of citizen data
    z1 <- filter(oneStation(), !(ECOLI_RMK %in% c('Level II', 'Level I')))
    if(nrow(z1) > 1){
      z <- bacteria_Assessment_OLD(z1,  'E.COLI', 126, 235)
      if(nrow(z) > 0 ){
        z <- dplyr::select(z, `Assessment Method`,everything()) }
      DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'))
    }
  })
  
  
  ### Raw Data and Individual window analysis
  
  output$rawData <- DT::renderDataTable({
    req(oneStation())
    z <- dplyr::select(oneStation(), FDT_STA_ID, FDT_DATE_TIME, E.COLI, RMK_ECOLI) %>% 
      mutate(FDT_DATE_TIME = as.Date(FDT_DATE_TIME, format = '%Y-%m-%D %H:%M:S'))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='ti'))  })
  
  output$windowChoice <- renderUI({
    req(oneStationAnalysis(), !is.na(oneStationDecisionData()))
    fluidRow(
      column(4, selectInput(ns('windowChoice_'),'Select 90 day window start date',
                            choices = unique(oneStationDecisionData()$`Date Window Starts`), width = '100%')),
      column(8, helpText('Orange line corresponds to the window geomean; wide black dashed line
                         corresponds to the geomean criteria; thin black dashed line corresponds
                         to the STV limit.')))})
  
  output$plotlyZoom <- renderPlotly({
    req(input$windowChoice_, oneStation(), !is.na(oneStationDecisionData()))
  
    windowData <- filter(oneStationDecisionData(), as.character(`Date Window Starts`) %in% input$windowChoice_) %>%
      dplyr::select( associatedData) %>%
      unnest(cols = c(associatedData)) %>%
      mutate(newSTV = 410, geomean = 126,
             `Date Time` = as.POSIXct(strptime(FDT_DATE_TIME, format="%Y-%m-%d")))#as.POSIXct(windowData$`Date Time`, format="%Y-%m-%d", tz='GMT') + as.difftime(1, units="days")
    
    plot_ly(data=windowData) %>%
      add_markers(x= ~`Date Time`, y= ~Value,mode = 'scatter', name="E. coli (CFU / 100 mL)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",`Date Time`),
                                               paste("E. coli: ",Value,"CFU / 100 mL"))) %>%
      add_lines(data=windowData, x=~`Date Time`, y=~geomean, mode='line', line = list(color = 'orange', dash= 'dash'),
                hoverinfo = "text", text= ~paste("Window Geomean: ", format(geomean,digits=3)," CFU / 100 mL", sep=''), 
                name="Window Geomean") %>%
      add_lines(data=windowData, x=~`Date Time`,y=~newSTV, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= "New STV: 410 CFU / 100 mL", name="New STV: 410 CFU / 100 mL") %>%
      add_lines(data=windowData, x=~`Date Time`,y=~geomean, mode='line', line = list(color = 'black', dash= 'dash'),
                hoverinfo = "text", text= "Geomean: 126 CFU / 100 mL", name="Geomean: 126 CFU / 100 mL") %>%
      layout(showlegend=FALSE,
             yaxis=list(title="E. coli (CFU / 100 mL)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
  })
   
  output$analysisTable <- DT::renderDataTable({
    req(!is.na(oneStationDecisionData()))
    z <- oneStationDecisionData() %>%
      dplyr::select(-associatedData) # remove embedded tibble to make table work
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='t'))
  })
}


ui <- fluidPage(
  helpText('Review each site using the single site visualization section. There are no WQS for Total Nitrogen.'),
  uiOutput('AUselection_'),
  h5(strong('AU information from last cycle')),
  DT::dataTableOutput('selectedAU'),br(),
  uiOutput('stationSelection_'),
  EcoliPlotlySingleStationUI('Ecoli')
)

server <- function(input,output,session){
  # for testing
  stationTable <- reactive({
    stationTable <-  read_csv('userDataToUpload/processedStationData/stationTableResults.csv')
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
  }) #for testing
  
  # Pull AU data from server
  # for testing
  # Pull AU data from server
  regionalAUs <- reactive({ 
    req(input$pullAUs)
    withProgress(message = 'Reading in Large Spatial File',
                 regionalAUsForTesting ) }) # for testing
  
  conventionals_HUC <- reactive({#eventReactive( input$pullHUCdata, {
    filter(conventionals, Huc6_Vahu6 %in% 'JM01') %>%
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
  
  stationSelected <- reactive({input$stationSelection})
  ecoli <- reactive({req(stationData())
    bacteriaAssessmentDecision(stationData(), 'E.COLI', 'ECOLI_RMK', 10, 410, 126)})
  
  callModule(EcoliPlotlySingleStation,'Ecoli', AUData, stationSelected, ecoli)#siteData$ecoli)
  
}

shinyApp(ui,server)








