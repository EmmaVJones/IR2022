library(tidyverse)
library(shiny)
library(shinyjs)
library(sf)
library(pins)
library(EnvStats)
library(lubridate)
library(config)
library(leaflet)
library(mapview)
library(DT)
library(plotly)
library(readxl)

source('appModulesAndFunctions/automatedAssessmentFunctions.R')

# Server connection things
conn <- config::get("connectionSettings") # get configuration settings

# use API key to register board
board_register_rsconnect(key = conn$CONNECT_API_KEY,  #Sys.getenv("CONNECT_API_KEY"),
                         server = conn$CONNECT_SERVER)#Sys.getenv("CONNECT_SERVER"))


# Pull data from server
conventionals <- pin_get("conventionals2022IRdraft", board = "rsconnect") %>%
  filter(FDT_DATE_TIME >= "2015-01-01 00:00:00 UTC" )
WQSlookup <- pin_get("WQSlookup-withStandards",  board = "rsconnect")
WQMstationFull <- pin_get("WQM-Station-Full", board = "rsconnect")


stationTable <- read_csv('userDataToUpload/processedStationData/stationTableResults.csv',
                         col_types = cols(COMMENTS = col_character())) # force to character bc parsing can incorrectly guess logical based on top 1000 rows
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


conventionals_HUC <- left_join(conventionals, dplyr::select(stationTable, STATION_ID:VAHU6,
                          WQS_ID:EPA_ECO_US_L3NAME),
            #WQS_ID:`Max Temperature (C)`), 
            by = c('FDT_STA_ID' = 'STATION_ID')) %>%
  filter(!is.na(ID305B_1)) %>%
  pHSpecialStandardsCorrection()





AmmoniaPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(2,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               column(4,
                      h4('Freshwater Criteria Default Analysis Settings'),
                      helpText("The below settings are applied to the station based on the WQS Class attributed to the station. All
                               analyses presented reflect these conditions."),
                      uiOutput(ns('optionsUI_'))),
               column(2),# helpText('The default settings are specified to expedite application rendering time. If the default analysis 
                        #          settings do not meet your needs, please contact Emma Jones (emma.jones@deq.virginia.gov)
                        #          to add more interactive analysis on the fly.')),
               column(2,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      h5('Stations Table Output for Selected Station'),
      DT::dataTableOutput(ns('stationsTable')),
      hr(),
      helpText('The default design flow for calculating steady state wasteload allocations for the acute ammonia criterion for 
               freshwater is the 1Q10 (see 9VAC25-260-140 B footnote 6) unless statistically valid methods are employed that 
               demonstrate compliance with the duration and return frequency of the water quality criteria. The default design 
               flow for calculating steady state wasteload allocations for the chronic ammonia criterion for freshwater is the 
               30Q10 (see 9VAC25-260-140 B footnote 6) unless statistically valid methods are employed which demonstrate 
               compliance with the duration and return frequency of the water quality criteria.'),
      helpText(strong('This assessment application does not currently support the autocalculation of flow statistics. Regional
                      assessors are responsible for validating suggested assessment results against flow statistics.')),
      plotlyOutput(ns('plotly')),
      br(),hr(),br(),
      h4(strong("Acute Ammonia Criteria Analysis Results")),
      fluidRow(
        column(8, h5('All ammonia records that are above the ',span(strong('acute criteria')),' for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', dataTableOutput(ns('rangeTableSingleSite')))),
        column(4, h5(span(strong('Acute criteria')), ' ammonia exceedance statistics calculated across three year windows 
                     for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns("stationAcuteExceedanceRate")))),
      br(),hr(),br(),
      h4(strong("Chronic Ammonia Criteria Analysis Results")),
      fluidRow(
        column(8, h5('All ammonia records that are above the ',span(strong('chronic criteria')),' for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', dataTableOutput(ns('rangeChronicTableSingleSite')))),
        column(4, h5(span(strong('Chronic criteria')), ' ammonia exceedance statistics calculated across three year windows
                     for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns("stationChronicExceedanceRate")))),
      h5(strong('Chronic Ammonia Criteria In Depth Analysis')),
      helpText('Review the 30 day windows (identified by each sample date) for chronic criteria exceedances.
               To view the dataset within each 30 day window, use the drop down box to select the start of the window in question.'),
      fluidRow(
        column(6, helpText('Below is the data averaged over each 30 day window associated with the ',span('selected site'),'.'), 
               h5(strong('30 day Window Averaged Data')),DT::dataTableOutput(ns('avg30dayData'))),
        column(6, helpText('Click a row on the table to left to reveal a detailed interactive plot of the data
                           included in the selected 30 day window. The orange dashed line is the ammonia 
                           averaged across the 30 day window. The black dashed line is the chronic criteria
                           calculated from the averaged temperature and pH measures in the 30 day window.'),
               plotlyOutput(ns('chronicPlotlyZoom')))),
      br(),hr(),br(),
      h4(strong("Four Day Ammonia Criteria Analysis Results")),
      fluidRow(
        column(8, h5('All ammonia records that are above the ',span(strong('four day criteria')),' for the ',span(strong('selected site')),' are highlighted below. 
                    Four day ammonia criteria is calculated as 2.5 times the chronic criterion within a 30-day period. '),
               div(style = 'height:150px;overflow-y: scroll', dataTableOutput(ns('range4DayTableSingleSite')))),
        column(4, h5(span(strong('Four day criteria')), ' ammonia exceedance statistics calculated across three year windows
                     for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns("station4DayExceedanceRate")))),
      h5(strong('Four Day Ammonia Criteria In Depth Analysis')),
      helpText('Review the four day windows (identified by each sample date) for four day criteria exceedances.
               To view the dataset within each 4 day window, use the drop down box to select the start of the window in question.'),
      fluidRow(
        column(6, helpText('Below is the data averaged over each four day window associated with the ',span('selected site'),'.'), 
               h5(strong('4 day Window Averaged Data')),DT::dataTableOutput(ns('avg4DayData'))),
        column(6, helpText('Click a row on the table to left to reveal a detailed interactive plot of the data
                           included in the selected 4 day window. The orange dashed line is the ammonia 
                           averaged across the 4 day window. The black dashed line is the chronic criteria
                           calculated from the averaged temperature and pH measures in the 4 day window.'),
               plotlyOutput(ns('fourDayPlotlyZoom'))))
      
      #verbatimTextOutput(ns('test')),
      
    )
  )
}


AmmoniaPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove, ammoniaAnalysis){
  ns <- session$ns
  
  # Select One station for individual review
  output$oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({
    req(ns(input$oneStationSelection))
    filter(AUdata(), FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(AMMONIA))})
  
  output$optionsUI_ <- renderUI({req(nrow(oneStation()) > 0)
    defaultTrout <- ifelse(unique(oneStation()$CLASS) %in% c('V','VI'), TRUE, FALSE)
    list(checkboxInput(ns('trout'), 'Trout Present', value = defaultTrout),
         checkboxInput(ns('mussels'), 'Mussels Present', value = TRUE),
         checkboxInput(ns('earlyLife'), 'Early Life Stages of Fish Present', value = TRUE)) })
      #disabled(checkboxInput(ns('trout'), 'Trout Present', value = defaultTrout)),
      #disabled(checkboxInput(ns('mussels'), 'Mussels Present', value = TRUE)),
      #disabled(checkboxInput(ns('earlyLife'), 'Early Life Stages of Fish Present', value = TRUE)))})
  
  
  oneStationAnalysis <- reactive({req(nrow(oneStation()) > 0)
    freshwaterNH3limit(oneStation(), trout = input$trout, mussels = input$mussels, earlyLife = input$earlyLife)   })
    
    ## extract pre run ammonia analysis
    #dat <- filter(ammoniaAnalysis, StationID %in% unique(oneStation()$FDT_STA_ID)) %>%
    #  map_df(1) 
    #dat$AmmoniaAnalysis })
  
  
  
  output$stationsTable <- DT::renderDataTable({
    req(nrow(oneStationAnalysis()) > 0)
    oneStationDecision <- ammoniaDecision(list(acute = freshwaterNH3Assessment(oneStationAnalysis(), 'acute'),
                                               chronic = freshwaterNH3Assessment(oneStationAnalysis(), 'chronic'),
                                               fourDay = freshwaterNH3Assessment(oneStationAnalysis(), 'four-day')))
    
    DT::datatable(oneStationDecision, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(oneStationDecision), scrollX = TRUE, scrollY = "50px", dom='t'),
                  selection = 'none') %>%
      formatStyle(c('AMMONIA_EXC','AMMONIA_STAT'), 'AMMONIA_STAT', backgroundColor = styleEqual(c('Review', 'IM'), c('yellow','red')))  })

  
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
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID:FDT_COMMENT, AMMONIA, RMK_AMMONIA)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t'),
                  selection = 'none') %>%
      formatStyle(c('AMMONIA','RMK_AMMONIA'), 'RMK_AMMONIA', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  
  
  output$plotly <- renderPlotly({
    req(nrow(oneStationAnalysis()) > 0, nrow(oneStation()) > 0)
    if(nrow(oneStationAnalysis()) > 0){
      dat <- mutate(oneStationAnalysis(), over = ifelse(acuteExceedance == TRUE, '#D11814', '#535559'))# 'VIOLATION', 'GOOD'))
      dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
      if(nrow(dat) > 0){ 
        plot_ly(data=dat)%>%
          #add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "#B0B3B7",opacity=0.6, line = list(width = 0),
          #             hoverinfo="text", name =paste('Most recent three years of data in assessment window')) %>%
          
          add_markers(data=dat, x= ~SampleDate, y= ~AMMONIA,mode = 'scatter', name="Ammonia (mg/L as N)", marker = list(color= ~over),#list(color = '#D11814'),#for testing
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   #paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Ammonia: ", AMMONIA,"mg/L as N"),
                                                   paste('Acute Ammonia Limit: ',format(acuteNH3limit, digits=3), "mg/L as N"),
                                                   paste('Temperature: ', FDT_TEMP_CELCIUS, '(Celsius)'),
                                                   paste('pH: ', FDT_FIELD_PH, '(unitless)')))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title="Ammonia (mg/L as N)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
      }
    } 
  })
  
  ## Acute results
  output$rangeTableSingleSite <- renderDataTable({
    req(nrow(oneStation()) > 0)
    z <- filter(oneStationAnalysis(), acuteExceedance == TRUE) %>%
      dplyr::select(FDT_DATE_TIME:FDT_FIELD_PH, 'AMMONIA Rounded to WQS Format' = AMMONIA, acuteNH3limit)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none') })
  
  output$stationAcuteExceedanceRate <- renderDataTable({
    req(nrow(oneStation())> 0)
    z <- freshwaterNH3Assessment(oneStationAnalysis(), 'acute')[[1]]
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "100px", dom='t'),
              selection = 'none') })
  
  ## Chronic results
  output$rangeChronicTableSingleSite <- renderDataTable({
    req(nrow(oneStation()) > 0)
    z <- filter(oneStationAnalysis(), chronicExceedance == TRUE) %>%
      dplyr::select(FDT_DATE_TIME,  '30 Day Averaged Ammonia Rounded to WQS Format' = `30dayAmmoniaAvg`, 
                    '30 Day Averaged Temperature' = TempAvg, 
                    '30 Day Averaged pH' = pHAvg, chronicNH3limit)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none') })
  
  output$stationChronicExceedanceRate <- renderDataTable({
    req(nrow(oneStation())> 0)
    z <- freshwaterNH3Assessment(oneStationAnalysis(), 'chronic')[[1]]
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "100px", dom='t'),
              selection = 'none') })
  
  
  ### 30 day averaged Data and Individual window analysis
  
  chronicData <- reactive({req(nrow(oneStation()) > 0)
    dplyr::select(oneStationAnalysis(), FDT_DATE_TIME, `30dayAmmoniaAvg`:chronicNH3limit, chronicExceedance, associatedWindowData)  %>%
      filter(!is.na(chronicNH3limit)) })
  
  output$avg30dayData <- DT::renderDataTable({
    req(chronicData())
    z <- dplyr::select(chronicData(), "30 Day Window Begin Date" = FDT_DATE_TIME, 
                       '30 Day Averaged Ammonia Rounded to WQS Format' = `30dayAmmoniaAvg`, 
                       '30 Day Averaged Temperature' = TempAvg, 
                       '30 Day Averaged pH' = pHAvg, chronicNH3limit)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "300px", dom='ti'),
                  selection = 'single') })
  
  
  
  windowData <-  reactive({req(chronicData(), input$avg30dayData_rows_selected)
    chronicSelection <- chronicData()[input$avg30dayData_rows_selected, ]
    
    windowData <- dplyr::select(chronicSelection, associatedWindowData) %>%
      unnest(cols = c(associatedWindowData)) %>%
      mutate(`30dayAmmoniaAvg` = chronicSelection$`30dayAmmoniaAvg`,
             chronicNH3limit = chronicSelection$chronicNH3limit)
    windowData$`Date Time` <- as.Date(windowData$FDT_DATE_TIME, format="%m/%d/%y")
    return(windowData)
  })
  
  
  
  output$chronicPlotlyZoom <- renderPlotly({
    req(windowData())
    
    
    plot_ly(data=windowData()) %>%
      add_markers(x= ~`Date Time`, y= ~AMMONIA, mode = 'scatter', name="Ammonia (mg/L as N)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",`Date Time`),
                                               paste("Ammonia: ",AMMONIA,"mg/L as N"))) %>%
      add_lines(data=windowData(), x=~`Date Time`, y=~`30dayAmmoniaAvg`, mode='line', line = list(color = 'orange', dash= 'dash'),
                hoverinfo = "text", text= ~paste("30 day Window Ammonia Average: ", `30dayAmmoniaAvg`," mg/L as N", sep=''), 
                name="30 Day Window Ammonia Average") %>%
      add_lines(data=windowData(), x=~`Date Time`,y=~chronicNH3limit, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= ~paste("30 Day Window Chronic Criteria", chronicNH3limit," mg/L as N", sep=''), 
                name="30 Day Window Ammonia Criteria") %>% 
      layout(showlegend=FALSE,
             yaxis=list(title="Ammonia (mg/L as N)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) 
  })
  
  ## 4 day results
  output$range4DayTableSingleSite <- renderDataTable({    req(nrow(oneStation()) > 0)
    z <- filter(oneStationAnalysis(), fourDayExceedance == TRUE) %>%
      dplyr::select(FDT_DATE_TIME,  '4 Day Averaged Ammonia Rounded to WQS Format' = `30dayAmmoniaAvg`, 
                    '4 Day Ammonia Criteria' =  fourDayAvglimit)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none') })
  
  output$station4DayExceedanceRate <- renderDataTable({    req(nrow(oneStation())> 0)
    z <- freshwaterNH3Assessment(oneStationAnalysis(), 'four-day')[[1]]
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "100px", dom='t'),
              selection = 'none') })
  
  
  
  ### 4 day averaged Data and Individual window analysis
  
  fourDayData <- reactive({req(nrow(oneStation()) > 0)
    dplyr::select(oneStationAnalysis(), FDT_DATE_TIME, fourDayAmmoniaAvg, fourDayAvglimit, fourDayExceedance, fourDayWindowData)  %>%
      filter(!is.na(fourDayAmmoniaAvg)) })
  
  output$avg4DayData <- DT::renderDataTable({
    req(fourDayData())
    z <- dplyr::select(fourDayData(), 
                       "4 Day Window Begin Date" = FDT_DATE_TIME, 
                       '4 Day Averaged Ammonia Rounded to WQS Format' = fourDayAmmoniaAvg,
                       '4 Day Ammonia Criteria' = fourDayAvglimit)
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "300px", dom='ti'),
                  selection = 'single') })
  
  
  fourDayWindowData <-  reactive({req(fourDayData(), input$avg4DayData_rows_selected)
    fourDaySelection <- fourDayData()[input$avg4DayData_rows_selected, ]
    
    fourDayWindowData <- dplyr::select(fourDaySelection, fourDayWindowData) %>%
      unnest(cols = c(fourDayWindowData)) %>%
      mutate(`fourDayAmmoniaAvg` = fourDaySelection$fourDayAmmoniaAvg,
             fourDayAvgLimit = fourDaySelection$fourDayAvglimit) 
    fourDayWindowData$`Date Time` <- as.Date(fourDayWindowData$FDT_DATE_TIME, format="%m/%d/%y")
    return(fourDayWindowData) })
  
  
  output$test <- renderPrint({ req(oneStationAnalysis())
    fourDayWindowData()  })
  
  output$fourDayPlotlyZoom <- renderPlotly({
    req(fourDayWindowData())
    
    plot_ly(data=fourDayWindowData()) %>%
      add_markers(x= ~`Date Time`, y= ~AMMONIA, mode = 'scatter', name="Ammonia (mg/L as N)", marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",`Date Time`),
                                               paste("Ammonia: ",AMMONIA,"mg/L as N"))) %>%
      add_lines(data=fourDayWindowData(), x=~`Date Time`, y=~`fourDayAmmoniaAvg`, mode='line', line = list(color = 'orange', dash= 'dash'),
                hoverinfo = "text", text= ~paste("4 day Window Ammonia Average: ", `fourDayAmmoniaAvg`," mg/L as N", sep=''), 
                name="4 Day Window Ammonia Average") %>%
      add_lines(data=fourDayWindowData(), x=~`Date Time`,y=~fourDayAvgLimit, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                hoverinfo = "text", text= ~paste("4 Day Window Ammonia Criteria ", fourDayAvgLimit," mg/L as N", sep=''), 
                name="4 Day Window Ammonia Criteria") %>% 
      layout(showlegend=FALSE,
             yaxis=list(title="Ammonia (mg/L as N)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
}




ui <- fluidPage(
  useShinyjs(),
  helpText('Review each site using the single site visualization section.'),
  AmmoniaPlotlySingleStationUI('Ammonia'))

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({conventionals_HUC})

  callModule(AmmoniaPlotlySingleStation,'Ammonia', AUData, stationSelected, ammoniaAnalysis)
  
}

shinyApp(ui,server)
