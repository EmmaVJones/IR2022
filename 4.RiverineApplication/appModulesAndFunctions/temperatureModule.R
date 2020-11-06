temperaturePlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6,uiOutput(ns('temperature_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      plotlyOutput(ns('Tempplotly')),
      br(),hr(),br(),
      fluidRow(
        column(8, h5('All temperature records that are above the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('TempRangeTableSingleSite')))),
        column(4, h5('Individual temperature exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               tableOutput(ns("stationTempExceedanceRate")))
      )
    )
  )
}

temperaturePlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$temperature_oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('temperature_oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='300px', selected = stationSelectedAbove())})
  
  temperature_oneStation <- reactive({
    req(ns(input$temperature_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$temperature_oneStationSelection) %>%
      filter(!is.na(FDT_TEMP_CELCIUS))})
  
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
    req(temperature_oneStation())
    parameterFilter <- dplyr::select(temperature_oneStation(), FDT_STA_ID:FDT_COMMENT, FDT_TEMP_CELCIUS, FDT_TEMP_CELCIUS_RMK)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c( 'FDT_TEMP_CELCIUS', 'FDT_TEMP_CELCIUS_RMK'), 'FDT_TEMP_CELCIUS_RMK', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  
  output$Tempplotly <- renderPlotly({
    req(input$temperature_oneStationSelection, temperature_oneStation())
    dat <- mutate(temperature_oneStation(),top = `Max Temperature (C)`)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
    plot_ly(data=dat)%>%
      add_lines(x=~SampleDate,y=~top, mode='line',line = list(color = 'black'),
                hoverinfo = "text", text="Temperature Standard", name="Temperature Standard") %>%
      add_markers(x= ~SampleDate, y= ~FDT_TEMP_CELCIUS,mode = 'scatter', name="Temperature (Celsius)",marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Temperature: ",FDT_TEMP_CELCIUS,"C")))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Temperature (Celsius)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))
  })
  
  output$TempRangeTableSingleSite <- renderTable({
    req(temperature_oneStation())
    temp_Assessment(temperature_oneStation())})
  
  # Temperature Station Exceedance Rate
  output$stationTempExceedanceRate <- renderTable({
    req(ns(input$temperature_oneStationSelection), temperature_oneStation())
    exceedance_temp(temperature_oneStation()) %>%
      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
  #req(input$stationTempExceedanceRateSelect)
  #z <- filter(AUdata(),FDT_STA_ID %in% input$stationTempExceedanceRateSelect)
  #exceedance_temp(z) %>%
  #  dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  
}

temperatureExceedanceAnalysisUI <- function(id){
  ns <- NS(id)
  tagList(
    #fluidRow(
    #column(6,
    h5('All temperature records that exceed the threshold for the',span(strong('assessment unit')),' are highlighted below. 
                If no records are presented in the table below, then no data exceedes the temperature threshold.'),
    tableOutput(ns('tempRangeTable'))#),
    # column(6,
    #wellPanel(
    #h5('Station Exceedance Rate:'),
    #uiOutput(ns('stationTempExceedanceRateSelect_UI')),
    #tableOutput(ns("stationTempExceedanceRate")))))#,
    #hr(),
    #h5('Assessment Unit Exceedance Rate:'),
    #tableOutput(ns("tempExceedanceRate"))))
    
  )
}

temperatureExceedanceAnalysis <- function(input, output, session, AUdata){
  ns <- session$ns
  
  # Temperature Raw Exceedance Results (all AU)
  output$tempRangeTable <- renderTable({
    req(AUdata)
    temp_Assessment(AUdata())})
  
  #  # Temperature Station Exceedance Rate
  #  output$stationTempExceedanceRateSelect_UI <- renderUI({
  #   req(AUdata)
  #  selectInput(ns('stationTempExceedanceRateSelect'),strong('Select Station to Review for individual temperature exceedance statistics'),
  #              choices=unique(AUdata())$FDT_STA_ID,width='300px')})
  
  # output$stationTempExceedanceRate <- renderTable({
  #    req(input$stationTempExceedanceRateSelect)
  #    z <- filter(AUdata(),FDT_STA_ID %in% input$stationTempExceedanceRateSelect)
  #    exceedance_temp(z) %>%
  #      dplyr::select(nSamples,nExceedance,exceedanceRate)}) # don't give assessment determination for single station})
  #  
  # Temperature AU Exceedance Rate
  #output$tempExceedanceRate <- renderTable({
  #  req(AUdata)
  #  exceedance_temp(AUdata())})
  
}
