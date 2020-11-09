source('appTestingData.R')


temperaturePlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(4,uiOutput(ns('temperature_oneStationSelectionUI'))),
               column(4,uiOutput(ns('temperature_changeWQSUI'))),
               column(4,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      plotlyOutput(ns('Tempplotly')),
      br(),hr(),br(),
      fluidRow(
        column(8, h5('All temperature records that are above the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns('TempRangeTableSingleSite'))),
        column(4, h5('Individual temperature exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns("stationTempExceedanceRate")))
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
                width='200px', selected = stationSelectedAbove())})
  
  temperature_oneStation_original <- reactive({
    req(ns(input$temperature_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$temperature_oneStationSelection) %>%
      filter(!is.na(FDT_TEMP_CELCIUS))})
  
  # Option to change WQS used for modal
  output$temperature_changeWQSUI <- renderUI({
    req(temperature_oneStation_original())
    selectInput(ns('temperature_changeWQS'),strong('WQS For Analysis'),
                choices= WQSvalues$CLASS_DESCRIPTION,
                width='400px', selected = unique(temperature_oneStation_original()$CLASS_DESCRIPTION)) })
  
  temperature_oneStation <- reactive({
    req(temperature_oneStation_original(), input$temperature_changeWQS)
    if(input$temperature_changeWQS != unique(temperature_oneStation_original()$CLASS_DESCRIPTION)){
      changedWQS <- filter(WQSvalues, CLASS_DESCRIPTION %in% input$temperature_changeWQS)
      return(
        dplyr::select(temperature_oneStation_original(), -c(`Description Of Waters`:CLASS_DESCRIPTION)) %>%
          mutate(CLASS = changedWQS$CLASS, 
                 `Description Of Waters` = changedWQS$`Description Of Waters` ) %>%
          left_join(changedWQS, by = c('CLASS', 'Description Of Waters')) )
      } else { return(temperature_oneStation_original())} })
    
  
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
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))  })
  
  
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
  
  output$TempRangeTableSingleSite <- renderDataTable({
    req(temperature_oneStation())
    z <- tempExceedances(temperature_oneStation()) %>%
      rename("FDT_TEMP" = 'parameter', 'Criteria' = 'limit', 'Parameter Rounded to WQS Format' = 'parameterRound') %>%
      filter(exceeds == TRUE) %>%
      dplyr::select(-exceeds)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "300px", dom='t'))})
  
  # Temperature Station Exceedance Rate
  output$stationTempExceedanceRate <- renderDataTable({
    req(ns(input$temperature_oneStationSelection), temperature_oneStation())
    z <- tempExceedances(temperature_oneStation()) %>% quickStats('TEMP') %>% dplyr::select(-TEMP_STAT)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t')) })
}



ui <- fluidPage(
  helpText('Review each site using the single site visualization section, then 
           proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
           span(strong('NOTE: The temperature exceedance analysis results at the bottom of the page include data
                       from ALL stations within the assessment unit.'))),
  temperaturePlotlySingleStationUI('temperature'))
  
server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) ) })
    #filter(conventionals_HUC, FDT_STA_ID %in% c('4APKP-4-DRBA', '2-JKS018.68'))}) 
  #AUData <- reactive({filter(conventionals_HUC, ID305B_1 %in% 'VAW-H01R_JMS04A00' | 
  #                             ID305B_2 %in% 'VAW-H01R_JMS04A00' | 
  #                             ID305B_2 %in% 'VAW-H01R_JMS04A00')%>% 
  #    left_join(WQSvalues, by = 'CLASS')})
  
  callModule(temperaturePlotlySingleStation,'temperature', AUData, stationSelected)
}

shinyApp(ui,server)




