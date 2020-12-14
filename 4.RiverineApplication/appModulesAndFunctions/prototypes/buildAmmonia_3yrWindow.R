source('appTestingData.R')



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
                      #checkboxInput(ns('mussels'), 'Mussels Present', value = TRUE),
                      #checkboxInput(ns('earlyLife'), 'Early Life Stages of Fish Present', value = TRUE)),
               column(2),
               column(2,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
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
      h4("Acute Ammonia Criteria Analysis Results"),
      fluidRow(
        column(8, h5('All ammonia records that are above the acute criteria for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', dataTableOutput(ns('rangeTableSingleSite')))),
        column(4, h5('Individual ammonia exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns("stationAcuteExceedanceRate")))),
      br(),hr(),br(),
      h4("Chronic Ammonia Criteria Analysis Results"),
      fluidRow(
        column(8, h5('All ammonia records that are above the chronic criteria for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', dataTableOutput(ns('rangeChronicTableSingleSite')))),
        column(4, h5('Individual ammonia exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns("stationChronicExceedanceRate")))),
      h5(strong('Chronic Ammonia Criteria In Depth Analysis')),
      helpText('Review the 30 day windows (identified by each sample date) for chronic criteria exceedances.
               To view the dataset within each 90 day window, use the drop down box to select the start of the window in question.'),
      verbatimTextOutput(ns('test')),
      
      fluidRow(
        column(6, helpText('Below is the data averaged over each 30 day window associated with the ',span('selected site'),'.'), 
               h5(strong('30 day Window Averaged Data')),DT::dataTableOutput(ns('avg30dayData'))),
        column(6, #uiOutput(ns('windowChoice_')),
               plotlyOutput(ns('chronicPlotlyZoom'))))
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
    list(
      disabled(checkboxInput(ns('trout'), 'Trout Present', value = defaultTrout)),
      disabled(checkboxInput(ns('mussels'), 'Mussels Present', value = TRUE)),
      disabled(checkboxInput(ns('earlyLife'), 'Early Life Stages of Fish Present', value = TRUE)))})

  
  oneStationAnalysis <- reactive({req(nrow(oneStation()) > 0)
    # extract pre run ammonia analysis
    dat <- filter(ammoniaAnalysis, StationID %in% unique(oneStation()$FDT_STA_ID)) %>%
      map_df(1) 
    dat$AmmoniaAnalysis })
   
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
                    options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
        formatStyle(c('AMMONIA','RMK_AMMONIA'), 'RMK_AMMONIA', 
                    backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
    })
  
    
                            
  output$plotly <- renderPlotly({
    req(oneStationAnalysis())
    if(nrow(oneStationAnalysis()) > 0){
      dat <- mutate(oneStationAnalysis(), over = ifelse(acuteExceedance == TRUE, '#D11814', '#535559'))# 'VIOLATION', 'GOOD'))
      dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
      #dat <- acuteNH3limit(oneStation()) %>%
      #  mutate(over=ifelse(AMMONIA > NH3limit, '#D11814', '#535559'))# 'VIOLATION', 'GOOD'))
      #dat$SampleDate <- as.POSIXct(as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%Y %H:%M"), format="%m/%d/%y")
      
      #last3years <- mutate(dat, sampleYear = lubridate::year(SampleDate)) %>%
      #  filter(sampleYear %in% lastXyears(dat, 'SampleDate', 3, TRUE))
      #box1 <- data.frame(SampleDate = c(min(last3years$FDT_DATE_TIME2), min(last3years$FDT_DATE_TIME2),
      #                                  max(last3years$FDT_DATE_TIME2),max(last3years$FDT_DATE_TIME2)), 
      #                   y = c(min(dat$AMMONIA), max(dat$AMMONIA), max(dat$AMMONIA), min(dat$AMMONIA)))
      
      
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
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t')) })
  
  output$stationAcuteExceedanceRate <- renderDataTable({
    req(nrow(oneStation())> 0)
    z <- freshwaterNH3Assessment(oneStationAnalysis(), 'acute')[[1]]
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "100px", dom='t')) })
  
  ## Chronic results
  output$rangeChronicTableSingleSite <- renderDataTable({
    req(nrow(oneStation()) > 0)
    z <- filter(oneStationAnalysis(), chronicExceedance == TRUE) %>%
      dplyr::select(FDT_DATE_TIME,  '30 Day Averaged Ammonia Rounded to WQS Format' = `30dayAmmoniaAvg`, 
                    '30 Day Averaged Temperature' = TempAvg, 
                    '30 Day Averaged pH' = pHAvg, chronicNH3limit)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t')) })
  
  output$stationChronicExceedanceRate <- renderDataTable({
    req(nrow(oneStation())> 0)
    z <- freshwaterNH3Assessment(oneStationAnalysis(), 'chronic')[[1]]
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "100px", dom='t')) })
  

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
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "400px", dom='ti'),
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
  
  
  output$test <- renderPrint({ req(oneStationAnalysis())
    windowData()  })
  
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
  
  
  
}




ui <- fluidPage(
  useShinyjs(),
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  AmmoniaPlotlySingleStationUI('Ammonia'))

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({#filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) ) })
    filter(conventionals, FDT_STA_ID == '2-XDD000.40') %>%
      left_join(dplyr::select(stationTable, STATION_ID:VAHU6,
                              WQS_ID:EPA_ECO_US_L3NAME),
                #WQS_ID:`Max Temperature (C)`), 
                by = c('FDT_STA_ID' = 'STATION_ID')) %>%
      filter(!is.na(ID305B_1)) %>%
      pHSpecialStandardsCorrection() })
  
  
  
  callModule(AmmoniaPlotlySingleStation,'Ammonia', AUData, stationSelected, ammoniaAnalysis)
  
}

shinyApp(ui,server)




output$windowChoice_ <- renderUI({
  req(chronicData())
  fluidRow(
    column(4, selectInput(ns('windowChoice'),'Select 30 day window start date',
                          choices = unique(as.Date(chronicData()$FDT_DATE_TIME)), width = '100%')),
    column(8, helpText('Orange line corresponds to the window chronic criteria; wide black dashed line
                         corresponds to the geomean criteria; thin black dashed line corresponds
                         to the STV limit.')))})


