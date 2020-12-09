source('appTestingData.R')



AmmoniaPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(2,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               column(4,
                      h4('Freshwater Criteria Analysis Settings'),
                      uiOutput(ns('optionsUI_'))),
                      #checkboxInput(ns('mussels'), 'Mussels Present', value = TRUE),
                      #checkboxInput(ns('earlyLife'), 'Early Life Stages of Fish Present', value = TRUE)),
               column(3),
               column(2,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('The default design flow for calculating steady state wasteload allocations for the acute ammonia criterion for 
               freshwater is the 1Q10 (see 9VAC25-260-140 B footnote 6) unless statistically valid methods are employed that 
               demonstrate compliance with the duration and return frequency of the water quality criteria. The default design 
               flow for calculating steady state wasteload allocations for the chronic ammonia criterion for freshwater is the 
               30Q10 (see 9VAC25-260-140 B footnote 6) unless statistically valid methods are employed which demonstrate 
               compliance with the duration and return frequency of the water quality criteria.'),
      helpText(strong('This assessment application does not currently support the autocalculation of flow statistics. Regional
                      assessors are responsible for validating suggested assessment results against flow statistics.')),
      verbatimTextOutput(ns('test')),
      plotlyOutput(ns('plotly')),
      br(),hr(),br(),
      fluidRow(
        column(8, h5('All ammonia records that are above the acute criteria for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', dataTableOutput(ns('rangeTableSingleSite')))),
        column(4, h5('Individual ammonia exceedance statistics for the ',span(strong('selected site')),' are highlighted below.',span(strong('Note: the ammonia
                                                                                                                                             samples from the last consecutive three years of data collected are the only samples utilized for exceedance calculations.'))),
               dataTableOutput(ns("stationExceedanceRate"))))
    )
  )
}


AmmoniaPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
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
      checkboxInput(ns('trout'), 'Trout Present', value = defaultTrout),
      checkboxInput(ns('mussels'), 'Mussels Present', value = TRUE),
      checkboxInput(ns('earlyLife'), 'Early Life Stages of Fish Present', value = TRUE))})
  
  oneStationAnalysis <- reactive({req(nrow(oneStation()) > 0)#, input$trout, input$mussels, input$earlyLife)
    freshwaterNH3limit(oneStation(), trout = input$trout, mussels = input$mussels, earlyLife = input$earlyLife)  })
   
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
  
    
    output$test <- renderPrint({ #req(oneStationAnalysis())
      #input$mussels #input$trout
      mutate(oneStationAnalysis(), over = ifelse(AMMONIA > acuteNH3limit, '#D11814', '#535559'))# 'VIOLATION', 'GOOD'))
    })
                              
  output$plotly <- renderPlotly({
    req(oneStationAnalysis())
    if(nrow(oneStationAnalysis()) > 0){
      dat <- mutate(oneStationAnalysis(), over = ifelse(AMMONIA > acuteNH3limit, '#D11814', '#535559'))# 'VIOLATION', 'GOOD'))
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
                                                   paste('pH: ', FDT_FIELD_PH, '(unitless)')))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title="Ammonia (mg/L as N)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
      }
    } 
  })
  
  
  
  output$AmmoniaRangeTableSingleSite <- renderTable({
    req(oneStation())
    if(nrow(oneStation()) > 0){
      acuteNH3limit(oneStation()) %>%
        mutate(Exceedance = ifelse(AMMONIA > NH3limit, TRUE, FALSE)) %>%
        filter(Exceedance == TRUE)  }
    })
  
  output$stationAmmoniaExceedanceRate <- renderTable({
    req(input$oneStationSelection, oneStation())
    if(nrow(oneStation()) > 0){
      acuteNH3exceedance(oneStation()) %>%
        dplyr::select(1:3) %>%# don't give assessment determination for single station
        dplyr::rename(nSamples = AcuteAmmonia_SAMP,nExceedance= AcuteAmmonia_VIO,exceedanceRate= AcuteAmmonia_exceedanceRate) # make it match everything else
      } })
      
  
  
}




ui <- fluidPage(
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  AmmoniaPlotlySingleStationUI('Ammonia'))

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) ) })
  
  
  
  callModule(AmmoniaPlotlySingleStation,'Ammonia', AUData, stationSelected)
  
}

shinyApp(ui,server)

