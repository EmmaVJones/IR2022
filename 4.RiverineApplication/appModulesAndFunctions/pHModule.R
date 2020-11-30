

pHPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(2,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               column(2,br(),checkboxInput(ns('displayBSAcolors'), 'Display Benthic Stressor Analysis Colors on Plot', value = TRUE)),
               column(1),
               column(2,uiOutput(ns('changeWQSUI'))),
               column(1),
               column(2,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application.'),
      plotlyOutput(ns('plotly')),
      br(),hr(),br(),
      fluidRow(
        column(8, h5('All pH records that are outside the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns('rangeTableSingleSite'))),
        column(4, h5('Individual pH exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns("stationExceedanceRate"))))
    )
  )
}

pHPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
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
      filter(!is.na(FDT_FIELD_PH)) %>%
      # special step for pH to make the CLASS_BASIN update if pH special standards exist
      mutate(CLASS_DESCRIPTION = case_when(str_detect(as.character(SPSTDS), '6.5-9.5') ~ 'SPSTDS = 6.5-9.5',
                                           TRUE ~ CLASS_DESCRIPTION))})
  
  
  # Option to change WQS used for modal
  output$changeWQSUI <- renderUI({
    req(oneStation_original())
    selectInput(ns('changeWQS'),strong('WQS For Analysis'),
                choices= c(WQSvalues$CLASS_DESCRIPTION, 'SPSTDS = 6.5-9.5'), # special just for pH
                width='400px', selected = unique(oneStation_original()$CLASS_DESCRIPTION)) })
  
  # change WQS for rest of module if user chooses to do so
  oneStation <- reactive({req(oneStation_original(), input$changeWQS)
    changeWQSfunction(oneStation_original(), input$changeWQS) })
  
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
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID:FDT_COMMENT, FDT_FIELD_PH, FDT_FIELD_PH_RMK)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('FDT_FIELD_PH','FDT_FIELD_PH_RMK'), 'FDT_FIELD_PH_RMK', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation())
    dat <- mutate(oneStation(),top = `pH Max`, bottom = `pH Min`)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
    
    # Fix look of single measure
    if(nrow(dat) == 1){
      print('yes')
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5))))
    }
    
    if(input$displayBSAcolors == TRUE){
      box1 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(9, 14, 14, 9))
      box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(6, 9, 9, 6))
      box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 6, 6, 0))
      
      plot_ly(data=dat)%>%
        add_polygons(data = box1, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
        
        add_lines(data=dat, x=~SampleDate,y=~top, mode='line',line = list(color = 'black'),
                  hoverinfo = "text",text="pH Standard", name="pH Standard") %>%
        add_lines(data=dat, x=~SampleDate,y=~bottom, mode='line',line = list(color = 'black'),
                  hoverinfo = "text", text="pH Standard", name="pH Standard") %>%
        add_markers(data=dat, x= ~SampleDate, y= ~FDT_FIELD_PH,mode = 'scatter', name="pH (unitless)",  marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("pH: ",FDT_FIELD_PH," (unitless)")))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="pH (unitless)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    } else {
      plot_ly(data=dat)%>%
        add_lines(data=dat, x=~SampleDate,y=~top, mode='line',line = list(color = 'black'),
                  hoverinfo = "text",text="pH Standard", name="pH Standard") %>%
        add_lines(data=dat, x=~SampleDate,y=~bottom, mode='line',line = list(color = 'black'),
                  hoverinfo = "text", text="pH Standard", name="pH Standard") %>%
        add_markers(data=dat, x= ~SampleDate, y= ~FDT_FIELD_PH,mode = 'scatter', name="pH (unitless)",  marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("pH: ",FDT_FIELD_PH," (unitless)")))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="pH (unitless)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))    }  })
  
  output$rangeTableSingleSite <- renderDataTable({
    req(oneStation())
    z <- pHExceedances(oneStation()) %>%
      filter(exceeds == TRUE) %>%
      rename('Outside WQS Criteria' = 'exceeds', 'Parameter Rounded to WQS Format' = 'parameterRound') %>%
      dplyr::select(-c(FDT_DEPTH, limit, interval))
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "300px", dom='t'))})
  
  
  output$stationExceedanceRate <- renderDataTable({
    req(ns(input$oneStationSelection), oneStation())
    z <- pHExceedances(oneStation()) %>% quickStats('PH') %>% dplyr::select(-PH_STAT)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t')) })
  
}