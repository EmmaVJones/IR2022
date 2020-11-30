
DOPlotlySingleStationUI <- function(id){
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
        column(8, h5('All DO records that are outside the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               h6('All Dissolved Oxygen Measures'),dataTableOutput(ns("minTableSingleSite")), br(), hr(), br(),
               h6('All Daily Average Dissolved Oxygen Measures'),dataTableOutput(ns("dailyAverageTableSingleSite"))),
        column(4, h5('Individual DO exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               h6('All Dissolved Oxygen Measures'), dataTableOutput(ns("stationExceedanceRate")),
               h6('All Daily Average Dissolved Oxygen Measures'),dataTableOutput(ns("stationDailyAverageExceedanceRate"))))
    )
  )
}

DOPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
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
      filter(!is.na(DO))})
  
  # Option to change WQS used for modal
  output$changeWQSUI <- renderUI({
    req(oneStation_original())
    selectInput(ns('changeWQS'),strong('WQS For Analysis'),
                choices= WQSvalues$CLASS_DESCRIPTION,
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
  output$parameterData <- DT::renderDataTable({  req(oneStation())
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID:FDT_COMMENT, DO, DO_RMK)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('DO','DO_RMK'), 'DO_RMK',backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray')) })
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation())
    dat <- mutate(oneStation(), bottom = `Dissolved Oxygen Min (mg/L)`)
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
    
    # Fix look of single measure
    if(nrow(dat) == 1){
      print('yes')
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5))))
    }
    
    maxheight <- ifelse(max(dat$DO, na.rm=T) < 10, 12, max(dat$DO, na.rm=T)* 1.2)
    
    if(input$displayBSAcolors == TRUE){
      box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(10, maxheight, maxheight, 10))
      box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(8, 10, 10, 8))
      box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(7, 8, 8, 7))
      box4 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 7, 7, 0))
      
      plot_ly(data=box1)%>%
        add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
        add_lines(data=dat, x=~SampleDate,y=~bottom, mode='line', line = list(color = 'black'),
                  hoverinfo = "text", text="DO Standard", name="DO Standard") %>%
        add_markers(data=dat, x= ~SampleDate, y= ~DO,mode = 'scatter', name="DO (mg/L)", marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("DO: ",DO," (mg/L)")))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="DO (unitless)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    } else {
      plot_ly(data=dat)%>%
        add_lines(data=dat, x=~SampleDate,y=~bottom, mode='line', line = list(color = 'black'),
                  hoverinfo = "text", text="DO Standard", name="DO Standard") %>%
        add_markers(data=dat, x= ~SampleDate, y= ~DO,mode = 'scatter', name="DO (mg/L)", marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("DO: ",DO," (mg/L)")))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="DO (unitless)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))    }  })
  
  
  output$minTableSingleSite <- renderDataTable({req(oneStation())
    z <- DOExceedances_Min(oneStation()) %>%
      rename("DO" = 'parameter', 'Criteria' = 'limit', 'Parameter Rounded to WQS Format' = 'parameterRound') %>%
      filter(exceeds == TRUE) %>%
      dplyr::select(-exceeds)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'))})
  
  
  output$dailyAverageTableSingleSite <- renderDataTable({ req(oneStation())
    z <- DO_Assessment_DailyAvg(oneStation()) %>%
      dplyr::select('Date' = date, `DO Daily Average (Rounded to WQS Format)` = DO_DailyAverage, 
                    `n Daily Samples` = n_Samples_Daily, 'Criteria' = limit )
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'))})
  
  output$stationExceedanceRate <- renderDataTable({req(input$oneStationSelection, oneStation())
    z <- DOExceedances_Min(oneStation()) %>% quickStats('DO') %>% dplyr::select(-DO_STAT)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t')) })
  
  output$stationDailyAverageExceedanceRate <- renderDataTable({
    req(input$oneStationSelection, oneStation())
    z <- DO_Assessment_DailyAvg(oneStation()) %>% quickStats('DO_Daily_Avg') %>% dplyr::select(-DO_Daily_Avg_STAT)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t')) })
  
}


