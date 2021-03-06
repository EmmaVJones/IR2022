#source('appTestingData.R')

TPPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(2,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               column(2,br(),checkboxInput(ns('displayBSAcolors'), 'Display Benthic Stressor Analysis Colors on Plot', value = TRUE)),
               column(1),
               column(2,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application.'),
      plotlyOutput(ns('plotly')) ,
      br(), br(), br()
      
      
      # TP 0.2 limit no longer used after 12/21/2020 email with Tish/Amanda
      #h5('Total Phosphorus exceedances above 0.2 mg/L for the ',span(strong('selected site')),' are highlighted below to highlight
      #   potenial observed effects for Aquatic Life Use.'),
      #fluidRow(
      #  column(6, h6('If there is no data listed below then none of the measures exceeded 0.2 mg/L Total Phosphorus.'), 
      #         dataTableOutput(ns('rangeTableSingleSite'))),
      #  column(6, h6('Total Phosphorus > 0.2 mg/L exceedance rate'),
      #         dataTableOutput(ns("stationExceedanceRate"))))
  ) )
}


TPPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({
    req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(PHOSPHORUS_mg_L))})
  
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
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID:FDT_COMMENT, PHOSPHORUS_mg_L, RMK_PHOSPHORUS, LEVEL_PHOSPHORUS)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t'),
                  selection = 'none') %>%
      formatStyle(c('PHOSPHORUS_mg_L','RMK_PHOSPHORUS', 'LEVEL_PHOSPHORUS'), 'LEVEL_PHOSPHORUS', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation())
    dat <- oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
    
    # Fix look of single measure
    if(nrow(dat) == 1){
      print('yes')
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5))))
    }
    
    maxheight <- ifelse(max(dat$PHOSPHORUS_mg_L, na.rm=T) < 0.1, 0.12, max(dat$PHOSPHORUS_mg_L, na.rm=T)* 1.2)
    
    if(input$displayBSAcolors == TRUE){
      box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0.1, maxheight, maxheight, 0.1))
      box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0.05, 0.1, 0.1, 0.05))
      box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0.02, 0.05, 0.05, 0.02))
      box4 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 0.02, 0.02, 0))
      
      plot_ly(data=dat)%>%
        add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
        add_markers(data=dat, x= ~SampleDate, y= ~PHOSPHORUS_mg_L,mode = 'scatter', name="Total Phosphorus (mg/L)",marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Total Phosphorus: ",PHOSPHORUS_mg_L,"mg/L")))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="Total Phosphorus (mg/L)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    } else {
      plot_ly(data=dat)%>%
        add_markers(data=dat, x= ~SampleDate, y= ~PHOSPHORUS_mg_L,mode = 'scatter', name="Total Phosphorus (mg/L)",marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Total Phosphorus: ",PHOSPHORUS_mg_L,"mg/L")))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="Total Phosphorus (mg/L)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    }
    
  })
  
  # TP 0.2 limit no longer used after 12/21/2020 email with Tish/Amanda
  #output$rangeTableSingleSite <- renderDataTable({
  #  req(oneStation())
  #  z <- countNutrients(oneStation(), PHOSPHORUS, RMK_PHOSPHORUS, 0.2) %>%
  #    filter(exceeds == TRUE) %>%
  #    dplyr::select(FDT_DATE_TIME, PHOSPHORUS = parameter, RMK_PHOSPHORUS, LIMIT = limit)
  #  datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
  #            selection = 'none')})
  
  
  #output$stationExceedanceRate <- renderDataTable({
  #  req(ns(input$oneStationSelection), oneStation())
  #  z <- countNutrients(oneStation(), PHOSPHORUS, RMK_PHOSPHORUS, 0.2) %>% quickStats('NUT_TP') %>%
  #    dplyr::select(-NUT_TP_STAT)
  #  datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
  #            selection = 'none') })
  
}



ui <- fluidPage(
  helpText('Review each site using the single site visualization section, then 
           proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
           span(strong('NOTE: The pH exceedance analysis results at the bottom of the page include data
                       from ALL stations within the assessment unit.'))),
  TPPlotlySingleStationUI('TP')     )

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) ) })
  callModule(TPPlotlySingleStation,'TP', AUData, stationSelected)
  
}

shinyApp(ui,server)

