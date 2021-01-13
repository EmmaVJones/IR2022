
temperaturePlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(4,uiOutput(ns('oneStationSelectionUI'))),
               column(4,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application.'),
      plotlyOutput(ns('plotly')),
      br(),hr(),br(),
      fluidRow(
        column(7, h5('All temperature records that are above the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns('rangeTableSingleSite'))),
        column(1),
        column(4, h5('Individual temperature exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns("stationExceedanceRate")))),
      br(),
      wellPanel(
        h4(strong('AU Assessment')),
        helpText("The 2022 IR Guidance states: 'In most cases, a single monitoring station should represent a lake/reservoir assessment unit. 
                 In cases where there are multiple stations in an assessment unit and it is determined that the water in that unit is 
                 homogenous and not influenced by tributary contribution, the data may be pooled to determine temperature exceedance rates 
                 in that AU.'"),
        helpText(strong('This application provides an AU level assessment, but it is up to the assessor to determine if pooling should be used.')),
        fluidRow(
          column(7, h5('All temperature records that are above the criteria for the ',span(strong('Assessment Unit')),' are highlighted below.'),
                 dataTableOutput(ns('rangeTableAU'))),
          column(1),
          column(4, h5('Temperature exceedance statistics for the ',span(strong('AU')),' are highlighted below.'),
                 dataTableOutput(ns("AUExceedanceRate")))),
        
      )
    )
  )
}

temperaturePlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$oneStationSelectionUI <- renderUI({req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
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
    req(oneStation())
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID:FDT_COMMENT, FDT_TEMP_CELCIUS, RMK_FDT_TEMP_CELCIUS, LEVEL_FDT_TEMP_CELCIUS)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t'),
                  selection = 'none') %>%
      formatStyle(c( 'FDT_TEMP_CELCIUS', 'RMK_FDT_TEMP_CELCIUS', 'LEVEL_FDT_TEMP_CELCIUS'), 'LEVEL_FDT_TEMP_CELCIUS', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))  })
  
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation())
    dat <- mutate(oneStation(),top = `Max Temperature (C)`,
                  LakeStratification = replace_na(LakeStratification,"NA")) %>%
      mutate(LakeStratification = factor(LakeStratification,levels=c("Epilimnion",'NA',"Hypolimnion")))#,ordered=T)
    
    suppressWarnings(suppressMessages(
      plot_ly(data=dat)%>%
        add_lines(x=~SampleDate,y=~top, mode='line',line = list(color = 'black'),
                  hoverinfo = "text", text="Temperature Standard", name="Temperature Standard") %>%
        add_markers(x= ~SampleDate, y= ~FDT_TEMP_CELCIUS,mode = 'scatter', name="Temperature (Celsius)",
                    color=~LakeStratification,# colors=c('#BF382A', '#0C4B8E'),#marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Temperature: ",FDT_TEMP_CELCIUS,"C"),
                                                 paste("LakeStratification: ",LakeStratification)))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="Temperature (Celsius)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10))) ))
  })
  
  output$rangeTableSingleSite <- renderDataTable({    req(oneStation())
    z <- tempExceedances(oneStation()) %>%
      rename("FDT_TEMP" = 'parameter', 'Criteria' = 'limit', 'Parameter Rounded to WQS Format' = 'parameterRound') %>%
      filter(exceeds == TRUE) %>%
      dplyr::select(-exceeds)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
              selection = 'none')})
  
  # Temperature Station Exceedance Rate
  output$stationExceedanceRate <- renderDataTable({
    req(ns(input$oneStationSelection), oneStation())
    z <- tempExceedances(oneStation()) %>% quickStats('TEMP') %>% dplyr::select(-TEMP_STAT)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "60px", dom='t'),
              selection = 'none') })
  
  
  ## AU results 
  output$rangeTableAU <-  renderDataTable({    req(oneStation())
    z <- tempExceedances( filter(AUdata(), !is.na(FDT_TEMP_CELCIUS)) ) %>%
      rename("FDT_TEMP" = 'parameter', 'Criteria' = 'limit', 'Parameter Rounded to WQS Format' = 'parameterRound') %>%
      filter(exceeds == TRUE) %>%
      dplyr::select(-exceeds)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
              selection = 'none')})
  
  output$AUExceedanceRate <- renderDataTable({  req(ns(input$oneStationSelection), oneStation())
    z <- tempExceedances( filter(AUdata(), !is.na(FDT_TEMP_CELCIUS)) )  %>% quickStats('TEMP') %>% dplyr::select(-TEMP_STAT)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "60px", dom='t'),
              selection = 'none') })
  
}
