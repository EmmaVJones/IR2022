
pHPlotlySingleStationUI <- function(id){
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
        column(7, h5('All pH records that are outside the criteria for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns('rangeTableSingleSite'))),
        column(1),
        column(4, h5('Individual pH exceedance statistics for the ',span(strong('selected site')),' are highlighted below.'),
               dataTableOutput(ns("stationExceedanceRate")))),
      br(),
      wellPanel(
        h4(strong('AU Assessment')),
        helpText("The 2022 IR Guidance states: 'In most cases, a single monitoring station should represent a lake/reservoir
        assessment unit. In cases where there are multiple stations in an assessment unit and it is determined that the water
        in that unit is homogenous and not influenced by tributary contribution, the data may be pooled to determine pH
        exceedance rates in that AU.'"),
        fluidRow(
          column(7, h5('All pH records that are above the criteria for the ',span(strong('Assessment Unit')),' are highlighted below.'),
                 dataTableOutput(ns('rangeTableAU'))),
          column(1),
          column(4, h5('pH exceedance statistics for the ',span(strong('AU')),' are highlighted below.'),
                 dataTableOutput(ns("AUExceedanceRate"))) ) )
      
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
  
  oneStation <- reactive({    req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(FDT_FIELD_PH)) %>%
      # special step for pH to make the CLASS_BASIN update if pH special standards exist
      mutate(CLASS_DESCRIPTION = case_when(str_detect(as.character(SPSTDS), '6.5-9.5') ~ 'SPSTDS = 6.5-9.5',
                                           TRUE ~ CLASS_DESCRIPTION))})
  
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
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID:FDT_COMMENT, FDT_FIELD_PH, RMK_FDT_FIELD_PH, LEVEL_FDT_FIELD_PH, LakeStratification)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t'),
                  selection = 'none') %>%
      formatStyle(c('FDT_FIELD_PH','RMK_FDT_FIELD_PH', 'LEVEL_FDT_FIELD_PH'), 'LEVEL_FDT_FIELD_PH', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray')) })
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation())
    dat <- mutate(oneStation(),top = `pH Max`, bottom = `pH Min`,
                  LakeStratification = replace_na(LakeStratification,"NA")) %>%
      mutate(LakeStratification = factor(LakeStratification,levels=c("Epilimnion",'NA',"Hypolimnion")))#,ordered=T)
    
    # Fix look of single measure
    if(nrow(dat) == 1){
      print('yes')
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5))))    }
    
    plot_ly(data=dat)%>%
      add_lines(data=dat, x=~SampleDate,y=~top, mode='line',line = list(color = 'black'),
                hoverinfo = "text",text="pH Standard", name="pH Standard") %>%
      add_lines(data=dat, x=~SampleDate,y=~bottom, mode='line',line = list(color = 'black'),
                hoverinfo = "text", text="pH Standard", name="pH Standard") %>%
      add_markers(data=dat, x= ~SampleDate, y= ~FDT_FIELD_PH,mode = 'scatter', name="pH (unitless)",  
                  color=~LakeStratification, #marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("pH: ",FDT_FIELD_PH," (unitless)"),
                                               paste("LakeStratification: ",LakeStratification)))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="pH (unitless)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10)))     })
  
  output$rangeTableSingleSite <- renderDataTable({
    req(oneStation())
    z <- pHExceedances(oneStation()) %>%
      filter(exceeds == TRUE) %>%
      rename('Parameter Rounded to WQS Format' = 'parameterRound') %>%
      dplyr::select(-c(FDT_STA_ID, limit, interval, exceeds)) %>%
      dplyr::select(FDT_DATE_TIME, FDT_DEPTH, FDT_FIELD_PH, LEVEL_FDT_FIELD_PH, LakeStratification, everything()) %>%
      arrange(FDT_DATE_TIME, FDT_DEPTH)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "300px", dom='t'),
              selection = 'none')})
  
  
  output$stationExceedanceRate <- renderDataTable({
    req(ns(input$oneStationSelection), oneStation())
    z <- pHExceedances(oneStation()) %>% quickStats('PH') %>% dplyr::select(-PH_STAT)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "60px", dom='t'),
              selection = 'none') })
  
  output$rangeTableAU <- renderDataTable({req(AUdata())
    z <- pHExceedances(AUdata()) %>%
      filter(exceeds == TRUE) %>%
      rename('Parameter Rounded to WQS Format' = 'parameterRound') %>%
      dplyr::select(-c(limit, interval, exceeds)) %>%
      dplyr::select(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH, FDT_FIELD_PH, LEVEL_FDT_FIELD_PH, LakeStratification, everything()) %>%
      arrange(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "300px", dom='t'),
              selection = 'none')})
  
  
  output$AUExceedanceRate <- renderDataTable({ req(AUdata())
    z <- pHExceedances(AUdata()) %>% quickStats('PH') %>% dplyr::select(-PH_STAT)
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "60px", dom='t'),
              selection = 'none') })
  
}
