TPPlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(3,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               column(2,uiOutput(ns('changeLacustrineUI'))),
               column(2,helpText('The default Lacustrine Zone selection is what is designated in the user uploaded stations table.')),
               column(1),
               column(3,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application.'),
      plotlyOutput(ns('plotly')) ,
      br(), hr(), br(),
      fluidRow(
        column(7, h5('Annual Total Phosphorus median values for the ',span(strong('selected site')),' are displayed below.'),
               helpText('Note: Assessment is based on the two most recent monitoring years that data are available within the 
                        assessment window. A third sampling year may be needed if the two previous years result in differing 
                        assessment statuses. '),
               dataTableOutput(ns("annualMedianTableSingleSite"))),
        column(1),
        column(4, 
               h5('Total Phosphorus exceedance statistics for the ',span(strong('selected site')),' are highlighted below.
                  These are the results reflected in the stations table above.'),
               dataTableOutput(ns("stationExceedanceRate")))),
      br(),
      wellPanel(
        h4(strong('AU Assessment')),
        helpText("The 2022 IR Guidance states: For lake or reservoir assessment units with multiple stations, 
        chlorophyll-a and total phosphorous data should be pooled by calculating the median of same-month 
        observations from April to October of any given year in a homogenous unit. The 90th percentile of monthly 
        chlorophyll-a medians is the value used to compare to the chlorophyll-a criterion for a particular 
        lake/reservoir.  The median of monthly TP medians should be used to assess against the applicable total 
        phosphorous criterion. '"),
        helpText(strong('This application provides an AU level assessment, but it is up to the assessor to determine 
                        if algaecides have been used in any part of the lake.')),
        fluidRow(
          column(7, h5('Annual Total Phosphorus median values for only stations that have an ID305B_1 matching
                       the ',span(strong('selected Assessment Unit')),' are displayed below.'),
                 dataTableOutput(ns('annualMedianTableAU'))),
          column(1),
          column(4, h5('Total Phosphorus exceedance statistics for the ',span(strong('Assessment Unit')),' are highlighted below.'),
                 dataTableOutput(ns("AUExceedanceRate"))))
      )
    ) )
}


TPPlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove, AUselectionFromOutsideModal){
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
      filter(!is.na(PHOSPHORUS_mg_L))})
  
  # Option to change lacustrine designation and force function to calculate resutls
  output$changeLacustrineUI <- renderUI({
    req(oneStation_original())
    selectInput(ns('changeLacustrine'),strong('Lacustrine Zone Designation For Analysis'),
                choices= c('NA', 'Y'),
                width='400px', selected = unique(oneStation_original()$LACUSTRINE)) })
  
  # change WQS for rest of module if user chooses to do so
  oneStation <- reactive({req(oneStation_original(), input$changeLacustrine)
    mutate(oneStation_original(), LACUSTRINE = input$changeLacustrine) })
  
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
    req(oneStation_original())
    parameterFilter <- dplyr::select(oneStation_original(), FDT_STA_ID:FDT_COMMENT, PHOSPHORUS_mg_L, RMK_PHOSPHORUS, LEVEL_PHOSPHORUS, LACUSTRINE)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t'),
                  selection = 'none') %>%
      formatStyle(c('PHOSPHORUS_mg_L','RMK_PHOSPHORUS', 'LEVEL_PHOSPHORUS'), 'LEVEL_PHOSPHORUS', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation())
    dat <- mutate(oneStation(), top = `Total Phosphorus (mg/L)`,
                  LakeStratification = replace_na(LakeStratification,"NA")) %>%
      mutate(LakeStratification = factor(LakeStratification,levels=c("Epilimnion",'NA',"Hypolimnion")))#,ordered=T)
    
    
    # Fix look of single measure
    if(nrow(dat) == 1){
      print('yes')
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5))))    }
    
    plot_ly(data=dat) %>%
      add_lines(data=dat, x=~SampleDate,y=~top, mode='line', line = list(color = 'black'),
                hoverinfo = "text", text="Total Phosphorus Limit", name="Total Phosphorus Limit") %>%
      add_markers(data=dat, x= ~SampleDate, y= ~PHOSPHORUS_mg_L,mode = 'scatter', name="Total Phosphorus (mg/L)",
                  color=~LakeStratification, #marker = list(color= '#535559'),
                  hoverinfo="text",text=~paste(sep="<br>",
                                               paste("Date: ",SampleDate),
                                               paste("Depth: ",FDT_DEPTH, "m"),
                                               paste("Total Phosphorus: ",PHOSPHORUS_mg_L,"mg/L"),
                                               paste("LakeStratification: ",LakeStratification)))%>%
      layout(showlegend=FALSE,
             yaxis=list(title="Total Phosphorus (mg/L)"),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
  
  output$annualMedianTableSingleSite <- renderDataTable({req(oneStation())
    z <- TP_analysis(oneStation()) %>%
      rename('Total Phosphorus Limit' = 'Total Phosphorus (mg/L)')
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none') })
  
  output$stationExceedanceRate <- renderDataTable({req(oneStation())
    z <- TP_Assessment(oneStation())
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "70px", dom='t'),
              selection = 'none') })
  
  
  # AU Assessment Section
  justAUID305B_1 <- reactive({
    filter(AUdata(), ID305B_1 %in% AUselectionFromOutsideModal()) %>%
      filter(!is.na(PHOSPHORUS_mg_L))})
  
  output$annualMedianTableAU <- renderDataTable({req(justAUID305B_1())
    z <- TP_analysis(justAUID305B_1()) %>%
      rename('Total Phosphorus Limit' = 'Total Phosphorus (mg/L)')
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "200px", dom='t'),
              selection = 'none') })
  
  output$AUExceedanceRate <- renderDataTable({req(justAUID305B_1())
    z <- TP_Assessment(justAUID305B_1())
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "70px", dom='t'),
              selection = 'none') })
  
}