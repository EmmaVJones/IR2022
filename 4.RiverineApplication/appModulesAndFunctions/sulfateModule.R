DSulfatePlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6,uiOutput(ns('DSulfate_oneStationSelectionUI'))),
               column(6,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      selectInput(ns('sulfateType'),'Select Total or Dissolved Sulfate', choices = c('Total Sulfate', 'Dissolved Sulfate'),
                  width = '30%'),
      plotlyOutput(ns('DSulfateplotly')),
      conditionalPanel(paste0("input['",ns('sulfateType'),"'] == 'Total Sulfate'"),
                       #ns("input.sulfateType == 'Total Sulfate'"),
                       fluidRow(
                         column(8, h5('All total sulfate records that are above the PWS criteria (where applicable) for the ',span(strong('selected site')),' are highlighted below.'),
                                div(style = 'height:150px;overflow-y: scroll', tableOutput(ns('TSulfateRangeTableSingleSite')))),
                         column(4, h5('Individual total sulfate exceedance statistics for the ',span(strong('selected site')),' are highlighted below.
                                      If no data is presented, then the PWS criteria is not applicable to the station.'),
                                tableOutput(ns("stationTSulfateExceedanceRate"))))
      )
      
    )
  )
}


DSulfatePlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$DSulfate_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('DSulfate_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  DSulfate_oneStation <- reactive({
    req(ns(input$DSulfate_oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$DSulfate_oneStationSelection) })
  # because we are dealing with two variables here, do NOT filter by NA occurrences in case you drop unintended rows
  #filter(!is.na(SULFATE_DISS)) %>%
  #filter(!is.na(SULFATE_TOTAL)) })
  
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
    req(DSulfate_oneStation())
    parameterFilter <- dplyr::select(DSulfate_oneStation(), FDT_STA_ID:FDT_COMMENT, SULFATE_DISS, RMK_00946, SULFATE_TOTAL, RMK_00945)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t')) %>%
      formatStyle(c('SULFATE_DISS','RMK_00946'), 'RMK_00946', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray')) %>%
      formatStyle(c('SULFATE_TOTAL','RMK_00945'), 'RMK_00945', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
    
  })
  
  
  output$DSulfateplotly <- renderPlotly({
    req(input$DSulfate_oneStationSelection, DSulfate_oneStation(), input$sulfateType)
    if(input$sulfateType == 'Dissolved Sulfate'){
      dat <- DSulfate_oneStation()
      dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
      
      maxheight <- ifelse(max(dat$SULFATE_DISS, na.rm=T) < 75, 100, max(dat$SULFATE_DISS, na.rm=T)* 1.2)
      box1 <- data.frame(SampleDate = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(75, maxheight, maxheight, 75))
      box2 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(25, 75, 75, 25))
      box3 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(10, 25, 25, 10))
      box4 <- data.frame(x = c(min(dat$SampleDate), min(dat$SampleDate), max(dat$SampleDate),max(dat$SampleDate)), y = c(0, 10, 10, 0))
      
      
      plot_ly(data=dat)%>%
        add_polygons(x = ~SampleDate, y = ~y, data = box1, fillcolor = "firebrick",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('High Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box2, x = ~x, y = ~y, fillcolor = "#F0E442",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Medium Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box3, x = ~x, y = ~y, fillcolor = "#009E73",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('Low Probability of Stress to Aquatic Life')) %>%
        add_polygons(data = box4, x = ~x, y = ~y, fillcolor = "#0072B2",opacity=0.6, line = list(width = 0),
                     hoverinfo="text", name =paste('No Probability of Stress to Aquatic Life')) %>%
        add_markers(data=dat, x= ~SampleDate, y= ~SULFATE_DISS,mode = 'scatter', name="Dissolved Sulfate (mg/L)",marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Dissolved Sulfate: ",SULFATE_DISS,"mg/L")))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="Dissolved Sulfate (mg/L)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    }else{
      dat <- mutate(DSulfate_oneStation(), top = 250)
      dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME2, format="%m/%d/%y")
      
      plot_ly(data=dat)%>%
        add_lines(data=dat, x=~SampleDate,y=~top, mode='line', line = list(color = 'black'),
                  hoverinfo = "text", text = "Sulfate PWS Criteria (250,000 ug/L)", name="Sulfate PWS Criteria (250 mg/L)") %>%
        add_markers(data=dat, x= ~SampleDate, y= ~SULFATE_TOTAL,mode = 'scatter', name="Total Sulfate (mg/L)", marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Total Sulfate: ",SULFATE_TOTAL," (mg/L)")))%>%
        layout(showlegend=FALSE,
               yaxis=list(title="Total Sulfate (mg/L)"),
               xaxis=list(title="Sample Date",tickfont = list(size = 10)))
    }
    
  })
  
  output$TSulfateRangeTableSingleSite <- renderTable({
    req(DSulfate_oneStation())
    if(grepl('PWS', unique(DSulfate_oneStation()$SPSTDS))){
      return(dplyr::select(citmonOutOfBacteria(DSulfate_oneStation(), SULFATE_TOTAL, RMK_00945), FDT_DATE_TIME, FDT_DEPTH, SULFATE_TOTAL) %>%
               filter(!is.na(SULFATE_TOTAL)) %>% #get rid of NA's
               mutate(PWSlimit = 250) %>%
               mutate(exceeds = ifelse(SULFATE_TOTAL > PWSlimit, T, F)) %>% # Identify where above PWS limit
               filter(exceeds == TRUE))  
    }else {
      return('Station not designated PWS')
    }  })
  
  output$stationTSulfateExceedanceRate <- renderTable({
    req(DSulfate_oneStation())
    if(grepl('PWS', unique(DSulfate_oneStation()$SPSTDS))){
      x <- TSulfatePWS(citmonOutOfBacteria(DSulfate_oneStation(), SULFATE_TOTAL, RMK_00945))
      if(nrow(x) >0) {
        return(dplyr::select(x,1:3) %>%# don't give assessment determination for single station
                 dplyr::rename(nSamples = PWS_Acute_Total_Sulfate_SAMP,
                               nExceedance= PWS_Acute_Total_Sulfate_VIO,
                               exceedanceRate= PWS_Acute_Total_Sulfate_exceedanceRate))# make it match everything else
      }
    } else {
      return('Station not designated PWS')
    }  }) 
  
  
  
}
