source('appTestingData.R')


DSulfatePlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(6,uiOutput(ns('oneStationSelectionUI'))),
               column(6,br(),actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      fluidRow(
        column(3, selectInput(ns('sulfateType'),'Select Total or Dissolved Sulfate', choices = c('Total Sulfate', 'Dissolved Sulfate'))),
        conditionalPanel(paste0("input['",ns('sulfateType'),"'] == 'Dissolved Sulfate'"),column(6,br(),checkboxInput(ns('displayBSAcolors'), 'Display Benthic Stressor Analysis Colors on Plot', value = TRUE))),
        conditionalPanel(paste0("input['",ns('sulfateType'),"'] == 'Total Sulfate'"), column(6,br(), uiOutput(ns('changeWQSUI'))))),
      plotlyOutput(ns('plotly')),
      conditionalPanel(paste0("input['",ns('sulfateType'),"'] == 'Total Sulfate'"),
                       #ns("input.sulfateType == 'Total Sulfate'"),
                       fluidRow(
                         column(8, h5('All total sulfate records that are above the PWS criteria (where applicable) for the ',span(strong('selected site')),' are highlighted below.'),
                                div(style = 'height:150px;overflow-y: scroll', dataTableOutput(ns('rangeTableSingleSite')))),
                         column(4, h5('Individual total sulfate exceedance statistics for the ',span(strong('selected site')),' are highlighted below.
                                      If no data is presented, then the PWS criteria is not applicable to the station.'),
                                dataTableOutput(ns("stationExceedanceRate"))))
      )
      
    )
  )
}


DSulfatePlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  output$oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  
  oneStation <- reactive({
    req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
      mutate(`Parameter Rounded to WQS Format` = round(SULFATE_TOTAL_mg_L, digits = 0),
             PWSlimit = 250)})
  # because we are dealing with two variables here, do NOT filter by NA occurrences in case you drop unintended rows
      #filter(!is.na(SULFATE_DISS_mg_L)) %>%
      #filter(!is.na(SULFATE_TOTAL_mg_L)) })
  
  # Option to change WQS used for modal
  output$changeWQSUI <- renderUI({
    req(oneStation())
    if(nrow(oneStation()) > 0){
      defaultPWS <- unique(oneStation()$PWS) %in% c("Yes")
    } else { defaultPWS <- FALSE}
    checkboxInput(ns('changeWQS'),'Apply Public Water Supply Water Quality Standards (Automatically selected if PWS standards apply to the selected station)', value = defaultPWS) })
  
  
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
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID:FDT_COMMENT, SULFATE_DISS_mg_L, RMK_SULFATE_DISS, LEVEL_SULFATE_DISS,
                                     SULFATE_TOTAL_mg_L, RMK_SULFATE_TOTAL, LEVEL_SULFATE_TOTAL)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t'),
                  selection = 'none') %>%
      formatStyle(c('SULFATE_DISS_mg_L','RMK_SULFATE_DISS', 'LEVEL_SULFATE_DISS'), 'LEVEL_SULFATE_DISS', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray')) %>%
      formatStyle(c('SULFATE_TOTAL_mg_L','RMK_SULFATE_TOTAL', 'LEVEL_SULFATE_TOTAL'), 'LEVEL_SULFATE_TOTAL', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))  })
  
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation(), input$sulfateType)
    dat <- oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
    
    # Fix look of single measure
    if(nrow(dat) == 1){
      print('yes')
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5)),
                              PWSlimit = c(250, 250)))
    }
    
    if(input$sulfateType == 'Dissolved Sulfate'){
      maxheight <- ifelse(max(dat$SULFATE_DISS_mg_L, na.rm=T) < 75, 100, max(dat$SULFATE_DISS_mg_L, na.rm=T)* 1.2)
      
      if(input$displayBSAcolors == TRUE){
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
          add_markers(data=dat, x= ~SampleDate, y= ~SULFATE_DISS_mg_L,mode = 'scatter', name="Dissolved Sulfate (mg/L)",marker = list(color= '#535559'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Dissolved Sulfate: ",SULFATE_DISS_mg_L,"mg/L")))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title="Dissolved Sulfate (mg/L)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
      } else {
        plot_ly(data=dat)%>%
          add_markers(data=dat, x= ~SampleDate, y= ~SULFATE_DISS_mg_L,mode = 'scatter', name="Dissolved Sulfate (mg/L)",marker = list(color= '#535559'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Dissolved Sulfate: ",SULFATE_DISS_mg_L,"mg/L")))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title="Dissolved Sulfate (mg/L)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
      }
      
      
    }else{
      if(input$changeWQS == TRUE){
        plot_ly(data=dat)%>%
          add_lines(data=dat, x=~SampleDate,y=~PWSlimit, mode='line', line = list(color = 'black'),
                    hoverinfo = "text", text = "Sulfate PWS Criteria (250,000 ug/L)", name="Sulfate PWS Criteria (250 mg/L)") %>%
          add_markers(data=dat, x= ~SampleDate, y= ~SULFATE_TOTAL_mg_L,mode = 'scatter', name="Total Sulfate (mg/L)", marker = list(color= '#535559'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Total Sulfate: ",SULFATE_TOTAL_mg_L," (mg/L)")))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title="Total Sulfate (mg/L)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
      } else {
        plot_ly(data=dat)%>%
          add_markers(data=dat, x= ~SampleDate, y= ~SULFATE_TOTAL_mg_L,mode = 'scatter', name="Total Sulfate (mg/L)", marker = list(color= '#535559'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Total Sulfate: ",SULFATE_TOTAL_mg_L," (mg/L)")))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title="Total Sulfate (mg/L)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
      }
    }
    
  })
  
  output$rangeTableSingleSite <- renderDataTable({
    req(oneStation())
    if(input$changeWQS == TRUE){
      z <- filter(oneStation(), `Parameter Rounded to WQS Format` > PWSlimit) %>%
        dplyr::select(FDT_DATE_TIME, SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL, Criteria = PWSlimit, `Parameter Rounded to WQS Format`) 
    } else { z <- NULL}
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
              selection = 'none') })
  
  output$stationTSulfateExceedanceRate <- renderDataTable({
    req(input$oneStationSelection, oneStation())
    if(input$changeWQS == TRUE){
      totalSulfate <- dplyr::select(oneStation(), FDT_DATE_TIME, FDT_DEPTH, SULFATE_TOTAL_mg_L, LEVEL_SULFATE_TOTAL) %>%
        filter(!(LEVEL_SULFATE_TOTAL %in% c('Level II', 'Level I'))) %>% # get lower levels out
        filter(!is.na(SULFATE_TOTAL_mg_L)) %>% #get rid of NA's
        mutate(`Parameter Rounded to WQS Format` = round(SULFATE_TOTAL_mg_L, digits = 0),  # round to WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
               limit = 250) %>%
        rename(parameter = !!names(.[5])) %>% # rename columns to make functions easier to apply
        mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above NH3 WQS limit
      z <- quickStats(totalSulfate, 'PWS_Total_Sulfate') %>% dplyr::select(-PWS_Total_Sulfate_STAT) 
      datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
                selection = 'none') }}) 
}





ui <- fluidPage(
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  DSulfatePlotlySingleStationUI('DSulfate')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  #AUData <- reactive({filter_at(conventionals_HUC, vars(starts_with("ID305B")), any_vars(. %in% AUselection) ) })
  AUData <- reactive({filter(conventionals, Huc6_Vahu6 %in% c("JU11")) %>%#c('JM01','JM02', 'JM03', 'JM04', 'JM05', 'JM06')) %>%
      left_join(dplyr::select(stationTable, STATION_ID:VAHU6,
                              WQS_ID:CLASS_DESCRIPTION),
                #WQS_ID:`Max Temperature (C)`), 
                by = c('FDT_STA_ID' = 'STATION_ID')) %>%
      filter(!is.na(ID305B_1)) %>%
      pHSpecialStandardsCorrection() %>%
      filter(!is.na(SULFATE_DISS_mg_L))})
  
  callModule(DSulfatePlotlySingleStation,'DSulfate', AUData, stationSelected)
  
}

shinyApp(ui,server)

