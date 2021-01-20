#source('appTestingData.R')



NitratePlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(3,uiOutput(ns('oneStationSelectionUI'))),
               column(1),
               column(3,br(), uiOutput(ns('changeWQSUI'))),
               column(1),
               column(3,br(),actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText('All data presented in the interactive plot is raw data. Rounding rules are appropriately applied to the 
               assessment functions utilized by the application.'),
      plotlyOutput(ns('plotly')),
      fluidRow(
        column(8, h5('All nitrate records that are above the PWS criteria (where applicable) for the ',span(strong('selected site')),' are highlighted below.'),
               div(style = 'height:150px;overflow-y: scroll', dataTableOutput(ns('rangeTableSingleSite')))),
        column(4, h5('Individual nitrate exceedance statistics for the ',span(strong('selected site')),' are highlighted below.
                     If no data is presented, then the PWS criteria is not applicable to the station.'),
               dataTableOutput(ns("stationExceedanceRate"))))
    )
  )
}


NitratePlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
  ns <- session$ns
  
  output$oneStationSelectionUI <- renderUI({
    req(AUdata)
    selectInput(ns('oneStationSelection'),strong('Select Station to Review'),
                choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))), # Change this based on stationSelectedAbove
                width='200px', selected = stationSelectedAbove())})
  
  oneStation <- reactive({
    req(ns(input$oneStationSelection))
    filter(AUdata(),FDT_STA_ID %in% input$oneStationSelection) %>%
      filter(!is.na(NITRATE_mg_L)) %>%
      mutate(`Parameter Rounded to WQS Format` = round(NITRATE_mg_L, digits = 0),
             PWSlimit = 10)})
  
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
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID:FDT_COMMENT, NITRATE_mg_L, RMK_NITRATE, LEVEL_NITRATE)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t'),
                  selection = 'none') %>%
      formatStyle(c('NITRATE_mg_L','RMK_NITRATE', 'LEVEL_NITRATE'), 'LEVEL_NITRATE', backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))
  })
  
  output$plotly <- renderPlotly({
    req(input$oneStationSelection, oneStation())
    dat <- oneStation()
    dat$SampleDate <- as.POSIXct(dat$FDT_DATE_TIME, format="%m/%d/%y")
    
    # Fix look of single measure
    if(nrow(dat) == 1){
      print('yes')
      dat <- bind_rows(dat,
                       tibble(SampleDate = c(dat$SampleDate- days(5), dat$SampleDate + days(5)),
                              PWSlimit = c(10, 10)))
    }
    

    if(input$changeWQS == TRUE){
        
        plot_ly(data=dat)%>%
          add_lines(data=dat, x=~SampleDate,y=~PWSlimit, mode='line', line = list(color = 'black'),
                    hoverinfo = "text", text= "PWS Criteria (10 mg/L)", name="PWS Criteria (10 mg/L)") %>%
          add_markers(data=dat, x= ~SampleDate, y= ~NITRATE_mg_L,mode = 'scatter', name="Dissolved Nitrate (mg/L)",marker = list(color= '#535559'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Dissolved Nitrate: ",NITRATE_mg_L,"mg/L")))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title="Dissolved Nitrate (mg/L)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
      } else {
        plot_ly(data=dat)%>%
          add_markers(data=dat, x= ~SampleDate, y= ~NITRATE_mg_L,mode = 'scatter', name="Dissolved Nitrate (mg/L)",marker = list(color= '#535559'),
                      hoverinfo="text",text=~paste(sep="<br>",
                                                   paste("Date: ",SampleDate),
                                                   paste("Depth: ",FDT_DEPTH, "m"),
                                                   paste("Dissolved Nitrate: ",NITRATE_mg_L,"mg/L")))%>%
          layout(showlegend=FALSE,
                 yaxis=list(title="Dissolved Nitrate (mg/L)"),
                 xaxis=list(title="Sample Date",tickfont = list(size = 10)))
        
       }
  })
  
  
  output$rangeTableSingleSite <- renderDataTable({
    req(oneStation())
    if(input$changeWQS == TRUE){
      z <- filter(oneStation(), `Parameter Rounded to WQS Format` > PWSlimit) %>%
        dplyr::select(FDT_DATE_TIME, NITRATE_mg_L, LEVEL_NITRATE, Criteria = PWSlimit, `Parameter Rounded to WQS Format`) 
    } else { z <- NULL}
    datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
              selection = 'none') })
  
  output$stationExceedanceRate <- renderDataTable({
    req(input$oneStationSelection, oneStation())
    if(input$changeWQS == TRUE){
      nitrate <- dplyr::select(oneStation(), FDT_DATE_TIME, FDT_DEPTH, NITRATE_mg_L, LEVEL_NITRATE) %>%
        filter(!(LEVEL_NITRATE %in% c('Level II', 'Level I'))) %>% # get lower levels out
        filter(!is.na(NITRATE_mg_L)) %>% #get rid of NA's
        mutate(`Parameter Rounded to WQS Format` = round(NITRATE_mg_L, digits = 0),  # round to WQS https://law.lis.virginia.gov/admincode/title9/agency25/chapter260/section140/
               limit = 10) %>%
        rename(parameter = !!names(.[5])) %>% # rename columns to make functions easier to apply
        mutate(exceeds = ifelse(parameter > limit, T, F)) # Identify where above NH3 WQS limit
      z <- quickStats(nitrate, 'PWS_Nitrate') %>% dplyr::select(-PWS_Nitrate_STAT) 
      datatable(z, rownames = FALSE, options= list(pageLength = nrow(z), scrollX = TRUE, scrollY = "150px", dom='t'),
                selection = 'none') }}) 
  
  
  
}
  
 



ui <- fluidPage(
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  NitratePlotlySingleStationUI('Nitrate')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter_at(conventionalsLake1, vars(starts_with("ID305B")), any_vars(. %in% selectedAU1) ) })
  
  
  callModule(NitratePlotlySingleStation,'Nitrate', AUData, stationSelected)
  
}

shinyApp(ui,server)





