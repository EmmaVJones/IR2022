#source('appTestingData.R')


thermoclinePlotlySingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      h4(strong('Single Station Data Visualization')),
      fluidRow(column(4,uiOutput(ns('oneStationSelectionUI'))),
               column(4,actionButton(ns('reviewData'),"Review Raw Parameter Data",class='btn-block', width = '250px'))),
      helpText(span(strong('Click on a Sample Date below to plot the thermocline graph for the selected Sample Date.'),
                    'All data presented in the interactive plot is raw data. Rounding rules are appropriately applied 
                    to the assessment functions utilized by the application.')),
      fluidRow(
        column(4,
               h5(strong("Thermocline Results")),
               DT::dataTableOutput(ns('thermoclineResults'))),
        column(8,h5(strong("Thermocline Results By Sample Date")),
               plotlyOutput(ns('plotly')))),
      #verbatimTextOutput(ns('test')),
      br(),br(),br()
    )
  )
}



thermoclinePlotlySingleStation <- function(input,output,session, AUdata, stationSelectedAbove){
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
  output$parameterData <- DT::renderDataTable({req(oneStation())
    parameterFilter <- dplyr::select(oneStation(), FDT_STA_ID:FDT_COMMENT, FDT_TEMP_CELCIUS, RMK_FDT_TEMP_CELCIUS, LEVEL_FDT_TEMP_CELCIUS,
                                     ThermoclineDepth, LakeStratification)
    
    DT::datatable(parameterFilter, rownames = FALSE, 
                  options= list(dom= 't', pageLength = nrow(parameterFilter), scrollX = TRUE, scrollY = "400px", dom='t'),
                  selection = 'none') %>%
      formatStyle(c( 'FDT_TEMP_CELCIUS', 'RMK_FDT_TEMP_CELCIUS', 'LEVEL_FDT_TEMP_CELCIUS'), 'LEVEL_FDT_TEMP_CELCIUS', 
                  backgroundColor = styleEqual(c('Level II', 'Level I'), c('yellow','orange'), default = 'lightgray'))  })
  
  thermoclineByDay <- reactive({ req(oneStation())
    dplyr::select(oneStation(), SampleDate, ThermoclineDepth) %>%
      distinct(SampleDate,.keep_all = TRUE) %>%
      mutate(ThermoclineDepth = ifelse(is.na(ThermoclineDepth), 'No Thermocline', ThermoclineDepth))  })
  
  # Thermocline Analysis Table
  output$thermoclineResults <- DT::renderDataTable({ req(oneStation())
    DT::datatable(thermoclineByDay(), rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(thermoclineByDay()), 
                                                                    scrollY = "300px", dom='ti'),
                  selection = 'single') %>%
      formatStyle("ThermoclineDepth", backgroundColor = styleEqual('No Thermocline',"gray")) })
  
  # Thermoncline plotly based on user date input
  output$plotly <- renderPlotly({
    req(oneStation(), input$thermoclineResults_rows_selected)
    dateSelected <- thermoclineByDay()[input$thermoclineResults_rows_selected,]$SampleDate
    dat <- dplyr::filter(oneStation(), SampleDate %in% dateSelected)
    suppressWarnings(suppressMessages(
      plot_ly(data=dat)%>%
        add_lines(x=~FDT_TEMP_CELCIUS, y=~ThermoclineDepth, mode='line',line = list(color = '#E50606'),
                  hoverinfo = "text", text= paste("Thermocline Depth:",unique(dat$ThermoclineDepth),sep=''))%>%
        add_markers(x= ~FDT_TEMP_CELCIUS, y= ~FDT_DEPTH,mode = 'scatter', name="Temperature",
                    color=~LakeStratification, colors=c('#BF382A', '#0C4B8E'),
                    hoverinfo="text",text=~paste(sep="<br>",paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste("Temperature:",FDT_TEMP_CELCIUS,"C"),
                                                 paste("LakeStratification: ", LakeStratification)))%>%
        layout(xaxis = list(autorange = "reversed",title="Temperature (Celcius)"),
               yaxis = list(autorange = "reversed",title="Depth (m)"),
               showlegend=FALSE) ))  })
  
    # output$test <- renderPrint({thermoclineByDay()[input$thermoclineResults_rows_selected,]$SampleDate })
      
}





ui <- fluidPage(
  helpText('Review each site using the single site visualization section, then 
           proceed to the bottom of the page to find exceedance rate for the entire assessment unit.',br(),
           span(strong('NOTE: The temperature exceedance analysis results at the bottom of the page include data
                       from ALL stations within the assessment unit.'))),
  thermoclinePlotlySingleStationUI('thermocline'))

server <- function(input,output,session){
  
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData1, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  
  AUData <- reactive({filter_at(conventionalsLake1, vars(starts_with("ID305B")), any_vars(. %in% selectedAU1) ) })
  
  callModule(thermoclinePlotlySingleStation, 'thermocline', AUData, stationSelected)
}

shinyApp(ui,server)


