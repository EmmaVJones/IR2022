
metalsTableSingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel('Water Column Metals',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('WCmetals_oneStationSelectionUI')),
                 h5('All water column metals data available for the ',span(strong('selected site')),' are available below. 
                    If no data is presented, then the station does not have any water column metals data available.'),
                 DT::dataTableOutput(ns('WCmetalsRangeTableSingleSite')),br(), br(), br(),
                 h5('Metals assessments for the ',span(strong('selected site')),' are highlighted below.'),
                 DT::dataTableOutput(ns("WCstationmetalsExceedanceRate")))),
      tabPanel('Sediment Metals',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('Smetals_oneStationSelectionUI')),
                 h5('All sediment metals data available for the ',span(strong('selected site')),' are available below. 
                    If no data is presented, then the station does not have any sediment metals data available.'),
                 DT::dataTableOutput(ns('SmetalsRangeTableSingleSite')),br(), br(), br(),
                 h5('Metals assessments for the ',span(strong('selected site')),' are highlighted below.'),
                 DT::dataTableOutput(ns("SstationmetalsExceedanceRate"))))
      
      ))
}


metalsTableSingleStation <- function(input,output,session, AUdata, WCmetals ,Smetals, stationSelectedAbove){
  ns <- session$ns
  
  # Select One station for individual review
  output$WCmetals_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('WCmetals_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  WCmetals_oneStation <- reactive({
    req(ns(input$WCmetals_oneStationSelection))
    filter(WCmetals, FDT_STA_ID %in% input$WCmetals_oneStationSelection)})
  
  
  output$WCmetalsRangeTableSingleSite <- DT::renderDataTable({
    req(WCmetals_oneStation())
    z <- dplyr::select(WCmetals_oneStation(), FDT_STA_ID:HARDNESS)
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')     })
  
  output$WCstationmetalsExceedanceRate <- DT::renderDataTable({
    req(input$WCmetals_oneStationSelection, WCmetals_oneStation())
    z <- dplyr::select(WCmetals_oneStation(), FDT_STA_ID, `FDT_DATE_TIME`,`ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`)
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none') %>%
      formatStyle(names(z), backgroundColor = styleEqual(c('NSP'), c('red'))) # highlight cells red if not supporting
  }) 
  
  
  # Sediment Metals
  # Select One station for individual review
  output$Smetals_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('Smetals_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  Smetals_oneStation <- reactive({
    req(ns(input$Smetals_oneStationSelection))
    filter(Smetals, FDT_STA_ID %in% input$Smetals_oneStationSelection)})
  
  
  output$SmetalsRangeTableSingleSite <- DT::renderDataTable({
    req(Smetals_oneStation())
    z <- dplyr::select(Smetals_oneStation(), FDT_STA_ID, FDT_DATE_TIME:ENDRINT)
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')     })
  
  output$SstationmetalsExceedanceRate <- DT::renderDataTable({
    req(input$Smetals_oneStationSelection, Smetals_oneStation())
    z <- dplyr::select(Smetals_oneStation(), FDT_STA_ID, `FDT_DATE_TIME`,ARSENIC:COMMENT)
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none') %>%
      formatStyle(names(z), backgroundColor = styleEqual(c('OE'), c('red'))) # highlight cells red if not supporting
  }) 
  
  
}
