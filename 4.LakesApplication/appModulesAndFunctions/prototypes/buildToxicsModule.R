# source('appTestingData.R')

toxicsSingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel('Water Column PCBs',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('WCPBC_oneStationSelectionUI')),
                 h5('All water column PBC data available for the ',span(strong('selected site')),' are available below. 
         If no data is presented, then the station does not have any water column PBC data available.'),
                 DT::dataTableOutput(ns('WCPBCRangeTableSingleSite')),br(), br(), br())),#,
                 #h5('PBC assessments for the ',span(strong('selected site')),' are highlighted below.'),
                 #DT::dataTableOutput(ns("WCstationPBCExceedanceRate")))),
      tabPanel('Sediment PCBs',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('SPBC_oneStationSelectionUI')),
                 h5('All sediment PBC data available for the ',span(strong('selected site')),' are available below. 
                    If no data is presented, then the station does not have any sediment PBC data available.'),
                 DT::dataTableOutput(ns('SPBCRangeTableSingleSite')),br(), br(), br())),#,
                 #h5('PBC assessments for the ',span(strong('selected site')),' are highlighted below.'),
                 #DT::dataTableOutput(ns("SstationPBCExceedanceRate")))),
      tabPanel('Fish Tissue PCBs',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 fluidRow(column(6, uiOutput(ns('FPBC_oneStationSelectionUI'))),
                          column(6, DT::dataTableOutput(ns('FPBCscreeningValues')))),
                        
                 #verbatimTextOutput(ns('test')),
                 h5('All fish tissue PBC exceedances for the ',span(strong('selected site')),' are highlighted according to the 
                    screening values listed above. If no data is presented, then the station does not have any fish tissue PBC data.'),
                 DT::dataTableOutput(ns('FPBCRangeTableSingleSite')),br(), br(), br()))#,
                 #h5('All fish tissue PBC data available for the ',span(strong('selected site')),' are available below. 
                #    If no data is presented, then the station does not have any fish tissue PBC data available.'),
                # helpText('All concentrations expressed as ppm (mg/kg), wet weight, in edible fish tissue fillet'),
                # DT::dataTableOutput(ns('FPBCRangeTableSingleSite'))) )
      
    ))
}

toxicsSingleStation <- function(input,output,session, AUdata, markPCB, fishPCB, stationSelectedAbove){
  ns <- session$ns
  
  ## Water Column PCBs
  
  # Select One station for individual review
  output$WCPBC_oneStationSelectionUI <- renderUI({req(stationSelectedAbove)
    selectInput(ns('WCPBC_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  WCPBC_oneStation <- reactive({req(ns(input$WCPBC_oneStationSelection))
    filter(markPCB, SampleMedia %in% c( "Water (whole)", "Water (dissolved)")) %>%
      filter(StationID %in% input$WCPBC_oneStationSelection)})
  
  
  output$WCPBCRangeTableSingleSite <- DT::renderDataTable({req(WCPBC_oneStation())
    DT::datatable(WCPBC_oneStation(), rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(WCPBC_oneStation()), scrollY = "250px", dom='t'),
                  selection = 'none')     })
  
  # output$WCstationPBCExceedanceRate <- DT::renderDataTable({
  #   req(input$WCPBC_oneStationSelection, WCPBC_oneStation())
  #   z <- dplyr::select(WCPBC_oneStation(), FDT_STA_ID, `FDT_DATE_TIME`,`ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`)
  #   z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
  #   DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
  #                 selection = 'none') %>%
  #     formatStyle(names(z), backgroundColor = styleEqual(c('NSP'), c('red'))) # highlight cells red if not supporting
  # }) 
  
  
  ## Sediment PCBs
  
  # Select One station for individual review
  output$SPBC_oneStationSelectionUI <- renderUI({req(stationSelectedAbove)
    selectInput(ns('SPBC_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  SPBC_oneStation <- reactive({req(ns(input$SPBC_oneStationSelection))
    filter(markPCB, SampleMedia == 'Sediment') %>%
      filter(StationID %in% input$SPBC_oneStationSelection)})
  
  
  output$SPBCRangeTableSingleSite <- DT::renderDataTable({req(SPBC_oneStation())
    DT::datatable(SPBC_oneStation(), rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(SPBC_oneStation()), scrollY = "250px", dom='t'),
                  selection = 'none')     })
  
  # output$SstationPBCExceedanceRate <- DT::renderDataTable({
  #   req(input$SPBC_oneStationSelection, SPBC_oneStation())
  #   z <- dplyr::select(SPBC_oneStation(), FDT_STA_ID, `FDT_DATE_TIME`,ARSENIC:COMMENT)
  #   z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
  #   DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
  #                 selection = 'none') %>%
  #     formatStyle(names(z), backgroundColor = styleEqual(c('OE'), c('red'))) # highlight cells red if not supporting
  # }) 
  
  ## Fish Tissue PCBs
  
  output$FPBC_oneStationSelectionUI <- renderUI({req(stationSelectedAbove)
    selectInput(ns('FPBC_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  FPBC_oneStation <- reactive({req(ns(input$FPBC_oneStationSelection))
    filter(fishPCB, `DEQ rivermile` %in% input$FPBC_oneStationSelection)  })
  
  output$FPBCscreeningValues <- DT::renderDataTable({
    tibble(Description = c('DEQ screening value of 18 ppb', 'DEQ screening value of 20 ppb', 'VDH lower level of concern of 100 ppb', 'VDH upper level of concern of 500 ppb'),
           `Screening Value` = c(18.001, 20.001, 100.001, 500.001)) %>% # extra digits to force colors to come in correctly
      datatable(rownames = FALSE, options= list(scrollX = TRUE, pageLength = 4, dom='t')) %>%
      formatRound('Screening Value',digits = 0) %>%
      formatStyle(c('Description', 'Screening Value'), backgroundColor = styleInterval(c( 18, 20, 100, 500), c(NA, '#f2a972', '#c34eed', '#4a57e8', '#ed4242' )))})
  
  
  output$FPBCRangeTableSingleSite <- DT::renderDataTable({ req(input$FPBC_oneStationSelection, FPBC_oneStation())
    DT::datatable(FPBC_oneStation(), rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(FPBC_oneStation()), scrollY = "250px", dom='Bti', buttons=list('copy')),
                  selection = 'none')  %>%
      formatStyle('Parameter Rounded to WQS Format', backgroundColor = styleInterval(c( 18, 20, 100, 500), c(NA, '#f2a972', '#c34eed', '#4a57e8', '#ed4242' ))) }) 
  #formatStyle('Total PCBs', backgroundColor = styleInterval(c( 18, 20, 100, 500), c(NA, '#f2a972', '#c34eed', '#4a57e8', '#ed4242' )))   }) 
  
  
}

ui <- fluidPage(
  helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
  toxicsSingleStationUI('PBC')
)

server <- function(input,output,session){
  stationData <- eventReactive( input$stationSelection, {
    filter(AUData, FDT_STA_ID %in% input$stationSelection) })
  stationSelected <- reactive({input$stationSelection})
  
  AUData <- reactive({filter(conventionals_HUC, FDT_STA_ID %in% filter(stationTable, VAHU6 %in% 'JU21')$STATION_ID) %>% 
      left_join(WQSvalues, by = 'CLASS')})
  
  callModule(toxicsSingleStation,'PBC', AUData, markPCB, fishPCB, stationSelected)
  
}

shinyApp(ui,server)

