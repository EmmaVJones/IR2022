metalsTableSingleStationUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel('Water Column Metals',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('WCmetals_oneStationSelectionUI')),
                 tabsetPanel(
                   tabPanel('2022 IR Analyzed Data',
                            br(),
                            fluidRow(
                              column(2, numericInput(ns('WER'), strong('Water Effects Ratio (WER) used for module analysis'), value = 1)),
                              column(1),
                              column(9,  h5('A summary of the exceedances of water column metals criteria available for the ',span(strong('selected site')),' and user
                                            input WER are available below. If no data is presented, then the station does not have any water 
                                            column metals data available. Hardness based criteria are calculated as applicable.'),
                                     h5(strong("These analyses are still undergoing rigorous review. Please verify all assessment decisions 
                                            based off these results until further notice.")) ) ),
                            DT::dataTableOutput(ns('WCmetalsSingleSiteSummary')),
                            hr(), 
                            fluidRow(
                              column(7, helpText('Below are the calculated results associated with the ',span('selected site'),'. Click on a row to reveal the
                                                  data included in the selected criteria window in the plot to the right.'), 
                                     h5(strong('Analyzed Data')),DT::dataTableOutput(ns('analyzedData'))),
                              column(5, helpText('Click a row on the table to left to reveal a detailed interactive plot of the data
                                                 included in the selected criteria window. When criteria with static limits are selected, a 
                                                 black dashed line appears corresponding to the appropriate criteria. When criteria with
                                                 hardness based limits are selected, measured values appear as gray if they fall below the 
                                                 calculated criteria and red if they exceed the calculated criteria.'),
                                     #verbatimTextOutput(ns('test')),
                                     plotlyOutput(ns('plotlyZoom'))))),
                   tabPanel('2022 IR Raw Data',
                            h5('All water column metals data available for the ',span(strong('selected site')),' are available below. 
                               If no data is presented, then the station does not have any water column metals data available.'),
                            DT::dataTableOutput(ns('WCmetalsRangeTableSingleSite'))),
                   tabPanel('2020 IR Assessment',
                            h4('The 2020 IR data pulls included pre-analyzed metals data. The results of those analyses are presented below.'),
                            h5('All water column metals data available for the ',span(strong('selected site')),' are available below. 
                               If no data is presented, then the station does not have any water column metals data available.'),
                            DT::dataTableOutput(ns('IR2020WCmetalsRangeTableSingleSite')),br(), br(), br(),
                            h5('Metals assessments for the ',span(strong('selected site')),' are highlighted below.'),
                            DT::dataTableOutput(ns("IR2020WCstationmetalsExceedanceRate")))))),
      tabPanel('Sediment Metals',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('Smetals_oneStationSelectionUI')),
                 tabsetPanel(
                   tabPanel('2022 IR Raw Data',
                            h5('All sediment metals data available for the ',span(strong('selected site')),' are available below. 
                               If no data is presented, then the station does not have any sediment metals data available.'),
                            DT::dataTableOutput(ns('SmetalsRangeTableSingleSite'))),
                   tabPanel('2020 IR Assessment',
                            h5('All sediment metals data available for the ',span(strong('selected site')),' are available below. 
                               If no data is presented, then the station does not have any sediment metals data available.'),
                            DT::dataTableOutput(ns('IR2020SmetalsRangeTableSingleSite')),br(), br(), br(),
                            h5('Metals assessments for the ',span(strong('selected site')),' are highlighted below.'),
                            DT::dataTableOutput(ns("IR2020SstationmetalsExceedanceRate")))))),
      tabPanel('Fish Tissue Metals',
               wellPanel(
                 h4(strong('Single Station Data Visualization')),
                 uiOutput(ns('Fmetals_oneStationSelectionUI')),
                 #verbatimTextOutput(ns('test')),
                 h5('All fish tissue metals exceedances for the ',span(strong('selected site')),' are available below. 
                    If no data is presented, then the station does not have any fish tissue metals exceedances.'),
                 DT::dataTableOutput(ns('Fmetals_exceedance')),br(),
                 h5('All fish tissue metals data available for the ',span(strong('selected site')),' are available below. 
                    If no data is presented, then the station does not have any fish tissue metals data available.'),
                 helpText('All concentrations expressed as ppm (mg/kg), wet weight, in edible fish tissue fillet'),
                 DT::dataTableOutput(ns('FmetalsRangeTableSingleSite'))) )
      
    ))
}


metalsTableSingleStation <- function(input,output,session, AUdata, WCmetals , WCmetalsForAnalysis, IR2020WCmetals, Smetals, IR2020Smetals, Fmetals, 
                                     metalsSV, stationSelectedAbove, staticLimit){
  ns <- session$ns
  
  ## Water Column Metals
  
  # Select One station for individual review
  output$WCmetals_oneStationSelectionUI <- renderUI({    req(stationSelectedAbove)
    selectInput(ns('WCmetals_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  WCmetals_oneStation <- reactive({req(ns(input$WCmetals_oneStationSelection))
    filter(WCmetals, FDT_STA_ID %in% input$WCmetals_oneStationSelection)})
  
  WCmetals_oneStationForAnalysis <- reactive({req(ns(input$WCmetals_oneStationSelection))
    filter(WCmetalsForAnalysis, Station_Id %in% input$WCmetals_oneStationSelection)})
  
  
  # Calculate based on user input
  WCmetals_oneStationAnalysis <- reactive({req(nrow(WCmetals_oneStationForAnalysis()) > 0, input$WER)
    metalsAnalysis(WCmetals_oneStationForAnalysis(), AUdata(), WER= input$WER)    })
  
  WCmetals_oneStationAssessment <- reactive({req(nrow(WCmetals_oneStationAnalysis()) > 0)
    metalsAssessmentFunction(WCmetals_oneStationAnalysis())})
  
  output$WCmetalsSingleSiteSummary <- DT::renderDataTable({req(WCmetals_oneStationAssessment())
    DT::datatable(WCmetals_oneStationAssessment(), rownames = FALSE,extensions = 'Buttons',
                  options= list(scrollX = TRUE, pageLength = nrow(WCmetals_oneStationAssessment()), scrollY = "80px", dom='Bt',
                                buttons=list('copy',
                                             list(extend='csv',filename=paste('WCmetalsSummary_',paste(assessmentCycle,input$WCmetals_oneStationSelection, collapse = "_"),Sys.Date(),sep='')),
                                             list(extend='excel',filename=paste('WCmetalsSummary_',paste(assessmentCycle,input$WCmetals_oneStationSelection, collapse = "_"),Sys.Date(),sep='')))),
                  selection = 'none')     })
  
  # Zoomed plot section
  output$analyzedData <- DT::renderDataTable({req(WCmetals_oneStationAnalysis())
    DT::datatable(WCmetals_oneStationAnalysis(), rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(WCmetals_oneStationAnalysis()), 
                                                                                 scrollY = "300px", dom='ti'),
                  selection = 'single') })
  
  
  output$plotlyZoom <- renderPlotly({ req(input$analyzedData_rows_selected, WCmetals_oneStationAnalysis())
    criteriaSelection <- WCmetals_oneStationAnalysis()[input$analyzedData_rows_selected, ]$Criteria 
    dat <- filter(WCmetals_oneStationAnalysis(), Criteria ==  criteriaSelection)
    dat$SampleDate <- as.POSIXct(dat$WindowDateTimeStart, format="%m/%d/%y")
    
    plot_ly(data=dat) %>%
      {if(criteriaSelection %in% staticLimit)
        add_markers(., x= ~SampleDate, y= ~Value,mode = 'scatter', name=~Metal, marker = list(color= '#535559'),
                    hoverinfo="text",text=~paste(sep="<br>",
                                                 paste("StationID: ",Station_Id),
                                                 paste("Date: ",SampleDate),
                                                 paste("Depth: ",FDT_DEPTH, "m"),
                                                 paste(Metal,":",Value, "ug/L"),
                                                 paste('Static Criteria:', CriteriaValue, "ug/L"))) %>%
          add_lines(data=dat, x=~SampleDate,y=~CriteriaValue, mode='line', line = list(color = '#484a4c',dash = 'dot'),
                    hoverinfo = "text", text= ~paste(criteriaSelection, "Criteria:",  CriteriaValue, "ug/L"), name="Static Criteria") 
        else add_markers(., data=dat, x= ~SampleDate, y= ~Value, mode = 'scatter', name=~Metal, marker = list(color= ~Exceedance), colors = c('#535559', 'red'), #color= ~Exceedance, #colors = c('#535559', 'red'),#marker = list(color= '#535559'),
                         symbol =  ~Exceedance, symbols = c(16,15), 
                         hoverinfo="text",text=~paste(sep="<br>",
                                                      paste("StationID: ",Station_Id),
                                                      paste("Date: ",SampleDate),
                                                      paste("Depth: ",FDT_DEPTH, "m"),
                                                      paste(Metal,":",Value, "ug/L"),
                                                      paste('Hardness Based Criteria:', CriteriaValue, "ug/L")))       } %>%
      layout(showlegend=FALSE,
             yaxis=list(title=paste(stringr::word(criteriaSelection, 1), "ug/L")),
             xaxis=list(title="Sample Date",tickfont = list(size = 10))) })
  
  
  
  #output$test <- renderPrint({WCmetals_oneStationAnalysis()[input$analyzedData_rows_selected, ]$Criteria    })
  
  
  
  
  # Raw data
  output$WCmetalsRangeTableSingleSite <- DT::renderDataTable({req(WCmetals_oneStation())
    z <- WCmetals_oneStation()
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE,extensions = 'Buttons',
                  options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='Bt',
                                buttons=list('copy',
                                             list(extend='csv',filename=paste('WCmetalsRaw_',paste(assessmentCycle,input$WCmetals_oneStationSelection, collapse = "_"),Sys.Date(),sep='')),
                                             list(extend='excel',filename=paste('WCmetalsRaw_',paste(assessmentCycle,input$WCmetals_oneStationSelection, collapse = "_"),Sys.Date(),sep='')))),
                  selection = 'none')     })
  
  
  
  
  output$IR2020WCmetalsRangeTableSingleSite <- DT::renderDataTable({req(input$WCmetals_oneStationSelection)
    z <- filter(IR2020WCmetals, FDT_STA_ID %in% input$WCmetals_oneStationSelection) %>%
      dplyr::select( FDT_STA_ID:HARDNESS)
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')     })
  
  output$IR2020WCstationmetalsExceedanceRate <- DT::renderDataTable({ req(input$WCmetals_oneStationSelection)
    z <-  filter(IR2020WCmetals, FDT_STA_ID %in% input$WCmetals_oneStationSelection) %>%
      dplyr::select(FDT_STA_ID, `FDT_DATE_TIME`,`ANTIMONY HUMAN HEALTH PWS`:`ZINC ALL OTHER SURFACE WATERS`)
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none') %>%
      formatStyle(names(z), backgroundColor = styleEqual(c('NSP'), c('red'))) # highlight cells red if not supporting
  })
  
  
  
  ## Sediment Metals
  
  # Select One station for individual review
  output$Smetals_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('Smetals_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  Smetals_oneStation <- reactive({
    req(ns(input$Smetals_oneStationSelection))
    filter(Smetals, Station_Id %in% input$Smetals_oneStationSelection)})
  
  output$SmetalsRangeTableSingleSite <- DT::renderDataTable({req(Smetals_oneStation())
    z <- Smetals_oneStation()
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')     })
  
  
  
  
  output$IR2020SmetalsRangeTableSingleSite <- DT::renderDataTable({req(input$Smetals_oneStationSelection)
    z <- filter(IR2020Smetals, FDT_STA_ID %in% input$Smetals_oneStationSelection) %>%
      dplyr::select(FDT_STA_ID, FDT_DATE_TIME:ENDRINT)
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')     })
  
  
  output$IR2020SmetalsRangeTableSingleSite <- DT::renderDataTable({req(input$Smetals_oneStationSelection)
    z <- filter(IR2020Smetals, FDT_STA_ID %in% input$Smetals_oneStationSelection) %>%
      dplyr::select( FDT_STA_ID, FDT_DATE_TIME:ENDRINT)
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none')     })
  
  output$IR2020SstationmetalsExceedanceRate <- DT::renderDataTable({
    req(input$Smetals_oneStationSelection, Smetals_oneStation())
    z <- filter(IR2020Smetals, FDT_STA_ID %in% input$Smetals_oneStationSelection) %>%
      dplyr::select(FDT_STA_ID, `FDT_DATE_TIME`,ARSENIC:COMMENT)
    z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(z, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(z), scrollY = "250px", dom='t'),
                  selection = 'none') %>%
      formatStyle(names(z), backgroundColor = styleEqual(c('OE'), c('red'))) # highlight cells red if not supporting
  })
  
  ## Fish Tissue Metals
  
  output$Fmetals_oneStationSelectionUI <- renderUI({
    req(stationSelectedAbove)
    selectInput(ns('Fmetals_oneStationSelection'),strong('Select Station to Review'),choices= sort(unique(c(stationSelectedAbove(),AUdata()$FDT_STA_ID))),#unique(AUdata())$FDT_STA_ID,
                width='300px', selected = stationSelectedAbove())})# "2-JMS279.41" )})
  
  
  Fmetals_oneStation <- reactive({req(ns(input$Fmetals_oneStationSelection))
    filter(Fmetals, Station_ID %in% input$Fmetals_oneStationSelection)})
  
  output$Fmetals_exceedance <- DT::renderDataTable({req(Fmetals_oneStation())
    FmetalsSV <- dplyr::select(Fmetals_oneStation(), Station_ID, Collection_Date_Time, Sample_ID,  `# of Fish`, Species_Name, length, weight, Beryllium:Lead) %>%
      dplyr::select(-contains('RMK_')) %>%
      group_by( Station_ID, Collection_Date_Time, Sample_ID, `# of Fish`, Species_Name, length, weight) %>%
      pivot_longer(cols= Beryllium:Lead, names_to = "Metal", values_to = 'Measure') %>%
      left_join(metalsSV, by = 'Metal') %>%
      filter(Measure > `Screening Value`) %>%
      arrange(Metal)
    DT::datatable(FmetalsSV, rownames = FALSE, options= list(scrollX = TRUE, pageLength = nrow(FmetalsSV),
                                                             scrollY = "250px", dom='Bti', buttons=list('copy')), selection = 'none') })
  
  
  output$FmetalsRangeTableSingleSite <- DT::renderDataTable({ req(input$Fmetals_oneStationSelection, Fmetals_oneStation())
    # z <- dplyr::select(Smetals_oneStation(), FDT_STA_ID, `FDT_DATE_TIME`,ARSENIC:COMMENT)
    # z$FDT_DATE_TIME <- as.character(as.POSIXct(z$FDT_DATE_TIME, format="%m/%d/%Y %H:%M"))
    DT::datatable(Fmetals_oneStation(), rownames = FALSE,
                  options= list(scrollX = TRUE, pageLength = nrow(Fmetals_oneStation()), scrollY = "250px", dom='Bti', buttons=list('copy')),
                  selection = 'none') #%>%
    #formatStyle(names(z), backgroundColor = styleEqual(c('OE'), c('red'))) # highlight cells red if not supporting
  })
}
