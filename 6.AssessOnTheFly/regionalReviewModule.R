
regionalReviewMapUI <- function(id){
  ns <- NS(id)
  tagList(
    #verbatimTextOutput(ns('test')),
    leafletOutput(ns('regionalMap')),
    h5('Click on a station above to view parameter information in table below.'),
    dataTableOutput(ns('stationTableParameterSummary')),
    br(), br(),br())
}


regionalReviewMap <- function(input,output,session, assessmentSummary, parameterChoice, stationTableResults){
  ns <- session$ns
  
  #output$test <- renderPrint({assessmentSummary()})
  
  ### Regional Map Tab
  output$regionalMap <- renderLeaflet({req(assessmentSummary(), parameterChoice())
    indStatusMap(parameterChoice(), assessmentSummary())})
  
  output$stationTableParameterSummary <- renderDataTable({ req(input$regionalMap_marker_click)
    z <- filter(stationTableResults(), STATION_ID %in% input$regionalMap_marker_click$id) %>% 
      #filter_at(vars(ends_with("_EXC")), any_vars( . > 0)) %>% 
      dplyr::select(STATION_ID, Sta_Desc, TEMP_EXC:LONGITUDE, everything()) 
    
    datatable(z, rownames = F, escape= F, extensions = c('Buttons','FixedColumns'),
              options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',selection = 'none',
                             pageLength = nrow(z), fixedColumns = list(leftColumns = 1),
                             buttons=list('copy',
                                          list(extend='excel',filename=paste0('StationSummary',input$regionChoice)))) ) %>% 
      formatStyle(c('TEMP_EXC','TEMP_SAMP'), 'TEMP_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow')))  %>%
      formatStyle(c('DO_EXC','DO_SAMP'), 'DO_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow')))  %>%
      formatStyle(c('PH_EXC','PH_SAMP'), 'PH_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow')))  %>%
      formatStyle(c('ECOLI_EXC','ECOLI_SAMP'), 'ECOLI_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow')))  %>%
      formatStyle(c('ECOLI_GM_EXC','ECOLI_GM_SAMP'), 'ECOLI_GM_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow'))) %>%
      formatStyle(c('ENTER_SAMP','ENTER_EXC'), 'ENTER_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow')))  %>%
      formatStyle(c("ENTER_GM_EXC","ENTER_GM_SAMP"), 'ENTER_GM_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow'))) %>%
      formatStyle(c('AMMONIA_EXC'), 'AMMONIA_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow'))) %>%
      formatStyle(c('NUT_TP_EXC','NUT_TP_SAMP'), 'NUT_TP_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow'))) %>% 
      formatStyle(c('NUT_CHLA_EXC','NUT_CHLA_SAMP'), 'NUT_CHLA_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow'))) })
}



regionalReviewStationStatusUI <- function(id){
  ns <- NS(id)
  tagList(
    h4('Preliminary Station Status Results'),
    helpText('The table below presents preliminary station status results for all stations monitored in the query window for the selected region.'),
    dataTableOutput(ns('stationTable')),
    br(), hr(),br(),
    h4('Stations with Exceedances'),
    helpText('Below is a summary table of all monitored stations with exceedances. Exceeding parameters are highlighted yellow 
              if one parameter exceedance is noted while parameters projected to surpass the the 10.5% rule are highlighted in red.'),
    dataTableOutput(ns('exceedanceTable')),
    br(), br(),br())
}

regionalReviewStationStatus <-  function(input,output,session, stationTableResults, conventionals){
  ns <- session$ns
  
  ### Exceedance Summary Tab
  output$stationTable <- renderDataTable({req(stationTableResults())
    z <- stationTableResults() %>% 
      left_join(conventionals() %>% 
                  group_by(FDT_STA_ID) %>% 
                  summarise(SPGsummary = paste0(unique(FDT_SPG_CODE, collapse = ' | '))) %>% 
                  summarise(SPGsummary = paste0(SPGsummary, collapse = ' | ')),
                by = c('STATION_ID' = 'FDT_STA_ID')) %>% 
      dplyr::select(STATION_ID, Sta_Desc, SPGsummary, TEMP_EXC:LONGITUDE, everything()) 
    
    #dplyr::select(stationTableResults(), STATION_ID, Sta_Desc, TEMP_EXC:LONGITUDE, everything())
    datatable(z, rownames = F, escape= F, extensions = c('Buttons','FixedColumns'),
              options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',selection = 'none',
                             pageLength = nrow(z), fixedColumns = list(leftColumns = 1),
                             buttons=list('copy',
                                          list(extend='excel',filename=paste0('PreliminaryAssessmentResults',input$regionChoice)))) ) })
  
  output$exceedanceTable <- renderDataTable({req(stationTableResults())
    z <- filter_at(stationTableResults(), vars(ends_with("_EXC")), any_vars( . > 0)) %>% 
      dplyr::select(STATION_ID, Sta_Desc, TEMP_EXC:LONGITUDE, everything()) 
    
    datatable(z, rownames = F, escape= F, extensions = c('Buttons','FixedColumns'),
              options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',selection = 'none',
                             pageLength = nrow(z), fixedColumns = list(leftColumns = 1),
                             buttons=list('copy',
                                          list(extend='excel',filename=paste0('StationSummary',input$regionChoice)))) ) %>% 
      formatStyle(c('TEMP_EXC','TEMP_SAMP'), 'TEMP_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow')))  %>%
      formatStyle(c('DO_EXC','DO_SAMP'), 'DO_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow')))  %>%
      formatStyle(c('PH_EXC','PH_SAMP'), 'PH_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow')))  %>%
      formatStyle(c('ECOLI_EXC','ECOLI_SAMP'), 'ECOLI_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow')))  %>%
      formatStyle(c('ECOLI_GM_EXC','ECOLI_GM_SAMP'), 'ECOLI_GM_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow'))) %>%
      formatStyle(c('ENTER_SAMP','ENTER_EXC'), 'ENTER_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow')))  %>%
      formatStyle(c("ENTER_GM_EXC","ENTER_GM_SAMP"), 'ENTER_GM_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow'))) %>%
      formatStyle(c('AMMONIA_EXC'), 'AMMONIA_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow'))) %>%
      formatStyle(c('NUT_TP_EXC','NUT_TP_SAMP'), 'NUT_TP_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow'))) %>% 
      formatStyle(c('NUT_CHLA_EXC','NUT_CHLA_SAMP'), 'NUT_CHLA_EXC', backgroundColor = styleInterval(0, c(NA, 'yellow'))) })
  
}
  



regionalReviewMonitoringSummaryUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(6,h4('Regional Monitoring Runs By Collector'),
                                                   dataTableOutput(ns('runSummaryTable')), br(),br(),br()),
                                            column(6, radioButtons(ns('monScheduleBy'), h4('Regional Monitoring Run Schedule'),
                                                                   choices = c('Run ID', 'StationID'), inline = T),
                                                   dataTableOutput(ns('monSchedule')))),
                                   hr(),
                                   fluidRow(column(6,
                                                   fluidRow(column(6,h4('Detailed Monitoring Run Breakdown')),
                                                            column(6,uiOutput(ns('monitorSelection_')))),
                                                   dataTableOutput(ns('monitorSummaryTable')), br(),br(),br()),
                                            column(6,
                                                   helpText('Below is a heatmap of stations monitored by the selected monitoring staff member.'),
                                                   leafletOutput(ns('monitorMap')))),
                                   hr(),
                                   fluidRow(column(6,
                                                   h4('QA Sample Breakdown'),
                                                   dataTableOutput(ns('QAbreakdown')) ),
                                            column(6,
                                                   h4('QA Samples'),
                                                   dataTableOutput(ns('QAsamples')) ))
  )
}

regionalReviewMonitoringSummary <-  function(input,output,session, runSummary, stationFieldData, stationGIS_View, assessmentLayer){
  ns <- session$ns
  
  ### Monitoring Summary Tab
  
  output$runSummaryTable <- renderDataTable({req(runSummary())
    z <- runSummary() %>% 
      #filter(Fdt_Collector_Id == 'RJS' & Fdt_Run_Id %in% c('W21A1','W21A2', 'W21D1')) %>% 
      group_by(Fdt_Collector_Id) %>% 
      mutate(`Station Count` = `Stations Per Run` * `Times Completed`) %>% 
      summarise(`Unique Runs Completed` = length(unique(Fdt_Run_Id)),
                `Total Runs Completed` = sum(`Times Completed`),
                `Unique Stations Monitored` = sum(`Stations Per Run`),
                `Total Stations Monitored` = sum(`Station Count`))
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',selection = 'none',
                             pageLength = nrow(z),
                             buttons=list('copy',
                                          list(extend='excel',filename=paste0('RunSummary',input$regionChoice)))) ) })
  
  
  output$monSchedule <-  renderDataTable({req(stationFieldData(), input$monScheduleBy)
    z <- monthlyBreakdown(stationFieldData(), input$monScheduleBy)
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',selection = 'none',
                             pageLength = nrow(z),
                             buttons=list('copy',
                                          list(extend='excel',filename=paste0('MonitoringScheduleSummary_',input$monScheduleBy)))) ) })
  
  
  output$monitorSelection_ <- renderUI({req(runSummary())
    selectInput(ns('monitorSelection'), 'Monitor Details', choices = sort(unique(runSummary()$Fdt_Collector_Id)))})
  
  output$monitorSummaryTable <- renderDataTable({req(runSummary, input$monitorSelection)
    z <- filter(runSummary(), Fdt_Collector_Id %in% input$monitorSelection)
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX = TRUE, scrollY = '310px',selection = 'none',
                             pageLength = nrow(z),
                             buttons=list('copy',
                                          list(extend='excel',filename=paste0('MonitoringSummary',input$monitorSelection)))) )})
  
  
  output$monitorMap <- renderLeaflet({req(input$monitorSelection, stationFieldData())
    collectorHeatmap(stationFieldData(), stationGIS_View(),
                     assessmentLayer(), collectorID = input$monitorSelection)    })
  
  
  
  # QA breakdown
  output$QAbreakdown <-  renderDataTable({req(stationFieldData())
    z <- left_join(
      stationFieldData() %>% 
        distinct(Fdt_Collector_Id) %>% 
        arrange(Fdt_Collector_Id),
      stationFieldData() %>% 
        filter(Fdt_Spg_Code == 'QA') %>% 
        group_by(Fdt_Collector_Id) %>% 
        summarise(`QA Samples` = length(unique(Fdt_Date_Time))) %>% 
        left_join(
          stationFieldData() %>% 
            group_by(Fdt_Collector_Id) %>% 
            summarise(`Total Samples` = length(unique(Fdt_Date_Time))),
          by = 'Fdt_Collector_Id'  ) %>% 
        mutate(`QA Sample Percentage` = format(`QA Samples` / `Total Samples` * 100, digits = 2)),
      by = 'Fdt_Collector_Id'  ) 
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX = TRUE, scrollY = '310px',selection = 'none',
                             pageLength = nrow(z),
                             buttons=list('copy',
                                          list(extend='excel',filename=paste0('QASummary')))) )})
  
  output$QAsamples <-  renderDataTable({req(stationFieldData())
    z <- filter(stationFieldData(), Fdt_Spg_Code == 'QA') %>% 
      dplyr::select(Fdt_Run_Id, Fdt_Collector_Id, Fdt_Sta_Id:Spg_Description, Fdt_Date_Time, Fdt_Depth, Fdt_Temp_Celcius:Fdt_Specific_Conductance)
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX = TRUE, scrollY = '310px',selection = 'none',
                             pageLength = nrow(z),
                             buttons=list('copy',
                                          list(extend='excel',filename=paste0('QASamples')))) )})
}



#                         column(4, uiOutput(ns('parameterChoice_')),
#                         column(4,
#                                conditionalPanel(condition = "input.parameterChoice == 'Total Phosphorus'",
#                                                 helpText('For non-lake stations, a Total Phosphorus threshold of 0.2 mg/L was used as an exceedance flag to indicate the potential
#                                                                     for nutrient problems.')),
#                                conditionalPanel(condition = "input.parameterChoice == 'Chlorophyll a'",
#                                                 helpText('Chlorophyll a analyses are only performed on lake stations.')))),
#                leafletOutput('regionalMap'),
#                h5('Click on a station above to view parameter information in table below.'),
#                dataTableOutput('stationTableParameterSummary'),
#                br(), br(),br()),
#       tabPanel('Exceedance Summary',
#                h4('Preliminary Station Status Results'),
#                helpText('The table below presents preliminary station summary results for all stations monitored in the query window for the selected region.'),
#                dataTableOutput('stationTable'),
#                br(), hr(),br(),
#                h4('Stations with Exceedances'),
#                helpText('Below is a summary table of all monitored stations with exceedances. Exceeding parameters are highlighted yellow if one parameter exceedance is noted 
#                                           while parameters exceeding the 10.5% rule are highlighted in red.'),
#                dataTableOutput('exceedanceTable'),
#                br(), br(),br()),
#       tabPanel('Monitoring Summary',
#                fluidRow(column(6,h4('Regional Monitoring Runs By Collector'),
#                                dataTableOutput('runSummaryTable'), br(),br(),br()),
#                         column(6, radioButtons('monScheduleBy', h4('Regional Monitoring Run Schedule'), 
#                                                choices = c('Run ID', 'StationID'), inline = T),
#                                dataTableOutput('monSchedule'))),
#                hr(),
#                fluidRow(column(6, 
#                                fluidRow(column(6,h4('Detailed Monitoring Run Breakdown')),
#                                         column(6,uiOutput('monitorSelection_'))),
#                                dataTableOutput('monitorSummaryTable'), br(),br(),br()),
#                         column(6,
#                                helpText('Below is a heatmap of stations monitored by the selected monitoring staff member.'),
#                                leafletOutput('monitorMap'))),
#                hr(),
#                fluidRow(column(6,
#                                h4('QA Sample Breakdown'),
#                                dataTableOutput('QAbreakdown') ),
#                         column(6, 
#                                h4('QA Samples'),
#                                dataTableOutput('QAsamples') ))
#       )
#     ))
#   )
# }

# regionalReview <- function(input,output,session, parameterEXCcrosswalk){
#   ns <- session$ns
# 
#   # output$parameterChoice_ <- renderUI({
#   #   list(
#   #   # column(4, selectizeInput(ns('parameterChoice'), 'Choose a parameter to visualize regional exceedances.',
#   #   #                          choices = c('Overall Status',
#   #   #                                      parameterEXCcrosswalk$Parameter[!parameterEXCcrosswalk$Parameter %in% c('Water Column Metals', 'Water Column Toxics', 'Sediment Metals', 
#   #   #                                                                                                              'Sediment Toxics', 'Fish Tissue Metals', 'Fish Tissue Toxics',
#   #   #                                                                                                              'Benthics')]))),
#   #   column(4,
#   #          conditionalPanel(condition = "input['id1-parameterChoice'] == 'Total Phosphorus'",
#   #                           helpText('For non-lake stations, a Total Phosphorus threshold of 0.2 mg/L was used as an exceedance flag to indicate the potential
#   #                                                                   for nutrient problems.')),
#   #          conditionalPanel(condition = "input.parameterChoice == 'Chlorophyll a'",
#   #                           helpText('Chlorophyll a analyses are only performed on lake stations.')))) })
# 
#   
# }


