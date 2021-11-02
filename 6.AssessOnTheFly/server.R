#source('global.R')

assessmentLayer <- st_read('data/GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326))

shinyServer(function(input, output, session) {
  
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
 # Subset statewideResults by chosen Region
  
  observeEvent(input$runData, {
    reactive_objects$regionResults <- statewideResults[[input$regionChoice]]    })
  
  observe({req(reactive_objects$regionResults)
    reactive_objects$stationTableResults <- left_join(reactive_objects$regionResults$`Assessment Results`$stationTableResults,
                                                      dplyr::select(reactive_objects$regionResults$stationGIS_View,
                                                                    STATION_ID = Station_Id, LATITUDE = Latitude, LONGITUDE = Longitude),
                                                      by = 'STATION_ID')
    reactive_objects$runSummary <- summarizeRuns(reactive_objects$regionResults$stationFieldData)
    reactive_objects$assessmentSummary <- stationSummary(reactive_objects$stationTableResults, parameterEXCcrosswalk)  })
  
  
  # Display date ranges for each tab
  output$oneYearDateRange <- renderUI({req(reactive_objects$regionResults)
    fluidRow(h3('This tab presents data from ', paste0('01-01-', year(min(reactive_objects$regionResults$stationFieldData$Fdt_Date_Time)), ' to ',
           format(statewideResults$BRRO$pullDate, "%m-%d-%Y"))))    })
  
  # output$twoYearDateRange <- renderUI({req(reactive_objects$regionResults)
  #   fluidRow(h3('This tab presents data from ', paste0('01-01-', year(min(reactive_objects$regionResults$stationFieldData$Fdt_Date_Time)), ' to ',
  #                                                      format(statewideResults$BRRO$pullDate, "%m-%d-%Y"))))    })
  
  
  
  ### Regional Map Tab
  output$regionalMap <- renderLeaflet({req(reactive_objects$assessmentSummary, input$parameterChoice)
    indStatusMap(input$parameterChoice, reactive_objects$assessmentSummary)})
  
  #output$test <- renderPrint({dplyr::select(reactive_objects$stationTableResults, STATION_ID, Sta_Desc, TEMP_EXC:LONGITUDE, everything())  })
  
  
  # add in click information from map to subset station table results in table here
  
  
    
  ### Exceedance Summary Tab
  output$stationTable <- renderDataTable({req(reactive_objects$stationTableResults)
    z <- reactive_objects$stationTableResults %>% 
      left_join(reactive_objects$regionResults$Conventionals %>% 
                  group_by(FDT_STA_ID) %>% 
                  summarise(SPGsummary = paste0(unique(FDT_SPG_CODE, collapse = ' | '))) %>% 
                  summarise(SPGsummary = paste0(SPGsummary, collapse = ' | ')),
                by = c('STATION_ID' = 'FDT_STA_ID')) %>% 
      dplyr::select(STATION_ID, Sta_Desc, SPGsummary, TEMP_EXC:LONGITUDE, everything())
      #dplyr::select(reactive_objects$stationTableResults, STATION_ID, Sta_Desc, TEMP_EXC:LONGITUDE, everything())
    datatable(z, rownames = F, escape= F, extensions = c('Buttons','FixedColumns'),
              options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',selection = 'none',
                             pageLength = nrow(z), fixedColumns = list(leftColumns = 1),
                             buttons=list('copy',
                                          list(extend='excel',filename=paste0('PreliminaryAssessmentResults',input$regionChoice)))) ) })
  
  output$exceedanceTable <- renderDataTable({req(reactive_objects$stationTableResults)
    z <- filter_at(reactive_objects$stationTableResults, vars(ends_with("_EXC")), any_vars( . > 0)) %>% 
      dplyr::select(STATION_ID, Sta_Desc, TEMP_EXC:LONGITUDE, everything())
    datatable(z, rownames = F, escape= F, extensions = c('Buttons','FixedColumns'),
              options = list(dom = 'Bift', scrollX = TRUE, scrollY = '350px',selection = 'none',
                             pageLength = nrow(z), fixedColumns = list(leftColumns = 1),
                             buttons=list('copy',
                                          list(extend='excel',filename=paste0('ExceedanceSummary',input$regionChoice)))) ) %>% 
      formatStyle(c('TEMP_EXC','TEMP_SAMP','TEMP_STAT'), 'TEMP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('DO_EXC','DO_SAMP','DO_STAT'), 'DO_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('PH_EXC','PH_SAMP','PH_STAT'), 'PH_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('ECOLI_EXC','ECOLI_SAMP','ECOLI_GM_EXC','ECOLI_GM_SAMP','ECOLI_STAT'), 'ECOLI_STAT', backgroundColor = styleEqual(c('IM'), c('red'))) %>%
      formatStyle(c('ENTER_SAMP','ENTER_EXC',"ENTER_GM_EXC","ENTER_GM_SAMP",'ENTER_STAT'), 'ENTER_STAT', backgroundColor = styleEqual(c('IM'), c('red'))) %>%
      formatStyle(c('AMMONIA_EXC','AMMONIA_STAT'), 'AMMONIA_STAT', backgroundColor = styleEqual(c('Review', 'IM'), c('yellow','red'))) %>%
      formatStyle(c('NUT_TP_EXC','NUT_TP_SAMP'), 'NUT_TP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>% 
      formatStyle(c('NUT_CHLA_EXC','NUT_CHLA_SAMP'), 'NUT_CHLA_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) })

  
  ### Monitoring Summary Tab
  
  output$runSummaryTable <- renderDataTable({req(reactive_objects$runSummary)
    z <- reactive_objects$runSummary %>% 
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
  
  
  output$monSchedule <-  renderDataTable({req(reactive_objects$regionResults, input$monScheduleBy)
    z <- monthlyBreakdown(reactive_objects$regionResults$stationFieldData, input$monScheduleBy)
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX = TRUE, scrollY = '350px',selection = 'none',
                             pageLength = nrow(z),
                             buttons=list('copy',
                                          list(extend='excel',filename=paste0('MonitoringScheduleSummary_',input$monScheduleBy)))) ) })
  
  
  output$monitorSelection_ <- renderUI({req(reactive_objects$runSummary)
    selectInput('monitorSelection', 'Monitor Details', choices = sort(unique(reactive_objects$runSummary$Fdt_Collector_Id)))})
  
  output$monitorSummaryTable <- renderDataTable({req(reactive_objects$runSummary, input$monitorSelection)
    z <- filter(reactive_objects$runSummary, Fdt_Collector_Id %in% input$monitorSelection)
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX = TRUE, scrollY = '310px',selection = 'none',
                             pageLength = nrow(z),
                             buttons=list('copy',
                                          list(extend='excel',filename=paste0('MonitoringSummary',input$monitorSelection)))) )})
 
  
  output$monitorMap <- renderLeaflet({req(input$monitorSelection, reactive_objects$regionResults$stationFieldData)
    collectorHeatmap(reactive_objects$regionResults$stationFieldData, reactive_objects$regionResults$stationGIS_View,
                     assessmentLayer, collectorID = input$monitorSelection)    })
  
  
  
  # QA breakdown
  output$QAbreakdown <-  renderDataTable({req(reactive_objects$regionResults)
    z <- left_join(
      reactive_objects$regionResults$stationFieldData %>% 
        distinct(Fdt_Collector_Id) %>% 
        arrange(Fdt_Collector_Id),
      reactive_objects$regionResults$stationFieldData %>% 
        filter(Fdt_Spg_Code == 'QA') %>% 
        group_by(Fdt_Collector_Id) %>% 
        summarise(`QA Samples` = length(unique(Fdt_Date_Time))) %>% 
        left_join(
          reactive_objects$regionResults$stationFieldData %>% 
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
  
  output$QAsamples <-  renderDataTable({req(reactive_objects$regionResults)
    z <- filter(reactive_objects$regionResults$stationFieldData, Fdt_Spg_Code == 'QA') %>% 
      dplyr::select(Fdt_Run_Id, Fdt_Collector_Id, Fdt_Sta_Id:Spg_Description, Fdt_Date_Time, Fdt_Depth, Fdt_Temp_Celcius:Fdt_Specific_Conductance)
    datatable(z, rownames = F, escape= F, extensions = 'Buttons',
              options = list(dom = 'Bift', scrollX = TRUE, scrollY = '310px',selection = 'none',
                             pageLength = nrow(z),
                             buttons=list('copy',
                                          list(extend='excel',filename=paste0('QASamples')))) )})
})