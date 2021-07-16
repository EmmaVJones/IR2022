
assessmentUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      p(span('This area of the application allows users to assess water quality monitoring data on the fly based on
                                          previous station and date window selections. CEDS does not contain all data necessary to fully assess
                                          a station and potential date ranges and stations selected may not meet necessary assessment assumptions, so this tool
                                          is considered a ', strong('PRELIMINARY ASSESSMENT'), 'and is ', strong('NOT VALID AS AN OFFICIAL ASSESSMENT DECISION'),
             '; however, understanding how station(s) compare to relevant standards/criteria is useful for monitoring planning
                                          and QA purposes.')),
      helpText('To perform an assessment on the selected stations and date range, first verify the station information on the 
                                            `Assessment Station Metadata` tab. Please complete the lake designations (if applicable) information
                                            to appropriately assess the chosen data. When the metadata is complete, press the `Assess` button and navigate
                                            to the `Results` tab to view station summary.')),
    mainPanel(
      tabsetPanel(
        tabPanel('Assessment Station Metadata',
                 # modFunctionUI(ns("editable")),
                 DT::dataTableOutput(ns('stationReview')),
                 br(),
                 fluidRow(column(4,wellPanel(uiOutput(ns('lakeStationSelection_')) )),
                          column(4,wellPanel(uiOutput(ns('lacustrineZoneSelection_')) ) ),
                          column(4, wellPanel(h5('Once metadata verification is complete, click the button below to begin rapid assessment.'),
                                              actionButton(ns('rapidAssessmentRun'), 'Assess', class='btn-block'))))),
        tabPanel('Results',
                 DT::dataTableOutput(ns('stationTableResults')),
                 verbatimTextOutput(ns('test'))),
        tabPanel("Conventionals Dataset",
                 helpText("Below is the conventionals dataset for the chosen stations and data window."),
                 DT::dataTableOutput(ns('conventionalsDataset'))  ) 
      ))) }
      

assessment <- function(input,output,session, multistationFieldDataUserFilter, multistationAnalyteDataUserFilter, WQM_Stations_Filter, multistationSelection, VSCIresults){
  ns <- session$ns
  
  modal_reactive <- reactiveValues()
  
  multistationInfo <- reactive({req(nrow(multistationFieldDataUserFilter()) > 0)
    pool %>% tbl(in_schema("wqm",  "Wqm_Stations_View")) %>%
      filter(Sta_Id %in% !! WQM_Stations_Filter()$StationID) %>%
      as_tibble() })
  
  multistationGIS_View <- reactive({req(nrow(multistationFieldDataUserFilter()) > 0)
    pool %>% tbl(in_schema("wqm",  "Wqm_Sta_GIS_View")) %>%
      filter(Station_Id %in% !! WQM_Stations_Filter()$StationID) %>%
      as_tibble() })
  
  
  ## Assessment Station Metadata Tab
  
  stationTable <- reactive({req(nrow(multistationFieldDataUserFilter()) > 0)
      left_join(tibble(STATION_ID = unique(multistationSelection()$Sta_Id)),
                                               WQSlookup, by = c('STATION_ID'='StationID')) %>%
      mutate(CLASS_BASIN = paste(CLASS,substr(BASIN, 1,1), sep="_")) %>%
      mutate(CLASS_BASIN = ifelse(CLASS_BASIN == 'II_7', "II_7", as.character(CLASS))) %>%
      # Fix for Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard)
      left_join(WQSvalues, by = 'CLASS_BASIN') %>%
      dplyr::select(-c(CLASS.y,CLASS_BASIN)) %>%
      rename('CLASS' = 'CLASS.x') %>%
      left_join(WQMstationSpatial %>% distinct(StationID, .keep_all = TRUE), by = c('STATION_ID' = 'StationID')) %>%
      # last cycle had code to fix Class II Tidal Waters in Chesapeake (bc complicated DO/temp/etc standard) but not sure if necessary
      lakeNameStandardization() %>% # standardize lake names


      # extra special step
      mutate(Lake_Name = case_when(STATION_ID %in% c('2-TRH000.40') ~ 'Thrashers Creek Reservoir',
                                   STATION_ID %in% c('2-LSL000.16') ~ 'Lone Star Lake F (Crystal Lake)',
                                   STATION_ID %in% c('2-LSL000.04') ~ 'Lone Star Lake G (Crane Lake)',
                                   STATION_ID %in% c('2-LSL000.20') ~ 'Lone Star Lake I (Butler Lake)',
                                   STATION_ID %in% c('2-NWB002.93','2-NWB004.67', '2-NWB006.06') ~ 'Western Branch Reservoir',
                                   STATION_ID %in% c('2-LDJ000.60') ~ 'Lake Nottoway (Lee Lake)',
                                   TRUE ~ as.character(Lake_Name))) %>%
      left_join(lakeNutStandards %>%
                  mutate(Lakes_187B = 'y'),  # special step to make sure the WQS designation for 187 are correct even when not
                by = c('Lake_Name')) %>%
      # lake drummond special standards
      mutate(Lakes_187B = ifelse(is.na(Lakes_187B.y ), Lakes_187B.x, Lakes_187B.y),
             `Chlorophyll a (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 35,
                                                TRUE ~ as.numeric(`Chlorophyll a (ug/L)`)),
             `Total Phosphorus (ug/L)` = case_when(Lake_Name %in% c('Lake Drummond') ~ 40,
                                                   TRUE ~ as.numeric(`Total Phosphorus (ug/L)`))) %>%
      dplyr::select(STATION_ID:StreamType, Lakes_187B, `Description Of Waters`:`Total Phosphorus (ug/L)`) }) 
  
  # stationTableUI <- reactive({req(nrow(stationTable()) > 0)
  #   stationTable <- dplyr::select(stationTable(), STATION_ID, SEC:Lakes_187B) 
  #   
  #   for (i in 1:nrow(stationTable)) {
  #     stationTable$`Lake Station`[i] <- as.character(selectInput(paste0("sel", i), "", choices = c(TRUE, FALSE), width = "100px"))
  #     stationTable$`Lacustrine Zone`[i] <- as.character(selectInput(paste0("sel", i), "", choices = c(TRUE, FALSE), width = "100px"))    }
  #   return(stationTable)
  #   })
  
  output$stationReview <-  DT::renderDataTable({req(stationTable())
      datatable(stationTable(), escape = FALSE, selection = 'none',
                options = list(dom = 'it', scrollX= TRUE, scrollY = '300px', pageLength = nrow(stationTable())))})
  
  
  output$lakeStationSelection_ <- renderUI({req(nrow(stationTable()) > 0)
    checkboxGroupInput('lakeStationSelection', "Identify all lake stations", choices = sort(unique(stationTable()$STATION_ID)))  })
  
  output$lacustrineZoneSelection_ <- renderUI({req(nrow(stationTable()) > 0)
    checkboxGroupInput('lacustrineZoneSelection', "Identify all lake stations that fall in the lacustrine zone", 
                       choices = sort(unique(stationTable()$STATION_ID)))  })
  
  
  
  # organize conventionals on command
  observe({req(input$rapidAssessmentRun, nrow(stationTable()) > 0)
  #conventionals <- reactive({req(input$rapidAssessmentRun, nrow(stationTable()) > 0)
    modal_reactive$conventionals <- conventionalsSummary(conventionals= conventionalsTemplate,
                        stationFieldDataUserFilter= multistationFieldDataUserFilter(),
                        stationAnalyteDataUserFilter = multistationAnalyteDataUserFilter(),
                        multistationInfo(), multistationGIS_View(), dropCodes = c('QF')) %>%
      arrange(FDT_STA_ID, FDT_DATE_TIME, FDT_DEPTH) })
  
  
  # run assessment on command
  observeEvent(req(input$rapidAssessmentRun, nrow(stationTable()) > 0, nrow(modal_reactive$conventionals) > 0), {
    show_modal_spinner(spin = 'flower')
    modal_reactive$assessmentResults <- automatedAssessmentFunction(stationTable(), modal_reactive$conventionals,
                                lakeStations = filter(stationTable(), STATION_ID %in% input$lakeStationSelection),
                                lacustrineDesignation = filter(stationTable(), STATION_ID %in% input$lacustrineZoneSelection),
                                VSCIresults = VSCIresults())
    remove_modal_spinner()
    })
  
  # assessmentResults <-  reactive({req(input$rapidAssessmentRun, nrow(stationTable()) > 0, nrow(conventionals()) > 0)
  #   show_modal_spinner(spin = 'flower')
  #   
  #   automatedAssessmentFunction(stationTable(), conventionals(),
  #                               lakeStations = filter(stationTable(), STATION_ID %in% input$lakeStationSelection),
  #                               lacustrineDesignation = filter(stationTable(), STATION_ID %in% input$lacustrineZoneSelection),
  #                               VSCIresults = VSCIresults()) 
  #   remove_modal_spinner()})
  
  ## Results Tab
  output$stationTableResults <- DT::renderDataTable({req(modal_reactive$assessmentResults)
    datatable(modal_reactive$assessmentResults$stationTableResults, escape = FALSE, selection = 'none', extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX= TRUE, scrollY = '500px', pageLength = nrow(modal_reactive$assessmentResults$stationTableResults),
                             buttons=list('copy',
                                          list(extend='csv',filename=paste('rapidStationAssessment',Sys.Date(),sep='')),
                                          list(extend='excel',filename=paste('rapidStationAssessment',Sys.Date(),sep='')))) ) %>% 
      formatStyle(c('TEMP_EXC','TEMP_SAMP','TEMP_STAT'), 'TEMP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('DO_EXC','DO_SAMP','DO_STAT'), 'DO_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('PH_EXC','PH_SAMP','PH_STAT'), 'PH_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
      formatStyle(c('ECOLI_EXC','ECOLI_SAMP','ECOLI_GM_EXC','ECOLI_GM_SAMP','ECOLI_STAT'), 'ECOLI_STAT', backgroundColor = styleEqual(c('IM'), c('red'))) %>%
      formatStyle(c('ENTER_SAMP','ENTER_EXC',"ENTER_GM_EXC","ENTER_GM_SAMP",'ENTER_STAT'), 'ENTER_STAT', backgroundColor = styleEqual(c('IM'), c('red'))) %>%
      formatStyle(c('AMMONIA_EXC','AMMONIA_STAT'), 'AMMONIA_STAT', backgroundColor = styleEqual(c('Review', 'IM'), c('yellow','red'))) %>%
      formatStyle(c('BENTHIC_STAT'), 'BENTHIC_STAT', backgroundColor = styleEqual(c('Review'), c('yellow'))) %>%
      formatStyle(c('NUT_TP_EXC','NUT_TP_SAMP'), 'NUT_TP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red')))  })
  
  ## Conventionals Tab
  output$conventionalsDataset <- DT::renderDataTable({req(modal_reactive$conventionals)
    datatable(modal_reactive$conventionals, escape = FALSE, selection = 'none', extensions = 'Buttons',
              options = list(dom = 'Bit', scrollX= TRUE, scrollY = '500px', pageLength = nrow(modal_reactive$conventionals),
                             buttons=list('copy',
                                          list(extend='csv',filename=paste('conventionals',Sys.Date(),sep='')),
                                          list(extend='excel',filename=paste('conventionals',Sys.Date(),sep='')))) )  })
  
  #output$test <- renderPrint({conventionals()})
}
  

ui <- fluidPage(
  assessmentUI('assessmentModule')
)

pool <- dbPool(
  drv = odbc::odbc(),
  Driver = "ODBC Driver 11 for SQL Server",#"SQL Server Native Client 11.0",
  Server= "DEQ-SQLODS-PROD,50000",
  dbname = "ODS",
  trusted_connection = "yes"
)
server <- function(input,output,session){
  

  callModule(assessment,'assessmentModule',  multistationFieldDataUserFilter = reactive(multistationFieldDataUserFilter), 
             multistationAnalyteDataUserFilter = reactive( multistationAnalyteDataUserFilter),
             WQM_Stations_Filter = reactive(WQM_Stations_Filter), 
             multistationSelection = reactive(multistationInfoFin),
             VSCIresults = reactive(VSCIresults))
  
}

shinyApp(ui,server)











# output$stationReview <-  DT::renderDataTable(
#   stationTableUI(), escape = FALSE, selection = 'none', server = FALSE,
#   options = list(dom = 't', scrollX= TRUE, scrollY = '300px'),
#   callback = JS("table.rows().every(function(i, tab, row) {
#     var $this = $(this.node());
#     $this.attr('id', this.data()[0]);
#     $this.addClass('shiny-input-container');
#   });
#   Shiny.unbindAll(table.table().node());
#   Shiny.bindAll(table.table().node());")
# )
# 
# output$sel = renderPrint({#glimpse(stationTableUI())
#   str(sapply(1:nrow(stationTableUI()), function(i) input[[paste0("sel", i)]]))
# })




# demodata <- stationTableUI()
# edited <- callModule(modFunction,"editable", demodata,
#                      reset = reactive(input$reset))
# data_df_final <- reactiveValues()
# observe({data_df_final$data <- edited$data}  )
# observe(print(data_df_final$data))
# 
# 