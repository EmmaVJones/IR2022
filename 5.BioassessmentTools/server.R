source('global.R')

# assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
# ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp')

shinyServer(function(input, output, session) {
  
  # empty reactive objects list
  reactive_objects = reactiveValues() 
  
  inputFile <- reactive({inFile <- input$userData
  if(is.null(inFile))
    return(NULL)
  read_excel(inFile$datapath) }) 
  
  # Validate Input data
  validInputData <- reactive({req(inputFile())
    stationValidation(inputFile()) })
    
  #OGpinnedDecisions <- reactive({pin_get('IR2022bioassessmentDecisions_test', board = 'rsconnect') })

  output$userStations_ <- renderUI({ req(pinnedDecisions())
    selectInput('userStations', "Choose a station to generate report",
                   choices = unique(pinnedDecisions()$StationID))  })#unique(OGpinnedDecisions()$StationID))  })
  
  observe({req(validInputData())
    updateSelectInput(session, "userStations", "Choose a station to generate report",
                      choices = bind_rows(filter(pinnedDecisions(), StationID %in% validInputData()$StationID),
                                          filter(pinnedDecisions(), ! StationID %in% validInputData()$StationID)) %>%
                        dplyr::select(StationID) %>% pull() )  })
  
  # output$userStations_ <- renderUI({ req(inputFile())
  #   selectizeInput('userStations', "Choose a station to generate report",
  #                  choices = #unique(inputFile()$StationID)) 
  #                    bind_rows(filter(pinnedDecisions(), StationID %in% inputFile()$StationID),
  #                              filter(pinnedDecisions(), ! StationID %in% inputFile()$StationID)) %>%
  #                    dplyr::select(StationID) %>% pull() ) })
  
  # Display user input data
  output$inputTable <- DT::renderDataTable({req(inputFile())
    # Identify any stations with issues
    invalidInputData <- filter(inputFile(), ! StationID %in% validInputData()$StationID)
    
    if(nrow(invalidInputData) > 0){
      DT::datatable(mutate(inputFile(), invalidData = case_when(StationID %in% invalidInputData$StationID ~ 1, TRUE ~ 0)),
                    escape=F, rownames = F, options=list(scrollX = TRUE, scrollY = "800px",pageLength=nrow(inputFile()))) %>%
        formatStyle('StationID', 'invalidData', backgroundColor = styleEqual(c(0, 1), c(NA, 'yellow'))  )
    } else {
      DT::datatable(inputFile(),escape=F, rownames = F,
                    options=list(scrollX = TRUE, scrollY = "800px",pageLength=nrow(inputFile())))  }  })
  
  pinnedDecisions <- reactive({
    if(is.null(inputFile())){
      return(pin_get('IR2022bioassessmentDecisions_test', board = 'rsconnect'))
    } else {
      pinCheck('IR2022bioassessmentDecisions_test', validInputData()) # can change to real deal pin in time
      # pull new pin
      pinnedDecisions <- pin_get('IR2022bioassessmentDecisions_test', board = 'rsconnect')
      return(pinnedDecisions)
    } })
  
  #output$test <- renderPrint({ reactive_objects$SCI_UserSelection })#  assessmentDecision_UserSelection() }) #filter(OGpinnedDecisions(), StationID %in% input$userStations) })#reactive_objects$assessmentDecision_UserSelection})
  
  assessmentDecision_UserSelection <- reactive({req(pinnedDecisions())
    filter(pinnedDecisions(), StationID %in% input$userStations) })

  observe({req(assessmentDecision_UserSelection())
    reactive_objects$SCI_UserSelection <- filter(VSCIresults, StationID %in% filter(assessmentDecision_UserSelection(), AssessmentMethod == 'VSCI')$StationID) %>%
      bind_rows(
        filter(VCPMI63results, StationID %in% filter(assessmentDecision_UserSelection(), AssessmentMethod == 'VCPMI + 63')$StationID)  ) %>%
      bind_rows(
        filter(VCPMI65results, StationID %in% filter(assessmentDecision_UserSelection(), AssessmentMethod == 'VCPMI - 65')$StationID)  ) %>%
      # add back in description information
      left_join(filter(benSamps, StationID %in% input$userStations) %>%
                  dplyr::select(StationID, Sta_Desc, BenSampID,US_L3CODE, US_L3NAME, HUC_12, VAHU6, Basin, Basin_Code),
                by = c('StationID', 'BenSampID')) %>%
      dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, everything())
    reactive_objects$habitatUserSelection <- habitatConsolidation( input$userStations, habSamps, habValues)  })


  # have to make separate reactive object in order to send appropriate station name to the download title
  fileNameForReport <- reactive({paste("IR",assessmentCycle," ", as.character(unique(input$userStations))," Benthic Assessment Fact Sheet.html", sep = "")})


  output$downloadReport_ <- renderUI({req(reactive_objects$habitatUserSelection)
    list(downloadButton('downloadReport', 'Generate Report'),
         helpText('This button must be clicked for each station in order to generate a unique report.'))})

  output$downloadReport <- downloadHandler(
    filename = fileNameForReport,
    content= function(file){
      tempReport <- normalizePath('bioassessmentFactSheet.Rmd')
      imageToSend1 <- normalizePath('images/riskCategories.PNG') #NEW
      imageToSend2 <- normalizePath('images/HabitatColor.jpg') #NEW

      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      file.copy(tempReport, 'bioassessmentFactSheet.Rmd')
      file.copy(imageToSend1, 'images/riskCategories.PNG') #NEW
      file.copy(imageToSend2, 'images/HabitatColor.jpg') #NEW

      params <- list(assessmentDecision =  assessmentDecision_UserSelection(),
                     SCI = reactive_objects$SCI_UserSelection,
                     habitat = reactive_objects$habitatUserSelection)

      rmarkdown::render(tempReport,output_file = file,
                        params=params,envir=new.env(parent = globalenv()))})

})
