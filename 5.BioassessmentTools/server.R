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
  
  # Display user input data
  output$inputTable <- DT::renderDataTable({
    DT::datatable(inputFile(),escape=F, rownames = F,
                  options=list(scrollX = TRUE, scrollY = "800px",pageLength=nrow(inputFile())))})
  
  pinnedDecisions <- reactive({req(inputFile())
    pinCheck('IR2022bioassessmentDecisions_test', inputFile()) # can change to real deal pin in time
    # pull new pin
    pinnedDecisions <- pin_get('IR2022bioassessmentDecisions_test', board = 'rsconnect')
    return(pinnedDecisions)})
  
  
  output$userStations_ <- renderUI({ req(inputFile())
    selectizeInput('userStations', "Choose a station to generate report",
                choices = #unique(inputFile()$StationID)) 
                  bind_rows(filter(pinnedDecisions(), StationID %in% inputFile()$StationID),
                            filter(pinnedDecisions(), ! StationID %in% inputFile()$StationID)) %>%
                  dplyr::select(StationID) %>% pull() ) })
  
  observe({
    reactive_objects$assessmentDecision_UserSelection <- filter(pinnedDecisions(), StationID %in% input$userStations)
    reactive_objects$SCI_UserSelection <- filter(VSCIresults, StationID %in% filter(reactive_objects$assessmentDecision_UserSelection, AssessmentMethod == 'VSCI')$StationID) %>%
      bind_rows(
        filter(VCPMI63results, StationID %in% filter(reactive_objects$assessmentDecision_UserSelection, AssessmentMethod == 'VCPMI + 63')$StationID)  ) %>%
      bind_rows(
        filter(VCPMI65results, StationID %in% filter(reactive_objects$assessmentDecision_UserSelection, AssessmentMethod == 'VCPMI - 65')$StationID)  ) %>%
      # add back in description information
      left_join(filter(benSamps, StationID %in% input$userStations) %>%
                  dplyr::select(StationID, Sta_Desc, BenSampID,US_L3CODE, US_L3NAME, HUC_12, VAHU6, Basin, Basin_Code),
                by = c('StationID', 'BenSampID')) %>%
      dplyr::select(StationID, Sta_Desc, BenSampID, `Collection Date`, RepNum, everything())
    reactive_objects$habitatUserSelection <- habitatConsolidation( input$userStations, habSamps, habValues)  })
  
  #output$test <- renderPrint({fileNameForReport()})#class(reactive_objects$fileName)})#reactive_objects$SCI_UserSelection})
  
  # have to make separate reactive object in order to send appropriate station name to the download title
  fileNameForReport <- reactive({paste("IR",assessmentCycle," ", as.character(unique(input$userStations))," Benthic Assessment Fact Sheet.html", sep = "")})
  
  
  output$downloadReport_ <- renderUI({req(reactive_objects$habitatUserSelection)
    downloadButton('downloadReport', 'Generate Report')})

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

      params <- list(assessmentDecision =  reactive_objects$assessmentDecision_UserSelection,
                     SCI = reactive_objects$SCI_UserSelection,
                     habitat = reactive_objects$habitatUserSelection)

      rmarkdown::render(tempReport,output_file = file,
                        params=params,envir=new.env(parent = globalenv()))})

})
