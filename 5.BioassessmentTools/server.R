source('global.R')

assessmentRegions <- st_read( 'data/GIS/AssessmentRegions_simple.shp')
ecoregion <- st_read('data/GIS/vaECOREGIONlevel3__proj84.shp') 

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
                  options=list(scrollX = TRUE, scrollY = "300px",pageLength=nrow(inputFile())))})
  
  pinnedDecisions <- reactive({req(inputFile())
    pinCheck('IR2022bioassessmentDecisions_test', inputFile()) # can change to real deal pin in time
    # pull new pin
    pinnedDecisions <- pin_get('IR2022bioassessmentDecisions_test', board = 'rsconnect')
    return(pinnedDecisions)})
  
  
  output$userStations_ <- renderUI({ req(inputFile())
    selectInput('userStations', "Choose station(s) to generate report",
                choices = unique(inputFile()$StationID)) })
  
  output$test <- renderPrint({req(pinnedDecisions())
    pinnedDecisions()})
  

  output$downloadReport <- downloadHandler(
    'TestReport', #paste(unique(userData[["stats_wTemp"]][,1])[1],"_BenthicStressorReportTemplate.docx",sep=""),
    content= function(file){
      tempReport <- normalizePath('bioassessmentFactSheetTest.Rmd')
      # imageToSend1 <- normalizePath('RmdFiller_Figure1.jpg') #NEW
      # imageToSend2 <- normalizePath('RmdFiller_Figure2.jpg') #NEW

      owd <- setwd(tempdir())
      on.exit(setwd(owd))

      file.copy(tempReport, 'bioassessmentFactSheetTest.Rmd')
      # file.copy(imageToSend1, 'RmdFiller_Figure1.jpg') #NEW
      # file.copy(imageToSend2, 'RmdFiller_Figure2.jpg') #NEW

      params <- list(assessmentDecision = as_tibble(filter(assessmentDecision_UserSelection, StationID %in% input$userStations)),
                     SCI = filter(SCI_UserSelection, StationID %in% input$userStations),
                     habitat = filter(habitatUserSelection, StationID %in% input$userStations))

      rmarkdown::render(tempReport,output_file = file,
                        params=params,envir=new.env(parent = globalenv()))})

})