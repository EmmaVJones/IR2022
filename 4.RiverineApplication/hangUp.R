



output$stationTableDataSummary <- DT::renderDataTable({
  req(stationData(),siteData$StationTableOutput)
  datatable(siteData$StationTableOutput, extensions = 'Buttons', escape=F, rownames = F, editable = TRUE,
            options= list(scrollX = TRUE, pageLength = nrow(siteData$StationTableOutput),
                          # hide certain columns
                          #columnDefs = list(list(targets = 6, visible = FALSE)),
                          dom='Bt', buttons=list('copy',
                                                 list(extend='csv',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                 list(extend='excel',filename=paste('AssessmentResults_',paste(assessmentCycle,input$stationSelection, collapse = "_"),Sys.Date(),sep=''))))) %>% 
    formatStyle(c('TEMP_EXC','TEMP_SAMP','TEMP_STAT'), 'TEMP_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('DO_EXC','DO_SAMP','DO_STAT'), 'DO_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('PH_EXC','PH_SAMP','PH_STAT'), 'PH_STAT', backgroundColor = styleEqual(c('Review', '10.5% Exceedance'), c('yellow','red'))) %>%
    formatStyle(c('ECOLI_EXC','ECOLI_SAMP','ECOLI_GM_EXC','ECOLI_GM_SAMP','ECOLI_STAT'), 'ECOLI_STAT', backgroundColor = styleEqual(c('IM'), c('red'))) %>%
    formatStyle(c('ENTER_SAMP','ENTER_EXC',"ENTER_GM_EXC","ENTER_GM_SAMP",'ENTER_STAT'), 'ENTER_STAT', backgroundColor = styleEqual(c('IM'), c('red')))
})





## PWS table



#### Data Sub Tab ####---------------------------------------------------------------------------------------------------

# Display Data 
output$AURawData <- DT::renderDataTable({ AUData()
  DT::datatable(AUData(), extensions = 'Buttons', escape=F, rownames = F, 
                options= list(scrollX = TRUE, pageLength = nrow(AUData()), scrollY = "300px", 
                              dom='Btf', buttons=list('copy',
                                                      list(extend='csv',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')),
                                                      list(extend='excel',filename=paste('AUData_',paste(input$stationSelection, collapse = "_"),Sys.Date(),sep='')))))})
# Summarize data
output$stationDataTableRecords <- renderText({
  req(AUData())
  paste(nrow(AUData()), 'records were retrieved for',as.character(input$AUselection),sep=' ')})
output$uniqueStationDataTableRecords <- renderTable({
  req(AUData())
  plyr::count(AUData(), vars = c("FDT_STA_ID")) %>% dplyr::rename('Number of Records'='freq')})
output$stationDataTableAssessmentWindow <- renderText({
  req(AUData())
  withinAssessmentPeriod(AUData())})


# Need this as a reactive to regenerate below modules when user changes station 
stationSelected <- reactive({input$stationSelection})


## Temperature Sub Tab ##------------------------------------------------------------------------------------------------------

#callModule(temperaturePlotlySingleStation,'temperature', AUData, stationSelected)
