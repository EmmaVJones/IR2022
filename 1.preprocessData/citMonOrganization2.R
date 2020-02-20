library(tidyverse)
library(readxl)
library(sf)
library(shiny)
library(shinyBS)
library(mapview)
library(leaflet)
library(inlmisc)
library(DT)



# too big to read in using read_excel
#cit <- read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/CitizenNonAgency/2020IR Citizen Ambient4.14.19 (2).csv') %>%
#  dplyr::select(Group_Station_ID:`DEQ QA Comments`) %>% # drop junk columns
#  #filter(str_detect(FDT_STA_ID, 'FC') | is.na(Latitude) | is.na(Longitude)) %>%
#  filter(Group_Station_ID %in% c('R7','R07','1.1', 'FC06','FCAF', 'LH3D', 'LONG BRANCH CREEK') | 
#           FDT_STA_ID %in% c('4AROA-1-1-FC','4AROA-R07-FC','4AROA-1-2-FC')) # get some ferrum and missing lat/long sites


#write.csv(cit, 'cit_FC.csv', row.names = F)

cit <- read_csv('cit_FC.csv')


cit <- reassignColumns(cit, Group_Station_ID, Latitude, Longitude)

notEnoughInfo <- filter(cit, is.na(originalStationID), is.na(Latitude)|is.na(Longitude)) # separate sites without location information or identifier

citUnique <- filter(cit, !is.na(originalStationID), !is.na(Latitude)|!is.na(Longitude))  %>% # drop sites without location information
  distinct( originalStationID, Latitude, Longitude, .keep_all =T)  %>% #distinct by location and name
  mutate(UID = row_number()) %>%
  dplyr::select(UID, everything())

citData <- cit %>%
  group_by(originalStationID, Latitude, Longitude) %>%
  mutate(UID = row_number()) %>%
  dplyr::select(UID, everything())

test <- full_join(citUnique, citData, by = 'UID')






options(shiny.maxRequestSize=15*1024^2)
  
ui <- fluidPage(
    headerPanel(
      title='Citmon organization'),
    
    mainPanel(width=11,
              bsCollapse(id='collapse_panels', multiple=T, open=1,
                         bsCollapsePanel(list(icon('file-import'),"Start"), value=1,
                                         fluidRow(textInput('reviewer', 'Reviewer (Entry Required for All Further Analyses)')),
                                         fluidRow(
                                           column(2, fileInput("import_sites", "Import site file", accept=".csv"))
                                         ),
                                         dataTableOutput('originalTable')
                         )
              ),
              bsCollapsePanel(list(icon('exchange'),"Establish Variables"), value=2,
                              fluidRow(
                                column(2, uiOutput('IDfield1')),
                                column(2, uiOutput('IDfield2')),
                                column(2, uiOutput('IDfield3')),
                                column(2, actionButton('adjustInput', icon=icon("refresh"), label='', style = "margin-top: 25px;"))
                              ),
                              dataTableOutput('updatedTable')
              ),
              bsCollapsePanel(list(icon('cog'), 'Review reactive '), value = 3,
                              
                              verbatimTextOutput('test'))
    )
)
                        

server <- function(input, output, session){
  
  # empty reactive objects list
  reactive_objects=reactiveValues()
  
  
  observeEvent(input$collapse_panels, {
    if((2 %in% input$collapse_panels | 3 %in% input$collapse_panels) & (input$reviewer=="" | is.null(reactive_objects$sites_input))){
      showModal(modalDialog(easyClose=F, title='Inputs needed', "Please input your name and upload a sites file under the 'Start' box above before proceeding."))
    }	
  })
  
#### Start Tab ####
  
  # Upload user manipulated site data
  inputFile <- reactive({inFile <- input$import_sites
  if(is.null(inFile))
    return(NULL)
  read_csv(inFile$datapath)  })
  
  observe(reactive_objects$sites_input <- inputFile() )
  observe(reactive_objects$reviewr <- input$reviewer)


  output$originalTable <- DT::renderDataTable({
    req(input$import_sites, input$reviewer)
    datatable(reactive_objects$sites_input, rownames = F, options=list(scrollX = TRUE, scrollY = "300px"))  })

#### Establish Variable Tab ####
  
  output$IDfield1 <- renderUI({
    req(input$import_sites, input$reviewer)
    selectizeInput('IDfield1_UI', label = 'Choose Station Name Field', 
                   choices = names(reactive_objects$sites_input), multiple = FALSE)  })
  output$IDfield2 <- renderUI({
    req(input$import_sites, input$reviewer)
    selectizeInput('IDfield2_UI', label = 'Choose Latitude Field', 
                   choices = names(reactive_objects$sites_input), multiple = FALSE)  })
  output$IDfield3 <- renderUI({
    req(input$import_sites, input$reviewer)
    selectizeInput('IDfield3_UI', label = 'Choose Longitude Field', 
                   choices = names(reactive_objects$sites_input), multiple = FALSE)  })
  
  observeEvent(input$adjustInput, {
    reactive_objects$sites_Adjusted = reassignColumns(reactive_objects$sites_input, 
                                                      get(input$IDfield1_UI),
                                                      get(input$IDfield2_UI),
                                                      get(input$IDfield3_UI) ) })
  observeEvent(input$adjustInput, {
    reactive_objects$notEnoughInfo <- filter(reactive_objects$sites_Adjusted, is.na(originalStationID), is.na(Latitude)|is.na(Longitude)) })# separate sites without location information or identifier)
  observeEvent(input$adjustInput, {
    reactive_objects$sitesUnique <- filter(reactive_objects$sites_Adjusted, !is.na(originalStationID), !is.na(Latitude)|!is.na(Longitude))  %>% # drop sites without location information
    distinct(originalStationID, Latitude, Longitude, .keep_all =T)  %>% #distinct by location and name
    mutate(UID = row_number()) %>%
    dplyr::select(UID, everything()) })
  
  observeEvent(input$adjustInput, {
    reactive_objects$sitesData <- reactive_objects$sites_Adjusted %>%
              group_by(originalStationID, Latitude, Longitude) %>%
              mutate(UID = row_number()) %>%
              dplyr::select(UID, everything()) })
  
  
  output$updatedTable <- DT::renderDataTable({
    req(reactive_objects$sites_Adjusted)
    datatable(reactive_objects$sites_Adjusted, rownames = F, options=list(scrollX = TRUE, scrollY = "300px"))  })

#### Review Map ####
  
  
  
  
  output$test <- renderText({
    print(str(reactive_objects))
  })
  

}

shinyApp(ui, server)
