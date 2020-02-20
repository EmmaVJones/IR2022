library(tidyverse)
library(readxl)
library(sf)
library(shiny)
library(shinyBS)
library(mapview)
library(leaflet)
library(inlmisc)
library(DT)

cit <- read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/CitizenNonAgency/2020IR Citizen Ambient4.14.19 (2).csv') %>%
  dplyr::select(Group_Station_ID:`DEQ QA Comments`) # drop junk columns


citmon <- read_csv('BRROcit.csv', locale = readr::locale(encoding = "latin1"))
citmon <- read_csv('test.csv', locale = readr::locale(encoding = "latin1"))


reassignColumns <- function(df, stationName, latName, longName){
  stationName_en <- enquo(stationName)
  latName_en <- enquo(latName)
  longName_en <- enquo(longName)
  
  mutate(df, 
         finalStationID = NA,
         originalStationID = !! stationName_en, 
         Latitude = !! latName_en, 
         Longitude = !! longName_en) %>%
    dplyr::select(originalStationID, finalStationID, Latitude, Longitude, everything())#,
                 # -c(!! stationName_en, !! latName_en,!! longName_en))
    
}


names(reassignColumns(cit, FDT_STA_ID, Latitude, Longitude))
  
%>%
  dplyr::select(Group_Station_ID:`DEQ QA Comments`) %>% # drop junk columns
  filter(!is.na(Latitude)|!is.na(Longitude)) %>% # drop sites without location information
  distinct(Group_Station_ID, FDT_STA_ID, Latitude, Longitude, .keep_all =T)  %>% #distinct by location and name
  dplyr::select(Group_Station_ID,FDT_STA_ID:FDT_SPG_CODE, Latitude:STA_CBP_NAME) %>%# drop data to avoid confusion
  mutate(UID = row_number()) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng, 














options(shiny.maxRequestSize=30*1024^2)

ui <- fluidPage(
  headerPanel(
    title='Citmon organization'),
  
  mainPanel(width=11,
            bsCollapse(id='collapse_panels', multiple=T, open=1,
                       bsCollapsePanel(list(icon('file-import'),"Start"), value=1,
                                       fluidRow(textInput('reviewer', 'Reviewer')),
                                       fluidRow(
                                         column(2, fileInput("import_sites", "Import site file", accept=".csv"))
                                       ),
                                       verbatimTextOutput('test'),
                                       tableOutput('originalTable')
                       ),
                       bsCollapsePanel(list(icon('file-import'),"Establish Variables"), value=1,
                                       fluidRow(
                                         column(2, uiOutput('IDfield1')),
                                         column(2, uiOutput('IDfield2')),
                                         column(2, uiOutput('IDfield3')),
                                         column(2, actionButton('adjustInput', icon=icon("refresh"), label='', style = "margin-top: 25px;"))
                                       ),
                                       tableOutput('updatedTable')
                                       
                                       
                       )
            ))
  
)

server <- server <- function(input, output, session){
  
  # empty reactive objects list
  reactive_objects=reactiveValues()
  
  
  observeEvent(input$collapse_panels, {
    if((2 %in% input$collapse_panels | 3 %in% input$collapse_panels) & (input$reviewer=="" | is.null(reactive_objects$sites_input))){
      showModal(modalDialog(easyClose=F, title='Inputs needed', "Please input your name and upload a sites file under the 'Start' box above before proceeding."))
    }	
  })
  
  # Read input files
  observeEvent(input$import_sites,{
    sites_file=input$import_sites$datapath
    if(is.null(sites_file)){
      return(NULL)
    }else{
      sites=as.data.frame(read_csv(sites_file, locale = readr::locale(encoding = "latin1")))
      #sites <- citmon
      reactive_objects$sites_input = sites
      reactive_objects$reviewer = input$reviewer
    }
  })
  
  output$test <- renderText({
    reactive_objects$sites_input
  })
  
  output$originalTable <- renderTable({
    #req(reactive_objects$sites_input)
    reactive_objects$sites_input
  })
  
  output$IDfield1 <- renderUI({
    req(reactive_objects$sites_input, reactive_objects$reviewer)
    selectizeInput('IDfield1_UI', label = 'Choose Station Name Field', 
                   choices = names(reactive_objects$sites_input), multiple = FALSE)
  })
  output$IDfield2 <- renderUI({
    req(reactive_objects$sites_input, reactive_objects$reviewer)
    selectizeInput('IDfield2_UI', label = 'Choose Latitude Field', 
                   choices = names(reactive_objects$sites_input), multiple = FALSE)
  })
  output$IDfield3 <- renderUI({
    req(reactive_objects$sites_input, reactive_objects$reviewer)
    selectizeInput('IDfield3_UI', label = 'Choose Longitude Field', 
                   choices = names(reactive_objects$sites_input), multiple = FALSE)
  })
  
  observeEvent(input$adjustInput, {
    reactive_objects$sites_Adjusted = reassignColumns(reactive_objects$sites_input, 
                                                      input$IDfield1_UI,
                                                      input$IDfield2_UI,
                                                      input$IDfield3_UI)
  })
  
  output$updatedTable <- renderTable({
    req(reactive_objects$sites_Adjusted)
    reactive_objects$sites_Adjusted
  })
}

shinyApp(ui, server)

