library(tidyverse)
library(readxl)
library(sf)
library(shiny)
library(shinyBS)
library(mapview)
library(leaflet)
library(inlmisc)
library(DT)

assessmentRegions <- st_read('GIS/AssessmentRegions_VA84_basins.shp')

# Make existing stations layer
existingStations <- read_csv('C:/HardDriveBackup/R/GitHub/Rivers-StreamsAssessment/R&S_app_v4/processedStationData/RegionalResultsRiverine_BRROCitMonNonAgencyFINAL.csv') %>%
  bind_rows(read_csv('C:/HardDriveBackup/R/GitHub/LakesAssessment2020/app2020/LakeAssessmentApp_v2/processedStationData/final2020data/lakeStations2020_BRRO_citmonNonAgency.csv')) %>%
  mutate_if(is.character, function(x) {Encoding(x) <- 'latin1'; return(x)}) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng, 




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
  dplyr::select(UID, everything()) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng, 


citData <- cit %>%
  group_by(originalStationID, Latitude, Longitude) %>%
  mutate(UID = row_number()) %>%
  dplyr::select(UID, everything())

test <- full_join(citUnique, citData, by = 'UID')



### For testing within distance (150m = ~500ft)
st_distance(citUnique, citUnique) < 150
z <- st_is_within_distance(citUnique, citUnique, dist= 150)
str(z)


CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
  setView(-78, 37.5, zoom=6) %>%
  addCircleMarkers(data=citUnique,color='yellow', fillColor='blue', radius = 5,
                   fillOpacity = 0.5,opacity=1,weight = 2,stroke=T,group="sites",
                   clusterOptions = markerClusterOptions(),
                   label = ~originalStationID,
                   popup = leafpop::popupTable(citUnique, zcol=c( "UID","originalStationID" ))) %>%
  
 
  #addPolylines(data=riverineAUs, group = 'Riverine AUs', label = ~riverineAUs$ID305B) %>% hideGroup('Riverine AUs') %>%
  inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
  inlmisc::AddSearchButton(group = "sites", zoom = 15,propertyName = "label",
                           textPlaceholder = "Search stations") %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c('sites','Riverine AUs'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft')





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
              bsCollapsePanel(list(icon('map-marked-alt'), 'Review Sites'), value = 3,
                              
                              # Map
                              fluidRow(shinycssloaders::withSpinner(leaflet::leafletOutput("map", height="600px"),size=2, color="#0080b7"))
              ),
              
              bsCollapsePanel(list(icon('cog'), 'Review reactive '), value = 4,
                              
                              verbatimTextOutput('test'))
    )
)
                        

server <- function(input, output, session){
  
  pal <- colorFactor(
    palette = topo.colors(7),
    domain = assessmentRegions$ASSESS_REG)
  
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
      dplyr::select(UID, everything()) %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326) # add coordinate reference system, needs to be geographic for now bc entering lat/lng
    })
  
  observeEvent(input$adjustInput, {
    reactive_objects$sitesData <- reactive_objects$sites_Adjusted %>%
              group_by(originalStationID, Latitude, Longitude) %>%
              mutate(UID = row_number()) %>%
              dplyr::select(UID, everything()) })
  
  
  output$updatedTable <- DT::renderDataTable({
    req(reactive_objects$sites_Adjusted)
    datatable(reactive_objects$sites_Adjusted, rownames = F, options=list(scrollX = TRUE, scrollY = "300px"))  })

#### Review Map ####
  
  # Map output
  #session$onFlushed(once = T, function() {
    output$map <- renderLeaflet({
      
      CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE) %>%
        setView(-78, 37.5, zoom=6) %>%
        #addCircleMarkers(data=reactive_objects$sitesUnique, label=~originalStationID, group="Sites", 
        #                 color='yellow', fillColor='red', radius = 5,
        #                 fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%
        addCircleMarkers(data = existingStations, color='orange', fillColor='black', radius = 3,
                         fillOpacity = 0.5,opacity=0.5,weight = 1,stroke=T,group="Existing Stations",
                         label = ~FDT_STA_ID,
                         popup = leafpop::popupTable(existingStations, 
                                                     zcol=c( "FDT_STA_ID", "STA_DESC", "Deq_Region",
                                                             "Huc6_Vahu6","ID305B_1", "ID305B_2", 
                                                             "ID305B_3"  ))) %>% 
        addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
                    fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
                    group="Assessment Regions",
                    popup=leafpop::popupTable(assessmentRegions, zcol=c('ASSESS_REG','VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
        inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
        inlmisc::AddSearchButton(group = "Existing Stations", zoom = 15,propertyName = "label",
                                 textPlaceholder = "Search Existing Stations") %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c('Existing Stations','Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') %>%
        hideGroup("Existing Stations") }) # })
  
  map_proxy=leafletProxy("map")
  
  
  # Add sites via proxy on site_types change
  observeEvent(input$adjustInput, {
    map_proxy %>%
      addCircleMarkers(data=reactive_objects$sitesUnique,
                       label=~originalStationID, group="Sites", 
                       color='yellow', fillColor='red', radius = 5,
                       fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T,
                       popup=leafpop::popupTable(reactive_objects$sitesUnique, 
                                                 zcol=c('UID','originalStationID','finalStationID'))) %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Sites','Existing Stations','Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') })
  

  
  
  
### Review data structure  ###  
  output$test <- renderText({
    str(reactive_objects$sitesUnique)
  })

}


shinyApp(ui, server)











#observeEvent(#{
#  reactive_objects$sitesUnique,{#}, ignoreNULL = F, ignoreInit=T, {
#    #if(dim(reactive_objects$sitesUnique)[1]>0){
#      map_proxy %>% 
#        clearGroup(group='sites') %>% 
#        #clearGroup(group='Merged sites') %>% clearGroup(group='Site IDs') %>%  clearGroup(group='Site names') %>% 
#        addCircleMarkers(data=reactive_objects$sitesUnique, label=~originalStationID, group="sites", 
#                         color='yellow', fillColor='red', radius = 5,
#                         fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T)#,  options = pathOptions(pane = "markers"))


# %>%
#addCircles(data=mlocs, group="locationID", stroke=F, fill=F, label=~MonitoringLocationIdentifier,
#           popup = paste0(
#             mlocs$MonitoringLocationIdentifier,
#             "<br>", mlocs$MonitoringLocationName)) %>%
#addCircles(data=mlocs, group="locationName", stroke=F, fill=F, label=~MonitoringLocationName,
#           popup = paste0(
#             mlocs$MonitoringLocationIdentifier,
#             "<br>", mlocs$MonitoringLocationName)) %>%
#addLabelOnlyMarkers(data=reactive_objects$map_sites, group="Site IDs", lat=~lat, lng=~long, options = pathOptions(pane = "labels"),
#                    label=~MonitoringLocationIdentifier,labelOptions = labelOptions(noHide = T, textsize = "15px"),
#                    clusterOptions=markerClusterOptions(spiderfyOnMaxZoom=T)) %>%
#addLabelOnlyMarkers(data=reactive_objects$map_sites, group="Site names", lat=~lat, lng=~long, options = pathOptions(pane = "labels"),
#                    label=~MonitoringLocationName,labelOptions = labelOptions(noHide = T, textsize = "15px"),
#                    clusterOptions=markerClusterOptions(spiderfyOnMaxZoom=T)) %>%
#          inlmisc::AddSearchButton(group = "sites", zoom = 15, propertyName = "label",
#                                   textPlaceholder = "Search stations")  
#leaflet.extras::removeSearchFeatures() %>%
#leaflet.extras::addSearchFeatures(
#  targetGroups = c('au_ids','au_names','locationID'),
#  options = leaflet.extras::searchFeaturesOptions(
#    zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
#    autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ))


#if(input$auto_zoom){
#  map_proxy %>% fitBounds(min(reactive_objects$map_sites$long)*0.99, min(reactive_objects$map_sites$lat)*0.99, max(reactive_objects$map_sites$long)*1.01, max(reactive_objects$map_sites$lat)*1.01)
#}




#}
#    })



