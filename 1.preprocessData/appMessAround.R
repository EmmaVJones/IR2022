library(tidyverse)
library(readxl)
library(sf)
library(shiny)
library(shinyBS)
library(mapview)
library(leaflet)
library(inlmisc)
library(DT)
library(writexl)

source('appModules/multipleDependentSelectizeArguments.R')
source('snappingFunctions/snapOrganizationFunctions_messAround.R')

snapList_AU <- readRDS("data/preAnalyzedRegionalAUdata/BRRO/Riverine/James River Basin_snapList.RDS")
  #readRDS("data/preAnalyzedRegionalAUdata/BRRO/Riverine/New River Basin_snapList.RDS")
tooMany <- snapCheck(snapList_AU[['sf_output']])

# All conventionals sites
conventionals_D <- st_read('GIS/conventionals_D.shp')

assessmentRegions <- st_read( 'GIS/AssessmentRegions_simple.shp')
assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 
#riverineAUs <-  st_read('C:/HardDriveBackup/GIS/Assessment/va_2018_aus_riverine.shp') %>%
#  st_transform(st_crs(assessmentLayer))

riverineAUs <- st_read(paste0('C:/HardDriveBackup/GIS/Assessment/va_2018_aus_',
                              tolower('Riverine'),'.shp')) %>%
  st_transform(4326)  
AUs1 <- suppressWarnings(st_intersection(st_zm(riverineAUs), 
                                        filter(assessmentLayer, ASSESS_REG == "BRRO") %>%
                                          filter(Basin == 'James River Basin')))

ui <- shinyUI(fluidPage(theme= "yeti.css",
                        navbarPage("Regional Assessment Metadata Validation",
                                   tabPanel("Watershed Selection",
                                            sidebarPanel(
                                              dynamicSelectInput("DEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE),
                                              dynamicSelectInput("basinSelection", "Select Major Basin", multiple = FALSE),
                                              selectInput('assessmentType','Assessment Type', choices = c('Riverine','Lacustrine','Estuarine')),
                                              #dynamicSelectInput("HUC6Selection", "Select VAHU6", multiple = FALSE),
                                              br(),
                                              actionButton('begin',"Begin",class='btn-block')),
                                            mainPanel(
                                              leafletOutput('VAmap'),
                                              br(),
                                              h5(strong('Assessment Units in Selected Major Basin')),
#                                              ,
                                              DT::dataTableOutput('AUSummary')
                                            )),
                                   tabPanel("Assessment Unit Review",
                                            wellPanel(
                                              fluidRow(column(4, textOutput('regionalSitesSummary'),
                                                              actionButton('plotRegionalSitesSummary', 'Plot stations in the selected Region/Basin')),
                                                       column(4, textOutput('snapTooManySummary'),
                                                              actionButton('plotSnapTooManySummary', 'Plot stations that snapped to > 1 AU Segment')),
                                                       column(4, textOutput('noSnapSummary'),
                                                              actionButton('plotNoSnapSummary', 'Plot stations that snapped to 0 AU segments in preprocessing')))),
                                            leafletOutput('AUmap'),
                                            DT::dataTableOutput('snapSummary'),
                                            verbatimTextOutput('test')
                                            ),
                                   tabPanel("WQS Review"),
                                   tabPanel("About",
                                            p('This app was created to help assessors attach correct AU and WQS information to 
                                              stations for each assessment cycle.'),
                                            p('Emma has already run the joining and snapping scripts such that the assessor
                                              is just identifying the correct AU/WQS where more than one segment snapped to a site 
                                              or no segments snapped to a site in addition to reviewing all metadata prior
                                              to assessing stations.'))
                                   )))


server <- shinyServer(function(input, output, session) {
  
  # color palette for assessment polygons
  pal <- colorFactor(
    palette = topo.colors(7),
    domain = assessmentRegions$ASSESS_REG)
  
  
  # empty reactive objects list
  reactive_objects=reactiveValues()
  
  
  ## Watershed Selection Tab
  
  # Query VAHUC6's By Selectize arguments
  the_data <- reactive({assessmentLayer})
  region_filter <- shiny::callModule(dynamicSelect, "DEQregionSelection", the_data, "ASSESS_REG" )
  basin_filter <- shiny::callModule(dynamicSelect, "basinSelection", region_filter, "Basin" )
  
  
  ################## FOR TESTING ###########################################################
  assessmentType_sf <- eventReactive(input$begin, {
    req(basin_filter(), input$assessmentType)
   riverineAUs})
  AUs <- eventReactive(input$begin, { AUs1 })
  
  
  ####### FOR REAL ##########################################################################
  #assessmentType_sf <- eventReactive(input$begin, {
  #  req(basin_filter(), input$assessmentType)
  #  typeName <- ifelse(input$assessmentType != 'Lacustrine', input$assessmentType, 'Reservoir')
  #  st_read(paste0('C:/HardDriveBackup/GIS/Assessment/va_2018_aus_',
  #                                    tolower(typeName),'.shp')) %>%
  #    st_transform(4326)})
  
  #AUs <- eventReactive(input$begin, {
  #  req(basin_filter(), input$assessmentType, assessmentType_sf())
  #  suppressWarnings(st_intersection(st_zm(assessmentType_sf()), basin_filter()))})
  
  # Map output of basin and assessmentType_sf
  output$VAmap <- renderLeaflet({
    input$begin
    
    m <- mapview(basin_filter(),label= basin_filter()$VAHU6, layer.name = 'VAHU6 in Selected Basin',
                 popup= leafpop::popupTable(basin_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")))
    
    m@map %>% setView(st_bbox(basin_filter())$xmax[[1]],st_bbox(basin_filter())$ymax[[1]],zoom = 7)  })
  
  
  # Table of AUs within Selected Region/Basin
  #output$AUSummary <-  DT::renderDataTable({ req(AUs())
  #  DT::datatable(AUs() %>% st_set_geometry(NULL), rownames = FALSE, 
  #                options= list(scrollX = TRUE, pageLength = nrow(AUs()), 
  #                              scrollY = "300px", dom='t'))   })
  
  
  
  
  ## Assessment Unit Review Tab
  
  observeEvent(input$begin, {
    #reactive_objects$snap_input <- readRDS(paste0('data/preAnalyzedRegionalAUdata/',
    #                                              unique(region_filter()$ASSESS_REG), '/',
    #                                              input$assessmentType, '/',
    #                                              unique(basin_filter()$Basin), '_snapList.RDS'))
    reactive_objects$snap_input <- snapList_AU
    reactive_objects$tooMany <- snapCheck(snapList_AU[['sf_output']] %>%
                                            mutate(FDT_STA_ID = `Point Unique Identifier`) %>%
                                            group_by(FDT_STA_ID) %>%
                                            mutate(colorFac = row_number()) %>% ungroup())
    # Make dataset of all sites for highlighting purposes
    reactive_objects$tooMany_sites <- filter(reactive_objects$snap_input[['inputSites']], FDT_STA_ID %in% reactive_objects$tooMany$`Point Unique Identifier`)})
  
  # UI summaries of data pulled in to app
  output$regionalSitesSummary <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$snap_input[['inputSites']]), ' stations in the selected Region/Basin.'))})
  output$snapTooManySummary <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$tooMany_sites), ' stations that snapped to > 1 AU segment in preprocessing.'))})
  output$noSnapSummary <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$tbl_output), ' stations that snapped to 0 AU segments in preprocessing.'))})
  
  output$AUmap <- renderLeaflet({
    req(reactive_objects$snap_input)
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE, 
                 options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20)) %>%
      setView(-78, 37.5, zoom=7)  %>% 
      addCircleMarkers(data = conventionals_D, color='blue', fillColor='yellow', radius = 4,
                       fillOpacity = 0.5,opacity=0.5,weight = 1,stroke=T, group="Conventionals Stations",
                       label = ~FDT_STA, layerId = ~FDT_STA, 
                       popup = leafpop::popupTable(conventionals_D)) %>% 
      addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
                  fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
                  group="Assessment Regions",
                  popup=leafpop::popupTable(assessmentRegions, zcol=c('ASSESS_REG'))) %>% hideGroup('Assessment Regions') %>% #,'VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
      #addPolygons(data= assessmentLayer,  color = 'black', weight = 1,
      #            fillOpacity = 0.5,stroke=0.1,
      #            group="Assessment Regions",
      #            popup=leafpop::popupTable(assessmentLayer, zcol=c('ASSESS_REG','VAHU6', 'VaName','Basin'))) %>% hideGroup('Assessment Regions') %>% #,'VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      inlmisc::AddSearchButton(group = "Conventionals Stations", zoom = 15,propertyName = "label",
                               textPlaceholder = "Search Conventionals Stations") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Conventionals Stations','Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') %>%
      hideGroup("Conventionals Stations")
    })
    
  map_proxy <- leafletProxy("AUmap")
  
  # Add layers to map as available
  
  observeEvent(input$plotSnapTooManySummary, {
    pal2 <- colorNumeric(c('green','yellow', 'blue','red'), domain = reactive_objects$tooMany$colorFac)
    
    map_proxy %>%
      addCircleMarkers(data=reactive_objects$tooMany_sites,
                       layerId = ~FDT_STA_ID,
                       label=~FDT_STA_ID, group="Sites Snapped to > 1 Segment", 
                       color='black', fillColor='orange', radius = 5,
                       fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%#,popup=leafpop::popupTable(reactive_objects$sitesUnique, zcol=c('UID','originalStationID','finalStationID'))
      addPolylines(data=reactive_objects$tooMany,
                   layerId = ~ID305B,
                   label=~ID305B, group="Segments of Sites Snapped to > 1 Segment", 
                   color = ~pal2(reactive_objects$tooMany$colorFac),weight = 3,stroke=T,
                   popup=leafpop::popupTable(reactive_objects$tooMany)) %>%
      
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Sites Snapped to > 1 Segment",'Conventionals Stations','Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') })
  
  
  
  #output$test <- renderPrint({
  #  reactive_objects$tooMany
  #})
  
})
  
shinyApp(ui, server)
  