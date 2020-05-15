

ui <- shinyUI(fluidPage(theme= "yeti.css",
                        navbarPage("Regional Assessment Metadata Validation",
                                   navbarMenu("Water Quality Standards QA",
                                              tabPanel("Watershed Selection",
                                                       sidebarPanel(
                                                         fluidRow(column(5, selectInput('WQSwaterbodyType','Waterbody Type', choices = unique(WQSlayerConversion$waterbodyType))),
                                                                  column(7, actionButton('WQSstart',HTML("Begin Review With <br/>Waterbody Selection <br/>(Clears Cached Results)"),
                                                                                         class='btn-block'))),
                                                         hr(),
                                                       conditionalPanel(condition = ("input.WQSstart"), # dynamicSelectInput did not work with progress bar for spatial file
                                                                        uiOutput('WQSDEQregionSelection_'),
                                                                        uiOutput('WQSsubbasinSelection_'),
                                                                        br(),
                                                                        uiOutput('WQSbegin_'))),
                                                       
                                                       mainPanel(
                                                         leafletOutput('WQSVAmap'),
                                                         h5(strong('Preprocessing Data Recap for Selected Region/Subbasin/Type Combination')),
                                                         fluidRow(column(3, textOutput('singleSnapSummary1WQS')),
                                                                  column(3, textOutput('snapTooManySummary1WQS')),
                                                                  column(3, textOutput('noSnapSummary1WQS')),
                                                                  column(3, textOutput('regionalSitesSummary1WQS'))),
                                                         verbatimTextOutput('test'),
                                                         
                                                         br()) ),
                                              tabPanel('Manual Review',
                                                       wellPanel(
                                                         fluidRow(column(4, textOutput('singleSnapSummary2WQS'),
                                                                         actionButton('plotSingleSnapSummaryWQS', HTML('Plot stations that snapped <br/>to 1 WQS Segment'))),
                                                                  column(4, textOutput('snapTooManySummary2WQS'),
                                                                         actionButton('plotSnapTooManySummaryWQS', HTML('Plot stations that snapped <br/>to > 1 WQS Segment'))),
                                                                  column(4, textOutput('noSnapSummary2WQS'),
                                                                         actionButton('plotNoSnapSummaryWQS', HTML('Plot stations that snapped <br/>to 0 WQS segments'))))),
                                                                  #column(3, textOutput('regionalSitesSummary2WQS'),
                                                                  #       actionButton('plotRegionalSitesSummaryWQS', HTML('Plot all stations in <br/>the selected Region/Basin'))))),
                                                       leafletOutput('WQSmap'),
                                                       fluidRow(
                                                         actionButton('clear_allWQS', 'Clear Selection', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('backspace')),
                                                         actionButton('acceptWQS', 'Accept', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('check-circle')),
                                                         actionButton('changeWQS', 'Manual WQS Adjustment', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('exchange')),
                                                         actionButton('checkMeOutWQS', 'Check Me Out', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('exchange')),
                                                         actionButton("saveWQS", label = "Export reviews",style='color: #fff; background-color: #228b22; border-color: #134e13')        ),
                                                         #downloadButton('downloadWQS', label = "Export reviews",style='color: #fff; background-color: #228b22; border-color: #134e13')        ),
                                                       br(),
                                                       tabsetPanel(tabPanel(strong('Stations Data and Spatially Joined WQS'),
                                                                             br(),
                                                                             h5(strong('Selected Station Information')),
                                                                             DT::dataTableOutput('selectedSiteTableWQS'), br(),
                                                                             h5(strong('Spatially Joined WQS Information')),
                                                                             DT::dataTableOutput('associatedWQSTableWQS'),
                                                                             br(), br(), br()),
                                                                    tabPanel(strong('Updated Stations Data and Manually QAed WQS'),
                                                                             br(),
                                                                             fluidRow(
                                                                               h5(strong('Adjusted Station Data')),
                                                                               div(DT::dataTableOutput("adjustedStationsTableWQS"), style = "font-size:80%")),
                                                                             fluidRow(
                                                                               h5(strong('Manually QAed WQS Information')),
                                                                               div(DT::dataTableOutput("associatedWQSTableWQSQA"), style = "font-size:80%")),
                                                                             
                                                                             
                                                                             br(), br(), br() )
                                                       )
                                              )))))

server <- shinyServer(function(input, output, session) {
  # color palette for assessment polygons
  pal <- colorFactor(
    palette = topo.colors(7),
    domain = assessmentRegions$ASSESS_REG)
  
  palBufferDistance <- colorFactor(
    palette = terrain.colors(5),#colorRamps::blue2red(5),
    levels = c("20 m", "40 m", "60 m", "80 m", "No connections within 80 m"))
  
  
  # empty reactive objects list
  WQSreactive_objects = reactiveValues() # for WQS
  
  ## Watershed Selection Tab WQS
  
  # Bring in WQS layer statewide
  WQSstatewide <- eventReactive(input$WQSstart, {
    typeName <- case_when(input$WQSwaterbodyType == 'Lacustrine' ~ 'lakes_reservoirs',
                          input$WQSwaterbodyType == 'Estuarine' ~ 'estuarinepolygons',
                          TRUE ~ as.character(input$WQSwaterbodyType))
    withProgress(message = 'Reading in Large Spatial File',
                 st_zm(
                   st_read('GIS/WQS_layers_05082020.gdb', layer = paste0(tolower(typeName),'_05082020') , fid_column_name = "OBJECTID")) %>%
                   st_transform(4326) )})
    #withProgress(test) }) # for testing
  
  WQSstatewideEL <- eventReactive(input$WQSstart, {
    req(input$WQSwaterbodyType == "Estuarine")
    withProgress(message = 'Reading in Additional Estuarine Spatial File',
                 
   #              WQSsELtest  # for testing
                 st_zm(
                   st_read('GIS/WQS_layers_05082020.gdb', layer = 'estuarinelines_05082020' , fid_column_name = "OBJECTID")) %>%
                   st_transform(4326) %>%
                   # match polygon structure
                   rename('WQS_COMMEN' = 'WQS_COMMENT') %>% # match polygon structure
                   mutate(Shape_Area = NA) %>%
                   dplyr::select(names(WQSs())) 
   ) }) # technically makes this dependent on input$WQSbegin
  
  # Update map Subbasin based on user selection
  output$WQSDEQregionSelection_ <- renderUI({
    req(WQSstatewide())
    op <- filter(basinAssessmentRegion, BASIN %in% unique(WQSstatewide()$BASIN)) %>%
      distinct(ASSESS_REG) %>% 
      pull()
    selectInput("WQSDEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE,
                choices= op)  })
  
  output$WQSsubbasinSelection_ <- renderUI({
    req(WQSstatewide(), input$WQSDEQregionSelection)
    op <- filter(basinAssessmentRegion, BASIN %in% unique(WQSstatewide()$BASIN)) %>%
      filter(ASSESS_REG %in% input$WQSDEQregionSelection) %>%
      distinct(Basin_Code) %>% 
      pull()
    selectInput("WQSsubbasinSelection", "Select Subbasin", multiple = FALSE,
                choices= op)  })
  output$WQSbegin_ <- renderUI({
    req(WQSstatewide(), input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    actionButton('WQSbegin', HTML("Begin Review With Subbasin Selection <br/>(Retrieves Last Saved Result)"),
                 class='btn-block')  })
    
  basinCodes <- reactive({
    req(WQSstatewide(), input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    filter(basinAssessmentRegion, BASIN %in% unique(WQSstatewide()$BASIN)) %>%
      filter(ASSESS_REG %in% input$WQSDEQregionSelection) %>%
      filter(Basin_Code %in% input$WQSsubbasinSelection) %>%
      distinct(BASIN_CODE) %>% 
      pull() })

  WQSs <- eventReactive(input$WQSbegin, {
    req(WQSstatewide(), input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    WQSstatewide() %>%
      # limit to just selected filters
      filter(BASIN %in% as.character(basinCodes()))      })
  
  WQSsEL <- reactive({
    req(WQSstatewideEL(), input$WQSwaterbodyType == "Estuarine")
    filter(WQSstatewideEL(), BASIN %in% as.character(basinCodes())) })
    
  
  # Make an object (once per Subbasin filter) that encompasses all WQS_ID options for said subbasin for manual WQS_ID adjustment modal, speeds rendering
  WQS_ID_subbasinOptions <- reactive({req(WQSs())
    if(input$WQSwaterbodyType != "Estuarine"){
      as.character(WQSs()$WQS_ID)
      } else {  c(as.character(WQSs()$WQS_ID), as.character(WQSsEL()$WQS_ID))   }      })
  
  
  ## Map output of selected subbasin
  output$WQSVAmap <- renderLeaflet({
    req(input$WQSbegin, WQSstatewide(), input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    subbasins <- filter(basin7, BASIN_CODE %in% as.character(basinCodes()))
    
    m <- mapview( subbasins, label= subbasins$SUBBASIN, layer.name = 'Selected Subbasin',
                 popup= leafpop::popupTable(subbasins, zcol=c('BASIN_NAME', 'BASIN_CODE', 'SUBBASIN')))
    m@map %>% setView(st_bbox(subbasins)$xmax[[1]],st_bbox(subbasins)$ymax[[1]],zoom = 7)  })
  
  
  ### WQS reactive 
  observeEvent(input$WQSbegin, {
    # Bring in existing WQS information
    WQSreactive_objects$WQSlookup <- loadData("WQSlookupTable")
    # limit conventionals_DWQS to just chosen subbasin
    WQSreactive_objects$conventionals_DWQS_Region <- st_intersection(conventionals_DWQS, 
                                                                     filter(basin7, BASIN_CODE %in% basinCodes()))
    
    # All sites limited to waterbody type and subbasin
    WQSreactive_objects$snap_input <- readRDS('data/processedWQS/WQStable.RDS') %>%
      filter(str_extract(WQS_ID, "^.{2}") %in% filter(WQSlayerConversion, waterbodyType %in% input$WQSwaterbodyType)$WQS_ID) %>%
      filter(gsub("_","",str_extract(WQS_ID, ".{3}_")) %in% 
               str_pad(unique(filter(basinAssessmentRegion, BASIN_CODE %in% basinCodes())$BASIN_CODE), 
                       width = 2, side = 'left', pad = '0')) %>%
      # filter out any sites that happen to have existing WQS_ID
      filter(! StationID %in% WQSreactive_objects$WQSlookup$StationID) %>%
      group_by(StationID) %>%
      mutate(n = n()) %>% ungroup()
    # Sites limited to just region of interest
    WQSreactive_objects$snap_input_Region <- WQSreactive_objects$snap_input %>%
      left_join(WQSreactive_objects$conventionals_DWQS_Region, by = 'StationID') %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = T, # don't remove these lat/lon cols from df
               crs = 4326) %>%
      st_intersection(filter(assessmentRegions, ASSESS_REG %in% input$WQSDEQregionSelection)) %>%
      st_drop_geometry() %>% # back to tibble
      rename('Buffer Distance' = 'Buffer.Distance') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n)
    # Make dataset of all sites for highlighting purposes, preliminary list
    WQSreactive_objects$sitesUnique <- WQSreactive_objects$snap_input %>%
      full_join(WQSreactive_objects$conventionals_DWQS_Region, by = 'StationID') %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326)
    ## Make dataset of all WQS_IDs available for table purposes, this will hold corrected WQS_ID information after user review
    #WQSreactive_objects$WQS_IDs <- WQSreactive_objects$snap_input 
    # Make dataset of multiple segments snapped to single site IN REGION
    WQSreactive_objects$tooMany <- filter(WQSreactive_objects$snap_input_Region, n > 1) %>%
      group_by(StationID) %>% mutate(colorFac = row_number()) %>% ungroup() 
    # Make a dataset of actual segments for plotting
    WQSreactive_objects$tooMany_sf <- filter(WQSs(), WQS_ID %in% WQSreactive_objects$tooMany$WQS_ID) %>%
      left_join(WQSreactive_objects$tooMany, by = 'WQS_ID') %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())
    if(input$WQSwaterbodyType == 'Estuarine'){
      WQSreactive_objects$tooMany_sf_EL <- filter(WQSsEL(), WQS_ID %in% WQSreactive_objects$tooMany$WQS_ID) %>%              # bonus polyline feature for Estuarine
        left_join(WQSreactive_objects$tooMany, by = 'WQS_ID') %>%
        dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())    }
    # Make dataset of sites associated with too many segments IN REGION
    WQSreactive_objects$tooMany_sites <- filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$tooMany$StationID) %>%
      left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID') %>%
      {if(input$WQSwaterbodyType == 'Estuarine')
        rbind(left_join(filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$tooMany$StationID), 
                        WQSsEL() %>% st_drop_geometry(), by = 'WQS_ID'))
        else . } %>%
      distinct(StationID, .keep_all = T) %>%
      dplyr::select(-c(WQS_ID, `Buffer Distance`, n))
    # Make dataset of sites that snapped to a single WQS and join WQS info  IN REGION
    WQSreactive_objects$snapSingle <- filter(WQSreactive_objects$sitesUnique, n == 1) %>%
      filter(StationID %in% WQSreactive_objects$snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
      left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID') %>%
      {if(input$WQSwaterbodyType == 'Estuarine')
        filter(., str_extract(WQS_ID, "^.{2}") == 'EP') %>% # keep just polygon result from above
          rbind(filter(WQSreactive_objects$sitesUnique, n == 1) %>%
                  filter(StationID %in% WQSreactive_objects$snap_input_Region$StationID & str_extract(WQS_ID, "^.{2}") == 'EL') %>%
                  left_join(WQSsEL() %>% st_drop_geometry(), by = 'WQS_ID') )
        else . } %>%
      mutate(`Buffer Distance` = as.factor(`Buffer Distance`))
    # Make dataset of sites associated with no segments IN REGION
    WQSreactive_objects$snapNone <- filter(WQSreactive_objects$sitesUnique, is.na(WQS_ID)) %>%
      filter(StationID %in% WQSreactive_objects$snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
      left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
    # Make empty dataset of sites that assessors touched
    WQSreactive_objects$sitesAdjusted <-  WQSreactive_objects$sitesUnique[0,]  %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`) %>%
      mutate(Comments = as.character())
    # Make dataset for user to download
    WQSreactive_objects$finalWQS <- WQSreactive_objects$WQSlookup
    # Make dataset of all selectable sites on map
    #WQSreactive_objects$sitesUniqueFin <- WQSreactive_objects$conventionals_DWQS_Region 
  })
  
#  # Make dataset of all selectable sites on map
#  observe({
#    req(WQSreactive_objects$sitesUnique, WQSreactive_objects$conventionals_DWQS_Region)
#    WQSreactive_objects$sitesUniqueFin <- WQSreactive_objects$conventionals_DWQS_Region })
#    # for now, all sites are in conventionals pull, but this may not always be true
#      #rbind(WQSreactive_objects$sitesUnique, mutate(WQSreactive_objects$conventionals_DWQS_Region, WQS_ID = NA, `Buffer Distance` = NA, n = NA) %>% dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, FDT_STA_ID, everything()))  })
  
  # UI summaries of data pulled in to app, first and second tab
  output$singleSnapSummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapSingle), ' stations that snapped to 1 AU segment in preprocessing.'))})
  output$singleSnapSummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapSingle), ' stations that snapped to 1 AU segment in preprocessing.'))})
  output$snapTooManySummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$tooMany_sites), ' stations that snapped to > 1 AU segment in preprocessing.'))})
  output$snapTooManySummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$tooMany_sites), ' stations that snapped to > 1 AU segment in preprocessing.'))})
  output$noSnapSummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapNone), ' stations that snapped to 0 AU segments in preprocessing.'))})
  output$noSnapSummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
    cat(paste0('There are ', nrow(WQSreactive_objects$snapNone), ' stations that snapped to 0 AU segments in preprocessing.'))})
#  output$regionalSitesSummary1WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
#    cat(paste0('There are ', nrow(WQSreactive_objects$snap_input[['inputSites']]), ' stations in the selected Region/Basin.'))})
#  output$regionalSitesSummary2WQS <- renderPrint({ req(WQSreactive_objects$snap_input)
#    cat(paste0('There are ', nrow(WQSreactive_objects$snap_input[['inputSites']]), ' stations in the selected Region/Basin.'))})
# not sure I'm going to do that for WQS  
  
 
   
  ### WQS REVIEW TAB ##################################################################################
  
  output$test <- renderPrint({req(WQSs())
    class(st_geometry(WQSs()))})
  
  
  # WQS Map
  output$WQSmap <- renderLeaflet({
    req(WQSreactive_objects$snap_input)
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE, 
                 options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20)) %>%
      setView(-78, 37.5, zoom=7)  %>% 
      addCircleMarkers(data = WQSreactive_objects$conventionals_DWQS_Region, color='blue', fillColor='yellow', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 1,stroke=T, group="Conventionals Stations in Basin",
                       label = ~FDT_STA_ID, layerId = ~FDT_STA_ID) %>% 
      {if("sfc_MULTIPOLYGON" %in% class(st_geometry(WQSs()))) 
        addPolygons(., data = WQSs(),
                    layerId = ~WQS_ID,
                    label=~WQS_ID, group="All WQS in selected Region/Basin", 
                    color = 'blue', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
                    weight = 3,stroke=T,
                    popup=leafpop::popupTable(WQSs()),
                    popupOptions = popupOptions( maxHeight = 100 )) %>% 
          hideGroup("All WQS in selected Region/Basin") 
        else addPolylines(., data = WQSs(),
                          layerId = ~WQS_ID,
                          label=~WQS_ID, group="All WQS in selected Region/Basin", 
                          color = 'blue', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
                          weight = 3,stroke=T,
                          popup=leafpop::popupTable(WQSs()),
                          popupOptions = popupOptions( maxHeight = 100 )) %>% 
          hideGroup("All WQS in selected Region/Basin")  } %>%
      addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
                  fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
                  group="Assessment Regions",
                  popup=leafpop::popupTable(assessmentRegions, zcol=c('ASSESS_REG'))) %>% hideGroup('Assessment Regions') %>% #,'VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
      {if(input$WQSwaterbodyType == 'Estuarine')
        addPolylines(., data =WQSsEL(), # WQSs(),
                     layerId = ~WQS_ID,
                     label=~WQS_ID, group="All WQS in selected Region/Basin", 
                     color = 'orange',
                     weight = 3,stroke=T,
                     popup=leafpop::popupTable(WQSsEL()),#WQSs()),
                     popupOptions = popupOptions( maxHeight = 100 )) %>% 
          hideGroup("All WQS in selected Region/Basin") 
        else . } %>%
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      inlmisc::AddSearchButton(group = "Conventionals Stations in Basin", zoom = 15,propertyName = "label",
                               textPlaceholder = "Search Conventionals Stations in Basin") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Conventionals Stations in Basin',"All WQS in selected Region/Basin",'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')  %>%
      hideGroup("Conventionals Stations in Basin")    
  })
  
  WQSmap_proxy <- leafletProxy("WQSmap")
  
  # Add layers to map as requested- Single snapped sites
  observeEvent(input$plotSingleSnapSummaryWQS, {
    if (nrow(WQSreactive_objects$snapSingle) > 0 ){
      WQSmap_proxy %>%
        addCircleMarkers(data=WQSreactive_objects$snapSingle,
                         layerId = ~paste0(StationID,'_snapSingle'), # need unique layerID 
                         label=~StationID, group="Stations Snapped to 1 WQS Segment", 
                         radius = 5, fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T, color = 'black',
                         fillColor= ~palBufferDistance(WQSreactive_objects$snapSingle$`Buffer Distance`)) %>%
        addLegend(position = 'topright', pal = palBufferDistance, values = WQSreactive_objects$snapSingle$`Buffer Distance`, 
                  group = 'Stations Snapped to 1 WQS Segment') %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 WQS Segment",
                                           "Stations Snapped to > 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 WQS Segments",
                                           #"All stations in the selected Region/Basin",
                                           'Conventionals Stations in Basin',
                                           "All WQS in selected Region/Basin",'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')
    } else {
      showNotification("There are no sites that snapped to only 1 WQS segment in preprocessing steps. Nothing to plot.")
    }
  })
  
  # Add layers to map as requested- Too many snapped sites
  observeEvent(input$plotSnapTooManySummaryWQS, {
    
    if(nrow(WQSreactive_objects$tooMany_sites) > 0){
      palTooMany <- colorNumeric(c('green','yellow', 'blue','red', 'pink','purple'), domain = WQSreactive_objects$tooMany$colorFac)
      
      WQSmap_proxy %>%
        addCircleMarkers(data=WQSreactive_objects$tooMany_sites,
                         layerId = ~paste0(StationID,'_tooMany'),  # need unique layerID 
                         label=~StationID, group="Stations Snapped to > 1 WQS Segment", 
                         color='black', fillColor='red', radius = 5,
                         fillOpacity = 0.8,opacity=0.5,weight = 2,stroke=T) %>%
        {if(nrow(WQSreactive_objects$tooMany_sf) > 0) 
        {if("sfc_MULTIPOLYGON" %in% class(st_geometry(WQSreactive_objects$tooMany_sf))) 
          addPolygons(., data=WQSreactive_objects$tooMany_sf,
                      layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                      label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                      color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                      popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                      popupOptions = popupOptions( maxHeight = 100 )) %>% 
            hideGroup("All WQS in selected Region/Basin")
          else 
            addPolylines(., data=WQSreactive_objects$tooMany_sf,
                         layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                         label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                         color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                         popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                         popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")}
          else . } %>%
        {if(nrow(WQSreactive_objects$tooMany_sf_EL) > 0)
          addPolylines(., data=WQSreactive_objects$tooMany_sf_EL,
                       layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                       color = ~palTooMany(WQSreactive_objects$tooMany_sf_EL$colorFac),weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf_EL),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")  
          else . } %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 WQS Segment",
                                           "Stations Snapped to > 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 WQS Segments",
                                           #"All stations in the selected Region/Basin",
                                           'Conventionals Stations in Basin',
                                           "All WQS in selected Region/Basin",'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') 
    } else {
      showNotification("There are no sites that snapped to > 1 WQS segment in preprocessing steps. Nothing to plot.")
    }   })
  
  # Add layers to map as requested- 0 snapped sites
  observeEvent(input$plotNoSnapSummaryWQS, {
    if (nrow(WQSreactive_objects$snapNone) > 0 ){
      WQSmap_proxy %>%
        addCircleMarkers(data=WQSreactive_objects$snapNone,
                         layerId = ~paste0(StationID,'_snapNone'), # need unique layerID 
                         label=~StationID, 
                         group="Stations Snapped to 0 WQS Segments", 
                         color='black', fillColor='yellow', radius = 5,
                         fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 WQS Segment",
                                           "Stations Snapped to > 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 WQS Segments",
                                           #"All stations in the selected Region/Basin",
                                           'Conventionals Stations in Basin',
                                           "All WQS in selected Region/Basin",'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')
    } else {
      showNotification("There are no sites that snapped to 0 WQS segments in preprocessing steps. Nothing to plot.")
    }
  })
  
  # Map marker click (to identify selected sites
  observeEvent(input$WQSmap_marker_click, {
    site_click <- input$WQSmap_marker_click # this is all the info based on your click
    siteid <- strsplit(site_click$id, "_")[[1]][1] # this is just the layerID associated with your click
    # have to remove the unique layerID after _ to make sense of StationID
    
    if(!is.null(siteid)){ # if you clicked a point with info, find all Stations that match (with a round)
      # first find site matches from user input dataset, by lat and long
      siteMatches <- filter(WQSreactive_objects$sitesUnique, 
                            StationID %in% siteid) %>%
        st_drop_geometry() %>%
        pull(StationID)
      
      # and save all this info for later
      siteid_current <-  c(siteMatches)#, as.character(existingSiteMatches))
      
      # add the current site(s) to the selected list for highlighting and displaying in table
      if(is.null(WQSreactive_objects$namesToSmash)){
        WQSreactive_objects$namesToSmash <- siteid_current
      } else {
        WQSreactive_objects$namesToSmash <- append(siteid_current, WQSreactive_objects$namesToSmash)    }
    }
  })
  
  # Update map marker highlights
  observeEvent(WQSreactive_objects$namesToSmash, ignoreNULL=F, {
    if(!is.null(WQSreactive_objects$namesToSmash)){
      WQSmap_proxy %>%
        clearGroup(group='highlight') %>%
        addCircleMarkers(data=filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$namesToSmash),
                         layerId = ~paste0(StationID,'_sitesHighlighted'),  # need unique layerID 
                         group='highlight', 
                         radius = 20, 
                         color='chartreuse', opacity = 0.75, fillOpacity = 0.4)  
    } else {
      WQSmap_proxy %>%
        clearGroup(group='highlight') }  })
  
  ## Clear all selected sites
  observeEvent(input$clear_allWQS, {
    WQSreactive_objects$namesToSmash=NULL
    WQSmap_proxy %>%
      clearGroup(group='highlight')  })
  
  ## Accept Snapped WQS Modal
  observeEvent(input$acceptWQS, {
    showModal(modalDialog(title = 'Accept Snapped WQS', size = 'l',
                          DT::renderDataTable({
                            filter(WQSreactive_objects$sitesUnique, FDT_STA_ID %in% WQSreactive_objects$namesToSmash) %>%
                              st_drop_geometry() %>%
                              datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '125px'))  }),
                          br(), br(),
                          textInput('acceptCommentWQS', 'Additional Comments and Documentation'),
                          actionButton('accept_okWQS', 'Accept', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('check-circle')),
                          actionButton('accept_cancelWQS', 'Cancel', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('window-close'))    ))  })
  
  # Do something with AU Accept Modal
  observeEvent(input$accept_cancelWQS, {removeModal()})
  observeEvent(input$accept_okWQS, {
    # Get name and WQS_ID information from tooMany
    sitesUpdated <- filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$namesToSmash) %>%
      #st_drop_geometry() %>%
      #distinct(FDT_STA_ID, .keep_all = T) %>%
      mutate(`Buffer Distance` = paste0('Manual Review | ', `Buffer Distance`),
             Comments = paste0('Manual Accept | ',input$acceptCommentWQS)) %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, Comments)
    
    
    # add the current site(s) to the adjusted list 
    #if(nrow(reactive_objects$sitesAdjusted) == 0){
    #  reactive_objects$sitesAdjusted
    #} else {
    WQSreactive_objects$sitesAdjusted <- rbind(WQSreactive_objects$sitesAdjusted, sitesUpdated) # rbind works better for sf objects
    #}
    
    dropMe <- unique(sitesUpdated$StationID)
    
    ## Remove Site from "to do' list
    # remove from snap to > 1 WQS sites and segments
    WQSreactive_objects$tooMany_sites <- filter(WQSreactive_objects$tooMany_sites, !(StationID %in% dropMe)) # drop sites
    WQSreactive_objects$tooMany_sf <- filter(WQSreactive_objects$tooMany_sf, !(StationID %in% dropMe)) # drop segments
    WQSreactive_objects$tooMany_sf_EL <- filter(WQSreactive_objects$tooMany_sf_EL, !(StationID %in% dropMe)) # drop segments
    
    
    # and if part of snap to 1 WQS, fix that data
    WQSreactive_objects$snapSingle <- filter(WQSreactive_objects$snapSingle, !(StationID%in% dropMe)) # drop sites
    
    # and if part of snap to 0 WQS, fix that data
    WQSreactive_objects$snapNone <- filter(WQSreactive_objects$snapNone, !(StationID %in% dropMe)) # drop sites
    
    # update output dataset
    WQSreactive_objects$finalWQS <- filter(WQSreactive_objects$finalWQS, !(StationID %in% dropMe)) %>%
      bind_rows(sitesUpdated)
    
    
    # Empty map selection
    WQSreactive_objects$namesToSmash <- NULL
    
    ### Clear modal
    removeModal()
  })
  
  
  
  ## Manual WQS Adjustment Modal
  observeEvent(input$changeWQS, {
    showModal(modalDialog(title = 'Manually Adjust WQS', size = 'l',
                          DT::renderDataTable({
                            filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$namesToSmash) %>%
                              st_drop_geometry() %>%
                              datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '125px'))  }),
                          br(), br(),
                          selectInput('mergeWQSID','Choose WQS to connect to station', 
                                      choices = unique(c(as.character(filter(WQSreactive_objects$tooMany, 
                                                                             StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID), # likely WQS
                                                         WQS_ID_subbasinOptions()))), # less likely WQS but an option
                          textInput('adjustCommentWQS', 'Additional Comments and Documentation'),
                          actionButton('adjust_okWQS', 'Accept', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('check-circle')),
                          actionButton('adjust_cancelWQS', 'Cancel', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('window-close'))    ))  })
  
  # Do something with WQS Adjustment Modal
  observeEvent(input$adjust_cancelWQS, {removeModal()})
  observeEvent(input$adjust_okWQS, {
    # Get name and location information from tooMany
    sitesUpdated <- filter(WQSreactive_objects$sitesUnique, StationID %in% WQSreactive_objects$namesToSmash) %>%
      #st_drop_geometry() %>%
      distinct(StationID, .keep_all = T) %>% # this time you do want to run a distinct to avoid duplicated rows
      mutate(WQS_ID = input$mergeWQSID,
             `Buffer Distance` = paste0('Manual Review | ', `Buffer Distance`),
             Comments = paste0('Manual Accept | ',input$adjustCommentWQS)) %>%
      dplyr::select(StationID, WQS_ID, `Buffer Distance`, Comments)
    
    # add the current site(s) to the adjusted list 
    WQSreactive_objects$sitesAdjusted <- rbind(WQSreactive_objects$sitesAdjusted, sitesUpdated) # rbind works better for sf objects
    
    dropMe <- unique(sitesUpdated$StationID)
    
    ## Remove Site from "to do' list
    # remove from snap to > 1 WQS sites and segments
    WQSreactive_objects$tooMany_sites <- filter(WQSreactive_objects$tooMany_sites, !(StationID %in% dropMe)) # drop sites
    WQSreactive_objects$tooMany_sf <- filter(WQSreactive_objects$tooMany_sf, !(StationID %in% dropMe)) # drop segments
    
    # and if part of snap to 1 WQS, fix that data
    WQSreactive_objects$snapSingle <- filter(WQSreactive_objects$snapSingle, !(StationID%in% dropMe)) # drop sites
    
    # and if part of snap to 0 WQS, fix that data
    WQSreactive_objects$snapNone <- filter(WQSreactive_objects$snapNone, !(StationID %in% dropMe)) # drop sites
    
    # update output dataset
    WQSreactive_objects$finalWQS <- filter(WQSreactive_objects$finalWQS, !(StationID %in% dropMe)) %>%
      bind_rows(sitesUpdated)
    
    # Empty map selection
    WQSreactive_objects$namesToSmash <- NULL
    
    ### Clear modal
    removeModal()
  })
  
  
  
  # update WQSmap after WQS adjustment
  observe({
    req(WQSreactive_objects$sitesAdjusted)
    if(nrow(WQSreactive_objects$tooMany_sites)> 0){
      palTooMany <- colorNumeric(c('green','yellow', 'blue','red', 'pink','purple'), domain = WQSreactive_objects$tooMany$colorFac)
    }
    
    ## Update proxy map
    if(nrow(WQSreactive_objects$sitesAdjusted) > 0){
      WQSmap_proxy %>% 
        # have to manually clear old sites to 'wipe' leaflet memory of joined sites
        clearGroup("Stations Snapped to 0 WQS Segments") %>%
        clearGroup("Stations Snapped to 1 WQS Segment") %>%
        clearGroup("Stations Snapped to > 1 WQS Segment") %>%
        clearGroup("WQS Segments of Stations Snapped to > 1 Segment") %>%
        
        
        {if(nrow(WQSreactive_objects$tooMany_sites) > 0)
          addCircleMarkers(., data=WQSreactive_objects$tooMany_sites,
                           layerId = ~paste0(StationID,'_tooMany'),  # need unique layerID 
                           label=~StationID, group="Stations Snapped to > 1 WQS Segment", 
                           color='black', fillColor='red', radius = 5,
                           fillOpacity = 0.8,opacity=0.5,weight = 2,stroke=T) 
          else . } %>%
        {if(nrow(WQSreactive_objects$tooMany_sf) > 0) 
        {if("sfc_MULTIPOLYGON" %in% class(st_geometry(WQSreactive_objects$tooMany_sf))) 
          addPolygons(., data=WQSreactive_objects$tooMany_sf,
                      layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                      label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                      color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                      popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                      popupOptions = popupOptions( maxHeight = 100 )) %>% 
            hideGroup("All WQS in selected Region/Basin")
          else
            addPolylines(., data=WQSreactive_objects$tooMany_sf,
                         layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                         label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                         color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                         popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                         popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")}
          else . } %>%
        {if(nrow(WQSreactive_objects$tooMany_sf_EL) > 0)
          addPolylines(., data=WQSreactive_objects$tooMany_sf_EL,
                       layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                       color = ~palTooMany(WQSreactive_objects$tooMany_sf_EL$colorFac),weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf_EL),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")  
          else . } %>%
        {if(nrow(WQSreactive_objects$snapSingle) > 0)
          addCircleMarkers(., data=WQSreactive_objects$snapSingle,
                           layerId = ~paste0(StationID,'_snapSingle'), # need unique layerID 
                           label=~StationID, group="Stations Snapped to 1 WQS Segment", 
                           radius = 5, fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T, color = 'black',
                           fillColor= ~palBufferDistance(WQSreactive_objects$snapSingle$`Buffer Distance`)) #%>%
          else .}  %>%
        {if(nrow(WQSreactive_objects$snapNone) > 0)
          addCircleMarkers(., data=WQSreactive_objects$snapNone,
                           layerId = ~paste0(StationID,'_snapNone'), # need unique layerID 
                           label=~StationID, 
                           group="Stations Snapped to 0 WQS Segments", 
                           color='black', fillColor='yellow', radius = 5,
                           fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T)
          else . } %>%
        addCircleMarkers(data=WQSreactive_objects$sitesAdjusted,
                         layerId = ~paste0(StationID,'_sitesAdjusted'),  # need unique layerID 
                         label=~StationID, group="Adjusted Sites", 
                         color='black', fillColor='purple', radius = 5,
                         fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>% 
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 WQS Segment",
                                           "Stations Snapped to > 1 WQS Segment",
                                           "WQS Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 WQS Segments",
                                           #"All stations in the selected Region/Basin",
                                           'Conventionals Stations in Basin',
                                           "All WQS in selected Region/Basin",'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') 
    }    })
  
  
  
  ### Stations Data and Spatially Joined WQS Tab
  output$selectedSiteTableWQS <- DT::renderDataTable({
    req(WQSreactive_objects$namesToSmash)
    filter(WQSreactive_objects$sitesUnique, FDT_STA_ID %in% WQSreactive_objects$namesToSmash) %>%
      st_drop_geometry() %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px'))  })
  
  output$associatedWQSTableWQS <- DT::renderDataTable({
    req(WQSreactive_objects$namesToSmash)
    filter(WQSs(), WQS_ID %in% filter(WQSreactive_objects$snap_input, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) %>%
      {if(input$WQSwaterbodyType == 'Estuarine')
        rbind(.,
              filter(WQSsEL(), WQS_ID %in%
                       filter(WQSreactive_objects$snap_input, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) ) } %>%
      st_drop_geometry() %>%
      dplyr::select(WQS_ID, everything()) %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '200px'))  })
  
  ### Updated Stations Data and Manually QAed WQS Tab
  output$adjustedStationsTableWQS <- DT::renderDataTable({
    req(WQSreactive_objects$namesToSmash, WQSreactive_objects$sitesAdjusted)
    filter(WQSreactive_objects$sitesAdjusted, StationID %in% WQSreactive_objects$namesToSmash) %>%
      st_drop_geometry() %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px'))  })
  
  ## User adjusted WQS table, WQS details
  output$associatedWQSTableWQSQA <- DT::renderDataTable({
    req(WQSreactive_objects$namesToSmash, WQSreactive_objects$sitesAdjusted)
    filter(WQSs() %>% st_drop_geometry(), WQS_ID %in% filter(WQSreactive_objects$sitesAdjusted, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) %>%
      {if(input$WQSwaterbodyType == 'Estuarine')
        rbind(.,
              filter(WQSsEL() %>% st_drop_geometry(), WQS_ID %in% 
                       filter(WQSreactive_objects$sitesAdjusted, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) )  } %>%
      #   st_drop_geometry() %>%
      dplyr::select(WQS_ID, everything()) %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '200px'))  })
  
  #  ## Download WQS Information
  #  export_file=reactive(paste0('WQSlookupTable.csv'))#, region(), '_', basin(),'_',input$assessmentType, '_', Sys.Date(),'.csv'))
  #  output$downloadWQS <- downloadHandler(
  #    filename=function(){export_file()},
  #    content = function(file) {
  #      write.csv(WQSreactive_objects$finalWQS %>%
  #                  # get rid of geometry if needed
  #                  {if('geometry' %in% names(WQSreactive_objects$finalWQS))
  #                    dplyr::select(., -geometry) 
  #                    else . } %>%
  #                  as.data.frame(), file, row.names = F) }) 
  
  observeEvent(input$saveWQS, {
    saveData(WQSreactive_objects$finalWQS %>%
               # get rid of geometry if needed
               {if('geometry' %in% names(WQSreactive_objects$finalWQS))
                 dplyr::select(., -geometry) 
                 else . } %>%
               as.data.frame(), "WQSlookupTable")
  })  

})

shinyApp(ui, server)


