#source('global.R')

shinyServer(function(input, output, session) {
  
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
  AUs <- eventReactive(input$begin, { AUs2 })
  
  
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
#  output$VAmap <- renderLeaflet({
#    input$begin
    
#    m <- mapview(basin_filter(),label= basin_filter()$VAHU6, layer.name = 'VAHU6 in Selected Basin',
#                 popup= leafpop::popupTable(basin_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")),

#  popupOptions = popupOptions( maxHeight = 100 )) %>%
#    
#    m@map %>% setView(st_bbox(basin_filter())$xmax[[1]],st_bbox(basin_filter())$ymax[[1]],zoom = 7)  })
  
  
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
    # Make dataset of all sites for highlighting purposes
    reactive_objects$sitesUnique <- reactive_objects$snap_input[['inputSites']] %>%
      mutate(`Point Unique Identifier` = FDT_STA_ID,
             Comments = NA) %>%
      left_join(st_drop_geometry(reactive_objects$snap_input[['sf_output']]), by = 'Point Unique Identifier') %>%
      dplyr::select(FDT_STA_ID, ID305B, `Buffer Distance`, VAHU6, ASSESS_REG, VaName, Basin, Latitude, Longitude, Comments)
    # Make dataset of all AUs for table purposes, this will hold corrected AU information after user review
    reactive_objects$AUsegments <- reactive_objects$snap_input[['sf_output']] %>%
      mutate(FDT_STA_ID = `Point Unique Identifier`) %>%
      dplyr::select(ID305B, FDT_STA_ID, `Buffer Distance`, everything())
    # Make dataset of multiple segments snapped to single site
    reactive_objects$tooMany <- snapCheck(reactive_objects$snap_input[['sf_output']] %>%
                                            mutate(FDT_STA_ID = `Point Unique Identifier`) %>%
                                            group_by(FDT_STA_ID) %>%
                                            mutate(colorFac = row_number()) %>% ungroup())
    # Make dataset of sites associated with too many segments
    reactive_objects$tooMany_sites <- filter(reactive_objects$snap_input[['inputSites']], FDT_STA_ID %in% reactive_objects$tooMany$`Point Unique Identifier`)
    # Make dataset of sites that snapped to a single AU and join AU info  
    reactive_objects$snapSingle <- filter(reactive_objects$sitesUnique, FDT_STA_ID %in% reactive_objects$snap_input[['sf_output']]$`Point Unique Identifier`) %>%
      filter(!(FDT_STA_ID %in% reactive_objects$tooMany_sites$FDT_STA_ID))# filter out sites that attached to more than one segment
    # Make empty dataset of sites that assessors touched
    reactive_objects$sites_Adjusted <-  reactive_objects$sitesUnique[0,]
    })
  
  # UI summaries of data pulled in to app
  output$regionalSitesSummary <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$snap_input[['inputSites']]), ' stations in the selected Region/Basin.'))})
  output$snapTooManySummary <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$tooMany_sites), ' stations that snapped to > 1 AU segment in preprocessing.'))})
  output$noSnapSummary <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$tbl_output), ' stations that snapped to 0 AU segments in preprocessing.'))})
  
  # AU Map
  output$AUmap <- renderLeaflet({
    req(reactive_objects$snap_input)
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE, 
                 options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20)) %>%
      setView(-78, 37.5, zoom=7)  %>% 
#      addCircleMarkers(data = conventionals_D, color='blue', fillColor='yellow', radius = 4,
#                       fillOpacity = 0.5,opacity=0.5,weight = 1,stroke=T, group="Conventionals Stations",
#                       label = ~FDT_STA, layerId = ~FDT_STA, 
#                       popup = leafpop::popupTable(conventionals_D),
#                       popupOptions = popupOptions( maxHeight = 100 )) %>% 
#      addPolylines(data=AUs(),
#                   layerId = ~ID305B,
#                   label=~ID305B, group="All AUs in selected Region/Basin", 
#                   color = 'blue', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
#                   weight = 3,stroke=T,
#                   popup=leafpop::popupTable(riverineAUs),
#                   popupOptions = popupOptions( maxHeight = 100 )) %>% hideGroup("All AUs in selected Region/Basin") %>%
#      addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
#                  fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
#                  group="Assessment Regions",
#                  popup=leafpop::popupTable(assessmentRegions, zcol=c('ASSESS_REG'))) %>% hideGroup('Assessment Regions') %>% #,'VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
#      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
#      inlmisc::AddSearchButton(group = "Conventionals Stations", zoom = 15,propertyName = "label",
#                               textPlaceholder = "Search Conventionals Stations") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Conventionals Stations',"All AUs in selected Region/Basin",'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') %>%
      hideGroup("Conventionals Stations")  })
  
  map_proxy <- leafletProxy("AUmap")

  # Add layers to map as available
  
  observeEvent(input$plotRegionalSitesSummary, {
    map_proxy %>%
      addCircleMarkers(data=reactive_objects$snapSingle,
                       layerId = ~FDT_STA_ID,
                       label=~FDT_STA_ID, group="Stations in the selected Region/Basin", 
                       color='black', fillColor='cyan', radius = 5,
                       fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T,
                       popup = leafpop::popupTable(reactive_objects$snapSingle),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
      addPolylines(data=filter(reactive_objects$snap_input[['sf_output']],
                               !(`Point Unique Identifier` %in% reactive_objects$tooMany_sites$FDT_STA_ID)),
                   layerId = ~ID305B,
                   label=~ID305B, group="Segments of Stations in the selected Region/Basin", 
                   color = 'cyan', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
                   weight = 3,stroke=T,
                   popup=leafpop::popupTable(reactive_objects$snap_input[['sf_output']]),
                   popupOptions = popupOptions( maxHeight = 100 )) %>%
      hideGroup("Segments of Stations in the selected Region/Basin") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Stations in the selected Region/Basin",
                                         "Segments of Stations in the selected Region/Basin",
                                         "Stations Snapped to > 1 Segment",
                                         "Segments of Stations Snapped to > 1 Segment",
                                         'Conventionals Stations',"All AUs in selected Region/Basin",'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') })
  
  observeEvent(input$plotSnapTooManySummary, {
    palTooMany <- colorNumeric(c('green','yellow', 'blue','red'), domain = reactive_objects$tooMany$colorFac)
    
    map_proxy %>%
      addCircleMarkers(data=reactive_objects$tooMany_sites,
                       layerId = ~FDT_STA_ID,
                       label=~FDT_STA_ID, group="Stations Snapped to > 1 Segment", 
                       color='black', fillColor='orange', radius = 5,
                       fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%
      addPolylines(data=reactive_objects$tooMany,
                   layerId = ~ID305B,
                   label=~ID305B, group="Segments of Stations Snapped to > 1 Segment", 
                   color = ~palTooMany(reactive_objects$tooMany$colorFac),weight = 3,stroke=T,
                   popup=leafpop::popupTable(reactive_objects$tooMany),
                   popupOptions = popupOptions( maxHeight = 100 )) %>%
      hideGroup("Segments of Stations Snapped to > 1 Segment") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Stations in the selected Region/Basin",
                                         "Segments of Stations in the selected Region/Basin",
                                         "Stations Snapped to > 1 Segment",
                                         "Segments of Stations Snapped to > 1 Segment",
                                         'Conventionals Stations',"All AUs in selected Region/Basin",'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') })
  
  
  # Map marker click (to identify selected sites, will also select sites w/ identical (round(lat/long, 4) but different names
  observeEvent(input$AUmap_marker_click, {
    site_click <- input$AUmap_marker_click # this is all the info based on your click
    siteid <- site_click$id # this is just the layerID associated with your click
    
    if(!is.null(siteid)){ # if you clicked a point with info, find all Stations that match (with a round)
      # first find site matches from user input dataset, by lat and long
      siteMatches <- filter(reactive_objects$sitesUnique, #reactive_objects$snap_input[['inputSites']], 
                            FDT_STA_ID %in% siteid |
                              Latitude %in% round(site_click$lat, 4) & 
                              Longitude %in% round(site_click$lng, 4)) %>%
        mutate(sites = FDT_STA_ID) %>%
        dplyr::select(sites) %>% 
        st_drop_geometry() %>%
        pull()
    
    
    # and save all this info for later
    siteid_current <-  c(siteMatches)#, as.character(existingSiteMatches))
    
    # add the current site(s) to the selected list for highlighting and displaying in table
    if(is.null(reactive_objects$namesToSmash)){
      reactive_objects$namesToSmash <- siteid_current
    } else {
      reactive_objects$namesToSmash <- append(siteid_current, reactive_objects$namesToSmash)    }
    }
  })
  
  # Update map marker highlights
  observeEvent(reactive_objects$namesToSmash, ignoreNULL=F, {
    if(!is.null(reactive_objects$namesToSmash)){
      map_proxy %>%
        clearGroup(group='highlight') %>%
        addCircleMarkers(data=filter(reactive_objects$snap_input[['inputSites']], FDT_STA_ID %in% reactive_objects$namesToSmash),
                         group='highlight', 
                         radius = 20, 
                         color='chartreuse', opacity = 0.75, fillOpacity = 0.4)  
    } else {
      map_proxy %>%
        clearGroup(group='highlight') }  })
  
  
  ## Clear all selected sites
  observeEvent(input$clear_all, {
    reactive_objects$namesToSmash=NULL
    map_proxy %>%
      clearGroup(group='highlight')  })
  
  output$selectedSiteTable <- DT::renderDataTable({
    req(reactive_objects$namesToSmash)
    filter(reactive_objects$sitesUnique, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
      st_drop_geometry() %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px'))  })
  
  output$associatedAUTable <- DT::renderDataTable({
    req(reactive_objects$namesToSmash)
    filter(reactive_objects$AUsegments, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
      st_drop_geometry() %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '200px'))  })
  
  
  
  
  
  ## Manual AU Adjustment Modal
  observeEvent(input$changeAU, {
    showModal(modalDialog(title = 'Manually Adjust AUs', size = 'l',
                          DT::renderDataTable({
                            filter(reactive_objects$sitesUnique, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
                              st_drop_geometry() %>%
                              datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '125px'))  }),
                          br(), br(),
                          selectInput('mergeAUID','Choose AU to connect to station', 
                                      choices = c(as.character(unique(filter(reactive_objects$tooMany, `Point Unique Identifier` %in% reactive_objects$namesToSmash)$ID305B)), # likely AUs
                                                  unique(as.character(AUs1$ID305B)))), # less likely AUs but an option
                          textInput('adjustComment', 'Additional Comments and Documentation'),
                          actionButton('adjust_ok', 'Accept', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('check-circle')),
                          actionButton('adjust_cancel', 'Cancel', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('window-close'))    ))  })
  
  # Do something with AU Adjustment Modal
  observeEvent(input$adjust_cancel, {removeModal()})
  observeEvent(input$adjust_ok, {
    # Get name and location information from tooMany
    sitesUpdated <- filter(reactive_objects$sitesUnique, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
      st_drop_geometry() %>%
      distinct(FDT_STA_ID, .keep_all = T) %>%
      mutate(ID305B = input$mergeAUID,
             `Buffer Distance` = 'Manual Review',
             Comments = input$adjustComment)
   
    # add the current site(s) to the adjusted list 
    reactive_objects$sitesAdjusted <- bind_rows(reactive_objects$sitesAdjusted, sitesUpdated)
    
    dropMe <- unique(sitesUpdated$FDT_STA_ID)
    
    ## Remove Site from "to do' list
    reactive_objects$tooMany_sites <- filter(reactive_objects$tooMany_sites, !(FDT_STA_ID %in% dropMe)) # drop sites
    reactive_objects$tooMany <- filter(reactive_objects$tooMany, !(`Point Unique Identifier` %in% dropMe)) # drop segments
    
    # Empty map selection
    reactive_objects$namesToSmash <- NULL
    
    ### Clear modal
    removeModal()
  })
  
  observe({
    req(reactive_objects$sitesAdjusted)
    ## Update proxy map
    if(nrow(reactive_objects$sitesAdjusted) > 0){
      if(nrow(reactive_objects$tooMany_sites)> 0){
        palTooMany <- colorNumeric(c('green','yellow', 'blue','red'), domain = reactive_objects$tooMany$colorFac)
        
        map_proxy %>% 
          # have to manually clear old sites to 'wipe' leaflet memory of joined sites
          clearGroup("Stations Snapped to > 1 Segment") %>%
          clearGroup("Segments of Stations Snapped to > 1 Segment") %>%
          addCircleMarkers(data=reactive_objects$sitesAdjusted,
                           layerId = ~FDT_STA_ID,
                           label=~FDT_STA_ID, group="Adjusted Sites", 
                           color='black', fillColor='purple', radius = 5,
                           fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T,
                           popup=leafpop::popupTable(reactive_objects$sitesAdjusted),
                           popupOptions = popupOptions( maxHeight = 100 )) %>%
          addCircleMarkers(data=reactive_objects$tooMany_sites,
                           layerId = ~FDT_STA_ID,
                           label=~FDT_STA_ID, group="Stations Snapped to > 1 Segment", 
                           color='black', fillColor='orange', radius = 5,
                           fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%
          addPolylines(data=reactive_objects$tooMany,
                       layerId = ~ID305B,
                       label=~ID305B, group="Segments of Stations Snapped to > 1 Segment", 
                       color = ~palTooMany(reactive_objects$tooMany$colorFac),weight = 3,stroke=T,
                       popup=leafpop::popupTable(reactive_objects$tooMany),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
          hideGroup("Segments of Stations Snapped to > 1 Segment") %>%
          addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                           overlayGroups = c("Adjusted Sites",
                                             "Stations in the selected Region/Basin",
                                             "Segments of Stations in the selected Region/Basin",
                                             "Stations Snapped to > 1 Segment",
                                             "Segments of Stations Snapped to > 1 Segment",
                                             'Conventionals Stations',"All AUs in selected Region/Basin",'Assessment Regions'),
                           options=layersControlOptions(collapsed=T),
                           position='topleft') 
      } else {
        map_proxy %>% 
          # have to manually clear old sites to 'wipe' leaflet memory of joined sites
          clearGroup("Stations Snapped to > 1 Segment") %>%
          clearGroup("Segments of Stations Snapped to > 1 Segment") %>%
          addCircleMarkers(data=reactive_objects$sitesAdjusted,
                           layerId = ~FDT_STA_ID,
                           label=~FDT_STA_ID, group="Adjusted Sites", 
                           color='black', fillColor='purple', radius = 5,
                           fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T,
                           popup=leafpop::popupTable(reactive_objects$sitesAdjusted),
                           popupOptions = popupOptions( maxHeight = 100 )) %>%
          addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                           overlayGroups = c("Adjusted Sites",
                                             "Stations in the selected Region/Basin",
                                             "Segments of Stations in the selected Region/Basin",
                                             "Stations Snapped to > 1 Segment",
                                             "Segments of Stations Snapped to > 1 Segment",
                                             'Conventionals Stations',"All AUs in selected Region/Basin",'Assessment Regions'),
                           options=layersControlOptions(collapsed=T),
                           position='topleft')}  }    })
        
  
   
  output$test <- renderPrint({
    reactive_objects$tooMany_sites
  })
  
  
  
  output$adjustedStationsTable <- DT::renderDataTable({
    req(reactive_objects$namesToSmash, reactive_objects$sitesAdjusted)
    filter(reactive_objects$sitesAdjusted, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px'))  })
})