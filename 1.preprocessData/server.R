source('global.R')

### All conventionals sites
####conventionals_D <- st_read('GIS/conventionals_D.shp') %>%
conventionals_DWQS <- readRDS('data/conventionals_D.RDS') %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) %>%
  mutate(StationID= FDT_STA_ID)


assessmentRegions <- st_read( 'GIS/AssessmentRegions_simple.shp')
assessmentLayer <- st_read('GIS/AssessmentRegions_VA84_basins.shp') %>%
  st_transform( st_crs(4326)) 
subbasins <- st_read('GIS/DEQ_VAHUSB_subbasins_EVJ.shp') %>%
  rename('SUBBASIN' = 'SUBBASIN_1')
#basin7 <- st_read('GIS/deq_basins07.shp') %>%
#  st_transform(4326) %>%
#  mutate(BASIN_CODE = case_when(BASIN_CODE == '3-' ~ '3',
#                                BASIN_CODE == '8-' ~ '8',
#                                BASIN_CODE == '9-' ~ '9',
#                                TRUE ~ as.character(BASIN_CODE)))



shinyServer(function(input, output, session) {
  
########################################################################################################################################################  
  
  ## Assessment Unit side of application 
  
  # color palette for assessment polygons
  pal <- colorFactor(
    palette = topo.colors(7),
    domain = assessmentRegions$ASSESS_REG)
  
  
  # empty reactive objects list
  reactive_objects = reactiveValues() # for AU part of app

  
  ## Watershed Selection Tab Assessment Unit
  
  # Query VAHUC6's By Selectize arguments
  the_data <- reactive({assessmentLayer})
  region_filter <- shiny::callModule(dynamicSelect, "DEQregionSelection", the_data, "ASSESS_REG" )
  basin_filter <- shiny::callModule(dynamicSelect, "basinSelection", region_filter, "Basin" )
  
  # Copy data as observer for download filename
  region <- reactive({req(input$begin) 
    unique(region_filter()$ASSESS_REG)})
  basin <- reactive({req(input$begin)
    unique(basin_filter()$Basin)})
  
  
  # FOR TESTING ############################################################################
#  assessmentType_sf <- eventReactive(input$begin, {
#    req(basin_filter(), input$assessmentType)
#    AUs1}) # for speed #riverineAUs})
#  AUs <- eventReactive(input$begin, { AUs1 })
  
  
  # FOR REAL ################################################################################
#  assessmentType_sf <- eventReactive(input$begin, {
#    req(basin_filter(), input$assessmentType)
#    typeName <- ifelse(input$assessmentType != 'Lacustrine', input$assessmentType, 'Reservoir_Basins')
#    st_read(paste0('data/processedGIS/va_2018_aus_', tolower(typeName),'_', 
#                   region(), "_", basin(), '.shp')) %>%
#    
#   # st_read(paste0('C:/HardDriveBackup/GIS/Assessment/va_2018_aus_',
#  #                                    tolower(typeName),'.shp')) %>%
#      st_transform(4326)})
#  AUs <- eventReactive(input$begin, {
#    req(basin_filter(), input$assessmentType, assessmentType_sf())
#    suppressWarnings(st_intersection(st_zm(assessmentType_sf()), basin_filter()))})
  AUs <- eventReactive(input$begin, {
    req(basin_filter(), input$assessmentType)
    typeName <- ifelse(input$assessmentType != 'Lacustrine', input$assessmentType, 'Reservoir')
    st_zm(
      st_read(paste0('data/processedGIS/va_2018_aus_', tolower(typeName),'_', 
                   region(), "_", basin(), '.shp'))) %>%
      st_transform(4326)})
  
 
  
  ## Map output of basin and assessmentType_sf
  output$VAmap <- renderLeaflet({
    input$begin
  
    m <- mapview(basin_filter(),label= basin_filter()$VAHU6, layer.name = 'VAHU6 in Selected Basin',
                 popup= leafpop::popupTable(basin_filter(), zcol=c('VAHU6',"VaName","VAHU5","ASSESS_REG")))
    m@map %>% setView(st_bbox(basin_filter())$xmax[[1]],st_bbox(basin_filter())$ymax[[1]],zoom = 7)  })
  
  
  ## Table of AUs within Selected Region/Basin
  output$AUSummary <-  DT::renderDataTable({ req(AUs())
    DT::datatable(AUs() %>% st_set_geometry(NULL), rownames = FALSE, 
                  options= list(scrollX = TRUE, pageLength = nrow(AUs()), 
                                scrollY = "300px", dom='t'))   })
  
  
  
  
  ## Assessment Unit Review Tab
  
  observeEvent(input$begin, {
    print(paste0('data/preAnalyzedRegionalAUdata/',
                 unique(region_filter()$ASSESS_REG), '/',
                 input$assessmentType, '/',
                 unique(basin_filter()$Basin), '.RDS'))
    reactive_objects$snap_input <- readRDS(paste0('data/preAnalyzedRegionalAUdata/',
                                                  unique(region_filter()$ASSESS_REG), '/',
                                                  input$assessmentType, '/',
                                                  unique(basin_filter()$Basin), '.RDS'))
    # Make dataset of all sites for highlighting purposes, preliminary list
    reactive_objects$sitesUnique <- reactive_objects$snap_input[['inputSites']] %>%
      mutate(`Point Unique Identifier` = FDT_STA_ID, Comments = NA, fromPreviousAssessment = NA) %>%
      dplyr::select(-c(ID305B)) %>% # column only populated for lakes so dropping
      left_join(st_drop_geometry(reactive_objects$snap_input[['sf_output']]), by = c('Point Unique Identifier')) %>%
      mutate(fromPreviousAssessment = ifelse(is.na(ID305B), 'Yes', 'No'),
        ID305B = ifelse(is.na(ID305B), ID305B_1, as.character(ID305B))) %>% # use snapped ID305B, then fill in with previously attributed data
      dplyr::select(FDT_STA_ID, ID305B, VAHU6, ASSESS_REG, VaName, Basin, Latitude, 
                    Longitude, fromPreviousAssessment, Comments)
    
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
    # Make dataset of sites associated with no many segments
    reactive_objects$snapNone <- filter(reactive_objects$sitesUnique, FDT_STA_ID %in% reactive_objects$snap_input[['tbl_output']]$`Point Unique Identifier`) 
    # Make empty dataset of sites that assessors touched
    reactive_objects$sitesAdjusted <-  reactive_objects$sitesUnique[0,]
    # Make dataset for user to download
    reactive_objects$finalAU <- reactive_objects$sitesUnique %>% st_drop_geometry()
    })
  
  # Bring in conventionals sites, filtered by basin (not including region in case horse trading going on)
  conventionals_D <- reactive({
    req(basin(), input$begin, reactive_objects$sitesUnique)
    st_read(paste0('data/conventionals_D_', unique(basin()), '.shp')) %>%
      # prep for smashing into sitesUnique so these sites can be selected if needed
      mutate(FDT_STA_ID = FDT_STA,
             ID305B = NA,
             VAHU6 = Hc6_Vh6,
             ASSESS_REG = STA_REC,
             VaName = H6_H_12_,
             Latitude = Latitud,
             Longitude = Longitd,
             fromPreviousAssessment = NA,
             Comments = 'Conventionals Station from Basin Filter') %>%
      dplyr::select(FDT_STA_ID, ID305B, VAHU6, ASSESS_REG, VaName, Basin, Latitude, 
                    Longitude, fromPreviousAssessment, Comments) %>%
    filter(!FDT_STA_ID %in% reactive_objects$sitesUnique$FDT_STA_ID) 
  })
  
  observe({
    req(reactive_objects$sitesUnique, conventionals_D())
    reactive_objects$sitesUniqueFin <- rbind(reactive_objects$sitesUnique, conventionals_D())  
  })
  
  # UI summaries of data pulled in to app, first and second tab
  output$singleSnapSummary1 <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$snapSingle), ' stations that snapped to 1 AU segment in preprocessing.'))})
  output$singleSnapSummary2 <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$snapSingle), ' stations that snapped to 1 AU segment in preprocessing.'))})
  output$snapTooManySummary1 <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$tooMany_sites), ' stations that snapped to > 1 AU segment in preprocessing.'))})
  output$snapTooManySummary2 <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$tooMany_sites), ' stations that snapped to > 1 AU segment in preprocessing.'))})
  output$noSnapSummary1 <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$snapNone), ' stations that snapped to 0 AU segments in preprocessing.'))})
  output$noSnapSummary2 <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$snapNone), ' stations that snapped to 0 AU segments in preprocessing.'))})
  output$regionalSitesSummary1 <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$snap_input[['inputSites']]), ' stations in the selected Region/Basin.'))})
  output$regionalSitesSummary2 <- renderPrint({ req(reactive_objects$snap_input)
    cat(paste0('There are ', nrow(reactive_objects$snap_input[['inputSites']]), ' stations in the selected Region/Basin.'))})
  
  # AU Map
  output$AUmap <- renderLeaflet({
    req(reactive_objects$snap_input)
    CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE, 
                 options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20)) %>%
      setView(-78, 37.5, zoom=7)  %>% 
      addCircleMarkers(data = conventionals_D(), color='blue', fillColor='yellow', radius = 4,
                       fillOpacity = 0.5,opacity=0.8,weight = 1,stroke=T, group="Conventionals Stations in Basin",
                       label = ~FDT_STA_ID, layerId = ~FDT_STA_ID) %>%  
                       #popup = leafpop::popupTable(conventionals_D()), # nixed this when allowed for horse trading
                       #popupOptions = popupOptions( maxHeight = 100 )) %>% 
      
      {if("sfc_MULTIPOLYGON" %in% class(st_geometry(AUs()))) 
        addPolygons(., data = AUs(),
                    layerId = ~ID305B,
                    label=~ID305B, group="All AUs in selected Region/Basin", 
                    color = 'blue', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
                    weight = 3,stroke=T,
                    popup=leafpop::popupTable(AUs()),
                    popupOptions = popupOptions( maxHeight = 100 )) %>% 
          hideGroup("All AUs in selected Region/Basin") 
        else addPolylines(., data = AUs(),
                          layerId = ~ID305B,
                          label=~ID305B, group="All AUs in selected Region/Basin", 
                          color = 'blue', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
                          weight = 3,stroke=T,
                          popup=leafpop::popupTable(AUs()),
                          popupOptions = popupOptions( maxHeight = 100 )) %>% 
          hideGroup("All AUs in selected Region/Basin")  } %>%
      
      # first working method
     # addPolylines(data=AUs(),
     #              layerId = ~ID305B,
     #              label=~ID305B, group="All AUs in selected Region/Basin", 
     #              color = 'blue', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
     #              weight = 3,stroke=T,
     #              popup=leafpop::popupTable(AUs()),
     #              popupOptions = popupOptions( maxHeight = 100 )) %>% hideGroup("All AUs in selected Region/Basin") %>%
      addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
                  fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
                  group="Assessment Regions",
                  popup=leafpop::popupTable(assessmentRegions, zcol=c('ASSESS_REG'))) %>% hideGroup('Assessment Regions') %>% #,'VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
      inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
      inlmisc::AddSearchButton(group = "Conventionals Stations in Basin", zoom = 15,propertyName = "label",
                               textPlaceholder = "Search Conventionals Stations in Basin") %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c('Conventionals Stations in Basin',"All AUs in selected Region/Basin",'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') %>%
      hideGroup("Conventionals Stations in Basin")  })
  
  map_proxy <- leafletProxy("AUmap")

  # Add layers to map as requested
  observeEvent(input$plotSingleSnapSummary, {
    if (nrow(reactive_objects$snapSingle) > 0 ){
      map_proxy %>%
        addCircleMarkers(data=reactive_objects$snapSingle,
                         layerId = ~paste0(FDT_STA_ID,'_snapSingle'), # need unique layerID 
                         label=~FDT_STA_ID, group="Stations Snapped to 1 AU segment", 
                         color='black', fillColor='cyan', radius = 5,
                         fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%
        {if("sfc_MULTIPOLYGON" %in% class(st_geometry(reactive_objects$snap_input[['sf_output']]))) 
          addPolygons(., data=filter(reactive_objects$snap_input[['sf_output']],
                                     !(`Point Unique Identifier` %in% reactive_objects$tooMany_sites$FDT_STA_ID)),
                      layerId = ~paste0(ID305B,'_snapSingle'), # need unique layerID 
                      label=~ID305B, group="Segments of Stations that snapped to 1 AU segment", 
                      color = 'cyan',
                      fill = 'cyan', #color = ~palTooMany(tooMany$colorFac),
                      weight = 3,stroke=T,
                      popup=leafpop::popupTable(reactive_objects$snap_input[['sf_output']]),
                      popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("Segments of Stations that snapped to 1 AU segment") 
          else addPolylines(., data=filter(reactive_objects$snap_input[['sf_output']],
                                           !(`Point Unique Identifier` %in% reactive_objects$tooMany_sites$FDT_STA_ID)),
                            layerId = ~paste0(ID305B,'_snapSingle'), # need unique layerID 
                            label=~ID305B, group="Segments of Stations that snapped to 1 AU segment", 
                            color = 'cyan', #color = ~palTooMany(tooMany$colorFac),
                            weight = 3,stroke=T,
                            popup=leafpop::popupTable(reactive_objects$snap_input[['sf_output']]),
                            popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("Segments of Stations that snapped to 1 AU segment") } %>%
       # first working method 
        #addPolylines(data=filter(reactive_objects$snap_input[['sf_output']],
        #                         !(`Point Unique Identifier` %in% reactive_objects$tooMany_sites$FDT_STA_ID)),
        #             layerId = ~paste0(ID305B,'_snapSingle'),  # need unique layerID 
        #             label=~ID305B, group="Segments of Stations that snapped to 1 AU segment", 
        #             color = 'cyan', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
        #             weight = 3,stroke=T,
        #             popup=leafpop::popupTable(reactive_objects$snap_input[['sf_output']]),
        #             popupOptions = popupOptions( maxHeight = 100 )) %>%
        #hideGroup("Segments of Stations that snapped to 1 AU segment") %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 AU segment",
                                           "Segments of Stations that snapped to 1 AU segment",
                                           "Stations Snapped to > 1 Segment",
                                           "Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 Segments",
                                           "All stations in the selected Region/Basin",
                                           'Conventionals Stations in Basin',"All AUs in selected Region/Basin",'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')
    } else {
      showNotification("There are no sites that snapped to only 1 AU segment in preprocessing steps. Nothing to plot.")
    }
     })
  
  observeEvent(input$plotSnapTooManySummary, {
    
    if(nrow(reactive_objects$tooMany) > 0){
      palTooMany <- colorNumeric(c('green','yellow', 'blue','red'), domain = reactive_objects$tooMany$colorFac)
      
      map_proxy %>%
        addCircleMarkers(data=reactive_objects$tooMany_sites,
                         layerId = ~paste0(FDT_STA_ID,'_tooMany'),  # need unique layerID 
                         label=~FDT_STA_ID, group="Stations Snapped to > 1 Segment", 
                         color='black', fillColor='orange', radius = 5,
                         fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%
        addPolylines(data=reactive_objects$tooMany,
                     layerId = ~paste0(ID305B,'_tooMany'),  # need unique layerID 
                     label=~ID305B, group="Segments of Stations Snapped to > 1 Segment", 
                     color = ~palTooMany(reactive_objects$tooMany$colorFac),weight = 3,stroke=T,
                     popup=leafpop::popupTable(reactive_objects$tooMany),
                     popupOptions = popupOptions( maxHeight = 100 )) %>%
        hideGroup("Segments of Stations Snapped to > 1 Segment") %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 AU segment",
                                           "Segments of Stations that snapped to 1 AU segment",
                                           "Stations Snapped to > 1 Segment",
                                           "Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 Segments",
                                           "All stations in the selected Region/Basin",
                                           'Conventionals Stations in Basin',"All AUs in selected Region/Basin",'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') 
    } else {
      showNotification("There are no sites that snapped to > 1 AU segment in preprocessing steps. Nothing to plot.")
    }   })
  
  observeEvent(input$plotNoSnapSummary, {
    if (nrow(reactive_objects$snapNone) > 0 ){
      map_proxy %>%
        addCircleMarkers(data=reactive_objects$snapNone,
                         layerId = ~paste0(FDT_STA_ID,'_snapNone'), # need unique layerID 
                         label=~FDT_STA_ID, 
                         group="Stations Snapped to 0 Segments", 
                         color='black', fillColor='yellow', radius = 5,
                         fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 AU segment",
                                           "Segments of Stations that snapped to 1 AU segment",
                                           "Stations Snapped to > 1 Segment",
                                           "Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 Segments",
                                           "All stations in the selected Region/Basin",
                                           'Conventionals Stations in Basin',"All AUs in selected Region/Basin",'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')
    } else {
      showNotification("There are no sites that snapped to 0 AU segments in preprocessing steps. Nothing to plot.")
    }
  })
  
  observeEvent(input$plotRegionalSitesSummary, {
    map_proxy %>%
      addCircleMarkers(data=reactive_objects$sitesUnique,
                       layerId = ~paste0(FDT_STA_ID,'_sitesUnique'),  # need unique layerID 
                       label=~FDT_STA_ID, group="All stations in the selected Region/Basin", 
                       color='black', fillColor='red', radius = 5,
                       fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%#, memory issues
      #popup = leafpop::popupTable(reactive_objects$snapSingle),
      #popupOptions = popupOptions( maxHeight = 100 )) %>%
      addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                       overlayGroups = c("Adjusted Sites",
                                         "Stations Snapped to 1 AU segment",
                                         "Segments of Stations that snapped to 1 AU segment",
                                         "Stations Snapped to > 1 Segment",
                                         "Segments of Stations Snapped to > 1 Segment",
                                         "Stations Snapped to 0 Segments",
                                         "All stations in the selected Region/Basin",
                                         'Conventionals Stations in Basin',"All AUs in selected Region/Basin",'Assessment Regions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft') })
  
  
  
  
  # Map marker click (to identify selected sites, will also select sites w/ identical (round(lat/long, 4) but different names
  observeEvent(input$AUmap_marker_click, {
    site_click <- input$AUmap_marker_click # this is all the info based on your click
    siteid <- strsplit(site_click$id, "_")[[1]][1] # this is just the layerID associated with your click
    # have to remove the unique layerID after _ to make sense of StationID
   
    if(!is.null(siteid)){ # if you clicked a point with info, find all Stations that match (with a round)
      # first find site matches from user input dataset, by lat and long
      siteMatches <- filter(reactive_objects$sitesUniqueFin, 
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
        addCircleMarkers(data=filter(reactive_objects$sitesUniqueFin, FDT_STA_ID %in% reactive_objects$namesToSmash),
                         
#        addCircleMarkers(data=filter(reactive_objects$snap_input[['inputSites']], FDT_STA_ID %in% reactive_objects$namesToSmash),
                         layerId = ~paste0(FDT_STA_ID,'_sitesHighlighted'),  # need unique layerID 
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
    filter(reactive_objects$sitesUniqueFin, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
      st_drop_geometry() %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px'))  })
  
  output$associatedAUTable <- DT::renderDataTable({
    req(reactive_objects$namesToSmash)
    filter(reactive_objects$AUsegments, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
      st_drop_geometry() %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '200px'))  })
  
  
  ## Accept Snapped AU Modal
  observeEvent(input$accept, {
    showModal(modalDialog(title = 'Accept Snapped AU', size = 'l',
                          DT::renderDataTable({
                            filter(reactive_objects$sitesUniqueFin, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
                              st_drop_geometry() %>%
                              datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '125px'))  }),
                          br(), br(),
                          textInput('acceptComment', 'Additional Comments and Documentation'),
                          actionButton('accept_ok', 'Accept', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('check-circle')),
                          actionButton('accept_cancel', 'Cancel', 
                                       style='color: #fff; background-color: #337ab7; border-color: #2e6da4;font-size:120%', 
                                       icon=icon('window-close'))    ))  })
  
  # Do something with AU Accept Modal
  observeEvent(input$accept_cancel, {removeModal()})
  observeEvent(input$accept_ok, {
    # Get name and location information from tooMany
    sitesUpdated <- filter(reactive_objects$sitesUniqueFin, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
      #st_drop_geometry() %>%
      distinct(FDT_STA_ID, .keep_all = T) %>%
      mutate(#`Buffer Distance` = 'Manual Review', # may not need column anymore
             Comments = paste0('Manual Accept | ',input$acceptComment))
    
    # add the current site(s) to the adjusted list 
    #if(nrow(reactive_objects$sitesAdjusted) == 0){
    #  reactive_objects$sitesAdjusted
    #} else {
    reactive_objects$sitesAdjusted <- rbind(reactive_objects$sitesAdjusted, sitesUpdated) # rbind works better for sf objects
    #}
    
    dropMe <- unique(sitesUpdated$FDT_STA_ID)
    
    ## Remove Site from "to do' list
    reactive_objects$tooMany_sites <- filter(reactive_objects$tooMany_sites, !(FDT_STA_ID %in% dropMe)) # drop sites
    reactive_objects$tooMany <- filter(reactive_objects$tooMany, !(`Point Unique Identifier` %in% dropMe)) # drop segments
    
    # and if part of snap to 1 AU, fix that data
    reactive_objects$snapSingle <- filter(reactive_objects$snapSingle, !(FDT_STA_ID %in% dropMe)) # drop sites
    #reactive_objects$snap_input[['sf_output']] <- filter(reactive_objects$snap_input[['sf_output']], !(`Point Unique Identifier` %in% dropMe)) # drop segments
    # whole map regenerates if I fix above
    
    # and if part of snap to 1 AU, fix that data
    reactive_objects$snapNone <- filter(reactive_objects$snapNone, !(FDT_STA_ID %in% dropMe)) # drop sites
    
    # update output dataset
    reactive_objects$finalAU <- filter(reactive_objects$finalAU, !(FDT_STA_ID %in% dropMe)) %>%
      bind_rows(sitesUpdated)
    
    
    # Empty map selection
    reactive_objects$namesToSmash <- NULL
    
    ### Clear modal
    removeModal()
  })
  
  
  
  
  
  ## Manual AU Adjustment Modal
  observeEvent(input$changeAU, {
    showModal(modalDialog(title = 'Manually Adjust AUs', size = 'l',
                          DT::renderDataTable({
                            filter(reactive_objects$sitesUniqueFin, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
                              st_drop_geometry() %>%
                              datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '125px'))  }),
                          br(), br(),
                          selectInput('mergeAUID','Choose AU to connect to station', 
                                      choices = c(as.character(unique(filter(reactive_objects$tooMany, `Point Unique Identifier` %in% reactive_objects$namesToSmash)$ID305B)), # likely AUs
                                                  unique(as.character(AUs()$ID305B)))), # less likely AUs but an option
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
    sitesUpdated <- filter(reactive_objects$sitesUniqueFin, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
      #st_drop_geometry() %>%
      distinct(FDT_STA_ID, .keep_all = T) %>%
      mutate(ID305B = input$mergeAUID,
             #`Buffer Distance` = 'Manual Review', # may not need column anymore
             Comments = paste0('Manual Review | ',input$adjustComment))
   
    # add the current site(s) to the adjusted list 
    #if(nrow(reactive_objects$sitesAdjusted) == 0){
    #  reactive_objects$sitesAdjusted
    #} else {
      reactive_objects$sitesAdjusted <- rbind(reactive_objects$sitesAdjusted, sitesUpdated) # rbind works better for sf objects
    #}
    dropMe <- unique(sitesUpdated$FDT_STA_ID)
    
    ## Remove Site from "to do' list
    reactive_objects$tooMany_sites <- filter(reactive_objects$tooMany_sites, !(FDT_STA_ID %in% dropMe)) # drop sites
    reactive_objects$tooMany <- filter(reactive_objects$tooMany, !(`Point Unique Identifier` %in% dropMe)) # drop segments
    
    # and if part of snap to 1 AU, fix that data
    reactive_objects$snapSingle <- filter(reactive_objects$snapSingle, !(FDT_STA_ID %in% dropMe)) # drop sites
    #reactive_objects$snap_input[['sf_output']] <- filter(reactive_objects$snap_input[['sf_output']], !(`Point Unique Identifier` %in% dropMe)) # drop segments
    # whole map regenerates if I fix above
    
    # and if part of snap to 1 AU, fix that data
    reactive_objects$snapNone <- filter(reactive_objects$snapNone, !(FDT_STA_ID %in% dropMe)) # drop sites

    # update output dataset
    reactive_objects$finalAU <- filter(reactive_objects$finalAU, !(FDT_STA_ID %in% dropMe)) %>%
      bind_rows(sitesUpdated)
      
    
    # Empty map selection
    reactive_objects$namesToSmash <- NULL
    
    ### Clear modal
    removeModal()
  })
  
  # update AUmap after AU adjustment
  observe({
    req(reactive_objects$sitesAdjusted)
    if(nrow(reactive_objects$tooMany_sites)> 0){
      palTooMany <- colorNumeric(c('green','yellow', 'blue','red'), domain = reactive_objects$tooMany$colorFac) }
    
    ## Update proxy map
    if(nrow(reactive_objects$sitesAdjusted) > 0){
      map_proxy %>% 
        # have to manually clear old sites to 'wipe' leaflet memory of joined sites
        clearGroup("Stations Snapped to 0 Segments") %>%
        clearGroup("Stations Snapped to 1 AU segment") %>%
        clearGroup("Stations Snapped to > 1 Segment") %>%
        clearGroup("Segments of Stations Snapped to > 1 Segment") %>%
        
        {if(nrow(reactive_objects$tooMany_sites)> 0)
          addCircleMarkers(., data=reactive_objects$tooMany_sites,
                           layerId = ~paste0(FDT_STA_ID,'_tooMany'),  # need unique layerID 
                           label=~FDT_STA_ID, group="Stations Snapped to > 1 Segment", 
                           color='black', fillColor='orange', radius = 5,
                           fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%
            addPolylines(., data=reactive_objects$tooMany,
                         layerId = ~paste0(ID305B,'_tooMany'),  # need unique layerID
                         label=~ID305B, group="Segments of Stations Snapped to > 1 Segment", 
                         color = ~palTooMany(reactive_objects$tooMany$colorFac),weight = 3,stroke=T,
                         popup=leafpop::popupTable(reactive_objects$tooMany),
                         popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("Segments of Stations Snapped to > 1 Segment")
          else . } %>%
        {if(nrow(reactive_objects$snapSingle) > 0)
          addCircleMarkers(., data=reactive_objects$snapSingle,
                           layerId = ~paste0(FDT_STA_ID,'_snapSingle'), # need unique layerID 
                           label=~FDT_STA_ID, group="Stations Snapped to 1 AU segment", 
                           color='black', fillColor='cyan', radius = 5,
                           fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T)
          else .}  %>%
        {if("sfc_MULTIPOLYGON" %in% class(st_geometry(reactive_objects$snap_input[['sf_output']]))) 
          addPolygons(., data=filter(reactive_objects$snap_input[['sf_output']],
                                  !(`Point Unique Identifier` %in% reactive_objects$tooMany_sites$FDT_STA_ID)),
                      layerId = ~paste0(ID305B,'_snapSingle'), # need unique layerID 
                      label=~ID305B, group="Segments of Stations that snapped to 1 AU segment", 
                      color = 'cyan',
                      fill = 'cyan', #color = ~palTooMany(tooMany$colorFac),
                      weight = 3,stroke=T,
                      popup=leafpop::popupTable(reactive_objects$snap_input[['sf_output']]),
                      popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("Segments of Stations that snapped to 1 AU segment") 
          else addPolylines(., data=filter(reactive_objects$snap_input[['sf_output']],
                                        !(`Point Unique Identifier` %in% reactive_objects$tooMany_sites$FDT_STA_ID)),
                            layerId = ~paste0(ID305B,'_snapSingle'), # need unique layerID 
                            label=~ID305B, group="Segments of Stations that snapped to 1 AU segment", 
                            color = 'cyan', #color = ~palTooMany(tooMany$colorFac),
                            weight = 3,stroke=T,
                            popup=leafpop::popupTable(reactive_objects$snap_input[['sf_output']]),
                            popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("Segments of Stations that snapped to 1 AU segment") } %>%
        {if(nrow(reactive_objects$snapNone) > 0)
          addCircleMarkers(., data=reactive_objects$snapNone,
                           layerId = ~paste0(FDT_STA_ID,'_snapNone'), # need unique layerID 
                           label=~FDT_STA_ID, 
                           group="Stations Snapped to 0 Segments", 
                           color='black', fillColor='yellow', radius = 5,
                           fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) 
          else . } %>%
        addCircleMarkers(data=reactive_objects$sitesAdjusted,
                         layerId = ~paste0(FDT_STA_ID,'_sitesAdjusted'),  # need unique layerID 
                         label=~FDT_STA_ID, group="Adjusted Sites", 
                         color='black', fillColor='purple', radius = 5,
                         fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>% 
        addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                         overlayGroups = c("Adjusted Sites",
                                           "Stations Snapped to 1 AU segment",
                                           "Segments of Stations that snapped to 1 AU segment",
                                           "Stations Snapped to > 1 Segment",
                                           "Segments of Stations Snapped to > 1 Segment",
                                           "Stations Snapped to 0 Segments",
                                           "All stations in the selected Region/Basin",
                                           'Conventionals Stations in Basin',"All AUs in selected Region/Basin",'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') 
    }    })
  
  ## User adjusted table 
  output$adjustedStationsTable <- DT::renderDataTable({
    req(reactive_objects$namesToSmash, reactive_objects$sitesAdjusted)
    filter(reactive_objects$sitesAdjusted, FDT_STA_ID %in% reactive_objects$namesToSmash) %>%
      datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px'))  })
  
  
  
  ## Checkout changes
  observeEvent(input$checkMeOut, {
    showModal(modalDialog(title = 'Download', size = 'l',
                          h4('All stations that were accepted or adjusted'),
                          DT::renderDataTable({
                            reactive_objects$sitesAdjusted %>%
                              st_drop_geometry() %>%
                              datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '300px'))  }),
                          br(), br(),
                          h4('Final AU results that will be downloaded'),
                          DT::renderDataTable({
                            reactive_objects$finalAU %>%
                              datatable(rownames = F, options = list(dom = 'ft', scrollX= TRUE, scrollY = '300px'))  })
                           ))  })
  
  ## Download Changes
  export_file=reactive(paste0('IR2022_AUreview_', region(), '_', basin(),'_',input$assessmentType, '_', Sys.Date(),'.csv'))
  output$downloadAU <- downloadHandler(
    filename=function(){export_file()},
    content = function(file) {
      write.csv(reactive_objects$finalAU %>%
                  # get rid of geometry if needed
                  {if('geometry' %in% names(reactive_objects$finalAU))
                    dplyr::select(., -geometry) 
                    else . } %>%
                  as.data.frame(), file, row.names = F) }) 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
##################################################################################################################################################
  
  ## WQS Side of Application
  
  palBufferDistance <- colorFactor(
    palette = terrain.colors(5),#colorRamps::blue2red(5),
    levels = c("20 m", "40 m", "60 m", "80 m", "No connections within 80 m"))
  
  
  # empty reactive objects list
  WQSreactive_objects = reactiveValues() # for WQS
  
  ## Watershed Selection Tab WQS
  
  ################################# PRE-SPLIT WQS LAYER METHOD, FASTER FOR APP RENDERING ######################################
  
  # Update map Subbasin based on user selection
  output$WQSDEQregionSelection_ <- renderUI({
    req(input$WQSwaterbodyType)
    op <- filter(subbasinOptionsByWQStype, waterbodyType %in% input$WQSwaterbodyType) %>%
      distinct(AssessmentRegion) %>% 
      pull()
    selectInput("WQSDEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE,
                choices= sort(op))  })
  
  output$WQSsubbasinSelection_ <- renderUI({
    req(input$WQSwaterbodyType, input$WQSDEQregionSelection)
    op <- filter(subbasinOptionsByWQStype, waterbodyType %in% input$WQSwaterbodyType) %>%
      filter(AssessmentRegion %in% input$WQSDEQregionSelection) %>%
      distinct(Basin_Code) %>% 
      pull() 
    selectInput("WQSsubbasinSelection", "Select Subbasin", multiple = FALSE,
                choices= sort(op))  })
  
  output$WQSbegin_ <- renderUI({
    req(input$WQSwaterbodyType, input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    actionButton('WQSbegin', HTML("Begin Review With Subbasin Selection <br/>(Retrieves Last Saved Result)"),
                 class='btn-block')  })
  
  basinCodes <- reactive({
    req(input$WQSwaterbodyType, input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    filter(subbasinOptionsByWQStype, waterbodyType %in% input$WQSwaterbodyType) %>%
      filter(AssessmentRegion %in% input$WQSDEQregionSelection) %>%
      filter(Basin_Code %in% input$WQSsubbasinSelection) %>%
      distinct(SubbasinOptions) %>% 
      pull() })
  
  WQSs <- eventReactive(input$WQSbegin, {
    req(input$WQSwaterbodyType, input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    
    typeName <- filter(WQSlayerConversion, waterbodyType %in% input$WQSwaterbodyType) %>%
      distinct(WQS_ID) %>% 
      pull() 
    
    withProgress(message = 'Reading in Large Spatial File',
                 st_zm(
                   st_read(paste0('GIS/processedWQS/',typeName[1],'_', basinCodes(), '.shp') , fid_column_name = "OBJECTID")) ) %>%
                   st_transform(4326)  })
  
  WQSsEL <- reactive({
    req(WQSs(), input$WQSwaterbodyType == "Estuarine")
    typeName <- filter(WQSlayerConversion, waterbodyType %in% input$WQSwaterbodyType) %>%
      distinct(WQS_ID) %>% 
      pull() 
    
    withProgress(message = 'Reading in Additional Estuarine Spatial File',
                 st_zm(
                   st_read(paste0('GIS/processedWQS/',typeName[2],'_', basinCodes(), '.shp') , fid_column_name = "OBJECTID")) ) %>%
      st_transform(4326) %>%
      # match polygon structure
      rename('WQS_COMMEN' = 'WQS_COMMENT') %>% # match polygon structure
      mutate(Shape_Area = NA) %>%
      dplyr::select(names(WQSs()))  })
  
  
  # Make an object (once per Subbasin filter) that encompasses all WQS_ID options for said subbasin for manual WQS_ID adjustment modal, speeds rendering
  WQS_ID_subbasinOptions <- reactive({req(WQSs())
    if(input$WQSwaterbodyType != "Estuarine"){
      as.character(WQSs()$WQS_ID)
    } else {  c(as.character(WQSs()$WQS_ID), as.character(WQSsEL()$WQS_ID))   }      })
  
  
  ## Map output of selected subbasin
  output$WQSVAmap <- renderLeaflet({
    req(input$WQSbegin, WQSs(), input$WQSDEQregionSelection, input$WQSsubbasinSelection)
    subbasins <- filter(subbasins, BASIN_CODE %in% as.character(basinCodes())) %>%
      filter(ASSESS_REG %in% input$WQSDEQregionSelection)
    
    m <- mapview( subbasins, label= subbasins$SUBBASIN, layer.name = 'Selected Subbasin',
                  popup= leafpop::popupTable(subbasins, zcol=c('BASIN_NAME', 'BASIN_CODE', 'SUBBASIN', 'ASSESS_REG', 'VAHU6_NOTE')))
    m@map %>% setView(st_bbox(subbasins)$xmax[[1]],st_bbox(subbasins)$ymax[[1]],zoom = 7)  })
  
  ### WQS reactive 
  observeEvent(input$WQSbegin, {
    # Bring in existing WQS information
    WQSreactive_objects$WQSlookup <- loadData("WQSlookupTable")
    # limit conventionals_DWQS to just chosen subbasin
    WQSreactive_objects$conventionals_DWQS_Region <- st_intersection(conventionals_DWQS, 
                                                                     filter(subbasins, BASIN_CODE %in% basinCodes()))
    # All sites limited to waterbody type and subbasin
    WQSreactive_objects$snap_input <- readRDS('data/processedWQS/WQStable.RDS') %>%
      filter(str_extract(WQS_ID, "^.{2}") %in% filter(WQSlayerConversion, waterbodyType %in% input$WQSwaterbodyType)$WQS_ID) %>%
      filter(gsub("_","",str_extract(WQS_ID, ".{3}_")) %in% 
               str_pad(unique(filter(subbasinOptionsByWQStype, SubbasinOptions %in% basinCodes())$SubbasinOptions), 
                       width = 2, side = 'left', pad = '0')) %>%
      # filter out any sites that happen to have existing WQS_ID
      filter(! StationID %in% WQSreactive_objects$WQSlookup$StationID) %>%
      group_by(StationID) %>%
      mutate(n = n()) %>% ungroup()
    # Sites limited to just region of interest
    WQSreactive_objects$snap_input_Region <- WQSreactive_objects$snap_input %>%
      left_join(WQSreactive_objects$conventionals_DWQS_Region, by = 'StationID') %>%
      filter(!is.na(Latitude) | !is.na(Longitude)) %>%
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
      filter(!is.na(Latitude) | !is.na(Longitude)) %>%
      st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
               remove = F, # don't remove these lat/lon cols from df
               crs = 4326)
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
        dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())    
    } else {WQSreactive_objects$tooMany_sf_EL <- WQSs()[0,]} # create dummy variable
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
        {if(nrow(WQSreactive_objects$tooMany_sf) > 0 & "sfc_MULTIPOLYGON" %in% class(st_geometry(WQSreactive_objects$tooMany_sf))) 
          addPolygons(., data=WQSreactive_objects$tooMany_sf,
                      layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                      label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                      color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                      popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                      popupOptions = popupOptions( maxHeight = 100 )) %>% 
            hideGroup("All WQS in selected Region/Basin")
          else . } %>%
        {if(nrow(WQSreactive_objects$tooMany_sf) > 0 & "sfc_MULTILINESTRING" %in% class(st_geometry(WQSreactive_objects$tooMany_sf)))
          addPolylines(., data=WQSreactive_objects$tooMany_sf,
                       layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                       label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                       color = ~palTooMany(WQSreactive_objects$tooMany_sf$colorFac),weight = 3,stroke=T,
                       popup=leafpop::popupTable(WQSreactive_objects$tooMany_sf),
                       popupOptions = popupOptions( maxHeight = 100 )) %>%
            hideGroup("WQS Segments of Stations Snapped to > 1 Segment")
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
                       filter(WQSreactive_objects$snap_input, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) ) 
        else . } %>%
      #st_drop_geometry() %>%
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
                       filter(WQSreactive_objects$sitesAdjusted, StationID %in% WQSreactive_objects$namesToSmash)$WQS_ID) )
        else . } %>%
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