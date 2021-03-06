
observeEvent(input$begin, {
  # limit conventionals_DWQS to just chosen subbasin
  reactive_objects$conventionals_DWQS_Region <- st_intersection(conventionals_DWQS, 
                                                                   filter(subbasins, BASIN_CODE %in% basinCodesAU())) %>%
    mutate(`DEQ GIS Web App Link` =  paste0(webLinkpart1, StationID, webLinkpart2, WQSreactive_objects$otherLayers, webLinkpart3)) %>%
    dplyr::select(`DEQ GIS Web App Link`, everything())
  
  
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











## Assessment Unit Review Tab


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




