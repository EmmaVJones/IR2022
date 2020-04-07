# update AUmap after AU adjustment
observe({
  req(reactive_objects$sitesAdjusted)
  ## Update proxy map
  if(nrow(reactive_objects$sitesAdjusted) > 0){
    if(nrow(reactive_objects$tooMany_sites)> 0){
      palTooMany <- colorNumeric(c('green','yellow', 'blue','red'), domain = reactive_objects$tooMany$colorFac)
      
      map_proxy %>% 
        # have to manually clear old sites to 'wipe' leaflet memory of joined sites
        clearGroup("Stations Snapped to 0 Segments") %>%
        clearGroup("Stations Snapped to > 1 Segment") %>%
        clearGroup("Segments of Stations Snapped to > 1 Segment") %>%
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
        
        #addPolylines(data=filter(reactive_objects$snap_input[['sf_output']],
        #                         !(`Point Unique Identifier` %in% reactive_objects$tooMany_sites$FDT_STA_ID)),
        #             layerId = ~paste0(ID305B,'_snapSingle'),  # need unique layerID 
        #             label=~ID305B, group="Segments of Stations that snapped to 1 AU segment", 
        #             color = 'cyan', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
        #             weight = 3,stroke=T,
        #             popup=leafpop::popupTable(reactive_objects$snap_input[['sf_output']]),
        #             popupOptions = popupOptions( maxHeight = 100 )) %>%
        {if(nrow(reactive_objects$snapNone) > 0)
          addCircleMarkers(data=reactive_objects$snapNone,
                           layerId = ~paste0(FDT_STA_ID,'_snapNone'), # need unique layerID 
                           label=~FDT_STA_ID, 
                           group="Stations Snapped to 0 Segments", 
                           color='black', fillColor='yellow', radius = 5,
                           fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T)
          else .} %>%
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
                                           'Conventionals Stations',"All AUs in selected Region/Basin",'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft') 
    }  else {
      map_proxy %>% 
        # have to manually clear old sites to 'wipe' leaflet memory of joined sites
        clearGroup("Stations Snapped to > 1 Segment") %>%
        clearGroup("Segments of Stations Snapped to > 1 Segment") %>%
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
                                           'Conventionals Stations',"All AUs in selected Region/Basin",'Assessment Regions'),
                         options=layersControlOptions(collapsed=T),
                         position='topleft')}  }    })