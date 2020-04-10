
# testing stuff

conventionals_D <- st_read(paste0('data/conventionals_D_James River Basin.shp'))


snap_input <- snapList_AU

sitesUnique1 <- snap_input[['inputSites']] %>%
  mutate(`Point Unique Identifier` = FDT_STA_ID,
         Comments = NA, fromPreviousAssessment = NA) %>%
  dplyr::select(-c(ID305B)) %>% # column only populated for lakes so dropping
  left_join(st_drop_geometry(snap_input[['sf_output']]), by = c('Point Unique Identifier')) %>%
  mutate(fromPreviousAssessment = ifelse(is.na(ID305B), 'Yes', 'No'),
         ID305B = ifelse(is.na(ID305B), ID305B_1, as.character(ID305B))) %>% #use snapped ID305B, then fill in with previously attributed data
  dplyr::select(FDT_STA_ID, ID305B, VAHU6, ASSESS_REG, VaName, Basin, Latitude, 
                Longitude, fromPreviousAssessment, Comments)

# make option to select sites in basin that did not make it past assessment region spatial filter (horse trade sites)
conventionals_Dconversion <- mutate(conventionals_D, FDT_STA_ID = FDT_STA,
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
  filter(!FDT_STA_ID %in% sitesUnique1$FDT_STA_ID)

sitesUnique <- rbind(sitesUnique1, conventionals_Dconversion)  
                                    
                                    

AUsegments <- snap_input[['sf_output']] %>%
  mutate(FDT_STA_ID = `Point Unique Identifier`) %>%
  dplyr::select(ID305B, FDT_STA_ID, `Buffer Distance`, everything())

tooMany <- snapCheck(snap_input[['sf_output']] %>%
                       mutate(FDT_STA_ID = `Point Unique Identifier`) %>%
                       group_by(FDT_STA_ID) %>%
                       mutate(colorFac = row_number()) %>% ungroup())

tooMany_sites <- filter(snap_input[['inputSites']], FDT_STA_ID %in% tooMany$`Point Unique Identifier`)

snapSingle <- filter(sitesUnique, FDT_STA_ID %in% snap_input[['sf_output']]$`Point Unique Identifier`) %>%
  filter(!(FDT_STA_ID %in% tooMany_sites$FDT_STA_ID))# filter out sites that attached to more than one segment

snapNone <- filter(sitesUnique, FDT_STA_ID %in% snap_input[['tbl_output']]$`Point Unique Identifier`) 

sitesAdjusted <-  sitesUnique[0,]

AUs <- AUs1




proxy <- CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE, 
             options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20)) %>%
  setView(-78, 37.5, zoom=7)  %>% 
  addCircleMarkers(data = conventionals_D, color='blue', fillColor='yellow', radius = 4,
                   fillOpacity = 0.5,opacity=0.5,weight = 1,stroke=T, group="Conventionals Stations",
                   label = ~FDT_STA, layerId = ~FDT_STA, 
                   popup = leafpop::popupTable(conventionals_D),
                   popupOptions = popupOptions( maxHeight = 100 )) %>% 
#  addPolylines(data=AUs,
#               layerId = ~ID305B,
#               label=~ID305B, group="All AUs in selected Region/Basin", 
#               color = 'blue', #color = ~palTooMany(tooMany$colorFac),
#               weight = 3,stroke=T,
#               popup=leafpop::popupTable(AUs),
#               popupOptions = popupOptions( maxHeight = 100 )) %>% hideGroup("All AUs in selected Region/Basin") %>%
#  addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
#              fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
#              group="Assessment Regions",
#              popup=leafpop::popupTable(assessmentRegions, zcol=c('ASSESS_REG'))) %>% hideGroup('Assessment Regions') %>% #,'VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
#  inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
#  inlmisc::AddSearchButton(group = "Conventionals Stations", zoom = 15,propertyName = "label",
#                           textPlaceholder = "Search Conventionals Stations") %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c('Conventionals Stations',"All AUs in selected Region/Basin",'Assessment Regions'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft') %>%
  hideGroup("Conventionals Stations") 


proxy %>%
  { if("sfc_MULTIPOLYGON" %in% class(st_geometry(snap_input[['sf_output']]))) 
    addPolygons(., data=filter(snap_input[['sf_output']],
                               !(`Point Unique Identifier` %in% tooMany_sites$FDT_STA_ID))) 
    else addPolylines(., data=filter(snap_input[['sf_output']],
                                     !(`Point Unique Identifier` %in% tooMany_sites$FDT_STA_ID)))} %>%
  addCircleMarkers(data=snapSingle,
                   layerId = ~FDT_STA_ID,
                   label=~FDT_STA_ID, group="Stations in the selected Region/Basin", 
                   color='black', fillColor='cyan', radius = 5,
                   fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) 
  
  
  

proxy %>%
  addCircleMarkers(data=snapSingle,
                   layerId = ~FDT_STA_ID,
                   label=~FDT_STA_ID, group="Stations in the selected Region/Basin", 
                   color='black', fillColor='cyan', radius = 5,
                   fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%#, memory issues
  {if("sfc_MULTIPOLYGON" %in% class(st_geometry(snap_input[['sf_output']]))) 
    addPolygons(., data=filter(snap_input[['sf_output']],
                          !(`Point Unique Identifier` %in% tooMany_sites$FDT_STA_ID)),
              layerId = ~paste0(ID305B,'_snapSingle'), # need unique layerID 
              label=~ID305B, group="Segments of Stations in the selected Region/Basin", 
              color = 'cyan',
              fill = 'cyan', #color = ~palTooMany(tooMany$colorFac),
              weight = 3,stroke=T,
              popup=leafpop::popupTable(snap_input[['sf_output']]),
              popupOptions = popupOptions( maxHeight = 100 )) %>%
      hideGroup("Segments of Stations in the selected Region/Basin")
    else addPolylines(., data=filter(snap_input[['sf_output']],
                                    !(`Point Unique Identifier` %in% tooMany_sites$FDT_STA_ID)),
                        layerId = ~paste0(ID305B,'_snapSingle'), # need unique layerID 
                        label=~ID305B, group="Segments of Stations in the selected Region/Basin", 
                        color = 'cyan', #color = ~palTooMany(tooMany$colorFac),
                        weight = 3,stroke=T,
                        popup=leafpop::popupTable(snap_input[['sf_output']]),
                        popupOptions = popupOptions( maxHeight = 100 )) } %>%
    addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c("Stations in the selected Region/Basin",
                                     "Segments of Stations in the selected Region/Basin",
                                     "Stations Snapped to > 1 Segment",
                                     "Segments of Stations Snapped to > 1 Segment",
                                     'Conventionals Stations',"All AUs in selected Region/Basin",'Assessment Regions'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft')


palTooMany <- colorNumeric(c('green','yellow', 'blue','red'), domain = tooMany$colorFac)

proxy %>%
  addCircleMarkers(data=tooMany_sites,
                   layerId = ~FDT_STA_ID,
                   label=~FDT_STA_ID, group="Stations Snapped to > 1 Segment", 
                   color='black', fillColor='orange', radius = 5,
                   fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%
  addPolylines(data=tooMany,
               layerId = ~ID305B,
               label=~ID305B, group="Segments of Stations Snapped to > 1 Segment", 
               color = ~palTooMany(tooMany$colorFac),weight = 3,stroke=T,
               popup=leafpop::popupTable(tooMany),
               popupOptions = popupOptions( maxHeight = 100 )) %>%
  hideGroup("Segments of Stations Snapped to > 1 Segment") %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c("Stations in the selected Region/Basin",
                                     "Segments of Stations in the selected Region/Basin",
                                     "Stations Snapped to > 1 Segment",
                                     "Segments of Stations Snapped to > 1 Segment",
                                     'Conventionals Stations',"All AUs in selected Region/Basin",'Assessment Regions'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft')


proxy %>%
  addCircleMarkers(data=snapNone,
                   layerId = ~FDT_STA_ID,
                   label=~FDT_STA_ID, group="Stations Snapped to 0 Segments", 
                   color='black', fillColor='yellow', radius = 5,
                   fillOpacity = 0.5,opacity=0.5,weight = 2,stroke=T) %>%#, memory issues
  hideGroup("Segments of Stations in the selected Region/Basin") %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c("Stations in the selected Region/Basin",
                                     "Segments of Stations in the selected Region/Basin",
                                     "Stations Snapped to > 1 Segment",
                                     "Stations Snapped to 0 Segments",
                                     "Segments of Stations Snapped to > 1 Segment",
                                     'Conventionals Stations',"All AUs in selected Region/Basin",'Assessment Regions'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft')


namesToSmash <- '2-JOB000.39'#'2-LMC001.37'

filter(sitesUnique, FDT_STA_ID %in% namesToSmash) %>%
  st_drop_geometry() %>%
  datatable(rownames = F, options = list(dom = 't', scrollX= TRUE, scrollY = '100px'))




sitesUpdated <- filter(sitesUnique, FDT_STA_ID %in% namesToSmash) %>%
  #st_drop_geometry() %>%
  distinct(FDT_STA_ID, .keep_all = T) %>%
  mutate(ID305B = 'VAW-I26R_LMC01A00',#input$mergeAUID,
         #`Buffer Distance` = 'Manual Review', # may not need column anymore
         Comments = paste0('Manual Review | ','test'))#input$adjustComment))

# add the current site(s) to the adjusted list 
sitesAdjusted <- rbind(sitesAdjusted, sitesUpdated) # rbind works better for sf objects

dropMe <- unique(sitesUpdated$FDT_STA_ID)

## Remove Site from "to do' list
tooMany_sites <- filter(tooMany_sites, !(FDT_STA_ID %in% dropMe)) # drop sites
tooMany <- filter(tooMany, !(`Point Unique Identifier` %in% dropMe)) # drop segments

snapNone <- filter(snapNone, !(FDT_STA_ID %in% dropMe)) # drop sites



rm(sitesUnique); rm(AUsegments); rm(tooMany); rm(tooMany_sites); rm(snapSingle); rm(sitesAdjusted)
rm(AUs); rm(proxy); rm(namesToSmash); rm(sitesAdjusted); rm(dropMe); rm(sitesUpdated);
rm(snap_input); rm(snapNone)
