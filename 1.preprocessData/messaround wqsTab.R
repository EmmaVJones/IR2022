
st_layers('GIS/WQS_layers_05082020.gdb')



test <- st_zm(st_read('GIS/WQS_layers_05082020.gdb', layer = 'riverine_05082020' , fid_column_name = "OBJECTID") %>%
                st_transform(4326) )


test <- st_zm(st_read('GIS/WQS_layers_05082020.gdb', layer = 'lakes_reservoirs_05082020' , fid_column_name = "OBJECTID") %>%
                st_transform(4326) )


test <- st_zm(st_read('GIS/WQS_layers_05082020.gdb', layer = 'estuarinelines_05082020' , fid_column_name = "OBJECTID") %>%
  st_transform(4326) )

WQSsELtest <- test %>%
#  filter(BASIN %in% filter(basinCodesConversion, Basin_Code %in% c('James-Lower'))$BASIN) %>%
  {if(WQSwaterbodyType == 'Estuarine')
    rename(., 'WQS_COMMEN' = 'WQS_COMMENT') %>% # match polygon structure
      mutate(Shape_Area = NA) %>%
      dplyr::select(names(test4))
    else .}


test3 <- st_zm(st_read('GIS/WQS_layers_05082020.gdb', layer = 'estuarinepolygons_05082020' , fid_column_name = "OBJECTID") %>%
                 st_transform(4326) )

test4 <- test3 %>%
  filter(BASIN %in% filter(basinCodesConversion, Basin_Code %in% c('James-Lower'))$BASIN)
#basinCodes <- filter(basinAssessmentRegion, BASIN %in% c('2A','2B','2C','3','5A', '7A')) %>% #unique(WQSstatewide()$BASIN)) %>%
#    filter(ASSESS_REG %in% 'PRO') %>% #input$WQSDEQregionSelection) %>%
#    filter(Basin_Code %in% 'James-Lower') %>% #input$WQSsubbasinSelection) %>%
#    distinct(BASIN_CODE) %>% 
#    pull() 






WQSwaterbodyType <- 'Riverine' #"Riverine"   "Lacustrine" "Estuarine" 

DEQregion <- 'BRRO'

subbasinSelection <- c( "James-Middle")

WQSs <-   st_zm(st_read('GIS/processedWQS/RL_2B.shp') , fid_column_name = "OBJECTID") %>%
  st_transform(4326) %>%
  rename("GNIS_Name" = "GNIS_Nm",
         "WATER_NAME" = "WATER_N" ,
         "WQS_COMMENT" = "WQS_COM" ,
         "Basin_Code" = "Basn_Cd",
         "Edit_Date"  = "Edit_Dt",
         "Tier_III" = "Tir_III" ,
         "SECTION_DESCRIPTION" = 'SECTION',
         "created_user" = "crtd_sr",      
         "created_date" ="crtd_dt",
         "last_edited_user" = "lst_dtd_s",
         "last_edited_date" = "lst_dtd_d", "Shape_Length" = "Shp_Lng", 
         "BASIN_CODE" = "BASIN_C", "ASSESS_REG"="ASSESS_" , "Subbasin" = "Subbasn") %>%
  {if(WQSwaterbodyType %in% c('Lacustrine', 'Estuarine'))
    rename(., "Shape_Area" = "Shap_Ar")
    else .}

WQSsEL <- st_zm(st_read('GIS/processedWQS/EL_1A.shp', fid_column_name = "OBJECTID"))%>%
    st_transform(4326) %>%
    # match polygon structure
    rename("GNIS_Name" = "GNIS_Nm",
           "WATER_NAME" = "WATER_N" ,
           #"WQS_COMMENT" = "WQS_COMMEN" , 
           "WQS_COMMENT" = "WQS_COM" ,
           "Basin_Code" = "Basn_Cd",
           "Edit_Date"  = "Edit_Dt",
           "Tier_III" = "Tir_III" ,
           "SECTION_DESCRIPTION" = 'SECTION',
           "created_user" = "crtd_sr",      
           "created_date" ="crtd_dt",
           "last_edited_user" = "lst_dtd_s",
           "last_edited_date" = "lst_dtd_d", "Shape_Length" = "Shp_Lng", 
           "BASIN_CODE" = "BASIN_C", "ASSESS_REG"="ASSESS_" , "Subbasin" = "Subbasn") %>%  # match polygon structure
    mutate(Shape_Area = NA) %>%
    dplyr::select(names(WQSs)) 

basinCodes <- filter(subbasinOptionsByWQStype, waterbodyType %in% WQSwaterbodyType ) %>%
  filter(AssessmentRegion %in% DEQregion) %>%
  filter(Basin_Code %in% subbasinSelection) %>%
  distinct(SubbasinOptions) %>% 
  pull() 

WQSlookup <- loadData("WQSlookupTable")
conventionals_DWQS_Region <- st_intersection(conventionals_DWQS, 
                                             filter(subbasins, BASIN_CODE %in% basinCodes))

# this is all sites limited to waterbody type and subbasin
snap_input <- readRDS('data/processedWQS/WQStable.RDS') %>%
  filter(str_extract(WQS_ID, "^.{2}") %in% filter(WQSlayerConversion, waterbodyType %in% WQSwaterbodyType)$WQS_ID) %>%   
  filter(gsub("_","",str_extract(WQS_ID, ".{3}_")) %in% 
           str_pad(unique(filter(subbasinOptionsByWQStype, SubbasinOptions %in% basinCodes)$SubbasinOptions), 
                   width = 2, side = 'left', pad = '0')) %>%
  # filter out any sites that happen to have existing WQS_ID
  filter(! StationID %in% WQSlookup$StationID) %>%
  group_by(StationID) %>%
  mutate(n = n()) %>% ungroup() 
snap_input_Region <- snap_input %>%
  # limit to just region of interest
  left_join(conventionals_DWQS_Region, by = 'StationID') %>%
  filter(!is.na(Latitude) | !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = T, # don't remove these lat/lon cols from df
           crs = 4326) %>%
  st_intersection(filter(assessmentRegions, ASSESS_REG %in% DEQregion)) %>%#input$WQSDEQregionSelection) %>%
  st_drop_geometry() %>% # back to tibble
  rename('Buffer Distance' = 'Buffer.Distance') %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`, n)


# Make dataset of all sites in subbasin for highlighting purposes, preliminary list, left at subbasin in case regional swapping occurs or outside state boundary
sitesUnique <- snap_input %>%
  full_join(conventionals_DWQS_Region, by = 'StationID') %>%
  filter(!is.na(Latitude) | !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) 
# Make dataset of all WQS_IDs available for table purposes, this will hold corrected WQS_ID information after user review
#WQS_IDs <- snap_input
# Make dataset of multiple segments snapped to single site
tooMany <- filter(snap_input_Region, n > 1) %>%
  group_by(StationID) %>% mutate(colorFac = row_number()) %>% ungroup() 

tooMany_sf <- filter(WQSs, WQS_ID %in% tooMany$WQS_ID) %>%              # defaults to polygon for Estuarine
  left_join(tooMany, by = 'WQS_ID') %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())
if(WQSwaterbodyType == 'Estuarine'){
  tooMany_sf_EL <- filter(WQSsEL, WQS_ID %in% tooMany$WQS_ID) %>%              # bonus polyline feature for Estuarine
    left_join(tooMany, by = 'WQS_ID') %>%
    dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())
}
# Make dataset of sites associated with too many segments
tooMany_sites <- filter(sitesUnique, StationID %in% tooMany$StationID) %>%
  left_join(WQSs %>% st_drop_geometry(), by = 'WQS_ID') %>% #left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
  {if(WQSwaterbodyType == 'Estuarine')
    rbind(left_join(filter(sitesUnique, StationID %in% tooMany$StationID), WQSsEL %>% st_drop_geometry(), by = 'WQS_ID'))
  else . } %>%
  distinct(StationID, .keep_all = T) %>%
  dplyr::select(-c(WQS_ID, `Buffer Distance`, n))
# Make dataset of sites that snapped to a single WQS and join WQS info  
snapSingle <- filter(sitesUnique, n == 1) %>%
  filter(StationID %in% snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
  left_join(WQSs %>% st_drop_geometry(), by = 'WQS_ID') %>%#left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
  {if(WQSwaterbodyType == 'Estuarine')
    filter(., str_extract(WQS_ID, "^.{2}") == 'EP') %>% # keep just polygon result from above
      rbind(filter(sitesUnique, n == 1) %>%
              filter(StationID %in% snap_input_Region$StationID & str_extract(WQS_ID, "^.{2}") == 'EL') %>%
              left_join(WQSsEL %>% st_drop_geometry(), by = 'WQS_ID') )
    else . } %>%
  mutate(`Buffer Distance` = as.factor(`Buffer Distance`))
# Make dataset of sites associated with no segments
snapNone <- filter(sitesUnique, is.na(WQS_ID)) %>%
  filter(StationID %in% snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
  left_join(WQSs %>% st_drop_geometry(), by = 'WQS_ID')#left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
# Make empty dataset of sites that assessors touched
sitesAdjusted <-  sitesUnique[0,] %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`) %>%
  mutate(Comments = as.character())
# Make dataset for user to download
finalWQS <- WQSlookup



##########################################
### start here tomorrow
#sitesUniqueFin <- conventionals_DWQS_Region
  #left_join(conventionals_DWQS_Region %>% st_drop_geometry(), 
                  #          dplyr::select(sitesUnique, StationID, WQS_ID = NA, `Buffer Distance` = NA, n = NA)  %>% st_drop_geometry(),  by = 'StationID') %>%
  #dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, FDT_STA_ID, everything())
  #rbind(sitesUnique,
                  #      mutate(conventionals_DWQS_Region, WQS_ID = NA, `Buffer Distance` = NA, n = NA) %>% 
                  #        dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, FDT_STA_ID, everything())) 

namesToSmash <- c('2-JMS109.16','2-GIL000.42')

selectedSiteTableWQS <- filter(sitesUnique, FDT_STA_ID %in% namesToSmash) %>%
  st_drop_geometry() 


sitesUpdated <- filter(sitesUnique, StationID %in% namesToSmash) %>%
  #st_drop_geometry() %>%
  distinct( StationID, .keep_all = T) %>%
  
  mutate(WQS_ID = case_when(StationID == "2-JMS109.16" ~ "EP_2C_000167", TRUE ~ as.character(WQS_ID))) %>%
  
  mutate(`Buffer Distance` = paste0('Manual Review | ', `Buffer Distance`),
         Comments = paste0('Manual Accept | ', 'comment comment comment')) %>%#input$acceptCommentWQS)) %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`, Comments)


sitesAdjusted <- rbind(sitesAdjusted, sitesUpdated) # rbind works better for sf objects



#filter(WQSs, WQS_ID %in% filter(snap_input, StationID %in% namesToSmash)$WQS_ID) %>%
#  st_drop_geometry() %>%
#  {if(WQSwaterbodyType == 'Estuarine')
#    filter(WQSsEL, WQS_ID %in% filter(snap_input, StationID %in% namesToSmash)$WQS_ID)  } %>%
#  dplyr::select(WQS_ID, everything())

                      

z <- filter(test %>% st_drop_geometry(), WQS_ID %in% filter(sitesAdjusted, StationID %in% namesToSmash)$WQS_ID) %>%
  {if(WQSwaterbodyType == 'Estuarine')
    rbind(., 
      filter(WQSsEL %>% st_drop_geometry(), WQS_ID %in% filter(sitesAdjusted, StationID %in% namesToSmash)$WQS_ID))  } %>%
#  st_drop_geometry() %>%
  dplyr::select(WQS_ID, everything())                      



filter(test %>% st_drop_geometry(), WQS_ID %in% filter(sitesAdjusted, StationID %in% namesToSmash)$WQS_ID) %>%
  {if(WQSwaterbodyType == 'Estuarine')
    rbind(.,
          filter(WQSsEL() %>% st_drop_geometry(), WQS_ID %in% 
                   filter(sitesAdjusted, StationID %in% namesToSmash)$WQS_ID) ) 
    else .} %>%
  #   st_drop_geometry() %>%
  dplyr::select(WQS_ID, everything()) 




pal <- colorFactor(
  palette = topo.colors(7),
  domain = assessmentRegions$ASSESS_REG)


palBufferDistance <- colorFactor(
  palette = terrain.colors(5),#colorRamps::blue2red(5),
  levels = c("20 m", "40 m", "60 m", "80 m", "No connections within 80 m"))
#  domain = assessmentRegions$ASSESS_REG)



CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE, 
             options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20)) %>%
  setView(-78, 37.5, zoom=7)  %>% 
  addCircleMarkers(data=snapSingle,
                   layerId = ~paste0(StationID,'_snapSingle'), # need unique layerID 
                   label=~StationID, group="Stations Snapped to 1 WQS Segment", 
                   weight = 1,
                   fillColor= ~palBufferDistance(snapSingle$`Buffer Distance`), fillOpacity = 0.5,stroke=0.3) %>%
  addLegend(position = 'topright', pal = palBufferDistance, values = snapSingle$`Buffer Distance`, 
            group = 'Stations Snapped to 1 WQS Segment') %>%
  
  
  
  addCircleMarkers(data = conventionals_DWQS_Region, color='blue', fillColor='yellow', radius = 4,
                   fillOpacity = 0.5,opacity=0.8,weight = 1,stroke=T, group="Conventionals Stations in Basin",
                   label = ~FDT_STA_ID, layerId = ~FDT_STA_ID) %>% 
  {if("sfc_MULTIPOLYGON" %in% class(st_geometry(WQSs)))#WQSs()))) 
    addPolygons(., data = WQSs,#WQSs(),
                layerId = ~WQS_ID,
                label=~WQS_ID, group="All WQS in selected Region/Basin", 
                color = 'blue', 
                weight = 3,stroke=T,
                popup=leafpop::popupTable(test2),#WQSs()),
                popupOptions = popupOptions( maxHeight = 100 )) %>% 
      hideGroup("All WQS in selected Region/Basin") 
    else addPolylines(., data =WQSs, # WQSs(),
              layerId = ~WQS_ID,
              label=~WQS_ID, group="All WQS in selected Region/Basin", 
              color = 'blue',
              weight = 3,stroke=T,
              popup=leafpop::popupTable(WQSs),#WQSs()),
              popupOptions = popupOptions( maxHeight = 100 )) %>% 
  hideGroup("All WQS in selected Region/Basin")  } %>%
  addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
              fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
              group="Assessment Regions",
              popup=leafpop::popupTable(assessmentRegions, zcol=c('ASSESS_REG'))) %>% hideGroup('Assessment Regions') %>% #,'VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
  {if(WQSwaterbodyType == 'Estuarine')
    addPolylines(., data =WQSsEL, # WQSs(),
                 layerId = ~WQS_ID,
                 label=~WQS_ID, group="All WQS in selected Region/Basin", 
                 color = 'orange',
                 weight = 3,stroke=T,
                 popup=leafpop::popupTable(WQSsEL),#WQSs()),
                 popupOptions = popupOptions( maxHeight = 100 )) %>% 
      hideGroup("All WQS in selected Region/Basin") } %>%
  inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
  inlmisc::AddSearchButton(group = "Conventionals Stations in Basin", zoom = 15,propertyName = "label",
                           textPlaceholder = "Search Conventionals Stations in Basin") %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c('Conventionals Stations in Basin',"All WQS in selected Region/Basin",'Assessment Regions'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft') %>%
  hideGroup("Conventionals Stations in Basin") 







palTooMany <- colorNumeric(c('green','yellow', 'blue','red', 'pink','purple'), domain =tooMany$colorFac)



CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE, 
             options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20)) %>%
  setView(-78, 37.5, zoom=7)  %>% 
  {if("sfc_MULTIPOLYGON" %in% class(st_geometry(WQSs))) 
    addPolygons(., data = WQSs(),
                layerId = ~WQS_ID,
                label=~WQS_ID, group="All WQS in selected Region/Basin", 
                color = 'blue', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
                weight = 3,stroke=T,
                popup=leafpop::popupTable(WQSs),
                popupOptions = popupOptions( maxHeight = 100 )) %>% 
      hideGroup("All WQS in selected Region/Basin") 
    else addPolylines(., data = WQSs,
                      layerId = ~WQS_ID,
                      label=~WQS_ID, group="All WQS in selected Region/Basin", 
                      color = 'blue', 
                      weight = 3,stroke=T,
                      popup=leafpop::popupTable(WQSs()),
                      popupOptions = popupOptions( maxHeight = 100 )) %>% 
      hideGroup("All WQS in selected Region/Basin")  } %>%
  addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
              fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
              group="Assessment Regions",
              popup=leafpop::popupTable(assessmentRegions, zcol=c('ASSESS_REG'))) %>% hideGroup('Assessment Regions') %>% #,'VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
  addCircleMarkers(data=tooMany_sites,
                   layerId = ~paste0(StationID,'_tooMany'),  # need unique layerID 
                   label=~StationID, group="Stations Snapped to > 1 WQS Segment", 
                   color='black', fillColor='red', radius = 5,
                   fillOpacity = 0.8,opacity=0.5,weight = 2,stroke=T) %>%
  {if(nrow(tooMany_sf) > 0 & "sfc_MULTIPOLYGON" %in% class(st_geometry(tooMany_sf))) 
    addPolygons(., data=tooMany_sf,
                layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                color = ~palTooMany(tooMany_sf$colorFac),weight = 3,stroke=T,
                popup=leafpop::popupTable(tooMany_sf),
                popupOptions = popupOptions( maxHeight = 100 )) %>% 
      hideGroup("All WQS in selected Region/Basin")
    else . } %>%
  {if(nrow(tooMany_sf) > 0 & c("sfc_LINESTRING") %in% class(st_geometry(tooMany_sf)))
    addPolylines(., data=tooMany_sf,
                 layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                 label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                 color = ~palTooMany(tooMany_sf$colorFac),weight = 3,stroke=T,
                 popup=leafpop::popupTable(tooMany_sf),
                 popupOptions = popupOptions( maxHeight = 100 )) %>%
      hideGroup("WQS Segments of Stations Snapped to > 1 Segment")
    else . } %>%
  {if(nrow(tooMany_sf_EL) > 0)
    addPolylines(., data=tooMany_sf_EL,
                 layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                 label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                 color = ~palTooMany(tooMany_sf_EL$colorFac),weight = 3,stroke=T,
                 popup=leafpop::popupTable(tooMany_sf_EL),
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
                   position='topleft') %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c('Conventionals Stations in Basin',"All WQS in selected Region/Basin",'Assessment Regions'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft')





CreateWebMap(maps = c("Topo","Imagery","Hydrography"), collapsed = TRUE, 
             options= leafletOptions(zoomControl = TRUE,minZoom = 3, maxZoom = 20)) %>%
  setView(-78, 37.5, zoom=7)  %>% 
  addCircleMarkers(data=tooMany_sites,
                   layerId = ~paste0(StationID,'_tooMany'),  # need unique layerID 
                   label=~StationID, group="Stations Snapped to > 1 WQS Segment", 
                   color='black', fillColor='red', radius = 5,
                   fillOpacity = 0.8,opacity=0.5,weight = 2,stroke=T) %>%
  {if(nrow(tooMany_sf) > 0) 
    {if("sfc_MULTIPOLYGON" %in% class(st_geometry(tooMany_sf))) 
      addPolygons(., data=tooMany_sf,
                  layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                  label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                  color = ~palTooMany(tooMany_sf$colorFac),weight = 3,stroke=T,
                  popup=leafpop::popupTable(tooMany_sf),
                  popupOptions = popupOptions( maxHeight = 100 )) %>% 
      hideGroup("All WQS in selected Region/Basin") 
    else 
      addPolylines(., data=tooMany_sf,
                   layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
                   label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
                   color = ~palTooMany(tooMany_sf$colorFac),weight = 3,stroke=T,
                   popup=leafpop::popupTable(tooMany_sf),
                   popupOptions = popupOptions( maxHeight = 100 )) %>%
      hideGroup("WQS Segments of Stations Snapped to > 1 Segment")}
    else . } %>%
#  {if(nrow(tooMany_sf_EL) > 0)
#    addPolylines(., data=tooMany_sf_EL,
#                layerId = ~paste0(WQS_ID,'_tooMany'),  # need unique layerID 
#                 label=~WQS_ID, group="WQS Segments of Stations Snapped to > 1 Segment", 
#                 color = ~palTooMany(tooMany_sf_EL$colorFac),weight = 3,stroke=T,
#                 popup=leafpop::popupTable(tooMany_sf_EL),
#                 popupOptions = popupOptions( maxHeight = 100 )) %>%
#      hideGroup("WQS Segments of Stations Snapped to > 1 Segment")  
#    else . } %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c("Adjusted Sites",
                                     "Stations Snapped to 1 WQS Segment",
                                     "Stations Snapped to > 1 WQS Segment",
                                     "WQS Segments of Stations Snapped to > 1 Segment",
                                     "Stations Snapped to 0 WQS Segments",
                                     #"All stations in the selected Region/Basin",
                                     #'Conventionals Stations in Basin',
                                     "All WQS in selected Region/Basin",'Assessment Regions'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft') 
  
  
  