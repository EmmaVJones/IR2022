
st_layers('GIS/WQS_layers_05082020.gdb')


test <- st_zm(st_read('GIS/WQS_layers_05082020.gdb', layer = 'riverine_05082020' , fid_column_name = "OBJECTID") %>%
  st_transform(4326) )

test2 <- test %>%
  filter(BASIN %in% filter(basinCodesConversion, Basin_Code %in% c('New'))$BASIN)

basinCodes <- filter(basinAssessmentRegion, BASIN %in% c('9', '2A', '2B')) %>% #unique(WQSstatewide()$BASIN)) %>%
    filter(ASSESS_REG %in% 'BRRO') %>% #input$WQSDEQregionSelection) %>%
    filter(Basin_Code %in% 'New') %>% #input$WQSsubbasinSelection) %>%
    distinct(BASIN_CODE) %>% 
    pull() 

# this is all sites limited to waterbody type and subbasin
snap_input <- readRDS('data/processedWQS/WQStable.RDS') %>%
  filter(str_extract(WQS_ID, "^.{2}") %in% filter(WQSlayerConversion, waterbodyType %in% 'Riverine')$WQS_ID) %>% 
  filter(gsub("_","",str_extract(WQS_ID, ".{3}_")) %in% 
           str_pad(unique(filter(basinAssessmentRegion, BASIN_CODE %in% basinCodes)$BASIN_CODE), 
                   width = 2, side = 'left', pad = '0')) %>%
  # filter out any sites that happen to have existing WQS_ID
  filter(! WQS_ID %in% WQSlookup$WQS_ID) %>%
  group_by(StationID) %>%
  mutate(n = n()) %>% ungroup() 
snap_input_Region <- snap_input %>%
  # limit to just region of interest
  left_join(conventionals_DWQS, by = 'StationID') %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = T, # don't remove these lat/lon cols from df
           crs = 4326) %>%
  st_intersection(filter(assessmentRegions, ASSESS_REG %in% 'BRRO')) %>%#input$WQSDEQregionSelection) %>%
  st_drop_geometry() %>% # back to tibble
  rename('Buffer Distance' = 'Buffer.Distance') %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`, n)


# Make dataset of all sites in subbasin for highlighting purposes, preliminary list, left at subbasin in case regional swapping occurs or outside state boundary
sitesUnique <- snap_input %>%
  full_join(conventionals_DWQS, by = 'StationID') %>%
  st_as_sf(coords = c("Longitude", "Latitude"),  # make spatial layer using these columns
           remove = F, # don't remove these lat/lon cols from df
           crs = 4326) 
# Make dataset of all WQS_IDs available for table purposes, this will hold corrected WQS_ID information after user review
WQS_IDs <- snap_input
# Make dataset of multiple segments snapped to single site
tooMany <- filter(snap_input_Region, n > 1) %>%
  group_by(StationID) %>% mutate(colorFac = row_number()) %>% ungroup() 
tooMany_sf <- filter(test2, WQS_ID %in% tooMany$WQS_ID) %>%
  left_join(tooMany, by = 'WQS_ID') %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, everything())
# Make dataset of sites associated with too many segments
tooMany_sites <- filter(sitesUnique, StationID %in% tooMany$StationID) %>%
  left_join(test2 %>% st_drop_geometry(), by = 'WQS_ID') %>%#left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
  distinct(StationID, .keep_all = T) %>%
  dplyr::select(-c(WQS_ID, `Buffer Distance`, n))
# Make dataset of sites that snapped to a single WQS and join WQS info  
snapSingle <- filter(sitesUnique, n == 1) %>%
  filter(StationID %in% snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
  left_join(test2 %>% st_drop_geometry(), by = 'WQS_ID') %>%#left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
  mutate(`Buffer Distance` = as.factor(`Buffer Distance`))
# Make dataset of sites associated with no segments
snapNone <- filter(sitesUnique, is.na(WQS_ID)) %>%
  filter(StationID %in% snap_input_Region$StationID) %>% # limit assignment to just what falls in a region
  left_join(test2 %>% st_drop_geometry(), by = 'WQS_ID')#left_join(WQSs() %>% st_drop_geometry(), by = 'WQS_ID')
# Make empty dataset of sites that assessors touched
sitesAdjusted <-  sitesUnique[0,] %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`) %>%
  mutate(Comments = as.character())
# Make dataset for user to download
finalWQS <- WQSlookup



##########################################
### start here tomorrow
#sitesUniqueFin <- conventionals_DWQS
  #left_join(conventionals_DWQS %>% st_drop_geometry(), 
                  #          dplyr::select(sitesUnique, StationID, WQS_ID = NA, `Buffer Distance` = NA, n = NA)  %>% st_drop_geometry(),  by = 'StationID') %>%
  #dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, FDT_STA_ID, everything())
  #rbind(sitesUnique,
                  #      mutate(conventionals_DWQS, WQS_ID = NA, `Buffer Distance` = NA, n = NA) %>% 
                  #        dplyr::select(StationID, WQS_ID, `Buffer Distance`, n, FDT_STA_ID, everything())) 

namesToSmash <- c('9-SNK005.38')

selectedSiteTableWQS <- filter(sitesUnique, FDT_STA_ID %in% namesToSmash) %>%
  st_drop_geometry() 


sitesUpdated <- filter(sitesUnique, StationID %in% namesToSmash) %>%
  #st_drop_geometry() %>%
  distinct( StationID, .keep_all = T) %>%
  mutate(`Buffer Distance` = paste0('Manual Review | ', `Buffer Distance`),
         Comments = paste0('Manual Accept | ', 'comment comment comment')) %>%#input$acceptCommentWQS)) %>%
  dplyr::select(StationID, WQS_ID, `Buffer Distance`, Comments)


sitesAdjusted <- rbind(sitesAdjusted, sitesUpdated) # rbind works better for sf objects

unique(c(as.character(filter(tooMany, StationID %in% namesToSmash)$WQS_ID), # likely WQS
         as.character(test2$WQS_ID)))
                                            

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
            group = 'Stations Snapped to 1 WQS Segment')
  
  
  
  addCircleMarkers(data = conventionals_DWQS, color='blue', fillColor='yellow', radius = 4,
                   fillOpacity = 0.5,opacity=0.8,weight = 1,stroke=T, group="Conventionals Stations in Basin",
                   label = ~FDT_STA_ID, layerId = ~FDT_STA_ID) %>% 
  {if("sfc_MULTIPOLYGON" %in% class(st_geometry(test2)))#WQSs()))) 
    addPolygons(., data = test2,#WQSs(),
                layerId = ~WQS_ID,
                label=~WQS_ID, group="All WQS in selected Region/Basin", 
                color = 'blue', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
                weight = 3,stroke=T,
                popup=leafpop::popupTable(test2),#WQSs()),
                popupOptions = popupOptions( maxHeight = 100 )) %>% 
      hideGroup("All WQS in selected Region/Basin") 
    else addPolylines(., data =test2, # WQSs(),
              layerId = ~WQS_ID,
              label=~WQS_ID, group="All WQS in selected Region/Basin", 
              color = 'blue', #color = ~palTooMany(reactive_objects$tooMany$colorFac),
              weight = 3,stroke=T,
              popup=leafpop::popupTable(test2),#WQSs()),
              popupOptions = popupOptions( maxHeight = 100 )) %>% 
  hideGroup("All WQS in selected Region/Basin")  } %>%
  addPolygons(data= assessmentRegions,  color = 'black', weight = 1,
              fillColor= ~pal(assessmentRegions$ASSESS_REG), fillOpacity = 0.5,stroke=0.1,
              group="Assessment Regions",
              popup=leafpop::popupTable(assessmentRegions, zcol=c('ASSESS_REG'))) %>% hideGroup('Assessment Regions') %>% #,'VAHU6','FedName'))) %>% hideGroup('Assessment Regions') %>%
  inlmisc::AddHomeButton(raster::extent(-83.89, -74.80, 36.54, 39.98), position = "topleft") %>%
  inlmisc::AddSearchButton(group = "Conventionals Stations in Basin", zoom = 15,propertyName = "label",
                           textPlaceholder = "Search Conventionals Stations in Basin") %>%
  addLayersControl(baseGroups=c("Topo","Imagery","Hydrography"),
                   overlayGroups = c('Conventionals Stations in Basin',"All WQS in selected Region/Basin",'Assessment Regions'),
                   options=layersControlOptions(collapsed=T),
                   position='topleft') %>%
  hideGroup("Conventionals Stations in Basin") 
